#!/usr/bin/env python3
"""Build the query-on-demand defensive runtime layer for BASE.

The defensive master supplies the seven fielders' starting locations. The
College26 pitch master supplies hitter, pitch, batted-ball, and outcome context.
Rows are matched by a unique PitchUID first, then by a unique
GameUID + PitchNo + Date combination. Both source identifiers and timestamps
remain in the output so every match can be audited.
"""

from __future__ import annotations

import argparse
import shutil
from pathlib import Path

import duckdb
import pyarrow as pa
import pyarrow.parquet as pq
from huggingface_hub import HfFileSystem


COLLEGE_COLUMNS = [
    "PitchUID", "GameUID", "GameID", "PitchNo", "Date", "Time",
    "LocalDateTime", "UTCDateTime", "PitcherTeam", "BatterTeam",
    "Pitcher", "PitcherId", "PitcherThrows", "Batter", "BatterId",
    "BatterSide", "Inning", "Top/Bottom", "Outs", "Balls", "Strikes",
    "TaggedPitchType", "AutoPitchType", "PitchCall", "TaggedHitType",
    "AutoHitType", "PlayResult", "OutsOnPlay", "RunsScored", "ExitSpeed",
    "Angle", "Direction", "Bearing", "Distance", "HangTime", "MaxHeight",
    "PositionAt110X", "PositionAt110Y", "PositionAt110Z",
    "ContactPositionX", "ContactPositionY", "ContactPositionZ",
    "HitLandingConfidence", "Level", "League", "Stadium",
]

POSITIONS = ("1B", "2B", "3B", "SS", "LF", "CF", "RF")


def sql_path(path: Path) -> str:
    return str(path.resolve()).replace("\\", "/").replace("'", "''")


def materialize_college_projection(source: str, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.exists():
        destination.unlink()

    fs = HfFileSystem() if source.startswith("hf://") else None
    handle = fs.open(source, "rb") if fs else source
    parquet = pq.ParquetFile(handle)
    available = set(parquet.schema_arrow.names)
    columns = [column for column in COLLEGE_COLUMNS if column in available]
    missing = sorted(set(("PitchUID", "GameUID", "PitchNo", "Date")).difference(columns))
    if missing:
        raise ValueError(f"College source is missing join fields: {', '.join(missing)}")

    writer = None
    try:
        for batch in parquet.iter_batches(batch_size=100_000, columns=columns):
            table = pa.Table.from_batches([batch])
            if writer is None:
                writer = pq.ParquetWriter(
                    destination,
                    table.schema,
                    compression="zstd",
                    compression_level=6,
                    use_dictionary=True,
                )
            writer.write_table(table)
    finally:
        if writer is not None:
            writer.close()
        if fs and hasattr(handle, "close"):
            handle.close()


def build_runtime(
    defense_source: Path,
    college_projection: Path,
    output: Path,
    buckets: int,
    fielding_teams: list[str],
) -> None:
    if output.exists():
        shutil.rmtree(output)
    events_dir = output / "events"
    catalogs_dir = output / "catalogs"
    output.mkdir(parents=True, exist_ok=True)
    events_dir.mkdir(parents=True, exist_ok=True)
    catalogs_dir.mkdir(parents=True, exist_ok=True)

    defense = sql_path(defense_source)
    college = sql_path(college_projection)
    events = sql_path(events_dir)
    catalogs = sql_path(catalogs_dir)
    joined_file = sql_path(output / "defense_events_joined.parquet")
    team_filter = ""
    if fielding_teams:
        quoted_teams = ", ".join(
            "'" + team.replace("'", "''") + "'" for team in fielding_teams
        )
        team_filter = f"WHERE PitcherTeam IN ({quoted_teams})"

    con = duckdb.connect(str(output / "defense_build.duckdb"))
    con.execute("PRAGMA threads=4")
    con.execute("PRAGMA preserve_insertion_order=false")

    position_sql = []
    for position in POSITIONS:
        position_sql.extend([
            f'nullif(d."{position}_Name", \'\') AS "{position}_Name"',
            f'nullif(d."{position}_Id", \'\') AS "{position}_Id"',
            f'try_cast(nullif(d."{position}_PositionAtReleaseX", \'\') AS DOUBLE) AS "{position}_Depth"',
            f'try_cast(nullif(d."{position}_PositionAtReleaseZ", \'\') AS DOUBLE) AS "{position}_Lateral"',
        ])

    joined_select = ",\n            ".join([
        "d.DefenseRowId",
        "CASE WHEN e.PitchUID IS NOT NULL THEN 'pitch_uid' WHEN f.GameUID IS NOT NULL THEN 'game_pitch_date' ELSE 'unmatched' END AS JoinMethod",
        "coalesce(e.PitchUID, f.PitchUID, nullif(d.PitchUID, '')) AS PitchUID",
        "nullif(d.PlayID, '') AS PlayID",
        "nullif(d.GameUID, '') AS DefenseGameUID",
        "coalesce(e.GameUID, f.GameUID, nullif(d.GameUID, '')) AS GameUID",
        "coalesce(e.GameID, f.GameID) AS GameID",
        "try_cast(nullif(d.PitchNo, '') AS BIGINT) AS PitchNo",
        "try_cast(nullif(d.Date, '') AS DATE) AS DefenseDate",
        "nullif(d.Time, '') AS DefenseTime",
        "coalesce(e.Date, f.Date, try_cast(nullif(d.Date, '') AS DATE)) AS Date",
        "coalesce(e.Time, f.Time) AS PitchTime",
        "CASE WHEN coalesce(e.Time, f.Time) IS NULL OR nullif(d.Time, '') IS NULL THEN 'time_missing' WHEN abs(date_diff('second', try_cast(nullif(d.Time, '') AS TIME), coalesce(e.Time, f.Time))) <= 3 THEN 'time_aligned' ELSE 'time_mismatch' END AS TimeAuditStatus",
        "CASE WHEN coalesce(e.Time, f.Time) IS NULL OR nullif(d.Time, '') IS NULL THEN NULL ELSE date_diff('second', try_cast(nullif(d.Time, '') AS TIME), coalesce(e.Time, f.Time)) END AS TimeDifferenceSeconds",
        "coalesce(e.LocalDateTime, f.LocalDateTime) AS LocalDateTime",
        "coalesce(e.UTCDateTime, f.UTCDateTime) AS UTCDateTime",
        "nullif(d.PitcherTeam, '') AS FieldingTeam",
        "coalesce(e.BatterTeam, f.BatterTeam, nullif(d.BatterTeam, '')) AS BatterTeam",
        "coalesce(e.Pitcher, f.Pitcher) AS Pitcher",
        "coalesce(e.PitcherId, f.PitcherId) AS PitcherId",
        "coalesce(e.PitcherThrows, f.PitcherThrows) AS PitcherThrows",
        "coalesce(e.Batter, f.Batter) AS Batter",
        "coalesce(e.BatterId, f.BatterId) AS BatterId",
        "coalesce(e.BatterSide, f.BatterSide) AS BatterSide",
        "coalesce(e.Inning, f.Inning) AS Inning",
        "coalesce(e.\"Top/Bottom\", f.\"Top/Bottom\") AS TopBottom",
        "coalesce(e.Outs, f.Outs) AS Outs",
        "coalesce(e.Balls, f.Balls) AS Balls",
        "coalesce(e.Strikes, f.Strikes) AS Strikes",
        "coalesce(e.TaggedPitchType, f.TaggedPitchType) AS TaggedPitchType",
        "coalesce(e.AutoPitchType, f.AutoPitchType) AS AutoPitchType",
        "coalesce(e.PitchCall, f.PitchCall, nullif(d.PitchCall, '')) AS PitchCall",
        "coalesce(e.TaggedHitType, f.TaggedHitType) AS TaggedHitType",
        "coalesce(e.AutoHitType, f.AutoHitType) AS AutoHitType",
        "coalesce(e.PlayResult, f.PlayResult, nullif(d.PlayResult, '')) AS PlayResult",
        "coalesce(e.OutsOnPlay, f.OutsOnPlay) AS OutsOnPlay",
        "coalesce(e.RunsScored, f.RunsScored) AS RunsScored",
        "coalesce(e.ExitSpeed, f.ExitSpeed) AS ExitSpeed",
        "coalesce(e.Angle, f.Angle) AS LaunchAngle",
        "coalesce(e.Direction, f.Direction) AS Direction",
        "coalesce(e.Bearing, f.Bearing) AS Bearing",
        "coalesce(e.Distance, f.Distance) AS Distance",
        "coalesce(e.HangTime, f.HangTime) AS HangTime",
        "coalesce(e.MaxHeight, f.MaxHeight) AS MaxHeight",
        "coalesce(e.PositionAt110X, f.PositionAt110X) AS PositionAt110X",
        "coalesce(e.PositionAt110Y, f.PositionAt110Y) AS PositionAt110Y",
        "coalesce(e.PositionAt110Z, f.PositionAt110Z) AS PositionAt110Z",
        "coalesce(e.ContactPositionX, f.ContactPositionX) AS ContactPositionX",
        "coalesce(e.ContactPositionY, f.ContactPositionY) AS ContactPositionY",
        "coalesce(e.ContactPositionZ, f.ContactPositionZ) AS ContactPositionZ",
        "coalesce(e.HitLandingConfidence, f.HitLandingConfidence) AS HitLandingConfidence",
        "coalesce(e.Level, f.Level) AS Level",
        "coalesce(e.League, f.League) AS League",
        "coalesce(e.Stadium, f.Stadium) AS Stadium",
        "nullif(d.DetectedShift, '') AS DetectedShift",
        "try_cast(nullif(d.FHC, '') AS BOOLEAN) AS FieldTrackingComplete",
        *position_sql,
        f"cast(hash(coalesce(nullif(d.PitcherTeam, ''), '<unknown>')) % {buckets} AS INTEGER) AS Bucket",
    ])

    con.execute(f"""
        COPY (
          WITH defense AS (
            SELECT
              hash(coalesce(nullif(PitchUID, ''), concat_ws('|', GameUID, PitchNo, Date, Time))) AS DefenseRowId,
              *
            FROM read_parquet('{defense}')
            {team_filter}
          ),
          college AS (
            SELECT *, try_cast(PitchNo AS BIGINT) AS NormalizedPitchNo
            FROM read_parquet('{college}')
            {team_filter}
          ),
          exact_keys AS (
            SELECT PitchUID
            FROM college
            WHERE PitchUID IS NOT NULL AND PitchUID <> ''
            GROUP BY PitchUID
            HAVING count(*) = 1
          ),
          exact_match AS (
            SELECT c.* EXCLUDE (NormalizedPitchNo)
            FROM college c
            INNER JOIN exact_keys k USING (PitchUID)
          ),
          fallback_keys AS (
            SELECT GameUID, NormalizedPitchNo, Date
            FROM college
            WHERE GameUID IS NOT NULL
            GROUP BY GameUID, NormalizedPitchNo, Date
            HAVING count(*) = 1
          ),
          fallback_match AS (
            SELECT c.* EXCLUDE (NormalizedPitchNo)
            FROM college c
            INNER JOIN fallback_keys k USING (GameUID, NormalizedPitchNo, Date)
          )
          SELECT
            {joined_select}
          FROM defense d
          LEFT JOIN exact_match e
            ON nullif(d.PitchUID, '') = e.PitchUID
          LEFT JOIN fallback_match f
            ON nullif(d.GameUID, '') = f.GameUID
           AND try_cast(nullif(d.PitchNo, '') AS BIGINT) = try_cast(f.PitchNo AS BIGINT)
           AND try_cast(nullif(d.Date, '') AS DATE) = f.Date
           AND (
             nullif(d.Time, '') IS NULL OR f.Time IS NULL OR
             abs(date_diff('second', try_cast(nullif(d.Time, '') AS TIME), f.Time)) <= 3
           )
        ) TO '{joined_file}' (
          FORMAT PARQUET, COMPRESSION SNAPPY,
          ROW_GROUP_SIZE 100000, OVERWRITE_OR_IGNORE TRUE
        )
    """)

    con.execute(f"""
        COPY (
          SELECT *
          FROM read_parquet('{joined_file}')
        ) TO '{events}' (
          FORMAT PARQUET, PARTITION_BY (Bucket), COMPRESSION ZSTD,
          COMPRESSION_LEVEL 3, ROW_GROUP_SIZE 100000, OVERWRITE_OR_IGNORE TRUE
        )
    """)

    con.execute(f"""
        COPY (
          SELECT
            FieldingTeam,
            min(Bucket) AS Bucket,
            count(*) AS Pitches,
            count(DISTINCT GameUID) AS Games,
            count(*) FILTER (WHERE PitchCall = 'InPlay') AS BallsInPlay,
            count(*) FILTER (WHERE FieldTrackingComplete) AS TrackedPitches,
            count(*) FILTER (WHERE JoinMethod <> 'unmatched') AS MatchedPitches,
            min(Date) AS FirstDate,
            max(Date) AS LastDate
          FROM read_parquet('{joined_file}')
          WHERE FieldingTeam IS NOT NULL
          GROUP BY FieldingTeam
          ORDER BY FieldingTeam
        ) TO '{catalogs}/teams.parquet' (
          FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 3,
          OVERWRITE_OR_IGNORE TRUE
        )
    """)

    player_unions = []
    for position in POSITIONS:
        player_unions.append(f"""
          SELECT FieldingTeam, Bucket, '{position}' AS Position,
                 \"{position}_Id\" AS PlayerId, \"{position}_Name\" AS Player,
                 GameUID, Date, PitchCall, JoinMethod
          FROM read_parquet('{joined_file}')
          WHERE \"{position}_Name\" IS NOT NULL
        """)
    con.execute(f"""
        COPY (
          WITH players AS ({' UNION ALL '.join(player_unions)})
          SELECT FieldingTeam, min(Bucket) AS Bucket, Position, PlayerId, Player,
                 count(*) AS Pitches, count(DISTINCT GameUID) AS Games,
                 count(*) FILTER (WHERE PitchCall = 'InPlay') AS BallsInPlay,
                 count(*) FILTER (WHERE JoinMethod <> 'unmatched') AS MatchedPitches,
                 min(Date) AS FirstDate, max(Date) AS LastDate
          FROM players
          GROUP BY FieldingTeam, Position, PlayerId, Player
          ORDER BY FieldingTeam, Position, Player
        ) TO '{catalogs}/players.parquet' (
          FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 3,
          OVERWRITE_OR_IGNORE TRUE
        )
    """)

    con.execute(f"""
        COPY (
          SELECT FieldingTeam, min(Bucket) AS Bucket, BatterTeam, BatterId,
                 Batter, BatterSide, count(*) AS Pitches,
                 count(*) FILTER (WHERE PitchCall = 'InPlay') AS BallsInPlay,
                 count(DISTINCT GameUID) AS Games
          FROM read_parquet('{joined_file}')
          WHERE FieldingTeam IS NOT NULL AND Batter IS NOT NULL
          GROUP BY FieldingTeam, BatterTeam, BatterId, Batter, BatterSide
          ORDER BY FieldingTeam, BatterTeam, Batter
        ) TO '{catalogs}/batters.parquet' (
          FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 3,
          OVERWRITE_OR_IGNORE TRUE
        )
    """)

    summary = con.execute(f"""
        SELECT JoinMethod, count(*) AS Rows,
               round(100.0 * count(*) / sum(count(*)) OVER (), 3) AS Percent
        FROM read_parquet('{joined_file}')
        GROUP BY JoinMethod
        ORDER BY Rows DESC
    """).fetchall()
    print("Join coverage:")
    for method, rows, percent in summary:
        print(f"  {method}: {rows:,} ({percent:.3f}%)")
    print(f"Runtime written to {output}")
    con.close()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--defense-source", required=True)
    parser.add_argument("--college-source", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--buckets", type=int, default=64)
    parser.add_argument(
        "--fielding-team",
        action="append",
        default=[],
        help="Fielding-team code to include. Repeat for additional teams; omit for all teams.",
    )
    parser.add_argument("--refresh-college", action="store_true")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    defense_source = Path(args.defense_source)
    output = Path(args.output)
    work_dir = Path(args.work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)
    college_projection = work_dir / "college26_defense_context.parquet"
    if args.refresh_college or not college_projection.exists():
        print("Materializing College26 defensive context projection...")
        materialize_college_projection(args.college_source, college_projection)
    print("Building joined defensive runtime...")
    build_runtime(
        defense_source,
        college_projection,
        output,
        args.buckets,
        args.fielding_team,
    )


if __name__ == "__main__":
    main()
