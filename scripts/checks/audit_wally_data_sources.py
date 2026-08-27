#!/usr/bin/env python3
"""Aggregate-only audit of candidate Wally/BASE sources; never exports player rows."""

from __future__ import annotations

import argparse
import json
from collections import Counter
from pathlib import Path

import pandas as pd
import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.parquet as pq


PROJECT_ROOT = Path(__file__).resolve().parents[2]
WALLY_ROOT = PROJECT_ROOT / "WallyApps"
MASTER_PATH = WALLY_ROOT / "D1 Files" / "D1 Pitching:Hitting File.parquet"
CATCHING_PATH = WALLY_ROOT / "D1 Files" / "D1 Catching File.parquet"
DEFENSE_PATH = WALLY_ROOT / "D1 Files" / "D1 Defense File.parquet"
BASE_LOCAL_PATH = PROJECT_ROOT / "data" / "local" / "texas_state_2027.csv"
DEFAULT_OUTPUT = PROJECT_ROOT / "docs" / "wallyapps" / "source-audit.json"

TEAM_PATTERN = r"TEX_BOB|Texas State|Texas St\.|TXST"
CORE_BASE_COLUMNS = {
    "PitchUID", "GameID", "GameUID", "Date", "LocalDateTime", "UTCDateTime",
    "Pitcher", "PitcherId", "PitcherTeam", "PitcherThrows", "Batter", "BatterId",
    "BatterTeam", "BatterSide", "TaggedPitchType", "AutoPitchType", "PitchCall",
    "KorBB", "TaggedHitType", "PlayResult", "Notes", "Top/Bottom", "RelSpeed",
    "SpinRate", "SpinAxis", "RelHeight", "RelSide", "Extension", "InducedVertBreak",
    "HorzBreak", "PlateLocHeight", "PlateLocSide", "ExitSpeed", "Angle", "Direction",
    "Bearing", "Distance", "Inning", "PAofInning", "PitchofPA", "Balls", "Strikes",
    "OutsOnPlay", "RunsScored",
}
RICH_FEATURE_COLUMNS = {
    "Catcher", "CatcherId", "CatcherThrows", "CatcherTeam", "PlayID", "HangTime",
    "PositionAt110X", "PositionAt110Y", "PositionAt110Z", "PopTime", "ExchangeTime",
    "ThrowSpeed", "TimeToBase", "PitchReleaseConfidence", "PitchLocationConfidence",
    "PitchMovementConfidence", "HitLaunchConfidence", "HitLandingConfidence",
    "SpinAxis3dActiveSpinRate", "SpinAxis3dSpinEfficiency", "ModelPitchType",
    "ModelConfidence", "ModelPitchTypeRaw", "ModelConfidenceRaw", "ModelRetagReason",
    "ModelTop3", "VertRelAngle", "HorzRelAngle", "EffectiveVelo", "VertApprAngle",
    "HorzApprAngle", "ZoneSpeed", "ZoneTime", "AutoHitType", "HomeTeam", "AwayTeam",
    "Stadium", "Level", "League",
}
LOCAL_TEAM_FILES = {
    "hitting_2025_season": WALLY_ROOT / "HittingApp" / "data" / "2025 Season -cleaned.csv",
    "hitting_2025_fall": WALLY_ROOT / "HittingApp" / "data" / "2025 Fall -cleaned.csv",
    "hitting_2026_squads": WALLY_ROOT / "HittingApp" / "data" / "2026 Squads - cleaned.csv",
    "hitting_2026_season": WALLY_ROOT / "HittingApp" / "data" / "2026 Season - cleaned.csv",
    "pitching_2025_season": WALLY_ROOT / "PitchingApp" / "data" / "2025 Season -cleaned.csv",
    "pitching_2025_fall": WALLY_ROOT / "PitchingApp" / "data" / "2025 Fall -cleaned.csv",
    "pitching_2026_squads": WALLY_ROOT / "PitchingApp" / "data" / "2026 Squads - cleaned.csv",
    "pitching_2026_season": WALLY_ROOT / "PitchingApp" / "data" / "2026 Season - cleaned.csv",
    "pitching_bullpens": WALLY_ROOT / "PitchingApp" / "data" / "Bullpens - cleaned.csv",
}


def clean_string(value: object) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    return text or None


def load_uid_column(path: Path) -> pa.Array:
    column = pq.read_table(path, columns=["PitchUID"])["PitchUID"].combine_chunks()
    return pc.drop_null(column)


def overlap_summary(candidate: pa.Array, master: pa.Array) -> dict[str, int | float]:
    unique_candidate = pc.unique(candidate)
    matched = pc.sum(pc.cast(pc.is_in(unique_candidate, value_set=master), pa.int64())).as_py() or 0
    total = len(unique_candidate)
    return {
        "unique_pitch_uids": total,
        "pitch_uids_in_master": int(matched),
        "pitch_uids_not_in_master": int(total - matched),
        "master_overlap_pct": round(100 * matched / total, 3) if total else 0.0,
    }


def audit_master(path: Path) -> tuple[dict[str, object], pa.Array]:
    parquet = pq.ParquetFile(path)
    schema_names = parquet.schema.names
    selected = [
        "Date", "PitchUID", "GameUID", "PitcherId", "PitcherTeam", "BatterId",
        "BatterTeam", "query_mode", "d1_data_type", "query_start_date",
        "query_end_date", "query_league", "source_scrape_date",
    ]
    non_null = Counter()
    year_rows = Counter()
    year_date_ranges: dict[str, dict[str, str]] = {}
    team_pitch_rows = Counter()
    team_hit_rows = Counter()
    team_pitcher_ids: dict[str, set[str]] = {}
    team_batter_ids: dict[str, set[str]] = {}
    team_codes: set[str] = set()
    pitcher_team_codes: set[str] = set()
    batter_team_codes: set[str] = set()
    game_ids: set[str] = set()
    metadata_values: dict[str, set[str]] = {
        name: set() for name in selected if name.startswith("query_") or name in {"d1_data_type", "source_scrape_date"}
    }
    min_date: str | None = None
    max_date: str | None = None
    team_min_date: str | None = None
    team_max_date: str | None = None

    for batch in parquet.iter_batches(columns=selected, batch_size=131_072):
        frame = batch.to_pandas()
        for column in selected:
            non_null[column] += int(frame[column].notna().sum())
        dates = frame["Date"].astype("string")
        valid_dates = dates.dropna()
        if len(valid_dates):
            batch_min = str(valid_dates.min())
            batch_max = str(valid_dates.max())
            min_date = batch_min if min_date is None else min(min_date, batch_min)
            max_date = batch_max if max_date is None else max(max_date, batch_max)
        years = dates.str.slice(0, 4)
        year_rows.update({str(year): int(count) for year, count in years.value_counts().items()})
        for year in years.dropna().unique():
            year_dates = dates[years.eq(year).fillna(False)].dropna()
            if not len(year_dates):
                continue
            year_text = str(year)
            batch_min = str(year_dates.min())
            batch_max = str(year_dates.max())
            current = year_date_ranges.get(year_text)
            year_date_ranges[year_text] = {
                "date_min": batch_min if current is None else min(current["date_min"], batch_min),
                "date_max": batch_max if current is None else max(current["date_max"], batch_max),
            }

        for field in metadata_values:
            metadata_values[field].update(
                value for value in (clean_string(item) for item in frame[field].unique()) if value is not None
            )
        game_ids.update(
            value for value in (clean_string(item) for item in frame["GameUID"].unique()) if value is not None
        )
        pitcher_team_codes.update(
            value for value in (clean_string(item) for item in frame["PitcherTeam"].unique()) if value is not None
        )
        batter_team_codes.update(
            value for value in (clean_string(item) for item in frame["BatterTeam"].unique()) if value is not None
        )

        pitcher_match = frame["PitcherTeam"].astype("string").str.contains(
            TEAM_PATTERN, case=False, regex=True, na=False
        ).fillna(False)
        batter_match = frame["BatterTeam"].astype("string").str.contains(
            TEAM_PATTERN, case=False, regex=True, na=False
        ).fillna(False)
        team_mask = pitcher_match | batter_match
        team_dates = dates[team_mask].dropna()
        if len(team_dates):
            batch_min = str(team_dates.min())
            batch_max = str(team_dates.max())
            team_min_date = batch_min if team_min_date is None else min(team_min_date, batch_min)
            team_max_date = batch_max if team_max_date is None else max(team_max_date, batch_max)

        for year, count in years[pitcher_match].value_counts().items():
            team_pitch_rows[str(year)] += int(count)
        for year, count in years[batter_match].value_counts().items():
            team_hit_rows[str(year)] += int(count)
        for year in years[pitcher_match].dropna().unique():
            mask = (pitcher_match & years.eq(year).fillna(False)).fillna(False)
            ids = {clean_string(item) for item in frame.loc[mask, "PitcherId"].unique()}
            team_pitcher_ids.setdefault(str(year), set()).update(item for item in ids if item is not None)
        for year in years[batter_match].dropna().unique():
            mask = (batter_match & years.eq(year).fillna(False)).fillna(False)
            ids = {clean_string(item) for item in frame.loc[mask, "BatterId"].unique()}
            team_batter_ids.setdefault(str(year), set()).update(item for item in ids if item is not None)
        for column, mask in (("PitcherTeam", pitcher_match), ("BatterTeam", batter_match)):
            team_codes.update(
                value for value in (clean_string(item) for item in frame.loc[mask, column].unique()) if value is not None
            )

    master_uids = load_uid_column(path)
    distinct_pitch_uids = pc.count_distinct(master_uids).as_py()
    result = {
        "path": str(path.relative_to(PROJECT_ROOT)),
        "format": "parquet",
        "size_bytes": path.stat().st_size,
        "rows": parquet.metadata.num_rows,
        "row_groups": parquet.metadata.num_row_groups,
        "columns": len(schema_names),
        "date_min": min_date,
        "date_max": max_date,
        "rows_by_year": dict(sorted(year_rows.items())),
        "date_ranges_by_year": dict(sorted(year_date_ranges.items())),
        "unique_pitch_uids": int(distinct_pitch_uids),
        "duplicate_pitch_uid_rows": int(len(master_uids) - distinct_pitch_uids),
        "unique_games": len(game_ids),
        "national_team_coverage": {
            "pitcher_team_codes": len(pitcher_team_codes),
            "batter_team_codes": len(batter_team_codes),
            "team_codes_union": len(pitcher_team_codes | batter_team_codes),
        },
        "metadata_values": {key: sorted(values) for key, values in metadata_values.items()},
        "nonnull_coverage": {
            key: {
                "rows": int(value),
                "pct": round(100 * value / parquet.metadata.num_rows, 3),
            }
            for key, value in sorted(non_null.items())
        },
        "base_core_schema": {
            "required_columns": len(CORE_BASE_COLUMNS),
            "present": sorted(CORE_BASE_COLUMNS & set(schema_names)),
            "missing": sorted(CORE_BASE_COLUMNS - set(schema_names)),
        },
        "additional_feature_columns": sorted(RICH_FEATURE_COLUMNS & set(schema_names)),
        "texas_state_coverage": {
            "matching_team_codes": sorted(team_codes),
            "date_min": team_min_date,
            "date_max": team_max_date,
            "pitching_rows_by_year": dict(sorted(team_pitch_rows.items())),
            "hitting_rows_by_year": dict(sorted(team_hit_rows.items())),
            "pitchers_by_year": {year: len(ids) for year, ids in sorted(team_pitcher_ids.items())},
            "hitters_by_year": {year: len(ids) for year, ids in sorted(team_batter_ids.items())},
        },
    }
    return result, master_uids


def audit_local_files(master_uids: pa.Array) -> tuple[dict[str, object], dict[str, set[str]]]:
    results: dict[str, object] = {}
    uid_sets: dict[str, set[str]] = {}
    for label, path in LOCAL_TEAM_FILES.items():
        header = pd.read_csv(path, nrows=0)
        use_columns = [name for name in ("PitchUID", "Date") if name in header.columns]
        frame = pd.read_csv(path, usecols=use_columns, dtype="string", low_memory=False)
        uids = {value for value in frame.get("PitchUID", pd.Series(dtype="string")).dropna().astype(str) if value.strip()}
        uid_sets[label] = uids
        raw_dates = frame.get("Date", pd.Series(dtype="string")).dropna().astype(str)
        dates = pd.to_datetime(raw_dates, errors="coerce").dropna()
        summary = overlap_summary(pa.array(sorted(uids), type=pa.string()), master_uids)
        pitch_uid_rows = int(frame.get("PitchUID", pd.Series(dtype="string")).notna().sum())
        results[label] = {
            "path": str(path.relative_to(PROJECT_ROOT)),
            "size_bytes": path.stat().st_size,
            "rows": len(frame),
            "columns": len(header.columns),
            "date_min": dates.min().date().isoformat() if len(dates) else None,
            "date_max": dates.max().date().isoformat() if len(dates) else None,
            "missing_pitch_uid_rows": int(len(frame) - pitch_uid_rows),
            "duplicate_pitch_uid_rows": int(pitch_uid_rows - len(uids)),
            **summary,
        }
    return results, uid_sets


def pairwise_local_overlap(uid_sets: dict[str, set[str]]) -> dict[str, object]:
    pairs = {
        "2025_season": ("hitting_2025_season", "pitching_2025_season"),
        "2025_fall": ("hitting_2025_fall", "pitching_2025_fall"),
        "2026_squads": ("hitting_2026_squads", "pitching_2026_squads"),
        "2026_season": ("hitting_2026_season", "pitching_2026_season"),
    }
    results: dict[str, object] = {}
    for label, (hitting, pitching) in pairs.items():
        left = uid_sets[hitting]
        right = uid_sets[pitching]
        union = left | right
        overlap = left & right
        results[label] = {
            "hitting_unique_pitch_uids": len(left),
            "pitching_unique_pitch_uids": len(right),
            "shared_pitch_uids": len(overlap),
            "union_pitch_uids": len(union),
            "duplicate_storage_pct_of_union": round(100 * len(overlap) / len(union), 3) if union else 0.0,
            "hitting_only_pitch_uids": len(left - right),
            "pitching_only_pitch_uids": len(right - left),
        }
    return results


def audit_base_local() -> dict[str, object]:
    header = pd.read_csv(BASE_LOCAL_PATH, nrows=0)
    rows = sum(1 for _ in BASE_LOCAL_PATH.open("r", encoding="utf-8-sig")) - 1
    return {
        "path": str(BASE_LOCAL_PATH.relative_to(PROJECT_ROOT)),
        "rows": max(rows, 0),
        "columns": len(header.columns),
        "note": "Local placeholder only; deployed BASE is configured to use mounted College26/derived Parquet data.",
    }


def build_audit() -> dict[str, object]:
    master, master_uids = audit_master(MASTER_PATH)
    local_files, uid_sets = audit_local_files(master_uids)
    catching_uids = load_uid_column(CATCHING_PATH)
    catching_overlap = overlap_summary(catching_uids, master_uids)
    del catching_uids
    defense_uids = load_uid_column(DEFENSE_PATH)
    defense_overlap = overlap_summary(defense_uids, master_uids)
    del defense_uids

    union_local = set().union(*uid_sets.values())
    union_summary = overlap_summary(pa.array(sorted(union_local), type=pa.string()), master_uids)
    return {
        "schema_version": 1,
        "privacy": "Aggregate counts and schemas only; no player names or pitch rows are exported.",
        "candidate_master": master,
        "local_team_exports": local_files,
        "local_team_export_union": union_summary,
        "hitting_vs_pitching_duplicates": pairwise_local_overlap(uid_sets),
        "national_dataset_overlap": {
            "catching_vs_pitching_hitting": catching_overlap,
            "defense_vs_pitching_hitting": defense_overlap,
        },
        "current_base_local": audit_base_local(),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    audit = build_audit()
    output = args.output.resolve()
    if args.check:
        if not output.exists() or json.loads(output.read_text(encoding="utf-8")) != audit:
            print("Wally source audit differs from the checked-in aggregate report.")
            return 1
        print("Wally source audit matches the checked-in aggregate report.")
        return 0
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(audit, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"Wrote aggregate Wally source audit to {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
