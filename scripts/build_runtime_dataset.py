#!/usr/bin/env python3
"""Build BASE's query-on-demand 2026 runtime data without altering the master.

The output keeps every pitch row needed by current features, partitions pitcher
data into stable hash buckets, writes a compact player catalog, and writes the
configured team's much smaller season file for startup-time reports.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import defaultdict
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq


def stable_bucket(team: object, player: object, buckets: int) -> int:
    key = f"{'' if team is None else team}\x1f{'' if player is None else player}"
    return int.from_bytes(hashlib.sha256(key.encode("utf-8")).digest()[:4], "big") % buckets


def valid_text(value: object) -> bool:
    return value is not None and str(value).strip() != ""


def build(args: argparse.Namespace) -> None:
    source = Path(args.source).resolve()
    output = Path(args.output).resolve()
    pitcher_root = output / "pitchers"
    team_root = output / "teams"
    catalog_root = output / "catalogs"
    for directory in (pitcher_root, team_root, catalog_root):
        directory.mkdir(parents=True, exist_ok=True)

    parquet = pq.ParquetFile(source)
    source_schema = parquet.schema_arrow
    required = {"Pitcher", "PitcherTeam"}
    missing = sorted(required.difference(source_schema.names))
    if missing:
        raise ValueError(f"Source is missing required columns: {', '.join(missing)}")

    team_pattern = re.compile(args.team_pattern, flags=re.IGNORECASE)
    catalog_counts: dict[tuple[str, str, int], int] = defaultdict(int)
    hitter_catalog_counts: dict[tuple[str, str, str, int], int] = defaultdict(int)
    bucket_writers: dict[int, pq.ParquetWriter] = {}
    bucket_buffers: dict[int, list[pa.Table]] = defaultdict(list)
    bucket_buffer_rows: dict[int, int] = defaultdict(int)
    team_writer: pq.ParquetWriter | None = None
    team_path = team_root / f"{args.team_code}.parquet"
    total_rows = 0
    pitcher_rows = 0
    team_rows = 0

    def flush_bucket(bucket: int) -> None:
        nonlocal pitcher_rows
        if not bucket_buffers[bucket]:
            return
        pitcher_table = pa.concat_tables(bucket_buffers[bucket])
        if bucket not in bucket_writers:
            bucket_dir = pitcher_root / f"Bucket={bucket}"
            bucket_dir.mkdir(parents=True, exist_ok=True)
            bucket_writers[bucket] = pq.ParquetWriter(
                bucket_dir / "part-00000.parquet",
                pitcher_table.schema,
                compression="zstd",
                compression_level=6,
                use_dictionary=True,
            )
        bucket_writers[bucket].write_table(
            pitcher_table,
            row_group_size=args.bucket_buffer_rows,
        )
        pitcher_rows += pitcher_table.num_rows
        bucket_buffers[bucket].clear()
        bucket_buffer_rows[bucket] = 0

    for batch_index, batch in enumerate(parquet.iter_batches(batch_size=args.batch_size)):
        table = pa.Table.from_batches([batch])
        total_rows += table.num_rows

        pitcher_names = table["Pitcher"].to_pylist()
        pitcher_teams = table["PitcherTeam"].to_pylist()
        batter_names = table["Batter"].to_pylist() if "Batter" in table.column_names else [None] * table.num_rows
        batter_teams = table["BatterTeam"].to_pylist() if "BatterTeam" in table.column_names else [None] * table.num_rows
        batter_sides = table["BatterSide"].to_pylist() if "BatterSide" in table.column_names else [None] * table.num_rows
        bucket_indices: dict[int, list[int]] = defaultdict(list)
        for row_index, (team, player, batter_team, batter, batter_side) in enumerate(
            zip(pitcher_teams, pitcher_names, batter_teams, batter_names, batter_sides)
        ):
            if not valid_text(team) or not valid_text(player):
                continue
            team_text = str(team)
            player_text = str(player)
            bucket = stable_bucket(team_text, player_text, args.buckets)
            bucket_indices[bucket].append(row_index)
            catalog_counts[(team_text, player_text, bucket)] += 1
            if valid_text(batter_team) and valid_text(batter):
                hitter_catalog_counts[
                    (str(batter_team), str(batter), "" if batter_side is None else str(batter_side), bucket)
                ] += 1

        for bucket, indices in bucket_indices.items():
            pitcher_table = table.take(pa.array(indices, type=pa.int64()))
            bucket_buffers[bucket].append(pitcher_table)
            bucket_buffer_rows[bucket] += pitcher_table.num_rows
            if bucket_buffer_rows[bucket] >= args.bucket_buffer_rows:
                flush_bucket(bucket)

        team_mask_values = []
        relevant_team_columns = [
            name for name in ("PitcherTeam", "BatterTeam", "CatcherTeam")
            if name in table.column_names
        ]
        team_columns = [table[name].to_pylist() for name in relevant_team_columns]
        for values in zip(*team_columns):
            team_mask_values.append(any(valid_text(v) and team_pattern.search(str(v)) for v in values))
        if any(team_mask_values):
            team_table = table.filter(pa.array(team_mask_values, type=pa.bool_()))
            if team_writer is None:
                team_writer = pq.ParquetWriter(
                    team_path,
                    team_table.schema,
                    compression="zstd",
                    compression_level=6,
                    use_dictionary=True,
                )
            team_writer.write_table(team_table, row_group_size=args.batch_size)
            team_rows += team_table.num_rows

    if team_writer is not None:
        team_writer.close()
    for bucket in list(bucket_buffers):
        flush_bucket(bucket)
    for writer in bucket_writers.values():
        writer.close()

    catalog_records = [
        {
            "PitcherTeam": team,
            "Pitcher": player,
            "Bucket": bucket,
            "PitchCount": count,
        }
        for (team, player, bucket), count in sorted(catalog_counts.items())
    ]
    catalog = pa.Table.from_pylist(
        catalog_records,
        schema=pa.schema([
            ("PitcherTeam", pa.string()),
            ("Pitcher", pa.string()),
            ("Bucket", pa.int16()),
            ("PitchCount", pa.int64()),
        ]),
    )
    pq.write_table(
        catalog,
        catalog_root / "pitchers.parquet",
        compression="zstd",
        compression_level=6,
        use_dictionary=True,
    )

    hitter_rollup: dict[tuple[str, str], dict[str, object]] = {}
    for (team, player, side, bucket), count in hitter_catalog_counts.items():
        entry = hitter_rollup.setdefault(
            (team, player),
            {"buckets": set(), "sides": defaultdict(int), "count": 0},
        )
        entry["buckets"].add(bucket)
        entry["sides"][side] += count
        entry["count"] += count

    hitter_records = []
    for (team, player), entry in sorted(hitter_rollup.items()):
        side_counts = entry["sides"]
        side = sorted(side_counts, key=lambda value: (-side_counts[value], value))[0] if side_counts else ""
        hitter_records.append({
            "BatterTeam": team,
            "Batter": player,
            "BatterSide": side,
            "Buckets": ",".join(str(value) for value in sorted(entry["buckets"])),
            "PitchCount": entry["count"],
        })

    hitter_catalog = pa.Table.from_pylist(
        hitter_records,
        schema=pa.schema([
            ("BatterTeam", pa.string()),
            ("Batter", pa.string()),
            ("BatterSide", pa.string()),
            ("Buckets", pa.string()),
            ("PitchCount", pa.int64()),
        ]),
    )
    pq.write_table(
        hitter_catalog,
        catalog_root / "hitters.parquet",
        compression="zstd",
        compression_level=6,
        use_dictionary=True,
    )

    metadata = {
        "source": source.name,
        "source_rows": total_rows,
        "source_columns": len(source_schema.names),
        "pitcher_rows": pitcher_rows,
        "pitcher_catalog_rows": len(catalog_records),
        "hitter_catalog_rows": len(hitter_records),
        "pitcher_buckets": args.buckets,
        "team_code": args.team_code,
        "team_pattern": args.team_pattern,
        "team_rows": team_rows,
    }
    (output / "metadata.json").write_text(json.dumps(metadata, indent=2), encoding="utf-8")
    print(json.dumps(metadata, indent=2))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", required=True, help="Runtime parquet containing all 2026 pitch rows")
    parser.add_argument("--output", required=True, help="Destination directory")
    parser.add_argument("--buckets", type=int, default=64)
    parser.add_argument("--batch-size", type=int, default=100_000)
    parser.add_argument("--bucket-buffer-rows", type=int, default=10_000)
    parser.add_argument("--team-code", default="TEX_BOB")
    parser.add_argument(
        "--team-pattern",
        default=r"TEX_BOB|Texas State|Texas St\.|TXST",
    )
    return parser.parse_args()


if __name__ == "__main__":
    build(parse_args())
