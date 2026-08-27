#!/usr/bin/env python3
"""Inventory imported Wally data/model artifacts without loading their contents.

The manifest records file hashes and structural metadata. Pickle and R session
files are treated as opaque bytes and are never deserialized by this script.
Browser profiles, rsconnect metadata, and browser debug captures are excluded
entirely because they may contain account or session information.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from pathlib import Path
from typing import Any

import pyarrow.parquet as pq


PROJECT_ROOT = Path(__file__).resolve().parents[2]
WALLY_ROOT = PROJECT_ROOT / "WallyApps"
DEFAULT_OUTPUT = PROJECT_ROOT / "docs" / "wallyapps" / "artifact-manifest.json"

ARTIFACT_GLOBS = (
    "D1 Files/*.parquet",
    "HittingApp/data/*.csv",
    "PitchingApp/data/*.csv",
    "DefenseApp/data/*.csv",
    "ScoutingApp/data/*.csv",
    "JucoStatsApp/data/juco_player_stats_latest.csv",
    "PitchingApp/models/*.pkl",
    "HittingApp/.RData",
    "PitchingApp/.RData",
)

NEVER_INSPECT_PATTERNS = (
    "WallyApps/**/.browser-profile*/",
    "WallyApps/**/rsconnect/",
    "WallyApps/JucoStatsApp/data/debug_njcaa/",
    "WallyApps/JucoStatsApp/data/raw_njcaa/",
    "WallyApps/ScoutingApp/.Renviron",
)

DERIVED_REFERENCE_NAMES = {
    "called_stuff_baseline.csv",
    "d1_pitch_metric_percentile_reference.csv",
    "d1_catcher_framing_metrics.csv",
}


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 * 1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def csv_metadata(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8-sig", errors="replace", newline="") as handle:
        reader = csv.reader(handle)
        columns = next(reader, [])
        rows = sum(1 for _ in reader)

    return {
        "rows": rows,
        "columns": columns,
        "column_count": len(columns),
    }


def parquet_metadata(path: Path) -> dict[str, Any]:
    parquet = pq.ParquetFile(path)
    fields = [
        {
            "name": field.name,
            "type": str(field.type),
            "nullable": field.nullable,
        }
        for field in parquet.schema_arrow
    ]
    schema_json = json.dumps(fields, sort_keys=True, separators=(",", ":"))

    return {
        "rows": parquet.metadata.num_rows,
        "row_groups": parquet.metadata.num_row_groups,
        "column_count": parquet.metadata.num_columns,
        "created_by": parquet.metadata.created_by,
        "schema_sha256": hashlib.sha256(schema_json.encode("utf-8")).hexdigest(),
        "schema": fields,
    }


def artifact_policy(relative_path: Path) -> str:
    relative_text = relative_path.as_posix()
    if relative_text.startswith("WallyApps/D1 Files/"):
        return "private-data-bucket"
    if relative_text.startswith("WallyApps/PitchingApp/models/"):
        return "private-model-store-quarantine"
    if relative_path.suffix == ".RData":
        return "do-not-version-r-session-state"
    if relative_path.name in DERIVED_REFERENCE_NAMES:
        return "derived-reference-review-before-versioning"
    if relative_text.endswith("JucoStatsApp/data/juco_player_stats_latest.csv"):
        return "generated-scouting-data"
    return "private-team-data"


def artifact_kind(path: Path) -> str:
    suffix = path.suffix.lower()
    if suffix == ".parquet":
        return "parquet-dataset"
    if suffix == ".csv":
        return "csv-dataset"
    if suffix == ".pkl":
        return "opaque-pickle-model"
    if suffix == ".rdata":
        return "opaque-r-session-state"
    return "opaque-artifact"


def collect_paths() -> list[Path]:
    if not WALLY_ROOT.is_dir():
        raise SystemExit(f"WallyApps working copy is missing: {WALLY_ROOT}")

    paths: set[Path] = set()
    for pattern in ARTIFACT_GLOBS:
        paths.update(path for path in WALLY_ROOT.glob(pattern) if path.is_file())
    ordered = sorted(paths, key=lambda path: path.relative_to(PROJECT_ROOT).as_posix())
    if not ordered:
        raise SystemExit(f"No Wally artifacts matched the configured inventory: {WALLY_ROOT}")
    return ordered


def build_manifest() -> dict[str, Any]:
    artifacts: list[dict[str, Any]] = []
    for path in collect_paths():
        relative_path = path.relative_to(PROJECT_ROOT)
        record: dict[str, Any] = {
            "path": relative_path.as_posix(),
            "kind": artifact_kind(path),
            "storage_policy": artifact_policy(relative_path),
            "size_bytes": path.stat().st_size,
            "sha256": sha256_file(path),
        }

        if path.suffix.lower() == ".parquet":
            record.update(parquet_metadata(path))
        elif path.suffix.lower() == ".csv":
            record.update(csv_metadata(path))
        elif path.suffix.lower() in {".pkl", ".rdata"}:
            record["deserialized"] = False

        artifacts.append(record)

    return {
        "manifest_version": 1,
        "source_root": "WallyApps",
        "artifact_count": len(artifacts),
        "total_size_bytes": sum(item["size_bytes"] for item in artifacts),
        "security": {
            "pickle_and_rdata_deserialized": False,
            "never_inspected_patterns": list(NEVER_INSPECT_PATTERNS),
        },
        "artifacts": artifacts,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Fail if the existing manifest differs from the current artifacts.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    output = args.output if args.output.is_absolute() else PROJECT_ROOT / args.output
    manifest = build_manifest()
    manifest_text = json.dumps(manifest, indent=2, sort_keys=True) + "\n"

    if args.check:
        if not output.exists():
            raise SystemExit(f"Manifest is missing: {output}")
        if output.read_text(encoding="utf-8") != manifest_text:
            raise SystemExit(
                "Wally artifact manifest is stale. Regenerate it with "
                "python3 scripts/checks/build_wallyapps_artifact_manifest.py"
            )
        print(f"Wally artifact manifest is current: {output}")
        return

    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_suffix(output.suffix + ".tmp")
    temporary.write_text(manifest_text, encoding="utf-8")
    temporary.replace(output)
    print(f"Wrote {len(manifest['artifacts'])} artifact records to {output}")


if __name__ == "__main__":
    main()
