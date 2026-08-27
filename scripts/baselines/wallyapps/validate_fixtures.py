#!/usr/bin/env python3
"""Fail if Wally baseline fixtures are incomplete or look non-synthetic."""

from __future__ import annotations

import argparse
import csv
import json
import re
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[3]
FIXTURE_ROOT = PROJECT_ROOT / "tests" / "fixtures" / "wallyapps"

FORBIDDEN_CELL_PATTERNS = {
    "local filesystem path": re.compile(r"(?i)(?:/users/|/home/|[a-z]:\\users\\)"),
    "email address": re.compile(r"(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b"),
    "Texas State name": re.compile(r"(?i)\btexas\s+state\b"),
    "Bobcats name": re.compile(r"(?i)\bbobcats?\b"),
    "known imported-owner name": re.compile(r"(?i)\b(?:austin\s+wallace|jamie\s+jones)\b"),
}

IDENTITY_COLUMN = re.compile(r"(?i)(?:^|_)(?:pitcher|batter|catcher|player|fielder|[123]b|ss|lf|cf|rf)(?:_name)?$")
NON_NAME_IDENTITY_COLUMN = re.compile(r"(?i)(?:id|team|throws?|hand|side|set|height|position|metric|type|rank|count)")
SYNTHETIC_NAME = re.compile(r"^(?:D1\s+)?Fixture\b")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=FIXTURE_ROOT)
    return parser.parse_args()


def validate_csv(path: Path) -> tuple[int, list[str]]:
    errors: list[str] = []
    with path.open("r", encoding="utf-8-sig", errors="strict", newline="") as handle:
        reader = csv.DictReader(handle)
        if not reader.fieldnames:
            return 0, [f"{path}: missing CSV header"]
        identity_columns = [
            column
            for column in reader.fieldnames
            if IDENTITY_COLUMN.search(column) and not NON_NAME_IDENTITY_COLUMN.search(column)
        ]
        row_count = 0
        for row_number, row in enumerate(reader, start=2):
            row_count += 1
            for column, raw_value in row.items():
                value = raw_value or ""
                for label, pattern in FORBIDDEN_CELL_PATTERNS.items():
                    if pattern.search(value):
                        errors.append(f"{path}:{row_number}:{column}: contains {label}")
                if column in identity_columns and value.strip() and not SYNTHETIC_NAME.match(value.strip()):
                    errors.append(
                        f"{path}:{row_number}:{column}: identity does not start with Fixture/D1 Fixture"
                    )
    if row_count == 0:
        errors.append(f"{path}: contains no data rows")
    return row_count, errors


def main() -> int:
    args = parse_args()
    root = args.root.resolve()
    manifest_path = root / "fixture-manifest.json"
    if not manifest_path.exists():
        raise SystemExit(f"Missing fixture manifest: {manifest_path}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    errors: list[str] = []
    if manifest.get("synthetic_only") is not True:
        errors.append("fixture-manifest.json must declare synthetic_only=true")

    csv_paths = sorted(root.rglob("*.csv"))
    if not csv_paths:
        errors.append("No fixture CSV files were found")

    row_counts: dict[Path, int] = {}
    for path in csv_paths:
        count, csv_errors = validate_csv(path)
        row_counts[path] = count
        errors.extend(csv_errors)

    sections = {
        "hitting": root / "hitting" / "data",
        "pitching": root / "pitching" / "data",
        "defense": root / "defense" / "data",
        "edge_cases": root / "edge_cases",
    }
    for section, expected_files in manifest.items():
        if section == "synthetic_only":
            continue
        section_root = sections.get(section)
        if section_root is None or not isinstance(expected_files, dict):
            errors.append(f"Unexpected fixture manifest section: {section}")
            continue
        for filename, expected_rows in expected_files.items():
            path = section_root / filename
            if not path.exists():
                errors.append(f"Manifest fixture is missing: {path}")
            elif row_counts.get(path) != expected_rows:
                errors.append(
                    f"{path}: expected {expected_rows} rows, found {row_counts.get(path)}"
                )

    if errors:
        print("Wally fixture privacy validation failed:")
        for error in errors:
            print(f"- {error}")
        return 1

    print(
        f"Wally fixtures passed synthetic/privacy checks: {len(csv_paths)} CSVs, "
        f"{sum(row_counts.values()):,} rows."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
