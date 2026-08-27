#!/usr/bin/env python3
"""Regenerate Wally outputs from synthetic fixtures and compare with goldens."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[3]
SCRIPT_ROOT = PROJECT_ROOT / "scripts" / "baselines" / "wallyapps"
GOLDEN_ROOT = PROJECT_ROOT / "tests" / "baselines" / "wallyapps" / "golden"
APP_DIRECTORIES = {
    "HittingApp": "hitting",
    "PitchingApp": "pitching",
    "DefenseApp": "defense",
}
BROWSER_ONLY_FILES = {"app-shell.jpg"}
CACHE_ROOT = PROJECT_ROOT / ".baseline-runtime" / "cache"


def run(command: list[str]) -> None:
    CACHE_ROOT.mkdir(parents=True, exist_ok=True)
    environment = os.environ.copy()
    environment["XDG_CACHE_HOME"] = str(CACHE_ROOT)
    result = subprocess.run(
        command,
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        check=False,
        env=environment,
    )
    if result.returncode != 0:
        if result.stdout:
            print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="", file=sys.stderr)
        raise SystemExit(result.returncode)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def render_pdf(path: Path, output_stem: Path) -> Path:
    renderer = shutil.which("pdftoppm")
    if renderer is None:
        raise SystemExit("pdftoppm is required to validate golden PDF rendering")
    run([renderer, "-f", "1", "-singlefile", "-r", "96", "-png", str(path), str(output_stem)])
    rendered = output_stem.with_suffix(".png")
    if not rendered.exists():
        raise SystemExit(f"PDF renderer did not create {rendered}")
    return rendered


def generate(destination_root: Path) -> None:
    for app, directory in APP_DIRECTORIES.items():
        destination = destination_root / directory
        destination.mkdir(parents=True, exist_ok=True)
        run(["Rscript", str(SCRIPT_ROOT / "generate_goldens.R"), app, str(destination)])


def compare_directory(expected: Path, actual: Path, scratch: Path) -> list[str]:
    errors: list[str] = []
    expected_files = {
        path.name: path
        for path in expected.iterdir()
        if path.is_file() and path.name not in BROWSER_ONLY_FILES
    }
    actual_files = {path.name: path for path in actual.iterdir() if path.is_file()}
    if set(expected_files) != set(actual_files):
        missing = sorted(set(expected_files) - set(actual_files))
        extra = sorted(set(actual_files) - set(expected_files))
        if missing:
            errors.append(f"{expected.name}: regenerated files missing {missing}")
        if extra:
            errors.append(f"{expected.name}: regenerated files added {extra}")
        return errors

    for name, expected_path in sorted(expected_files.items()):
        actual_path = actual_files[name]
        if expected_path.suffix.lower() != ".pdf":
            if sha256(expected_path) != sha256(actual_path):
                errors.append(f"{expected.name}/{name}: content differs")
            continue

        expected_render = render_pdf(expected_path, scratch / f"expected-{expected.name}-{expected_path.stem}")
        actual_render = render_pdf(actual_path, scratch / f"actual-{expected.name}-{actual_path.stem}")
        if sha256(expected_render) != sha256(actual_render):
            errors.append(f"{expected.name}/{name}: rendered first page differs")
    return errors


def assert_known_observations() -> list[str]:
    errors: list[str] = []
    hitting = json.loads((GOLDEN_ROOT / "hitting" / "summary.json").read_text(encoding="utf-8"))
    pitching = json.loads((GOLDEN_ROOT / "pitching" / "summary.json").read_text(encoding="utf-8"))
    if hitting.get("observed_case_insensitive_duplicate_load") is not True:
        errors.append("Hitting duplicate-load observation unexpectedly changed")
    if pitching.get("observed_walk_encoding_mismatch") is not True:
        errors.append("Pitching walk-encoding observation unexpectedly changed")
    for directory in APP_DIRECTORIES.values():
        screenshot = GOLDEN_ROOT / directory / "app-shell.jpg"
        if not screenshot.exists():
            errors.append(f"{directory}/app-shell.jpg: live-browser reference is missing")
            continue
        raw = screenshot.read_bytes()
        if not raw.startswith(b"\xff\xd8\xff") or not raw.endswith(b"\xff\xd9") or len(raw) < 10_000:
            errors.append(f"{directory}/app-shell.jpg: not a valid browser JPEG")
    return errors


def update_goldens(generated_root: Path) -> None:
    for directory in APP_DIRECTORIES.values():
        source = generated_root / directory
        destination = GOLDEN_ROOT / directory
        destination.mkdir(parents=True, exist_ok=True)
        generated_names = {path.name for path in source.iterdir() if path.is_file()} | BROWSER_ONLY_FILES
        for existing in destination.iterdir():
            if existing.is_file() and existing.name not in generated_names:
                existing.unlink()
        for path in source.iterdir():
            if path.is_file():
                shutil.copy2(path, destination / path.name)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true", help="replace checked-in goldens")
    args = parser.parse_args()

    run([sys.executable, str(SCRIPT_ROOT / "validate_fixtures.py")])
    for app in APP_DIRECTORIES:
        run(["Rscript", str(SCRIPT_ROOT / "smoke_source.R"), app])

    with tempfile.TemporaryDirectory(prefix="wally-golden-check-") as temp_name:
        temp_root = Path(temp_name)
        generated_root = temp_root / "generated"
        generate(generated_root)
        if args.update:
            update_goldens(generated_root)
            print(f"Updated Wally goldens in {GOLDEN_ROOT}")
            return 0

        errors: list[str] = []
        for directory in APP_DIRECTORIES.values():
            errors.extend(
                compare_directory(GOLDEN_ROOT / directory, generated_root / directory, temp_root)
            )
        errors.extend(assert_known_observations())
        if errors:
            print("Wally golden parity failed:")
            for error in errors:
                print(f"- {error}")
            return 1

    print("Wally golden parity passed for HittingApp, PitchingApp, and DefenseApp.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
