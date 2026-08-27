#!/usr/bin/env python3
"""Capture or verify the local runtime used to produce Wally golden outputs."""

from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import importlib.util
import json
import platform
import shutil
import subprocess
import sys
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_OUTPUT = PROJECT_ROOT / "tests" / "baselines" / "wallyapps" / "runtime-manifest.json"
R_PACKAGES = (
    "bslib", "cowplot", "curl", "data.table", "dplyr", "DT", "ggplot2", "ggplotify",
    "ggpubr", "ggtext", "glue", "gridExtra", "hms", "htmltools", "jpeg",
    "jsonlite", "lubridate", "magrittr", "patchwork", "plotly", "png", "purrr",
    "ragg", "readr", "readxl", "rlang", "scales", "shiny", "shinycssloaders",
    "shinyWidgets", "stringr", "tidyr", "tidyverse",
)
PYTHON_PACKAGES = ("numpy", "pandas", "pyarrow", "catboost", "scikit-learn", "playwright")
APP_SOURCES = {
    "HittingApp": PROJECT_ROOT / "WallyApps" / "HittingApp" / "HittingApp.R",
    "PitchingApp": PROJECT_ROOT / "WallyApps" / "PitchingApp" / "PitchingApp.R",
    "DefenseApp": PROJECT_ROOT / "WallyApps" / "DefenseApp" / "DefenseApp.R",
    "ScoutingApp": PROJECT_ROOT / "WallyApps" / "ScoutingApp" / "ScoutingApp.R",
}


def command_version(command: list[str]) -> str | None:
    if shutil.which(command[0]) is None:
        return None
    result = subprocess.run(command, text=True, capture_output=True, check=False)
    text = (result.stdout or result.stderr).strip().splitlines()
    return text[0] if result.returncode == 0 and text else None


def r_runtime() -> dict[str, object]:
    r_library = PROJECT_ROOT / ".baseline-runtime" / "R"
    quoted_packages = ",".join(json.dumps(package) for package in R_PACKAGES)
    code = f"""
    .libPaths(c({json.dumps(str(r_library))}, .libPaths()))
    packages <- c({quoted_packages})
    versions <- vapply(packages, function(x) if (requireNamespace(x, quietly=TRUE)) as.character(packageVersion(x)) else NA_character_, character(1))
    info <- list(version=R.version.string, platform=R.version$platform, packages=as.list(versions))
    cat(jsonlite::toJSON(info, auto_unbox=TRUE, na='null'))
    """
    result = subprocess.run(
        ["Rscript", "--vanilla", "-e", code],
        cwd=PROJECT_ROOT,
        text=True,
        capture_output=True,
        check=True,
    )
    return json.loads(result.stdout)


def python_packages() -> dict[str, str | None]:
    versions: dict[str, str | None] = {}
    for distribution in PYTHON_PACKAGES:
        module = "sklearn" if distribution == "scikit-learn" else distribution
        if importlib.util.find_spec(module) is None:
            versions[distribution] = None
            continue
        try:
            versions[distribution] = importlib.metadata.version(distribution)
        except importlib.metadata.PackageNotFoundError:
            versions[distribution] = "present-version-unknown"
    return versions


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def build_manifest() -> dict[str, object]:
    return {
        "schema_version": 1,
        "purpose": "Local, baseline-only runtime for reproducing untouched WallyApps outputs",
        "operating_system": {
            "system": platform.system(),
            "release": platform.release(),
            "machine": platform.machine(),
        },
        "r": r_runtime(),
        "python": {
            "version": platform.python_version(),
            "implementation": platform.python_implementation(),
            "packages": python_packages(),
        },
        "node": {
            "version": command_version(["node", "--version"]),
            "playwright_installed": (
                subprocess.run(
                    ["node", "-e", "require.resolve('playwright')"],
                    cwd=PROJECT_ROOT,
                    capture_output=True,
                    check=False,
                ).returncode == 0
                if shutil.which("node")
                else False
            ),
        },
        "docker": {"version": command_version(["docker", "--version"])},
        "application_sources": {
            name: {"path": str(path.relative_to(PROJECT_ROOT)), "sha256": sha256(path)}
            for name, path in APP_SOURCES.items()
        },
        "notes": [
            "R packages live in the ignored .baseline-runtime/R directory.",
            "The standalone Shiny apps consume synthetic CSV fixtures and precomputed model/reference CSVs.",
            "CatBoost, scikit-learn, and Playwright are not required to calculate the checked-in standalone goldens.",
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    current = build_manifest()
    output = args.output.resolve()

    if args.check:
        if not output.exists():
            print(f"Runtime manifest is missing: {output}")
            return 1
        expected = json.loads(output.read_text(encoding="utf-8"))
        if current != expected:
            print("Wally baseline runtime differs from runtime-manifest.json.")
            return 1
        print("Wally baseline runtime matches runtime-manifest.json.")
        return 0

    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(current, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"Wrote Wally baseline runtime manifest to {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
