#!/usr/bin/env python3
"""Validate the executable Step 4 Wally-to-BASE feature map."""

from __future__ import annotations

import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
FEATURE_PATH = ROOT / "tests" / "baselines" / "wallyapps" / "feature-register.json"
METRIC_PATH = ROOT / "tests" / "baselines" / "wallyapps" / "metric-register.json"
SOURCE_PATH = ROOT / "config" / "baseball_data_sources.json"
WORKSPACE_PATH = ROOT / "config" / "app_workspaces.json"


def fail(message: str) -> None:
    raise SystemExit(message)


def main() -> int:
    feature_register = json.loads(FEATURE_PATH.read_text(encoding="utf-8"))
    metric_register = json.loads(METRIC_PATH.read_text(encoding="utf-8"))
    source_contract = json.loads(SOURCE_PATH.read_text(encoding="utf-8"))
    workspace_contract = json.loads(WORKSPACE_PATH.read_text(encoding="utf-8"))

    features = feature_register.get("features", [])
    if not features:
        fail("Feature register has no features.")

    ids = [item.get("id") for item in features]
    if len(ids) != len(set(ids)):
        fail("Feature register contains duplicate feature IDs.")

    allowed_apps = {"HittingApp", "PitchingApp", "DefenseApp"}
    allowed_strategies = set(feature_register.get("strategies", []))
    known_metrics = {item["id"] for item in metric_register.get("decisions", [])}
    known_routes = set(source_contract.get("feature_routes", {}))
    known_workspaces = {item["id"] for item in workspace_contract.get("workspaces", [])}
    workspace_defaults = feature_register.get("app_workspace_defaults", {})
    if set(workspace_defaults) != allowed_apps:
        fail("Every Wally app must have exactly one default BASE workspace.")
    if not set(workspace_defaults.values()).issubset(known_workspaces):
        fail("Wally app defaults reference an unknown BASE workspace.")

    for item in features:
        feature_id = item.get("id", "<missing>")
        if item.get("app") not in allowed_apps:
            fail(f"{feature_id} has an unknown app.")
        if item.get("strategy") not in allowed_strategies:
            fail(f"{feature_id} has an unknown strategy.")
        if not item.get("target"):
            fail(f"{feature_id} has no BASE target.")
        if item.get("source_route") not in known_routes:
            fail(f"{feature_id} references an unknown source route.")
        unknown_metrics = set(item.get("metric_dependencies", [])) - known_metrics
        if unknown_metrics:
            fail(f"{feature_id} references unknown metrics: {sorted(unknown_metrics)}")
        if item.get("priority") not in {1, 2, 3, 4}:
            fail(f"{feature_id} has an invalid priority.")

    first = feature_register.get("first_vertical_slice", {})
    first_id = first.get("feature_id")
    flagged = [item["id"] for item in features if item.get("first_vertical_slice")]
    if flagged != [first_id]:
        fail("Exactly one feature must match first_vertical_slice.feature_id.")
    if not first.get("acceptance"):
        fail("First vertical slice has no acceptance criteria.")

    counts = {
        app: sum(item["app"] == app for item in features)
        for app in sorted(allowed_apps)
    }
    print(
        "Wally feature register is valid: "
        f"{len(features)} feature groups; "
        + ", ".join(f"{app}={count}" for app, count in counts.items())
        + f"; first slice={first_id}."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
