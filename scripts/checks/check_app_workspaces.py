#!/usr/bin/env python3
"""Validate the user-approved BASE workspace and navigation hierarchy."""

from __future__ import annotations

import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKSPACE_PATH = ROOT / "config" / "app_workspaces.json"
SOURCE_PATH = ROOT / "config" / "baseball_data_sources.json"
APP_PATH = ROOT / "R" / "app_main.R"
STYLE_PATH = ROOT / "www" / "styles.css"
WALLY_PITCHING_PATH = ROOT / "WallyApps" / "PitchingApp" / "PitchingApp.R"
WALLY_HITTING_PATH = ROOT / "WallyApps" / "HittingApp" / "HittingApp.R"
WALLY_DEFENSE_PATH = ROOT / "WallyApps" / "DefenseApp" / "DefenseApp.R"
WALLY_SCOUTING_PATH = ROOT / "WallyApps" / "ScoutingApp" / "ScoutingApp.R"
WALLY_JUCO_PATH = ROOT / "WallyApps" / "JucoStatsApp" / "app.R"


def fail(message: str) -> None:
    raise SystemExit(message)


def main() -> int:
    config = json.loads(WORKSPACE_PATH.read_text(encoding="utf-8"))
    sources = json.loads(SOURCE_PATH.read_text(encoding="utf-8"))
    workspaces = config.get("workspaces", [])

    expected_ids = [
        "postgame_reports",
        "team_pitching",
        "team_hitting",
        "opponent_scouting",
        "juco_stats",
        "defense",
        "homebase",
        "data_processing",
    ]
    ids = [item.get("id") for item in workspaces]
    if ids != expected_ids:
        fail(f"Workspace order differs from the approved hierarchy: {ids}")
    if [item.get("order") for item in workspaces] != list(range(1, 9)):
        fail("Workspace order values must be contiguous from 1 through 8.")

    navigation = config.get("navigation", {})
    if navigation.get("global_navbar") != "hidden":
        fail("The approved hierarchy hides the global navbar.")
    home_control = navigation.get("persistent_control", {})
    if home_control.get("type") != "home_button" or home_control.get("position") != "top_left":
        fail("A persistent top-left Home button is required.")

    by_id = {item["id"]: item for item in workspaces}
    postgame = by_id["postgame_reports"]
    expected_postgame_tabs = {"tab_pitcher", "tab_hitter", "tab_catcher"}
    actual_postgame_tabs = {
        tool.get("existing_base_tab") for tool in postgame.get("tools", [])
    }
    if actual_postgame_tabs != expected_postgame_tabs or len(postgame.get("tools", [])) != 3:
        fail("Postgame Reports must contain only the three existing BASE PDF generators.")

    opponent = by_id["opponent_scouting"]
    expected_scouting_tools = {
        "Hitter Card",
        "Pitch Type Tables",
        "Heat Maps",
        "Pitcher Card",
        "Stuff Sheet",
        "Matchup Grid",
    }
    if set(opponent.get("tools", [])) != expected_scouting_tools:
        fail("Opponent Scouting must expose all six ScoutingApp workflows.")
    if set(opponent.get("compatibility_tabs", [])) != {
        "tab_pitcher_player",
        "tab_hitter_scouting",
    }:
        fail("The superseded opponent tabs must remain hidden compatibility targets.")
    if set(opponent.get("source_routes", [])) != {
        "national_pitcher_scouting",
        "national_hitter_scouting",
    }:
        fail("ScoutingApp must retain both national scouting source routes.")

    homebase = by_id["homebase"]
    entry_types = {item.get("type") for item in homebase.get("entry_points", [])}
    if entry_types != {"global_player_search", "texas_state_roster_card"}:
        fail("HomeBASE requires both national search and Texas State roster-card entry.")

    defense = by_id["defense"]
    if "Interactive catcher" not in defense.get("catching_policy", ""):
        fail("Interactive catcher statistics must be routed to Defensive Analytics.")

    processing = by_id["data_processing"]
    retagger = next(
        (
            tool for tool in processing.get("tools", [])
            if isinstance(tool, dict) and tool.get("id") == "pitch_retagger"
        ),
        None,
    )
    if not retagger or retagger.get("existing_base_tab") != "tab_pitcher_player":
        fail("Data Processing must route to the persistent pitch retagger.")

    known_routes = set(sources.get("feature_routes", {}))
    for workspace in workspaces:
        route = workspace.get("source_route")
        if route and route not in known_routes:
            fail(f"Workspace {workspace['id']} references unknown source route {route}.")
        for route in workspace.get("source_routes", []):
            if route not in known_routes:
                fail(f"Workspace {workspace['id']} references unknown source route {route}.")
        for tool in workspace.get("tools", []):
            if isinstance(tool, dict):
                tool_route = tool.get("source_route")
                if tool_route and tool_route not in known_routes:
                    fail(f"Tool {tool.get('id')} references unknown source route {tool_route}.")

    app_source = APP_PATH.read_text(encoding="utf-8")
    style_source = STYLE_PATH.read_text(encoding="utf-8")
    routed_tabs = [
        "tab_postgame_reports",
        "tab_team_pitching",
        "tab_team_hitting",
        "tab_opponent_scouting",
        "tab_juco_stats",
        "tab_defense_workspace",
        "tab_homebase",
        "tab_data_processing",
    ]
    missing_tabs = [tab for tab in routed_tabs if tab not in app_source]
    if missing_tabs:
        fail(f"Unified shell is missing workspace routes: {missing_tabs}")

    card_targets = [
        '"postgame_reports", "01"',
        '"team_pitching", "02"',
        '"team_hitting", "03"',
        '"opponent_scouting", "04"',
        '"juco_stats", "05"',
        '"defense_workspace", "06"',
        '"homebase", "07"',
        '"data_processing", "08"',
    ]
    card_positions = [app_source.find(target) for target in card_targets]
    if any(position < 0 for position in card_positions) or card_positions != sorted(card_positions):
        fail("Home cards are missing or differ from the approved eight-workspace order.")

    if 'id = "base-shell-home"' not in app_source:
        fail("The persistent Home control is not implemented in the app shell.")
    if 'base_source("R/integrations/wally_pitching_workspace.R"' not in app_source:
        fail("The Pitching workspace adapter is not sourced by BASE.")
    if 'input, session, "tab_team_pitching"' not in app_source:
        fail("The Pitching workspace is not registered for lazy initialization.")
    if 'base_source("R/integrations/wally_scouting_workspace.R"' not in app_source:
        fail("The Scouting workspace adapter is not sourced by BASE.")
    if 'input, session, "tab_opponent_scouting"' not in app_source:
        fail("Opponent Scouting is not registered for lazy initialization.")
    if "base_opponent_scouting_workspace_ui()" not in app_source:
        fail("Opponent Scouting does not render the integrated ScoutingApp workspace.")
    if 'base_source("R/integrations/wally_juco_stats_workspace.R"' not in app_source:
        fail("The JUCO Stats workspace adapter is not sourced by BASE.")
    if 'input, session, "tab_juco_stats"' not in app_source:
        fail("JUCO Stats is not registered for lazy initialization.")
    if "base_juco_stats_workspace_ui()" not in app_source:
        fail("JUCO Stats does not render the integrated JucoStatsApp workspace.")
    if '#cpp-page .cpp-retag-card' not in app_source:
        fail("The Data Processing card does not deep-link to the persistent retagger.")
    if "body > nav.navbar { display: none !important; }" not in style_source:
        fail("The compatibility navbar is not hidden by the application stylesheet.")

    id_patterns = [
        r'(?:Input|Output)\(\s*["\']([^"\']+)',
        r'output\$([A-Za-z0-9_]+)',
        r'input\$([A-Za-z0-9_]+)',
    ]

    def shiny_ids(source: str) -> set[str]:
        return set().union(*(set(re.findall(pattern, source)) for pattern in id_patterns))

    integrated_sources = {
        "BASE": app_source,
        "Wally Pitching": WALLY_PITCHING_PATH.read_text(encoding="utf-8"),
        "Wally Hitting": WALLY_HITTING_PATH.read_text(encoding="utf-8"),
        "Wally Defense": WALLY_DEFENSE_PATH.read_text(encoding="utf-8"),
        "Wally Scouting": WALLY_SCOUTING_PATH.read_text(encoding="utf-8"),
        "Wally JUCO": WALLY_JUCO_PATH.read_text(encoding="utf-8"),
    }
    source_ids = {name: shiny_ids(source) for name, source in integrated_sources.items()}
    names = list(source_ids)
    for index, left_name in enumerate(names):
        for right_name in names[index + 1:]:
            shared_ids = sorted(source_ids[left_name].intersection(source_ids[right_name]))
            if shared_ids:
                fail(
                    f"{left_name} and {right_name} expose conflicting Shiny IDs: "
                    f"{shared_ids}"
                )

    print("BASE workspace hierarchy and unified shell are valid: Home plus 8 ordered workspaces.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
