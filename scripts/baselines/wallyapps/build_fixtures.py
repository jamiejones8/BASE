#!/usr/bin/env python3
"""Build deterministic, fully synthetic fixtures for the Wally baseline suite.

Only CSV headers are read from the imported working copy. No source rows are
copied. All identities, events, measurements, dates, and identifiers written to
the fixtures are synthetic.
"""

from __future__ import annotations

import csv
import json
from datetime import date, datetime, time, timedelta, timezone
from pathlib import Path
from typing import Any

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[3]
WALLY_ROOT = PROJECT_ROOT / "WallyApps"
FIXTURE_ROOT = PROJECT_ROOT / "tests" / "fixtures" / "wallyapps"

SEASON_FILES = (
    ("2025 Season -cleaned.csv", date(2025, 3, 1), "S25"),
    ("2025 Fall -cleaned.csv", date(2025, 10, 1), "F25"),
    ("2026 Squads - cleaned.csv", date(2026, 1, 15), "SQ26"),
    ("2026 Season - cleaned.csv", date(2026, 3, 15), "S26"),
)

PITCH_TYPES = (
    "Four-Seam",
    "Sinker",
    "Slider",
    "Sweeper",
    "Curveball",
    "Changeup",
    "Cutter",
    "Splitter",
)

PITCH_BASELINES = {
    "Four-Seam": (92.0, 2250.0, 16.0, -7.0),
    "Sinker": (90.0, 2150.0, 9.0, 15.0),
    "Slider": (83.0, 2450.0, 2.0, -10.0),
    "Sweeper": (81.0, 2550.0, 4.0, -16.0),
    "Curveball": (78.0, 2500.0, -12.0, -8.0),
    "Changeup": (84.0, 1750.0, 7.0, 13.0),
    "Cutter": (87.0, 2350.0, 8.0, -3.0),
    "Splitter": (85.0, 1650.0, 3.0, 8.0),
}

OUTCOME_SEQUENCES = (
    ("Single", ("BallCalled", "StrikeCalled", "InPlay")),
    ("Double", ("StrikeCalled", "FoulBallNotFieldable", "InPlay")),
    ("HomeRun", ("BallCalled", "FoulBallNotFieldable", "InPlay")),
    ("Out", ("BallCalled", "StrikeCalled", "InPlay")),
    ("Strikeout", ("StrikeCalled", "FoulBallNotFieldable", "StrikeSwinging")),
    ("Walk", ("BallCalled", "BallCalled", "BallCalled", "BallCalled")),
    ("Triple", ("StrikeCalled", "BallCalled", "InPlay")),
    ("HitByPitch", ("HitByPitch",)),
)


def read_header(path: Path) -> list[str]:
    if not path.exists():
        raise SystemExit(f"Fixture schema source is missing: {path}")
    with path.open("r", encoding="utf-8-sig", errors="replace", newline="") as handle:
        return next(csv.reader(handle), [])


def assign(row: dict[str, Any], **values: Any) -> None:
    for key, value in values.items():
        if key in row:
            row[key] = value


def called_pitch_counts(calls: tuple[str, ...], index: int) -> tuple[int, int]:
    balls = sum(call == "BallCalled" for call in calls[:index])
    strikes = sum(
        call in {"StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "FoulTip"}
        for call in calls[:index]
    )
    return min(balls, 3), min(strikes, 2)


def terminal_values(outcome: str, is_terminal: bool) -> dict[str, Any]:
    if not is_terminal:
        return {
            "KorBB": "Undefined",
            "PlayResult": "Undefined",
            "TaggedHitType": "Undefined",
            "AutoHitType": "",
            "OutsOnPlay": 0,
            "RunsScored": 0,
        }

    if outcome == "Strikeout":
        return {
            "KorBB": "Strikeout",
            "PlayResult": "Undefined",
            "TaggedHitType": "Undefined",
            "AutoHitType": "",
            "OutsOnPlay": 1,
            "RunsScored": 0,
        }
    if outcome == "Walk":
        return {
            "KorBB": "Walk",
            "PlayResult": "Undefined",
            "TaggedHitType": "Undefined",
            "AutoHitType": "",
            "OutsOnPlay": 0,
            "RunsScored": 0,
        }
    if outcome == "HitByPitch":
        return {
            "KorBB": "Undefined",
            "PlayResult": "Undefined",
            "TaggedHitType": "Undefined",
            "AutoHitType": "",
            "OutsOnPlay": 0,
            "RunsScored": 0,
        }

    hit_type = {
        "Single": "GroundBall",
        "Double": "LineDrive",
        "Triple": "LineDrive",
        "HomeRun": "FlyBall",
        "Out": "FlyBall",
    }[outcome]
    return {
        "KorBB": "Undefined",
        "PlayResult": outcome,
        "TaggedHitType": hit_type,
        "AutoHitType": hit_type,
        "OutsOnPlay": 1 if outcome == "Out" else 0,
        "RunsScored": 1 if outcome == "HomeRun" else 0,
    }


def synthetic_trackman_rows(
    columns: list[str],
    role: str,
    segment: str,
    start_date: date,
    games: int = 2,
    players: int = 2,
) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    pitch_number = 0

    for game_index in range(games):
        game_date = start_date + timedelta(days=game_index * 3)
        game_id = f"FIXTURE-{segment}-GAME-{game_index + 1:02d}"
        game_uid = f"fixture-{segment.lower()}-game-{game_index + 1:02d}"

        for player_index in range(players):
            team_pitcher = f"Fixture Player P{player_index + 1:02d}"
            team_hitter = f"Fixture Player H{player_index + 1:02d}"

            for pa_index, (outcome, calls) in enumerate(OUTCOME_SEQUENCES, start=1):
                inning = ((pa_index - 1) // 2) + 1
                opponent_index = (pa_index + player_index) % 6 + 1
                pitcher = team_pitcher if role == "pitching" else f"Fixture Opponent P{opponent_index:02d}"
                batter = team_hitter if role == "hitting" else f"Fixture Opponent H{opponent_index:02d}"
                pitcher_team = "TEX_BOB" if role == "pitching" else "FIX_OPP_A"
                batter_team = "TEX_BOB" if role == "hitting" else "FIX_OPP_A"

                for pitch_of_pa, pitch_call in enumerate(calls, start=1):
                    pitch_number += 1
                    row = {column: "" for column in columns}
                    is_terminal = pitch_of_pa == len(calls)
                    pitch_type = PITCH_TYPES[(pitch_number + player_index + game_index) % len(PITCH_TYPES)]
                    speed, spin, ivb, hb = PITCH_BASELINES[pitch_type]
                    variation = ((pitch_number % 7) - 3) * 0.45
                    balls, strikes = called_pitch_counts(calls, pitch_of_pa - 1)
                    plate_side = round(((pitch_number % 9) - 4) * 0.24, 3)
                    plate_height = round(1.55 + (pitch_number % 9) * 0.24, 3)
                    event_values = terminal_values(outcome, is_terminal)
                    in_play = is_terminal and outcome in {"Single", "Double", "Triple", "HomeRun", "Out"}
                    event_time = datetime.combine(
                        game_date,
                        time(12 + min(inning, 8), (pa_index * 7 + pitch_of_pa) % 60),
                        tzinfo=timezone(timedelta(hours=-5)),
                    )

                    assign(
                        row,
                        PitchNo=pitch_number,
                        Date=game_date.isoformat(),
                        Time=event_time.strftime("%H:%M:%S"),
                        PAofInning=((pa_index - 1) % 2) + 1,
                        PitchofPA=pitch_of_pa,
                        Pitcher=pitcher,
                        PitcherId=100000 + (player_index if role == "pitching" else opponent_index),
                        PitcherThrows="Left" if (player_index + pa_index) % 3 == 0 else "Right",
                        PitcherTeam=pitcher_team,
                        Batter=batter,
                        BatterId=200000 + (player_index if role == "hitting" else opponent_index),
                        BatterSide="Left" if (player_index + pa_index) % 2 == 0 else "Right",
                        BatterTeam=batter_team,
                        PitcherSet="Stretch" if pa_index % 2 == 0 else "Windup",
                        Inning=inning,
                        **{"Top/Bottom": "Top" if role == "pitching" else "Bottom"},
                        Outs=(pa_index - 1) % 3,
                        Balls=balls,
                        Strikes=strikes,
                        TaggedPitchType=pitch_type,
                        AutoPitchType=pitch_type,
                        ModelPitchType=pitch_type,
                        ModelPitchTypeRaw=pitch_type,
                        ModelConfidence=0.93,
                        ModelConfidenceRaw=0.89,
                        ModelRetagReason="fixture",
                        ModelTop3=pitch_type,
                        PitchCall=pitch_call,
                        Notes="",
                        RelSpeed=round(speed + variation, 3),
                        VertRelAngle=round(-2.0 + (pitch_number % 5) * 0.4, 3),
                        HorzRelAngle=round(-3.0 + (pitch_number % 7) * 0.7, 3),
                        SpinRate=round(spin + ((pitch_number * 37) % 180) - 90, 2),
                        SpinAxis=round((pitch_number * 19) % 360, 2),
                        Tilt=f"{(pitch_number % 12) + 1}:{(pitch_number * 5) % 60:02d}",
                        RelHeight=round(5.4 + (player_index * 0.25) + (pitch_number % 3) * 0.04, 3),
                        RelSide=round((-2.2 if player_index % 2 == 0 else 2.2) + (pitch_number % 3) * 0.08, 3),
                        Extension=round(5.8 + (pitch_number % 5) * 0.12, 3),
                        VertBreak=round(ivb - 11.0 + variation, 3),
                        InducedVertBreak=round(ivb + variation, 3),
                        HorzBreak=round(hb - variation, 3),
                        PlateLocHeight=plate_height,
                        PlateLocSide=plate_side,
                        ZoneSpeed=round(speed - 7.0 + variation, 3),
                        VertApprAngle=round(-4.2 - (pitch_number % 5) * 0.35, 3),
                        HorzApprAngle=round(-2.0 + (pitch_number % 6) * 0.7, 3),
                        ZoneTime=round(0.39 + (pitch_number % 5) * 0.012, 3),
                        ExitSpeed=round(82 + (pa_index * 3.1) + player_index, 2) if in_play else "",
                        Angle=round(-8 + pa_index * 5.5, 2) if in_play else "",
                        Direction=round(-30 + pa_index * 8.0, 2) if in_play else "",
                        Distance=round(120 + pa_index * 31.0, 2) if in_play else "",
                        LastTrackedDistance=round(118 + pa_index * 30.0, 2) if in_play else "",
                        Bearing=round(-28 + pa_index * 7.5, 2) if in_play else "",
                        HangTime=round(1.1 + pa_index * 0.42, 2) if in_play else "",
                        PositionAt110X=round(-45 + pa_index * 12.0, 2) if in_play else "",
                        PositionAt110Y=round(70 + pa_index * 16.0, 2) if in_play else "",
                        PositionAt110Z=round(max(0.0, 12 - pa_index), 2) if in_play else "",
                        HomeTeam="TEX_BOB",
                        AwayTeam="FIX_OPP_A",
                        Stadium="Fixture Park",
                        Level="D1",
                        League="SBELT",
                        GameID=game_id,
                        GameUID=game_uid,
                        PitchUID=f"fixture-{segment.lower()}-pitch-{pitch_number:06d}",
                        PlayID=f"fixture-{segment.lower()}-play-{pitch_number:06d}",
                        EffectiveVelo=round(speed + variation - 0.7, 3),
                        LocalDateTime=event_time.isoformat(),
                        UTCDateTime=event_time.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
                        UTCDate=event_time.astimezone(timezone.utc).date().isoformat(),
                        UTCTime=event_time.astimezone(timezone.utc).strftime("%H:%M:%S"),
                        System="fixture",
                        HomeTeamForeignID=900001,
                        AwayTeamForeignID=900002,
                        GameForeignID=game_id,
                        Catcher=f"Fixture Catcher {(game_index % 2) + 1:02d}",
                        CatcherId=300001 + (game_index % 2),
                        CatcherThrows="Right",
                        CatcherTeam="TEX_BOB",
                        PitchReleaseConfidence="High",
                        PitchLocationConfidence="High",
                        PitchMovementConfidence="High",
                        HitLaunchConfidence="High" if in_play else "",
                        HitLandingConfidence="High" if in_play else "",
                        query_mode="fixture",
                        exported_at="2026-08-24T00:00:00Z",
                        source_scrape_date="2026-08-24",
                        query_row_id=pitch_number,
                        matched_team_code="TEX_BOB",
                        matched_player=team_pitcher if role == "pitching" else team_hitter,
                        matched_data_type=role,
                        matched_query_start_date=start_date.isoformat(),
                        matched_query_end_date=(start_date + timedelta(days=30)).isoformat(),
                    )
                    for key, value in event_values.items():
                        if key in row:
                            row[key] = value
                    for count_column in ("BallsBeforePitch", "BallsPre"):
                        if count_column in row:
                            row[count_column] = balls
                    for count_column in ("StrikesBeforePitch", "StrikesPre"):
                        if count_column in row:
                            row[count_column] = strikes

                    rows.append(row)

    return rows


def write_rows(path: Path, columns: list[str], rows: list[dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    frame = pd.DataFrame(rows, columns=columns)
    frame.to_csv(path, index=False, lineterminator="\n")


def build_hitting_and_pitching() -> dict[str, Any]:
    summary: dict[str, Any] = {"hitting": {}, "pitching": {}}
    generated_pitching: list[tuple[str, list[dict[str, Any]]]] = []

    for app_name, role in (("HittingApp", "hitting"), ("PitchingApp", "pitching")):
        source_dir = WALLY_ROOT / app_name / "data"
        fixture_dir = FIXTURE_ROOT / role / "data"
        for filename, start_date, segment in SEASON_FILES:
            source = source_dir / filename
            columns = read_header(source)
            rows = synthetic_trackman_rows(columns, role, segment, start_date)
            write_rows(fixture_dir / filename, columns, rows)
            summary[role][filename] = len(rows)
            if role == "pitching":
                generated_pitching.append((filename, rows))

    bullpen_name = "Bullpens - cleaned.csv"
    bullpen_columns = read_header(WALLY_ROOT / "PitchingApp" / "data" / bullpen_name)
    bullpen_rows = synthetic_trackman_rows(
        bullpen_columns,
        "pitching",
        "BP26",
        date(2026, 1, 20),
        games=2,
        players=2,
    )
    write_rows(FIXTURE_ROOT / "pitching" / "data" / bullpen_name, bullpen_columns, bullpen_rows)
    summary["pitching"][bullpen_name] = len(bullpen_rows)
    generated_pitching.append((bullpen_name, bullpen_rows))

    called_stuff = pd.DataFrame(
        [
            {
                "pitch_type": pitch_type,
                "RelSpeed_mean": PITCH_BASELINES[pitch_type][0],
                "RelSpeed_sd": 2.0,
                "SpinRate_mean": PITCH_BASELINES[pitch_type][1],
                "SpinRate_sd": 180.0,
                "InducedVertBreak_mean": PITCH_BASELINES[pitch_type][2],
                "InducedVertBreak_sd": 3.0,
                "HorzBreak_mean": PITCH_BASELINES[pitch_type][3],
                "HorzBreak_sd": 3.0,
                "VertApprAngle_mean": -5.5,
                "VertApprAngle_sd": 0.8,
                "HorzApprAngle_mean": 0.0,
                "HorzApprAngle_sd": 1.5,
                "RelHeight_mean": 5.7,
                "RelHeight_sd": 0.35,
                "RelSide_mean": 0.0,
                "RelSide_sd": 2.0,
                "Extension_mean": 6.0,
                "Extension_sd": 0.4,
                "comp_mean": 0.0,
                "comp_sd": 1.0,
                "comp_n": 500,
            }
            for pitch_type in PITCH_TYPES
        ]
    )
    called_stuff.to_csv(
        FIXTURE_ROOT / "pitching" / "data" / "called_stuff_baseline.csv",
        index=False,
        lineterminator="\n",
    )

    xrv_rows: list[dict[str, Any]] = []
    pitchers: set[str] = set()
    for filename, rows in generated_pitching:
        for row_index, row in enumerate(rows, start=1):
            pitchers.add(str(row.get("Pitcher", "")))
            xrv_rows.append(
                {
                    "source_file": filename,
                    "row_in_file": row_index,
                    "xrv": round(-0.035 + (row_index % 15) * 0.005, 4),
                    "arm_angle": round(38 + (row_index % 20) * 1.4, 2),
                    "xiVB": round(-2.0 + (row_index % 9) * 0.5, 3),
                    "iVB_oe": round(-1.5 + (row_index % 7) * 0.5, 3),
                }
            )
    pd.DataFrame(xrv_rows).to_csv(
        FIXTURE_ROOT / "pitching" / "data" / "xrv_metrics.csv",
        index=False,
        lineterminator="\n",
    )
    pd.DataFrame(
        [
            {"player_name": pitcher, "height_in_inches": 72 + index % 7}
            for index, pitcher in enumerate(sorted(pitchers))
            if pitcher
        ]
    ).to_csv(
        FIXTURE_ROOT / "pitching" / "data" / "player_heights.csv",
        index=False,
        lineterminator="\n",
    )
    build_pitching_percentile_reference()
    return summary


def build_pitching_percentile_reference() -> None:
    metrics = (
        "avg_ev", "chase_rate", "extension", "heater_velocity",
        "performance_2k_zone_pct", "performance_baa", "performance_barrel_pct",
        "performance_bb9", "performance_bb_hbp_pct", "performance_bb_pct",
        "performance_chase_pct", "performance_csw_pct", "performance_ea_pct",
        "performance_fip", "performance_fps_pct", "performance_gb_pct",
        "performance_h9", "performance_izwhiff_pct", "performance_k9",
        "performance_k_pct", "performance_ops", "performance_pre2k_zone_pct",
        "performance_prv", "performance_put_away_pct", "performance_slg",
        "performance_strike_pct", "performance_whiff_pct", "performance_whip",
        "performance_win11_pct", "performance_woba", "performance_wobacon",
        "performance_zone_pct", "pitchtype_chase_rate", "pitchtype_extension",
        "pitchtype_haa", "pitchtype_hb", "pitchtype_ivb", "pitchtype_rpm",
        "pitchtype_vaa", "pitchtype_velocity", "pitchtype_whiff_rate",
        "pitchtype_woba", "release_height", "release_side", "whiff_rate", "woba",
    )
    pitch_types = ("",) + PITCH_TYPES
    rows: list[dict[str, Any]] = []
    for metric_index, metric in enumerate(metrics):
        for pitch_type in pitch_types:
            scope = "pitch_type" if pitch_type else "overall"
            for sample_index in range(25):
                base = 0.1 + (metric_index % 7) * 0.07
                rows.append(
                    {
                        "scope": scope,
                        "pitch_type": pitch_type,
                        "metric": metric,
                        "value": round(base + sample_index * 0.01, 5),
                        "sample_n": 100 + sample_index * 5,
                    }
                )
    pd.DataFrame(rows).to_csv(
        FIXTURE_ROOT / "pitching" / "data" / "d1_pitch_metric_percentile_reference.csv",
        index=False,
        lineterminator="\n",
    )


def synthetic_defense_sources() -> dict[str, int]:
    defense_source = WALLY_ROOT / "DefenseApp" / "data" / "BobcatsDefense2026.csv"
    batted_source = WALLY_ROOT / "DefenseApp" / "data" / "BobcatsDefenseBattedBalls.csv"
    catching_source = WALLY_ROOT / "DefenseApp" / "data" / "Catchers - 2026 Season-cleaned.csv"
    defense_columns = read_header(defense_source)
    batted_columns = read_header(batted_source)
    catching_columns = read_header(catching_source)

    defense_rows: list[dict[str, Any]] = []
    batted_rows: list[dict[str, Any]] = []
    positions = ("1B", "2B", "3B", "SS", "LF", "CF", "RF")
    anchors = {
        "1B": (55.0, 115.0), "2B": (25.0, 145.0), "3B": (-55.0, 115.0),
        "SS": (-25.0, 145.0), "LF": (-95.0, 245.0), "CF": (0.0, 285.0), "RF": (95.0, 245.0),
    }
    event_number = 0
    for game_index in range(2):
        game_date = date(2026, 3, 20) + timedelta(days=game_index * 3)
        game_uid = f"fixture-defense-game-{game_index + 1:02d}"
        for event_index in range(48):
            event_number += 1
            pitch_uid = f"fixture-defense-pitch-{event_number:05d}"
            play_id = f"fixture-defense-play-{event_number:05d}"
            bucket = ("GroundBall", "FlyBall", "LineDrive", "Popup")[event_index % 4]
            made_play = event_index % 5 != 0
            play_result = "Out" if made_play else ("Single" if bucket == "GroundBall" else "Double")
            bearing = -42.0 + (event_index % 13) * 7.0
            distance = 90.0 + (event_index % 10) * 24.0

            defense_row = {column: "" for column in defense_columns}
            assign(
                defense_row,
                query_mode="fixture",
                exported_at="2026-08-24T00:00:00Z",
                source_scrape_date="2026-08-24",
                source_file="fixture-defense",
                query_row_id=event_number,
                matched_team_code="TEX_BOB",
                matched_data_type="fielding",
                matched_query_start_date="2026-03-20",
                matched_query_end_date="2026-03-23",
                GameUID=game_uid,
                PitchNo=event_index + 1,
                PitchUID=pitch_uid,
                PlayID=play_id,
                Date=game_date.isoformat(),
                Time=f"{13 + event_index // 12:02d}:{event_index % 60:02d}:00",
                UTCDate=game_date.isoformat(),
                UTCTime=f"{18 + event_index // 12:02d}:{event_index % 60:02d}:00",
                LocalDateTime=f"{game_date.isoformat()}T13:00:00-05:00",
                UTCDateTime=f"{game_date.isoformat()}T18:00:00Z",
                PitcherTeam="TEX_BOB" if event_index % 2 == 0 else "FIX_OPP_A",
                BatterTeam="FIX_OPP_A" if event_index % 2 == 0 else "TEX_BOB",
                PitchCall="InPlay",
                PlayResult=play_result,
                DetectedShift="NoShift" if event_index % 3 else "Shift",
                FHC=1 if made_play else 0,
            )
            for position_index, position in enumerate(positions, start=1):
                x_anchor, z_anchor = anchors[position]
                assign(
                    defense_row,
                    **{
                        f"{position}_PositionAtReleaseX": round(x_anchor + ((event_index + position_index) % 5 - 2) * 2.2, 2),
                        f"{position}_PositionAtReleaseZ": round(z_anchor + ((event_index + position_index) % 5 - 2) * 2.8, 2),
                        f"{position}_Name": f"Fixture Defender {position} {(game_index % 2) + 1:02d}",
                        f"{position}_Id": 400000 + position_index * 10 + game_index,
                    },
                )
            defense_rows.append(defense_row)

            batted_row = {column: "" for column in batted_columns}
            assign(
                batted_row,
                query_mode="fixture",
                exported_at="2026-08-24T00:00:00Z",
                source_scrape_date="2026-08-24",
                source_file="fixture-batted-ball",
                query_row_id=event_number,
                matched_team_code="TEX_BOB",
                matched_data_type="pitching",
                GameUID=game_uid,
                GameID=f"FIXTURE-DEFENSE-GAME-{game_index + 1:02d}",
                PitchNo=event_index + 1,
                PitchUID=pitch_uid,
                PlayID=play_id,
                Date=game_date.isoformat(),
                Pitcher=f"Fixture Pitcher {(event_index % 2) + 1:02d}",
                PitcherId=100001 + event_index % 2,
                PitcherThrows="Right" if event_index % 3 else "Left",
                PitcherTeam=defense_row.get("PitcherTeam"),
                Batter=f"Fixture Hitter {(event_index % 6) + 1:02d}",
                BatterId=200001 + event_index % 6,
                BatterSide="Right" if event_index % 2 else "Left",
                BatterTeam=defense_row.get("BatterTeam"),
                PitchCall="InPlay",
                PlayResult=play_result,
                TaggedPitchType=PITCH_TYPES[event_index % len(PITCH_TYPES)],
                AutoPitchType=PITCH_TYPES[event_index % len(PITCH_TYPES)],
                TaggedHitType=bucket,
                AutoHitType=bucket,
                ExitSpeed=round(78 + (event_index % 12) * 2.2, 2),
                Angle=(-6, 18, 11, 42)[event_index % 4],
                Bearing=bearing,
                Direction=bearing,
                Distance=distance,
                LastTrackedDistance=distance - 2.0,
                PositionAt110X=round(distance * 0.35 * (1 if bearing >= 0 else -1), 2),
                PositionAt110Y=round(distance * 0.82, 2),
                PositionAt110Z=round(max(0.0, 14 - event_index % 9), 2),
                HomeTeam="TEX_BOB",
                AwayTeam="FIX_OPP_A",
                Stadium="Fixture Park",
                Level="D1",
                League="SBELT",
                Catcher=f"Fixture Catcher {(game_index % 2) + 1:02d}",
                CatcherId=300001 + game_index % 2,
                CatcherThrows="Right",
                CatcherTeam="TEX_BOB",
            )
            batted_rows.append(batted_row)

    catching_rows = synthetic_trackman_rows(
        catching_columns,
        "pitching",
        "CATCH26",
        date(2026, 3, 20),
        games=2,
        players=2,
    )
    for index, row in enumerate(catching_rows):
        row["Catcher"] = f"Fixture Catcher {(index // 40) % 2 + 1:02d}"
        row["CatcherId"] = 300001 + (index // 40) % 2

    output_dir = FIXTURE_ROOT / "defense" / "data"
    write_rows(output_dir / defense_source.name, defense_columns, defense_rows)
    write_rows(output_dir / batted_source.name, batted_columns, batted_rows)
    write_rows(output_dir / catching_source.name, catching_columns, catching_rows)
    build_catcher_baseline(output_dir / "d1_catcher_framing_metrics.csv")
    return {
        defense_source.name: len(defense_rows),
        batted_source.name: len(batted_rows),
        catching_source.name: len(catching_rows),
    }


def build_catcher_baseline(path: Path) -> None:
    metric_specs = (
        ("strikes_stolen", "Strikes Stolen", "high"),
        ("balls_lost", "Balls Lost", "low"),
        ("bottom_buffer", "Bottom Buffer", "high"),
        ("top_buffer", "Top Buffer", "high"),
        ("glove_side_buffer", "Glove Side Buffer", "high"),
        ("arm_side_buffer", "Arm Side Buffer", "high"),
        ("rhp", "RHP", "high"),
        ("lhp", "LHP", "high"),
        ("fastballs_sinkers", "Fastballs/Sinkers", "high"),
        ("breaking_balls", "Breaking Balls", "high"),
        ("soft", "Soft", "high"),
        ("lhp_fastballs_sinkers", "LHP Fastballs/Sinkers", "high"),
        ("lhp_breaking_balls", "LHP Breaking Balls", "high"),
        ("lhp_soft", "LHP Soft", "high"),
        ("rhp_fastballs_sinkers", "RHP Fastballs/Sinkers", "high"),
        ("rhp_breaking_balls", "RHP Breaking Balls", "high"),
        ("rhp_soft", "RHP Soft", "high"),
    )
    rows: list[dict[str, Any]] = []
    for metric_order, (metric_id, metric, better) in enumerate(metric_specs, start=1):
        for catcher_index in range(40):
            chances = 100 + catcher_index * 3
            centered = (catcher_index - 20) / 200
            value = (0.08 if metric_id == "strikes_stolen" else 0.0) + centered
            if metric_id == "balls_lost":
                value = 0.12 + centered
            numerator = round(value * chances, 4)
            rows.append(
                {
                    "metric_id": metric_id,
                    "metric": metric,
                    "Catcher": f"D1 Fixture Catcher {catcher_index + 1:03d}",
                    "value": round(value, 5),
                    "chances": chances,
                    "numerator": numerator,
                    "metric_order": metric_order,
                    "better": better,
                }
            )
    pd.DataFrame(rows).to_csv(path, index=False, lineterminator="\n")


def build_edge_cases() -> int:
    columns = [
        "PitchNo", "Date", "Pitcher", "PitcherTeam", "Batter", "BatterTeam",
        "PitchCall", "PlayResult", "TaggedPitchType", "PlateLocHeight",
        "PlateLocSide", "GameUID", "PitchUID",
    ]
    rows = [
        {
            "PitchNo": 1,
            "Date": "not-a-date",
            "Pitcher": "Fixture Missing Pitcher",
            "PitcherTeam": "TEX_BOB",
            "Batter": "Fixture Missing Hitter",
            "BatterTeam": "FIX_OPP_A",
            "PitchCall": "Undefined",
            "PlayResult": "Undefined",
            "TaggedPitchType": "Undefined",
            "PlateLocHeight": "",
            "PlateLocSide": "",
            "GameUID": "fixture-edge-game-01",
            "PitchUID": "fixture-edge-pitch-01",
        },
        {
            "PitchNo": 2,
            "Date": "2026-03-01",
            "Pitcher": "",
            "PitcherTeam": "",
            "Batter": "",
            "BatterTeam": "",
            "PitchCall": "BallCalled",
            "PlayResult": "",
            "TaggedPitchType": "",
            "PlateLocHeight": 3.2,
            "PlateLocSide": -0.4,
            "GameUID": "fixture-edge-game-01",
            "PitchUID": "fixture-edge-pitch-02",
        },
    ]
    write_rows(FIXTURE_ROOT / "edge_cases" / "missing_optional_columns.csv", columns, rows)
    return len(rows)


def main() -> None:
    summary = build_hitting_and_pitching()
    summary["defense"] = synthetic_defense_sources()
    summary["edge_cases"] = {"missing_optional_columns.csv": build_edge_cases()}
    summary["synthetic_only"] = True
    summary_path = FIXTURE_ROOT / "fixture-manifest.json"
    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"Wrote synthetic Wally fixtures under {FIXTURE_ROOT}")


if __name__ == "__main__":
    main()
