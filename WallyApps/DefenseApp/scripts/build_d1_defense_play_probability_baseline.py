#!/usr/bin/env python3
"""Build compact D1 defensive play-probability baselines from shared D1 parquet files."""

from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


APP_DIR = Path(__file__).resolve().parents[1]
WORKSPACE_DIR = APP_DIR.parent
SHARED_D1_DIR = Path(os.environ.get("D1_FILES_DIR", WORKSPACE_DIR / "D1 Files")).expanduser()
DEFENSE_PATH = SHARED_D1_DIR / "D1 Defense File.parquet"
HIT_PATH = SHARED_D1_DIR / "D1 Pitching:Hitting File.parquet"
OUT_PATH = APP_DIR / "data" / "d1_defense_play_probability_baseline.csv"

POSITIONS = ["1B", "2B", "3B", "SS", "LF", "CF", "RF"]
INFIELD = {"1B", "2B", "3B", "SS"}
OUTFIELD = {"LF", "CF", "RF"}
SHALLOW_AIR_DISTANCE_FT = 180

HIT_COLUMNS = [
    "PitchUID",
    "PitchCall",
    "PlayResult",
    "OutsOnPlay",
    "TaggedHitType",
    "AutoHitType",
    "ExitSpeed",
    "Angle",
    "Distance",
    "Bearing",
    "Direction",
    "HangTime",
]

DEFENSE_COLUMNS = [
    "PitchUID",
    "FHC",
    *[f"{pos}_PositionAtReleaseX" for pos in POSITIONS],
    *[f"{pos}_PositionAtReleaseZ" for pos in POSITIONS],
]

BASE_COLUMNS = [
    "level",
    "position",
    "batted_ball_bucket",
    "opportunity_zone",
    "distance_bin",
    "bearing_bin",
    "launch_angle_bin",
    "exit_speed_bin",
    "hang_time_bin",
    "range_sector",
    "chances",
    "made",
    "play_probability",
]


def canon_pitch_call(values: pd.Series) -> pd.Series:
    y = values.fillna("").astype(str).str.lower().str.replace(r"[^a-z]", "", regex=True)
    out = np.select(
        [
            y.isin(["inplay", "inplayout", "inplaynoout"]),
            y.isin(["strikecalled", "calledstrike"]),
            y.isin(["ballcalled", "ball", "ballinthedirt", "ballintentional"]),
        ],
        ["InPlay", "StrikeCalled", "BallCalled"],
        default=values.fillna("").astype(str),
    )
    return pd.Series(out, index=values.index)


def included_bip_result(pitch_call: pd.Series, play_result: pd.Series) -> pd.Series:
    pc = canon_pitch_call(pitch_call)
    pr = play_result.fillna("").astype(str).str.lower()
    bip = pc.eq("InPlay") | pr.str.contains(
        r"single|double|triple|home\s*run|homerun|\bhr\b|groundout|flyout|lineout|popout|fielderschoice|error|sacrifice",
        regex=True,
        na=False,
    )
    usable_result = ~pr.str.len().gt(0) | pr.str.contains(
        r"single|double|triple|home\s*run|homerun|\bhr\b|\bout\b|groundout|flyout|lineout|popout|forceout|fielderschoice|sacrifice|error|reached",
        regex=True,
        na=False,
    )
    hr = pr.str.contains(r"home\s*run|homerun|\bhr\b", regex=True, na=False)
    return bip & ~hr & usable_result


def made_play_from_result(play_result: pd.Series, outs_on_play: pd.Series) -> pd.Series:
    pr = play_result.fillna("").astype(str).str.lower()
    outs = pd.to_numeric(outs_on_play, errors="coerce")
    made = pd.Series(pd.NA, index=play_result.index, dtype="boolean")
    made[outs.gt(0)] = True
    made[pr.str.contains(r"\bout\b|groundout|flyout|lineout|popout|forceout|caught|sacrifice", regex=True, na=False)] = True
    made[pr.str.contains(r"single|double|triple|home\s*run|homerun|\bhr\b|error|safe|reached", regex=True, na=False)] = False
    return made


def play_result_bucket(play_result: pd.Series, made_play: pd.Series) -> pd.Series:
    pr = play_result.fillna("").astype(str).str.lower()
    out = pd.Series("Other", index=play_result.index, dtype=object)
    out[made_play.fillna(False).astype(bool) | pr.str.contains(r"\bout\b|groundout|flyout|lineout|popout|forceout|caught|sacrifice", regex=True, na=False)] = "Out"
    out[pr.str.contains(r"single|double|triple|home\s*run|homerun|\bhr\b", regex=True, na=False)] = "Hit"
    out[pr.str.contains(r"\berror\b", regex=True, na=False)] = "Error"
    return out


def batted_ball_bucket(hit_type: pd.Series, launch_angle: pd.Series) -> pd.Series:
    ht = hit_type.fillna("").astype(str).str.lower()
    la = pd.to_numeric(launch_angle, errors="coerce")
    out = pd.Series("Unknown", index=hit_type.index, dtype=object)
    out[la.notna()] = np.select(
        [la[la.notna()].lt(10), la[la.notna()].lt(25), la[la.notna()].lt(50)],
        ["Ground", "Line", "Fly"],
        default="Popup",
    )
    out[ht.str.contains(r"ground|gb|chopper|bunt", regex=True, na=False)] = "Ground"
    out[ht.str.contains(r"line|ld|liner", regex=True, na=False)] = "Line"
    out[ht.str.contains(r"fly|fb", regex=True, na=False)] = "Fly"
    out[ht.str.contains(r"pop|popup", regex=True, na=False)] = "Popup"
    return out


def opportunity_zone(pos: str, bucket: pd.Series, bearing: pd.Series, distance: pd.Series, made_play: pd.Series) -> pd.Series:
    out = pd.Series("", index=bucket.index, dtype=object)
    fair = bearing.between(-45, 45)
    made = made_play.fillna(False).astype(bool)
    is_ground = bucket.eq("Ground")
    is_air = bucket.isin(["Fly", "Line", "Popup"])
    is_shallow_air = is_air & (bucket.eq("Popup") | distance.le(SHALLOW_AIR_DISTANCE_FT))
    short_caught_air = is_air & made & distance.le(150)

    if pos == "3B":
        out[is_ground & fair & bearing.ge(-45) & bearing.lt(-22.5)] = "IF 3B"
    elif pos == "SS":
        out[is_ground & fair & bearing.ge(-22.5) & bearing.lt(0)] = "IF SS"
    elif pos == "2B":
        out[is_ground & fair & bearing.ge(0) & bearing.lt(22.5)] = "IF 2B"
    elif pos == "1B":
        out[is_ground & fair & bearing.ge(22.5) & bearing.le(45)] = "IF 1B"

    if pos in INFIELD:
        out[is_shallow_air] = f"Shallow Air {pos}"

    if pos == "LF":
        out[is_air & ~short_caught_air & fair & bearing.ge(-45) & bearing.lt(-15)] = "OF LF"
        out[is_air & ~short_caught_air & made & bearing.lt(-45)] = "OF LF Foul Out"
    elif pos == "CF":
        out[is_air & ~short_caught_air & fair & bearing.ge(-15) & bearing.le(15)] = "OF CF"
    elif pos == "RF":
        out[is_air & ~short_caught_air & fair & bearing.gt(15) & bearing.le(45)] = "OF RF"
        out[is_air & ~short_caught_air & made & bearing.gt(45)] = "OF RF Foul Out"

    return out


def range_sector(start_x: pd.Series, start_y: pd.Series, ball_x: pd.Series, ball_y: pd.Series) -> pd.Series:
    out = pd.Series("Unknown", index=start_x.index, dtype=object)
    ok = start_x.notna() & start_y.notna() & ball_x.notna() & ball_y.notna()
    if not ok.any():
        return out

    fx = -start_x[ok]
    fy = -start_y[ok]
    flen = np.sqrt(fx**2 + fy**2)
    valid = flen.gt(0) & np.isfinite(flen)
    idx = flen[valid].index
    if not len(idx):
        return out

    fx = fx.loc[idx] / flen.loc[idx]
    fy = fy.loc[idx] / flen.loc[idx]
    rx = fy
    ry = -fx
    bvx = ball_x.loc[idx] - start_x.loc[idx]
    bvy = ball_y.loc[idx] - start_y.loc[idx]
    side = bvx * rx + bvy * ry
    forward = bvx * fx + bvy * fy
    angle = np.degrees(np.arctan2(side, forward))
    back_angle = np.where(angle >= 0, angle - 180, angle + 180)
    out.loc[idx] = np.select(
        [
            (np.abs(angle) <= 90) & (angle < -30),
            (np.abs(angle) <= 90) & (angle > 30),
            np.abs(angle) <= 90,
            (np.abs(angle) > 90) & (back_angle < -30),
            (np.abs(angle) > 90) & (back_angle > 30),
        ],
        ["In Left", "In Right", "In", "Back Left", "Back Right"],
        default="Back",
    )
    return out


def cut_bin(values: pd.Series, bins: list[float], labels: list[str]) -> pd.Series:
    out = pd.cut(pd.to_numeric(values, errors="coerce"), bins=bins, labels=labels, include_lowest=True, right=False)
    out = out.astype(object).where(pd.notna(out), "Unknown")
    return out.astype(str)


def read_hit_data() -> pd.DataFrame:
    table = pq.read_table(HIT_PATH, columns=HIT_COLUMNS, use_threads=True)
    hit = table.to_pandas()
    hit = hit[hit["PitchUID"].notna()].copy()
    hit["PitchCallCanon"] = canon_pitch_call(hit["PitchCall"])
    hit = hit[included_bip_result(hit["PitchCall"], hit["PlayResult"])].copy()
    hit["made_play"] = made_play_from_result(hit["PlayResult"], hit["OutsOnPlay"])
    hit = hit[hit["made_play"].notna()].copy()
    hit["play_bucket"] = play_result_bucket(hit["PlayResult"], hit["made_play"])
    hit["launch_angle"] = pd.to_numeric(hit["Angle"], errors="coerce")
    hit["exit_speed"] = pd.to_numeric(hit["ExitSpeed"], errors="coerce")
    hit["distance"] = pd.to_numeric(hit["Distance"], errors="coerce")
    hit["bearing"] = pd.to_numeric(hit["Bearing"], errors="coerce").fillna(pd.to_numeric(hit["Direction"], errors="coerce"))
    hit["hang_time"] = pd.to_numeric(hit["HangTime"], errors="coerce")
    hit["batted_ball_bucket"] = batted_ball_bucket(
        hit["TaggedHitType"].where(hit["TaggedHitType"].fillna("").astype(str).ne("Undefined"), hit["AutoHitType"]),
        hit["launch_angle"],
    )
    hit = hit[hit["distance"].notna() & hit["bearing"].notna() & hit["batted_ball_bucket"].ne("Unknown")].copy()
    hit.loc[hit["batted_ball_bucket"].eq("Ground"), "distance"] = 135.0
    rad = np.radians(hit["bearing"])
    hit["ball_x"] = hit["distance"] * np.cos(rad)
    hit["ball_y"] = hit["distance"] * np.sin(rad)
    return hit.drop_duplicates("PitchUID")


def build_opportunity_rows(hit: pd.DataFrame) -> pd.DataFrame:
    defense_file = pq.ParquetFile(DEFENSE_PATH)
    rows: list[pd.DataFrame] = []
    keep_cols = [
        "PitchUID",
        "batted_ball_bucket",
        "play_bucket",
        "made_play",
        "distance",
        "bearing",
        "launch_angle",
        "exit_speed",
        "hang_time",
        "ball_x",
        "ball_y",
    ]
    hit_small = hit[keep_cols]

    for group_idx in range(defense_file.metadata.num_row_groups):
        defense = defense_file.read_row_group(group_idx, columns=DEFENSE_COLUMNS).to_pandas()
        merged = defense.merge(hit_small, on="PitchUID", how="inner")
        if merged.empty:
            continue

        for pos in POSITIONS:
            sx = pd.to_numeric(merged[f"{pos}_PositionAtReleaseX"], errors="coerce")
            sy = pd.to_numeric(merged[f"{pos}_PositionAtReleaseZ"], errors="coerce")
            zone = opportunity_zone(pos, merged["batted_ball_bucket"], merged["bearing"], merged["distance"], merged["made_play"])
            opp = zone.ne("")
            if pos in OUTFIELD:
                opp &= ~merged["batted_ball_bucket"].eq("Ground")
                opp &= ~(merged["batted_ball_bucket"].isin(["Fly", "Line", "Popup"]) & merged["made_play"].astype(bool) & merged["distance"].le(150))
            opp &= sx.notna() & sy.notna()
            if not opp.any():
                continue

            tmp = merged.loc[opp, keep_cols].copy()
            tmp["position"] = pos
            tmp["opportunity_zone"] = zone.loc[opp].values
            tmp["range_sector"] = range_sector(sx.loc[opp], sy.loc[opp], tmp["ball_x"], tmp["ball_y"]).values
            rows.append(tmp)

    if not rows:
        return pd.DataFrame()

    opp = pd.concat(rows, ignore_index=True)
    opp["made"] = opp["made_play"].astype(bool).astype(int)
    opp["distance_bin"] = cut_bin(opp["distance"], [-np.inf, 60, 90, 120, 150, 180, 220, 260, 320, np.inf], ["<60", "60-89", "90-119", "120-149", "150-179", "180-219", "220-259", "260-319", "320+"])
    opp["bearing_bin"] = cut_bin(opp["bearing"], [-np.inf, -35, -25, -15, -5, 5, 15, 25, 35, np.inf], ["<-35", "-35--25", "-25--15", "-15--5", "-5-5", "5-15", "15-25", "25-35", "35+"])
    opp["launch_angle_bin"] = cut_bin(opp["launch_angle"], [-np.inf, 0, 10, 20, 30, 45, np.inf], ["<0", "0-9", "10-19", "20-29", "30-44", "45+"])
    opp["exit_speed_bin"] = cut_bin(opp["exit_speed"], [-np.inf, 75, 85, 95, 105, np.inf], ["<75", "75-84", "85-94", "95-104", "105+"])
    opp["hang_time_bin"] = cut_bin(opp["hang_time"], [-np.inf, 2, 3, 4, 5, np.inf], ["<2", "2-2.9", "3-3.9", "4-4.9", "5+"])
    opp.loc[opp["batted_ball_bucket"].eq("Ground"), "hang_time_bin"] = "Ground"
    return opp


def summarise_level(opp: pd.DataFrame, level: str, keys: list[str], min_chances: int) -> pd.DataFrame:
    grouped = opp.groupby(keys, dropna=False, as_index=False).agg(chances=("made", "size"), made=("made", "sum"))
    grouped = grouped[grouped["chances"].ge(min_chances)].copy()
    grouped["play_probability"] = grouped["made"] / grouped["chances"]
    grouped["level"] = level
    for col in BASE_COLUMNS:
        if col not in grouped.columns:
            grouped[col] = "All" if col.endswith("_bin") or col in {"opportunity_zone", "range_sector", "batted_ball_bucket"} else ""
    return grouped[BASE_COLUMNS]


def main() -> None:
    missing = [str(path) for path in (DEFENSE_PATH, HIT_PATH) if not path.exists()]
    if missing:
        raise SystemExit("Missing D1 parquet file(s): " + ", ".join(missing))

    hit = read_hit_data()
    print(f"Loaded {len(hit):,} D1 batted-ball rows")
    opp = build_opportunity_rows(hit)
    print(f"Built {len(opp):,} D1 opportunity rows")
    if opp.empty:
        raise SystemExit("No D1 opportunities could be built from paired files.")

    levels = [
        ("detail", ["position", "batted_ball_bucket", "opportunity_zone", "distance_bin", "bearing_bin", "launch_angle_bin", "exit_speed_bin", "hang_time_bin", "range_sector"], 20),
        ("geometry", ["position", "batted_ball_bucket", "opportunity_zone", "distance_bin", "bearing_bin"], 30),
        ("range", ["position", "batted_ball_bucket", "range_sector", "distance_bin"], 30),
        ("zone", ["position", "batted_ball_bucket", "opportunity_zone"], 20),
        ("position_type", ["position", "batted_ball_bucket"], 20),
        ("position", ["position"], 20),
    ]
    baseline = pd.concat([summarise_level(opp, level, keys, min_chances) for level, keys, min_chances in levels], ignore_index=True)
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    baseline.to_csv(OUT_PATH, index=False)
    print(f"Wrote {len(baseline):,} baseline rows to {OUT_PATH}")


if __name__ == "__main__":
    main()
