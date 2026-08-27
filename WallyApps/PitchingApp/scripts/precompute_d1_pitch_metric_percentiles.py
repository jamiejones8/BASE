#!/usr/bin/env python3
"""Build a small D1 percentile reference for the Shiny Pitch Metrics page.

The app runtime does not have R parquet support installed, so this script reads
the large D1 parquet once with pyarrow and writes a compact CSV that Shiny can
load quickly.
"""

from __future__ import annotations

from pathlib import Path
import re

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


ROOT = Path(__file__).resolve().parents[1]
INFILE_CANDIDATES = [
    ROOT.parent / "D1 Files" / "D1 Pitching:Hitting File.parquet",
    ROOT.parent / "D1 Files" / "D1 Data File.parquet",
    ROOT / "data" / "D1 Data File.parquet",
]
INFILE = next((path for path in INFILE_CANDIDATES if path.exists()), INFILE_CANDIDATES[0])
OUTFILE = ROOT / "data" / "d1_pitch_metric_percentile_reference.csv"
MIN_PITCH_TYPE_PITCHES = 50
FIP_CONST = 3.214
BARREL_EV_MIN = 95
BARREL_LA_MIN = 5
BARREL_LA_MAX = 40

WOBA = {
    "BB": 0.690,
    "HBP": 0.720,
    "X1B": 0.880,
    "X2B": 1.247,
    "X3B": 1.578,
    "HR": 2.031,
}


def canon_pitch_type(s: pd.Series) -> pd.Series:
    x = s.fillna("").astype(str).str.strip()
    lo = x.str.lower().str.replace(r"[\s_-]+", "", regex=True)
    out = x.copy()
    mapping = {
        "changeup": "Changeup",
        "change": "Changeup",
        "changeups": "Changeup",
        "fastball": "Fastball",
        "fourseam": "Fastball",
        "four-seam": "Fastball",
        "4seam": "Fastball",
        "4-seam": "Fastball",
        "sinker": "Sinker",
        "twoseam": "Sinker",
        "two-seam": "Sinker",
        "2seam": "Sinker",
        "2-seam": "Sinker",
        "slider": "Slider",
        "sweeper": "Sweeper",
        "curveball": "Curveball",
        "curve": "Curveball",
        "cutter": "Cutter",
        "splitter": "Splitter",
        "split": "Splitter",
    }
    for key, val in mapping.items():
        out.loc[lo == key.replace("-", "")] = val
    return out


def is_bad_pitch_type(s: pd.Series) -> pd.Series:
    lo = s.fillna("").astype(str).str.strip().str.lower()
    return lo.isin({"", "undefined", "other", "untagged", "unknown", "nan", "none"})


def safe_bool_contains(s: pd.Series, pattern: str) -> pd.Series:
    return s.fillna("").astype(str).str.contains(pattern, case=False, regex=True)


def safe_div(num, den):
    num = pd.Series(num, copy=False)
    den = pd.Series(den, copy=False).replace(0, np.nan)
    return num / den


def scalar_div(num: float, den: float) -> float:
    return float(num) / float(den) if pd.notna(den) and float(den) != 0 else np.nan


def choose_columns(path: Path, wanted: list[str]) -> list[str]:
    available = set(pq.ParquetFile(path).schema_arrow.names)
    return [col for col in wanted if col in available]


def derive_zone(df: pd.DataFrame) -> pd.Series:
    height = pd.to_numeric(df["PlateLocHeight"], errors="coerce")
    side = pd.to_numeric(df["PlateLocSide"], errors="coerce")
    height_inches = height.where(~(height.notna() & (height < 10)), height * 12)
    side_inches = side.where(~(side.notna() & (side.abs() < 5)), side * 12)
    known = height_inches.notna() & side_inches.notna()
    in_zone = (
        height_inches.between(18.29, 44.08, inclusive="both")
        & side_inches.between(-9.97, 9.97, inclusive="both")
    )
    out = pd.Series(pd.NA, index=df.index, dtype="boolean")
    out.loc[known] = in_zone.loc[known]
    return out


def is_strike_call(pc: pd.Series) -> pd.Series:
    # Keep the D1 reference definition identical to the app's prepare_flags():
    # every swing result, including a ball put in play, is a strike outcome.
    compact = pc.fillna("").astype(str).str.replace(r"[\s_-]+", "", regex=True).str.lower()
    return compact.isin(
        {
            "strikecalled",
            "strikeswinging",
            "foulball",
            "foulballfieldable",
            "foulballnotfieldable",
            "foultip",
            "inplay",
            "inplayout",
            "inplaynoout",
        }
    )


def infer_outs(pa: pd.DataFrame, outcomes: pd.DataFrame) -> pd.Series:
    pr = outcomes["PR_txt"]
    if "OutsOnPlay" in pa.columns:
        outs_play = pd.to_numeric(pa["OutsOnPlay"], errors="coerce").fillna(0).astype(int)
    else:
        outs_play = pd.Series(0, index=pa.index)
    outs_play = outs_play.mask(pr.str.contains(r"triple\s*play", case=False, regex=True), 3)
    outs_play = outs_play.mask(pr.str.contains(r"double\s*play", case=False, regex=True), np.maximum(outs_play, 2))
    outs_play = outs_play.mask(
        pr.str.contains(r"\bout\b", case=False, regex=True) & ~outcomes["K"],
        np.maximum(outs_play, 1),
    )
    return outs_play.astype(int) + outcomes["K"].astype(int)


def add_performance_metrics(rows: list[dict], pitcher: pd.DataFrame) -> None:
    pa_base = pitcher["PA"] >= 50
    pitch_base = pitcher["Pitches"] >= 100
    ip_base = pa_base & (pitcher["IP"] > 0)
    bip_base = pitcher["BIP_EVLA"] >= 20
    bip_la_base = pitcher["BIP_LA"] >= 20

    metric_specs = [
        ("performance_baa", "BAA", pa_base, "PA"),
        ("performance_slg", "SLG", pa_base, "PA"),
        ("performance_ops", "OPS", pa_base, "PA"),
        ("performance_whip", "WHIP", ip_base, "PA"),
        ("performance_k9", "K9", ip_base, "PA"),
        ("performance_bb9", "BB9", ip_base, "PA"),
        ("performance_h9", "H9", ip_base, "PA"),
        ("performance_fip", "FIP", ip_base, "PA"),
        ("performance_woba", "wOBA", pa_base, "PA"),
        ("performance_wobacon", "wOBAcon", pitcher["BIP_pa"] >= 20, "BIP_pa"),
        ("performance_prv", "pRV", pa_base, "Pitches"),
        ("performance_k_pct", "K_pct", pa_base, "PA"),
        ("performance_bb_pct", "BB_pct", pa_base, "PA"),
        ("performance_bb_hbp_pct", "BB_HBP_pct", pa_base, "PA"),
        ("performance_barrel_pct", "Barrel_pct", bip_base, "BIP_EVLA"),
        ("performance_gb_pct", "GB_pct", bip_la_base, "BIP_LA"),
        ("performance_strike_pct", "Strike_pct", pitch_base, "Pitches"),
        ("performance_zone_pct", "Zone_pct", pitch_base, "Pitches"),
        ("performance_fps_pct", "FPS_pct", pa_base, "PA"),
        ("performance_ea_pct", "EA_pct", pa_base, "PA"),
        ("performance_pre2k_zone_pct", "Pre2kZone_pct", pitcher["Pre2kZone_den"] >= 25, "Pre2kZone_den"),
        ("performance_2k_zone_pct", "TwoKZone_pct", pitcher["TwoKZone_den"] >= 10, "TwoKZone_den"),
        ("performance_put_away_pct", "PutAway_pct", pitcher["PutAway_den"] >= 10, "PutAway_den"),
        ("performance_win11_pct", "Win11_pct", pitcher["Win11_den"] >= 10, "Win11_den"),
        ("performance_csw_pct", "CSW_pct", pitch_base, "Pitches"),
        ("performance_whiff_pct", "Whiff_pct", pitcher["Swings"] >= 25, "Swings"),
        ("performance_izwhiff_pct", "IZWhiff_pct", pitcher["IZSwings"] >= 10, "IZSwings"),
        ("performance_chase_pct", "Chase_pct", pitcher["OOZ"] >= 25, "OOZ"),
    ]

    for metric, col, mask, sample_col in metric_specs:
        add_metric(rows, "overall", metric, pitcher.loc[mask, col], pitcher.loc[mask, sample_col])


def pa_outcomes(pa: pd.DataFrame) -> pd.DataFrame:
    pr = pa["PlayResult"].fillna("").astype(str).str.strip()
    kb = pa["KorBB"].fillna("").astype(str).str.strip()
    pc = pa["PitchCall"].fillna("").astype(str).str.strip()
    pc_compact = pc.str.lower().str.replace(r"\s+", "", regex=True)

    is_k = (
        pr.str.contains(r"strike.?out|\bK\b", case=False, regex=True)
        | kb.str.contains(r"\bK\b|strikeout", case=False, regex=True)
    )
    is_bb = (
        kb.str.contains(r"\bwalk\b|\bbb\b", case=False, regex=True)
        | pr.str.contains(r"\bwalk\b", case=False, regex=True)
    )
    is_ibb = (
        kb.str.contains(r"intentional|\bibb\b", case=False, regex=True)
        | pr.str.contains(r"intentional", case=False, regex=True)
    )
    is_hbp = (
        pr.str.contains(r"hit by pitch|\bhbp\b", case=False, regex=True)
        | kb.str.contains(r"\bhbp\b", case=False, regex=True)
        | pc_compact.isin({"hitbypitch", "hbp"})
    )
    is_hr = pr.str.contains(r"home\s*run|\bhr\b", case=False, regex=True)
    is_3b = pr.str.contains(r"\btriple\b", case=False, regex=True)
    is_2b = pr.str.contains(r"\bdouble\b", case=False, regex=True) & ~pr.str.contains(
        r"double\s*play", case=False, regex=True
    )
    is_1b = pr.str.contains(r"\bsingle\b", case=False, regex=True)
    is_sf = pr.str.contains(r"sacrifice\s*fly|\bsf\b", case=False, regex=True)
    is_bip = (
        pc.str.contains(r"^in\s*play|InPlay", case=False, regex=True)
        | pr.str.contains(
            r"in\s*play|single|double|triple|home\s*run|\bhr\b|ground|fly|line|pop|error|reach|sac",
            case=False,
            regex=True,
        )
    )

    out = pd.DataFrame(
        {
            "K": is_k,
            "BB": is_bb & ~is_ibb,
            "AnyBB": is_bb,
            "HBP": is_hbp,
            "X1B": is_1b,
            "X2B": is_2b,
            "X3B": is_3b,
            "HR": is_hr,
            "SF": is_sf,
            "BIP_pa": is_bip,
            "PR_txt": pr,
        }
    )
    out["woba_num"] = (
        WOBA["BB"] * out["BB"].astype(float)
        + WOBA["HBP"] * out["HBP"].astype(float)
        + WOBA["X1B"] * out["X1B"].astype(float)
        + WOBA["X2B"] * out["X2B"].astype(float)
        + WOBA["X3B"] * out["X3B"].astype(float)
        + WOBA["HR"] * out["HR"].astype(float)
    )
    out["woba_den"] = (
        out["BB"].astype(float)
        + out["HBP"].astype(float)
        + out["BIP_pa"].astype(float)
        + out["K"].astype(float)
    )
    return out


def add_metric(rows: list[dict], scope: str, metric: str, values: pd.Series, sample: pd.Series, pitch_type: str = "") -> None:
    tmp = pd.DataFrame({"value": values, "sample_n": sample})
    tmp = tmp.replace([np.inf, -np.inf], np.nan).dropna(subset=["value"])
    for val, n in tmp[["value", "sample_n"]].itertuples(index=False):
        rows.append(
            {
                "scope": scope,
                "pitch_type": pitch_type,
                "metric": metric,
                "value": float(val),
                "sample_n": int(n) if pd.notna(n) else 0,
            }
        )


def main() -> None:
    cols = [
        "PitchNo",
        "Date",
        "PAofInning",
        "PitchofPA",
        "Pitcher",
        "PitcherThrows",
        "PitcherTeam",
        "Batter",
        "Inning",
        "Top/Bottom",
        "TaggedPitchType",
        "AutoPitchType",
        "ModelPitchType",
        "ModelPitchTypeRaw",
        "PitchCall",
        "KorBB",
        "PlayResult",
        "RelSpeed",
        "SpinRate",
        "RelHeight",
        "RelSide",
        "Extension",
        "InducedVertBreak",
        "HorzBreak",
        "PlateLocHeight",
        "PlateLocSide",
        "VertApprAngle",
        "HorzApprAngle",
        "ExitSpeed",
        "GameID",
        "GameUID",
    ]
    optional_cols = [
        "Angle",
        "OutsOnPlay",
        "BBType",
        "TaggedHitType",
        "Balls",
        "Strikes",
        "BallsBeforePitch",
        "StrikesBeforePitch",
        "BallsPre",
        "StrikesPre",
        "RunsScored",
    ]
    df = pq.read_table(INFILE, columns=choose_columns(INFILE, cols + optional_cols)).to_pandas()
    for col in cols + optional_cols:
        if col not in df.columns:
            df[col] = np.nan

    model = canon_pitch_type(df["ModelPitchType"])
    model_raw = canon_pitch_type(df["ModelPitchTypeRaw"])
    tagged = canon_pitch_type(df["TaggedPitchType"])
    auto = canon_pitch_type(df["AutoPitchType"])
    df["PitchTypeStd"] = model
    df["PitchTypeStd"] = df["PitchTypeStd"].mask(is_bad_pitch_type(df["PitchTypeStd"]), model_raw)
    df["PitchTypeStd"] = df["PitchTypeStd"].mask(is_bad_pitch_type(df["PitchTypeStd"]), tagged)
    df["PitchTypeStd"] = df["PitchTypeStd"].mask(is_bad_pitch_type(df["PitchTypeStd"]), auto)
    df = df.loc[~is_bad_pitch_type(df["PitchTypeStd"])].copy()

    for col in [
        "RelSpeed",
        "SpinRate",
        "RelHeight",
        "RelSide",
        "Extension",
        "InducedVertBreak",
        "HorzBreak",
        "PlateLocHeight",
        "PlateLocSide",
        "VertApprAngle",
        "HorzApprAngle",
        "ExitSpeed",
        "Angle",
        "PitchNo",
        "PitchofPA",
        "PAofInning",
        "Inning",
        "Balls",
        "Strikes",
        "BallsBeforePitch",
        "StrikesBeforePitch",
        "BallsPre",
        "StrikesPre",
        "RunsScored",
    ]:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    df.loc[(df["SpinRate"] < 500) | (df["SpinRate"] > 3800), "SpinRate"] = np.nan

    df["PitcherKey"] = (
        df["Pitcher"].fillna("").astype(str).str.strip()
        + " | "
        + df["PitcherTeam"].fillna("").astype(str).str.strip()
    )
    df = df.loc[df["PitcherKey"].str.strip().ne("|")].copy()
    throw_raw = df["PitcherThrows"].fillna("").astype(str).str.strip().str.lower()
    throw_side = np.where(
        throw_raw.str.startswith("l"),
        "L",
        np.where(throw_raw.str.startswith("r"), "R", ""),
    )
    fb_hb_mean = df.loc[df["PitchTypeStd"].isin(["Fastball", "Sinker"])].groupby("PitcherKey")["HorzBreak"].mean()
    inferred_throw = np.where(fb_hb_mean < 0, "L", "R")
    inferred_throw = pd.Series(inferred_throw, index=fb_hb_mean.index)
    df["ThrowSide"] = throw_side
    missing_throw = df["ThrowSide"].eq("")
    df.loc[missing_throw, "ThrowSide"] = df.loc[missing_throw, "PitcherKey"].map(inferred_throw).fillna("R")
    df["ArmSideHB"] = df["HorzBreak"] * np.where(df["ThrowSide"].eq("L"), -1, 1)
    df["HBForPercentile"] = np.where(df["PitchTypeStd"].eq("Fastball"), df["ArmSideHB"], np.abs(df["HorzBreak"]))
    df["IsHeater"] = df["PitchTypeStd"].isin(["Fastball", "Sinker"])
    sort_cols = ["GameID", "GameUID", "Date", "Inning", "Top/Bottom", "PAofInning", "PitchNo", "PitchofPA"]
    df = df.sort_values(sort_cols, kind="mergesort")
    pa_keys = ["GameID", "GameUID", "Inning", "Top/Bottom", "PAofInning", "PitcherKey", "Batter"]

    pc = df["PitchCall"].fillna("").astype(str)
    df["IsSwing"] = pc.str.contains("swing|foul|inplay", case=False, regex=True)
    df["IsWhiff"] = pc.str.contains("StrikeSwinging|swinging strike", case=False, regex=True)
    df["IsStrike"] = is_strike_call(pc)
    df["InZone"] = derive_zone(df)
    df["IsChase"] = (~df["InZone"]) & df["IsSwing"]
    df["IsBIP"] = safe_bool_contains(df["PitchCall"], r"^in\s*play|InPlay") | safe_bool_contains(
        df["PlayResult"], r"in\s*play|single|double|triple|home\s*run|\bhr\b|ground|fly|line|pop|error|reach|sac"
    )
    pc_compact = pc.str.replace(r"\s+", "", regex=True).str.lower()
    df["IsHBP"] = (
        safe_bool_contains(df["PlayResult"], r"hit by pitch|\bhbp\b")
        | safe_bool_contains(df["KorBB"], r"\bhbp\b")
        | pc_compact.isin(["hitbypitch", "hbp"])
    )
    df["IsBarrel"] = df["IsBIP"] & df["ExitSpeed"].ge(BARREL_EV_MIN) & df["Angle"].between(BARREL_LA_MIN, BARREL_LA_MAX)
    df["IsGB"] = df["IsBIP"] & df["Angle"].lt(5)
    df["BIP_EVLA"] = df["IsBIP"] & np.isfinite(df["ExitSpeed"]) & np.isfinite(df["Angle"])
    df["BIP_LA"] = df["IsBIP"] & np.isfinite(df["Angle"])

    df["PitchNum"] = df["PitchofPA"]
    missing_pitch_num = ~np.isfinite(df["PitchNum"])
    if missing_pitch_num.any():
        df.loc[missing_pitch_num, "PitchNum"] = df.loc[missing_pitch_num].groupby(pa_keys, dropna=False).cumcount() + 1
    df["FirstPitch"] = df["PitchNum"].eq(1)
    grouped_pa = df.groupby(pa_keys, dropna=False)
    df["BallsPre_calc"] = grouped_pa["Balls"].shift(1).fillna(0)
    df["StrikesPre_calc"] = grouped_pa["Strikes"].shift(1).fillna(0)
    df["BallsPre_calc"] = df["BallsPre"].where(np.isfinite(df["BallsPre"]), df["BallsPre_calc"])
    df["BallsPre_calc"] = df["BallsBeforePitch"].where(np.isfinite(df["BallsBeforePitch"]), df["BallsPre_calc"])
    df["StrikesPre_calc"] = df["StrikesPre"].where(np.isfinite(df["StrikesPre"]), df["StrikesPre_calc"])
    df["StrikesPre_calc"] = df["StrikesBeforePitch"].where(np.isfinite(df["StrikesBeforePitch"]), df["StrikesPre_calc"])
    df["Pre2k"] = np.isfinite(df["StrikesPre_calc"]) & (df["StrikesPre_calc"] < 2)
    df["TwoK"] = np.isfinite(df["StrikesPre_calc"]) & (df["StrikesPre_calc"] == 2)
    df["TwoKNo32"] = df["TwoK"] & np.isfinite(df["BallsPre_calc"]) & (df["BallsPre_calc"] != 3)
    df["At11"] = df["BallsPre_calc"].eq(1) & df["StrikesPre_calc"].eq(1)
    count_strike = pc.fillna("").astype(str).isin(
        ["StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip"]
    )
    df["Win11"] = df["At11"] & count_strike
    df["ZoneKnown"] = np.isfinite(df["PlateLocHeight"]) & np.isfinite(df["PlateLocSide"])

    pitch_counts = df.groupby("PitcherKey").size().rename("Pitches")
    pitcher = df.groupby("PitcherKey").agg(
        Extension=("Extension", "mean"),
        RelHeight=("RelHeight", "mean"),
        RelSideAbs=("RelSide", lambda x: np.nanmean(np.abs(x))),
        Whiffs=("IsWhiff", "sum"),
        Swings=("IsSwing", "sum"),
        Chases=("IsChase", "sum"),
        OOZ=("InZone", lambda x: int((~x).sum())),
        AvgEV=("ExitSpeed", "mean"),
        EVCount=("ExitSpeed", lambda x: int(np.isfinite(x).sum())),
        Strikes=("IsStrike", "sum"),
        ZonePitches=("ZoneKnown", "sum"),
        Zone=("InZone", "sum"),
        CSW=("PitchCall", lambda x: int(x.fillna("").astype(str).isin(["StrikeSwinging", "StrikeCalled"]).sum())),
        BIP_EVLA=("BIP_EVLA", "sum"),
        Barrels=("IsBarrel", "sum"),
        BIP_LA=("BIP_LA", "sum"),
        GB=("IsGB", "sum"),
        Pre2kZone_den=("Pre2k", lambda x: int((x & df.loc[x.index, "ZoneKnown"]).sum())),
        Pre2kZone_num=("Pre2k", lambda x: int((x & df.loc[x.index, "InZone"]).sum())),
        TwoKZone_den=("TwoKNo32", lambda x: int((x & df.loc[x.index, "ZoneKnown"]).sum())),
        TwoKZone_num=("TwoKNo32", lambda x: int((x & df.loc[x.index, "InZone"]).sum())),
        PutAway_den=("TwoK", "sum"),
        Win11_den=("At11", "sum"),
        Win11_num=("Win11", "sum"),
        IZSwings=("IsSwing", lambda x: int((x & df.loc[x.index, "InZone"]).sum())),
        IZWhiffs=("IsWhiff", lambda x: int((x & df.loc[x.index, "InZone"]).sum())),
    )
    pitcher = pitcher.join(pitch_counts)
    heater = (
        df.loc[df["IsHeater"]]
        .groupby("PitcherKey")
        .agg(
            HeaterVelocity=("RelSpeed", "mean"),
            HeaterExtension=("Extension", "mean"),
            HeaterPitches=("RelSpeed", lambda x: int(np.isfinite(x).sum())),
        )
    )
    pitcher = pitcher.join(heater)
    pitcher["WhiffRate"] = pitcher["Whiffs"] / pitcher["Swings"].replace(0, np.nan)
    pitcher["ChaseRate"] = pitcher["Chases"] / pitcher["OOZ"].replace(0, np.nan)
    pitcher["Whiff_pct"] = pitcher["WhiffRate"]
    pitcher["Chase_pct"] = pitcher["ChaseRate"]
    pitcher["Strike_pct"] = pitcher["Strikes"] / pitcher["Pitches"].replace(0, np.nan)
    pitcher["Zone_pct"] = pitcher["Zone"] / pitcher["ZonePitches"].replace(0, np.nan)
    pitcher["CSW_pct"] = pitcher["CSW"] / pitcher["Pitches"].replace(0, np.nan)
    pitcher["Barrel_pct"] = pitcher["Barrels"] / pitcher["BIP_EVLA"].replace(0, np.nan)
    pitcher["GB_pct"] = pitcher["GB"] / pitcher["BIP_LA"].replace(0, np.nan)
    pitcher["Pre2kZone_pct"] = pitcher["Pre2kZone_num"] / pitcher["Pre2kZone_den"].replace(0, np.nan)
    pitcher["TwoKZone_pct"] = pitcher["TwoKZone_num"] / pitcher["TwoKZone_den"].replace(0, np.nan)
    pitcher["Win11_pct"] = pitcher["Win11_num"] / pitcher["Win11_den"].replace(0, np.nan)
    pitcher["IZWhiff_pct"] = pitcher["IZWhiffs"] / pitcher["IZSwings"].replace(0, np.nan)

    pa = df.groupby(pa_keys, dropna=False).tail(1).copy()
    pa_out = pa_outcomes(pa)
    pa["woba_num"] = pa_out["woba_num"].to_numpy()
    pa["woba_den"] = pa_out["woba_den"].to_numpy()
    pa["wobacon_num"] = (
        WOBA["X1B"] * pa_out["X1B"].astype(float).to_numpy()
        + WOBA["X2B"] * pa_out["X2B"].astype(float).to_numpy()
        + WOBA["X3B"] * pa_out["X3B"].astype(float).to_numpy()
        + WOBA["HR"] * pa_out["HR"].astype(float).to_numpy()
    )
    pa["wobacon_den"] = pa_out["BIP_pa"].astype(float).to_numpy()
    pa["K_pa"] = pa_out["K"].astype(float).to_numpy()
    pa["BB_pa"] = pa_out["AnyBB"].astype(float).to_numpy()
    pa["FIP_BB_pa"] = pa_out["BB"].astype(float).to_numpy()
    pa["HBP_pa"] = pa_out["HBP"].astype(float).to_numpy()
    pa["H_pa"] = (pa_out["X1B"] | pa_out["X2B"] | pa_out["X3B"] | pa_out["HR"]).astype(float).to_numpy()
    pa["TB_pa"] = (
        pa_out["X1B"].astype(float).to_numpy()
        + 2 * pa_out["X2B"].astype(float).to_numpy()
        + 3 * pa_out["X3B"].astype(float).to_numpy()
        + 4 * pa_out["HR"].astype(float).to_numpy()
    )
    pa["HR_pa"] = pa_out["HR"].astype(float).to_numpy()
    pa["SF_pa"] = pa_out["SF"].astype(float).to_numpy()
    pa["BIP_pa"] = pa_out["BIP_pa"].astype(float).to_numpy()
    pa["Outs_pa"] = infer_outs(pa, pa_out).to_numpy()
    pa["AB_pa"] = np.maximum(1 - pa["BB_pa"] - pa["HBP_pa"] - pa["SF_pa"], 0)
    pa["RBI_pa"] = np.where(
        pa_out["BIP_pa"].to_numpy(),
        pd.to_numeric(pa["RunsScored"], errors="coerce").fillna(0).to_numpy(),
        0,
    )
    pa_first = df.groupby(pa_keys, dropna=False).head(1).copy()
    pa_first["FPS_pa"] = pa_first["IsStrike"].astype(float)

    pa_stats = pa.groupby("PitcherKey").agg(
        PA=("PitcherKey", "size"),
        K=("K_pa", "sum"),
        BB=("BB_pa", "sum"),
        FIP_BB=("FIP_BB_pa", "sum"),
        HBP=("HBP_pa", "sum"),
        H=("H_pa", "sum"),
        TB=("TB_pa", "sum"),
        HR=("HR_pa", "sum"),
        SF=("SF_pa", "sum"),
        AB=("AB_pa", "sum"),
        Outs=("Outs_pa", "sum"),
        RBI=("RBI_pa", "sum"),
        woba_num=("woba_num", "sum"),
        woba_den=("woba_den", "sum"),
        wobacon_num=("wobacon_num", "sum"),
        wobacon_den=("wobacon_den", "sum"),
        BIP_pa=("BIP_pa", "sum"),
    )
    fps = pa_first.groupby("PitcherKey").agg(FPS_pct=("FPS_pa", "mean"))
    pa_stats = pa_stats.join(fps)
    pa_stats["IP"] = pa_stats["Outs"] / 3
    pa_stats["BAA"] = pa_stats["H"] / pa_stats["AB"].replace(0, np.nan)
    pa_stats["SLG"] = pa_stats["TB"] / pa_stats["AB"].replace(0, np.nan)
    pa_stats["OBP"] = (pa_stats["H"] + pa_stats["BB"] + pa_stats["HBP"]) / (
        pa_stats["AB"] + pa_stats["BB"] + pa_stats["HBP"] + pa_stats["SF"]
    ).replace(0, np.nan)
    pa_stats["OPS"] = pa_stats["OBP"] + pa_stats["SLG"]
    pa_stats["WHIP"] = (pa_stats["BB"] + pa_stats["H"]) / pa_stats["IP"].replace(0, np.nan)
    pa_stats["K9"] = pa_stats["K"] * 9 / pa_stats["IP"].replace(0, np.nan)
    pa_stats["BB9"] = pa_stats["BB"] * 9 / pa_stats["IP"].replace(0, np.nan)
    pa_stats["H9"] = pa_stats["H"] * 9 / pa_stats["IP"].replace(0, np.nan)
    pa_stats["FIP"] = (13 * pa_stats["HR"] + 3 * (pa_stats["FIP_BB"] + pa_stats["HBP"]) - 2 * pa_stats["K"]) / pa_stats["IP"].replace(0, np.nan) + FIP_CONST
    pa_stats["wOBA"] = pa_stats["woba_num"] / pa_stats["woba_den"].replace(0, np.nan)
    pa_stats["wOBAcon"] = pa_stats["wobacon_num"] / pa_stats["wobacon_den"].replace(0, np.nan)
    pa_stats["K_pct"] = pa_stats["K"] / pa_stats["PA"].replace(0, np.nan)
    pa_stats["BB_pct"] = pa_stats["BB"] / pa_stats["PA"].replace(0, np.nan)
    pa_stats["BB_HBP_pct"] = (pa_stats["BB"] + pa_stats["HBP"]) / pa_stats["PA"].replace(0, np.nan)
    pa_stats["pRV"] = ((((pa_stats["TB"] + pa_stats["BB"] - pa_stats["K"]) / 4) + pa_stats["RBI"] + pa_stats["HR"]) / pitcher["Pitches"].replace(0, np.nan)) * 100

    ea_df = df.copy()
    ea_df["ea_first3"] = ea_df["PitchNum"].le(3)
    ea = ea_df.groupby(pa_keys, dropna=False).agg(
        PitcherKey=("PitcherKey", "first"),
        n_pitches=("PitchNum", "max"),
        strikes_first3=("IsStrike", lambda x: int((x & ea_df.loc[x.index, "ea_first3"]).sum())),
        early_bip=("IsBIP", lambda x: bool((x & ea_df.loc[x.index, "ea_first3"]).any())),
        any_hbp=("IsHBP", lambda x: bool((x & ea_df.loc[x.index, "ea_first3"]).any())),
        any_barrel=("IsBarrel", lambda x: bool((x & ea_df.loc[x.index, "ea_first3"]).any())),
    )
    ea["EA_success"] = (
        (ea["n_pitches"] > 0)
        & ~ea["any_hbp"]
        & ~ea["any_barrel"]
        & (ea["early_bip"] | (ea["strikes_first3"] >= 2))
    )
    ea_rate = ea.reset_index(drop=True).groupby("PitcherKey").agg(EA_pct=("EA_success", "mean"))
    pitcher = pitcher.join(pa_stats).join(ea_rate)
    put_away = pa.loc[pa["TwoK"].to_numpy() & pa_out["K"].to_numpy()].groupby("PitcherKey").size().rename("PutAway_num")
    pitcher = pitcher.join(put_away)
    pitcher["PutAway_num"] = pitcher["PutAway_num"].fillna(0)
    pitcher["PutAway_pct"] = pitcher["PutAway_num"] / pitcher["PutAway_den"].replace(0, np.nan)

    rows: list[dict] = []
    add_metric(rows, "overall", "heater_velocity", pitcher.loc[pitcher["HeaterPitches"] >= 25, "HeaterVelocity"], pitcher.loc[pitcher["HeaterPitches"] >= 25, "HeaterPitches"])
    add_metric(rows, "overall", "extension", pitcher.loc[pitcher["HeaterPitches"] >= 25, "HeaterExtension"], pitcher.loc[pitcher["HeaterPitches"] >= 25, "HeaterPitches"])
    add_metric(rows, "overall", "release_height", pitcher.loc[pitcher["Pitches"] >= 100, "RelHeight"], pitcher.loc[pitcher["Pitches"] >= 100, "Pitches"])
    add_metric(rows, "overall", "release_side", pitcher.loc[pitcher["Pitches"] >= 100, "RelSideAbs"], pitcher.loc[pitcher["Pitches"] >= 100, "Pitches"])
    add_metric(rows, "overall", "woba", pitcher.loc[pitcher["woba_den"] >= 50, "wOBA"], pitcher.loc[pitcher["woba_den"] >= 50, "woba_den"])
    add_metric(rows, "overall", "whiff_rate", pitcher.loc[pitcher["Swings"] >= 25, "WhiffRate"], pitcher.loc[pitcher["Swings"] >= 25, "Swings"])
    add_metric(rows, "overall", "chase_rate", pitcher.loc[pitcher["OOZ"] >= 25, "ChaseRate"], pitcher.loc[pitcher["OOZ"] >= 25, "OOZ"])
    add_metric(rows, "overall", "avg_ev", pitcher.loc[pitcher["EVCount"] >= 20, "AvgEV"], pitcher.loc[pitcher["EVCount"] >= 20, "EVCount"])
    add_performance_metrics(rows, pitcher)

    pt = df.groupby(["PitcherKey", "PitchTypeStd"]).agg(
        Pitches=("PitchTypeStd", "size"),
        Velocity=("RelSpeed", "mean"),
        Extension=("Extension", "mean"),
        IVB=("InducedVertBreak", "mean"),
        HBForPercentile=("HBForPercentile", "mean"),
        RPM=("SpinRate", "mean"),
        VAA=("VertApprAngle", "mean"),
        HAAAbs=("HorzApprAngle", lambda x: np.nanmean(np.abs(x))),
        Whiffs=("IsWhiff", "sum"),
        Swings=("IsSwing", "sum"),
        Chases=("IsChase", "sum"),
        OOZ=("InZone", lambda x: int((~x).sum())),
    )
    pt["WhiffRate"] = pt["Whiffs"] / pt["Swings"].replace(0, np.nan)
    pt["ChaseRate"] = pt["Chases"] / pt["OOZ"].replace(0, np.nan)

    pa_pt = pa.copy()
    pa_pt["woba_num"] = pa_out["woba_num"].to_numpy()
    pa_pt["woba_den"] = pa_out["woba_den"].to_numpy()
    pt_woba = pa_pt.groupby(["PitcherKey", "PitchTypeStd"]).agg(woba_num=("woba_num", "sum"), woba_den=("woba_den", "sum"))
    pt_woba["wOBA"] = pt_woba["woba_num"] / pt_woba["woba_den"].replace(0, np.nan)
    pt = pt.join(pt_woba[["wOBA", "woba_den"]])

    for pitch_type, sub in pt.groupby(level="PitchTypeStd"):
        sub = sub.reset_index(level="PitchTypeStd", drop=True)
        base = sub["Pitches"] >= MIN_PITCH_TYPE_PITCHES
        add_metric(rows, "pitch_type", "pitchtype_velocity", sub.loc[base, "Velocity"], sub.loc[base, "Pitches"], pitch_type)
        if pitch_type in {"Fastball", "Sinker"}:
            add_metric(rows, "pitch_type", "pitchtype_extension", sub.loc[base, "Extension"], sub.loc[base, "Pitches"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_ivb", sub.loc[base, "IVB"], sub.loc[base, "Pitches"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_hb", sub.loc[base, "HBForPercentile"], sub.loc[base, "Pitches"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_rpm", sub.loc[base, "RPM"], sub.loc[base, "Pitches"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_vaa", sub.loc[base, "VAA"], sub.loc[base, "Pitches"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_haa", sub.loc[base, "HAAAbs"], sub.loc[base, "Pitches"], pitch_type)
        whiff_base = base & (sub["Swings"] >= 10)
        chase_base = base & (sub["OOZ"] >= 10)
        woba_base = base & (sub["woba_den"] >= 10)
        add_metric(rows, "pitch_type", "pitchtype_whiff_rate", sub.loc[whiff_base, "WhiffRate"], sub.loc[whiff_base, "Swings"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_chase_rate", sub.loc[chase_base, "ChaseRate"], sub.loc[chase_base, "OOZ"], pitch_type)
        add_metric(rows, "pitch_type", "pitchtype_woba", sub.loc[woba_base, "wOBA"], sub.loc[woba_base, "woba_den"], pitch_type)

    out = pd.DataFrame(rows)
    out.to_csv(OUTFILE, index=False)
    print(f"Wrote {len(out):,} reference rows to {OUTFILE}")


if __name__ == "__main__":
    main()
