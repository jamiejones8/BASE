#!/usr/bin/env python3
"""Build compact D1 catcher framing percentile inputs from the D1 parquet file."""

from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


APP_DIR = Path(__file__).resolve().parents[1]
WORKSPACE_DIR = APP_DIR.parent
SHARED_D1_DIR = Path(os.environ.get("D1_FILES_DIR", WORKSPACE_DIR / "D1 Files")).expanduser()
PARQUET_PATH = SHARED_D1_DIR / "D1 Catching File.parquet"
OUT_PATH = APP_DIR / "data" / "d1_catcher_framing_metrics.csv"

COLUMNS = [
    "Catcher",
    "PitchCall",
    "PlateLocSide",
    "PlateLocHeight",
    "PitcherThrows",
    "TaggedPitchType",
    "AutoPitchType",
]

METRICS = [
    ("strikes_stolen", "Strikes Stolen"),
    ("balls_lost", "Balls Lost"),
    ("bottom_buffer", "Bottom Buffer"),
    ("top_buffer", "Top Buffer"),
    ("glove_side_buffer", "Glove Side Buffer"),
    ("arm_side_buffer", "Arm Side Buffer"),
    ("rhp", "RHP"),
    ("lhp", "LHP"),
    ("fastballs_sinkers", "Fastballs/Sinkers"),
    ("breaking_balls", "Breaking Balls"),
    ("soft", "Soft"),
    ("lhp_fastballs_sinkers", "LHP Fastballs/Sinkers"),
    ("lhp_breaking_balls", "LHP Breaking Balls"),
    ("lhp_soft", "LHP Soft"),
    ("rhp_fastballs_sinkers", "RHP Fastballs/Sinkers"),
    ("rhp_breaking_balls", "RHP Breaking Balls"),
    ("rhp_soft", "RHP Soft"),
]


def canonical_pitch_call(values: pd.Series) -> pd.Series:
    y = values.fillna("").astype(str).str.lower().str.replace(r"[^a-z]", "", regex=True)
    return np.select(
        [
            y.isin(["strikecalled", "calledstrike"]),
            y.isin(["ballcalled", "ball", "ballinthedirt", "ballintentional"]),
        ],
        ["StrikeCalled", "BallCalled"],
        default=values.fillna("").astype(str),
    )


def canonical_pitch_type(values: pd.Series) -> pd.Series:
    y = values.fillna("").astype(str).str.lower().str.strip()
    out = np.full(len(y), "Undefined", dtype=object)
    out[y.str.contains("four|two|fast", regex=True, na=False)] = "Fastball"
    out[y.str.contains("sink", regex=True, na=False)] = "Sinker"
    out[y.str.contains("change", regex=True, na=False)] = "Changeup"
    out[y.str.contains("split", regex=True, na=False)] = "Splitter"
    out[y.str.contains("slide", regex=True, na=False)] = "Slider"
    out[y.str.contains("cut", regex=True, na=False)] = "Cutter"
    out[y.str.contains("sweep", regex=True, na=False)] = "Sweeper"
    out[y.str.contains("curve|knuckle", regex=True, na=False)] = "Curveball"
    return pd.Series(out, index=values.index)


def framing_zone_type(s_in: pd.Series, h_in: pd.Series) -> pd.Series:
    abs_s = s_in.abs()
    zone = pd.Series("Chase", index=s_in.index, dtype=object)

    in_top = abs_s.le(6.7) & h_in.gt(38) & h_in.le(46)
    in_down = abs_s.le(6.7) & h_in.ge(14) & h_in.lt(22)
    in_glove = s_in.gt(6.7) & s_in.le(13.3) & h_in.between(22, 38)
    in_arm = s_in.ge(-13.3) & s_in.lt(-6.7) & h_in.between(22, 38)
    in_heart = abs_s.le(6.7) & h_in.between(22, 38)
    in_zone = abs_s.le(10.0) & h_in.between(18, 42) & ~in_heart
    in_shadow = (
        (abs_s.gt(10.0) & abs_s.le(13.3) & h_in.between(18, 42))
        | (abs_s.le(13.3) & h_in.between(42, 46))
        | (abs_s.le(13.3) & h_in.between(14, 18))
    )

    zone[in_shadow] = "Shadow"
    zone[in_zone] = "Zone"
    zone[in_heart] = "Heart"
    zone[in_down] = "Bottom Buffer"
    zone[in_arm] = "Arm Side Buffer"
    zone[in_glove] = "Gloveside Buffer"
    zone[in_top] = "Top Buffer"
    return zone


def summarise_rate(df: pd.DataFrame, metric_id: str, metric: str, denom_mask, num_mask) -> pd.DataFrame:
    tmp = df.loc[denom_mask, ["Catcher"]].copy()
    tmp["num"] = np.asarray(num_mask)[denom_mask]
    out = tmp.groupby("Catcher", as_index=False).agg(chances=("num", "size"), numerator=("num", "sum"))
    out["value"] = out["numerator"] / out["chances"]
    out["metric_id"] = metric_id
    out["metric"] = metric
    return out[["metric_id", "metric", "Catcher", "value", "chances", "numerator"]]


def summarise_net(df: pd.DataFrame, metric_id: str, metric: str, mask) -> pd.DataFrame:
    tmp = df.loc[mask, ["Catcher", "frame_value"]]
    out = tmp.groupby("Catcher", as_index=False).agg(chances=("frame_value", "size"), numerator=("frame_value", "sum"))
    out["value"] = out["numerator"] / out["chances"]
    out["metric_id"] = metric_id
    out["metric"] = metric
    return out[["metric_id", "metric", "Catcher", "value", "chances", "numerator"]]


def main() -> None:
    if not PARQUET_PATH.exists():
        raise SystemExit(
            "D1 catching parquet not found. Expected "
            f"{PARQUET_PATH}. Set D1_FILES_DIR to override the shared D1 folder."
        )

    table = pq.read_table(PARQUET_PATH, columns=COLUMNS)
    df = table.to_pandas()

    df["Catcher"] = df["Catcher"].fillna("").astype(str).str.strip()
    df = df[df["Catcher"].ne("")]

    df["pitch_call"] = canonical_pitch_call(df["PitchCall"])
    df["plate_x"] = pd.to_numeric(df["PlateLocSide"], errors="coerce")
    df["plate_z"] = pd.to_numeric(df["PlateLocHeight"], errors="coerce")
    df = df[df["plate_x"].notna() & df["plate_z"].notna()]

    sx_in = df["plate_x"] * 12
    hz_in = df["plate_z"] * 12
    df["in_zone"] = sx_in.abs().le(10.0) & hz_in.between(18, 42)
    df["zone_type"] = framing_zone_type(sx_in, hz_in)

    throws = df["PitcherThrows"].fillna("").astype(str).str.upper().str[0]
    df["PitcherHand"] = np.select([throws.eq("R"), throws.eq("L")], ["RHP", "LHP"], default="Unknown")

    tagged = canonical_pitch_type(df["TaggedPitchType"])
    auto = canonical_pitch_type(df["AutoPitchType"])
    df["PitchType"] = np.where(tagged.ne("Undefined"), tagged, auto)
    df["PitchGroup"] = np.select(
        [
            pd.Series(df["PitchType"]).isin(["Fastball", "Sinker"]),
            pd.Series(df["PitchType"]).isin(["Cutter", "Slider", "Sweeper", "Curveball"]),
            pd.Series(df["PitchType"]).isin(["Changeup", "Splitter"]),
        ],
        ["Fastballs/Sinkers", "Breaking Balls", "Soft"],
        default="Other",
    )

    df = df[df["pitch_call"].isin(["StrikeCalled", "BallCalled"])].copy()
    df["frame_value"] = np.select(
        [
            df["pitch_call"].eq("StrikeCalled") & ~df["in_zone"],
            df["pitch_call"].eq("BallCalled") & df["in_zone"],
        ],
        [1.0, -1.0],
        default=0.0,
    )

    rows = []
    rows.append(
        summarise_rate(
            df,
            "strikes_stolen",
            "Strikes Stolen",
            ~df["in_zone"],
            df["pitch_call"].eq("StrikeCalled"),
        )
    )
    rows.append(
        summarise_rate(
            df,
            "balls_lost",
            "Balls Lost",
            df["in_zone"],
            df["pitch_call"].eq("BallCalled"),
        )
    )

    buffer_map = [
        ("bottom_buffer", "Bottom Buffer", "Bottom Buffer"),
        ("top_buffer", "Top Buffer", "Top Buffer"),
        ("glove_side_buffer", "Glove Side Buffer", "Gloveside Buffer"),
        ("arm_side_buffer", "Arm Side Buffer", "Arm Side Buffer"),
    ]
    for metric_id, metric, zone_name in buffer_map:
        rows.append(
            summarise_rate(
                df,
                metric_id,
                metric,
                df["zone_type"].eq(zone_name),
                df["pitch_call"].eq("StrikeCalled"),
            )
        )

    net_filters = [
        ("rhp", "RHP", df["PitcherHand"].eq("RHP")),
        ("lhp", "LHP", df["PitcherHand"].eq("LHP")),
        ("fastballs_sinkers", "Fastballs/Sinkers", df["PitchGroup"].eq("Fastballs/Sinkers")),
        ("breaking_balls", "Breaking Balls", df["PitchGroup"].eq("Breaking Balls")),
        ("soft", "Soft", df["PitchGroup"].eq("Soft")),
        ("lhp_fastballs_sinkers", "LHP Fastballs/Sinkers", df["PitcherHand"].eq("LHP") & df["PitchGroup"].eq("Fastballs/Sinkers")),
        ("lhp_breaking_balls", "LHP Breaking Balls", df["PitcherHand"].eq("LHP") & df["PitchGroup"].eq("Breaking Balls")),
        ("lhp_soft", "LHP Soft", df["PitcherHand"].eq("LHP") & df["PitchGroup"].eq("Soft")),
        ("rhp_fastballs_sinkers", "RHP Fastballs/Sinkers", df["PitcherHand"].eq("RHP") & df["PitchGroup"].eq("Fastballs/Sinkers")),
        ("rhp_breaking_balls", "RHP Breaking Balls", df["PitcherHand"].eq("RHP") & df["PitchGroup"].eq("Breaking Balls")),
        ("rhp_soft", "RHP Soft", df["PitcherHand"].eq("RHP") & df["PitchGroup"].eq("Soft")),
    ]
    for metric_id, metric, mask in net_filters:
        rows.append(summarise_net(df, metric_id, metric, mask))

    out = pd.concat(rows, ignore_index=True)
    order = {metric_id: i + 1 for i, (metric_id, _) in enumerate(METRICS)}
    out["metric_order"] = out["metric_id"].map(order)
    out["better"] = np.where(out["metric_id"].eq("balls_lost"), "low", "high")
    out = out.sort_values(["metric_order", "Catcher"]).reset_index(drop=True)
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT_PATH, index=False)
    print(f"Wrote {len(out):,} metric rows to {OUT_PATH}")


if __name__ == "__main__":
    main()
