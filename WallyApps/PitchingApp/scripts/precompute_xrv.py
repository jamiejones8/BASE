#!/usr/bin/env python3
"""Precompute xRV metrics and write data/xrv_metrics.csv.

Requires: pandas, numpy, catboost, scikit-learn (for pickle compatibility)
"""

import argparse
import glob
import os
import pickle

import numpy as np
import pandas as pd


def ensure_libomp_path():
    """Ensure DYLD_LIBRARY_PATH includes a libomp location for xgboost."""
    candidates = [
        "/opt/homebrew/opt/libomp/lib",
        "/usr/local/opt/libomp/lib",
    ]
    for path in candidates:
        if os.path.exists(os.path.join(path, "libomp.dylib")):
            cur = os.environ.get("DYLD_LIBRARY_PATH", "")
            parts = [p for p in cur.split(":") if p]
            if path not in parts:
                parts.insert(0, path)
                os.environ["DYLD_LIBRARY_PATH"] = ":".join(parts)
            return


ensure_libomp_path()


FASTBALL_TYPES = {"Fastball", "Sinker", "Cutter"}


def format_name(name):
    if name is None or (isinstance(name, float) and np.isnan(name)):
        return name
    s = str(name)
    if ", " in s:
        last, first = s.split(", ", 1)
        return f"{first} {last}".strip()
    return s.strip()


def normalize_side(val):
    if val is None or (isinstance(val, float) and np.isnan(val)):
        return np.nan
    s = str(val).strip().lower()
    if s in {"l", "left", "lhh"}:
        return "Left"
    if s in {"r", "right", "rhh", "rigjht"}:
        return "Right"
    return np.nan


def coerce_numeric(df, cols):
    for c in cols:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")


def compute_xrv(df, heights_df, model_paths):
    required = [
        "RelSide", "RelHeight", "Extension", "RelSpeed", "SpinRate",
        "HorzBreak", "InducedVertBreak", "PlateLocHeight", "PlateLocSide",
        "SpinAxis", "VertApprAngle", "Pitcher", "PitcherId",
        "PitcherThrows", "BatterSide", "Balls", "Strikes"
    ]

    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # Use TaggedPitchType when available; fallback to PitchType.
    if "TaggedPitchType" in df.columns:
        df["pitch_type"] = df["TaggedPitchType"]
    elif "PitchType" in df.columns:
        df["pitch_type"] = df["PitchType"]
    else:
        raise ValueError("Missing TaggedPitchType/PitchType column")

    df["pitch_type"] = df["pitch_type"].astype(str).str.strip()

    # Normalize sides
    df["BatterSide"] = df["BatterSide"].apply(normalize_side)
    df["PitcherThrows"] = df["PitcherThrows"].apply(normalize_side)

    # Prep name/height
    df["player_name"] = df["Pitcher"].apply(format_name)
    df["player_name"] = df["player_name"].astype(str).str.strip().str.replace(r"\s+", " ", regex=True)

    heights_df = heights_df.copy()
    heights_df["player_name"] = heights_df["player_name"].astype(str).str.strip().str.replace(r"\s+", " ", regex=True)
    df = df.merge(heights_df, on="player_name", how="left")

    # Derived columns for models
    df["release_pos_x"] = df["RelSide"]
    df["release_pos_z"] = df["RelHeight"]
    df["release_extension"] = df["Extension"]

    coerce_numeric(df, [
        "release_pos_x", "release_pos_z", "release_extension", "height_in_inches",
        "RelSpeed", "SpinRate", "HorzBreak", "InducedVertBreak",
        "PlateLocHeight", "PlateLocSide", "SpinAxis", "VertApprAngle",
        "Balls", "Strikes"
    ])

    # Arm angle model
    df["interaction"] = df["release_pos_z"] * df["release_extension"] * df["height_in_inches"]
    arm_feats = ["release_pos_x", "release_pos_z", "release_extension", "height_in_inches", "interaction"]
    arm_mask = df[arm_feats].notna().all(axis=1)

    with open(model_paths["catboost"], "rb") as f:
        cb_model = pickle.load(f)

    df.loc[arm_mask, "arm_angle"] = cb_model.predict(df.loc[arm_mask, arm_feats])

    # iVB model
    ivb_feats = [
        "arm_angle", "release_pos_x", "release_pos_z",
        "release_extension", "RelSpeed", "SpinRate"
    ]
    ivb_mask = df[ivb_feats].notna().all(axis=1)
    with open(model_paths["ivb"], "rb") as f:
        ivb_model = pickle.load(f)
    df.loc[ivb_mask, "xiVB"] = ivb_model.predict(df.loc[ivb_mask, ivb_feats])
    df["iVB_oe"] = df["InducedVertBreak"] - df["xiVB"]

    # Fastball averages by pitcher/batter side
    fb_df = df[df["pitch_type"].isin(FASTBALL_TYPES)].copy()
    if not fb_df.empty:
        mode_fb = (
            fb_df.groupby(["PitcherId", "BatterSide"])["pitch_type"]
            .agg(lambda x: x.mode().iloc[0] if not x.mode().empty else np.nan)
            .reset_index()
            .rename(columns={"pitch_type": "most_common_fb"})
        )
        fb_df = fb_df.merge(mode_fb, on=["PitcherId", "BatterSide"], how="left")
        fb_df = fb_df[fb_df["pitch_type"] == fb_df["most_common_fb"]]

        avg_metrics = (
            fb_df.groupby(["PitcherId", "BatterSide", "most_common_fb"])
            .agg({
                "RelSpeed": "mean",
                "release_pos_x": "mean",
                "release_pos_z": "mean",
                "HorzBreak": "mean",
                "InducedVertBreak": "mean",
            })
            .reset_index()
            .rename(columns={
                "RelSpeed": "avg_RelSpeed",
                "release_pos_x": "avg_release_pos_x",
                "release_pos_z": "avg_release_pos_z",
                "HorzBreak": "avg_HorzBreak",
                "InducedVertBreak": "avg_InducedVertBreak",
            })
        )
        df = df.merge(avg_metrics, on=["PitcherId", "BatterSide"], how="left")
    else:
        df["avg_RelSpeed"] = np.nan
        df["avg_release_pos_x"] = np.nan
        df["avg_release_pos_z"] = np.nan
        df["avg_HorzBreak"] = np.nan
        df["avg_InducedVertBreak"] = np.nan

    # Platoon state
    conditions = [
        (df["BatterSide"] == "Left") & (df["PitcherThrows"] == "Left"),
        (df["BatterSide"] == "Left") & (df["PitcherThrows"] == "Right"),
        (df["BatterSide"] == "Right") & (df["PitcherThrows"] == "Left"),
        (df["BatterSide"] == "Right") & (df["PitcherThrows"] == "Right"),
    ]
    df["platoon_state"] = np.select(conditions, [0, 1, 2, 3], default=np.nan)

    # Count mapping
    count_mapping = {
        (0, 0): 0, (0, 1): 1, (0, 2): 2,
        (1, 0): 3, (1, 1): 4, (1, 2): 5,
        (2, 0): 6, (2, 1): 7, (2, 2): 8,
        (3, 0): 9, (3, 1): 10, (3, 2): 11,
    }
    balls = pd.to_numeric(df["Balls"], errors="coerce")
    strikes = pd.to_numeric(df["Strikes"], errors="coerce")
    pairs = zip(balls, strikes)
    df["count"] = [
        count_mapping.get((int(b), int(s))) if np.isfinite(b) and np.isfinite(s) else np.nan
        for b, s in pairs
    ]

    # xRV model
    loc_features = [
        "RelSpeed", "release_pos_x", "release_pos_z", "platoon_state",
        "count", "HorzBreak", "InducedVertBreak", "release_extension",
        "SpinRate", "PlateLocHeight", "PlateLocSide", "SpinAxis",
        "avg_RelSpeed", "avg_release_pos_x", "avg_release_pos_z",
        "avg_HorzBreak", "avg_InducedVertBreak", "arm_angle",
        "VertApprAngle", "iVB_oe",
    ]

    xrv_mask = df[loc_features].notna().all(axis=1)
    with open(model_paths["xrv"], "rb") as f:
        xrv_model = pickle.load(f)
    df.loc[xrv_mask, "xrv"] = xrv_model.predict(df.loc[xrv_mask, loc_features])

    return df


def main():
    parser = argparse.ArgumentParser(description="Precompute xRV metrics and write xrv_metrics.csv")
    parser.add_argument("--data-dir", default="data", help="Directory containing input CSVs")
    parser.add_argument("--heights", default="data/player_heights.csv", help="CSV with player_name,height_in_inches")
    parser.add_argument("--models-dir", default="models", help="Directory with model .pkl files")
    parser.add_argument("--output", default="data/xrv_metrics.csv", help="Output CSV path")
    args = parser.parse_args()

    data_dir = args.data_dir
    heights_path = args.heights
    models_dir = args.models_dir
    out_path = args.output

    if not os.path.exists(heights_path):
        raise SystemExit(f"Heights CSV not found: {heights_path}")

    model_paths = {
        "catboost": os.path.join(models_dir, "best_catboost_model.pkl"),
        "ivb": os.path.join(models_dir, "ivb_model.pkl"),
        "xrv": os.path.join(models_dir, "xrv_model.pkl"),
    }
    for k, p in model_paths.items():
        if not os.path.exists(p):
            raise SystemExit(f"Missing {k} model: {p}")

    heights_df = pd.read_csv(heights_path)

    csvs = sorted(glob.glob(os.path.join(data_dir, "*.csv")))
    skip = {os.path.basename(heights_path), os.path.basename(out_path)}
    csvs = [c for c in csvs if os.path.basename(c) not in skip]

    if not csvs:
        raise SystemExit("No input CSVs found.")

    out_frames = []
    for path in csvs:
        print(f"Processing {path}...")
        df = pd.read_csv(path, low_memory=False)
        df["source_file"] = os.path.basename(path)
        df["row_in_file"] = np.arange(1, len(df) + 1)
        df = compute_xrv(df, heights_df, model_paths)
        out_frames.append(df[[
            "source_file", "row_in_file", "xrv", "arm_angle", "xiVB", "iVB_oe"
        ]])

    out = pd.concat(out_frames, ignore_index=True)
    out.to_csv(out_path, index=False)
    print(f"Wrote {out_path} ({len(out)} rows)")


if __name__ == "__main__":
    main()
