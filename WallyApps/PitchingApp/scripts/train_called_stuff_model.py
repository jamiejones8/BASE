#!/usr/bin/env python3
"""Train a movement-only Stuff+ model (Called Stuff) to predict xRV.

Features: Velocity, SpinRate, iVB, HB, VAA, HAA, RelHeight, RelSide, Extension, TaggedPitchType.
Label: xRV (from existing xRV model).
"""

import argparse
import os
import pickle

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from catboost import CatBoostRegressor, Pool


FASTBALL_TYPES = {"Fastball", "Sinker", "Cutter"}


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


def compute_mode_fastball(pf):
    counts_list = []
    for rg in range(pf.num_row_groups):
        df = pf.read_row_group(
            rg, columns=["PitcherId", "BatterSide", "TaggedPitchType"]
        ).to_pandas()
        df["BatterSide"] = df["BatterSide"].apply(normalize_side)
        df["pitch_type"] = df["TaggedPitchType"].astype(str).str.strip()
        df = df[df["pitch_type"].isin(FASTBALL_TYPES)]
        df = df[df["PitcherId"].notna() & df["BatterSide"].notna()]
        if df.empty:
            continue
        grp = df.groupby(["PitcherId", "BatterSide", "pitch_type"]).size().reset_index(name="n")
        counts_list.append(grp)

    if not counts_list:
        raise SystemExit("No fastballs found to compute averages.")

    counts = pd.concat(counts_list, ignore_index=True)
    counts = counts.groupby(
        ["PitcherId", "BatterSide", "pitch_type"], as_index=False
    )["n"].sum()
    counts = counts.sort_values(
        ["PitcherId", "BatterSide", "n", "pitch_type"],
        ascending=[True, True, False, True],
    )
    return counts.drop_duplicates(["PitcherId", "BatterSide"]).rename(
        columns={"pitch_type": "most_common_fb"}
    )


def compute_fastball_avgs(pf, mode_fb):
    avg_tot = None
    avg_cols = ["RelSpeed", "RelSide", "RelHeight", "HorzBreak", "InducedVertBreak"]
    for rg in range(pf.num_row_groups):
        df = pf.read_row_group(
            rg, columns=["PitcherId", "BatterSide", "TaggedPitchType"] + avg_cols
        ).to_pandas()
        df["BatterSide"] = df["BatterSide"].apply(normalize_side)
        df["pitch_type"] = df["TaggedPitchType"].astype(str).str.strip()
        df = df.merge(
            mode_fb[["PitcherId", "BatterSide", "most_common_fb"]],
            on=["PitcherId", "BatterSide"],
            how="left",
        )
        df = df[df["pitch_type"] == df["most_common_fb"]]
        if df.empty:
            continue
        coerce_numeric(df, avg_cols)
        df = df[df["PitcherId"].notna() & df["BatterSide"].notna()]
        grp = df.groupby(["PitcherId", "BatterSide"], as_index=True).agg(
            sum_RelSpeed=("RelSpeed", "sum"),
            sum_release_pos_x=("RelSide", "sum"),
            sum_release_pos_z=("RelHeight", "sum"),
            sum_HorzBreak=("HorzBreak", "sum"),
            sum_InducedVertBreak=("InducedVertBreak", "sum"),
            count=("RelSpeed", "count"),
        )
        avg_tot = grp if avg_tot is None else avg_tot.add(grp, fill_value=0)

    if avg_tot is None or avg_tot.empty:
        raise SystemExit("No fastball averages computed.")

    avg_metrics = avg_tot.reset_index()
    avg_metrics["avg_RelSpeed"] = avg_metrics["sum_RelSpeed"] / avg_metrics["count"]
    avg_metrics["avg_release_pos_x"] = avg_metrics["sum_release_pos_x"] / avg_metrics["count"]
    avg_metrics["avg_release_pos_z"] = avg_metrics["sum_release_pos_z"] / avg_metrics["count"]
    avg_metrics["avg_HorzBreak"] = avg_metrics["sum_HorzBreak"] / avg_metrics["count"]
    avg_metrics["avg_InducedVertBreak"] = (
        avg_metrics["sum_InducedVertBreak"] / avg_metrics["count"]
    )
    return avg_metrics[
        [
            "PitcherId",
            "BatterSide",
            "avg_RelSpeed",
            "avg_release_pos_x",
            "avg_release_pos_z",
            "avg_HorzBreak",
            "avg_InducedVertBreak",
        ]
    ]


def main():
    parser = argparse.ArgumentParser(description="Train movement-only Stuff+ model.")
    parser.add_argument("--data", required=True, help="Parquet dataset path")
    parser.add_argument("--models-dir", default="models", help="Directory with .pkl models")
    parser.add_argument("--out-model", default="models/called_stuff_model.pkl", help="Output model path")
    parser.add_argument("--seed", type=int, default=13)
    args = parser.parse_args()

    pf = pq.ParquetFile(args.data)
    print(f"Row groups: {pf.num_row_groups} | Rows: {pf.metadata.num_rows}")

    mode_fb = compute_mode_fastball(pf)
    print(f"Computed mode FB for {len(mode_fb)} pitcher/side pairs.")

    avg_metrics = compute_fastball_avgs(pf, mode_fb)
    print(f"Computed fastball averages: {len(avg_metrics)} rows.")

    with open(os.path.join(args.models_dir, "best_catboost_model.pkl"), "rb") as f:
        cb_model = pickle.load(f)
    with open(os.path.join(args.models_dir, "ivb_model.pkl"), "rb") as f:
        ivb_model = pickle.load(f)
    with open(os.path.join(args.models_dir, "xrv_model.pkl"), "rb") as f:
        xrv_model = pickle.load(f)

    req_cols = [
        "RelSide",
        "RelHeight",
        "Extension",
        "RelSpeed",
        "SpinRate",
        "HorzBreak",
        "InducedVertBreak",
        "PlateLocHeight",
        "PlateLocSide",
        "SpinAxis",
        "VertApprAngle",
        "HorzApprAngle",
        "PitcherId",
        "PitcherThrows",
        "BatterSide",
        "Balls",
        "Strikes",
        "TaggedPitchType",
    ]

    stuff_frames = []

    for rg in range(pf.num_row_groups):
        df = pf.read_row_group(rg, columns=req_cols).to_pandas()
        df["BatterSide"] = df["BatterSide"].apply(normalize_side)
        df["PitcherThrows"] = df["PitcherThrows"].apply(normalize_side)
        df["pitch_type"] = df["TaggedPitchType"].astype(str).str.strip()

        df["release_pos_x"] = df["RelSide"]
        df["release_pos_z"] = df["RelHeight"]
        df["release_extension"] = df["Extension"]

        # Use release height (feet) as a proxy for height; convert to inches for model parity.
        df["height_in_inches"] = df["RelHeight"] * 12.0

        coerce_numeric(
            df,
            [
                "release_pos_x",
                "release_pos_z",
                "release_extension",
                "height_in_inches",
                "RelSpeed",
                "SpinRate",
                "HorzBreak",
                "InducedVertBreak",
                "PlateLocHeight",
                "PlateLocSide",
                "SpinAxis",
                "VertApprAngle",
                "HorzApprAngle",
                "Balls",
                "Strikes",
            ],
        )

        # Arm angle
        df["interaction"] = (
            df["release_pos_z"] * df["release_extension"] * df["height_in_inches"]
        )
        arm_feats = [
            "release_pos_x",
            "release_pos_z",
            "release_extension",
            "height_in_inches",
            "interaction",
        ]
        arm_mask = df[arm_feats].notna().all(axis=1)
        if arm_mask.any():
            df.loc[arm_mask, "arm_angle"] = cb_model.predict(df.loc[arm_mask, arm_feats])

        # iVB model
        ivb_feats = [
            "arm_angle",
            "release_pos_x",
            "release_pos_z",
            "release_extension",
            "RelSpeed",
            "SpinRate",
        ]
        ivb_mask = df[ivb_feats].notna().all(axis=1)
        if ivb_mask.any():
            df.loc[ivb_mask, "xiVB"] = ivb_model.predict(df.loc[ivb_mask, ivb_feats])
        df["iVB_oe"] = df["InducedVertBreak"] - df["xiVB"]

        # Merge avg fastball metrics
        df = df.merge(avg_metrics, on=["PitcherId", "BatterSide"], how="left")

        # platoon_state
        conditions = [
            (df["BatterSide"] == "Left") & (df["PitcherThrows"] == "Left"),
            (df["BatterSide"] == "Left") & (df["PitcherThrows"] == "Right"),
            (df["BatterSide"] == "Right") & (df["PitcherThrows"] == "Left"),
            (df["BatterSide"] == "Right") & (df["PitcherThrows"] == "Right"),
        ]
        df["platoon_state"] = np.select(conditions, [0, 1, 2, 3], default=np.nan)

        # count mapping
        count_mapping = {
            (0, 0): 0,
            (0, 1): 1,
            (0, 2): 2,
            (1, 0): 3,
            (1, 1): 4,
            (1, 2): 5,
            (2, 0): 6,
            (2, 1): 7,
            (2, 2): 8,
            (3, 0): 9,
            (3, 1): 10,
            (3, 2): 11,
        }
        balls = pd.to_numeric(df["Balls"], errors="coerce")
        strikes = pd.to_numeric(df["Strikes"], errors="coerce")
        df["count"] = [
            count_mapping.get((int(b), int(s))) if np.isfinite(b) and np.isfinite(s) else np.nan
            for b, s in zip(balls, strikes)
        ]

        loc_features = [
            "RelSpeed",
            "release_pos_x",
            "release_pos_z",
            "platoon_state",
            "count",
            "HorzBreak",
            "InducedVertBreak",
            "release_extension",
            "SpinRate",
            "PlateLocHeight",
            "PlateLocSide",
            "SpinAxis",
            "avg_RelSpeed",
            "avg_release_pos_x",
            "avg_release_pos_z",
            "avg_HorzBreak",
            "avg_InducedVertBreak",
            "arm_angle",
            "VertApprAngle",
            "iVB_oe",
        ]

        label_mask = df[loc_features].notna().all(axis=1)
        if not label_mask.any():
            continue
        df.loc[label_mask, "xrv"] = xrv_model.predict(df.loc[label_mask, loc_features])

        # Movement-only features
        feat_cols = [
            "TaggedPitchType",
            "RelSpeed",
            "SpinRate",
            "InducedVertBreak",
            "HorzBreak",
            "VertApprAngle",
            "HorzApprAngle",
            "RelHeight",
            "RelSide",
            "Extension",
        ]

        mask = label_mask & df[feat_cols].notna().all(axis=1)
        df_feat = df.loc[mask, feat_cols + ["xrv"]].copy()
        if df_feat.empty:
            continue

        df_feat["TaggedPitchType"] = df_feat["TaggedPitchType"].astype(str).str.strip()
        stuff_frames.append(df_feat)

        print(f"Row group {rg + 1}/{pf.num_row_groups}: {len(df_feat):,} rows")

    if not stuff_frames:
        raise SystemExit("No training rows produced.")

    stuff_df = pd.concat(stuff_frames, ignore_index=True)
    print("Training rows:", len(stuff_df))

    # Train/validation split
    rng = np.random.RandomState(args.seed)
    is_train = rng.rand(len(stuff_df)) < 0.9
    train_df = stuff_df[is_train]
    val_df = stuff_df[~is_train]

    feature_cols = [
        "TaggedPitchType",
        "RelSpeed",
        "SpinRate",
        "InducedVertBreak",
        "HorzBreak",
        "VertApprAngle",
        "HorzApprAngle",
        "RelHeight",
        "RelSide",
        "Extension",
    ]
    cat_features = [0]  # TaggedPitchType

    train_pool = Pool(train_df[feature_cols], label=train_df["xrv"], cat_features=cat_features)
    val_pool = Pool(val_df[feature_cols], label=val_df["xrv"], cat_features=cat_features)

    model = CatBoostRegressor(
        loss_function="RMSE",
        iterations=600,
        depth=8,
        learning_rate=0.05,
        random_seed=args.seed,
        eval_metric="RMSE",
        verbose=100,
        od_type="Iter",
        od_wait=50,
    )
    model.fit(train_pool, eval_set=val_pool, use_best_model=True)

    os.makedirs(os.path.dirname(args.out_model), exist_ok=True)
    model.save_model(args.out_model)
    print(f"Saved model to {args.out_model}")

    # Compute D1 mean/sd for Stuff+
    preds = model.predict(stuff_df[feature_cols])
    preds = np.asarray(preds, dtype=float)
    mean_pred = float(np.mean(preds))
    sd_pred = float(np.std(preds))
    print(f"Called Stuff mean (model output): {mean_pred}")
    print(f"Called Stuff sd (model output):   {sd_pred}")


if __name__ == "__main__":
    main()
