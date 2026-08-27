#!/usr/bin/env python3
"""Compute pitch-type baselines for movement-only Stuff+ (unsupervised).

We z-score each metric within pitch type, then combine to a composite score.
Composite is then scaled to Stuff+ using pitch-type mean/sd.
"""

import argparse
import math

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


METRICS = [
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

# Metrics where magnitude (absolute deviation) is used
ABS_METRICS = {
    "InducedVertBreak",
    "HorzBreak",
    "VertApprAngle",
    "HorzApprAngle",
    "RelHeight",
    "RelSide",
}


def coerce_numeric(df, cols):
    for c in cols:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")


def main():
    parser = argparse.ArgumentParser(description="Compute Stuff+ baselines by pitch type.")
    parser.add_argument("--data", required=True, help="Parquet dataset path")
    parser.add_argument("--out", default="data/called_stuff_baseline.csv")
    args = parser.parse_args()

    pf = pq.ParquetFile(args.data)
    print(f"Row groups: {pf.num_row_groups} | Rows: {pf.metadata.num_rows}")

    # ---- Pass 1: metric mean/sd per pitch type ----
    acc = {}

    for rg in range(pf.num_row_groups):
        df = pf.read_row_group(rg, columns=["TaggedPitchType"] + METRICS).to_pandas()
        df["pitch_type"] = df["TaggedPitchType"].astype(str).str.strip()
        coerce_numeric(df, METRICS)

        for metric in METRICS:
            tmp = df[["pitch_type", metric]].dropna()
            if tmp.empty:
                continue
            grp = tmp.groupby("pitch_type")[metric].agg(["sum", "count"]).reset_index()
            tmp["metric_sq"] = tmp[metric] ** 2
            grp_sq = tmp.groupby("pitch_type")["metric_sq"].sum().reset_index()
            grp = grp.merge(grp_sq, on="pitch_type", how="left")

            for _, row in grp.iterrows():
                pt = row["pitch_type"]
                if pt not in acc:
                    acc[pt] = {}
                if metric not in acc[pt]:
                    acc[pt][metric] = {"sum": 0.0, "sumsq": 0.0, "count": 0}
                acc[pt][metric]["sum"] += float(row["sum"])
                acc[pt][metric]["sumsq"] += float(row["metric_sq"])
                acc[pt][metric]["count"] += int(row["count"])

        print(f"Pass1 row group {rg + 1}/{pf.num_row_groups} complete")

    rows = []
    for pt, m in acc.items():
        row = {"pitch_type": pt}
        for metric in METRICS:
            stats = m.get(metric, {"sum": math.nan, "sumsq": math.nan, "count": 0})
            cnt = stats["count"]
            if cnt > 0:
                mean = stats["sum"] / cnt
                var = max(stats["sumsq"] / cnt - mean ** 2, 0.0)
                sd = math.sqrt(var)
            else:
                mean, sd = math.nan, math.nan
            row[f"{metric}_mean"] = mean
            row[f"{metric}_sd"] = sd
        rows.append(row)

    base = pd.DataFrame(rows)

    # ---- Pass 2: composite mean/sd per pitch type ----
    comp_acc = {}

    for rg in range(pf.num_row_groups):
        df = pf.read_row_group(rg, columns=["TaggedPitchType"] + METRICS).to_pandas()
        df["pitch_type"] = df["TaggedPitchType"].astype(str).str.strip()
        coerce_numeric(df, METRICS)
        df = df.merge(base, on="pitch_type", how="left")

        # compute z-scores
        z_cols = []
        for metric in METRICS:
            mean_col = f"{metric}_mean"
            sd_col = f"{metric}_sd"
            z = (df[metric] - df[mean_col]) / df[sd_col]
            if metric in ABS_METRICS:
                z = z.abs()
            z_cols.append(z)

        z_stack = np.vstack([z.to_numpy() for z in z_cols])
        comp = np.nanmean(z_stack, axis=0)

        df["comp"] = comp
        grp = df.groupby("pitch_type")["comp"].agg(["sum", "count"]).reset_index()
        grp["sumsq"] = df.groupby("pitch_type")["comp"].apply(lambda x: np.nansum(x ** 2)).values

        for _, row in grp.iterrows():
            pt = row["pitch_type"]
            if pt not in comp_acc:
                comp_acc[pt] = {"sum": 0.0, "sumsq": 0.0, "count": 0}
            comp_acc[pt]["sum"] += float(row["sum"])
            comp_acc[pt]["sumsq"] += float(row["sumsq"])
            comp_acc[pt]["count"] += int(row["count"])

        print(f"Pass2 row group {rg + 1}/{pf.num_row_groups} complete")

    comp_rows = []
    for pt, stats in comp_acc.items():
        cnt = stats["count"]
        if cnt > 0:
            mean = stats["sum"] / cnt
            var = max(stats["sumsq"] / cnt - mean ** 2, 0.0)
            sd = math.sqrt(var)
        else:
            mean, sd = math.nan, math.nan
        comp_rows.append({"pitch_type": pt, "comp_mean": mean, "comp_sd": sd, "comp_n": cnt})

    comp_df = pd.DataFrame(comp_rows)
    out = base.merge(comp_df, on="pitch_type", how="left")

    out.to_csv(args.out, index=False)
    print(f"Wrote {args.out} ({len(out)} pitch types)")


if __name__ == "__main__":
    main()
