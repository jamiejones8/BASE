#!/usr/bin/env python3
"""Build equal-team D1 trend benchmarks from full team pitch/PA totals.

python3 scripts/build_pitching_team_averages.py --source /path/to/D1.parquet
The runtime uses the small CSV; the national parquet is only needed to rebuild it.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("pitch_reference", ROOT / "WallyApps/PitchingApp/scripts/precompute_d1_pitch_metric_percentiles.py")
rules = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rules)


def prepare(rows: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    d = rows.copy()
    d = d.drop_duplicates("PitchUID") if "PitchUID" in d and d.PitchUID.notna().all() else d
    for col in ["PitchofPA", "PitchNo", "PAofInning", "Inning", "Balls", "Strikes", "OutsOnPlay",
                "RelSpeed", "Extension", "InducedVertBreak", "HorzBreak", "VertApprAngle", "ExitSpeed", "Angle", "PlateLocHeight", "PlateLocSide"]:
        d[col] = pd.to_numeric(d[col], errors="coerce") if col in d else np.nan
    for col in ["Pitcher", "PitcherTeam", "GameID", "GameUID", "Top/Bottom", "PitchCall", "PlayResult", "KorBB"]:
        d[col] = d[col].fillna("").astype(str) if col in d else ""
    d["PitchType"] = "Undefined"
    for col in ["ModelPitchType", "ModelPitchTypeRaw", "TaggedPitchType", "AutoPitchType"]:
        if col in d:
            candidate = rules.canon_pitch_type(d[col])
            use = rules.is_bad_pitch_type(d.PitchType) & ~rules.is_bad_pitch_type(candidate)
            d.loc[use, "PitchType"] = candidate[use]
    d = d.sort_values(["GameID", "GameUID", "Inning", "Top/Bottom", "PAofInning", "PitchofPA", "PitchNo"], kind="stable")
    # Team PAs count once, including a mid-PA pitching change.
    keys = ["GameID", "GameUID", "Inning", "Top/Bottom", "PAofInning", "PitcherTeam"]
    g = d.groupby(keys, dropna=False, sort=False)
    d["PA"] = g.ngroup()
    d["PitchNum"] = d.PitchofPA.fillna(g.cumcount() + 1)
    pc = d.PitchCall
    # Team Trends uses TrackMan before-pitch balls and chronological strike
    # reconstruction (fouls cannot advance a two-strike count).
    d["BallsPre"] = d.Balls
    if "BallsBeforePitch" in d:
        d["BallsPre"] = pd.to_numeric(d.BallsBeforePitch, errors="coerce").combine_first(d.BallsPre)
    strike_event = pc.isin(["StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip"])
    d["StrikesPre"] = (strike_event.groupby(d.PA).cumsum() - strike_event.astype(int)).clip(upper=2)
    d["Swing"] = pc.isin(["StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip", "InPlay", "InPlayOut", "InPlayNoOut"])
    d["Strike"] = d.Swing | pc.eq("StrikeCalled")
    d["Whiff"] = pc.eq("StrikeSwinging")
    d["CSW"] = pc.isin(["StrikeCalled", "StrikeSwinging"])
    known_zone = np.isfinite(d.PlateLocHeight) & np.isfinite(d.PlateLocSide)
    x, z = d.PlateLocSide * 12, d.PlateLocHeight * 12
    dx = np.maximum(np.maximum(-8.5 - x, 0), x - 8.5)
    dz = np.maximum(np.maximum(19.2 - z, 0), z - 40.8)
    d["Zone"] = (dx * dx + dz * dz <= 1.45 ** 2 + 1e-12).astype("boolean")
    d.loc[~known_zone, "Zone"] = pd.NA
    d["IZSwing"] = d.Zone & d.Swing
    d["IZWhiff"] = d.Zone & d.Whiff
    d["OOZ"] = ~d.Zone
    d["Chase"] = d.OOZ & d.Swing
    d["Pre2K"] = d.StrikesPre.lt(2) & d.Zone.notna()
    d["Pre2KZone"] = d.Pre2K & d.Zone
    d["At11"] = d.BallsPre.eq(1) & d.StrikesPre.eq(1)
    d["Win11"] = d.At11 & pc.isin(["StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip"])
    d["TwoK"] = d.StrikesPre.eq(2)
    outcomes = rules.pa_outcomes(d)
    d["BIP"] = outcomes.BIP_pa
    d["BIPLA"] = d.BIP & np.isfinite(d.Angle)
    d["BIPEVLA"] = d.BIPLA & np.isfinite(d.ExitSpeed)
    d["Barrel"] = d.BIP & rules.barrel_evla(d.ExitSpeed, d.Angle)
    barrel_ok = ~pd.to_datetime(d.Date).between("2026-02-27", "2026-03-01")
    d["BarrelN"] = d.Barrel & barrel_ok
    d["BarrelDen"] = d.BIPEVLA & barrel_ok
    d["GB"] = d.BIPLA & d.Angle.lt(5)
    d["PutAway"] = d.TwoK & outcomes.K & ~d.PA.duplicated(keep="last")
    d["EarlyStrike"] = d.PitchNum.le(3) & d.Strike
    d["EarlyBIP"] = d.PitchNum.le(3) & d.BIP
    d["EarlyBad"] = d.PitchNum.le(3) & (d.Barrel | outcomes.HBP)
    d["FirstStrike"] = d.PitchNum.eq(1) & d.Strike
    pa = d.groupby("PA", sort=False).tail(1).copy()
    summary = d.groupby("PA", sort=False).agg(EarlyStrike=("EarlyStrike", "sum"), EarlyBIP=("EarlyBIP", "max"), EarlyBad=("EarlyBad", "max"), FPS=("FirstStrike", "max"))
    pa = pa.join(summary, on="PA", rsuffix="_pa")
    pa["EA"] = ((pa.EarlyStrike_pa.ge(2) | pa.EarlyBIP_pa) & ~pa.EarlyBad_pa)
    o = rules.pa_outcomes(pa)
    for col in ["K", "BB", "HBP", "HR", "SF", "X1B", "X2B", "X3B", "woba_num", "woba_den"]:
        pa[col] = o[col]
    pa["AnyBB"] = (pa.KorBB.str.contains(r"walk|\bBB\b|\bIBB\b", case=False, regex=True) |
                   pa.PlayResult.str.contains(r"walk|\bBB\b|\bIBB\b", case=False, regex=True))
    pa["H"] = o.X1B | o.X2B | o.X3B | o.HR
    pa["TB"] = o.X1B.astype(int) + 2 * o.X2B + 3 * o.X3B + 4 * o.HR
    pa["BatHBP"] = pa.PitchCall.str.contains(r"hit\s*by\s*pitch|\bhbp\b|hitbypitch", case=False, regex=True)
    pa["AB"] = 1 - pa.BB.astype(int) - pa.BatHBP.astype(int) - o.SF.astype(int)
    pa["ContactNum"] = pa.woba_num - .690 * pa.BB - .720 * pa.HBP
    pa["IP"] = np.maximum(pd.to_numeric(pa.OutsOnPlay, errors="coerce").fillna(0), o.K.astype(int)) / 3
    # Text outs cover older exports lacking an OutsOnPlay value.
    pa.loc[pa.PlayResult.str.contains(r"\bout\b", case=False, regex=True) & ~o.K, "IP"] = np.maximum(pa.IP, 1 / 3)
    pa.loc[pa.PlayResult.str.contains(r"double\s*play", case=False, regex=True), "IP"] = 2 / 3
    pa.loc[pa.PlayResult.str.contains(r"triple\s*play", case=False, regex=True), "IP"] = 1
    return d, pa


def team_values(d: pd.DataFrame, pa: pd.DataFrame, by_pitch: bool = False) -> pd.DataFrame:
    keys = ["PitcherTeam"] + (["PitchType"] if by_pitch else [])
    grouped = d.groupby(keys, observed=True)
    p = grouped.agg(Pitches=("PitchUID", "size"), Swing=("Swing", "sum"), Whiff=("Whiff", "sum"),
        CSW=("CSW", "sum"), Zone=("Zone", "sum"), ZoneN=("Zone", "count"), IZSwing=("IZSwing", "sum"),
        IZWhiff=("IZWhiff", "sum"), OOZ=("OOZ", "sum"), Chase=("Chase", "sum"), Strike=("Strike", "sum"),
        Pre2K=("Pre2K", "sum"), Pre2KZone=("Pre2KZone", "sum"), At11=("At11", "sum"), Win11=("Win11", "sum"),
        TwoK=("TwoK", "sum"), PutAway=("PutAway", "sum"), BIPEVLA=("BIPEVLA", "sum"),
        Barrel=("BarrelN", "sum"), BarrelDen=("BarrelDen", "sum"), BIPLA=("BIPLA", "sum"), GB=("GB", "sum"),
        Velo=("RelSpeed", "mean"), MaxVelo=("RelSpeed", "max"), IVB=("InducedVertBreak", "mean"),
        HB=("HorzBreak", "mean"), Extension=("Extension", "mean"), VAA=("VertApprAngle", "mean"))
    a = pa.groupby(keys, observed=True).agg(PA=("PA", "size"), K=("K", "sum"), BB=("BB", "sum"), AnyBB=("AnyBB", "sum"),
        HBP=("HBP", "sum"), BatHBP=("BatHBP", "sum"), HR=("HR", "sum"), SF=("SF", "sum"), H=("H", "sum"), TB=("TB", "sum"), AB=("AB", "sum"),
        WobaN=("woba_num", "sum"), WobaD=("woba_den", "sum"), ContactN=("ContactNum", "sum"), BIP=("BIP", "sum"),
        IP=("IP", "sum"), FPS=("FPS", "mean"), EA=("EA", "mean"))
    t = p.join(a)
    ratio = lambda n, den: n / den.replace(0, np.nan)
    out = pd.DataFrame(index=t.index)
    for stat, n, den in [("K%", "K", "PA"), ("BB%", "AnyBB", "PA"), ("Barrel%", "Barrel", "BarrelDen"),
        ("GB%", "GB", "BIPLA"), ("Whiff%", "Whiff", "Swing"), ("CSW%", "CSW", "Pitches"),
        ("IZWhiff%", "IZWhiff", "IZSwing"), ("Chase%", "Chase", "OOZ"), ("Strike%", "Strike", "Pitches"),
        ("Zone%", "Zone", "ZoneN"), ("Pre2k Zone%", "Pre2KZone", "Pre2K"), ("1-1 Win%", "Win11", "At11"),
        ("Put Away%", "PutAway", "TwoK"), ("wOBA", "WobaN", "WobaD"), ("wOBAcon", "ContactN", "BIP"), ("SLG", "TB", "AB")]:
        out[stat] = ratio(t[n], t[den])
    out["OPS"] = ratio(t.H + t.BB + t.BatHBP, t.AB + t.BB + t.BatHBP + t.SF) + out.SLG
    out["FIP"] = ratio(13 * t.HR + 3 * (t.BB + t.HBP) - 2 * t.K, t.IP) + 3.214
    for stat, col in [("FPS%", "FPS"), ("E&A%", "EA"), ("Avg Velo", "Velo"), ("Max Velocity", "MaxVelo"),
        ("iVB", "IVB"), ("HB", "HB"), ("Extension", "Extension"), ("VAA", "VAA")]:
        out[stat] = t[col]
    # Overall Avg Velo follows Team Trends: the staff's most-used Fastball/Sinker.
    if not by_pitch:
        heater = d[d.PitchType.isin(["Fastball", "Sinker"])].groupby(["PitcherTeam", "PitchType"]).agg(n=("PitchUID", "size"), v=("RelSpeed", "mean"))
        best = heater.reset_index().sort_values(["PitcherTeam", "n"], ascending=[True, False]).drop_duplicates("PitcherTeam").set_index("PitcherTeam").v
        out["Avg Velo"] = best.reindex(out.index)
    total = d.groupby("PitcherTeam").size()
    out["Usage%"] = t.Pitches / (t.index.get_level_values("PitcherTeam").map(total) if by_pitch else t.Pitches)
    return out.replace([np.inf, -np.inf], np.nan)


def averages(d: pd.DataFrame, pa: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for by_pitch in [False, True]:
        values = team_values(d, pa, by_pitch)
        groups = values.groupby(level="PitchType") if by_pitch else [("", values)]
        for pitch, teams in groups:
            if by_pitch and rules.is_bad_pitch_type(pd.Series([pitch])).iloc[0]:
                continue
            for stat in teams:
                v = pd.to_numeric(teams[stat], errors="coerce").dropna()
                if by_pitch and stat == "Usage%":
                    # A team that never threw this pitch contributes zero usage.
                    v = v.droplevel("PitchType").reindex(d.PitcherTeam.unique(), fill_value=0)
                if len(v):
                    rows.append(dict(scope="pitch_type" if by_pitch else "overall", pitch_type=pitch, stat=stat,
                        value=float(v.mean()), team_n=len(v)))
    return pd.DataFrame(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--year", type=int, default=2026)
    parser.add_argument("--output", type=Path, default=ROOT / "WallyApps/PitchingApp/reference/d1_team_trend_averages.csv")
    args = parser.parse_args()
    wanted = ["PitchUID", "Date", "PitchNo", "PitchofPA", "PAofInning", "Pitcher", "PitcherTeam", "GameID", "GameUID", "Top/Bottom", "Inning",
        "PitchCall", "PlayResult", "KorBB", "Balls", "Strikes", "OutsOnPlay", "RelSpeed", "Extension", "InducedVertBreak", "HorzBreak", "VertApprAngle",
        "ExitSpeed", "Angle", "PlateLocHeight", "PlateLocSide", "ModelPitchType", "ModelPitchTypeRaw", "TaggedPitchType", "AutoPitchType"]
    columns = set(pq.ParquetFile(args.source).schema_arrow.names)
    rows = pq.read_table(args.source, columns=[c for c in wanted if c in columns]).to_pandas()
    teams = set(pd.read_csv(ROOT / "data/reference/NcaaColors.csv").team_abbr)
    rows = rows[rows.Date.astype(str).str.startswith(str(args.year)) & rows.PitcherTeam.isin(teams)]
    print(f"Preparing {len(rows):,} pitches from {rows.PitcherTeam.nunique()} mapped D1 teams", flush=True)
    d, pa = prepare(rows)
    out = averages(d, pa)
    out.insert(0, "season", args.year)
    out.to_csv(args.output, index=False)
    provenance = dict(source=args.source.name, source_bytes=args.source.stat().st_size, season=args.year,
        pitches=len(d), plate_appearances=len(pa), teams=d.PitcherTeam.nunique(),
        team_filter="Exact team codes in data/reference/NcaaColors.csv; source Level D1",
        averaging="Equal mean of full team-season metrics, no individual-pitcher qualification or averaging",
        pitch_type="Team totals for each pitch type; PA stats attributed to the final pitch type",
        usage="Pitch-type usage includes zero for represented teams that never threw that pitch",
        rows=len(out))
    args.output.with_suffix(".json").write_text(json.dumps(provenance, indent=2) + "\n")
    print(f"Wrote {len(out)} D1 team averages to {args.output}", flush=True)


if __name__ == "__main__":
    main()
