import importlib.util
from pathlib import Path
import unittest

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("team_averages", ROOT / "scripts/build_pitching_team_averages.py")
team = importlib.util.module_from_spec(spec)
spec.loader.exec_module(team)


def fixture():
    rows = []
    # Team A: 1 K / 10 PA across two very differently used pitchers.
    # Team B: 1 K / 2 PA. Equal-team average = 30%, pooled PA average = 16.7%,
    # and an equal-pitcher average = 50%.
    for club, results in [("A", [True] + [False] * 9), ("B", [True, False])]:
        for i, strikeout in enumerate(results):
            rows.append(dict(PitchUID=f"{club}-{i}", Date="2026-04-01", PitchNo=i + 1, PitchofPA=1,
                PAofInning=i + 1, Inning=1, GameID=club, GameUID=club, PitcherTeam=club,
                Pitcher="Starter" if i == 0 else "Reliever", **{"Top/Bottom": "Top"},
                PitchCall="StrikeSwinging" if strikeout else "InPlay", PlayResult="Out",
                KorBB="Strikeout" if strikeout else "Undefined", OutsOnPlay=1,
                TaggedPitchType="Fastball", PlateLocHeight=2.5, PlateLocSide=0, Balls=0, Strikes=0,
                RelSpeed=90, Extension=6, InducedVertBreak=15, HorzBreak=10, VertApprAngle=-5,
                ExitSpeed=np.nan if strikeout else 90, Angle=np.nan if strikeout else 10))
    return pd.DataFrame(rows)


class TeamAverages(unittest.TestCase):
    def test_live_app_parity_fixture(self):
        path = ROOT / "tests/fixtures/wallyapps/pitching"
        r = pd.read_csv(path / "data/2026 Season - cleaned.csv")
        r.Date = pd.to_datetime(r.Date, format="mixed").dt.strftime("%Y-%m-%d")
        d, pa = team.prepare(r)
        d.PitcherTeam = d.GameID
        pa.PitcherTeam = pa.GameID
        calculated = team.team_values(d, pa)
        expected = pd.read_csv(path / "team_trend_reference_check.csv").set_index("GameID")
        pd.testing.assert_frame_equal(calculated, expected[calculated.columns], check_names=False, check_dtype=False, atol=1e-12)

    def test_team_totals_then_equal_team_average(self):
        d, pa = team.prepare(fixture())
        values = team.team_values(d, pa)
        self.assertAlmostEqual(values.loc["A", "K%"], .1)
        self.assertAlmostEqual(values.loc["B", "K%"], .5)
        reference = team.averages(d, pa)
        row = reference[(reference.scope == "overall") & (reference.stat == "K%")].iloc[0]
        self.assertAlmostEqual(row.value, .3)
        self.assertEqual(row.team_n, 2)
        self.assertAlmostEqual(reference[(reference.scope == "pitch_type") & (reference.stat == "Usage%")].iloc[0].value, 1)

    def test_pitch_change_dedup_zone_and_missing_denominators(self):
        rows = fixture().iloc[:2].copy()
        rows.loc[rows.index[1], ["PAofInning", "PitchofPA", "PitchNo"]] = [1, 2, 2]
        rows.loc[rows.index[0], "PlateLocHeight"] = np.nan
        d, pa = team.prepare(pd.concat([rows, rows.iloc[[0]]]))
        self.assertEqual(len(d), 2)
        self.assertEqual(len(pa), 1)
        self.assertEqual(d.Zone.count(), 1)
        self.assertTrue(pd.isna(team.team_values(d, pa).loc["A", "Chase%"]))

    def test_strikes_fouls_and_terminal_k(self):
        rows = pd.concat([fixture().iloc[[0]]] * 5, ignore_index=True)
        rows.PitchUID = [f"pitch-{i}" for i in range(5)]
        rows.PitchofPA = rows.PitchNo = range(1, 6)
        rows.PitchCall = ["StrikeCalled", "FoulBall", "FoulBall", "BallCalled", "StrikeSwinging"]
        # Some exports repeat terminal outcomes on every pitch.
        d, pa = team.prepare(rows)
        self.assertEqual(d.StrikesPre.tolist(), [0, 1, 2, 2, 2])
        self.assertEqual(d.PutAway.sum(), 1)
        self.assertAlmostEqual(team.team_values(d, pa).loc["A", "Put Away%"], 1 / 3)

    def test_usage_includes_teams_without_the_pitch(self):
        rows = fixture()
        rows.loc[rows.PitcherTeam == "B", "TaggedPitchType"] = "Slider"
        d, pa = team.prepare(rows)
        reference = team.averages(d, pa)
        usage = reference[(reference.scope == "pitch_type") & (reference.stat == "Usage%")]
        self.assertEqual(set(usage.team_n), {2})
        self.assertEqual(usage.value.tolist(), [.5, .5])


if __name__ == "__main__":
    unittest.main()
