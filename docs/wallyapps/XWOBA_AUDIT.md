# Hitting xwOBA audit — September 4, 2026

The loaded 2026 season reproduces lower xwOBA for seven of eight hitters with
at least 20 PA. The largest discrepancy comes from expected contact values,
and a separate sample of opposing hitters shows the same direction. This is
evidence that the old grid needs validation for these data, not proof that
every player's difference is a modeling error.

## Current calculation

`summarize_overall()` takes the last recorded pitch of each PA. Actual wOBA
uses these weights: BB .690, HBP .720, single .880, double 1.247, triple 1.578,
HR 2.031, and zero for other outcomes. Intentional walks are excluded.

For contact, the app prefers a supplied expected-wOBA value; otherwise it
looks up `models/shared/xwoba_grid.rds` using exit velocity in mph and launch
angle in degrees. No contact predictions are supplied in the loaded team data,
so all predictions come from this grid. Its EV bins are 2 mph and angle bins
are 3 degrees. The indexing matches the old scouting implementation.

Full xwOBA is:

`(sum of contact predictions + .690 × BB + .720 × HBP) / scored PAs`

Strikeouts contribute zero and count in the denominator. Missing contact
predictions and unresolved terminal rows are omitted. xwOBAcon averages only
available contact predictions.

The artifact contains edges and a value matrix, with no training population,
season, sample sizes, outcome-probability components, or calibration metadata.
The repository does not contain its training script. Its actual training
population therefore remains unverified. MLB's definition also uses sprint
speed for some batted balls; this grid only uses EV and launch angle.
[MLB xwOBA glossary](https://www.mlb.com/glossary/statcast/expected-woba)

## Confirmed bug fixed

The source encodes HBP as `PitchCall = HitByPitch` while `PlayResult` and
`KorBB` remain `Undefined`. The calculation previously checked only the latter
two fields, missing all 38 HBP in the team's loaded 2026 season. Those PAs
received zero in actual wOBA and were omitted from xwOBA.

The outcome parser now accepts PitchCall. Before the fix, team wOBA/xwOBA was
.349/.318; after it, .370/.330. This fixes the outcome accounting, not the
contact-model discrepancy. The regression test now uses the actual source's
PitchCall-only encoding, which the earlier HBP test did not cover.

## Contact comparison on identical observations

| Sample | Scored batted balls | Actual contact wOBA | Expected contact wOBA |
| --- | ---: | ---: | ---: |
| Texas State hitters, 2026 | 770 | .449 | .372 |
| Opposing hitters facing Texas State pitchers, 2026 | 774 | .427 | .335 |

The opponent sample covers 271 distinct hitter names. It is a second local
sample, not a national calibration set. Comparing identical scored contacts
isolates the model discrepancy from walks, HBP, strikeouts, and missing data.

For Texas State, the largest mean gap is at 30–40° launch angles: 82 contacts,
.757 actual versus .475 expected. Above 40°, 125 contacts average .147 actual
versus .036 expected. This suggests investigating fly-ball calibration,
competition/park conditions, and measurement quality. It does not establish
which of those explanations is responsible.

## Player results after the HBP fix

| Hitter | PA | wOBA | xwOBA |
| --- | ---: | ---: | ---: |
| Brady Boles | 178 | .310 | .293 |
| Tanner Carson | 76 | .339 | .352 |
| Jackson Cotton | 210 | .368 | .327 |
| Ethan Farris | 100 | .464 | .317 |
| Bennett Fryman | 96 | .279 | .255 |
| Jacob Gillis | 176 | .350 | .333 |
| Clayton Namken | 201 | .424 | .377 |
| Jaquae Stewart | 250 | .393 | .347 |

## Coverage and next steps

Actual wOBA uses 1,287 recorded PAs; xwOBA uses 1,250. The 37 omitted PAs are
20 contacts missing EV/angle, four contacts with unavailable grid predictions,
and 13 unresolved terminal rows. Therefore these displayed rates currently
use different samples. Missing predictions are not being inserted as zeros.

The contact discrepancy remains when using exactly the same sample. It also
varies by season: fall 2025 contact wOBA is .457 actual versus .434 expected,
so a blanket upward offset is not justified.

Validate or rebuild the grid against a broad college contact dataset with
held-out evaluation, particularly by launch angle and EV. Preserve training
metadata and decide explicitly how to report missing-contact coverage. The
full national dataset is not available in this checkout, so no grid
recalibration or imputation was performed during this audit.

## Reproduce

From the repository root:

```sh
Rscript scripts/checks/audit_hitting_xwoba.R /tmp/base-xwoba-findings
Rscript scripts/tests/test_wally_hitting_integration.R
```

The audit verifies that its team calculations reproduce the production
summary and optionally exports player, season, contact-bin, missing-data,
and opponent-contact CSVs. Both commands passed with the loaded files.
