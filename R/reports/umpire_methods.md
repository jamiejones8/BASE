# Umpire scorecard methods

The scorecard is a BASE report inspired by the public Umpire Scorecards explainers:

- Accuracy: https://umpscorecards.com/page/info/explainers/accuracy
- Consistency: https://umpscorecards.com/page/info/explainers/consistency
- Favor: https://umpscorecards.com/page/info/explainers/favor

It does not reproduce proprietary MLB ABS ratings or claim identical outputs.

## Accuracy

Only `BallCalled` and `StrikeCalled` pitches with finite plate locations qualify.
A ball touching the existing BASE zone (17 inches wide, 1.60–3.40 feet high;
1.45-inch ball radius) is a strike. Missing locations are counted and excluded.
No MLB measurement-error tolerance, individualized ABS zone, or challenge data
is inferred. Called-ball and called-strike accuracy use their own denominators.

## Estimated favor

The public method compares expected runs following a ball with expected runs
following a strike. The TrackMan exports available to BASE contain outs, counts,
and runs scored, but no reliable runner occupancy. Exact 288-state base/out/count
values therefore cannot be recovered from these files or from a scorecard image.

BASE uses a 36-state count/out empirical reference, plus six walk/strikeout
endpoint states, generated from the cleaned 2026 D1 parquet. Only completed half
innings contribute. One observation per PA/count/out state prevents repeated
fouls from overweighting a state. The walk endpoint includes any immediate run;
the strikeout endpoint measures runs remaining after the strikeout. All states
average over unobserved runners. These are D1 estimates, not Umpire Scorecards'
five-year MLB base-state values. `Balls`, `Strikes`, and `Outs` are TrackMan's
pre-pitch counts. No future outcome from the reported game changes a missed
call's assigned value.

A missed ball call benefits the batting team; a missed strike benefits the
fielding team. Home favor equals bottom-half batting benefits minus top-half
batting benefits. A missing count, outs, or inning half on any missed call makes
total favor unavailable; individual rateable misses can still be listed. No
unknown state is silently interpreted as bases empty, 0-0, or zero outs.

Rebuild the compact reference with:

```sh
Rscript scripts/data/build_umpire_run_expectancy.R '/path/to/cleaned D1 file.parquet'
```

## Consistency

A penalized logistic location model fits the game's called-ball/strike boundary
using horizontal and vertical coordinates and polynomial terms. Consistency is
the fraction of calls agreeing with the model's 50% boundary. Minimum support
is 30 located takes, including at least five balls and five strikes. Insufficient
support or failed convergence yields N/A. This is BASE's estimate of agreement
with a fitted zone, not the private Umpire Scorecards model. The estimated zone
is not displayed on the card.

## Score and coverage

Scores are the sum of recorded runs in each half; they are omitted if runs are
missing or either half is absent. Complete game exports are required for final
game scores. The report includes both pitching teams, deduplicates known pitch
IDs, and excludes bullpen files. It omits umpire identity, branding, relative
accuracy, ABS challenges, and the estimated-zone overlay.
