# Development Goals

Hitting and Pitching each offer a Development Goals tab with an independent
single-player and season selector. Season values use the apps' existing numeric
summaries and D1 reference distributions, with the exact eleven metrics requested
for each side. Pitcher xwOBA uses the shared expected-contact grid and the same
BB/HBP/K treatment as Hitting; its empirical distribution qualifies pitchers at
50 completed PAs. Lower allowed wOBA, xwOBA, OPS, BB%, and Barrel% rank better.

Each objective contains a title, a selected season stat/current value, a written
goal, exactly three supporting actions, and zero to five optional drills. Export
is available when every required field is complete. Adding/removing Objective 2
preserves Objective 1. A player or season change clears the draft so one player's
instructions cannot accidentally be exported under another name. Drafts are
session-local and are not saved to a shared server file.

Charts plot one point per completed PA in season order, with a configurable
rolling window (default 25 PAs). The owning app recalculates its native metrics
from all pitches in each PA window, retaining swing/contact denominators.
Before the window fills, use the available completed PAs. D1 reference lines
reuse each Performance page's benchmarks (pitcher xwOBA uses its qualified-player
reference mean). Green segments begin/end at the exact numeric goal crossing,
with the appropriate higher/lower direction. A numeric chart-goal override
handles written goals containing multiple numbers.

The browser preview and PDF share the same renderer in
`WallyApps/shared/development_goals.R`. Single objectives are centered; two use
equal widths and margins. Empty drills are omitted. Text wraps and scales within
the boxes, with limits disclosed if entered text would become illegible.
The PDFs use original-photo cutouts on the left, PA charts in the center and
one column of red/blue percentile rankings at the far right. Each rounded text
box sits directly on the photograph; there is no large white goal panel. Missing photos use initials, and unavailable values/ranks display N/A.
With two objectives, their charts sit side by side at half width, with wrapped
benchmark captions. The larger player name is bold gold. Percentile labels and
values are bold white without outlines. A maroon footer matches the top bar;
its bold white values statement has gold C, A, T and S initials.

Portrait sources are recorded in `WallyApps/shared/portraits.csv`; run
`python3 scripts/fetch_development_references.py` to refresh them from the official
[TXST roster](https://txst.com/sports/baseball/roster). Refreshing requires curl,
R, and magick because SIDEARM sometimes serves WebP bytes under JPEG URLs.
The app itself reads the normalized JPEGs without a runtime download.
`build_development_portrait_masks.R` removes the edge-connected backdrop and
feathers the alpha edge; alpha masks preserve every original RGB byte. Faces
are never generated or retouched. The runtime applies the small masks to native
rasters, keeping its bounded image cache small.

The supplied ballpark photograph was treated with the built-in image tool using
the prompt saved beside `assets/development-ballpark-maroon.png`. The monochrome
treatment uses only maroon shades, without a gold/sepia tint. The renderer
places that image at exactly 90% opacity. Maroon, gold, cream panels and the
existing bobcat logo follow the provided goal-graphic references. Text, stat
values, bars and borders remain vector elements in the PDF.

## Full-game score recovery

The player-filtered internal exports omit some historical PAs and players. The
compact `PitchingApp/reference/pa_scores_2026.csv` restores PA-start home/away
scores from complete game context in the D1 parquet, without adding national
pitches to a player's statistics. Raw explicit score snapshots take precedence.
The qualifying bands remain +1 to +3 for Tight Lead, -1 to -3 for Trailing Close,
and +1 to +3 in inning 7 or later for Save Situations.

Rebuild both this score reference and the pitcher xwOBA distribution with:

```
Rscript scripts/build_development_references.R /path/to/D1.parquet
```

The provenance JSON records source size, qualification, grid and row counts.
For the supplied 2026 source, Cade Smith has 49 completed PAs in Tight Lead and
18 in Save Situations. PAs still lacking verified context remain unclassified.

`scripts/tests/test_development_goals.R` verifies exact metric sets, required
fields, one/two-objective transitions, real portrait decoding, xwOBA outcomes,
and the Cade score regression. The existing Hitting and Pitching integration
suites verify both new tabs. Browser QA exercises both actual PDF downloads;
the PDFs are rendered and visually inspected for single/two-column alignment.
