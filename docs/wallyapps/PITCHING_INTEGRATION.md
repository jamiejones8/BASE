# Pitching workspace integration

The first complete application slice mounts Wally's PitchingApp inside BASE as
the `team_pitching` workspace. It is not an iframe and does not launch a second
Shiny process. Its UI and server execute in an isolated R environment inside
the existing BASE session.

## Preserved behavior

The BASE Pitching workspace retains eleven team-analysis tabs in their
existing order:

1. Performance;
2. Pitch Metrics;
3. Season Summary;
4. Pitch Decay;
5. Locations;
6. Whiffs / Chases / Called Strikes / Barrels;
7. Stuff+;
8. Bullpens;
9. Leaderboard;
10. Team Report;
11. Team Trends.

AAR Builder and Recent AARs are exposed in the unified Postgame Reports
workspace with independent pitcher, season, and game controls. The standalone
PitchingApp retains the AAR in its original tab.

Nested tabs, output IDs, calculations, PDF builders, filters, and download
handlers remain owned by Wally's server. The standalone application still
loads its original files when run by itself, so the locked comparison baseline
continues to test the original workflow.

## Folder data route

When hosted by BASE, `R/integrations/wally_pitching_workspace.R` injects one
prepared payload built exclusively from these files in
`WallyApps/PitchingApp/data` before Wally's feature engineering runs:

- `2025 Season -cleaned.csv` maps to the 2025 Season checkbox;
- `2025 Fall -cleaned.csv` maps to the 2025 Fall checkbox;
- `2026 Squads - cleaned.csv` maps to the 2026 Squads checkbox;
- `2026 Season - cleaned.csv` maps to the 2026 Season checkbox; and
- `Bullpens - cleaned.csv` supplies bullpen mode and the Bullpens tab.

The integrated Pitching workspace no longer blends these rows with the shared
BASE startup partition or NCAA master. Duplicate pitch IDs across the folder
files are still removed, and each row retains its source filename.

## Situational pitching

Performance offers **Situational**, with Total followed by clean/mid-inning,
inning bands, PA-start score bands, Save Situations, Home/Away, and Day/Night.
The leaderboard's Situation selector applies the same masks before minimum-PA
qualification, table totals, and PDF exports. Save Situations means inning 7+
with a 1–3 run lead, so it overlaps Tight Lead.

Classify unfiltered game context before selecting pitchers or batter hands.
An appearance beginning with pitch 1 of PA 1 is clean; later entries stay mid
inning until the next half-inning. Deduplicate pitch events and accumulate runs
from both halves to get each PA's starting score. A missing half, PA, pitch
or run value, or a half without a recorded third out, invalidates subsequent
score reconstruction until an explicit HomeScoreBefore/AwayScoreBefore
snapshot anchors it. The embedded workspace
reads matching games from the Hitting CSVs and can query a compact projection
from the mounted NCAA master to fill context gaps. Those context rows never
contribute to performance statistics. Missing context remains unclassified, with coverage disclosed.

The bundled `reference/pa_scores_2026.csv` supplies verified PA-start scores from
the full D1 export when the internal player-filtered CSVs omit earlier PAs.
This restores historical close-lead/save classifications for relievers such as
Cade Smith. See [Development Goals](DEVELOPMENT_GOALS.md) for rebuild and provenance.

Day/Night uses each pitch's UTC instant, or venue-local wall clock when UTC
is absent. A pitch at least 30 minutes before sunset is Day; later pitches are
Night. A PA crossing the cutoff assigns its outcome to the terminal pitch's
side while retaining the full-PA count, FPS, and E&A context. Unknown venues
or timestamps are excluded from Day/Night, rather than assumed to be day.

`WallyApps/PitchingApp/reference/venue_locations.csv` maps the observed stadium
aliases and supplied schedule venues to local areas and IANA timezones. Area
coordinates come from the [2025 Census Gazetteer](https://www.census.gov/geographies/reference-files/2025/geo/gazetter-file.html).
Sunset is calculated offline using [USNO solar coordinates](https://aa.usno.navy.mil/faq/sun_approx)
and [NOAA's apparent-sunset horizon](https://gml.noaa.gov/grad/solcalc/solareqns.PDF).
San Marcos is the fallback for known TXST home games and scrimmages; a mapped
stadium or neutral venue takes precedence. Extend the alias reference for new
road venues. No live weather or geocoding API is required.

`scripts/tests/test_pitching_situations.R` covers classification boundaries,
missing context, doubleheaders, duplicate exports, DST, and venue-local time.
The Pitching integration test exercises all 15 leaderboard filters, Total
parity, and a PA crossing the Day/Night boundary.

## Navigation and appearance

The compatibility `navbarPage` still owns routing, but its global navbar is
hidden. Eight ordered cards on Home open the approved workspaces, and a fixed
Home control remains in the upper-left corner. Existing tool pages remain as
hidden compatibility routes while their parent workspaces are migrated.

The Pitching app receives a scoped BASE theme: BASE typography, surfaces,
spacing, controls, tabs, tables, borders, and shadows. Wally's global body and
watermark CSS is not injected into BASE, so it cannot restyle unrelated pages.

## Loading behavior

The entire Pitching source, data preparation, UI, and server graph initialize
only on the first visit to Pitching. The isolated environment is then shared as
read-only application state, while every session receives its own Wally server
reactives. The temporary prepared-data cache is cleared after initialization.

## Verification

`scripts/tests/test_wally_pitching_integration.R` verifies:

- resolution of the five required PitchingApp folder files;
- duplicate removal across folder files;
- retention of a unique bullpen event;
- exposure of all eleven hosted Pitching tabs plus the moved AAR UI;
- successful registration of the original server graph; and
- a live Pitch Decay payload plus table and Plotly output.

The existing Wally golden suite remains the calculation and rendering parity
gate for HittingApp, PitchingApp, and DefenseApp.

## Season-specific pitcher choices

Bennett Fryman and Jackson Cotton are hidden from pitcher dropdowns when all
selected seasons are Fall 2026 or later. Earlier seasons, mixed selections that
include an earlier season, and all-season selections retain their historical
availability. This applies to the sidebar, AAR, Stuff+, Team Trends, comparisons
and pitching Development Goals, including names restored from saved plans.
Hitting selections, saved plans and recorded pitching statistics are unchanged.

## Team Trends benchmarks and release profiles

Both the game and rolling charts show a dashed **D1 team avg** for each selected
metric, including pitch-type selections. These use the bundled
`reference/d1_team_trend_averages.csv`, calculated from full 2026 team totals:
each represented D1 team contributes one value to the average. Individual pitcher
qualification/percentiles are not used. Rates are pooled within each team before
averaging teams. The national parquet is not loaded by the app at runtime.
Pitch-type usage includes zero for teams that did not throw that pitch. Team
trend counts are reconstructed in PA/pitch order and restored to their original
rows, so preparing rows repeatedly cannot shift counts; a terminal strikeout is
counted once per PA for Put Away%.
The adjacent JSON records the source, season, team coverage and calculation rule.
Rebuild with `python3 scripts/build_pitching_team_averages.py --source /path/to/D1.parquet`;
the offline builder uses pandas, numpy and pyarrow. Its local calculation tests
run outside the Docker build; the deployment suite validates the bundled reference.

The Release Points chart and table share the **Rosters & Recruiting** imported
measurements, saved current-player edits, recruiting profiles and departures.
The roster-season selector uses the same academic-class progression and remaining
eligibility as the roster client. The chart uses its five class colors, signed
LHP/RHP sides, shoulder anchors at height minus 10 inches, equal axis units and
Signed/Offered/Committed line patterns. Missing or zero side defaults to 1.5 feet
for plotting, without changing the profile. Incomplete measurements are omitted.
Saved profile changes refresh automatically. This reader never seeds or writes
roster state; the shared `TXST_DEPTH_STATE` path matches the roster workspace.
Game Report and Player Report continue to summarize the selected tracked pitches.
