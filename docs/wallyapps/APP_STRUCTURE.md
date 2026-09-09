# Step 4 application structure

This document records the user-approved information architecture for the
integrated BASE and Wally application. It supersedes a flat page-by-page
migration. The executable version is
[`config/app_workspaces.json`](../../config/app_workspaces.json).

## Governing idea

BASE is one application with several focused workspaces that can feel like
their own apps. The Home screen is the directory. It retains the existing BASE
scoreboard, command-card area, branding, and Texas State roster instead of
copying the reference site's visual identity.

The global top navigation bar is hidden. A persistent Home control remains in
the upper-left corner on every screen. Each workspace owns its internal tabs
and controls, preventing the global navigation from becoming a list of every
report and visualization.

## Home card order

| Order | Workspace | Purpose |
|---:|---|---|
| 1 | Postgame Reports | Hitting, pitching, and catching AAR workflows |
| 2 | Pitching | Texas State pitcher evaluation, bullpens, trends, and leaderboards |
| 3 | Hitting | Texas State hitter evaluation, lineup planning, trends, and leaderboards |
| 4 | Opponent Scouting | National opposing-pitcher and opposing-hitter scouting |
| 5 | Defensive Analytics | Positioning, opportunities, range/OAA, and interactive catcher statistics |
| 6 | HomeBASE | Searchable individual-player snapshots |
| 7 | Data Processing | Retagging, validation, and future preparation utilities |

Data Processing must remain the final card. The card is visible to every user
for now.

## Postgame Reports

Postgame Reports is deliberately narrow. It contains the three Wally AAR workflows:

1. Pitching AAR;
2. Hitting AAR;
3. Catching AAR.

Grouping these pages must not change report calculations, layouts, or download
behavior. The AARs are removed from their former Pitching, Hitting, and
Defensive Analytics tab sets; trends, leaderboards, and other team analysis
remain in those workspaces.

## Pitching

Pitching is the workspace for evaluating Texas State pitchers. Wally's
PitchingApp is the starting workflow where its processing and presentation are
stronger. Its interface will be decomposed into BASE modules and restyled with
BASE typography, colors, cards, controls, responsiveness, and navigation.

Planned capabilities include performance, pitch metrics, season summaries,
pitch decay, location/outcome views, bullpens, staff and player trends,
leaderboards, and team/game reviews. Unresolved formulas and model populations
remain behind the metric gates in `metric-register.json`.

## Hitting

Hitting is the workspace for evaluating Texas State hitters. Wally's
HittingApp is similarly the starting workflow where stronger. Planned
capabilities include performance, Lineup Builder, damage/whiff views, swing
decisions, ball flight, contact point, team trends, leaderboards, and team/game
reviews.

The Hitting and Pitching workspaces use the shared national 2026 source filtered
to Texas State plus explicitly labeled team supplements. They do not reload
their standalone application data directories.

## Opponent Scouting

Opponent Scouting hosts Wally's complete ScoutingApp as one focused workspace.
Its Hitter Card, Pitch Type Tables, Heat Maps, Pitcher Card, Stuff Sheet, and
Matchup Grid replace the previous two-card landing page without changing their
calculations, controls, preview behavior, report ordering, or PDF downloads.

The former BASE Opposing Pitchers and Opposing Hitters routes remain hidden
compatibility targets for existing deep links and the Data Processing retagger.
They are no longer presented as the Opponent Scouting workspace.

## Defensive Analytics and catching

Defensive Analytics combines BASE positioning and attribution work with Wally
opportunity, range/OAA, and catcher-framing capabilities as they pass their
validation gates.

Catching intentionally appears in two workflows without duplicating its
calculation engine:

- Postgame Reports provides the single-game catcher AAR and PDF;
- Defensive Analytics provides season receiving/framing statistics.

Both eventually consume one shared catcher calculation payload and the same
versioned reference population.

## HomeBASE

BASE Media is replaced by HomeBASE. HomeBASE is a role-aware player profile
workspace with two required entry paths:

1. a national search for any player present in the available pitcher and hitter
   catalogs, not only Texas State;
2. a direct link from every Texas State roster card on Home.

Search results must show enough context to distinguish names: player, team, and
role at minimum. Identity uses a stable player ID when available. Without an
ID, the fallback key is normalized name plus team plus role. Same-name players
must never be silently merged. If one stable ID appears as both pitcher and
hitter, HomeBASE may present both role panels within one profile.

HomeBASE opens on the configured Texas State roster and keeps national search
as a secondary college-player search mode. A selected player receives role-aware
season metrics, a five-game performance history, D1 stat percentiles,
and on-demand previews and PDF exports from the Wally hitter card and the CAPS
pitcher report in `R/reports/Pitcher_Card.R`. Percentile rows use statistical
metric names and values rather than generated player-type descriptions.
Two-way players keep both roles in one profile. Returning
players resolve against the bundled 2026 team season files. College-player
search exclusively uses the same mounted 2026 national Parquet source as the
scouting workspace. Environment-configurable history paths provide the handoff
point for the incoming season feed.

TrackMan team identifiers remain the internal filter keys. User-facing team
labels resolve through `config/trackman_team_names.txt`, including full program
and nickname names such as `BAY_BEA` to `Baylor Bears`.

## Data Processing

Data Processing is visible to all users for now and owns operations that
prepare or alter application data. Pitch Retagger moves here from Pitcher
Scouting. Its saved overrides remain separate from the canonical master and
must disclose original type, effective type, source, `PitchUID`, save status,
and revert behavior.

Future upload, validation, coverage, and preparation tools belong here rather
than inside reporting or scouting workspaces.

## Integration rule: choose the stronger implementation

The integrated application does not preserve a feature merely because it came
from BASE or Wally. For every feature group:

1. compare behavior using the Step 2 synthetic goldens;
2. select the stronger workflow or calculation deliberately;
3. keep unresolved metric differences explicit;
4. connect it to the shared Step 3 source route;
5. restyle it to BASE rather than embedding a standalone app;
6. remove the superseded implementation only after parity and browser checks.

The current feature-level decisions are recorded in
[`feature-register.json`](../../tests/baselines/wallyapps/feature-register.json).

## Shell implementation sequence

1. Add the workspace registry, eight ordered Home cards, hidden global navbar,
   and persistent top-left Home button.
2. Create workspace landing shells and route the existing BASE PDF and opponent
   scouting pages into their approved parents without changing their servers.
3. Rename BASE Media to HomeBASE, implement national catalog search, and wire
   Texas State roster-card deep links.
4. Build the Team Pitching shell and migrate Wally Pitch Decay as the first
   analytical vertical slice.
5. Build Team Hitting, then migrate its features in dependency order.
6. Expand Defense and shared catching after metric and reference approval.
7. Move Pitch Retagger into Data Processing and add future utilities there.

At every stage, old routes may remain hidden compatibility targets until their
replacement passes calculation, PDF, browser, and source-routing checks.
