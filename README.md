---
title: Texas State BASE
emoji: ⚾
colorFrom: red
colorTo: yellow
sdk: docker
app_port: 7860
pinned: false
---

# Texas State BASE

**BASE — Baseball Analytics & Scouting Engine** is a configurable college-
baseball application built with R Shiny. One application image can serve
different teams by changing deployment settings, brand assets, and data files.

This deployment is preconfigured for the **Texas State Bobcats** and the
completed **2026 college season**. Team-report menus default to and remain
scoped to Texas State (`TEX_BOB`), while scouting tabs expose every team and
player in the complete College26 source.

## Unified application shell

Home is the directory for eight focused workspaces: Postgame Reports,
Pitching, Hitting, Opponent Scouting, Defensive Analytics, HomeBASE,
JUCO Scouting, and Data Processing. The legacy global navbar is hidden and a
persistent Home control remains available from every page.

Pitching, Hitting, Defense, Opponent Scouting, and JUCO Scouting are Wally workspace
integrations. Their team-analysis workflows run inside BASE, while the hitting,
pitching, and catching AARs are grouped under Postgame Reports. All six
ScoutingApp report workflows run inside BASE alongside the JUCO hitting and
pitching leaderboards.
They initialize only when first opened and use
scoped BASE styling. Catcher season analysis lives in Defense while the
single-game catcher AAR lives under Postgame Reports. See
[`docs/wallyapps/PITCHING_INTEGRATION.md`](docs/wallyapps/PITCHING_INTEGRATION.md),
[`docs/wallyapps/HITTING_INTEGRATION.md`](docs/wallyapps/HITTING_INTEGRATION.md),
[`docs/wallyapps/DEFENSE_INTEGRATION.md`](docs/wallyapps/DEFENSE_INTEGRATION.md),
and [`docs/wallyapps/SCOUTING_INTEGRATION.md`](docs/wallyapps/SCOUTING_INTEGRATION.md).

The integrated Pitching and Hitting workspaces use the season, squad, and
bullpen CSV files stored in their respective `WallyApps/*/data` folders. Their
player-menu checkboxes map directly to those filenames.

## 2026 data model

- The selected integration source is WallyApps' complete 2026 Division I
  pitching/hitting Parquet: 2,041,154 season rows across 351 team codes and 212
  fields. It replaces the older `College26.parquet` build input after parity
  validation; source routing is recorded in
  [`config/baseball_data_sources.json`](config/baseball_data_sources.json).
- The private data bucket contains a generated `derived2026/` runtime layer:
  compact player catalogs, 64 stable pitcher hash partitions, and a
  small Texas State subset. The bucket is mounted read-only, so the app
  does not download data at startup or depend on another Space or Dataset.
- Startup loads only the Texas State subset for team reports. All-college
  scouting menus use compact catalogs. Pitcher Scouting reads one hash
  partition; Hitter Scouting uses a bucket index to scan only partitions in
  which that hitter appears. Both retain recently viewed players in memory.
- If a generated hitter catalog is not present, BASE builds it once from the
  mounted partitions and caches it at `/base-data/app_state/hitter-catalog.rds`.
  This adds hitter search without duplicating the full pitch-level dataset.
- The large master is a staging/build input, not an app-local dependency. After
  cutover, production keeps one query-optimized national pitch dataset; the
  source master moves to cold archive instead of remaining as a second mounted
  copy. Small catalogs and the Texas State startup cache are permitted derived
  artifacts.
- `data/external/CapeCod26.parquet` is a supplemental source. When a selected college player
  has a name match in the Cape dataset, scouting pages can include those Cape
  pitches without changing the player's college affiliation.
- Pitch-type corrections from Pitcher Scouting are stored separately in
  `/base-data/app_state/pitch-retags.sqlite`, keyed by data source and
  `PitchUID`. The override layer is applied when a player loads, so corrections
  survive Space restarts without modifying either source Parquet file. Hitter
  Scouting reads the same override layer so pitch classifications stay aligned.
- `config/texas_state_roster_2027.csv` drives the home roster using the Texas
  State 2026 fall roster for the upcoming 2027 season.
- `config/texas_state_schedule_2027.csv` drives the upcoming schedule and the
  next-game card on the home page. It begins with the October 2026 fall
  scrimmages for the upcoming 2027 season.

## Repository layout

- `R/` holds the main application source, split by config, data helpers,
  modules, pages, reports, and integrations.
- `models/` stores local model artifacts used by scouting and report features.
- `data/reference/` stores reference tables used directly by the app.
- `data/external/` stores optional supplemental datasets such as Cape Cod data.
- `data/local/` stores local exploratory datasets and scratch exports.
- `scripts/build/`, `scripts/checks/`, and `scripts/tests/` separate runtime
  builders, validations, and focused tests.
- `leaderboards/` remains a self-contained embedded Shiny app.

More detailed folder conventions are documented in
[`docs/PROJECT_STRUCTURE.md`](docs/PROJECT_STRUCTURE.md).

The supplied Texas State primary, secondary, and SuperCat marks live in
`www/`. The SuperCat is used for compact navigation, the secondary mark on the
home scoreboard, and the primary mark on the analytics hub card.

## Configure a college team

1. Copy `.env.example` to `.env` for local use, or define the same variables in
   the deployment environment.
2. Set the team identity, `BASE_TEAM_DATA_CODE`, and
   `BASE_TEAM_DATA_PATTERN`. The code is used by leaderboards; the pattern is
   used to recognize the team in `PitcherTeam` and `BatterTeam` columns.
3. Put team logos and card images in `www/`. The embedded leaderboards can
   resolve its configured logo directly from that shared folder.
4. Stage the full master, then run `scripts/build/build_runtime_dataset.py` with
   `--season 2026` to create the one-copy query-on-demand runtime. Configure
   `BASE_NCAA_D1_MASTER_FILE` for provenance/build tooling and
   `BASE_RUNTIME_ROOT` for application reads. After validated cutover, archive
   the staged master rather than mounting both large copies. Set
   `BASE_CAPE_DATA_FILE` for an optional player supplement.
5. Set `BASE_ROSTER_FILE` and `BASE_SCHEDULE_FILE` for the college roster and
   schedule. Templates are available in `config_examples/`.

## College roster format

The roster CSV requires these columns:

| Column | Meaning |
| --- | --- |
| `Name` | Player's display name |
| `Pos` | Position abbreviation shown in BASE |
| `Number` | Jersey number |
| `Bats` | `R`, `L`, or `S` |
| `Throws` | `R` or `L` |
| `pos_type` | `Pitcher`, `Catcher`, `Infielder`, or `Outfielder` |

## College schedule format

The schedule CSV requires `DateTime`, `Opponent`, and `Venue`. It may also
contain `IsHome`, `OppAbbr`, `TeamWins`, `TeamLosses`, `OppWins`, and
`OppLosses`. `DateTime` is interpreted using `BASE_SCHEDULE_TIMEZONE`.

For normal college deployments, set `BASE_STATS_API_ENABLED=false`. The MLB
Stats API adapter remains available for leagues and teams represented by that
API, but is not required by the college application.

## Texas State brand sources

The configured digital colors are Texas State Maroon `#501214`, Texas State
Dark Gold `#AC9155`, and Texas State Bright Gold `#D7BD8A`. The included TXST
logo is an unmodified official logo downloaded from Texas State's public brand
site. Follow Texas State's current brand and trademark guidance when
distributing this application:

- Colors: https://brand.txst.edu/visual-identity/colors.html
- TXST logo guidance: https://brand.txst.edu/visual-identity/our-university-logo-system/other-logos/txst.html
- 2026 reference roster: https://txst.com/sports/baseball/roster/2026

## Main data expectations

The pitch-level season file can be parquet or CSV and should retain the
TrackMan-style fields used by the reports, including player/team names, pitch
tags, pitch calls and results, velocity/movement/release metrics, plate
location, count, date, and game identifiers. Individual pages tolerate some
missing optional metrics, but selectors require `PitcherTeam`, `BatterTeam`,
`Pitcher`, and `Batter` as appropriate.

## Run and verify

```powershell
Rscript check_team_config.R
Rscript leaderboards/scripts/precompute_leaderboards_cache.R
Rscript check_leaderboards_integration.R
R -e "shiny::runApp(host='0.0.0.0', port=7860)"
```

## Deployment image

Railway builds the application from `Dockerfile`, which uses the versioned
`ghcr.io/jamiejones8/base-r-dependencies` image. That image contains R, system
libraries, and R packages so normal application changes do not compile the
dependency stack again. GitHub Actions republishes it only when
`Dockerfile.dependencies` or its publishing workflow changes. When dependency
requirements change, update `Dockerfile.dependencies`, bump its image version,
publish it, and pin the resulting digest in `Dockerfile` before deployment.
