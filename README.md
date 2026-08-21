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

## 2026 data model

- The complete `College26.parquet` master file is stored in the private
  `jamiejones8/base-app-data` Storage Bucket and mounted read-only inside the
  BASE Space at `/base-data/College26.parquet`.
- The same private bucket contains a generated `derived2026/` runtime layer:
  compact player catalogs, 64 stable pitcher hash partitions, and a
  16,266-row Texas State subset. The bucket is mounted read-only, so the app
  does not download data at startup or depend on another Space or Dataset.
- Startup loads only the Texas State subset for team reports. All-college
  scouting menus use compact catalogs. Pitcher Scouting reads one hash
  partition; Hitter Scouting uses a bucket index to scan only partitions in
  which that hitter appears. Both retain recently viewed players in memory.
- If a generated hitter catalog is not present, BASE builds it once from the
  mounted partitions and caches it at `/base-data/app_state/hitter-catalog.rds`.
  This adds hitter search without duplicating the full pitch-level dataset.
- The 201-column master remains untouched beside the runtime projection. New
  features can query it or add its fields to a regenerated projection, so
  optimization never discards source data.
- `CapeCod26.parquet` is a supplemental source. When a selected college player
  has a name match in the Cape dataset, scouting pages can include those Cape
  pitches without changing the player's college affiliation.
- Pitch-type corrections from Pitcher Scouting are stored separately in
  `/base-data/app_state/pitch-retags.sqlite`, keyed by data source and
  `PitchUID`. The override layer is applied when a player loads, so corrections
  survive Space restarts without modifying either source Parquet file. Hitter
  Scouting reads the same override layer so pitch classifications stay aligned.
- `config/texas_state_roster_2026_reference.csv` drives the home roster.
- `config/texas_state_schedule_2026.csv` is intentionally header-only because
  the 2026 season is complete; the home page points users to reports and
  scouting instead of waiting for a future schedule.

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
4. Retain the full master in the deployment's `/base-data` bucket, then run
   `scripts/build_runtime_dataset.py` to create the query-on-demand runtime
   layer. Configure `BASE_RUNTIME_ROOT` to its mounted directory. Set
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
