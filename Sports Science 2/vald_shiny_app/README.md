# VALD strength and conditioning Shiny app

## Player health sections

The top navigation now contains VALD (the six existing views), ArmCare, PULSE, and Combined Health. Texas State logo assets are bundled in `www/health/`.

ArmCare combines all `Daily Log*.csv` files in `../data`, retaining historical records across exports. Overlapping exams are matched by player, date, time and exam type; the newest file wins. Exams without times are deduplicated only when their contents match. Keep earlier exports in this folder. If no Daily Log CSVs exist, the newest `.xlsx` workbook is used as a fallback. Workbooks use Fresh Exam Key Metrics, Post Exam Key Metrics, and Exam Data; CSV exams are separated by Exam Type. PULSE reads the latest `*_workload.csv` and `*_events.csv` there. Set `PLAYER_HEALTH_DATA_DIR` to use a different export directory. Files are checked for changes every five seconds. This is local export ingestion, not live ArmCare/PULSE API access. The existing Refresh data button continues to refresh VALD.

The ArmCare views include team/player summaries, strength and recovery, post-exam fatigue, strength/ROM exam tables, and CSV downloads. PULSE includes the diagnostic roster, dual-axis daily workload/A:C chart, chronic workload chart, throw-count calendar, selectable throw metrics, high-effort filtering, and CSV exports. Source values are preserved; relative strength is converted from a fraction to percent bodyweight. Daily workload dates preserve the source calendar date; event timestamps are interpreted in America/Chicago. Missing counts remain missing, not zero.

Combined Health uses the 2026 Fall roster and the confirmed name mappings in `config/player_aliases.csv`. The roster is read from `../2026 Fall Roster Template.xlsx`, sheet `Roster` (headers in row 2); set `FALL_ROSTER_FILE` for another path. Whitespace, capitalization, and Last, First ordering are normalized; other differences need an explicit mapping. All athlete dropdowns are limited to rostered players. VALD profile/curve dropdowns additionally require VALD history in the current season. Pitcher/hitter grouping comes from roster Position (RHP/LHP/P, including two-way positions). Raw historical vendor files are preserved. A missing/invalid roster produces an empty selectable roster with a visible error, rather than silently including former players.

### TrackMan performance KPIs

Keep the supplied CSV in `../data`. Add future exports with `trackman` anywhere in the filename and a `.csv` extension. The app checks for changes every five seconds while running. All matching files are combined, using `PitchUID` to remove overlap; the file with the newest modification time wins when the same pitch appears more than once. Keep earlier files for incremental history, or replace the cumulative export with a complete updated one. Missing required columns, pitch IDs and dates are reported in Combined Health.

The four KPIs are daily, per-player arithmetic means across individual pitches:

- Fastball/sinker velocity: `RelSpeed` (mph), tagged Fastball or Sinker. **Primary KPI / default selection.**
- Fastball/sinker spin: `SpinRate` (rpm), same tags.
- Breaking-ball velocity: `RelSpeed` (mph), tagged Cutter, Slider, Curveball or Sweeper.
- Breaking-ball spin: `SpinRate` (rpm), same breaking-ball tags.

The app uses `TaggedPitchType`, never AutoPitchType or model tags. It preserves the export's local `Date`. Missing/non-finite/nonpositive measurements are excluded from that metric's mean; a group with no valid measurements stays missing. Counts are reported separately for velocity and spin. Changeups, splitters and other tags are excluded from these four KPIs. No outlier trimming, interpolation, or imputation is applied.

The supplied `2026 Season Trackman data- cleaned.csv` is combined with the fall export and filtered to the current fall roster. The Combined Health date range initializes from the earliest imported TrackMan date so returning players' fall and spring history is visible. Placeholder rows and incomplete rows with no velocity/spin values are skipped with import notes; incomplete rows containing measurements are reported as errors. ISO and US month/day/year dates are accepted.

In **Combined Health**, choose a player and date range. Four fixed chart rows each have a searchable dropdown on the left, in this order:

1. **TrackMan:** fastball/sinker velocity by default; choose any of the four pitching KPIs.
2. **VALD:** CMJ Jump Height (Imp-Mom) by default. The searchable catalog is generated from every imported ForceDecks metric/test/unit combination and each populated numeric sprint field by sprint protocol. Bodyweight in Pounds is directly available. ForceDecks charts use the existing session-best summary values; sprint charts retain individual test measurements. Raw source units are preserved. Unavailable metrics are not invented or converted to a percent without a source definition. New imported metrics become selectable on refresh.
3. **ArmCare:** Arm Score by default. Total strength, internal rotation, external rotation, scaption and grip each display raw pounds and percentage of body weight on separately labeled axes. The source exports pounds, not PSI. IR/ER/scaption/grip use the vendor's relative-strength fields multiplied by 100. Total %BW is calculated as 100 × total strength / body weight; unavailable body weight leaves that series missing. These percentages are relative strength, not recovery percentages or Arm Scores.
4. **PULSE:** exported ACWR by default. Other choices are acute, chronic and daily workload, plus arm speed, arm slot, shoulder rotation and torque. Throw metrics use daily arithmetic means over finite measurements from exported events, including simulated throws, with counts in hover text; dates use America/Chicago. Workloads and ACWR are imported, not recalculated.

All four charts retain their position even when a player has no data for a source. Their date-axis ranges follow the same date filter. Missing measurements remain missing and break lines; values are never filled with zeros. Metric choices persist when changing players or refreshing data within the session. A new session starts with the four defaults above.

KPI cards show the latest valid daily pitching mean within the selected dates and its valid-pitch count. The daily table and CSV download include sample counts, normalized player keys, TrackMan pitcher IDs and source filenames for future correlation work. Correlation screening is available in the KPI Correlations sub-tab; no combined health score is calculated.

Additional verification: `Rscript tests/health_tests.R` and `Rscript tests/roster_trackman_tests.R`, and `Rscript tests/combined_trends_tests.R`.

This is a separate runnable copy of all 12 R scripts in `cmj_sprint_share_handoff`. The original folder is untouched. The six original tabs, Texas State styling, scoring thresholds, season dates, CSV/HTML exports, sprint views, and force-trace comparisons are preserved. Changes address startup, daily refresh, and data-loading reliability.

## Launch

From a terminal:

```sh
cd "/Users/austinwallace/Desktop/Sports Science/vald_shiny_app"
Rscript run.R
```

Open http://127.0.0.1:3840. The required packages are installed locally in `.R-library` on this computer. On a new machine, run `Rscript install.R` from this folder first. Use a fresh R session so older package versions are not already loaded. `app.R` is also the standard Shiny entry point.

## Connect real data

Copy `.Renviron.example` to `.Renviron` and fill in your VALD API client ID, client secret, and tenant/team ID. `VALD_REGION=use` is the default. Restart the app after changing settings. These are VALD API credentials, not a VALD Hub browser password.

When this workspace is embedded in BASE, the deployed Texas State defaults are
loaded from `R/config/player_health_deployment.R`; the ignored `.Renviron` is
only a standalone/local option. BASE also routes generated files to
`/base-data/app_state/player-health` when the Railway volume is mounted.

Place the optional VALD athlete export with `vald-id` and `Group 1`, `Group 2`, etc. columns in `data/legacy/athletes_export.csv`. It controls ingestion-stage grouping; the UI uses the fall roster positions. Existing profile groups are retained when the CSV is absent; new athletes without groups remain `Other` under the supplied classification logic. The original dashboard treats non-pitcher roles as hitters.

The original baseball filter is preserved: external IDs must match `TXST-Baseball-` followed by three digits. Athletes outside that pattern are excluded. The supplied cutoff is August 1, 2026; season phases extend through June 2027. Review these constants in `dashboard.R` for future seasons.

VALD uses the locally published snapshot from successful API refreshes. ArmCare, PULSE and TrackMan use the exports described above.

## Daily and on-demand refresh

With credentials configured, the app starts a background pull when data is due. Default interval: 24 hours after the last fully successful refresh. `VALD_REFRESH_HOURS` changes that interval; `VALD_AUTO_REFRESH=false` disables the app timer. A failed pull is retried after one hour. The **Refresh data** button requests an immediate pull.

Initial ForceDecks refresh pulls historical tests, then subsequent pulls resume with a one-day modification overlap. Tests are merged by test ID; modified tests have their metrics fetched again. Raw progress is retained to allow retry. SmartSpeed uses the supplied incremental merge pipeline. ForceDecks regional URLs now honor `VALD_REGION`.

Pulls run in a separate R process. A filesystem lock prevents concurrent writers. The UI reads a complete atomically published `data/refresh/dashboard.rds` snapshot, so it retains the previous data during failures and refreshes all connected sessions after publication. A successful ForceDecks rebuild can publish even if SmartSpeed fails; the status reports that partial failure and the complete-refresh timestamp does not advance.

The app timer operates only while the R process is running. It cannot wake a sleeping laptop or a suspended hosting service. For truly unattended daily operation, run this command using your hosting platform's scheduler on an always-on machine with persistent storage:

```sh
Rscript "/absolute/path/to/vald_shiny_app/refresh.R" --force
```

Without `--force`, it respects the configured interval and retry delay. The command exits nonzero on failure. No operating-system schedule or hosted deployment has been installed by this project. Deploy behind your organization's normal access controls before giving staff network access.

## Files and maintenance

- `dashboard.R`: original dashboard with refresh integration and empty-data fixes.
- `scripts/`: copied ForceDecks ingestion and summary scripts.
- `R/`: copied cleaning, validation, date and API helpers.
- `runtime/refresh_runtime.R`: background worker, locking, publication, scheduling.
- `data/legacy/`, `data/gold/`: pipeline files; distinct directories on macOS too.
- `data/refresh/status.rds`: last attempt, last full success, result message. Unreadable or invalid status files no longer interrupt polling: the app retains cached timestamps and retries the read on each poll. If no cached status exists, automatic refresh uses a one-hour backoff; manual refresh remains available. Status recovery does not delete or replace dashboard data.
- `data/refresh/worker.log`: current background pull log (may contain athlete records).

To migrate existing data, put the legacy RDS files in `data/legacy` before first launch. Once a published snapshot exists, use a successful refresh to publish changes. Preserve the whole `data` folder across app restarts/deploys. Keep `.Renviron` and `data` out of source control.

## Validation

```sh
Rscript tests/run_tests.R
```

Tests use temporary files and synthetic records, checking refresh timing, failed-pull snapshot retention, cross-process locking, modification-aware test merging, and populated Shiny server tables/scoring. Live API authentication and full historical reconciliation still require your credentials and must be verified against VALD Hub after the first pull. Very large histories may require additional memory for the worker and full snapshot.

Technical references: [VALD ForceDecks API guide](https://support.vald.com/hc/en-au/articles/38086939480729-A-guide-to-using-the-External-ForceDecks-API) and [Shiny reactive polling](https://shiny.posit.co/r/reference/shiny/0.9.1/reactivepoll.html).

## KPI Correlations

Combined Health now has **Time Series** and **KPI Correlations** sub-tabs. The player and date range at the top apply to both. Select any TrackMan KPI to rank up to ten eligible within-player associations by descending absolute Pearson r (default) or Spearman rho. No data from other players is pooled.

Candidates include all imported ForceDecks measurements, numeric SmartSpeed performance fields (excluding rep-count metadata), the eight PULSE chart metrics, ArmCare scores and raw/relative strengths, shoulder balance/SVR/velocity, post-exam strength/loss/retention, and exported ROM/arc/primer measurements. Categorical recovery labels are not converted to numbers. Metric units and exam/test types stay separate. ArmCare relative strength uses %BW, not PSI.

Each candidate is collapsed to a daily mean when multiple observations exist on a date. ForceDecks inputs are the existing session-best values; sprint inputs are individual imported test results; PULSE event inputs are daily means. TrackMan targets remain daily per-pitch means with at least five valid pitches by default. Dates without finite measurements are omitted, not filled.

Matching defaults to the closest measurement on the pitching date or up to three calendar days earlier. Later dates are excluded. Exact dates take priority; ties prefer the earlier pitching date. Greedy matching uses each source and pitching date only once per metric, trying other eligible dates when a closer source is already used. A same-day-only option is also available. Measurements stay within the selected date window. Same-day records are matched by calendar date, not appearance time, so this setting cannot exclude measurements taken after pitching on the same day.

At least eight distinct matched dates are required by default (adjustable, minimum five). Constant predictors or targets are excluded. Fewer than ten qualifying metrics produce fewer rows; an empty result reports coverage and missing overlap rather than lowering requirements. Strength labels are descriptive bins, not reliability grades: |r| < .30 weak, .30–.49 moderate, .50–.69 strong, and ≥ .70 very strong.

Click a result to inspect the paired-date scatterplot and export its raw pairs. Pearson detail includes a nominal Fisher 95% correlation interval and observed least-squares slope in KPI units per source unit. Spearman shows rank association without a linear-effect interpretation. Benjamini–Hochberg q-values are computed across every eligible candidate before selecting the top ten. Exports include all eligible metrics, settings and sample sizes.

These are exploratory associations, not causal impacts or prescribed training priorities. Screening many correlated metrics, outliers, shared seasonal trends, and serial dependence can inflate apparent relationships. Confidence intervals are not selection-adjusted or time-series-adjusted; p/q-values assume independence and should not be treated as confirmation in these repeated measurements. Duplicate or closely related metrics may occupy several ranks. Validate apparent relationships with additional dates and prospective staff review.

Implementation references: [R correlation tests](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html) and [R multiple-comparison adjustments](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html).

Validation: `Rscript tests/correlation_tests.R` covers positive/negative rankings, constants, sample thresholds, zero-to-three-day earlier matching and rejection of future dates, exact-date/tie priority, source-date non-reuse, multiplicity adjustment, sparse real-player data and a populated in-memory UI fixture. Synthetic observations are never written into app data.

### TrackMan source coverage (September 29, 2026)

The import combines `2026 Season Trackman data- cleaned.csv`, `2026 Fall Trackman data- cleaned.csv`, and `2026 Squads Trackman data- cleaned.csv`. The 2025 Fall export is also included. All four feed the four pitching KPIs and KPI Correlations through the same daily series, subject to the selected player/date range and valid-pitch minimum. Combined Health now initializes its date range from the earliest imported TrackMan date, including Fall 2025. ArmCare initializes from its earliest imported exam date. Subsequent manual date selections are preserved. Files are discovered automatically every five seconds.

The current Squads export contributes 1,442 roster pitch records (1,270 in the four KPI pitch groups). Its 285 measured rows with missing PitchUID on January 22–23 are excluded and reported by the importer; a corrected export with original pitch IDs is needed to include those records safely. Duplicate PitchUIDs across exports are counted once, with the newest file taking precedence.

ArmCare history audit: the supplied Daily Log spans August 27, 2024–September 28, 2026. Correlations use every imported matching player's exam in the selected date range, not just their latest exam. Source-specific eligible-metric counts and maximum paired-date counts are shown above the rankings, so absence from the top ten can be distinguished from insufficient overlap. Eligibility is recalculated using the selected earlier-only matching window; some players may still have too few matched dates despite a long overall export range. CSV ingestion does not retrieve additional records from ArmCare online; include the desired full history in your exports.

KPI Correlations has four result tabs: **Overall** retains the top ten across sources; **VALD**, **ArmCare**, and **PULSE** show every eligible metric for that source in a searchable, paginated table. Rankings remain descending absolute correlation, with the same minimum sample and three-day earlier-only matching rules. Q-values still adjust across all eligible metrics, not just the selected source. Clicking a row updates the shared detail, scatterplot, and paired-date export. Export current tab downloads all eligible rows for that source (or the overall top ten); Export all eligible correlations continues to include every source. The coverage table follows the selected source.
