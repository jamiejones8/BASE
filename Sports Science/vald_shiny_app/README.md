# VALD strength and conditioning Shiny app

Within BASE, this app is loaded lazily as the **Player Health** workspace through
`R/integrations/player_health_workspace.R`. The six dashboard tabs, scoring,
downloads, and refresh behavior are preserved; BASE supplies the outer shell and
scoped visual treatment. The standalone launch instructions below remain useful
for isolated Sports Science development.

This is a separate runnable copy of all 12 R scripts in `cmj_sprint_share_handoff`. The original folder is untouched. The six original tabs, Texas State styling, scoring thresholds, season dates, CSV/HTML exports, sprint views, and force-trace comparisons are preserved. Changes address startup, daily refresh, and data-loading reliability.

## Launch

From a terminal:

```sh
cd "Sports Science/vald_shiny_app"
Rscript run.R
```

Open http://127.0.0.1:3840. The required packages are installed locally in `.R-library` on this computer. On a new machine, run `Rscript install.R` from this folder first. Use a fresh R session so older package versions are not already loaded. `app.R` is also the standard Shiny entry point.

## Connect real data

Copy `.Renviron.example` to `.Renviron` and fill in your VALD API client ID, client secret, and tenant/team ID. `VALD_REGION=use` is the default. Restart the app after changing settings. These are VALD API credentials, not a VALD Hub browser password.

Place the optional VALD athlete export with `vald-id` and `Group 1`, `Group 2`, etc. columns in `data/legacy/athletes_export.csv`. It controls pitcher/position grouping. Existing profile groups are retained when the CSV is absent; new athletes without groups remain `Other` under the supplied classification logic. The original dashboard treats non-pitcher roles as hitters.

The original baseball filter is preserved: external IDs must match `TXST-Baseball-` followed by three digits. Athletes outside that pattern are excluded. The supplied cutoff is August 1, 2026; season phases extend through June 2027. Review these constants in `dashboard.R` for future seasons.

No ForceDecks RDS history or API credentials were included in the supplied folder. The three top-level CSV files are not inputs referenced by these scripts. The app therefore starts with an empty-data state rather than invented athlete results.

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
- `data/refresh/status.rds`: last attempt, last full success, result message.
- `data/refresh/worker.log`: current background pull log (may contain athlete records).

To migrate existing data, put the legacy RDS files in `data/legacy` before first launch. Once a published snapshot exists, use a successful refresh to publish changes. Preserve the whole `data` folder across app restarts/deploys. Keep `.Renviron` and `data` out of source control.

## Validation

```sh
Rscript tests/run_tests.R
```

Tests use temporary files and synthetic records, checking refresh timing, failed-pull snapshot retention, cross-process locking, modification-aware test merging, and populated Shiny server tables/scoring. Live API authentication and full historical reconciliation still require your credentials and must be verified against VALD Hub after the first pull. Very large histories may require additional memory for the worker and full snapshot.

Technical references: [VALD ForceDecks API guide](https://support.vald.com/hc/en-au/articles/38086939480729-A-guide-to-using-the-External-ForceDecks-API) and [Shiny reactive polling](https://shiny.posit.co/r/reference/shiny/0.9.1/reactivepoll.html).
