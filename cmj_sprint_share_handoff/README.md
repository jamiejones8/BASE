# TXST Baseball CMJ + Sprint Dashboard — Handoff to Slam Marcos Analytics

This is the setup guide for running the standalone `cmj_sprint_dashboard_share.R`
app (CMJ/ForceDecks + Sprint/SmartSpeed only — no ArmCare, no Trackman) on your
own infrastructure, including the "Refresh data" button that pulls new tests
live from VALD.

## 1. Files to copy

Everything lives relative to one folder. Keep this exact layout:

```
<app root>/
  cmj_sprint_dashboard_share.R
  scripts/
    01_config.R
    05_pull_forcedecks_profiles.R
    06_pull_forcedecks_tests_all_history.R
    07_pull_forcedecks_metrics_all_history_baseball.R
    08_refresh_forcedecks_baseball.R
  R/
    utils_dates.R
    utils_validation.R
    api_vald.R
    clean_forcedecks.R
    clean_smartspeed.R
    import_smartspeed.R
```

That's 12 files total. Nothing else from the source project is required —
no `config/`, no `modules/`, no ArmCare or Trackman code exists in this
handoff at all.

`scripts/05_pull_forcedecks_profiles.R` is included for completeness but is
off by default (see `VALD_RUN_VALDR_PROFILE_PULL` below); the app works fine
without ever using it.

### Why exactly these files, no more, no less

- `cmj_sprint_dashboard_share.R` is fully self-contained for *viewing* data —
  it has its own baked-in scoring thresholds, season-phase dates, and status
  logic. It does not need `config/*.yml` from the source project.
- The other 11 files are the **live VALD pull** used by the "Refresh data"
  button. Without them (or without VALD credentials — see below), Refresh
  still works, it just falls back to re-reading whatever's already on disk
  instead of pulling anything new.

## 2. R packages

Install once:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "DT", "plotly", "readr",
  "httr", "httr2", "jsonlite", "lubridate", "rlang", "tibble",
  "stringr", "tidyverse", "valdr", "keyring"
))
```

`valdr` is VALD's own official R package and is on CRAN — no special
repository or GitHub install needed. `keyring` is one of its dependencies
(used to store the VALD token securely) and will usually install
automatically with it, but it's listed explicitly since the refresh code
depends on it directly too.

## 3. Credentials (`.Renviron`)

Create a `.Renviron` file in the same folder as `cmj_sprint_dashboard_share.R`:

```
VALD_CLIENT_ID=<your VALD API client id>
VALD_CLIENT_SECRET=<your VALD API client secret>
VALD_TEAM_ID=<your VALD team/tenant id>
VALD_REGION=use
```

**`VALD_REGION` must be an actual VALD region code** (`use` for US East is
almost certainly correct for a US-based team) **— not a placeholder like
`prd`.** We hit this exact mistake in our own local setup while testing this
feature: the SmartSpeed pull builds its URL from this value
(`prd-<region>-api-...valdperformance.com`), and a wrong value fails with a
DNS error, not a clear "bad region" message. If Refresh reports a SmartSpeed
failure, check this first.

Without a valid `.Renviron`, the dashboard still works — every tab except
Curve View functions purely off whatever's already in `data/gold`/`Data`, and
Refresh just re-reads those files instead of pulling anything live.

### Optional environment variables

- `CMJ_SPRINT_SHARE_GOLD_DIR` / `CMJ_SPRINT_SHARE_LEGACY_DIR` — point the app
  at a data folder other than the default `./data/gold` / `./Data` (relative
  to wherever the app is launched from).
- `VALD_FORCE_FULL_REPULL_TESTS=true` / `VALD_FORCE_FULL_REPULL_SMARTSPEED=true`
  — force a full historical re-pull on the next Refresh instead of the normal
  incremental one. Use this only as a one-off recovery if the local data
  folder ever looks wrong/incomplete; unset it again afterward.
- Do **not** set `ARMCARE_RUN_PULL` or `ARMCARE_RUN_CSV_IMPORT` — this share
  copy forces both off on every refresh regardless, by design.

## 4. First launch

```r
shiny::runApp("cmj_sprint_dashboard_share.R", host = "0.0.0.0", port = <your port>)
```

(`host = "0.0.0.0"` if other people on your network need to reach it;
`127.0.0.1` for local-only access.)

If `data/gold`/`Data` don't exist yet or are empty, the app still starts —
Alert Inbox etc. will show an empty/"missing file" state until the first
Refresh populates them.

## 5. About the "Refresh data" button

- **First-ever refresh pulls full history** from VALD (however far back your
  VALD account has CMJ/SmartSpeed data) automatically — there's nothing to
  configure for this, it happens because the resume-from-last-pull logic has
  no prior local file to resume from yet. This can take several minutes
  (in our testing, low single-digit minutes for roughly a season's worth of
  CMJ data plus a full SmartSpeed history).
- **Every refresh after that is incremental** — it only asks VALD for tests
  modified since the last successful pull, so a same-day check (new CMJs
  taken an hour ago) is much faster. Note that the CMJ/ForceDecks side still
  *rebuilds its session-summary tables from the full historical dataset* on
  every refresh, even when the raw pull itself found nothing new — that
  rebuild step is the main thing that takes time on a "nothing changed"
  click, not the API call.
- **The whole app pauses for everyone while a refresh runs.** Shiny runs this
  synchronously and single-threaded, so if two coaches are looking at the
  dashboard and one clicks Refresh, both will see it freeze until the pull
  finishes. For a small staff tool this is an acceptable tradeoff (it keeps
  the alternative — a background job silently rebuilding files a viewer might
  be mid-read on — out of scope), but it's worth knowing before a first demo.
- ArmCare and Trackman are never touched by Refresh, regardless of any
  environment variable set on the host — this share copy doesn't use either.
- Pitcher/hitter role classification depends on an optional
  `Data/athletes_export.csv` (a VALD roster export with "Group 1/2/3..."
  columns). Without it, the refresh still succeeds; role classification just
  falls back to whatever's already in the existing roster data.

## 6. A filesystem gotcha worth knowing before your first deploy

The data layout uses two *different-cased* folders on purpose —
`data/gold/` (preferred) and `Data/` (legacy fallback), a convention carried
over from the source project. **On a case-insensitive filesystem (default
macOS, default Windows), these two folders silently collide into one.** We
hit this ourselves while building a test copy of this handoff on a Mac. It
is **not** a problem on a case-sensitive filesystem (any standard Linux
server, which is what we'd recommend for a real deployment anyway). If
you're first trying this out locally on a Mac or Windows machine, just be
aware `data/gold` and `Data` may end up pointing at the same physical
folder — it doesn't break anything functionally (the app just always finds
what it's looking for in one place instead of two), but it's a surprising
thing to notice if you go looking at the folder structure directly.

## 7. Quick sanity check after setup

1. Launch the app, confirm Alert Inbox loads without an error banner.
2. Click "Refresh data," wait for the notification (first run: expect a real
   wait; see above).
3. Confirm the notification says something like *"Pulled: CMJ/ForceDecks,
   SmartSpeed."* rather than a warning about missing credentials/files.
4. Spot-check one athlete's most recent CMJ date in Alert Inbox or Athlete
   Profile against what you know was tested most recently.

If Refresh reports a failure, the message will say which of the two
(CMJ/ForceDecks vs. SmartSpeed) failed and why — check `VALD_REGION` first
(see §3) if it's specifically SmartSpeed.
