# Opponent Scouting workspace integration

Wally's complete ScoutingApp now runs inside BASE as the
`opponent_scouting` workspace. It shares the main Shiny process and session; it
is not an iframe or a separately launched application.

## Preserved behavior

The original six workflows remain in their existing order:

1. Hitter Card;
2. Pitch Type Tables;
3. Heat Maps;
4. Pitcher Card;
5. Stuff Sheet; and
6. Matchup Grid.

ScoutingApp continues to own its hitter and pitcher standardization, report
calculations, player ordering, source filters, preview events, matchup scoring,
and PDF download handlers. BASE does not reimplement those calculations.

The previous BASE Opposing Pitchers and Opposing Hitters pages remain hidden
compatibility routes. This preserves existing deep links and keeps the
persistent Pitch Retagger available from Data Processing, but those pages are
no longer the visible Opponent Scouting entry point.

## Paths and deployment hygiene

Embedded Opponent Scouting defaults to **College season**. It opens the mounted
Parquet through Arrow, builds compact hitter and pitcher directories from grouped
team/name columns once per app process, and fetches pitches only for players
selected in the sidebar. Hitter and pitcher teams are independent, so Matchup
Grid can compare different schools without uploading CSVs. All six workflows
use their existing standardization and report calculations.

Set `BASE_SCOUTING_SEASON_FILE=/base-data/College26.parquet` on Railway to pin the
source explicitly. With no override, the adapter checks the configured
`BASE_NCAA_D1_MASTER_FILE` and the existing standard master-file locations,
including `/base-data/College26.parquet`. An explicit scouting override that is
missing produces an error instead of silently choosing another source.

No season copy or derived dataset is written. Arrow filters by both team and
player before collecting pitch rows into R. Recently selected groups share a
cache limited to 16 entries and 64 MiB per process. A single unsorted Parquet may
still require scanning much of the file for a first-time player lookup; production
latency should be measured on the actual mounted file. Existing `derived2026`
files are left untouched. Keep the master mounted while this workspace queries it.
Restart the app after replacing the season file to rebuild its menus and caches.

The **Game CSVs** option preserves the original file-based workflow.
Local development defaults to `WallyApps/ScoutingApp/data`, matching the
standalone app. A deployment may set `BASE_SCOUTING_DATA_DIR` to a writable,
approved scouting-report directory. FTP imports and the app's file selector
use that same directory.

The integrated loader does not read ScoutingApp's local `.Renviron`; BASE
receives credentials only from its process environment. Large local scouting
CSVs, R session files, generated plots, and output files remain outside Git,
Docker, and Railway contexts. Only `ScoutingApp.R` and its required visual
assets ship with the application image.

Embedded mode loads the individual data packages already present in BASE
instead of requiring the standalone `tidyverse` meta-package. The isolated
pitcher helper environment receives the same embedded flag and configured
data/output paths explicitly.

## Navigation, appearance, and loading

The Opponent Scouting Home card opens ScoutingApp directly. Its internal tabs
remain intact while the standalone title, page background, sidebar, cards,
tabs, inputs, buttons, and tables receive a scoped BASE treatment. Report plots
and exported PDFs are unchanged.

The workspace initializes only on its first visit and then reuses the same
isolated ScoutingApp environment for the session process.

## Verification

`scripts/tests/test_wally_scouting_integration.R` verifies the embedded shell,
all six workflow labels, the configured data-directory route, and both hitter
and pitcher standardization paths against synthetic fixtures. It also writes a
temporary season Parquet and verifies catalog-only initialization, exact player
and team filtering, team switching, cleared selections, shared matchup data,
metric parity, and useful missing-file/schema errors.

`scripts/checks/check_app_workspaces.py` verifies that Opponent Scouting is
lazy-loaded, that the old routes remain compatibility targets, and that
ScoutingApp adds no conflicting Shiny IDs to the unified process.
