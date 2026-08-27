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
and pitcher standardization paths against synthetic fixtures.

`scripts/checks/check_app_workspaces.py` verifies that Opponent Scouting is
lazy-loaded, that the old routes remain compatibility targets, and that
ScoutingApp adds no conflicting Shiny IDs to the unified process.
