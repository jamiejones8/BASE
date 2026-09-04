#!/usr/bin/env Rscript

source("team_config.R", local = FALSE)
source("R/integrations/wally_scouting_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_dir <- file.path(
  "tests", "fixtures", "wallyapps", "hitting", "data"
)
fixture_files <- list.files(fixture_dir, pattern = "\\.csv$", full.names = FALSE)
if (!length(fixture_files)) fail("Scouting integration fixture is unavailable.")

workspace <- base_wally_scouting_environment(fixture_dir)
if (!is.function(workspace$server)) fail("Embedded Scouting server is unavailable.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Scouting UI contains a nested full-page body element.")
}
if (!grepl("base-scouting-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Scouting UI is missing its scoped BASE layout wrapper.")
}

expected_tabs <- c(
  "Hitter Card", "Pitch Type Tables", "Heat Maps",
  "Pitcher Card", "Stuff Sheet", "Matchup Grid"
)
missing_tabs <- expected_tabs[
  !vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)
]
if (length(missing_tabs)) {
  fail("Embedded Scouting UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

shiny::testServer(workspace$server, {
  available <- data_files()
  if (!all(fixture_files %in% available)) {
    fail("ScoutingApp is not reading its BASE-configured data directory.")
  }
  session$setInputs(csv_files = fixture_files[[1]])
  session$flushReact()
  hitter_rows <- std_all()
  pitcher_rows <- pitcher_std_all()
  if (!nrow(hitter_rows)) fail("Hitter scouting standardization returned no fixture rows.")
  if (!nrow(pitcher_rows)) fail("Pitcher scouting standardization returned no fixture rows.")
  expected_splits <- c("Total", "v LHP", "v RHP")
  if (!identical(workspace$row1_table_numeric(hitter_rows)$Split, expected_splits) ||
      !identical(workspace$fps_metrics_by_hand(hitter_rows)$Split, expected_splits) ||
      !identical(workspace$fps_metrics_by_hand(hitter_rows[0, ])$Split, expected_splits)) {
    fail("Scouting handedness tables are not Total, left, right.")
  }
})

cat(
  "Wally Scouting integration passed:", length(expected_tabs),
  "workflows and shared hitter/pitcher server smoke tests.\n"
)
