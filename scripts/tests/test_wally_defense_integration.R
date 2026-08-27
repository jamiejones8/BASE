#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_defense_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
fixture_dir <- file.path("tests", "fixtures", "wallyapps", "defense", "data")

defense_fixture <- readr::read_csv(
  file.path(fixture_dir, "BobcatsDefense2026.csv"),
  show_col_types = FALSE
)
batted_fixture <- readr::read_csv(
  file.path(fixture_dir, "BobcatsDefenseBattedBalls.csv"),
  show_col_types = FALSE
)
catching_fixture <- readr::read_csv(
  file.path(fixture_dir, "Catchers - 2026 Season-cleaned.csv"),
  show_col_types = FALSE
)
baseline_fixture <- readr::read_csv(
  file.path(fixture_dir, "d1_catcher_framing_metrics.csv"),
  show_col_types = FALSE
)

defense_fixture$source_file <- "BobcatsDefense2026.csv"
defense_fixture$row_in_file <- seq_len(nrow(defense_fixture))
defense_fixture$SeasonGroup <- "S26"
catching_fixture$source_file <- "2026 Season - canonical.parquet"
catching_fixture$row_in_file <- seq_len(nrow(catching_fixture))
catching_fixture$SeasonGroup <- "S26"

# Override only the adapter's input boundary. Wally's original standardization,
# metrics, UI, and server still execute unchanged inside the embedded workspace.
base_prepare_wally_defense_rows <- function() defense_fixture
base_prepare_wally_batted_rows <- function(defense_rows) batted_fixture
base_prepare_wally_catching_rows <- function(startup_rows = NULL) catching_fixture
base_prepare_catcher_framing_baseline <- function() baseline_fixture

workspace <- base_wally_defense_environment(catching_fixture)
if (!is.function(workspace$server)) fail("Embedded Defense server is unavailable.")
if (!nrow(workspace$defense_df)) fail("Defense fixture produced no standardized opportunities.")
if (!nrow(workspace$catching_df)) fail("Catching fixture produced no receiving rows.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Defense UI contains a nested full-page body element.")
}
if (!grepl("base-defense-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Defense UI is missing its scoped BASE layout wrapper.")
}
expected_tabs <- c(
  "Leaderboard", "Opportunities", "OF OAA", "IF OAA", "OAA",
  "Catcher Reports", "Catcher Season"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Defense UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

summary_rows <- workspace$summarize_defense(workspace$defense_df)
if (!nrow(summary_rows)) fail("Defense summary returned no fixture rows.")
catcher_rows <- workspace$prepare_catcher_receiving_rows(workspace$catching_df)
if (!nrow(catcher_rows)) fail("Catcher receiving preparation returned no fixture rows.")

shiny::testServer(workspace$server, {
  session$setInputs(def_season_groups = "S26")
  session$flushReact()
  if (!nrow(filtered_data())) fail("Defense season filter returned no fixture rows.")
  if (!length(catchers_all())) fail("Catcher selector returned no fixture choices.")
  invisible(output$leaderboard_overall_table)
})

cat(
  "Wally Defense integration passed:", nrow(workspace$defense_df),
  "standardized opportunities,", nrow(catcher_rows), "catcher rows,",
  length(expected_tabs), "tabs, and server smoke tests.\n"
)
