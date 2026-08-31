#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_pitching_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_path <- file.path(
  "tests", "fixtures", "wallyapps", "pitching", "data",
  "2026 Season - cleaned.csv"
)
fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)
fixture$PitcherTeam <- TEAM_CONFIG$data_code
fixture$source_file <- "2026 Season - cleaned.csv"
fixture$row_in_file <- seq_len(nrow(fixture))
fixture$SeasonGroup <- "S26"
fixture$DataSource <- "Texas State internal — 2026 Season - cleaned.csv"
fixture$.base_source_priority <- 2L

# The integrated workspace must use the five CSVs in PitchingApp/data.
expected_files <- c(
  "2025 Season -cleaned.csv", "2025 Fall -cleaned.csv",
  "2026 Squads - cleaned.csv", "2026 Season - cleaned.csv",
  "Bullpens - cleaned.csv"
)
if (!identical(basename(base_pitching_supplement_paths()), expected_files)) {
  fail("Pitching workspace does not resolve the five PitchingApp folder CSVs.")
}

# Folder rows still deduplicate repeated pitch IDs while retaining bullpen rows.
supplement <- fixture[1:2, , drop = FALSE]
supplement$source_file <- "Bullpens - cleaned.csv"
supplement$DataSource <- "Texas State internal — Bullpens - cleaned.csv"
supplement$PitchUID[[2]] <- "fixture-unique-bullpen-pitch"

prepared <- base_prepare_team_pitching_data(dplyr::bind_rows(fixture, supplement))
if (sum(prepared$PitchUID == fixture$PitchUID[[1]], na.rm = TRUE) != 1L) {
  fail("Duplicate folder pitch was not removed.")
}
if (!any(prepared$PitchUID == "fixture-unique-bullpen-pitch", na.rm = TRUE)) {
  fail("Unique bullpen supplement row was lost.")
}

workspace <- base_wally_pitching_environment(prepared)
if (!is.function(workspace$server)) fail("Embedded Pitching server is unavailable.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Pitching UI contains a nested full-page body element.")
}
if (!grepl("base-pitching-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Pitching UI is missing its scoped BASE layout wrapper.")
}
expected_tabs <- c(
  "Performance", "Pitch Metrics", "Season Summary", "Pitch Decay",
  "Locations", "Whiffs / Chases / Called Strikes / Barrels", "Stuff+",
  "AAR", "Bullpens", "Leaderboard", "Team Report", "Team Trends"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Pitching UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

pitcher <- as.character(workspace$txst_pitchers[[1]])
games <- unname(workspace$games_txst)
shiny::testServer(workspace$server, {
  session$setInputs(
    PitcherInput = pitcher,
    season_groups = "S26",
    Bullpens = FALSE,
    GameInput = games,
    BatterHand = c("L", "R")
  )
  session$flushReact()
  decay <- pitch_decay_base()
  if (!is.list(decay) || !all(c("pitches", "pa_last") %in% names(decay))) {
    fail("Pitch Decay did not return its preserved Wally payload.")
  }
  if (!nrow(decay$pitches)) fail("Pitch Decay returned no fixture pitches.")
  invisible(output$pitch_decay_table)
  invisible(output$pitch_decay_velocity)
})

cat(
  "Wally Pitching integration passed:", nrow(prepared),
  "folder-backed rows,", length(expected_tabs), "tabs, and Pitch Decay server smoke test.\n"
)
