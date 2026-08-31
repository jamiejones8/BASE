#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_hitting_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_path <- file.path(
  "tests", "fixtures", "wallyapps", "hitting", "data",
  "2026 Season - cleaned.csv"
)
fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)
fixture$BatterTeam <- TEAM_CONFIG$data_code
fixture$source_file <- "2026 Season - cleaned.csv"
fixture$row_in_file <- seq_len(nrow(fixture))
fixture$SeasonGroup <- "S26"
fixture$DataSource <- "Texas State internal — 2026 Season - cleaned.csv"
fixture$.base_source_priority <- 2L

# The integrated workspace must use the four CSVs in HittingApp/data.
expected_files <- c(
  "2025 Season -cleaned.csv", "2025 Fall -cleaned.csv",
  "2026 Squads - cleaned.csv", "2026 Season - cleaned.csv"
)
if (!identical(basename(base_hitting_supplement_paths()), expected_files)) {
  fail("Hitting workspace does not resolve the four HittingApp folder CSVs.")
}

# Folder rows still deduplicate repeated pitch IDs across selected season files.
supplement <- fixture[1:2, , drop = FALSE]
supplement$source_file <- "2026 Squads - cleaned.csv"
supplement$DataSource <- "Texas State internal — 2026 Squads - cleaned.csv"
supplement$PitchUID[[2]] <- "fixture-unique-hitting-supplement"

prepared <- base_prepare_team_hitting_data(dplyr::bind_rows(fixture, supplement))
if (sum(prepared$PitchUID == fixture$PitchUID[[1]], na.rm = TRUE) != 1L) {
  fail("Duplicate folder pitch was not removed.")
}
if (!any(prepared$PitchUID == "fixture-unique-hitting-supplement", na.rm = TRUE)) {
  fail("Unique hitting supplement row was lost.")
}

workspace <- base_wally_hitting_environment(prepared)
if (!is.function(workspace$server)) fail("Embedded Hitting server is unavailable.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Hitting UI contains a nested full-page body element.")
}
if (!grepl("base-hitting-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Hitting UI is missing its scoped BASE layout wrapper.")
}
expected_tabs <- c(
  "Performance", "Lineup Builder", "Damage Heat Map", "Whiff Zones",
  "Team Report", "Game Reports (AAR)", "Leaderboard", "Swing Decisions",
  "Ball Flight", "Contact Point"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Hitting UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

hitter <- as.character(workspace$hitters_txst[[1]])
games <- unique(as.character(workspace$txst_df$CustomGameID))
games <- games[!is.na(games) & nzchar(games)]
if (!length(games)) fail("Hitting fixture exposed no selectable games.")

shiny::testServer(workspace$server, {
  # The embedded server starts before its dynamically inserted UI has sent any
  # input values. Startup must remain quiet until the first hitter arrives.
  session$flushReact()

  session$setInputs(
    Hitter = hitter,
    hit_season_groups = "S26",
    Game = games,
    PitcherHand = "All",
    hit_perf_split = "hand",
    hit_leader_seasons = "S26",
    hit_leader_hand = c("LHP", "RHP")
  )
  session$flushReact()
  filtered <- dat_filt()
  if (!nrow(filtered)) fail("Hitting Performance filter returned no fixture rows.")
  choices <- lineup_player_choices()
  if (!length(choices)) fail("Lineup Builder returned no fixture player choices.")
  invisible(output$perf_tbl)
  invisible(output$lineup_builder_ui)
})

cat(
  "Wally Hitting integration passed:", nrow(prepared),
  "folder-backed rows,", length(expected_tabs),
  "tabs, Performance, and Lineup Builder server smoke tests.\n"
)
