#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

source("team_config.R", local = FALSE)
source("R/data/team_season_imports.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

scratch <- tempfile("base-team-season-import-")
dir.create(scratch, recursive = TRUE)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

if (!identical(
  base_team_season_import_path("BP"),
  normalizePath(TEAM_CONFIG$data$bullpen_file, winslash = "/", mustWork = FALSE)
)) {
  fail("The bullpen importer and Pitching loader do not share the configured source file.")
}

game <- tibble::tibble(
  PitchUID = c("fall-pitch-1", "fall-pitch-2", "fall-pitch-3"),
  GameUID = "fall-game-1",
  PitchNo = as.character(1:3),
  Date = "09/15/2026",
  PitcherTeam = c(TEAM_CONFIG$data_code, "OPP", TEAM_CONFIG$data_code),
  BatterTeam = c("OPP", TEAM_CONFIG$data_code, "OPP"),
  Pitcher = c("Bobcat Pitcher", "Opponent Pitcher", "Bobcat Pitcher"),
  Batter = c("Opponent Hitter", "Bobcat Hitter", "Opponent Hitter"),
  PitchCall = c("StrikeCalled", "InPlay", "BallCalled")
)
game_path <- file.path(scratch, "fall-game.csv")
readr::write_csv(game, game_path)

first <- base_import_trackman_game(game_path, "F26", root = scratch)
if (!identical(first$inserted_rows, 3L) || first$total_rows != 3L ||
    !file.exists(first$destination)) {
  fail("The first 2026 Fall game was not written to its cumulative source.")
}

second <- base_import_trackman_game(game_path, "F26", root = scratch)
if (second$inserted_rows != 0L || second$duplicate_rows != 3L || second$total_rows != 3L) {
  fail("Re-importing the same game did not skip duplicate pitches.")
}

next_game <- game[1:2, ]
next_game$PitchUID <- c("fall-pitch-4", "fall-pitch-5")
next_game$GameUID <- "fall-game-2"
next_game$PitchNo <- c("1", "2")
next_game$NewTrackManField <- c("future-a", "future-b")
next_path <- file.path(scratch, "fall-game-2.csv")
readr::write_csv(next_game, next_path)
third <- base_import_trackman_game(next_path, "F26", root = scratch)
stored <- base_read_trackman_import(third$destination)
if (third$inserted_rows != 2L || third$total_rows != 5L ||
    !"NewTrackManField" %in% names(stored) ||
    !identical(tail(stored$NewTrackManField, 2), c("future-a", "future-b"))) {
  fail("Schema evolution or cumulative append behavior failed.")
}

bullpen <- game
bullpen$PitchUID <- paste0("bullpen-pitch-", seq_len(nrow(bullpen)))
bullpen$GameUID <- "bullpen-session-1"
bullpen$Date <- "09/15/2025"
bullpen_path <- file.path(scratch, "bullpen.csv")
readr::write_csv(bullpen, bullpen_path)
bullpen_first <- base_import_trackman_game(bullpen_path, "BP", root = scratch)
bullpen_second <- base_import_trackman_game(bullpen_path, "BP", root = scratch)
if (!identical(basename(bullpen_first$destination), "Bullpens - cleaned.csv") ||
    bullpen_first$inserted_rows != 3L || bullpen_first$total_rows != 3L ||
    bullpen_second$inserted_rows != 0L || bullpen_second$duplicate_rows != 3L) {
  fail("The bullpen drop did not append to and persist in its cumulative source.")
}
if ("BP" %in% names(base_team_season_import_paths(root = scratch))) {
  fail("The bullpen source leaked into the season-source loader list.")
}

wrong_year <- game
wrong_year$PitchUID <- paste0("wrong-", seq_len(nrow(wrong_year)))
wrong_path <- file.path(scratch, "wrong-year.csv")
readr::write_csv(wrong_year, wrong_path)
wrong_error <- tryCatch({
  base_import_trackman_game(wrong_path, "S27", root = scratch)
  NULL
}, error = identity)
if (is.null(wrong_error) || !grepl("only accepts 2027", conditionMessage(wrong_error), fixed = TRUE) ||
    file.exists(base_team_season_import_path("S27", root = scratch))) {
  fail("A 2026 game was not safely rejected from the 2027 Season source.")
}

multi_game <- game
multi_game$GameUID[[3]] <- "fall-game-other"
multi_path <- file.path(scratch, "multiple-games.csv")
readr::write_csv(multi_game, multi_path)
multi_error <- tryCatch({
  base_import_trackman_game(multi_path, "F26", root = scratch)
  NULL
}, error = identity)
if (is.null(multi_error) || !grepl("one game at a time", conditionMessage(multi_error), fixed = TRUE)) {
  fail("A multi-game upload was not rejected.")
}

status <- base_team_season_source_status("F26", root = scratch)
if (!isTRUE(status$exists) || status$size <= 0 || status$target$label != "2026 Fall") {
  fail("Season source status did not describe the saved cumulative file.")
}

TEAM_CONFIG$data$team_season_import_dir <- scratch
source("R/integrations/wally_pitching_workspace.R", local = FALSE)
source("R/integrations/wally_hitting_workspace.R", local = FALSE)
future_pitching <- base_read_pitching_source(first$destination)
future_hitting <- base_read_hitting_source(first$destination)
future_hitting <- future_hitting[base_team_matches(future_hitting$BatterTeam), , drop = FALSE]
if (nrow(future_pitching) != 3L || !all(future_pitching$SeasonGroup == "F26")) {
  fail("The Pitching loader did not select and label future Texas State pitching rows.")
}
if (nrow(future_hitting) != 2L || !all(future_hitting$SeasonGroup == "F26")) {
  fail("The Hitting loader did not select and label future Texas State batting rows.")
}

cat("Team TrackMan import tests passed: season and bullpen append, deduplication, schema evolution, and validation.\n")
