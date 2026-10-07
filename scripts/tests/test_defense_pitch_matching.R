#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source("team_config.R")
source("R/data/source_contract.R")
source("R/integrations/wally_defense_workspace.R")
# Load the join itself without model/data/UI initialization.
for (expr in parse("WallyApps/DefenseApp/DefenseApp.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("attach_batted_ball_data"))) eval(expr)
}
positioning <- tibble(
  PitchUID = c(" Already-Joined ", "fall-hit", NA, "", "wrong-id", NA, "duplicate"),
  GameUID = c("old", "fall", "fallback", "", "conflict", "ambiguous", "dup"),
  PitchNo = c(1, 2, 3, NA, 5, 6, 7),
  JoinMethod = c("pitch_uid", rep(NA_character_, 6)),
  Distance = c(300, rep(NA_real_, 6)), Bearing = c(12, rep(NA_real_, 6)),
  HangTime = c(4, rep(NA_real_, 6)), `CF_PositionAtReleaseX` = 280
)
contact <- tibble(
  PitchUID = c("fall-hit", "fallback-id", "", "other-id", "amb-1", "amb-2", "duplicate", "duplicate"),
  GameUID = c("fall", "fallback", "", "conflict", "ambiguous", "ambiguous", "dup", "dup"),
  PitchNo = c(2, 3, NA, 5, 6, 6, 7, 7),
  Distance = c(250, 200, 999, 999, 999, 999, NA, 320),
  Bearing = c(-20, 30, rep(0, 5), 15), HangTime = c(3, 2, rep(1, 5), 5)
)
out <- attach_batted_ball_data(positioning, contact)
stopifnot(nrow(out) == nrow(positioning), identical(out$Distance, c(300, 250, 200, NA, NA, NA, 320)),
          identical(out$HangTime, c(4, 3, 2, NA, NA, NA, 5)),
          identical(out$batted_ball_matched, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)),
          identical(out$CF_PositionAtReleaseX, positioning$CF_PositionAtReleaseX),
          !any(grepl("\\.[xy]$", names(out))))
# Matching is case/whitespace insensitive, and existing contact values win.
contact <- tibble(PitchUID = "already-joined", Distance = 999, Bearing = 999, HangTime = 999)
stopifnot(attach_batted_ball_data(positioning, contact)$Distance[[1]] == 300,
          attach_batted_ball_data(positioning, tibble())$batted_ball_matched[[1]])
# Runtime positions are refreshed without losing the already-joined metrics.
runtime <- tibble(PitchUID = "pitch-1", CF_PositionAtReleaseX = 200, Distance = 300,
                  HangTime = 4, JoinMethod = "pitch_uid")
upload <- tibble(PitchUID = c("pitch-1", "pitch-2"), CF_PositionAtReleaseX = c(280, 290))
merged <- base_merge_defense_positioning(runtime, upload)
stopifnot(nrow(merged) == 2, identical(merged$CF_PositionAtReleaseX, c(280, 290)),
          merged$Distance[[1]] == 300, merged$HangTime[[1]] == 4)
# A volume-backed season export must participate even with prejoined rows.
local({
  original_paths <- base_defense_contact_paths
  on.exit(assign("base_defense_contact_paths", original_paths, envir = .GlobalEnv))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  readr::write_csv(tibble(PitchUID = "pitch-2", Distance = 260, Bearing = 25, HangTime = 3.5), path)
  assign("base_defense_contact_paths", function() path, envir = .GlobalEnv)
  enriched <- attach_batted_ball_data(merged, base_prepare_wally_batted_rows(merged))
  stopifnot(all(enriched$batted_ball_matched), enriched$Distance[[2]] == 260, enriched$HangTime[[2]] == 3.5)
})
# The production Parquet path retrieves the relevant pitch/game rows only.
local({
  original_paths <- base_defense_contact_paths
  on.exit(assign("base_defense_contact_paths", original_paths, envir = .GlobalEnv))
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  arrow::write_parquet(tibble(PitchUID = c("pitch-2", "unrelated"),
    GameUID = c("game-2", "other"), Distance = c(260, 999), Bearing = c(25, 0), HangTime = c(3.5, 9)), path)
  assign("base_defense_contact_paths", function() path, envir = .GlobalEnv)
  input <- merged
  input$GameUID <- c("game-1", "game-2")
  contacts <- base_prepare_wally_batted_rows(input)
  stopifnot(nrow(contacts) == 1, contacts$PitchUID[[1]] == "pitch-2")
  enriched <- attach_batted_ball_data(input, contacts)
  stopifnot(all(enriched$batted_ball_matched), enriched$HangTime[[2]] == 3.5)
})
cat("Defense pitch-ID matching checks passed.\n")
