#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

source("team_config.R", local = FALSE)
source("leaderboards/helpers/calculate_hitter_stats.R", local = FALSE)
source("leaderboards/helpers/metric_helpers.R", local = FALSE)
source("leaderboards/helpers/period_sources.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fall_spec <- leaderboards_period_spec("2026-fall")
spring_spec <- leaderboards_period_spec("2027-season")
if (isTRUE(fall_spec$shared_source) ||
    !grepl("2026 Fall - cleaned[.]csv$", fall_spec$path) ||
    isTRUE(spring_spec$shared_source) ||
    !grepl("2027 Season - cleaned[.]csv$", spring_spec$path)) {
  fail("Leaderboard season controls are not mapped to the managed CSV volume.")
}

if (!identical(
  is_barrel(c(95, 94.9, 100, 100), c(5, 20, 35, 35.1)),
  c(TRUE, FALSE, TRUE, FALSE)
)) {
  fail("Leaderboard barrel boundaries are not consistently 95+ mph and 5-35 degrees.")
}

fixture <- tibble::tibble(
  Batter = rep("Integrity, Test", 3),
  PitchCall = c("InPlayOut", "InPlay", "FoulBallNotFieldable"),
  KorBB = c("", "", ""),
  PlayResult = c("HomeRun", "Single", ""),
  ExitSpeed = c(101, NA_real_, 130),
  Angle = c(20, NA_real_, 25),
  TaggedHitType = c("FlyBall", "GroundBall", ""),
  Direction = "",
  BatterSide = "Right",
  PlateLocHeight = 2.5,
  PlateLocSide = 0,
  GameID = "integrity-game",
  Inning = c(1, 2, 3),
  `Top/Bottom` = "Bottom",
  PAofInning = 1,
  PitchofPA = 1
)

stats <- calculate_hitter_stats(fixture)
if (nrow(stats) != 1L) fail("Leaderboard hitting fixture did not produce one hitter.")
if (!isTRUE(all.equal(stats$MaxEV, 101))) {
  fail("Leaderboard MaxEV includes a foul/non-BBE reading or excludes InPlayOut.")
}
if (!isTRUE(all.equal(stats$P90EV, 101))) {
  fail("Leaderboard P90EV is not limited to measured batted-ball events.")
}
if (!isTRUE(all.equal(stats$`Barrel%`, 100))) {
  fail("Leaderboard Barrel% includes unmeasured balls in its denominator.")
}
if (!isTRUE(all.equal(stats$`EV>95%`, 100))) {
  fail("Leaderboard hard-hit rate includes unmeasured balls in its denominator.")
}

cat("Leaderboard hitting integrity tests passed.\n")
