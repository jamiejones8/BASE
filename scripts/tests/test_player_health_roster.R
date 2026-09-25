#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

source("Sports Science/vald_shiny_app/R/player_health_roster.R", local = FALSE)
`%||%` <- function(a, b) if (!is.null(a)) a else b
source("Sports Science/vald_shiny_app/R/clean_smartspeed.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
tmp <- tempfile("player-health-roster-")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

team_path <- file.path(tmp, "roster.csv")
crosswalk_path <- file.path(tmp, "crosswalk.csv")

write_csv(tibble(
  Name = c("Exact Pitcher", "Mapped Catcher", "Untested Outfielder"),
  Pos = c("RHP", "C", "OF"),
  Number = c(1, 2, 3),
  Bats = c("R", "R", "L"),
  Throws = c("R", "R", "L"),
  pos_type = c("Pitcher", "Catcher", "Outfielder")
), team_path)

write_csv(tibble(
  roster_name = "Mapped Catcher",
  profileId = "p2",
  externalId = "TXST-Baseball-002"
), crosswalk_path)

vald <- tibble(
  profileId = c("p1", "p2", "historical"),
  athleteName = c("Exact Pitcher", "Different VALD Name", "Historical Athlete"),
  externalId = c("TXST-Baseball-001", "TXST-Baseball-002", "TXST-Baseball-099"),
  groupNames_csv = "",
  primaryGroup = "Other",
  notes = ""
)

roster <- reconcile_player_health_roster(vald, team_path, crosswalk_path)
identity <- player_health_identity_summary(roster, vald)

if (nrow(roster) != 3L) fail("Authoritative BASE roster rows were not retained.")
if (identity$matched != 2L || identity$unmatched != 1L) fail("Identity reconciliation counts are incorrect.")
if (!identical(sort(roster$roleGroup), sort(c("Pitchers", "Hitters", "Hitters")))) fail("Roster roles were not derived from pos_type.")
if (!grepl("^base-roster::", roster$profileId[roster$athleteName == "Untested Outfielder"])) fail("Untested roster athlete did not receive a stable placeholder identity.")
if (roster$matchMethod[roster$athleteName == "Mapped Catcher"] != "crosswalk") fail("Explicit crosswalk did not win over name matching.")

reconciled_again <- reconcile_player_health_roster(roster, team_path, crosswalk_path)
identity_again <- player_health_identity_summary(reconciled_again, roster)
if (identity_again$matched != 2L || identity_again$unmatched != 1L) fail("Reloading a reconciled snapshot promoted a placeholder to a VALD match.")

write_csv(tibble(
  roster_name = "Exact Pitcher",
  profileId = "not-a-real-profile",
  externalId = ""
), crosswalk_path)
invalid_roster <- reconcile_player_health_roster(vald, team_path, crosswalk_path)
if (invalid_roster$identityMatched[invalid_roster$athleteName == "Exact Pitcher"]) fail("Invalid crosswalk profile was treated as matched.")
if (invalid_roster$matchMethod[invalid_roster$athleteName == "Exact Pitcher"] != "invalid_crosswalk") fail("Invalid crosswalk was not surfaced for audit.")

# Restore the valid fixture for sprint reconciliation below.
write_csv(tibble(
  roster_name = "Mapped Catcher",
  profileId = "p2",
  externalId = "TXST-Baseball-002"
), crosswalk_path)

sprint <- tibble(
  athlete_id = c("p1", "p2", "historical"),
  athlete_name = c("Old 1", "Old 2", "Historical Athlete"),
  role = "Other",
  test_date = as.Date(c("2026-09-20", "2026-09-21", "2026-09-22")),
  best_10yd = c(1.6, 1.7, 1.8),
  best_30yd = NA_real_,
  best_flying_10yd = c(NA, 1.1, NA),
  max_velocity = c(8.0, 7.8, 7.5)
)

reconciled_sprint <- reconcile_player_health_sprint(sprint, roster)
if (!identical(sort(reconciled_sprint$athlete_id), c("p1", "p2"))) fail("Sprint data was not scoped to mapped current-roster athletes.")
if (!identical(sort(unique(reconciled_sprint$role)), c("Hitters", "Pitchers"))) fail("Sprint roles were not reconciled from the roster.")

choices <- player_health_sprint_metric_choices(reconciled_sprint, 28)
if (!all(c("best_10yd", "best_flying_10yd", "max_velocity") %in% unname(choices))) fail("Available sprint metrics were omitted.")
if ("best_30yd" %in% unname(choices)) fail("Unavailable sprint metric should not be offered.")

fly_raw <- list(
  profiles = tibble(profileId = "p2", givenName = "Mapped", familyName = "Catcher"),
  tests = tibble(
    id = "fly-1", profileId = "p2", testDateUtc = "2026-09-21T12:00:00Z",
    modifiedDateUtc = "2026-09-21T12:05:00Z", testTypeName = "OneWay",
    testName = "Build 15: Fly 10", distance = 9.144, isValid = TRUE,
    totalTimeSeconds = 1.08, bestSplitSeconds = 1.08,
    peakVelocityMetersPerSecond = 8.4
  )
)
fly <- standardize_smartspeed_export(fly_raw, roster = roster)
if (nrow(fly) != 1L || is.na(fly$best_flying_10yd[[1]]) || abs(fly$best_flying_10yd[[1]] - 1.08) > 1e-8) {
  fail("Flying 10-yard SmartSpeed tests were not routed into best_flying_10yd.")
}

cat("Player Health roster reconciliation passed.\n")
