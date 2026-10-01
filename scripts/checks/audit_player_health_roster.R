#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg)) normalizePath(file_arg[[1]], mustWork = TRUE) else normalizePath("scripts/checks/audit_player_health_roster.R", mustWork = TRUE)
base_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
app_root <- file.path(base_root, "Sports Science 2", "vald_shiny_app")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

Sys.setenv(
  VALD_APP_ROOT = app_root,
  FALL_ROSTER_FILE = Sys.getenv(
    "BASE_PLAYER_HEALTH_FALL_ROSTER_FILE",
    file.path(base_root, "Sports Science 2", "2026 Fall Roster Template.xlsx")
  )
)
source(file.path(app_root, "R", "roster_trackman.R"), local = FALSE)

state_dir <- Sys.getenv("BASE_PLAYER_HEALTH_STATE_DIR", file.path(app_root, "data", "refresh"))
snapshot_path <- file.path(state_dir, "dashboard.rds")
if (!file.exists(snapshot_path)) stop("Player Health snapshot is unavailable at ", snapshot_path, call. = FALSE)

current <- read_fall_roster()
if (!is.null(current$error)) stop(current$error, call. = FALSE)
snapshot <- readRDS(snapshot_path)
vald <- snapshot$roster
summary <- snapshot$session_summary
sprint <- snapshot$sprint
cutoff <- as.Date(Sys.getenv("BASE_PLAYER_HEALTH_ACTIVE_CUTOFF", "2026-08-01"))

vald_identity <- if (!is.null(vald) && is.data.frame(vald) && nrow(vald) &&
                     all(c("profileId", "athleteName") %in% names(vald))) {
  tibble(
    profileId = as.character(vald$profileId),
    vendor_name = as.character(vald$athleteName),
    key = roster_keys(vald$athleteName, current)
  ) |>
    filter(.data$key %in% current$players$key) |>
    distinct(.data$key, .keep_all = TRUE)
} else tibble(profileId = character(), vendor_name = character(), key = character())

cmj_latest <- if (!is.null(summary) && is.data.frame(summary) && nrow(summary) &&
                  all(c("profileId", "session_date") %in% names(summary))) {
  summary |>
    transmute(profileId = as.character(.data$profileId), session_date = as.Date(.data$session_date)) |>
    filter(!is.na(.data$session_date), .data$session_date >= cutoff) |>
    group_by(.data$profileId) |>
    summarise(latest_cmj = max(.data$session_date), .groups = "drop")
} else tibble(profileId = character(), latest_cmj = as.Date(character()))

sprint_latest <- if (!is.null(sprint) && is.data.frame(sprint) && nrow(sprint) &&
                     all(c("athlete_name", "test_date") %in% names(sprint))) {
  sprint |>
    transmute(key = roster_keys(.data$athlete_name, current), test_date = as.Date(.data$test_date)) |>
    filter(.data$key %in% current$players$key, !is.na(.data$test_date), .data$test_date >= cutoff) |>
    group_by(.data$key) |>
    summarise(latest_sprint = max(.data$test_date), .groups = "drop")
} else tibble(key = character(), latest_sprint = as.Date(character()))

roster_audit <- current$players |>
  left_join(vald_identity, by = "key") |>
  left_join(cmj_latest, by = "profileId") |>
  left_join(sprint_latest, by = "key") |>
  transmute(
    roster_name = .data$name,
    position = .data$position,
    role = .data$role,
    profileId = coalesce(.data$profileId, ""),
    vald_name = coalesce(.data$vendor_name, ""),
    latest_cmj = .data$latest_cmj,
    latest_sprint = .data$latest_sprint,
    action_needed = case_when(
      !nzchar(.data$profileId) ~ "Add an explicit alias or confirm no VALD profile exists",
      is.na(.data$latest_cmj) & is.na(.data$latest_sprint) ~ "Confirm athlete has no current-season VALD testing",
      TRUE ~ "None"
    )
  )

outside_roster <- if (is.null(vald) || !is.data.frame(vald) || !nrow(vald) ||
                     !all(c("athleteName", "profileId") %in% names(vald))) {
  tibble(athleteName = character(), profileId = character(), action_needed = character())
} else {
  keys <- roster_keys(vald$athleteName, current)
  tibble(athleteName = vald$athleteName, profileId = vald$profileId, key = keys) |>
    filter(!.data$key %in% current$players$key) |>
    transmute(
      .data$athleteName,
      .data$profileId,
      action_needed = "Confirm whether this VALD identity belongs on the current fall roster"
    ) |>
    distinct()
}

dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
roster_report <- file.path(state_dir, "player-health-roster-reconciliation.csv")
unrostered_report <- file.path(state_dir, "player-health-unrostered-active.csv")
write_csv(roster_audit, roster_report, na = "")
write_csv(outside_roster, unrostered_report, na = "")

cat(
  "Player Health roster audit complete:\n",
  " roster players:", nrow(roster_audit), "\n",
  " mapped VALD identities:", sum(nzchar(roster_audit$profileId)), "\n",
  " awaiting mapping:", sum(!nzchar(roster_audit$profileId)), "\n",
  " roster report:", roster_report, "\n",
  " unrostered report:", unrostered_report, "\n"
)
