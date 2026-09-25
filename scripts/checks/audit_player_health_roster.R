#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg)) normalizePath(file_arg[[1]], mustWork = TRUE) else normalizePath("scripts/checks/audit_player_health_roster.R", mustWork = TRUE)
base_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
app_root <- file.path(base_root, "Sports Science", "vald_shiny_app")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

Sys.setenv(
  VALD_APP_ROOT = app_root,
  BASE_PLAYER_HEALTH_ROSTER_FILE = Sys.getenv(
    "BASE_PLAYER_HEALTH_ROSTER_FILE",
    file.path(base_root, "config", "texas_state_roster_2027.csv")
  ),
  CMJ_SPRINT_SHARE_LEGACY_DIR = Sys.getenv(
    "BASE_PLAYER_HEALTH_LEGACY_DIR",
    file.path(app_root, "data", "legacy")
  )
)

source(file.path(app_root, "R", "player_health_roster.R"), local = FALSE)

state_dir <- Sys.getenv("BASE_PLAYER_HEALTH_STATE_DIR", file.path(app_root, "data", "refresh"))
snapshot_path <- file.path(state_dir, "dashboard.rds")
if (!file.exists(snapshot_path)) stop("Player Health snapshot is unavailable at ", snapshot_path, call. = FALSE)

snapshot <- readRDS(snapshot_path)
raw_roster <- snapshot$raw_roster
if (is.null(raw_roster)) raw_roster <- snapshot$roster
roster <- reconcile_player_health_roster(raw_roster)

summary <- snapshot$session_summary
sprint <- snapshot$sprint
cutoff <- as.Date(Sys.getenv("BASE_PLAYER_HEALTH_ACTIVE_CUTOFF", "2026-08-01"))

cmj_latest <- if (!is.null(summary) && is.data.frame(summary) && nrow(summary) && all(c("profileId", "session_date") %in% names(summary))) {
  summary |>
    mutate(session_date = as.Date(.data$session_date)) |>
    filter(!is.na(.data$session_date), .data$session_date >= cutoff) |>
    group_by(.data$profileId) |>
    summarise(latest_cmj = max(.data$session_date), .groups = "drop")
} else tibble(profileId = character(), latest_cmj = as.Date(character()))

sprint_latest <- if (!is.null(sprint) && is.data.frame(sprint) && nrow(sprint) && all(c("athlete_id", "test_date") %in% names(sprint))) {
  sprint |>
    mutate(test_date = as.Date(.data$test_date)) |>
    filter(!is.na(.data$test_date), .data$test_date >= cutoff) |>
    group_by(.data$athlete_id) |>
    summarise(latest_sprint = max(.data$test_date), .groups = "drop")
} else tibble(athlete_id = character(), latest_sprint = as.Date(character()))

roster_audit <- roster |>
  left_join(cmj_latest, by = "profileId") |>
  left_join(sprint_latest, by = c("profileId" = "athlete_id")) |>
  transmute(
    roster_name = .data$athleteName,
    position = .data$position,
    pos_type = .data$pos_type,
    profileId = ifelse(.data$identityMatched, .data$profileId, ""),
    externalId = .data$externalId,
    match_method = .data$matchMethod,
    latest_cmj = .data$latest_cmj,
    latest_sprint = .data$latest_sprint,
    action_needed = case_when(
      !.data$identityMatched ~ "Map this roster name to a VALD profileId or externalId",
      is.na(.data$latest_cmj) & is.na(.data$latest_sprint) ~ "Confirm athlete has no current-season testing",
      TRUE ~ "None"
    )
  )

matched_ids <- roster$profileId[roster$identityMatched]
active_vald <- raw_roster |>
  filter(.data$profileId %in% unique(c(
    as.character(cmj_latest$profileId),
    as.character(sprint_latest$athlete_id)
  )), !(.data$profileId %in% matched_ids)) |>
  transmute(
    athleteName = .data$athleteName,
    profileId = .data$profileId,
    externalId = .data$externalId,
    action_needed = "Confirm whether this active VALD identity belongs on the current BASE roster"
  ) |>
  distinct()

dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
roster_report <- file.path(state_dir, "player-health-roster-reconciliation.csv")
unrostered_report <- file.path(state_dir, "player-health-unrostered-active.csv")
write_csv(roster_audit, roster_report, na = "")
write_csv(active_vald, unrostered_report, na = "")

cat(
  "Player Health roster audit complete:\n",
  " roster players:", nrow(roster_audit), "\n",
  " mapped identities:", sum(roster$identityMatched), "\n",
  " awaiting mapping:", sum(!roster$identityMatched), "\n",
  " active VALD identities outside roster:", nrow(active_vald), "\n",
  " roster report:", roster_report, "\n",
  " unrostered report:", unrostered_report, "\n"
)

