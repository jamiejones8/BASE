# scripts/01_config.R
# ------------------------------------------------------------
# Project configuration + shared helpers
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

# ----------------------------
# VALD credentials fallback/mapping (temporary for basic/free hosting)
# ----------------------------
# Preferred setup is secure host environment variables.
# The pipeline expects:
#   VALD_CLIENT_ID, VALD_CLIENT_SECRET, VALD_TEAM_ID
#
# Support local naming from VALD support messages:
#   VALD_USERNAME -> VALD_CLIENT_ID
#   VALD_PASSWORD -> VALD_CLIENT_SECRET
#   VALD_DUENDE_ID -> VALD_TEAM_ID only if no real API team/tenant ID is set
#   USERNAME/PASSWORD/DUENDE_ID are accepted as additional local aliases.
#
# Prefer explicit local aliases from .Renviron over stale inherited VALD_CLIENT_* values.
if (nzchar(Sys.getenv("VALD_USERNAME"))) {
  Sys.setenv(VALD_CLIENT_ID = Sys.getenv("VALD_USERNAME"))
}
if (nzchar(Sys.getenv("VALD_PASSWORD"))) {
  Sys.setenv(VALD_CLIENT_SECRET = Sys.getenv("VALD_PASSWORD"))
}
if (!nzchar(Sys.getenv("VALD_TEAM_ID")) && nzchar(Sys.getenv("VALD_DUENDE_ID"))) {
  Sys.setenv(VALD_TEAM_ID = Sys.getenv("VALD_DUENDE_ID"))
}
if (!nzchar(Sys.getenv("VALD_CLIENT_ID")) && nzchar(Sys.getenv("USERNAME"))) {
  Sys.setenv(VALD_CLIENT_ID = Sys.getenv("USERNAME"))
}
if (!nzchar(Sys.getenv("VALD_CLIENT_SECRET")) && nzchar(Sys.getenv("PASSWORD"))) {
  Sys.setenv(VALD_CLIENT_SECRET = Sys.getenv("PASSWORD"))
}
if (!nzchar(Sys.getenv("VALD_TEAM_ID")) && nzchar(Sys.getenv("DUENDE_ID"))) {
  Sys.setenv(VALD_TEAM_ID = Sys.getenv("DUENDE_ID"))
}
if (!nzchar(Sys.getenv("VALD_TEAM_ID")) && nzchar(Sys.getenv("VALD_TENANT_ID"))) {
  Sys.setenv(VALD_TEAM_ID = Sys.getenv("VALD_TENANT_ID"))
}

# Backward-compatible aliases used by existing Shiny code, valdr wrappers, and URL builders.
if (!nzchar(Sys.getenv("VALD_USERNAME")) && nzchar(Sys.getenv("VALD_CLIENT_ID"))) {
  Sys.setenv(VALD_USERNAME = Sys.getenv("VALD_CLIENT_ID"))
}
if (!nzchar(Sys.getenv("VALD_PASSWORD")) && nzchar(Sys.getenv("VALD_CLIENT_SECRET"))) {
  Sys.setenv(VALD_PASSWORD = Sys.getenv("VALD_CLIENT_SECRET"))
}
if (!nzchar(Sys.getenv("VALD_DUENDE_ID")) && nzchar(Sys.getenv("VALD_TEAM_ID"))) {
  Sys.setenv(VALD_DUENDE_ID = Sys.getenv("VALD_TEAM_ID"))
}
if (!nzchar(Sys.getenv("VALD_TENANT_ID")) && nzchar(Sys.getenv("VALD_TEAM_ID"))) {
  Sys.setenv(VALD_TENANT_ID = Sys.getenv("VALD_TEAM_ID"))
}

# Helper for scripts that require live VALD API calls.
assert_vald_credentials <- function() {
  missing_cred_vars <- c("VALD_CLIENT_ID", "VALD_CLIENT_SECRET", "VALD_TEAM_ID")
  missing_cred_vars <- missing_cred_vars[
    !vapply(missing_cred_vars, function(v) nzchar(Sys.getenv(v)), logical(1))
  ]
  if (length(missing_cred_vars) > 0) {
    stop(
      "Missing required VALD credentials: ",
      paste(missing_cred_vars, collapse = ", "),
      "\nSet them as environment variables (or map VALD_USERNAME/VALD_PASSWORD/VALD_DUENDE_ID).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

vald_token_url <- function() {
  Sys.getenv("VALD_TOKEN_URL", "https://auth.prd.vald.com/oauth/token")
}

vald_auth_body <- function() {
  assert_vald_credentials()
  list(
    grant_type = "client_credentials",
    audience = "vald-api-external",
    client_id = Sys.getenv("VALD_CLIENT_ID"),
    client_secret = Sys.getenv("VALD_CLIENT_SECRET")
  )
}

vald_get_token <- function() {
  token_response <- httr::POST(
    vald_token_url(),
    httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
    body = vald_auth_body(),
    encode = "form", httr::timeout(60)
  )
  if (httr::status_code(token_response) != 200) {
    stop(
      "VALD token request failed (status ", httr::status_code(token_response), "): ",
      httr::content(token_response, "text", encoding = "UTF-8"),
      call. = FALSE
    )
  }
  httr::content(token_response)$access_token
}

# ----------------------------
# ArmCare API configuration (scaffold)
# ----------------------------
# Set these in your environment once ArmCare provides API access.
# Required:
#   ARMCARE_BASE_URL
#   ARMCARE_USERNAME
#   ARMCARE_PASSWORD
# Optional:
#   ARMCARE_DUENDE_ID
#   ARMCARE_TOKEN_URL   (defaults to "<BASE_URL>/oauth/token")
assert_armcare_credentials <- function() {
  missing_cred_vars <- c("ARMCARE_BASE_URL", "ARMCARE_USERNAME", "ARMCARE_PASSWORD")
  missing_cred_vars <- missing_cred_vars[
    !vapply(missing_cred_vars, function(v) nzchar(Sys.getenv(v)), logical(1))
  ]
  if (length(missing_cred_vars) > 0) {
    stop(
      "Missing required ArmCare settings: ",
      paste(missing_cred_vars, collapse = ", "),
      "\nSet ARMCARE_BASE_URL, ARMCARE_USERNAME, ARMCARE_PASSWORD (and optional ARMCARE_DUENDE_ID).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ----------------------------
# Paths (robust project-root resolution)
# ----------------------------
# Prefer: here::here() if available (best for RStudio projects / deployments)
# Fallback: getwd() if here is not installed
get_root_dir <- function() {
  normalizePath(Sys.getenv("VALD_APP_ROOT", getwd()), winslash = "/", mustWork = TRUE)
}

ROOT_DIR    <- get_root_dir()
DATA_DIR    <- Sys.getenv("CMJ_SPRINT_SHARE_LEGACY_DIR", file.path(ROOT_DIR, "data", "legacy"))
SCRIPTS_DIR <- file.path(ROOT_DIR, "scripts")
MODULES_DIR <- file.path(ROOT_DIR, "modules")

if (!dir.exists(DATA_DIR)) {
  # The active app prefers data/gold/ and only falls back to this legacy
  # directory for filenames gold doesn't have (see dashboard_data_path()) --
  # a hosted deploy bundle that omits the legacy Data/ folder (e.g. a
  # case-insensitive macOS build machine collapsing "Data" into "data" when
  # bundling for a case-sensitive Linux host) shouldn't crash the whole app
  # before that fallback ever gets a chance to run.
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------
# Small utilities
# ----------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

assert_file_exists <- function(path, label = NULL) {
  if (!file.exists(path)) {
    stop((label %||% "Missing file"), ": ", path, call. = FALSE)
  }
  invisible(TRUE)
}

assert_cols <- function(df, cols, df_name = "data frame") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required columns: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Ensure common columns exist even if upstream file changes
ensure_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (!("athleteName" %in% names(df))) df$athleteName <- NA_character_
  if (!("externalId"  %in% names(df))) df$externalId  <- ""
  if (!("notes"       %in% names(df))) df$notes       <- ""
  df
}

# After joins, name columns can become athleteName.x / athleteName.y
standardize_joined_names <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  
  if (!("athleteName" %in% names(df))) {
    ax <- if ("athleteName.x" %in% names(df)) as.character(df[["athleteName.x"]]) else NA_character_
    ay <- if ("athleteName.y" %in% names(df)) as.character(df[["athleteName.y"]]) else NA_character_
    df$athleteName <- dplyr::coalesce(ay, ax)
  }
  if (!("externalId" %in% names(df))) {
    ex <- if ("externalId.x" %in% names(df)) as.character(df[["externalId.x"]]) else ""
    ey <- if ("externalId.y" %in% names(df)) as.character(df[["externalId.y"]]) else ""
    df$externalId <- dplyr::coalesce(ey, ex)
  }
  df
}

metric_col_name <- function(df) if ("metricName" %in% names(df)) "metricName" else "metricKey"

# ----------------------------
# Roster loader (SOURCE OF TRUTH)
# ----------------------------
# Standardize filename across project:
#   roster_baseball.rds
read_roster_baseball <- function(data_dir = DATA_DIR,
                                 filename = "roster_baseball.rds") {
  path <- file.path(data_dir, filename)
  assert_file_exists(path, "Roster file not found")
  
  roster <- readRDS(path)
  
  needed <- c("profileId", "athleteName", "externalId", "primaryGroup", "groupNames_csv")
  assert_cols(roster, needed, "roster_baseball")
  
  roster %>%
    transmute(
      profileId      = as.character(profileId),
      athleteName    = as.character(athleteName),
      externalId     = as.character(externalId),
      primaryGroup   = as.character(primaryGroup),
      groupNames_csv = as.character(groupNames_csv)
    ) %>%
    distinct() %>%
    arrange(primaryGroup, athleteName)
}

build_roster_choices <- function(roster_tbl) {
  assert_cols(roster_tbl, c("profileId", "athleteName", "primaryGroup"), "roster_tbl")
  
  athlete_choices <- setNames(
    roster_tbl$profileId,
    paste0(roster_tbl$athleteName, " (", roster_tbl$primaryGroup, ")")
  )
  
  group_choices <- sort(unique(roster_tbl$primaryGroup))
  
  list(
    athlete_choices = athlete_choices,
    group_choices   = group_choices
  )
}

# ----------------------------
# TXST Colors (shared)
# ----------------------------
TXST <- list(
  maroon = "#501214",
  gold   = "#F6BE00",
  green  = "#16a34a",
  yellow = "#f59e0b",
  red    = "#ef4444",
  dark   = "#0b0f19",
  line   = "#1f2937",
  white  = "rgba(255,255,255,0.92)"
)
