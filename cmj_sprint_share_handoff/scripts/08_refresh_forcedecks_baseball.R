# scripts/08_refresh_forcedecks_baseball.R
# ------------------------------------------------------------
# Build ForceDecks BASEBALL session summary + session-to-session
# comparison tables (precomputed) for Shiny + includes Groups.
#
# Inputs (expected in DATA_DIR):
#   - force_metrics_long_all_history_baseball.rds
#   - force_tests_baseball_all_history_named.rds
#   - OPTIONAL: athletes_export.csv  (VALD athlete export with Group 1/2/3...)
#   - OPTIONAL: athletes-*.csv       (VALD athlete export; script will pick latest)
#
# Outputs (written to DATA_DIR):
#   - groups.rds
#   - profiles.rds        <-- profiles with raw groups + canonical primaryGroup
#   - roster_baseball.rds <-- clean roster table for Shiny (baseball only)
#   - force_sessions_summary_baseball.rds
#   - force_sessions_compare_baseball.rds
#   - force_metrics_long_all_history_baseball_enriched.rds
#
# Core decisions:
# - Group membership is not exposed via API for your tenant (404 routes).
# - CSV export is the authoritative source of group membership.
#
# Improvement vs prior version:
# - Keep rawGroupNames_csv + rawPrimaryGroup from CSV
# - Canonicalize primaryGroup to: Pitchers, Catchers, Infielders, Outfielders, Other
# - Add manual overrides for "Hitters" guys that should map to IF/OF/C
# ------------------------------------------------------------

source("scripts/01_config.R")
assert_vald_credentials()

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(readr)
})

# ----------------------------
# Helpers, config, and a fresh groups/profiles pull -- this now runs BEFORE
# the full VALD pull below (05/06/07). 06/07 classify "is this athlete
# baseball" by reading Data/profiles.rds from disk; previously this
# profiles pull+save ran AFTER 06/07 sourced, so any athlete whose VALD
# externalId changed since the last run (e.g. a newly-assigned roster ID)
# was classified using the prior run's stale profiles.rds and silently
# excluded from that run's baseball tests/metrics -- requiring a second
# full pipeline run to pick them up.
# ----------------------------
# ----------------------------
# Helpers
# ----------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

pick_first_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

get_token_vald <- function() {
  vald_get_token()
}

get_json <- function(url, token) {
  resp <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  if (status_code(resp) != 200) {
    stop(
      "GET failed (status ", status_code(resp), ") url=", url, " body=",
      content(resp, "text", encoding = "UTF-8")
    )
  }
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

try_get_json_first <- function(urls, token) {
  for (u in urls) {
    out <- tryCatch(get_json(u, token), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  NULL
}

probe_group_membership <- function(group_id, tenant_id, token, TENANTS_API_BASE, PROFILES_API_BASE) {
  tid <- URLencode(tenant_id, reserved = TRUE)
  gid <- URLencode(group_id, reserved = TRUE)
  
  candidates <- c(
    paste0(TENANTS_API_BASE,  "/groups/", gid, "/members?tenantId=", tid),
    paste0(TENANTS_API_BASE,  "/groups/", gid, "/members?teamId=",   tid),
    paste0(TENANTS_API_BASE,  "/groups/", gid, "/profiles?tenantId=", tid),
    paste0(TENANTS_API_BASE,  "/groups/", gid, "/profiles?teamId=",   tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/members?tenantId=", tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/members?teamId=",   tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/profiles?tenantId=", tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/profiles?teamId=",   tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/athletes?tenantId=", tid),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/athletes?teamId=",   tid),
    paste0(TENANTS_API_BASE,  "/groups/", gid, "/members?tenantId=", tid, "&page=1&pageSize=200"),
    paste0(PROFILES_API_BASE, "/groups/", gid, "/members?tenantId=", tid, "&page=1&pageSize=200")
  )
  
  cat("\n--- Probing groupId:", group_id, "---\n")
  for (u in candidates) {
    resp <- tryCatch(
      GET(u, add_headers(Authorization = paste("Bearer", token))),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      cat("ERR  :", u, "\n"); next
    }
    sc <- status_code(resp)
    txt <- content(resp, "text", encoding = "UTF-8")
    preview <- substr(gsub("\\s+", " ", txt), 1, 220)
    cat(sprintf("[%s] %s\n  %s\n", sc, u, preview))
  }
  cat("--- end probe ---\n")
}

find_latest_athletes_csv <- function(data_dir) {
  fixed <- file.path(data_dir, "athletes_export.csv")
  if (file.exists(fixed)) return(fixed)
  
  files <- list.files(data_dir, pattern = "^athletes-.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  
  info <- file.info(files)
  files[which.max(info$mtime)]
}

build_groups_from_csv <- function(csv_path) {
  if (is.na(csv_path) || !file.exists(csv_path)) {
    return(tibble(
      profileId = character(0),
      groupNames = list(),
      groupNames_csv = character(0),
      primaryGroup = character(0)
    ))
  }
  
  cat("\n[CSV] Reading athlete export for groups:\n ", csv_path, "\n")
  x <- suppressWarnings(readr::read_csv(csv_path, show_col_types = FALSE))
  
  id_col <- pick_first_col(x, c("vald-id", "vald_id", "profileId", "profile_id", "id"))
  if (is.null(id_col)) {
    cat("[CSV] Could not find vald-id/profileId column. Columns were:\n")
    print(names(x))
    return(tibble(profileId = character(0), groupNames = list(), groupNames_csv = character(0), primaryGroup = character(0)))
  }
  
  # Typical VALD export: "Group 1", "Group 2", ...
  group_cols <- names(x)[str_detect(names(x), "^Group\\s*[0-9]+$")]
  if (length(group_cols) == 0) {
    cat("[CSV] No 'Group 1/2/3...' columns found. Columns were:\n")
    print(names(x))
    return(tibble(profileId = character(0), groupNames = list(), groupNames_csv = character(0), primaryGroup = character(0)))
  }
  
  out <- x %>%
    transmute(profileId = as.character(.data[[id_col]]), across(all_of(group_cols), as.character)) %>%
    pivot_longer(cols = all_of(group_cols), values_to = "groupName", names_to = "groupSlot") %>%
    mutate(groupName = str_trim(groupName)) %>%
    filter(!is.na(profileId), nzchar(profileId), !is.na(groupName), nzchar(groupName)) %>%
    group_by(profileId) %>%
    summarise(
      groupNames = list(unique(groupName)),
      groupNames_csv = paste(unique(groupName), collapse = "; "),
      primaryGroup = dplyr::first(unique(groupName)),
      .groups = "drop"
    ) %>%
    mutate(
      groupNames_csv = ifelse(is.na(groupNames_csv), "", groupNames_csv),
      primaryGroup   = ifelse(is.na(primaryGroup), "", primaryGroup)
    )
  
  out
}

# Canonicalize to the dashboard buckets; everything else -> "Other"
# VECTORIZED and includes manual overrides by externalId for the 6 "Hitters" guys.
canonical_primary_group <- function(groupNames_csv, fallback_primary, externalId) {
  x   <- groupNames_csv %||% ""
  y   <- fallback_primary %||% ""
  eid <- externalId %||% ""
  combined <- paste(x, y, sep = "; ")
  
  # Overrides (vectorized)
  override <- dplyr::case_when(
    eid == "TXST-Baseball-054" ~ "Infielders",   # Blake Beheler
    eid == "TXST-Baseball-058" ~ "Catchers",     # Clayton Namken
    eid == "TXST-Baseball-065" ~ "Outfielders",  # Jackson Cotton
    eid == "TXST-Baseball-059" ~ "Infielders",   # Manny Salas
    eid == "TXST-Baseball-061" ~ "Infielders",   # Ty Powell
    eid == "TXST-Baseball-066" ~ "Infielders",   # Victor Coronado
    TRUE ~ NA_character_
  )
  
  canon <- dplyr::case_when(
    str_detect(combined, regex("\\bpitch",    ignore_case = TRUE)) ~ "Pitchers",
    str_detect(combined, regex("\\bcatch",    ignore_case = TRUE)) ~ "Catchers",
    str_detect(combined, regex("\\binfield",  ignore_case = TRUE)) ~ "Infielders",
    str_detect(combined, regex("\\boutfield", ignore_case = TRUE)) ~ "Outfielders",
    TRUE ~ "Other"
  )
  
  out <- dplyr::if_else(!is.na(override), override, canon)
  out <- dplyr::if_else(is.na(out) | out == "", "Other", out)
  out
}

# ----------------------------
# Config
# ----------------------------
BASEBALL_ID_REGEX <- "^TXST-Baseball-[0-9]{3}$"
COV_THRESHOLD     <- 0.10
TRIAL_SIZE_ALPHA  <- 0.05

PROFILES_API_BASE <- "https://prd-use-api-externalprofile.valdperformance.com"
TENANTS_API_BASE  <- "https://prd-use-api-externaltenants.valdperformance.com"

IN_METRICS_RDS <- file.path(DATA_DIR, "force_metrics_long_all_history_baseball.rds")
TESTS_RDS      <- file.path(DATA_DIR, "force_tests_baseball_all_history_named.rds")

OUT_GROUPS_RDS           <- file.path(DATA_DIR, "groups.rds")
OUT_PROFILES_RDS         <- file.path(DATA_DIR, "profiles.rds")
OUT_ROSTER_RDS           <- file.path(DATA_DIR, "roster_baseball.rds")
OUT_SESS_SUMMARY_RDS     <- file.path(DATA_DIR, "force_sessions_summary_baseball.rds")
OUT_SESS_COMPARE_RDS     <- file.path(DATA_DIR, "force_sessions_compare_baseball.rds")
OUT_METRICS_ENRICHED_RDS <- file.path(DATA_DIR, "force_metrics_long_all_history_baseball_enriched.rds")

stopifnot(file.exists(IN_METRICS_RDS))
stopifnot(file.exists(TESTS_RDS))

tenant_id <- Sys.getenv("VALD_TEAM_ID", Sys.getenv("VALD_TENANT_ID", Sys.getenv("VALD_DUENDE_ID")))
if (!nzchar(tenant_id)) stop("VALD_DUENDE_ID env var is empty. Need Duende/team ID to pull profiles/groups.")

# ----------------------------
# Helper: robust A-crit lookup
# ----------------------------
get_a_crit <- function(n_trials, alpha = 0.05) {
  if (is.na(n_trials) || n_trials < 3) return(NA_real_)
  
  tab <- tibble::tribble(
    ~n, ~a_0.10, ~a_0.05, ~a_0.01,
    3,  1.3733,  1.6533,  2.2133,
    4,  1.2643,  1.5058,  1.9867,
    5,  1.1597,  1.3662,  1.7788,
    6,  1.0629,  1.2408,  1.6044,
    7,  0.9751,  1.1306,  1.4623,
    8,  0.8960,  1.0351,  1.3473,
    9,  0.8270,  0.9536,  1.2542,
    10, 0.7673,  0.8857,  1.1776
  )
  
  n_use <- min(as.integer(n_trials), 10L)
  row <- tab %>% dplyr::filter(n == n_use)
  if (nrow(row) == 0) return(NA_real_)
  
  alpha_use <- alpha
  if (!(alpha_use %in% c(0.10, 0.05, 0.01))) alpha_use <- 0.05
  
  if (alpha_use == 0.10) return(row$a_0.10[[1]])
  if (alpha_use == 0.01) return(row$a_0.01[[1]])
  row$a_0.05[[1]]
}

# ----------------------------
# Pull Groups + Profiles
# ----------------------------
# Needed here (rather than only in the Toggles section below) because this
# whole block now runs before Toggles is defined -- see reorder note above.
RUN_MEMBERSHIP_PROBE <- FALSE  # keep FALSE unless debugging VALD routes again
cat("Pulling tenant groups + profiles ...\n")
token <- get_token_vald()

# Groups (still useful for reference even if membership routes fail)
groups_urls <- c(
  paste0(TENANTS_API_BASE, "/groups?tenantId=", URLencode(tenant_id, reserved = TRUE)),
  paste0(TENANTS_API_BASE, "/groups?teamId=",   URLencode(tenant_id, reserved = TRUE))
)

groups_raw <- try_get_json_first(groups_urls, token)
if (is.null(groups_raw)) stop("Unable to fetch groups using either tenantId or teamId.")
if (is.list(groups_raw) && "groups" %in% names(groups_raw)) groups_raw <- groups_raw$groups
groups_df0 <- as_tibble(groups_raw)

cat("\n[DIAG] groups_df0 columns:\n"); print(names(groups_df0))
cat("\n[DIAG] first group record (glimpse):\n"); print(dplyr::glimpse(groups_df0[1, ], width = 120))

gid_col   <- pick_first_col(groups_df0, c("id", "groupId", "groupID"))
gname_col <- pick_first_col(groups_df0, c("name", "groupName"))
if (is.null(gid_col)) stop("Groups response missing id column. Columns: ", paste(names(groups_df0), collapse = ", "))
if (is.null(gname_col)) { groups_df0$.__tmp_name <- ""; gname_col <- ".__tmp_name" }

groups_df <- groups_df0 %>%
  transmute(
    groupId   = as.character(.data[[gid_col]]),
    groupName = as.character(.data[[gname_col]])
  ) %>%
  mutate(
    groupId   = ifelse(is.na(groupId), "", groupId),
    groupName = ifelse(is.na(groupName), "", groupName)
  ) %>%
  filter(nzchar(groupId)) %>%
  distinct()

saveRDS(groups_df, OUT_GROUPS_RDS)
cat("Saved groups:\n ", OUT_GROUPS_RDS, "\n")
cat("Groups pulled:", nrow(groups_df), "\n")

if (RUN_MEMBERSHIP_PROBE && nrow(groups_df) > 0) {
  probe_group_membership(
    group_id = groups_df$groupId[[1]],
    tenant_id = tenant_id,
    token = token,
    TENANTS_API_BASE = TENANTS_API_BASE,
    PROFILES_API_BASE = PROFILES_API_BASE
  )
}

# Profiles
profiles_urls <- c(
  paste0(PROFILES_API_BASE, "/profiles?tenantId=", URLencode(tenant_id, reserved = TRUE)),
  paste0(PROFILES_API_BASE, "/profiles?teamId=",   URLencode(tenant_id, reserved = TRUE))
)

profiles_raw <- try_get_json_first(profiles_urls, token)
if (is.null(profiles_raw)) stop("Unable to fetch profiles using either tenantId or teamId.")
if (is.list(profiles_raw) && "profiles" %in% names(profiles_raw)) profiles_raw <- profiles_raw$profiles
profiles_df0 <- as_tibble(profiles_raw)

cat("\nRaw profiles rows pulled:", nrow(profiles_df0), "\n")
if (nrow(profiles_df0) == 0) stop("Profiles endpoint returned 0 rows.")

cat("\n[DIAG] profiles_df0 columns:\n"); print(names(profiles_df0))

pid_col <- pick_first_col(profiles_df0, c("profileId", "id"))
gn_col  <- pick_first_col(profiles_df0, c("givenName", "firstName"))
fn_col  <- pick_first_col(profiles_df0, c("familyName", "lastName", "surname"))
ext_col <- pick_first_col(profiles_df0, c("externalId", "externalID"))

if (is.null(pid_col)) stop("Profiles response missing profile id column. Columns: ", paste(names(profiles_df0), collapse = ", "))
if (is.null(gn_col))  { profiles_df0$.__tmp_given <- "";  gn_col  <- ".__tmp_given" }
if (is.null(fn_col))  { profiles_df0$.__tmp_family <- ""; fn_col  <- ".__tmp_family" }
if (is.null(ext_col)) { profiles_df0$.__tmp_ext <- "";    ext_col <- ".__tmp_ext" }

profiles_base <- profiles_df0 %>%
  mutate(
    profileId   = as.character(.data[[pid_col]]),
    givenName   = as.character(.data[[gn_col]]),
    familyName  = as.character(.data[[fn_col]]),
    externalId  = as.character(.data[[ext_col]]),
    athleteName = trimws(paste(givenName, familyName))
  ) %>%
  transmute(profileId, externalId, athleteName) %>%
  distinct()

# ----------------------------
# Groups: CSV fallback (source of truth)
# ----------------------------
profiles_groups <- profiles_base %>%
  mutate(
    groupNames        = purrr::map(profileId, ~ character(0)),
    rawGroupNames_csv = "",
    rawPrimaryGroup   = "",
    groupNames_csv    = "",
    primaryGroup      = "Other"
  )

csv_path   <- find_latest_athletes_csv(DATA_DIR)
csv_groups <- build_groups_from_csv(csv_path)

if (nrow(csv_groups) > 0) {
  profiles_groups <- profiles_groups %>%
    left_join(
      csv_groups %>% select(profileId, groupNames, groupNames_csv, primaryGroup),
      by = "profileId",
      suffix = c(".base", ".csv")
    ) %>%
    mutate(
      rawGroupNames_csv = dplyr::coalesce(groupNames_csv.csv, ""),
      rawPrimaryGroup   = dplyr::coalesce(primaryGroup.csv, ""),
      groupNames_csv    = rawGroupNames_csv,
      primaryGroup      = canonical_primary_group(rawGroupNames_csv, rawPrimaryGroup, externalId),
      
      # list-col safety
      groupNames = purrr::map(groupNames.csv, function(g) {
        if (is.null(g)) return(character(0))
        if (length(g) == 1 && is.na(g)) return(character(0))
        as.character(g)
      })
    ) %>%
    select(profileId, externalId, athleteName, groupNames,
           rawGroupNames_csv, rawPrimaryGroup, groupNames_csv, primaryGroup)
  
  cat("[CSV] Joined CSV group info onto profiles.\n")
} else {
  cat("[CSV] No CSV group info found/usable. Profiles will have primaryGroup='Other'.\n")
}

cat("Profiles table built:", nrow(profiles_groups), "\n")
cat("Profiles by primaryGroup:\n")
print(profiles_groups %>% count(primaryGroup, sort = TRUE))

if (nrow(profiles_groups) == 0) stop("profiles_groups ended up empty; stopping to avoid overwriting outputs.")
saveRDS(profiles_groups, OUT_PROFILES_RDS)
cat("Saved profiles (with groups):\n ", OUT_PROFILES_RDS, "\n")

# ----------------------------
# Toggles
# ----------------------------
RUN_MEMBERSHIP_PROBE <- FALSE  # keep FALSE unless debugging VALD routes again
RUN_FULL_PULL_FIRST <- tolower(Sys.getenv("VALD_RUN_FULL_PULL_FIRST", "true")) %in% c("true","1","yes","y")
RUN_VALDR_PROFILE_PULL <- tolower(Sys.getenv("VALD_RUN_VALDR_PROFILE_PULL", "false")) %in% c("true","1","yes","y")
RUN_ARMCARE_PULL <- tolower(Sys.getenv("ARMCARE_RUN_PULL", "false")) %in% c("true","1","yes","y")
RUN_ARMCARE_CSV_IMPORT <- tolower(Sys.getenv("ARMCARE_RUN_CSV_IMPORT", "false")) %in% c("true","1","yes","y")
RUN_ARMCARE_BUILD <- tolower(Sys.getenv("ARMCARE_BUILD_OUTPUTS", "true")) %in% c("true","1","yes","y")

if (RUN_FULL_PULL_FIRST) {
  cat("\n[Refresh] Running full incremental VALD pull first (profiles -> tests -> metrics)...\n")
  if (RUN_VALDR_PROFILE_PULL) {
    # Profile pull via valdr can fail during auth migrations; don't block full refresh.
    # We also pull profiles later in this script via direct API calls.
    tryCatch(
      source(file.path("scripts", "05_pull_forcedecks_profiles.R"), local = TRUE),
      error = function(e) {
        cat("[Refresh] WARNING: 05_pull_forcedecks_profiles.R failed; continuing.\n")
        cat("[Refresh] Reason:", conditionMessage(e), "\n")
      }
    )
  } else {
    cat("[Refresh] Skipping valdr profile pull (VALD_RUN_VALDR_PROFILE_PULL=false).\n")
  }
  source(file.path("scripts", "06_pull_forcedecks_tests_all_history.R"), local = TRUE)
  source(file.path("scripts", "07_pull_forcedecks_metrics_all_history_baseball.R"), local = TRUE)
  cat("[Refresh] Full pull completed. Rebuilding baseball dashboard outputs...\n")
} else {
  cat("\n[Refresh] Skipping full pull (VALD_RUN_FULL_PULL_FIRST=false). Rebuilding from local files only.\n")
}

if (RUN_ARMCARE_PULL) {
  cat("\n[Refresh] ARMCARE_RUN_PULL=true -> attempting ArmCare pull...\n")
  tryCatch(
    source(file.path("scripts", "10_pull_armcare_all_history.R"), local = TRUE),
    error = function(e) {
      cat("[Refresh] WARNING: ArmCare pull failed; continuing VALD rebuild.\n")
      cat("[Refresh] ArmCare reason:", conditionMessage(e), "\n")
    }
  )

  if (RUN_ARMCARE_BUILD && file.exists(file.path(DATA_DIR, "armcare_raw_all_history.rds"))) {
    tryCatch(
      source(file.path("scripts", "11_build_armcare_outputs.R"), local = TRUE),
      error = function(e) {
        cat("[Refresh] WARNING: ArmCare build step failed.\n")
        cat("[Refresh] ArmCare build reason:", conditionMessage(e), "\n")
      }
    )
  }
}

if (RUN_ARMCARE_CSV_IMPORT) {
  cat("\n[Refresh] ARMCARE_RUN_CSV_IMPORT=true -> importing ArmCare CSV...\n")
  tryCatch(
    source(file.path("scripts", "12_pull_armcare_from_csv.R"), local = TRUE),
    error = function(e) {
      cat("[Refresh] WARNING: ArmCare CSV import failed; continuing VALD rebuild.\n")
      cat("[Refresh] ArmCare CSV reason:", conditionMessage(e), "\n")
    }
  )

  if (RUN_ARMCARE_BUILD && file.exists(file.path(DATA_DIR, "armcare_raw_all_history.rds"))) {
    tryCatch(
      source(file.path("scripts", "11_build_armcare_outputs.R"), local = TRUE),
      error = function(e) {
        cat("[Refresh] WARNING: ArmCare build step failed after CSV import.\n")
        cat("[Refresh] ArmCare build reason:", conditionMessage(e), "\n")
      }
    )
  }
}

# ----------------------------
# Load ForceDecks inputs + join tests + join profiles
# ----------------------------
m <- readRDS(IN_METRICS_RDS)
tests <- readRDS(TESTS_RDS)

get_col_chr <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

tests_small <- tibble(
  testId = as.character(tests$testId),
  test_profileId = get_col_chr(tests, c("profileId")),
  test_testType = get_col_chr(tests, c("testType")),
  test_recordedUTC = get_col_chr(tests, c("recordedUTC", "recordedDateUtc")),
  test_recordedOffset = get_col_chr(tests, c("recordedOffset", "recordedDateOffset")),
  test_recordedTimezone = get_col_chr(tests, c("recordedTimezone", "recordedDateTimezone"))
)

stopifnot("testId" %in% names(m))
stopifnot("testId" %in% names(tests_small))

m <- m %>%
  left_join(tests_small, by = "testId") %>%
  mutate(
    profileId = dplyr::coalesce(as.character(profileId), as.character(test_profileId)),
    testType = dplyr::coalesce(as.character(testType), as.character(test_testType)),
    recordedUTC = dplyr::coalesce(as.character(recordedUTC), as.character(test_recordedUTC)),
    recordedOffset = dplyr::coalesce(as.character(recordedOffset), as.character(test_recordedOffset)),
    recordedTimezone = dplyr::coalesce(as.character(recordedTimezone), as.character(test_recordedTimezone))
  ) %>%
  select(-any_of(c("test_profileId", "test_testType", "test_recordedUTC", "test_recordedOffset", "test_recordedTimezone")))

if (!("profileId" %in% names(m)) || all(is.na(m$profileId) | !nzchar(as.character(m$profileId)))) {
  stop("After joining tests, profileId is still missing from metrics.")
}
if (!("testType" %in% names(m)) || all(is.na(m$testType) | !nzchar(as.character(m$testType)))) {
  stop("After joining tests, testType is still missing from metrics.")
}

m <- m %>% left_join(profiles_groups, by = "profileId")

# Normalize common columns that can appear with .x/.y suffixes after joins.
coalesce_chr_cols <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  out <- as.character(df[[hit[[1]]]])
  if (length(hit) > 1) {
    for (h in hit[-1]) out <- dplyr::coalesce(out, as.character(df[[h]]))
  }
  out
}

m <- m %>%
  mutate(
    athleteName = coalesce_chr_cols(., c("athleteName", "athleteName.x", "athleteName.y")),
    externalId = coalesce_chr_cols(., c("externalId", "externalId.x", "externalId.y")),
    groupNames_csv = coalesce_chr_cols(., c("groupNames_csv", "groupNames_csv.x", "groupNames_csv.y")),
    primaryGroup = coalesce_chr_cols(., c("primaryGroup", "primaryGroup.x", "primaryGroup.y"))
  )

if (!("groupNames_csv" %in% names(m))) m$groupNames_csv <- ""
if (!("primaryGroup"   %in% names(m))) m$primaryGroup   <- "Other"

# ----------------------------
# Baseball filter
# ----------------------------
m <- m %>%
  mutate(
    externalId  = ifelse(is.na(externalId), "", externalId),
    is_baseball = grepl(BASEBALL_ID_REGEX, externalId)
  ) %>%
  filter(is_baseball) %>%
  select(-is_baseball)

if (nrow(m) == 0) stop("After baseball filter, 0 rows remain. Not writing outputs.")

cat("Loaded metric rows (baseball):", nrow(m), "\n")
cat("Unique athletes:", dplyr::n_distinct(m$profileId), "\n")

# ----------------------------
# Roster output (baseball only)
# ----------------------------
roster <- m %>%
  distinct(profileId, athleteName, externalId, groupNames_csv, primaryGroup) %>%
  arrange(primaryGroup, athleteName)

saveRDS(roster, OUT_ROSTER_RDS)
cat("Saved roster:\n ", OUT_ROSTER_RDS, "\n")

# ----------------------------
# Save enriched trial-level metrics
# ----------------------------
saveRDS(m, OUT_METRICS_ENRICHED_RDS)
cat("Saved enriched trial-level metrics:\n ", OUT_METRICS_ENRICHED_RDS, "\n")

# ----------------------------
# Session summaries (mean/sd across trials)
# ----------------------------
sess <- m %>%
  mutate(
    recordedUTC_dt = suppressWarnings(lubridate::as_datetime(recordedUTC, tz = "UTC")),
    session_date   = as.Date(recordedUTC_dt)
  ) %>%
  group_by(
    profileId, athleteName, externalId,
    groupNames_csv, primaryGroup,
    testType, session_date,
    metricKey, metricName, metricUnit
  ) %>%
  summarise(
    n_trials   = n_distinct(trialId),
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    cv_value   = dplyr::if_else(
      is.finite(mean_value) & mean_value != 0,
      sd_value / abs(mean_value),
      NA_real_
    ),
    best_value      = suppressWarnings(max(value, na.rm = TRUE)),
    recordedUTC_max = suppressWarnings(max(recordedUTC_dt, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    cv_flag = !is.na(cv_value) & (cv_value >= COV_THRESHOLD)
  )

saveRDS(sess, OUT_SESS_SUMMARY_RDS)
cat("Saved session summary:\n ", OUT_SESS_SUMMARY_RDS, "\n")

# ----------------------------
# Most recent vs previous comparison
# ----------------------------
recent2 <- sess %>%
  arrange(profileId, testType, metricKey, desc(session_date), desc(recordedUTC_max)) %>%
  group_by(profileId, testType, metricKey) %>%
  mutate(rank_session = dplyr::row_number()) %>%
  filter(rank_session <= 2) %>%
  ungroup()

cur <- recent2 %>%
  filter(rank_session == 1) %>%
  select(
    profileId, athleteName, externalId, groupNames_csv, primaryGroup,
    testType, metricKey, metricName, metricUnit,
    mean_value, sd_value, cv_value, best_value, n_trials, session_date, recordedUTC_max, cv_flag
  ) %>%
  rename_with(~ paste0(.x, "_cur"),
              c(mean_value, sd_value, cv_value, best_value, n_trials, session_date, recordedUTC_max, cv_flag))

prev <- recent2 %>%
  filter(rank_session == 2) %>%
  select(
    profileId, testType, metricKey,
    mean_value, sd_value, cv_value, best_value, n_trials, session_date, recordedUTC_max, cv_flag
  ) %>%
  rename_with(~ paste0(.x, "_prev"),
              c(mean_value, sd_value, cv_value, best_value, n_trials, session_date, recordedUTC_max, cv_flag))

comp <- cur %>%
  inner_join(prev, by = c("profileId", "testType", "metricKey")) %>%
  mutate(
    delta_mean = mean_value_cur - mean_value_prev,
    pct_change_mean = dplyr::if_else(
      !is.na(mean_value_prev) & mean_value_prev != 0,
      (mean_value_cur - mean_value_prev) / abs(mean_value_prev),
      NA_real_
    ),
    
    n_for_alpha          = pmin(n_trials_cur, n_trials_prev, na.rm = TRUE),
    model_stat_available = !is.na(n_for_alpha) & n_for_alpha >= 3,
    a_crit               = purrr::map_dbl(n_for_alpha, get_a_crit, alpha = TRIAL_SIZE_ALPHA),
    
    pooled_sd = sqrt((sd_value_cur^2 + sd_value_prev^2) / 2),
    model_stat = dplyr::if_else(
      model_stat_available & !is.na(pooled_sd) & pooled_sd > 0,
      abs(delta_mean) / pooled_sd,
      NA_real_
    ),
    
    flag_cv = !is.na(cv_value_cur) & (cv_value_cur >= COV_THRESHOLD),
    
    flag_model = dplyr::if_else(
      model_stat_available & !is.na(model_stat) & !is.na(a_crit) & (model_stat >= a_crit),
      TRUE, FALSE
    ),
    
    flag_either = flag_cv | flag_model,
    flag_both   = flag_cv & flag_model,
    
    badge = dplyr::case_when(
      flag_both   ~ "Red",
      flag_either ~ "Yellow",
      TRUE        ~ "Green"
    ),
    badge_level = dplyr::case_when(
      flag_both   ~ 3L,
      flag_either ~ 2L,
      TRUE        ~ 1L
    ),
    
    cov_threshold = COV_THRESHOLD
  ) %>%
  select(
    profileId, athleteName, externalId,
    groupNames_csv, primaryGroup,
    testType,
    metricKey, metricName, metricUnit,
    session_date_cur, session_date_prev,
    n_trials_cur, n_trials_prev, n_for_alpha,
    mean_value_cur, sd_value_cur, cv_value_cur,
    mean_value_prev, sd_value_prev, cv_value_prev,
    delta_mean, pct_change_mean,
    cov_threshold,
    model_stat, a_crit, model_stat_available,
    flag_cv, flag_model, flag_either, flag_both,
    badge, badge_level
  )

saveRDS(comp, OUT_SESS_COMPARE_RDS)
cat("Saved session comparison:\n ", OUT_SESS_COMPARE_RDS, "\n")

# ----------------------------
# Diagnostics summary
# ----------------------------
cat("\nDiagnostics:\n")
cat("Groups:", nrow(groups_df), "\n")
cat("Profiles rows:", nrow(profiles_groups), "\n")
cat("Profiles by primaryGroup:\n")
print(profiles_groups %>% count(primaryGroup, sort = TRUE))

cat("Roster rows:", nrow(roster), "\n")
cat("Roster by primaryGroup:\n")
print(roster %>% count(primaryGroup, sort = TRUE))

cat("Session summary rows:", nrow(sess), "\n")
cat("Comparison rows:", nrow(comp), "\n")
cat("Model stat available (TRUE):", sum(comp$model_stat_available %in% TRUE, na.rm = TRUE), "\n")
cat("Flagged by CV:", sum(comp$flag_cv %in% TRUE, na.rm = TRUE), "\n")
cat("Flagged by model stat:", sum(comp$flag_model %in% TRUE, na.rm = TRUE), "\n")
cat("Flagged by BOTH:", sum(comp$flag_both %in% TRUE, na.rm = TRUE), "\n")
