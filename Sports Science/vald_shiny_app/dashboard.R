# ============================================================
# TXST Baseball | CMJ + Sprint Performance Dashboard (SHARE COPY)
# ============================================================
# Standalone, single-file Shiny app extracted from the internal TXST Baseball
# ForceDecks dashboard for sharing outside the program (e.g. a central
# multi-sport/multi-team dashboard hub). It intentionally includes ONLY the
# CMJ (VALD ForceDecks) and Sprint (SmartSpeed) portions of that dashboard.
#
# EXCLUDED ON PURPOSE:
#   - ArmCare (pitcher shoulder/arm data) -- not included anywhere in this
#     file: no ArmCare trends, no ArmCare status, no "Integrated" CMJ+ArmCare
#     combined status. Pitchers here are scored on CMJ alone.
#   - Trackman -- not part of the source dashboard this was extracted from,
#     so there is nothing to exclude; noted here only because it was named
#     explicitly when this file was requested.
#   - The persistent staff review/decision workflow (review status, staff
#     notes, SQLite storage) from the internal Alert Inbox. This copy shows
#     the same CMJ flagging logic and filters, but does not read/write the
#     internal app's staff_workflow.sqlite -- it is a read-only view.
#   - Session Summary, Advanced Compare, Reports/Admin, and the Daily Staff
#     Report were not requested for this share and are not included.
#
# WHAT'S INCLUDED (all CMJ/Sprint, matching the internal dashboard's current
# scoring logic as of 2026-08-07, including the season-phase-aware baselines,
# "No Recent Data" (not silent Green) on insufficient baseline, and the
# empirically recalibrated RSI-modified / hitters' Concentric Impulse
# thresholds):
#   - Alert Inbox (CMJ-only): filters, KPI strip, flagged-athlete table,
#     "why flagged" detail panel, CSV export, Open Athlete Profile handoff.
#   - Team Overview (CMJ-only): role filter, KPIs, general CMJ preset table,
#     pitcher performance/injury-driver add-on tables, bar chart, and the
#     status-first coach report (Team/Pitcher/Hitter HTML).
#   - Athlete Profile (CMJ/Sprint only): status cards, CMJ longitudinal
#     trends, Sprint performance (summary/trend/history), the CMJ<->Sprint
#     relationship check, current CMJ status detail, and a handoff to Curve
#     View.
#   - CMJ Monitoring: monitoring window selector, pitcher staff table and
#     hitter readiness table (both CMJ-only, color-coded), status
#     distribution chart, readiness trends table.
#   - Sprint / SmartSpeed: latest testing table, trend chart, top movers,
#     stale-data table.
#   - Curve View (Force Tracing): single/two-trials/two-sessions force-time
#     curve comparison, exactly as in the internal app.
#
# NOTE: pitchers and hitters are both scored through the SAME CMJ
# vulnerability/performance pipeline here (role-specific thresholds still
# come from THRESHOLDS below). The internal app kept them on separate code
# paths only because pitchers also combined with ArmCare into an "Integrated"
# status; with ArmCare removed there is no reason to keep that split. Hitter
# readiness is still exploratory/CMJ-only, consistent with the source
# dashboard's product guardrails -- labeled as such in the UI.
#
# ------------------------------------------------------------
# REQUIRED DATA FILES
# ------------------------------------------------------------
# Point DATA_DIR (below) at a directory containing these files, produced by
# the internal dashboard's existing pipeline (run_pipeline.R):
#   - roster_baseball.rds
#   - force_sessions_summary_baseball.rds
#   - sprint_session_level.rds                 (optional; Sprint tab degrades
#                                                gracefully if absent)
#   - force_tests_baseball_all_history_named.rds            (Curve View only)
#   - force_metrics_long_all_history_baseball_enriched.rds  (Curve View only;
#     falls back to force_metrics_long_all_history_baseball.rds if absent)
#
# Curve View additionally needs live VALD API access at click-time (it pulls
# the actual force-time recording per session, not from the RDS files above),
# so these environment variables must be set: VALD_CLIENT_ID,
# VALD_CLIENT_SECRET, VALD_TEAM_ID. Every other tab works with zero VALD
# credentials, using only the RDS files.
#
# "Refresh data" (top nav) can ALSO pull new CMJ/ForceDecks and SmartSpeed
# tests live from VALD before reloading the files above -- CMJs happen all
# day and staff shouldn't have to wait on a scheduled job to see them. This
# needs the pipeline scripts this app was handed off alongside (see "Live
# VALD refresh" below, near load_shared_data()) plus the same VALD_CLIENT_ID/
# VALD_CLIENT_SECRET/VALD_TEAM_ID. Without those files/credentials present,
# Refresh falls back to today's behavior: just re-reading whatever's already
# on disk. ArmCare and Trackman are never pulled here, on any refresh --
# this share copy doesn't use either.
# ============================================================

options(sass.cache = file.path(tempdir(), "vald-sass"))
BASE_PLAYER_HEALTH_EMBEDDED <- isTRUE(get0(
  "BASE_PLAYER_HEALTH_EMBEDDED",
  ifnotfound = FALSE,
  inherits = FALSE
))
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(plotly)
  library(readr)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(rlang)
  library(tibble)
})

# ------------------------------------------------------------
# Data directory -- EDIT THIS if your copy of the RDS files lives elsewhere.
# Defaults to ./data/gold, falling back to ./Data (matches the internal
# dashboard's own gold-layer-first, legacy-fallback convention).
# ------------------------------------------------------------
GOLD_DIR   <- Sys.getenv("CMJ_SPRINT_SHARE_GOLD_DIR", file.path(getwd(), "data", "gold"))
LEGACY_DIR <- Sys.getenv("CMJ_SPRINT_SHARE_LEGACY_DIR", file.path(getwd(), "data", "legacy"))

dashboard_data_path <- function(filename) {
  gold_path <- file.path(GOLD_DIR, filename)
  if (file.exists(gold_path)) return(gold_path)
  file.path(LEGACY_DIR, filename)
}

# ============================================================
# Small shared utilities
# ============================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

ensure_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (!("athleteName" %in% names(df))) df$athleteName <- NA_character_
  if (!("externalId" %in% names(df))) df$externalId <- ""
  if (!("notes" %in% names(df))) df$notes <- ""
  df
}

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

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}
get_date_col      <- function(df) pick_col(df, c("session_date", "sessionDateUtc", "recordedDateUtc", "date", "test_date"))
get_best_col      <- function(df) pick_col(df, c("best_value", "bestValue", "best", "Best", "value_best", "bestValueNormalized"))
get_mean_col      <- function(df) pick_col(df, c("mean_value", "meanValue", "mean", "Mean"))
get_sd_col        <- function(df) pick_col(df, c("sd_value", "sdValue", "sd", "SD"))
get_cv_col        <- function(df) pick_col(df, c("cv_value", "cvValue", "cv", "CV"))
get_metric_id_col <- function(df) pick_col(df, c("metricName", "metricKey"))
as_num <- function(x) suppressWarnings(as.numeric(x))

as_date_safely <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.character(x)) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dx <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
      if (all(is.na(dx))) dx <- suppressWarnings(lubridate::ymd(x))
      if (!all(is.na(dx))) return(as.Date(dx))
    }
  }
  suppressWarnings(as.Date(x))
}

safe_slope <- function(dates, values) {
  d <- as.numeric(as_date_safely(dates))
  v <- as.numeric(values)
  ok <- !is.na(d) & !is.na(v)
  if (sum(ok) < 2) return(NA_real_)
  d <- d[ok]; v <- v[ok]
  if (length(unique(d)) < 2) return(NA_real_)
  tryCatch(unname(coef(lm(v ~ d))[2]), error = function(e) NA_real_)
}

cfg_get <- function(x, path, default = NULL) {
  cur <- x
  for (part in path) {
    if (is.null(cur) || is.null(cur[[part]])) return(default)
    cur <- cur[[part]]
  }
  cur %||% default
}

empty_dt <- function(message, reason = NULL) {
  txt <- if (!is.null(reason) && nzchar(reason)) paste0(message, " | Reason: ", reason) else message
  DT::datatable(tibble(Message = txt), options = list(dom = "t"), rownames = FALSE)
}
plotly_empty <- function() plotly::plotly_empty(type = "scatter", mode = "markers")

TXST <- list(
  maroon = "#501214", gold = "#F6BE00", green = "#16a34a",
  yellow = "#f59e0b", red = "#ef4444", dark = "#0b0f19",
  line = "#1f2937", white = "rgba(255,255,255,0.92)"
)

ACTIVE_CMJ_CUTOFF <- as.Date("2026-08-01")

# ============================================================
# Season phases (config/season_phases.yml equivalent) -- update these dates
# each season; a date outside every range below is treated as "Unassigned"
# and only baselines against other "Unassigned"-dated sessions.
# ============================================================
SEASON_PHASES <- data.frame(
  name  = c("Fall", "Off-season", "Preseason", "In-season", "Postseason",
            "Fall", "Off-season", "Preseason", "In-season", "Postseason"),
  start = as.Date(c("2025-08-01", "2025-12-16", "2026-01-16", "2026-02-13", "2026-06-01",
                     "2026-08-01", "2026-12-16", "2027-01-16", "2027-02-13", "2027-06-01")),
  end   = as.Date(c("2025-12-15", "2026-01-15", "2026-02-12", "2026-05-31", "2026-06-30",
                     "2026-12-15", "2027-01-15", "2027-02-12", "2027-05-31", "2027-06-30")),
  stringsAsFactors = FALSE
)

assign_season_phase <- function(date, phases = SEASON_PHASES) {
  d <- as_date_safely(date)
  out <- rep("Unassigned", length(d))
  if (!is.data.frame(phases) || nrow(phases) == 0) return(out)
  for (i in seq_len(nrow(phases))) {
    hit <- !is.na(d) & d >= phases$start[[i]] & d <= phases$end[[i]]
    out[hit] <- phases$name[[i]]
  }
  out
}

# ============================================================
# Thresholds (config/thresholds.yml equivalent -- CMJ + Sprint sections only)
# Values match the internal dashboard's model_version txst-monitoring-2026-08-07,
# including the 2026-08-07 empirical RSI-modified / hitters' Concentric
# Impulse recalibration against this team's own session-to-session history.
# ============================================================
THRESHOLDS <- list(
  status_points = list(Red = 2L, Yellow = 1L, `Blue` = 0L, Green = 0L, `No Recent Data` = 0L),
  confidence = list(high_recent_days = 7, moderate_recent_days = 14, stale_days = 28,
                    min_baseline_n_high = 4, min_baseline_n_moderate = 2),
  cmj = list(
    pitchers = list(
      performance = list(composite_yellow_below = -0.75, composite_red_below = -1.25, green_blue_min_positive_triggers = 3L),
      vulnerability = list(
        composite_yellow_below = -1.0, composite_red_below = -1.5, red_trigger_count = 2L,
        contraction_time = list(yellow_absolute_change = 10, red_absolute_change = 15, direction = "higher_is_concern"),
        concentric_impulse = list(yellow_percent_change = -5, red_percent_change = -8, direction = "lower_is_concern"),
        rsi_modified = list(yellow_percent_change = -10, red_percent_change = -15, direction = "lower_is_concern"),
        jump_height = list(yellow_z = -1.0, red_z = -1.5, direction = "lower_is_concern")
      )
    ),
    hitters = list(
      performance = list(composite_yellow_below = -0.6, composite_red_below = -1.0, green_blue_min_positive_triggers = 3L),
      vulnerability = list(
        composite_yellow_below = -0.75, composite_red_below = -1.25, red_trigger_count = 2L,
        contraction_time = list(yellow_absolute_change = 7, red_absolute_change = 12, direction = "higher_is_concern"),
        concentric_impulse = list(yellow_percent_change = -4.5, red_percent_change = -7.5, direction = "lower_is_concern"),
        rsi_modified = list(yellow_percent_change = -11, red_percent_change = -17, direction = "lower_is_concern"),
        jump_height = list(yellow_z = -1.0, red_z = -1.5, direction = "lower_is_concern")
      )
    )
  ),
  smartspeed = list(
    performance = list(
      best_10yd = list(yellow_percent_change = 3, red_percent_change = 5, min_baseline_n = 2, direction = "higher_is_concern"),
      best_30yd = list(yellow_percent_change = 3, red_percent_change = 5, min_baseline_n = 2, direction = "higher_is_concern"),
      best_flying_10yd = list(yellow_percent_change = 3, red_percent_change = 5, min_baseline_n = 2, direction = "higher_is_concern"),
      max_velocity = list(yellow_percent_change = -3, red_percent_change = -5, min_baseline_n = 2, direction = "lower_is_concern")
    )
  )
)
load_thresholds <- function() THRESHOLDS

SPRINT_METRIC_LABEL <- c(
  best_10yd = "Best 10-yard", best_30yd = "Best 30-yard",
  best_flying_10yd = "Best flying 10-yard", max_velocity = "Max velocity"
)

# ============================================================
# CMJ metric constants (R/build_monitoring_status.R equivalent)
# ============================================================
CMJ_MONITOR_METRICS <- c("Contraction Time", "Concentric Impulse", "RSI-modified", "Eccentric Braking Impulse", "Jump Height (Imp-Mom)")
CMJ_METRIC_LABEL <- c(
  "Contraction Time" = "Contraction Time (ms)",
  "Concentric Impulse" = "Concentric Impulse (N*s)",
  "RSI-modified" = "RSI-Modified (m/s)",
  "Eccentric Braking Impulse" = "Eccentric Braking Impulse",
  "Jump Height (Imp-Mom)" = "Jump Height"
)

HITTER_METRICS_ORDERED <- c(
  "Eccentric Acceleration Phase Duration", "Eccentric Deceleration Phase Duration", "Eccentric Braking RFD",
  "Eccentric Duration", "Countermovement Depth", "Force at Zero Velocity", "P1 Concentric Impulse",
  "P2 Concentric Impulse", "P2 Concentric Impulse:P1 Concentric Impulse", "Concentric Impulse",
  "Vertical Velocity at Takeoff", "RSI-modified", "Peak Power / BM", "Peak Power", "Bodyweight in Pounds"
)
PITCHER_PERF_METRICS <- c(
  "Jump Height (Imp-Mom)", "RSI-modified", "Concentric Mean Power / BM", "Concentric Impulse (Abs) / BM",
  "Concentric Peak Force / BM", "Contraction Time", "Braking Phase Duration"
)
PITCHER_INJURY_VALUE_METRICS <- c("Jump Height (Imp-Mom)", "Peak Power", "Concentric Impulse", "Braking Phase Duration", "Contraction Time")
PITCHER_INJURY_SD_METRICS <- c("Jump Height (Imp-Mom)", "Peak Power")

dt_team_opts <- function(page_len = 18, left_cols = 2) {
  list(pageLength = page_len, dom = "Btip", buttons = list("colvis"), scrollX = TRUE,
       scrollY = FALSE, scrollCollapse = FALSE, autoWidth = TRUE, fixedColumns = list(leftColumns = left_cols))
}

# ============================================================
# Roster / CMJ monitoring status builders
# (R/build_monitoring_status.R equivalent, CMJ-only)
# ============================================================
derive_roster_tbl <- function(raw_roster) {
  if (is.null(raw_roster) || !is.data.frame(raw_roster) || nrow(raw_roster) == 0) return(tibble())
  raw_roster %>%
    mutate(
      profileId = as.character(profileId %||% ""),
      athleteName = as.character(athleteName %||% ""),
      externalId = as.character(externalId %||% ""),
      primaryGroup = as.character(primaryGroup %||% "Other"),
      roleGroup = ifelse(primaryGroup == "Pitchers", "Pitchers", "Hitters")
    ) %>%
    filter(nzchar(profileId)) %>%
    distinct(profileId, .keep_all = TRUE)
}

build_cmj_monitor_long <- function(session_summary, roster, metric_map = NULL, cmj_metrics = CMJ_MONITOR_METRICS) {
  s <- session_summary
  if (is.null(s) || !is.data.frame(s) || nrow(s) == 0) return(tibble())

  mc <- get_metric_id_col(s); dc <- get_date_col(s); bc <- get_best_col(s)
  if (is.null(mc) || is.null(dc) || is.null(bc)) return(tibble())

  df <- s %>%
    filter(testType == "CMJ") %>%
    mutate(session_date = as_date_safely(.data[[dc]]), value = as_num(.data[[bc]])) %>%
    filter(!is.na(session_date), !is.na(value))

  if (mc == "metricKey" && !("metricName" %in% names(df)) && !is.null(metric_map) && nrow(metric_map) > 0) {
    df <- df %>% left_join(metric_map, by = c("metricKey" = "metricKey"))
  }
  mdisp <- if ("metricName" %in% names(df)) "metricName" else mc

  df <- df %>%
    filter(.data[[mdisp]] %in% cmj_metrics) %>%
    transmute(
      profileId = as.character(profileId), athleteName = as.character(athleteName),
      externalId = as.character(externalId), metric = as.character(.data[[mdisp]]),
      session_date,
      # VALD's raw API export reports Jump Height in centimeters, so its
      # "RSI-modified" value (Jump Height / Time to Takeoff) comes out ~100x
      # the conventional 0.2-1.0 m/s scale (e.g. 77.5 instead of 0.775).
      # Percent-change and z-score thresholds are scale-invariant, so this
      # only affects displayed/absolute values, not Red/Yellow/Green calls.
      value = ifelse(as.character(.data[[mdisp]]) == "RSI-modified", value / 100, value)
    ) %>%
    group_by(profileId, athleteName, externalId, metric, session_date) %>%
    summarise(value = max(value, na.rm = TRUE), .groups = "drop")

  if (!is.null(roster) && is.data.frame(roster) && nrow(roster) > 0) {
    df <- df %>%
      left_join(roster %>% select(profileId, roleGroup), by = "profileId") %>%
      mutate(roleGroup = ifelse(is.na(roleGroup), "Hitters", roleGroup))
  } else {
    df <- df %>% mutate(roleGroup = "Hitters")
  }
  df
}

# Season-phase-aware baseline: baseline_mean/sd only pulls from sessions in
# the SAME season phase as the anchor date, not unbounded prior history.
build_cmj_metric_status <- function(cmj_long, window_days, anchor_date = NULL, metric_labels = CMJ_METRIC_LABEL) {
  d <- cmj_long
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(tibble())

  anchor <- if (!is.null(anchor_date)) as.Date(anchor_date) else suppressWarnings(max(d$session_date, na.rm = TRUE))
  if (is.na(anchor)) return(tibble())
  win_start <- anchor - (as.integer(window_days) - 1L)

  anchor_phase <- assign_season_phase(anchor)
  baseline <- d %>%
    mutate(session_phase = assign_season_phase(session_date)) %>%
    filter(session_date < win_start, session_phase == anchor_phase) %>%
    group_by(profileId, metric) %>%
    summarise(baseline_mean = mean(value, na.rm = TRUE), baseline_sd = sd(value, na.rm = TRUE), .groups = "drop")

  slope_hist <- d %>%
    group_by(profileId, metric) %>%
    arrange(session_date, .by_group = TRUE) %>%
    mutate(dt_days = as.numeric(session_date - dplyr::lag(session_date)),
           slope_step = (value - dplyr::lag(value)) / dt_days) %>%
    filter(!is.na(slope_step), is.finite(slope_step), dt_days > 0) %>%
    summarise(slope_mu = mean(slope_step, na.rm = TRUE), slope_sd = sd(slope_step, na.rm = TRUE), .groups = "drop")

  st <- dplyr::full_join(baseline, slope_hist, by = c("profileId", "metric"))

  d %>%
    filter(session_date >= win_start, session_date <= anchor) %>%
    group_by(profileId, athleteName, externalId, roleGroup, metric) %>%
    arrange(session_date, .by_group = TRUE) %>%
    summarise(
      first_value = dplyr::first(value), last_value = dplyr::last(value),
      delta = last_value - first_value, current_slope = safe_slope(session_date, value),
      n_points = dplyr::n(), last_date = max(session_date, na.rm = TRUE), .groups = "drop"
    ) %>%
    left_join(st, by = c("profileId", "metric")) %>%
    mutate(
      baseline_sd = ifelse(is.na(baseline_sd) | baseline_sd == 0, NA_real_, baseline_sd),
      slope_sd = ifelse(is.na(slope_sd) | slope_sd == 0, NA_real_, slope_sd),
      z_last = (last_value - baseline_mean) / baseline_sd,
      slope_z = (current_slope - slope_mu) / slope_sd,
      delta_pct_vs_baseline = 100 * (last_value - baseline_mean) / ifelse(is.na(baseline_mean) | baseline_mean == 0, NA_real_, abs(baseline_mean)),
      metric_label = dplyr::recode(metric, !!!as.list(metric_labels))
    )
}

# ============================================================
# Confidence / plain-language reason helpers (R/utils_status.R equivalent)
# ============================================================
status_rank <- function(status) {
  dplyr::case_when(
    status == "Red" ~ 4L, status == "Yellow" ~ 3L, status == "Blue" ~ 2L,
    status == "Green" ~ 1L, status == "No Recent Data" ~ 0L, TRUE ~ 0L
  )
}

confidence_from_recency <- function(days_since_last, baseline_n, thresholds = load_thresholds()) {
  if (is.na(days_since_last)) return("No Recent Data")
  high_days <- as.numeric(cfg_get(thresholds, c("confidence", "high_recent_days"), 7))
  moderate_days <- as.numeric(cfg_get(thresholds, c("confidence", "moderate_recent_days"), 14))
  min_high <- as.numeric(cfg_get(thresholds, c("confidence", "min_baseline_n_high"), 4))
  min_mod <- as.numeric(cfg_get(thresholds, c("confidence", "min_baseline_n_moderate"), 2))
  if (!is.na(days_since_last) && days_since_last <= high_days && baseline_n >= min_high) return("High")
  if (!is.na(days_since_last) && days_since_last <= moderate_days && baseline_n >= min_mod) return("Moderate")
  "Low"
}

human_reason <- function(metric, current_value, baseline_value, absolute_change, percent_change, threshold_used, direction, trigger_level) {
  change_text <- if (!is.na(percent_change)) {
    paste0(round(percent_change, 1), "%")
  } else if (!is.na(absolute_change)) {
    paste0(round(absolute_change, 2), " (absolute)")
  } else "an unavailable amount"
  paste0(
    metric, " changed by ", change_text, " from baseline",
    ifelse(is.na(baseline_value), "", paste0(" (baseline ", round(baseline_value, 2), ")")),
    ifelse(is.na(current_value), "", paste0(" to ", round(current_value, 2))),
    ". Threshold used: ", threshold_used, ". Direction: ", direction, ". Trigger level: ", trigger_level, "."
  )
}

# ============================================================
# CMJ scoring (R/score_cmj_vulnerability.R + score_cmj_performance.R
# equivalent -- same logic for pitchers AND hitters; role-specific thresholds
# come from THRESHOLDS$cmj$pitchers / $hitters above)
# ============================================================
cmj_metric_value <- function(df, metric_name, col_name) {
  v <- df |> filter(metric == metric_name) |> pull(.data[[col_name]])
  if (length(v) == 0) NA_real_ else v[[1]]
}

score_cmj_vulnerability <- function(ms, thresholds = load_thresholds()) {
  if (is.null(ms) || nrow(ms) == 0) return(tibble())

  ms |>
    group_by(profileId, athleteName, externalId, roleGroup) |>
    group_modify(~{
      df <- .x
      role_key <- if (identical(.y$roleGroup[[1]], "Pitchers")) "pitchers" else "hitters"
      base_path <- c("cmj", role_key, "vulnerability")

      ct_delta <- cmj_metric_value(df, "Contraction Time", "delta")
      ct_base <- cmj_metric_value(df, "Contraction Time", "baseline_mean")
      imp_delta <- cmj_metric_value(df, "Concentric Impulse", "delta")
      imp_delta_pct <- cmj_metric_value(df, "Concentric Impulse", "delta_pct_vs_baseline")
      imp_base <- cmj_metric_value(df, "Concentric Impulse", "baseline_mean")
      rsi_delta <- cmj_metric_value(df, "RSI-modified", "delta")
      rsi_delta_pct <- cmj_metric_value(df, "RSI-modified", "delta_pct_vs_baseline")
      rsi_base <- cmj_metric_value(df, "RSI-modified", "baseline_mean")
      jump_last_z <- cmj_metric_value(df, "Jump Height (Imp-Mom)", "z_last")
      jump_last <- cmj_metric_value(df, "Jump Height (Imp-Mom)", "last_value")
      jump_base <- cmj_metric_value(df, "Jump Height (Imp-Mom)", "baseline_mean")

      ct_y <- as.numeric(cfg_get(thresholds, c(base_path, "contraction_time", "yellow_absolute_change"), 10))
      ct_r <- as.numeric(cfg_get(thresholds, c(base_path, "contraction_time", "red_absolute_change"), 15))
      imp_y <- as.numeric(cfg_get(thresholds, c(base_path, "concentric_impulse", "yellow_percent_change"), -5))
      imp_r <- as.numeric(cfg_get(thresholds, c(base_path, "concentric_impulse", "red_percent_change"), -8))
      rsi_y <- as.numeric(cfg_get(thresholds, c(base_path, "rsi_modified", "yellow_percent_change"), -5))
      rsi_r <- as.numeric(cfg_get(thresholds, c(base_path, "rsi_modified", "red_percent_change"), -8))
      jump_y <- as.numeric(cfg_get(thresholds, c(base_path, "jump_height", "yellow_z"), -1))
      jump_r <- as.numeric(cfg_get(thresholds, c(base_path, "jump_height", "red_z"), -1.5))

      ct_level <- dplyr::case_when(!is.na(ct_delta) & ct_delta >= ct_r ~ "Red", !is.na(ct_delta) & ct_delta >= ct_y ~ "Yellow", TRUE ~ "Green")
      imp_level <- dplyr::case_when(!is.na(imp_delta_pct) & imp_delta_pct <= imp_r ~ "Red", !is.na(imp_delta_pct) & imp_delta_pct <= imp_y ~ "Yellow", TRUE ~ "Green")
      rsi_level <- dplyr::case_when(!is.na(rsi_delta_pct) & rsi_delta_pct <= rsi_r ~ "Red", !is.na(rsi_delta_pct) & rsi_delta_pct <= rsi_y ~ "Yellow", TRUE ~ "Green")
      jump_level <- dplyr::case_when(!is.na(jump_last_z) & jump_last_z <= jump_r ~ "Red", !is.na(jump_last_z) & jump_last_z <= jump_y ~ "Yellow", TRUE ~ "Green")

      red_count <- sum(c(ct_level, imp_level, rsi_level, jump_level) == "Red", na.rm = TRUE)
      yellow_count <- sum(c(ct_level, imp_level, rsi_level, jump_level) == "Yellow", na.rm = TRUE)
      trigger_count <- red_count + yellow_count
      z_vec <- c(jump_last_z, cmj_metric_value(df, "Concentric Impulse", "z_last"), cmj_metric_value(df, "RSI-modified", "z_last"),
                 { z <- cmj_metric_value(df, "Contraction Time", "z_last"); if (is.na(z)) NA_real_ else -z })
      composite <- if (all(is.na(z_vec))) NA_real_ else mean(z_vec, na.rm = TRUE)
      red_trigger_count <- as.integer(cfg_get(thresholds, c(base_path, "red_trigger_count"), 2))
      composite_y <- as.numeric(cfg_get(thresholds, c(base_path, "composite_yellow_below"), -1))
      composite_r <- as.numeric(cfg_get(thresholds, c(base_path, "composite_red_below"), -1.5))

      # No baseline at all (e.g. an athlete's first sessions in a new season
      # phase) means every comparison above came back NA. Report "No Recent
      # Data" instead of silently falling through to "Green".
      no_baseline <- sum(!is.na(c(ct_base, imp_base, rsi_base, jump_base))) == 0

      status <- dplyr::case_when(
        trigger_count >= red_trigger_count | (!is.na(composite) & composite <= composite_r) ~ "Red",
        trigger_count >= 1 | (!is.na(composite) & composite <= composite_y) ~ "Yellow",
        no_baseline ~ "No Recent Data",
        TRUE ~ "Green"
      )
      perf_up <- (!is.na(rsi_delta) && rsi_delta > 0) && (!is.na(ct_delta) && ct_delta < 0) && (!is.na(imp_delta) && imp_delta > 0)
      if (identical(status, "Green") && perf_up) status <- "Blue"

      reasons <- character(0)
      if (ct_level != "Green") reasons <- c(reasons, human_reason("Contraction Time", ct_delta, ct_base, ct_delta, NA_real_, paste0("Yellow +", ct_y, "; Red +", ct_r), "higher_is_concern", ct_level))
      if (imp_level != "Green") reasons <- c(reasons, human_reason("Concentric Impulse", cmj_metric_value(df, "Concentric Impulse", "last_value"), imp_base, imp_delta, imp_delta_pct, paste0("Yellow ", imp_y, "%; Red ", imp_r, "%"), "lower_is_concern", imp_level))
      if (rsi_level != "Green") reasons <- c(reasons, human_reason("RSI-modified", cmj_metric_value(df, "RSI-modified", "last_value"), rsi_base, rsi_delta, rsi_delta_pct, paste0("Yellow ", rsi_y, "%; Red ", rsi_r, "%"), "lower_is_concern", rsi_level))
      if (jump_level != "Green") reasons <- c(reasons, human_reason("Jump Height", jump_last, jump_base, jump_last - jump_base, NA_real_, paste0("Yellow z ", jump_y, "; Red z ", jump_r), "lower_is_concern", jump_level))

      tibble(
        TriggerCount = trigger_count, RedTriggers = red_count, CMJ_Composite = composite,
        PerformanceTrendingUp = perf_up, Status = status,
        reason_text = if (length(reasons) == 0) "No CMJ vulnerability triggers in the selected window." else paste(reasons, collapse = " ")
      )
    }) |>
    ungroup()
}

score_cmj_performance <- function(ms, thresholds = load_thresholds()) {
  if (is.null(ms) || nrow(ms) == 0) return(tibble())

  ms |>
    group_by(profileId, athleteName, externalId, roleGroup) |>
    group_modify(~{
      df <- .x
      role_key <- if (identical(.y$roleGroup[[1]], "Pitchers")) "pitchers" else "hitters"
      base_path <- c("cmj", role_key, "performance")
      ct_delta <- cmj_metric_value(df, "Contraction Time", "delta")
      imp_delta <- cmj_metric_value(df, "Concentric Impulse", "delta")
      rsi_delta <- cmj_metric_value(df, "RSI-modified", "delta")
      jump_last_z <- cmj_metric_value(df, "Jump Height (Imp-Mom)", "z_last")
      ct_last_z <- cmj_metric_value(df, "Contraction Time", "z_last")
      imp_last_z <- cmj_metric_value(df, "Concentric Impulse", "z_last")
      rsi_last_z <- cmj_metric_value(df, "RSI-modified", "z_last")
      z_vec <- c(jump_last_z, imp_last_z, rsi_last_z, ifelse(is.na(ct_last_z), NA_real_, -ct_last_z))
      composite <- if (all(is.na(z_vec))) NA_real_ else mean(z_vec, na.rm = TRUE)
      pos <- sum(c(!is.na(rsi_delta) && rsi_delta > 0, !is.na(ct_delta) && ct_delta < 0, !is.na(imp_delta) && imp_delta > 0), na.rm = TRUE)
      yellow_below <- as.numeric(cfg_get(thresholds, c(base_path, "composite_yellow_below"), -0.75))
      red_below <- as.numeric(cfg_get(thresholds, c(base_path, "composite_red_below"), -1.25))
      min_positive <- as.integer(cfg_get(thresholds, c(base_path, "green_blue_min_positive_triggers"), 3))
      no_baseline <- all(is.na(z_vec))
      status <- dplyr::case_when(
        !is.na(composite) & composite <= red_below ~ "Red",
        !is.na(composite) & composite <= yellow_below ~ "Yellow",
        pos >= min_positive ~ "Blue",
        no_baseline ~ "No Recent Data",
        TRUE ~ "Green"
      )
      tibble(
        CMJ_Performance_Status = status, CMJ_Performance_Composite = composite, CMJ_Performance_PositiveTriggers = pos,
        CMJ_Performance_Reason = paste0(
          "CMJ performance composite = ", ifelse(is.na(composite), "unavailable", round(composite, 2)),
          "; positive readiness triggers = ", pos, "."
        )
      )
    }) |>
    ungroup()
}

# ============================================================
# Sprint / SmartSpeed scoring (R/score_smartspeed.R equivalent)
# ============================================================
score_smartspeed_performance <- function(sprint, thresholds = load_thresholds()) {
  if (is.null(sprint) || !is.data.frame(sprint) || nrow(sprint) == 0) {
    return(data.frame(athlete_id = character(), athlete_name = character(), role = character(),
                       Sprint_Status = character(), Sprint_Confidence = character(), Sprint_Reason = character(),
                       Sprint_Triggers = integer(), Sprint_Last_Date = as.Date(character()), Sprint_Baseline_N = integer(),
                       stringsAsFactors = FALSE))
  }
  cfg <- cfg_get(thresholds, c("smartspeed", "performance"), list())
  metric_map <- list(
    best_10yd = list(label = "10-yard sprint", direction = "higher_is_concern"),
    best_30yd = list(label = "30-yard sprint", direction = "higher_is_concern"),
    best_flying_10yd = list(label = "Flying 10-yard", direction = "higher_is_concern"),
    max_velocity = list(label = "Max velocity", direction = "lower_is_concern")
  )

  sprint |>
    mutate(test_date = as.Date(test_date)) |>
    filter(!is.na(test_date)) |>
    group_by(athlete_id, athlete_name, role) |>
    group_modify(function(df, key) {
      df <- arrange(df, test_date)
      overall_latest_date <- df$test_date[[nrow(df)]]
      reasons <- character(0)
      trigger_count <- 0L; red_count <- 0L

      for (metric in names(metric_map)) {
        if (!(metric %in% names(df))) next
        metric_vals <- suppressWarnings(as.numeric(df[[metric]]))
        have_idx <- which(!is.na(metric_vals))
        if (length(have_idx) == 0) next
        current_idx <- have_idx[length(have_idx)]
        current <- metric_vals[current_idx]
        base_vals <- metric_vals[have_idx[-length(have_idx)]]
        if (length(base_vals) == 0) next
        base <- mean(base_vals, na.rm = TRUE)
        delta <- current - base
        pct <- if (!is.na(base) && base != 0) delta / base * 100 else NA_real_
        metric_cfg <- cfg[[metric]] %||% list()
        direction <- as.character(metric_cfg$direction %||% metric_map[[metric]]$direction)
        yellow <- as.numeric(metric_cfg$yellow_percent_change %||% if (direction == "higher_is_concern") 3 else -3)
        red <- as.numeric(metric_cfg$red_percent_change %||% if (direction == "higher_is_concern") 5 else -5)
        min_n <- as.integer(metric_cfg$min_baseline_n %||% 2)
        if (length(base_vals) < min_n) next
        level <- "Green"
        if (!is.na(pct)) {
          if (direction == "higher_is_concern") { if (pct >= red) level <- "Red" else if (pct >= yellow) level <- "Yellow" }
          else { if (pct <= red) level <- "Red" else if (pct <= yellow) level <- "Yellow" }
        }
        if (level != "Green") {
          trigger_count <- trigger_count + 1L
          if (level == "Red") red_count <- red_count + 1L
          reasons <- c(reasons, human_reason(metric_map[[metric]]$label, current, base, delta, pct,
                                              paste0("Yellow ", yellow, "%; Red ", red, "%"), direction, level))
        }
      }

      days_since <- as.numeric(difftime(Sys.Date(), overall_latest_date, units = "days"))
      confidence <- confidence_from_recency(days_since, max(0L, nrow(df) - 1L), thresholds)
      status <- dplyr::case_when(
        is.na(overall_latest_date) ~ "No Recent Data", red_count > 0L ~ "Red", trigger_count > 0L ~ "Yellow", TRUE ~ "Green"
      )
      data.frame(
        Sprint_Status = status, Sprint_Confidence = confidence,
        Sprint_Reason = if (length(reasons) > 0) paste(reasons, collapse = " ") else "No elevated SmartSpeed performance warning in the current monitoring window.",
        Sprint_Triggers = trigger_count, Sprint_Last_Date = overall_latest_date, Sprint_Baseline_N = max(0L, nrow(df) - 1L),
        stringsAsFactors = FALSE
      )
    }) |>
    ungroup()
}


# ============================================================
# Pitcher performance/injury-driver table shaping (CMJ-only)
# ============================================================
build_pitcher_perf_table <- function(df, multi_athlete = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  mdisp <- if ("metricName" %in% names(df)) "metricName" else get_metric_id_col(df)
  if (is.null(mdisp)) return(tibble())
  wide_val <- df %>%
    filter(.data[[mdisp]] %in% PITCHER_PERF_METRICS) %>%
    { if (multi_athlete) select(., profileId, athleteName, roleGroup, externalId, session_date, metric = all_of(mdisp), value = best_value)
      else select(., metric = all_of(mdisp), value = best_value) } %>%
    mutate(metric = factor(metric, levels = PITCHER_PERF_METRICS)) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)
  if (multi_athlete) {
    last_date <- df %>% distinct(profileId, session_date) %>% group_by(profileId) %>% summarise(`Last CMJ` = max(session_date, na.rm = TRUE), .groups = "drop")
    wide_val %>% left_join(last_date, by = "profileId") %>%
      transmute(Athlete = athleteName, Role = "Pitchers", `Last CMJ` = `Last CMJ`, !!!rlang::syms(PITCHER_PERF_METRICS))
  } else {
    wide_val %>% mutate(`Last CMJ` = suppressWarnings(max(df$session_date, na.rm = TRUE))) %>% select(`Last CMJ`, all_of(PITCHER_PERF_METRICS))
  }
}

build_pitcher_injury_table <- function(df, multi_athlete = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  mdisp <- if ("metricName" %in% names(df)) "metricName" else get_metric_id_col(df)
  if (is.null(mdisp)) return(tibble())
  wide_val <- df %>%
    filter(.data[[mdisp]] %in% PITCHER_INJURY_VALUE_METRICS) %>%
    { if (multi_athlete) select(., profileId, athleteName, roleGroup, externalId, session_date, metric = all_of(mdisp), value = best_value)
      else select(., metric = all_of(mdisp), value = best_value) } %>%
    mutate(metric = factor(metric, levels = PITCHER_INJURY_VALUE_METRICS)) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)
  wide_sd <- df %>%
    filter(.data[[mdisp]] %in% PITCHER_INJURY_SD_METRICS) %>%
    { if (multi_athlete) select(., profileId, metric = all_of(mdisp), sd = sd_value) else select(., metric = all_of(mdisp), sd = sd_value) } %>%
    mutate(metric_sd_name = case_when(metric == "Jump Height (Imp-Mom)" ~ "Jump Height SD", metric == "Peak Power" ~ "Peak Power SD", TRUE ~ paste0(metric, " SD"))) %>%
    { if (multi_athlete) select(., profileId, metric_sd_name, sd) else select(., metric_sd_name, sd) } %>%
    tidyr::pivot_wider(names_from = metric_sd_name, values_from = sd)
  if (multi_athlete) {
    last_date <- df %>% distinct(profileId, session_date) %>% group_by(profileId) %>% summarise(`Last CMJ` = max(session_date, na.rm = TRUE), .groups = "drop")
    wide_val %>% left_join(wide_sd, by = "profileId") %>% left_join(last_date, by = "profileId") %>%
      mutate(`Force/Impulse Asymmetry` = NA_real_) %>%
      transmute(Athlete = athleteName, Role = "Pitchers", `Last CMJ` = `Last CMJ`,
                !!!rlang::syms(PITCHER_INJURY_VALUE_METRICS), `Jump Height SD` = `Jump Height SD`, `Peak Power SD` = `Peak Power SD`,
                `Force/Impulse Asymmetry` = `Force/Impulse Asymmetry`)
  } else {
    wide_val %>% bind_cols(wide_sd) %>%
      mutate(`Force/Impulse Asymmetry` = NA_real_, `Last CMJ` = suppressWarnings(max(df$session_date, na.rm = TRUE))) %>%
      select(`Last CMJ`, all_of(PITCHER_INJURY_VALUE_METRICS), `Jump Height SD`, `Peak Power SD`, `Force/Impulse Asymmetry`)
  }
}

# ============================================================
# VALD Curve View helpers -- live API calls, need VALD_CLIENT_ID /
# VALD_CLIENT_SECRET / VALD_TEAM_ID in the environment.
# ============================================================
vald_token_url <- function() Sys.getenv("VALD_TOKEN_URL", "https://auth.prd.vald.com/oauth/token")
vald_get_token <- function() {
  missing_vars <- c("VALD_CLIENT_ID", "VALD_CLIENT_SECRET", "VALD_TEAM_ID")
  missing_vars <- missing_vars[!vapply(missing_vars, function(v) nzchar(Sys.getenv(v)), logical(1))]
  if (length(missing_vars) > 0) stop("Missing VALD credentials: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  resp <- httr::POST(
    vald_token_url(), httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
    body = list(grant_type = "client_credentials", audience = "vald-api-external",
                client_id = Sys.getenv("VALD_CLIENT_ID"), client_secret = Sys.getenv("VALD_CLIENT_SECRET")),
    encode = "form", httr::timeout(60)
  )
  if (httr::status_code(resp) != 200) stop("VALD token request failed (status ", httr::status_code(resp), "): ", httr::content(resp, "text", encoding = "UTF-8"), call. = FALSE)
  httr::content(resp)$access_token
}
fd_get_trials <- function(token, team_id, test_id) {
  url <- paste0(paste0("https://prd-", Sys.getenv("VALD_REGION", "use"), "-api-extforcedecks.valdperformance.com/v2019q3/teams/"), team_id, "/tests/", test_id, "/trials")
  resp <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token)), httr::timeout(60))
  if (httr::status_code(resp) != 200) return(NULL)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}
fd_get_recording <- function(token, team_id, test_id) {
  url <- paste0(paste0("https://prd-", Sys.getenv("VALD_REGION", "use"), "-api-extforcedecks.valdperformance.com/v2019q3/teams/"), team_id, "/tests/", test_id, "/recording?includeSampleData=true")
  resp <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token)), httr::timeout(60))
  if (httr::status_code(resp) != 200) return(NULL)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}
slice_curve_to_trial <- function(df_curve, start_s, end_s) df_curve %>% filter(time_s >= start_s, time_s <= end_s)
pick_best_trial_id <- function(trial_metrics, best_metric_key, limb = "Both") {
  trial_metrics %>% filter(metricKey == best_metric_key, trialLimb == limb) %>%
    group_by(trialId) %>% summarise(val = suppressWarnings(max(value, na.rm = TRUE)), .groups = "drop") %>%
    arrange(desc(val)) %>% slice(1) %>% pull(trialId)
}

# ============================================================
# UI
# ============================================================
custom_css <- tags$head(
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap"),
  tags$style(HTML("
  :root {
    --txst-maroon: #501214;
    --txst-gold: #F6BE00;
    --bg-page: #F5F6F7;
    --bg-card: #ffffff;
    --border-card: #e4e7eb;
    --text-heading: #1a1d21;
    --text-body: #344054;
    --text-muted: #667085;
    --status-normal-bg: #eaf6ee; --status-normal-fg: #226339;
    --status-perf-bg: #e8f0fe;   --status-perf-fg: #1d4ed8;
    --status-watch-bg: #fff2d9;  --status-watch-fg: #9a4d00;
    --status-action-bg: #feeceb; --status-action-fg: #8d1b13;
    --status-na-bg: #eef0f3;     --status-na-fg: #475467;
    --change-up-bg: #eaf6ee;   --change-up-fg: #226339;
    --change-down-bg: #feeceb; --change-down-fg: #8d1b13;
    --disclaimer-bg: #fbf3e6; --disclaimer-border: #e8cfa0; --disclaimer-fg: #6b4a1f;
  }
  [data-bs-theme='dark'] {
    --txst-maroon: #d98e90;
    --txst-gold: #e8c568;
    --bg-page: #14171c;
    --bg-card: #1c2128;
    --border-card: #333a45;
    --text-heading: #f2f3f5;
    --text-body: #d0d3d8;
    --text-muted: #9aa1ac;
    --status-normal-bg: #143523; --status-normal-fg: #6fd39a;
    --status-perf-bg: #1a2a4a;   --status-perf-fg: #8ab0f8;
    --status-watch-bg: #3a2a0c;  --status-watch-fg: #f0b969;
    --status-action-bg: #3a1614; --status-action-fg: #f19992;
    --status-na-bg: #262b33;     --status-na-fg: #aeb4bd;
    --change-up-bg: #143523;   --change-up-fg: #6fd39a;
    --change-down-bg: #3a1614; --change-down-fg: #f19992;
    --disclaimer-bg: #2a2210; --disclaimer-border: #5a4520; --disclaimer-fg: #e3c88f;
  }

  html, body { background: var(--bg-page) !important; color: var(--text-body) !important; font-family: 'Inter', -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif; }
  h1, h2, h3, h4, h5, h6 { font-weight: 850; letter-spacing: -0.035em; color: var(--text-heading); }

  .eyebrow, .kpi-title, .txst-label { text-transform: uppercase; font-weight: 800; letter-spacing: .07em; font-size: .7rem; color: var(--text-muted) !important; }

  .navbar, .bslib-page-navbar > .navbar, .navbar.navbar-dark { background-color: var(--txst-maroon) !important; }
  .navbar .nav-link, .navbar-brand, .navbar .form-check-label { color: rgba(255,255,255,.92) !important; }
  .navbar .nav-link.active { color: #ffffff !important; font-weight: 700; }

  .card, .bslib-card { background: var(--bg-card) !important; border: 1px solid var(--border-card) !important; border-radius: 14px !important; box-shadow: 0 1px 3px rgba(16,24,40,.06) !important; }
  .bslib-card .card-header { background: var(--bg-card) !important; border-bottom: 1px solid var(--border-card) !important; color: var(--text-heading) !important; font-weight: 800; }
  .bslib-card .card-body { background: var(--bg-card) !important; overflow: visible !important; color: var(--text-body) !important; }
  .subtle, .tiny { color: var(--text-muted) !important; }

  /* Metric card -- the .kpi tiles used throughout are styled as metric cards */
  .kpi { position: relative; background: var(--bg-card) !important; border: 1px solid var(--border-card) !important; border-radius: 14px; padding: 12px 14px 12px 18px; box-shadow: 0 1px 3px rgba(16,24,40,.06); overflow: hidden; }
  .kpi::before { content: ''; position: absolute; left: 0; top: 8px; bottom: 8px; width: 4px; border-radius: 0 4px 4px 0; background: var(--txst-gold); }
  .kpi-value { color: var(--text-heading) !important; font-size: 1.5rem; font-weight: 850; letter-spacing: -0.035em; margin-top: 2px; }
  .metric-detail { font-size: .78rem; color: var(--text-muted); margin-top: 4px; }

  /* Status pill -- the real Watch/Action judgment */
  .status-pill { display: inline-block; padding: 4px 11px; border-radius: 999px; font-weight: 800; font-size: .72rem; white-space: nowrap; }
  .status-pill-normal { background: var(--status-normal-bg); color: var(--status-normal-fg); }
  .status-pill-perf   { background: var(--status-perf-bg);   color: var(--status-perf-fg); }
  .status-pill-watch  { background: var(--status-watch-bg);  color: var(--status-watch-fg); }
  .status-pill-action { background: var(--status-action-bg); color: var(--status-action-fg); }
  .status-pill-na      { background: var(--status-na-bg);     color: var(--status-na-fg); }

  /* Coverage badge -- data-completeness only, never colored like a status pill */
  .coverage-badge { display: inline-flex; align-items: center; gap: 5px; padding: 3px 9px; border-radius: 999px; font-weight: 700; font-size: .68rem; background: transparent; border: 1px dashed var(--border-card); color: var(--text-muted); }
  .coverage-badge .dot { width: 6px; height: 6px; border-radius: 50%; background: var(--text-muted); }
  .coverage-badge.covered .dot { background: var(--txst-gold); }
  .coverage-badge.covered { color: var(--text-heading); }

  /* Stat-check badge -- 'is this number statistically meaningful' */
  .stat-check-badge { display: inline-flex; align-items: center; gap: 5px; padding: 3px 9px; border-radius: 999px; font-weight: 700; font-size: .68rem; background: transparent; border: 1px dashed var(--border-card); color: var(--text-muted); }
  .stat-check-badge .dot { width: 6px; height: 6px; border-radius: 50%; background: var(--text-muted); }
  .stat-check-badge.confirmed { border-style: solid; border-color: var(--status-normal-fg); color: var(--status-normal-fg); }
  .stat-check-badge.confirmed .dot { background: var(--status-normal-fg); }

  /* Change pill -- compact +/-X.X% inline in tables */
  .change-pill { display: inline-block; padding: 2px 8px; border-radius: 999px; font-weight: 800; font-size: .72rem; }
  .change-pill-up   { background: var(--change-up-bg);   color: var(--change-up-fg); }
  .change-pill-down { background: var(--change-down-bg); color: var(--change-down-fg); }

  /* Sparkline */
  .cmj-sparkline-svg polyline { fill: none; stroke-width: 2.4; stroke-linecap: round; stroke-linejoin: round; }
  .cmj-sparkline-svg.spark-up polyline { stroke: var(--status-normal-fg); }
  .cmj-sparkline-svg.spark-down polyline { stroke: var(--status-action-fg); }

  /* CMJ Monitoring: individual readiness trends table */
  .cmj-sparkline-table { width: 100%; border-collapse: collapse; font-size: .82rem; }
  .cmj-sparkline-table th { text-align: left; padding: 6px 10px; border-bottom: 2px solid var(--border-card); color: var(--text-muted); font-weight: 700; }
  .cmj-sparkline-table td { padding: 6px 10px; border-bottom: 1px solid var(--border-card); white-space: nowrap; }
  .cmj-sparkline-table td.trend-up { color: var(--status-normal-fg); font-weight: 700; }
  .cmj-sparkline-table td.trend-down { color: var(--status-action-fg); font-weight: 700; }
  .cmj-sparkline-table td.trend-flat { color: var(--text-muted); }

  /* Disclaimer box */
  .disclaimer-box { background: var(--disclaimer-bg); border: 1px solid var(--disclaimer-border); color: var(--disclaimer-fg) !important; border-radius: 10px; padding: 10px 14px; font-size: .8rem; }

  /* Athlete hero -- fixed rich gradient regardless of theme */
  .athlete-hero { border-radius: 16px; padding: 20px 24px; background: linear-gradient(120deg, #501214, #7a1f22); color: #fff; display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 14px; margin-bottom: 4px; }
  .athlete-hero .name { font-size: 1.6rem; font-weight: 850; letter-spacing: -0.035em; color: #fff; }
  .athlete-hero .meta { font-size: .85rem; color: rgba(255,255,255,.75); margin-top: 2px; }

  /* Brand lockup in the navbar */
  .brand-mark { width: 34px; height: 34px; border: 2px solid rgba(255,255,255,.55); border-radius: 8px; display: flex; align-items: center; justify-content: center; font-weight: 900; font-size: .8rem; color: #fff; flex: none; }
  .brand-lockup { display: flex; align-items: center; gap: 10px; }
  .brand-text-primary { font-weight: 800; color: #fff; font-size: .95rem; line-height: 1.15; }
  .brand-text-secondary { font-size: .66rem; color: rgba(255,255,255,.7); text-transform: uppercase; letter-spacing: .07em; line-height: 1.2; }

  /* Chart card responsive heights */
  .chart-card-xl .js-plotly-plot, .chart-card-xl .plotly { height: clamp(420px, 55vh, 640px) !important; }
  .chart-card-lg .js-plotly-plot, .chart-card-lg .plotly { height: clamp(340px, 45vh, 480px) !important; }
  .chart-card-md .js-plotly-plot, .chart-card-md .plotly { height: clamp(260px, 35vh, 360px) !important; }
  .chart-card-sm .js-plotly-plot, .chart-card-sm .plotly { height: clamp(180px, 25vh, 240px) !important; }

  /* Tables -- both DT widgets and plain renderTable()/Bootstrap tables get
     the same maroon-header / gold-underline / no-vertical-gridline look */
  table.dataTable thead th, table thead th { color: var(--txst-maroon) !important; background: var(--bg-card) !important; border-bottom: 2px solid var(--txst-gold) !important; font-weight: 800; }
  table.dataTable.stripe tbody tr.odd, table.dataTable tbody tr:nth-child(odd), table.table-striped tbody tr:nth-child(odd) { background-color: rgba(127,127,127,.04) !important; }
  table.dataTable, table.dataTable td, table.dataTable th, table td, table th { border-right: none !important; border-left: none !important; }
  table, table td { color: var(--text-body); }

  .curve-panel { border: 1px solid var(--border-card); border-radius: 14px; background: var(--bg-card); padding: 12px; margin-top: 10px; }
  .curve-title { font-size: 15px; font-weight: 800; color: var(--txst-maroon); margin-bottom: 6px; }
  .curve-divider { border: none; border-top: 1px solid var(--border-card); margin: 10px 0; }
"))
)

# ------------------------------------------------------------
# Component helpers -- status pill / coverage badge / stat-check badge /
# change pill / sparkline / disclaimer box / athlete hero. These render the
# CSS classes above; the app's own status vocabulary (Red/Yellow/Green/
# Blue/No Recent Data) maps onto the four pill colors rather than
# being replaced -- semantics are unchanged, only the visual treatment is.
# ------------------------------------------------------------
status_pill_ui <- function(status) {
  status <- as.character(status %||% "No Recent Data")
  cls <- switch(status,
    "Green" = "status-pill-normal",
    "Blue" = "status-pill-perf",
    "Yellow" = "status-pill-watch",
    "Red" = "status-pill-action",
    "status-pill-na"
  )
  tags$span(class = paste("status-pill", cls), status)
}
status_pill_html <- function(status) as.character(status_pill_ui(status))

coverage_badge_ui <- function(confidence) {
  confidence <- as.character(confidence %||% "No Recent Data")
  covered <- confidence %in% c("High", "Moderate")
  tags$span(
    class = paste("coverage-badge", if (covered) "covered" else ""),
    tags$span(class = "dot"), confidence
  )
}
coverage_badge_html <- function(confidence) as.character(coverage_badge_ui(confidence))

stat_check_badge_ui <- function(n, min_n = 3, label_ok = "Baseline OK", label_low = "Low baseline n") {
  ok <- isTRUE(!is.na(n) && n >= min_n)
  tags$span(
    class = paste("stat-check-badge", if (ok) "confirmed" else ""),
    tags$span(class = "dot"), if (ok) label_ok else label_low
  )
}

change_pill_ui <- function(pct, digits = 1) {
  if (is.null(pct) || is.na(pct)) return(tags$span(class = "change-pill", style = "background:var(--status-na-bg);color:var(--status-na-fg);", "N/A"))
  up <- pct >= 0
  tags$span(class = paste("change-pill", if (up) "change-pill-up" else "change-pill-down"),
            paste0(if (up) "+" else "", formatC(pct, digits = digits, format = "f"), "%"))
}
change_pill_html <- function(pct, digits = 1) as.character(change_pill_ui(pct, digits))

sparkline_svg_html <- function(values, width = 84, height = 28) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values)]
  if (length(values) < 2) return("")
  rng <- range(values)
  span <- if (diff(rng) == 0) 1 else diff(rng)
  n <- length(values)
  xs <- seq(2, width - 2, length.out = n)
  ys <- height - 2 - (values - rng[1]) / span * (height - 4)
  pts <- paste(round(xs, 1), round(ys, 1), sep = ",", collapse = " ")
  dir_cls <- if (values[n] >= values[1]) "spark-up" else "spark-down"
  sprintf("<svg class='cmj-sparkline-svg %s' width='%d' height='%d' viewBox='0 0 %d %d'><polyline points='%s'/></svg>",
          dir_cls, width, height, width, height, pts)
}

disclaimer_box_ui <- function(text = "Monitoring support only -- these statuses flag signals for staff review and are not a diagnosis or a prediction of injury.") {
  tags$div(class = "disclaimer-box", tags$strong("Note: "), text)
}

athlete_hero_ui <- function(name, meta_line, status) {
  tags$div(class = "athlete-hero",
    tags$div(tags$div(class = "name", name), tags$div(class = "meta", meta_line)),
    status_pill_ui(status)
  )
}

# ============================================================
# Shared data store -- loaded ONCE per R process, not once per browser session
# ============================================================
# The data used to be (re-)read synchronously inside server(), which runs
# once per new session AND once per click of "Reload data files" (~9.5s
# each, since these are multi-megabyte RDS files). Shiny processes one
# session's work at a time by default, so concurrent users queue up strictly
# behind each other: stress testing found the 4th of 4 people opening the
# dashboard around the same time waited over 45 seconds just to see data,
# and the 4th of 4 simultaneous "Reload data files" clicks waited nearly 34
# seconds. Loading once here, before shinyApp() is even called, means every
# session shares the same in-memory data.frames with zero per-session load
# cost -- only an explicit reload still pays the read cost, and it updates
# one shared store every session reads from, rather than each session
# maintaining (and separately paying for) its own copy. Concurrent reload
# clicks still serialize -- Shiny has no async execution here -- but that's
# now a rare, deliberate action instead of something every page load pays.
shared_data <- new.env()

resolve_data_paths <- function() {
  list(
    roster = dashboard_data_path("roster_baseball.rds"),
    summary = dashboard_data_path("force_sessions_summary_baseball.rds"),
    sprint = dashboard_data_path("sprint_session_level.rds"),
    tests = dashboard_data_path("force_tests_baseball_all_history_named.rds"),
    trials_long = {
      enriched <- dashboard_data_path("force_metrics_long_all_history_baseball_enriched.rds")
      raw <- dashboard_data_path("force_metrics_long_all_history_baseball.rds")
      if (file.exists(enriched)) enriched else raw
    }
  )
}

load_shared_data <- function() {
  paths <- resolve_data_paths()
  shared_data$paths <- paths
  shared_data$roster <- ensure_cols(safe_read_rds(paths$roster))
  shared_data$session_summary <- ensure_cols(safe_read_rds(paths$summary))
  shared_data$sprint <- safe_read_rds(paths$sprint)
  shared_data$tests <- ensure_cols(safe_read_rds(paths$tests))
  shared_data$trials_long <- safe_read_rds(paths$trials_long)

  missing <- c()
  if (!file.exists(paths$roster)) missing <- c(missing, "roster_baseball.rds")
  if (!file.exists(paths$summary)) missing <- c(missing, "force_sessions_summary_baseball.rds")
  shared_data$status <- if (length(missing) == 0) {
    paste0("Player Health data loaded at ", format(Sys.time(), "%H:%M:%S"))
  } else {
    paste0("Missing required file(s): ", paste(missing, collapse = ", "), ". Set CMJ_SPRINT_SHARE_GOLD_DIR / CMJ_SPRINT_SHARE_LEGACY_DIR or copy files into ./data/gold or ./Data.")
  }
  invisible(shared_data)
}
# Data is loaded by refresh_runtime.R from a complete published snapshot.

# ------------------------------------------------------------
# Live VALD refresh (optional) -- CMJ/ForceDecks + SmartSpeed only.
# ------------------------------------------------------------
# "Refresh data" calls run_live_vald_refresh() to pull new tests from VALD
# and rebuild the gold files, THEN load_shared_data() re-reads them -- so
# staff can check same-day CMJ/sprint results on demand instead of waiting
# on a separate scheduled pipeline run. ArmCare and Trackman are never
# touched: ArmCare's pull is off by default in 08_refresh_forcedecks_
# baseball.R, and this block forces that explicitly regardless of any
# stray env var on the host; Trackman was never part of this pipeline.
#
# This needs the pipeline files this app is handed off alongside (not just
# this one file): scripts/01_config.R, 06-08, and a handful of R/*.R
# helpers. If they aren't present next to this file, or VALD credentials
# aren't set, live refresh is simply unavailable and the button falls back
# to today's behavior (re-read whatever's already on disk) -- nothing here
# breaks a bare copy of just this dashboard file.
# scripts/06-08 are NOT sourced here at startup, on purpose: they are
# top-level pipeline scripts that DO live work (call the VALD API, etc.) the
# instant they're sourced, not just function definitions -- 08 sources 06/07
# itself, but only when refresh_vald_forcedecks() actually runs, i.e. only
# when a coach clicks Refresh. Sourcing them here would mean every app
# *start* pulls from VALD, not just every Refresh click. Only pull in the
# files below now (pure function/constant definitions, safe to load eagerly);
# their existence, plus 06-08 existing on disk for refresh_vald_forcedecks()
# to source later, is what live_refresh_available checks.
PIPELINE_DEFINITION_FILES <- c(
  "scripts/01_config.R", "R/utils_dates.R", "R/utils_validation.R", "R/api_vald.R",
  "R/clean_forcedecks.R", "R/clean_smartspeed.R", "R/import_smartspeed.R"
)
PIPELINE_RUNTIME_FILES <- c(
  "scripts/06_pull_forcedecks_tests_all_history.R",
  "scripts/07_pull_forcedecks_metrics_all_history_baseball.R", "scripts/08_refresh_forcedecks_baseball.R"
)
# This file was originally extracted from the same codebase as the pipeline
# files above, so several names -- %||%, safe_read_rds, ensure_cols,
# standardize_joined_names, cfg_get, vald_get_token, vald_token_url, TXST --
# are also (re)defined by them, mostly as byte-identical copies (harmless).
# Three are NOT harmless: load_thresholds(), dashboard_data_path(), and
# assign_season_phase() have pipeline versions that read config/thresholds.yml,
# this file's own DATA_DIR/gold_data_dir() (ignoring this share copy's
# GOLD_DIR/LEGACY_DIR env-var overrides), and load_season_phases()
# respectively -- none of which exist in this share copy's handoff, and the
# scoring functions that call load_thresholds()/assign_season_phase() do so
# as a lazy default argument, so whichever definition is bound at CALL time
# wins, not definition time. Rather than re-asserting each one by name (and
# risk missing the next one some future edit introduces), snapshot every
# colliding name's binding now and restore the whole snapshot after sourcing.
SHARE_OWN_FN_NAMES <- c(
  "%||%", "safe_read_rds", "ensure_cols", "standardize_joined_names", "cfg_get",
  "load_thresholds", "dashboard_data_path", "assign_season_phase",
  "vald_get_token", "vald_token_url", "TXST"
)
PLAYER_HEALTH_SOURCE_ENV <- environment()
SHARE_OWN_FN_NAMES <- SHARE_OWN_FN_NAMES[vapply(
  SHARE_OWN_FN_NAMES,
  exists,
  logical(1),
  envir = PLAYER_HEALTH_SOURCE_ENV,
  inherits = FALSE
)]
SHARE_OWN_FN_SNAPSHOT <- mget(SHARE_OWN_FN_NAMES, envir = PLAYER_HEALTH_SOURCE_ENV)

live_refresh_available <- all(file.exists(c(PIPELINE_DEFINITION_FILES, PIPELINE_RUNTIME_FILES)))
if (live_refresh_available) {
  tryCatch({
    for (f in PIPELINE_DEFINITION_FILES) {
      sys.source(f, envir = PLAYER_HEALTH_SOURCE_ENV, chdir = TRUE, keep.source = FALSE)
    }
    list2env(SHARE_OWN_FN_SNAPSHOT, envir = PLAYER_HEALTH_SOURCE_ENV)
  }, error = function(e) {
    message("[Live VALD refresh] Failed to load pipeline files -- Refresh will fall back to local-file reload only. Reason: ", conditionMessage(e))
    live_refresh_available <<- FALSE
  })
}

run_live_vald_refresh <- function() {
  cred_status <- vald_credentials_status()
  if (!isTRUE(cred_status$ok)) return(list(ok = FALSE, message = cred_status$message))

  # Belt-and-suspenders: force ArmCare off for this call no matter what the
  # host environment has set, and restore whatever was there afterward.
  old_env <- Sys.getenv(c("ARMCARE_RUN_PULL", "ARMCARE_RUN_CSV_IMPORT"), unset = NA, names = TRUE)
  Sys.setenv(ARMCARE_RUN_PULL = "false", ARMCARE_RUN_CSV_IMPORT = "false")
  on.exit({
    for (nm in names(old_env)) {
      if (is.na(old_env[[nm]])) Sys.unsetenv(nm) else Sys.setenv(structure(old_env[[nm]], names = nm))
    }
  }, add = TRUE)

  steps_ok <- c()
  steps_failed <- c()

  tryCatch({
    refresh_vald_forcedecks()
    clean_forcedecks_dashboard_objects()
    steps_ok <- c(steps_ok, "CMJ/ForceDecks")
  }, error = function(e) steps_failed <<- c(steps_failed, paste0("CMJ/ForceDecks (", conditionMessage(e), ")")))

  tryCatch({
    sprint_result <- import_smartspeed_api(save_raw = TRUE, build_gold = TRUE)
    if (!isTRUE(sprint_result$ok)) stop(sprint_result$message)
    steps_ok <- c(steps_ok, "SmartSpeed")
  }, error = function(e) steps_failed <<- c(steps_failed, paste0("SmartSpeed (", conditionMessage(e), ")")))

  # Defensive: make sure whatever the pipeline just wrote actually landed in
  # THIS app's configured GOLD_DIR/LEGACY_DIR, even if a custom
  # CMJ_SPRINT_SHARE_GOLD_DIR/CMJ_SPRINT_SHARE_LEGACY_DIR made them diverge
  # from the pipeline's own default <project root>/data/gold and .../Data.
  tryCatch({
    pipeline_gold <- gold_data_dir()
    if (!identical(normalizePath(pipeline_gold, mustWork = FALSE), normalizePath(GOLD_DIR, mustWork = FALSE))) {
      for (f in list.files(pipeline_gold, pattern = "\\.(rds|csv)$", full.names = FALSE)) {
        file.copy(file.path(pipeline_gold, f), file.path(GOLD_DIR, f), overwrite = TRUE)
      }
    }
  }, error = function(e) NULL)

  list(
    ok = length(steps_failed) == 0 && length(steps_ok) > 0,
    forcedecks_ok = "CMJ/ForceDecks" %in% steps_ok,
    message = paste0(
      if (length(steps_ok) > 0) paste0("Pulled: ", paste(steps_ok, collapse = ", "), ". ") else "",
      if (length(steps_failed) > 0) paste0("Failed: ", paste(steps_failed, collapse = "; ")) else ""
    )
  )
}

source("runtime/refresh_runtime.R", local = TRUE)

brand_lockup <- tags$div(class = "brand-lockup",
  tags$div(class = "brand-mark", "TX"),
  tags$div(
    tags$div(class = "brand-text-primary", "Texas State Baseball"),
    tags$div(class = "brand-text-secondary", "CMJ + Sprint (Share Copy)")
  )
)

player_health_dashboard_content <- tagList(
    if (!BASE_PLAYER_HEALTH_EMBEDDED) custom_css,
    fluidRow(
      column(8, tags$div(class = "tiny subtle", uiOutput("data_status_ui"))),
      column(4, disclaimer_box_ui())
    ),

  navset_tab(
    id = "main_nav",
  nav_panel(
    tagList(icon("bell"), "Alert Inbox"),
    tags$h2("Alert Inbox"),
    tags$div(class = "tiny subtle", style = "margin-bottom:10px;", "Athletes currently flagged by CMJ status, with the reason and confidence behind each flag."),
    uiOutput("alert_inbox_kpis"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters", width = 300, open = "desktop",
        selectInput("alert_role_filter", "Role", choices = c("Pitchers", "Hitters"), selected = c("Pitchers", "Hitters"), multiple = TRUE),
        selectInput("alert_status_filter", "Status", choices = c("Red", "Yellow", "Blue", "Green", "No Recent Data"), selected = c("Red", "Yellow", "Blue", "Green", "No Recent Data"), multiple = TRUE),
        selectInput("alert_confidence_filter", "Confidence", choices = c("High", "Moderate", "Low", "No Recent Data"), selected = c("High", "Moderate", "Low", "No Recent Data"), multiple = TRUE),
        selectInput("alert_window_days", "Date window", choices = c("7 days" = 7, "14 days" = 14, "28 days" = 28, "Season" = 365), selected = 28),
        textInput("alert_search", "Search athlete", value = "", placeholder = "Name or externalId"),
        checkboxInput("alert_triggered_only", "Triggered only (Red/Yellow)", value = TRUE),
        downloadButton("alert_inbox_csv", "CSV")
      ),
      fluidRow(
        column(8, uiOutput("alert_inbox_dt")),
        column(4, card(card_header(tags$b("Why Flagged")), card_body(
          uiOutput("alert_detail_ui"),
          tags$div(style = "margin-top:10px;", actionButton("alert_open_profile", "Open Athlete Profile"))
        )))
      )
    )
  ),

  nav_panel(
    tagList(icon("users"), "Team Overview"),
    tags$h2("Team Overview"),
    tags$div(class = "tiny subtle", style = "margin-bottom:10px;", "Roster snapshot, coach export pack, and the most recent CMJ preset for every active athlete."),
    layout_columns(
      col_widths = c(1, 11),
      card(card_body(selectInput("team_group", NULL, choices = c("All", "Pitchers", "Hitters"), selected = "All", width = "100%"))),
      card(card_header(tags$b("Team snapshot")), card_body(uiOutput("team_kpis")))
    ),
    card(card_header(tags$b("Coach Export Pack")), card_body(
      tags$div(class = "tiny subtle", "Status-first HTML exports from current filters."),
      tags$div(style = "margin-top:10px;", downloadButton("team_export_team_report_html", "Download Team Report (HTML)")),
      tags$div(style = "margin-top:8px;", downloadButton("team_export_pitcher_report_html", "Download Pitcher Report (HTML)")),
      tags$div(style = "margin-top:8px;", downloadButton("team_export_hitter_report_html", "Download Hitter Report (HTML)"))
    )),
    card(
      card_header(tags$b("General CMJ preset (All athletes | most recent CMJ)")),
      card_body(tags$div(style = "overflow-x:auto;", tableOutput("team_general_cmj_dt"))),
      card_footer(tags$span(class = "tiny subtle", "Includes derived Peak Power / BW (lb) = Peak Power / Bodyweight in Pounds."))
    ),
    uiOutput("team_pitcher_addon_ui"),
    card(class = "chart-card-lg", card_header(tags$b("Team bar chart (most recent CMJ)")), card_body(
      uiOutput("team_bar_metric_ui"), plotlyOutput("team_bar_plot", height = "420px")
    ))
  ),

  nav_panel(
    tagList(icon("id-badge"), "Athlete Profile"),
    tags$h2("Athlete Profile"),
    tags$div(class = "tiny subtle", style = "margin-bottom:10px;", "Single-athlete status, longitudinal trends, and sprint history."),
    uiOutput("profile_athlete_ui"),
    uiOutput("profile_header_ui"),
    uiOutput("profile_summary_cards"),
    card(class = "chart-card-md", card_header(tags$b("CMJ Longitudinal Trends")), card_body(
      uiOutput("profile_trend_metrics_ui"),
      plotlyOutput("profile_cmj_trends", height = "360px")
    )),
    uiOutput("profile_sprint_ui"),
    card(card_header(tags$b("Current CMJ Status")), card_body(tags$div(style = "overflow-x:auto;", tableOutput("profile_status_dt")))),
    card(card_header(tags$b("Curve View")), card_body(
      tags$div(class = "tiny subtle", "Open this athlete in Curve View for force-time trace inspection."),
      actionButton("profile_open_curve", "Open Curve View")
    ))
  ),

  nav_panel(
    tagList(icon("chart-line"), "CMJ Monitoring"),
    tags$h2("CMJ Monitoring"),
    tags$div(class = "tiny subtle", style = "margin-bottom:10px;",
             "Team-level CMJ trends and data coverage. For who needs attention right now and why, see Alert Inbox -- this page is about the bigger picture over time."),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters", width = 280, open = "desktop",
        selectInput("cmj_monitor_role", "Role", choices = c("All", "Pitchers", "Hitters"), selected = "All"),
        selectInput("cmj_window_days", "Status window (for coverage + distribution)", choices = c("Last 7 days" = 7, "Last 14 days" = 14, "Last 28 days" = 28), selected = 28),
        downloadButton("cmj_report_csv", "Download CMJ Report CSV")
      ),
      card(card_header(tags$b("Team Status Distribution")), card_body(
        tags$div(class = "tiny subtle", style = "margin-bottom:8px;",
                 "How many athletes are in each CMJ status right now, by role. Red/Yellow means Alert Inbox has a reason worth reading; Blue means performance is trending up; Green means no current warning."),
        checkboxInput("viz_show_no_recent", "Include No Recent Data", value = FALSE),
        plotlyOutput("status_distribution_plot", height = "240px")
      )),
      card(class = "chart-card-sm", card_header(tags$b("Team Trend (last 60 days)")), card_body(
        tags$div(class = "tiny subtle", style = "margin-bottom:8px;",
                 "Team-average CMJ composite: each athlete's Jump Height, RSI-modified, Concentric Impulse, and Contraction Time converted to a z-score against THEIR OWN history, averaged into one number per athlete per day, then averaged again across the team. A rising line means the team is collectively producing more force/power than usual; a falling line can mean accumulated fatigue -- it is not a per-athlete diagnosis."),
        plotlyOutput("cmj_team_trend_plot", height = "260px")
      )),
      card(card_header(tags$b("Data Coverage")), card_body(
        tags$div(class = "tiny subtle", style = "margin-bottom:8px;",
                 "How much recent testing backs the statuses shown elsewhere. A lot of Low/No Recent Data means the team's flags are running on thin history right now, not that everyone is actually fine."),
        plotlyOutput("cmj_coverage_plot", height = "200px")
      )),
      card(card_header(tags$b("Individual Readiness Trends")), card_body(
        tags$div(class = "tiny subtle", style = "margin-bottom:8px;",
                 "Each athlete's composite trend at a glance. The sparkline shows the shape of their last 28 days; the delta columns show how much the composite moved over each window (positive = trending up)."),
        tags$div(style = "overflow-x:auto;", uiOutput("composite_sparklines_dt"))
      ))
    )
  ),

  nav_panel(
    tagList(icon("person-running"), "Sprint / SmartSpeed"),
    tags$h2("Sprint / SmartSpeed"),
    tags$div(class = "tiny subtle", style = "margin-bottom:10px;", "Timing-gate performance domain -- sprint flags indicate performance change, not injury risk."),
    uiOutput("sprint_summary_ui"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters", width = 280, open = "desktop",
        selectInput("sprint_window_days", "Date window", choices = c("Last 14 days" = 14, "Last 28 days" = 28, "Season" = 365), selected = 28),
        selectInput("sprint_role_filter", "Role", choices = c("All", "Pitchers", "Hitters"), selected = "All"),
        selectInput("sprint_metric", "Sprint metric", choices = c("Best 10-yard" = "best_10yd", "Best 30-yard" = "best_30yd", "Flying 10-yard" = "best_flying_10yd", "Max velocity" = "max_velocity"), selected = "best_10yd"),
        textInput("sprint_search", "Search athlete", value = "", placeholder = "Name or ID")
      ),
      card(card_header(tags$b("Latest Sprint Testing")), card_body(tags$div(style = "overflow-x:auto;", tableOutput("sprint_latest_dt")))),
      card(class = "chart-card-md", card_header(tags$b("Sprint Trend (Best 10-yard)")), card_body(
        tags$div(class = "tiny subtle", style = "margin-bottom:8px;", "Team average shown by default -- add specific players to compare against it."),
        uiOutput("sprint_trend_add_athlete_ui"),
        plotlyOutput("sprint_trend_plot", height = "320px")
      )),
      card(card_header(tags$b("Top Sprint Movers")), card_body(tags$div(style = "overflow-x:auto;", tableOutput("sprint_movers_dt"))))
    )
  ),

  nav_panel(
    tagList(icon("wave-square"), "Curve View (Force Tracing)"),
    tags$div(
      fluidRow(
        column(4, tags$div(class = "curve-panel",
          tags$div(class = "curve-title", "Compare Setup"),
          fluidRow(column(6, uiOutput("curve_athlete_ui")), column(6, uiOutput("curve_testtype_ui"))),
          fluidRow(column(6, uiOutput("curve_metric_ui")), column(6, selectInput("curve_session_compare_mode", "Compare mode", choices = c("Single session" = "single", "Two trials (same session)" = "trials", "Two sessions" = "sessions"), selected = "single"))),
          fluidRow(
            column(4, selectInput("curve_limb", "Curve limb", choices = c("Total (Left+Right)" = "total", "Left" = "left", "Right" = "right"), selected = "total")),
            column(4, checkboxInput("curve_show_multiples", "Show all limbs", value = FALSE)),
            column(4, checkboxInput("curve_normalize_force", "Normalize to % peak", value = FALSE))
          ),
          checkboxInput("curve_shared_y", "Shared Y-axis", value = TRUE)
        )),
        column(4, tags$div(
          tags$div(class = "curve-panel",
            tags$div(class = "curve-title", "Rep Controls"),
            selectInput("curve_rep_rule_A", "Curve A rep", choices = c("Best rep (by selected metric)" = "best", "Choose rep manually" = "manual"), selected = "best"),
            uiOutput("curve_rep_A_ui"),
            tags$hr(class = "curve-divider"),
            tags$div(class = "curve-title", "Curve B Controls"),
            uiOutput("curve_B_controls")
          ),
          tags$div(class = "curve-panel",
            tags$div(class = "curve-title", "Session Timeline"),
            tags$div(class = "tiny subtle", "Click rows to set A/B. In Two sessions mode, choose whether your click assigns A or B."),
            uiOutput("curve_assign_ui"),
            uiOutput("curve_session_timeline")
          )
        )),
        column(4, tags$div(class = "curve-panel",
          tags$div(class = "curve-title", "Selection & Checks"),
          uiOutput("curve_selected_summary"),
          fluidRow(column(6, actionButton("curve_swap_ab", "Swap A/B")), column(6, actionButton("curve_clear_b", "Clear B"))),
          tags$hr(class = "curve-divider"),
          tags$div(class = "curve-title", "Quality Checks"), uiOutput("curve_quality_ui"),
          tags$hr(class = "curve-divider"),
          tags$div(class = "curve-title", "Delta Strip"), uiOutput("curve_delta_ui"),
          tags$div(style = "margin-top:10px;", downloadButton("curve_snapshot_csv", "Download comparison snapshot"))
        ))
      ),
      fluidRow(
        column(12, plotOutput("curve_plot", height = "560px")),
        column(12, plotOutput("curve_metric_bar", height = "300px"))
      )
    )
  )
  ) # end navset_tab(id = "main_nav", ...)
)

ui <- if (BASE_PLAYER_HEALTH_EMBEDDED) {
  tags$div(class = "base-player-health-embedded", player_health_dashboard_content)
} else {
  page_navbar(
    title = brand_lockup,
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    navbar_options = navbar_options(bg = TXST$maroon, theme = "dark"),
    fluid = TRUE,
    nav_spacer(),
    nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
    nav_item(actionButton("reload_data", "Refresh data", icon = icon("arrows-rotate"), class = "btn-sm")),
    # Keep the standalone app's proven outer-navbar/inner-tab nesting. BASE
    # sets BASE_PLAYER_HEALTH_EMBEDDED and receives only the inner dashboard.
    nav_panel("Dashboard", player_health_dashboard_content)
  )
}

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # ---- Data access (read-only; reads from the process-wide shared_data
  # store populated by load_shared_data() at startup -- see that function's
  # comment above for why this isn't per-session). reload_trigger is
  # session-local: it exists so THIS session's outputs re-evaluate right
  # after THIS session clicks "Reload data files"; the shared_data content
  # itself is process-wide, so every session's next natural re-render (a tab
  # switch, a filter change) picks up a fresh reload too, even without
  # clicking the button themselves. ----
  reload_trigger <- reactiveVal(0)
  fd_roster <- reactive({ reload_trigger(); shared_data$roster })
  fd_session_summary <- reactive({ reload_trigger(); shared_data$session_summary })
  fd_sprint <- reactive({ reload_trigger(); shared_data$sprint })
  fd_tests <- reactive({ reload_trigger(); shared_data$tests })
  fd_trials_long <- reactive({ reload_trigger(); shared_data$trials_long })

  observe({
    invalidateLater(2000, session)
    poll_refresh()
    reload_trigger(shared_revision())
  })
  observeEvent(input$reload_data, {
    msg <- start_refresh(force = TRUE)
    showNotification(msg, type = "message", duration = 6)
  }, ignoreInit = TRUE)
  output$data_status_ui <- renderUI({
    reload_trigger(); refresh_message()
    tags$span(paste(shared_data$status, refresh_message(), sep = " | "))
  })

  team_id <- Sys.getenv("VALD_TEAM_ID")

  # ---- Roster ----
  # Empty-but-correctly-columned fallback: a bare tibble() (0 columns) here
  # used to propagate downstream into code that unconditionally references
  # roleGroup/athleteName/etc (e.g. `filter(roleGroup == "Pitchers")`),
  # crashing with "object 'roleGroup' not found" instead of just showing an
  # empty roster -- e.g. when roster_baseball.rds hasn't been copied into
  # place yet. Every early-return below preserves this schema.
  empty_roster_tbl <- tibble(profileId = character(0), athleteName = character(0), externalId = character(0),
                              primaryGroup = character(0), roleGroup = character(0))

  roster_tbl <- reactive({
    r <- fd_roster()
    if (is.null(r) || nrow(r) == 0) return(empty_roster_tbl)
    r %>% mutate(
      profileId = as.character(profileId %||% ""), athleteName = as.character(athleteName %||% ""),
      externalId = as.character(externalId %||% ""), primaryGroup = as.character(primaryGroup %||% "Other"),
      roleGroup = ifelse(primaryGroup == "Pitchers", "Pitchers", "Hitters")
    ) %>% filter(nzchar(profileId)) %>% distinct(profileId, .keep_all = TRUE)
  })

  season_roster_tbl <- reactive({
    r <- roster_tbl(); s <- fd_session_summary()
    if (is.null(r) || nrow(r) == 0) return(empty_roster_tbl)
    if (is.null(s) || nrow(s) == 0) return(r %>% filter(FALSE))
    dc <- get_date_col(s); if (is.null(dc)) return(r %>% filter(FALSE))
    in_season_ids <- s %>% mutate(session_date__ = as_date_safely(.data[[dc]])) %>%
      filter(!is.na(session_date__), session_date__ >= ACTIVE_CMJ_CUTOFF) %>% distinct(profileId) %>% pull(profileId) %>% as.character()
    r %>% filter(profileId %in% in_season_ids)
  })

  athlete_is_pitcher <- function(profile_id) {
    r <- roster_tbl()
    if (is.null(r) || nrow(r) == 0 || is.null(profile_id) || !nzchar(profile_id)) return(FALSE)
    row <- r %>% filter(profileId == profile_id) %>% slice(1)
    if (nrow(row) == 0) return(FALSE)
    identical(as.character(row$primaryGroup[[1]]), "Pitchers")
  }

  athlete_choices <- reactive({
    r <- season_roster_tbl()
    if (!is.null(r) && nrow(r) > 0) {
      return(r %>% transmute(profileId, athleteName = ifelse(is.na(athleteName) | !nzchar(athleteName), "(Unknown name)", athleteName),
                              externalId = ifelse(is.na(externalId), "", externalId),
                              label = ifelse(nzchar(externalId), paste0(athleteName, " (", externalId, ")"), athleteName)) %>% arrange(athleteName))
    }
    tibble(profileId = character(0), athleteName = character(0), externalId = character(0), label = character(0))
  })

  athlete_latest_dates <- reactive({
    s <- fd_session_summary()
    if (is.null(s) || nrow(s) == 0) return(tibble(profileId = character(0), latest_date = as.Date(character(0))))
    dc <- get_date_col(s); if (is.null(dc)) return(tibble(profileId = character(0), latest_date = as.Date(character(0))))
    s %>% mutate(session_date__ = as_date_safely(.data[[dc]])) %>% filter(!is.na(session_date__)) %>%
      group_by(profileId) %>% summarise(latest_date = max(session_date__, na.rm = TRUE), .groups = "drop")
  })

  default_athlete_id <- function(a_tbl) {
    if (is.null(a_tbl) || nrow(a_tbl) == 0) return("")
    latest <- athlete_latest_dates()
    if (!is.null(latest) && nrow(latest) > 0) {
      j <- a_tbl %>% left_join(latest, by = "profileId") %>% arrange(desc(latest_date), athleteName)
      return(j$profileId[1] %||% a_tbl$profileId[1] %||% "")
    }
    a_tbl$profileId[1] %||% ""
  }

  test_types <- reactive({
    s <- fd_session_summary()
    if (is.null(s) || nrow(s) == 0 || !("testType" %in% names(s))) return(character(0))
    sort(unique(na.omit(as.character(s$testType))))
  })
  athlete_test_types <- function(profile_id = NULL) {
    s <- fd_session_summary()
    if (is.null(s) || nrow(s) == 0 || !all(c("profileId", "testType") %in% names(s))) return(test_types())
    if (!is.null(profile_id) && nzchar(profile_id)) s <- s %>% filter(profileId == profile_id)
    sort(unique(na.omit(as.character(s$testType))))
  }
  preferred_test_type <- function(available, current = NULL) {
    available <- available[nzchar(available %||% character(0))]
    if (length(available) == 0) return("")
    if (!is.null(current) && current %in% available) return(current)
    if ("CMJ" %in% available) return("CMJ")
    available[1] %||% ""
  }
  metric_choices <- reactive({
    s <- fd_session_summary()
    if (is.null(s) || nrow(s) == 0) return(character(0))
    mc <- get_metric_id_col(s); if (is.null(mc)) return(character(0))
    sort(unique(na.omit(as.character(s[[mc]]))))
  })
  metric_lookup <- reactive({
    tl <- fd_trials_long(); s <- fd_session_summary()
    if (!is.null(tl) && nrow(tl) > 0 && all(c("metricKey", "metricName") %in% names(tl))) {
      return(tl %>% distinct(metricKey, metricName) %>% filter(!is.na(metricKey), !is.na(metricName)))
    }
    if (!is.null(s) && nrow(s) > 0 && all(c("metricKey", "metricName") %in% names(s))) {
      return(s %>% distinct(metricKey, metricName) %>% filter(!is.na(metricKey), !is.na(metricName)))
    }
    tibble(metricKey = character(0), metricName = character(0))
  })
  preset_to_available_ids <- function(df, preset_metric_names) {
    mc <- get_metric_id_col(df); if (is.null(mc)) return(character(0))
    if (mc == "metricName") return(preset_metric_names)
    if (mc == "metricKey") {
      map <- metric_lookup(); if (nrow(map) == 0) return(character(0))
      return(unique(map %>% filter(metricName %in% preset_metric_names) %>% pull(metricKey)))
    }
    preset_metric_names
  }

  # ---- Sync selected athlete across tabs ----
  syncing <- reactiveVal(FALSE)
  sync_all <- function(athlete = NULL, testType = NULL, metric = NULL) {
    if (isTRUE(syncing())) return(invisible())
    syncing(TRUE); on.exit(syncing(FALSE), add = TRUE)
    if (!is.null(athlete) && nzchar(athlete)) {
      updateSelectInput(session, "curve_athlete", selected = athlete)
      updateSelectInput(session, "profile_athlete", selected = athlete)
    }
    if (!is.null(testType) && nzchar(testType)) updateSelectInput(session, "curve_testType", selected = testType)
    if (!is.null(metric) && nzchar(metric)) updateSelectInput(session, "curve_metric", selected = metric)
  }
  observeEvent(input$curve_athlete, { sync_all(athlete = input$curve_athlete) }, ignoreInit = TRUE)
  observeEvent(input$profile_athlete, { sync_all(athlete = input$profile_athlete) }, ignoreInit = TRUE)
  observeEvent(input$curve_testType, { sync_all(testType = input$curve_testType) }, ignoreInit = TRUE)
  observeEvent(input$curve_metric, { sync_all(metric = input$curve_metric) }, ignoreInit = TRUE)

  # Debounced athlete selections. Stress testing found that switching the
  # selected athlete rapidly (e.g. clicking through many athletes in a few
  # seconds) queued a full recompute of every dependent output -- profile
  # cards, CMJ trend plot, sprint history, the relationship table, and
  # (through sync_all) Curve View's own session lookups -- for EACH
  # intermediate selection, backing the session up for minutes and leaving
  # some tables permanently blank until reload. debounce() means only the
  # value still selected after a short quiet period triggers that work; the
  # sync_all() observers above stay on the raw input since they only do cheap
  # updateSelectInput() calls, not the expensive part.
  profile_athlete_debounced <- debounce(reactive(input$profile_athlete), 400)
  curve_athlete_debounced <- debounce(reactive(input$curve_athlete), 400)

  # ============================================================
  # CMJ monitoring status (unified pitchers + hitters -- CMJ only)
  # ============================================================
  cmj_monitor_long <- reactive({
    s <- fd_session_summary(); if (is.null(s) || nrow(s) == 0) return(tibble())
    map <- tryCatch(metric_lookup(), error = function(e) tibble())
    build_cmj_monitor_long(s, roster_tbl(), metric_map = map)
  })
  cmj_window_bounds <- reactive({
    d <- cmj_monitor_long(); req(nrow(d) > 0)
    anchor <- max(d$session_date, na.rm = TRUE)
    days <- as.integer(input$cmj_window_days %||% 28L)
    tibble(anchor = anchor, start = anchor - (days - 1))
  })
  cmj_metric_status_all <- reactive({
    days <- as.integer(input$cmj_window_days %||% 28L)
    d <- cmj_monitor_long(); if (nrow(d) == 0) return(tibble())
    b <- cmj_window_bounds()
    build_cmj_metric_status(d, days, anchor_date = b$anchor[[1]], metric_labels = CMJ_METRIC_LABEL)
  })
  cmj_confidence_lookup <- reactive({
    src <- fd_session_summary()
    if (is.null(src) || nrow(src) == 0) return(tibble(profileId=character(), CMJ_Last_Date=as.Date(character()),
      cmj_days_since_last_test=numeric(), cmj_baseline_n=integer(), CMJ_Confidence=character()))
    latest <- fd_session_summary() %>%
      { if (is.null(.) || !is.data.frame(.) || nrow(.) == 0 || !("profileId" %in% names(.)) || !("session_date" %in% names(.))) {
          tibble(profileId = character(0), session_date = as.Date(character(0)))
        } else . } %>%
      mutate(CMJ_Last_Date = as_date_safely(session_date)) %>% filter(!is.na(CMJ_Last_Date)) %>%
      group_by(profileId) %>% summarise(CMJ_Last_Date = max(CMJ_Last_Date, na.rm = TRUE), .groups = "drop")
    baseline_src <- cmj_metric_status_all()
    baseline_n <- if (is.null(baseline_src) || nrow(baseline_src) == 0 || !all(c("profileId", "baseline_mean") %in% names(baseline_src))) {
      tibble(profileId = character(0), cmj_baseline_n = integer(0))
    } else baseline_src %>% group_by(profileId) %>% summarise(cmj_baseline_n = sum(!is.na(baseline_mean)), .groups = "drop")
    latest %>% full_join(baseline_n, by = "profileId") %>%
      mutate(cmj_baseline_n = ifelse(is.na(cmj_baseline_n), 0L, cmj_baseline_n),
             cmj_days_since_last_test = as.numeric(Sys.Date() - CMJ_Last_Date),
             CMJ_Confidence = mapply(confidence_from_recency, cmj_days_since_last_test, cmj_baseline_n)) %>%
      select(profileId, CMJ_Last_Date, cmj_days_since_last_test, cmj_baseline_n, CMJ_Confidence)
  })

  # Correctly-columned empty fallback -- see empty_roster_tbl note above.
  # score_cmj_vulnerability()/score_cmj_performance() both return a bare,
  # zero-column tibble() when given no rows, which used to propagate into
  # cmj_athlete_status_all()'s mutate() below and crash with "object 'Status'
  # not found" whenever there was no CMJ data loaded at all.
  empty_cmj_calc_tbl <- tibble(
    profileId = character(0), athleteName = character(0), externalId = character(0), roleGroup = character(0),
    TriggerCount = integer(0), RedTriggers = integer(0), CMJ_Composite = double(0), PerformanceTrendingUp = logical(0),
    Status = character(0), reason_text = character(0),
    CMJ_Performance_Status = character(0), CMJ_Performance_Composite = double(0),
    CMJ_Performance_PositiveTriggers = integer(0), CMJ_Performance_Reason = character(0)
  )
  compute_cmj_athlete_status <- function(ms) {
    if (is.null(ms) || nrow(ms) == 0) return(empty_cmj_calc_tbl)
    vulnerability <- score_cmj_vulnerability(ms)
    performance <- score_cmj_performance(ms) %>% select(profileId, CMJ_Performance_Status, CMJ_Performance_Composite, CMJ_Performance_PositiveTriggers, CMJ_Performance_Reason)
    vulnerability %>% left_join(performance, by = "profileId") %>% ungroup()
  }

  # No early-return-on-empty-roster here (there used to be one) -- it skipped
  # the left_join(cmj_confidence_lookup()) step below, so the result was
  # missing CMJ_Last_Date/CMJ_Confidence entirely and the mutate() a few lines
  # down crashed with "object 'CMJ_Last_Date' not found" the moment roster
  # data was empty or missing. Always running the same join+mutate pipeline
  # on a (possibly zero-row, but correctly-columned) roster_base fixes that:
  # the result is just an empty-but-valid tibble instead of an error.
  cmj_athlete_status_all <- reactive({
    calc <- compute_cmj_athlete_status(cmj_metric_status_all())
    roster_base <- season_roster_tbl() %>%
      transmute(profileId = as.character(profileId), athleteName = as.character(athleteName),
                externalId = as.character(externalId), roleGroup = as.character(roleGroup)) %>%
      distinct(profileId, .keep_all = TRUE)
    roster_base %>% left_join(calc, by = "profileId", suffix = c("_roster", "")) %>% left_join(cmj_confidence_lookup(), by = "profileId") %>%
      mutate(
        athleteName = dplyr::coalesce(athleteName, athleteName_roster), externalId = dplyr::coalesce(externalId, externalId_roster),
        roleGroup = dplyr::coalesce(roleGroup, roleGroup_roster),
        athleteName = ifelse(is.na(athleteName) | !nzchar(athleteName), "(Unknown name)", athleteName),
        Status = ifelse(is.na(Status), "No Recent Data", Status), RedTriggers = ifelse(is.na(RedTriggers), 0, RedTriggers),
        TriggerCount = ifelse(is.na(TriggerCount), 0, TriggerCount),
        PerformanceTrendingUp = ifelse(is.na(PerformanceTrendingUp), FALSE, PerformanceTrendingUp),
        CMJ_Confidence = dplyr::coalesce(CMJ_Confidence, "No Recent Data"),
        reason_text = dplyr::coalesce(reason_text, "No CMJ vulnerability triggers in the selected window.")
      ) %>% select(-any_of(c("athleteName_roster", "externalId_roster", "roleGroup_roster")))
  })

  active_roster_tbl <- reactive({
    r <- roster_tbl(); s <- fd_session_summary()
    empty_active <- r %>% filter(FALSE) %>% select(profileId, athleteName, externalId, roleGroup)
    if (is.null(r) || nrow(r) == 0 || is.null(s) || nrow(s) == 0) return(empty_active)
    dc <- get_date_col(s); if (is.null(dc)) return(empty_active)
    active_ids <- s %>% filter(testType == "CMJ", profileId %in% r$profileId) %>%
      mutate(session_date__ = as_date_safely(.data[[dc]])) %>% filter(!is.na(session_date__), session_date__ >= ACTIVE_CMJ_CUTOFF) %>%
      distinct(profileId) %>% pull(profileId)
    r %>% filter(profileId %in% active_ids) %>% select(profileId, athleteName, externalId, roleGroup)
  })

  # ---- Staff alert rows: CMJ-only, both roles, no persistent review workflow ----
  staff_alert_rows_raw <- reactive({
    st <- cmj_athlete_status_all()
    # No is.null()/nrow()==0 early return here (there used to be one): it
    # replaced a validly-columned-but-zero-row `st` (e.g. no roster loaded)
    # with a bare, columnless tibble(), which then crashed every downstream
    # reader of this reactive with "object 'athlete_id' not found" -- the
    # mutate/transmute pipeline below is already safe on zero rows as long as
    # the columns it references exist, which cmj_athlete_status_all()
    # guarantees unconditionally.
    st %>% mutate(
      alert_date = dplyr::coalesce(CMJ_Last_Date, Sys.Date()),
      domain = "CMJ Vulnerability",
      season_phase = assign_season_phase(alert_date),
      alert_id = paste(profileId, domain, alert_date, sep = "::")
    ) %>% transmute(
      alert_id, athlete_id = profileId, athlete = athleteName, externalId, role = roleGroup,
      date = alert_date, season_phase, current_status = Status, domain,
      primary_reason = reason_text, trigger_count = TriggerCount,
      cmj_performance_status = CMJ_Performance_Status, cmj_vulnerability_status = Status,
      confidence = dplyr::coalesce(as.character(CMJ_Confidence), "No Recent Data"),
      cmj_days_since_last_test, cmj_baseline_n
    )
  })

  filtered_alert_rows <- reactive({
    rows <- staff_alert_rows_raw()
    if (is.null(rows) || nrow(rows) == 0) return(tibble())
    role_keep <- input$alert_role_filter %||% c("Pitchers", "Hitters")
    status_keep <- input$alert_status_filter %||% c("Red", "Yellow", "Blue", "Green", "No Recent Data")
    conf_keep <- input$alert_confidence_filter %||% c("High", "Moderate", "Low", "No Recent Data")
    days <- as.integer(input$alert_window_days %||% 28L)
    q <- tolower(trimws(input$alert_search %||% ""))
    out <- rows %>% filter(role %in% role_keep, current_status %in% status_keep, confidence %in% conf_keep, date >= Sys.Date() - days)
    if (isTRUE(input$alert_triggered_only)) out <- out %>% filter(current_status %in% c("Red", "Yellow"))
    if (nzchar(q)) out <- out %>% filter(grepl(q, tolower(athlete), fixed = TRUE) | grepl(q, tolower(externalId), fixed = TRUE))
    out %>% arrange(desc(status_rank(current_status)), desc(date), athlete)
  })

  output$alert_inbox_kpis <- renderUI({
    rows <- filtered_alert_rows(); if (is.null(rows) || nrow(rows) == 0) return(NULL)
    layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Shown"), div(class = "kpi-value", nrow(rows))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Red"), div(class = "kpi-value", sum(rows$current_status == "Red"))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Yellow"), div(class = "kpi-value", sum(rows$current_status == "Yellow"))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Blue"), div(class = "kpi-value", sum(rows$current_status == "Blue"))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Green"), div(class = "kpi-value", sum(rows$current_status == "Green"))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "No Recent"), div(class = "kpi-value", sum(rows$current_status == "No Recent Data")))))
    )
  })

  # Manual clickable HTML table (renderUI + HTML()), not renderDT: renderDT
  # widgets intermittently failed to draw client-side across this app (see
  # note above team_general_cmj_dt) and row-click selection is core to this
  # table's UX, so a plain DT->renderTable swap wasn't an option here. Each
  # row's onclick sets alert_row_click directly to its alert_id.
  # Full HTML escaper (including quotes) even though every current call site
  # only places the result in text content, not inside an attribute value --
  # stress testing confirmed that's currently safe, but an escaper that's
  # only safe for its current callers is a trap for the next one.
  html_esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE); x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
  }
  output$alert_inbox_dt <- renderUI({
    rows <- filtered_alert_rows()
    if (is.null(rows) || nrow(rows) == 0) return(tags$div(class = "tiny subtle", "No alerts match current filters."))
    selected_id <- input$alert_row_click %||% ""
    header <- "<tr><th>Athlete</th><th>Role</th><th>Date</th><th>Phase</th><th>Status</th><th>Reason</th><th>Triggers</th><th>Confidence</th></tr>"
    body_rows <- vapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      row_style <- if (identical(as.character(r$alert_id), selected_id)) "background:rgba(246,190,0,0.15);cursor:pointer;" else "cursor:pointer;"
      paste0(
        "<tr style='", row_style, "' onclick='Shiny.setInputValue(", "\"alert_row_click\"", ", ", jsonlite::toJSON(as.character(r$alert_id), auto_unbox = TRUE), ", {priority:\"event\"})'>",
        "<td>", html_esc(r$athlete), "</td><td>", html_esc(r$role), "</td><td>", html_esc(as.character(r$date)), "</td><td>", html_esc(r$season_phase), "</td>",
        "<td>", status_pill_html(r$current_status), "</td>",
        "<td style='max-width:340px;'>", html_esc(r$primary_reason), "</td><td>", html_esc(r$trigger_count), "</td><td>", coverage_badge_html(r$confidence), "</td></tr>"
      )
    }, character(1))
    HTML(paste0(
      "<div style='overflow-x:auto;'><table class='table table-striped table-hover' style='width:100%;font-size:13px;'>",
      "<thead>", header, "</thead><tbody>", paste(body_rows, collapse = ""), "</tbody></table></div>"
    ))
  })

  output$alert_inbox_csv <- downloadHandler(
    filename = function() paste0("cmj_alert_inbox_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      rows <- filtered_alert_rows()
      export_cols <- c("athlete", "role", "date", "season_phase", "current_status", "primary_reason", "trigger_count", "confidence")
      readr::write_csv(rows[, intersect(export_cols, names(rows)), drop = FALSE], file, na = "")
    }
  )

  selected_alert_row <- reactive({
    rows <- filtered_alert_rows()
    sel_id <- input$alert_row_click %||% ""
    if (is.null(rows) || nrow(rows) == 0 || !nzchar(sel_id)) return(NULL)
    hit <- rows %>% filter(alert_id == sel_id)
    if (nrow(hit) != 1) return(NULL)
    hit
  })
  output$alert_detail_ui <- renderUI({
    row <- selected_alert_row()
    if (is.null(row)) return(tags$div(class = "tiny subtle", "Select an alert row to see details."))
    tagList(
      tags$div(style = "font-weight:900;color:#501214;", paste(row$athlete, row$current_status, sep = " - ")),
      tags$div(class = "tiny subtle", paste0(row$role, " | ", row$season_phase, " | Confidence: ", row$confidence)),
      tags$hr(),
      tags$div(tags$b("Why flagged")),
      tags$div(class = "tiny subtle", row$primary_reason),
      tags$hr(),
      tags$div(class = "tiny subtle", paste0("CMJ last tested: ", row$cmj_days_since_last_test, " days ago | Baseline sessions: ", row$cmj_baseline_n))
    )
  })
  observeEvent(input$alert_open_profile, {
    row <- selected_alert_row()
    if (is.null(row)) { showNotification("Select an alert row first.", type = "warning", duration = 4); return(invisible()) }
    sync_all(athlete = row$athlete_id[[1]])
    bslib::nav_select("main_nav", selected = "Athlete Profile", session = session)
  }, ignoreInit = TRUE)

  # ============================================================
  # Team Overview (CMJ-only)
  # ============================================================
  team_latest_cmj_needed <- reactive({
    s <- fd_session_summary(); req(!is.null(s), nrow(s) > 0)
    r <- active_roster_tbl(); if (is.null(r) || nrow(r) == 0) return(tibble())
    mc <- get_metric_id_col(s); dc <- get_date_col(s); bc <- get_best_col(s); mnc <- get_mean_col(s); sdc <- get_sd_col(s); cvc <- get_cv_col(s)
    req(!is.null(mc), !is.null(dc)); req(!is.null(bc) || !is.null(mnc))
    role_sel <- input$team_group %||% "All"
    roster2 <- r; if (role_sel %in% c("Pitchers", "Hitters")) roster2 <- roster2 %>% filter(roleGroup == role_sel)
    needed_names <- unique(c(HITTER_METRICS_ORDERED, "Peak Power", "Bodyweight in Pounds", PITCHER_PERF_METRICS, PITCHER_INJURY_VALUE_METRICS, PITCHER_INJURY_SD_METRICS))
    needed_ids <- preset_to_available_ids(s, needed_names); if (length(needed_ids) == 0) return(tibble())
    long <- s %>% filter(testType == "CMJ", profileId %in% roster2$profileId, .data[[mc]] %in% needed_ids) %>%
      mutate(session_date__ = as_date_safely(.data[[dc]])) %>% filter(!is.na(session_date__)) %>%
      group_by(profileId) %>% filter(session_date__ == max(session_date__, na.rm = TRUE)) %>% ungroup() %>%
      left_join(roster2, by = "profileId") %>% standardize_joined_names() %>%
      select(-any_of(c("athleteName.x", "athleteName.y", "externalId.x", "externalId.y"))) %>%
      mutate(athleteName = ifelse(is.na(athleteName) | !nzchar(athleteName), "(Unknown name)", athleteName),
             best_value = if (!is.null(bc)) as_num(.data[[bc]]) else NA_real_, mean_value = if (!is.null(mnc)) as_num(.data[[mnc]]) else NA_real_,
             sd_value = if (!is.null(sdc)) as_num(.data[[sdc]]) else NA_real_, cv_value = if (!is.null(cvc)) as_num(.data[[cvc]]) else NA_real_) %>%
      mutate(session_date = session_date__)
    if (mc == "metricKey") { map <- metric_lookup(); if (nrow(map) > 0) long <- long %>% left_join(map, by = c("metricKey" = "metricKey")) }
    # See the matching comment in build_cmj_monitor_long(): VALD's raw API
    # reports Jump Height in centimeters, so its "RSI-modified" value comes
    # out ~100x the conventional 0.2-1.0 m/s scale. This is a separate raw
    # pull from build_cmj_monitor_long() (feeds the Team Overview preset
    # table and bar chart, not the monitoring/scoring pipeline), so it needs
    # the same correction applied independently.
    mdisp2 <- if ("metricName" %in% names(long)) "metricName" else mc
    if (mdisp2 %in% names(long)) {
      long <- long %>% mutate(
        best_value = ifelse(.data[[mdisp2]] == "RSI-modified", best_value / 100, best_value),
        mean_value = ifelse(.data[[mdisp2]] == "RSI-modified", mean_value / 100, mean_value)
      )
    }
    long
  })

  # Broader than team_latest_cmj_needed(): that reactive is intentionally
  # restricted to the curated preset metric lists (feeds the fixed-column
  # preset table + pitcher tables), but the team bar chart should let staff
  # pick ANY CMJ metric, not just the preset ones.
  team_bar_source <- reactive({
    s <- fd_session_summary(); req(!is.null(s), nrow(s) > 0)
    r <- active_roster_tbl(); if (is.null(r) || nrow(r) == 0) return(tibble())
    mc <- get_metric_id_col(s); dc <- get_date_col(s); bc <- get_best_col(s)
    req(!is.null(mc), !is.null(dc), !is.null(bc))
    role_sel <- input$team_group %||% "All"
    roster2 <- r; if (role_sel %in% c("Pitchers", "Hitters")) roster2 <- roster2 %>% filter(roleGroup == role_sel)
    long <- s %>% filter(testType == "CMJ", profileId %in% roster2$profileId) %>%
      mutate(session_date__ = as_date_safely(.data[[dc]])) %>% filter(!is.na(session_date__)) %>%
      group_by(profileId) %>% filter(session_date__ == max(session_date__, na.rm = TRUE)) %>% ungroup() %>%
      left_join(roster2, by = "profileId") %>% standardize_joined_names() %>%
      select(-any_of(c("athleteName.x", "athleteName.y", "externalId.x", "externalId.y"))) %>%
      mutate(athleteName = ifelse(is.na(athleteName) | !nzchar(athleteName), "(Unknown name)", athleteName),
             best_value = as_num(.data[[bc]])) %>%
      mutate(session_date = session_date__)
    if (mc == "metricKey") { map <- metric_lookup(); if (nrow(map) > 0) long <- long %>% left_join(map, by = c("metricKey" = "metricKey")) }
    # See the RSI-modified unit comment in build_cmj_monitor_long().
    mdisp2 <- if ("metricName" %in% names(long)) "metricName" else mc
    if (mdisp2 %in% names(long)) {
      long <- long %>% mutate(best_value = ifelse(.data[[mdisp2]] == "RSI-modified", best_value / 100, best_value))
    }
    long
  })

  output$team_kpis <- renderUI({
    r <- active_roster_tbl(); role_sel <- input$team_group %||% "All"
    roster2 <- r; if (role_sel %in% c("Pitchers", "Hitters")) roster2 <- roster2 %>% filter(roleGroup == role_sel)
    df <- team_latest_cmj_needed()
    last_dates <- if (is.null(df) || nrow(df) == 0) tibble() else df %>% distinct(profileId, session_date)
    layout_columns(col_widths = c(3, 3, 3, 3),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Active roster size"), div(class = "kpi-value", nrow(roster2))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Athletes with CMJ shown"), div(class = "kpi-value", nrow(last_dates))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Most recent CMJ"), div(class = "kpi-value", if (nrow(last_dates) > 0) as.character(max(last_dates$session_date, na.rm = TRUE)) else "-")))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Role filter"), div(class = "kpi-value", role_sel))))
    )
  })

  team_general_cmj_board <- reactive({
    df <- team_latest_cmj_needed(); if (is.null(df) || nrow(df) == 0) return(tibble())
    mdisp <- if ("metricName" %in% names(df)) "metricName" else get_metric_id_col(df); req(!is.null(mdisp))
    long <- df %>% filter(.data[[mdisp]] %in% unique(c(HITTER_METRICS_ORDERED, "Peak Power", "Bodyweight in Pounds"))) %>%
      select(profileId, athleteName, roleGroup, externalId, session_date, metric = all_of(mdisp), value = best_value)
    if (nrow(long) == 0) return(tibble())
    wide <- long %>% mutate(metric = factor(metric, levels = HITTER_METRICS_ORDERED)) %>% tidyr::pivot_wider(names_from = metric, values_from = value)
    if (!("Peak Power" %in% names(wide))) wide$`Peak Power` <- NA_real_
    if (!("Bodyweight in Pounds" %in% names(wide))) wide$`Bodyweight in Pounds` <- NA_real_
    wide <- wide %>% mutate(`Peak Power / BW (lb)` = ifelse(!is.na(`Peak Power`) & !is.na(`Bodyweight in Pounds`) & `Bodyweight in Pounds` > 0, `Peak Power` / `Bodyweight in Pounds`, NA_real_))
    last_date <- long %>% distinct(profileId, session_date) %>% group_by(profileId) %>% summarise(`Last CMJ` = max(session_date, na.rm = TRUE), .groups = "drop")
    out <- wide %>% left_join(last_date, by = "profileId") %>%
      transmute(Athlete = athleteName, Role = roleGroup, `Last CMJ` = `Last CMJ`, !!!rlang::syms(HITTER_METRICS_ORDERED), `Peak Power / BW (lb)` = `Peak Power / BW (lb)`)
    # DT/DataTables column names must not contain ":" -- it collides with
    # DataTables' own column-selector syntax and silently breaks widget
    # rendering client-side (the container never gets populated). Rename for
    # display only; the metric name used above for matching against the raw
    # VALD data is untouched.
    if ("P2 Concentric Impulse:P1 Concentric Impulse" %in% names(out)) {
      out <- out %>% rename(`P2 over P1 Concentric Impulse` = `P2 Concentric Impulse:P1 Concentric Impulse`)
    }
    out
  })

  # renderTable (base Shiny, no DataTables/htmlwidget JS layer) rather than
  # renderDT here: this table intermittently failed to draw client-side in
  # testing (valid data delivered server-side, htmlwidget constructed without
  # error, but the DataTables JS widget never populated) despite matching the
  # internal dashboard's own working DT pattern -- root cause not isolated.
  # renderTable trades away client-side sort/search/colvis for guaranteed
  # rendering. Revisit with renderDT if this turns out to be specific to the
  # environment this was built in rather than a real issue for your users.
  output$team_general_cmj_dt <- renderTable({
    out <- team_general_cmj_board()
    if (is.null(out) || nrow(out) == 0) return(data.frame(Message = "No CMJ rows found for this selection."))
    num_cols <- setdiff(names(out), c("Athlete", "Role", "externalId", "Last CMJ"))
    for (nc in num_cols) out[[nc]] <- round(out[[nc]], 2)
    out$`Last CMJ` <- as.character(out$`Last CMJ`)
    out
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")

  pitcher_latest_cmj_long_team <- reactive({
    df <- team_latest_cmj_needed(); if (is.null(df) || nrow(df) == 0) return(tibble())
    df %>% filter(roleGroup == "Pitchers")
  })
  pitcher_perf_table_team <- reactive(build_pitcher_perf_table(pitcher_latest_cmj_long_team(), multi_athlete = TRUE))
  pitcher_injury_table_team <- reactive(build_pitcher_injury_table(pitcher_latest_cmj_long_team(), multi_athlete = TRUE))

  output$team_pitcher_addon_ui <- renderUI({
    role_sel <- input$team_group %||% "All"
    if (!(role_sel %in% c("All", "Pitchers"))) return(NULL)
    layout_columns(col_widths = c(6, 6),
      card(card_header(tags$b("Pitchers | Performance drivers (CMJ)")), card_body(tags$div(style = "overflow-x:auto;", tableOutput("pitcher_perf_dt_team")))),
      card(card_header(tags$b("Pitchers | Injury drivers (CMJ)")), card_body(tags$div(style = "overflow-x:auto;", tableOutput("pitcher_injury_dt_team"))))
    )
  })
  output$pitcher_perf_dt_team <- renderTable({
    out <- pitcher_perf_table_team(); if (is.null(out) || nrow(out) == 0) return(data.frame(Message = "No pitcher CMJ rows found."))
    num_cols <- setdiff(names(out), c("Athlete", "Role", "externalId", "Last CMJ"))
    for (nc in num_cols) out[[nc]] <- round(out[[nc]], 2)
    out$`Last CMJ` <- as.character(out$`Last CMJ`)
    out
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  output$pitcher_injury_dt_team <- renderTable({
    out <- pitcher_injury_table_team(); if (is.null(out) || nrow(out) == 0) return(data.frame(Message = "No pitcher CMJ rows found."))
    num_cols <- setdiff(names(out), c("Athlete", "Role", "externalId", "Last CMJ"))
    for (nc in num_cols) out[[nc]] <- round(out[[nc]], 2)
    out$`Last CMJ` <- as.character(out$`Last CMJ`)
    out
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")

  output$team_bar_metric_ui <- renderUI({
    map <- metric_lookup()
    choices <- if (nrow(map) > 0) sort(unique(map$metricName)) else HITTER_METRICS_ORDERED
    sel <- isolate(input$team_bar_metric) %||% "RSI-modified"; if (!(sel %in% choices)) sel <- choices[1] %||% ""
    selectInput("team_bar_metric", "Metric", choices = choices, selected = sel)
  })
  output$team_bar_plot <- renderPlotly({
    df <- team_bar_source(); if (is.null(df) || nrow(df) == 0) return(plotly_empty())
    met_name <- input$team_bar_metric %||% HITTER_METRICS_ORDERED[1]
    mdisp <- if ("metricName" %in% names(df)) "metricName" else get_metric_id_col(df); if (is.null(mdisp)) return(plotly_empty())
    if (mdisp == "metricKey" && !("metricName" %in% names(df))) { map <- metric_lookup(); if (nrow(map) > 0) df <- df %>% left_join(map, by = c("metricKey" = "metricKey")); mdisp <- if ("metricName" %in% names(df)) "metricName" else "metricKey" }
    d <- df %>% filter(.data[[mdisp]] == met_name, !is.na(best_value)) %>% mutate(athlete = ifelse(is.na(athleteName) | !nzchar(athleteName), profileId, athleteName), role = roleGroup) %>% arrange(best_value)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~best_value, y = ~reorder(athlete, best_value), type = "bar", orientation = "h", color = ~role,
            hovertemplate = paste0("%{y}<br>", met_name, ": %{x:.2f}<extra></extra>")) %>%
      layout(margin = list(l = 160, r = 10, t = 10, b = 40), xaxis = list(title = met_name), yaxis = list(title = ""))
  })

  # ---- Coach report (status-first, CMJ-only) ----
  empty_team_report_status_rows <- tibble(
    Athlete = character(0), externalId = character(0), Role = character(0),
    Status = character(0), Reason = character(0), `Last Tested` = as.Date(character(0))
  )
  build_team_report_status_rows <- function(role_sel) {
    rows <- tryCatch(staff_alert_rows_raw(), error = function(e) tibble())
    if (is.null(rows) || nrow(rows) == 0) return(empty_team_report_status_rows)
    if (role_sel %in% c("Pitchers", "Hitters")) rows <- rows %>% filter(role == role_sel)
    if (nrow(rows) == 0) return(empty_team_report_status_rows)
    rows %>% mutate(Reason = trimws(sub("\\s*Threshold used:.*$", "", as.character(primary_reason %||% ""))),
                     Reason = ifelse(nzchar(Reason), Reason, "No elevated signal."), rank__ = status_rank(current_status)) %>%
      transmute(Athlete = athlete, externalId, Role = role, Status = current_status, Reason, `Last Tested` = date, rank__) %>%
      arrange(desc(rank__), Athlete) %>% select(-rank__)
  }
  build_team_report_payload <- function(role_override = NULL) {
    role_sel <- role_override %||% (input$team_group %||% "All")
    active <- active_roster_tbl(); if (!is.null(active) && nrow(active) > 0 && role_sel %in% c("Pitchers", "Hitters")) active <- active %>% filter(roleGroup == role_sel)
    active_n <- if (is.null(active)) 0L else nrow(active)
    status_rows <- build_team_report_status_rows(role_sel)
    list(generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE), role_filter = role_sel, active_cutoff = format(ACTIVE_CMJ_CUTOFF, "%Y-%m-%d"),
         summary_counts = tibble(active_roster = active_n, red = sum(status_rows$Status == "Red", na.rm = TRUE), yellow = sum(status_rows$Status == "Yellow", na.rm = TRUE),
                                  green = sum(status_rows$Status == "Green", na.rm = TRUE), blue = sum(status_rows$Status == "Blue", na.rm = TRUE),
                                  no_data = sum(status_rows$Status == "No Recent Data", na.rm = TRUE)),
         needs_attention = status_rows %>% filter(Status %in% c("Red", "Yellow")), stable_rows = status_rows %>% filter(Status %in% c("Green", "Blue")),
         no_data_rows = status_rows %>% filter(Status == "No Recent Data"))
  }
  write_team_report_html <- function(file, payload) {
    esc <- function(x) { x <- as.character(x); x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE); x }
    status_badge <- function(status) {
      colors <- c("Red" = "#ef4444", "Yellow" = "#f59e0b", "Blue" = "#2563eb", "Green" = "#16a34a", "No Recent Data" = "#6b7280")
      col <- unname(colors[as.character(status)]); col <- ifelse(is.na(col), "#6b7280", col)
      paste0("<span style='display:inline-block;padding:2px 8px;border-radius:999px;color:#fff;font-size:11px;font-weight:700;background:", col, "'>", esc(status), "</span>")
    }
    needs_attention_html <- function(df) {
      if (is.null(df) || nrow(df) == 0) return("<p style='color:#166534;font-weight:600;'>No athletes currently flagged Red or Yellow.</p>")
      show <- as.data.frame(df, stringsAsFactors = FALSE); if (inherits(show$`Last Tested`, "Date")) show$`Last Tested` <- as.character(show$`Last Tested`)
      header <- "<tr><th>Athlete</th><th>Role</th><th>Status</th><th>Reason</th><th>Last Tested</th></tr>"
      rows <- vapply(seq_len(nrow(show)), function(i) paste0("<tr><td>", esc(show$Athlete[i]), "</td><td>", esc(show$Role[i]), "</td><td>", status_badge(show$Status[i]), "</td><td>", esc(show$Reason[i]), "</td><td>", esc(show$`Last Tested`[i]), "</td></tr>"), character(1))
      paste0("<table><thead>", header, "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
    }
    compact_list_html <- function(df, status_vals, label) {
      if (is.null(df) || nrow(df) == 0) return("")
      sub <- df[df$Status %in% status_vals, , drop = FALSE]; if (nrow(sub) == 0) return("")
      paste0("<p style='font-size:12.5px;color:#374151;margin:4px 0;'><strong>", esc(label), " (", nrow(sub), "):</strong> ", paste(esc(sub$Athlete), collapse = ", "), "</p>")
    }
    counts <- payload$summary_counts; cnt <- function(nm) if (!is.null(counts) && nm %in% names(counts)) counts[[nm]][[1]] else 0L
    html <- paste0(
      "<!doctype html><html><head><meta charset='utf-8'><title>TXST CMJ Report</title>",
      "<style>body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;margin:16px;color:#111827;}",
      "h1{margin:0 0 6px 0;}h2{margin:16px 0 8px 0;font-size:18px;}.sub{color:#374151;font-size:13px;}",
      ".grid{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:8px;margin:10px 0 14px 0;}",
      ".card{border:1px solid #d1d5db;border-radius:10px;padding:10px;background:#f9fafb;}.k{font-size:11px;color:#4b5563;text-transform:uppercase;}.v{font-size:22px;font-weight:700;}",
      "table{border-collapse:collapse;width:100%;font-size:12px;}th,td{border:1px solid #d1d5db;padding:6px;text-align:left;}th{background:#f3f4f6;}</style></head><body>",
      "<h1>TXST Baseball CMJ Coach Brief</h1>",
      "<div class='sub'><strong>Generated (UTC):</strong> ", esc(payload$generated_at), " | <strong>Role filter:</strong> ", esc(payload$role_filter), " | <strong>Active CMJ cutoff:</strong> ", esc(payload$active_cutoff), "</div>",
      "<div class='grid' style='grid-template-columns:repeat(6,minmax(0,1fr));'><div class='card'><div class='k'>Active Roster</div><div class='v'>", esc(cnt("active_roster")), "</div></div>",
      "<div class='card'><div class='k'>Red</div><div class='v' style='color:#ef4444;'>", esc(cnt("red")), "</div></div>",
      "<div class='card'><div class='k'>Yellow</div><div class='v' style='color:#f59e0b;'>", esc(cnt("yellow")), "</div></div>",
      "<div class='card'><div class='k'>Blue</div><div class='v' style='color:#2563eb;'>", esc(cnt("blue")), "</div></div>",
      "<div class='card'><div class='k'>Green</div><div class='v' style='color:#16a34a;'>", esc(cnt("green")), "</div></div>",
      "<div class='card'><div class='k'>No Recent Data</div><div class='v' style='color:#6b7280;'>", esc(cnt("no_data")), "</div></div></div>",
      "<h2>Needs Attention</h2>", needs_attention_html(payload$needs_attention),
      "<h2>Everyone Else</h2>", compact_list_html(payload$stable_rows, "Blue", "Blue"), compact_list_html(payload$stable_rows, "Green", "Green"), compact_list_html(payload$no_data_rows, "No Recent Data", "No Recent Data"),
      "</body></html>"
    )
    writeLines(html, con = file, useBytes = TRUE)
  }
  # Explicit "All" -- Team Report must always mean the full roster,
  # independent of whatever role the Team Overview page filter happens to be
  # set to when a coach clicks download (there are dedicated Pitcher/Hitter
  # Report buttons for role-scoped exports).
  output$team_export_team_report_html <- downloadHandler(filename = function() paste0("cmj_team_report_", format(Sys.Date(), "%Y%m%d"), ".html"), content = function(file) write_team_report_html(file, build_team_report_payload("All")))
  output$team_export_pitcher_report_html <- downloadHandler(filename = function() paste0("cmj_pitcher_report_", format(Sys.Date(), "%Y%m%d"), ".html"), content = function(file) write_team_report_html(file, build_team_report_payload("Pitchers")))
  output$team_export_hitter_report_html <- downloadHandler(filename = function() paste0("cmj_hitter_report_", format(Sys.Date(), "%Y%m%d"), ".html"), content = function(file) write_team_report_html(file, build_team_report_payload("Hitters")))

  # ============================================================
  # Athlete Profile (CMJ / Sprint only)
  # ============================================================
  output$profile_athlete_ui <- renderUI({
    a <- athlete_choices(); dflt <- default_athlete_id(a); cur <- isolate(input$profile_athlete)
    sel <- if (!is.null(cur) && cur %in% a$profileId) cur else dflt
    selectInput("profile_athlete", "Athlete", choices = setNames(a$profileId, a$label), selected = sel)
  })
  selected_profile_id <- reactive({ profile_athlete_debounced() %||% default_athlete_id(athlete_choices()) })
  selected_profile_roster <- reactive({ pid <- selected_profile_id(); roster_tbl() %>% filter(profileId == pid) %>% slice(1) })
  selected_profile_status <- reactive({ pid <- selected_profile_id(); staff_alert_rows_raw() %>% filter(athlete_id == pid) })

  output$profile_header_ui <- renderUI({
    r <- selected_profile_roster(); if (is.null(r) || nrow(r) == 0) return(NULL)
    st <- selected_profile_status(); latest <- if (nrow(st) > 0) st %>% slice(1) else NULL
    meta_line <- paste0(
      r$roleGroup[[1]], " | ", r$externalId[[1]],
      " | Phase: ", if (!is.null(latest)) latest$season_phase[[1]] else "Unassigned",
      " | Confidence: ", if (!is.null(latest)) latest$confidence[[1]] else "No Recent Data"
    )
    athlete_hero_ui(
      name = r$athleteName[[1]],
      meta_line = meta_line,
      status = if (!is.null(latest)) latest$current_status[[1]] else "No Recent Data"
    )
  })
  output$profile_summary_cards <- renderUI({
    st <- selected_profile_status(); latest <- if (nrow(st) > 0) st %>% slice(1) else NULL
    if (is.null(latest)) return(card(card_body(tags$div(class = "tiny subtle", "No CMJ monitoring rows available for this athlete."))))
    pid <- selected_profile_id(); sp_st <- sprint_scored()
    sprint_row <- if (is.data.frame(sp_st) && nrow(sp_st) > 0) sp_st %>% filter(athlete_id == pid) %>% slice(1) else data.frame()
    layout_columns(col_widths = c(3, 3, 3, 3),
      card(card_body(div(class = "kpi",
        div(class = "kpi-title", "CMJ Performance"),
        div(style = "margin-top:6px;", status_pill_ui(latest$cmj_performance_status[[1]])),
        div(class = "metric-detail", "Is force/power trending up or down")
      ))),
      card(card_body(div(class = "kpi",
        div(class = "kpi-title", "CMJ Vulnerability"),
        div(style = "margin-top:6px;", status_pill_ui(latest$cmj_vulnerability_status[[1]])),
        div(class = "metric-detail", "Risk signals in recent CMJ data")
      ))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Sprint Performance"), div(style = "margin-top:6px;", status_pill_ui(if (nrow(sprint_row) > 0) sprint_row$Sprint_Status[[1]] else "No Recent Data")), div(class = "metric-detail", if (nrow(sprint_row) > 0) sprint_row$Sprint_Confidence[[1]] else "No Recent Data")))),
      card(card_body(div(class = "kpi",
        div(class = "kpi-title", "CMJ Confidence"),
        div(style = "margin-top:6px;", coverage_badge_ui(latest$confidence[[1]])),
        div(class = "metric-detail", "How much recent data backs this status")
      )))
    )
  })
  profile_trend_default_metrics <- c("Jump Height (Imp-Mom)", "RSI-modified", "Contraction Time", "Concentric Impulse", "Eccentric Braking Impulse")
  # Broader than cmj_monitor_long() (which is intentionally fixed to the 5
  # scoring metrics): this lets staff plot ANY CMJ metric on the trend chart
  # without touching what feeds the vulnerability/performance scoring engine.
  profile_trend_source <- reactive({
    pid <- selected_profile_id(); s <- fd_session_summary()
    if (is.null(s) || nrow(s) == 0 || !nzchar(pid)) return(tibble())
    mc <- get_metric_id_col(s); dc <- get_date_col(s); bc <- get_best_col(s)
    if (is.null(mc) || is.null(dc) || is.null(bc)) return(tibble())
    d <- s %>% filter(testType == "CMJ", profileId == pid) %>%
      mutate(session_date = as_date_safely(.data[[dc]]), value = as_num(.data[[bc]])) %>%
      filter(!is.na(session_date), !is.na(value))
    if (nrow(d) == 0) return(tibble())
    mdisp <- if ("metricName" %in% names(d)) "metricName" else mc
    if (mdisp == "metricKey" && !("metricName" %in% names(d))) { map <- metric_lookup(); if (nrow(map) > 0) d <- d %>% left_join(map, by = c("metricKey" = "metricKey")); mdisp <- if ("metricName" %in% names(d)) "metricName" else "metricKey" }
    d <- d %>% transmute(metric = as.character(.data[[mdisp]]), session_date, value) %>%
      group_by(metric, session_date) %>% summarise(value = max(value, na.rm = TRUE), .groups = "drop")
    # See the RSI-modified unit comment in build_cmj_monitor_long().
    d %>% mutate(value = ifelse(metric == "RSI-modified", value / 100, value))
  })
  output$profile_trend_metrics_ui <- renderUI({
    d <- profile_trend_source(); avail <- if (nrow(d) > 0) sort(unique(d$metric)) else profile_trend_default_metrics
    cur <- isolate(input$profile_trend_metrics)
    sel <- if (!is.null(cur) && length(intersect(cur, avail)) > 0) intersect(cur, avail) else intersect(profile_trend_default_metrics, avail)
    if (length(sel) == 0) sel <- avail[1]
    selectizeInput("profile_trend_metrics", "Metrics to plot", choices = avail, selected = sel, multiple = TRUE,
                    options = list(plugins = list("remove_button")))
  })
  output$profile_cmj_trends <- renderPlotly({
    d <- profile_trend_source(); if (is.null(d) || nrow(d) == 0) return(plotly_empty())
    wanted <- input$profile_trend_metrics %||% profile_trend_default_metrics
    d <- d %>% filter(metric %in% wanted) %>% arrange(session_date)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~session_date, y = ~value, color = ~metric, type = "scatter", mode = "lines+markers") %>%
      layout(margin = list(l = 40, r = 10, t = 10, b = 40), xaxis = list(title = ""), yaxis = list(title = "Value"))
  })
  output$profile_status_dt <- renderTable({
    st <- selected_profile_status(); if (is.null(st) || nrow(st) == 0) return(data.frame(Message = "No CMJ status rows for this athlete."))
    st %>% transmute(Date = as.character(date), Phase = season_phase, Status = current_status, Reason = primary_reason, Confidence = confidence)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  output$profile_sprint_ui <- renderUI({
    pid <- selected_profile_id(); sp <- fd_sprint()
    if (is.null(sp) || !is.data.frame(sp) || nrow(sp) == 0 || !("athlete_id" %in% names(sp)) || !nzchar(pid)) return(NULL)
    d <- sp %>% filter(athlete_id == pid); if (nrow(d) == 0) return(NULL)
    card(card_header(tags$b("Sprint / SmartSpeed Performance")), card_body(
      uiOutput("profile_sprint_summary_ui"), plotlyOutput("profile_sprint_trend_plot", height = "300px"),
      tags$div(style = "overflow-x:auto;", tableOutput("profile_sprint_history_dt"))
    ))
  })
  # Collapses raw per-rep SmartSpeed rows to one best-per-metric row per day
  # for the selected athlete -- multiple reps can be logged under the same
  # test_date, and a bad/stumbled rep sitting earlier in row order would
  # otherwise get picked up as "latest"/plotted directly (see the same fix
  # applied to the team-level sprint_filtered() reactive).
  profile_sprint_daily <- reactive({
    pid <- selected_profile_id(); sp <- fd_sprint()
    if (is.null(sp) || !is.data.frame(sp) || nrow(sp) == 0 || !("athlete_id" %in% names(sp)) || !nzchar(pid)) return(tibble())
    d <- sp %>% filter(athlete_id == pid) %>% mutate(test_date = as_date_safely(test_date)) %>% filter(!is.na(test_date))
    if (nrow(d) == 0) return(tibble())
    safe_min <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else min(x) }
    safe_max <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else max(x) }
    d %>% group_by(test_date) %>%
      summarise(
        best_10yd = safe_min(best_10yd), best_30yd = safe_min(best_30yd),
        best_flying_10yd = safe_min(best_flying_10yd), max_velocity = safe_max(max_velocity),
        .groups = "drop"
      ) %>% arrange(desc(test_date))
  })
  output$profile_sprint_summary_ui <- renderUI({
    d <- profile_sprint_daily(); if (nrow(d) == 0) return(NULL)
    latest <- d[1, , drop = FALSE]
    layout_columns(col_widths = c(3, 3, 3, 3),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Latest Sprint Date"), div(class = "kpi-value", as.character(latest$test_date[[1]]))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Best 10yd"), div(class = "kpi-value", ifelse(is.na(latest$best_10yd[[1]]), "-", round(latest$best_10yd[[1]], 3)))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Best 30yd"), div(class = "kpi-value", ifelse(is.na(latest$best_30yd[[1]]), "-", round(latest$best_30yd[[1]], 3)))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Max Velocity"), div(class = "kpi-value", ifelse(is.na(latest$max_velocity[[1]]), "-", round(latest$max_velocity[[1]], 2))))))
    )
  })
  output$profile_sprint_trend_plot <- renderPlotly({
    d <- profile_sprint_daily(); if (nrow(d) == 0) return(plotly_empty())
    d <- d %>% tidyr::pivot_longer(-test_date, names_to = "metric", values_to = "value") %>% filter(!is.na(value))
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~test_date, y = ~value, color = ~metric, type = "scatter", mode = "lines+markers") %>%
      layout(margin = list(l = 40, r = 10, t = 10, b = 40), xaxis = list(title = ""), yaxis = list(title = "Sprint value"))
  })
  output$profile_sprint_history_dt <- renderTable({
    pid <- selected_profile_id(); sp <- fd_sprint(); if (is.null(sp) || nrow(sp) == 0) return(data.frame(Message = "No SmartSpeed data for this athlete."))
    d <- sp %>% filter(athlete_id == pid) %>% arrange(desc(test_date)); if (nrow(d) == 0) return(data.frame(Message = "No SmartSpeed data for this athlete."))
    d %>% select(any_of(c("test_date", "test_name", "best_10yd", "best_30yd", "best_flying_10yd", "max_velocity", "total_time", "n_reps", "valid_reps", "source"))) %>%
      mutate(test_date = as.character(test_date))
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  observeEvent(input$profile_open_curve, {
    pid <- selected_profile_id()
    if (nzchar(pid)) { sync_all(athlete = pid); bslib::nav_select("main_nav", selected = "Curve View (Force Tracing)", session = session) }
  }, ignoreInit = TRUE)

  # ============================================================
  # CMJ Monitoring
  # ============================================================
  cmj_monitor_role_status <- reactive({
    st <- cmj_athlete_status_all()
    role_filter <- input$cmj_monitor_role %||% "All"
    if (role_filter != "All" && "roleGroup" %in% names(st)) st <- st %>% filter(roleGroup == role_filter)
    st
  })
  output$status_distribution_plot <- renderPlotly({
    st <- cmj_monitor_role_status(); if (is.null(st) || nrow(st) == 0) return(plotly_empty())
    include_nrd <- isTRUE(input$viz_show_no_recent %||% FALSE)
    lvls <- if (include_nrd) c("Red", "Yellow", "Blue", "Green", "No Recent Data") else c("Red", "Yellow", "Blue", "Green")
    d <- st %>% mutate(Status = factor(Status, levels = lvls)) %>% filter(Status %in% lvls) %>% count(roleGroup, Status, .drop = FALSE)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~roleGroup, y = ~n, color = ~Status, colors = c("#ef4444", "#f59e0b", "#2563eb", "#16a34a", "#6b7280"), type = "bar") %>%
      layout(barmode = "stack", margin = list(l = 40, r = 20, t = 10, b = 40), xaxis = list(title = ""), yaxis = list(title = "Athletes"))
  })
  cmj_daily_composite <- reactive({
    d <- cmj_monitor_long(); if (is.null(d) || nrow(d) == 0) return(tibble())
    wanted <- c("Contraction Time", "Concentric Impulse", "RSI-modified", "Jump Height (Imp-Mom)")
    d %>% filter(metric %in% wanted) %>% group_by(profileId, athleteName, externalId, roleGroup, metric, session_date) %>%
      summarise(value = max(value, na.rm = TRUE), .groups = "drop") %>% group_by(profileId, metric) %>%
      mutate(mu = mean(value, na.rm = TRUE), sig = sd(value, na.rm = TRUE), z = ifelse(is.na(sig) | sig == 0, 0, (value - mu) / sig)) %>% ungroup() %>%
      mutate(z_adj = ifelse(metric == "Contraction Time", -z, z)) %>% group_by(profileId, athleteName, externalId, roleGroup, session_date) %>%
      summarise(composite = mean(z_adj, na.rm = TRUE), n_metrics = sum(!is.na(z_adj)), .groups = "drop") %>% filter(n_metrics >= 2)
  })
  cmj_daily_composite_role <- reactive({
    d <- cmj_daily_composite(); if (is.null(d) || nrow(d) == 0) return(d)
    role_filter <- input$cmj_monitor_role %||% "All"
    if (role_filter != "All") d <- d %>% filter(roleGroup == role_filter)
    d
  })
  output$cmj_team_trend_plot <- renderPlotly({
    d <- cmj_daily_composite_role(); if (is.null(d) || nrow(d) == 0) return(plotly_empty())
    anchor <- max(d$session_date, na.rm = TRUE)
    d <- d %>% filter(session_date >= anchor - 59)
    if (nrow(d) == 0) return(plotly_empty())
    team <- d %>% group_by(session_date) %>% summarise(composite = mean(composite, na.rm = TRUE), .groups = "drop") %>% arrange(session_date)
    if (nrow(team) == 0) return(plotly_empty())
    plot_ly(team, x = ~session_date, y = ~composite, type = "scatter", mode = "lines+markers",
            line = list(color = TXST$maroon, width = 3), marker = list(color = TXST$maroon, size = 6),
            hovertemplate = "%{x}<br>%{y:.2f}<extra></extra>") %>%
      layout(margin = list(l = 45, r = 10, t = 10, b = 40), xaxis = list(title = ""),
             yaxis = list(title = "Team-average composite (z-score)", zeroline = TRUE))
  })
  output$cmj_coverage_plot <- renderPlotly({
    st <- cmj_monitor_role_status(); if (is.null(st) || nrow(st) == 0) return(plotly_empty())
    lvls <- c("High", "Moderate", "Low", "No Recent Data")
    d <- st %>% mutate(CMJ_Confidence = factor(CMJ_Confidence, levels = lvls)) %>% filter(!is.na(CMJ_Confidence)) %>% count(CMJ_Confidence, .drop = FALSE)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~CMJ_Confidence, y = ~n, type = "bar",
            marker = list(color = c("#16a34a", "#2563eb", "#f59e0b", "#6b7280"))) %>%
      layout(margin = list(l = 40, r = 20, t = 10, b = 40), xaxis = list(title = ""), yaxis = list(title = "Athletes"))
  })
  # HTML table built by hand (not renderTable/DT) so sparkline SVGs render as
  # markup instead of being escaped as literal text -- same pattern as the
  # Alert Inbox card.
  output$composite_sparklines_dt <- renderUI({
    d <- cmj_daily_composite_role(); if (is.null(d) || nrow(d) == 0) return(tags$p(class = "tiny subtle", "No composite trend rows available."))
    anchor <- max(d$session_date, na.rm = TRUE)
    make_win <- function(days) d %>% filter(session_date >= anchor - (days - 1), session_date <= anchor) %>% arrange(profileId, session_date) %>%
      group_by(profileId, athleteName, roleGroup) %>% summarise(start = dplyr::first(composite), last = dplyr::last(composite), delta = last - start, n_points = n(), .groups = "drop")
    trend_lbl <- function(x) dplyr::case_when(is.na(x) ~ "No data", x >= 0.25 ~ "Up", x <= -0.25 ~ "Down", TRUE ~ "Stable")
    spark28 <- d %>% filter(session_date >= anchor - 27, session_date <= anchor) %>% arrange(profileId, session_date) %>%
      group_by(profileId) %>% summarise(spark = sparkline_svg_html(composite), .groups = "drop")
    d7 <- make_win(7) %>% transmute(profileId, athleteName, roleGroup, d7_delta = delta, d7_last = last, d7_n = n_points)
    d14 <- make_win(14) %>% transmute(profileId, d14_delta = delta, d14_n = n_points)
    d28 <- make_win(28) %>% transmute(profileId, d28_delta = delta, d28_n = n_points)
    rows <- d7 %>% left_join(d14, by = "profileId") %>% left_join(d28, by = "profileId") %>% left_join(spark28, by = "profileId") %>%
      mutate(Trend = trend_lbl(d7_delta)) %>% arrange(roleGroup, athleteName)
    if (nrow(rows) == 0) return(tags$p(class = "tiny subtle", "No composite trend rows available."))
    trend_cls <- function(x) dplyr::case_when(x == "Up" ~ "trend-up", x == "Down" ~ "trend-down", TRUE ~ "trend-flat")
    fmt_delta <- function(x) if (is.na(x)) "--" else sprintf("%+.2f", x)
    body_rows <- vapply(seq_len(nrow(rows)), function(i) {
      spark <- rows$spark[[i]]
      sprintf(
        "<tr><td>%s</td><td>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s/%s/%s</td></tr>",
        htmltools::htmlEscape(rows$athleteName[[i]]), htmltools::htmlEscape(rows$roleGroup[[i]]),
        if (!is.na(spark) && nzchar(spark)) spark else "<span class='tiny subtle'>n/a</span>",
        trend_cls(rows$Trend[[i]]), htmltools::htmlEscape(rows$Trend[[i]]),
        fmt_delta(rows$d7_delta[[i]]), fmt_delta(rows$d14_delta[[i]]), fmt_delta(rows$d28_delta[[i]]),
        rows$d7_n[[i]] %||% 0L, rows$d14_n[[i]] %||% 0L, rows$d28_n[[i]] %||% 0L
      )
    }, character(1))
    HTML(paste0(
      "<table class='cmj-sparkline-table'><thead><tr>",
      "<th>Athlete</th><th>Role</th><th>Last 28 days</th><th>Trend</th><th>7d</th><th>14d</th><th>28d</th><th>Points (7/14/28)</th>",
      "</tr></thead><tbody>", paste(body_rows, collapse = ""), "</tbody></table>"
    ))
  })
  output$cmj_report_csv <- downloadHandler(
    filename = function() paste0("cmj_monitor_report_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      st <- cmj_monitor_role_status(); if (is.null(st) || nrow(st) == 0) { readr::write_csv(tibble(Message = "No CMJ monitoring rows available."), file); return(invisible()) }
      readr::write_csv(st, file)
    }
  )

  # ============================================================
  # Sprint / SmartSpeed
  # ============================================================
  sprint_scored <- reactive({
    sp <- fd_sprint(); if (is.null(sp) || nrow(sp) == 0) return(data.frame())
    tryCatch(score_smartspeed_performance(sp), error = function(e) data.frame())
  })
  sprint_filtered <- reactive({
    sp <- fd_sprint(); if (is.null(sp) || nrow(sp) == 0) return(data.frame())
    metric <- input$sprint_metric %||% "best_10yd"; if (!("test_date" %in% names(sp))) return(data.frame())
    sp <- sp %>% mutate(test_date = as_date_safely(test_date))
    max_date <- suppressWarnings(max(sp$test_date, na.rm = TRUE)); days <- suppressWarnings(as.numeric(input$sprint_window_days %||% 28))
    if (!is.na(max_date) && !is.na(days)) sp <- sp %>% filter(test_date >= max_date - days)
    role_filter <- input$sprint_role_filter %||% "All"; if (role_filter != "All" && "role" %in% names(sp)) sp <- sp %>% filter(role == role_filter)
    q <- trimws(input$sprint_search %||% "")
    if (nzchar(q)) { q_lower <- tolower(q); sp <- sp %>% filter(grepl(q_lower, tolower(athlete_name), fixed = TRUE) | grepl(q_lower, tolower(athlete_id), fixed = TRUE)) }
    if (!(metric %in% names(sp))) sp[[metric]] <- NA_real_
    # SmartSpeed logs one row per REP, and a session can have several (e.g. 3
    # x 10-yard sprint) -- including every rep as its own trend/mover point
    # means a single bad rep (false start, stumble, gate glitch) can get
    # picked up as "Latest" by pure row order and blow out the whole chart's
    # scale (seen with a genuine 5.68s 10-yard rep sitting next to 1.5s reps
    # from the same session). Collapse to each athlete's BEST rep per day for
    # whichever metric is currently selected -- lower is better for every
    # metric here except max_velocity.
    if (!("athlete_id" %in% names(sp)) || nrow(sp) == 0) return(sp)
    sp <- sp %>% filter(!is.na(.data[[metric]]))
    if (nrow(sp) == 0) return(sp)
    if (identical(metric, "max_velocity")) {
      sp <- sp %>% group_by(athlete_id, test_date) %>% slice_max(order_by = .data[[metric]], n = 1, with_ties = FALSE) %>% ungroup()
    } else {
      sp <- sp %>% group_by(athlete_id, test_date) %>% slice_min(order_by = .data[[metric]], n = 1, with_ties = FALSE) %>% ungroup()
    }
    sp
  })
  output$sprint_summary_ui <- renderUI({
    sp <- fd_sprint(); scored <- sprint_scored()
    if (is.null(sp) || nrow(sp) == 0) return(card(card_body(tags$div(class = "tiny subtle", "No SmartSpeed data available. The dashboard remains fully usable with CMJ data."))))
    max_dt <- if ("test_date" %in% names(sp)) suppressWarnings(max(as_date_safely(sp$test_date), na.rm = TRUE)) else as.Date(NA)
    athletes <- if ("athlete_id" %in% names(sp)) dplyr::n_distinct(sp$athlete_id) else 0L
    red_yellow <- if (nrow(scored) > 0 && "Sprint_Status" %in% names(scored)) sum(scored$Sprint_Status %in% c("Red", "Yellow")) else 0L
    layout_columns(col_widths = c(3, 3, 3, 3),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Sprint Rows"), div(class = "kpi-value", nrow(sp))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Athletes"), div(class = "kpi-value", athletes)))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Latest Sprint Date"), div(class = "kpi-value", as.character(max_dt))))),
      card(card_body(div(class = "kpi", div(class = "kpi-title", "Red/Yellow Sprint"), div(class = "kpi-value", red_yellow))))
    )
  })
  output$sprint_latest_dt <- renderTable({
    sp <- sprint_filtered(); if (is.null(sp) || nrow(sp) == 0) return(data.frame(Message = "No SmartSpeed rows match current filters."))
    scored <- sprint_scored()
    latest <- sp %>% filter(!is.na(test_date)) %>% group_by(athlete_id) %>% filter(test_date == max(test_date, na.rm = TRUE)) %>%
      summarise(athlete_name = dplyr::first(athlete_name), role = dplyr::first(role), test_date = dplyr::first(test_date), source = dplyr::first(source),
                best_10yd = if (all(is.na(best_10yd))) NA_real_ else stats::na.omit(best_10yd)[[1]],
                best_30yd = if (all(is.na(best_30yd))) NA_real_ else stats::na.omit(best_30yd)[[1]],
                best_flying_10yd = if (all(is.na(best_flying_10yd))) NA_real_ else stats::na.omit(best_flying_10yd)[[1]],
                max_velocity = if (all(is.na(max_velocity))) NA_real_ else stats::na.omit(max_velocity)[[1]],
                total_time = if (all(is.na(total_time))) NA_real_ else stats::na.omit(total_time)[[1]], .groups = "drop")
    if (nrow(scored) > 0) latest <- latest %>% left_join(scored, by = c("athlete_id", "athlete_name", "role"))
    for (nm in c("Sprint_Status", "Sprint_Confidence")) if (!(nm %in% names(latest))) latest[[nm]] <- "No Recent Data"
    latest %>% transmute(Athlete = athlete_name, Role = role, Date = as.character(test_date), `Sprint Status` = Sprint_Status, Confidence = Sprint_Confidence,
                          `Best 10yd` = best_10yd, `Best 30yd` = best_30yd, `Flying 10yd` = best_flying_10yd, `Max Velocity` = max_velocity, `Total Time` = total_time, Source = source)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  # Dedicated to the Sprint Trend chart -- always best_10yd, and independent
  # of the sidebar's role/search/metric filters used by the Movers/Latest
  # tables below. Best rep per athlete per day (lower is better), same
  # robustness reasoning as sprint_filtered()'s per-day collapse.
  sprint_trend_source <- reactive({
    sp <- fd_sprint(); if (is.null(sp) || nrow(sp) == 0 || !all(c("test_date", "best_10yd", "athlete_id") %in% names(sp))) return(tibble())
    sp <- sp %>% mutate(test_date = as_date_safely(test_date)) %>% filter(!is.na(test_date), !is.na(best_10yd))
    if (nrow(sp) == 0) return(tibble())
    max_date <- suppressWarnings(max(sp$test_date, na.rm = TRUE)); days <- suppressWarnings(as.numeric(input$sprint_window_days %||% 28))
    if (!is.na(max_date) && !is.na(days)) sp <- sp %>% filter(test_date >= max_date - days)
    role_filter <- input$sprint_role_filter %||% "All"; if (role_filter != "All" && "role" %in% names(sp)) sp <- sp %>% filter(role == role_filter)
    if (nrow(sp) == 0) return(tibble())
    sp %>% group_by(athlete_id, athlete_name, test_date) %>% slice_min(order_by = best_10yd, n = 1, with_ties = FALSE) %>% ungroup()
  })
  output$sprint_trend_add_athlete_ui <- renderUI({
    d <- sprint_trend_source()
    choices <- if (nrow(d) > 0) { a <- d %>% distinct(athlete_id, athlete_name) %>% arrange(athlete_name); setNames(a$athlete_id, a$athlete_name) } else c()
    selectizeInput("sprint_trend_athletes", NULL, choices = choices, selected = isolate(input$sprint_trend_athletes), multiple = TRUE,
                    options = list(placeholder = "Add players to compare against team average...", plugins = list("remove_button")))
  })
  output$sprint_trend_plot <- renderPlotly({
    d <- sprint_trend_source(); if (is.null(d) || nrow(d) == 0) return(plotly_empty())
    team_avg <- d %>% group_by(test_date) %>% summarise(best_10yd = mean(best_10yd, na.rm = TRUE), .groups = "drop") %>% arrange(test_date)
    picked <- input$sprint_trend_athletes %||% character(0)
    # Built as explicit traces (not a single color=~ grouped call): a formula
    # like line=list(width=~ifelse(athlete_name=="Team Average",4,2)) doesn't
    # scope per-trace the way it looks like it should -- it gets evaluated
    # once across the whole combined data and applied identically to every
    # trace, so both Team Average and every player ended up with the same
    # garbled per-point width array instead of one bold line + thinner ones.
    p <- plot_ly() %>%
      add_trace(data = team_avg, x = ~test_date, y = ~best_10yd, name = "Team Average", type = "scatter", mode = "lines+markers",
                line = list(color = TXST$maroon, width = 4), marker = list(color = TXST$maroon, size = 7),
                hovertemplate = "Team Average<br>%{x}<br>%{y:.2f}s<extra></extra>")
    if (length(picked) > 0) {
      palette <- c("#2563eb", "#16a34a", "#f59e0b", "#8b5cf6", "#ef4444", "#0891b2")
      athletes <- d %>% filter(athlete_id %in% picked) %>% distinct(athlete_id, athlete_name) %>% arrange(athlete_name)
      for (i in seq_len(nrow(athletes))) {
        dd <- d %>% filter(athlete_id == athletes$athlete_id[[i]]) %>% arrange(test_date)
        col <- palette[[((i - 1) %% length(palette)) + 1]]
        p <- p %>% add_trace(data = dd, x = ~test_date, y = ~best_10yd, name = athletes$athlete_name[[i]], type = "scatter", mode = "lines+markers",
                              line = list(color = col, width = 2), marker = list(color = col, size = 5),
                              hovertemplate = paste0(athletes$athlete_name[[i]], "<br>%{x}<br>%{y:.2f}s<extra></extra>"))
      }
    }
    p %>% layout(margin = list(l = 45, r = 10, t = 10, b = 40), xaxis = list(title = ""), yaxis = list(title = "Best 10-yard (s)"))
  })
  output$sprint_movers_dt <- renderTable({
    sp <- sprint_filtered(); metric <- input$sprint_metric %||% "best_10yd"
    if (is.null(sp) || nrow(sp) == 0 || !(metric %in% names(sp))) return(data.frame(Message = "No sprint mover rows available."))
    out <- sp %>% filter(!is.na(.data[[metric]])) %>% arrange(athlete_id, test_date) %>% group_by(athlete_id, athlete_name, role) %>%
      summarise(First = dplyr::first(.data[[metric]]), Latest = dplyr::last(.data[[metric]]), Change = Latest - First, Points = n(), .groups = "drop") %>%
      filter(!is.na(Change)) %>% arrange(desc(abs(Change))) %>% slice_head(n = 20)
    if (nrow(out) == 0) return(data.frame(Message = "No sprint mover rows available."))
    out %>% mutate(First = round(First, 3), Latest = round(Latest, 3), Change = round(Change, 3)) %>%
      transmute(Athlete = athlete_name, Role = role, First, Latest, Change, Points)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")

  # ============================================================
  # Curve View (Force Tracing) -- needs live VALD credentials
  # ============================================================
  output$curve_athlete_ui <- renderUI({
    a <- athlete_choices(); dflt <- default_athlete_id(a); cur <- isolate(input$curve_athlete)
    sel <- if (!is.null(cur) && cur %in% a$profileId) cur else dflt
    selectInput("curve_athlete", "Athlete", choices = setNames(a$profileId, a$label), selected = sel)
  })
  output$curve_testtype_ui <- renderUI({
    tt <- athlete_test_types(curve_athlete_debounced() %||% ""); cur <- isolate(input$curve_testType); sel <- preferred_test_type(tt, cur)
    selectInput("curve_testType", "Test type", choices = tt, selected = sel)
  })
  output$curve_metric_ui <- renderUI({
    m <- metric_choices(); cur <- isolate(input$curve_metric); default_metric <- m[grepl("Peak", m, ignore.case = TRUE)][1] %||% m[1] %||% ""
    sel <- if (!is.null(cur) && cur %in% m) cur else default_metric
    selectInput("curve_metric", "Metric (best rep based on)", choices = m, selected = sel)
  })
  curve_role_defaults <- reactive({
    is_pitcher <- athlete_is_pitcher(curve_athlete_debounced() %||% "")
    if (is_pitcher) list(metric_prefs = c("Braking Phase Duration", "RSI-modified", "Jump Height (Imp-Mom)"), compare_mode = "sessions")
    else list(metric_prefs = c("RSI-modified", "Peak Power / BM", "Countermovement Depth"), compare_mode = "single")
  })
  observeEvent(curve_athlete_debounced(), {
    defs <- curve_role_defaults(); m <- metric_choices()
    target_metric <- defs$metric_prefs[defs$metric_prefs %in% m][1] %||% m[1] %||% ""
    if (nzchar(target_metric)) updateSelectInput(session, "curve_metric", selected = target_metric)
    updateSelectInput(session, "curve_session_compare_mode", selected = defs$compare_mode %||% "single")
  }, ignoreInit = TRUE)

  output$curve_assign_ui <- renderUI({
    if ((input$curve_session_compare_mode %||% "single") != "sessions") return(NULL)
    radioButtons("curve_assign_to", "Click assigns to", choices = c("Session A" = "A", "Session B" = "B"), selected = "A", inline = TRUE)
  })

  cache <- reactiveValues(rec_df = list(), trials_df = list())
  get_cached <- function(test_id) {
    if (is.null(cache$rec_df[[test_id]]) || is.null(cache$trials_df[[test_id]])) {
      token <- vald_get_token()
      rec <- fd_get_recording(token, team_id, test_id); tr <- fd_get_trials(token, team_id, test_id)
      req(!is.null(rec), !is.null(tr))
      df <- as_tibble(rec$recordingData, .name_repair = "minimal"); names(df) <- rec$recordingDataHeader
      req("Time" %in% names(df))
      left_col <- names(df)[grepl("Left", names(df), ignore.case = TRUE)][1]; right_col <- names(df)[grepl("Right", names(df), ignore.case = TRUE)][1]
      req(!is.na(left_col), !is.na(right_col))
      df <- df %>% rename(time_s = Time) %>% mutate(force_left = .data[[left_col]], force_right = .data[[right_col]], force_total = force_left + force_right)
      cache$rec_df[[test_id]] <- df; cache$trials_df[[test_id]] <- tr
    }
    list(df = cache$rec_df[[test_id]], tr = cache$trials_df[[test_id]])
  }

  metric_key_for_best <- reactive({
    tl <- fd_trials_long(); req(!is.null(tl), nrow(tl) > 0); req(input$curve_metric)
    if ("metricName" %in% names(tl)) {
      mk <- tl %>% filter(metricName == input$curve_metric) %>% distinct(metricKey) %>% slice(1) %>% pull(metricKey)
      mk %||% input$curve_metric
    } else input$curve_metric
  })
  curve_timeline <- reactive({
    tests <- fd_tests(); req(!is.null(tests), nrow(tests) > 0)
    a <- curve_athlete_debounced(); req(a, input$curve_testType)
    tests %>% select(any_of(c("testId", "profileId", "testType", "recordedDateUtc", "notes", "externalId", "athleteName"))) %>%
      mutate(notes = ifelse(is.na(notes), "", notes), recordedUTC_dt = suppressWarnings(lubridate::as_datetime(recordedDateUtc, tz = "UTC")),
             session_date = as.Date(recordedUTC_dt), session_time = ifelse(!is.na(recordedUTC_dt), format(recordedUTC_dt, "%H:%M"), ""),
             session_label = ifelse(!is.na(recordedUTC_dt), paste0(session_date, " (", session_time, " UTC)"), "(unknown time)")) %>%
      filter(profileId == a, testType == input$curve_testType) %>% arrange(desc(recordedUTC_dt))
  })
  curve_timeline_view <- reactive({
    tl <- fd_trials_long(); tests <- curve_timeline(); if (is.null(tests) || nrow(tests) == 0) return(tibble())
    out <- tests %>% transmute(testId = as.character(testId), session_label = as.character(session_label), session_date = as.character(session_date),
                                session_time = as.character(session_time), notes = as.character(ifelse(is.na(notes), "", notes)))
    out$metric_available <- FALSE
    if (!is.null(tl) && nrow(tl) > 0 && all(c("testId", "metricKey") %in% names(tl))) {
      mk <- metric_key_for_best()
      avail_ids <- tl %>% filter(testId %in% out$testId, metricKey == mk, trialLimb == "Both") %>% distinct(testId) %>% pull(testId) %>% as.character()
      out$metric_available <- out$testId %in% avail_ids
    }
    out$metric_flag <- ifelse(out$metric_available, "Y", "")
    out
  })
  selected_testA <- reactiveVal(NULL); selected_testB <- reactiveVal(NULL)
  # ignoreInit must be FALSE (the default) here: curve_timeline() typically
  # settles from its req()-failed startup state to its real value in a single
  # step, so ignoreInit=TRUE was skipping that one and only firing -- Session
  # A/B never got auto-picked for whichever athlete Curve View opened to,
  # leaving curve_plot permanently blank until the user switched athletes or
  # clicked a session row by hand.
  observeEvent(curve_timeline(), {
    tt <- curve_timeline()
    if (is.null(tt) || nrow(tt) == 0) { selected_testA(NULL); selected_testB(NULL) }
    else { selected_testA(as.character(tt$testId[1])); selected_testB(if (nrow(tt) >= 2) as.character(tt$testId[2]) else NULL) }
  })
  # Manual clickable HTML table -- see note above alert_inbox_dt.
  output$curve_session_timeline <- renderUI({
    dat <- curve_timeline_view(); if (is.null(dat) || nrow(dat) == 0) return(tags$div(class = "tiny subtle", "No sessions for this athlete/test type."))
    a <- selected_testA(); b <- selected_testB(); n <- nrow(dat)
    isA <- if (!is.null(a) && nzchar(a)) dat$testId == a else rep(FALSE, n); isB <- if (!is.null(b) && nzchar(b)) dat$testId == b else rep(FALSE, n)
    show <- dat %>% mutate(AB = dplyr::case_when(isA ~ "A", isB ~ "B", TRUE ~ ""), Notes = ifelse(is.na(notes), "", notes))
    header <- "<tr><th>AB</th><th>Metric</th><th>Session</th><th>Notes</th></tr>"
    body_rows <- vapply(seq_len(nrow(show)), function(i) {
      r <- show[i, ]
      paste0(
        "<tr style='cursor:pointer;' onclick='Shiny.setInputValue(", "\"curve_row_click\"", ", ", jsonlite::toJSON(as.character(r$testId), auto_unbox = TRUE), ", {priority:\"event\"})'>",
        "<td style='font-weight:700;'>", html_esc(r$AB), "</td><td>", html_esc(r$metric_flag), "</td><td>", html_esc(r$session_label), "</td><td>", html_esc(r$Notes), "</td></tr>"
      )
    }, character(1))
    HTML(paste0(
      "<div style='overflow-x:auto;max-height:320px;overflow-y:auto;'><table class='table table-striped table-hover' style='width:100%;font-size:12.5px;'>",
      "<thead>", header, "</thead><tbody>", paste(body_rows, collapse = ""), "</tbody></table></div>"
    ))
  })
  observeEvent(input$curve_row_click, {
    dat <- curve_timeline_view(); clicked <- input$curve_row_click %||% ""
    req(nzchar(clicked), clicked %in% dat$testId)
    mode <- input$curve_session_compare_mode %||% "single"
    if (mode == "sessions") { assign_to <- input$curve_assign_to %||% "A"; if (assign_to == "A") selected_testA(clicked) else selected_testB(clicked) }
    else { selected_testA(clicked); selected_testB(NULL) }
  }, ignoreInit = TRUE)
  observeEvent(input$curve_clear_b, { selected_testB(NULL) })
  observeEvent(input$curve_swap_ab, { a <- selected_testA(); b <- selected_testB(); if (!is.null(a) && !is.null(b) && nzchar(a) && nzchar(b)) { selected_testA(b); selected_testB(a) } })
  test_label <- function(test_id) {
    tt <- curve_timeline(); if (is.null(test_id) || !nzchar(test_id)) return("-")
    row <- tt %>% filter(testId == test_id) %>% slice(1); if (nrow(row) == 0) return(test_id); row$session_label[[1]]
  }
  output$curve_selected_summary <- renderUI(tagList(tags$div(tags$b("Session A: "), test_label(selected_testA())), tags$div(tags$b("Session B: "), if (!is.null(selected_testB())) test_label(selected_testB()) else "-")))
  trial_index_map <- function(test_id) {
    tl <- fd_trials_long(); req(!is.null(tl), nrow(tl) > 0); req(!is.null(test_id), nzchar(test_id))
    tl %>% filter(testId == test_id) %>% distinct(trialId, trialStartTime, trialEndTime) %>% arrange(trialStartTime, trialEndTime, trialId) %>%
      mutate(trial_num = row_number(), trial_label = paste0("Trial ", trial_num, " (", sprintf("%.2f", trialStartTime), "-", sprintf("%.2f", trialEndTime), "s)"))
  }
  trial_label_for_id <- function(trial_id, mp) { if (is.null(trial_id) || !nzchar(trial_id) || is.null(mp) || nrow(mp) == 0) return("Trial ?"); lab <- mp$trial_label[match(trial_id, mp$trialId)]; ifelse(is.na(lab), "Trial ?", lab) }
  output$curve_rep_A_ui <- renderUI({
    if ((input$curve_rep_rule_A %||% "best") != "manual") return(NULL)
    testA <- selected_testA(); req(!is.null(testA), nzchar(testA)); mp <- trial_index_map(testA)
    selectInput("curve_trial_manual_A", paste0("Pick Curve A rep (", test_label(testA), ")"), choices = setNames(mp$trialId, mp$trial_label), selected = mp$trialId[1] %||% "")
  })
  output$curve_B_controls <- renderUI({
    mode <- input$curve_session_compare_mode %||% "single"; testA <- selected_testA(); testB <- selected_testB()
    if (mode == "single") return(tags$div(class = "tiny subtle", "No Curve B in Single session mode."))
    if (mode == "trials") {
      req(!is.null(testA), nzchar(testA)); mp <- trial_index_map(testA)
      tagList(selectInput("curve_rep_rule_B", "Curve B rep (same session)", choices = c("Choose rep manually" = "manual"), selected = "manual"),
              selectInput("curve_trial_manual_B", paste0("Pick Curve B rep (", test_label(testA), ")"), choices = setNames(mp$trialId, mp$trial_label), selected = mp$trialId[2] %||% mp$trialId[1] %||% ""))
    } else {
      tagList(
        tags$div(style = "margin-bottom:6px;", if (is.null(testB) || !nzchar(testB)) tags$span(style = paste0("color:", TXST$red, "; font-weight:700;"), "Session B not set yet - choose 'Session B' then click a row.") else tags$span(style = paste0("color:", TXST$green, "; font-weight:700;"), "Session B set")),
        selectInput("curve_rep_rule_B", "Curve B rep (Session B)", choices = c("Best rep (by selected metric)" = "best", "Choose rep manually" = "manual"), selected = "best"),
        uiOutput("curve_rep_B_ui")
      )
    }
  })
  output$curve_rep_B_ui <- renderUI({
    mode <- input$curve_session_compare_mode %||% "single"; if (mode != "sessions") return(NULL)
    if ((input$curve_rep_rule_B %||% "best") != "manual") return(NULL)
    testB <- selected_testB(); req(!is.null(testB), nzchar(testB)); mp <- trial_index_map(testB)
    selectInput("curve_trial_manual_B_sessions", paste0("Pick Curve B rep (", test_label(testB), ")"), choices = setNames(mp$trialId, mp$trial_label), selected = mp$trialId[1] %||% "")
  })
  build_segment_cached <- function(test_id, trial_id, limb = "total") {
    obj <- get_cached(test_id); df <- obj$df; tr <- obj$tr; req(!is.null(tr))
    w <- tr %>% filter(id == trial_id) %>% slice(1); req(nrow(w) == 1)
    seg <- slice_curve_to_trial(df, w$startTime, w$endTime); req(nrow(seg) > 2)
    ycol <- switch(limb, left = "force_left", right = "force_right", total = "force_total", "force_total")
    out <- seg %>% transmute(time_s = time_s, y = .data[[ycol]]); out <- out %>% mutate(x = time_s - suppressWarnings(min(time_s, na.rm = TRUE))); out %>% select(x, y)
  }
  build_segment_safe <- function(test_id, trial_id, limb = "total") tryCatch(build_segment_cached(test_id, trial_id, limb = limb), error = function(e) NULL)
  metric_value_for_trial <- function(test_id, trial_id, metric_key, limb_metric = "Both") {
    tl <- fd_trials_long(); if (is.null(tl) || nrow(tl) == 0) return(NA_real_)
    if (is.null(test_id) || is.null(trial_id) || is.null(metric_key)) return(NA_real_)
    v <- tl %>% filter(testId == test_id, trialId == trial_id, metricKey == metric_key, trialLimb == limb_metric) %>% summarise(val = suppressWarnings(max(value, na.rm = TRUE))) %>% pull(val)
    if (length(v) == 0) NA_real_ else as.numeric(v)
  }
  curve_selection_info <- reactive({
    tl <- fd_trials_long(); req(!is.null(tl), nrow(tl) > 0)
    mode <- input$curve_session_compare_mode %||% "single"; mk <- metric_key_for_best()
    testA <- selected_testA(); req(!is.null(testA), nzchar(testA))
    trial_metrics_A <- tl %>% filter(testId == testA)
    trialA <- if ((input$curve_rep_rule_A %||% "best") == "manual") input$curve_trial_manual_A else pick_best_trial_id(trial_metrics_A, best_metric_key = mk, limb = "Both")
    req(!is.null(trialA), nzchar(trialA))
    testB <- NULL; trialB <- NULL
    if (mode == "trials") { testB <- testA; trialB <- input$curve_trial_manual_B; req(!is.null(trialB), nzchar(trialB)) }
    if (mode == "sessions") {
      testB <- selected_testB(); req(!is.null(testB), nzchar(testB))
      trial_metrics_B <- tl %>% filter(testId == testB)
      trialB <- if ((input$curve_rep_rule_B %||% "best") == "manual") input$curve_trial_manual_B_sessions else pick_best_trial_id(trial_metrics_B, best_metric_key = mk, limb = "Both")
      req(!is.null(trialB), nzchar(trialB))
    }
    list(mode = mode, mk = mk, testA = testA, trialA = trialA, testB = testB, trialB = trialB)
  })
  curve_delta_values <- reactive({
    info <- curve_selection_info(); valA <- metric_value_for_trial(info$testA, info$trialA, info$mk, limb_metric = "Both")
    valB <- if (!is.null(info$testB) && !is.null(info$trialB)) metric_value_for_trial(info$testB, info$trialB, info$mk, limb_metric = "Both") else NA_real_
    delta <- ifelse(is.na(valA) || is.na(valB), NA_real_, valB - valA)
    pct <- ifelse(is.na(delta) || is.na(valA) || valA == 0, NA_real_, 100 * delta / abs(valA))
    tibble(metricKey = info$mk, valA = valA, valB = valB, delta = delta, pct_delta = pct)
  })
  curve_quality_tbl <- reactive({
    info <- curve_selection_info()
    check_one <- function(label, test_id, trial_id) {
      if (is.null(test_id) || is.null(trial_id)) return(tibble(Session = label, Samples = NA_integer_, Duration_s = NA_real_, PeakForce = NA_real_, MetricAvailable = "N", Status = "Not selected"))
      seg <- build_segment_safe(test_id, trial_id, limb = "total"); n_samples <- if (is.null(seg)) 0L else nrow(seg)
      duration_s <- if (is.null(seg) || nrow(seg) == 0) NA_real_ else suppressWarnings(max(seg$x, na.rm = TRUE) - min(seg$x, na.rm = TRUE))
      peak_force <- if (is.null(seg) || nrow(seg) == 0) NA_real_ else suppressWarnings(max(seg$y, na.rm = TRUE))
      metric_ok <- !is.na(metric_value_for_trial(test_id, trial_id, info$mk, limb_metric = "Both"))
      status <- dplyr::case_when(n_samples < 5 ~ "Fail: too few samples", is.na(peak_force) || !is.finite(peak_force) || peak_force <= 0 ~ "Check force channel",
                                  is.na(duration_s) || duration_s < 0.20 ~ "Check: short trial", !metric_ok ~ "Check: metric missing", TRUE ~ "OK")
      tibble(Session = label, Samples = n_samples, Duration_s = duration_s, PeakForce = peak_force, MetricAvailable = ifelse(metric_ok, "Y", "N"), Status = status)
    }
    bind_rows(check_one("A", info$testA, info$trialA), check_one("B", info$testB, info$trialB))
  })
  output$curve_quality_ui <- renderUI({
    q <- curve_quality_tbl(); if (is.null(q) || nrow(q) == 0) return(tags$div(class = "tiny subtle", "No quality checks available."))
    tagList(lapply(seq_len(nrow(q)), function(i) {
      status <- q$Status[[i]]; col <- if (grepl("^OK", status)) TXST$green else if (grepl("^Check", status)) TXST$yellow else TXST$red
      tags$div(style = "margin-bottom:6px;", tags$span(style = "font-weight:700;", paste0(q$Session[[i]], ": ")), tags$span(style = paste0("color:", col, "; font-weight:700;"), status),
                tags$span(class = "tiny subtle", paste0(" (n=", q$Samples[[i]], ", dur=", ifelse(is.na(q$Duration_s[[i]]), "NA", sprintf('%.2f', q$Duration_s[[i]])), "s)")))
    }))
  })
  output$curve_delta_ui <- renderUI({
    d <- curve_delta_values(); req(nrow(d) > 0); fmt <- function(x) ifelse(is.na(x), "NA", format(round(x, 2), nsmall = 2))
    tagList(tags$div(tags$b("Metric: "), as.character(d$metricKey[[1]])), tags$div(tags$b("A: "), fmt(d$valA[[1]])), tags$div(tags$b("B: "), fmt(d$valB[[1]])),
             tags$div(tags$b("Delta (B-A): "), fmt(d$delta[[1]])), tags$div(tags$b("% Delta: "), ifelse(is.na(d$pct_delta[[1]]), "NA", paste0(fmt(d$pct_delta[[1]]), "%"))))
  })
  output$curve_snapshot_csv <- downloadHandler(
    filename = function() paste0("curve_snapshot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      info <- curve_selection_info(); d <- curve_delta_values()
      out <- tibble(generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), athlete_profileId = input$curve_athlete %||% "", testType = input$curve_testType %||% "",
                    compare_mode = info$mode, metric_key = info$mk, sessionA = test_label(info$testA), trialA = info$trialA,
                    sessionB = ifelse(is.null(info$testB), "", test_label(info$testB)), trialB = info$trialB %||% "",
                    metric_value_A = d$valA[[1]], metric_value_B = d$valB[[1]], delta_B_minus_A = d$delta[[1]], pct_delta = d$pct_delta[[1]])
      readr::write_csv(out, file)
    }
  )
  output$curve_plot <- renderPlot({
    info <- curve_selection_info(); normalize_force <- isTRUE(input$curve_normalize_force); show_all_limbs <- isTRUE(input$curve_show_multiples); shared_y <- isTRUE(input$curve_shared_y)
    limbs <- if (show_all_limbs) c("total", "left", "right") else (input$curve_limb %||% "total")
    limb_label <- function(l) switch(l, left = "Left", right = "Right", total = "Total", l)
    prep_seg <- function(seg) { if (is.null(seg) || nrow(seg) == 0) return(NULL); out <- seg; if (normalize_force) { peak <- suppressWarnings(max(abs(out$y), na.rm = TRUE)); if (is.finite(peak) && peak > 0) out$y <- 100 * out$y / peak }; out }
    seg_store <- lapply(limbs, function(limb) {
      segA <- prep_seg(build_segment_safe(info$testA, info$trialA, limb = limb)); segB <- NULL
      if (!is.null(info$testB) && !is.null(info$trialB)) segB <- prep_seg(build_segment_safe(info$testB, info$trialB, limb = limb))
      list(limb = limb, A = segA, B = segB)
    })
    names(seg_store) <- limbs
    valid_panels <- vapply(seg_store, function(x) !is.null(x$A), logical(1))
    if (!any(valid_panels)) {
      # Reset to a single full-size panel before this fallback plot.new():
      # a PREVIOUS successful render can leave par(mfrow=...) set to several
      # rows (e.g. "show all limbs"), and drawing into that stale multi-panel
      # layout with the default margins is what throws "figure margins too
      # large" here instead of showing this message.
      op0 <- par(mfrow = c(1, 1), mar = c(4, 4, 3, 1)); on.exit(par(op0), add = TRUE)
      plot.new(); text(0.5, 0.5, "Unable to build curve segments for current selection.", cex = 1.1); return(invisible(NULL))
    }
    y_global <- NULL
    if (shared_y) { yy <- unlist(lapply(seg_store[valid_panels], function(x) c(x$A$y, if (!is.null(x$B)) x$B$y else numeric(0)))); if (length(yy) > 0) y_global <- range(yy, na.rm = TRUE) }
    op <- par(mfrow = c(length(limbs), 1), mar = c(4, 4, 3, 1)); on.exit(par(op), add = TRUE)
    mpA <- trial_index_map(info$testA); labA <- trial_label_for_id(info$trialA, mpA); labB <- ""
    if (!is.null(info$testB) && !is.null(info$trialB)) { if (info$mode == "trials") labB <- trial_label_for_id(info$trialB, mpA) else { mpB <- trial_index_map(info$testB); labB <- trial_label_for_id(info$trialB, mpB) } }
    for (limb in limbs) {
      item <- seg_store[[limb]]; segA <- item$A; segB <- item$B
      if (is.null(segA)) { plot.new(); text(0.5, 0.5, paste("No data for", limb_label(limb), "curve."), cex = 1.0); next }
      x_all <- segA$x; y_all <- segA$y; if (!is.null(segB)) { x_all <- c(x_all, segB$x); y_all <- c(y_all, segB$y) }
      xlim <- range(x_all, na.rm = TRUE); ylim <- if (!is.null(y_global)) y_global else range(y_all, na.rm = TRUE)
      ylab <- if (normalize_force) "% Peak Force" else paste0("Force (", limb_label(limb), ")"); xlab <- "Time (s) from trial start"
      plot(segA$x, segA$y, type = "l", col = "#1f77b4", lwd = 2, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = paste0(limb_label(limb), " | A: ", test_label(info$testA), " - ", labA))
      pkA <- which.max(segA$y); if (length(pkA) == 1 && is.finite(pkA)) { abline(v = segA$x[pkA], col = "#1f77b4", lty = 3); points(segA$x[pkA], segA$y[pkA], pch = 16, col = "#1f77b4") }
      if (!is.null(segB)) {
        lines(segB$x, segB$y, col = "#d62728", lwd = 2)
        pkB <- which.max(segB$y); if (length(pkB) == 1 && is.finite(pkB)) { abline(v = segB$x[pkB], col = "#d62728", lty = 3); points(segB$x[pkB], segB$y[pkB], pch = 16, col = "#d62728") }
        legend("topright", legend = c(paste0("A (", labA, ")"), paste0("B (", labB, ")"), "Peak time markers"), col = c("#1f77b4", "#d62728", "#6b7280"), lty = c(1, 1, 3), lwd = c(2, 2, 1), bty = "n")
      } else title(sub = "Choose Two trials or Two sessions for A/B compare")
    }
  })
  output$curve_metric_bar <- renderPlot({
    info <- curve_selection_info(); d <- curve_delta_values(); mk <- info$mk; valA <- d$valA[[1]]; valB <- d$valB[[1]]
    labs <- c(paste0("A\n", test_label(info$testA))); vals <- c(valA)
    if (!is.null(info$testB) && !is.null(info$trialB)) { labs <- c(labs, paste0("B\n", test_label(info$testB))); vals <- c(vals, valB) }
    op <- par(mar = c(7, 4, 3, 1)); on.exit(par(op), add = TRUE)
    y_max <- suppressWarnings(max(vals, na.rm = TRUE)); if (!is.finite(y_max)) y_max <- 0; ylim_top <- y_max * 1.25 + ifelse(y_max == 0, 1, 0)
    bp <- barplot(vals, names.arg = labs, las = 1, ylim = c(0, ylim_top), ylab = paste0("Value (", mk, ")"), main = "Selected rep metric (used for Best rep)")
    text(bp, vals, labels = ifelse(is.na(vals), "NA", format(round(vals, 2), nsmall = 2)), pos = 3, cex = 1.0)
  })
  # Curve View is not the default tab, so these two base-R plots would
  # otherwise take their first renderPlot() pass while their container is
  # still display:none (0x0), leaving them blank until the user leaves the
  # tab and comes back. suspendWhenHidden=FALSE forces the real render to
  # happen as soon as the data is ready instead of deferring to a visibility
  # change that isn't reliably firing for this nested tab structure.
  outputOptions(output, "curve_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "curve_metric_bar", suspendWhenHidden = FALSE)
}

if (!identical(Sys.getenv("VALD_REFRESH_WORKER"), "true") && !BASE_PLAYER_HEALTH_EMBEDDED) {
  shinyApp(ui, server)
}
