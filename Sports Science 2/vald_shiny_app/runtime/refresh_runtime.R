# One complete snapshot is published only after a successful ForceDecks rebuild.
# Viewers never read intermediate pipeline files during a refresh.
refresh_dir <- Sys.getenv(
  "VALD_REFRESH_DIR",
  file.path(Sys.getenv("VALD_APP_ROOT", getwd()), "data", "refresh")
)
dir.create(refresh_dir, recursive = TRUE, showWarnings = FALSE)
snapshot_file <- file.path(refresh_dir, "dashboard.rds")
state_file <- file.path(refresh_dir, "status.rds")
lock_file <- file.path(refresh_dir, "refresh.lock")
atomic_rds <- function(value, path) {
  tmp <- tempfile(".publish-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(value, tmp)
  if (!file.rename(tmp, path)) stop("Could not publish ", basename(path))
}

# callr's default environment is deliberately minimal.  The live refresh needs
# the deployment's credentials and storage paths, so pass only the variables
# used by this pipeline instead of relying on ambient process inheritance.
refresh_worker_environment <- function() {
  names <- c(
    "VALD_CLIENT_ID", "VALD_CLIENT_SECRET", "VALD_TEAM_ID", "VALD_REGION",
    "VALD_USERNAME", "VALD_PASSWORD", "VALD_TENANT_ID", "VALD_DUENDE_ID",
    "VALD_TOKEN_URL", "VALD_REFRESH_DIR", "VALD_REFRESH_HOURS",
    "VALD_FORCE_FULL_REPULL_TESTS", "VALD_FORCE_FULL_REPULL_METRICS",
    "VALD_FORCE_FULL_REPULL_SMARTSPEED", "VALD_METRICS_PULL_MAX_ACTIVE",
    "VALD_RUN_FULL_PULL_FIRST", "VALD_RUN_VALDR_PROFILE_PULL",
    "CMJ_SPRINT_SHARE_GOLD_DIR", "CMJ_SPRINT_SHARE_LEGACY_DIR",
    "SMARTSPEED_FILE", "PLAYER_HEALTH_DATA_DIR", "FALL_ROSTER_FILE"
  )
  values <- Sys.getenv(names, unset = NA_character_, names = TRUE)
  values[!is.na(values)]
}

dashboard_snapshot_ready <- function(value) {
  is.list(value) &&
    is.data.frame(value$roster) && nrow(value$roster) > 0L &&
    is.data.frame(value$session_summary) && nrow(value$session_summary) > 0L
}

refresh_state_cache <- new.env(parent = emptyenv())
read_refresh_state <- function() {
  key <- normalizePath(state_file, mustWork = FALSE)
  cached <- refresh_state_cache[[key]]
  empty <- list(message = "Not refreshed yet", attempted = as.POSIXct(NA), success = as.POSIXct(NA))
  if (!file.exists(state_file) && is.null(cached)) return(empty)
  state <- tryCatch({
    value <- suppressWarnings(readRDS(state_file))
    valid_time <- function(x) is.null(x) || (inherits(x, "POSIXt") && length(x) == 1L && (is.na(x) || is.finite(as.numeric(x))))
    if (!is.list(value) || length(value$message) != 1L || !is.character(value$message) || is.na(value$message) ||
        !valid_time(value$attempted) || !valid_time(value$success)) stop("Invalid refresh status")
    value
  }, error = function(e) NULL)
  if (!is.null(state)) {
    refresh_state_cache[[key]] <- state
    return(state)
  }
  # Keep the last known refresh timestamps and retry reading on the next poll.
  # On a cold start, use the normal one-hour backoff rather than a retry storm.
  if (is.null(cached)) {
    cached <- empty
    cached$attempted <- Sys.time()
    refresh_state_cache[[key]] <- cached
  }
  cached$message <- "Refresh status could not be read; keeping existing dashboard data. Status will be checked again automatically."
  cached
}
refresh_due <- function(state, now = Sys.time()) {
  interval <- suppressWarnings(as.numeric(Sys.getenv("VALD_REFRESH_HOURS", "24")))
  if (!is.finite(interval) || interval <= 0) stop("VALD_REFRESH_HOURS must be positive")
  retry_seconds <- 3600
  if (!is.null(state$attempted) && !is.na(state$attempted) && as.numeric(difftime(now, state$attempted, units = "secs")) < retry_seconds) return(FALSE)
  is.null(state$success) || is.na(state$success) || as.numeric(difftime(now, state$success, units = "hours")) >= interval
}
run_refresh_job <- function(force = FALSE) {
  lock <- filelock::lock(lock_file, timeout = 0)
  if (is.null(lock)) return(list(ok = FALSE, message = "A refresh is already running"))
  on.exit(filelock::unlock(lock), add = TRUE)
  state <- read_refresh_state()
  if (!force && !refresh_due(state)) return(state)
  state$attempted <- Sys.time()
  state$message <- "Refreshing VALD data in the background"
  atomic_rds(state, state_file)
  result <- tryCatch({
    if (!live_refresh_available) stop("Pipeline dependencies are unavailable")
    run_live_vald_refresh()
  }, error = function(e) list(ok = FALSE, forcedecks_ok = FALSE, message = conditionMessage(e)))
  if (isTRUE(result$forcedecks_ok)) {
    published <- tryCatch({
      load_shared_data()
      if (!dashboard_snapshot_ready(as.list(shared_data))) stop("Missing or empty dashboard outputs")
      atomic_rds(as.list(shared_data), snapshot_file)
      TRUE
    }, error = function(e) { result$message <<- paste("Snapshot publication failed:", conditionMessage(e)); FALSE })
    if (!published) result$ok <- FALSE
  }
  if (isTRUE(result$ok)) state$success <- Sys.time()
  state$ok <- isTRUE(result$ok)
  state$message <- result$message
  state$finished <- Sys.time()
  atomic_rds(state, state_file)
  state
}

shared_revision <- shiny::reactiveVal(0L)
refresh_message <- shiny::reactiveVal(read_refresh_state()$message)
refresh_process <- NULL
snapshot_stamp <- ""
load_published_snapshot <- function() {
  if (!file.exists(snapshot_file)) return(FALSE)
  stamp <- paste(file.info(snapshot_file)$mtime, file.info(snapshot_file)$size)
  if (identical(stamp, snapshot_stamp)) return(FALSE)
  snapshot <- readRDS(snapshot_file)
  if (!dashboard_snapshot_ready(snapshot)) {
    shared_data$status <- "Waiting for the first successful VALD refresh; no populated dashboard snapshot is available yet."
    snapshot_stamp <<- stamp
    return(FALSE)
  }
  list2env(snapshot, shared_data)
  # Do not surface a serialized absolute path from whichever machine created
  # the snapshot. The configured directories remain available in diagnostics.
  shared_data$status <- paste0(
    "Published Player Health snapshot loaded at ",
    format(file.info(snapshot_file)$mtime, "%Y-%m-%d %H:%M:%S")
  )
  snapshot_stamp <<- stamp
  shared_revision(shiny::isolate(shared_revision()) + 1L)
  TRUE
}
if (!identical(Sys.getenv("VALD_REFRESH_WORKER"), "true")) {
  if (!file.exists(snapshot_file)) {
    lock <- filelock::lock(lock_file, timeout = 0)
    if (!is.null(lock)) {
      load_shared_data()
      if (dashboard_snapshot_ready(as.list(shared_data))) {
        atomic_rds(as.list(shared_data), snapshot_file)
      } else {
        shared_data$status <- "Waiting for the first successful VALD refresh; deployment storage does not contain populated VALD data yet."
      }
      filelock::unlock(lock)
    } else {
      shared_data$status <- "Waiting for first data refresh"
    }
  }
  load_published_snapshot()
}
start_refresh <- function(force = FALSE) {
  if (!is.null(refresh_process) && refresh_process$is_alive()) return("A refresh is already running")
  if (!vald_credentials_status()$ok) {
    msg <- "Live refresh needs VALD credentials in the deployment configuration; existing data remains available."
    refresh_message(msg)
    return(msg)
  }
  if (!force && !refresh_due(read_refresh_state())) return("Refresh not due")
  refresh_process <<- callr::r_bg(function(app_root, force) {
    setwd(app_root)
    source("local_library.R", local = TRUE)
    use_vald_local_library()
    Sys.setenv(VALD_APP_ROOT = app_root, VALD_REFRESH_WORKER = "true")
    source("dashboard.R", local = globalenv())
    run_refresh_job(force)
  }, args = list(app_root = Sys.getenv("VALD_APP_ROOT", getwd()), force = force),
  libpath = .libPaths(), stdout = file.path(refresh_dir, "worker.log"),
  stderr = "2>&1", supervise = TRUE,
  env = c(callr::rcmd_safe_env(), R_ENVIRON_USER = "", refresh_worker_environment()))
  refresh_message("Refreshing VALD data in the background")
  "Refresh started; you can continue using the dashboard."
}
poll_refresh <- function() {
  if (!is.null(refresh_process) && !refresh_process$is_alive()) {
    result <- tryCatch(refresh_process$get_result(), error = function(e) list(message = "Refresh worker failed; see data/refresh/worker.log"))
    refresh_message(result$message)
    refresh_process <<- NULL
  }
  load_published_snapshot()
  if (is.null(refresh_process)) refresh_message(read_refresh_state()$message)
}
# Process-level timer keeps running without an open browser while R is alive.
# A suspended/stopped host needs the standalone refresh command instead.
if (!identical(Sys.getenv("VALD_REFRESH_WORKER"), "true")) {
  refresh_timer <- shiny::observe({
    shiny::invalidateLater(60000, session = NULL)
    shiny::isolate({
      poll_refresh()
      if (tolower(Sys.getenv("VALD_AUTO_REFRESH", "true")) %in% c("true", "1", "yes")) start_refresh()
    })
  }, domain = NULL)
  shiny::onStop(function() {
    refresh_timer$destroy()
    if (!is.null(refresh_process) && refresh_process$is_alive()) refresh_process$kill()
  })
}
