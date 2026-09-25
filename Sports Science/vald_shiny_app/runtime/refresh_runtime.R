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
read_refresh_state <- function() {
  if (!file.exists(state_file)) return(list(message = "Not refreshed yet", attempted = as.POSIXct(NA), success = as.POSIXct(NA)))
  readRDS(state_file)
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
      if (is.null(shared_data$roster) || is.null(shared_data$session_summary)) stop("Missing dashboard outputs")
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
  list2env(snapshot, shared_data)
  reconcile_shared_player_health_identity()
  # Snapshots may have been produced on another machine. Never surface its
  # absolute filesystem path in the staff-facing status strip.
  shared_data$status <- paste0(
    "Player Health snapshot loaded at ",
    format(file.info(snapshot_file)$mtime, "%H:%M:%S")
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
      atomic_rds(as.list(shared_data), snapshot_file)
      filelock::unlock(lock)
    } else {
      shared_data$status <- "Waiting for first data refresh"
    }
  }
  load_published_snapshot()
}
start_refresh <- function(force = FALSE) {
  if (!is.null(refresh_process) && refresh_process$is_alive()) return("A refresh is already running")
  missing_runtime <- c("callr", "filelock", "httr2", "valdr", "keyring")
  missing_runtime <- missing_runtime[
    !vapply(missing_runtime, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_runtime)) {
    msg <- paste0("Live refresh is unavailable; missing packages: ", paste(missing_runtime, collapse = ", "), ".")
    refresh_message(msg)
    return(msg)
  }
  if (!vald_credentials_status()$ok) {
    msg <- "Live refresh needs VALD credentials in .Renviron; existing data remains available."
    refresh_message(msg)
    return(msg)
  }
  if (!force && !refresh_due(read_refresh_state())) return("Refresh not due")
  refresh_process <<- callr::r_bg(function(app_root, force) {
    setwd(app_root)
    if (dir.exists(".R-library")) .libPaths(c(normalizePath(".R-library"), .libPaths()))
    if (file.exists(".Renviron")) readRenviron(".Renviron")
    Sys.setenv(VALD_APP_ROOT = app_root, VALD_REFRESH_WORKER = "true")
    source("dashboard.R", local = globalenv())
    run_refresh_job(force)
  }, args = list(app_root = Sys.getenv("VALD_APP_ROOT", getwd()), force = force),
  libpath = .libPaths(), stdout = file.path(refresh_dir, "worker.log"),
  stderr = "2>&1", supervise = TRUE)
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
