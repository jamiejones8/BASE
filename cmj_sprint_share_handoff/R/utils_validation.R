# Shared validation/config helpers for the staff monitoring workflow.

`%||%` <- function(a, b) if (!is.null(a)) a else b

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required for this workflow. ",
      "Install it locally before running the dashboard pipeline.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_yaml_config <- function(path) {
  require_pkg("yaml")
  if (!file.exists(path)) stop("Missing config file: ", path, call. = FALSE)
  yaml::read_yaml(path)
}

cfg_get <- function(x, path, default = NULL) {
  cur <- x
  for (part in path) {
    if (is.null(cur) || is.null(cur[[part]])) return(default)
    cur <- cur[[part]]
  }
  cur %||% default
}

root_dir <- function() {
  if (exists("ROOT_DIR", inherits = TRUE)) return(get("ROOT_DIR", inherits = TRUE))
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

config_path <- function(...) file.path(root_dir(), "config", ...)
gold_data_dir <- function() ensure_dir(file.path(root_dir(), "data", "gold"))
log_data_dir <- function() ensure_dir(file.path(root_dir(), "data", "logs"))
legacy_data_dir <- function() {
  if (exists("DATA_DIR", inherits = TRUE)) return(get("DATA_DIR", inherits = TRUE))
  file.path(root_dir(), "Data")
}

load_thresholds <- function(path = config_path("thresholds.yml")) {
  read_yaml_config(path)
}

load_metrics_config <- function(path = config_path("metrics.yml")) {
  read_yaml_config(path)
}

load_report_settings <- function(path = config_path("report_settings.yml")) {
  read_yaml_config(path)
}

dashboard_data_path <- function(filename) {
  gold_path <- file.path(gold_data_dir(), filename)
  if (file.exists(gold_path)) return(gold_path)
  file.path(legacy_data_dir(), filename)
}

write_refresh_log <- function(event, status = "ok", details = list()) {
  ensure_dir(log_data_dir())
  log_file <- file.path(log_data_dir(), "data_refresh_log.csv")
  row <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    event = as.character(event),
    status = as.character(status),
    details = paste(names(details), unlist(details), sep = "=", collapse = " | "),
    stringsAsFactors = FALSE
  )
  if (file.exists(log_file)) {
    old <- tryCatch(read.csv(log_file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.data.frame(old)) row <- rbind(old, row)
  }
  utils::write.csv(row, log_file, row.names = FALSE)
  invisible(log_file)
}

sync_dashboard_gold <- function(filenames = NULL) {
  if (is.null(filenames)) {
    filenames <- c(
      "roster_baseball.rds",
      "force_sessions_summary_baseball.rds",
      "force_sessions_compare_baseball.rds",
      "force_tests_baseball_all_history_named.rds",
      "force_metrics_long_all_history_baseball_enriched.rds",
      "force_metrics_long_all_history_baseball.rds",
      "armcare_raw_all_history.rds",
      "armcare_metrics_long.rds"
    )
  }
  src_dir <- legacy_data_dir()
  dst_dir <- gold_data_dir()
  copied <- character(0)
  for (nm in filenames) {
    src <- file.path(src_dir, nm)
    if (file.exists(src)) {
      file.copy(src, file.path(dst_dir, nm), overwrite = TRUE)
      copied <- c(copied, nm)
    }
  }
  write_refresh_log("sync_dashboard_gold", "ok", list(files = paste(copied, collapse = ",")))
  copied
}

