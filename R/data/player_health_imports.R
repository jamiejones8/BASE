# Atomic replacement imports for the Sports Science workspace.
#
# These sources are vendor snapshots, not cumulative feeds. A successful
# upload replaces every older matching export so the embedded ArmCare and
# PULSE views read only the newly supplied file.

BASE_SPORTS_SCIENCE_IMPORT_TARGETS <- list(
  PULSE_EVENTS = list(
    id = "PULSE_EVENTS",
    label = "PULSE Events",
    filename = "pulse_events.csv",
    pattern = "_events[.]csv$",
    required = c(
      "firstName", "lastName", "datetime", "tag", "highEffort",
      "armSlot", "armSpeed", "shoulderRotation", "torque",
      "ballVelocity", "ballWeight", "preferredBallWeightUnit", "simulated"
    ),
    date_column = "datetime",
    date_kind = "datetime"
  ),
  PULSE_WORKLOAD = list(
    id = "PULSE_WORKLOAD",
    label = "PULSE Workload",
    filename = "pulse_workload.csv",
    pattern = "_workload[.]csv$",
    required = c(
      "firstName", "lastName", "date", "A:C Ratio", "Acute Workload",
      "Chronic Workload", "One Day Workload", "Total Throw Count",
      "High Effort Throw Count"
    ),
    date_column = "date",
    date_kind = "date"
  ),
  ARM_CARE = list(
    id = "ARM_CARE",
    label = "Arm Care",
    filename = "Daily Log.csv",
    pattern = "^Daily Log.*[.]csv$",
    required = c(
      "Exam Date", "First Name", "Last Name", "Time", "Exam Type",
      "Arm Score", "Total Strength", "IRTARM Max-Lbs", "ERTARM Max-Lbs",
      "STARM Max-Lbs", "GTARM Max-Lbs"
    ),
    date_column = "Exam Date",
    date_kind = "armcare_date"
  )
)

base_sports_science_import_root <- function(root = NULL) {
  if (!is.null(root) && length(root) && nzchar(trimws(root[[1]]))) {
    return(normalizePath(root[[1]], winslash = "/", mustWork = FALSE))
  }
  for (env_name in c("BASE_PLAYER_HEALTH_EXPORT_DIR", "PLAYER_HEALTH_DATA_DIR")) {
    configured <- Sys.getenv(env_name, unset = "")
    if (nzchar(trimws(configured))) {
      return(normalizePath(configured, winslash = "/", mustWork = FALSE))
    }
  }
  storage_root <- Sys.getenv("BASE_PLAYER_HEALTH_STORAGE_ROOT", unset = "")
  if (nzchar(trimws(storage_root))) {
    return(normalizePath(file.path(storage_root, "imports"), winslash = "/", mustWork = FALSE))
  }
  if (dir.exists("/base-data")) return("/base-data/app_state/player-health/imports")
  normalizePath(base_project_path("Sports Science 2", "data"), winslash = "/", mustWork = FALSE)
}

base_sports_science_import_target <- function(target_id) {
  target_id <- toupper(trimws(as.character(target_id)[[1]]))
  target <- BASE_SPORTS_SCIENCE_IMPORT_TARGETS[[target_id]]
  if (is.null(target)) stop("Unknown Sports Science import target: ", target_id, call. = FALSE)
  target
}

base_sports_science_import_files <- function(target_id, root = NULL) {
  target <- base_sports_science_import_target(target_id)
  directory <- base_sports_science_import_root(root)
  if (!dir.exists(directory)) return(character())
  sort(list.files(
    directory,
    pattern = target$pattern,
    full.names = TRUE,
    ignore.case = TRUE
  ))
}

base_sports_science_import_path <- function(target_id, root = NULL) {
  target <- base_sports_science_import_target(target_id)
  normalizePath(
    file.path(base_sports_science_import_root(root), target$filename),
    winslash = "/",
    mustWork = FALSE
  )
}

base_read_sports_science_import <- function(path) {
  tibble::as_tibble(readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE,
    name_repair = "minimal"
  ))
}

base_validate_sports_science_import <- function(rows, target_id) {
  target <- base_sports_science_import_target(target_id)
  rows <- tibble::as_tibble(rows)
  if (!nrow(rows)) stop("The selected ", target$label, " CSV has no data rows.", call. = FALSE)

  missing <- setdiff(target$required, names(rows))
  if (length(missing)) {
    stop(
      "This is not a valid ", target$label, " export. Missing required columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  first_name <- if (identical(target$id, "ARM_CARE")) rows[["First Name"]] else rows$firstName
  last_name <- if (identical(target$id, "ARM_CARE")) rows[["Last Name"]] else rows$lastName
  named_rows <- nzchar(trimws(ifelse(is.na(first_name), "", first_name))) |
    nzchar(trimws(ifelse(is.na(last_name), "", last_name)))
  if (!any(named_rows)) {
    stop(target$label, " must contain at least one athlete name.", call. = FALSE)
  }

  raw_dates <- trimws(as.character(rows[[target$date_column]]))
  parsed <- switch(
    target$date_kind,
    datetime = suppressWarnings(as.POSIXct(raw_dates, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")),
    date = suppressWarnings(as.Date(substr(raw_dates, 1L, 10L))),
    armcare_date = {
      result <- suppressWarnings(as.Date(raw_dates, format = "%m/%d/%Y"))
      missing_date <- is.na(result)
      result[missing_date] <- suppressWarnings(as.Date(raw_dates[missing_date], format = "%Y-%m-%d"))
      result
    }
  )
  invalid <- !nzchar(raw_dates) | is.na(parsed)
  if (any(invalid)) {
    stop(
      target$label, " contains ", sum(invalid),
      if (sum(invalid) == 1L) " row with a missing or invalid date." else " rows with missing or invalid dates.",
      call. = FALSE
    )
  }

  list(rows = rows, target = target, dates = parsed)
}

base_replace_sports_science_import <- function(upload_path, target_id, root = NULL) {
  target <- base_sports_science_import_target(target_id)
  directory <- base_sports_science_import_root(root)
  destination <- base_sports_science_import_path(target$id, root = directory)
  if (!file.exists(upload_path)) stop("The uploaded file is no longer available.", call. = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the Sports Science import directory: ", directory, call. = FALSE)
  }
  if (file.access(directory, 2L) != 0L) {
    stop("The Sports Science import directory is not writable: ", directory, call. = FALSE)
  }

  lock <- file.path(directory, paste0(".", tolower(target$id), "-replace.lock"))
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop(target$label, " is already being replaced by another upload.", call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)

  incoming <- tryCatch(
    base_read_sports_science_import(upload_path),
    error = function(e) stop(target$label, " could not be read: ", conditionMessage(e), call. = FALSE)
  )
  checked <- base_validate_sports_science_import(incoming, target$id)

  staged <- tempfile(pattern = ".base-sports-science-", tmpdir = directory, fileext = ".csv")
  on.exit(if (file.exists(staged)) unlink(staged), add = TRUE)
  if (!file.copy(upload_path, staged, overwrite = TRUE) || file.info(staged)$size <= 0) {
    stop("The uploaded ", target$label, " file could not be staged.", call. = FALSE)
  }

  prior_files <- base_sports_science_import_files(target$id, root = directory)
  backup_dir <- tempfile(pattern = paste0(".base-", tolower(target$id), "-previous-"), tmpdir = directory)
  if (!dir.create(backup_dir, showWarnings = FALSE)) {
    stop("Could not prepare the atomic ", target$label, " replacement.", call. = FALSE)
  }
  committed <- FALSE
  on.exit({
    if (!committed && dir.exists(backup_dir)) {
      backups <- list.files(backup_dir, full.names = TRUE)
      for (backup in backups) {
        file.rename(backup, file.path(directory, basename(backup)))
      }
    }
    if (dir.exists(backup_dir)) unlink(backup_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  for (source in prior_files) {
    backup <- file.path(backup_dir, basename(source))
    if (!file.rename(source, backup)) {
      stop("The existing ", target$label, " source could not be secured for replacement.", call. = FALSE)
    }
  }
  if (!file.rename(staged, destination)) {
    stop("The new ", target$label, " file could not replace the existing source.", call. = FALSE)
  }
  committed <- TRUE
  unlink(backup_dir, recursive = TRUE, force = TRUE)

  list(
    ok = TRUE,
    target_id = target$id,
    target_label = target$label,
    destination = destination,
    received_rows = nrow(checked$rows),
    replaced_files = length(prior_files),
    first_date = as.character(min(checked$dates)),
    last_date = as.character(max(checked$dates))
  )
}

base_sports_science_source_status <- function(target_id, root = NULL) {
  target <- base_sports_science_import_target(target_id)
  files <- base_sports_science_import_files(target$id, root = root)
  if (!length(files)) {
    return(list(
      exists = FALSE,
      target = target,
      path = base_sports_science_import_path(target$id, root = root),
      paths = character(),
      size = 0,
      modified = as.POSIXct(NA)
    ))
  }
  canonical <- base_sports_science_import_path(target$id, root = root)
  path <- if (canonical %in% normalizePath(files, winslash = "/", mustWork = TRUE)) {
    canonical
  } else {
    files[[which.max(file.info(files)$mtime)]]
  }
  info <- file.info(path)
  list(
    exists = TRUE,
    target = target,
    path = path,
    paths = files,
    size = unname(info$size[[1]]),
    modified = unname(info$mtime[[1]])
  )
}
