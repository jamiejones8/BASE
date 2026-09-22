# Persistent TrackMan game and bullpen imports for Texas State team data.
#
# Each target remains one append-only CSV, matching the existing Wally loading
# model. Season files keep the complete game export; the Hitting and Pitching
# adapters select the Texas State batting and pitching rows they need. Bullpen
# imports update the active cumulative file used by the Pitching workspace.

BASE_TEAM_SEASON_IMPORT_TARGETS <- list(
  F26 = list(
    id = "F26",
    label = "2026 Fall",
    filename = "2026 Fall - cleaned.csv",
    expected_year = 2026L
  ),
  S27 = list(
    id = "S27",
    label = "2027 Season",
    filename = "2027 Season - cleaned.csv",
    expected_year = 2027L
  )
)

BASE_TEAM_BULLPEN_IMPORT_TARGETS <- list(
  BP = list(
    id = "BP",
    label = "Bullpens",
    filename = "Bullpens - cleaned.csv",
    expected_year = NULL
  )
)

base_trackman_import_targets <- function() {
  c(BASE_TEAM_SEASON_IMPORT_TARGETS, BASE_TEAM_BULLPEN_IMPORT_TARGETS)
}

base_team_season_import_root <- function() {
  configured <- tryCatch(TEAM_CONFIG$data$team_season_import_dir, error = function(e) NULL)
  if (!is.null(configured) && length(configured) && nzchar(configured[[1]])) {
    return(configured[[1]])
  }
  base_project_path("app_state", "team-season-imports")
}

base_team_season_import_target <- function(target_id) {
  target_id <- toupper(trimws(as.character(target_id)[[1]]))
  target <- base_trackman_import_targets()[[target_id]]
  if (is.null(target)) {
    stop("Unknown TrackMan import target: ", target_id, call. = FALSE)
  }
  target
}

base_team_season_import_path <- function(target_id, root = NULL) {
  target <- base_team_season_import_target(target_id)
  if (is.null(root)) {
    if (identical(target$id, "BP")) {
      configured <- tryCatch(TEAM_CONFIG$data$bullpen_file, error = function(e) NULL)
      if (!is.null(configured) && length(configured) && nzchar(configured[[1]])) {
        return(normalizePath(configured[[1]], winslash = "/", mustWork = FALSE))
      }
      return(normalizePath(
        base_project_path("WallyApps", "PitchingApp", "data", target$filename),
        winslash = "/",
        mustWork = FALSE
      ))
    }
    root <- base_team_season_import_root()
  }
  normalizePath(file.path(root, target$filename), winslash = "/", mustWork = FALSE)
}

base_team_season_import_paths <- function(existing_only = FALSE, root = base_team_season_import_root()) {
  paths <- vapply(
    BASE_TEAM_SEASON_IMPORT_TARGETS,
    function(target) base_team_season_import_path(target$id, root = root),
    character(1)
  )
  if (isTRUE(existing_only)) paths <- paths[file.exists(paths)]
  paths
}

base_read_trackman_import <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE
  ) %>%
    tibble::as_tibble()
}

base_parse_trackman_dates <- function(values) {
  text <- trimws(as.character(values))
  text <- sub(
    "^\\s*([0-9]{1,4}[-/][0-9]{1,2}[-/][0-9]{1,4}).*$",
    "\\1",
    text,
    perl = TRUE
  )
  text <- gsub("-", "/", text, fixed = TRUE)
  result <- rep(as.Date(NA), length(text))

  # R will accept "9/17/26" with %Y and interpret it as year 26. Match the
  # shape first so two-digit TrackMan years reach %y and become 2026.
  formats <- list(
    list(pattern = "^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", format = "%Y/%m/%d"),
    list(pattern = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", format = "%m/%d/%Y"),
    list(pattern = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$", format = "%m/%d/%y")
  )
  for (spec in formats) {
    matches <- is.na(result) & nzchar(text) & grepl(spec$pattern, text)
    if (any(matches)) {
      result[matches] <- suppressWarnings(as.Date(text[matches], format = spec$format))
    }
  }
  result
}

base_trackman_value <- function(rows, name) {
  if (name %in% names(rows)) {
    value <- trimws(as.character(rows[[name]]))
    value[is.na(value)] <- ""
    value
  } else {
    rep("", nrow(rows))
  }
}

base_trackman_event_key <- function(rows) {
  rows <- tibble::as_tibble(rows)
  n <- nrow(rows)
  pitch_uid <- base_trackman_value(rows, "PitchUID")
  play_id <- base_trackman_value(rows, "PlayID")
  game_uid <- base_trackman_value(rows, "GameUID")
  game_id <- base_trackman_value(rows, "GameID")
  pitch_no <- base_trackman_value(rows, "PitchNo")
  fallback <- do.call(
    paste,
    c(
      lapply(
        c(
          "Date", "GameUID", "GameID", "PitchNo", "Pitcher", "Batter",
          "Inning", "PAofInning", "PitchofPA", "RelSpeed",
          "PlateLocSide", "PlateLocHeight"
        ),
        function(name) base_trackman_value(rows, name)
      ),
      sep = "\u001f"
    )
  )
  fallback_has_identity <- nzchar(game_uid) | nzchar(game_id) |
    (nzchar(pitch_no) & nzchar(base_trackman_value(rows, "Date")))
  dplyr::case_when(
    nzchar(pitch_uid) ~ paste0("pitch:", pitch_uid),
    nzchar(play_id) ~ paste0("play:", play_id),
    fallback_has_identity ~ paste0("event:", fallback),
    TRUE ~ NA_character_
  )
}

base_trackman_game_id <- function(rows, dates) {
  for (column in c("GameUID", "GameID", "GameId")) {
    values <- unique(base_trackman_value(rows, column))
    values <- values[nzchar(values)]
    if (length(values)) return(values)
  }
  unique(as.character(dates[!is.na(dates)]))
}

base_validate_trackman_import <- function(rows, target_id) {
  target <- base_team_season_import_target(target_id)
  rows <- tibble::as_tibble(rows)
  if (!nrow(rows)) stop("The selected TrackMan CSV has no data rows.", call. = FALSE)

  required <- c("Date", "PitcherTeam", "BatterTeam", "Pitcher", "Batter", "PitchCall")
  missing <- setdiff(required, names(rows))
  if (length(missing)) {
    stop(
      "The TrackMan CSV is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  dates <- base_parse_trackman_dates(rows$Date)
  if (any(is.na(dates))) {
    stop("Every uploaded row must have a recognizable TrackMan Date.", call. = FALSE)
  }
  years <- unique(as.integer(format(dates, "%Y")))
  if (!is.null(target$expected_year) && !identical(years, target$expected_year)) {
    stop(
      target$label, " only accepts ", target$expected_year,
      " game dates; this file contains ", paste(sort(years), collapse = ", "), ".",
      call. = FALSE
    )
  }

  game_ids <- base_trackman_game_id(rows, dates)
  if (length(game_ids) != 1L) {
    stop(
      "Upload one game at a time. This file contains ", length(game_ids),
      " game identifiers.",
      call. = FALSE
    )
  }

  team_rows <- base_team_matches(rows$PitcherTeam) | base_team_matches(rows$BatterTeam)
  if (!any(team_rows)) {
    stop("No Texas State batting or pitching rows were found in this file.", call. = FALSE)
  }

  keys <- base_trackman_event_key(rows)
  if (any(is.na(keys) | !nzchar(keys))) {
    stop(
      "Every row needs PitchUID, PlayID, or TrackMan game/pitch identity fields for safe deduplication.",
      call. = FALSE
    )
  }

  list(
    rows = rows,
    dates = dates,
    game_id = game_ids[[1]],
    event_keys = keys,
    target = target
  )
}

base_atomic_write_trackman_csv <- function(rows, path) {
  directory <- dirname(path)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the season-data directory: ", directory, call. = FALSE)
  }
  if (file.access(directory, 2L) != 0L) {
    stop("The season-data directory is not writable: ", directory, call. = FALSE)
  }

  staged <- tempfile(pattern = ".base-trackman-", tmpdir = directory, fileext = ".csv")
  on.exit(if (file.exists(staged)) unlink(staged), add = TRUE)
  readr::write_csv(rows, staged, na = "")
  if (!file.exists(staged) || file.info(staged)$size <= 0) {
    stop("The updated season file could not be staged.", call. = FALSE)
  }

  if (!file.rename(staged, path)) {
    stop("The updated season file could not replace ", basename(path), ".", call. = FALSE)
  }
  invisible(path)
}

base_import_trackman_game <- function(upload_path, target_id, root = NULL) {
  target <- base_team_season_import_target(target_id)
  destination <- base_team_season_import_path(target$id, root = root)
  directory <- dirname(destination)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the season-data directory: ", directory, call. = FALSE)
  }

  lock <- file.path(directory, paste0(".", target$id, "-import.lock"))
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop(target$label, " is already being updated by another import.", call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)

  incoming <- tryCatch(
    base_read_trackman_import(upload_path),
    error = function(e) stop("The TrackMan CSV could not be read: ", conditionMessage(e), call. = FALSE)
  )
  checked <- base_validate_trackman_import(incoming, target$id)
  incoming <- checked$rows
  incoming_keys <- checked$event_keys
  keep_incoming <- !duplicated(incoming_keys)
  incoming <- incoming[keep_incoming, , drop = FALSE]
  incoming_keys <- incoming_keys[keep_incoming]

  current <- if (file.exists(destination)) {
    tryCatch(
      base_read_trackman_import(destination),
      error = function(e) stop(
        "The current ", target$label, " source could not be read: ",
        conditionMessage(e),
        call. = FALSE
      )
    )
  } else {
    tibble::tibble()
  }
  current_keys <- if (nrow(current)) base_trackman_event_key(current) else character()
  if (any(is.na(current_keys) | !nzchar(current_keys))) {
    stop(
      "The current ", target$label,
      " source contains rows without stable pitch identity; no changes were written.",
      call. = FALSE
    )
  }

  add <- !incoming_keys %in% current_keys
  inserted <- incoming[add, , drop = FALSE]
  combined <- if (nrow(current)) dplyr::bind_rows(current, inserted) else inserted
  combined[] <- lapply(combined, as.character)

  if (nrow(inserted)) base_atomic_write_trackman_csv(combined, destination)

  list(
    ok = TRUE,
    target_id = target$id,
    target_label = target$label,
    destination = destination,
    game_id = checked$game_id,
    received_rows = nrow(checked$rows),
    inserted_rows = nrow(inserted),
    duplicate_rows = nrow(checked$rows) - nrow(inserted),
    total_rows = nrow(combined),
    game_date = as.character(min(checked$dates))
  )
}

base_team_season_source_status <- function(target_id, root = NULL) {
  target <- base_team_season_import_target(target_id)
  path <- base_team_season_import_path(target$id, root = root)
  if (!file.exists(path)) {
    return(list(exists = FALSE, target = target, path = path, size = 0, modified = NA))
  }
  info <- file.info(path)
  list(
    exists = TRUE,
    target = target,
    path = path,
    size = unname(info$size[[1]]),
    modified = unname(info$mtime[[1]])
  )
}
