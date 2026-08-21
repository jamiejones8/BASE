# Persistent pitch-type overrides for the college pitcher scouting page.
#
# The source Parquet files remain immutable.  Only user-entered corrections are
# stored here, keyed by a stable source/PitchUID identity (with a deterministic
# game/pitch fallback for rows that do not contain PitchUID).

BASE_RETAG_DB_FILE <- TEAM_CONFIG$data$retag_db_file

base_retag_chr <- function(x, n) {
  if (is.null(x)) return(rep("", n))
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

base_retag_column <- function(df, name) {
  base_retag_chr(if (name %in% names(df)) df[[name]] else NULL, nrow(df))
}

base_add_pitch_keys <- function(df) {
  if (is.null(df)) return(df)
  if (!nrow(df)) {
    if (!"base_pitch_key" %in% names(df)) df$base_pitch_key <- character()
    return(df)
  }

  source_name <- base_retag_column(df, "DataSource")
  source_name[!nzchar(source_name)] <- "Unknown source"
  pitch_uid <- base_retag_column(df, "PitchUID")

  fallback_fields <- c(
    "GameUID", "GameID", "PlayID", "PitchNo", "Date", "LocalDateTime",
    "PitcherTeam", "Pitcher", "Batter", "Inning", "Top/Bottom",
    "PAofInning", "PitchofPA", "RelSpeed", "SpinRate", "PlateLocHeight",
    "PlateLocSide"
  )
  fallback_parts <- lapply(fallback_fields, function(name) base_retag_column(df, name))
  fallback_id <- do.call(paste, c(fallback_parts, sep = "|"))

  df$base_pitch_key <- ifelse(
    nzchar(pitch_uid),
    paste0(source_name, "::uid::", pitch_uid),
    paste0(source_name, "::fallback::", fallback_id)
  )
  df
}

base_prepare_retag_rows <- function(df) {
  if (is.null(df)) return(df)
  if (!"TaggedPitchType" %in% names(df)) df$TaggedPitchType <- NA_character_
  df$TaggedPitchType <- as.character(df$TaggedPitchType)

  if (!"OriginalTaggedPitchType" %in% names(df)) {
    df$OriginalTaggedPitchType <- df$TaggedPitchType
  } else {
    original <- as.character(df$OriginalTaggedPitchType)
    missing_original <- is.na(original) | !nzchar(trimws(original))
    original[missing_original] <- df$TaggedPitchType[missing_original]
    df$OriginalTaggedPitchType <- original
  }

  base_add_pitch_keys(df)
}

base_retag_connect <- function(path = BASE_RETAG_DB_FILE) {
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create persistent retag directory: ", parent)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000")
  DBI::dbGetQuery(con, "PRAGMA journal_mode = DELETE")
  DBI::dbExecute(con, "PRAGMA synchronous = FULL")
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS pitch_tag_overrides (",
    "pitch_key TEXT PRIMARY KEY,",
    "data_source TEXT NOT NULL,",
    "pitch_uid TEXT,",
    "pitcher_team TEXT,",
    "pitcher TEXT,",
    "original_tag TEXT,",
    "retagged_type TEXT NOT NULL,",
    "updated_at TEXT NOT NULL",
    ")"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS pitch_tag_history (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "pitch_key TEXT NOT NULL,",
    "previous_tag TEXT,",
    "new_tag TEXT,",
    "action TEXT NOT NULL,",
    "changed_at TEXT NOT NULL",
    ")"
  ))
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_pitch_tag_overrides_pitcher ON pitch_tag_overrides (pitcher_team, pitcher)"
  )
  con
}

base_retag_storage_status <- function(path = BASE_RETAG_DB_FILE) {
  tryCatch({
    con <- base_retag_connect(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pitch_tag_overrides")
    list(ok = TRUE, path = path, message = "Persistent retag storage is ready.")
  }, error = function(e) {
    list(ok = FALSE, path = path, message = conditionMessage(e))
  })
}

base_apply_pitch_retags <- function(df, path = BASE_RETAG_DB_FILE) {
  df <- base_prepare_retag_rows(df)
  if (is.null(df)) return(df)
  if (!nrow(df)) {
    df$base_has_saved_retag <- logical()
    return(df)
  }

  overrides <- tryCatch({
    con <- base_retag_connect(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(
      con,
      "SELECT pitch_key, retagged_type FROM pitch_tag_overrides"
    )
  }, error = function(e) {
    message("Persistent pitch retags unavailable: ", conditionMessage(e))
    NULL
  })

  df$base_has_saved_retag <- FALSE
  if (is.null(overrides) || !nrow(overrides)) return(df)

  matched <- match(df$base_pitch_key, overrides$pitch_key)
  has_override <- !is.na(matched)
  df$TaggedPitchType[has_override] <- overrides$retagged_type[matched[has_override]]
  df$base_has_saved_retag <- has_override
  df
}

base_save_pitch_retags <- function(rows, new_type, path = BASE_RETAG_DB_FILE) {
  rows <- base_prepare_retag_rows(rows)
  if (is.null(rows) || !nrow(rows)) return(0L)
  new_type <- trimws(as.character(new_type)[[1]])
  if (!nzchar(new_type)) stop("Choose a valid pitch type before saving.")

  rows <- rows[!is.na(rows$base_pitch_key) & nzchar(rows$base_pitch_key), , drop = FALSE]
  rows <- rows[!duplicated(rows$base_pitch_key), , drop = FALSE]
  if (!nrow(rows)) stop("The selected pitches do not have stable identities.")

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  con <- base_retag_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(rows))) {
      key <- rows$base_pitch_key[[i]]
      previous <- DBI::dbGetQuery(
        con,
        "SELECT retagged_type FROM pitch_tag_overrides WHERE pitch_key = ?",
        params = list(key)
      )
      previous_tag <- if (nrow(previous)) previous$retagged_type[[1]] else rows$TaggedPitchType[[i]]

      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO pitch_tag_overrides",
          "(pitch_key, data_source, pitch_uid, pitcher_team, pitcher, original_tag, retagged_type, updated_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
          "ON CONFLICT(pitch_key) DO UPDATE SET",
          "data_source = excluded.data_source, pitch_uid = excluded.pitch_uid,",
          "pitcher_team = excluded.pitcher_team, pitcher = excluded.pitcher,",
          "original_tag = excluded.original_tag, retagged_type = excluded.retagged_type,",
          "updated_at = excluded.updated_at"
        ),
        params = list(
          key,
          base_retag_column(rows[i, , drop = FALSE], "DataSource")[[1]],
          base_retag_column(rows[i, , drop = FALSE], "PitchUID")[[1]],
          base_retag_column(rows[i, , drop = FALSE], "PitcherTeam")[[1]],
          base_retag_column(rows[i, , drop = FALSE], "Pitcher")[[1]],
          base_retag_column(rows[i, , drop = FALSE], "OriginalTaggedPitchType")[[1]],
          new_type,
          now
        )
      )
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO pitch_tag_history",
          "(pitch_key, previous_tag, new_tag, action, changed_at)",
          "VALUES (?, ?, ?, 'retag', ?)"
        ),
        params = list(key, previous_tag, new_type, now)
      )
    }
  })

  nrow(rows)
}

base_revert_pitch_retags <- function(rows, path = BASE_RETAG_DB_FILE) {
  rows <- base_prepare_retag_rows(rows)
  if (is.null(rows) || !nrow(rows)) return(0L)
  keys <- unique(rows$base_pitch_key[!is.na(rows$base_pitch_key) & nzchar(rows$base_pitch_key)])
  if (!length(keys)) return(0L)

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  con <- base_retag_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  reverted <- 0L

  DBI::dbWithTransaction(con, {
    for (key in keys) {
      previous <- DBI::dbGetQuery(
        con,
        "SELECT retagged_type FROM pitch_tag_overrides WHERE pitch_key = ?",
        params = list(key)
      )
      if (!nrow(previous)) next
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO pitch_tag_history",
          "(pitch_key, previous_tag, new_tag, action, changed_at)",
          "VALUES (?, ?, NULL, 'revert', ?)"
        ),
        params = list(key, previous$retagged_type[[1]], now)
      )
      reverted <- reverted + DBI::dbExecute(
        con,
        "DELETE FROM pitch_tag_overrides WHERE pitch_key = ?",
        params = list(key)
      )
    }
  })

  as.integer(reverted)
}
