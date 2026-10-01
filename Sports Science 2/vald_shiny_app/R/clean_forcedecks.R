clean_forcedecks_dashboard_objects <- function() {
  # The current ForceDecks refresh script already writes dashboard-ready summary,
  # compare, test, and long metric objects. This function is the migration point
  # for future silver/gold transformations.
  sync_dashboard_gold()
}

validate_forcedecks_export_columns <- function(df, required = c("profileId", "testId")) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(ok = FALSE, missing = required, message = "ForceDecks export is not a data frame."))
  }
  missing <- setdiff(required, names(df))
  list(
    ok = length(missing) == 0,
    missing = missing,
    message = if (length(missing) == 0) "ForceDecks export has required columns." else paste0("Missing columns: ", paste(missing, collapse = ", "))
  )
}

standardize_forcedecks_csv_export <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(data.frame())
  pick <- function(candidates) {
    hit <- candidates[tolower(candidates) %in% tolower(names(df))]
    if (length(hit) == 0) return(NULL)
    names(df)[match(tolower(hit[[1]]), tolower(names(df)))]
  }
  profile_col <- pick(c("profileId", "profile_id", "VALD ID", "vald-id", "id"))
  test_col <- pick(c("testId", "test_id", "Test ID"))
  athlete_col <- pick(c("athleteName", "athlete_name", "Name", "Athlete"))
  date_col <- pick(c("session_date", "sessionDate", "recordedDateUtc", "Date"))
  metric_col <- pick(c("metricName", "metric", "metricKey", "Result"))
  value_col <- pick(c("value", "best_value", "mean_value", "Value", "Result Value"))
  data.frame(
    profileId = if (!is.null(profile_col)) as.character(df[[profile_col]]) else NA_character_,
    testId = if (!is.null(test_col)) as.character(df[[test_col]]) else NA_character_,
    athleteName = if (!is.null(athlete_col)) as.character(df[[athlete_col]]) else NA_character_,
    session_date = if (!is.null(date_col)) as_date_safely2(df[[date_col]]) else as.Date(NA),
    metric = if (!is.null(metric_col)) as.character(df[[metric_col]]) else NA_character_,
    value = if (!is.null(value_col)) suppressWarnings(as.numeric(df[[value_col]])) else NA_real_,
    source = "ForceDecks CSV",
    stringsAsFactors = FALSE
  )
}

read_forcedecks_csv_folder <- function(path, recursive = TRUE) {
  if (is.null(path) || !dir.exists(path)) {
    warning("ForceDecks CSV folder not found: ", path, call. = FALSE)
    return(data.frame())
  }
  files <- list.files(path, pattern = "[.]csv$", full.names = TRUE, recursive = recursive)
  if (length(files) == 0) {
    warning("No ForceDecks CSV files found in: ", path, call. = FALSE)
    return(data.frame())
  }
  rows <- lapply(files, function(f) {
    raw <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) e)
    if (inherits(raw, "error")) {
      warning("Skipping invalid ForceDecks CSV ", basename(f), ": ", conditionMessage(raw), call. = FALSE)
      return(data.frame())
    }
    std <- standardize_forcedecks_csv_export(raw)
    std$file <- basename(f)
    std
  })
  dplyr::bind_rows(rows) |>
    dplyr::distinct()
}
