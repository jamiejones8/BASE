# Resolve fill boundaries without adding derived columns to the saved CSV.
base_csv_fill_context <- function(rows, target_id = NULL, contacts = NULL) {
  if (!nrow(rows)) return(list(groups = integer(), halves = character(), protected = integer()))
  half_names <- c("Top/Bottom", "Top.Bottom", "TopBottom")
  value <- function(data, name) base_trackman_value(data, name)
  half_col <- intersect(half_names, names(rows))
  half <- if (length(half_col)) value(rows, half_col[1]) else rep("", nrow(rows))
  inning <- value(rows, "Inning")
  target <- if (!is.null(target_id)) base_team_season_import_target(target_id) else NULL
  positioning <- identical(target$kind, "positioning")

  if (positioning && any(!nzchar(half) | !nzchar(inning))) {
    if (is.null(contacts)) {
      seasons <- if (is.null(target$season_id)) c("F26", "PS27", "S27") else target$season_id
      paths <- vapply(seasons, base_team_season_import_path, character(1))
      if ("F26" %in% seasons) paths <- c(paths, list.files(
        base_project_path("WallyApps", "DefenseApp", "data"),
        pattern = "^2026 Fall Batted Balls.*[.]csv$", full.names = TRUE))
      if (is.null(target$season_id)) paths <- c(paths,
        base_project_path("WallyApps", "PitchingApp", "data", "2026 Season - cleaned.csv"))
      columns <- c("PitchUID", "GameUID", "Date", "Inning", half_names)
      contacts <- dplyr::bind_rows(lapply(unique(paths[file.exists(paths)]), function(path) {
        readr::read_csv(path, col_select = dplyr::any_of(columns),
          col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, progress = FALSE)
      }))
    }
    contact_half <- intersect(half_names, names(contacts))
    if (length(contact_half) && "Inning" %in% names(contacts)) {
      # Pitch UUID + game UUID + date avoids doubleheader and cross-season matches.
      key <- function(data) paste(value(data, "PitchUID"), value(data, "GameUID"),
        base_parse_trackman_dates(value(data, "Date")), sep = "\u001f")
      context <- unique(data.frame(key = key(contacts), inning = value(contacts, "Inning"),
        half = sub("^bot$", "bottom", tolower(value(contacts, contact_half[1]))), stringsAsFactors = FALSE))
      ambiguous <- duplicated(context$key) | duplicated(context$key, fromLast = TRUE)
      context <- context[!ambiguous, , drop = FALSE]
      index <- match(key(rows), context$key)
      matched <- !is.na(index) & nzchar(value(rows, "PitchUID")) & nzchar(value(rows, "GameUID")) &
        !is.na(base_parse_trackman_dates(value(rows, "Date")))
      matched <- matched & (!nzchar(half) | sub("^bot$", "bottom", tolower(half)) == context$half[index]) &
        (!nzchar(inning) | inning == context$inning[index])
      take <- matched & !nzchar(half)
      half[take] <- context$half[index[take]]
      take <- matched & !nzchar(inning)
      inning[take] <- context$inning[index[take]]
    }
  }
  half <- tolower(trimws(half)); half[half == "bot"] <- "bottom"
  game_columns <- intersect(c("GameID", "GameUID", "GameId", "Date", "HomeTeam", "AwayTeam"), names(rows))
  parts <- c(lapply(game_columns, function(name) value(rows, name)), list(inning, half))
  keys <- do.call(paste, c(parts, sep = "\u001f"))
  valid <- half %in% c("top", "bottom") & nzchar(inning)
  for (column in game_columns) valid <- valid & nzchar(value(rows, column))
  groups <- cumsum(c(TRUE, tail(keys, -1) != head(keys, -1)))
  groups[!valid] <- NA_integer_
  list(groups = unname(groups), halves = unname(half),
    protected = which(names(rows) %in% c(game_columns, "Inning", half_names)))
}
