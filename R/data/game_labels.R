# Display-only game labels. Values remain the original IDs used by filters.
base_game_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  for (fmt in c("%Y-%m-%d", "%Y%m%d", "%m/%d/%Y", "%m/%d/%y")) {
    missing <- is.na(out)
    out[missing] <- suppressWarnings(as.Date(substr(x[missing], 1, if (fmt == "%Y%m%d") 8 else 10), format = fmt))
  }
  out
}

base_game_choices <- function(ids, data = NULL, id_col = "CustomGameID", team = NULL, team_col = "PitcherTeam") {
  ids <- unique(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(stats::setNames(character(0), character(0)))
  n <- length(ids)
  # Match once per game, rather than repeatedly scanning pitch-level data.
  rows <- if (is.data.frame(data) && id_col %in% names(data)) {
    data[match(ids, as.character(data[[id_col]])), , drop = FALSE]
  } else data.frame(row.names = seq_len(n))
  column <- function(names) {
    out <- rep("", n)
    for (name in intersect(names, names(rows))) {
      x <- trimws(as.character(rows[[name]]))
      use <- !nzchar(out) & !is.na(x) & nzchar(x)
      out[use] <- x[use]
    }
    out
  }
  dates <- base_game_date(column(c("GameDate", "game_date", "Date", "date", "UTCDate", "LocalDateTime")))
  missing <- is.na(dates)
  dates[missing] <- base_game_date(ids[missing])
  missing <- is.na(dates)
  dates[missing] <- base_game_date(column(c("source_file"))[missing])
  home <- column(c("HomeTeam", "home"))
  away <- column(c("AwayTeam", "away"))
  # Older custom IDs store the matchup directly: YYYY-MM-DD: AWAY @ HOME.
  encoded <- grepl(":.* @ ", ids)
  away[encoded & !nzchar(away)] <- sub("^.*:\\s*(.*?) @ .*", "\\1", ids[encoded & !nzchar(away)])
  home[encoded & !nzchar(home)] <- sub("^.* @ ", "", ids[encoded & !nzchar(home)])
  same <- function(a, b) toupper(trimws(a)) == toupper(trimws(b))
  own <- function(x) base_team_matches(x)
  if (is.null(team)) {
    team <- column(c(team_col, "CatcherTeam", "PitcherTeam", "BatterTeam"))
    # Team exports and scrimmages take precedence over mixed historical rows.
    team[own(home) | own(away)] <- TEAM_CONFIG$data_code
  }
  team <- rep_len(as.character(team), n)
  team[is.na(team) | !nzchar(team)] <- TEAM_CONFIG$data_code
  opponent <- column(c("Opponent", "opponent", "OpponentTeam", "Opp"))
  home_is_team <- same(home, team) | (own(home) & own(team))
  away_is_team <- same(away, team) | (own(away) & own(team))
  opponent[home_is_team & nzchar(away)] <- away[home_is_team & nzchar(away)]
  opponent[away_is_team & nzchar(home)] <- home[away_is_team & nzchar(home)]
  # Some exports omit home/away but retain the two sides of each pitch.
  pitcher <- column(c("PitcherTeam", "CatcherTeam", "FieldingTeam"))
  batter <- column(c("BatterTeam"))
  missing <- !nzchar(opponent)
  opponent[missing & same(pitcher, team)] <- batter[missing & same(pitcher, team)]
  missing <- !nzchar(opponent)
  opponent[missing & same(batter, team)] <- pitcher[missing & same(batter, team)]
  scrimmage <- (own(home) & own(away)) | (own(pitcher) & own(batter)) |
    (own(team) & own(opponent))
  label <- ids
  known <- !is.na(dates)
  date_text <- paste0(month.abb[as.integer(format(dates, "%m"))], " ",
                      as.integer(format(dates, "%d")), ", ", format(dates, "%Y"))
  label[known] <- date_text[known]
  vs <- known & nzchar(opponent) & !scrimmage
  opponent_names <- base_team_display_name(opponent)
  # School name for our club ("Texas State" rather than its mascot suffix).
  school <- TEAM_CONFIG$full_name
  suffix <- paste0(" ", TEAM_CONFIG$name)
  if (endsWith(school, suffix)) school <- substr(school, 1, nchar(school) - nchar(suffix))
  opponent_names[own(opponent)] <- school
  label[vs] <- paste(date_text[vs], "vs", opponent_names[vs])
  label[known & scrimmage] <- paste(date_text[known & scrimmage], "Scrimmage")
  # Keep doubleheaders distinguishable without exposing raw filenames.
  duplicate <- duplicated(label) | duplicated(label, fromLast = TRUE)
  for (text in unique(label[duplicate])) {
    which <- which(label == text)
    label[which] <- paste0(text, " (Game ", seq_along(which), ")")
  }
  stats::setNames(ids, label)
}

base_correct_player_names <- function(x) {
  x <- as.character(x)
  x <- gsub("\\bJustin\\s+Antcil\\b", "Justin Anctil", x, ignore.case = TRUE, perl = TRUE)
  gsub("\\bAntcil,\\s*Justin\\b", "Anctil, Justin", x, ignore.case = TRUE, perl = TRUE)
}
