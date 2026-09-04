# Team schedule readers used by the Home scoreboard.

parse_base_schedule_times <- function(schedule) {
  if ("DateTime" %in% names(schedule)) {
    raw_times <- trimws(as.character(schedule$DateTime))
  } else if ("Date" %in% names(schedule)) {
    game_clock <- if ("Time" %in% names(schedule)) {
      trimws(as.character(schedule$Time))
    } else {
      rep("12:00 PM", nrow(schedule))
    }
    raw_times <- paste(trimws(as.character(schedule$Date)), game_clock)
  } else {
    return(as.POSIXct(character(), tz = TEAM_CONFIG$schedule_timezone))
  }

  parsed <- suppressWarnings(lubridate::parse_date_time(
    raw_times,
    orders = c(
      "ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p",
      "mdy HMS", "mdy HM", "mdy IMS p", "mdy IM p", "mdy",
      "Ymd HMS", "Ymd HM"
    ),
    tz = TEAM_CONFIG$schedule_timezone,
    quiet = TRUE
  ))
  as.POSIXct(parsed, tz = TEAM_CONFIG$schedule_timezone)
}

read_next_game_from_schedule <- function(path = TEAM_CONFIG$data$schedule_file) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  schedule <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message("Unable to read BASE_SCHEDULE_FILE: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(schedule)) return(NULL)

  has_datetime <- "DateTime" %in% names(schedule) || "Date" %in% names(schedule)
  if (!has_datetime || !"Opponent" %in% names(schedule)) {
    message("BASE_SCHEDULE_FILE must contain Opponent plus DateTime or Date/Time columns")
    return(NULL)
  }

  game_times <- parse_base_schedule_times(schedule)
  active_rows <- rep(TRUE, nrow(schedule))
  if ("Status" %in% names(schedule)) {
    active_rows <- !tolower(trimws(as.character(schedule$Status))) %in%
      c("cancelled", "canceled", "final", "completed", "postponed")
  }
  upcoming <- which(active_rows & !is.na(game_times) & game_times >= Sys.time())
  if (!length(upcoming)) return(NULL)
  i <- upcoming[which.min(game_times[upcoming])]

  value_or <- function(column, default) {
    if (!column %in% names(schedule) || is.na(schedule[[column]][i]) ||
        !nzchar(trimws(as.character(schedule[[column]][i])))) {
      default
    } else {
      schedule[[column]][i]
    }
  }
  bool_or <- function(column, default = TRUE) {
    value <- tolower(trimws(as.character(value_or(column, default))))
    if (value %in% c("true", "t", "1", "yes", "home", "h")) TRUE
    else if (value %in% c("false", "f", "0", "no", "away", "a")) FALSE
    else default
  }
  game_time <- game_times[i]

  list(
    opponent = as.character(schedule$Opponent[i]),
    venue = as.character(value_or("Venue", "Venue TBD")),
    is_home = bool_or("IsHome", TRUE),
    datetime = game_time,
    time_str = format(game_time, "%A, %B %d · %I:%M %p"),
    ms = as.numeric(game_time) * 1000,
    wins = as.integer(value_or("TeamWins", 0L)),
    losses = as.integer(value_or("TeamLosses", 0L)),
    opp_wins = as.integer(value_or("OppWins", 0L)),
    opp_losses = as.integer(value_or("OppLosses", 0L)),
    opp_abbr = as.character(value_or("OppAbbr", "OPP"))
  )
}

fetch_next_team_game <- function() {
  configured_game <- read_next_game_from_schedule()
  if (!is.null(configured_game)) return(configured_game)
  if (!isTRUE(TEAM_CONFIG$stats_api_enabled) || is.na(TEAM_CONFIG$mlb_team_id)) {
    return(NULL)
  }

  resp <- tryCatch(
    httr::GET(
      paste0(
        "https://statsapi.mlb.com/api/v1/schedule",
        "?sportId=", TEAM_CONFIG$sport_id,
        "&leagueId=", TEAM_CONFIG$league_id,
        "&teamId=", TEAM_CONFIG$mlb_team_id,
        "&startDate=", format(Sys.Date(), "%Y-%m-%d"),
        "&endDate=", format(Sys.Date() + 30, "%Y-%m-%d"),
        "&hydrate=team,venue"
      ),
      httr::timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::http_error(resp)) return(NULL)

  sched <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  if (!length(sched$dates)) return(NULL)

  for (date in sched$dates) {
    game <- date$games[[1]]
    if (!game$status$abstractGameState %in% c("Preview", "Live")) next

    is_home <- game$teams$home$team$id == TEAM_CONFIG$mlb_team_id
    opponent <- if (is_home) game$teams$away$team$name else game$teams$home$team$name
    team_side <- if (is_home) game$teams$home else game$teams$away
    opp_side <- if (is_home) game$teams$away else game$teams$home
    game_dt_utc <- as.POSIXct(
      game$gameDate,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    game_dt_local <- lubridate::with_tz(
      game_dt_utc,
      TEAM_CONFIG$schedule_timezone
    )

    teams_resp <- tryCatch(
      httr::GET(
        paste0(
          "https://statsapi.mlb.com/api/v1/teams?leagueId=",
          TEAM_CONFIG$league_id
        ),
        httr::timeout(10)
      ),
      error = function(e) NULL
    )
    opp_abbr <- if (!is.null(teams_resp) && !httr::http_error(teams_resp)) {
      teams <- jsonlite::fromJSON(
        httr::content(teams_resp, "text", encoding = "UTF-8"),
        simplifyVector = TRUE
      )$teams
      row <- teams[teams$name == opponent, ]
      if (nrow(row)) row$abbreviation[[1]] else "OPP"
    } else {
      "OPP"
    }

    return(list(
      opponent = opponent,
      venue = game$venue$name,
      is_home = is_home,
      datetime = game_dt_local,
      time_str = format(game_dt_local, "%A, %B %d · %I:%M %p"),
      ms = as.numeric(game_dt_utc) * 1000,
      wins = team_side$leagueRecord$wins,
      losses = team_side$leagueRecord$losses,
      opp_wins = opp_side$leagueRecord$wins,
      opp_losses = opp_side$leagueRecord$losses,
      opp_abbr = opp_abbr
    ))
  }

  NULL
}
