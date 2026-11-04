#!/usr/bin/env Rscript

source("team_config.R")
source("R/services/schedule.R")

now <- as.POSIXct("2026-11-04 12:00:00", tz = "America/Chicago")
fixture <- tempfile(fileext = ".csv")

utils::write.csv(data.frame(DateTime = "2026-11-01 6:00 PM", Opponent = "Past game"), fixture, row.names = FALSE)
stopifnot(is.null(read_next_game_from_schedule(fixture, now)))
utils::write.csv(data.frame(
  DateTime = c("2027-02-20 6:00 PM", "2027-02-19 6:00 PM", "2027-02-18 6:00 PM"),
  Opponent = c("Later", "Fixture Oakland", "Cancelled"),
  Status = c("scheduled", "scheduled", "cancelled")
), fixture, row.names = FALSE)
game <- read_next_game_from_schedule(fixture, now)
stopifnot(game$opponent == "Fixture Oakland", format(game$datetime, "%H:%M") == "18:00",
          isTRUE(game$is_home), !game$is_neutral, !game$time_is_estimated,
          is.null(read_next_game_from_schedule(paste0(fixture, "missing"), now)))
games <- read_upcoming_games_from_schedule(fixture, now)
stopifnot(identical(vapply(games, `[[`, character(1), "opponent"), c("Fixture Oakland", "Later")),
          length(read_upcoming_games_from_schedule(fixture, now, limit = 1L)) == 1L,
          length(read_upcoming_games_from_schedule(fixture, now, limit = 0L)) == 0L,
          length(read_upcoming_games_from_schedule(paste0(fixture, "missing"), now)) == 0L)

# Both headers, common flag values, and old neutral uploads remain compatible.
for (header in c("IsHome", "IsHomeAway")) {
  for (flag in c("TRUE", "HOME", "FALSE", "AWAY", "NEUTRAL", "NUETRAL", " neutral ")) {
    row <- data.frame(Date = "02/24/2027", Time = "6:00 PM", Opponent = "Opponent")
    row[[header]] <- flag
    utils::write.csv(row, fixture, row.names = FALSE)
    game <- read_next_game_from_schedule(fixture, now)
    neutral <- toupper(trimws(flag)) %in% c("NEUTRAL", "NUETRAL")
    stopifnot(identical(game$is_neutral, neutral),
              identical(game$is_home, flag %in% c("TRUE", "HOME")))
  }
}
utils::write.csv(data.frame(
  DateTime = "2027-02-24 18:00:00", Opponent = "Opponent",
  IsHome = TRUE, IsHomeAway = "NEUTRAL"
), fixture, row.names = FALSE)
game <- read_next_game_from_schedule(fixture, now)
stopifnot(game$is_neutral, !game$is_home)
unlink(fixture)

schedule <- readr::read_csv(TEAM_CONFIG$data$schedule_file, show_col_types = FALSE)
times <- parse_base_schedule_times(schedule)
reference <- readr::read_csv(TEAM_CONFIG$data$ncaa_colors_file, show_col_types = FALSE)
stopifnot(nrow(schedule) == 61L, !anyNA(times), !is.unsorted(times),
          sum(format(times, "%Y") == "2027") == 57L,
          sum(schedule$IsHomeAway == "NEUTRAL") == 5L,
          all(schedule$TimeIsEstimated[format(times, "%Y") == "2027"]))
known_opponents <- schedule$Opponent != "TBD"
stopifnot(!anyNA(schedule$OppAbbr[known_opponents]),
          all(schedule$OppAbbr[known_opponents] %in% reference$team_abbr))
logos <- reference$NCAA_img[match(schedule$OppAbbr[known_opponents], reference$team_abbr)]
stopifnot(all(!is.na(logos) & nzchar(logos)))
event_logos <- unique(na.omit(schedule$EventLogo))
stopifnot(length(event_logos) == 3L, all(file.exists(file.path("www", event_logos))))

opener <- read_next_game_from_schedule(now = now)
next_four <- read_upcoming_games_from_schedule(now = now)
stopifnot(length(next_four) == 4L, identical(next_four[[1]], opener),
          identical(vapply(next_four, `[[`, character(1), "opponent"),
                    c("Oakland", "Oakland", "Oakland", "Oklahoma State")),
          identical(vapply(next_four, function(game) format(game$datetime, "%Y-%m-%d"), character(1)),
                    c("2027-02-19", "2027-02-20", "2027-02-21", "2027-02-24")))
stopifnot(opener$opponent == "Oakland", opener$opp_abbr == "OAK_GOL", opener$is_home,
          format(opener$datetime, "%Y-%m-%d %H:%M") == "2027-02-19 18:00",
          opener$time_is_estimated, grepl("CST$", opener$time_str))
for (opponent in c("San Diego State", "Oregon State", "Washington State")) {
  starts <- format(times[schedule$Opponent == opponent], "%H:%M")
  stopifnot(identical(starts, c("20:00", "16:00", "15:00")))
}
stopifnot(identical(format(times[schedule$Opponent == "UTRGV"], "%H:%M"),
                    c("18:00", "14:00", "13:00")))

game_on <- function(date) read_next_game_from_schedule(
  now = as.POSIXct(paste(date, "00:00:00"), tz = "America/Chicago")
)
frisco <- game_on("2027-02-24")
round_rock <- game_on("2027-02-26")
away <- game_on("2027-04-16")
tournament <- game_on("2027-05-25")
conference <- game_on("2027-04-09")
all_games <- read_upcoming_games_from_schedule(
  now = as.POSIXct("2026-01-01", tz = "America/Chicago"), limit = nrow(schedule)
)
conference_games <- Filter(function(game) identical(game$event, "Pac-12 Conference Game"), all_games)
stopifnot(length(conference_games) == 18L,
          all(vapply(conference_games, function(game) identical(game$event_logo, "pac12logo.webp"), logical(1))),
          conference$opp_abbr == "GON_BUL", conference$event_logo == "pac12logo.webp",
          !nzchar(game_on("2027-03-19")$event_logo),
          length(read_upcoming_games_from_schedule(now = tournament$datetime)) == 1L,
          length(read_upcoming_games_from_schedule(now = tournament$datetime + 1)) == 0L)
stopifnot(frisco$is_neutral, !frisco$is_home, frisco$opp_abbr == "OKL_COW",
          frisco$event_logo == "events/frisco-classic.jpeg",
          round_rock$is_neutral, round_rock$opp_abbr == "TEX_RAI",
          round_rock$event_logo == "events/round-rock-classic.avif",
          !away$is_home, !away$is_neutral, grepl("CDT$", away$time_str),
          as.numeric(away$datetime) == as.numeric(as.POSIXct("2027-04-17 01:00:00", tz = "UTC")),
          tournament$opponent == "TBD", tournament$opp_abbr == "OPP",
          tournament$is_neutral, tournament$event_logo == "pac12logo.webp",
          is.null(game_on("2027-05-26")))

# Render the production scoreboard expression without initializing data workspaces.
app <- parse("R/app_main.R")
server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                   identical(x[[2]], as.name("server")), as.list(app))[[1]][[3]]
initialize <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("base_auth_initialize_server")),
                     as.list(server[[3]]))[[1]][[3]]
scoreboard <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                       identical(x[[2]], quote(output$scoreboard_hero)), as.list(initialize[[3]]))[[1]][[3]][[2]]
tags <- htmltools::tags
tagList <- htmltools::tagList
HTML <- htmltools::HTML
opponent_logo <- function(code) reference$NCAA_img[match(code, reference$team_abbr)]
render_scoreboard <- function(game, games = NULL) {
  next_games_reactive <- function() {
    if (!is.null(games)) games
    else if (is.null(game)) list()
    else read_upcoming_games_from_schedule(now = game$datetime)
  }
  htmltools::renderTags(eval(scoreboard))$html
}
for (game in list(frisco, round_rock)) {
  html <- render_scoreboard(game)
  stopifnot(grepl('home-game-location-badge">Neutral<', html),
            grepl('home-matchup-versus">VS<', html),
            grepl(game$event_logo, html, fixed = TRUE),
            grepl(opponent_logo(game$opp_abbr), html, fixed = TRUE),
            grepl("Estimated start time", html, fixed = TRUE))
}
stopifnot(grepl('home-game-location-badge">Home<', render_scoreboard(opener)),
          grepl('home-matchup-versus">AT<', render_scoreboard(away)),
          !grepl('alt="TBD logo"', render_scoreboard(tournament), fixed = TRUE),
          grepl("Schedule pending", render_scoreboard(NULL), fixed = TRUE))
opener_html <- render_scoreboard(opener)
stopifnot(grepl("Next 4", opener_html, fixed = TRUE),
          length(gregexpr('class="home-upcoming-game"', opener_html, fixed = TRUE)[[1]]) == 4L,
          grepl('datetime="2027-02-24T18:00:00-0600"', opener_html, fixed = TRUE),
          grepl("events/frisco-classic.jpeg", opener_html, fixed = TRUE),
          !grepl("home-next-four-grid", render_scoreboard(NULL), fixed = TRUE))

# Optional local preview used for visual checks at desktop and mobile sizes.
preview_dir <- Sys.getenv("BASE_SCHEDULE_PREVIEW_DIR")
if (nzchar(preview_dir)) {
  dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in c("opener", "frisco", "round_rock", "away", "conference", "tournament")) {
    html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
                   '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&amp;family=Courier+Prime&amp;family=Source+Sans+3:wght@400;600&amp;display=swap">',
                   '<link rel="stylesheet" href="/www/styles.css"><base href="/www/"></head><body>',
                   render_scoreboard(get(name)), '</body></html>')
    writeLines(html, file.path(preview_dir, paste0(name, ".html")))
  }
}
cat("Schedule import, flags, timezones, estimates, logos, and scoreboard rendering passed.\n")
