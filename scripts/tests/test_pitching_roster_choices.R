#!/usr/bin/env Rscript
# Roster visibility changes options only, including saved-goal names, not data.
suppressPackageStartupMessages(library(shiny))
helpers <- c("SEASON_CHOICES", "name_display", "name_norm", "pitching_player_choices")
for (expr in parse("WallyApps/PitchingApp/PitchingApp.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]])[1] %in% helpers) eval(expr)
}
players <- c("Bennett Fryman", "Jackson Cotton", "Cade Smith", "Fryman, Bennett", "Cotton, Jackson")
for (season in c("F26", "PS27", "S27", "F27", "PS28", "S28", "2026 Fall")) {
  stopifnot(identical(pitching_player_choices(players, season), "Cade Smith"))
}
for (seasons in list("S25", "F25", "SQ26", "S26", c("S26", "F26"), character(), "PORT")) {
  stopifnot(identical(pitching_player_choices(players, seasons), players))
}
stopifnot(!length(pitching_player_choices(players[1:2], "F26")),
  identical(pitching_player_choices(c(" bennett  FRYMAN ", "JACKSON COTTON", "Cade Smith"), "S27"), "Cade Smith"))

source("team_config.R")
source("WallyApps/shared/development_goals.R")
path <- tempfile(fileext = ".sqlite")
on.exit(unlink(path), add = TRUE)
invisible(development_store_save(path, list(player = "Bennett Fryman", player_key = "bennettfryman",
  mode = "pitching", baseline_season = "S26", objectives = list(list(title = "Improve K rate",
    metric = "K%", goal = "25%", target = .25, baseline_value = .2,
    supports = c("Action one", "Action two", "Action three"), drills = character())))))
fixture <- expand.grid(Pitcher = c("Fryman, Bennett", "Cotton, Jackson", "Smith, Cade"),
  SeasonGroup = c("S26", "F26", "PS27"), stringsAsFactors = FALSE)
seasons <- c("2026 Season" = "S26", "2026 Fall" = "F26", "2027 Scrimmages" = "PS27")
messages <- list()
for (mode in c("pitching", "hitting")) {
  args <- list(rows = reactive(fixture), player_column = "Pitcher", seasons = seasons,
    summarize = function(d) list(`K%` = .2), percentile = function(value, metric) 50,
    mode = mode, store_path = path)
  if (mode == "pitching") args$player_choices <- pitching_player_choices
  testServer(development_server, args = args, {
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(id, message) messages[[id]] <<- message
    session$setInputs(season = "S26")
    stopifnot(grepl("Fryman", messages$player$options), grepl("Cotton", messages$player$options))
    for (season in c("F26", "PS27")) {
      session$setInputs(season = season)
      options <- messages$player$options
      stopifnot(grepl("Smith", options),
        grepl("Fryman", options) == (mode == "hitting"),
        grepl("Cotton", options) == (mode == "hitting"))
    }
    session$setInputs(season = "S26")
    stopifnot(grepl("Fryman", messages$player$options), grepl("Cotton", messages$player$options))
  })
}
testServer(base_comparison_server, args = list(rows = reactive(fixture), seasons = seasons,
  player_column = "Pitcher", summarize = function(d) list(`K%` = .2),
  percentile = function(value, metric) 50, metrics = "K%", lower_better = character(),
  player_choices = pitching_player_choices), {
  root_session <- session$rootScope()
  root_session$sendInputMessage <- function(id, message) messages[[id]] <<- message
  session$setInputs(local_season_left = "S26", local_player_left = "Fryman, Bennett",
    local_season_right = "F26", opponents_season_left = "PS27")
  stopifnot(grepl("Fryman", messages$local_player_left$options),
    !grepl("Fryman|Cotton", messages$local_player_right$options),
    !grepl("Fryman|Cotton", messages$opponents_player_left$options))
  session$setInputs(local_season_left = "F26", local_season_right = "S26")
  stopifnot(!grepl("Fryman|Cotton", messages$local_player_left$options),
    identical(messages$local_player_left$value, "Team Total"),
    grepl("Fryman", messages$local_player_right$options))
})
stopifnot(nrow(fixture) == 9L, length(development_store_read(path, "pitching")) == 1L)
cat("Pitcher choices: Fall 2026 cutoff, historical/mixed seasons, name formats, saved goals, comparisons and hitting isolation passed.\n")
