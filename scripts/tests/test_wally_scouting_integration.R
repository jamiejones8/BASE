#!/usr/bin/env Rscript

source("team_config.R", local = FALSE)
source("R/integrations/wally_scouting_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_dir <- file.path(
  "tests", "fixtures", "wallyapps", "hitting", "data"
)
fixture_files <- list.files(fixture_dir, pattern = "\\.csv$", full.names = FALSE)
if (!length(fixture_files)) fail("Scouting integration fixture is unavailable.")

season_fixture <- readr::read_csv(file.path(fixture_dir, fixture_files[[1]]), show_col_types = FALSE)
# A same-name player at a different school must not leak into either query.
other_team <- season_fixture
other_team$BatterTeam <- "OTHER_TEAM"
other_team$PitcherTeam <- "OTHER_TEAM"
other_team$PitchUID <- paste0("other-", other_team$PitchUID)
season_fixture <- dplyr::bind_rows(season_fixture, other_team)
season_path <- tempfile(fileext = ".parquet")
arrow::write_parquet(season_fixture, season_path)
season_source <- base_scouting_season_source(season_path)
for (role in c("hitter", "pitcher")) {
  teams <- season_source$teams(role)
  catalog <- season_source$catalog(role, "OTHER_TEAM")
  if (!"OTHER_TEAM" %in% teams || !nrow(catalog) ||
      !all(catalog$Team == "OTHER_TEAM")) {
    fail("Team-first national player directory is incomplete.")
  }
}
loads <- list()
original_load <- season_source$load_players
season_source$load_players <- function(role, team, players) {
  loads[[length(loads) + 1L]] <<- list(role = role, team = team, players = players)
  original_load(role, team, players)
}
workspace <- base_wally_scouting_environment(fixture_dir, season_source)
if (!is.function(workspace$server)) fail("Embedded Scouting server is unavailable.")
if ("package:MASS" %in% search()) {
  fail("ScoutingApp attached MASS and can mask dplyr functions in other BASE workspaces.")
}
launch_html <- paste(as.character(base_opponent_scouting_workspace_ui()), collapse = "")
if (!all(vapply(
  c("base-scouting-launch", "base_scouting_team", "base_scouting_open", "base_scouting_open_csv"),
  grepl, logical(1), x = launch_html, fixed = TRUE
))) {
  fail("Opponent Scouting does not expose the deferred team-first launch screen.")
}

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Scouting UI contains a nested full-page body element.")
}
if (!grepl("base-scouting-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Scouting UI is missing its scoped BASE layout wrapper.")
}

expected_tabs <- c(
  "Hitter Card", "Pitch Type Tables", "Heat Maps",
  "Pitcher Card", "Stuff Sheet", "Matchup Grid"
)
missing_tabs <- expected_tabs[
  !vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)
]
if (length(missing_tabs)) {
  fail("Embedded Scouting UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

shiny::testServer(workspace$server, {
  session$setInputs(scout_data_source = "csv")
  available <- data_files()
  if (!all(fixture_files %in% available)) {
    fail("ScoutingApp is not reading its BASE-configured data directory.")
  }
  session$setInputs(csv_files = fixture_files[[1]])
  session$flushReact()
  hitter_rows <- std_all()
  pitcher_rows <- pitcher_std_all()
  if (!nrow(hitter_rows)) fail("Hitter scouting standardization returned no fixture rows.")
  if (!nrow(pitcher_rows)) fail("Pitcher scouting standardization returned no fixture rows.")
  expected_splits <- c("Total", "v LHP", "v RHP")
  if (!identical(workspace$row1_table_numeric(hitter_rows)$Split, expected_splits) ||
      !identical(workspace$fps_metrics_by_hand(hitter_rows)$Split, expected_splits) ||
      !identical(workspace$fps_metrics_by_hand(hitter_rows[0, ])$Split, expected_splits)) {
    fail("Scouting handedness tables are not Total, left, right.")
  }
})

shiny::testServer(workspace$server, {
  session$setInputs(scout_data_source = "season")
  session$flushReact()
  season_teams()
  if (length(loads)) fail("Pitch data loaded before any season players were selected.")
  original_team <- season_fixture$BatterTeam[[1]]
  hitter_team <- "OTHER_TEAM"
  hitter <- season_fixture$Batter[[1]]
  pitcher <- season_fixture$Pitcher[[1]]
  session$setInputs(base_scouting_team = hitter_team)
  session$flushReact()
  session$setInputs(scout_season_hitters = hitter, scout_season_pitchers = pitcher)
  session$flushReact()
  hitter_rows <- std_all()
  pitcher_rows <- pitcher_std_all()
  expected_hitter <- season_fixture[season_fixture$BatterTeam == hitter_team & season_fixture$Batter == hitter, ]
  expected_pitcher <- season_fixture[season_fixture$PitcherTeam == hitter_team & season_fixture$Pitcher == pitcher, ]
  if (!setequal(hitter_rows$PitchUID, expected_hitter$PitchUID)) fail("Season hitter query is incomplete or includes another player/team.")
  if (!setequal(pitcher_rows$PitchUID, expected_pitcher$PitchUID)) fail("Season pitcher query is incomplete or includes another player/team.")
  if (!identical(matchup_hitters_raw()$PitchUID, season_hitter_rows()$PitchUID) ||
      !identical(matchup_pitchers_raw()$PitchUID, season_pitcher_rows()$PitchUID)) {
    fail("Matchup Grid did not reuse the selected season players.")
  }
  if (!nrow(matchup_hitters_std()) || !nrow(matchup_pitchers_std())) fail("Season matchup standardization failed.")
  # A selected pitcher may have no pitches against one side. Both Total pages
  # must render, including the empty-side message in the embedded environment.
  render_path <- tempfile(fileext = ".pdf")
  grDevices::pdf(render_path)
  pages <- build_pitcher_card_pages(pitcher_rows, "Total")
  if (length(pages) != 2L) fail("Pitcher card is missing a handedness page.")
  for (page in pages) ggplot2::ggplotGrob(page)
  grDevices::dev.off()
  unlink(render_path)
  if (!all(hitter_rows$.source_file == basename(season_path))) fail("Season source tracking was lost.")
  # Preserve the CSV report's numbers when the same pitches come from Parquet.
  expected_std <- workspace$std_cols(expected_hitter)
  if (!isTRUE(all.equal(workspace$row1_table_numeric(hitter_rows), workspace$row1_table_numeric(expected_std)))) {
    fail("Season routing changed hitter report metrics.")
  }
  session$setInputs(base_scouting_team = original_team, scout_season_hitters = hitter)
  session$flushReact()
  session$setInputs(scout_season_hitters = hitter)
  session$flushReact()
  if (!all(std_all()$BatterTeam == original_team)) fail("Changing teams retained the previous school's player data.")
  session$setInputs(scout_season_hitters = character())
  cleared <- tryCatch(std_all(), shiny.silent.error = function(e) NULL)
  if (!is.null(cleared)) fail("Clearing the player selection retained stale season rows.")
})

# Merely opening the tab must register the lightweight launch screen without
# sourcing or starting the full scouting application.
original_environment <- base_wally_scouting_environment
environment_calls <- 0L
server_calls <- 0L
base_wally_scouting_environment <- function(...) {
  environment_calls <<- environment_calls + 1L
  list(
    ui = htmltools::tags$div("Deferred scouting fixture"),
    server = function(input, output, session) server_calls <<- server_calls + 1L
  )
}
shiny::testServer(
  function(input, output, session) {
    base_opponent_scouting_workspace_server(
      input, output, session,
      data_dir = fixture_dir,
      season_source = season_source
    )
  },
  {
    session$flushReact()
    if (environment_calls != 0L || server_calls != 0L) {
      fail("Opponent Scouting initialized its full engine before a launch action.")
    }
    session$setInputs(base_scouting_team = "OTHER_TEAM", base_scouting_open = 1)
    session$flushReact()
    session$flushReact()
    if (environment_calls != 1L || server_calls != 1L) {
      fail("Opponent Scouting did not initialize exactly once after opening a team.")
    }
  }
)
base_wally_scouting_environment <- original_environment

missing_source <- base_scouting_season_source(paste0(season_path, "-missing"))
missing_message <- tryCatch({ missing_source$teams("hitter"); "" }, error = conditionMessage)
if (!grepl("BASE_SCOUTING_SEASON_FILE", missing_message, fixed = TRUE)) fail("Missing season data has no actionable error.")
bad_path <- tempfile(fileext = ".parquet")
arrow::write_parquet(tibble::tibble(Unrelated = 1), bad_path)
bad_message <- tryCatch({ base_scouting_season_source(bad_path)$teams("hitter"); "" }, error = conditionMessage)
if (!grepl("missing columns", bad_message, fixed = TRUE)) fail("Invalid season schema was accepted.")
unlink(c(season_path, bad_path))

cat(
  "Wally Scouting integration passed:", length(expected_tabs),
  "workflows, CSV compatibility, and on-demand season hitter/pitcher queries.\n"
)
