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
  catalog <- season_source$catalog(role)
  if (!nrow(catalog) || !"OTHER_TEAM" %in% catalog$Team) fail("National player directory is incomplete.")
}
loads <- list()
original_load <- season_source$load_players
season_source$load_players <- function(role, team, players) {
  loads[[length(loads) + 1L]] <<- list(role = role, team = team, players = players)
  original_load(role, team, players)
}
workspace <- base_wally_scouting_environment(fixture_dir, season_source)
if (!is.function(workspace$server)) fail("Embedded Scouting server is unavailable.")

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
  season_catalogs()
  if (length(loads)) fail("Pitch data loaded before any season players were selected.")
  hitter_team <- season_fixture$BatterTeam[[1]]
  pitcher_team <- season_fixture$PitcherTeam[[1]]
  hitter <- season_fixture$Batter[[1]]
  pitcher <- season_fixture$Pitcher[[1]]
  session$setInputs(scout_hitter_team = hitter_team, scout_pitcher_team = pitcher_team)
  session$setInputs(scout_season_hitters = hitter, scout_season_pitchers = pitcher)
  session$flushReact()
  hitter_rows <- std_all()
  pitcher_rows <- pitcher_std_all()
  expected_hitter <- season_fixture[season_fixture$BatterTeam == hitter_team & season_fixture$Batter == hitter, ]
  expected_pitcher <- season_fixture[season_fixture$PitcherTeam == pitcher_team & season_fixture$Pitcher == pitcher, ]
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
  session$setInputs(scout_hitter_team = "OTHER_TEAM", scout_season_hitters = hitter)
  session$flushReact()
  if (!all(std_all()$BatterTeam == "OTHER_TEAM")) fail("Changing teams retained the previous school's player data.")
  session$setInputs(scout_season_hitters = character())
  cleared <- tryCatch(std_all(), shiny.silent.error = function(e) NULL)
  if (!is.null(cleared)) fail("Clearing the player selection retained stale season rows.")
})

missing_source <- base_scouting_season_source(paste0(season_path, "-missing"))
missing_message <- tryCatch({ missing_source$catalog("hitter"); "" }, error = conditionMessage)
if (!grepl("BASE_SCOUTING_SEASON_FILE", missing_message, fixed = TRUE)) fail("Missing season data has no actionable error.")
bad_path <- tempfile(fileext = ".parquet")
arrow::write_parquet(tibble::tibble(Unrelated = 1), bad_path)
bad_message <- tryCatch({ base_scouting_season_source(bad_path)$catalog("hitter"); "" }, error = conditionMessage)
if (!grepl("missing columns", bad_message, fixed = TRUE)) fail("Invalid season schema was accepted.")
unlink(c(season_path, bad_path))

cat(
  "Wally Scouting integration passed:", length(expected_tabs),
  "workflows, CSV compatibility, and on-demand season hitter/pitcher queries.\n"
)
