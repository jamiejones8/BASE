source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
fixture_dir <- 'tests/fixtures/wallyapps/hitting/data'
raw <- readr::read_csv(file.path(fixture_dir,'2026 Season - cleaned.csv'), show_col_types=FALSE)
failing <- TRUE
src <- list(
 teams=function(role)'TEST',
 catalog=function(role,team) {
   if (failing) stop('test player directory unavailable')
   data.frame(Team='TEST',Player=if(role=='pitcher')raw$Pitcher[1] else raw$Batter[1])
 },
 load_players=function(role,team,players) raw
)
w <- base_wally_scouting_environment(fixture_dir,src)
# Reproduce the multi-workspace package namespace collision.
w$validate <- function(...) stop('wrong validate function called')
w$need <- function(...) stop('wrong need function called')
shiny::testServer(w$server, {
 session$setInputs(scout_data_source='season',scout_team='TEST',scout_season_pitchers=raw$Pitcher[1])
 session$flushReact()
 stopifnot(!session$isClosed())
 err <- tryCatch(pitcher_std_all(),error=identity)
 stopifnot(inherits(err,'shiny.silent.error'),grepl('directory unavailable',conditionMessage(err)))
 # A missing season file must be a validation message, not a fatal observer error.
 TEAM_CONFIG$data$team_season_import_dir <- tempfile('missing-seasons-')
 session$setInputs(scout_data_source='F26')
 session$flushReact()
 stopifnot(!session$isClosed())
 err <- tryCatch(season_teams(),error=identity)
 stopifnot(inherits(err,'shiny.silent.error'))
 # Recover in the same session and render Rick's pitcher selector.
 failing <<- FALSE
 session$setInputs(scout_data_source='season',scout_team='TEST',scout_season_pitchers=raw$Pitcher[1])
 session$flushReact()
 stopifnot(!session$isClosed(),nrow(pitcher_std_all())>0)
 stopifnot(grepl('rick_pitcher',output$rick_pitcher_ui$html,fixed=TRUE))
 # Missing CSVs exercise a second validation path under namespace masking.
 session$setInputs(scout_data_source='csv',csv_files='does-not-exist.csv')
 err <- tryCatch(df_all(),error=identity)
 stopifnot(inherits(err,'shiny.silent.error'),!session$isClosed())
})
cat('Scouting missing directories, missing seasons, masked validators, and same-session recovery passed.\n')
