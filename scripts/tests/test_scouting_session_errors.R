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
# CSV launch must never touch the national directory before its radio input binds.
national_reads <- 0L
w$SCOUTING_SEASON_SOURCE$teams <- function(role) {
  national_reads <<- national_reads+1L
  stop('unexpected national directory access')
}
shiny::testServer(function(input,output,session) {
  w$server(input,output,session,initial_source='csv')
}, {
  session$setInputs(csv_files=basename(file.path(fixture_dir,'2026 Season - cleaned.csv')))
  session$flushReact()
  stopifnot(!session$isClosed())
})
stopifnot(national_reads == 0L)
# Production hitter metadata and rows stay team-filtered; never invoke the
# global hitter catalog builder or a loader that indirectly builds it.
path <- tempfile(fileext='.parquet')
arrow::write_parquet(raw,path)
Sys.setenv(BASE_SCOUTING_SEASON_FILE=path)
base_pitcher_catalog <- data.frame(PitcherTeam=raw$PitcherTeam[1],Pitcher=raw$Pitcher[1],PitchCount=1)
base_get_hitter_catalog <- function(...) stop('national catalog must remain lazy')
base_load_hitter_rows <- function(...) stop('global hitter loader must remain lazy')
base_load_pitcher_rows <- function(...)raw
bounded <- base_scouting_season_source(path)
stopifnot(length(bounded$teams('hitter'))>0,
          nrow(bounded$catalog('hitter',raw$BatterTeam[1]))>0,
          nrow(bounded$load_players('hitter',raw$BatterTeam[1],raw$Batter[1]))>0)
unlink(path);Sys.unsetenv('BASE_SCOUTING_SEASON_FILE')
cat('CSV launch isolation and team-filtered national hitter loading passed.\n')

for (id in c('csv','F26','PS27','S27','season')) {
  ui <- base_scouting_initial_ui(w$ui,id)
  checked <- character()
  visit <- function(node) {
    if(inherits(node,'shiny.tag')) {
      if(identical(node$name,'select') && identical(node$attribs$id,'scout_data_source')) {
        for(option in node$children) if(inherits(option,'shiny.tag') && !is.null(option$attribs$selected))
          checked <<- c(checked,option$attribs$value)
      }
      lapply(node$children,visit)
    } else if(is.list(node))lapply(node,visit)
    invisible(NULL)
  }
  visit(ui);stopifnot(identical(checked,id))
}
cat('Initial source dropdown bindings match each launch mode.\n')
