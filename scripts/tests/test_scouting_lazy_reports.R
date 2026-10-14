#!/usr/bin/env Rscript
source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
fixture_dir <- 'tests/fixtures/wallyapps/hitting/data'
file <- '2026 Season - cleaned.csv'
raw <- readr::read_csv(file.path(fixture_dir,file),show_col_types=FALSE)
catalog_calls <- character()
source <- list(
  teams=function(role) {stopifnot(role == 'pitcher'); 'TEST'},
  catalog=function(role,team) {
    catalog_calls <<- c(catalog_calls,role)
    data.frame(Team='TEST',Player=if(role=='pitcher')raw$Pitcher[1] else raw$Batter[1])
  },
  load_players=function(...) raw
)
w <- base_wally_scouting_environment(fixture_dir,source)
stopifnot(isTRUE(base_preload_wally_scouting(fixture_dir,source)$ok))
stuff_calls <- 0L
fail_stuff <- FALSE
original <- w$pitcher_bust_table_numeric
w$pitcher_bust_table_numeric <- function(...) {
  stuff_calls <<- stuff_calls+1L
  if(fail_stuff)stop('fixture report calculation failure')
  original(...)
}
shiny::testServer(w$server, {
  session$setInputs(main_tab='rick_advance',scout_data_source='season',scout_team='TEST',
    pitcher_bust_hand='Total',scout_season_pitchers=raw$Pitcher[1])
  session$flushReact()
  stopifnot(!session$isClosed(),stuff_calls==0L,!('hitter' %in% catalog_calls),
            nrow(pitcher_std_all())>0)
  # Opening the hitter tab loads its directory on demand.
  session$setInputs(main_tab='Hitter Card',scout_season_hitters=raw$Batter[1])
  session$flushReact()
  stopifnot('hitter' %in% catalog_calls,stuff_calls==0L,nrow(std_all())>0)
  session$setInputs(main_tab='matchup_grid')
  session$flushReact()
  stopifnot(nrow(matchup_hitter_meta())>0,nrow(matchup_pitcher_meta())>0,stuff_calls==0L)
  session$setInputs(main_tab='Stuff Sheet')
  session$flushReact()
  stopifnot(stuff_calls>0L,nrow(pitcher_bust_meta())>0,!session$isClosed())
  # A bad report cannot disconnect the app, and another selection can recover.
  fail_stuff <<- TRUE
  session$setInputs(pitcher_bust_hand='LHP')
  session$flushReact()
  err <- tryCatch(pitcher_bust_meta(),error=identity)
  stopifnot(inherits(err,'shiny.silent.error'),!session$isClosed())
  fail_stuff <<- FALSE
  session$setInputs(pitcher_bust_hand='Total')
  session$flushReact()
  stopifnot(nrow(pitcher_bust_meta())>0,!session$isClosed())
})
stuff_calls <- 0L
shiny::testServer(w$server, {
  session$setInputs(main_tab='rick_advance',scout_data_source='csv',csv_files=file,
                    pitcher_bust_hand='Total')
  session$flushReact()
  stopifnot(stuff_calls==0L,nrow(pitcher_std_all())>0,!session$isClosed())
  session$setInputs(main_tab='Stuff Sheet')
  session$flushReact()
  stopifnot(stuff_calls>0L,nrow(pitcher_bust_meta())>0,!session$isClosed())
})
cat('Scouting role directories, lazy reports, CSV loading, and calculation-error recovery passed.\n')
