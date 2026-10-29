source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
scratch <- tempfile('scout-hitter-teams-'); dir.create(scratch)
raw <- readr::read_csv('tests/fixtures/wallyapps/hitting/data/2026 Season - cleaned.csv', show_col_types=FALSE)
a <- raw; a$BatterTeam <- 'TEX_BOB'; a$Batter <- 'Texas State Hitter'
b <- raw; b$BatterTeam <- 'OPP'; b$Batter <- 'Opponent Hitter'
path <- file.path(scratch,'mixed.csv')
readr::write_csv(dplyr::bind_rows(a,b),path)
w <- base_wally_scouting_environment(scratch)
shiny::testServer(w$server, {
  session$setInputs(main_tab='Hitter Card',scout_data_source='csv',csv_files='mixed.csv')
  stopifnot(setequal(hitter_teams(),c('TEX_BOB','OPP')))
  err <- tryCatch(std_all(),error=identity)
  stopifnot(inherits(err,'shiny.silent.error'))
  session$setInputs(hitter_team='TEX_BOB')
  stopifnot(all(std_all()$BatterTeam=='TEX_BOB'), identical(game_hitter_order(),'Texas State Hitter'),
            length(report_hitter_order())==0L)
  session$setInputs(hitter_order='Texas State Hitter')
  stopifnot(identical(report_hitter_order(),'Texas State Hitter'))
  session$setInputs(hitter_team='OPP')
  stopifnot(all(std_all()$BatterTeam=='OPP'), identical(game_hitter_order(),'Opponent Hitter'),
            length(report_hitter_order())==0L)
  session$setInputs(main_tab='matchup_grid',matchup_hitters_file=data.frame(datapath=path))
  stopifnot(all(matchup_hitters_std()$BatterTeam=='OPP'),
            identical(matchup_hitter_choices(),'Opponent Hitter'))
  session$setInputs(hitter_team='TEX_BOB')
  stopifnot(identical(matchup_hitter_choices(),'Texas State Hitter'), !session$isClosed())
})
unlink(scratch,recursive=TRUE)
cat('Mixed-team hitter filtering, explicit selection, and matchup filtering passed.\n')
