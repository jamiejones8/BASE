#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
root <- tempfile('scouting-dates-');dir.create(root)
fixture <- readr::read_csv('tests/fixtures/wallyapps/hitting/data/2026 Season - cleaned.csv',show_col_types=FALSE)
rows <- fixture[rep(1,8),]
rows$PitchUID <- paste0('date-',seq_len(nrow(rows)))
rows$Date <- c('2026-10-14','10/14/2026','14-Oct-2026','20261014',NA,'','unknown','2026-02-30')
rows$TaggedPitchType <- c('Fastball','Slider','Undefined',' undefined ','UNDEFINED','Fastball','Slider','Fastball')
rows$AutoPitchType <- 'Fastball'
path <- file.path(root,'input.csv');readr::write_csv(rows,path)
saved <- base_import_scouting_file(path,'dates.csv',root)
stored <- base_read_trackman_import(saved$path)
stopifnot(saved$rows==8L,identical(stored$PitchUID,rows$PitchUID),
          identical(stored$Date[c(1:4,7:8)],rows$Date[c(1:4,7:8)]),all(is.na(stored$Date[5:6])))
rows$Date <- NULL
readr::write_csv(rows,path)
undated <- base_import_scouting_file(path,'undated.csv',root)
stopifnot(undated$rows==8L,all(is.na(base_read_trackman_import(undated$path)$Date)))
w <- base_wally_scouting_environment(root)
expected <- rows$PitchUID[c(1,2,6,7,8)]
# A tagged Undefined pitch stays excluded even when AutoPitchType is Fastball.
standard <- w$pitcher_env$standardize_tm(stored)
stopifnot(identical(standard$PitchUID,expected),nrow(w$rick_prepare(standard))==5L)
extra <- standard[1,,drop=FALSE];extra$PitchType <- 'Undefined'
stopifnot(nrow(w$rick_prepare(dplyr::bind_rows(standard,extra)))==5L)
shiny::testServer(w$server, {
  session$setInputs(scout_data_source='csv',csv_files=basename(saved$path),
                    main_tab='rick_advance',rick_side='R')
  stopifnot(identical(pitcher_std_all()$PitchUID,expected),nrow(df_all())==8L)
  session$setInputs(main_tab='matchup_grid',matchup_pitchers_file=data.frame(datapath=saved$path))
  stopifnot(identical(matchup_pitchers_std()$PitchUID,expected),!session$isClosed())
  session$setInputs(csv_files=basename(undated$path))
  stopifnot(identical(pitcher_std_all()$PitchUID,expected),!session$isClosed())
})
# The same centralized report filter applies to season/Parquet rows.
parquet <- file.path(root,'opponent.parquet')
arrow::write_parquet(stored,parquet)
season <- base_scouting_season_source(parquet)
loaded <- season$load_players('pitcher',rows$PitcherTeam[1],rows$Pitcher[1])
stopifnot(identical(w$pitcher_env$standardize_tm(loaded)$PitchUID,expected))
all_undefined <- stored
all_undefined$TaggedPitchType <- 'Undefined'
stopifnot(nrow(w$pitcher_env$standardize_tm(all_undefined))==0L)
unlink(root,recursive=TRUE)
cat('Scouting uploads retain all dates and undated rows; pitcher reports exclude Undefined tags.\n')
