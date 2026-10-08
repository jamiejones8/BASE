#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R'); source('R/data/source_contract.R')
source('R/integrations/wally_defense_workspace.R')
for (expr in parse('WallyApps/DefenseApp/DefenseApp.R')) {
  if (is.call(expr) && identical(expr[[1]],as.name('<-')) &&
      identical(expr[[2]],as.name('attach_batted_ball_data'))) eval(expr)
}
scratch <- tempfile('positioning-seasons-'); dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
TEAM_CONFIG$data$player_positioning_file <- file.path(scratch,'legacy.csv')
TEAM_CONFIG$data$season_file <- file.path(scratch,'absent.csv')
position <- base_read_trackman_import('WallyApps/DefenseApp/data/2026 Fall Defense.csv')[1,,drop=FALSE]
position$PitchUID <- 'new-season-pitch'; position$GameUID <- 'new-season-game'
position$Date <- '2027-01-20'
path <- file.path(scratch,'upload.csv'); readr::write_csv(position,path)
result <- base_import_trackman_game(path,'PP_PS27')
stopifnot(result$inserted_rows==1,base_import_trackman_game(path,'PP_PS27')$inserted_rows==0)
stopifnot(inherits(tryCatch(base_import_trackman_game(path,'PP_F26'),error=identity),'error'))
positions <- base_prepare_wally_defense_rows()
p <- positions[positions$PitchUID=='new-season-pitch',]
stopifnot(nrow(p)==1,p$SeasonGroup=='PS27')
joined <- attach_batted_ball_data(p,base_prepare_wally_batted_rows(p))
stopifnot(!joined$batted_ball_matched)
# The same pitch ID in another season must not match.
contact <- tibble(PitchUID='new-season-pitch',GameUID='new-season-game',Date='2027-02-20',
 PitcherTeam='TEX_BOB',BatterTeam='TEX_BOB',Pitcher='Pitcher',Batter='Hitter',
 PitchCall='InPlay',Distance=300,HangTime=4,Bearing=10,Angle=25,ExitSpeed=95)
readr::write_csv(contact,path); base_import_trackman_game(path,'S27')
stopifnot(!attach_batted_ball_data(p,base_prepare_wally_batted_rows(p))$batted_ball_matched)
# Importing the companion season later makes the existing position match.
contact$Date <- '2027-01-20'; contact$Distance <- 250
readr::write_csv(contact,path); base_import_trackman_game(path,'PS27')
joined <- attach_batted_ball_data(p,base_prepare_wally_batted_rows(p))
stopifnot(joined$batted_ball_matched,as.numeric(joined$Distance)==250,as.numeric(joined$HangTime)==4)
stopifnot(all(file.exists(base_positioning_season_paths()[['PS27']])))
unlink(scratch,recursive=TRUE)
cat('Positioning seasons: append, duplicate prevention, year validation, season isolation, and later contact imports passed.\n')

suppressPackageStartupMessages(library(shiny))
source("R/data/player_health_imports.R")
for (expr in parse('R/app_main.R')) {
 if (is.call(expr) && identical(expr[[1]],as.name('<-')) && is.symbol(expr[[2]]) &&
     as.character(expr[[2]]) %in% c('BASE_NAV_TABS','data_processing_workspace_ui','workspace_tool_card','base_nav_click_js')) eval(expr)
}
html <- as.character(data_processing_workspace_ui())
stopifnot(!grepl('dp_positioning_season',html,fixed=TRUE),
          grepl('2026 fall player positioning',html,fixed=TRUE),
          grepl('2027 Scrimmages player positioning',html,fixed=TRUE),
          grepl('dp_positioning_f26_file',html,fixed=TRUE),
          grepl('dp_positioning_scrimmages_file',html,fixed=TRUE))
cat('Both fixed positioning upload cards render without a season dropdown.\n')
