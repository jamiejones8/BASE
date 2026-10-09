source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
TEAM_CONFIG$data$team_season_import_dir <- tempfile('rick-test-store-')
workspace <- base_wally_scouting_environment('tests/fixtures/wallyapps/hitting/data')
for (name in c('advance','duplex','annotations')) {
  sys.source(paste0('WallyApps/ScoutingApp/tests/test_rick_',name,'.R'),envir=workspace)
}
unlink(TEAM_CONFIG$data$team_season_import_dir,recursive=TRUE)
