source('team_config.R')
source('R/integrations/wally_scouting_workspace.R')
scratch <- tempfile('scout-sources-'); dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
raw <- readr::read_csv('tests/fixtures/wallyapps/hitting/data/2026 Season - cleaned.csv', show_col_types=FALSE)
for (id in c('F26','PS27','S27')) {
  rows <- raw; rows$PitchUID <- paste0(id,'-',seq_len(nrow(rows)))
  readr::write_csv(rows,base_team_season_import_path(id))
}
workspace <- base_wally_scouting_environment(file.path(scratch,'scouting'))
shiny::testServer(workspace$server, {
  for (id in c('F26','PS27','S27')) {
    session$setInputs(scout_data_source=id,scout_team=raw$PitcherTeam[1],
      scout_season_pitchers=raw$Pitcher[1],scout_season_hitters=raw$Batter[1])
    loaded <- pitcher_std_all()
    stopifnot(nrow(loaded)>0,all(startsWith(loaded$PitchUID,paste0(id,'-'))))
    session$setInputs(scout_team=raw$BatterTeam[1],scout_season_hitters=raw$Batter[1])
    hitters <- std_all()
    stopifnot(nrow(hitters)>0,all(startsWith(hitters$PitchUID,paste0(id,'-'))))
  }
})
# Opponents need not be Texas State, and one file may contain many games.
raw$PitcherTeam <- 'OPPONENT';raw$BatterTeam <- 'OTHER';raw$PitchUID <- paste0('opp-',seq_len(nrow(raw)))
upload <- file.path(scratch,'opponent.csv');readr::write_csv(raw,upload)
saved <- base_import_scouting_file(upload,'../../Opponent cleaned.csv')
stopifnot(file.exists(saved$path),dirname(saved$path)==base_scouting_data_dir())
source <- base_scouting_season_source(saved$path)
stopifnot(identical(source$teams('pitcher'),'OPPONENT'),nrow(source$catalog('pitcher','OPPONENT'))>0)
# Invalid uploads must leave the previously saved source intact.
readr::write_csv(data.frame(Bad=1),upload)
stopifnot(inherits(tryCatch(base_import_scouting_file(upload,'bad.csv'),error=identity),'error'),file.exists(saved$path))
unlink(scratch,recursive=TRUE)
cat('All season selectors, opponent upload validation, and CSV team/player queries passed.\n')

context <- readLines('.dockerignore')
stopifnot(all(c('!WallyApps/ScoutingApp/RickAdvanceSheet.R',
               '!WallyApps/ScoutingApp/reference/') %in% context))
