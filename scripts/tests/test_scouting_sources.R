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
# Every uploaded file is individually selectable, newest first after TXST.
Sys.setFileTime(saved$path, Sys.time()-60)
raw$PitchUID <- paste0('new-',seq_len(nrow(raw)))
raw$PitcherTeam <- rep(c('TEAM_A','TEAM_B'),length.out=nrow(raw))
raw$BatterTeam <- rep(c('TEAM_B','TEAM_A'),length.out=nrow(raw))
readr::write_csv(raw,upload)
new_saved <- base_import_scouting_file(upload,'New opponent.csv')
catalog <- base_scouting_source_catalog()
stopifnot(identical(catalog$id[1:4],c('S26','F26','PS27','S27')),
  all(startsWith(catalog$label[1:4],'[TXST]')),
  identical(catalog$path[5:6],c(new_saved$path,saved$path)))
shiny::testServer(workspace$server, {
  session$setInputs(scout_data_source=paste0('upload:',new_saved$filename))
  stopifnot(nrow(season_pitcher_rows())==nrow(raw),nrow(season_hitter_rows())==nrow(raw))
  loaded <- pitcher_std_all()
  stopifnot(nrow(loaded)>0,all(startsWith(loaded$PitchUID,'new-')),!session$isClosed(),
    all(c('TEAM_A','TEAM_B') %in% loaded$PitcherTeam))
  session$setInputs(scout_team='STALE_TEAM',scout_season_pitchers='STALE_PLAYER')
  stopifnot(nrow(season_pitcher_rows())==nrow(raw))
})
unlink(scratch,recursive=TRUE)
cat('All season selectors, opponent upload validation, and CSV team/player queries passed.\n')

context <- readLines('.dockerignore')
stopifnot(all(c('!WallyApps/ScoutingApp/RickAdvanceSheet.R',
               '!WallyApps/ScoutingApp/reference/') %in% context))
