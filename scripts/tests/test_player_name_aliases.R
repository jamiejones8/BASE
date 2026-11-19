#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R');source('R/data/source_contract.R');source('R/data/team_season_imports.R')
source('R/integrations/wally_hitting_workspace.R');source('R/integrations/wally_defense_workspace.R')
source('WallyApps/shared/development_goals.R')

names <- c('Cadyn Byrd','Byrd, Cadyn','  cadyn  BYRD  ','BYRD, CADYN','Cayden Byrd',
 'Shaw, Cadyn','Cadyn Shaw','Garrett Byrd','Cadyn Byrdson','Other Cadyn Byrd',NA_character_,'')
expected <- c('Cayden Byrd','Byrd, Cayden','Cayden Byrd','Byrd, Cayden',names[5:12])
stopifnot(identical(base_correct_player_names(names),expected),
 identical(base_correct_player_names(expected),expected))
split <- data.frame(First=c('Cadyn','Cadyn','Garrett'),Last=c('Byrd','Shaw','Byrd'),id=1:3)
stopifnot(identical(base_correct_player_name_columns(split)$First,c('Cayden','Cadyn','Garrett')),
 identical(base_correct_player_name_columns(split)$id,split$id))
stopifnot(identical(development_name('Byrd, Cadyn'),'Cayden Byrd'),
 identical(development_portrait('Cadyn Byrd'),development_portrait('Cayden Byrd')),
 file.exists(development_portrait('Cadyn Byrd')))

scratch <- tempfile('byrd-names-');dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
TEAM_CONFIG$data$player_positioning_file <- file.path(scratch,'legacy.csv')
TEAM_CONFIG$data$bullpen_file <- file.path(scratch,'bullpens.csv')
rows <- tibble(PitchUID=paste0('name-pitch-',1:3),GameUID='name-game',Date='2026-10-17',
 PitcherTeam='TEX_BOB',BatterTeam='TEX_BOB',Pitcher='Other Pitcher',
 Batter=c('Byrd, Cadyn','Cadyn Byrd','Shaw, Cadyn'),BatterId=c('1000608881','1000608881','other-id'),
 PitchCall='BallCalled',Notes='Cadyn Byrd')
upload <- file.path(scratch,'upload.csv');readr::write_csv(rows,upload)
result <- base_import_trackman_game(upload,'F26')
path <- result$destination;stored <- base_read_trackman_import(path)
stopifnot(identical(stored$Batter,c('Byrd, Cayden','Cayden Byrd','Shaw, Cadyn')),
 identical(stored$BatterId,rows$BatterId),identical(stored$Notes,rows$Notes),nrow(stored)==3,
 base_import_trackman_game(upload,'F26')$inserted_rows==0)
# A whole-file replacement and a manual save use the same rule.
base_replace_trackman_csv(upload,'F26',base_trackman_csv_fingerprint(path))
stored <- base_read_trackman_import(path)
edited <- stored;edited$Batter[1] <- 'Byrd, Cadyn';edited$PitchCall[2] <- 'StrikeCalled'
saved <- base_save_trackman_manual_edit(edited,'F26',base_trackman_csv_fingerprint(path))
stopifnot(saved$changed_cells==1,identical(saved$rows$Batter,stored$Batter),
 identical(base_read_trackman_import(path)$Batter,stored$Batter))
# Existing hitting files are corrected at startup, with exact originals backed up.
readr::write_csv(rows,path)
base_correct_stored_trackman_names()
backups <- list.files(file.path(scratch,'backups'),full.names=TRUE)
stopifnot(any(vapply(backups,function(p) identical(base_read_trackman_import(p),rows),logical(1))),
 identical(base_read_trackman_import(path)$Batter,stored$Batter))
stamp <- base_trackman_csv_fingerprint(path)
base_correct_stored_trackman_names()
stopifnot(identical(stamp,base_trackman_csv_fingerprint(path)))
# Exports without UUIDs must normalize both stored and incoming player names
# before constructing their fallback identities, avoiding duplicate pitches.
fallback <- select(rows,-PitchUID);fallback$PitchNo <- as.character(1:3)
fallback_root <- file.path(scratch,'fallback');dir.create(fallback_root)
fallback_path <- base_team_season_import_path('F26',root=fallback_root)
readr::write_csv(fallback,fallback_path)
readr::write_csv(fallback,upload)
matched <- base_import_trackman_game(upload,'F26',root=fallback_root)
stopifnot(matched$inserted_rows==0,nrow(base_read_trackman_import(fallback_path))==3)
readr::write_csv(rows,upload)

# The actual Hitting and Defense readers expose one spelling, including old
# vendor rows that have not yet passed through the managed import workflow.
raw <- rows;raw$.base_source_priority <- 2L
hitting <- base_prepare_team_hitting_data(raw)
stopifnot(identical(hitting$Batter,stored$Batter),nrow(hitting)==3)
defense <- base_read_defense_csv(upload,convert_types=FALSE)
stopifnot(identical(defense$Batter,stored$Batter))
source('Sports Science 2/vald_shiny_app/R/roster_trackman.R')
stopifnot(identical(player_name_key(c('Cadyn Byrd','Byrd, Cadyn','Cayden Byrd')),rep('cayden byrd',3)))
roster <- read_fall_roster('Sports Science 2/2026 Fall Roster Template.xlsx',
 'Sports Science 2/vald_shiny_app/config/player_aliases.csv')
stopifnot(is.null(roster$error),'Cayden Byrd'%in%roster$players$name,
 !'Cadyn Byrd'%in%roster$players$name)
unlink(scratch,recursive=TRUE)
cat('One-player alias: both name orders, case/whitespace, unrelated names, hitting/defense/roster/PDF readers, append/replacement/manual save, backups and repeat startup passed.\n')
