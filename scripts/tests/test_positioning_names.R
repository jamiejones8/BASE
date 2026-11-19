#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
source('R/data/team_season_imports.R')
scratch <- tempfile('positioning-names-');dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
TEAM_CONFIG$data$player_positioning_file <- file.path(scratch,'legacy.csv')

rows <- base_read_trackman_import('WallyApps/DefenseApp/data/2026 Fall Defense.csv')[1:3,]
rows$CF_Name <- c('Byrd, Cadyn','Cadyn Byrd','Shaw, Cadyn')
rows$RF_Name <- c('Byrd, Cadyn','Other Player','Byrd, Cayden')
rows$FieldedBy <- c('Byrd, Cadyn','Cadyn Byrd',NA_character_)
expected <- base_normalize_positioning_names(rows)
stopifnot(identical(expected$CF_Name,c('Byrd, Cayden','Cayden Byrd','Shaw, Cadyn')),
 identical(expected$FieldedBy,c('Byrd, Cayden','Cayden Byrd',NA_character_)),
 identical(rows[,!names(rows)%in%c('CF_Name','RF_Name','FieldedBy')],
           expected[,!names(rows)%in%c('CF_Name','RF_Name','FieldedBy')]))

# Startup corrects both existing legacy and season volume files, preserving a
# backup. Repeating it makes no writes or extra backups.
paths <- c(base_team_season_import_path('PP'),base_team_season_import_path('PP_F26'))
for(path in paths) base_atomic_write_trackman_csv(rows,path)
base_correct_stored_trackman_names(BASE_PLAYER_POSITIONING_IMPORT_TARGETS)
for(path in paths) stopifnot(identical(base_read_trackman_import(path),expected))
backups <- list.files(file.path(scratch,'backups'),full.names=TRUE)
stopifnot(length(backups)==2,all(vapply(backups,function(path)
 identical(base_read_trackman_import(path),rows),logical(1))))
fingerprints <- lapply(paths,base_trackman_csv_fingerprint)
base_correct_stored_trackman_names(BASE_PLAYER_POSITIONING_IMPORT_TARGETS)
stopifnot(identical(fingerprints,lapply(paths,base_trackman_csv_fingerprint)),
 length(list.files(file.path(scratch,'backups')))==2)

# Appends and whole-file replacements cannot reintroduce the misspelling.
upload <- file.path(scratch,'upload.csv');readr::write_csv(rows,upload)
appended <- base_import_trackman_game(upload,'PP',root=scratch)
stored <- base_read_trackman_import(appended$destination)
stopifnot(identical(stored$CF_Name,expected$CF_Name),identical(stored$FieldedBy,expected$FieldedBy))
replaced <- base_replace_trackman_csv(upload,'PP_F26',base_trackman_csv_fingerprint(paths[2]))
stored <- base_read_trackman_import(replaced$destination)
stopifnot(identical(stored$CF_Name,expected$CF_Name),identical(stored$FieldedBy,expected$FieldedBy))

bundled <- base_read_trackman_import('WallyApps/DefenseApp/data/2026 Fall Defense.csv')
stopifnot(identical(bundled,base_normalize_positioning_names(bundled)),
 sum(bundled$CF_Name=='Byrd, Cayden',na.rm=TRUE)==495,
 sum(bundled$RF_Name=='Byrd, Cayden',na.rm=TRUE)==271,
 sum(bundled$FieldedBy=='Byrd, Cayden',na.rm=TRUE)==12)
unlink(scratch,recursive=TRUE)
cat('Positioning names: Cayden Byrd in bundled data, backed-up existing volume files, repeat startup, append and replacement; other players and columns preserved.\n')
