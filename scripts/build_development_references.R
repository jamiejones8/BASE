#!/usr/bin/env Rscript
# Rscript scripts/build_development_references.R /path/to/D1.parquet
suppressPackageStartupMessages({library(dplyr);library(arrow);library(readr);library(lubridate)})
source("WallyApps/PitchingApp/helpers/situations.R")
source("WallyApps/shared/development_goals.R")
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)>0,file.exists(args[1]))
dataset<-open_dataset(args[1])
own<-read_csv("WallyApps/PitchingApp/data/2026 Season - cleaned.csv",show_col_types=FALSE)
games<-unique(na.omit(own$GameID));games<-games[nzchar(games) & games!="NA"]
context<-dataset %>% filter(GameID %in% games) %>% select(any_of(PITCHING_SITUATION_CONTEXT_COLUMNS)) %>% collect()
message("Read ",nrow(context)," full-game context pitches")
venues<-read.csv("WallyApps/PitchingApp/reference/venue_locations.csv")
annotated<-pitching_annotate_situations(context,venues=venues)
snapshots<-annotated %>% filter(is.finite(.sit_home_score),is.finite(.sit_away_score)) %>%
  transmute(GameID,TopBottom=.data[["Top/Bottom"]],Inning,PAofInning,
    HomeScoreBefore=.sit_home_score,AwayScoreBefore=.sit_away_score) %>% distinct()
write_csv(snapshots,"WallyApps/PitchingApp/reference/pa_scores_2026.csv")
message("Saved ",nrow(snapshots)," PA score snapshots from ",n_distinct(snapshots$GameID)," games")
fields<-c("GameID","GameUID","PitchUID","Pitcher","PitcherId","PitcherTeam","Inning","Top/Bottom","PAofInning","PitchofPA","PitchCall","KorBB","PlayResult","ExitSpeed","Angle","xwOBA")
last<-dataset %>% filter(PitchCall %in% c("InPlay","InPlayOut","InPlayNoOut","HitByPitch") | KorBB %in% c("Strikeout","Walk","BB","K","HBP","IBB")) %>% select(any_of(fields)) %>% collect()
last<-last %>% arrange(PitchofPA) %>% distinct(PitchUID,.keep_all=TRUE) %>%
  group_by(GameID,Inning,.data[["Top/Bottom"]],PAofInning,PitcherTeam,PitcherId) %>% slice_tail(n=1) %>% ungroup()
last$expected<-development_xwoba_values(last,readRDS("models/shared/xwoba_grid.rds"))
values<-last %>% filter(!is.na(Pitcher),!grepl("walk.?on",Pitcher,ignore.case=TRUE)) %>%
  group_by(PitcherTeam,PitcherId,Pitcher) %>% summarise(sample_n=n(),value=mean(expected,na.rm=TRUE),.groups="drop") %>%
  filter(sample_n>=50,is.finite(value)) %>% transmute(scope="overall",pitch_type="",metric="performance_xwoba",value,sample_n)
write_csv(values,"WallyApps/PitchingApp/reference/d1_pitching_xwoba_percentiles.csv")
jsonlite::write_json(list(source=basename(args[1]),source_bytes=file.info(args[1])$size,
  full_game_context_pitches=nrow(context),score_pa_snapshots=nrow(snapshots),score_games=n_distinct(snapshots$GameID),
  qualified_pitchers=nrow(values),minimum_pa=50,xwoba_weights=list(BB=.690,HBP=.720),
  expected_contact_grid="models/shared/xwoba_grid.rds",created=as.character(Sys.Date())),
  "WallyApps/PitchingApp/reference/development_reference_provenance.json",pretty=TRUE,auto_unbox=TRUE)
cat(nrow(values),"qualified D1 pitcher xwOBA values.\n")
