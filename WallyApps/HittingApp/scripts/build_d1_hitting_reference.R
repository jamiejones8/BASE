#!/usr/bin/env Rscript
# Run from repository root with D1_HITTING_PARQUET pointing to the D1 export.
suppressPackageStartupMessages({library(dplyr); library(arrow); library(readr)})
source('team_config.R')
source('R/data/source_contract.R')
source('R/integrations/wally_hitting_workspace.R')
path <- Sys.getenv('D1_HITTING_PARQUET', TEAM_CONFIG$data$ncaa_d1_master_file)
stopifnot(file.exists(path))
fixture <- read_csv('tests/fixtures/wallyapps/hitting/data/2026 Season - cleaned.csv',show_col_types=FALSE)
fixture$source_file <- '2026 Season - cleaned.csv'; fixture$row_in_file <- seq_len(nrow(fixture))
w <- base_wally_hitting_environment(fixture)
cols <- c('TaggedPitchType','ModelPitchType','AutoPitchType','Batter','BatterId','BatterTeam','BatterSide','PitchCall','PlayResult','KorBB','ExitSpeed','Angle','Bearing','RunsScored',
          'PlateLocSide','PlateLocHeight','Balls','Strikes','GameID','GameUID','PitchUID','Date','Inning','Top/Bottom','PAofInning','PitchofPA')
d <- open_dataset(path) %>% select(any_of(cols)) %>% collect()
raw_n <- nrow(d)
message('Read ',raw_n,' pitches')
d <- w$dedupe_hitting_events(d)
message("Deduplicated pitches")
names_all <- unique(d$Batter)
excluded <- names_all[is.na(names_all) | !nzchar(trimws(names_all)) | w$is_walk_on_player_name(names_all)]
d <- d[!d$Batter %in% excluded,]
message("Filtered hitter names")
# Cleaned TaggedPitchType is the final corrected tag (not ModelPitchTypeRaw or AutoPitchType).
stopifnot(all(c("TaggedPitchType","ModelPitchType") %in% names(d)),
          identical(as.character(d$TaggedPitchType),as.character(d$ModelPitchType)))
d$PitchType_UNI <- w$canonical_pitch_fuzzy(d$TaggedPitchType)
tag_counts <- as.list(table(d$PitchType_UNI))
d$pitch_call <- w$canon_pitch_call(d$PitchCall)
d$play_result <- as.character(d$PlayResult)
d$ev <- w$to_num(d$ExitSpeed); d$la <- w$to_num(d$Angle)
d$bearing <- w$to_num(d$Bearing)
d$bats <- substr(toupper(as.character(d$BatterSide)),1,1)
# Same identity fields as make_pa_id(), without a Cartesian factor expansion
# over millions of pitches. Compute per-player factors only in the summarizer.
d$PA_ID <- paste(w$resolve_game_key(d), d[['Top/Bottom']], d$Inning, d$PAofInning, d$Batter, sep='\037')
message('Normalized and deduplicated ',nrow(d),' pitches')
d$in_zone <- base_pitch_in_zone(w$to_num(d$PlateLocSide),w$to_num(d$PlateLocHeight))
# Compare full-season hitter rates using the exact app definitions. Group by
# team and identity to avoid merging different players with the same name.
key <- paste(d$BatterTeam,ifelse(!is.na(d$BatterId) & nzchar(d$BatterId), d$BatterId,d$Batter),sep='::')
indices <- split(seq_len(nrow(d)),key)
groups <- c("Total",names(w$hitting_pitch_groups))
qualified <- list()
outs <- lapply(groups,function(group) {
  message("Building ",group," D1 distribution")
  metrics <- bind_rows(lapply(seq_along(indices),function(i) {
    dd <- w$hitting_pitch_group_rows(d[indices[[i]],],group)
    if (length(unique(dd$PA_ID)) < 50L) return(NULL)
    if (i %% 250L == 0) message(group,' hitter ',i,' / ',length(indices))
    w$summarize_overall(dd)
  }))
  stopifnot(nrow(metrics)>100L)
  qualified[[group]] <<- nrow(metrics)
  metric_names <- if(group == "Total") w$performance_table_metrics else w$hitting_group_percentile_metrics
  tidyr::pivot_longer(metrics %>% select(all_of(metric_names)),everything(),names_to='metric',values_to='value') %>%
    filter(is.finite(value)) %>% mutate(group=group,.before=1) %>% arrange(metric,value)
})
out <- bind_rows(outs)
avgs <- out %>% group_by(group,metric) %>% summarise(average=mean(value),players=n(),.groups='drop')
dir <- 'WallyApps/HittingApp/data'
write_csv(out,file.path(dir,'d1_hitting_percentiles.csv'))
write_csv(avgs,file.path(dir,'d1_hitting_averages.csv'))
jsonlite::write_json(list(source=basename(path),source_bytes=file.info(path)$size,raw_pitches=raw_n,
  unique_pitches=nrow(d),qualified_hitters=qualified, pitch_tag_counts=tag_counts,
  pitch_type_source="Cleaned TaggedPitchType; never raw model or automatic tags",minimum_pa=50,
  average_definition='Mean of qualified hitter season values; 50 PA minimum within each pitch group',
  stat_definition='HittingApp summarize_overall; physical ball/zone intersection; current barrel boundaries'),
  file.path(dir,'d1_hitting_reference_provenance.json'),pretty=TRUE,auto_unbox=TRUE)
message('Wrote ',nrow(out),' percentile values across four independent pitch-group pools.')
