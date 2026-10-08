#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
source('R/data/source_contract.R')
source('R/integrations/wally_defense_workspace.R')
# Isolate bundled fall coverage from any machine-specific upload volume.
TEAM_CONFIG$data$player_positioning_file <- tempfile(fileext='.csv')
TEAM_CONFIG$data$team_season_import_dir <- tempfile()
TEAM_CONFIG$data$season_file <- tempfile(fileext='.parquet')
base_prepare_wally_catching_rows <- function(startup_rows=NULL) {
  base_read_defense_csv('tests/fixtures/wallyapps/defense/data/Catchers - 2026 Season-cleaned.csv')
}
base_prepare_catcher_framing_baseline <- function() {
  base_read_defense_csv('tests/fixtures/wallyapps/defense/data/d1_catcher_framing_metrics.csv')
}
workspace <- base_wally_defense_environment()
positioning <- workspace$BASE_DEFENSE_DATA
joined <- workspace$raw_df
stopifnot(nrow(positioning)==1029L, nrow(joined)==nrow(positioning),
          !anyDuplicated(joined$PitchUID), all(joined$batted_ball_matched),
          all(positioning$SeasonGroup=='F26'),
          all(c('HangTime','Distance','Bearing','ExitSpeed','Angle') %in% names(joined)),
          nrow(workspace$defense_df)>0L, all(workspace$defense_df$season=='F26'),
          nrow(workspace$summarize_defense(workspace$defense_df))>0L)
# Contact metrics must agree with the companion export for each pitch ID.
contacts <- workspace$BASE_DEFENSE_BATTED_DATA
idx <- match(joined$PitchUID,contacts$PitchUID)
for (metric in c('HangTime','Distance','Bearing','ExitSpeed','Angle')) {
  expected <- suppressWarnings(as.numeric(contacts[[metric]][idx]))
  actual <- suppressWarnings(as.numeric(joined[[metric]]))
  keep <- is.finite(expected)
  stopifnot(isTRUE(all.equal(actual[keep],expected[keep],check.attributes=FALSE)))
}
# A legacy configured positioning file cannot reintroduce spring data.
path <- TEAM_CONFIG$data$player_positioning_file
write.csv(data.frame(PitchUID='spring-unusable',Date='2026-04-01'),path,row.names=FALSE)
stopifnot(!'spring-unusable' %in% base_prepare_wally_defense_rows()$PitchUID)
unlink(path)
cat('Fall defense: all 1,029 pitches matched, contact metrics retained, fall-only opportunities and summaries passed.\n')
