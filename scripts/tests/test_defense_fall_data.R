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
# Volume uploads can use TrackMan M/D/YY dates while the bundled file uses ISO
# dates. Exercise both positioning merges, including a fully matched upload.
local({
  original_config <- TEAM_CONFIG
  upload_root <- tempfile('defense-date-imports-')
  dir.create(upload_root)
  on.exit({
    TEAM_CONFIG <<- original_config
    base_clear_wally_defense_state()
    unlink(upload_root, recursive = TRUE)
  })
  TEAM_CONFIG$data$team_season_import_dir <<- upload_root
  bundled <- base_read_defense_csv(base_defense_dev_file('2026 Fall Defense.csv'))
  upload <- bundled[1, , drop = FALSE]
  expected_date <- as.Date(upload$Date)
  upload$Date <- format(expected_date, '%m/%d/%y')
  upload$GameDate <- upload$Date
  upload$CF_PositionAtReleaseX <- upload$CF_PositionAtReleaseX + 1
  path <- file.path(upload_root, 'legacy-positioning.csv')
  readr::write_csv(upload, path)
  TEAM_CONFIG$data$player_positioning_file <<- path

  # Even zero unmatched rows used to fail bind_rows(Date, character).
  matched <- base_prepare_wally_defense_rows()
  i <- match(upload$PitchUID, matched$PitchUID)
  stopifnot(nrow(matched) == nrow(bundled), inherits(matched$Date, 'Date'),
            matched$Date[i] == expected_date,
            matched$CF_PositionAtReleaseX[i] == upload$CF_PositionAtReleaseX)

  new_fall <- upload
  new_fall$PitchUID <- 'date-import-fall'
  new_fall$Date <- '10/30/2026'
  new_fall$GameDate <- new_fall$Date
  readr::write_csv(dplyr::bind_rows(upload, new_fall), base_positioning_season_paths()[['F26']])
  future <- upload
  future$PitchUID <- 'date-import-spring'
  future$Date <- '2027-02-12'
  future$GameDate <- future$Date
  readr::write_csv(future, base_positioning_season_paths()[['S27']])

  workspace <- base_wally_defense_environment()
  rows <- workspace$BASE_DEFENSE_DATA
  fall <- rows[rows$PitchUID == 'date-import-fall', , drop = FALSE]
  spring <- rows[rows$PitchUID == 'date-import-spring', , drop = FALSE]
  stopifnot(is.function(workspace$server), nrow(workspace$defense_df) > 0L,
            nrow(rows) == nrow(bundled) + 2L, !anyDuplicated(rows$PitchUID),
            inherits(rows$Date, 'Date'), inherits(rows$GameDate, 'Date'),
            nrow(fall) == 1L, fall$Date == as.Date('2026-10-30'), fall$SeasonGroup == 'F26',
            nrow(spring) == 1L, spring$Date == as.Date('2027-02-12'), spring$SeasonGroup == 'S27')
})
workspace <- base_wally_defense_environment()
positioning <- workspace$BASE_DEFENSE_DATA
joined <- workspace$raw_df
stopifnot(nrow(positioning)==nrow(base_read_defense_csv(base_defense_dev_file("2026 Fall Defense.csv"))), nrow(joined)==nrow(positioning),
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
cat('Fall defense: all bundled pitches matched, contact metrics retained, fall-only opportunities and summaries passed.\n')

# Bundled data must be present in deployment, not merely on the developer Mac.
for (ignore_file in c('.dockerignore', '.railwayignore')) {
context <- readLines(ignore_file)
stopifnot(all(c('!WallyApps/DefenseApp/data/',
               '!WallyApps/DefenseApp/data/2026 Fall Defense.csv',
               '!WallyApps/DefenseApp/data/2026 Fall Batted Balls*.csv',
               '!WallyApps/DefenseApp/data/d1_defense_play_probability_baseline.csv') %in% context))
}
