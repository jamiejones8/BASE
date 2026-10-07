#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(shiny))
source('team_config.R')
source('R/integrations/player_health_workspace.R')
for (file in c('roster_trackman.R', 'combined_trends.R', 'kpi_correlations.R', 'player_health.R')) {
  source(file.path('Sports Science 2/vald_shiny_app/R', file))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
scratch <- tempfile('health-seasons-')
dir.create(scratch)
exports <- file.path(scratch, 'exports'); dir.create(exports)
seasons <- file.path(scratch, 'seasons'); dir.create(seasons)
TEAM_CONFIG$data$team_season_import_dir <- seasons
BASE_PLAYER_HEALTH_TRACKMAN_FILES <- base_player_health_trackman_files
Sys.setenv(PLAYER_HEALTH_DATA_DIR = exports)
roster <- list(players = data.frame(name='Test Pitcher',key='test pitcher',role='Pitchers'),
               aliases=data.frame(alias=character(),key=character()))
read_fall_roster <- function(...) roster
row <- function(uid, date, speed) data.frame(PitchUID=uid,Pitcher='Pitcher, Test',Date=date,
  TaggedPitchType='Fastball',RelSpeed=speed,SpinRate=2200)
write.csv(row('a','2026-09-20',80),file.path(exports,'old_trackman.csv'),row.names=FALSE)
fall <- file.path(seasons,'2026 Fall - cleaned.csv')
write.csv(row('a','9/20/26',90),fall,row.names=FALSE)
# Season data wins over a more recently modified copied health export.
Sys.setFileTime(file.path(exports,'old_trackman.csv'),Sys.time()+60)
d <- trackman_read(exports,roster)
stopifnot(nrow(d$pitches)==1L,d$pitches$velocity_mph==90,d$pitches$date==as.Date('2026-09-20'))
date_updates <- list()
updateDateRangeInput <- function(session, inputId, start, end, ...) {
  date_updates[[length(date_updates)+1L]] <<- c(start,end)
  shiny::updateDateRangeInput(session,inputId,start=start,end=end,...)
}
shiny::testServer(function(input,output,session) {
  health <- health_server(input,output,session,reactive(data.frame()),reactive(data.frame()),
                          fall_roster=reactive(roster))
}, {
  session$setInputs(hc_combined_player='test pitcher')
  stopifnot(nrow(health$pitching_daily())==1L)
  session$setInputs(hc_combined_dates=date_updates[[1]])
  before <- file_signature(trackman_files(exports))
  # A new nested season and vendor filename are discovered while the session runs.
  future <- file.path(seasons,'2027 Season'); dir.create(future)
  game <- file.path(future,'20270220-TexasStateUniversity-Private-1.csv')
  write.csv(row('b','02/20/2027',94),game,row.names=FALSE)
  stopifnot(before != file_signature(trackman_files(exports)))
  session$elapse(5100); session$flushReact()
  session$setInputs(hc_combined_dates=tail(date_updates,1)[[1]])
  d <- health$pitching_daily()
  stopifnot(nrow(d)==2L,max(d$date)==as.Date('2027-02-20'))
  stopifnot(tail(date_updates,1)[[1]][2]==as.Date('2027-02-20'))
  session$setInputs(hc_combined_dates=tail(date_updates,1)[[1]])
  plot_data <- health$combined_plot_data()$trackman
  stopifnot(nrow(plot_data)==2L)
  write.csv(rbind(row('b','02/20/2027',96),row('c','2027-02-21',92)),game,row.names=FALSE)
  session$elapse(5100); session$flushReact()
  session$setInputs(hc_combined_dates=tail(date_updates,1)[[1]])
  d <- health$pitching_daily()
  stopifnot(nrow(d)==3L,d$velocity_mph[d$date==as.Date('2027-02-20')]==96)
  unlink(game)
  session$elapse(5100); session$flushReact()
  stopifnot(nrow(health$pitching_daily())==1L)
})
unlink(scratch,recursive=TRUE)
cat('Sports Science season discovery, live refresh, date expansion, deduplication, and chart data passed.\n')
