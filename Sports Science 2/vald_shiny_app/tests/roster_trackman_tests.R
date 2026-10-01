source("local_library.R", local = TRUE)
use_vald_local_library()
Sys.setenv(VALD_AUTO_REFRESH='false',VALD_REFRESH_WORKER='true',VALD_APP_ROOT=normalizePath(getwd()))
# Reproduce Shiny's pre-app alphabetical autoload in a clean environment.
autoload_env <- new.env(parent=baseenv())
for(f in sort(list.files('R',pattern='[.]R$',full.names=TRUE))) sys.source(f,envir=autoload_env)
stopifnot(identical(autoload_env$health_key(' Anders, Jonathan '),'jonathan anders'))
source('dashboard.R')
library(testthat)
r <- read_fall_roster()
stopifnot(is.null(r$error), nrow(r$players)==38)
test_that('roster matching normalizes names and rejects non-roster names', {
  d <- data.frame(name=c(' Anders, Jonathan ', 'Former Player', 'Riley Leininger'),key=c('x','y','z'))
  f <- roster_filter(d,r)
  expect_equal(f$name,c('Jonathan Anders','Ryley Leininger'))
  expect_equal(f$key,c('jonathan anders','ryley leininger'))
  bad <- read_fall_roster(tempfile())
  expect_equal(nrow(roster_filter(d,bad)),0)
  expect_match(bad$error,'Fall roster')
})
test_that('pitch averages use tags, independent counts, and newest unique pitch IDs', {
  root <- tempfile();dir.create(root)
  d <- data.frame(PitchUID=paste0('p',1:9),Pitcher='Anders, Jonathan',PitcherId='id1',Date='2026-09-17',
    TaggedPitchType=c('Fastball','Sinker','Cutter','Slider','Curveball','Sweeper','Changeup','Fastball','Slider'),
    RelSpeed=c(90,94,80,82,78,76,70,NA,NA), SpinRate=c(2000,2400,2500,2600,2700,2800,1500,2200,NA))
  write.csv(d,file.path(root,'a_trackman.csv'),row.names=FALSE)
  newer <- d[1,,drop=FALSE];newer$RelSpeed <- 92
  write.csv(newer,file.path(root,'b_trackman.csv'),row.names=FALSE)
  Sys.setFileTime(file.path(root,'a_trackman.csv'),Sys.time()-60)
  t <- trackman_read(root,r)
  expect_length(t$errors,0);expect_equal(t$duplicate_count,1);expect_equal(nrow(t$pitches),9)
  fb <- t$daily[t$daily$pitch_group=='Fastball/sinker',];bb <- t$daily[t$daily$pitch_group=='Breaking ball',]
  expect_equal(fb$velocity_mph,93);expect_equal(fb$spin_rpm,2200)
  expect_equal(fb$velocity_n,2);expect_equal(fb$spin_n,3);expect_equal(fb$pitch_count,3)
  expect_equal(bb$velocity_mph,79);expect_equal(bb$spin_rpm,2650);expect_equal(bb$pitch_count,5)
  d$RelSpeed <- NA;d$SpinRate <- NA
  write.csv(d,file.path(root,'b_trackman.csv'),row.names=FALSE)
  t <- trackman_read(root,r)
  expect_true(all(is.na(t$daily$velocity_mph)));expect_true(all(is.na(t$daily$spin_rpm)));expect_true(all(t$daily$velocity_n==0))
})
test_that('supplied data reconcile to independent pitch-level calculations', {
  raw <- read.csv('../data/2026 Fall Trackman data- cleaned.csv',check.names=FALSE)
  t <- trackman_read('../data',r)
  expect_true(all(grepl('measured TrackMan rows excluded: missing PitchUID',t$errors,fixed=TRUE)))
  for(group in c('Fastball/sinker','Breaking ball')) {
    tags <- if(group=='Fastball/sinker') c('Fastball','Sinker') else c('Cutter','Slider','Curveball','Sweeper')
    rows <- raw[raw$Pitcher=='Smith, Cade' & raw$Date=='2026-09-17' & raw$TaggedPitchType %in% tags,]
    actual <- t$daily[t$daily$key=='cade smith' & t$daily$date==as.Date('2026-09-17') & t$daily$pitch_group==group,]
    expect_equal(actual$velocity_mph,mean(rows$RelSpeed,na.rm=TRUE));expect_equal(actual$spin_rpm,mean(rows$SpinRate,na.rm=TRUE));expect_equal(actual$pitch_count,nrow(rows))
  }
})
select <- function(x) stop('Wrong select implementation was called')
test_that('VALD views tolerate masked select and only offer fall players', {
  shiny::testServer(server, {
    session$setInputs(alert_triggered_only=FALSE,cmj_window_days=28,team_group='All',sprint_window_days=365,sprint_role_filter='All')
    session$flushReact()
    expect_gt(nrow(roster_tbl()),0)
    expect_true(all(player_name_key(roster_tbl()$athleteName) %in% r$players$key))
    expect_true(all(player_name_key(athlete_choices()$athleteName) %in% r$players$key))
    expect_true(all(player_name_key(sprint_trend_source()$athlete_name) %in% r$players$key))
    expect_no_error(cmj_monitor_long());expect_no_error(staff_alert_rows_raw());expect_no_error(pitcher_perf_table_team());expect_no_error(output$alert_inbox_kpis)
  })
})
rm(select)
test_that('Combined Health renders four pitching metrics and respects player/date filters', {
  shiny::testServer(function(input,output,session) {health <- health_server(input,output,session,reactive(data.frame()),reactive(data.frame()))}, {
    session$setInputs(hc_combined_player='cade smith',hc_combined_dates=as.Date(c('2026-09-17','2026-09-26')),hc_combined_metrics=names(trackman_metrics))
    expect_equal(length(health$combined_plot_data()),4);expect_true(all(health$pitching_daily()$key=='cade smith'))
    expect_no_error(output$hc_trackman_trend);expect_no_error(output$hc_trackman_table);expect_match(output$hc_trackman_cards$html,'KPI #1')
    session$setInputs(hc_combined_dates=as.Date(c('2026-09-24','2026-09-24')))
    expect_true(all(health$pitching_daily()$date==as.Date('2026-09-24')))
    session$setInputs(hc_combined_player='brady boles');expect_equal(nrow(health$pitching_daily()),0);expect_match(output$hc_trackman_cards$html,'No measurements')
    session$setInputs(hc_combined_player='jonathan anders',hc_combined_dates=as.Date(c('2026-09-01','2026-09-29')));expect_gt(nrow(health$pitching_daily()),0)
  })
})
cat('Roster, TrackMan, masked-select and Combined Health checks passed.\n')
