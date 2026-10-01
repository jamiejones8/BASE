source("local_library.R", local = TRUE)
use_vald_local_library()
Sys.setenv(VALD_AUTO_REFRESH='false',VALD_REFRESH_WORKER='true',VALD_APP_ROOT=normalizePath(getwd()))
source('dashboard.R')
load_published_snapshot()
library(testthat)
test_that('ArmCare strength shows raw pounds and vendor bodyweight percentages', {
  d <- data.frame(date=as.Date('2026-09-20'),check.names=FALSE)
  d[['Total Strength']]<-200;d[['Weight (lbs)']]<-200;d[['Arm Score']]<-103
  for(p in c('IRTARM','ERTARM','STARM','GTARM')) {d[[paste(p,'Strength')]]<-50;d[[paste(p,'RS')]]<-.26}
  expect_equal(combined_arm_series(d,'score')$value,103)
  for(m in c('ir','er','scaption','grip')) {x<-combined_arm_series(d,m);expect_equal(x$value,c(50,26));expect_equal(x$unit,c('lbs','%BW'));expect_equal(x$axis,c('y','y2'))}
  expect_equal(combined_arm_series(d,'total')$value,c(200,100))
  d[['Weight (lbs)']]<-NA
  expect_true(is.na(combined_arm_series(d,'total')$value[2]))
})
test_that('PULSE defaults to ACWR and aggregates throw metrics by local day', {
  w<-data.frame(date=as.Date('2026-09-20'),check.names=FALSE);w[['A:C Ratio']]<-1.2;w[['One Day Workload']]<-40
  e<-data.frame(date=as.Date(c('2026-09-20','2026-09-20','2026-09-21')),armSpeed=c(1000,1400,NA))
  expect_equal(combined_pulse_series(w,e)$value,1.2)
  expect_equal(combined_pulse_series(w,e,'One Day Workload')$value,40)
  x<-combined_pulse_series(w,e,'armSpeed');expect_equal(x$value,c(1200,NA));expect_equal(x$n,c(2L,0L))
})
test_that('VALD catalog includes all imported metrics and separates test types', {
  c<-combined_vald_catalog(shared_data$session_summary,shared_data$sprint)
  expect_true(combined_default_vald %in% c$id)
  expect_true(any(grepl('Bodyweight in Pounds',c$label)))
  expect_true(any(grepl('Eccentric Peak Velocity',c$label)))
  expect_true(any(grepl('Countermovement Depth',c$label)))
  expect_true(any(c$source=='SmartSpeed'))
  expect_equal(sum(c$source=='ForceDecks'),nrow(unique(shared_data$session_summary[,c('testType','metricKey','metricUnit')])))
  v<-combined_vald_series(shared_data$session_summary,shared_data$sprint,c)
  expect_true(all(grepl('Jump Height',v$label)))
  expect_true(all(v$unit=='Centimeter'))
})
test_that('four fixed plots default correctly and each dropdown updates only its source', {
  shiny::testServer(function(input,output,session) {
    health<-health_server(input,output,session,reactive(roster_filter(shared_data$roster,read_fall_roster(),'athleteName')),
      reactive(shared_data$session_summary),vald_sprint=reactive(roster_filter(shared_data$sprint,read_fall_roster(),'athlete_name')))
  }, {
    session$setInputs(hc_combined_player='cade smith',hc_combined_dates=as.Date(c('2026-08-01','2026-09-29')))
    d<-health$combined_plot_data()
    expect_named(d,c('trackman','armcare','pulse','vald'))
    expect_true(all(grepl('velocity',d$trackman$label)))
    expect_true(all(d$armcare$label=='Arm Score'));expect_true(all(d$pulse$label=='ACWR'))
    expect_true(all(grepl('Jump Height',d$vald$label)))
    for(id in c('hc_trackman_trend','hc_armcare_trend','hc_pulse_trend','hc_vald_trend')) expect_no_error(output[[id]])
    session$setInputs(hc_arm_metric='ir',hc_pulse_metric='torque',hc_trackman_metric='bb_spin')
    d<-health$combined_plot_data();expect_equal(unique(d$armcare$unit),c('lbs','%BW'));expect_true(all(d$pulse$unit=='Nm'));expect_true(all(d$trackman$unit=='rpm'))
    for(id in c('hc_trackman_trend','hc_armcare_trend','hc_pulse_trend')) expect_no_error(output[[id]])
    c<-combined_vald_catalog(shared_data$session_summary,shared_data$sprint)
    id<-c$id[c$source=='SmartSpeed' & c$metric=='best_10yd' & c$test=='10yd Sprint'][1]
    session$setInputs(hc_vald_metric=id);expect_true(all(health$combined_plot_data()$vald$unit=='s'));expect_no_error(output$hc_vald_trend)
    session$setInputs(hc_vald_metric='fd|CMJ|BODY_WEIGHT_LBS|Pound');expect_true(all(health$combined_plot_data()$vald$unit=='Pound'));expect_no_error(output$hc_vald_trend)
  })
})
cat('Four-source dropdown and metric checks passed.\n')
