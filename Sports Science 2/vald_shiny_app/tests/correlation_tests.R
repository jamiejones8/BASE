source("local_library.R", local = TRUE)
use_vald_local_library()
Sys.setenv(VALD_REFRESH_WORKER='true',VALD_AUTO_REFRESH='false',VALD_APP_ROOT=normalizePath(getwd()))
source('dashboard.R')
library(testthat)
dates<-as.Date('2026-01-01')+0:11
kpi<-data.frame(date=dates,value=2*(1:12)+3,n=10L)
predictor<-function(id,x,source='VALD')data.frame(id=id,source=source,label=id,unit='lbs',date=dates,value=x)
candidates<-rbind(predictor('positive',1:12),predictor('negative',12:1,'ArmCare'),predictor('constant',rep(1,12)),predictor('weak',c(5,1,11,4,8,2,12,6,3,10,7,9),'PULSE'))
test_that('rankings use absolute correlation, exclude constants, and adjust all tests',{
  x<-correlation_screen(kpi,candidates)
  expect_equal(nrow(x$results),3)
  expect_equal(sort(x$results$Correlation[1:2]),c(-1,1),tolerance=1e-8)
  expect_equal(x$results$Slope[x$results$id=='positive'],2,tolerance=1e-8)
  expect_true(all(x$results$Pairs==12));expect_equal(x$coverage$Reason[x$coverage$id=='constant'],'No variation')
  expect_equal(x$results$FDR_q,p.adjust(x$results$P_value,'BH'))
  expect_equal(nrow(correlation_screen(kpi[1:4,],candidates)$results),0)
  expect_true(all(is.na(correlation_screen(kpi,candidates,method='spearman')$results$Slope)))
})
test_that('source tabs include eligible metrics below the overall top ten without changing statistics',{
  results<-data.frame(id=1:45,Source=rep(c('VALD','ArmCare','PULSE'),each=15),Correlation=seq(.99,.1,length.out=45),FDR_q=seq(.01,.4,length.out=45))
  expect_equal(nrow(correlation_view(results)),10)
  for(source in c('VALD','ArmCare','PULSE')) {
    selected<-correlation_view(results,source)
    expect_equal(nrow(selected),15)
    expect_true(all(selected$Source==source))
    expect_equal(selected$FDR_q,results$FDR_q[results$Source==source])
    expect_equal(selected$id,results$id[results$Source==source])
  }
})
test_that('pairing excludes future values and does not reuse source dates',{
  p<-data.frame(date=dates[c(1,5,12)],value=c(1,5,12))
  matched<-correlation_pairs(kpi,p,3)
  expect_false(anyDuplicated(matched$source_date)>0)
  expect_true(all(matched$source_date<=matched$date));expect_true(all(matched$age_days<=3))
  expect_equal(nrow(matched),3)
  expect_equal(nrow(correlation_pairs(kpi[1:2,],p[3,,drop=FALSE],7)),0)
  p$date<-p$date-1
  same<-correlation_pairs(kpi,p,0);expect_true(all(same$age_days==0))
})
test_that('default matching accepts days zero through three before and rejects later or older dates',{
  k<-kpi[6,,drop=FALSE]
  for(age in 0:3) {
    p<-data.frame(date=k$date-age,value=1)
    expect_equal(correlation_pairs(k,p)$age_days,as.integer(age))
  }
  for(age in c(-1,4)) {
    p<-data.frame(date=k$date-age,value=1)
    expect_equal(nrow(correlation_pairs(k,p)),0)
  }
  p<-data.frame(date=k$date+c(-3,-1,0,1),value=1:4)
  expect_equal(correlation_pairs(k,p)$x,3)
  expect_equal(correlation_pairs(k,p[p$value!=3,])$x,2)
  expect_equal(nrow(correlation_pairs(k,data.frame(date=k$date-4,value=1),7)),0)
})
test_that('real player screen loads without inventing rows for sparse overlap',{
 load_published_snapshot()
 shiny::testServer(function(input,output,session){
  h<-health_server(input,output,session,reactive(roster_filter(shared_data$roster,read_fall_roster(),'athleteName')),reactive(shared_data$session_summary),vald_sprint=reactive(shared_data$sprint))
 },{
  session$setInputs(hc_combined_player='cade smith',hc_combined_dates=as.Date(c('2026-01-01','2026-09-29')),hc_corr_kpi='fb_velocity',hc_corr_min=8,hc_corr_pitches=5,hc_corr_age='0',hc_corr_method='pearson')
  x<-h$correlations();expect_gt(nrow(h$correlation_data()),0)
  expect_no_error(output$hc_corr_top);expect_no_error(output$hc_corr_status);expect_no_error(output$hc_corr_coverage)
  expect_true(!nrow(x$results)||all(x$results$Pairs>=8))
  cat('CADE coverage:',nrow(x$coverage),'metrics;',nrow(x$results),'eligible; max pairs',max(x$coverage$Pairs),'\n')
  session$setInputs(hc_corr_age='3');expect_no_error(h$correlations());cat('CADE prior 3-day max pairs:',max(h$correlations()$coverage$Pairs),'\n')
  session$setInputs(hc_corr_age='3',hc_corr_method='spearman');expect_no_error(h$correlations())
  session$setInputs(hc_combined_player='brady boles');expect_equal(nrow(h$correlations()$results),0)
 })
})
cat('Correlation pairing, ranking and server tests passed.\n')
test_that('a populated screen renders ranked results, scatterplots and detail',{
  old_health<-health_read;old_trackman<-trackman_read
  on.exit({health_read<<-old_health;trackman_read<<-old_trackman},add=TRUE)
  a<-data.frame(name='Cade Smith',key='cade smith',date=dates,order=seq_along(dates),check.names=FALSE)
  a[['Arm Score']]<-seq_along(dates);a[['Total Strength']]<-seq_along(dates)*3;a[['Weight (lbs)']]<-200
  health_read<<-function(...)list(fresh=a,post=data.frame(),exams=data.frame(),workload=data.frame(),events=data.frame(),errors=character())
  daily<-data.frame(key='cade smith',name='Cade Smith',date=dates,pitch_group='Fastball/sinker',pitch_count=10L,velocity_n=10L,spin_n=10L,
    velocity_mph=kpi$value,spin_rpm=kpi$value*30,pitcher_ids='test',source_files='in-memory fixture')
  trackman_read<<-function(...)list(daily=daily,errors=character(),files='in-memory fixture',duplicate_count=0L,unmatched=character(),excluded_tags=character())
  shiny::testServer(function(input,output,session){h<-health_server(input,output,session,reactive(data.frame()),reactive(data.frame()))},{
    session$setInputs(hc_combined_player='cade smith',hc_combined_dates=range(dates),hc_corr_kpi='fb_velocity',hc_corr_min=8,hc_corr_pitches=5,hc_corr_age='0',hc_corr_method='pearson')
    expect_equal(nrow(h$correlations()$results),3)
    expect_no_error(output$hc_corr_top);expect_no_error(output$hc_corr_detail);expect_no_error(output$hc_corr_scatter)
    expect_match(output$hc_corr_detail$html,'not an estimated causal impact')
    exported<-read.csv(output$hc_corr_export);expect_equal(nrow(exported),3);expect_true(all(exported$player=='cade smith'))
    paired<-read.csv(output$hc_corr_pairs_export);expect_equal(nrow(paired),12)
    session$setInputs(hc_corr_age='near1') # Old bookmarked mode must not re-enable future dates.
    exported<-read.csv(output$hc_corr_export);expect_true(all(exported$max_age_days==3));expect_true(all(exported$matching=='same_or_earlier'))
    paired<-read.csv(output$hc_corr_pairs_export);expect_true(all(paired$age_days>=0 & paired$age_days<=3))
    session$setInputs(hc_corr_min=NA_real_,hc_corr_pitches=NA_real_);expect_no_error(h$correlations())
    session$setInputs(hc_corr_top_rows_selected=2,hc_corr_method='spearman')
    expect_match(output$hc_corr_detail$html,'Spearman');expect_no_error(output$hc_corr_scatter)
    session$setInputs(hc_corr_source='ArmCare',hc_corr_top_rows_selected=1)
    expect_no_error(output$hc_corr_top);expect_match(output$hc_corr_detail$html,'ArmCare')
    tab_export<-read.csv(output$hc_corr_view_export);expect_equal(nrow(tab_export),3);expect_true(all(tab_export$Source=='ArmCare'))
    expect_equal(nrow(read.csv(output$hc_corr_pairs_export)),12)
    session$setInputs(hc_corr_source='VALD');expect_no_error(output$hc_corr_top);expect_null(output$hc_corr_detail)
    expect_equal(nrow(read.csv(output$hc_corr_view_export)),0)
    expect_equal(nrow(read.csv(output$hc_corr_pairs_export)),0)
    session$setInputs(hc_corr_source='PULSE');expect_no_error(output$hc_corr_top)
    session$setInputs(hc_corr_source='overall');expect_match(output$hc_corr_detail$html,'ArmCare')
    session$setInputs(hc_combined_player='brady boles');expect_equal(nrow(h$correlations()$results),0)
  })
})
