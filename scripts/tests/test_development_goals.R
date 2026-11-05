#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(dplyr);library(readr);library(lubridate)})
`%||%`<-function(a,b) if(is.null(a)) b else a
source("WallyApps/shared/development_goals.R")
source("WallyApps/PitchingApp/helpers/situations.R")
stopifnot(identical(names(development_specs("hitting")),c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZWhiff%","Chase%","EV>95%","10-35%")),
  identical(names(development_specs("pitching")),c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZWhiff%","GB%","Strike%","E&A%")),
  file.exists(development_portrait("Boles, Brady")),file.exists(development_portrait("Cade Smith")),
  development_portrait("Unknown, Player")=="")
for(path in list.files(file.path(development_root,"assets"),pattern="^portrait-.*[.]jpg$",full.names=TRUE)) stopifnot(inherits(development_image(path),"nativeRaster"))
stopifnot(length(grep("portrait-",ls(.development_image_cache),fixed=TRUE))<=6)
data<-data.frame(Pitcher="Smith, Cade",SeasonGroup="S26",is_bullpen=FALSE)
vals<-setNames(rep(.3,11),unname(development_specs("pitching")))
pdf_plans<-list()
shiny::testServer(development_server,args=list(rows=reactive(data),player_column="Pitcher",seasons=c("2026 Season"="S26"),
  summarize=function(d) as.list(vals),percentile=function(value,metric) 70,mode="pitching"),{
  session$setInputs(season="S26",player="Smith, Cade",stat1="K%",stat2="BB%")
  session$setInputs(title1="Improve strikeout rate",goal1="30%",support1_1="Attack the zone",support1_2="Finish breaking balls",support1_3="Win two-strike counts",drill1_1="Bullpen targets")
  p<-plan();stopifnot(length(p$objectives)==1,!length(development_payload_errors(p)),length(p$objectives[[1]]$drills)==1)
  pdf_plans[[1]]<<-p
  session$setInputs(add=1);stopifnot(length(plan()$objectives)==2,length(development_payload_errors(plan()))>0,plan()$objectives[[1]]$goal=="30%")
  session$setInputs(title2="Reduce walks",goal2="6%",support2_1="Repeat delivery",support2_2="Fastball command",support2_3="Compete in the zone")
  stopifnot(!length(development_payload_errors(plan())))
  pdf_plans[[2]]<<-plan()
  session$setInputs(remove=1);stopifnot(length(plan()$objectives)==1,plan()$objectives[[1]]$goal=="30%")
})
for(i in 1:2) {
  file<-tempfile(fileext=".pdf");development_pdf(file,pdf_plans[[i]])
  stopifnot(file.exists(file),pdftools::pdf_info(file)$pages==1)
  content<-pdftools::pdf_text(file)
  stopifnot(grepl("OBJECTIVE 1",content,fixed=TRUE),grepl("OBJECTIVE 2",content,fixed=TRUE)==(i==2),grepl("30%",content,fixed=TRUE))
  unlink(file)
}
# Real-data regression: the compact full-game snapshots must recover Cade's
# known close leads and seventh-inning-or-later save situations.
own<-read_csv("WallyApps/PitchingApp/data/2026 Season - cleaned.csv",show_col_types=FALSE)
snapshots<-read_csv("WallyApps/PitchingApp/reference/pa_scores_2026.csv",show_col_types=FALSE)
venues<-read.csv("WallyApps/PitchingApp/reference/venue_locations.csv")
a<-pitching_annotate_situations(own,venues=venues,score_snapshots=snapshots)
cade<-a[grepl("Cade",a$Pitcher),]
stopifnot(sum(pitching_situation_mask(cade,"Tight Lead"))>0,sum(pitching_situation_mask(cade,"Save Situations"))>0)
save<-cade[pitching_situation_mask(cade,"Save Situations"),]
stopifnot(all(save$Inning>=7),all(save$.sit_lead>=1 & save$.sit_lead<=3))
# xwOBA: observed BB/HBP/K outcomes and expected contact share one PA mean.
last<-data.frame(PitchCall=c("InPlay","BallCalled","HitByPitch","StrikeSwinging"),KorBB=c("Undefined","Walk","Undefined","Strikeout"),PlayResult="Undefined",xwOBA=c(.5,NA,NA,NA))
stopifnot(isTRUE(all.equal(development_xwoba_values(last,NULL),c(.5,.690,.720,0))))
cat("Development Goals: exact metric sets, editor validation, one/two objectives, portraits, xwOBA and Cade score regression passed.\n")
