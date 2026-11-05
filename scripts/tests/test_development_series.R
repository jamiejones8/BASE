#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(lubridate)})
source("WallyApps/shared/development_goals.R")
mk<-function(game,pa,pitch,swing,miss,date="2026-02-14",start="2026-02-14 19:00:00") data.frame(
  GameID=game,Inning=1,`Top/Bottom`="Top",PAofInning=pa,PitchofPA=pitch,
  PitchUID=paste(game,pa,pitch),PitchCall=ifelse(pitch==max(pitch),"InPlay","BallCalled"),
  KorBB="Undefined",Strikes=0,Date=date,UTCDateTime=start,Swing=swing,Miss=miss,check.names=FALSE)
one<-mk("game-one",1,1,TRUE,TRUE)
two<-mk("game-one",2,1:9,TRUE,FALSE)
three<-mk("game-two",1,1,FALSE,FALSE,start="2026-02-14 23:00:00")
rows<-bind_rows(three,two,one,two)
summary<-function(d) list(`Whiff%`=sum(d$Miss)/sum(d$Swing),PA=length(unique(d$.development_pa)))
series<-development_rolling_values(rows,2,summary,"test",cache=FALSE)
stopifnot(nrow(series)==3,identical(series$PAIndex,1:3),
  isTRUE(all.equal(series$`Whiff%`,c(1,.1,0))),identical(series$PA,c(1,2,2)))
# Partial PAs cannot add a chart point, and both doubleheader games remain distinct.
partial<-mk("game-two",2,1,TRUE,FALSE);partial$PitchCall<-"BallCalled"
ordered<-development_pa_order(bind_rows(rows,partial))
stopifnot(max(ordered$.development_pa)==3,n_distinct(ordered$GameID)==2)
stopifnot(development_goal_target("35%","EV>95%")==.35,
  development_goal_target(".350","wOBA")==.350,
  development_goal_target("0.35","EV>95%")==.35,
  is.na(development_goal_target("from 20% to 35%","EV>95%")),
  development_goal_target("from 20% to 35%","EV>95%",35)==.35)
# Exact threshold crossings split a segment; lower-is-better reverses green.
line<-data.frame(PAIndex=1:3,Value=c(.1,.3,.2))
high<-development_goal_segments(line,.2,FALSE)
low<-development_goal_segments(line,.2,TRUE)
stopifnot(nrow(high)==3,high$xend[1]==1.5,high$yend[1]==.2,
  identical(high$met,c(FALSE,TRUE,TRUE)),identical(low$met,c(TRUE,FALSE,FALSE)))
gap<-line;gap$Value[2]<-NA_real_
stopifnot(nrow(development_goal_segments(gap,.2,FALSE))==0)
# Cutout masks change only alpha, preserving the original photo's RGB bits.
for(name in c("Brady Boles","Cade Smith","Clayton Namken")) {
  path<-development_portrait(name);original<-jpeg::readJPEG(path,native=TRUE);cutout<-development_image(path)
  original_bytes<-writeBin(as.integer(original),raw(),size=4L,endian="little")
  cutout_bytes<-writeBin(as.integer(cutout),raw(),size=4L,endian="little")
  rgb<-seq_along(original_bytes)%%4L!=0L
  stopifnot(identical(original_bytes[rgb],cutout_bytes[rgb]))
  alpha<-matrix(as.integer(cutout_bytes[seq.int(4L,length(cutout_bytes),by=4L)]),nrow=nrow(cutout),byrow=TRUE)
  stopifnot(alpha[1,1]==0,alpha[round(nrow(alpha)*.4),round(ncol(alpha)*.5)]==255,all(tail(alpha,1)==0))
}
cat("Development series: PA ordering, proper denominators, doubleheaders, partial PAs, exact goal crossings and original-photo cutouts passed.\n")
