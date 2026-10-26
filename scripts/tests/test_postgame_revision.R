suppressPackageStartupMessages({library(dplyr);library(ggplot2)})
source("team_config.R")
p <- new.env(parent=globalenv())
for(expr in parse("WallyApps/PitchingApp/PitchingApp.R")) if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&is.symbol(expr[[2]])&&as.character(expr[[2]]) %in% c("pitching_pa_key","order_pitching_report_rows","rebuild_pitching_report_pa")) eval(expr,p)
d <- tibble(PitchNo=258:264,GameUID="corrected-game",Inning=7,`Top/Bottom`="Top",
 PAofInning=c(1,1,1,2,2,2,2),PitchofPA=c(1,2,3,1,2,3,4),
 Batter=c(rep("Tanner Carson",3),rep("Drayton Mitchell",4)),
 PlayResult=c("Undefined","Undefined","HomeRun","Undefined","Undefined","Undefined","Out"),PA_ID=99,PitchNum=99)
# Corrected pitch 261 remains physically first in the saved CSV.
r <- p$rebuild_pitching_report_pa(d[c(4,1:3,5:7),])
stopifnot(identical(r$PitchNo,258:264),identical(as.integer(table(r$PA_ID)),c(3L,4L)),identical(r$PitchNum,c(1L,2L,3L,1L,2L,3L,4L)))
stopifnot(identical(r$PlayResult[c(3,7)],c("HomeRun","Out")))
matchup <- base_report_matchup(data.frame(PitcherTeam="TEX_BOB",HomeTeam="TCU_HFG",AwayTeam="TEX_BOB"))
stopifnot(matchup$external,matchup$separator=="@",matchup$opponent=="TCU_HFG")
stopifnot(!base_report_matchup(data.frame(PitcherTeam="TEX_BOB",HomeTeam="TEX_BOB",AwayTeam="TEX_BOB"))$external)
stopifnot(base_report_matchup(data.frame(BatterTeam="TEX_BOB",HomeTeam="TEX_BOB",AwayTeam="TCU_HFG"),"BatterTeam")$separator=="vs.")
team_palette <- function(code) list(logo_url=if(code=="TCU_HFG") "fixture-tcu" else NA_character_)
fetch_team_logo <- function(code,url) if(!is.na(url)) array(1,dim=c(2,2,3)) else NULL
stopifnot(!is.null(base_report_opponent_image(matchup)),is.null(base_report_opponent_image(list(external=TRUE,opponent="UNKNOWN"))),
          is.null(base_report_opponent_image(list(external=FALSE,opponent="TEX_BOB"))))
rm(team_palette,fetch_team_logo)
ref <- base_umpire_run_reference()
stopifnot(nrow(ref)==42,all(ref$samples>0))
g <- data.frame(PitchUID=paste0("p",1:80),PitchNo=1:80,Date="2026-10-24",HomeTeam="TEX_BOB",AwayTeam="TCU_HFG",
  Pitcher="Test Pitcher",Batter="Test Batter",Inning=rep(1:8,each=10),`Top/Bottom`=rep(c("Top","Bottom"),40),
  PitchCall=rep(c("StrikeCalled","BallCalled","BallCalled","StrikeCalled"),20),
  PlateLocSide=rep(c(0,0,1.3,1.3),20),PlateLocHeight=2.5,Balls=1,Strikes=1,Outs=1,check.names=FALSE)
card <- base_umpire_scorecard_data(g,ref)
stopifnot(card$accuracy==.5,card$ball_accuracy==.5,card$strike_accuracy==.5,is.finite(card$consistency),is.finite(card$favor))
# Inside called ball favors batting side; outside called strike favors fielding side.
stopifnot(card$pitches$.offense_runs[2]>0,card$pitches$.offense_runs[4]<0,
          card$pitches$.home_runs[2]>0,card$pitches$.home_runs[4]<0)
# Missing location excluded, missing count does not silently become a 0-0 count.
bad<-g;bad$PlateLocSide[1]<-NA;bad$Balls[2]<-NA
badcard<-base_umpire_scorecard_data(bad,ref)
stopifnot(badcard$excluded==1,is.na(badcard$favor))
stopifnot(is.finite(base_umpire_call_gap(3,2,2,ref)),is.na(base_umpire_call_gap(NA,2,2,ref)))
stopifnot(is.na(base_umpire_consistency(c(0,1),c(2,3),c(TRUE,FALSE))))
if(nzchar(Sys.getenv("BASE_UMPIRE_QA"))) {
  pdf(Sys.getenv("BASE_UMPIRE_QA"),width=8.5,height=11,useDingbats=FALSE)
  print(base_umpire_card_plot(card));dev.off()
}
cat("Corrected PA sequence, matchup codes, umpire scoring signs, missing inputs, and reference checks passed.\n")

# Whole-game scores include in-play pitches, and duplicate exports do not add runs.
h <- g
h$RunsScored <- 0
h$RunsScored[c(3,4)] <- c(2,1)
h$PitchCall[c(3,4)] <- "InPlay"
h$Strikes[2] <- 2; h$Outs[2] <- 2
hc <- base_umpire_scorecard_data(rbind(h[80:1,],h[3,]),ref)
stopifnot(hc$total==80,hc$taken==78,hc$score["away"]==2,hc$score["home"]==1,
  hc$pitches$.later_runs[hc$pitches$PitchNo==2]==1,nrow(hc$misses)==39)
stopifnot(hc$pitches$.favor_side[hc$pitches$PitchNo==2]=="In favor")
h$HomeTeam <- "TCU_HFG"; h$AwayTeam <- "TEX_BOB"
stopifnot(base_umpire_scorecard_data(h,ref)$pitches$.favor_side[2]=="Against")
h$HomeTeam <- "TEX_BOB"
stopifnot(base_umpire_scorecard_data(h,ref)$focus_home)
names(h)[names(h)=="RunsScored"] <- "Runs Scored On Play"
stopifnot(base_umpire_scorecard_data(h,ref)$score["away"]==2)
h$GameUID <- "game";h$GameUID[1]<-""
stopifnot(length(unique(base_umpire_game_keys(h)))==1)
# Both plot point layers retain every miss, not just the three impact summaries.
stopifnot(sum(hc$misses$.miss_type=="Ball called strike")+sum(hc$misses$.miss_type=="Strike called ball")==39)
