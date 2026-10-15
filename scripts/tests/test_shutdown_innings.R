source('team_config.R')
suppressPackageStartupMessages(library(dplyr))
e <- new.env(parent=globalenv())
e$sdiv <- function(a,b) ifelse(b>0,a/b,NA_real_)
e$prefer_team_or_portal <- function(d,col,team) d[d[[col]]==team,]
e$TEAM_CODE <- 'TEX_BOB'
find_helper <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (identical(x[[1]],as.name('<-')) && identical(x[[2]],as.name('compute_shutdown_by_pitcher'))) eval(x,e)
  else for (v in as.list(x)[-1]) if(!missing(v) && is.call(v)) find_helper(v)
}
for (expr in parse('WallyApps/PitchingApp/PitchingApp.R')) find_helper(expr)
half <- function(game,inning,side,pitcher,runs=0,outs=3,home='TEX_BOB',away='OPP') {
  n <- max(1,outs)
  data.frame(CustomGameID=game,Inning=inning,TopBottom=side,Pitcher=pitcher,
    PitcherTeam=if(side=='Top')home else away,HomeTeam=home,AwayTeam=away,
    RunsScored=c(runs,rep(0,n-1)),OutsOnPlay=rep(if(outs)1 else 0,n),
    PitchUID=paste(game,inning,side,seq_len(n),sep='-'))
}
d <- bind_rows(
 half('home',1,'Top','NoOpportunity'),half('home',1,'Bottom','Opponent',1),half('home',2,'Top','HomeSuccess'),
 half('away',1,'Top','Opponent',1,home='OPP',away='TEX_BOB'),half('away',1,'Bottom','AwaySuccess',home='OPP',away='TEX_BOB'),
 half('scrim1',2,'Top','SideA',1,away='TEX_BOB'),half('scrim1',2,'Bottom','ScrimSuccess',away='TEX_BOB'),
 half('scrim2',4,'Bottom','SideB',1,away='TEX_BOB'),half('scrim2',5,'Top','ScrimFailure',1,away='TEX_BOB'),
 half('short',1,'Bottom','Opponent',1),half('short',2,'Top','Incomplete',outs=2),
 half('scoreless',1,'Bottom','Opponent'),half('scoreless',2,'Top','NoScore'),
 half('relief',1,'Bottom','Opponent',1),half('relief',2,'Top','Starter',outs=2),
 transform(half('relief',2,'Top','Reliever',outs=1),PitchUID='reliever-1'))
p <- d[d$PitcherTeam=='TEX_BOB',]
result <- e$compute_shutdown_by_pitcher(p,bind_rows(d,d[1:3,]))
for (name in c('HomeSuccess','AwaySuccess','ScrimSuccess')) {
 x<-result[result$Pitcher==name,];stopifnot(x$ShutDownOpp==1,x$ShutDown==1)
}
for (name in c('ScrimFailure','Incomplete','Starter','Reliever')) {
 x<-result[result$Pitcher==name,];stopifnot(x$ShutDownOpp==1,x$ShutDown==0)
}
for (name in c('NoOpportunity','NoScore')) {
 x<-result[result$Pitcher==name,];stopifnot(x$ShutDownOpp==0,is.na(x$ShutDown_pct))
}
# A pitch-type or hand filter cannot erase the other outs in the same appearance.
x<-e$compute_shutdown_by_pitcher(p[p$Pitcher=='HomeSuccess',][1,],d)
stopifnot(x$ShutDown==1)
# TrackMan uses zero OutsOnPlay for most strikeouts; the K result still records an out.
k <- d[d$CustomGameID=='home',]
k$PlayResult <- 'Undefined'; k$KorBB <- 'Undefined'
k$KorBB[k$Pitcher=='HomeSuccess'] <- 'Strikeout'
k$OutsOnPlay[k$Pitcher=='HomeSuccess'] <- 0
x <- e$compute_shutdown_by_pitcher(k[k$Pitcher=='HomeSuccess',],k)
stopifnot(x$ShutDown==1)
cat('Shutdown innings: home, away, scrimmages, first inning, runs, outs, relief, duplicates and filters passed.\n')
