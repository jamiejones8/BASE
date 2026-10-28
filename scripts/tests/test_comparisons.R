library(shiny)
library(dplyr)
source("team_config.R")
calls <- 0L
fixture <- data.frame(Batter=c("A","B","A","C"),BatterTeam=TEAM_CONFIG$data_code,
  SeasonGroup=c("S26","S26","F26","F26"),GameID=c("g1","g1","g2","g2"),Date=c("2026-01-01","2026-01-01","2026-09-01","2026-09-01"),value=c(.2,.4,.6,.8))
loader <- function(column,team=NULL) { calls <<- calls+1L; if(is.null(team)) c("TCU_HFG") else fixture }
summary_fun <- function(d) list(AVG=mean(d$value))
args <- list(rows=reactive(fixture),seasons=c("2026 Season"="S26","2026 Fall"="F26"),player_column="Batter",
 summarize=summary_fun,percentile=function(v,m) v*100,metrics="AVG",lower_better=character(),d1_loader=loader)
testServer(base_comparison_server,args=args,{
 # Controls must contain their choices when first rendered, without update messages.
 season_html <- output$local_season_left_ui$html
 metric_html <- output$local_metric_ui$html
 stopifnot(grepl('value="S26"',season_html,fixed=TRUE),grepl('value="AVG"',metric_html,fixed=TRUE),calls==0L)
 session$setInputs(local_season_left="S26")
 stopifnot(grepl('Team Total',output$local_player_left_ui$html,fixed=TRUE),
           grepl('value="B"',output$local_player_left_ui$html,fixed=TRUE))
 session$setInputs(tab="local",local_season_left="S26",local_player_left="Team Total",local_season_right="F26",local_player_right="A",local_metric="AVG")
 stopifnot(calls==0L)
 html <- output$local_rankings$html
 stopifnot(grepl("30th percentile",html),grepl("60th percentile",html),grepl("→",html))
 plot <- output$local_plot_left
 stopifnot(length(plot)>0,calls==0L)
 session$setInputs(tab="opponents",opponents_season_left="S26",opponents_player_left="A",opponents_team="TCU_HFG",opponents_metric="AVG")
 html <- output$opponents_rankings$html
 stopifnot(calls>=2L,grepl("20th percentile",html),grepl("50th percentile",html))
})
for(pt in c("Fastball","Sinker","Cutter","Slider","Curveball","Sweeper","Changeup","Splitter")) {
 stopifnot(base_movement_trend_favorable(pt,"Velo (Max)",1),!base_movement_trend_favorable(pt,"Velo (Max)",-1))
}
stopifnot(base_movement_trend_favorable("Fastball","HB",-3),
 !base_movement_trend_favorable("Sinker","IVB",2),
 base_movement_trend_favorable("Changeup","Spin",-100),
 base_movement_trend_favorable("Cutter","IVB",2))
cat("Comparison selectors, pooled stats, arrows, lazy D1 reads and trend rules passed.\n")
# Rolling rates pool observations instead of averaging unequal game denominators.
rolling_fixture <- data.frame(GameID=c("a","b","b","b","c"),
 Date=c("2026-01-01",rep("2026-01-02",3),"2026-01-03"),value=c(0,1,1,1,0))
rolled <- base_comparison_rolling_series(rolling_fixture,"AVG",summary_fun,2)
stopifnot(isTRUE(all.equal(rolled$value,c(0,.75,.75))),grepl("Trailing 2 games",rolled$label[3]))
cat("Trailing-window denominator weighting passed.\n")
