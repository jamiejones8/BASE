#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(dplyr)})
source("WallyApps/shared/development_goals.R")
goal <- list(title="Improve K rate",metric="K%",goal="14%",target=.14,baseline_value=.4,
  lower_better=TRUE,supports="Recognize spin",drills=character())
plan <- function(player,season,saved) list(player=player,player_key=development_name_key(player),
  mode="hitting",baseline_season=season,saved_at=saved,objectives=list(goal))
plans <- list(plan("Player A","S26","2026-10-01 UTC"),plan("Player A","F26","2026-11-01 UTC"),
  plan("Player B","S26","2026-10-01 UTC"),plan("Player C","PS27","2027-01-01 UTC"))
make_rows <- function(player,season,rates,date,opponent="OPP",bullpen=FALSE) {
  n <- length(rates)
  data.frame(Player=player,SeasonGroup=season,Rate=rates,Date=as.character(as.Date(date)+seq_len(n)-1),
    GameID=paste(player,season,opponent,seq_len(n)),PitchUID=paste(player,season,opponent,seq_len(n)),
    Inning=1,PAofInning=1,PitchofPA=1,PitchCall="InPlay",PitcherTeam="TEX_BOB",BatterTeam=opponent,is_bullpen=bullpen)
}
rows <- bind_rows(make_rows("Player A","S25",.25,"2025-04-01"),
  make_rows("Player A","S26",c(.2,.2),"2026-04-01"),
  make_rows("Player A","F26",.12,"2026-10-01"),
  make_rows("Player A","PS27",c(.16,.16,.16),"2027-01-01"),
  make_rows("Player A","PS27",.9,"2027-01-20",bullpen=TRUE),
  make_rows("Player B","S26",.3,"2026-04-01"),
  make_rows("Player C","PS27",.1,"2027-01-01"))
summary <- function(d) list(`K%`=mean(d$Rate))
seasons <- c("2026 Season"="S26","2026 Fall"="F26","2027 Scrimmages"="PS27","2027 Season"="S27")
choices <- development_tracking_options(seasons,unique(rows$SeasonGroup),plans)
stopifnot(identical(unname(choices$options),c("S26","S27")),choices$default=="S27",
  choices$baseline_default=="PS27",all(grepl("^S[0-9]{2}$",choices$options)),
  identical(unname(development_goal_season(c("S26","F26","PS27"))),rep("S27",3)))
latest <- development_latest_plans(plans)
stopifnot(length(latest)==3L,latest[[1]]$baseline_season=="F26")
# The most recently SAVED version wins even when an older baseline is edited.
edited <- plans[[1]];edited$saved_at <- "2027-01-02 UTC"
stopifnot(development_latest_plans(c(plans,list(edited)))[[1]]$baseline_season=="S26")
period <- development_tracking_period(rows,"S27")
stopifnot(period$fallback,identical(period$sources,c("F26","PS27")),nrow(period$rows)==5L)
models <- development_tracking_models(plans,"S27",rows,"Player",summary)
a <- models[[1]]$objectives[[1]]$progress
stopifnot(length(models)==3L,models[[1]]$plan$baseline_season=="F26",a$baseline==.2,
  isTRUE(all.equal(a$current,.15)),a$state=="Improving",a$color=="#171717",
  !is.finite(models[[2]]$objectives[[1]]$progress$current),models[[3]]$objectives[[1]]$progress$hit,
  models[[3]]$objectives[[1]]$progress$baseline==.4,
  grepl("Saved baseline",models[[3]]$objectives[[1]]$progress$baseline_label))
series <- development_game_values(development_rows_for_season(period$rows,period$seasons,"Player","playera"),summary,"hitting")
stopifnot(nrow(series)==4L,isTRUE(all.equal(tail(series$`K%`,1),a$current)))
# A year selection changes the progress data, never hides a saved player.
past <- development_tracking_models(plans,"S26",rows,"Player",summary)
stopifnot(length(past)==3L,past[[1]]$objectives[[1]]$progress$baseline==.25,
  past[[1]]$objectives[[1]]$progress$current==.2)
# A same-team practice labeled S27 cannot prematurely replace preseason data.
practice <- make_rows("Player A","S27",.99,"2027-02-01",opponent="TEX_BOB")
stopifnot(development_tracking_period(bind_rows(rows,practice),"S27")$fallback)
competition <- make_rows("Player A","S27",.1,"2027-02-19")
started <- bind_rows(rows,practice,competition)
period <- development_tracking_period(started,"S27")
active <- development_tracking_models(plans,"S27",started,"Player",summary)
stopifnot(!period$fallback,nrow(period$rows)==1L,identical(period$sources,"S27"),
  active[[1]]$objectives[[1]]$progress$current==.1,active[[1]]$objectives[[1]]$progress$hit,
  !is.finite(active[[2]]$objectives[[1]]$progress$current),length(active)==3L)
no_data <- development_tracking_models(plans,"S28",rows,"Player",summary)
stopifnot(length(no_data)==3L,all(vapply(no_data,function(m) !is.finite(m$objectives[[1]]$progress$current),logical(1))))
cat("NCAA goal tracking: annual choices, every saved player, latest plan, weighted fall/scrimmage pool, annual baselines, outside-competition switch and missing data passed.\n")
