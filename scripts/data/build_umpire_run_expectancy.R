# Empirical count/out run expectancy for TrackMan exports without base occupancy.
suppressPackageStartupMessages({library(arrow);library(dplyr);library(readr)})
args <- commandArgs(TRUE)
stopifnot(length(args)==1L,file.exists(args[[1]]))
ds <- open_dataset(args[[1]])
cols <- c("PitchUID","GameUID","GameID","Date","Inning","Top/Bottom","PitchNo","PAofInning","PitchofPA","Outs","Balls","Strikes","RunsScored","OutsOnPlay","KorBB","PitchCall")
d <- ds %>% select(any_of(cols)) %>% collect()
for(nm in c("Inning","PitchNo","PAofInning","PitchofPA","Outs","Balls","Strikes","RunsScored","OutsOnPlay")) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
d <- d %>% distinct(PitchUID,.keep_all=TRUE) %>%
  filter(!is.na(GameUID),nzchar(GameUID),`Top/Bottom` %in% c("Top","Bottom"),is.finite(Inning)) %>%
  arrange(GameUID,Inning,`Top/Bottom`,PitchNo) %>%
  group_by(GameUID,Inning,`Top/Bottom`) %>%
  mutate(complete=any(Outs+pmax(coalesce(OutsOnPlay,0),as.numeric(KorBB %in% c("Strikeout","K")))>=3,na.rm=TRUE),
         remaining_runs=rev(cumsum(rev(coalesce(RunsScored,0))))) %>% ungroup() %>%
  filter(complete,Outs %in% 0:2,Balls %in% 0:3,Strikes %in% 0:2)
d_full <- d
d <- d %>% distinct(GameUID,Inning,`Top/Bottom`,PAofInning,Outs,Balls,Strikes,.keep_all=TRUE)
ref <- d %>% group_by(outs=Outs,balls=Balls,strikes=Strikes) %>%
  summarise(expected_runs=mean(remaining_runs),samples=n(),.groups="drop")
stopifnot(nrow(ref)==36,all(ref$samples>100),all(is.finite(ref$expected_runs)))
walks <- d_full %>% filter(KorBB %in% c("Walk","BB")) %>% group_by(outs=Outs) %>%
  summarise(expected_runs=mean(remaining_runs),samples=n(),.groups="drop") %>% mutate(balls=4L,strikes=0L)
ks <- d_full %>% filter(KorBB %in% c("Strikeout","K")) %>% group_by(outs=Outs) %>%
  summarise(expected_runs=mean(remaining_runs),samples=n(),.groups="drop") %>% mutate(balls=0L,strikes=3L)
ref <- bind_rows(ref,walks,ks)
stopifnot(nrow(ref)==42)
write_csv(ref,"data/reference/umpire_count_out_run_expectancy.csv")
writeLines(c("Source: cleaned 2026 NCAA D1 Pitching/Hitting parquet.",
 "Method: mean remaining runs in complete half innings, one observation per PA/count/out state.",
 "TrackMan source does not contain base occupancy. This 36-state reference plus walk/strikeout endpoints averages over runners and is an estimate, not Umpire Scorecards' 288-state MLB model.",
 paste("Complete-state observations:",nrow(d)),paste("Complete half innings:",nrow(distinct(d,GameUID,Inning,`Top/Bottom`)))),"data/reference/umpire_run_expectancy_provenance.txt")
print(ref)
