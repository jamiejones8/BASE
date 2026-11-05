#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(dplyr); library(lubridate) })
source("WallyApps/PitchingApp/helpers/situations.R")
venues <- read.csv("WallyApps/PitchingApp/reference/venue_locations.csv", stringsAsFactors = FALSE)
schedule <- read.csv("config/texas_state_schedule_2027.csv", stringsAsFactors = FALSE)
fixture <- expand.grid(PAofInning = 1:3, half = 1:18)
fixture <- fixture[order(fixture$half, fixture$PAofInning), ]
fixture$Inning <- (fixture$half + 1L) %/% 2L
fixture$`Top/Bottom` <- ifelse(fixture$half %% 2, "Top", "Bottom")
fixture$PitchofPA <- 1L
fixture$PitchNo <- seq_len(nrow(fixture))
fixture$GameUID <- "complete-game"
fixture$GameID <- "complete-game"
fixture$PitchUID <- paste0("pitch-", fixture$PitchNo)
fixture$Pitcher <- ifelse(fixture$half %% 2 == 0, "Opponent", ifelse(fixture$half < 5 | fixture$half == 5 & fixture$PAofInning == 1, "Starter", "Reliever"))
fixture$PitcherTeam <- ifelse(fixture$half %% 2, "TEX_BOB", "OPP")
fixture$BatterTeam <- ifelse(fixture$half %% 2, "OPP", "TEX_BOB")
fixture$HomeTeam <- "TEX_BOB"
fixture$AwayTeam <- "OPP"
fixture$RunsScored <- 0
fixture$PitchCall <- "InPlay"
fixture$KorBB <- "Undefined"
fixture$PlayResult <- "Out"
fixture$OutsOnPlay <- 1
fixture$Strikes <- 0
fixture$Date <- "2026-06-21"
fixture$Stadium <- "TexasStateUniversity"
fixture$UTCDateTime <- "2026-06-22T01:30:00Z"
score <- function(half, runs) fixture$RunsScored[fixture$half == half & fixture$PAofInning == 1] <<- runs
score(1, 4); score(2, 1); score(4, 3); score(8, 2); score(15, 6)
a <- pitching_annotate_situations(fixture, venues = venues)
at <- function(half, pa = 1) which(fixture$half == half & fixture$PAofInning == pa)
stopifnot(a$.sit_lead[at(1)] == 0, a$.sit_lead[at(1, 2)] == -4,
  a$.sit_lead[at(2)] == 4, a$.sit_lead[at(2, 2)] == 3,
  a$.sit_lead[at(3)] == -3, a$.sit_lead[at(5)] == 0,
  a$.sit_lead[at(9)] == 2, a$.sit_lead[at(13)] == 2,
  a$.sit_lead[at(15)] == 2, a$.sit_lead[at(15, 2)] == -4)
stopifnot(a$.sit_entry[at(5)] == "Clean inning", a$.sit_entry[at(5, 2)] == "Mid inning",
  a$.sit_entry[at(5, 3)] == "Mid inning", a$.sit_entry[at(7)] == "Clean inning",
  a$.sit_phase[at(5)] == "Early innings", a$.sit_phase[at(7)] == "Mid innings",
  a$.sit_phase[at(13)] == "Late innings",
  all(a$.sit_home_away[fixture$half %% 2 == 1] == "Home"),
  all(a$.sit_home_away[fixture$half %% 2 == 0] == "Away"))
stopifnot(pitching_situation_mask(a, "Save Situations")[at(15)],
  !pitching_situation_mask(a, "Save Situations")[at(15, 2)],
  !pitching_situation_mask(a, "Save Situations")[at(9)],
  all(pitching_situation_mask(a, "Save Situations") <= pitching_situation_mask(a, "Tight Lead")))
# Context survives shuffling, duplicate exports, pitcher filters and doubleheaders.
set.seed(3)
duplicated <- bind_rows(fixture[sample(nrow(fixture)), ], fixture)
b <- pitching_annotate_situations(fixture[fixture$Pitcher == "Reliever", ], duplicated, venues)
stopifnot(identical(b$.sit_entry, a$.sit_entry[fixture$Pitcher == "Reliever"]),
  identical(b$.sit_lead, a$.sit_lead[fixture$Pitcher == "Reliever"]))
second <- fixture; second$GameUID <- second$GameID <- "doubleheader-two"; second$RunsScored <- 0
both <- pitching_annotate_situations(bind_rows(fixture, second), venues = venues)
stopifnot(all(both$.sit_lead[seq_len(nrow(second)) + nrow(fixture)] == 0))
# A change during PA 1 does not turn the reliever into the inning starter.
m <- fixture[1:3, ]; m$GameUID <- m$GameID <- "mid-pa"; m$PAofInning <- c(1, 1, 2)
m$PitchofPA <- c(1, 2, 1); m$Pitcher <- c("Starter", "Reliever", "Reliever")
m$PitchCall[1] <- "BallCalled"; m$RunsScored <- 0
m <- pitching_annotate_situations(m, venues = venues)
stopifnot(identical(m$.sit_entry, c("Clean inning", "Mid inning", "Mid inning")), !m$.sit_terminal[1])
# No missing half or missing PA may be silently treated as a zero-run segment.
partial <- pitching_annotate_situations(fixture[fixture$half %% 2 == 1, ], venues = venues)
stopifnot(all(is.na(partial$.sit_lead[partial$Inning > 1])))
gap <- fixture[-at(1, 2), ]; gap <- pitching_annotate_situations(gap, venues = venues)
stopifnot(is.na(gap$.sit_lead[which(gap$half == 1 & gap$PAofInning == 3)]))
anchor <- fixture[at(13):at(13, 3), ]; anchor$HomeScoreBefore <- 6; anchor$AwayScoreBefore <- 4
anchor <- pitching_annotate_situations(anchor, venues = venues)
stopifnot(all(anchor$.sit_lead == 2), all(pitching_situation_mask(anchor, "Save Situations")))
# A change of half does not prove that an incompletely recorded half ended.
unclosed <- fixture[c(at(1), at(2)), ]
unclosed <- pitching_annotate_situations(unclosed, venues = venues)
stopifnot(is.na(unclosed$.sit_lead[2]))
# A caught stealing may close a half without ending the PA at the plate.
runner_out <- fixture
runner_out$PitchCall[at(1, 3)] <- "BallCalled"
runner_out$PlayResult[at(1, 3)] <- "Undefined"
runner_out <- pitching_annotate_situations(runner_out, venues = venues)
stopifnot(runner_out$.sit_lead[at(2)] == 4)
# GameID is the shared export key even when producers assign different GameUIDs.
shared <- fixture
shared$GameUID <- "another-export-uid"
joined <- pitching_annotate_situations(selected <- fixture[fixture$half %% 2 == 1, ], shared, venues)
stopifnot(identical(joined$.sit_lead, a$.sit_lead[fixture$half %% 2 == 1]))
# Same UTC instant is after the San Marcos cutoff but still day in San Diego.
light <- fixture[1:2, ]; light$Stadium <- c("TexasStateUniversity", "Tony Gwynn Stadium")
light$`Top/Bottom` <- c("Top", "Bottom")
light <- pitching_annotate_situations(light, venues = venues)
stopifnot(identical(light$.sit_day_night, c("Night", "Day")))
sm <- venues[venues$city == "San Marcos, TX", ]
sunset <- pitching_sunset_utc(as.Date("2026-06-21"), sm$latitude, sm$longitude)
stopifnot(format(sunset, "%Y-%m-%d", tz = "America/Chicago") == "2026-06-21",
  abs(as.numeric(difftime(sunset, as.POSIXct("2026-06-21 20:35:00", tz = "America/Chicago"), units = "mins"))) < 3)
cutoff <- as.numeric(sunset) - 1800
boundary <- fixture[1:2, ]; boundary$UTCDateTime <- as.POSIXct(c(floor(cutoff), ceiling(cutoff)), origin = "1970-01-01", tz = "UTC")
boundary <- pitching_annotate_situations(boundary, venues = venues)
stopifnot(identical(boundary$.sit_day_night, c("Day", "Night")))
# Local wall clocks honor DST; misleading LocalDateTime values with Z stay UTC.
clock <- data.frame(Date = c("2026-03-07", "2026-03-08"), Time = "18:00:00")
parsed <- pitching_sit_timestamp(clock, rep("America/Chicago", 2))
stopifnot(identical(format(parsed, "%Y-%m-%d %H:%M", tz = "UTC"), c("2026-03-08 00:00", "2026-03-08 23:00")))
clock$LocalDateTime <- c("2026-03-08T00:00:00Z", "2026-03-08T23:00:00Z")
stopifnot(identical(as.numeric(parsed), as.numeric(pitching_sit_timestamp(clock, rep("America/Chicago", 2)))))
unknown <- fixture[1, ]; unknown$Stadium <- "Unmapped away ballpark"; unknown$`Top/Bottom` <- "Bottom"
stopifnot(is.na(pitching_annotate_situations(unknown, venues = venues)$.sit_day_night))
road <- fixture[1, ]; road$Stadium <- NA; road$Date <- "2027-04-16"; road$HomeTeam <- "SAN_AZT"; road$AwayTeam <- "TEX_BOB"; road$`Top/Bottom` <- "Bottom"
loc <- pitching_sit_locations(road, venues, schedule)
stopifnot(loc$city == "San Diego, CA", loc$timezone == "America/Los_Angeles")
road$Date <- "04/16/2027"
stopifnot(pitching_sit_locations(road, venues, schedule)$city == "San Diego, CA")
unknown_neutral <- fixture[1, ]; unknown_neutral$Stadium <- NA; unknown_neutral$Venue <- "Unmapped neutral field"
stopifnot(is.na(pitching_annotate_situations(unknown_neutral, venues = venues)$.sit_day_night))
extra <- fixture[1, ]; extra$Inning <- 12; extra$GameUID <- "extras"
stopifnot(pitching_annotate_situations(extra, venues = venues)$.sit_phase == "Late innings")
cat("Pitching situations: inning entry, PA-start scores, overlaps, missing context, sunset cutoffs, DST, and venues passed.\n")
# The optional NCAA lookup projects only requested games and fills context, not stats.
source("team_config.R")
source("R/integrations/wally_pitching_workspace.R")
context_file <- tempfile(fileext = ".parquet")
extra_game <- second
extra_game$UnusedPayload <- paste(rep("x", 1000), collapse = "")
master <- bind_rows(fixture, extra_game)
arrow::write_parquet(master, context_file)
original_master <- TEAM_CONFIG$data$ncaa_d1_master_file
TEAM_CONFIG$data$ncaa_d1_master_file <- context_file
selected <- fixture[fixture$half %% 2 == 1, ]
# Blank game IDs must not select blank rows from unrelated hitting exports.
selected <- bind_rows(selected, selected[1, ])
selected$GameUID[nrow(selected)] <- selected$GameID[nrow(selected)] <- NA_character_
context <- base_pitching_situation_context(selected)
stopifnot(nrow(context) == nrow(fixture), !"UnusedPayload" %in% names(context),
  all(context$GameUID == "complete-game"))
selected <- selected[-nrow(selected), ]
recovered <- pitching_annotate_situations(selected, context, venues)
stopifnot(identical(recovered$.sit_lead, a$.sit_lead[fixture$half %% 2 == 1]), nrow(recovered) == nrow(selected))
TEAM_CONFIG$data$ncaa_d1_master_file <- paste0(context_file, ".missing")
stopifnot(nrow(base_pitching_situation_context(selected)) == 0L)
long_ids <- selected[rep(1, 400), ]
long_ids$GameID <- paste0("game-", seq_len(nrow(long_ids)), "-", paste(rep("x", 60), collapse = ""))
long_ids$GameUID <- long_ids$GameID
stopifnot(nrow(base_pitching_situation_context(long_ids)) == 0L)
TEAM_CONFIG$data$ncaa_d1_master_file <- original_master
unlink(context_file)
cat("Read-only full-game context lookup and projection passed.\n")

stopifnot(pitching_situation_file_suffix("Total") == "", pitching_situation_file_suffix("Up 4+") == "_Up_4plus")
