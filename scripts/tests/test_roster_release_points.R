source("WallyApps/shared/roster_release.R")
root <- "WallyApps/DepthChartApp"
players <- roster_release_profiles(root, tempfile())
points <- roster_release_points(players, 2027)
stopifnot(nrow(points) == 16L, !any(points$Pitcher %in% c("Bennett Fryman", "Jackson Cotton")),
  points$RelSide[points$Pitcher == "Cole Wisenbaker"] < 0,
  abs(points$AnchorY[points$Pitcher == "Cade Smith"] - 5.5) < 1e-12)
path <- tempfile(fileext = ".rds")
state <- readRDS(file.path(root, "data", "depth-chart-state.rds"))
state$playerDetails[["roster-28"]] <- list(heightIn = 78, releaseHeightFt = 6.2, releaseSideFt = 0)
recruit <- list(id = "recruit-fixture", first = "New", last = "Pitcher", positions = "LHP", throws = "L",
  entryClass = 1L, entryYear = 2027L, status = "Offered", recruit = TRUE,
  heightIn = 72, releaseHeightFt = 5.5, releaseSideFt = 2)
state$profiles <- c(state$profiles, list(recruit))
saveRDS(state, path)
signature <- roster_release_signature(root, path)
updated <- roster_release_points(roster_release_profiles(root, path), 2027)
cade <- updated[updated$Pitcher == "Cade Smith", ]
new <- updated[updated$id == "recruit-fixture", ]
stopifnot(cade$RelHeight == 6.2, cade$RelSide == 1.5, cade$SideDefault,
  abs(cade$AnchorY - 68/12) < 1e-12, cade$Source == "Saved roster profile",
  new$RelSide == -2, new$Status == "Offered", new$Class == "Freshman")
state$playerDetails[["roster-28"]]["releaseHeightFt"] <- list(NULL)
state$departures[["recruit-fixture"]] <- 2028L
saveRDS(state, path)
cleared <- roster_release_points(roster_release_profiles(root, path), 2027)
later <- roster_release_points(roster_release_profiles(root, path), 2028)
stopifnot(!"Cade Smith" %in% cleared$Pitcher, !"recruit-fixture" %in% later$id,
  roster_release_signature(root, path) != signature,
  identical(readRDS(path), state))
state$profiles[[length(state$profiles)]]$status <- "Recruiting"
saveRDS(state, path)
stopifnot(!"recruit-fixture" %in% roster_release_points(roster_release_profiles(root, path), 2027)$id)
unlink(path)
plot <- plotly::plotly_build(roster_release_plot(updated))$x
stopifnot(plot$layout$yaxis$scaleanchor == "x", plot$layout$yaxis$scaleratio == 1,
  length(plot$layout$annotations) == nrow(updated),
  any(vapply(plot$data, function(x) identical(x$line$dash, "9px,6px"), logical(1))))
source("WallyApps/PitchingApp/helpers/team_trends.R")
suppressPackageStartupMessages(library(dplyr))
counts <- data.frame(PA_ID = c("a", "b", "a", "a", "b"), PitchNum = c(3, 1, 1, 2, 2),
  PitchCall = c("FoulBall", "BallCalled", "StrikeCalled", "FoulBall", "StrikeCalled"),
  Balls = c(1, 0, 0, 1, 1), StrikesPre = 2, BallsPre = 3)
fixed <- pitching_team_trend_counts(counts)
stopifnot(identical(fixed$StrikesPre, c(2L, 0L, 0L, 1L, 0L)),
  identical(fixed$BallsPre, counts$Balls),
  identical(pitching_team_trend_counts(fixed), fixed))
ref <- read.csv("WallyApps/PitchingApp/reference/d1_team_trend_averages.csv")
stopifnot(pitching_team_d1_average(ref, "K%")$team_n > 290,
  pitching_team_d1_average(ref, "Whiff%", "Slider")$scope == "pitch_type",
  is.null(pitching_team_d1_average(ref, "Unknown stat")))
cat("Roster release points: shared saved data, edits/clears, season eligibility, hand orientation, angles, status styles and team reference lookup passed.\n")
