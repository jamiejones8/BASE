#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(readr)})
source("team_config.R")
source("R/plots/pitch_geometry.R")
needed <- c("to_num", "nz_chr", "pick_first", "canon_pitch_call", "canonical_pitch_fuzzy",
  "facet_levels", "pitch_levels_all", "zone_s_in", "zone_h_in", "framing_in_zone",
  "framing_zone_type", "prepare_catcher_receiving_rows", "catcher_framing_subset",
  "framing_metric_base", "framing_pitch_group")
for (expr in parse("WallyApps/DefenseApp/DefenseApp.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% needed) eval(expr)
}
make <- function(x, z, catcher = "Namken, Clayton", season = "F26") {
  tibble(Catcher = catcher, SeasonGroup = season, PlateLocSide = x, PlateLocHeight = z,
         PitchCall = "StrikeCalled", PitchType = "Fastball", PitcherHand = "RHP")
}
# One distant pitch must not collapse an otherwise ordinary season's chart.
x <- c(rep(c(-1.1, 1.05, -.5, .5), 10), 6)
raw <- make(x, rep(2.5, length(x)))
prepared <- prepare_catcher_receiving_rows(raw)
stolen <- catcher_framing_subset(prepared, "ball_to_strike")
stopifnot(identical(prepared$plate_x, raw$PlateLocSide), identical(prepared$plate_z, raw$PlateLocHeight),
          identical(stolen$plot_x, stolen$plate_x), identical(stolen$plot_z, stolen$plate_z))
# Small subsets formerly crossed the q90 heuristic and changed percentile zones.
subset <- make(c(-1.1, 1.05, -.5, .5, 6), rep(2.5, 5))
small <- prepare_catcher_receiving_rows(subset)
stopifnot(identical(small$plate_x, subset$PlateLocSide),
          identical(framing_metric_base(small)$in_zone, c(FALSE, FALSE, TRUE, TRUE, FALSE)))
# Filtering or re-preparing the same pitch cannot change its coordinates.
one <- prepare_catcher_receiving_rows(subset[5, ])
again <- prepare_catcher_receiving_rows(small)
stopifnot(one$plate_x == 6, identical(again$plate_x, small$plate_x))
stale <- subset
stale$plate_x <- stale$PlateLocSide / 12
stale$plate_z <- stale$PlateLocHeight / 12
recovered <- prepare_catcher_receiving_rows(stale)
stopifnot(identical(recovered$plate_x, subset$PlateLocSide),
          identical(recovered$plate_z, subset$PlateLocHeight))
for (name in c("Namken, Clayton", "McKnight, Jaxon", "Anctil, Justin")) {
  for (season in c("S26", "F26")) {
    d <- make(c(-.9, .4, 1.2), c(2, 3, 3.7), name, season)
    p <- prepare_catcher_receiving_rows(d)
    stopifnot(identical(p$plate_x, d$PlateLocSide), identical(p$plate_z, d$PlateLocHeight))
  }
}
inches <- tibble(PlateLocSideInches = c(-12, 6), PlateLocHeightInches = c(30, 24),
                PitchCall = "StrikeCalled")
p <- prepare_catcher_receiving_rows(inches)
stopifnot(identical(p$plate_x, c(-1, .5)), identical(p$plate_z, c(2.5, 2)))
cat("Catcher coordinates: TrackMan feet preserved across outliers, filtering, repeated preparation, catchers and seasons; explicit inches converted once.\n")
