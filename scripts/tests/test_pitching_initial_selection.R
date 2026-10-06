#!/usr/bin/env Rscript
# Exercise startup selection without loading models or the full Shiny app.
expressions <- parse("WallyApps/PitchingApp/PitchingApp.R")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("initial_pitching_selection"))) eval(expr)
}
rows <- data.frame(
  Pitcher = c("Alphabetical", "Reliever", "Starter", "Excluded"),
  CustomGameID = c("spring", "fall", "fall", "fall"),
  GameDate = as.Date(c("2026-05-01", rep("2026-10-01", 3))),
  SeasonGroup = c("S26", "F26", "F26", "F26"),
  Inning = c(1, 6, 1, 1), PitchNo = c(1, 120, 2, 1)
)
choices <- c("Alphabetical", "Reliever", "Starter")
result <- initial_pitching_selection(rows, choices, c("fall", "spring"))
stopifnot(identical(result, list(pitcher = "Starter", game = "fall", season = "F26")))
# Within the same inning the first pitch determines the starter, not row order.
rows$Inning <- 1
stopifnot(identical(initial_pitching_selection(rows, choices, c("fall", "spring")), result))
rows$GameDate <- as.Date(NA)
stopifnot(identical(initial_pitching_selection(rows, choices, c("fall", "spring")),
                    list(pitcher = "Alphabetical", game = character(0), season = "F26")))
stopifnot(identical(initial_pitching_selection(rows[0, ], choices, character(0))$pitcher,
                    "Alphabetical"))
cat("Pitching initial selection checks passed.\n")
