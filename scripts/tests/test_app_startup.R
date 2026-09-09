#!/usr/bin/env Rscript

app <- source(
  "app.R",
  local = new.env(parent = globalenv())
)$value

if (!inherits(app, "shiny.appobj")) {
  stop("app.R did not produce a Shiny application object.", call. = FALSE)
}

# Regression check for the HomeBASE percentile bug: this player was previously
# sent through the scouting scorer and incorrectly displayed at the 1st
# percentile. The original CAPS BrewStuff path resolves the same tracked sample
# to roughly 109 Stuff+ / 81st percentile.
alec_rows <- homebase_load_history_rows("pitcher", "Alec Beversdorf", refresh = TRUE)
if (nrow(alec_rows)) {
  alec_scored <- homebase_score_pitcher_rows(alec_rows)
  alec_metrics <- homebase_pitcher_metrics(alec_scored)
  alec_percentiles <- homebase_percentile_rows(alec_scored, "pitcher")
  alec_stuff_percentile <- alec_percentiles$Percentile[alec_percentiles$Label == "Stuff+"]
  if (!is.finite(alec_metrics$`Stuff+`) || alec_metrics$`Stuff+` < 105 || alec_metrics$`Stuff+` > 112 ||
      length(alec_stuff_percentile) != 1L || !is.finite(alec_stuff_percentile) || alec_stuff_percentile < 75) {
    stop("HomeBASE Alec Beversdorf Stuff+ integrity regression failed.", call. = FALSE)
  }
}

cat("BASE startup and HomeBASE Stuff+ integrity smoke tests passed.\n")
