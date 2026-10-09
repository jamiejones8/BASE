#!/usr/bin/env Rscript

app <- source(
  "app.R",
  local = new.env(parent = globalenv())
)$value

if (!inherits(app, "shiny.appobj")) {
  stop("app.R did not produce a Shiny application object.", call. = FALSE)
}

data_processing_html <- paste(as.character(data_processing_workspace_ui()), collapse = "")
required_data_processing_controls <- c(
  unlist(lapply(
    c("dp_f26", "dp_ps27", "dp_s27", "dp_bp", "dp_positioning_f26",
      "dp_positioning_scrimmages", "dp_positioning_s27"),
    function(prefix) paste0(prefix, c("_file", "_append", "_status"))
  ), use.names = FALSE),
  "dp_pulse_events_replace", "dp_pulse_workload_replace", "dp_arm_care_replace",
  "Replace Arm Care and PULSE exports",
  "dp_csv_source", "dp_csv_editor", "dp_csv_save",
  "dp_scouting_file", "dp_scouting_save", "dp_scouting_status"
)
missing_data_processing_controls <- required_data_processing_controls[
  !vapply(required_data_processing_controls, grepl, logical(1),
          x = data_processing_html, fixed = TRUE)
]
if (length(missing_data_processing_controls)) {
  stop("Data Processing is missing required workflows: ",
       paste(missing_data_processing_controls, collapse = ", "), call. = FALSE)
}

duplicate_percentiles <- tibble::tibble(
  Stat = c("Stuff", "Stuff", "Stuff"),
  TaggedPitchType = c("Fastball", "Fastball", "Fastball"),
  Value = c(90, 90, 100),
  Percentile = c(20, 40, 80)
)
percentile_warnings <- character(0)
duplicate_percentile_result <- withCallingHandlers(
  get_percentile(90, "Stuff", "Fastball", duplicate_percentiles),
  warning = function(w) {
    percentile_warnings <<- c(percentile_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
if (length(percentile_warnings) || !isTRUE(all.equal(duplicate_percentile_result, 30))) {
  stop("Duplicate percentile reference values are not collapsed safely.", call. = FALSE)
}
if (!is.na(base_finite_max(c(NA_real_, NaN))) || base_finite_max(c(NA, 91.2, 93.4)) != 93.4) {
  stop("Finite maximum helper does not handle all-missing velocity groups.", call. = FALSE)
}
if (base_can_density_2d(data.frame(x = rep(1, 10), y = seq_len(10)), "x", "y") ||
    !base_can_density_2d(data.frame(x = c(1, 2, 1, 2), y = c(1, 1, 2, 2)), "x", "y")) {
  stop("Density eligibility does not reject degenerate location samples.", call. = FALSE)
}

# Regression check for the HomeBASE percentile bug: this player was previously
# sent through the scouting scorer and incorrectly displayed at the 1st
# percentile. The original CAPS BrewStuff path resolves the same tracked sample
# to roughly 109 Stuff+ / 81st percentile.
alec_rows <- homebase_load_history_rows("pitcher", "Alec Beversdorf", refresh = TRUE)
brewstuff_path <- TEAM_CONFIG$data$brewstuff_model_file
brewstuff_available <- file.exists(brewstuff_path) && !base_is_lfs_pointer(brewstuff_path)
if (nrow(alec_rows) && brewstuff_available) {
  alec_scored <- homebase_score_pitcher_rows(alec_rows)
  alec_metrics <- homebase_pitcher_metrics(alec_scored)
  alec_percentiles <- homebase_percentile_rows(alec_scored, "pitcher")
  alec_stuff_percentile <- alec_percentiles$Percentile[alec_percentiles$Label == "Stuff+"]
  if (!is.finite(alec_metrics$`Stuff+`) || alec_metrics$`Stuff+` < 105 || alec_metrics$`Stuff+` > 112 ||
      length(alec_stuff_percentile) != 1L || !is.finite(alec_stuff_percentile) || alec_stuff_percentile < 75) {
    stop("HomeBASE Alec Beversdorf Stuff+ integrity regression failed.", call. = FALSE)
  }
}

if (brewstuff_available) {
  gholston_rows <- tibble::tibble(
    Pitcher = "Jacob Gholston",
    PitcherTeam = "TEX_BOB",
    TaggedPitchType = "Fastball",
    PitcherThrows = "R",
    BatterSide = "L",
    RelSpeed = 91,
    InducedVertBreak = 15,
    HorzBreak = 10,
    SpinRate = 2200,
    RelHeight = 6.1,
    RelSide = 1.8,
    Extension = 5.8,
    PitchCall = "StrikeCalled",
    PlayResult = "Undefined"
  )
  gholston_scored <- getBrewStuff(gholston_rows, model, bullpen = FALSE)
  if (nrow(gholston_scored) != 1L || !is.finite(gholston_scored$height[[1]]) ||
      !is.finite(gholston_scored$armangle[[1]]) || !is.finite(gholston_scored$Stuff[[1]])) {
    stop("Stuff+ did not score the Pitching page's First Last name and L/R handedness values.", call. = FALSE)
  }
}

if (!brewstuff_available) {
  cat("HomeBASE Stuff+ regression deferred until the runtime model is mounted.\n")
}

single_pitch_grade <- pitcher_grade_detail(tibble::tibble(
  PitchType = "Fastball",
  StuffPlus = 105,
  LocationPlus = 100,
  PitchingPlus = 102
))
if (!"Fastball" %in% single_pitch_grade$Category) {
  stop("Scouting Stuff+ still requires a minimum pitch-type sample.", call. = FALSE)
}
cat("BASE startup smoke test passed.\n")
