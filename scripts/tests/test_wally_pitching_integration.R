#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_pitching_workspace.R", local = FALSE)

# The integration fixture verifies that the Wally Stuff+ path delegates to the
# app's one BrewStuff scorer without requiring the production LightGBM asset.
model <- structure(list(name = "brewstuff-test-model"), class = "brewstuff_test_model")
getBrewStuff <- function(game, final_model, bullpen = FALSE, ...) {
  stopifnot(inherits(final_model, "brewstuff_test_model"), identical(bullpen, FALSE))
  game$rv <- -0.01
  game$Stuff <- 123
  game
}

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_path <- file.path(
  "tests", "fixtures", "wallyapps", "pitching", "data",
  "2026 Season - cleaned.csv"
)
fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)
fixture$PitcherTeam <- TEAM_CONFIG$data_code
fixture$source_file <- "2026 Season - cleaned.csv"
fixture$row_in_file <- seq_len(nrow(fixture))
fixture$SeasonGroup <- "S26"
fixture$DataSource <- "Texas State internal — 2026 Season - cleaned.csv"
fixture$.base_source_priority <- 2L

# The integrated workspace must use the five CSVs in PitchingApp/data.
expected_files <- c(
  "2025 Season -cleaned.csv", "2025 Fall -cleaned.csv",
  "2026 Squads - cleaned.csv", "2026 Season - cleaned.csv",
  "Bullpens - cleaned.csv"
)
if (!identical(basename(base_pitching_supplement_paths()), expected_files)) {
  fail("Pitching workspace does not resolve the five PitchingApp folder CSVs.")
}
if (!identical(
  names(base_pitching_supplement_candidates()),
  c("S25", "F25", "SQ26", "S26", "BP", "F26", "S27")
)) {
  fail("Pitching workspace is not prepared for optional 2026 Fall and 2027 Season sources.")
}

# Folder rows still deduplicate repeated pitch IDs while retaining bullpen rows.
supplement <- fixture[1:2, , drop = FALSE]
supplement$source_file <- "Bullpens - cleaned.csv"
supplement$DataSource <- "Texas State internal — Bullpens - cleaned.csv"
supplement$PitchUID[[2]] <- "fixture-unique-bullpen-pitch"

prepared <- base_prepare_team_pitching_data(dplyr::bind_rows(fixture, supplement))
if (sum(prepared$PitchUID == fixture$PitchUID[[1]], na.rm = TRUE) != 1L) {
  fail("Duplicate folder pitch was not removed.")
}
if (!any(prepared$PitchUID == "fixture-unique-bullpen-pitch", na.rm = TRUE)) {
  fail("Unique bullpen supplement row was lost.")
}

workspace <- base_wally_pitching_environment(prepared)
if (!is.function(workspace$server)) fail("Embedded Pitching server is unavailable.")
brew_scored <- workspace$compute_brew_stuff(prepared[1:3, , drop = FALSE])
if (!identical(as.numeric(brew_scored$stuff_plus), rep(123, 3))) {
  fail("Pitching Stuff+ does not delegate to BrewStuff.")
}
if (exists("compute_called_stuff", envir = workspace, inherits = FALSE)) {
  fail("Legacy called-stuff scoring remains active in the Pitching workspace.")
}
if (!all(c(F26 = "2026 Fall", S27 = "2027 Season") %in% stats::setNames(names(workspace$SEASON_CHOICES), workspace$SEASON_CHOICES))) {
  fail("Pitching season controls do not expose 2026 Fall and 2027 Season.")
}
if (!grepl("rgba\\(227,52,52", workspace$.severity_fill(1, "coach"))) {
  fail("Coach-facing Pitching shading is not red for favorable values.")
}
if (!grepl("rgba\\(46,125,50", workspace$.severity_fill(1, "player"))) {
  fail("Player-facing Pitching shading is not green for favorable values.")
}
aar_fill <- workspace$aar_severity_fill(.40, .30)
if (!grepl("^#[0-9A-Fa-f]{6}$", aar_fill) || grepl("rgba", aar_fill, fixed = TRUE)) {
  fail("Pitching AAR shading is not using PDF-safe hexadecimal colors.")
}
if (is.null(workspace$.get_batter_silhouette_grob("R"))) {
  fail("Embedded pitching AAR cannot resolve the batter silhouette asset.")
}

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Pitching UI contains a nested full-page body element.")
}
if (!grepl("base-pitching-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Pitching UI is missing its scoped BASE layout wrapper.")
}
if (!all(vapply(
  c(
    "xrv_season_stuff_hero", "xrv_season_stuff_by_pitch",
    "xrv_game_stuff_hero", "xrv_game_stuff_by_pitch"
  ),
  grepl, logical(1), x = html, fixed = TRUE
))) {
  fail("Stuff+ is missing its metric-first cards and pitch-type visual layout.")
}
if (grepl("xrv_season_stats", html, fixed = TRUE) ||
    grepl("xrv_season_usage_lhh", html, fixed = TRUE) ||
    grepl("xrv_season_usage_rhh", html, fixed = TRUE)) {
  fail("Stuff+ still renders duplicated general stats or handedness usage panels.")
}
style_html <- htmltools::renderTags(base_pitching_embedded_head())$head
if (!grepl("grid-template-columns: repeat(2, minmax(0, 1fr))", style_html, fixed = TRUE) ||
    !grepl("shiny-input-checkboxgroup[id$='season_groups']", style_html, fixed = TRUE)) {
  fail("Pitching sidebar season controls are missing the aligned checkbox grid.")
}
expected_tabs <- c(
  "Performance", "Pitch Metrics", "Season Summary", "Pitch Decay",
  "Locations", "Whiffs / Chases / Called Strikes / Barrels", "Stuff+",
  "Bullpens", "Leaderboard", "Team Report", "Team Trends"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Pitching UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}
pitching_source <- paste(readLines("WallyApps/PitchingApp/PitchingApp.R", warn = FALSE), collapse = "\n")
if (grepl("xrv_leader_min_pitches", pitching_source, fixed = TRUE) ||
    grepl("Minimum pitches to qualify", pitching_source, fixed = TRUE)) {
  fail("Stuff+ still applies or exposes a minimum-pitch qualification filter.")
}
if (!grepl("performance_table", html, fixed = TRUE) ||
    !grepl("base-pitching-performance-insights", html, fixed = TRUE)) {
  fail("Pitching Performance is missing its unified table and insights layout.")
}
if (any(vapply(
  c("performance_traditional_table", "performance_process_table", "performance_modern_table", "performance_results_table"),
  grepl, logical(1), x = html, fixed = TRUE
))) {
  fail("Pitching Performance still renders the disjointed four-table layout.")
}
if (grepl(">AAR<", html, fixed = TRUE)) {
  fail("Pitching AAR is still present in the Pitching workspace.")
}
postgame_html <- paste(as.character(workspace$base_pitching_postgame_ui), collapse = "")
if (!grepl("aar_dl", postgame_html, fixed = TRUE) ||
    !grepl("pitch_aar_season_groups", postgame_html, fixed = TRUE)) {
  fail("Pitching AAR was not exposed to the Postgame Reports workspace.")
}

pitcher <- as.character(workspace$txst_pitchers[[1]])
games <- unname(workspace$games_txst)
aar_render_payload <- NULL
results_leaderboard_pdf <- Sys.getenv("BASE_PITCHING_RESULTS_LEADERBOARD_QA", unset = "")
if (!nzchar(results_leaderboard_pdf)) {
  results_leaderboard_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(results_leaderboard_pdf), add = TRUE)
}
process_leaderboard_pdf <- Sys.getenv("BASE_PITCHING_PROCESS_LEADERBOARD_QA", unset = "")
if (!nzchar(process_leaderboard_pdf)) {
  process_leaderboard_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(process_leaderboard_pdf), add = TRUE)
}
shiny::testServer(workspace$server, {
  session$setInputs(
    PitcherInput = pitcher,
    season_groups = "S26",
    Bullpens = FALSE,
    GameInput = games,
    BatterHand = c("L", "R"),
    perf_split = "hand",
    leader_seasons = "S26",
    leader_pitch_types = "All",
    leader_hand = c("L", "R"),
    pitch_aar_season_groups = "S26",
    aar_pitcher = pitcher,
    aar_game = games[[1]]
  )
  session$flushReact()
  session$setInputs(season_groups = "F26", GameInput = games)
  session$flushReact()
  if (length(season_selected_game_ids()) || nrow(dataFilter())) {
    fail("Pitching expanded an unavailable selected season back to the pitcher's other games.")
  }
  session$setInputs(season_groups = "S26", GameInput = games)
  session$flushReact()
  stuff_table <- xrv_pitch_table_df(xrv_season_data(), xrv_scale_params())
  if (!identical(
    names(stuff_table),
    c("PitchType", "Pitches", "Usage %", "Stuff+", "Avg Velocity (Max)", "iVB", "HB", "Spin")
  )) {
    fail("Stuff+ pitch table is not using the focused pitch-shape layout.")
  }
  invisible(output$xrv_season_stuff_hero)
  invisible(output$xrv_season_stuff_by_pitch)
  perf <- performance_table_data()
  perf_raw <- attr(perf, "raw_df")
  if (!identical(as.character(perf_raw$Batter), c("TOTAL", "vLHH", "vRHH"))) {
    fail("Pitching handedness Performance table is not Total, left, right.")
  }
  invisible(output$performance_table)
  splits <- season_summary_split_summary(dataFilter())
  if (!identical(splits$Split, c("Total", "v LHH", "v RHH"))) {
    fail("Season Summary handedness table is not Total, left, right.")
  }
  staff <- build_self_staff_table(dataFilter())
  if (!identical(as.character(staff$Split), c("Total", "vLHH", "vRHH"))) {
    fail("Staff handedness table is not Total, left, right.")
  }
  for (column in c("wOBAcon", "2k Zone%", "2K Zone%")) {
    values <- if (column == "wOBAcon") c("0.000", "2.000", "NA") else c("100%", "0%", "NA")
    pct <- workspace$pitching_cell_percentiles(values, column)
    if (!identical(unname(pct), c(99, 1, NA_real_))) fail("Incorrect D1 percentile direction for ", column)
    cells <- data.frame(values, check.names = FALSE)
    names(cells) <- column
    shaded <- workspace$shade_columns_txst(cells, cols = column)[[column]]
    if (!grepl("rgba(227,52,52", shaded[1], fixed = TRUE) ||
        !grepl("rgba(93,126,188", shaded[2], fixed = TRUE) ||
        grepl("background-color", shaded[3], fixed = TRUE)) {
      fail("Pitching percentile cells do not shade favorable, unfavorable, and missing values correctly.")
    }
  }
  decay <- pitch_decay_base()
  if (!is.list(decay) || !all(c("pitches", "pa_last") %in% names(decay))) {
    fail("Pitch Decay did not return its preserved Wally payload.")
  }
  if (!nrow(decay$pitches)) fail("Pitch Decay returned no fixture pitches.")
  recent_aars <- aar_recent_reports()
  if (!nrow(recent_aars)) fail("Moved Pitching AAR archive returned no fixture rows.")
  gp <- aar_game_data()
  sp <- aar_season_data()
  aar_date <- as.Date("2026-03-15")
  aar_render_payload <<- list(game = gp, season = sp, date = aar_date)
  invisible(output$pitch_decay_table)
  invisible(output$pitch_decay_velocity)

  leaderboard_rows <- leaderboard_summary(leaderboard_data())
  if (!nrow(leaderboard_rows)) fail("Pitching leaderboard returned no fixture rows.")
  render_leaderboard_pdf(results_leaderboard_pdf, "results", leaderboard_rows)
  render_leaderboard_pdf(process_leaderboard_pdf, "process", leaderboard_rows)
  for (pdf_path in c(results_leaderboard_pdf, process_leaderboard_pdf)) {
    if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 4 ||
        !identical(readBin(pdf_path, what = "raw", n = 4L), charToRaw("%PDF"))) {
      fail("Pitching leaderboard download did not produce a PDF: ", basename(pdf_path))
    }
  }
})

aar_pdf <- Sys.getenv("BASE_PITCHING_AAR_QA", unset = "")
if (!nzchar(aar_pdf)) {
  aar_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(aar_pdf), add = TRUE)
}
workspace$render_AAR_pdf(
  game_p = aar_render_payload$game,
  season_p = aar_render_payload$season,
  pitcher_name = pitcher,
  game_date = aar_render_payload$date,
  opponent = "Fixture Opponent",
  outfile = aar_pdf,
  arm_angle_deg = NULL,
  season_col_label = "Season"
)
pitch_type_table <- workspace$build_pitchtype_perf_table(
  aar_render_payload$game,
  aar_render_payload$season
)
if (!identical(
  names(pitch_type_table)[1:6],
  c("PitchType", "Whiff%", "Chase%", "GB%", "BAA", "Avg EV")
)) {
  fail("Pitching AAR pitch-type performance table does not match the final reference format.")
}
season_perf_fixture <- aar_render_payload$game
game_perf_fixture <- season_perf_fixture
season_perf_fixture$PitchCall <- "StrikeSwinging"
game_perf_fixture$PitchCall <- "BallCalled"
season_perf_table <- workspace$build_pitchtype_perf_table(game_perf_fixture, season_perf_fixture)
if (!any(season_perf_table[["Whiff%"]] == "100%", na.rm = TRUE)) {
  fail("Pitching AAR pitch-type performance does not use season averages.")
}
missing_call_fixture <- aar_render_payload$game
missing_call_fixture$PitchCall[[1]] <- NA_character_
if (inherits(try(workspace$build_process_table(missing_call_fixture, missing_call_fixture), silent = TRUE), "try-error") ||
    inherits(try(workspace$build_pitchtype_perf_table(missing_call_fixture, missing_call_fixture), silent = TRUE), "try-error")) {
  fail("Pitching AAR tables fail when TrackMan PitchCall values are missing.")
}
angle_segments <- workspace$arm_angle_segments(aar_render_payload$game)
if (nrow(angle_segments) < 2L || anyDuplicated(angle_segments$PitchType)) {
  fail("Pitching AAR does not draw a separate arm-angle ray for each pitch type.")
}
if (any(angle_segments$y != 0) || any(!is.finite(angle_segments$yend)) || any(angle_segments$yend <= 0)) {
  fail("Pitching AAR arm-angle rays extend outside the positive y axis.")
}
movement_layers <- workspace$movement_plot(aar_render_payload$game)$layers
if (any(vapply(movement_layers, function(layer) inherits(layer$geom, "GeomAbline"), logical(1)))) {
  fail("Pitching AAR still contains the old full-width arm-angle line.")
}
missing_release_segments <- workspace$arm_angle_segments(
  dplyr::select(aar_render_payload$game, -dplyr::any_of(c("RelHeight", "RelSide")))
)
if (!identical(names(missing_release_segments), c("PitchType", "x", "y", "xend", "yend")) ||
    nrow(missing_release_segments) != 0L) {
  fail("Missing release data does not produce a safe empty arm-angle layer.")
}
if (!file.exists(aar_pdf) || file.info(aar_pdf)$size <= 0) {
  fail("Pitching AAR PDF did not render successfully.")
}

cat(
  "Wally Pitching integration passed:", nrow(prepared),
  "folder-backed rows,", length(expected_tabs), "tabs, and Pitch Decay server smoke test.\n"
)
