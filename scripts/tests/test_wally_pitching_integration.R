#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
source("R/data/source_contract.R", local = FALSE)
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
  c("S25", "F25", "SQ26", "S26", "BP", "F26", "PS27", "S27")
)) {
  fail("Pitching workspace is not prepared for optional 2026 Fall, 2027 Scrimmages, and 2027 Season sources.")
}

# Folder rows still deduplicate repeated pitch IDs while retaining bullpen rows.
supplement <- fixture[1:2, , drop = FALSE]
supplement$source_file <- "Bullpens - cleaned.csv"
supplement$DataSource <- "Texas State internal — Bullpens - cleaned.csv"
supplement$PitchUID[[2]] <- "fixture-unique-bullpen-pitch"
walk_on_row <- fixture[1, , drop = FALSE]
walk_on_row$Pitcher <- "Pitcher, Walk On"
walk_on_row$PitchUID <- "fixture-walk-on-pitcher"

prepared <- base_prepare_team_pitching_data(dplyr::bind_rows(fixture, supplement, walk_on_row))
if (sum(prepared$PitchUID == fixture$PitchUID[[1]], na.rm = TRUE) != 1L) {
  fail("Duplicate folder pitch was not removed.")
}
if (!any(prepared$PitchUID == "fixture-unique-bullpen-pitch", na.rm = TRUE)) {
  fail("Unique bullpen supplement row was lost.")
}
if (any(base_is_walk_on_player_name(prepared$Pitcher)) ||
    !all(base_is_walk_on_player_name(c("Walk On Player", "Player, Walk On"))) ||
    base_is_walk_on_player_name("Walker On Player")) {
  fail("Walk On pitchers are not excluded at the shared data boundary.")
}

# Opponent identities must never remove a pitcher's PA, even without IDs.
unknown_batters <- fixture[1:3, , drop = FALSE]
unknown_batters$Batter <- c("Walk On Hitter", NA_character_, "")
unknown_batters$BatterId <- NA_character_
unknown_batters$PitchUID <- NA_character_
unknown_batters$PlayID <- NA_character_
unknown_batters$PAofInning <- 1:3
unknown_batters$PitchofPA <- 1L
unknown_batters$Catcher <- "Walk On Catcher"
kept_unknown <- base_prepare_team_pitching_data(unknown_batters)
stopifnot(nrow(kept_unknown) == 3L)

workspace_rows <- dplyr::bind_rows(
  prepared,
  dplyr::mutate(prepared[1, , drop = FALSE], Pitcher = "Walk On Workspace", PitchUID = "workspace-walk-on-pitcher")
)
workspace <- base_wally_pitching_environment(workspace_rows)
if (!is.function(workspace$server)) fail("Embedded Pitching server is unavailable.")
if (any(workspace$is_walk_on_player_name(workspace$df$Pitcher)) ||
    any(workspace$is_walk_on_player_name(workspace$txst_df$Pitcher))) {
  fail("Standalone Pitching data still exposes a Walk On pitcher.")
}
report_logo <- png::readPNG(workspace$BASE_PITCHING_REPORT_LOGO_PATH)
if (length(dim(report_logo)) != 3L || dim(report_logo)[3] != 4L || !any(report_logo[, , 4] == 0)) {
  fail("Pitching leaderboard is not configured with a transparent TS report logo.")
}
expected_d1_pitching <- base_d1_aar_benchmarks()$pitching
actual_d1_pitching <- unlist(workspace$D1_REF[names(expected_d1_pitching)])
if (!isTRUE(all.equal(actual_d1_pitching, expected_d1_pitching, tolerance = 1e-12))) {
  fail("Pitching AAR is not using the parquet-derived D1 process benchmarks.")
}
label_metrics <- c(`K%`="performance_k_pct",`BB%`="performance_bb_pct",
  `Barrel%`="performance_barrel_pct",`Whiff%`="performance_whiff_pct",
  `CSW%`="performance_csw_pct",`Avg Velo`="heater_velocity",`E&A%`="performance_ea_pct",
  `Strike%`="performance_strike_pct",`Zone%`="performance_zone_pct",
  `Pre2k Zone%`="performance_pre2k_zone_pct",`FPS%`="performance_fps_pct",`Put Away%`="performance_put_away_pct")
reference <- workspace$load_d1_pitch_metric_reference()
for (label in names(label_metrics)) {
  rows <- reference[reference$scope=="overall" & reference$metric==label_metrics[[label]] &
    is.finite(reference$value) & reference$sample_n>0, ]
  average <- weighted.mean(rows$value,rows$sample_n)
  expected <- if(label=="Avg Velo") sprintf("Avg %.1f mph",average) else sprintf("Avg %.1f%%",100*average)
  stopifnot(identical(workspace$pitching_leaderboard_benchmark_label(label),expected))
  if(label %in% names(workspace$D1_PCT_AVG)) stopifnot(abs(workspace$D1_PCT_AVG[[label]]-average)<1e-12)
}
empty_evla <- workspace$resolve_ev_la_strict(tibble::tibble(Angle = numeric()))
if (length(empty_evla$ev) || length(empty_evla$la)) {
  fail("Empty EV/launch-angle input did not return empty vectors safely.")
}
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
leaderboard_reference_metrics <- c(
  `K%` = "performance_k_pct",
  `BB%` = "performance_bb_pct",
  `Barrel%` = "performance_barrel_pct",
  `Whiff%` = "performance_whiff_pct",
  `CSW%` = "performance_csw_pct",
  `Strike%` = "performance_strike_pct",
  `Zone%` = "performance_zone_pct",
  `Pre2k Zone%` = "performance_pre2k_zone_pct",
  `FPS%` = "performance_fps_pct",
  `E&A%` = "performance_ea_pct",
  `Put Away%` = "performance_put_away_pct"
)
leaderboard_reference <- workspace$load_d1_pitch_metric_reference()
for (column in names(leaderboard_reference_metrics)) {
  metric <- leaderboard_reference_metrics[[column]]
  pool <- leaderboard_reference$value[
    leaderboard_reference$scope == "overall" &
      leaderboard_reference$metric == metric &
      is.finite(leaderboard_reference$value)
  ]
  if (!length(pool)) fail("Pitching leaderboard percentile reference is missing: ", metric)
  midpoint_percentile <- workspace$pitching_cell_percentiles(stats::median(pool), column)
  if (!is.finite(midpoint_percentile) || abs(midpoint_percentile - 50) > 1) {
    fail("Pitching leaderboard is not using the empirical D1 percentile pool for ", column)
  }
}
for (column in c('Max Velocity','Avg Velo')) {
  metric <- if(column=='Max Velocity') 'max_velocity' else 'heater_velocity'
  pool <- leaderboard_reference$value[leaderboard_reference$scope=='overall' & leaderboard_reference$metric==metric]
  stopifnot(length(pool)>100,
    workspace$pitching_cell_percentiles(quantile(pool,.9),column)>80,
    workspace$pitching_cell_percentiles(quantile(pool,.1),column)<20)
}
bb_pool <- leaderboard_reference$value[
  leaderboard_reference$scope == "overall" &
    leaderboard_reference$metric == "performance_bb_pct" &
    is.finite(leaderboard_reference$value)
]
if (workspace$pitching_cell_percentiles(stats::quantile(bb_pool, 0.25), "BB%") <= 50) {
  fail("Pitching leaderboard did not reverse the lower-is-better BB% percentile direction.")
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
  "Bullpens", "Leaderboard", "Team Trends"
)
team_html <- as.character(htmltools::renderTags(workspace$base_pitching_team_postgame_ui)$html)
stopifnot(grepl("AAR Builder",team_html,fixed=TRUE),grepl("Recent AARs",team_html,fixed=TRUE),
          !grepl('data-value="Team Report"',html,fixed=TRUE))
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Pitching UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}
if (grepl("leader_exclude", html, fixed = TRUE) || grepl("Exclude from PDF", html, fixed = TRUE)) {
  fail("Pitching leaderboard still exposes the removed player exclusion control.")
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
if (!all(vapply(
  c("Pitcher View", "Hitter View", "hitter_view_show_rays", "hitter_view_show_distances", "hitter_view_movement"),
  grepl, logical(1), x = html, fixed = TRUE
))) {
  fail("Pitch Metrics is missing the Pitcher/Hitter sub-tabs or hitter-view controls.")
}
heater_reference <- workspace$load_heater_movement_reference()
if (!nrow(heater_reference) ||
    !all(c("RHP", "LHP") %in% heater_reference$hand) ||
    !all(c("Fastball", "Sinker") %in% heater_reference$pitch_type)) {
  fail("Pitching hitter view cannot load the D1 heater movement reference.")
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
pa_grid_game <- workspace$txst_df %>%
  dplyr::filter(.data$CustomGameID == games[[1]])
pa_grid <- workspace$compose_AAR_pa_grid_plot(pa_grid_game, max_cols = 2, max_rows = 9)
if (!identical(pa_grid$patches$layout$byrow, FALSE)) {
  fail("Pitching AAR plate appearances do not fill top-to-bottom before moving right.")
}
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
    hitter_view_show_rays = TRUE,
    hitter_view_show_distances = TRUE,
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
  hitter_avg <- hitter_view_pitch_averages()
  if (!nrow(hitter_avg) || any(!is.finite(hitter_avg$HB)) || any(!is.finite(hitter_avg$IVB))) {
    fail("Pitching hitter view did not calculate finite mirrored pitch averages.")
  }
  source_avg <- movement_plot_data() %>%
    dplyr::group_by(PitchType_plot) %>%
    dplyr::summarise(HB = mean(HorzBreak, na.rm = TRUE), .groups = "drop")
  mirrored <- hitter_avg %>% dplyr::select(PitchType_plot, HB) %>%
    dplyr::inner_join(source_avg, by = "PitchType_plot", suffix = c("_hitter", "_pitcher"))
  if (!nrow(mirrored) || any(abs(mirrored$HB_hitter + mirrored$HB_pitcher) > 1e-8)) {
    fail("Pitching hitter view does not mirror horizontal break.")
  }
  if (!is.finite(hitter_view_arm_angle())) {
    fail("Pitching hitter view did not resolve the selected pitcher's arm angle.")
  }
  invisible(output$hitter_view_movement)
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
  stopifnot(grepl('season_summary_movement',as.character(workspace$ui),fixed=TRUE))
  invisible(output$season_summary_movement)
  for (display in list(season_summary_split_display(dataFilter()),
                       season_summary_pitch_display(dataFilter()),
                       season_summary_hand_pitch_perf_display(dataFilter(),"R"))) {
    fills <- attr(display$shaded,"cell_fills")
    stopifnot(any(!is.na(fills)))
    pdf_plot <- build_season_summary_pdf_table(display$fmt,"Test",display$shaded)
    # The PDF uses exactly the same per-cell colors as the HTML tables.
    index <- cbind(pdf_plot$data$row,pdf_plot$data$col)
    expected <- fills[index]
    stopifnot(all(pdf_plot$data$fill[!is.na(expected)]==expected[!is.na(expected)]))
  }
  # Exercise the real download: an error-only PDF is only a few KB.
  summary_pdf <- output$season_summary_pdf
  stopifnot(file.exists(summary_pdf),file.info(summary_pdf)$size > 50000)
  con <- file(summary_pdf,"rb")
  stopifnot(identical(readChar(con,5,useBytes=TRUE),"%PDF-"))
  close(con)
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
  # Rolling plots include initial outings instead of discarding four dates.
  stopifnot(isTRUE(all.equal(roll_mean(1:6, 5), c(1, 1.5, 2, 2.5, 3, 4))))
  dates <- tibble::tibble(GameIndex = 1:6, GameLabel = paste0("10/", 1:6, "/26"),
                          wOBA = seq(.2, .7, .1))
  trend <- build_perf_ts_plot_gg(dates, tibble::tibble(), "wOBA", list(), roll = TRUE)
  stopifnot(identical(trend$data$GameIndex, 1:6))

  # Align heaters by their sequence within each outing, not overall pitch number.
  heaters <- tibble::tibble(CustomGameID = c(rep("a",4),rep("b",3)),
    .game_pitch_order = c(1:4,1:3), OutingInning = c(1,1,2,2,1,1,1),
    PitchType = c("Fastball","Slider","Fastball","Sinker","Slider","Fastball","Fastball"),
    RelSpeed = c(90,80,88,86,80,94,92))
  per_pitch <- pitch_decay_velocity_data(list(pitches=heaters), "pitch")
  stopifnot(identical(per_pitch$Velo, c(92,90,86)), identical(per_pitch$Pitches,c(2L,2L,1L)))
  per_game <- pitch_decay_velocity_data(list(pitches=heaters[1:4,]), "pitch")
  stopifnot(identical(per_game$Velo,c(90,88,86)))
  per_inning <- pitch_decay_velocity_data(list(pitches=heaters), "inning")
  stopifnot(identical(per_inning$Velo,c(92,87)))
  blank_bp <- plotly::plotly_build(build_bullpen_accum_movement_plotly(data.frame(), "Test"))
  stopifnot(identical(blank_bp$x$layout$annotations[[1]]$text,"No Bullpen data for this pitcher"))
  outcome <- plotly::plotly_build(make_location_plotly(tibble::tibble(
    PitchType_plot="Fastball", PlateLocSide=0, PlateLocHeight=2.5), "Test"))
  shapes <- outcome$x$layout$shapes
  stopifnot(sum(vapply(shapes,function(s) identical(s$type,"rect"),logical(1)))==1L,
            sum(vapply(shapes,function(s) identical(s$type,"line"),logical(1)))==nrow(workspace$home_plate_segments),
            identical(shapes[[1]]$type,"circle"), identical(shapes[[1]]$fillcolor,"#FF0000"),
            abs(shapes[[1]]$x1-shapes[[1]]$x0-2.90/12)<1e-9,
            outcome$x$layout$yaxis$range[[1]]==0)
  # Unknown batters remain distinct throughout AAR grouping and rendering.
  unknown <- dataFilter()[rep(1L,3L),]
  unknown$Batter <- c("Walk On Hitter",NA_character_,"")
  unknown$BatterId <- NA_character_
  unknown$BatterSide <- NA_character_
  unknown$PAofInning <- 1:3
  unknown$PitchofPA <- 1L
  unknown$PitchCall <- "InPlay"
  unknown$PlayResult <- "Out"
  unknown$PitchUID <- paste0("unknown-",1:3)
  unknown$pitch_uid <- unknown$PitchUID
  unknown <- dplyr::select(unknown,-dplyr::any_of(c("PA_ID","PitchNum")))
  stopifnot(dplyr::n_distinct(workspace$prepare_aar_flags(unknown)$PA_ID)==3L,
            dplyr::n_distinct(workspace$pitching_pa_key(unknown))==3L)
  invisible(ggplot2::ggplotGrob(workspace$compose_AAR_pa_grid_plot(unknown)))
  decay <- pitch_decay_base()
  if (!is.list(decay) || !all(c("pitches", "pa_last") %in% names(decay))) {
    fail("Pitch Decay did not return its preserved Wally payload.")
  }
  if (!nrow(decay$pitches)) fail("Pitch Decay returned no fixture pitches.")
  recent_aars <- aar_recent_reports()
  if (!nrow(recent_aars)) fail("Moved Pitching AAR archive returned no fixture rows.")
  original_rows <- rv$df
  opponent_row <- original_rows[1, , drop = FALSE]
  opponent_row$Pitcher <- "Opponent, TCU"
  opponent_row$PitcherTeam <- "TCU_HFG"
  rv$df <- dplyr::bind_rows(original_rows, opponent_row)
  if ("Opponent, TCU" %in% aar_recent_reports()$Pitcher) {
    fail("Pitching Recent AARs includes an opposing pitcher.")
  }
  rv$df <- opponent_row
  if (nrow(aar_recent_reports())) {
    fail("Pitching Recent AARs falls back to opposing pitchers when no team rows exist.")
  }
  rv$df <- original_rows
  gp <- aar_game_data()
  sp <- aar_season_data()
  aar_date <- as.Date("2026-03-15")
  aar_render_payload <<- list(game = gp, season = sp, date = aar_date)
  invisible(output$pitch_decay_table)
  invisible(output$pitch_decay_velocity)
  session$setInputs(pitch_decay_season_mode="pitch", pitch_decay_single_mode="pitch")
  invisible(output$pitch_decay_velocity)
  invisible(output$pitch_decay_single_velocity)

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
