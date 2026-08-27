#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: generate_goldens.R HittingApp|PitchingApp|DefenseApp OUTPUT_DIR", call. = FALSE)
}

app_name <- args[[1]]
output_dir <- normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE)[[1]])
source(file.path(dirname(normalizePath(script_file, mustWork = TRUE)), "load_wally_app.R"), local = FALSE)

loaded <- load_wally_baseline_app(app_name)
on.exit(loaded$cleanup(), add = TRUE)
old_wd <- setwd(loaded$sandbox)
on.exit(setwd(old_wd), add = TRUE)
env <- loaded$env

write_table <- function(value, filename) {
  value <- as.data.frame(value, check.names = FALSE)
  utils::write.csv(
    value,
    file.path(output_dir, filename),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

write_summary <- function(value) {
  jsonlite::write_json(
    value,
    file.path(output_dir, "summary.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    digits = 15
  )
}

save_plot_pair <- function(plot, stem, width = 8, height = 6) {
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".png")),
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = 144,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".pdf")),
    plot = plot,
    device = grDevices::pdf,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

if (identical(app_name, "HittingApp")) {
  pitches <- env$filter_team(env$df, "BatterTeam")
  overall <- env$summarize_overall(pitches)
  by_pitch <- env$summarize_by_pitchtype(pitches)
  pitch_usage <- env$summarize_pitch_usage(pitches)
  swing_metrics <- env$calc_hitter_metrics(pitches)

  write_table(overall, "overall.csv")
  write_table(by_pitch, "by-pitch-type.csv")
  write_table(pitch_usage, "pitch-usage.csv")
  write_table(
    data.frame(metric = names(swing_metrics), value = unlist(swing_metrics), check.names = FALSE),
    "swing-decisions.csv"
  )
  save_plot_pair(env$aar_strike_zone_plot(pitches), "strike-zone", width = 7, height = 7)

  write_summary(list(
    app = app_name,
    fixture_rows_loaded = nrow(env$df),
    team_rows = nrow(pitches),
    plate_appearances = overall$PA[[1]],
    observed_case_insensitive_duplicate_load = nrow(env$df) == 736L,
    golden_tables = c("overall.csv", "by-pitch-type.csv", "pitch-usage.csv", "swing-decisions.csv"),
    golden_visuals = c("strike-zone.png", "strike-zone.pdf")
  ))
} else if (identical(app_name, "PitchingApp")) {
  pitches <- env$filter_team_or_portal(env$df_nonbp, "PitcherTeam")
  pitches <- env$ensure_pa(pitches)
  pitches <- env$prepare_aar_flags(pitches)
  statline <- env$count_statline(pitches)
  process <- env$build_process_table(pitches, pitches, "Fixture Season")
  pitch_performance <- env$build_pitchtype_perf_table(pitches, pitches)
  stuff_rows <- env$compute_called_stuff(pitches)
  stuff_summary <- stuff_rows |>
    dplyr::filter(is.finite(.data$stuff_plus)) |>
    dplyr::group_by(.data$Pitcher, .data$PitchType_stuff) |>
    dplyr::summarise(
      pitches = dplyr::n(),
      mean_stuff_plus = mean(.data$stuff_plus),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$Pitcher, .data$PitchType_stuff)

  write_table(
    data.frame(
      metric = c("strikeouts", "walks", "hits", "plate_appearances", "walks_from_korbb", "walks_from_play_result"),
      value = c(statline$k_n, statline$bb_n, statline$h_n, statline$pa_n,
                statline$dbg$bb_from_korbb, statline$dbg$bb_from_pr),
      check.names = FALSE
    ),
    "statline.csv"
  )
  write_table(process, "process-metrics.csv")
  write_table(pitch_performance, "pitch-type-performance.csv")
  write_table(stuff_summary, "called-stuff.csv")
  save_plot_pair(env$strike_zone_plot(pitches, title = "Fixture Pitch Locations"), "strike-zone", width = 7, height = 7)
  save_plot_pair(env$movement_plot(pitches), "pitch-movement", width = 7.5, height = 6.5)

  write_summary(list(
    app = app_name,
    fixture_rows_loaded = nrow(env$df),
    non_bullpen_rows = nrow(pitches),
    plate_appearances = statline$pa_n,
    observed_walk_count = statline$bb_n,
    expected_fixture_korbb_walk_rows = sum(trimws(as.character(pitches$KorBB)) == "Walk", na.rm = TRUE),
    observed_walk_encoding_mismatch = statline$bb_n == 0 && any(trimws(as.character(pitches$KorBB)) == "Walk", na.rm = TRUE),
    golden_tables = c("statline.csv", "process-metrics.csv", "pitch-type-performance.csv", "called-stuff.csv"),
    golden_visuals = c("strike-zone.png", "strike-zone.pdf", "pitch-movement.png", "pitch-movement.pdf")
  ))
} else if (identical(app_name, "DefenseApp")) {
  defense_summary <- env$summarize_defense(env$defense_df)
  leaderboard <- env$summarize_leaderboard(env$defense_df)
  catcher_rows <- env$prepare_catcher_receiving_rows(env$catching_df)
  catcher_metrics <- env$summarise_catcher_percentile_metrics(catcher_rows)
  catcher_percentiles <- env$add_d1_percentiles(catcher_metrics)

  write_table(defense_summary, "defense-summary.csv")
  write_table(leaderboard, "leaderboard.csv")
  write_table(catcher_percentiles, "catcher-framing.csv")

  shortstop_rows <- env$defense_df[env$defense_df$position == "SS", , drop = FALSE]
  save_plot_pair(env$opportunity_spray_plot(env$defense_df, pos = "SS", title = "Fixture SS Opportunities"),
                 "shortstop-opportunities", width = 8, height = 7)
  save_plot_pair(env$range_360_plot(shortstop_rows, title = "Fixture SS 360 Degree Range"),
                 "shortstop-range", width = 8, height = 7)
  stolen <- env$catcher_framing_subset(catcher_rows, "ball_to_strike")
  save_plot_pair(env$catcher_framing_zone_plot(stolen, title = "Fixture Strikes Stolen"),
                 "catcher-framing", width = 7, height = 7)

  write_summary(list(
    app = app_name,
    source_defense_rows = nrow(env$defense_raw),
    exploded_defense_rows = nrow(env$defense_df),
    catcher_rows = nrow(catcher_rows),
    catcher_metric_count = nrow(catcher_percentiles),
    golden_tables = c("defense-summary.csv", "leaderboard.csv", "catcher-framing.csv"),
    golden_visuals = c(
      "shortstop-opportunities.png", "shortstop-opportunities.pdf",
      "shortstop-range.png", "shortstop-range.pdf",
      "catcher-framing.png", "catcher-framing.pdf"
    )
  ))
} else {
  stop("Unknown Wally app: ", app_name, call. = FALSE)
}

cat(app_name, "goldens written to", output_dir, "\n")
