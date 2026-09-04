#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_defense_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
fixture_dir <- file.path("tests", "fixtures", "wallyapps", "defense", "data")

defense_fixture <- readr::read_csv(
  file.path(fixture_dir, "BobcatsDefense2026.csv"),
  show_col_types = FALSE
)
batted_fixture <- readr::read_csv(
  file.path(fixture_dir, "BobcatsDefenseBattedBalls.csv"),
  show_col_types = FALSE
)
catching_fixture <- readr::read_csv(
  file.path(fixture_dir, "Catchers - 2026 Season-cleaned.csv"),
  show_col_types = FALSE
)
baseline_fixture <- readr::read_csv(
  file.path(fixture_dir, "d1_catcher_framing_metrics.csv"),
  show_col_types = FALSE
)

defense_fixture$source_file <- "BobcatsDefense2026.csv"
defense_fixture$row_in_file <- seq_len(nrow(defense_fixture))
defense_fixture$SeasonGroup <- "S26"
catching_fixture$source_file <- "2026 Season - canonical.parquet"
catching_fixture$row_in_file <- seq_len(nrow(catching_fixture))
catching_fixture$SeasonGroup <- "S26"

# Override only the adapter's input boundary. Wally's original standardization,
# metrics, UI, and server still execute unchanged inside the embedded workspace.
base_prepare_wally_defense_rows <- function() defense_fixture
base_prepare_wally_batted_rows <- function(defense_rows) batted_fixture
base_prepare_wally_catching_rows <- function(startup_rows = NULL) catching_fixture
base_prepare_catcher_framing_baseline <- function() baseline_fixture

workspace <- base_wally_defense_environment(catching_fixture)
if (!is.function(workspace$server)) fail("Embedded Defense server is unavailable.")
if (!nrow(workspace$defense_df)) fail("Defense fixture produced no standardized opportunities.")
if (!nrow(workspace$catching_df)) fail("Catching fixture produced no receiving rows.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Defense UI contains a nested full-page body element.")
}
if (!grepl("base-defense-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Defense UI is missing its scoped BASE layout wrapper.")
}
expected_tabs <- c(
  "Leaderboard", "Opportunities", "OF OAA", "IF OAA", "OAA",
  "Catcher Season"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Defense UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}
if (grepl("Catcher Reports", html, fixed = TRUE)) {
  fail("Catcher AAR is still present in the Defense workspace.")
}
postgame_html <- paste(as.character(workspace$base_catching_postgame_ui), collapse = "")
if (!grepl("def_catcher_framing_pdf", postgame_html, fixed = TRUE)) {
  fail("Catcher AAR was not exposed to the Postgame Reports workspace.")
}
if (!length(workspace$base_catching_postgame_choices)) {
  fail("Moved Catcher AAR rendered without initial catcher choices.")
}
first_postgame_catcher <- unname(workspace$base_catching_postgame_choices[[1]])
if (!grepl(first_postgame_catcher, postgame_html, fixed = TRUE)) {
  fail("Moved Catcher AAR did not render its catcher choices into the selector.")
}
if (!length(workspace$base_catching_postgame_game_choices)) {
  fail("Moved Catcher AAR rendered without initial game choices.")
}

summary_rows <- workspace$summarize_defense(workspace$defense_df)
if (!nrow(summary_rows)) fail("Defense summary returned no fixture rows.")
catcher_rows <- workspace$prepare_catcher_receiving_rows(workspace$catching_df)
if (!nrow(catcher_rows)) fail("Catcher receiving preparation returned no fixture rows.")

# Check both numbered report charts and unnumbered season charts. Explicit
# layer data must render on ggplot2 3.x as well as 4.x, without data-frame `+`.
local({
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  framing_rows <- data.frame(
    plot_x = c(-0.4, 0.6, NA_real_), plot_z = c(2.1, 2.7, 2.3),
    PitchType = rep(workspace$pitch_levels_all[[1]], 3),
    PitchNumSub = c(1L, 2L, 3L)
  )
  for (numbered in c(TRUE, FALSE)) {
    plot <- workspace$catcher_framing_zone_plot(framing_rows, show_pitch_numbers = numbered)
    built <- ggplot2::ggplot_build(plot)
    points <- built$data[[5]]
    if (nrow(points) != 2L ||
        !isTRUE(all.equal(points$x, c(-0.4, 0.6))) ||
        !isTRUE(all.equal(points$y, c(2.1, 2.7)))) {
      fail("Catcher framing chart changed pitch locations or retained invalid coordinates.")
    }
    if (numbered && !identical(as.integer(built$data[[6]]$label), c(1L, 2L))) {
      fail("Catcher framing chart lost pitch-number labels.")
    }
    if (!numbered && length(built$data) != 5L) fail("Season framing chart retained pitch labels.")
    invisible(ggplot2::ggplotGrob(plot))
  }
  for (empty_rows in list(framing_rows[FALSE, ], framing_rows[3, , drop = FALSE])) {
    empty_plot <- workspace$catcher_framing_zone_plot(empty_rows)
    if (length(ggplot2::ggplot_build(empty_plot)$data) != 4L) {
      fail("Empty catcher framing chart should contain only zone and plate outlines.")
    }
  }
})

report_catcher <- catcher_rows$Catcher[[1]]
report_game <- catcher_rows$CustomGameID[[1]]
report_rows <- catcher_rows %>%
  dplyr::filter(.data$Catcher == report_catcher, .data$CustomGameID == report_game)
report_path <- tempfile(fileext = ".pdf")
workspace$write_catcher_receiving_pdf(
  report_rows,
  workspace$name_display(report_catcher),
  report_path
)
if (!file.exists(report_path) || file.info(report_path)$size <= 0) {
  fail("Moved Catcher AAR did not generate a PDF.")
}
unlink(report_path)

shiny::testServer(workspace$server, {
  session$setInputs(def_season_groups = "S26")
  session$flushReact()
  if (!nrow(filtered_data())) fail("Defense season filter returned no fixture rows.")
  catcher_choices <- catchers_all()
  if (!length(catcher_choices)) fail("Catcher selector returned no fixture choices.")
  catcher <- unname(catcher_choices[[1]])
  session$setInputs(def_AARCatcher = catcher)
  session$flushReact()
  catcher_games <- games_for_catcher()
  if (!nrow(catcher_games)) fail("Moved Catcher AAR returned no game choices.")
  session$setInputs(def_AARCatchGame = catcher_games$gid[[1]])
  session$flushReact()
  if (!nrow(aar_catch_data())) fail("Moved Catcher AAR returned no fixture rows.")
  invisible(output$leaderboard_overall_table)
})

cat(
  "Wally Defense integration passed:", nrow(workspace$defense_df),
  "standardized opportunities,", nrow(catcher_rows), "catcher rows,",
  length(expected_tabs), "tabs, and server smoke tests.\n"
)
