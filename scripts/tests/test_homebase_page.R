#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (!is.null(x)) x else y
base_project_path <- function(...) normalizePath(file.path(getwd(), ...), winslash = "/", mustWork = FALSE)
base_env_path <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) value <- default
  if (startsWith(value, "/")) value else base_project_path(value)
}
TEAM_CONFIG <- list(
  data_pattern = "TEX_BOB|Texas State|TXST",
  abbreviation = "TXST",
  season_label = "2026 College Season"
)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tibble)
  library(readr)
})

source("R/pages/homebase_page.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

if (!identical(homebase_name_key("Carson, Tanner"), homebase_name_key("Tanner Carson"))) {
  fail("HomeBASE name normalization did not match Last, First to First Last.")
}
if (!identical(homebase_format_game_date(as.Date("2026-05-12")), "May 12")) {
  fail("HomeBASE game-date formatting did not render May 12 correctly.")
}
if (!identical(homebase_format_innings(17), "5.2")) {
  fail("HomeBASE innings formatting did not convert outs to baseball notation.")
}
team_palette <- function(team_code) list(
  primary = "#003087", secondary = "#F2A900",
  logo_url = paste0("https://logos.example/", team_code, ".svg")
)
if (!identical(homebase_team_logo_url("BAY_BEA"), "https://logos.example/BAY_BEA.svg") ||
    !identical(homebase_team_monogram("Baylor Bears"), "BB")) {
  fail("HomeBASE did not resolve an opponent team logo and fallback monogram.")
}
team_theme <- homebase_team_theme("BAY_BEA")
if (!identical(team_theme$primary, "#003087") ||
    !identical(team_theme$secondary, "#F2A900") ||
    !identical(team_theme$on_primary, "#FFFFFF") ||
    !grepl("--hb-team-primary:#003087", homebase_team_theme_css("BAY_BEA"), fixed = TRUE)) {
  fail("HomeBASE did not build an accessible theme from the selected team's stored colors.")
}
rm(team_palette)

returning_hitter <- homebase_load_history_rows("hitter", "Tanner Carson", refresh = TRUE)
returning_pitcher <- homebase_load_history_rows("pitcher", "Jesus Tovar", refresh = TRUE)
if (!nrow(returning_hitter) || !all(returning_hitter$BatterTeam == "TEX_BOB")) {
  fail("HomeBASE did not load Tanner Carson from the 2026 hitting baseline.")
}
if (!nrow(returning_pitcher) || !all(returning_pitcher$PitcherTeam == "TEX_BOB")) {
  fail("HomeBASE did not load Jesus Tovar from the 2026 pitching baseline.")
}

# The college-player directory must use the compact mounted catalogs and
# partitioned row loaders rather than rescanning the full national Parquet.
base_pitcher_catalog <- tibble::tibble(
  PitcherTeam = "OTHER_U", Pitcher = "National, Pitcher",
  Bucket = 1L, PitchCount = 30
)
base_get_hitter_catalog <- function(refresh = FALSE) tibble::tibble(
  BatterTeam = "ANY_CLG", Batter = "National, Hitter", BatterSide = "Right",
  Buckets = "2", PitchCount = 25
)
base_load_hitter_rows <- function(team, hitter) {
  tibble::tibble(BatterTeam = team, Batter = hitter, PitchCall = "InPlay")
}
base_load_pitcher_rows <- function(team, pitcher) {
  tibble::tibble(PitcherTeam = team, Pitcher = pitcher, PitchCall = "StrikeCalled")
}
national_catalog <- homebase_catalog()
if (!any(national_catalog$Name == "Hitter National" & national_catalog$Team == "ANY_CLG") ||
    !identical(attr(national_catalog, "homebase_scope"), "national")) {
  fail("HomeBASE did not build its college-player directory from the mounted compact catalogs.")
}
national_match <- homebase_find_catalog_player("Hitter National", "ANY_CLG")$hitter
national_rows <- homebase_load_national_rows(
  "hitter", national_match$BatterTeam[[1]], national_match$Batter[[1]]
)
if (!nrow(national_rows) || national_rows$BatterTeam[[1]] != "ANY_CLG") {
  fail("HomeBASE did not load a player outside the local opponent history from the partitioned national dataset.")
}
homebase_source <- paste(readLines("R/pages/homebase_page.R", warn = FALSE), collapse = "\n")
homebase_styles <- paste(readLines("www/styles.css", warn = FALSE), collapse = "\n")
if (!grepl('uiOutput("hb_team_theme")', homebase_source, fixed = TRUE) ||
    !grepl("var(--hb-team-primary)", homebase_styles, fixed = TRUE) ||
    !grepl("var(--hb-team-secondary)", homebase_styles, fixed = TRUE)) {
  fail("HomeBASE is not applying the selected player's team theme to the page.")
}
if (grepl("base_scouting_season_source", homebase_source, fixed = TRUE) ||
    grepl("$load_players", homebase_source, fixed = TRUE)) {
  fail("HomeBASE still scans the full scouting Parquet instead of using compact runtime catalogs.")
}

synthetic <- tibble::tibble(
  Date = rep(c("2026-03-01", "2026-03-08"), each = 4),
  GameID = rep(c("g1", "g2"), each = 4),
  Inning = c(1, 1, 3, 3, 2, 2, 5, 5),
  PAofInning = rep(c(1, 1, 2, 2), 2),
  PitchofPA = rep(c(1, 2), 4),
  Batter = "Carson, Tanner",
  BatterTeam = "TEX_BOB",
  BatterSide = "Right",
  Pitcher = "Opponent, Pitcher",
  PitcherTeam = "OPP",
  PitcherThrows = "Right",
  TaggedPitchType = rep(c("Fastball", "Slider"), 4),
  PitchCall = c("BallCalled", "InPlay", "StrikeCalled", "StrikeSwinging", "BallCalled", "BallCalled", "FoulBallNotFieldable", "InPlay"),
  PlayResult = c("", "Single", "", "Strikeout", "", "Walk", "", "HomeRun"),
  KorBB = c("", "", "", "Strikeout", "", "Walk", "", ""),
  RelSpeed = c(92, 93, 84, 85, 91, 92, 83, 84),
  PlateLocSide = c(0, .2, -.3, .1, 1.1, 1.2, -.2, .3),
  PlateLocHeight = c(2.5, 2.7, 2.4, 2.6, 2.2, 2.3, 2.5, 2.8),
  ExitSpeed = c(NA, 96, NA, NA, NA, NA, NA, 104),
  Angle = c(NA, 18, NA, NA, NA, NA, NA, 28),
  RunsScored = c(0, 0, 0, 0, 0, 0, 0, 1)
)

pa <- homebase_pa_rows(synthetic)
if (nrow(pa) != 4L) fail("HomeBASE did not reduce pitch rows to four plate appearances.")

hitter <- homebase_hitter_metrics(synthetic)
if (hitter$PA != 4L || abs(hitter$AVG - (2 / 3)) > 1e-9 || abs(hitter$SLG - (5 / 3)) > 1e-9) {
  fail("HomeBASE hitter overview metrics differ from the synthetic outcomes.")
}
required_hitter_metrics <- c(
  "Games", "Pitches", "PA", "AB", "H", "2B", "3B", "HR", "XBH",
  "AVG", "OBP", "SLG", "OPS", "ISO", "K%", "BB%", "Swing%", "ZoneSwing%",
  "Contact%", "ZoneContact%", "Whiff%", "Chase%", "HardHit%", "Barrel%",
  "Avg EV", "90th EV", "Max EV", "Avg LA", "SweetSpot%", "GB%", "LD%", "FB%", "PU%"
)
if (!all(required_hitter_metrics %in% names(hitter)) || hitter$HR != 1L || hitter$XBH != 1L) {
  fail("HomeBASE hitter stat board is missing detailed performance metrics.")
}
hitter_board_html <- paste(as.character(homebase_hitter_stat_board(hitter)), collapse = "")
if (!all(vapply(
  c("hb-hitter-stat-board", "is-workload", "is-production", "is-approach", "is-impact", "is-batted-ball"),
  grepl, logical(1), x = hitter_board_html, fixed = TRUE
))) {
  fail("HomeBASE hitter statistics are not organized into the detailed visual panels.")
}
if (!grepl(".hb-percentile-grid.is-two-way", homebase_styles, fixed = TRUE) ||
    !grepl('else "is-two-way"', homebase_source, fixed = TRUE)) {
  fail("HomeBASE does not give two-way percentile cards a clean full-width rail layout.")
}

hitter_games <- homebase_recent_games(synthetic, "hitter")
if (nrow(hitter_games) != 2L || hitter_games$.hb_game[[1]] != "2026-03-08::g2" || hitter_games$HR[[1]] != 1L) {
  fail("HomeBASE hitter recent-game ordering or totals are incorrect.")
}

pitcher_games <- homebase_recent_games(synthetic, "pitcher")
if (nrow(pitcher_games) != 2L || pitcher_games$Pitches[[1]] != 4L || pitcher_games$K[[2]] != 1L) {
  fail("HomeBASE pitcher recent-game ordering or totals are incorrect.")
}

hitter_percentiles <- homebase_percentile_rows(synthetic, "hitter")
pitcher_percentiles <- homebase_percentile_rows(synthetic, "pitcher")
if (nrow(hitter_percentiles) != 6L || nrow(pitcher_percentiles) != 22L) {
  fail("HomeBASE did not build the six-row hitter and expanded pitcher percentile profiles.")
}
if (!any(is.finite(hitter_percentiles$Percentile))) {
  fail("HomeBASE hitter percentile profile did not resolve against its benchmarks.")
}
if (any(is.finite(pitcher_percentiles$Percentile))) {
  fail("HomeBASE displayed pitcher percentiles for an ineligible four-PA sample.")
}
if (!identical(hitter_percentiles$Label, c("K%", "BB%", "SLG", "Contact%", "Chase%", "Max EV"))) {
  fail("HomeBASE hitter percentile board contains non-stat labels.")
}
if (!identical(
  pitcher_percentiles$Label,
  c(
    "Stuff+", "FIP", "WHIP", "BAA", "SLG allowed", "wOBA allowed", "K/9", "BB/9", "H/9", "K%", "BB%",
    "CSW%", "Whiff%", "Chase%", "Strike%", "Zone%", "FPS%", "Barrel%", "GB%", "Avg EV", "FB velo", "Extension"
  )
)) {
  fail("HomeBASE pitcher percentile board contains non-stat labels.")
}

pitcher_metrics <- homebase_pitcher_metrics(synthetic)
required_pitcher_metrics <- c(
  "Games", "IP", "R/9", "FIP", "WHIP", "BAA", "SLG", "K/9", "BB/9", "K-BB%",
  "Strike%", "Zone%", "FPS%", "CSW%", "Whiff%", "Chase%", "Contact%", "Barrel%", "HardHit%", "GB%"
)
if (!all(required_pitcher_metrics %in% names(pitcher_metrics))) {
  fail("HomeBASE pitcher stat board is missing detailed performance metrics.")
}

TEAM_CONFIG$full_name <- "Texas State Bobcats"
TEAM_CONFIG$colors <- list(primary = "#501214", secondary = "#AC9155")
TEAM_CONFIG$data <- list(ncaa_colors_file = base_project_path("data", "reference", "NcaaColors.csv"))
if (grepl('selectInput\\(\\s*"hb_pitcher_card_page"', homebase_source, perl = TRUE)) {
  fail("HomeBASE still exposes the split CAPS pitcher-card page selector.")
}
if (!grepl("build_pitcher_card_page", homebase_source, fixed = TRUE) ||
    !grepl("draw_cards_to_pdf", homebase_source, fixed = TRUE)) {
  fail("HomeBASE is not wired to the original complete CAPS pitcher-card renderer and export.")
}
if (!all(vapply(
  c("is-workload", "is-results", "is-execution", "is-contact", "is-traits"),
  grepl, logical(1), x = homebase_source, fixed = TRUE
))) {
  fail("HomeBASE pitcher statistics are not organized into the five visual panels.")
}

cat("HomeBASE 2026 sources, complete CAPS pitcher card, detailed stats, dates, team names, and percentile tests passed.\n")
