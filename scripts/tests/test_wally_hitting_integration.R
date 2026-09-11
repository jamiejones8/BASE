#!/usr/bin/env Rscript

baseline_library <- file.path(getwd(), ".baseline-runtime", "R")
if (dir.exists(baseline_library)) .libPaths(c(baseline_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("team_config.R", local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source("R/integrations/wally_hitting_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

fixture_path <- file.path(
  "tests", "fixtures", "wallyapps", "hitting", "data",
  "2026 Season - cleaned.csv"
)
fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)
fixture$BatterTeam <- TEAM_CONFIG$data_code
fixture$source_file <- "2026 Season - cleaned.csv"
fixture$row_in_file <- seq_len(nrow(fixture))
fixture$SeasonGroup <- "S26"
fixture$DataSource <- "Texas State internal — 2026 Season - cleaned.csv"
fixture$.base_source_priority <- 2L

# The integrated workspace must use the four CSVs in HittingApp/data.
expected_files <- c(
  "2025 Season -cleaned.csv", "2025 Fall -cleaned.csv",
  "2026 Squads - cleaned.csv", "2026 Season - cleaned.csv"
)
if (!identical(basename(base_hitting_supplement_paths()), expected_files)) {
  fail("Hitting workspace does not resolve the four HittingApp folder CSVs.")
}

# Folder rows still deduplicate repeated pitch IDs across selected season files.
supplement <- fixture[1:2, , drop = FALSE]
supplement$source_file <- "2026 Squads - cleaned.csv"
supplement$DataSource <- "Texas State internal — 2026 Squads - cleaned.csv"
supplement$PitchUID[[2]] <- "fixture-unique-hitting-supplement"

prepared <- base_prepare_team_hitting_data(dplyr::bind_rows(fixture, supplement))
if (sum(prepared$PitchUID == fixture$PitchUID[[1]], na.rm = TRUE) != 1L) {
  fail("Duplicate folder pitch was not removed.")
}
if (!any(prepared$PitchUID == "fixture-unique-hitting-supplement", na.rm = TRUE)) {
  fail("Unique hitting supplement row was lost.")
}

workspace <- base_wally_hitting_environment(prepared)
if (!is.function(workspace$server)) fail("Embedded Hitting server is unavailable.")
# Exercise the embedded path from outside both the repo and standalone app.
grid <- readRDS(TEAM_CONFIG$data$xwoba_grid_file)
lookup_from_other_directory <- function() {
  previous <- setwd(tempdir())
  on.exit(setwd(previous))
  workspace$hitting_xwoba_lookup(c(100, NA, 200), c(20, 20, 20))
}
expected_contact <- grid$grid[findInterval(100, grid$ev_edges), findInterval(20, grid$la_edges)]
actual_contact <- lookup_from_other_directory()
if (!is.finite(expected_contact) ||
    !isTRUE(all.equal(actual_contact, c(expected_contact, NA_real_, NA_real_)))) {
  fail("Embedded hitting xwOBA lookup did not resolve the real configured model.")
}
if (!grepl("rgba\\(227,52,52", workspace$stat_severity_fill(1))) {
  fail("Coach-facing Hitting shading is not red for favorable values.")
}
if (!grepl("rgba\\(46,125,50", workspace$player_severity_fill(1))) {
  fail("Player-facing Hitting shading is not green for favorable values.")
}
if (!grepl("^#[0-9A-Fa-f]{6}$", workspace$d1_shade_fill(.40, .30))) {
  fail("Hitting AAR shading is not using PDF-safe hexadecimal colors.")
}
# One standard deviation above/below the supplied means is approximately
# the 84th/16th percentile; the mean is neutral at the 50th percentile.
shade_values <- tibble::tibble(
  `Swing%` = c(.4866, .3566, NA_real_, .4216),
  MaxEV = c(111.90, 100.66, NA_real_, 106.28)
)
shaded <- workspace$apply_hitting_percentile_shading(shade_values, shade_values)
for (column in names(shade_values)) {
  if (!grepl("rgba(227,52,52", shaded[[column]][1], fixed = TRUE) ||
      !grepl("rgba(93,126,188", shaded[[column]][2], fixed = TRUE) ||
      !is.na(shaded[[column]][3]) ||
      grepl("background-color", shaded[[column]][4], fixed = TRUE)) {
    fail("Hitting percentile cells do not shade high, low, and missing values correctly: ", column)
  }
  if (!all(mapply(function(cell, percentile) grepl(percentile, cell, fixed = TRUE),
                  shaded[[column]][c(1, 2, 4)], c("84 percentile", "16 percentile", "50 percentile")))) {
    fail("Hitting percentiles do not use the supplied mean and standard deviation: ", column)
  }
}

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Hitting UI contains a nested full-page body element.")
}
if (!grepl("base-hitting-embedded-layout", html, fixed = TRUE)) {
  fail("Embedded Hitting UI is missing its scoped BASE layout wrapper.")
}
style_html <- htmltools::renderTags(base_hitting_embedded_head())$head
for (marker in c(
  "base-hitting-performance-controls",
  "base-hitting-performance-table",
  "base-hitting-performance-heading"
)) {
  if (!grepl(marker, html, fixed = TRUE)) {
    fail("Hitting Performance is missing pitcher-matched table styling: ", marker)
  }
}
for (marker in c(
  "#perf_tbl table.dataTable thead tr.group-header th",
  "tbody tr.base-total-row td"
)) {
  if (!grepl(marker, style_html, fixed = TRUE)) {
    fail("Hitting Performance is missing pitcher-matched table styling: ", marker)
  }
}
expected_tabs <- c(
  "Performance", "Lineup Builder", "Damage Heat Map", "Whiff Zones",
  "Team Report", "Leaderboard", "Swing Decisions",
  "Ball Flight", "Contact Point"
)
missing_tabs <- expected_tabs[!vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)]
if (length(missing_tabs)) {
  fail("Embedded Hitting UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}
if (grepl("Game Reports (AAR)", html, fixed = TRUE)) {
  fail("Hitting AAR is still present in the Hitting workspace.")
}
postgame_html <- paste(as.character(workspace$base_hitting_postgame_ui), collapse = "")
if (!grepl("hit_aar_pdf", postgame_html, fixed = TRUE) ||
    !grepl("aar_hitter", postgame_html, fixed = TRUE)) {
  fail("Hitting AAR was not exposed to the Postgame Reports workspace.")
}

hitter <- as.character(workspace$hitters_txst[[1]])
games <- unique(as.character(workspace$txst_df$CustomGameID))
games <- games[!is.na(games) & nzchar(games)]
if (!length(games)) fail("Hitting fixture exposed no selectable games.")

shiny::testServer(workspace$server, {
  # The embedded server starts before its dynamically inserted UI has sent any
  # input values. Startup must remain quiet until the first hitter arrives.
  session$flushReact()

  session$setInputs(
    Hitter = hitter,
    hit_season_groups = "S26",
    Game = games,
    PitcherHand = "All",
    hit_perf_split = "hand",
    hit_leader_seasons = "S26",
    hit_leader_hand = c("LHP", "RHP")
  )
  session$flushReact()
  filtered <- dat_filt()
  if (!nrow(filtered)) fail("Hitting Performance filter returned no fixture rows.")
  perf_summary <- workspace$summarize_overall(filtered)
  if (!is.finite(perf_summary$xwOBAcon)) {
    fail("Fixture contact is still missing expected wOBA after loading the model.")
  }
  # Contact alone defines xwOBAcon; BB/HBP/K contribute only to full xwOBA.
  contact_cases <- filtered[rep(1, 6), , drop = FALSE]
  contact_cases$PA_ID <- paste0("contact-case-", seq_len(6))
  contact_cases$pitch_call <- contact_cases$PitchCall <- c("InPlay", "InPlay", "BallCalled", "HitByPitch", "StrikeSwinging", "BallCalled")
  contact_cases$play_result <- contact_cases$PlayResult <- c("Single", "Out", "Walk", "Undefined", "Strikeout", "IntentionalWalk")
  contact_cases$KorBB <- c("", "", "BB", "Undefined", "K", "IBB")
  contact_cases$ev <- 100
  contact_cases$la <- 20
  contact_cases$xwOBA <- NA_real_
  contact_summary <- workspace$summarize_overall(contact_cases)
  if (!isTRUE(all.equal(contact_summary$xwOBAcon, expected_contact)) ||
      !isTRUE(all.equal(contact_summary$xwOBA, (2 * expected_contact + .69 + .72) / 5))) {
    fail("Expected-contact and expected-PA wOBA calculations have incorrect denominators.")
  }
  if (!isTRUE(all.equal(contact_summary$wOBA, (.88 + .69 + .72) / 5))) {
    fail("PitchCall-only hit-by-pitches are not included in actual wOBA.")
  }
  required_perf_metrics <- c(
    "xwOBA", "xwOBAcon", "Swing%", "IZ-Swing%", "MaxEV", "90th EV",
    "10-35*%", "GB%", "LD%", "FB%", "PU%", "Foul Ball%"
  )
  missing_perf_metrics <- setdiff(required_perf_metrics, names(perf_summary))
  if (length(missing_perf_metrics)) {
    fail("Hitting Performance summary is missing: ", paste(missing_perf_metrics, collapse = ", "))
  }
  visible_metrics <- workspace$performance_table_metrics
  if (any(c("Contact%", "Z-Contact%", "LD+FB%") %in% visible_metrics)) {
    fail("Removed Hitting Performance metrics are still visible.")
  }
  if (!identical(visible_metrics[match("GB%", visible_metrics) + 0:3], c("GB%", "LD%", "FB%", "PU%"))) {
    fail("Batted-ball columns are not ordered GB%, LD%, FB%, PU%.")
  }
  if (match("MaxEV", visible_metrics) + 1L != match("90th EV", visible_metrics)) {
    fail("MaxEV is not immediately before EV90 in Hitting Performance.")
  }
  test_grid <- list(
    ev_edges = c(0, 100, 200), la_edges = c(-90, 0, 90),
    grid = matrix(c(.100, .200, .300, .400), nrow = 2, byrow = TRUE)
  )
  if (!isTRUE(all.equal(workspace$hitting_xwoba_lookup(50, -45, test_grid), .100))) {
    fail("CAPS xwOBA grid lookup was not reused correctly.")
  }
  pa_check <- filtered
  pa_check$PA_ID <- workspace$make_pa_id(pa_check)
  pa_check <- pa_check %>%
    dplyr::group_by(.data$PA_ID) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  pc_check <- workspace$.get_chr(pa_check, c("pitch_call", "PitchCall"))
  pr_check <- workspace$.get_chr(pa_check, c("play_result", "PlayResult"))
  la_check <- if ("la" %in% names(pa_check)) pa_check$la else workspace$.get_num(pa_check, c("Angle", "LaunchAngle"))
  bip_check <- workspace$is_bip_txst(pc_check, pr_check)
  bip_den <- sum((bip_check %in% TRUE) & is.finite(la_check))
  expected_bip_rates <- c(
    `GB%` = workspace$safe_ratio(sum(bip_check & is.finite(la_check) & la_check < 5), bip_den),
    `LD%` = workspace$safe_ratio(sum(bip_check & is.finite(la_check) & la_check >= 5 & la_check < 25), bip_den),
    `FB%` = workspace$safe_ratio(sum(bip_check & is.finite(la_check) & la_check >= 25 & la_check <= 50), bip_den),
    `PU%` = workspace$safe_ratio(sum(bip_check & is.finite(la_check) & la_check > 50), bip_den),
    `10-35*%` = workspace$safe_ratio(sum(bip_check & is.finite(la_check) & la_check >= 10 & la_check <= 35), bip_den)
  )
  actual_bip_rates <- unlist(perf_summary[names(expected_bip_rates)], use.names = TRUE)
  if (!isTRUE(all.equal(actual_bip_rates, expected_bip_rates, check.attributes = FALSE))) {
    fail("Hitting Performance BIP launch-angle rates do not match the requested buckets.")
  }
  displayed_bip_rates <- readr::parse_number(unlist(
    workspace$format_perf_table(perf_summary)[c("GB%", "LD%", "FB%", "PU%")],
    use.names = FALSE
  ))
  if (all(is.finite(displayed_bip_rates)) && !isTRUE(all.equal(sum(displayed_bip_rates), 100))) {
    fail("Displayed Hitting Performance BIP buckets do not total exactly 100.0%.")
  }
  if (isTRUE(workspace$is_bip_txst("FoulBallNotFieldable", ""))) {
    fail("A foul ball was incorrectly included in the BIP sample.")
  }
  pitch_calls <- workspace$.get_chr(filtered, c("pitch_call", "PitchCall"))
  foul_contacts <- grepl("foul", pitch_calls, ignore.case = TRUE)
  bip_contacts <- grepl("in\\s*play|inplay|ball\\s*in\\s*play", pitch_calls, ignore.case = TRUE)
  expected_foul_pct <- workspace$safe_ratio(sum(foul_contacts), sum(foul_contacts | bip_contacts))
  if (!isTRUE(all.equal(perf_summary$`Foul Ball%`, expected_foul_pct))) {
    fail("Foul Ball% does not use foul balls divided by foul balls plus balls in play.")
  }
  if (bip_den > 0) {
    with_expected_contact <- filtered
    with_expected_contact$xwOBA <- 0.321
    if (!isTRUE(all.equal(workspace$summarize_overall(with_expected_contact)$xwOBAcon, 0.321))) {
      fail("xwOBAcon did not use expected contact values on balls in play.")
    }
  }
  choices <- lineup_player_choices()
  if (!length(choices)) fail("Lineup Builder returned no fixture player choices.")
  session$setInputs(aar_hitter = hitter, hit_aar_season_groups = "S26")
  session$flushReact()
  aar_games <- aar_game_choices()
  if (!length(aar_games)) fail("Moved Hitting AAR returned no game choices.")
  session$setInputs(AARGame = aar_games[[1]])
  session$flushReact()
  if (!nrow(aar_data())) fail("Moved Hitting AAR returned no fixture rows.")
  invisible(output$aar_kpi)
  invisible(output$aar_swing_tbl)
  invisible(output$perf_tbl)
  invisible(output$lineup_builder_ui)
})

cat(
  "Wally Hitting integration passed:", nrow(prepared),
  "folder-backed rows,", length(expected_tabs),
  "tabs, Performance, and Lineup Builder server smoke tests.\n"
)
