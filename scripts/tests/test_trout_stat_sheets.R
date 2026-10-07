#!/usr/bin/env Rscript
# Integration fixtures exercise the same filtered summaries used by the exports.
eval(parse("scripts/tests/test_wally_hitting_integration.R"), envir = .GlobalEnv)
output_dir <- Sys.getenv("BASE_TROUT_PREVIEW_DIR", unset = tempdir())
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
pa <- tibble::tibble(PA_ID = paste0("pa", 1:9), PitchofPA = 1,
  PitchCall = c("InPlay", "InPlay", "InPlay", "StrikeSwinging", "BallCalled", "HitByPitch", "InPlay", "InPlay", "InPlay"),
  PlayResult = c("Single", "Double", "Out", "Strikeout", "Walk", "Undefined", "SacrificeFly", "SacrificeBunt", "CatcherInterference"),
  KorBB = c("Undefined", "Undefined", "Undefined", "Strikeout", "Walk", rep("Undefined", 4)))
stats <- workspace$summarize_overall(pa)
stopifnot(stats$H == 2, stats$K == 1, stats$BB == 1, stats$AVG == .5)
shiny::testServer(workspace$server, {
  session$setInputs(hit_leader_seasons = "S26", hit_leader_hand = c("LHP", "RHP"))
  stats <- leaderboard_summary(leaderboard_data())
  stopifnot(nrow(stats) > 0, all(c("AVG", "K", "BB", "H") %in% names(stats)))
  stopifnot(identical(workspace$leaderboard_table_metrics[1:5], c("PA", "K", "BB", "H", "wOBA")))
  stats <- stats[order(-stats$PA, stats$Hitter), c("Hitter", workspace$leaderboard_table_metrics)]
  formatted <- workspace$format_perf_table(stats)[names(stats)]
  shaded <- apply_leaderboard_shading(stats, formatted)[names(stats)]
  stopifnot(identical(shaded[c("K", "BB", "H")], formatted[c("K", "BB", "H")]),
            any(grepl("background", shaded$AVG)))
  invisible(output$hit_leaderboard_table)
  invisible(output$leaderboard_team_table)
  base_write_trout_stat_sheet(file.path(output_dir, "trout-hitting.pdf"), formatted, shaded, "Hitting", "PA", "2026 Season")
})
eval(parse("scripts/tests/test_wally_pitching_integration.R"), envir = .GlobalEnv)
shiny::testServer(workspace$server, {
  session$setInputs(leader_seasons = "S26", leader_hand = c("L", "R"), leader_min_pa = 0)
  tables <- leaderboard_tables()
  stopifnot(nrow(tables$numeric) > 0,
            identical(names(tables$formatted)[1:8], c("Pitcher", "PA", "IP", "K", "BB", "HBP", "H", "BAA")),
            isTRUE(all.equal(tables$numeric$KBBp, tables$numeric$Kp - tables$numeric$BBp)))
  stopifnot(identical(names(tables$formatted)[match("BB%", names(tables$formatted)) + 1:2], c("K%-BB%", "BB+HBP%")))
  invisible(output$leaderboard_table)
  invisible(output$leaderboard_totals_table)
  ord <- order(-tables$numeric$IP, tables$numeric$Pitcher)
  base_write_trout_stat_sheet(file.path(output_dir, "trout-pitching.pdf"), tables$formatted[ord, ], tables$shaded[ord, ], "Pitching", "IP", "2026 Season")
})
# A recorded strikeout out must not be counted twice. IP uses baseball thirds.
saved_rows <- workspace$txst_df
rows <- saved_rows[rep(1, 9), ]
rows$Pitcher <- c(rep("Test Starter", 7), rep("Test Reliever", 2))
rows$PitchUID <- rows$pitch_uid <- paste0("trout-pitch-", 1:9)
rows$row_id <- 1:9
rows$PA_ID <- paste0("trout-pa-", 1:9)
rows$PAofInning <- 1:9
rows$PitchofPA <- 1
rows$PlayResult <- c("Single", "Double", "Out", "Strikeout", "Walk", "Undefined", "SacrificeFly", "TriplePlay", "Strikeout")
rows$KorBB <- c(rep("Undefined", 3), "Strikeout", "Walk", "Undefined", "Undefined", "Undefined", "Strikeout")
rows$PitchCall <- c(rep("InPlay", 3), "StrikeSwinging", "BallCalled", "HitByPitch", "InPlay", "InPlay", "StrikeSwinging")
rows$OutsOnPlay <- c(0, 0, 1, 1, 0, 0, 1, 3, 1)
workspace$txst_df <- rows
shiny::testServer(workspace$server, {
  tables <- leaderboard_tables()
  starter <- tables$formatted[tables$formatted$Pitcher == "Test Starter", ]
  reliever <- tables$formatted[tables$formatted$Pitcher == "Test Reliever", ]
  stopifnot(starter$IP == "1.0", reliever$IP == "1.1", starter$K == 1,
            starter$BB == 1, starter$HBP == 1, starter$H == 2)
})
workspace$txst_df <- saved_rows
cat("Trout Stat Sheet columns, counts, innings, shading, sorting inputs, and PDF exports passed.\n")
