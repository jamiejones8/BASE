#!/usr/bin/env Rscript
source("team_config.R")
source("R/data/source_contract.R")
ids <- c("20261003-TexasStateunviersity-Private-1.csv", "away-game", "home-game", "doubleheader")
rows <- data.frame(CustomGameID = ids, GameDate = as.Date(rep("2026-10-03", 4)),
  HomeTeam = c("TEX_BOB", "UTS_ROA", "TEX_BOB", "TEX_BOB"),
  AwayTeam = c("TEX_BOB", "TEX_BOB", "TEX_LON", "TEX_LON"))
choices <- base_game_choices(ids, rows)
stopifnot(identical(unname(choices), ids), names(choices)[1] == "Oct 3, 2026 Scrimmage",
          names(choices)[2] == "Oct 3, 2026 vs UTSA Roadrunners",
          identical(names(choices)[3:4], paste0("Oct 3, 2026 vs Texas Longhorns (Game ", 1:2, ")")))
stopifnot(names(base_game_choices("away-game", rows, team = "UTS_ROA")) == "Oct 3, 2026 vs Texas State")
# Filename date fallback and old custom IDs remain supported.
rows$GameDate <- NULL
stopifnot(names(base_game_choices(ids[1], rows)) == "Oct 3, 2026 Scrimmage",
          names(base_game_choices("2026-10-03: TEX_BOB @ TEX_BOB")) == "Oct 3, 2026 Scrimmage",
          names(base_game_choices("undated")) == "undated",
          length(base_game_choices(character())) == 0)
# Exports without home/away can identify scrimmages from the pitch's teams.
rows <- data.frame(CustomGameID = "scrim", Date = "10/3/2026", PitcherTeam = "TEX_BOB", BatterTeam = "TEX_BOB")
stopifnot(names(base_game_choices("scrim", rows)) == "Oct 3, 2026 Scrimmage")
# Both TrackMan name orders resolve to the corrected spelling on import.
players <- data.frame(Catcher = c("Justin Antcil", "Antcil, Justin", "Justin Anctil", "Other Catcher"))
stopifnot(identical(base_exclude_walk_on_players(players)$Catcher,
                    c("Justin Anctil", "Anctil, Justin", "Justin Anctil", "Other Catcher")))
# A transfer's historical games use the relevant player's team as context.
rows <- data.frame(CustomGameID = "transfer", Date = "2026-03-01",
                   HomeTeam = "UTS_ROA", AwayTeam = "TEX_LON",
                   PitcherTeam = "UTS_ROA", BatterTeam = "TEX_LON")
stopifnot(names(base_game_choices("transfer", rows)) == "Mar 1, 2026 vs Texas Longhorns",
          names(base_game_choices("transfer", rows, team_col = "BatterTeam")) == "Mar 1, 2026 vs UTSA Roadrunners")
cat("Game labels and player spelling checks passed.\n")
