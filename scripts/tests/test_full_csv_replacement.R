#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(dplyr); library(shiny) })
source("team_config.R")
source("R/data/source_contract.R")
source("R/data/team_season_imports.R")
source("R/data/player_health_imports.R")
source("R/integrations/wally_scouting_workspace.R")
scratch <- tempfile("full-csv-"); dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
Sys.setenv(BASE_SCOUTING_DATA_DIR = file.path(scratch, "scouting"))
rows <- tibble::tibble(PitchUID = c("a", "b"), GameUID = c("game1", "game2"),
 Date = c("2026-10-17", "2026-10-18"), PitcherTeam = "TEX_BOB", BatterTeam = "TCU_HFG",
 Pitcher = "Pitcher", Batter = "Batter", PitchCall = "StrikeCalled", Catcher = "Original")
path <- base_team_season_import_path("F26")
base_atomic_write_trackman_csv(rows, path)
upload <- file.path(scratch, "edited.csv")
edited <- rows[2, ]; edited$Catcher <- "Corrected"
readr::write_csv(edited, upload)
fingerprint <- base_trackman_csv_fingerprint(path)
result <- base_replace_trackman_csv(upload, "F26", fingerprint)
stopifnot(result$total_rows == 1, file.exists(result$backup),
 nrow(base_read_trackman_import(result$backup)) == 2,
 base_read_trackman_import(path)$Catcher == "Corrected")
# Multiple games and added rows are supported, with complete replacement.
readr::write_csv(rows, upload)
base_replace_trackman_csv(upload, "F26", base_trackman_csv_fingerprint(path))
stopifnot(nrow(base_read_trackman_import(path)) == 2)
expect_unchanged <- function(bad) {
 readr::write_csv(bad, upload)
 prior <- readBin(path, "raw", n = file.info(path)$size)
 error <- tryCatch(base_replace_trackman_csv(upload, "F26", base_trackman_csv_fingerprint(path)), error = identity)
 stopifnot(inherits(error, "error"), identical(prior, readBin(path, "raw", n = file.info(path)$size)))
}
expect_unchanged(rows[FALSE, ])
expect_unchanged(bind_rows(rows, rows[1, ]))
bad <- rows; bad$Date[1] <- "not a date"; expect_unchanged(bad)
bad <- rows; bad$Date <- "2027-01-01"; expect_unchanged(bad)
bad <- rows; bad$PitchUID <- NA_character_; bad$GameUID <- NA_character_; expect_unchanged(bad)
bad <- rows; bad$PitcherTeam <- "OTHER"; expect_unchanged(bad)
expect_unchanged(select(rows, -PitchCall))
readr::write_csv(edited, upload)
stopifnot(inherits(tryCatch(base_replace_trackman_csv(upload, "F26", fingerprint), error = identity), "error"))
# Uploaded opponent sources replace the selected file rather than creating another.
scout <- base_import_scouting_file(upload, "TCU.csv")
result <- base_replace_scouting_csv(upload, paste0("upload:", scout$filename), base_trackman_csv_fingerprint(scout$path))
stopifnot(normalizePath(result$destination) == normalizePath(scout$path), file.exists(result$backup),
 length(list.files(dirname(scout$path), pattern = "[.]csv$")) == 1)
stopifnot(inherits(tryCatch(base_replace_scouting_csv(upload, "../../outside.csv", NULL), error = identity), "error"))
# Render every card and exercise the confirmation observer with isolated storage.
positioning <- base_read_trackman_import("WallyApps/DefenseApp/data/2026 Fall Defense.csv")[1:2, ]
positioning$PitchUID <- c("position1", "position2")
positioning$GameUID <- c("position-game1", "position-game2")
position_upload <- file.path(scratch, "positions.csv")
readr::write_csv(positioning, position_upload)
position_result <- base_replace_trackman_csv(position_upload, "PP_F26", NULL)
stopifnot(position_result$total_rows == 2,
 all(base_read_trackman_import(position_result$destination)$SeasonGroup == "F26"))
# Sports Science replacement backs up the previous file and rejects stale reviews.
health_root <- file.path(scratch, "health")
required <- base_sports_science_import_target("PULSE_WORKLOAD")$required
health <- as.data.frame(stats::setNames(rep(list("1"), length(required)), required), check.names = FALSE)
health$date <- "2026-10-17"
health_upload <- file.path(scratch, "health.csv"); readr::write_csv(health, health_upload)
base_replace_sports_science_import(health_upload, "PULSE_WORKLOAD", root = health_root)
health_path <- base_sports_science_import_path("PULSE_WORKLOAD", root = health_root)
health_before <- base_trackman_csv_fingerprint(health_path)
health$firstName <- "Edited"; readr::write_csv(health, health_upload)
base_replace_sports_science_import(health_upload, "PULSE_WORKLOAD", root = health_root,
 expected_fingerprint = health_before, check_fingerprint = TRUE)
stopifnot(length(list.files(file.path(health_root, "backups"))) == 1,
 base_read_sports_science_import(health_path)$firstName == "Edited",
 inherits(tryCatch(base_replace_sports_science_import(health_upload, "PULSE_WORKLOAD", root = health_root,
 expected_fingerprint = health_before, check_fingerprint = TRUE), error = identity), "error"))
for (expr in parse("R/app_main.R")) {
 if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) &&
     as.character(expr[[2]]) %in% c("BASE_NAV_TABS", "data_processing_workspace_ui", "data_processing_server", "workspace_tool_card", "base_nav_click_js")) eval(expr)
}
html <- as.character(data_processing_workspace_ui())
prefixes <- c("dp_f26", "dp_ps27", "dp_s27", "dp_bp", "dp_positioning_f26",
 "dp_positioning_scrimmages", "dp_positioning_s27", "dp_scouting")
for (prefix in prefixes) for (suffix in c("_download_full", "_full_file", "_full_status")) {
 stopifnot(grepl(paste0(prefix, suffix), html, fixed = TRUE))
}
# Vendor drop zones append dates; they must not expose replacement uploaders.
for(prefix in c("dp_pulse_events","dp_pulse_workload","dp_arm_care","dp_blast")) {
 stopifnot(grepl(paste0(prefix,"_file"),html,fixed=TRUE),
  grepl(paste0(prefix,"_replace"),html,fixed=TRUE),
  !grepl(paste0(prefix,"_full_file"),html,fixed=TRUE))
}
for (name in c("base_clear_team_pitching_cache", "base_clear_team_hitting_cache", "base_clear_wally_pitching_state", "base_clear_wally_hitting_state", "base_clear_wally_defense_state", "homebase_clear_history_cache")) assign(name, function() NULL)
shiny::testServer(data_processing_server, {
 session$flushReact()
 blast_upload <- file.path(scratch,"blast-upload.csv")
 utils::write.csv(data.frame(`Swing Date`=c("2026-10-20","2026-10-21"),BatSpeed=c(70,75),check.names=FALSE),blast_upload,row.names=FALSE)
 session$setInputs(dp_blast_file=list(datapath=blast_upload,name="blast-upload.csv"),dp_blast_replace=1)
 stopifnot(states$BLAST()$type=="success",states$BLAST()$result$inserted_rows==2)
 session$setInputs(dp_blast_replace=2)
 stopifnot(states$BLAST()$result$inserted_rows==0,states$BLAST()$result$skipped_rows==2)
 session$setInputs(dp_f26_full_file = list(datapath = upload, name = "edited.csv"))
 stopifnot(pending_full_file()$target_id == "F26", nrow(base_read_trackman_import(path)) == 2)
 session$setInputs(dp_confirm_full_replace = 1)
 stopifnot(nrow(base_read_trackman_import(path)) == 1, is.null(pending_full_file()))
 # Loop registrations must retain their own target, not the last loop value.
 session$setInputs(dp_ps27_full_file = list(datapath = upload, name = "edited.csv"))
 stopifnot(pending_full_file()$target_id == "PS27")
})
unlink(scratch, recursive = TRUE)
cat("Full CSV replacement passed: backups, multiple games, row changes, validation, stale-source protection, scouting selection, every card, and confirmation.\n")
