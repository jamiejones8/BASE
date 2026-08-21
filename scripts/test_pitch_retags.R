source("team_config.R", local = FALSE)
source("pitch_retags.R", local = FALSE)

db_path <- tempfile(fileext = ".sqlite")
on.exit(unlink(c(db_path, paste0(db_path, "-journal"))), add = TRUE)

source_rows <- data.frame(
  DataSource = c("2026 College Season", "2026 Cape Cod League"),
  PitchUID = c("college-pitch-1", "cape-pitch-1"),
  PitcherTeam = c("TEX_BOB", "TEX_BOB"),
  Pitcher = c("Test, Pitcher", "Test, Pitcher"),
  TaggedPitchType = c("Fastball", "Slider"),
  GameUID = c("game-1", "game-2"),
  stringsAsFactors = FALSE
)

prepared <- base_prepare_retag_rows(source_rows)
stopifnot(length(unique(prepared$base_pitch_key)) == 2L)

saved <- base_save_pitch_retags(prepared[1, , drop = FALSE], "Cutter", db_path)
stopifnot(saved == 1L)

reloaded <- base_apply_pitch_retags(source_rows, db_path)
stopifnot(
  identical(reloaded$TaggedPitchType, c("Cutter", "Slider")),
  identical(reloaded$base_has_saved_retag, c(TRUE, FALSE))
)

saved <- base_save_pitch_retags(reloaded[1, , drop = FALSE], "Sinker", db_path)
stopifnot(saved == 1L)

reloaded <- base_apply_pitch_retags(source_rows, db_path)
stopifnot(identical(reloaded$TaggedPitchType, c("Sinker", "Slider")))

reverted <- base_revert_pitch_retags(reloaded[1, , drop = FALSE], db_path)
stopifnot(reverted == 1L)

restored <- base_apply_pitch_retags(source_rows, db_path)
stopifnot(
  identical(restored$TaggedPitchType, c("Fastball", "Slider")),
  !any(restored$base_has_saved_retag)
)

con <- base_retag_connect(db_path)
history_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pitch_tag_history")$n[[1]]
DBI::dbDisconnect(con)
stopifnot(history_n == 3L)

cat("Persistent pitch retag tests passed.\n")
