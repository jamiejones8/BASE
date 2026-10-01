#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Sports Science 2/vald_shiny_app/R/roster_trackman.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
tmp <- tempfile("player-health-roster-")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

roster <- list(
  players = data.frame(
    name = c("Exact Pitcher", "Mapped Catcher"),
    position = c("RHP", "C"),
    key = c("exact pitcher", "mapped catcher"),
    role = c("Pitchers", "Hitters")
  ),
  aliases = data.frame(
    alias = "different vendor name",
    roster_name = "Mapped Catcher",
    key = "mapped catcher"
  ),
  error = NULL
)

if (!identical(player_name_key(" Pitcher, Exact "), "exact pitcher")) {
  fail("Last, First roster normalization failed.")
}

vendor_rows <- data.frame(
  name = c("Exact Pitcher", "Different Vendor Name", "Former Player"),
  key = c("wrong", "wrong", "wrong"),
  role = "Other"
)
filtered <- roster_filter(vendor_rows, roster)
if (!identical(filtered$name, c("Exact Pitcher", "Mapped Catcher"))) {
  fail("Current-roster filtering or alias mapping failed.")
}
if (!identical(filtered$key, c("exact pitcher", "mapped catcher")) ||
    !identical(filtered$role, c("Pitchers", "Hitters"))) {
  fail("Roster keys or roles were not reconciled.")
}

trackman <- data.frame(
  PitchUID = paste0("pitch-", 1:6),
  Pitcher = "Pitcher, Exact",
  PitcherId = "trackman-1",
  Date = "2026-09-17",
  TaggedPitchType = c("Fastball", "Sinker", "Cutter", "Slider", "Changeup", "Fastball"),
  RelSpeed = c(90, 94, 82, 80, 84, NA),
  SpinRate = c(2100, 2300, 2500, 2700, 1800, 2200)
)
write.csv(trackman, file.path(tmp, "first_trackman.csv"), row.names = FALSE, na = "")

newer <- trackman[1, , drop = FALSE]
newer$RelSpeed <- 92
write.csv(newer, file.path(tmp, "second_trackman.csv"), row.names = FALSE)
Sys.setFileTime(file.path(tmp, "first_trackman.csv"), Sys.time() - 60)

imported <- trackman_read(tmp, roster)
if (length(imported$errors)) fail("TrackMan import reported: ", paste(imported$errors, collapse = "; "))
if (imported$duplicate_count != 1L || nrow(imported$pitches) != 6L) {
  fail("TrackMan pitch-ID deduplication failed.")
}

fastballs <- imported$daily[imported$daily$pitch_group == "Fastball/sinker", , drop = FALSE]
breaking <- imported$daily[imported$daily$pitch_group == "Breaking ball", , drop = FALSE]
if (nrow(fastballs) != 1L || fastballs$pitch_count != 3L ||
    fastballs$velocity_n != 2L || fastballs$spin_n != 3L ||
    abs(fastballs$velocity_mph - 93) > 1e-8 || abs(fastballs$spin_rpm - 2200) > 1e-8) {
  fail("Fastball/sinker daily KPI calculation failed.")
}
if (nrow(breaking) != 1L || breaking$pitch_count != 2L ||
    abs(breaking$velocity_mph - 81) > 1e-8 || abs(breaking$spin_rpm - 2600) > 1e-8) {
  fail("Breaking-ball daily KPI calculation failed.")
}
if (!identical(imported$excluded_tags, "Changeup")) {
  fail("Excluded TrackMan pitch tags were not reported.")
}

cat("Player Health roster and TrackMan reconciliation passed.\n")
