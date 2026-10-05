#!/usr/bin/env Rscript

source("team_config.R", local = FALSE)
source("R/data/player_health_imports.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
scratch <- tempfile("base-sports-science-import-")
dir.create(scratch, recursive = TRUE)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

example_rows <- function(target_id, count = 2L, marker = "new") {
  target <- base_sports_science_import_target(target_id)
  rows <- as.data.frame(
    stats::setNames(replicate(length(target$required), rep("1", count), simplify = FALSE), target$required),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(target$id, "ARM_CARE")) {
    rows[["First Name"]] <- paste0("Arm", seq_len(count))
    rows[["Last Name"]] <- "Athlete"
    rows[["Exam Date"]] <- format(as.Date("2026-09-20") + seq_len(count), "%m/%d/%Y")
    rows$Time <- "12:00:00"
    rows[["Exam Type"]] <- "Fresh"
    rows[["Arm Score"]] <- marker
  } else {
    rows$firstName <- paste0("Pulse", seq_len(count))
    rows$lastName <- "Athlete"
    if (identical(target$id, "PULSE_EVENTS")) {
      rows$datetime <- paste0("2026-09-2", seq_len(count), "T12:00:00.000Z")
      rows$tag <- marker
    } else {
      rows$date <- as.character(as.Date("2026-09-20") + seq_len(count))
      rows[["A:C Ratio"]] <- marker
    }
  }
  rows
}

write_fixture <- function(rows, path) {
  utils::write.csv(rows, path, row.names = FALSE, na = "")
  invisible(path)
}

for (target_id in names(BASE_SPORTS_SCIENCE_IMPORT_TARGETS)) {
  target <- base_sports_science_import_target(target_id)
  old_names <- if (identical(target_id, "ARM_CARE")) {
    c("Daily Log old.csv", "Daily Log newer.csv")
  } else if (identical(target_id, "PULSE_EVENTS")) {
    c("old_events.csv")
  } else {
    c("old_workload.csv")
  }
  for (old_name in old_names) {
    write_fixture(example_rows(target_id, 1L, marker = "old"), file.path(scratch, old_name))
  }
  upload <- tempfile(fileext = ".csv")
  on.exit(unlink(upload), add = TRUE)
  write_fixture(example_rows(target_id, 2L), upload)

  result <- base_replace_sports_science_import(upload, target_id, root = scratch)
  files <- base_sports_science_import_files(target_id, root = scratch)
  if (!identical(basename(files), target$filename)) {
    fail(target$label, " did not replace all older matching exports with its canonical source.")
  }
  if (result$received_rows != 2L || result$replaced_files != length(old_names)) {
    fail(target$label, " replacement reported incorrect row or replacement counts.")
  }
  saved <- base_read_sports_science_import(files[[1]])
  marker_column <- if (identical(target_id, "ARM_CARE")) "Arm Score" else if (identical(target_id, "PULSE_EVENTS")) "tag" else "A:C Ratio"
  if (nrow(saved) != 2L || !all(saved[[marker_column]] == "new")) {
    fail(target$label, " retained old rows or failed to save the new snapshot.")
  }
}

events_path <- base_sports_science_import_path("PULSE_EVENTS", root = scratch)
before <- unname(tools::md5sum(events_path))
wrong_upload <- tempfile(fileext = ".csv")
on.exit(unlink(wrong_upload), add = TRUE)
write_fixture(example_rows("PULSE_WORKLOAD", 1L), wrong_upload)
error <- tryCatch({
  base_replace_sports_science_import(wrong_upload, "PULSE_EVENTS", root = scratch)
  NULL
}, error = identity)
if (!inherits(error, "error") || !grepl("Missing required columns", conditionMessage(error), fixed = TRUE)) {
  fail("A PULSE Workload file was not rejected by the PULSE Events importer.")
}
after <- unname(tools::md5sum(events_path))
if (!identical(before, after)) {
  fail("A rejected Sports Science upload changed the active production source.")
}

cat("Sports Science imports passed: schema validation, atomic replacement, old-file removal, and rollback safety.\n")
