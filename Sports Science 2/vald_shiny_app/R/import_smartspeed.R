import_smartspeed_api <- function(save_raw = TRUE, build_gold = TRUE) {
  res <- pull_smartspeed_full(save = save_raw)
  if (!isTRUE(res$ok)) {
    if (exists("write_refresh_log", mode = "function")) {
      write_refresh_log("import_smartspeed", "skipped", list(reason = res$message))
    }
    if (isTRUE(build_gold)) build_sprint_session_level(raw = NULL)
    return(invisible(res))
  }
  if (isTRUE(build_gold)) build_sprint_session_level(raw = res$data)
  invisible(res)
}

import_smartspeed_tests_only <- function(save_raw = TRUE, build_gold = TRUE) {
  res <- pull_smartspeed_tests_only(save = save_raw)
  if (!isTRUE(res$ok)) {
    if (exists("write_refresh_log", mode = "function")) {
      write_refresh_log("import_smartspeed_tests", "skipped", list(reason = res$message))
    }
    if (isTRUE(build_gold)) build_sprint_session_level(raw = NULL)
    return(invisible(res))
  }
  if (isTRUE(build_gold)) build_sprint_session_level(raw = res$data)
  invisible(res)
}

import_smartspeed_local_file <- function(path = Sys.getenv("SMARTSPEED_FILE", unset = "")) {
  if (!nzchar(path) || !file.exists(path)) {
    if (exists("write_refresh_log", mode = "function")) {
      write_refresh_log("import_smartspeed_local", "skipped", list(reason = "No SMARTSPEED_FILE found."))
    }
    return(invisible(list(ok = FALSE, message = "No SmartSpeed local file found.")))
  }
  ext <- tolower(tools::file_ext(path))
  raw <- switch(
    ext,
    rds = readRDS(path),
    csv = utils::read.csv(path, stringsAsFactors = FALSE),
    txt = utils::read.csv(path, stringsAsFactors = FALSE),
    stop("Unsupported SmartSpeed file type: ", ext, call. = FALSE)
  )
  sprint <- build_sprint_session_level(raw = raw)
  if (exists("write_refresh_log", mode = "function")) {
    write_refresh_log("import_smartspeed_local", "ok", list(file = basename(path), rows = nrow(sprint)))
  }
  invisible(list(ok = TRUE, data = sprint, message = "SmartSpeed local file imported."))
}
