# Safe VALD API wrappers.
#
# Shiny should read processed local files only. These helpers are intended for
# run_pipeline.R, admin refresh actions, and local data refresh scripts.

check_valdr_available <- function() {
  pkg_path <- suppressWarnings(tryCatch(find.package("valdr", quiet = TRUE), error = function(e) character(0)))
  ok <- length(pkg_path) > 0
  list(
    available = ok,
    version = if (ok) as.character(utils::packageVersion("valdr")) else NA_character_,
    message = if (ok) "valdr is installed." else "valdr is not installed; using existing local data."
  )
}

vald_credentials_status <- function() {
  client_id <- Sys.getenv("VALD_USERNAME", unset = "")
  if (!nzchar(client_id)) client_id <- Sys.getenv("VALD_CLIENT_ID", unset = "")
  if (!nzchar(client_id)) client_id <- Sys.getenv("USERNAME", unset = "")
  client_secret <- Sys.getenv("VALD_PASSWORD", unset = "")
  if (!nzchar(client_secret)) client_secret <- Sys.getenv("VALD_CLIENT_SECRET", unset = "")
  if (!nzchar(client_secret)) client_secret <- Sys.getenv("PASSWORD", unset = "")
  team_id <- Sys.getenv("VALD_TEAM_ID", unset = "")
  if (!nzchar(team_id)) team_id <- Sys.getenv("VALD_TENANT_ID", unset = "")
  if (!nzchar(team_id)) team_id <- Sys.getenv("VALD_DUENDE_ID", unset = "")
  if (!nzchar(team_id)) team_id <- Sys.getenv("DUENDE_ID", unset = "")
  region <- Sys.getenv("VALD_REGION", unset = "")
  if (!nzchar(region)) region <- "use"
  present <- c(
    VALD_CLIENT_ID = nzchar(client_id),
    VALD_CLIENT_SECRET = nzchar(client_secret),
    VALD_TEAM_ID = nzchar(team_id)
  )
  list(
    ok = all(present),
    present = present,
    team_id = if (nzchar(team_id)) team_id else NA_character_,
    tenant_id = if (nzchar(team_id)) team_id else NA_character_,
    region = region,
    message = if (all(present)) {
      "VALD credentials are available from environment variables."
    } else {
      paste0("Missing VALD credential variables: ", paste(names(present)[!present], collapse = ", "))
    }
  )
}

setup_vald_credentials_instructions <- function() {
  paste(
    "Set VALD credentials outside the project files.",
    "Preferred local setup:",
    "  Sys.setenv(VALD_CLIENT_ID = '...',",
    "             VALD_CLIENT_SECRET = '...',",
    "             VALD_TEAM_ID = '...',",
    "             VALD_REGION = 'use')",
    "If your local credential names came from VALD as username/password/Duende ID, map them to:",
    "  VALD_CLIENT_ID, VALD_CLIENT_SECRET, and VALD_TEAM_ID respectively.",
    "For valdr only:",
    "  valdr::set_credentials(client_id = Sys.getenv('VALD_CLIENT_ID'),",
    "                         client_secret = Sys.getenv('VALD_CLIENT_SECRET'),",
    "                         tenant_id = Sys.getenv('VALD_TEAM_ID'),",
    "                         region = Sys.getenv('VALD_REGION', 'use'))",
    "Environment variables supported by the pipeline:",
    "  VALD_CLIENT_ID, VALD_CLIENT_SECRET, VALD_TEAM_ID, VALD_REGION",
    "Legacy VALD_USERNAME/VALD_PASSWORD/VALD_DUENDE_ID and USERNAME/PASSWORD/DUENDE_ID are also accepted as local aliases.",
    sep = "\n"
  )
}

log_vald_pull <- function(source, status = "ok", row_count = NA_integer_,
                          max_test_date = as.Date(NA), error_message = NA_character_) {
  details <- list(
    source = source,
    rows = ifelse(is.na(row_count), "", as.character(row_count)),
    max_test_date = ifelse(is.na(max_test_date), "", as.character(max_test_date)),
    error = ifelse(is.na(error_message), "", as.character(error_message))
  )
  if (exists("write_refresh_log", mode = "function")) {
    write_refresh_log(paste0("vald_", source), status, details)
  }
  invisible(details)
}

configure_valdr_credentials <- function() {
  availability <- check_valdr_available()
  if (!availability$available) return(list(ok = FALSE, message = availability$message))
  creds <- vald_credentials_status()
  if (!creds$ok) return(list(ok = FALSE, message = creds$message))
  fn <- suppressWarnings(tryCatch(getExportedValue("valdr", "set_credentials"), error = function(e) NULL))
  if (is.null(fn)) return(list(ok = TRUE, message = "valdr has no exported set_credentials(); continuing."))
  client_id <- Sys.getenv("VALD_USERNAME", unset = Sys.getenv("VALD_CLIENT_ID", unset = Sys.getenv("USERNAME", unset = "")))
  client_secret <- Sys.getenv("VALD_PASSWORD", unset = Sys.getenv("VALD_CLIENT_SECRET", unset = Sys.getenv("PASSWORD", unset = "")))
  team_id <- Sys.getenv("VALD_TEAM_ID", unset = Sys.getenv("VALD_TENANT_ID", unset = Sys.getenv("VALD_DUENDE_ID", unset = Sys.getenv("DUENDE_ID", unset = ""))))
  args <- list(
    client_id = client_id,
    client_secret = client_secret,
    tenant_id = team_id
  )
  if (nzchar(creds$region) && "region" %in% names(formals(fn))) args$region <- creds$region
  tryCatch({
    do.call(fn, args[names(args) %in% names(formals(fn))])
    list(ok = TRUE, message = "valdr credentials configured.")
  }, error = function(e) {
    list(ok = FALSE, message = paste0("valdr credential setup failed: ", conditionMessage(e)))
  })
}

safe_valdr_call <- function(function_names, source, ..., save_path = NULL) {
  availability <- check_valdr_available()
  if (!availability$available) {
    log_vald_pull(source, "skipped", error_message = availability$message)
    return(list(ok = FALSE, data = NULL, message = availability$message))
  }
  creds <- configure_valdr_credentials()
  if (!isTRUE(creds$ok)) {
    log_vald_pull(source, "skipped", error_message = creds$message)
    return(list(ok = FALSE, data = NULL, message = creds$message))
  }
  fn <- NULL
  fn_name <- NA_character_
  for (candidate in function_names) {
    fn <- suppressWarnings(tryCatch(getExportedValue("valdr", candidate), error = function(e) NULL))
    if (!is.null(fn)) {
      fn_name <- candidate
      break
    }
  }
  if (is.null(fn)) {
    msg <- paste0("No compatible valdr function found for ", source, ": ", paste(function_names, collapse = ", "))
    log_vald_pull(source, "skipped", error_message = msg)
    return(list(ok = FALSE, data = NULL, message = msg))
  }
  out <- tryCatch(do.call(fn, list(...)), error = function(e) e)
  if (inherits(out, "error")) {
    msg <- conditionMessage(out)
    log_vald_pull(source, "error", error_message = msg)
    return(list(ok = FALSE, data = NULL, message = msg, function_name = fn_name))
  }
  if (!is.null(save_path)) {
    ensure_dir(dirname(save_path))
    saveRDS(out, save_path)
  }
  n <- if (is.data.frame(out)) nrow(out) else if (is.list(out)) length(out) else NA_integer_
  max_dt <- tryCatch({
    if (is.data.frame(out)) {
      date_col <- names(out)[grepl("date|time|recorded|modified", names(out), ignore.case = TRUE)][[1]]
      d <- as_date_safely2(out[[date_col]])
      if (all(is.na(d))) as.Date(NA) else max(d, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
  }, error = function(e) as.Date(NA))
  log_vald_pull(source, "ok", n, max_dt)
  list(ok = TRUE, data = out, message = paste0("Pulled ", source, " using valdr::", fn_name, "."), function_name = fn_name)
}

pull_vald_profiles <- function(save = TRUE) {
  safe_valdr_call(
    c("get_profiles_only", "get_profiles"),
    "profiles",
    save_path = if (save) file.path(legacy_data_dir(), "profiles.rds") else NULL
  )
}

pull_vald_profile_mappings <- function(save = TRUE) {
  safe_valdr_call(
    c("get_profile_mappings", "get_profiles_only", "get_profiles"),
    "profile_mappings",
    save_path = if (save) file.path(gold_data_dir(), "vald_profile_mappings.rds") else NULL
  )
}

pull_forcedecks_full <- function(save = TRUE) {
  safe_valdr_call(
    c("get_forcedecks_data", "get_forcedecks_summary", "get_forcedecks_tests"),
    "forcedecks_full",
    save_path = if (save) file.path(gold_data_dir(), "vald_forcedecks_api_raw.rds") else NULL
  )
}

pull_forcedecks_tests_trials <- function(save = TRUE) {
  safe_valdr_call(
    c("get_forcedecks_tests_trials", "get_forcedecks_trials", "get_forcedecks_tests"),
    "forcedecks_tests_trials",
    save_path = if (save) file.path(gold_data_dir(), "vald_forcedecks_tests_trials_raw.rds") else NULL
  )
}

pull_forcedecks_result_definitions <- function(save = TRUE) {
  safe_valdr_call(
    c("get_forcedecks_result_definitions", "get_result_definitions"),
    "forcedecks_result_definitions",
    save_path = if (save) file.path(gold_data_dir(), "vald_forcedecks_result_definitions.rds") else NULL
  )
}

pull_smartspeed_full <- function(save = TRUE, start_date = NULL) {
  # Passed explicitly rather than relying on valdr::set_start_date()'s global
  # state, which gets cleared each time safe_valdr_call() re-runs
  # configure_valdr_credentials() -> valdr::set_credentials() before the pull.
  #
  # Unlike the ForceDecks tests/metrics scripts (06/07), this had no
  # incremental resume of its own -- every call re-pulled full history from
  # 2010, every time, and every call's result REPLACED the saved raw file
  # outright. That's fine for a once-daily scheduled job, but not for a
  # staff-facing Refresh button clicked repeatedly through the day: a
  # NULL/unset start_date now resumes from the latest modifiedDateUtc already
  # in the raw file (same field/pattern script 06 resumes ForceDecks tests
  # from), and the newly-pulled tests/profiles are MERGED into the existing
  # raw file (deduping by id/profileId, newest wins) rather than replacing it
  # -- otherwise only that call's incremental slice would remain, silently
  # dropping every earlier test build_sprint_session_level() had already
  # turned into gold data. An explicit start_date argument still overrides
  # the resume-date computation below, but merge-before-save still happens.
  existing_raw_path <- file.path(gold_data_dir(), "smartspeed_api_raw.rds")
  existing_raw <- if (file.exists(existing_raw_path)) tryCatch(readRDS(existing_raw_path), error = function(e) NULL) else NULL
  existing_tests <- if (!is.null(existing_raw) && is.list(existing_raw) && is.data.frame(existing_raw$tests)) existing_raw$tests else NULL

  if (is.null(start_date)) {
    force_full <- tolower(Sys.getenv("VALD_FORCE_FULL_REPULL_SMARTSPEED", "false")) %in% c("true", "1", "yes", "y")
    start_date <- "2010-01-01T00:00:00Z"
    if (!force_full && !is.null(existing_tests) && "modifiedDateUtc" %in% names(existing_tests) && nrow(existing_tests) > 0) {
      max_modified <- suppressWarnings(max(existing_tests$modifiedDateUtc, na.rm = TRUE))
      if (!is.na(max_modified) && nzchar(max_modified)) {
        parsed <- suppressWarnings(lubridate::ymd_hms(sub("Z$", "", max_modified), tz = "UTC", quiet = TRUE))
        # Whole seconds only, no nudge forward: unlike the ForceDecks REST
        # endpoint script 06 resumes from, valdr's own get_smartspeed_tests()
        # client-side-validates start_date against
        # ^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$ -- no fractional seconds
        # accepted at all, so there's no sub-second granularity to nudge by.
        # Truncating down to the whole second of the last known record and
        # re-requesting from THAT second (inclusive) can re-return a test we
        # already have, but that's harmless -- it gets deduped by id below --
        # whereas rounding/nudging forward a whole second risks silently
        # skipping a real test modified in that same second.
        if (!is.na(parsed)) start_date <- format(parsed, "%Y-%m-%dT%H:%M:%SZ")
      }
    }
  }

  res <- safe_valdr_call(
    c("get_smartspeed_data", "get_smartspeed_tests", "get_smartspeed_tests_only"),
    "smartspeed_full",
    start_date = start_date,
    save_path = NULL  # saved below, after merging with any existing raw file
  )
  if (!isTRUE(res$ok)) return(res)

  # VALD's SmartSpeed API returns some numeric-looking fields (maxVelocity
  # among them) typed inconsistently across pages/pulls -- character in one
  # response, double in another, depending on what values that page happened
  # to contain. bind_rows() errors on that type clash. Everything here is
  # re-coerced to its real type downstream in standardize_smartspeed_export()
  # (via as.numeric()/as.character()) regardless of what type it arrives as,
  # so flattening every column to character before merging is safe and sidesteps
  # the whole class of type mismatch rather than chasing it column by column.
  coerce_chr_df <- function(df) { if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df); dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character)) }

  merged <- res$data
  if (!is.null(existing_raw) && is.list(existing_raw)) {
    if (!is.null(existing_tests) && "id" %in% names(existing_tests) && is.data.frame(merged$tests) && "id" %in% names(merged$tests)) {
      merged$tests <- dplyr::distinct(dplyr::bind_rows(coerce_chr_df(merged$tests), coerce_chr_df(existing_tests)), id, .keep_all = TRUE)
    } else if (is.null(merged$tests) || !is.data.frame(merged$tests)) {
      merged$tests <- existing_tests
    }
    existing_profiles <- if (is.data.frame(existing_raw$profiles)) existing_raw$profiles else NULL
    if (!is.null(existing_profiles) && "profileId" %in% names(existing_profiles) && is.data.frame(merged$profiles) && "profileId" %in% names(merged$profiles)) {
      merged$profiles <- dplyr::distinct(dplyr::bind_rows(coerce_chr_df(merged$profiles), coerce_chr_df(existing_profiles)), profileId, .keep_all = TRUE)
    } else if (is.null(merged$profiles) || !is.data.frame(merged$profiles)) {
      merged$profiles <- existing_profiles
    }
  }

  if (isTRUE(save)) {
    ensure_dir(gold_data_dir())
    saveRDS(merged, existing_raw_path)
  }
  res$data <- merged
  res
}

pull_smartspeed_tests_only <- function(save = TRUE, start_date = "2010-01-01T00:00:00Z") {
  safe_valdr_call(
    c("get_smartspeed_tests_only", "get_smartspeed_tests"),
    "smartspeed_tests_only",
    start_date = start_date,
    save_path = if (save) file.path(gold_data_dir(), "smartspeed_tests_raw.rds") else NULL
  )
}

pull_smartspeed_test_details <- function(test_ids, save_dir = file.path(gold_data_dir(), "smartspeed_details_raw")) {
  if (is.null(test_ids) || length(test_ids) == 0) {
    msg <- "No SmartSpeed test IDs supplied for detail pull."
    log_vald_pull("smartspeed_details", "skipped", error_message = msg)
    return(list(ok = FALSE, data = list(), message = msg))
  }
  ensure_dir(save_dir)
  results <- list()
  for (test_id in unique(as.character(test_ids))) {
    res <- safe_valdr_call(c("get_smartspeed_test_details", "get_smartspeed_test_detail"), "smartspeed_details", test_id = test_id)
    if (isTRUE(res$ok)) {
      results[[test_id]] <- res$data
      saveRDS(res$data, file.path(save_dir, paste0(test_id, ".rds")))
    }
  }
  list(ok = length(results) > 0, data = results, message = paste0("SmartSpeed details pulled: ", length(results)))
}

refresh_vald_forcedecks <- function() {
  source(file.path(root_dir(), "scripts", "08_refresh_forcedecks_baseball.R"), local = TRUE)
  invisible(TRUE)
}
