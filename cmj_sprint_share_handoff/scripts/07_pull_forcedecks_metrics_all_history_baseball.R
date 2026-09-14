source("scripts/01_config.R")
library(httr2)
library(jsonlite)
library(tidyverse)
library(lubridate)

# ----------------------------
# Files
# ----------------------------
tests_path    <- file.path(DATA_DIR, "force_tests_all_history.rds")
profiles_path <- file.path(DATA_DIR, "profiles.rds")

out_metrics_rds <- file.path(DATA_DIR, "force_metrics_long_all_history_baseball.rds")
out_metrics_csv <- file.path(DATA_DIR, "force_metrics_long_all_history_baseball.csv")

partial_list_file <- file.path(DATA_DIR, "force_metrics_all_history_partial_list.rds")

FORCE_FULL_REPULL_METRICS <- tolower(Sys.getenv("VALD_FORCE_FULL_REPULL_METRICS", "false")) %in% c("true", "1", "yes", "y")
PULL_MAX_ACTIVE <- as.integer(Sys.getenv("VALD_METRICS_PULL_MAX_ACTIVE", "6"))

stopifnot(file.exists(tests_path))
stopifnot(file.exists(profiles_path))

tests_df    <- readRDS(tests_path)
profiles_df <- readRDS(profiles_path)

if (!("profileId" %in% names(profiles_df))) {
  stop("profiles.rds is missing profileId column.")
}
if (!("externalId" %in% names(profiles_df))) {
  profiles_df$externalId <- ""
}

# Handle multiple profile schemas:
# - givenName/familyName (raw profile pull)
# - athleteName (processed profile table)
if ("athleteName" %in% names(profiles_df)) {
  profiles_df$athleteName <- as.character(profiles_df$athleteName)
} else {
  given <- if ("givenName" %in% names(profiles_df)) as.character(profiles_df$givenName) else ""
  family <- if ("familyName" %in% names(profiles_df)) as.character(profiles_df$familyName) else ""
  profiles_df$athleteName <- trimws(paste(given, family))
}

# ----------------------------
# Baseball roster filter (externalId pattern)
# ----------------------------
baseball_profiles <- profiles_df %>%
  mutate(externalId = as.character(externalId)) %>%
  filter(grepl("^TXST-Baseball-[0-9]{3}$", externalId)) %>%
  mutate(athleteName = ifelse(is.na(athleteName), "", athleteName)) %>%
  select(profileId, externalId, athleteName)

# Baseball tests
force_baseball_tests <- tests_df %>%
  inner_join(baseball_profiles, by = "profileId")

cat("Baseball tests:", nrow(force_baseball_tests), "\n")
cat("Unique baseball athletes with ForceDecks tests:", n_distinct(force_baseball_tests$externalId), "\n")

saveRDS(force_baseball_tests, file.path(DATA_DIR, "force_tests_baseball_all_history_named.rds"))

test_ids_all <- force_baseball_tests %>% distinct(testId) %>% pull(testId)
cat("Unique baseball testIds:", length(test_ids_all), "\n")

# ----------------------------
# Cross-run incremental skip: only pull trials/metrics for testIds we don't
# already have in the saved output. This is the dominant cost of a refresh --
# without it, every run re-fetches all-history trials one HTTP call per test.
# ----------------------------
existing_final <- NULL
existing_test_ids <- character(0)
if (!FORCE_FULL_REPULL_METRICS && file.exists(out_metrics_rds)) {
  existing_final <- tryCatch(readRDS(out_metrics_rds), error = function(e) NULL)
  if (!is.null(existing_final) && "testId" %in% names(existing_final)) {
    existing_test_ids <- unique(as.character(existing_final$testId))
  }
}
if (FORCE_FULL_REPULL_METRICS) {
  cat("VALD_FORCE_FULL_REPULL_METRICS=true -> ignoring existing metrics file and re-pulling all baseball tests.\n")
}

test_ids <- setdiff(as.character(test_ids_all), existing_test_ids)
cat("Already have metrics for:", length(existing_test_ids), "testIds | New to pull:", length(test_ids), "\n")

# ----------------------------
# AUTH
# ----------------------------
get_token <- function() {
  vald_get_token()
}

team_id <- Sys.getenv("VALD_TEAM_ID", Sys.getenv("VALD_TENANT_ID", Sys.getenv("VALD_DUENDE_ID")))

trials_url_for <- function(test_id) {
  paste0(
    "https://prd-use-api-extforcedecks.valdperformance.com",
    "/v2019q3/teams/", team_id,
    "/tests/", test_id,
    "/trials"
  )
}

build_trial_request <- function(test_id, token) {
  httr2::request(trials_url_for(test_id)) %>%
    httr2::req_headers(Authorization = paste("Bearer", token)) %>%
    httr2::req_timeout(30) %>%
    httr2::req_throttle(capacity = 10, fill_time_s = 1) %>%
    httr2::req_retry(
      max_tries = 4,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504)
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE)
}

parse_trials_response <- function(resp, test_id) {
  if (inherits(resp, "error") || inherits(resp, "httr2_failure")) {
    message("FAILED testId=", test_id, " | ", conditionMessage(resp))
    return(list(status = "error", data = NULL))
  }
  sc <- httr2::resp_status(resp)
  if (sc %in% c(204, 404)) return(list(status = "empty", data = NULL))
  if (sc %in% c(401, 403)) return(list(status = "auth", data = NULL))
  if (sc != 200) {
    message("Non-200 for testId=", test_id, " HTTP ", sc)
    return(list(status = "error", data = NULL))
  }
  trials <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE),
    error = function(e) {
      message("FAILED to parse response for testId=", test_id, " | ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(trials) || nrow(trials) == 0) return(list(status = "empty", data = NULL))

  long <- tryCatch({
    trials %>%
      mutate(testId = test_id) %>%
      select(
        testId,
        trialId = id,
        recordedUTC,
        recordedOffset,
        recordedTimezone,
        trialStartTime = startTime,
        trialEndTime = endTime,
        trialLimb = limb,
        results
      ) %>%
      tidyr::unnest(results) %>%
      transmute(
        testId,
        trialId,
        recordedUTC,
        recordedOffset,
        recordedTimezone,
        trialStartTime,
        trialEndTime,
        trialLimb,
        metricKey  = definition.result,
        metricName = definition.name,
        metricUnit = definition.unit,
        value
      )
  }, error = function(e) {
    message("FAILED to flatten trials for testId=", test_id, " | ", conditionMessage(e))
    NULL
  })
  list(status = if (is.null(long)) "error" else "ok", data = long)
}

# ----------------------------
# Resume logic: only trust a saved partial list if it was built against the
# EXACT SAME test_ids vector we're about to pull (same set, same order).
# Otherwise a stale partial file could silently pair cached results with the
# wrong testId.
# ----------------------------
all_metrics <- vector("list", length(test_ids))
start_i <- 1

if (length(test_ids) > 0 && file.exists(partial_list_file)) {
  partial_state <- tryCatch(readRDS(partial_list_file), error = function(e) NULL)
  if (is.list(partial_state) && identical(partial_state$test_ids, test_ids)) {
    all_metrics <- partial_state$metrics
    start_i <- length(Filter(Negate(is.null), all_metrics)) + 1
    if (start_i > length(test_ids)) start_i <- length(test_ids) + 1L
    cat("Resuming metrics pull from test", start_i, "of", length(test_ids), "\n")
  } else {
    cat("Discarding stale partial pull state (test_id set changed since last attempt).\n")
    unlink(partial_list_file)
  }
}

save_partial <- function() {
  saveRDS(list(test_ids = test_ids, metrics = all_metrics), partial_list_file)
}

# ----------------------------
# Pull trials for new test_ids only, in bounded-concurrency batches, with
# retry/backoff on transient errors baked into each request (see
# build_trial_request()).
# ----------------------------
if (length(test_ids) == 0) {
  cat("No new baseball tests need metrics pulled -- reusing existing metrics file.\n")
} else if (start_i <= length(test_ids)) {
  token <- get_token()
  pending_idx <- seq(from = start_i, to = length(test_ids))
  cat("Pulling trials/metrics for", length(pending_idx), "new baseball test(s), up to",
      PULL_MAX_ACTIVE, "at a time...\n")

  auth_retry_ids <- character(0)

  batch_size <- 200L
  for (batch_start in seq(1, length(pending_idx), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1L, length(pending_idx))
    idx_batch <- pending_idx[batch_start:batch_end]
    ids_batch <- test_ids[idx_batch]

    reqs <- lapply(ids_batch, build_trial_request, token = token)
    resps <- httr2::req_perform_parallel(reqs, max_active = PULL_MAX_ACTIVE, on_error = "continue", progress = FALSE)

    for (k in seq_along(idx_batch)) {
      i <- idx_batch[k]
      tid <- ids_batch[k]
      parsed <- parse_trials_response(resps[[k]], tid)
      if (identical(parsed$status, "auth")) {
        auth_retry_ids <- c(auth_retry_ids, tid)
        all_metrics[i] <- list(NULL)
      } else {
        all_metrics[i] <- list(parsed$data)
      }
    }

    save_partial()
    cat("Pulled", batch_end, "of", length(pending_idx), "new tests (batch)\n")
  }

  # One fallback sequential pass, with a freshly refreshed token, for any
  # requests that came back 401/403 (token likely expired mid-pull).
  if (length(auth_retry_ids) > 0) {
    cat("Refreshing token and retrying", length(auth_retry_ids), "test(s) that returned an auth error...\n")
    token <- get_token()
    for (tid in auth_retry_ids) {
      i <- which(test_ids == tid)[[1]]
      req <- build_trial_request(tid, token)
      resp <- httr2::req_perform(req)
      parsed <- parse_trials_response(resp, tid)
      all_metrics[i] <- list(parsed$data)
    }
    save_partial()
  }
} else {
  cat("Partial pull state already covers all", length(test_ids), "new test(s).\n")
}

new_metrics_long <- if (length(test_ids) == 0) tibble() else bind_rows(all_metrics)
cat("New metric rows pulled this run:", nrow(new_metrics_long), "\n")

# ----------------------------
# Join athlete + test info onto newly-pulled rows only (cheap in-memory join)
# ----------------------------
test_info <- force_baseball_tests %>%
  select(testId, profileId, externalId, athleteName, testType, recordedDateUtc, modifiedDateUtc, weight)

new_metrics_joined <- if (nrow(new_metrics_long) == 0) {
  tibble()
} else {
  new_metrics_long %>%
    left_join(test_info, by = "testId") %>%
    mutate(
      recordedUTC     = as.POSIXct(recordedUTC, tz = "UTC"),
      recordedDateUtc = as.POSIXct(recordedDateUtc, tz = "UTC")
    )
}

# ----------------------------
# Combine with existing history and dedupe (guards against any accidental
# overlap between existing_final and newly-pulled rows).
# ----------------------------
metrics_long_joined <- dplyr::bind_rows(existing_final, new_metrics_joined) %>%
  distinct(testId, trialId, metricKey, .keep_all = TRUE)

stopifnot(nrow(metrics_long_joined) > 0)
cat("Total metric rows:", nrow(metrics_long_joined), "\n")

# ----------------------------
# Save outputs
# ----------------------------
saveRDS(metrics_long_joined, out_metrics_rds)
write_csv(metrics_long_joined, out_metrics_csv)

# Reaching this point means the pull loop (if any) finished without a fatal
# error, so any in-progress partial-pull state is no longer needed. Any
# individual test that still failed after retries is simply absent from
# metrics_long_joined and will be picked up again by the skip-logic on the
# next run (it won't be in existing_test_ids next time).
if (file.exists(partial_list_file)) unlink(partial_list_file)

cat("Saved:\n", out_metrics_rds, "\n", out_metrics_csv, "\n")
