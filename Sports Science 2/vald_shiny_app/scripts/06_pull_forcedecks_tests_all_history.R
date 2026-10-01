source("scripts/01_config.R")
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

tenant_id <- Sys.getenv("VALD_TEAM_ID", Sys.getenv("VALD_TENANT_ID", Sys.getenv("VALD_DUENDE_ID")))
out_file  <- file.path(DATA_DIR, "force_tests_all_history.rds")
FORCE_FULL_REPULL <- tolower(Sys.getenv("VALD_FORCE_FULL_REPULL_TESTS", "false")) %in% c("true", "1", "yes", "y")

# ----------------------------
# AUTH helper (so we can refresh token if needed)
# ----------------------------
get_token <- function() {
  vald_get_token()
}

token <- get_token()

# ----------------------------
# Resume cursor from file if it exists
# ----------------------------
if (file.exists(out_file) && !FORCE_FULL_REPULL && nrow(readRDS(out_file)) > 0) {
  tests_existing <- readRDS(out_file)
  tests_existing <- as_tibble(tests_existing)
  
  last_modified <- max(tests_existing$modifiedDateUtc, na.rm = TRUE)
  last_mod_time <- ymd_hms(sub("Z$", "", last_modified), tz = "UTC", quiet = TRUE)
  modified_from <- format(last_mod_time - days(1), "%Y-%m-%dT%H:%M:%OS3Z")
  
  seen_ids <- unique(tests_existing$testId)
  
  cat("Resuming from:", modified_from, "\n")
  cat("Existing tests:", nrow(tests_existing), "\n")
} else {
  if (FORCE_FULL_REPULL && file.exists(out_file)) {
    cat("VALD_FORCE_FULL_REPULL_TESTS=true -> ignoring existing force_tests_all_history.rds and rebuilding from scratch.\n")
  }
  tests_existing <- tibble()
  seen_ids <- character()
  modified_from <- "2010-01-01T00:00:00.000Z"
  cat("Starting full history from:", modified_from, "\n")
}

# ----------------------------
# Request helper with retry/backoff
# ----------------------------
get_tests_page <- function(modified_from_utc) {
  url <- paste0(
    paste0("https://prd-", Sys.getenv("VALD_REGION", "use"), "-api-extforcedecks.valdperformance.com/tests"),
    "?tenantId=", tenant_id,
    "&modifiedFromUtc=", URLencode(modified_from_utc, reserved = TRUE)
  )
  
  resp <- GET(url, add_headers(Authorization = paste("Bearer", token)), httr::timeout(60))
  sc <- status_code(resp)
  
  # Success / done
  if (sc == 204) return(list(done = TRUE, tests = NULL, last_modified = NULL))
  if (sc == 200) {
    payload <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    page <- payload$tests
    if (is.null(page) || nrow(page) == 0) return(list(done = TRUE, tests = NULL, last_modified = NULL))
    last_modified_raw <- max(page$modifiedDateUtc)
    return(list(done = FALSE, tests = page, last_modified = last_modified_raw))
  }
  
  # Token expired/invalid -> refresh token once
  if (sc %in% c(401, 403)) {
    message("Auth error HTTP ", sc, " — refreshing token and retrying once.")
    token <<- get_token()
    resp2 <- GET(url, add_headers(Authorization = paste("Bearer", token)), httr::timeout(60))
    sc2 <- status_code(resp2)
    
    if (sc2 == 204) return(list(done = TRUE, tests = NULL, last_modified = NULL))
    if (sc2 == 200) {
      payload <- fromJSON(content(resp2, "text", encoding = "UTF-8"), flatten = TRUE)
      page <- payload$tests
      if (is.null(page) || nrow(page) == 0) return(list(done = TRUE, tests = NULL, last_modified = NULL))
      last_modified_raw <- max(page$modifiedDateUtc)
      return(list(done = FALSE, tests = page, last_modified = last_modified_raw))
    }
    
    # if still failing, fall through to error handling
    sc <- sc2
    resp <- resp2
  }
  
  # Rate limit / transient server errors -> signal to caller
  if (sc %in% c(408, 429, 500, 502, 503, 504)) {
    return(list(retryable = TRUE, status = sc))
  }
  
  # Anything else: not expected
  body_txt <- tryCatch(content(resp, "text", encoding = "UTF-8"), error = function(e) "")
  stop("Non-retryable HTTP ", sc, " | body: ", substr(body_txt, 1, 300))
}

# ----------------------------
# Main loop
# ----------------------------
page_count <- 0
no_new_count <- 0
last_cursor <- modified_from

repeat {
  page_count <- page_count + 1
  
  # retry loop
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    res <- get_tests_page(modified_from)
    
    if (isTRUE(res$done)) {
      cat("No more pages. Done.\n")
      break
    }
    
    if (isTRUE(res$retryable)) {
      if (attempt >= 6) stop("VALD tests request failed after 6 attempts.")
      wait <- min(60, 2 ^ attempt)  # exponential backoff capped at 60s
      message("Retryable HTTP ", res$status, " on attempt ", attempt, " — sleeping ", wait, "s then retrying.")
      Sys.sleep(wait)
      next
    }
    
    # got data
    page <- res$tests
    last_modified_raw <- res$last_modified
    break
  }
  
  if (isTRUE(res$done)) break
  
  # de-dupe and append
  page_new <- page %>% filter(!testId %in% seen_ids)
  if (nrow(page_new) > 0) {
    tests_existing <- bind_rows(tests_existing, page_new)
    seen_ids <- unique(c(seen_ids, page_new$testId))
    no_new_count <- 0
  } else {
    no_new_count <- no_new_count + 1
  }
  
  tests_existing <- bind_rows(as_tibble(page), tests_existing) %>% distinct(testId, .keep_all = TRUE)

  # advance cursor using the true last_modified from the payload
  # keep exact timestamp (no +1ms) to avoid skipping records that share a boundary timestamp
  last_mod_time <- ymd_hms(sub("Z$", "", last_modified_raw), tz = "UTC", quiet = TRUE)
  if (is.na(last_mod_time)) stop("Could not parse modifiedDateUtc: ", last_modified_raw)
  
  modified_from <- format(last_mod_time, "%Y-%m-%dT%H:%M:%OS3Z")

  # save progress every page
  saveRDS(tests_existing, out_file)
  
  if (page_count %% 5 == 0) {
    cat("Pages:", page_count,
        "| Total tests:", nrow(tests_existing),
        "| next modifiedFromUtc:", modified_from, "\n")
  }

  # guard against cursor loops or repeated duplicate pages
  if (identical(modified_from, last_cursor) && no_new_count >= 3) {
    cat("Stopping: cursor did not advance and no new tests were found for 3 consecutive pages.\n")
    break
  }
  if (no_new_count >= 8) {
    cat("Stopping: no new tests found for 8 consecutive pages.\n")
    break
  }
  last_cursor <- modified_from
}

cat("FINAL total tests:", nrow(tests_existing), "\n")
cat("ModifiedDate range:\n")
print(range(tests_existing$modifiedDateUtc))
cat("Saved to:\n", out_file, "\n")

saveRDS(tests_existing, out_file)
