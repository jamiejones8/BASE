# Run from the app folder: Rscript tests/run_tests.R
.libPaths(c(normalizePath(".R-library"), .libPaths()))
Sys.setenv(VALD_AUTO_REFRESH = "false", VALD_REFRESH_WORKER = "true", VALD_APP_ROOT = normalizePath(getwd()))
source("dashboard.R")
library(testthat)
for (f in list.files(recursive=TRUE, pattern="\\.R$", full.names=TRUE)) {
  if (!grepl(".R-library", f, fixed=TRUE)) parse(f)
}
# Isolate all test writes from real dashboard data.
refresh_dir <- tempfile("refresh-test-"); dir.create(refresh_dir)
snapshot_file <- file.path(refresh_dir,"dashboard.rds")
state_file <- file.path(refresh_dir,"status.rds")
lock_file <- file.path(refresh_dir,"refresh.lock")
old_refresh <- run_live_vald_refresh
old_load <- load_shared_data
load_shared_data <- function() {shared_data$roster <- data.frame(profileId="test"); shared_data$session_summary <- data.frame(value=42)}
test_that("daily refresh respects success intervals and failure backoff", {
  now <- Sys.time()
  expect_true(refresh_due(list(),now))
  expect_false(refresh_due(list(success=now,attempted=now),now))
  expect_false(refresh_due(list(attempted=now-60),now))
  expect_true(refresh_due(list(success=now-90000,attempted=now-4000),now))
})
test_that("failed pulls retain the last published snapshot", {
  atomic_rds(list(marker="previous"),snapshot_file)
  run_live_vald_refresh <<- function() list(ok=FALSE,forcedecks_ok=FALSE,message="Mock API outage")
  result <- run_refresh_job(TRUE)
  expect_false(result$ok)
  expect_identical(readRDS(snapshot_file)$marker,"previous")
  expect_true(is.na(result$success))
  run_live_vald_refresh <<- function() list(ok=TRUE,forcedecks_ok=TRUE,message="Mock success")
  result <- run_refresh_job(TRUE)
  expect_true(result$ok)
  expect_equal(readRDS(snapshot_file)$session_summary$value,42)
  expect_false(is.na(result$success))
})
test_that("cross-process lock rejects overlapping refreshes", {
  child <- callr::r_bg(function(path) {l <- filelock::lock(path); cat("locked\n"); flush.console(); Sys.sleep(20)},
    args=list(lock_file),libpath=.libPaths(),stdout="|")
  on.exit(child$kill())
  for(i in 1:100) {if(length(child$read_output_lines())) break; Sys.sleep(.05)}
  expect_match(run_refresh_job(TRUE)$message,"already running")
  child$kill()
})
run_live_vald_refresh <- old_refresh
load_shared_data <- old_load

test_that("test import retains history and replaces modified test metadata", {
  env <- new.env(parent=globalenv())
  env$DATA_DIR <- tempfile("tests-"); dir.create(env$DATA_DIR)
  old <- data.frame(testId="old", modifiedDateUtc="2026-09-20T12:00:00Z", notes="before")
  saveRDS(old,file.path(env$DATA_DIR,"force_tests_all_history.rds"))
  env$source <- function(...) invisible(NULL)
  env$vald_get_token <- function() "mock"
  pages <- list(data.frame(testId=c("old","new"),modifiedDateUtc=c("2026-09-21T12:00:00Z","2026-09-21T13:00:00Z"),notes=c("after","new")))
  env$GET <- function(...) {
    page <- if(length(pages)) pages[[1]] else NULL
    if(length(pages)) pages <<- pages[-1]
    structure(list(url="https://mock.invalid",headers=list(`Content-Type`="application/json"),status_code=if(is.null(page)) 204L else 200L,
      content=charToRaw(jsonlite::toJSON(list(tests=page),auto_unbox=TRUE))),class="response")
  }
  sys.source("scripts/06_pull_forcedecks_tests_all_history.R",envir=env)
  result <- readRDS(file.path(env$DATA_DIR,"force_tests_all_history.rds"))
  expect_equal(nrow(result),2)
  expect_identical(result$notes[result$testId=="old"],"after")
})
test_that("modified tests replace old metrics rather than retaining stale trials", {
  env <- new.env(parent=globalenv())
  env$DATA_DIR <- tempfile("metrics-"); dir.create(env$DATA_DIR)
  env$source <- function(...) invisible(NULL)
  env$vald_get_token <- function() "mock"
  profiles <- data.frame(profileId="p1",externalId="TXST-Baseball-001",athleteName="Test Athlete")
  tests <- data.frame(testId="t1",profileId="p1",testType="CMJ",recordedDateUtc="2026-09-21T12:00:00Z",modifiedDateUtc="2026-09-22T12:00:00Z",weight=80)
  saveRDS(profiles,file.path(env$DATA_DIR,"profiles.rds"))
  saveRDS(tests,file.path(env$DATA_DIR,"force_tests_all_history.rds"))
  saveRDS(data.frame(testId="t1",trialId="obsolete",metricKey="JH",modifiedDateUtc="2026-09-21T12:00:00Z"),
    file.path(env$DATA_DIR,"force_metrics_long_all_history_baseball.rds"))
  payload <- '[{"id":"new-trial","recordedUTC":"2026-09-21T12:00:00Z","recordedOffset":0,"recordedTimezone":"UTC","startTime":0,"endTime":1,"limb":"Both","results":[{"definition":{"result":"JH","name":"Jump Height (Imp-Mom)","unit":"cm"},"value":42}]}]'
  local_mocked_bindings(req_perform_parallel=function(...) list("mock"),resp_status=function(...) 200L,
    resp_body_string=function(...) payload,.package="httr2")
  sys.source("scripts/07_pull_forcedecks_metrics_all_history_baseball.R",envir=env)
  result <- readRDS(file.path(env$DATA_DIR,"force_metrics_long_all_history_baseball.rds"))
  expect_equal(nrow(result),1)
  expect_identical(result$trialId,"new-trial")
  expect_equal(result$value,42)
})
test_that("a first refresh builds summaries without pre-existing metrics files", {
  env <- new.env(parent=globalenv())
  env$DATA_DIR <- tempfile("first-run-"); dir.create(env$DATA_DIR)
  env$assert_vald_credentials <- function() TRUE
  env$vald_get_token <- function() "mock"
  withr::local_envvar(c(VALD_TEAM_ID="test-tenant"))
  env$GET <- function(url, ...) {
    payload <- if(grepl("/groups",url)) list(groups=data.frame(id="g",name="Baseball")) else
      list(profiles=data.frame(profileId="p",givenName="Test",familyName="Athlete",externalId="TXST-Baseball-001"))
    structure(list(url=url,headers=list(`Content-Type`="application/json"),status_code=200L,
      content=charToRaw(jsonlite::toJSON(payload,auto_unbox=TRUE))),class="response")
  }
  env$source <- function(file, ...) {
    if(grepl("07_pull",file)) {
      saveRDS(data.frame(testId="t",profileId="p",testType="CMJ",recordedDateUtc="2026-09-22T12:00:00Z"),file.path(env$DATA_DIR,"force_tests_baseball_all_history_named.rds"))
      saveRDS(data.frame(testId="t",profileId="p",testType="CMJ",trialId=c("a","b","c"),
        recordedUTC="2026-09-22T12:00:00Z",recordedOffset=0,recordedTimezone="UTC",metricKey="JH",
        metricName="Jump Height (Imp-Mom)",metricUnit="cm",value=c(40,42,44)),
        file.path(env$DATA_DIR,"force_metrics_long_all_history_baseball.rds"))
    }
  }
  sys.source("scripts/08_refresh_forcedecks_baseball.R",envir=env)
  result <- readRDS(file.path(env$DATA_DIR,"force_sessions_summary_baseball.rds"))
  expect_equal(result$best_value,44)
  expect_equal(result$n_trials,3)
  expect_true(file.exists(file.path(env$DATA_DIR,"roster_baseball.rds")))
})
# Synthetic data exists only in this process, never in the delivered app.
metrics <- unique(c(HITTER_METRICS_ORDERED,PITCHER_PERF_METRICS,PITCHER_INJURY_VALUE_METRICS,CMJ_MONITOR_METRICS))
shared_data$roster <- tibble(profileId=c("p1","p2"),athleteName=c("Test Pitcher","Test Hitter"),externalId=c("TXST-Baseball-001","TXST-Baseball-002"),primaryGroup=c("Pitchers","Infielders"))
shared_data$session_summary <- tidyr::crossing(profileId=c("p1","p2"),session_date=seq(Sys.Date()-49,Sys.Date(),by=7),metricName=metrics) %>%
  left_join(shared_data$roster,by="profileId") %>% mutate(testType="CMJ",metricKey=metricName,metricUnit="test",best_value=100+as.numeric(session_date-Sys.Date())/10,mean_value=best_value,sd_value=2,cv_value=.02)
shared_data$sprint <- empty_sprint_session_level()
shared_data$tests <- NULL; shared_data$trials_long <- NULL
shared_data$status <- "Synthetic test fixture"
load_published_snapshot <- function() FALSE
shiny::testServer(server, {
  session$setInputs(alert_triggered_only=FALSE,cmj_window_days=7,team_group="All",profile_athlete="p1")
  session$flushReact()
  stopifnot(nrow(team_general_cmj_board())==2,nrow(pitcher_perf_table_team())==1,nrow(staff_alert_rows_raw())==2)
  stopifnot(grepl("Test Pitcher",output$team_general_cmj_dt))
})
shared_data$roster <- NULL; shared_data$session_summary <- NULL
shiny::testServer(server, {
  session$flushReact()
  stopifnot(nrow(staff_alert_rows_raw())==0)
  stopifnot(grepl("No alerts match",output$alert_inbox_dt$html))
})
cat("All refresh and dashboard checks passed.\n")
