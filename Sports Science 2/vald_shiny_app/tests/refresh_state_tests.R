source("local_library.R", local = TRUE)
use_vald_local_library()
library(testthat)
root<-tempfile('refresh-state-');dir.create(root)
Sys.setenv(VALD_APP_ROOT=root,VALD_REFRESH_WORKER='true',VALD_AUTO_REFRESH='false')
source('runtime/refresh_runtime.R')
shared_data<-new.env()
test_that('a truncated status preserves cached timestamps and recovers on the next valid read',{
  now<-Sys.time()
  state<-list(message='Ready',attempted=now,success=now,ok=TRUE)
  atomic_rds(state,state_file)
  expect_identical(read_refresh_state(),state)
  writeBin(as.raw(c(31,139)),state_file)
  expect_no_warning(fallback<-read_refresh_state())
  expect_equal(fallback$success,now)
  expect_equal(fallback$attempted,now)
  expect_match(fallback$message,'could not be read')
  expect_false(refresh_due(fallback))
  expect_no_error(shiny::isolate(poll_refresh()))
  state$message<-'Recovered';state$success<-now+1
  atomic_rds(state,state_file)
  expect_identical(read_refresh_state(),state)
  expect_no_error(shiny::isolate(poll_refresh()))
  expect_equal(shiny::isolate(refresh_message()),'Recovered')
})
test_that('cold-start corruption and invalid status schemas do not break polling',{
  state_file<<-file.path(refresh_dir,'cold-status.rds')
  writeBin(raw(),state_file)
  s<-read_refresh_state()
  expect_true(is.na(s$success));expect_false(refresh_due(s))
  expect_identical(read_refresh_state()$attempted,s$attempted)
  expect_true(refresh_due(s,now=s$attempted+3601))
  expect_no_error(shiny::isolate(poll_refresh()))
  atomic_rds(list(message='Bad date',attempted='invalid'),state_file)
  expect_match(read_refresh_state()$message,'could not be read')
  atomic_rds('not a state list',state_file)
  expect_match(read_refresh_state()$message,'could not be read')
})
test_that('a missing status on first launch still permits the initial refresh',{
  state_file<<-file.path(refresh_dir,'new-status.rds')
  expect_equal(read_refresh_state()$message,'Not refreshed yet')
  expect_true(refresh_due(read_refresh_state()))
})
test_that('the background worker receives deployment credentials and storage paths',{
  withr::local_envvar(c(
    VALD_CLIENT_ID='worker-client',
    VALD_CLIENT_SECRET='worker-secret',
    VALD_TEAM_ID='worker-team',
    CMJ_SPRINT_SHARE_GOLD_DIR='/tmp/worker-gold',
    CMJ_SPRINT_SHARE_LEGACY_DIR='/tmp/worker-legacy',
    VALD_REFRESH_DIR='/tmp/worker-refresh'
  ))
  worker_env<-refresh_worker_environment()
  expect_identical(unname(worker_env['VALD_CLIENT_ID']),'worker-client')
  expect_identical(unname(worker_env['VALD_CLIENT_SECRET']),'worker-secret')
  expect_identical(unname(worker_env['VALD_TEAM_ID']),'worker-team')
  expect_identical(unname(worker_env['CMJ_SPRINT_SHARE_GOLD_DIR']),'/tmp/worker-gold')
  expect_identical(unname(worker_env['CMJ_SPRINT_SHARE_LEGACY_DIR']),'/tmp/worker-legacy')
  expect_identical(unname(worker_env['VALD_REFRESH_DIR']),'/tmp/worker-refresh')
})
test_that('an empty cold-start data set is never considered publishable',{
  expect_false(dashboard_snapshot_ready(list(roster=NULL,session_summary=NULL)))
  expect_false(dashboard_snapshot_ready(list(roster=data.frame(),session_summary=data.frame(value=1))))
  expect_true(dashboard_snapshot_ready(list(
    roster=data.frame(profileId='p1'),
    session_summary=data.frame(profileId='p1',value=42)
  )))
})
unlink(root,recursive=TRUE)
