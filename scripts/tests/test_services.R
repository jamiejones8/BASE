#!/usr/bin/env Rscript

source("team_config.R", local = FALSE)
source("R/services/schedule.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

# The local .env loader must work on a fresh process and preserve explicit
# deployment variables.
env_path <- tempfile(fileext = ".env")
writeLines(c("BASE_TEST_ENV_LOADER=from_file", "# ignored"), env_path)
on.exit(unlink(env_path), add = TRUE)
Sys.unsetenv("BASE_TEST_ENV_LOADER")
base_load_env_file(env_path)
if (!identical(Sys.getenv("BASE_TEST_ENV_LOADER"), "from_file")) {
  fail("The local .env loader did not set a missing variable.")
}
Sys.setenv(BASE_TEST_ENV_LOADER = "from_environment")
base_load_env_file(env_path)
if (!identical(Sys.getenv("BASE_TEST_ENV_LOADER"), "from_environment")) {
  fail("The local .env loader overwrote an explicit environment variable.")
}
Sys.unsetenv("BASE_TEST_ENV_LOADER")

schedule <- data.frame(
  DateTime = c("2099-03-02 18:30", "2099-03-01 13:00", "2099-02-28 12:00"),
  Opponent = c("Later", "Next", "Completed"),
  Venue = c("Away Park", "Bobcat Ballpark", "Bobcat Ballpark"),
  Status = c("Scheduled", "Scheduled", "Completed"),
  IsHome = c(FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
schedule_path <- tempfile(fileext = ".csv")
utils::write.csv(schedule, schedule_path, row.names = FALSE)
on.exit(unlink(schedule_path), add = TRUE)

next_game <- read_next_game_from_schedule(schedule_path)
if (is.null(next_game) || !identical(next_game$opponent, "Next")) {
  fail("Schedule selection did not return the earliest active future game.")
}
if (!identical(attr(next_game$datetime, "tzone"), TEAM_CONFIG$schedule_timezone)) {
  fail("Schedule parsing did not retain the configured timezone.")
}
if (!isTRUE(next_game$is_home)) {
  fail("Schedule parsing did not preserve the home/away flag.")
}

cat("Configuration and schedule service tests passed.\n")
