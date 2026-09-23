#!/usr/bin/env Rscript

Sys.setenv(VALD_AUTO_REFRESH = "false")
source("team_config.R", local = FALSE)
source("R/integrations/player_health_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

landing_html <- paste(as.character(base_player_health_workspace_ui()), collapse = "")
for (needle in c("Player Health", "VALD + SmartSpeed", "base_player_health_app", "reload_data")) {
  if (!grepl(needle, landing_html, fixed = TRUE)) {
    fail("Player Health landing UI is missing: ", needle)
  }
}

workspace <- base_player_health_environment()
if (!is.function(workspace$server)) fail("Embedded Player Health server is unavailable.")
if (!isTRUE(workspace$BASE_PLAYER_HEALTH_EMBEDDED)) fail("Player Health did not enter embedded mode.")

html <- paste(as.character(workspace$ui), collapse = "")
if (grepl("bslib-page-navbar", html, fixed = TRUE) || grepl("<body", html, fixed = TRUE)) {
  fail("Embedded Player Health UI contains a nested full-page shell.")
}
if (!grepl("base-player-health-embedded", html, fixed = TRUE)) {
  fail("Embedded Player Health UI is missing its scoped wrapper.")
}
expected_tabs <- c(
  "Alert Inbox", "Team Overview", "Athlete Profile", "CMJ Monitoring",
  "Sprint / SmartSpeed", "Curve View (Force Tracing)"
)
missing_tabs <- expected_tabs[
  !vapply(expected_tabs, grepl, logical(1), x = html, fixed = TRUE)
]
if (length(missing_tabs)) {
  fail("Embedded Player Health UI is missing tabs: ", paste(missing_tabs, collapse = ", "))
}

if (!is.environment(workspace$shared_data) || is.null(workspace$shared_data$status)) {
  fail("Player Health did not initialize its shared data store.")
}
if (!identical(base_player_health_environment(), workspace)) {
  fail("Player Health engine is not cached after first initialization.")
}

cat(
  "Player Health integration passed:", length(expected_tabs),
  "Sports Science workflows embedded with scoped BASE presentation.\n"
)
