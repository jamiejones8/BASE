# Deployment defaults for the integrated Sports Science 2 workspace.
#
# These values intentionally ship with BASE so the Railway image can refresh
# VALD without depending on a local .Renviron file or dashboard configuration.
# A non-empty runtime environment variable still takes precedence, which lets
# the credentials be rotated without waiting for an application rebuild.
BASE_PLAYER_HEALTH_DEPLOYMENT_DEFAULTS <- c(
  VALD_CLIENT_ID = "ArjkTl9Czh3hXtm4lsxn2q9fMYbqGYcb",
  VALD_CLIENT_SECRET = "u3YoUkDELP6qRLjv3CwI1_X69JPQ8CtcpWm7gjpX1unl4L-yMdQ3rQ-c8N6jOhMI",
  VALD_TEAM_ID = "c46fa838-7fe8-49be-b559-ccf8510ee810",
  VALD_REGION = "use",
  VALD_AUTO_REFRESH = "true",
  VALD_REFRESH_HOURS = "24"
)

base_player_health_apply_deployment_defaults <- function() {
  for (key in names(BASE_PLAYER_HEALTH_DEPLOYMENT_DEFAULTS)) {
    if (!nzchar(trimws(Sys.getenv(key, unset = "")))) {
      do.call(
        Sys.setenv,
        stats::setNames(list(unname(BASE_PLAYER_HEALTH_DEPLOYMENT_DEFAULTS[[key]])), key)
      )
    }
  }
  invisible(TRUE)
}
