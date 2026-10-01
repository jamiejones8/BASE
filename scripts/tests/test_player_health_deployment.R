#!/usr/bin/env Rscript

source("team_config.R", local = FALSE)
source("R/integrations/player_health_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

# Prove the shipped configuration, not a developer .Renviron, is sufficient.
credential_names <- c(
  "VALD_CLIENT_ID", "VALD_CLIENT_SECRET", "VALD_TEAM_ID", "VALD_REGION",
  "VALD_AUTO_REFRESH", "VALD_REFRESH_HOURS"
)
old_credentials <- Sys.getenv(credential_names, unset = NA_character_, names = TRUE)
on.exit({
  for (key in names(old_credentials)) {
    if (is.na(old_credentials[[key]])) {
      Sys.unsetenv(key)
    } else {
      do.call(Sys.setenv, stats::setNames(list(old_credentials[[key]]), key))
    }
  }
}, add = TRUE)

Sys.unsetenv(credential_names)
base_player_health_apply_deployment_defaults()
if (any(!nzchar(Sys.getenv(credential_names)))) {
  fail("The shipped Player Health deployment configuration is incomplete.")
}
if (!identical(Sys.getenv("VALD_AUTO_REFRESH"), "true") ||
    !identical(Sys.getenv("VALD_REFRESH_HOURS"), "24")) {
  fail("The deployed VALD refresh schedule is not enabled for every 24 hours.")
}

# Simulate Railway's mounted storage and verify first-start seeding without
# touching the real /base-data volume.
deployment_root <- tempfile("base-player-health-deployment-")
on.exit(unlink(deployment_root, recursive = TRUE, force = TRUE), add = TRUE)
old_storage <- Sys.getenv("BASE_PLAYER_HEALTH_STORAGE_ROOT", unset = NA_character_)
old_path_overrides <- Sys.getenv(
  c(
    "BASE_PLAYER_HEALTH_GOLD_DIR", "BASE_PLAYER_HEALTH_LEGACY_DIR",
    "BASE_PLAYER_HEALTH_STATE_DIR", "BASE_PLAYER_HEALTH_EXPORT_DIR",
    "BASE_PLAYER_HEALTH_FALL_ROSTER_FILE"
  ),
  unset = NA_character_, names = TRUE
)
on.exit({
  if (is.na(old_storage)) Sys.unsetenv("BASE_PLAYER_HEALTH_STORAGE_ROOT") else
    Sys.setenv(BASE_PLAYER_HEALTH_STORAGE_ROOT = old_storage)
  for (key in names(old_path_overrides)) {
    if (is.na(old_path_overrides[[key]])) Sys.unsetenv(key) else
      do.call(Sys.setenv, stats::setNames(list(old_path_overrides[[key]]), key))
  }
}, add = TRUE)

Sys.setenv(BASE_PLAYER_HEALTH_STORAGE_ROOT = deployment_root)
Sys.unsetenv(names(old_path_overrides))
paths <- base_player_health_runtime_paths()
base_player_health_seed_deployment_storage(paths)

if (!isTRUE(paths$deployed)) fail("Mounted Player Health storage was not detected.")
for (path in unname(unlist(paths[c("gold", "legacy", "state", "exports")]))) {
  if (!dir.exists(path) || file.access(path, 2L) != 0L) {
    fail("Deployment storage was not created as writable: ", path)
  }
}
expected_seeds <- list.files(
  base_project_path("Sports Science 2", "data"),
  full.names = FALSE, recursive = FALSE, all.files = FALSE
)
missing_seeds <- expected_seeds[!file.exists(file.path(paths$exports, expected_seeds))]
if (length(missing_seeds)) {
  fail("Deployment import seeds were not copied: ", paste(missing_seeds, collapse = ", "))
}
if (!file.exists(paths$roster)) fail("The deployment roster was not seeded.")

# Existing persistent files must win over bundled seeds on later restarts.
seed_file <- file.path(paths$exports, expected_seeds[[1]])
original_mtime <- file.info(seed_file)$mtime
Sys.sleep(0.01)
base_player_health_seed_deployment_storage(paths)
if (!identical(file.info(seed_file)$mtime, original_mtime)) {
  fail("A restart overwrote an existing persistent Player Health import.")
}

cat("Player Health deployment defaults, persistence, and first-start data seeding passed.\n")
