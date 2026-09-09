#!/usr/bin/env Rscript

# One-command repository verification used locally and during image builds.
check_args <- commandArgs(trailingOnly = TRUE)
if (length(setdiff(check_args, "--build"))) {
  stop("Usage: run_all.R [--build]", call. = FALSE)
}
checks <- list(
  c("Rscript", "scripts/checks/check_team_config.R", check_args),
  c("python3", "scripts/tests/test_deployment_checks.py"),
  c("Rscript", "scripts/checks/check_data_source_contract.R"),
  c("python3", "scripts/checks/check_app_workspaces.py"),
  c("python3", "scripts/checks/check_wally_feature_register.py"),
  c("Rscript", "scripts/tests/test_services.R"),
  c("Rscript", "scripts/tests/test_lazy_workspace.R"),
  c("Rscript", "scripts/tests/test_homebase_page.R"),
  c("Rscript", "scripts/tests/test_pitch_retags.R"),
  c("Rscript", "scripts/tests/test_wally_pitching_integration.R"),
  c("Rscript", "scripts/tests/test_wally_hitting_integration.R"),
  c("Rscript", "scripts/tests/test_wally_defense_integration.R"),
  c("Rscript", "scripts/tests/test_wally_scouting_integration.R"),
  c("Rscript", "scripts/tests/test_app_startup.R")
)

for (check in checks) {
  label <- paste(check, collapse = " ")
  cat("\n==>", label, "\n")
  status <- system2(check[[1]], check[-1])
  if (status != 0L) {
    stop("Check failed: ", label, call. = FALSE)
  }
}

cat("\nAll BASE checks passed.\n")
