#!/usr/bin/env Rscript

source("R/config/team_config.R", local = FALSE)
source("R/data/source_contract.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
contract <- BASE_DATA_SOURCE_CONTRACT
primary <- base_data_source("ncaa_d1_pitch_events_2026")
audit_path <- base_project_path("docs", "wallyapps", "source-audit.json")
if (!file.exists(audit_path)) fail("Aggregate source audit was not found: ", audit_path)
audit <- jsonlite::fromJSON(audit_path, simplifyVector = FALSE)$candidate_master

if (!identical(as.integer(contract$primary_season), 2026L)) {
  fail("The selected primary season must be 2026.")
}
if (!identical(primary$status, "selected")) {
  fail("The NCAA D1 pitch source is not selected.")
}
if (!identical(primary$identity_key, "PitchUID")) {
  fail("The NCAA D1 pitch source must use PitchUID as its primary identity key.")
}
if (!identical(
  as.integer(primary$coverage_evidence$rows),
  as.integer(audit$rows_by_year[["2026"]])
)) {
  fail("Selected 2026 row count no longer matches the aggregate audit.")
}
if (!identical(
  as.integer(primary$coverage_evidence$team_codes),
  as.integer(audit$national_team_coverage$team_codes_union)
)) {
  fail("The checked source coverage no longer matches the aggregate audit.")
}
if (!identical(
  primary$coverage_evidence$date_min,
  audit$date_ranges_by_year[["2026"]]$date_min
) || !identical(
  primary$coverage_evidence$date_max,
  audit$date_ranges_by_year[["2026"]]$date_max
)) {
  fail("Selected 2026 date range no longer matches the aggregate audit.")
}
if (!identical(
  base_feature_sources("national_pitcher_scouting"),
  c("ncaa_d1_pitch_events_2026", "reference_metrics")
)) {
  fail("National pitcher scouting is not routed to the selected D1 source.")
}
if ("ncaa_d1_catching_2026" %in% base_feature_sources("catching")) {
  fail("Catching must derive from the primary pitch source, not a duplicate national file.")
}
if (!"ncaa_d1_pitch_events_2026" %in% base_feature_sources("catching")) {
  fail("Catching is not routed to the selected D1 pitch source.")
}
if (!all(c(
  "ncaa_d1_defense_alignment_2026",
  "ncaa_d1_pitch_events_2026"
) %in% base_feature_sources("defense"))) {
  fail("Defense must combine the narrow alignment source with primary pitch context.")
}
if (!identical(as.integer(contract$storage_policy$deployed_large_copy_limit), 1L)) {
  fail("The contract must permit only one deployed large pitch-event copy.")
}

cat("Data-source contract is internally consistent.\n")
