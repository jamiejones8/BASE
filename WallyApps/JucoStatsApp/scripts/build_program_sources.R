#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
base_path <- if (length(args) >= 1) args[[1]] else file.path("JucoStatsApp", "data", "program_source_bases.csv")
output_path <- if (length(args) >= 2) args[[2]] else file.path("JucoStatsApp", "data", "program_sources.csv")

source_bases <- read_csv(base_path, show_col_types = FALSE)

presto_positions <- tibble(
  source_kind = c("hitting", "pitching", "fielding"),
  pos = c("h", "p", "f")
)

presto_sources <- source_bases %>%
  filter(include, source_system == "presto_team", !is.na(base_team_url), base_team_url != "") %>%
  tidyr::crossing(presto_positions) %>%
  mutate(source_url = paste0(base_team_url, "?view=lineup&pos=", pos)) %>%
  select(program_name, stats_team_name, source_group, njcaa_region, state, source_kind, source_url, include)

sidearm_sources <- source_bases %>%
  filter(include, source_system == "sidearm_stats", !is.na(base_team_url), base_team_url != "") %>%
  mutate(
    source_kind = "sidearm_stats",
    source_url = base_team_url
  ) %>%
  select(program_name, stats_team_name, source_group, njcaa_region, state, source_kind, source_url, include)

sources <- bind_rows(presto_sources, sidearm_sources) %>%
  arrange(source_group, program_name, source_kind)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(sources, output_path)
message("Wrote ", nrow(sources), " source rows to ", output_path)
