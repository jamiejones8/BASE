#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

slugify <- function(value) {
  value <- tolower(as.character(value))
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("^-|-$", "", value)
}

args <- commandArgs(trailingOnly = TRUE)
sources_path <- if (length(args) >= 1) args[[1]] else file.path("JucoStatsApp", "data", "program_sources.csv")
raw_dir <- if (length(args) >= 2) args[[2]] else file.path("JucoStatsApp", "data", "raw_njcaa", "sources", "2025-26")
output_path <- if (length(args) >= 3) args[[3]] else file.path(raw_dir, "source_manifest_rebuilt.csv")

sources <- read_csv(sources_path, show_col_types = FALSE) %>%
  filter(include, !is.na(source_url), source_url != "") %>%
  mutate(expected_file = file.path(raw_dir, paste0(slugify(program_name), "-", slugify(source_kind), ".html")))

manifest <- sources %>%
  filter(file.exists(expected_file)) %>%
  transmute(
    file = expected_file,
    program_name,
    stats_team_name,
    source_group,
    njcaa_region,
    state,
    source_kind,
    source_url
  ) %>%
  arrange(source_group, program_name, source_kind)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(manifest, output_path)
message("Wrote ", nrow(manifest), " manifest rows to ", output_path)
