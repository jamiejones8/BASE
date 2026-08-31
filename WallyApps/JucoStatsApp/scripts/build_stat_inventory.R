#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

source(file.path("JucoStatsApp", "scripts", "scrape_njcaa_stats.R"))

parse_inventory_args <- function(args) {
  out <- list(
    inputs = character(),
    output_dir = file.path("JucoStatsApp", "data")
  )

  for (arg in args) {
    if (grepl("^--output-dir=", arg)) {
      out$output_dir <- sub("^--output-dir=", "", arg)
    } else if (!grepl("^--", arg)) {
      out$inputs <- c(out$inputs, arg)
    }
  }

  if (length(out$inputs) == 0) {
    out$inputs <- file.path("JucoStatsApp", "data", "juco_player_stats_latest.csv")
  }

  out
}

read_stats_file <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(inventory_source_file = path)
}

build_inventory <- function(stats) {
  identity_cols <- c(
    "stat_type", "player_name", "name", "team", "program_name", "stats_team_name",
    "source_group", "njcaa_region", "state", "yr", "pos", "source_url", "scraped_at",
    "bio_link", "inventory_source_file", "col_1", "no", "position", "year", "status",
    "height", "weight", "bats", "throws", "dob", "hometown"
  )
  helper_cols <- c(
    "gp_from_gp_gs", "gs_from_gp_gs", "app_from_app_gs", "gs_from_app_gs",
    "w_from_w_l", "l_from_w_l", "sb_from_sb_att", "cs_from_sb_att",
    "gp", "so", "ob", "fld", "f", "c", "w_l", "app_gs", "gp_gs", "sb_att"
  )
  stat_cols <- setdiff(names(stats), c(identity_cols, helper_cols))

  stats %>%
    select(stat_type, program_name, all_of(stat_cols)) %>%
    pivot_longer(
      cols = all_of(stat_cols),
      names_to = "stat",
      values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    group_by(stat_type, stat) %>%
    summarise(
      programs_with_stat = n_distinct(program_name[!is.na(value) & value != ""]),
      total_programs = n_distinct(program_name),
      coverage_pct = round(programs_with_stat / total_programs, 3),
      .groups = "drop"
    ) %>%
    arrange(stat_type, desc(coverage_pct), stat)
}

main <- function() {
  args <- parse_inventory_args(commandArgs(trailingOnly = TRUE))
  missing_inputs <- args$inputs[!file.exists(args$inputs)]
  if (length(missing_inputs) > 0) {
    stop("Missing input file(s): ", paste(missing_inputs, collapse = ", "), call. = FALSE)
  }

  stats <- bind_rows(lapply(args$inputs, read_stats_file)) %>%
    normalize_stat_columns() %>%
    standardize_stat_aliases()

  inventory <- build_inventory(stats)
  common <- inventory %>%
    filter(total_programs > 0, programs_with_stat == total_programs) %>%
    arrange(stat_type, stat)

  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(inventory, file.path(args$output_dir, "stat_availability.csv"), na = "")
  write_csv(common, file.path(args$output_dir, "common_display_stats.csv"), na = "")

  message("Wrote stat inventory for ", n_distinct(stats$program_name), " program(s).")
  message("Common stat counts:")
  print(common %>% count(stat_type, name = "common_stats"))
}

if (sys.nframe() == 0) {
  main()
}
