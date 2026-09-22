#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tibble)
})

source("team_config.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

cape_path <- TEAM_CONFIG$data$cape_file
if (file.exists(cape_path) && identical(tolower(tools::file_ext(cape_path)), "parquet")) {
  cape_dataset <- arrow::open_dataset(cape_path, format = "parquet")
  attr(cape_dataset, "base_data_source_label") <- "2026 Cape Cod League"
  if (!inherits(cape_dataset, "Dataset")) fail("Cape Parquet was materialized instead of opened lazily.")

  sample_player <- cape_dataset %>%
    dplyr::select(Pitcher, dplyr::any_of("PitcherId")) %>%
    dplyr::filter(!is.na(.data$Pitcher), .data$Pitcher != "") %>%
    utils::head(1L) %>%
    dplyr::collect()
  if (!nrow(sample_player)) fail("Cape dataset has no pitcher available for the player-query test.")

  rows <- base_player_supplement_rows(
    sample_player,
    cape_dataset,
    sample_player$Pitcher[[1]],
    role = "Pitcher"
  )
  if (!nrow(rows) ||
      any(base_player_key(rows$Pitcher) != base_player_key(sample_player$Pitcher[[1]])) ||
      !identical(unique(rows$DataSource), "2026 Cape Cod League")) {
    fail("Player-level Cape query did not preserve the full-data matching contract.")
  }

  cached <- base_player_supplement_rows(
    sample_player,
    cape_dataset,
    sample_player$Pitcher[[1]],
    role = "Pitcher"
  )
  if (!identical(rows, cached) || length(ls(.base_supplement_rows_cache)) != 1L) {
    fail("Player-level Cape rows were not reused from the bounded cache.")
  }
}

cat("Memory-efficiency tests passed: lazy Cape queries preserve player rows and source labels.\n")
