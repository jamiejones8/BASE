#!/usr/bin/env Rscript

output <- tempfile(fileext = ".csv")
cmd_args <- c(
  "JucoStatsApp/scripts/scrape_njcaa_stats.R",
  "--fixture-dir=JucoStatsApp/tests/fixtures",
  paste0("--output=", output)
)

status <- system2("Rscript", cmd_args)
if (!identical(status, 0L)) {
  stop("Fixture parser command failed.", call. = FALSE)
}

parsed <- readr::read_csv(output, show_col_types = FALSE)
stopifnot(nrow(parsed) == 2L)
stopifnot(all(c("A Standout", "B Prospect") %in% parsed$player_name))
stopifnot(!"C Other" %in% parsed$player_name)
stopifnot(all(c("Region 14", "Extra") %in% parsed$source_group))

message("Fixture parser test passed.")
