#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: smoke_source.R HittingApp|PitchingApp|DefenseApp", call. = FALSE)

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE)[[1]])
source(file.path(dirname(normalizePath(script_file, mustWork = TRUE)), "load_wally_app.R"), local = FALSE)

loaded <- load_wally_baseline_app(args[[1]])
on.exit(loaded$cleanup(), add = TRUE)
env <- loaded$env

row_object <- switch(
  args[[1]],
  HittingApp = "df",
  PitchingApp = "df",
  DefenseApp = "defense_df"
)
if (!exists(row_object, envir = env, inherits = FALSE)) {
  stop("Expected data object was not created: ", row_object, call. = FALSE)
}
rows <- nrow(get(row_object, envir = env, inherits = FALSE))
if (!is.finite(rows) || rows <= 0) stop("Standalone app loaded no fixture rows", call. = FALSE)
if (!exists("ui", envir = env, inherits = FALSE) || !exists("server", envir = env, inherits = FALSE)) {
  stop("Standalone app did not create ui and server", call. = FALSE)
}

cat(args[[1]], "baseline source smoke test passed with", rows, "rows.\n")
