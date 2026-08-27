#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: run_fixture_app.R HittingApp|PitchingApp|DefenseApp PORT", call. = FALSE)
}

app_name <- args[[1]]
port <- suppressWarnings(as.integer(args[[2]]))
if (!is.finite(port) || port < 1024L || port > 65535L) stop("Invalid port", call. = FALSE)

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE)[[1]])
source(file.path(dirname(normalizePath(script_file, mustWork = TRUE)), "load_wally_app.R"), local = FALSE)

loaded <- load_wally_baseline_app(app_name)
on.exit(loaded$cleanup(), add = TRUE)
old_wd <- setwd(loaded$sandbox)
on.exit(setwd(old_wd), add = TRUE)

cat(app_name, "fixture app listening at", sprintf("http://127.0.0.1:%d", port), "\n")
shiny::runApp(
  shiny::shinyApp(ui = loaded$env$ui, server = loaded$env$server),
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE,
  quiet = TRUE
)
