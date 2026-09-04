#!/usr/bin/env Rscript

app <- source(
  "app.R",
  local = new.env(parent = globalenv())
)$value

if (!inherits(app, "shiny.appobj")) {
  stop("app.R did not produce a Shiny application object.", call. = FALSE)
}

cat("BASE startup smoke test passed.\n")
