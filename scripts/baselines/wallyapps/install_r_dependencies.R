#!/usr/bin/env Rscript

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

project_root <- normalizePath(
  file.path(get_script_dir(), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
baseline_library <- file.path(project_root, ".baseline-runtime", "R")
dir.create(baseline_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(baseline_library, .libPaths()))

required <- c(
  "bslib", "cowplot", "data.table", "dplyr", "DT", "ggplot2",
  "ggplotify", "ggpubr", "ggtext", "glue", "gridExtra", "hms",
  "htmltools", "jpeg", "jsonlite", "lubridate", "magrittr", "patchwork", "plotly",
  "png", "purrr", "ragg", "readr", "rlang", "scales", "shiny",
  "shinycssloaders", "shinyWidgets", "stringr", "tidyr", "tidyverse"
)

lockfile <- file.path(project_root, "tests", "baselines", "wallyapps", "renv.lock")
if (file.exists(lockfile)) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages(
      "renv",
      lib = baseline_library,
      repos = "https://packagemanager.posit.co/cran/latest"
    )
  }
  message("Restoring the pinned baseline-only R package set from ", lockfile)
  renv::restore(
    project = project_root,
    lockfile = lockfile,
    library = baseline_library,
    packages = required,
    clean = FALSE,
    prompt = FALSE
  )
} else {
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing baseline-only R packages: ", paste(missing, collapse = ", "))
    install.packages(
      missing,
      lib = baseline_library,
      repos = "https://packagemanager.posit.co/cran/latest",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
}

still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop("Baseline R packages still missing: ", paste(still_missing, collapse = ", "))
}

versions <- vapply(required, function(package) {
  as.character(utils::packageVersion(package))
}, character(1))

message("Baseline R dependency set is ready at ", baseline_library)
for (package in names(versions)) {
  message(sprintf("%-20s %s", package, versions[[package]]))
}
