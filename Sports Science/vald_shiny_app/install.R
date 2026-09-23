# Run from this directory in a fresh R session.
dir.create(".R-library", showWarnings = FALSE)
.libPaths(c(normalizePath(".R-library"), .libPaths()))
packages <- c("shiny", "bslib", "dplyr", "tidyr", "DT", "plotly", "readr",
              "httr", "httr2", "jsonlite", "lubridate", "rlang", "tibble",
              "stringr", "tidyverse", "valdr", "keyring", "callr", "filelock", "testthat")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (requireNamespace("bslib", quietly=TRUE) && packageVersion("bslib") < "0.9.0") missing <- union(missing,"bslib")
if(length(missing)) install.packages(missing, lib=".R-library", repos="https://cloud.r-project.org")
cat("Dependencies installed. Restart R before launching.\n")
