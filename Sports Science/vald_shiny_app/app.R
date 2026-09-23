# Run with shiny::runApp("vald_shiny_app") or RStudio's Run App button.
if (dir.exists(".R-library")) .libPaths(c(normalizePath(".R-library"), .libPaths()))
if (file.exists(".Renviron")) readRenviron(".Renviron")
Sys.setenv(VALD_APP_ROOT = normalizePath(getwd()))
options(sass.cache = file.path(tempdir(), "vald-sass"))
source("dashboard.R", local = globalenv())$value
