# Run with shiny::runApp("vald_shiny_app") or RStudio's Run App button.
source("local_library.R", local = TRUE)
use_vald_local_library()
if (file.exists(".Renviron")) readRenviron(".Renviron")
Sys.setenv(VALD_APP_ROOT = normalizePath(getwd()))
options(sass.cache = file.path(tempdir(), "vald-sass"))
source("dashboard.R", local = globalenv())$value
