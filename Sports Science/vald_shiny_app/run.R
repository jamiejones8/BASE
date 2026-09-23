# Rscript /absolute/path/to/vald_shiny_app/run.R
args <- commandArgs(trailingOnly=FALSE)
f <- sub("^--file=", "", args[grepl("^--file=", args)])
if(length(f)) setwd(dirname(normalizePath(f[[1]])))
if(dir.exists(".R-library")) .libPaths(c(normalizePath(".R-library"), .libPaths()))
shiny::runApp(".", host="127.0.0.1", port=as.integer(Sys.getenv("PORT","3840")), launch.browser=FALSE)
