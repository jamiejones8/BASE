#!/usr/bin/env Rscript
# Runtime only: Railway mounts volumes after the image build/pre-deploy stages.
args <- commandArgs(trailingOnly=FALSE)
file <- sub("^--file=","",args[startsWith(args,"--file=")][1])
root <- normalizePath(file.path(dirname(file),"..",".."),mustWork=TRUE)
source(file.path(root,"team_config.R"),local=FALSE)
source(file.path(root,"WallyApps","shared","development_goals.R"),local=FALSE)
result <- development_store_preflight()
cat("Development Goals storage:",result$path,"\n")
if(is.null(result$backup)) cat("No existing database to back up; no database was created or reset.\n") else {
  cat("Verified startup backup:",result$backup$path,"\n")
  cat("Saved player/season plans preserved:",result$backup$rows,"\n")
}
