#!/usr/bin/env Rscript
source("WallyApps/shared/development_goals.R")
root <- tempfile("goal-deploy-");dir.create(root)
on.exit(unlink(root,recursive=TRUE),add=TRUE)
volume <- file.path(root,"persistent volume");dir.create(volume)
path <- file.path(volume,"app_state","development-goals.sqlite")
mountinfo <- paste("1 0 0:1 /",gsub(" ","\\040",volume,fixed=TRUE),"rw - ext4 /dev/fixture rw")
goal <- list(title="Improve K rate",metric="K%",goal="20%",target=.2,baseline_value=.15,
  lower_better=FALSE,supports=c("Action one","Action two","Action three"),drills=c("Drill one","Drill two"))
record <- function(mode,season="S26") list(player="Saved Player",player_key="savedplayer",mode=mode,
  baseline_season=season,objectives=list(goal))
invisible(development_store_save(path,record("hitting"),team="texbob"))
invisible(development_store_save(path,record("pitching"),team="texbob"))
defensive <- record("hitting","F26")
defensive$defense <- list(title="Defensive Goals",goal="Improve throwing accuracy",
  supports=c("Read contact","Set the feet","Finish the throw"),drills=c("Target throws","Ground balls"))
invisible(development_store_save(path,defensive,team="texbob"))
payloads <- function(path) {
  con <- development_store_connect(path);on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con,"SELECT * FROM development_goals ORDER BY team_key,mode,player_key,baseline_season")
}
before <- payloads(path)
hash <- unname(tools::md5sum(path))
check <- development_store_preflight(path,volume=volume,railway=TRUE,mountinfo=mountinfo)
stopifnot(check$backup$rows==3L,file.exists(check$backup$path),
  identical(payloads(check$backup$path),before),identical(payloads(path),before),
  identical(unname(tools::md5sum(path)),hash))
again <- development_store_preflight(path,volume=volume,railway=TRUE,mountinfo=mountinfo)
stopifnot(again$backup$path!=check$backup$path,file.exists(check$backup$path),
  identical(payloads(again$backup$path),before))

reject <- function(expr,pattern) {
  error <- tryCatch({force(expr);NULL},error=function(e) conditionMessage(e))
  stopifnot(!is.null(error),grepl(pattern,error,fixed=TRUE),identical(payloads(path),before))
}
reject(development_store_preflight(path,volume="",railway=TRUE,mountinfo=mountinfo),"mounted")
reject(development_store_preflight(path,volume=volume,railway=TRUE,mountinfo=character()),"mounted")
outside <- file.path(root,"code","app_state","development-goals.sqlite")
reject(development_store_preflight(outside,volume=volume,railway=TRUE,mountinfo=mountinfo),"inside")
stopifnot(!dir.exists(dirname(outside)))
if(file.symlink(root,file.path(volume,"outside-link"))) {
  reject(development_storage_guard(file.path(volume,"outside-link","goals.sqlite"),volume=volume,railway=TRUE,mountinfo=mountinfo),"inside")
}
fresh <- file.path(volume,"new-team","development-goals.sqlite")
new <- development_store_preflight(fresh,volume=volume,railway=TRUE,mountinfo=mountinfo)
stopifnot(is.null(new$backup),!file.exists(fresh))

# Model a new release directory and a fresh R process opening the SAME volume.
# No existing SQLite bytes or saved goal payloads are seeded from the release.
release <- file.path(root,"new-release");dir.create(release)
stopifnot(file.copy("WallyApps/shared/development_tracking.R",file.path(release,"development_tracking.R")))
harness <- file.path(release,"check.R")
writeLines(c(
  "args<-commandArgs(TRUE)",
  "source(args[1]);Sys.setenv(BASE_DEVELOPMENT_GOALS_FILE=args[2])",
  "stopifnot(identical(development_store_path(),args[2]))",
  "r<-development_store_preflight(volume='',railway=FALSE)",
  "h<-development_store_read(args[2],'hitting',team='texbob')",
  "p<-development_store_read(args[2],'pitching',team='texbob')",
  "stopifnot(r$backup$rows==3L,length(h)==2L,length(p)==1L)",
  "stopifnot(any(vapply(h,function(x)!is.null(x$defense),logical(1))))",
  "cat('New release reopened all saved plans.\\n')"),harness)
status <- system2(file.path(R.home("bin"),"Rscript"),shQuote(c(harness,file.path(release,"development_tracking.R"),path)),
  stdout=file.path(root,"restart.log"),stderr=file.path(root,"restart.log"))
stopifnot(status==0L,identical(payloads(path),before))

# WAL-mode committed changes may not yet be in the main .sqlite file. The backup
# must include them, along with future tables/fields it does not interpret.
writer <- development_store_connect(path,write=TRUE)
invisible(DBI::dbGetQuery(writer,"PRAGMA journal_mode=WAL"))
invisible(DBI::dbGetQuery(writer,"PRAGMA wal_autocheckpoint=0"))
DBI::dbWriteTable(writer,"future_notes",data.frame(note="Committed data still in WAL"))
stopifnot(file.exists(paste0(path,"-wal")))
wal <- development_store_backup(path)
reader <- development_store_connect(wal$path)
stopifnot(DBI::dbGetQuery(reader,"SELECT note FROM future_notes")$note=="Committed data still in WAL",
  identical(payloads(wal$path),before))
DBI::dbDisconnect(reader);DBI::dbDisconnect(writer)
corrupt <- file.path(volume,"corrupt.sqlite");writeLines("not a database",corrupt)
hash <- unname(tools::md5sum(corrupt))
stopifnot(inherits(suppressWarnings(try(development_store_backup(corrupt),silent=TRUE)),"try-error"),
  identical(unname(tools::md5sum(corrupt)),hash),
  !length(list.files(file.path(volume,"development-goals-backups"),pattern="[.]pending$")))
unlink(root,recursive=TRUE)
cat("Goal deployment safety: legacy/defensive plans preserved, verified append-only backups, mounted-volume guard, WAL data and fresh-release restart passed.\n")
