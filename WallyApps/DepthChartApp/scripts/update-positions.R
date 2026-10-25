# Run from DepthChartApp: Rscript scripts/update-positions.R OLD_CSV NEW_CSV
source('model.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2)
old <- read_roster(args[1]); new <- read_roster(args[2])
key <- function(p) paste(p$first,p$last)
# Saved IDs use row numbers: refuse to attach them to different people.
stopifnot(identical(vapply(old,key,''),vapply(new,key,'')))
changed <- which(!vapply(seq_along(old),function(i) setequal(old[[i]]$positions,new[[i]]$positions),logical(1)))
fits <- function(p,pos) {
 ps <- p$positions
 pos %in% ps || (pos %in% c('SP','Bullpen','Closer') && any(ps %in% c('P','RHP','LHP'))) ||
 (pos %in% c('1B','2B','3B','SS') && 'INF' %in% ps) ||
 (pos %in% c('LF','CF','RF') && 'OF' %in% ps) ||
 (pos == 'DH' && any(!ps %in% c('P','RHP','LHP')))
}
path <- Sys.getenv('TXST_DEPTH_STATE','data/depth-chart-state.rds')
if(file.exists(path) && length(changed)) {
 lock <- paste0(path,'.lock')
 if(!dir.create(lock,showWarnings=FALSE)) stop('A save is in progress; retry later.')
 tryCatch({
  s <- readRDS(path)
  backup <- paste0(path,'.before-position-update-',format(Sys.time(),'%Y%m%d-%H%M%S'))
  stopifnot(file.copy(path,backup,overwrite=FALSE))
  for(y in names(s$charts)) {
   ch <- s$charts[[y]]
   for(i in changed) {
    p <- new[[i]]
    for(pos in names(ch)) {
     if(!fits(p,pos)) ch[[pos]] <- as.list(setdiff(unlist(ch[[pos]]),p$id))
     if(!pos %in% c('SP','Bullpen','Closer') && fits(p,pos) && !fits(old[[i]],pos) &&
        project_class(p,as.integer(y)) %in% 1:5 && !p$id %in% unlist(ch[[pos]])) {
       ch[[pos]] <- c(ch[[pos]],list(p$id))
     }
    }
   }
   while(length(ch$SP)<4 && length(ch$Bullpen)>0) {
    ch$SP <- c(ch$SP,ch$Bullpen[1]);ch$Bullpen <- ch$Bullpen[-1]
   }
   s$charts[[y]] <- ch
  }
  s$revision <- s$revision+1L
  validate_state(s,vapply(new,`[[`,'','id'))
  tmp <- tempfile(tmpdir=dirname(path));saveRDS(s,tmp);stopifnot(file.rename(tmp,path))
  cat('Saved charts updated. Backup:',backup,'\n')
 },finally=unlink(lock,recursive=TRUE))
}
cat(length(changed),'players have updated positions.\n')
for(i in changed)cat(key(new[[i]]),':',paste(old[[i]]$positions,collapse='/'),'->',paste(new[[i]]$positions,collapse='/'),'\n')
