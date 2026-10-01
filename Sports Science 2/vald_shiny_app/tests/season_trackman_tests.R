# Run from the app folder; read-only reconciliation of the four supplied TrackMan exports.
source("local_library.R", local = TRUE)
use_vald_local_library()
source('R/roster_trackman.R')
r<-read_fall_roster();t<-trackman_read('../data',r)
p<-t$pitches[grepl('Season',t$pitches$source_file),]
stopifnot(all(grepl("measured TrackMan rows excluded: missing PitchUID.",t$errors,fixed=TRUE)),nrow(p)==4599,length(unique(p$key))==12,!anyDuplicated(t$pitches$PitchUID),all(t$pitches$key %in% r$players$key))
# Independently reconcile a returning athlete's season fastball mean.
raw<-read.csv('../data/2026 Season Trackman data- cleaned.csv',check.names=FALSE)
x<-raw[which(raw$Pitcher=='Smith, Cade' & raw$TaggedPitchType %in% c('Fastball','Sinker') & !is.na(raw$Date) & !is.na(raw$PitchUID)),]
date<-sort(unique(x$Date))[1];x<-x[x$Date==date,]
y<-t$daily[t$daily$key=='cade smith' & t$daily$date==as.Date(date) & t$daily$pitch_group=='Fastball/sinker',]
stopifnot(nrow(y)==1,isTRUE(all.equal(y$velocity_mph,mean(x$RelSpeed,na.rm=TRUE))),isTRUE(all.equal(y$spin_rpm,mean(x$SpinRate,na.rm=TRUE))))
cat('Season import validated:',nrow(p),'pitch records,',length(unique(p$key)),'players,',as.character(min(p$date)),'to',as.character(max(p$date)),'\n')
print(t$notes)

# Every supplied export contributes both daily KPI values and correlation targets.
expected<-c('2026 Season Trackman data- cleaned.csv','2026 Fall Trackman data- cleaned.csv','2026 Squads Trackman data- cleaned.csv','2025 Fall Trackman data -cleaned.csv')
stopifnot(all(expected %in% t$files))
series<-trackman_series(t$daily)
for(f in expected) {
  stopifnot(any(t$pitches$source_file==f),any(grepl(f,t$daily$source_files,fixed=TRUE)),any(grepl(f,series$source_files,fixed=TRUE)))
}
squads<-t$pitches[t$pitches$source_file==expected[3],]
stopifnot(nrow(squads)==1442,sum(!is.na(squads$pitch_group))==1270)
# Reconcile a Squads-only pitching day independently against the raw export.
squad_raw<-read.csv(file.path('../data',expected[3]),check.names=FALSE)
x<-squad_raw[which(squad_raw$Pitcher=='Smith, Cade' & squad_raw$TaggedPitchType %in% c('Fastball','Sinker') & !is.na(squad_raw$Date) & !is.na(squad_raw$PitchUID)),]
date<-sort(unique(x$Date))[1];x<-x[x$Date==date,]
y<-t$daily[t$daily$key=='cade smith' & t$daily$date==as.Date(date) & t$daily$pitch_group=='Fastball/sinker',]
stopifnot(nrow(y)==1,isTRUE(all.equal(y$velocity_mph,mean(x$RelSpeed,na.rm=TRUE))),isTRUE(all.equal(y$spin_rpm,mean(x$SpinRate,na.rm=TRUE))))
cat('All four exports contribute to the correlation target series; Squads adds',nrow(squads),'roster pitch records.\n')
