#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(dplyr)})
source('WallyApps/shared/development_goals.R')
root <- tempfile('goal-player-aliases-');dir.create(root)
seasons <- c('2026 Season'='S26','2026 Fall'='F26','2027 Scrimmages'='PS27','2027 Season'='S27')
data <- data.frame(Player=c('Byrd, Cayden','Cayden Byrd','Byrd, Cadyn','Cadyn Byrd','Garrett Byrd','Cayden Byrd'),
 SeasonGroup=c(rep('F26',5),'PS27'),Rate=c(.1,.2,.3,.4,.5,.1),
 GameID=paste0('game',1:6),PitchUID=paste0('pitch',1:6),Date=c(rep('2026-10-17',5),'2027-01-20'),
 Inning=1,PAofInning=1,PitchofPA=1,PitchCall='InPlay')
objective <- function(goal=.18) list(title='Improve strikeout rate',metric='K%',goal=paste0(goal*100,'%'),
 target=goal,baseline_value=.3,lower_better=TRUE,supports='Recognize spin',drills='Machine work')
record <- function(mode,legacy=TRUE) list(player=if(legacy)'Cadyn Byrd'else'Cayden Byrd',
 player_key=if(legacy)'cadynbyrd'else'caydenbyrd',mode=mode,baseline_season='F26',
 objectives=list(objective(if(legacy).18 else .22)),
 defense=if(mode=='hitting')list(title='Defensive Goals',goal='Improve throwing accuracy',
 supports='Set the feet',drills='Target throws')else NULL)
count_label <- function(options) {
 matches <- gregexpr('>Cayden Byrd</option>',options,fixed=TRUE)[[1]]
 sum(matches>0)
}
for(mode in c('hitting','pitching')) {
 path <- file.path(root,paste0(mode,'.sqlite'))
 invisible(development_store_save(path,record(mode)))
 args <- list(rows=reactive(data),player_column='Player',seasons=seasons,
   summarize=function(d)list(`K%`=mean(d$Rate)),percentile=function(value,metric)50,
   mode=mode,store_path=path)
 messages <- list()
 shiny::testServer(development_server,args=args,{
   root_session <- session$rootScope()
   root_session$sendInputMessage <- function(id,message) messages[[id]] <<- message
   # The old spelling remains a valid selection but produces exactly one label.
   session$setInputs(season='F26',player='Cadyn Byrd',stat1='K%',tracking_season='S27')
   stopifnot(count_label(messages$player$options)==1,
     grepl('Garrett Byrd',messages$player$options,fixed=TRUE),
     development_name(messages$player$value)=='Cayden Byrd',nrow(player_rows())==4,
     stats()[['K%']]==.25,loaded_record()$player=='Cayden Byrd',
     loaded_record()$stored_player_key=='cadynbyrd',loaded_record()$objectives[[1]]$goal=='18%',
     length(tracking())==1,tracking()[[1]]$player=='Cayden Byrd')
   if(mode=='hitting')stopifnot(loaded_record()$defense$goal=='Improve throwing accuracy')
   session$setInputs(title1='Improve strikeout rate',goal1='18%',
     support1_1='Recognize spin',drill1_1='Machine work')
   if(mode=='hitting')session$setInputs(defense_goal='Improve throwing accuracy',
     defense_support1='Set the feet',defense_drill1='Target throws')
   # Repeated exports must update the original legacy record, not create a
   # second record or lose the revision, actions, drills, or written defense.
   file <- tempfile(fileext='.pdf')
   for(revision in 2:3) {
     saved <- save_pdf(file)
     stopifnot(saved$revision==revision,saved$stored_player_key=='cadynbyrd',
       length(records()$plans)==1,count_label(messages$player$options)==1)
   }
   persisted <- development_store_read(path,mode)
   stopifnot(length(persisted)==1,persisted[[1]]$player=='Cayden Byrd',
     persisted[[1]]$player_key=='caydenbyrd',persisted[[1]]$stored_player_key=='cadynbyrd',
     persisted[[1]]$objectives[[1]]$supports=='Recognize spin',
     persisted[[1]]$objectives[[1]]$drills=='Machine work')
   if(mode=='hitting')stopifnot(persisted[[1]]$defense$drills=='Target throws')
   unlink(file)
 })
 # A new session must find the original SQL key even after its payload has
 # been saved with the corrected name/key.
 shiny::testServer(development_server,args=args,{
   session$setInputs(season='F26',player='Byrd, Cayden',stat1='K%')
   stopifnot(loaded_record()$revision==3L,loaded_record()$stored_player_key=='cadynbyrd')
 })
 # Both spellings may already have saved plans. Show one player and load the
 # latest plan for that baseline; retain every original database row.
 invisible(development_store_save(path,record(mode,FALSE)))
 con <- development_store_connect(path,write=TRUE)
 DBI::dbExecute(con,"UPDATE development_goals SET saved_at = CASE player_key WHEN 'cadynbyrd' THEN '2026-11-01 UTC' ELSE '2026-11-02 UTC' END")
 DBI::dbDisconnect(con)
 before <- unname(tools::md5sum(path))
 shiny::testServer(development_server,args=args,{
   root_session <- session$rootScope()
   root_session$sendInputMessage <- function(id,message) messages[[id]] <<- message
   session$setInputs(season='F26',player='Cayden Byrd',stat1='K%',tracking_season='S27')
   stopifnot(count_label(messages$player$options)==1,length(records()$plans)==2,
     loaded_record()$objectives[[1]]$goal=='22%',length(tracking())==1,
     identical(unname(tools::md5sum(path)),before))
   session$setInputs(season='S27')
   stopifnot(count_label(messages$player$options)==1,loaded_record()$baseline_season=='F26')
   session$setInputs(season='PS27',title1='Improve strikeout rate',goal1='22%',
     support1_1='Recognize spin',drill1_1='Machine work')
   if(mode=='hitting')session$setInputs(defense_goal='Improve throwing accuracy',
     defense_support1='Set the feet',defense_drill1='Target throws')
   file <- tempfile(fileext='.pdf');saved <- save_pdf(file)
   stopifnot(saved$revision==1L,saved$stored_player_key=='caydenbyrd',
     length(development_store_read(path,mode))==3,count_label(messages$player$options)==1)
   unlink(file)
 })
}
unlink(root,recursive=TRUE)
cat('Development player aliases: one dropdown entry in both apps, combined stats, legacy goal reload/update, revisions, defense, duplicate saved histories and season carry-forward passed.\n')
