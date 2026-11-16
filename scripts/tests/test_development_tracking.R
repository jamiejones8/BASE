#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(dplyr);library(readr)})
"%||%" <- function(a,b) if(is.null(a)) b else a
source("WallyApps/shared/development_goals.R")
stopifnot(identical(vapply(c("S26","F26","PS27","S27"),development_next_season,character(1)),
  c(S26="F26",F26="PS27",PS27="S27",S27="F27")),
  development_previous_season("PS27")=="F26",development_previous_season("F26")=="S26")
for (spec in list(
  list(.20,.15,.18,TRUE,TRUE,"#087d3e",1),
  list(.20,.25,.30,FALSE,FALSE,"#171717",.5),
  list(.10,.08,.12,FALSE,FALSE,"#bc2026",0),
  list(.10,.12,.08,TRUE,FALSE,"#bc2026",0),
  list(.10,.10,.12,FALSE,FALSE,"#171717",0))) {
  p<-do.call(development_goal_progress,spec[1:4])
  stopifnot(identical(p$hit,spec[[5]]),p$color==spec[[6]],isTRUE(all.equal(p$progress,spec[[7]])))
}
stopifnot(development_goal_progress(.2,NA_real_,.3,FALSE)$state=="No data yet")
root<-tempfile("development-store-");dir.create(root)
path<-file.path(root,"goals.sqlite")
objective<-function(metric="K%",target=.18,lower=TRUE) list(title="Improve strikeout rate",metric=metric,goal="18%",
  target=target,target_input=NA_real_,lower_better=lower,baseline_value=.2,
  supports=c("Recognize spin","Attack good pitches","Win two strikes"),drills=c("Machine work","Short bat"))
record<-function(player="Brady Boles",mode="hitting",season="S26",two=TRUE) list(
  player=player,player_key=development_name_key(player),mode=mode,baseline_season=season,
  objectives=if(two)list(objective(),objective("BB%",.12,FALSE))else list(objective()))
r<-development_store_save(path,record())
stopifnot(r$revision==1L)
empty <- record(player="Empty Plan")
empty$objectives[[1]]$supports <- c("", " \n ", "")
stopifnot(inherits(try(development_store_save(path,empty),silent=TRUE),"try-error"))
read<-development_store_read(path,"hitting")
stopifnot(length(read)==1,identical(read[[1]]$objectives[[1]]$supports,record()$objectives[[1]]$supports),
  identical(read[[1]]$objectives[[1]]$drills,record()$objectives[[1]]$drills))
stale<-tryCatch({development_store_save(path,record());FALSE},error=function(e)grepl("another session",conditionMessage(e)))
stopifnot(stale,development_store_read(path,"hitting")[[1]]$revision==1L)
updated<-record();updated$objectives[[1]]$drills<-"Updated drill"
invisible(development_store_save(path,updated,expected_revision=1L))
invisible(development_store_save(path,record(mode="pitching")))
invisible(development_store_save(path,record(player="Cade Smith",two=FALSE)))
invisible(development_store_save(path,record(season="F26",two=FALSE)))
all<-development_store_read(path,"hitting")
stopifnot(length(all)==3,length(development_store_read(path,"pitching"))==1)
fall<-development_effective_plans(all,"S26")
scrimmages<-development_effective_plans(all,"F26")
stopifnot(length(fall)==2,
  length(Filter(function(p)p$player_key=="bradyboles",fall)[[1]]$objectives)==2,
  length(Filter(function(p)p$player_key=="bradyboles",scrimmages)[[1]]$objectives)==1)
models<-lapply(1:15,function(i)list(objectives=list(list(progress=list(hit=TRUE)),list(progress=list(hit=FALSE)))))
totals<-development_tracking_totals(models)
stopifnot(totals$goals==30,totals$hit==15,totals$fraction==.5)
totals<-development_tracking_totals(c(models,list(list(objectives=list(list(progress=list(hit=TRUE)))))))
stopifnot(totals$goals==31,totals$hit==16)

seasons<-c("2026 Season"="S26","2026 Fall"="F26","2027 Scrimmages"="PS27","2027 Season"="S27")
ui<-as.character(development_ui("dev",seasons))
stopifnot(grepl("Track Goals",ui),grepl("Build Goals",ui))
for(mode in c("hitting","pitching")) {
  module_path<-file.path(root,paste0(mode,".sqlite"))
  d<-tibble(Player="Boles, Brady",SeasonGroup=rep(c("S26","F26","PS27"),each=2),
    GameID=paste0("game",1:6),Date=rep(c("2026-05-01","2026-10-01","2027-01-20"),each=2),
    Inning=1,PAofInning=1,PitchofPA=1,PitchUID=paste0("pitch",1:6),PitchCall="InPlay",
    KRate=rep(if(mode=="hitting")c(.20,.15,.12)else c(.20,.25,.30),each=2),
    BBRate=rep(c(.10,.08,.12),each=2))
  summary<-function(d) setNames(list(mean(d$KRate),mean(d$BBRate)),c("K%","BB%"))
  args<-list(rows=reactive(d),player_column="Player",seasons=seasons,summarize=summary,
    percentile=function(value,metric)50,mode=mode,store_path=module_path,
    lower_better=function(metric)if(mode=="hitting")metric=="K%"else metric=="BB%",
    average=function(metric).2)
  shiny::testServer(development_server,args=args,{
    session$setInputs(season="S26",player="Boles, Brady",stat1="K%",stat2="BB%",tracking_season="F26")
    session$setInputs(title1="Improve K rate",goal1=if(mode=="hitting")"18%"else"30%",
      support1_2=" \n ",drill1_1="Drill one")
    stopifnot(length(errors())>0)
    session$setInputs(support1_2=" Action one ")
    stopifnot(!length(errors()),identical(plan()$objectives[[1]]$supports,"Action one"))
    session$setInputs(add=1)
    session$setInputs(title2="Improve BB rate",goal2=if(mode=="hitting")"12%"else"9%",
      support2_1="Action A",support2_3="Action C",drill2_1="Drill two")
    file<-tempfile(fileext=".pdf")
    saved<-save_pdf(file)
    stopifnot(saved$revision==1L,file.exists(file),length(records()$plans)==1,
      identical(saved$objectives[[1]]$supports,"Action one"),
      identical(saved$objectives[[2]]$supports,c("Action A","Action C")),
      pdftools::pdf_info(file)$pages==1)
    models<-tracking()
    stopifnot(length(models)==1,length(models[[1]]$objectives)==2,
      models[[1]]$objectives[[1]]$progress$current==if(mode=="hitting").15 else .25)
    session$setInputs(track_player="bradyboles")
    table<-output$tracking_table
    stopifnot(grepl("Team Total",table$html),grepl("tracking_plot1",table$html),grepl("tracking_plot2",table$html))
    invisible(output$tracking_plot1)
    session$setInputs(tracking_season="PS27")
    carried<-tracking()[[1]]
    stopifnot(carried$baseline_season=="F26",
      carried$objectives[[1]]$progress$baseline==if(mode=="hitting").15 else .25)
    unlink(file)
  })
  # A fresh Shiny session restores the complete plan from disk.
  messages <- list()
  shiny::testServer(development_server,args=args,{
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(id,message) messages[[id]] <<- message
    session$setInputs(season="S26",player="Boles, Brady",stat1="K%",stat2="BB%",tracking_season="F26")
    p<-loaded_record()
    stopifnot(!is.null(p),length(p$objectives)==2,two(),
      identical(p$objectives[[1]]$supports,"Action one"),p$objectives[[2]]$drills[1]=="Drill two",
      messages$support1_1$value=="Action one",messages$support1_2$value=="",messages$support1_3$value=="",
      messages$support2_2$value=="Action C",messages$support2_3$value=="")
    session$setInputs(season="F26")
    stopifnot(loaded_record()$baseline_season=="S26",two())
  })
}
unlink(root,recursive=TRUE)
cat("Development tracking: durable save/reload, revision conflicts, mode/history isolation, season carry-forward, team counts, progress colors, expandable charts and saved PDF exports passed.\n")
