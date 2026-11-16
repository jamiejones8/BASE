#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(dplyr)})
`%||%` <- function(a,b) if(is.null(a)) b else a
source("WallyApps/shared/development_goals.R")
seasons <- c("2026 Season"="S26","2026 Fall"="F26","2027 Scrimmages"="PS27")
for(mode in c("hitting","pitching")) {
  ui <- as.character(development_ui("test",seasons,mode))
  stopifnot(grepl("development-ballpark-template.png",ui,fixed=TRUE),
    grepl('id="test-defense_goal"',ui,fixed=TRUE)==(mode=="hitting"))
  if(mode=="hitting") stopifnot(regexpr('id="test-defense_goal"',ui,fixed=TRUE)[1] > regexpr('id="test-drill2_5"',ui,fixed=TRUE)[1])
}
rows <- data.frame(Player="Boles, Brady",SeasonGroup=c("S26","F26","PS27"),
  GameID=paste0("game",1:3),Date=c("2026-04-01","2026-10-01","2027-01-10"),
  PitchUID=paste0("p",1:3),Inning=1,PAofInning=1,PitchofPA=1,PitchCall="InPlay",KRate=c(.2,.15,.12),BBRate=c(.1,.14,.15))
path <- tempfile(fileext=".sqlite")
args <- list(rows=reactive(rows),player_column="Player",seasons=seasons,
  summarize=function(d) list(`K%`=mean(d$KRate),`BB%`=mean(d$BBRate)),
  percentile=function(value,metric) 50,mode="hitting",store_path=path,
  lower_better=function(metric) metric=="K%")
plans <- list()
testServer(development_server,args=args,{
  session$setInputs(season="S26",player="Boles, Brady",stat1="K%",stat2="BB%",tracking_season="F26")
  session$setInputs(title1="Improve K rate",goal1="12%",support1_1="H1ActionOne",support1_2="H1ActionTwo",support1_3="H1ActionThree",
    drill1_1="H1DrillOne",drill1_2="H1DrillTwo",drill1_3="H1DrillThree",drill1_4="H1DrillFour",drill1_5="H1DrillFive")
  stopifnot(is.null(plan()$defense),!length(errors()))
  session$setInputs(defense_goal="Improve first-step reads")
  stopifnot(length(errors())>0)
  session$setInputs(defense_support1="DFActionOne",defense_support2="DFActionTwo",defense_support3="DFActionThree",
    defense_drill1="DFDrillOne",defense_drill2="DFDrillTwo",defense_drill3="DFDrillThree",defense_drill4="DFDrillFour",defense_drill5="DFDrillFive")
  stopifnot(!length(errors()),length(plan()$objectives)==1L,length(plan()$defense$drills)==5L)
  plans[[1]] <<- plan()
  session$setInputs(add=1,title2="Improve walks",goal2="12%",support2_1="H2ActionOne",support2_2="H2ActionTwo",support2_3="H2ActionThree",
    drill2_1="H2DrillOne",drill2_2="H2DrillTwo",drill2_3="H2DrillThree",drill2_4="H2DrillFour",drill2_5="H2DrillFive")
  stopifnot(!length(errors()),plan()$defense$goal=="Improve first-step reads")
  plans[[2]] <<- plan()
  file <- tempfile(fileext=".pdf")
  saved <- save_pdf(file)
  stopifnot(saved$schema_version==2L,saved$defense$goal==plan()$defense$goal)
  model <- tracking()[[1]]
  stopifnot(length(model$objectives)==2L,development_tracking_totals(list(model))$goals==2L,
    identical(model$plan$defense$supports,plan()$defense$supports))
  session$setInputs(track_player="bradyboles")
  table <- output$tracking_table$html
  stopifnot(grepl("Defensive Goals",table,fixed=TRUE),grepl("DFDrillFive",table,fixed=TRUE),grepl('colspan="5"',table,fixed=TRUE))
  session$setInputs(remove=1)
  stopifnot(length(plan()$objectives)==1L,plan()$defense$goal=="Improve first-step reads")
  unlink(file)
})
restored <- development_store_read(path,"hitting")[[1]]
stopifnot(identical(restored$defense,plans[[2]]$defense))
messages <- list()
testServer(development_server,args=args,{
  root <- session$rootScope()
  root$sendInputMessage <- function(id,message) messages[[id]] <<- message
  session$setInputs(season="S26",player="Boles, Brady")
  stopifnot(loaded_record()$defense$goal=="Improve first-step reads",
    messages$defense_goal$value=="Improve first-step reads",messages$defense_drill5$value=="DFDrillFive")
  session$setInputs(season="F26")
  stopifnot(loaded_record()$baseline_season=="S26",messages$defense_support2$value=="DFActionTwo")
})
invalid <- restored;invalid$mode <- "pitching"
stopifnot(inherits(try(development_store_save(tempfile(),invalid),silent=TRUE),"try-error"))
partial <- restored
partial$defense$supports <- c("", " OnlyDefenseAction ", "")
stopifnot(!length(development_defense_errors(partial$defense,"hitting")))
saved_partial <- development_store_save(path,partial,expected_revision=restored$revision)
stopifnot(identical(saved_partial$defense$supports,"OnlyDefenseAction"),
  identical(saved_partial$objectives,restored$objectives))
empty <- partial;empty$defense$supports <- c("", "  ", "")
stopifnot(length(development_defense_errors(empty$defense,"hitting"))>0)
messages <- list()
testServer(development_server,args=args,{
  root_session <- session$rootScope()
  root_session$sendInputMessage <- function(id,message) messages[[id]] <<- message
  session$setInputs(season="S26",player="Boles, Brady")
  stopifnot(messages$defense_support1$value=="OnlyDefenseAction",
    messages$defense_support2$value=="",messages$defense_support3$value=="")
})
plans[[3]] <- plans[[2]]
plans[[3]]$objectives[[1]]$supports <- "H1ActionOne"
plans[[3]]$objectives[[2]]$supports <- c("H2ActionOne","H2ActionTwo")
plans[[3]]$defense$supports <- "OnlyDefenseAction"
for(i in seq_along(plans)) {
  p <- plans[[i]]
  columns <- development_goal_columns(p)
  stopifnot(all(head(columns$x+columns$width,-1) < tail(columns$x,-1)),tail(columns$x,1)==740)
  file <- tempfile(fileext=".pdf");development_pdf(file,p)
  stopifnot(pdftools::pdf_info(file)$pages==1L,grepl("Defensive Goals",pdftools::pdf_text(file),fixed=TRUE))
  if(i==3L) {
    content <- pdftools::pdf_text(file)
    stopifnot(grepl("OnlyDefenseAction",content,fixed=TRUE),
      !grepl("H1ActionTwo",content,fixed=TRUE),!grepl("H2ActionThree",content,fixed=TRUE),
      !grepl("Supporting action",content,fixed=TRUE))
  }
  if(i==2L) {
    tokens <- pdftools::pdf_data(file)[[1]]
    # Inspect rendered PDF text positions, including every action/drill marker.
    for(j in 1:3) {
      prefix <- c("H1","H2","DF")[j]
      t <- tokens[startsWith(tokens$text,prefix),]
      stopifnot(nrow(t)>=8L,all(t$x >= (j-1)*1100/3),all(t$x+t$width <= j*1100/3))
    }
  }
  unlink(file)
}
unlink(path)
cat("Defensive Goals: hitter-only UI, shared background, validation, save/reload/carry-forward, written tracking and PDF column boundaries passed.\n")
