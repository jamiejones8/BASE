# Exercise the actual fill observer without starting the full application.
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
source('R/data/team_season_imports.R')
source('R/data/csv_editor.R')
expr <- parse('R/app_main.R')
server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) && identical(x[[2]], as.name('data_processing_server')), as.list(expr))[[1]]
body <- server[[3]][[3]]
fill <- Filter(function(x) is.call(x) && identical(x[[1]], quote(shiny::observeEvent)) && identical(x[[2]], quote(input$dp_csv_fill_edit)), as.list(body))[[1]][[3]]
original <- data.frame(GameID=c('a','a','a','a','b'), Inning=c('1','1','1','2','1'), check.names=FALSE,
  'Top/Bottom'=c('Top','Top','Bottom','Top','Top'), Catcher=c('A','B','C','D','E'), Batter=letters[1:5])
current <- original
csv_rows <- function(value) { if (!missing(value)) current <<- value; current }
csv_original <- function() original
csv_fill_context <- function() context
csv_message <- function(value) invisible(value)
context <- base_csv_fill_context(original)
run <- function(rows,cols=4L,source=1L,source_col=4L) {
  input <- list(dp_csv_fill_edit=list(rows=rows,cols=cols,source=source,sourceCol=source_col))
  eval(fill)
}
run(1:2)
stopifnot(identical(current$Catcher,c('A','A','C','D','E')))
run(1:2,4:5)
stopifnot(identical(current$Batter[1:2],c('A','A')))
for (rows in list(1:3,c(1,4),c(1,5),c(1,99))) {
  before <- current
  rejected <- tryCatch({run(rows); FALSE}, shiny.silent.error=function(e) TRUE)
  stopifnot(rejected, identical(before,current))
}
stopifnot(tryCatch({run(1:2,2); FALSE},shiny.silent.error=function(e) TRUE))
# Raw positioning has no Inning or Top/Bottom. Match exact companion pitches.
original <- data.frame(PitchUID=paste0('pitch',1:6),GameUID=c(rep('game1',5),'game2'),
 Date='2026-10-17',CF_Name=c('Cayden Byrd',rep('Old name',5)),RF_Name='Other player')
contacts <- data.frame(PitchUID=original$PitchUID,GameUID=original$GameUID,Date='10/17/26',
 Inning=c(1,1,1,2,1,1),'Top/Bottom'=c('Top','Top','Bottom','Top','Top','Top'),check.names=FALSE)
context <- base_csv_fill_context(original,'PP_F26',contacts)
current <- original
run(1:2)
stopifnot(identical(current$CF_Name,c('Cayden Byrd','Cayden Byrd',rep('Old name',4))))
run(1:2,4:5)
stopifnot(all(current$RF_Name[1:2]=='Cayden Byrd'))
for (rows in list(1:3,c(1,4),c(1,5),c(1,6))) {
 before <- current
 stopifnot(tryCatch({run(rows);FALSE},shiny.silent.error=function(e) TRUE),identical(current,before))
}
stopifnot(tryCatch({run(1:2,2);FALSE},shiny.silent.error=function(e) TRUE))
# No guessing for missing matches, conflicting source rows, or another season.
stopifnot(all(is.na(base_csv_fill_context(original,'PP_F26',contacts[FALSE,])$groups)))
wrong_season <- contacts;wrong_season$Date <- '2027-10-17'
stopifnot(all(is.na(base_csv_fill_context(original,'PP_F26',wrong_season)$groups)))
conflict <- contacts[1,];conflict$Inning <- 9
stopifnot(is.na(base_csv_fill_context(original,'PP_F26',bind_rows(contacts,conflict))$groups[1]))
# Real bundled positions retain their exact CSV shape while all 1,305 pitches
# receive half-inning metadata from the companion exports.
bundled <- base_read_trackman_import('WallyApps/DefenseApp/data/2026 Fall Defense.csv')
before <- bundled
actual <- base_csv_fill_context(bundled,'PP_F26')
stopifnot(!anyNA(actual$groups),identical(bundled,before),length(actual$groups)==1305)
cat('CSV fills: raw positioning context, cross-row/column copies, half-inning/game guards, hidden intervals, unmatched/ambiguous pitches and all bundled matches passed.\n')
