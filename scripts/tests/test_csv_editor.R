# Exercise the actual fill observer without starting the full application.
expr <- parse('R/app_main.R')
server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) && identical(x[[2]], as.name('data_processing_server')), as.list(expr))[[1]]
body <- server[[3]][[3]]
fill <- Filter(function(x) is.call(x) && identical(x[[1]], quote(shiny::observeEvent)) && identical(x[[2]], quote(input$dp_csv_fill_edit)), as.list(body))[[1]][[3]]
original <- data.frame(GameID=c('a','a','a','a','b'), Inning=c('1','1','1','2','1'), check.names=FALSE,
  'Top/Bottom'=c('Top','Top','Bottom','Top','Top'), Catcher=c('A','B','C','D','E'), Batter=letters[1:5])
current <- original
csv_rows <- function(value) { if (!missing(value)) current <<- value; current }
csv_original <- function() original
csv_message <- function(value) invisible(value)
base_trackman_changed_cells <- function(a,b) sum(a != b)
run <- function(rows,cols=4L,source=1L) {
  input <- list(dp_csv_fill_edit=list(rows=rows,cols=cols,source=source,sourceCol=4L))
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
cat('CSV fills copy across rows/columns and reject half-inning, game, identifier, and index violations.\n')
