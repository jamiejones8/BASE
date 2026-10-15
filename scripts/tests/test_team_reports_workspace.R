source('team_config.R')
suppressPackageStartupMessages(library(shiny))
for (expr in parse('R/app_main.R')) {
  if (is.call(expr) && identical(expr[[1]],as.name('<-')) &&
      identical(expr[[2]],as.name('postgame_reports_workspace_ui'))) eval(expr)
}
page <- as.character(htmltools::renderTags(postgame_reports_workspace_ui())$html)
stopifnot(regexpr('data-value="Team Reports"',page,fixed=TRUE)[1] >
            regexpr('data-value="Catching AAR"',page,fixed=TRUE)[1],
  grepl('base_postgame_pitching_team',page,fixed=TRUE),grepl('base_postgame_hitting_team',page,fixed=TRUE))
choices <- c('Oct 14, 2026 Scrimmage'='latest','Oct 3, 2026 vs Opponent'='older')
ui <- base_team_report_ui('test_team','test_game',choices,'test_preview','test_download')
html <- as.character(htmltools::renderTags(ui)$html)
stopifnot(grepl('AAR Builder',html,fixed=TRUE),grepl('Recent AARs',html,fixed=TRUE),
  grepl('test_game',html,fixed=TRUE),grepl('test_download',html,fixed=TRUE))
messages <- list()
shiny::testServer(function(input,output,session) {
  session$sendInputMessage <- function(id,message) messages[[id]] <<- message
  base_team_report_recent_server(input,output,session,'test_team','test_game',choices)
}, {
  recent <- output$test_team_recent$html
  stopifnot(grepl('Oct 14, 2026 Scrimmage',recent,fixed=TRUE),grepl('older',recent,fixed=TRUE))
  session$setInputs(test_team_open='older')
  stopifnot(identical(messages$test_game$value,'older'),!session$isClosed())
})
# Both leaderboards use a single table for horizontal scrolling, not split headers.
for (app in c('Hitting','Pitching')) {
 text <- paste(readLines(paste0('WallyApps/',app,'App/',app,'App.R')),collapse='\n')
 id <- if(app=='Hitting') 'hit_leaderboard_table' else 'leaderboard_table'
 start <- regexpr(paste0('output\\$',id,' <-'),text)[1]
 block <- substring(text,start,start+6500)
 stopifnot(grepl('scrollX  = FALSE',block,fixed=TRUE))
}
cat('Team report builder, recent-game selection, and unified leaderboard table checks passed.\n')
