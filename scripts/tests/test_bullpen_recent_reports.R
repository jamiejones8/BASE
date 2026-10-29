# Exercise the exact report-selection server block without loading graphics/models.
library(shiny)
library(dplyr)
source("team_config.R")
text <- paste(readLines("WallyApps/PitchingApp/PitchingApp.R"),collapse="\n")
start <- regexpr("  bullpen_open_request <- reactiveVal(NULL)",text,fixed=TRUE)[1]
end <- regexpr("  bullpen_report_data <- reactive({",text,fixed=TRUE)[1]
block <- parse(text=substr(text,start,end-1))
fixture <- data.frame(Pitcher=c("First Pitcher","Second Pitcher"),
 CustomGameID_BP=c("older-session","latest-session"),Date=as.Date(c("2026-10-26","2026-10-27")))
shiny::testServer(function(input,output,session) {
 bullpens_live_df <- reactive(fixture)
 bullpen_report_date_value <- function(d) min(d$Date)
 as_named_choices <- function(x) stats::setNames(x,x)
 eval(block)
}, {
 recent <- bullpen_recent_sessions()
 stopifnot(identical(recent$Pitcher,c("Second Pitcher","First Pitcher")))
 html <- output$bullpen_recent_sessions$html
 stopifnot(grepl("First Pitcher",html),grepl("Second Pitcher",html),!grepl("<select",html,fixed=TRUE))
 session$setInputs(bullpen_open_session="older-session")
 stopifnot(identical(bullpen_open_request()$Pitcher,"First Pitcher"))
 stopifnot(grepl('value="First Pitcher" selected',output$bullpen_report_pitcher_ui$html,fixed=TRUE))
 session$setInputs(bullpen_report_pitcher="First Pitcher")
 stopifnot(grepl('value="older-session" selected',output$bullpen_report_game_ui$html,fixed=TRUE))
 session$setInputs(bullpen_open_session="latest-session")
 stopifnot(grepl('value="Second Pitcher" selected',output$bullpen_report_pitcher_ui$html,fixed=TRUE))
 session$setInputs(bullpen_report_pitcher="Second Pitcher")
 stopifnot(grepl('value="latest-session" selected',output$bullpen_report_game_ui$html,fixed=TRUE))
})
cat("Recent bullpens across players, date order and builder selection passed.\n")
