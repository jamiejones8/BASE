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
 name_display <- identity
 order_bullpen_rows <- identity
 compose_bullpen_report_pages <- function(...) list(NULL)
 as_named_choices <- function(x) stats::setNames(x,x)
 eval(block)
}, {
 recent <- bullpen_recent_sessions()
 stopifnot(identical(recent$Pitcher,c("Second Pitcher","First Pitcher")))
 html <- output$bullpen_recent_sessions$html
 stopifnot(grepl("First Pitcher",html),grepl("Second Pitcher",html),!grepl("<select",html,fixed=TRUE))
 stopifnot(identical(recent$Pitches,c(1L,1L)),grepl("Pitches",html),grepl("bullpen_recent_download_1",html))
 session$setInputs(bullpen_recent_preview_1=1)
 stopifnot(identical(bullpen_recent_preview()$Pitcher,"Second Pitcher"))
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
# Verify the displayed Total row uses pitch-weighted data and only the requested cells.
summary_start <- regexpr('  bullpen_metric_summary_raw <- function',text,fixed=TRUE)[1]
summary_end <- regexpr('  format_bullpen_pitch_log <- function',text,fixed=TRUE)[1]
eval(parse(text=substr(text,summary_start,summary_end-1)))
prepare_bullpen_report_data <- identity
safe_max_local <- function(x) max(x,na.rm=TRUE)
bullpen_pitch_levels <- c('Fastball','Slider')
pitches <- data.frame(PitchType_plot=c('Fastball','Fastball','Slider'),
 ZoneFlag=c(TRUE,TRUE,FALSE),Velo=c(90,92,80),RPM=2000,IVB=15,HB=5,
 RelHt=c(5,6,7),RelSideNum=c(1,2,3),Ext=c(5,6,7),VAA=-5,HAA=2,
 TopThirdFlag=TRUE,BottomThirdFlag=FALSE)
for(report in c(TRUE,FALSE)) {
 summary <- format_bullpen_summary(pitches,include_total=TRUE,report=report)
 total <- summary[summary$`Pitch Type`=='Total',]
 keep <- c('Pitch Type','#','Zone%','Rel Ht','Rel Side','Extension')
 stopifnot(total$`#`==3,total$`Zone%`=='67%',total$`Rel Ht`=='6.0',
   total$`Rel Side`=='2.0',total$Extension=='6.0',
   all(unlist(total[,setdiff(names(total),keep)])==''),
   summary$RPM[1]=='2000')
}
cat('Selective Total row and pitch-weighted averages passed.\n')
