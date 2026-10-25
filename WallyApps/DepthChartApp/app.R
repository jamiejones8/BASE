library(shiny)
source('model.R', local=TRUE)
state_path <- Sys.getenv('TXST_DEPTH_STATE', unset='data/depth-chart-state.rds')
read_state <- function() {
  if (file.exists(state_path)) readRDS(state_path) else list(profiles=list(), charts=list(), revision=0L)
}
ui <- fluidPage(tags$head(tags$title('Texas State • Roster Studio'), tags$meta(name='description',content='Texas State roster, recruiting, budget, and pitcher release planning'), tags$link(rel='stylesheet', href='style.css'), tags$script(src='depth-model.js'), tags$script(src='app.js')), div(id='studio'), tags$script(HTML("Shiny.addCustomMessageHandler('boot', function(x){window.studioBoot(x)}); Shiny.addCustomMessageHandler('saved', function(x){window.studioSaved(x)});")))
server <- function(input, output, session) {
 roster <- read_roster('data/roster.csv')
 session$onFlushed(function() {
   tryCatch(session$sendCustomMessage('boot', list(roster=roster, state=read_state(), currentYear=season_year(rollover_month=as.integer(Sys.getenv('TXST_SEASON_ROLLOVER_MONTH','8'))))), error=function(e) showNotification(paste('Cannot read saved roster:',conditionMessage(e)),type='error',duration=NULL))
 }, once=TRUE)
 observeEvent(input$save_state, {
   tryCatch({
    s <- input$save_state
    if (is.null(s$profiles)) s$profiles <- list()
    if (is.null(s$charts)) s$charts <- list()
    if (!is.null(s$funding)) {
      if (is.null(s$funding$budgets)) s$funding$budgets <- list()
      if (is.null(s$funding$awards)) s$funding$awards <- list()
    }
    validate_state(s, vapply(roster, `[[`, '', 'id'))
    dir.create(dirname(state_path), recursive=TRUE, showWarnings=FALSE)
    lock <- paste0(state_path,'.lock')
    if (!dir.create(lock, showWarnings=FALSE)) stop('Another save is in progress. Try again.')
    on.exit(unlink(lock, recursive=TRUE), add=TRUE)
    old <- read_state()
    if (!identical(as.integer(s$revision),as.integer(old$revision))) stop('Another session saved changes. Reload before saving to avoid overwriting them.')
    if (file.exists(state_path) && is.null(old$schemaVersion) && !is.null(s$schemaVersion)) {
      backup <- paste0(state_path,'.before-plans-v2-',format(Sys.time(),'%Y%m%d-%H%M%S'))
      if (!file.copy(state_path,backup,overwrite=FALSE)) stop('Could not back up the previous saved roster.')
    }
    s$revision <- old$revision+1L
    tmp <- tempfile(tmpdir=dirname(state_path)); saveRDS(s,tmp)
    if (!file.rename(tmp,state_path)) stop('Could not write saved roster.')
    session$sendCustomMessage('saved',list(ok=TRUE,revision=s$revision))
   }, error=function(e) session$sendCustomMessage('saved',list(ok=FALSE,message=conditionMessage(e))))
 })
}
shinyApp(ui,server)
