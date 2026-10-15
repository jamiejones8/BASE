# Reuse the existing report calculation, preview, and download handlers.
base_team_report_ui <- function(prefix, game_id, choices, preview_id, download_id) {
  bslib::navset_tab(id=paste0(prefix,"_tabs"),
    bslib::nav_panel("AAR Builder",
      shiny::div(class="base-report-builder",
        shiny::div(class="base-report-controls base-report-controls-team",
          shiny::selectInput(game_id,"Game date",choices=choices,
            selected=if(length(choices)) unname(choices[[1]]) else NULL),
          shiny::downloadButton(download_id,"Download Team Report PDF")),
        shiny::div(class="base-report-preview base-report-preview-landscape",
          shiny::imageOutput(preview_id,height="1000px")))),
    bslib::nav_panel("Recent AARs", shiny::uiOutput(paste0(prefix,"_recent"))))
}
base_team_report_recent_server <- function(input, output, session, prefix, game_id, choices) {
  output[[paste0(prefix,"_recent")]] <- shiny::renderUI({
    recent <- head(choices,20)
    if (!length(recent)) return(shiny::tags$p("No team reports available yet."))
    shiny::tagList(lapply(seq_along(recent), function(i) {
      label <- if (is.null(names(recent))) recent[[i]] else names(recent)[[i]]
      shiny::tags$div(class="mb-2", shiny::tags$button(type="button",class="btn btn-outline-primary",
        onclick=sprintf("Shiny.setInputValue(%s, %s, {priority:'event'});",
          jsonlite::toJSON(paste0(prefix,"_open"),auto_unbox=TRUE),
          jsonlite::toJSON(unname(recent[[i]]),auto_unbox=TRUE)),label))
    }))
  })
  shiny::observeEvent(input[[paste0(prefix,"_open")]], {
    selected <- input[[paste0(prefix,"_open")]]
    shiny::req(selected %in% unname(choices))
    shiny::updateSelectInput(session,game_id,selected=selected)
    bslib::nav_select(paste0(prefix,"_tabs"),"AAR Builder",session=session)
  })
}
