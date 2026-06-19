# =========================================================
#  BREWSTER WHITECAPS ANALYTICS — Bundled Data File Page
# =========================================================
library(shiny)
library(DT)
library(dplyr)

upload_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "main-container",

      div(class = "txst-header", "Data Files"),
      div(
        class = "page-subtitle",
        paste(APP_TITLE, "loads one CSV directly from the parent CAPS folder")
      ),

      div(
        class = "leaderboard-card-full",
        h4("Configured Data File"),
        p(
          "The app is configured to read one explicit file from the parent CAPS folder.",
          paste("Current target:", basename(WHITE_CAPS_SOURCE_FILE))
        ),
        actionButton(ns("refresh_local_btn"), "Reload Bundled Data", class = "txst-btn"),
        br(), br(),
        textOutput(ns("sync_status")),
        br(),
        DTOutput(ns("files_table"))
      )
    )
  )
}

# =========================================================
#  SERVER
# =========================================================
upload_page_server <- function(id, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    bundled_files <- reactive({
      refresh_trigger()
      list_bundled_data_files()
    })

    observeEvent(input$refresh_local_btn, {
      output$sync_status <- renderText("⏳ Reloading configured data file from the parent CAPS folder...")
      refresh_trigger(runif(1))
      active_file <- pick_active_bundled_file()
      output$sync_status <- renderText({
        if (is.null(active_file$path) || !nzchar(active_file$path)) {
          return(paste0("⚠️ Could not find ", basename(WHITE_CAPS_SOURCE_FILE), " in the parent CAPS folder."))
        }

        paste0("✅ Reloaded bundled data from ", active_file$label, ".")
      })
    }, ignoreInit = TRUE)

    output$files_table <- renderDT({
      df <- bundled_files() %>%
        mutate(Active = ifelse(row_number() == 1, "Yes", "")) %>%
        transmute(
          Active,
          File,
          Rows,
          `Size (MB)` = SizeMB,
          Modified
        )

      datatable(
        df,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t",
          ordering = FALSE
        )
      )
    })
  })
}
