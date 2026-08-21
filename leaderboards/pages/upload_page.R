# =========================================================
#  TEAM ANALYTICS — Bundled Data File Page
# =========================================================
library(shiny)
library(DT)
library(dplyr)

upload_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "main-container",
      div(
        class = "leaderboard-card-full",
        h4("Current Source"),
        p(
          paste("Leaderboards read from", TEAM_CONFIG$data$season_file, "."),
          "Reload here if the source file changes."
        ),
        actionButton(ns("refresh_local_btn"), "Reload Data", class = "txst-btn"),
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
      output$sync_status <- renderText("⏳ Reloading leaderboard data source...")
      refresh_trigger(runif(1))
      active_file <- pick_active_bundled_file()
      output$sync_status <- renderText({
        if (is.null(active_file$path) || !nzchar(active_file$path)) {
          return("⚠️ No leaderboard data source file was found.")
        }

        if (isTRUE(active_file$is_lfs_pointer)) {
          return(
            paste0(
              "⚠️ Reloaded source metadata for ",
              active_file$label,
              ", but the local file is still a Git LFS pointer."
            )
          )
        }

        paste0("✅ Reloaded leaderboard data from ", active_file$label, ".")
      })
    }, ignoreInit = TRUE)

    output$files_table <- renderDT({
      df <- bundled_files() %>%
        mutate(
          Active = ifelse(row_number() == 1, "Yes", ""),
          Status = ifelse(IsLfsPointer, "Git LFS pointer", "Ready")
        ) %>%
        transmute(
          Active,
          File,
          Status,
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
