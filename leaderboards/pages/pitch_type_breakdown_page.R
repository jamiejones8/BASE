# =========================================================
#  TXST BASEBALL ANALYTICS — Pitch Type Breakdown Page
#  Summarizes each pitcher's performance by pitch type
#  (Corrected: true Whiff% = Whiffs / Swings, simplified Zone%, improved ordering)
# =========================================================
library(shiny)
library(DT)
library(dplyr)
library(stringr)

pitch_type_breakdown_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "main-container",
      div(class = "txst-header", "Pitch Type Breakdown"),
      div(
        class = "page-subtitle",
        "Each pitcher's performance by pitch type"
      ),
      tags$hr(),
      div(
        class = "leaderboard-card-full",
        DTOutput(ns("pitch_type_table"))
      )
    )
  )
}

pitch_type_breakdown_page_server <- function(id, pitch_data) {
  moduleServer(id, function(input, output, session) {
    output$pitch_type_table <- renderDT({
      req(pitch_data())
      df <- prepare_pitch_type_breakdown_data(pitch_data())
      
      percent_cols <- c("Whiff%", "CSW%", "GB%", "Zone%")
      percent_idx  <- which(names(df) %in% percent_cols) - 1  # DT is 0-based
      
      datatable(
        df,
        rownames   = FALSE,
        class      = "stripe hover compact",
        extensions = "Buttons",
        options = list(
          scrollX    = TRUE,
          pageLength = 100,
          dom        = "Bfrtip",
          buttons    = list(
            list(extend = "copy",  text = "📋 Copy",  className = "txst-dt-btn"),
            list(extend = "csv",   text = "💾 CSV",   className = "txst-dt-btn"),
            list(extend = "excel", text = "📊 Excel", className = "txst-dt-btn")
          ),
          # Default sort: Pitcher asc, then Pitches desc (usage within pitcher)
          order = list(list(0, "asc"), list(2, "desc")),
          columnDefs = list(
            list(
              targets = percent_idx,
              render = JS("
                function(data, type, row, meta) {
                  if (type !== 'display') return data;
                  if (data == null || data === '') return '';
                  var val = parseFloat(data);
                  if (isNaN(val)) return data;
                  return val.toFixed(1) + '%';
                }
              ")
            )
          )
        )
      )
    })
  })
}
