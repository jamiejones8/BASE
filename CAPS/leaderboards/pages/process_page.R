# =========================================================
#  PITCHING PROCESS PAGE — TXST Baseball Leaderboards
#  + D1 Avg cell highlighting (green when "better than D1 avg")
#    - Uses hidden numeric columns for styling so "%"/formatting never breaks comparisons
# =========================================================
library(shiny)
library(DT)
library(dplyr)

process_page_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "main-container",

    # --- Leaderboard Grid (2x2) ---
    div(
      class = "leaderboard-grid",
      
      div(class = "leaderboard-card", 
          h4("Strike%"),
          DT::dataTableOutput(ns("top_strike"))
      ),
      div(class = "leaderboard-card",
          h4("Zone%"),
          DT::dataTableOutput(ns("top_zone"))
      ),
      div(class = "leaderboard-card",
          h4("First Pitch Strike"),
          DT::dataTableOutput(ns("top_fps"))
      ),
      div(class = "leaderboard-card",
          h4("Early Ahead"),
          DT::dataTableOutput(ns("top_ea"))
      )
    ),
    
    tags$hr(),
    div(
      class = "page-footer-note",
      p(em("Updates with the latest TrackMan data."))
    )
  )
}

# =========================================================
#  SERVER LOGIC
# =========================================================
process_page_server <- function(id, pitching_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    #  D1 AVERAGES (Pitching Process) — from your handwritten sheet
    #  All four are "higher is better" for this page
    # =========================================================
    d1_proc <- list(
      `Strike%`           = 60.5,
      `Zone%`             = 46.0,
      `FirstPitchStrike%` = 57.5,
      `EarlyAhead%`       = NA_real_  # not on your sheet (you had Early Ahead% in hitting; pitching sheet had "1–1 Min%" etc.)
    )
    
    apply_d1_highlight <- function(dt, metric_name, num_col) {
      thr <- d1_proc[[metric_name]]
      if (is.null(thr) || is.na(thr)) return(dt)
      
      green_bg   <- "#c6efce"
      green_text <- "#006100"
      
      dt %>% DT::formatStyle(
        columns      = metric_name,  # displayed
        valueColumns = num_col,       # numeric helper
        backgroundColor = DT::styleInterval(thr, c(NA, green_bg)),
        color           = DT::styleInterval(thr, c(NA, green_text)),
        fontWeight      = DT::styleInterval(thr, c(NA, "700"))
      )
    }
    
    # ---- Hardening: flatten anything DT can't render ----
    sanitize_for_dt <- function(df) {
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      for (nm in names(df)) {
        col <- df[[nm]]
        if (is.list(col)) {
          df[[nm]] <- vapply(col, function(x) {
            if (length(x) == 0) "" else if (length(x) == 1) as.character(x) else paste(as.character(x), collapse = ", ")
          }, character(1))
        } else if (inherits(col, "AsIs")) {
          df[[nm]] <- as.character(col)
        }
      }
      df
    }
    
    # ---- Helper: identical structure to your pitching page (hidden numeric column + highlight) ----
    format_table <- function(df, metric_name) {
      req(df)
      
      df <- df %>%
        select(Pitcher, all_of(metric_name)) %>%
        filter(!is.na(.data[[metric_name]])) %>%
        mutate(Rank = row_number()) %>%
        relocate(Rank, Pitcher)
      
      # hidden numeric column for comparisons (robust even if upstream already has %)
      num_col <- paste0(metric_name, "__num")
      df[[num_col]] <- suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", df[[metric_name]])))
      
      # pretty display
      if (grepl("%", metric_name)) {
        df[[metric_name]] <- sprintf("%.1f%%", df[[num_col]])
      } else {
        df[[metric_name]] <- ifelse(is.na(df[[num_col]]), "", sprintf("%.1f", df[[num_col]]))
      }
      
      df <- sanitize_for_dt(df)
      
      num_target <- which(names(df) == num_col) - 1  # DT uses 0-based indexing
      
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(visible = FALSE, targets = num_target)
          )
        ),
        class = "compact stripe hover",
        style = "bootstrap5"
      )
      
      apply_d1_highlight(dt, metric_name, num_col)
    }
    
    leaderboard_tables <- reactive({
      req(pitching_data())

      list(
        strike = rank_metric_leaders(pitching_data(), "Pitcher", "Strike%"),
        zone = rank_metric_leaders(pitching_data(), "Pitcher", "Zone%"),
        fps = rank_metric_leaders(pitching_data(), "Pitcher", "FirstPitchStrike%"),
        ea = rank_metric_leaders(pitching_data(), "Pitcher", "EarlyAhead%")
      )
    })

    # ---- TOP-10 Leaderboards (sort BEFORE formatting) ----
    output$top_strike <- DT::renderDataTable({
      format_table(leaderboard_tables()$strike, "Strike%")
    })
    
    output$top_zone <- DT::renderDataTable({
      format_table(leaderboard_tables()$zone, "Zone%")
    })
    
    output$top_fps <- DT::renderDataTable({
      format_table(leaderboard_tables()$fps, "FirstPitchStrike%")
    })
    
    output$top_ea <- DT::renderDataTable({
      format_table(leaderboard_tables()$ea, "EarlyAhead%")
    })
  })
}
