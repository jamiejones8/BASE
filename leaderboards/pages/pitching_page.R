# =========================================================
#  PITCHING PAGE MODULE — TXST Baseball Leaderboards (2x3 Layout + PPI)
#  + D1 Avg cell highlighting (green when "better than D1 avg")
#    - Uses hidden numeric columns for styling so "%"/"mph" formatting never breaks
# =========================================================
library(shiny)
library(DT)
library(dplyr)

pitching_page_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "main-container",

    # --- Leaderboard Grid (3x3 for symmetry) ---
    div(
      class = "leaderboard-grid",
      
      # Row 1
      div(class = "leaderboard-card",
          h4("Strikeout Rate"),
          DT::dataTableOutput(ns("top_k"))
      ),
      div(class = "leaderboard-card",
          h4("Walk Rate"),
          DT::dataTableOutput(ns("top_bb"))
      ),
      div(class = "leaderboard-card",
          h4("Barrel Rate"),
          DT::dataTableOutput(ns("top_barrel"))
      ),
      
      # Row 2
      div(class = "leaderboard-card",
          h4("Max Velo"),
          DT::dataTableOutput(ns("top_velo"))
      ),
      div(class = "leaderboard-card",
          h4("Whiff Rate"),
          DT::dataTableOutput(ns("top_whiff"))
      ),
      div(class = "leaderboard-card",
          h4("CSW%"),
          DT::dataTableOutput(ns("top_csw"))
      ),
      
      # Row 3 — Composite Performance
      div(class = "leaderboard-card-wide",
          h4("Pitching PPI"),
          DT::dataTableOutput(ns("top_ppi"))
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
#  SERVER
# =========================================================
pitching_page_server <- function(id, pitching_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    #  D1 AVERAGES (from your handwritten sheet)
    #  - Highlight green when "better":
    #      K%, Whiff%  : higher better
    #      BB%         : lower better
    #  - The others weren't on the sheet -> NA (no highlight unless you set them)
    # =========================================================
    d1_pitch <- list(
      `K%`        = 19.3,
      `BB%`       = 11.3,
      `Whiff%`    = 22.1,
      `Barrel%`   = NA_real_,
      MaxVelo     = NA_real_,
      `CSW%`      = NA_real_,
      `PPI (ERA)` = NA_real_
    )
    
    higher_better <- c("K%", "Whiff%", "CSW%", "MaxVelo")
    lower_better  <- c("BB%", "Barrel%", "PPI (ERA)")
    
    # =========================================================
    #  Helper Functions
    # =========================================================
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
    
    apply_d1_highlight <- function(dt, metric_name, num_col) {
      thr <- d1_pitch[[metric_name]]
      if (is.null(thr) || is.na(thr)) return(dt)
      
      green_bg   <- "#c6efce"
      green_text <- "#006100"
      
      if (metric_name %in% higher_better) {
        dt %>% DT::formatStyle(
          columns      = metric_name,   # displayed col
          valueColumns = num_col,        # hidden numeric col for comparisons
          backgroundColor = DT::styleInterval(thr, c(NA, green_bg)),
          color           = DT::styleInterval(thr, c(NA, green_text)),
          fontWeight      = DT::styleInterval(thr, c(NA, "700"))
        )
      } else if (metric_name %in% lower_better) {
        dt %>% DT::formatStyle(
          columns      = metric_name,
          valueColumns = num_col,
          backgroundColor = DT::styleInterval(thr, c(green_bg, NA)),
          color           = DT::styleInterval(thr, c(green_text, NA)),
          fontWeight      = DT::styleInterval(thr, c("700", NA))
        )
      } else {
        dt
      }
    }
    
    # ---------------------------------------------------------
    #  Generic DT formatter (+ hidden numeric col + D1 highlight)
    # ---------------------------------------------------------
    format_table <- function(df, metric_name, descending = TRUE) {
      req(df)
      
      df <- df %>%
        select(Pitcher, all_of(metric_name)) %>%
        filter(!is.na(.data[[metric_name]]))
      
      # sort BEFORE formatting
      df <- if (descending) arrange(df, desc(.data[[metric_name]])) else arrange(df, .data[[metric_name]])
      
      # create rank
      df <- df %>%
        mutate(Rank = row_number()) %>%
        relocate(Rank, Pitcher)
      
      # hidden numeric col (robust even if upstream values already include symbols)
      num_col <- paste0(metric_name, "__num")
      df[[num_col]] <- suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", df[[metric_name]])))
      
      # pretty display text (does not affect comparisons)
      if (grepl("%", metric_name)) {
        df[[metric_name]] <- sprintf("%.1f%%", df[[num_col]])
      } else if (grepl("Velo", metric_name, ignore.case = TRUE)) {
        df[[metric_name]] <- sprintf("%.1f mph", df[[num_col]])
      } else if (metric_name == "PPI (ERA)") {
        df[[metric_name]] <- sprintf("%.2f", df[[num_col]])
      } else {
        # fallback numeric display
        df[[metric_name]] <- ifelse(is.na(df[[num_col]]), "", sprintf("%.1f", df[[num_col]]))
      }
      
      df <- sanitize_for_dt(df)
      
      num_target <- which(names(df) == num_col) - 1  # DT targets are 0-based
      
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

      ppi_data <- calculate_pitcher_ppi_metric(pitching_data())

      list(
        k = rank_metric_leaders(ppi_data, "Pitcher", "K%"),
        bb = rank_metric_leaders(ppi_data, "Pitcher", "BB%", descending = FALSE),
        barrel = rank_metric_leaders(ppi_data, "Pitcher", "Barrel%", descending = FALSE),
        velo = rank_metric_leaders(ppi_data, "Pitcher", "MaxVelo"),
        whiff = rank_metric_leaders(ppi_data, "Pitcher", "Whiff%"),
        csw = rank_metric_leaders(ppi_data, "Pitcher", "CSW%"),
        ppi = rank_metric_leaders(ppi_data, "Pitcher", "PPI (ERA)", descending = FALSE)
      )
    })
    
    # =========================================================
    #  Leaderboards
    # =========================================================
    output$top_k <- DT::renderDataTable({
      format_table(leaderboard_tables()$k, "K%")
    })
    
    output$top_bb <- DT::renderDataTable({
      format_table(leaderboard_tables()$bb, "BB%", descending = FALSE)
    })
    
    output$top_barrel <- DT::renderDataTable({
      format_table(leaderboard_tables()$barrel, "Barrel%", descending = FALSE)
    })
    
    output$top_velo <- DT::renderDataTable({
      format_table(leaderboard_tables()$velo, "MaxVelo")
    })
    
    output$top_whiff <- DT::renderDataTable({
      format_table(leaderboard_tables()$whiff, "Whiff%")
    })
    
    output$top_csw <- DT::renderDataTable({
      format_table(leaderboard_tables()$csw, "CSW%")
    })
    
    output$top_ppi <- DT::renderDataTable({
      format_table(leaderboard_tables()$ppi, "PPI (ERA)", descending = FALSE)
    })
  })
}
