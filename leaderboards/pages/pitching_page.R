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
    
    # ---- Header ----
    div(class = "txst-header", "Pitching Leaderboards"),
    p(class = "page-subtitle",
      paste("Top 10", TEAM_DISPLAY_NAME, "pitchers across seven key outcome metrics.")),
    tags$hr(),
    
    # --- Leaderboard Grid (3x3 for symmetry) ---
    div(
      class = "leaderboard-grid",
      
      # Row 1
      div(class = "leaderboard-card",
          h4("Strikeout Rate (K%)"),
          DT::dataTableOutput(ns("top_k"))
      ),
      div(class = "leaderboard-card",
          h4("Walk Rate (BB%)"),
          DT::dataTableOutput(ns("top_bb"))
      ),
      div(class = "leaderboard-card",
          h4("Barrel Rate Allowed (No D1 Average)"),
          DT::dataTableOutput(ns("top_barrel"))
      ),
      
      # Row 2
      div(class = "leaderboard-card",
          h4("Max Velocity (No D1 Average)"),
          DT::dataTableOutput(ns("top_velo"))
      ),
      div(class = "leaderboard-card",
          h4("Whiff Rate (Whiff%)"),
          DT::dataTableOutput(ns("top_whiff"))
      ),
      div(class = "leaderboard-card",
          h4("CSW% (No D1 Average)"),
          DT::dataTableOutput(ns("top_csw"))
      ),
      
      # Row 3 — Composite Performance
      div(class = "leaderboard-card-wide",
          h4("Pitching Performance Index (No D1 Average)"),
          DT::dataTableOutput(ns("top_ppi"))
      )
    ),
    
    tags$hr(),
    div(
      class = "page-footer-note",
      p(em("Leaderboards update automatically with the latest TrackMan data."))
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
    
    # =========================================================
    #  PPI Calculation (ERA-like scale)
    #  NOTE: keep PPI as NUMERIC for sorting; formatting happens in format_table()
    # =========================================================
    calculate_ppi <- function(df) {
      df %>%
        mutate(
          across(c(`K%`, `BB%`, `Barrel%`), as.numeric),
          
          Z_K      = (`K%` - mean(`K%`, na.rm = TRUE)) / sd(`K%`, na.rm = TRUE),
          Z_BB     = (`BB%` - mean(`BB%`, na.rm = TRUE)) / sd(`BB%`, na.rm = TRUE),
          Z_Barrel = (`Barrel%` - mean(`Barrel%`, na.rm = TRUE)) / sd(`Barrel%`, na.rm = TRUE),
          
          RawPPI = (1.2 * Z_K) - (0.9 * Z_BB) - (0.9 * Z_Barrel),
          
          `PPI (ERA)` = round(4.50 - (0.5 * RawPPI), 2)
        ) %>%
        select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
    }
    
    ppi_data <- reactive({
      req(pitching_data())
      calculate_ppi(pitching_data())
    })
    
    # =========================================================
    #  Leaderboards
    # =========================================================
    output$top_k <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(desc(`K%`)) %>% slice_head(n = 10)
      format_table(df, "K%")
    })
    
    output$top_bb <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(`BB%`) %>% slice_head(n = 10)
      format_table(df, "BB%", descending = FALSE)
    })
    
    output$top_barrel <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(`Barrel%`) %>% slice_head(n = 10)
      format_table(df, "Barrel%", descending = FALSE)
    })
    
    output$top_velo <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(desc(MaxVelo)) %>% slice_head(n = 10)
      format_table(df, "MaxVelo")
    })
    
    output$top_whiff <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(desc(`Whiff%`)) %>% slice_head(n = 10)
      format_table(df, "Whiff%")
    })
    
    output$top_csw <- DT::renderDataTable({
      req(ppi_data())
      df <- ppi_data() %>% arrange(desc(`CSW%`)) %>% slice_head(n = 10)
      format_table(df, "CSW%")
    })
    
    output$top_ppi <- DT::renderDataTable({
      req(ppi_data())
      # lower PPI(ERA) is better, so ascending
      df <- ppi_data() %>% arrange(`PPI (ERA)`) %>% slice_head(n = 10)
      format_table(df, "PPI (ERA)", descending = FALSE)
    })
  })
}
