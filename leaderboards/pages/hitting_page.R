# =========================================================
#  HITTING PAGE MODULE — TXST Baseball Leaderboards (Hitter PPI scaled 0–1, fixed 3 decimals)
#  + D1 Avg highlighting (green when better than D1 avg)
#    - Uses hidden numeric columns for styling so "%"/"mph" formatting doesn't break comparisons
# =========================================================
library(shiny)
library(DT)
library(dplyr)

# =========================================================
#  UI
# =========================================================
hitting_page_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "main-container",

    # --- Leaderboard Grid (2x3 + 1 wide) ---
    div(
      class = "leaderboard-grid",
      
      div(class = "leaderboard-card", 
          h4("In-Zone Contact"),
          DT::dataTableOutput(ns("top_zcontact"))
      ),
      div(class = "leaderboard-card",
          h4("Chase%"),
          DT::dataTableOutput(ns("top_chase"))
      ),
      div(class = "leaderboard-card",
          h4("In-Zone Swing"),
          DT::dataTableOutput(ns("top_zswing"))
      ),
      div(class = "leaderboard-card",
          h4("Max Exit Velo"),
          DT::dataTableOutput(ns("top_maxev"))
      ),
      div(class = "leaderboard-card",
          h4("90th EV"),
          DT::dataTableOutput(ns("top_p90ev"))
      ),
      div(class = "leaderboard-card",
          h4("Barrel%"),
          DT::dataTableOutput(ns("top_barrel"))
      ),
      div(class = "leaderboard-card-wide",
          h4("Hitter PPI"),
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
hitting_page_server <- function(id, hitting_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    #  D1 AVERAGE BENCHMARKS (Hitting) + HIGHLIGHT RULES
    #  Green if "better than D1 avg"
    # =========================================================
    d1_hit <- list(
      `Z-Contact%` = 84.3,
      `Chase%`     = 24.2,
      `Z-Swing%`   = NA_real_,   # not on your sheet
      MaxEV        = NA_real_,   # not on your sheet
      P90EV        = 103.1,   # not on your sheet
      `Barrel%`    = 17.4,
      HitterPPI    = NA_real_    # custom metric, no national baseline yet
    )
    
    higher_better <- c("Z-Contact%", "Z-Swing%", "MaxEV", "P90EV", "Barrel%", "HitterPPI")
    lower_better  <- c("Chase%")
    
    apply_d1_highlight <- function(dt, metric_name, num_col) {
      thr <- d1_hit[[metric_name]]
      if (is.null(thr) || is.na(thr)) return(dt)  # no benchmark -> no styling
      
      green_bg   <- "#c6efce"
      green_text <- "#006100"
      
      if (metric_name %in% higher_better) {
        dt %>% DT::formatStyle(
          columns = metric_name,        # the displayed column
          valueColumns = num_col,        # the hidden numeric column used for comparisons
          backgroundColor = DT::styleInterval(thr, c(NA, green_bg)),
          color           = DT::styleInterval(thr, c(NA, green_text)),
          fontWeight      = DT::styleInterval(thr, c(NA, "700"))
        )
      } else if (metric_name %in% lower_better) {
        # green when BELOW threshold
        dt %>% DT::formatStyle(
          columns = metric_name,
          valueColumns = num_col,
          backgroundColor = DT::styleInterval(thr, c(green_bg, NA)),
          color           = DT::styleInterval(thr, c(green_text, NA)),
          fontWeight      = DT::styleInterval(thr, c("700", NA))
        )
      } else {
        dt
      }
    }
    
    # ---- Helper: sanitize lists for DT ----
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
    
    # ---- Reusable table formatter ----
    format_table <- function(df, metric_name, batter_col = "Batter", descending = TRUE) {
      req(df)
      
      df <- df %>%
        select(all_of(batter_col), all_of(metric_name)) %>%
        filter(!is.na(.data[[metric_name]]))
      
      df <- if (descending) arrange(df, desc(.data[[metric_name]])) else arrange(df, .data[[metric_name]])
      df <- df %>%
        mutate(Rank = row_number()) %>%
        relocate(Rank, all_of(batter_col))
      
      # ---- KEEP A HIDDEN NUMERIC COLUMN FOR STYLING ----
      num_col <- paste0(metric_name, "__num")
      df[[num_col]] <- suppressWarnings(as.numeric(df[[metric_name]]))
      
      # ---- DISPLAY FORMATTING (SAFE TO TURN INTO TEXT NOW) ----
      if (metric_name == "HitterPPI") {
        df[[metric_name]] <- sprintf("%.3f", as.numeric(df[[metric_name]]))
      } else if (grepl("%", metric_name)) {
        df[[metric_name]] <- sprintf("%.1f%%", as.numeric(df[[metric_name]]))
      } else if (metric_name %in% c("MaxEV", "P90EV")) {
        df[[metric_name]] <- sprintf("%.1f mph", as.numeric(df[[metric_name]]))
      }
      
      df <- sanitize_for_dt(df)
      
      # ---- hide the numeric column (DT uses 0-based indexing for targets) ----
      num_target <- which(names(df) == num_col) - 1
      
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(visible = FALSE, targets = num_target)  # hide numeric helper
          )
        ),
        class = "compact stripe hover",
        style = "bootstrap5"
      )
      
      # ---- D1 Avg highlight on the displayed metric column (using hidden numeric values) ----
      apply_d1_highlight(dt, metric_name, num_col)
    }
    
    leaderboard_tables <- reactive({
      req(hitting_data())

      base <- hitting_data()
      ppi <- calculate_hitter_ppi_metric(base)

      list(
        zcontact = rank_metric_leaders(base, "Batter", "Z-Contact%"),
        chase = rank_metric_leaders(base, "Batter", "Chase%", descending = FALSE),
        zswing = rank_metric_leaders(base, "Batter", "Z-Swing%"),
        maxev = rank_metric_leaders(base, "Batter", "MaxEV"),
        p90ev = rank_metric_leaders(base, "Batter", "P90EV"),
        barrel = rank_metric_leaders(base, "Batter", "Barrel%"),
        ppi = rank_metric_leaders(ppi, "Batter", "HitterPPI")
      )
    })
    
    # =========================================================
    #  Leaderboard Outputs
    # =========================================================
    output$top_zcontact <- DT::renderDataTable({
      format_table(leaderboard_tables()$zcontact, "Z-Contact%")
    })
    
    output$top_chase <- DT::renderDataTable({
      format_table(leaderboard_tables()$chase, "Chase%", descending = FALSE)
    })
    
    output$top_zswing <- DT::renderDataTable({
      format_table(leaderboard_tables()$zswing, "Z-Swing%")
    })
    
    output$top_maxev <- DT::renderDataTable({
      format_table(leaderboard_tables()$maxev, "MaxEV")
    })
    
    output$top_p90ev <- DT::renderDataTable({
      format_table(leaderboard_tables()$p90ev, "P90EV")
    })
    
    output$top_barrel <- DT::renderDataTable({
      format_table(leaderboard_tables()$barrel, "Barrel%")
    })
    
    output$top_ppi <- DT::renderDataTable({
      format_table(leaderboard_tables()$ppi, "HitterPPI")
    })
  })
}
