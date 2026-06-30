# =========================================================
#  TXST BASEBALL ANALYTICS — Hitting Totals Page (TXST Styled, HitterPPI after PA)
#  + D1 Avg highlighting (green when "better than D1 avg")
#    - Uses hidden numeric comparisons implicitly (DT renders keep numeric for sort/type)
#    - Forces background color with !important so Bootstrap stripe/hover can't override
# =========================================================
library(shiny)
library(DT)
library(dplyr)

hitting_totals_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "main-container",

        # ---- Main Leaderboard ----
        div(class = "leaderboard-card-full",
            DTOutput(ns("full_hitting_table"))
        ),
        br(),
        
        # ---- Team Totals Table ----
        div(class = "team-totals-card",
            h4("Team Averages"),
            DTOutput(ns("team_totals_table"))
        )
    )
  )
}

# =========================================================
#  SERVER
# =========================================================
hitting_totals_page_server <- function(id, hitting_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    #  D1 AVERAGE BENCHMARKS (Hitting) + HIGHLIGHT RULES
    #  Green if "better than D1 avg"
    # =========================================================
    d1_hit <- list(
      wOBA        = 0.364,
      BABIP       = NA_real_,
      `K%`        = 19.2,
      `BB%`       = 11.4,
      `Barrel%`   = 17.4,
      `Contact%`  = 77.1,
      `Z-Contact%`= 84.3,
      `Chase%`    = 24.2,
      `EV>95%`    = 103.1,
      P90EV       = NA_real_,   # not on sheet
      MaxEV       = NA_real_,   # not on sheet
      HitterPPI   = NA_real_    # custom metric
    )
    
    higher_better <- c("wOBA","BB%","Barrel%","Contact%","Z-Contact%","Z-Swing%","EV>95%","MaxEV","P90EV","HitterPPI")
    lower_better  <- c("K%","Chase%")
    
    # Apply highlight to a DT widget for one column
    highlight_col_vs_d1 <- function(dt, df, col_name) {
      thr <- d1_hit[[col_name]]
      if (is.null(thr) || is.na(thr) || !(col_name %in% names(df))) return(dt)
      
      green_bg   <- "#c6efce"
      green_text <- "#006100"
      
      if (col_name %in% higher_better) {
        dt %>% DT::formatStyle(
          columns = col_name,
          `background-color` = DT::styleInterval(thr, c(NA, paste0(green_bg, " !important"))),
          color              = DT::styleInterval(thr, c(NA, green_text)),
          fontWeight         = DT::styleInterval(thr, c(NA, "700"))
        )
      } else if (col_name %in% lower_better) {
        dt %>% DT::formatStyle(
          columns = col_name,
          `background-color` = DT::styleInterval(thr, c(paste0(green_bg, " !important"), NA)),
          color              = DT::styleInterval(thr, c(green_text, NA)),
          fontWeight         = DT::styleInterval(thr, c("700", NA))
        )
      } else {
        dt
      }
    }
    
    prepared_hitting_totals <- reactive({
      req(hitting_data())

      safe_num <- function(x) suppressWarnings(as.numeric(x))
      safe_round <- function(x, digits = 1) suppressWarnings(round(safe_num(x), digits))
      normalize_pct_vec <- function(x) {
        v <- safe_num(x)
        if (all(is.na(v))) return(v)
        mx <- suppressWarnings(max(v, na.rm = TRUE))
        if (!is.finite(mx)) return(v)
        if (mx > 100) v <- v / 100
        else if (mx <= 1.0001) v <- v * 100
        v
      }

      percent_cols <- c("K%","BB%","Barrel%","Contact%","Z-Contact%","Z-Swing%","Chase%","EV>95%")
      velo_cols <- c("MaxEV","P90EV")

      df <- calculate_hitter_ppi_metric(hitting_data()) %>%
        mutate(
          across(all_of(percent_cols), normalize_pct_vec),
          across(all_of(percent_cols), ~ safe_round(.x, 1)),
          wOBA = safe_round(wOBA, 3),
          BABIP = safe_round(BABIP, 3),
          across(all_of(velo_cols), ~ safe_round(.x, 1))
        )

      if ("PA" %in% names(df) && "HitterPPI" %in% names(df)) {
        df <- df %>% relocate(HitterPPI, .after = PA)
      } else if ("HitterPPI" %in% names(df)) {
        df <- df %>% relocate(HitterPPI, .after = 1)
      }

      df <- df %>% arrange(desc(as.numeric(HitterPPI)))

      list(
        df = df,
        percent_cols = percent_cols,
        velo_cols = velo_cols
      )
    })
    
    # =========================================================
    # MAIN TABLE
    # =========================================================
    output$full_hitting_table <- renderDT({
      prepared <- prepared_hitting_totals()
      df <- prepared$df
      percent_idx <- which(names(df) %in% prepared$percent_cols) - 1
      velo_idx <- which(names(df) %in% prepared$velo_cols) - 1
      
      dt <- datatable(
        df,
        rownames = FALSE,
        class = "stripe hover compact",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy",  text = "📋 Copy",  className = "txst-dt-btn"),
            list(extend = "csv",   text = "💾 CSV",   className = "txst-dt-btn"),
            list(extend = "excel", text = "📊 Excel", className = "txst-dt-btn")
          ),
          ordering = TRUE,
          columnDefs = list(
            list(
              targets = percent_idx,
              render = DT::JS(
                "function(data, type, row, meta) {
                   var v = parseFloat(data);
                   if (isNaN(v)) return (type === 'display') ? '' : null;
                   if (type === 'display') return v.toFixed(1) + '%';
                   return v;
                 }"
              )
            ),
            list(
              targets = velo_idx,
              render = DT::JS(
                "function(data, type, row, meta) {
                   var v = parseFloat(data);
                   if (isNaN(v)) return (type === 'display') ? '' : null;
                   if (type === 'display') return v.toFixed(1) + ' mph';
                 return v;
                 }"
              )
            )
          )
        )
      ) %>%
        formatRound(c("wOBA", "BABIP"), 3) %>%
        formatRound("HitterPPI", 3)
      
      # ---- Apply highlighting to any columns that exist in the table ----
      cols_to_highlight <- intersect(names(d1_hit), names(df))
      for (col in cols_to_highlight) {
        dt <- highlight_col_vs_d1(dt, df, col)
      }
      
      dt
    })
    
    # =========================================================
    # TEAM TOTALS TABLE
    # =========================================================
    output$team_totals_table <- renderDT({
      df <- prepared_hitting_totals()$df
      
      numeric_cols <- c("PA", "wOBA", "BABIP", "HitterPPI", "K%", "BB%", "Barrel%", "Contact%",
                        "Z-Contact%", "Z-Swing%", "Chase%", "EV>95%", "MaxEV", "P90EV")
      numeric_cols <- intersect(numeric_cols, names(df))
      
      totals <- df %>%
        summarise(across(all_of(numeric_cols), ~ mean(as.numeric(.x), na.rm = TRUE))) %>%
        mutate(
          wOBA = round(wOBA, 3),
          BABIP = round(BABIP, 3),
          HitterPPI = sprintf('%.3f', HitterPPI),
          across(c("PA", "K%", "BB%", "Barrel%", "Contact%", "Z-Contact%", "Z-Swing%",
                   "Chase%", "EV>95%", "MaxEV", "P90EV"), ~ round(.x, 1))
        ) %>%
        mutate(Batter = "Team Total") %>%
        select(Batter, all_of(numeric_cols)) %>%
        mutate(
          across(c(`K%`, `BB%`, `Barrel%`, `Contact%`, `Z-Contact%`,
                   `Z-Swing%`, `Chase%`, `EV>95%`), ~ paste0(.x, "%")),
          across(c(MaxEV, P90EV), ~ paste0(.x, " mph"))
        )
      
      datatable(
        totals,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          dom = "t"
        )
      )
    })
  })
}
