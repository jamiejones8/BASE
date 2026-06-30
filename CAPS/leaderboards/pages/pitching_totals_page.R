# =========================================================
#  TXST BASEBALL ANALYTICS — Pitching Totals Page (Main + Team Totals + PPI, reordered)
#  + D1 Avg highlighting (green when "better than D1 avg")
#    - Forces background color with !important so Bootstrap stripe/hover can't override
# =========================================================
library(shiny)
library(DT)
library(dplyr)

pitching_totals_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "main-container",

        # ---- Main Leaderboard ----
        div(class = "leaderboard-card-full",
            DTOutput(ns("full_pitching_table"))
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
pitching_totals_page_server <- function(id, pitching_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================
    #  D1 AVERAGES (Pitching) — from your handwritten sheet
    #  Green if "better than D1 avg"
    # =========================================================
    d1_pitch <- list(
      `K%`        = 19.3,
      `BB%`       = 11.3,
      BABIP       = NA_real_,
      `Whiff%`    = 22.1,
      `Strike%`   = 60.5,
      `Zone%`     = 46.0,
      `FPS%`      = 57.5,   # renamed from FirstPitchStrike%
      wOBA        = 0.363,
      SLG         = 0.441,
      OPS         = 0.825,
      OBP         = 0.384,
      WHIP        = 1.41,
      FIP         = 5.08,
      xFIP        = 5.06,
      ERA         = 6.21,
      `PPI (ERA)` = NA_real_,
      MaxVelo     = NA_real_
    )
    
    higher_better <- c("K%","Whiff%","Strike%","Zone%","FPS%","MaxVelo")
    lower_better  <- c("BB%","wOBA","SLG","OPS","OBP","WHIP","FIP","xFIP","ERA","PPI (ERA)")
    
    highlight_col_vs_d1 <- function(dt, df, col_name) {
      thr <- d1_pitch[[col_name]]
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
    
    prepared_pitching_totals <- reactive({
      req(pitching_data())

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

      df <- pitching_data() %>%
        rename(
          `FPS%` = `FirstPitchStrike%`,
          `EA%`  = `EarlyAhead%`
        ) %>%
        calculate_pitcher_ppi_metric() %>%
        select(-any_of("Pitches"))

      percent_cols <- grep("%$", names(df), value = TRUE)
      velo_cols <- intersect(c("MaxVelo"), names(df))

      df <- df %>%
        mutate(
          across(all_of(percent_cols), normalize_pct_vec),
          across(all_of(percent_cols), ~ safe_round(.x, 1)),
          BABIP = safe_round(BABIP, 3),
          across(all_of(velo_cols), ~ safe_round(.x, 1))
        )

      if ("PA" %in% names(df) && "PPI (ERA)" %in% names(df)) {
        df <- df %>% relocate(`PPI (ERA)`, .after = PA)
      } else if ("PPI (ERA)" %in% names(df)) {
        df <- df %>% relocate(`PPI (ERA)`, .after = 1)
      }

      df <- df %>% arrange(as.numeric(`PPI (ERA)`))

      list(
        df = df,
        percent_cols = percent_cols,
        velo_cols = velo_cols
      )
    })
    
    # =========================================================
    #  MAIN TABLE
    # =========================================================
    output$full_pitching_table <- renderDT({
      prepared <- prepared_pitching_totals()
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
              render = DT::JS("
                function(data, type, row, meta) {
                  var v = parseFloat(data);
                  if (isNaN(v)) return (type === 'display') ? '' : null;
                  if (type === 'display') return v.toFixed(1) + '%';
                  return v;
                }"
              )
            ),
            list(
              targets = velo_idx,
              render = DT::JS("
                function(data, type, row, meta) {
                  var v = parseFloat(data);
                  if (isNaN(v)) return (type === 'display') ? '' : null;
                  if (type === 'display') return v.toFixed(1) + ' mph';
                  return v;
                }"
              )
            )
          )
        )
      )

      if ("BABIP" %in% names(df)) {
        dt <- dt %>% formatRound("BABIP", 3)
      }
      
      # ---- Apply highlighting to any columns that exist in the table ----
      cols_to_highlight <- intersect(names(d1_pitch), names(df))
      for (col in cols_to_highlight) {
        dt <- highlight_col_vs_d1(dt, df, col)
      }
      
      dt
    })
    
    # =========================================================
    #  TEAM TOTALS TABLE
    # =========================================================
    output$team_totals_table <- renderDT({
      df <- prepared_pitching_totals()$df
      
      numeric_cols <- df %>%
        select(where(is.numeric)) %>%
        names()
      
      totals <- df %>%
        summarise(across(all_of(numeric_cols), mean, na.rm = TRUE)) %>%
        mutate(
          across(all_of(setdiff(numeric_cols, "BABIP")), round, 1),
          BABIP = if ("BABIP" %in% names(.)) round(BABIP, 3) else BABIP
        )
      
      first_col <- names(df)[1]
      totals <- totals %>%
        mutate("{first_col}" := "Team Total") %>%
        select(all_of(c(first_col, numeric_cols)))
      
      percent_cols <- grep("%$", names(totals), value = TRUE)
      totals <- totals %>%
        mutate(
          across(all_of(percent_cols), ~ paste0(.x, "%")),
          BABIP = if ("BABIP" %in% names(totals)) sprintf("%.3f", BABIP) else BABIP,
          MaxVelo = if ("MaxVelo" %in% names(totals)) paste0(MaxVelo, " mph") else MaxVelo,
          `PPI (ERA)` = sprintf("%.2f", mean(as.numeric(df$`PPI (ERA)`), na.rm = TRUE))
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
