# =========================================================
#  TXST BASEBALL ANALYTICS — Strength of Competition Page
#  Two tables: hitters & pitchers vs opponent terciles
# =========================================================
library(shiny)
library(DT)
library(dplyr)

source("helpers/strength_of_competition.R", local = TRUE)

strength_of_competition_page_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "main-container",
      div(class = "txst-header", "Strength of Competition"),
      div(class = "page-subtitle",
          "wOBA splits vs Top/Middle/Bottom thirds of opponent quality (scrimmages)"),
      tags$hr(),
      
      div(
        class = "leaderboard-card-full",
        
        fluidRow(
          column(
            4,
            sliderInput(
              ns("min_pa"),
              "Minimum PA to qualify (tiering + splits)",
              min = 0, max = 50, value = 0, step = 1
            )
          )
        ),
        
        tags$hr(),
        
        tags$h4("Hitters"),
        DTOutput(ns("hitters_table")),
        
        tags$hr(),
        
        tags$h4("Pitchers"),
        DTOutput(ns("pitchers_table"))
      ),
      
      # Column header colors: Bottom (red), Middle (yellow), Top (green)
      tags$style(HTML("
        th.tier-bottom { background: #8b1e1e !important; color: #fff !important; }
        th.tier-middle { background: #b58900 !important; color: #111 !important; }
        th.tier-top    { background: #1f7a3a !important; color: #fff !important; }
      "))
    )
  )
}

strength_of_competition_page_server <- function(id, pitch_data) {
  moduleServer(id, function(input, output, session) {
    
    tables <- reactive({
      req(pitch_data())
      
      pd <- pitch_data()
      
      # Build hitters SOC from the active team batter PA only
      hitters_pd <- pd %>% filter(BatterTeam == ACTIVE_TEAM_CODE)
      
      # Build pitchers SOC from the active team pitcher PA only
      pitchers_pd <- pd %>% filter(PitcherTeam == ACTIVE_TEAM_CODE)
      
      list(
        hitters  = build_strength_of_comp_tables(hitters_pd,  min_pa_qualify = input$min_pa)$hitters,
        pitchers = build_strength_of_comp_tables(pitchers_pd, min_pa_qualify = input$min_pa)$pitchers
      )
    })
    
    
    # JS: color headers for the three tier wOBA columns
    header_color_js <- JS("
      function(thead, data, start, end, display) {
        var cols = $(thead).find('th');
        cols.each(function(i){
          var txt = $(this).text();

          if (txt.includes('Top'))    $(this).addClass('tier-top');
          if (txt.includes('Middle')) $(this).addClass('tier-middle');
          if (txt.includes('Bottom')) $(this).addClass('tier-bottom');
        });
      }
    ")
    
    fmt_woba <- JS("
      function(data, type, row, meta) {
        if (type !== 'display') return data;
        if (data == null || data === '') return '';
        var v = parseFloat(data);
        if (isNaN(v)) return data;
        return v.toFixed(3);
      }
    ")
    
    # ---- Hitters table ----
    output$hitters_table <- renderDT({
      df <- tables()$hitters %>%
        select(-any_of("BatterTeam"))
      
      # 0-based indices for wOBA columns to format to 3 decimals
      woba_cols <- c("wOBA_Total", "wOBA vs LHP", "wOBA vs RHP", "wOBA vs Top", "wOBA vs Middle", "wOBA vs Bottom")
      woba_idx  <- which(names(df) %in% woba_cols) - 1
      
      datatable(
        df,
        rownames   = FALSE,
        class      = "stripe hover compact",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 50,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy",  text = "📋 Copy",  className = "txst-dt-btn"),
            list(extend = "csv",   text = "💾 CSV",   className = "txst-dt-btn"),
            list(extend = "excel", text = "📊 Excel", className = "txst-dt-btn")
          ),
          order = list(list(3, "desc")), # default sort by wOBA vs Top
          headerCallback = header_color_js,
          columnDefs = list(
            list(targets = woba_idx, render = fmt_woba)
          )
        )
      )
    })
    
    # ---- Pitchers table ----
    output$pitchers_table <- renderDT({
      df <- tables()$pitchers %>%
        select(-any_of("PitcherTeam"))
      
      woba_cols <- c("wOBA_Allowed_Total", "wOBA Allowed vs Top", "wOBA Allowed vs Middle", "wOBA Allowed vs Bottom")
      woba_idx  <- which(names(df) %in% woba_cols) - 1
      
      datatable(
        df,
        rownames   = FALSE,
        class      = "stripe hover compact",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 50,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy",  text = "📋 Copy",  className = "txst-dt-btn"),
            list(extend = "csv",   text = "💾 CSV",   className = "txst-dt-btn"),
            list(extend = "excel", text = "📊 Excel", className = "txst-dt-btn")
          ),
          order = list(list(3, "asc")),
          headerCallback = header_color_js,
          columnDefs = list(
            list(targets = woba_idx, render = fmt_woba)
          )
        )
      )
    })
    
  })
}
