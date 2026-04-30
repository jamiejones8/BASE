library(shiny)
library(htmltools)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(grid)
library(magick)
library(readr)

# ==========================================
# STANDINGS
# ==========================================
fetch_standings <- function() {
  url  <- "https://baseball.pointstreak.com/standings.html?leagueid=166&seasonid=33239"
  page <- read_html(url)
  tables <- xml_find_all(page, ".//table")
  lapply(tables, function(t) {
    rows <- xml_find_all(t, ".//tr")
    data <- lapply(rows, function(r) {
      xml_text(xml_find_all(r, ".//td|.//th"))
    })
    data <- data[sapply(data, length) > 0]
    df <- as.data.frame(do.call(rbind, data[-1]), stringsAsFactors = FALSE)
    names(df) <- data[[1]]
    df
  })
}

standings <- tryCatch(fetch_standings(), error = function(e) NULL)

# ==========================================
# CATCHER — HELPER FUNCTIONS
# ==========================================
format_name <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) paste(trimws(parts[2]), trimws(parts[1])) else name
}

draw_grid_table <- function(df,
                            title        = NULL,
                            y_top        = 0.95,
                            x_center     = 0.5,
                            row_h        = 0.028,
                            table_width  = 0.85,
                            header_bg    = "#0C2340",
                            zebra_bg     = "#f0f4f8",
                            title_cex    = 0.75,
                            header_cex   = 0.60,
                            cell_cex     = 0.58,
                            color_matrix = NULL) {
  df[] <- lapply(df, function(x) ifelse(is.na(x), "-", as.character(x)))
  headers    <- names(df)
  n_cols     <- length(headers)
  col_widths <- rep(table_width / n_cols, n_cols)
  x_start    <- x_center - sum(col_widths) / 2
  x_pos      <- c(x_start, x_start + cumsum(col_widths[-n_cols]))
  y_cursor   <- y_top

  if (!is.null(title)) {
    grid.text(title, x = x_center, y = y_cursor,
              gp = gpar(fontface = "bold", cex = title_cex, col = "#0C2340"))
    y_cursor <- y_cursor - row_h * 0.9
  }

  for (i in seq_along(headers)) {
    grid.rect(x = x_pos[i], y = y_cursor,
              width = col_widths[i] * 0.98, height = row_h,
              just = c("left","top"),
              gp = gpar(fill = header_bg, col = "black", lwd = 0.5))
    grid.text(headers[i],
              x = x_pos[i] + col_widths[i] * 0.49,
              y = y_cursor - row_h * 0.5,
              gp = gpar(col = "white", cex = header_cex, fontface = "bold"))
  }
  y_cursor <- y_cursor - row_h

  for (r in seq_len(nrow(df))) {
    for (i in seq_along(headers)) {
      bg <- if (!is.null(color_matrix)) color_matrix[r, i] else if (r %% 2 == 0) zebra_bg else "white"
      grid.rect(x = x_pos[i], y = y_cursor,
                width = col_widths[i] * 0.98, height = row_h,
                just = c("left","top"),
                gp = gpar(fill = bg, col = "grey80", lwd = 0.3))
      grid.text(as.character(df[[i]][r]),
                x = x_pos[i] + col_widths[i] * 0.49,
                y = y_cursor - row_h * 0.5,
                gp = gpar(cex = cell_cex, fontface = "bold"))
    }
    y_cursor <- y_cursor - row_h
  }
  invisible(y_cursor)
}

# ==========================================
# CATCHER — DATA PREP
# ==========================================
prep_catcher_data <- function(raw, team) {
  raw <- raw %>%
    filter(CatcherTeam == team) %>%
    mutate(
      TaggedPitchType = case_when(
        TaggedPitchType %in% c("Fastball","FourSeamFastBall","Four-Seam","OneSeamFastball",
                                "FourSeamFastball","Sinker","TwoSeamFastball","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","CurveBall","Slider","Sweeper","Slurve")     ~ "Breaking Ball",
        TaggedPitchType %in% c("ChangeUp","Changeup","Splitter")                        ~ "Offspeed",
        TRUE ~ TaggedPitchType
      )
    )

  dfFraming <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, BatterSide,
           PitchCall, PlateLocHeight, PlateLocSide, Balls, Strikes, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PlateLocHeight), !is.na(PlateLocSide),
           PitchCall %in% c("BallCalled","StrikeCalled"))

  dfThrowing <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, KorBB, PitchCall,
           Strikes, OutsOnPlay, PopTime, ExchangeTime, TimeToBase, ThrowSpeed,
           BasePositionX, BasePositionY, BasePositionZ, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PopTime), PitchCall %in% c("BallCalled","StrikeCalled","StrikeSwinging"))

  list(framing = dfFraming, throwing = dfThrowing)
}

# ==========================================
# CATCHER — STAT FUNCTIONS
# ==========================================
overallStealInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    summarise(
      `Caught Stealing` = sum(OutsOnPlay == 1, na.rm = TRUE),
      `Stolen Bases`    = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`             = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`        = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time`   = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`        = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`     = round(mean(ThrowSpeed,   na.rm = TRUE), 1)
    )
}

overallStealByPitch <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    group_by(`Pitch Type` = TaggedPitchType) %>%
    summarise(
      `CS`            = sum(OutsOnPlay == 1, na.rm = TRUE),
      `SB`            = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`           = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`      = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time` = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`      = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`   = round(mean(ThrowSpeed,   na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(CS))
}

overallFramingInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0)
    ) %>%
    summarise(
      `Strikes Won`  = sum(PhysicalZone == 0 & PitchCall == "StrikeCalled", na.rm = TRUE),
      `Strikes Lost` = sum(PhysicalZone == 1 & PitchCall == "BallCalled",   na.rm = TRUE),
      `Ratio`        = round(`Strikes Won` / `Strikes Lost`, 2)
    )
}

gameStealInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date) %>%
    mutate(
      `Caught Stealing` = if_else(OutsOnPlay == 1, "Yes", "No"),
      `Pop Time`        = round(PopTime,      2),
      `Exchange Time`   = round(ExchangeTime, 2),
      `Air Time`        = round(TimeToBase,   2),
      `Throw Speed`     = round(ThrowSpeed,   1)
    ) %>%
    rename(Pitch = TaggedPitchType) %>%
    select(Inning, Batter, Pitch, `Caught Stealing`, `Pop Time`, `Exchange Time`, `Air Time`, `Throw Speed`)
}

gameFramingInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date,
           PitchCall %in% c("StrikeCalled","BallCalled")) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      Count          = paste0(Balls, "-", Strikes),
      `Plate Height` = round(PlateLocHeight, 2),
      `Plate Side`   = round(PlateLocSide,   2)
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    rename(Pitch = TaggedPitchType) %>%
    mutate(`#` = row_number()) %>%
    select(`#`, Inning, Batter, Pitch, `Strike Outcome`, Count, `Plate Height`, `Plate Side`)
}

framingPlotData <- function(catcher, df, game_date = NULL) {
  out <- df %>% filter(Catcher == catcher, PitchCall %in% c("StrikeCalled","BallCalled"))
  if (!is.null(game_date)) out <- out %>% filter(as.Date(Date) == game_date)
  out %>%
    mutate(
      PhysicalZone     = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      `Plate Height` = PlateLocHeight,
      `Plate Side`   = PlateLocSide
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    mutate(`#` = row_number()) %>%
    select(-Catcher)
}

plot_framing <- function(plot_df, outcome_filter, plot_title) {
  df_filtered <- plot_df %>% filter(`Strike Outcome` == outcome_filter)
  pt_color    <- ifelse(outcome_filter == "Won", "#00840D", "#E1463E")

  ggplot() +
    geom_polygon(data = data.frame(x = c(-0.708, 0.708, 0.708, 0, -0.708),
                                   y = c(0, 0, 0.25, 0.5, 0.25)),
                 aes(x = x, y = y), fill = "grey90", color = "black") +
    annotate("rect", xmin = -0.83083, xmax = 0.83083, ymin = 1.5, ymax = 3.3775,
             fill = NA, color = "black", linewidth = 1) +
    geom_point(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`),
               color = pt_color, size = 6, alpha = 0.9) +
    geom_text(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`, label = `#`),
              color = "white", size = 2.5, fontface = "bold") +
    xlim(-1.5, 1.5) + ylim(0, 3.75) +
    coord_fixed() +
    theme_minimal() +
    labs(title = plot_title, x = "Horizontal (ft)", y = "Height (ft)") +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ==========================================
# CATCHER — PDF GENERATION
# ==========================================
generate_catcher_pdf <- function(game_framing, game_throwing, season_framing, season_throwing, catcher, game_date, output_file, logo_path = NULL) {

  catcher_name <- format_name(catcher)

  g_steal        <- gameStealInfo(catcher, game_throwing, game_date)
g_framing      <- gameFramingInfo(catcher, game_framing, game_date)
g_frame_coords <- framingPlotData(catcher, game_framing, game_date)
  g_won_p        <- plot_framing(g_frame_coords, "Won",  paste0(game_date, " — Strikes Won"))
  g_lost_p       <- plot_framing(g_frame_coords, "Lost", paste0(game_date, " — Strikes Lost"))

  s_steal        <- overallStealInfo(catcher, season_throwing)
s_steal_pitch  <- overallStealByPitch(catcher, season_throwing)
s_framing      <- overallFramingInfo(catcher, season_framing)
s_frame_coords <- framingPlotData(catcher, season_framing)
  s_won_p        <- plot_framing(s_frame_coords, "Won",  "Season — Strikes Won")
  s_lost_p       <- plot_framing(s_frame_coords, "Lost", "Season — Strikes Lost")

  logo_grob <- tryCatch({
    img <- magick::image_read(logo_path)
    img <- magick::image_resize(img, "x100")
    rasterGrob(as.raster(img), interpolate = TRUE)
  }, error = function(e) nullGrob())

  pdf(output_file, width = 11, height = 15)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  tryCatch({

    # PAGE 1 — GAME
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "— Postgame Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
    grid.text(as.character(game_date), x = 0.5, y = 0.05,
              gp = gpar(cex = 0.85, col = "#0C2340"))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    draw_grid_table(g_steal,
                    title = "Game Throwing — Steal Attempts",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.88, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(g_framing,
                    title = "Game Framing — Strike Log",
                    y_top = 0.77, x_center = 0.5, row_h = 0.020,
                    table_width = 0.75, header_cex = 0.68, cell_cex = 0.68, title_cex = 0.90)

    grid.text("Game Framing — Strike Locations", x = 0.5, y = 0.46,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
    print(g_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
    print(g_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

    # PAGE 2 — SEASON
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "— Season Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    draw_grid_table(s_steal,
                    title = "Season Throwing — Overall",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_steal_pitch,
                    title = "Season Throwing — By Pitch Type",
                    y_top = 0.78, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_framing,
                    title = "Season Framing — Strike Summary",
                    y_top = 0.63, x_center = 0.5, row_h = 0.022,
                    table_width = 0.40, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    grid.text("Season Framing — Strike Locations", x = 0.5, y = 0.54,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
    print(s_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
    print(s_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

  }, error = function(e) message("PDF generation error: ", conditionMessage(e)))
}

# ==========================================
# HUB UI
# ==========================================
apps <- list(
  list(id = "catcher", title = "Catcher Reports",        page = "catcher", status = "live"),
  list(id = "hitter",  title = "Postgame Hitter Reports", page = NULL,      status = "live"),
  list(id = "pitcher", title = "Postgame Pitcher Reports",page = NULL,      status = "live"),
  list(id = "umpire",  title = "Umpire Reports",          page = NULL,      status = "live")
)

make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class  <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"

  if (!is.null(app$page) && app$status == "live") {
    onclick_js <- paste0("Shiny.setInputValue('nav_to', '", app$page, "', {priority: 'event'})")
    tags$div(
      onclick = onclick_js,
      class   = card_class,
      style   = "cursor: pointer;",
      tags$img(src = paste0(app$id, ".png"), class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", "→")
        )
      )
    )
  } else {
    tags$a(
      href   = if (!is.null(app$url)) app$url else "#",
      target = "_blank",
      class  = card_class,
      tags$img(src = paste0(app$id, ".png"), class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", "→")
        )
      )
    )
  }
}

hub_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",

      tags$div(class = "section-label", "Applications"),
      tags$div(class = "app-grid", lapply(apps, make_card)),

      tags$div(class = "section-label", style = "margin-top: 40px;",
               "2025 Cape Cod League Standings"),

      tags$div(
        class = "standings-wrapper",
        if (!is.null(standings)) {
          tagList(
            tags$div(class = "standings-division-label", "East Division"),
            tableOutput("east_standings"),
            tags$div(class = "standings-division-label", "West Division"),
            tableOutput("west_standings")
          )
        } else {
          tags$p("Standings unavailable.", style = "color: var(--text-muted);")
        }
      )
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

catcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button(
          "← Back to Hub",
          onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
          class = "btn btn-outline-secondary btn-sm"
        )
      ),
      tags$h2("Catcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),

      # ── Controls row ──
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",

        tags$div(
          tags$h4("Postgame Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("team_select", "Select Team:", choices = NULL),
          selectInput("game_date_select", "Select Game Date:", choices = NULL),
          selectInput("catcher_name", "Select Catcher:", choices = NULL),
          actionButton("generate_catcher", "Generate Report",
                       class = "btn btn-primary w-100")
        ),

        tags$div(
          tags$h4("Season Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("season_csvs", "Upload Season CSVs:", accept = ".csv", multiple = TRUE,
                    buttonLabel = "Browse", placeholder = "No files selected"),
          selectInput("season_team_select", "Select Team:", choices = NULL)
        )
      ),

      uiOutput("catcher_status"),
      br(),
      uiOutput("catcher_download_ui")
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

# ==========================================
# UI
# ==========================================
ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=4")
  ),

  tags$div(
    class = "hub-header",
    tags$div(
      class = "header-text",
      tags$h1("Brewster Whitecaps"),
      tags$p("C.A.P.S. - Centralized Application Platform for Staff")
    ),
    tags$img(src = "logo.png", class = "team-logo")
  ),

  uiOutput("page_content")
)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  current_page <- reactiveVal("hub")

  observeEvent(input$nav_to, {
    current_page(input$nav_to)
  })

  output$page_content <- renderUI({
    if (current_page() == "hub") hub_ui() else catcher_ui()
  })

  # ── Standings ──
  output$east_standings <- renderTable({ standings[[1]] }, striped = TRUE, hover = TRUE)
  output$west_standings <- renderTable({ standings[[2]] }, striped = TRUE, hover = TRUE)

  # ── Game CSV ──
  raw_game <- reactive({
    req(input$game_csv)
    read_csv(input$game_csv$datapath, show_col_types = FALSE)
  })

  observe({
    req(raw_game())
    teams <- sort(unique(raw_game()$CatcherTeam))
    updateSelectInput(session, "team_select", choices = teams)
  })

  game_data <- reactive({
    req(raw_game(), input$team_select)
    prep_catcher_data(raw_game(), input$team_select)
  })

  observe({
    req(game_data())
    dates <- sort(unique(as.Date(game_data()$framing$Date)), decreasing = TRUE)
    updateSelectInput(session, "game_date_select", choices = as.character(dates))
  })

  observe({
    req(game_data())
    catchers <- sort(unique(game_data()$framing$Catcher))
    updateSelectInput(session, "catcher_name", choices = catchers)
  })

  # ── Season CSVs ──
  raw_season <- reactive({
    req(input$season_csvs)
    bind_rows(lapply(input$season_csvs$datapath, function(f) {
      read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
    })) %>% type.convert(as.is = TRUE)
  })

  observe({
    req(raw_season())
    teams <- sort(unique(raw_season()$CatcherTeam))
    updateSelectInput(session, "season_team_select", choices = teams)
  })

  season_data <- reactive({
    req(raw_season(), input$season_team_select)
    prep_catcher_data(raw_season(), input$season_team_select)
  })

  # ── Generate ──
  catcher_pdf_path <- reactiveVal(NULL)

observeEvent(input$generate_catcher, {
  req(input$catcher_name, game_data(), season_data(), input$game_date_select)
  output$catcher_status <- renderUI({
    div(style = "color: orange; font-weight: bold;", "Generating report...")
  })
  tryCatch({
    tmp_pdf   <- tempfile(fileext = ".pdf")
    game_date <- as.Date(input$game_date_select)

    generate_catcher_pdf(
      game_framing    = game_data()$framing  %>% mutate(Date = as.Date(as.character(Date))),
      game_throwing   = game_data()$throwing %>% mutate(Date = as.Date(as.character(Date))),
      season_framing  = season_data()$framing  %>% mutate(Date = as.Date(as.character(Date))),
      season_throwing = season_data()$throwing %>% mutate(Date = as.Date(as.character(Date))),
      catcher     = input$catcher_name,
      game_date   = game_date,
      output_file = tmp_pdf,
      logo_path   = "www/logo.png"
    )

    catcher_pdf_path(tmp_pdf)
    output$catcher_status <- renderUI({
      div(style = "color: green; font-weight: bold;", "\u2713 Report ready!")
    })
  }, error = function(e) {
    output$catcher_status <- renderUI({
      div(style = "color: red;", paste("Error:", e$message))
    })
  })
})

  # ── Download ──
  output$catcher_download_ui <- renderUI({
    req(catcher_pdf_path())
    downloadButton("download_catcher_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_catcher_pdf <- downloadHandler(
    filename = function() {
      paste0(gsub(", ", "_", input$catcher_name), "_CatcherReport.pdf")
    },
    content = function(file) {
      req(catcher_pdf_path())
      file.copy(catcher_pdf_path(), file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)