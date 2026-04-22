library(shiny)
library(htmltools)
library(httr)
library(xml2)

library(xml2)

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

standings_error <- NULL

standings <- tryCatch(
  fetch_standings(),
  error = function(e) {
    standings_error <<- e$message
    NULL
  }
)
                      
apps <- list(
  list(
    id     = "catcher",
    title  = "Catcher Reports",
    url    = "#",
    status = "live"
  ),
  list(
    id     = "hitter",
    title  = "Postgame Hitter Reports",
    url    = "#",
    status = "live"
  ),
  list(
    id     = "pitcher",
    title  = "Postgame Pitcher Reports",
    url    = "#",
    status = "live"
  ),
  list(
    id     = "umpire",
    title  = "Umpire Reports",
    url    = "#",
    status = "live"
  ),
  list(
    id     = "mac",
    title  = "MAC — Matchup Analysis",
    url    = "#",
    status = "live"
  )
)

# ── Helper: render one card ────────────────────────────────────────────────────
make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class  <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"

  tags$a(
    href   = app$url,
    target = "_blank",
    class  = card_class,

    tags$img(src = paste0(app$id, ".png"), class = "card-img"),

    tags$div(
      class = "card-body",
      tags$div(class = "card-icon",  app$icon),
      tags$div(class = "card-title", app$title),
      tags$div(
        class = "card-footer",
        tags$span(class = badge_class, badge_label),
        tags$span(class = "card-arrow", "→")
      )
    )
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(

  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # ── Header
  tags$div(
    class = "hub-header",
    tags$div(
      class = "header-text",
      tags$h1("Brewster Whitecaps"),
      tags$p("Centralized Application Platform for Staff")
    ),
    tags$img(src = "logo.png", class = "team-logo")
  ),

 # ── Main
  tags$div(
    class = "hub-main",

    tags$div(class = "section-label", "Applications"),

    tags$div(
      class = "app-grid",
      lapply(apps, make_card)
    ),

    tags$div(
      class = "section-label",
      style = "margin-top: 40px;",
      "2025 Cape Cod League Standings"
    ),

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

  ), # ← closes hub-main

  # ── Footer
  tags$div(
    class = "hub-footer",
    paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
  )

) # ← closes fluidPage

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  output$east_standings <- renderTable({
    standings[[1]]
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$west_standings <- renderTable({
    standings[[2]]
  }, striped = TRUE, hover = TRUE, bordered = FALSE)
}

shinyApp(ui = ui, server = server)