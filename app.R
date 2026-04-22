library(shiny)
library(htmltools)

# ── Card data ──────────────────────────────────────────────────────────────────
# When a HF Space is deployed, swap "#" for the full URL.

apps <- list(
  list(
    title  = "Scouting Reports",
    url    = "#",
    status = "live"
  ),
  list(
    title  = "Postgame Hitter Reports",
    url    = "#",
    status = "live"
  ),
  list(
    title  = "Postgame Pitcher Reports",
    url    = "#",
    status = "live"
  ),
  list(
    title  = "Umpire Reports",
    desc   = "Zone tendency and called-strike rate profiles for upcoming umpire assignments.",
    url    = "#",
    status = "live"
  ),
  list(
    title  = "MAC — Matchup Analysis",
    url    = "#",
    status = "live"
  )
)

# ── Helper: render one card ────────────────────────────────────────────────────

make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"

  tags$a(
    href   = app$url,
    target = "_blank",
    class  = card_class,
    tags$div(class = "card-title", app$title),
    tags$div(class = "card-desc",  app$desc),

    tags$div(
      class = "card-footer",
      tags$span(class = badge_class, badge_label),
      tags$span(class = "card-arrow", "→")
    )
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  # Pull in Google Fonts + our stylesheet
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
      tags$p("Analytics Hub · Cape Cod Baseball League")
    )
  ),

  # ── Main
  tags$div(
    class = "hub-main",

    tags$div(class = "section-label", "Applications"),

    tags$div(
      class = "app-grid",
      lapply(apps, make_card)
    )
  ),

  # ── Footer
  tags$div(
    class = "hub-footer",
    paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
# Framework only — no server logic needed at this stage.

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)