# Adapter for Wally's JucoStatsApp inside the unified BASE application.

BASE_WALLY_JUCO_FILE <- base_project_path("WallyApps", "JucoStatsApp", "app.R")
BASE_WALLY_JUCO_REQUIRED_PACKAGES <- c("bslib", "dplyr", "DT", "readr", "shiny")

.base_wally_juco_state <- new.env(parent = emptyenv())

base_juco_embedded_head <- function() {
  htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
    .base-juco-workspace { color:var(--base-ink);font-family:var(--base-font-body); }
    .base-juco-workspace .base-juco-embedded > .nav { margin:0 18px;padding:7px;border:1px solid var(--base-border);border-radius:var(--base-radius);background:var(--base-surface);box-shadow:var(--base-shadow-sm); }
    .base-juco-workspace .base-juco-embedded > .nav .nav-link { border:0!important;border-radius:var(--base-radius-sm)!important;color:var(--base-muted)!important;font-weight:700; }
    .base-juco-workspace .base-juco-embedded > .nav .nav-link.active { background:var(--base-maroon)!important;color:#fff!important; }
    .base-juco-workspace .dt-wrap::before { background-image:url('/wally-juco-assets/Bobcatlogo.png'); }
  ")))
}

base_wally_juco_environment <- function() {
  if (exists("environment", envir = .base_wally_juco_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_wally_juco_state, inherits = FALSE))
  }

  missing <- BASE_WALLY_JUCO_REQUIRED_PACKAGES[
    !vapply(BASE_WALLY_JUCO_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("JUCO Stats dependencies are missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(BASE_WALLY_JUCO_FILE)) {
    stop("Wally JucoStatsApp source is unavailable at ", BASE_WALLY_JUCO_FILE)
  }

  asset_prefix <- "wally-juco-assets"
  if (!(asset_prefix %in% names(shiny::resourcePaths()))) {
    shiny::addResourcePath(
      asset_prefix,
      normalizePath(base_project_path("WallyApps", "JucoStatsApp", "www"), mustWork = TRUE)
    )
  }

  workspace <- new.env(parent = globalenv())
  workspace$BASE_JUCO_EMBEDDED <- TRUE
  workspace$BASE_JUCO_HEAD <- base_juco_embedded_head()
  sys.source(BASE_WALLY_JUCO_FILE, envir = workspace, chdir = TRUE, keep.source = FALSE)
  if (!inherits(workspace$ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("Wally JucoStatsApp did not expose a usable UI and server.")
  }

  assign("environment", workspace, envir = .base_wally_juco_state)
  workspace
}

base_juco_stats_workspace_ui <- function() {
  tags$div(
    class = "base-workspace-page base-juco-host",
    tags$div(
      class = "base-workspace-heading",
      tags$div(
        tags$div(class = "base-eyebrow", "Junior-college scouting pool"),
        tags$h1("JUCO Stats"),
        tags$p("Search and compare junior-college hitting and pitching leaderboards.")
      ),
      tags$div(
        class = "base-source-chip",
        tags$span(class = "home-status-dot"),
        "Wally JUCO leaderboard source"
      )
    ),
    tags$div(
      id = "base-juco-loading",
      class = "base-workspace-loading",
      tags$div(class = "base-loading-mark", "B"),
      tags$strong("Preparing JUCO Stats"),
      tags$span("The workspace loads once, when first opened.")
    ),
    shiny::uiOutput("base_juco_stats_app")
  )
}

base_juco_stats_workspace_server <- function(input, output, session) {
  workspace <- base_wally_juco_environment()
  output$base_juco_stats_app <- shiny::renderUI({
    tags$div(class = "base-juco-workspace", workspace$ui)
  })
  workspace$server(input, output, session)
  shinyjs::hide("base-juco-loading")
  invisible(workspace)
}
