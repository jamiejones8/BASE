# Adapter for the Sports Science VALD/SmartSpeed dashboard inside BASE.

BASE_PLAYER_HEALTH_ROOT <- base_project_path("Sports Science", "vald_shiny_app")
BASE_PLAYER_HEALTH_FILE <- file.path(BASE_PLAYER_HEALTH_ROOT, "dashboard.R")
BASE_PLAYER_HEALTH_REQUIRED_PACKAGES <- c(
  "bslib", "dplyr", "DT", "httr", "jsonlite", "lubridate", "plotly",
  "purrr", "readr", "rlang", "shiny", "stringr", "tibble", "tidyr"
)

.base_player_health_state <- new.env(parent = emptyenv())

base_player_health_path <- function(env_name, ...) {
  configured <- Sys.getenv(env_name, unset = "")
  if (nzchar(trimws(configured))) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(BASE_PLAYER_HEALTH_ROOT, ...), winslash = "/", mustWork = FALSE)
}

base_player_health_embedded_head <- function() {
  htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
    .base-player-health-host { max-width:1800px; }
    .base-player-health-workspace,
    .base-player-health-embedded {
      --txst-maroon:var(--base-maroon);
      --txst-gold:var(--base-gold-bright);
      --bg-page:var(--base-page);
      --bg-card:var(--base-surface);
      --border-card:var(--base-border);
      --text-heading:var(--base-ink);
      --text-body:var(--base-ink-soft);
      --text-muted:var(--base-muted);
      --status-normal-bg:#eaf6ee;--status-normal-fg:#226339;
      --status-perf-bg:#e8f0fe;--status-perf-fg:#1d4ed8;
      --status-watch-bg:#fff2d9;--status-watch-fg:#9a4d00;
      --status-action-bg:#feeceb;--status-action-fg:#8d1b13;
      --status-na-bg:#eef0f3;--status-na-fg:#475467;
      --change-up-bg:#eaf6ee;--change-up-fg:#226339;
      --change-down-bg:#feeceb;--change-down-fg:#8d1b13;
      --disclaimer-bg:#fbf3e6;--disclaimer-border:#e8cfa0;--disclaimer-fg:#6b4a1f;
      color:var(--text-body);
      font-family:var(--base-font-body);
    }
    .base-player-health-host .base-workspace-heading-actions {
      display:flex;align-items:center;justify-content:flex-end;flex-wrap:wrap;gap:9px;
    }
    .base-player-health-host .base-player-health-refresh { margin:0;white-space:nowrap; }
    .base-player-health-embedded > .row:first-child {
      align-items:center;margin:0 0 14px;padding:12px 14px;border:1px solid var(--base-border);
      border-radius:var(--base-radius-sm);background:var(--base-surface-soft);
    }
    .base-player-health-embedded h1,
    .base-player-health-embedded h2,
    .base-player-health-embedded h3,
    .base-player-health-embedded h4,
    .base-player-health-embedded h5,
    .base-player-health-embedded h6 {
      color:var(--text-heading);font-family:var(--base-font-display);font-weight:750;letter-spacing:-.015em;
    }
    .base-player-health-embedded > .nav,
    .base-player-health-embedded > ul.nav-tabs {
      display:flex;flex-wrap:wrap;gap:5px;margin:0 0 18px;padding:5px;border:1px solid var(--base-border);
      border-radius:10px;background:var(--base-surface);box-shadow:var(--base-shadow-sm);
    }
    .base-player-health-embedded > .nav > li,
    .base-player-health-embedded > ul.nav-tabs > li { margin:0; }
    .base-player-health-embedded > .nav .nav-link,
    .base-player-health-embedded > .nav > li > a,
    .base-player-health-embedded > ul.nav-tabs > li > a {
      margin:0;padding:9px 13px;border:0!important;border-radius:7px!important;background:transparent!important;
      color:var(--base-muted)!important;font-size:12px;font-weight:750;
    }
    .base-player-health-embedded > .nav .nav-link.active,
    .base-player-health-embedded > .nav > li.active > a,
    .base-player-health-embedded > ul.nav-tabs > li.active > a {
      background:var(--base-maroon)!important;box-shadow:0 4px 12px rgba(80,18,20,.18);color:#fff!important;
    }
    .base-player-health-embedded .tab-content,
    .base-player-health-embedded .tab-pane { min-width:0;overflow:visible; }
    .base-player-health-embedded .card,
    .base-player-health-embedded .bslib-card {
      margin-bottom:14px;border:1px solid var(--border-card)!important;border-radius:var(--base-radius)!important;
      background:var(--bg-card)!important;box-shadow:var(--base-shadow-sm)!important;
    }
    .base-player-health-embedded .bslib-card .card-header {
      border-bottom:1px solid var(--border-card)!important;background:linear-gradient(110deg,var(--base-surface),var(--base-surface-soft))!important;
      color:var(--text-heading)!important;font-weight:800;
    }
    .base-player-health-embedded .bslib-card .card-body {
      overflow:visible!important;background:var(--bg-card)!important;color:var(--text-body)!important;
    }
    .base-player-health-embedded .eyebrow,
    .base-player-health-embedded .kpi-title,
    .base-player-health-embedded .txst-label {
      color:var(--text-muted)!important;font-family:var(--base-font-data);font-size:10px;font-weight:700;
      letter-spacing:.08em;text-transform:uppercase;
    }
    .base-player-health-embedded .subtle,
    .base-player-health-embedded .tiny { color:var(--text-muted)!important; }
    .base-player-health-embedded .kpi {
      position:relative;overflow:hidden;padding:12px 14px 12px 18px;border:1px solid var(--border-card);
      border-radius:var(--base-radius-sm);background:var(--bg-card);box-shadow:var(--base-shadow-sm);
    }
    .base-player-health-embedded .kpi::before {
      position:absolute;top:8px;bottom:8px;left:0;width:4px;border-radius:0 4px 4px 0;background:var(--txst-gold);content:'';
    }
    .base-player-health-embedded .kpi-value { margin-top:2px;color:var(--text-heading)!important;font-family:var(--base-font-display);font-size:24px;font-weight:800; }
    .base-player-health-embedded .metric-detail { margin-top:4px;color:var(--text-muted);font-size:12px; }
    .base-player-health-embedded .status-pill {
      display:inline-block;padding:4px 11px;border-radius:999px;font-size:11px;font-weight:800;white-space:nowrap;
    }
    .base-player-health-embedded .status-pill-normal { background:var(--status-normal-bg);color:var(--status-normal-fg); }
    .base-player-health-embedded .status-pill-perf { background:var(--status-perf-bg);color:var(--status-perf-fg); }
    .base-player-health-embedded .status-pill-watch { background:var(--status-watch-bg);color:var(--status-watch-fg); }
    .base-player-health-embedded .status-pill-action { background:var(--status-action-bg);color:var(--status-action-fg); }
    .base-player-health-embedded .status-pill-na { background:var(--status-na-bg);color:var(--status-na-fg); }
    .base-player-health-embedded .coverage-badge,
    .base-player-health-embedded .stat-check-badge {
      display:inline-flex;align-items:center;gap:5px;padding:3px 9px;border:1px dashed var(--border-card);
      border-radius:999px;color:var(--text-muted);font-size:10px;font-weight:700;
    }
    .base-player-health-embedded .coverage-badge .dot,
    .base-player-health-embedded .stat-check-badge .dot { width:6px;height:6px;border-radius:50%;background:var(--text-muted); }
    .base-player-health-embedded .coverage-badge.covered { color:var(--text-heading); }
    .base-player-health-embedded .coverage-badge.covered .dot { background:var(--txst-gold); }
    .base-player-health-embedded .stat-check-badge.confirmed { border-style:solid;border-color:var(--status-normal-fg);color:var(--status-normal-fg); }
    .base-player-health-embedded .stat-check-badge.confirmed .dot { background:var(--status-normal-fg); }
    .base-player-health-embedded .change-pill { display:inline-block;padding:2px 8px;border-radius:999px;font-size:11px;font-weight:800; }
    .base-player-health-embedded .change-pill-up { background:var(--change-up-bg);color:var(--change-up-fg); }
    .base-player-health-embedded .change-pill-down { background:var(--change-down-bg);color:var(--change-down-fg); }
    .base-player-health-embedded .cmj-sparkline-svg polyline { fill:none;stroke-width:2.4;stroke-linecap:round;stroke-linejoin:round; }
    .base-player-health-embedded .cmj-sparkline-svg.spark-up polyline { stroke:var(--status-normal-fg); }
    .base-player-health-embedded .cmj-sparkline-svg.spark-down polyline { stroke:var(--status-action-fg); }
    .base-player-health-embedded .cmj-sparkline-table { width:100%;border-collapse:collapse;font-size:12px; }
    .base-player-health-embedded .cmj-sparkline-table th,
    .base-player-health-embedded .cmj-sparkline-table td { padding:6px 10px;border-bottom:1px solid var(--border-card);white-space:nowrap; }
    .base-player-health-embedded .cmj-sparkline-table th { color:var(--text-muted);font-weight:700;text-align:left; }
    .base-player-health-embedded .cmj-sparkline-table .trend-up { color:var(--status-normal-fg);font-weight:700; }
    .base-player-health-embedded .cmj-sparkline-table .trend-down { color:var(--status-action-fg);font-weight:700; }
    .base-player-health-embedded .cmj-sparkline-table .trend-flat { color:var(--text-muted); }
    .base-player-health-embedded .disclaimer-box {
      padding:9px 12px;border:1px solid var(--disclaimer-border);border-radius:8px;background:var(--disclaimer-bg);
      color:var(--disclaimer-fg)!important;font-size:11px;
    }
    .base-player-health-embedded .athlete-hero {
      display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:14px;margin-bottom:8px;padding:18px 20px;
      border-radius:var(--base-radius);background:linear-gradient(120deg,var(--base-maroon-deep),var(--base-maroon));color:#fff;
    }
    .base-player-health-embedded .athlete-hero .name { color:#fff;font-family:var(--base-font-display);font-size:25px;font-weight:800; }
    .base-player-health-embedded .athlete-hero .meta { margin-top:2px;color:rgba(255,255,255,.75);font-size:12px; }
    .base-player-health-embedded .chart-card-xl .js-plotly-plot,
    .base-player-health-embedded .chart-card-xl .plotly { height:clamp(420px,55vh,640px)!important; }
    .base-player-health-embedded .chart-card-lg .js-plotly-plot,
    .base-player-health-embedded .chart-card-lg .plotly { height:clamp(340px,45vh,480px)!important; }
    .base-player-health-embedded .chart-card-md .js-plotly-plot,
    .base-player-health-embedded .chart-card-md .plotly { height:clamp(260px,35vh,360px)!important; }
    .base-player-health-embedded .chart-card-sm .js-plotly-plot,
    .base-player-health-embedded .chart-card-sm .plotly { height:clamp(180px,25vh,240px)!important; }
    .base-player-health-embedded table.dataTable thead th,
    .base-player-health-embedded table thead th {
      border-bottom:2px solid var(--txst-gold)!important;background:var(--bg-card)!important;color:var(--txst-maroon)!important;font-weight:800;
    }
    .base-player-health-embedded table.dataTable,
    .base-player-health-embedded table.dataTable td,
    .base-player-health-embedded table.dataTable th,
    .base-player-health-embedded table td,
    .base-player-health-embedded table th { border-right:0!important;border-left:0!important;color:var(--text-body); }
    .base-player-health-embedded .curve-panel {
      margin-top:10px;padding:12px;border:1px solid var(--border-card);border-radius:var(--base-radius);background:var(--bg-card);
    }
    .base-player-health-embedded .curve-title { margin-bottom:6px;color:var(--txst-maroon);font-weight:800; }
    .base-player-health-embedded .curve-divider { margin:10px 0;border:0;border-top:1px solid var(--border-card); }
    .base-player-health-embedded .btn-primary,
    .base-player-health-embedded .btn-default:hover { border-color:var(--base-maroon);background:var(--base-maroon);color:#fff; }
    .base-player-health-embedded input[type='checkbox'] { accent-color:var(--base-maroon); }
    @media(max-width:900px){
      .base-player-health-embedded > .nav > li,
      .base-player-health-embedded > ul.nav-tabs > li { flex:1 1 145px; }
      .base-player-health-embedded > .nav > li > a,
      .base-player-health-embedded > ul.nav-tabs > li > a { width:100%;text-align:center; }
      .base-player-health-embedded .bslib-sidebar-layout { --_sidebar-width:100%!important; }
    }
  ")))
}

base_player_health_environment <- function() {
  if (exists("environment", envir = .base_player_health_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_player_health_state, inherits = FALSE))
  }

  missing <- BASE_PLAYER_HEALTH_REQUIRED_PACKAGES[
    !vapply(BASE_PLAYER_HEALTH_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Player Health dependencies are missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(BASE_PLAYER_HEALTH_FILE)) {
    stop("Player Health source is unavailable at ", BASE_PLAYER_HEALTH_FILE)
  }

  configured_roster <- Sys.getenv("BASE_PLAYER_HEALTH_ROSTER_FILE", unset = "")
  if (!nzchar(trimws(configured_roster)) && exists("TEAM_CONFIG", inherits = TRUE)) {
    roster_candidate <- get("TEAM_CONFIG", inherits = TRUE)$data$roster_file
    configured_roster <- if (is.null(roster_candidate)) "" else roster_candidate
  }
  if (!nzchar(trimws(configured_roster))) {
    configured_roster <- base_project_path("config", "texas_state_roster_2027.csv")
  }

  Sys.setenv(
    VALD_APP_ROOT = normalizePath(BASE_PLAYER_HEALTH_ROOT, winslash = "/", mustWork = TRUE),
    CMJ_SPRINT_SHARE_GOLD_DIR = base_player_health_path("BASE_PLAYER_HEALTH_GOLD_DIR", "data", "gold"),
    CMJ_SPRINT_SHARE_LEGACY_DIR = base_player_health_path("BASE_PLAYER_HEALTH_LEGACY_DIR", "data", "legacy"),
    VALD_REFRESH_DIR = base_player_health_path("BASE_PLAYER_HEALTH_STATE_DIR", "data", "refresh"),
    BASE_PLAYER_HEALTH_ROSTER_FILE = normalizePath(configured_roster, winslash = "/", mustWork = FALSE)
  )

  workspace <- new.env(parent = globalenv())
  workspace$BASE_PLAYER_HEALTH_EMBEDDED <- TRUE
  sys.source(BASE_PLAYER_HEALTH_FILE, envir = workspace, chdir = TRUE, keep.source = FALSE)
  if (!inherits(workspace$ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("The Player Health dashboard did not expose a usable embedded UI and server.")
  }

  assign("environment", workspace, envir = .base_player_health_state)
  workspace
}

base_player_health_workspace_ui <- function() {
  htmltools::tagList(
    base_player_health_embedded_head(),
    htmltools::tags$div(
      class = "base-workspace-page base-player-health-host",
      htmltools::tags$div(
        class = "base-workspace-heading",
        htmltools::tags$div(
          htmltools::tags$div(class = "base-eyebrow", "Sports science monitoring"),
          htmltools::tags$h1("Player Health"),
          htmltools::tags$p("Review CMJ readiness, sprint performance, athlete trends, alerts, and ForceDecks traces.")
        ),
        htmltools::tags$div(
          class = "base-workspace-heading-actions",
          htmltools::tags$div(
            class = "base-source-chip",
            htmltools::tags$span(class = "home-status-dot"),
            "VALD + SmartSpeed"
          ),
          shiny::actionButton(
            "reload_data",
            "Refresh data",
            icon = shiny::icon("arrows-rotate"),
            class = "btn btn-primary base-player-health-refresh"
          )
        )
      ),
      htmltools::tags$div(
        id = "base-player-health-loading",
        class = "base-workspace-loading",
        htmltools::tags$div(class = "base-loading-mark", "B"),
        htmltools::tags$strong("Preparing Player Health"),
        htmltools::tags$span("The monitoring workspace loads once, when first opened.")
      ),
      shiny::uiOutput("base_player_health_app")
    )
  )
}

base_player_health_workspace_server <- function(input, output, session) {
  workspace <- base_player_health_environment()
  output$base_player_health_app <- shiny::renderUI({
    htmltools::tags$div(class = "base-player-health-workspace", workspace$ui)
  })
  workspace$server(input, output, session)
  shinyjs::hide("base-player-health-loading")
  invisible(workspace)
}
