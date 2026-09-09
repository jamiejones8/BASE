# Adapter for Wally's JucoStatsApp inside the unified BASE application.

BASE_WALLY_JUCO_FILE <- base_project_path("WallyApps", "JucoStatsApp", "app.R")
BASE_WALLY_JUCO_REQUIRED_PACKAGES <- c("bslib", "dplyr", "DT", "readr", "shiny")

.base_wally_juco_state <- new.env(parent = emptyenv())

base_juco_embedded_head <- function() {
  htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
    .base-juco-host { max-width:1600px; }
    .base-juco-workspace {
      color:var(--base-ink);
      font-family:var(--base-font-body);
    }
    .base-juco-workspace .base-juco-embedded > .nav,
    .base-juco-workspace .base-juco-embedded > ul.nav-tabs {
      display:inline-flex;
      gap:5px;
      width:auto;
      margin:0 0 14px;
      padding:5px;
      border:1px solid var(--base-border);
      border-radius:999px;
      background:var(--base-surface);
      box-shadow:var(--base-shadow-sm);
    }
    .base-juco-workspace .base-juco-embedded > .nav > li,
    .base-juco-workspace .base-juco-embedded > ul.nav-tabs > li { margin:0; }
    .base-juco-workspace .base-juco-embedded > .nav .nav-link,
    .base-juco-workspace .base-juco-embedded > .nav > li > a,
    .base-juco-workspace .base-juco-embedded > ul.nav-tabs > li > a {
      min-width:112px;
      margin:0;
      padding:9px 20px;
      border:0!important;
      border-radius:999px!important;
      background:transparent!important;
      color:var(--base-muted)!important;
      font-size:14px;
      font-weight:750;
      text-align:center;
      transition:background 140ms ease,color 140ms ease,box-shadow 140ms ease;
    }
    .base-juco-workspace .base-juco-embedded > .nav .nav-link.active,
    .base-juco-workspace .base-juco-embedded > .nav > li.active > a,
    .base-juco-workspace .base-juco-embedded > ul.nav-tabs > li.active > a {
      background:var(--base-maroon)!important;
      box-shadow:0 5px 14px rgba(80,18,20,.2);
      color:#fff!important;
    }
    .base-juco-workspace .tab-content,
    .base-juco-workspace .tab-pane { overflow:visible; }
    .base-juco-workspace .juco-shell {
      max-width:none;
      margin:0;
      padding:0 0 28px;
    }
    .base-juco-workspace .common-strip {
      display:grid;
      grid-template-columns:190px minmax(0,1fr);
      gap:18px;
      align-items:center;
      margin:0 0 12px;
      padding:15px 18px;
      border:1px solid rgba(215,189,138,.28);
      border-left:0;
      border-radius:var(--base-radius);
      background:
        radial-gradient(circle at 92% 0,rgba(215,189,138,.18),transparent 28%),
        linear-gradient(110deg,var(--base-maroon-deep),var(--base-maroon));
      box-shadow:0 12px 28px rgba(49,10,12,.13);
      color:#fff;
    }
    .base-juco-workspace .juco-guide-copy { display:grid;gap:2px; }
    .base-juco-workspace .juco-guide-copy strong {
      color:#fff;
      font-family:var(--base-font-display);
      font-size:21px;
      font-weight:750;
      letter-spacing:.01em;
    }
    .base-juco-workspace .juco-kicker {
      color:var(--base-gold-bright);
      font-family:var(--base-font-data);
      font-size:10px;
      font-weight:700;
      letter-spacing:.12em;
    }
    .base-juco-workspace .juco-stat-pills { display:flex;flex-wrap:wrap;gap:6px; }
    .base-juco-workspace .juco-stat-pill {
      min-width:42px;
      padding:5px 8px;
      border:1px solid rgba(255,255,255,.16);
      border-radius:6px;
      background:rgba(255,255,255,.08);
      color:#fff;
      font-family:var(--base-font-data);
      font-size:11px;
      font-weight:700;
      text-align:center;
    }
    .base-juco-workspace .juco-filter-card {
      margin-bottom:12px;
      padding:15px 16px 14px;
      border:1px solid var(--base-border);
      border-radius:var(--base-radius);
      background:var(--base-surface);
      box-shadow:var(--base-shadow-sm);
    }
    .base-juco-workspace .juco-filter-heading,
    .base-juco-workspace .juco-table-heading {
      display:flex;
      justify-content:space-between;
      gap:18px;
      align-items:center;
    }
    .base-juco-workspace .juco-filter-heading { margin-bottom:12px;padding:0 2px 10px;border-bottom:1px solid var(--base-border); }
    .base-juco-workspace .juco-filter-heading h3,
    .base-juco-workspace .juco-table-heading h2 {
      margin:2px 0 0;
      color:var(--base-ink);
      font-family:var(--base-font-display);
      font-weight:750;
      line-height:1.05;
    }
    .base-juco-workspace .juco-filter-heading h3 { font-size:19px; }
    .base-juco-workspace .juco-filter-hint,
    .base-juco-workspace .juco-table-heading p { color:var(--base-muted);font-size:12px; }
    .base-juco-workspace .filter-row,
    .base-juco-workspace .filter-row.pitching {
      gap:9px;
      margin:0;
    }
    .base-juco-workspace .checkbox-panel {
      min-height:78px;
      padding:10px 12px;
      border:1px solid var(--base-border);
      border-radius:var(--base-radius-sm);
      background:var(--base-surface-soft);
    }
    .base-juco-workspace .checkbox-panel label.control-label,
    .base-juco-workspace .checkbox-panel > .form-group > label {
      margin-bottom:7px;
      color:var(--base-maroon);
      font-family:var(--base-font-data);
      font-size:10px;
      font-weight:700;
      letter-spacing:.08em;
    }
    .base-juco-workspace .checkbox-panel .form-check,
    .base-juco-workspace .checkbox-panel .checkbox-inline { margin:2px 12px 2px 0; }
    .base-juco-workspace .checkbox-panel .form-check-label,
    .base-juco-workspace .checkbox-panel .checkbox-inline { color:var(--base-ink);font-size:12px;font-weight:650; }
    .base-juco-workspace .checkbox-panel input[type='checkbox'] { accent-color:var(--base-maroon); }
    .base-juco-workspace .juco-leaderboard-card {
      overflow:hidden;
      border:1px solid var(--base-border);
      border-radius:var(--base-radius);
      background:var(--base-surface);
      box-shadow:var(--base-shadow);
    }
    .base-juco-workspace .juco-table-heading {
      padding:16px 18px;
      border-bottom:1px solid var(--base-border);
      background:linear-gradient(110deg,var(--base-surface),var(--base-surface-soft));
    }
    .base-juco-workspace .juco-table-heading h2 { font-size:23px; }
    .base-juco-workspace .juco-table-heading p { margin:5px 0 0; }
    .base-juco-workspace .juco-role-badge {
      padding:6px 9px;
      border-radius:5px;
      background:var(--base-maroon);
      color:#fff;
      font-family:var(--base-font-data);
      font-size:10px;
      font-weight:700;
      letter-spacing:.1em;
    }
    .base-juco-workspace .dt-wrap {
      padding:14px 16px 16px;
      border:0;
      background:var(--base-surface);
    }
    .base-juco-workspace .dt-wrap::before {
      background-image:url('/wally-juco-assets/Bobcatlogo.png');
      background-position:center 62%;
      background-size:min(34vw,360px) auto;
      opacity:.025;
    }
    .base-juco-workspace .dataTables_wrapper { color:var(--base-ink);font-size:13px; }
    .base-juco-workspace .dataTables_length,
    .base-juco-workspace .dataTables_filter { margin:0 0 12px;color:var(--base-muted);font-size:12px;font-weight:650; }
    .base-juco-workspace .dataTables_filter input,
    .base-juco-workspace .dataTables_length select {
      min-height:34px;
      margin-left:7px;
      border:1px solid var(--base-border-strong);
      border-radius:7px;
      background:#fff;
      color:var(--base-ink);
      outline:none;
    }
    .base-juco-workspace .dataTables_filter input:focus,
    .base-juco-workspace .dataTables_length select:focus { border-color:var(--base-maroon);box-shadow:0 0 0 3px rgba(80,18,20,.09); }
    .base-juco-workspace table.dataTable { border:1px solid var(--base-border)!important;border-radius:8px;overflow:hidden; }
    .base-juco-workspace table.dataTable thead tr:first-child th {
      padding:10px 8px!important;
      border-right:1px solid rgba(255,255,255,.09)!important;
      background:var(--base-maroon-deep)!important;
      color:var(--base-gold-bright)!important;
      font-family:var(--base-font-data);
      font-size:10px;
      font-weight:700;
      letter-spacing:.04em;
      text-transform:uppercase;
    }
    .base-juco-workspace table.dataTable thead tr:nth-child(2) th {
      padding:7px 6px!important;
      border-right:1px solid var(--base-border)!important;
      border-bottom:1px solid var(--base-border-strong)!important;
      background:#f4efe8!important;
    }
    .base-juco-workspace table.dataTable thead input[type='search'],
    .base-juco-workspace table.dataTable thead input[type='text'],
    .base-juco-workspace table.dataTable thead input[type='number'],
    .base-juco-workspace .juco-head-filter-btn {
      min-height:30px;
      border:1px solid rgba(80,18,20,.18);
      border-radius:6px;
      background:#fff;
      color:var(--base-ink);
      font-size:11px;
    }
    .base-juco-workspace table.dataTable tbody td {
      padding:9px 10px;
      border-bottom:1px solid rgba(80,18,20,.07)!important;
      background:rgba(255,255,255,.94)!important;
      color:var(--base-ink);
      font-size:12.5px;
    }
    .base-juco-workspace table.dataTable tbody tr.odd td { background:rgba(248,245,240,.94)!important; }
    .base-juco-workspace table.dataTable tbody tr:hover td { background:#f3eadb!important; }
    .base-juco-workspace table.dataTable tbody td:first-child { color:var(--base-maroon);font-weight:750; }
    .base-juco-workspace table.dataTable tbody td:nth-child(2) { font-weight:650; }
    .base-juco-workspace .cf-cell {
      margin:-5px -6px;
      padding:5px 6px;
      border-radius:5px;
      text-align:center;
      font-family:var(--base-font-data);
      font-size:12px;
      font-weight:700;
    }
    .base-juco-workspace .juco-head-filter-menu {
      border:1px solid var(--base-border-strong);
      border-radius:8px;
      box-shadow:var(--base-shadow);
    }
    .base-juco-workspace .dataTables_info { padding-top:14px!important;color:var(--base-muted);font-size:12px; }
    .base-juco-workspace .dataTables_paginate { padding-top:10px!important; }
    .base-juco-workspace .dataTables_paginate .paginate_button {
      min-width:32px;
      margin:0 2px;
      padding:5px 8px!important;
      border:0!important;
      border-radius:6px!important;
      color:var(--base-muted)!important;
    }
    .base-juco-workspace .dataTables_paginate .paginate_button.current {
      background:var(--base-maroon)!important;
      box-shadow:none!important;
      color:#fff!important;
    }
    @media(max-width:900px){
      .base-juco-workspace .common-strip { grid-template-columns:1fr;gap:10px; }
      .base-juco-workspace .juco-filter-heading { align-items:flex-start;flex-direction:column;gap:3px; }
    }
    @media(max-width:640px){
      .base-juco-workspace .base-juco-embedded > .nav,
      .base-juco-workspace .base-juco-embedded > ul.nav-tabs { display:flex;width:100%; }
      .base-juco-workspace .base-juco-embedded > .nav > li,
      .base-juco-workspace .base-juco-embedded > ul.nav-tabs > li { flex:1; }
      .base-juco-workspace .base-juco-embedded > .nav .nav-link,
      .base-juco-workspace .base-juco-embedded > .nav > li > a,
      .base-juco-workspace .base-juco-embedded > ul.nav-tabs > li > a { width:100%;min-width:0;padding:9px 12px; }
      .base-juco-workspace .juco-table-heading { align-items:flex-start; }
      .base-juco-workspace .juco-filter-hint { display:none; }
      .base-juco-workspace .dt-wrap { padding:10px; }
    }
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
    stop("JUCO Scouting dependencies are missing: ", paste(missing, collapse = ", "))
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
        tags$h1("JUCO Scouting"),
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
      tags$strong("Preparing JUCO Scouting"),
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
