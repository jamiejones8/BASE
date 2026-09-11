# Adapter for Wally's DefenseApp inside the unified BASE application.

BASE_WALLY_DEFENSE_FILE <- base_project_path("WallyApps", "DefenseApp", "DefenseApp.R")
BASE_WALLY_DEFENSE_REQUIRED_PACKAGES <- c(
  "bslib", "dplyr", "DT", "ggplot2", "gridExtra", "htmltools", "purrr", "readr",
  "shiny", "shinycssloaders", "shinyWidgets", "stringr", "tibble", "tidyr"
)

.base_wally_defense_state <- new.env(parent = emptyenv())

base_defense_dev_file <- function(name) {
  base_project_path("WallyApps", "DefenseApp", "data", name)
}

base_read_defense_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(source_file = basename(path), row_in_file = dplyr::row_number()) %>%
    readr::type_convert(col_types = readr::cols(.default = readr::col_guess()))
}

base_prepare_wally_defense_rows <- function() {
  rows <- tryCatch({
    if (exists("base_load_defense_team", mode = "function", inherits = TRUE)) {
      base_load_defense_team(TEAM_CONFIG$data_code)
    } else tibble::tibble()
  }, error = function(e) {
    message("Shared defense runtime unavailable: ", e$message)
    tibble::tibble()
  })

  if (!nrow(rows)) {
    return(base_read_defense_csv(base_defense_dev_file("BobcatsDefense2026.csv")))
  }

  rows <- tibble::as_tibble(rows)
  if (!"PitcherTeam" %in% names(rows) && "FieldingTeam" %in% names(rows)) {
    rows$PitcherTeam <- rows$FieldingTeam
  }
  for (position in c("1B", "2B", "3B", "SS", "LF", "CF", "RF")) {
    depth <- paste0(position, "_Depth")
    lateral <- paste0(position, "_Lateral")
    release_x <- paste0(position, "_PositionAtReleaseX")
    release_z <- paste0(position, "_PositionAtReleaseZ")
    if (!release_x %in% names(rows) && depth %in% names(rows)) rows[[release_x]] <- rows[[depth]]
    if (!release_z %in% names(rows) && lateral %in% names(rows)) rows[[release_z]] <- rows[[lateral]]
  }
  rows$source_file <- "2026 Defense - shared runtime.parquet"
  rows$row_in_file <- seq_len(nrow(rows))
  rows$SeasonGroup <- "S26"
  rows$DataSource <- "2026 NCAA Division I defense runtime"
  rows
}

base_prepare_wally_batted_rows <- function(defense_rows) {
  # The shared defense runtime is already joined to canonical pitch/contact
  # context. The standalone batted-ball companion is needed only in development.
  if (nrow(defense_rows) && any(grepl("shared runtime", defense_rows$source_file, fixed = TRUE))) {
    return(tibble::tibble())
  }
  base_read_defense_csv(base_defense_dev_file("BobcatsDefenseBattedBalls.csv"))
}

base_prepare_wally_catching_rows <- function(startup_rows = NULL) {
  rows <- tibble::as_tibble(startup_rows %||% tibble::tibble())
  if (nrow(rows) && "CatcherTeam" %in% names(rows)) {
    rows <- rows[base_team_matches(rows$CatcherTeam), , drop = FALSE]
  }
  if (!nrow(rows)) {
    rows <- base_read_defense_csv(base_defense_dev_file("Catchers - 2026 Season-cleaned.csv"))
  }
  if (!nrow(rows)) return(rows)
  rows$source_file <- "2026 Season - NCAA D1.parquet"
  rows$row_in_file <- seq_len(nrow(rows))
  rows$SeasonGroup <- "S26"
  rows$DataSource <- BASE_NCAA_D1_SOURCE_LABEL
  rows
}

base_prepare_catcher_framing_baseline <- function() {
  configured <- TEAM_CONFIG$data$catcher_framing_reference_file %||% ""
  path <- if (nzchar(configured)) configured else base_defense_dev_file("d1_catcher_framing_metrics.csv")
  if (!file.exists(path)) return(tibble::tibble(
    metric_id = character(), metric = character(), Catcher = character(),
    value = numeric(), chances = integer(), numerator = numeric(),
    metric_order = integer(), better = character()
  ))
  readr::read_csv(path, show_col_types = FALSE)
}

base_defense_embedded_head <- function() {
  htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
    .base-defense-workspace { color:var(--base-ink);font-family:var(--base-font-body); }
    .base-defense-workspace .base-defense-embedded-layout{display:grid;min-height:760px;grid-template-columns:minmax(210px,250px) minmax(0,1fr);gap:18px;align-items:start}
    .base-defense-workspace .base-defense-sidebar{position:sticky;top:72px;padding:20px;border:1px solid var(--base-border);border-radius:var(--base-radius);background:var(--base-surface);box-shadow:var(--base-shadow-sm)}
    .base-defense-workspace .base-defense-sidebar .sidebar-title{margin:0 0 16px;color:var(--base-ink);font-family:var(--base-font-display);font-size:20px;font-weight:600}
    .base-defense-workspace .base-defense-main{min-width:0}
    .base-defense-workspace .nav-tabs{display:flex;overflow-x:auto;gap:4px;padding:7px;border:1px solid var(--base-border);border-radius:var(--base-radius);background:var(--base-surface)!important;box-shadow:var(--base-shadow-sm)}
    .base-defense-workspace .nav-tabs .nav-link{border:0!important;border-radius:var(--base-radius-sm)!important;background:transparent!important;color:var(--base-muted)!important;font-size:12px;font-weight:650;white-space:nowrap}
    .base-defense-workspace .nav-tabs .nav-link.active{background:var(--base-maroon)!important;color:#fff!important;box-shadow:none!important}
    .base-defense-workspace .defense-panel,.base-defense-workspace .card,.base-defense-workspace .well{border:1px solid var(--base-border)!important;border-radius:var(--base-radius)!important;background:var(--base-surface)!important;box-shadow:var(--base-shadow-sm)!important}
    .base-defense-workspace table.dataTable thead th,.base-defense-workspace .table thead th{background:var(--base-maroon)!important;color:#fff!important}
    .base-defense-workspace .btn-primary{border-color:var(--base-maroon)!important;background:var(--base-maroon)!important;color:#fff!important}
    .base-defense-workspace .metric-tile,.base-defense-workspace .cr-percentile-card{border-color:var(--base-border)!important;background:var(--base-surface)!important;box-shadow:var(--base-shadow-sm)}
    .base-defense-workspace .cr-percentile-column{display:flex}.base-defense-workspace .cr-percentile-column>.shiny-html-output{display:flex;width:100%}
    .base-defense-workspace .cr-percentile-card{width:100%;min-height:0!important;padding:16px 18px;margin-bottom:12px;display:flex;flex-direction:column}
    .base-defense-workspace .cr-percentile-header{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;padding-bottom:9px;margin-bottom:10px;border-bottom:2px solid var(--base-maroon)}
    .base-defense-workspace .cr-percentile-title{color:var(--base-ink);font-family:var(--base-font-display);font-size:16px;font-weight:700;line-height:1.1}
    .base-defense-workspace .cr-percentile-subtitle{color:var(--base-maroon);font-size:12px;font-weight:750;text-align:right}
    .base-defense-workspace .cr-percentile-scale{display:grid;grid-template-columns:repeat(3,1fr);margin:0 46px 8px 128px;font-size:10px;font-weight:750;letter-spacing:.04em;text-transform:uppercase}
    .base-defense-workspace .cr-percentile-scale span:nth-child(1){color:#5D7EBC;text-align:left}.base-defense-workspace .cr-percentile-scale span:nth-child(2){color:var(--base-muted);text-align:center}.base-defense-workspace .cr-percentile-scale span:nth-child(3){color:#E33434;text-align:right}
    .base-defense-workspace .cr-percentile-rows{display:flex;flex:1;flex-direction:column;gap:7px}.base-defense-workspace .cr-percentile-row{display:grid;grid-template-columns:118px minmax(140px,1fr) 44px;gap:9px;align-items:center;min-height:24px}
    .base-defense-workspace .cr-percentile-label{color:var(--base-muted);font-size:11px;line-height:1.1;text-align:right}.base-defense-workspace .cr-percentile-track-wrap{position:relative;height:20px}
    .base-defense-workspace .cr-percentile-track{position:absolute;inset:7px 0 auto;height:6px;border-radius:999px;background:#e6eaec}.base-defense-workspace .cr-percentile-fill{position:absolute;left:0;top:5px;height:10px;border-radius:999px}
    .base-defense-workspace .cr-percentile-average{position:absolute;left:50%;top:3px;width:2px;height:14px;background:rgba(80,18,20,.28)}.base-defense-workspace .cr-percentile-badge{position:absolute;top:0;transform:translateX(-50%);min-width:28px;height:20px;padding:0 6px;border:2px solid #fff;border-radius:999px;box-shadow:0 1px 4px rgba(30,22,18,.2);color:#fff;font-size:10px;font-weight:800;line-height:16px;text-align:center}
    .base-defense-workspace .cr-percentile-value{color:var(--base-ink);font-variant-numeric:tabular-nums;font-size:11px;text-align:right;white-space:nowrap}.base-defense-workspace .cr-percentile-row-empty{opacity:.5}.base-defense-workspace .cr-percentile-empty{color:var(--base-muted);font-size:13px}
    @media(max-width:900px){.base-defense-workspace .base-defense-embedded-layout{display:block!important}.base-defense-workspace .base-defense-sidebar{position:static;margin-bottom:16px}}
  ")))
}

base_wally_defense_environment <- function(startup_rows = NULL) {
  if (exists("environment", envir = .base_wally_defense_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_wally_defense_state, inherits = FALSE))
  }
  missing <- BASE_WALLY_DEFENSE_REQUIRED_PACKAGES[
    !vapply(BASE_WALLY_DEFENSE_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) stop("Defense workspace dependencies are missing: ", paste(missing, collapse = ", "))
  if (!file.exists(BASE_WALLY_DEFENSE_FILE)) stop("Wally DefenseApp source is unavailable at ", BASE_WALLY_DEFENSE_FILE)

  defense_rows <- base_prepare_wally_defense_rows()
  workspace <- new.env(parent = globalenv())
  workspace$BASE_DEFENSE_DATA <- defense_rows
  workspace$BASE_DEFENSE_BATTED_DATA <- base_prepare_wally_batted_rows(defense_rows)
  workspace$BASE_CATCHING_DATA <- base_prepare_wally_catching_rows(startup_rows)
  workspace$BASE_CATCHER_FRAMING_BASELINE <- base_prepare_catcher_framing_baseline()
  workspace$BASE_DEFENSE_EMBEDDED <- TRUE
  workspace$BASE_DEFENSE_THEME <- bslib::bs_theme(version = 3, primary = TEAM_CONFIG$colors$primary)
  workspace$BASE_DEFENSE_HEAD <- base_defense_embedded_head()
  sys.source(BASE_WALLY_DEFENSE_FILE, envir = workspace, chdir = TRUE, keep.source = FALSE)
  if (!inherits(workspace$base_catching_postgame_ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("Wally DefenseApp did not expose a usable UI and server.")
  }
  assign("environment", workspace, envir = .base_wally_defense_state)
  workspace
}

base_team_defense_workspace_ui <- function() {
  tags$div(
    class = "base-workspace-page base-defense-host",
    tags$div(
      class = "base-workspace-heading",
      tags$div(tags$div(class = "base-eyebrow", "Run prevention"), tags$h1("Defensive Analytics"), tags$p("Opportunities, OAA, positioning, and interactive catcher receiving in one workspace.")),
      tags$div(class = "base-source-chip", tags$span(class = "home-status-dot"), "Shared defense runtime + canonical catching source")
    ),
    tags$div(id = "base-defense-loading", class = "base-workspace-loading", tags$div(class = "base-loading-mark", "B"), tags$strong("Preparing Defense"), tags$span("The workspace loads once, when first opened.")),
    shiny::uiOutput("base_team_defense_app")
  )
}

base_team_defense_workspace_server <- function(input, output, session, startup_rows = NULL) {
  workspace <- base_wally_defense_environment(startup_rows)
  output$base_team_defense_app <- shiny::renderUI(tags$div(class = "base-defense-workspace", workspace$ui))
  output$base_postgame_catching_aar <- shiny::renderUI({
    tags$div(
      class = "base-defense-workspace base-postgame-aar",
      workspace$head_css,
      workspace$base_catching_postgame_ui
    )
  })
  workspace$server(input, output, session)
  shinyjs::hide("base-defense-loading")
  invisible(workspace)
}
