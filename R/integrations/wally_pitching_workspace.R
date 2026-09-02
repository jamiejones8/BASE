# Adapter for Wally's PitchingApp inside the unified BASE application.
#
# The original application remains the calculation authority. BASE supplies a
# normalized team-only payload and a scoped visual shell, then evaluates the
# app in an isolated environment on the first visit to the Pitching workspace.

BASE_WALLY_PITCHING_FILE <- base_project_path(
  "WallyApps", "PitchingApp", "PitchingApp.R"
)

BASE_WALLY_PITCHING_REQUIRED_PACKAGES <- c(
  "bslib", "cowplot", "dplyr", "DT", "ggplot2", "ggplotify",
  "ggpubr", "ggtext", "gridExtra", "hms", "htmltools", "jpeg",
  "lubridate", "magrittr", "patchwork", "plotly", "png", "purrr",
  "ragg", "readr", "rlang", "scales", "shiny", "shinycssloaders",
  "shinyWidgets", "stringr", "tibble", "tidyr"
)

.base_team_pitching_cache <- new.env(parent = emptyenv())
.base_wally_pitching_state <- new.env(parent = emptyenv())

BASE_WALLY_PITCHING_DATA_FILES <- c(
  S25 = "2025 Season -cleaned.csv",
  F25 = "2025 Fall -cleaned.csv",
  SQ26 = "2026 Squads - cleaned.csv",
  S26 = "2026 Season - cleaned.csv",
  BP = "Bullpens - cleaned.csv"
)

base_pitching_dev_supplement_paths <- function() {
  root <- base_project_path("WallyApps", "PitchingApp", "data")
  file.path(root, unname(BASE_WALLY_PITCHING_DATA_FILES))
}

base_pitching_supplement_paths <- function() {
  paths <- base_pitching_dev_supplement_paths()
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Wally PitchingApp data files are missing: ", paste(basename(missing), collapse = ", "))
  }
  paths
}

base_read_pitching_source <- function(path) {
  ext <- tolower(tools::file_ext(path))
  rows <- if (ext == "parquet") {
    arrow::read_parquet(path) %>% tibble::as_tibble()
  } else {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  }
  rows$source_file <- basename(path)
  rows$row_in_file <- seq_len(nrow(rows))
  source_group <- names(BASE_WALLY_PITCHING_DATA_FILES)[
    match(basename(path), BASE_WALLY_PITCHING_DATA_FILES)
  ]
  rows$SeasonGroup <- if (identical(source_group, "BP")) NA_character_ else source_group
  rows
}

base_pitching_canonical_rows <- function(startup_rows = NULL) {
  rows <- tibble::as_tibble(startup_rows %||% tibble::tibble())
  if (nrow(rows) && "PitcherTeam" %in% names(rows)) {
    rows <- rows[base_team_matches(rows$PitcherTeam), , drop = FALSE]
  }

  if (!nrow(rows)) {
    path <- TEAM_CONFIG$data$ncaa_d1_master_file
    if (!file.exists(path)) return(tibble::tibble())
    dataset <- arrow::open_dataset(path, format = "parquet")
    rows <- dataset %>%
      dplyr::filter(.data$PitcherTeam == TEAM_CONFIG$data_code) %>%
      dplyr::collect() %>%
      tibble::as_tibble()
  }

  if (!nrow(rows)) return(rows)
  rows$source_file <- "2026 Season - NCAA D1.parquet"
  rows$row_in_file <- seq_len(nrow(rows))
  rows$SeasonGroup <- "S26"
  rows$DataSource <- BASE_NCAA_D1_SOURCE_LABEL
  rows$.base_source_priority <- 1L
  rows
}

base_pitching_supplement_rows <- function() {
  paths <- base_pitching_supplement_paths()
  if (!length(paths)) return(tibble::tibble())

  rows <- lapply(paths, function(path) {
    tryCatch(
      base_read_pitching_source(path),
      error = function(e) {
        message("Texas State pitching supplement failed: ", path, " — ", e$message)
        tibble::tibble()
      }
    )
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(tibble::tibble())

  # Convert through character before binding because TrackMan exports can type
  # the same field differently across competition files.
  rows <- lapply(rows, function(frame) {
    frame[] <- lapply(frame, as.character)
    frame
  })
  out <- dplyr::bind_rows(rows)
  out$DataSource <- paste0("Texas State internal — ", out$source_file)
  out$.base_source_priority <- 2L
  out
}

base_pitching_event_key <- function(rows) {
  n <- nrow(rows)
  value <- function(name) {
    if (name %in% names(rows)) trimws(as.character(rows[[name]])) else rep("", n)
  }
  pitch_uid <- value("PitchUID")
  play_id <- value("PlayID")
  fallback <- do.call(
    paste,
    c(lapply(
      c(
        "Date", "GameID", "GameUID", "Pitcher", "Inning", "PAofInning",
        "PitchofPA", "Batter", "RelSpeed", "PlateLocSide", "PlateLocHeight"
      ),
      value
    ), sep = "\u001f")
  )
  has_fallback <- nzchar(gsub("\u001f", "", fallback, fixed = TRUE))
  dplyr::case_when(
    nzchar(pitch_uid) ~ paste0("pitch:", pitch_uid),
    nzchar(play_id) ~ paste0("play:", play_id),
    has_fallback ~ paste0("event:", fallback),
    TRUE ~ paste0("row:", seq_len(n))
  )
}

base_prepare_team_pitching_data <- function(source_rows = NULL) {
  folder_rows <- source_rows %||% base_pitching_supplement_rows()
  sources <- Filter(function(frame) nrow(frame) > 0L, list(folder_rows))
  if (!length(sources)) return(tibble::tibble())

  # Wally's original loader reads mixed exports as character and then performs
  # one type-conversion pass. Repeating that behavior preserves its formulas.
  sources <- lapply(sources, function(frame) {
    frame[] <- lapply(frame, as.character)
    frame
  })
  rows <- dplyr::bind_rows(sources)
  rows$.base_source_priority <- suppressWarnings(as.integer(rows$.base_source_priority))
  rows <- rows %>%
    dplyr::arrange(.data$.base_source_priority) %>%
    dplyr::mutate(.base_event_key = base_pitching_event_key(.)) %>%
    dplyr::distinct(.data$.base_event_key, .keep_all = TRUE) %>%
    dplyr::select(-".base_event_key", -".base_source_priority")

  id_like <- intersect(
    names(rows),
    c(
      "PitchUID", "PlayID", "PitcherId", "BatterId", "CatcherId",
      "GameId", "PitcherID", "BatterID", "CatcherID", "GameID", "GameUID",
      "source_file"
    )
  )
  spec <- readr::cols(.default = readr::col_guess())
  for (name in id_like) spec$cols[[name]] <- readr::col_character()
  rows <- suppressMessages(readr::type_convert(rows, col_types = spec))
  rows
}

base_load_team_pitching_data <- function(source_rows = NULL, refresh = FALSE) {
  key <- "team_pitching"
  if (!isTRUE(refresh) && exists(key, envir = .base_team_pitching_cache, inherits = FALSE)) {
    return(base::get(key, envir = .base_team_pitching_cache, inherits = FALSE))
  }
  rows <- base_prepare_team_pitching_data(source_rows)
  assign(key, rows, envir = .base_team_pitching_cache)
  message(
    "Prepared Wally PitchingApp folder payload: ",
    format(nrow(rows), big.mark = ","), " rows"
  )
  rows
}

base_clear_team_pitching_cache <- function() {
  keys <- ls(.base_team_pitching_cache, all.names = TRUE)
  if (length(keys)) rm(list = keys, envir = .base_team_pitching_cache)
  invisible(TRUE)
}

base_pitching_embedded_head <- function() {
  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML("
      .base-pitching-workspace {
        --txst-maroon: var(--base-maroon);
        --txst-gold: var(--base-gold-bright);
        width: 100%;
        min-width: 0;
        max-width: 100%;
        color: var(--base-ink);
        font-family: var(--base-font-body);
      }
      .base-pitching-workspace .bslib-page-sidebar,
      .base-pitching-workspace .bslib-sidebar-layout,
      .base-pitching-workspace .main {
        min-height: 760px;
        background: transparent !important;
      }
      .base-pitching-workspace .base-pitching-embedded-layout {
        display: grid;
        min-height: 760px;
        width: 100%;
        min-width: 0;
        max-width: 100%;
        grid-template-columns: clamp(230px, 16vw, 270px) minmax(0, 1fr);
        gap: 18px;
        align-items: start;
      }
      .base-pitching-workspace .base-pitching-sidebar {
        position: sticky;
        top: 72px;
        padding: 20px;
        border: 1px solid var(--base-border);
        border-radius: var(--base-radius);
        background: var(--base-surface);
        box-shadow: var(--base-shadow-sm);
      }
      .base-pitching-workspace .base-pitching-sidebar .sidebar-title {
        margin: 0 0 16px;
        color: var(--base-ink);
        font-family: var(--base-font-display);
        font-size: 20px;
        font-weight: 600;
      }
      .base-pitching-workspace .base-pitching-main,
      .base-pitching-workspace .base-pitching-main > .tabbable,
      .base-pitching-workspace .base-pitching-main .tab-content,
      .base-pitching-workspace .base-pitching-main .tab-pane {
        width: 100%;
        min-width: 0;
        max-width: 100%;
      }
      .base-pitching-workspace .base-pitching-main .row {
        margin-right: 0;
        margin-left: 0;
      }
      .base-pitching-workspace .base-pitching-main .row > [class*='col-'] {
        min-width: 0;
        padding-right: 8px;
        padding-left: 8px;
      }
      .base-pitching-workspace .bslib-sidebar-layout > .sidebar {
        border: 1px solid var(--base-border);
        border-radius: var(--base-radius);
        background: var(--base-surface) !important;
        box-shadow: var(--base-shadow-sm);
      }
      .base-pitching-workspace .nav-tabs {
        display: flex;
        overflow-x: auto;
        gap: 4px;
        padding: 7px;
        border: 1px solid var(--base-border);
        border-radius: var(--base-radius);
        background: var(--base-surface) !important;
        box-shadow: var(--base-shadow-sm);
        scrollbar-width: thin;
      }
      .base-pitching-workspace .nav-tabs > li > a,
      .base-pitching-workspace .nav-tabs .nav-link {
        border: 0 !important;
        border-radius: var(--base-radius-sm) !important;
        background: transparent !important;
        color: var(--base-muted) !important;
        font-size: 12px;
        font-weight: 650;
        white-space: nowrap;
      }
      .base-pitching-workspace .nav-tabs > li.active > a,
      .base-pitching-workspace .nav-tabs .nav-link.active {
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-pitching-workspace .table-title {
        display: inline-block;
        margin-bottom: 8px;
        padding: 7px 11px;
        border-radius: var(--base-radius-sm);
        background: var(--base-maroon) !important;
        color: #fff !important;
        font-family: var(--base-font-display);
        font-weight: 600;
        letter-spacing: .025em;
      }
      .base-pitching-workspace .card,
      .base-pitching-workspace .well {
        border: 1px solid var(--base-border) !important;
        border-radius: var(--base-radius) !important;
        background: var(--base-surface) !important;
        box-shadow: var(--base-shadow-sm) !important;
      }
      .base-pitching-workspace table.dataTable thead th {
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-pitching-workspace .btn-primary,
      .base-pitching-workspace .btn-success {
        border-color: var(--base-maroon) !important;
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-pitching-workspace .cr-percentile-card {
        border-color: var(--base-border-strong) !important;
        border-radius: var(--base-radius) !important;
        background: var(--base-surface) !important;
        box-shadow: var(--base-shadow-sm);
      }
      .base-pitching-workspace table.dataTable tbody td {
        position: relative;
      }
      .base-pitching-workspace table.dataTable tbody td:has(> .cf-cell) {
        padding: 2px !important;
        background: #fff !important;
      }
      .base-pitching-workspace table.dataTable tbody td > .cf-cell {
        display: block !important;
        width: 100% !important;
        min-height: 32px;
        margin: 0 !important;
        padding: 6px 8px !important;
        box-sizing: border-box !important;
        border: 2px solid #fff !important;
        border-radius: 4px !important;
        background-clip: padding-box !important;
        text-align: center;
      }
      .base-pitching-workspace .shiny-spinner-output-container,
      .base-pitching-workspace .shiny-html-output,
      .base-pitching-workspace .shiny-plot-output,
      .base-pitching-workspace .html-widget {
        min-width: 0;
        max-width: 100%;
      }
      .base-pitching-workspace .dataTables_wrapper {
        width: 100% !important;
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
        -webkit-overflow-scrolling: touch;
      }
      .base-pitching-workspace .dataTables_wrapper table.dataTable { margin: 0 !important; }
      .base-pitching-workspace .cr-percentile-column { display: flex; min-width: 0; }
      .base-pitching-workspace .cr-percentile-column > .shiny-spinner-output-container,
      .base-pitching-workspace .cr-percentile-column > .shiny-html-output {
        display: flex;
        width: 100% !important;
        min-width: 0;
        flex: 1 1 100%;
      }
      .base-pitching-workspace .cr-percentile-card {
        width: 100%;
        min-height: 0 !important;
        padding: 16px 18px;
        margin-bottom: 12px;
        display: flex;
        flex-direction: column;
      }
      .base-pitching-workspace .cr-percentile-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 12px;
        padding-bottom: 9px;
        margin-bottom: 10px;
        border-bottom: 2px solid var(--base-maroon);
      }
      .base-pitching-workspace .cr-percentile-title {
        color: var(--base-ink);
        font-family: var(--base-font-display);
        font-size: 16px;
        font-weight: 700;
        line-height: 1.1;
      }
      .base-pitching-workspace .cr-percentile-subtitle {
        color: var(--base-maroon);
        font-size: 12px;
        font-weight: 750;
        text-align: right;
      }
      .base-pitching-workspace .cr-percentile-scale {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        margin: 0 58px 8px 112px;
        font-size: 10px;
        font-weight: 750;
        letter-spacing: .04em;
        text-transform: uppercase;
      }
      .base-pitching-workspace .cr-percentile-scale span:nth-child(1) { color: #5D7EBC; text-align: left; }
      .base-pitching-workspace .cr-percentile-scale span:nth-child(2) { color: var(--base-muted); text-align: center; }
      .base-pitching-workspace .cr-percentile-scale span:nth-child(3) { color: #E33434; text-align: right; }
      .base-pitching-workspace .cr-percentile-rows {
        display: flex;
        flex: 1;
        flex-direction: column;
        gap: 7px;
      }
      .base-pitching-workspace .cr-percentile-row {
        display: grid;
        grid-template-columns: 102px minmax(150px, 1fr) 54px;
        gap: 10px;
        align-items: center;
        min-height: 24px;
      }
      .base-pitching-workspace .cr-percentile-label {
        color: var(--base-muted);
        font-size: 11px;
        line-height: 1.1;
        text-align: right;
      }
      .base-pitching-workspace .cr-percentile-track-wrap { position: relative; height: 20px; }
      .base-pitching-workspace .cr-percentile-track {
        position: absolute;
        inset: 7px 0 auto;
        height: 6px;
        border-radius: 999px;
        background: #e6eaec;
      }
      .base-pitching-workspace .cr-percentile-fill {
        position: absolute;
        left: 0;
        top: 5px;
        height: 10px;
        border-radius: 999px;
      }
      .base-pitching-workspace .cr-percentile-average {
        position: absolute;
        left: 50%;
        top: 3px;
        width: 2px;
        height: 14px;
        background: rgba(80,18,20,.28);
      }
      .base-pitching-workspace .cr-percentile-badge {
        position: absolute;
        top: 0;
        transform: translateX(-50%);
        min-width: 28px;
        height: 20px;
        padding: 0 6px;
        border: 2px solid #fff;
        border-radius: 999px;
        box-shadow: 0 1px 4px rgba(30,22,18,.2);
        color: #fff;
        font-size: 10px;
        font-weight: 800;
        line-height: 16px;
        text-align: center;
      }
      .base-pitching-workspace .cr-percentile-value {
        color: var(--base-ink);
        font-variant-numeric: tabular-nums;
        font-size: 11px;
        text-align: right;
        white-space: nowrap;
      }
      .base-pitching-workspace .cr-percentile-row-empty { opacity: .5; }
      .base-pitching-workspace .cr-percentile-empty { color: var(--base-muted); font-size: 13px; }
      .base-pitching-workspace .base-multi-filter { max-width: 100%; }
      .base-pitching-workspace .base-multi-filter .selectize-input { min-height: 38px; }
      .base-pitching-workspace .base-pitch-metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
        gap: 12px;
        margin-top: 8px;
      }
      .base-pitching-workspace .base-pitch-metric-filter { min-width: 0; }
      .base-pitching-workspace .base-pitch-metric-label {
        display: block;
        margin-bottom: 5px;
        color: var(--base-ink);
        font-size: 12px;
        font-weight: 700;
      }
      .base-pitching-workspace .base-pitching-performance-tables,
      .base-pitching-workspace .base-pitching-performance-details {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 18px;
        margin: 0 !important;
      }
      .base-pitching-workspace .base-pitching-performance-tables::before,
      .base-pitching-workspace .base-pitching-performance-tables::after,
      .base-pitching-workspace .base-pitching-performance-details::before,
      .base-pitching-workspace .base-pitching-performance-details::after {
        display: none !important;
        content: none !important;
      }
      .base-pitching-workspace .base-pitching-performance-tables > [class*='col-'],
      .base-pitching-workspace .base-pitching-performance-details > [class*='col-'] {
        float: none;
        width: auto;
        padding: 0;
      }
      .base-pitching-workspace .base-season-summary-page {
        display: grid;
        gap: 18px;
        width: 100%;
        min-width: 0;
      }
      .base-pitching-workspace .base-season-summary-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
        min-width: 0;
      }
      .base-pitching-workspace .base-season-summary-header > .shiny-html-output {
        flex: 1 1 auto;
        min-width: 0;
      }
      .base-pitching-workspace .base-season-summary-header .btn {
        flex: 0 0 auto;
        white-space: nowrap;
      }
      .base-pitching-workspace .base-season-summary-section,
      .base-pitching-workspace .base-season-summary-hand-panel {
        min-width: 0;
      }
      .base-pitching-workspace .base-season-summary-hands {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 18px;
        align-items: start;
      }
      .base-pitching-workspace .base-season-summary-hand-panel {
        border: 1px solid rgba(80, 18, 20, 0.12);
        border-radius: 10px;
        background: #fff;
        overflow: hidden;
      }
      .base-pitching-workspace .base-season-summary-hand-panel .table-title {
        margin: 0 !important;
        border-radius: 0;
      }
      .base-pitching-workspace .base-season-summary-table-label {
        padding: 8px 12px;
        color: #501214;
        font-weight: 700;
        border-top: 1px solid rgba(80, 18, 20, 0.12);
        background: #f7f3ed;
      }
      .base-pitching-workspace .base-season-summary-page .dataTables_wrapper {
        margin: 0;
      }
      .base-pitching-workspace .base-team-trends-dashboard {
        display: grid;
        grid-template-columns: minmax(320px, .72fr) minmax(0, 1.48fr);
        gap: 18px;
        align-items: start;
        margin-top: 14px;
      }
      .base-pitching-workspace .base-team-trends-charts {
        display: grid;
        min-width: 0;
        grid-template-columns: 1fr;
        gap: 18px;
      }
      .base-pitching-workspace .base-team-trends-charts .shiny-spinner-output-container,
      .base-pitching-workspace .base-team-trends-charts .html-widget {
        width: 100% !important;
      }
      @media (max-width: 1450px) {
        .base-pitching-workspace .base-pitching-performance-tables,
        .base-pitching-workspace .base-pitching-performance-details {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 1100px) {
        .base-pitching-workspace .base-season-summary-hands {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 720px) {
        .base-pitching-workspace .base-season-summary-header {
          align-items: flex-start;
          flex-direction: column;
        }
      }
      @media (max-width: 1180px) {
        .base-pitching-workspace .base-team-trends-dashboard { grid-template-columns: 1fr; }
      }
      @media (max-width: 900px) {
        .base-pitching-workspace .bslib-sidebar-layout,
        .base-pitching-workspace .base-pitching-embedded-layout {
          display: block !important;
        }
        .base-pitching-workspace .base-pitching-sidebar {
          position: static;
          margin-bottom: 16px;
        }
        .base-pitching-workspace .base-pitch-metric-grid { grid-template-columns: 1fr; }
      }
    "))
  )
}

base_wally_pitching_environment <- function(team_rows) {
  if (exists("environment", envir = .base_wally_pitching_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_wally_pitching_state, inherits = FALSE))
  }
  missing <- BASE_WALLY_PITCHING_REQUIRED_PACKAGES[
    !vapply(BASE_WALLY_PITCHING_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Pitching workspace dependencies are missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(BASE_WALLY_PITCHING_FILE)) {
    stop("Wally PitchingApp source is unavailable at ", BASE_WALLY_PITCHING_FILE)
  }

  workspace <- new.env(parent = globalenv())
  workspace$BASE_PITCHING_DATA <- team_rows
  workspace$BASE_PITCHING_EMBEDDED <- TRUE
  workspace$BASE_PITCHING_PERCENTILE_REFERENCE_PATH <- base_project_path(
    "WallyApps", "PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv"
  )
  workspace$BASE_PITCHING_TXST_LOGO_PATH <- base_project_path(
    "WallyApps", "PitchingApp", "www", "txstlogo.jpeg"
  )
  workspace$BASE_PITCHING_BOBCAT_LOGO_PATH <- base_project_path(
    "WallyApps", "PitchingApp", "www", "Bobcatlogo.png"
  )
  workspace$BASE_PITCHING_THEME <- bslib::bs_theme(
    version = 3,
    primary = TEAM_CONFIG$colors$primary
  )
  workspace$BASE_PITCHING_HEAD <- base_pitching_embedded_head()

  prior_warn <- getOption("warn")
  prior_baseline_flag <- getOption("stuff_baseline_missing")
  on.exit({
    options(warn = prior_warn)
    options(stuff_baseline_missing = prior_baseline_flag)
  }, add = TRUE)
  sys.source(
    BASE_WALLY_PITCHING_FILE,
    envir = workspace,
    chdir = TRUE,
    keep.source = FALSE
  )
  if (!inherits(workspace$ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !inherits(workspace$base_pitching_postgame_ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("Wally PitchingApp did not expose a usable UI and server.")
  }
  assign("environment", workspace, envir = .base_wally_pitching_state)
  workspace
}

base_team_pitching_workspace_ui <- function() {
  tags$div(
    class = "base-workspace-page base-pitching-host",
    tags$div(
      class = "base-workspace-heading",
      tags$div(
        tags$div(class = "base-eyebrow", "Texas State player development"),
        tags$h1("Pitching"),
        tags$p(
          "Wally's complete pitching workflow, using its season and bullpen files in WallyApps/PitchingApp/data."
        )
      ),
      tags$div(
        class = "base-source-chip",
        tags$span(class = "home-status-dot"),
        "PitchingApp folder CSVs"
      )
    ),
    tags$div(
      id = "base-pitching-loading",
      class = "base-workspace-loading",
      tags$div(class = "base-loading-mark", "B"),
      tags$strong("Preparing Pitching"),
      tags$span("The workspace loads once, when first opened.")
    ),
    shiny::uiOutput("base_team_pitching_app")
  )
}

base_team_pitching_workspace_server <- function(input, output, session) {
  team_rows <- base_load_team_pitching_data()
  workspace <- base_wally_pitching_environment(team_rows)
  base_clear_team_pitching_cache()
  output$base_team_pitching_app <- shiny::renderUI({
    tags$div(class = "base-pitching-workspace", workspace$ui)
  })
  output$base_postgame_pitching_aar <- shiny::renderUI({
    tags$div(
      class = "base-pitching-workspace base-postgame-aar",
      workspace$head_css,
      workspace$base_pitching_postgame_ui
    )
  })
  workspace$server(input, output, session)
  shinyjs::hide("base-pitching-loading")
  invisible(workspace)
}
