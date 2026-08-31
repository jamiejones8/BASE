# Adapter for Wally's HittingApp inside the unified BASE application.
#
# Wally's original server remains the calculation authority. BASE injects one
# normalized Texas State payload, evaluates the app in an isolated environment,
# and supplies a visual wrapper scoped to the Hitting workspace.

BASE_WALLY_HITTING_FILE <- base_project_path(
  "WallyApps", "HittingApp", "HittingApp.R"
)

BASE_WALLY_HITTING_REQUIRED_PACKAGES <- c(
  "bslib", "cowplot", "dplyr", "DT", "ggplot2", "ggplotify",
  "ggpubr", "gtable", "gridExtra", "htmltools", "jpeg", "magrittr",
  "patchwork", "plotly", "png", "purrr", "ragg", "readr", "scales",
  "shiny", "shinycssloaders", "shinyWidgets", "stringr", "tibble", "tidyr"
)

.base_team_hitting_cache <- new.env(parent = emptyenv())
.base_wally_hitting_state <- new.env(parent = emptyenv())

BASE_WALLY_HITTING_DATA_FILES <- c(
  S25 = "2025 Season -cleaned.csv",
  F25 = "2025 Fall -cleaned.csv",
  SQ26 = "2026 Squads - cleaned.csv",
  S26 = "2026 Season - cleaned.csv"
)

base_hitting_dev_supplement_paths <- function() {
  root <- base_project_path("WallyApps", "HittingApp", "data")
  file.path(root, unname(BASE_WALLY_HITTING_DATA_FILES))
}

base_hitting_supplement_paths <- function() {
  paths <- base_hitting_dev_supplement_paths()
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Wally HittingApp data files are missing: ", paste(basename(missing), collapse = ", "))
  }
  paths
}

base_read_hitting_source <- function(path) {
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
  season_group <- names(BASE_WALLY_HITTING_DATA_FILES)[
    match(basename(path), BASE_WALLY_HITTING_DATA_FILES)
  ]
  rows$SeasonGroup <- season_group
  rows
}

base_hitting_canonical_rows <- function(startup_rows = NULL) {
  rows <- tibble::as_tibble(startup_rows %||% tibble::tibble())
  if (nrow(rows) && "BatterTeam" %in% names(rows)) {
    rows <- rows[base_team_matches(rows$BatterTeam), , drop = FALSE]
  }

  if (!nrow(rows)) {
    path <- TEAM_CONFIG$data$ncaa_d1_master_file
    if (!file.exists(path)) return(tibble::tibble())
    dataset <- arrow::open_dataset(path, format = "parquet")
    rows <- dataset %>%
      dplyr::filter(.data$BatterTeam == TEAM_CONFIG$data_code) %>%
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

base_hitting_supplement_rows <- function() {
  paths <- base_hitting_supplement_paths()
  if (!length(paths)) return(tibble::tibble())

  rows <- lapply(paths, function(path) {
    tryCatch(
      base_read_hitting_source(path),
      error = function(e) {
        message("Texas State hitting supplement failed: ", path, " — ", e$message)
        tibble::tibble()
      }
    )
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(tibble::tibble())

  rows <- lapply(rows, function(frame) {
    if ("BatterTeam" %in% names(frame)) {
      frame <- frame[base_team_matches(frame$BatterTeam), , drop = FALSE]
    }
    frame[] <- lapply(frame, as.character)
    frame
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(tibble::tibble())

  out <- dplyr::bind_rows(rows)
  out$DataSource <- paste0("Texas State internal — ", out$source_file)
  out$.base_source_priority <- 2L
  out
}

base_hitting_event_key <- function(rows) {
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
        "Date", "GameID", "GameUID", "Batter", "Inning", "PAofInning",
        "PitchofPA", "Pitcher", "RelSpeed", "PlateLocSide", "PlateLocHeight"
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

base_prepare_team_hitting_data <- function(source_rows = NULL) {
  folder_rows <- source_rows %||% base_hitting_supplement_rows()
  sources <- Filter(function(frame) nrow(frame) > 0L, list(folder_rows))
  if (!length(sources)) return(tibble::tibble())

  # Wally reads mixed exports as character before a single type-conversion
  # pass. Mirroring that behavior keeps its calculations unchanged.
  sources <- lapply(sources, function(frame) {
    frame[] <- lapply(frame, as.character)
    frame
  })
  rows <- dplyr::bind_rows(sources)
  rows$.base_source_priority <- suppressWarnings(as.integer(rows$.base_source_priority))
  rows <- rows %>%
    dplyr::arrange(.data$.base_source_priority) %>%
    dplyr::mutate(.base_event_key = base_hitting_event_key(.)) %>%
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
  suppressMessages(readr::type_convert(rows, col_types = spec))
}

base_load_team_hitting_data <- function(source_rows = NULL, refresh = FALSE) {
  key <- "team_hitting"
  if (!isTRUE(refresh) && exists(key, envir = .base_team_hitting_cache, inherits = FALSE)) {
    return(base::get(key, envir = .base_team_hitting_cache, inherits = FALSE))
  }
  rows <- base_prepare_team_hitting_data(source_rows)
  assign(key, rows, envir = .base_team_hitting_cache)
  message(
    "Prepared Wally HittingApp folder payload: ",
    format(nrow(rows), big.mark = ","), " rows"
  )
  rows
}

base_clear_team_hitting_cache <- function() {
  keys <- ls(.base_team_hitting_cache, all.names = TRUE)
  if (length(keys)) rm(list = keys, envir = .base_team_hitting_cache)
  invisible(TRUE)
}

base_hitting_embedded_head <- function() {
  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML("
      .base-hitting-workspace {
        --txst-maroon: var(--base-maroon);
        --txst-gold: var(--base-gold-bright);
        width: 100%;
        min-width: 0;
        max-width: 100%;
        color: var(--base-ink);
        font-family: var(--base-font-body);
      }
      .base-hitting-workspace .base-hitting-embedded-layout {
        display: grid;
        min-height: 760px;
        width: 100%;
        min-width: 0;
        max-width: 100%;
        grid-template-columns: clamp(230px, 16vw, 270px) minmax(0, 1fr);
        gap: 18px;
        align-items: start;
      }
      .base-hitting-workspace .base-hitting-sidebar {
        position: sticky;
        top: 72px;
        padding: 20px;
        border: 1px solid var(--base-border);
        border-radius: var(--base-radius);
        background: var(--base-surface);
        box-shadow: var(--base-shadow-sm);
      }
      .base-hitting-workspace .base-hitting-sidebar .sidebar-title {
        margin: 0 0 16px;
        color: var(--base-ink);
        font-family: var(--base-font-display);
        font-size: 20px;
        font-weight: 600;
      }
      .base-hitting-workspace .base-hitting-main,
      .base-hitting-workspace .base-hitting-main > .tabbable,
      .base-hitting-workspace .base-hitting-main .tab-content,
      .base-hitting-workspace .base-hitting-main .tab-pane {
        width: 100%;
        min-width: 0;
        max-width: 100%;
      }
      .base-hitting-workspace .base-hitting-main .row {
        margin-right: 0;
        margin-left: 0;
      }
      .base-hitting-workspace .base-hitting-main .row > [class*='col-'] {
        min-width: 0;
        padding-right: 8px;
        padding-left: 8px;
      }
      .base-hitting-workspace .nav-tabs {
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
      .base-hitting-workspace .nav-tabs > li > a,
      .base-hitting-workspace .nav-tabs .nav-link {
        border: 0 !important;
        border-radius: var(--base-radius-sm) !important;
        background: transparent !important;
        color: var(--base-muted) !important;
        font-size: 12px;
        font-weight: 650;
        white-space: nowrap;
      }
      .base-hitting-workspace .nav-tabs > li.active > a,
      .base-hitting-workspace .nav-tabs .nav-link.active {
        background: var(--base-maroon) !important;
        color: #fff !important;
        box-shadow: none !important;
      }
      .base-hitting-workspace .card,
      .base-hitting-workspace .bslib-card,
      .base-hitting-workspace .well,
      .base-hitting-workspace .aar-card {
        border: 1px solid var(--base-border) !important;
        border-radius: var(--base-radius) !important;
        background: var(--base-surface) !important;
        box-shadow: var(--base-shadow-sm) !important;
      }
      .base-hitting-workspace table.dataTable thead th,
      .base-hitting-workspace .table thead th,
      .base-hitting-workspace .aar-kpi thead th {
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-hitting-workspace .btn-primary,
      .base-hitting-workspace .btn-success {
        border-color: var(--base-maroon) !important;
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-hitting-workspace .form-control,
      .base-hitting-workspace .selectize-input,
      .base-hitting-workspace .bootstrap-select > .dropdown-toggle {
        border-color: var(--base-border-strong) !important;
        border-radius: var(--base-radius-sm) !important;
        background: #fff !important;
      }
      .base-hitting-workspace .aar-title,
      .base-hitting-workspace .aar-subtitle {
        color: var(--base-maroon) !important;
      }
      .base-hitting-workspace .aar-kpi-title {
        background: var(--base-maroon) !important;
        color: #fff !important;
      }
      .base-hitting-workspace .lineup-table select { min-width: 120px; }
      .base-hitting-workspace .lineup-total td { font-weight: 800; }
      .base-hitting-workspace table.dataTable tbody td:has(> .cf-cell) {
        padding: 0 !important;
      }
      .base-hitting-workspace table.dataTable tbody td > .cf-cell {
        display: flex !important;
        width: 100% !important;
        min-height: 36px;
        padding: 8px 10px !important;
        box-sizing: border-box !important;
        align-items: center;
        justify-content: center;
        border-radius: 0 !important;
      }
      .base-hitting-workspace .shiny-spinner-output-container,
      .base-hitting-workspace .shiny-html-output,
      .base-hitting-workspace .shiny-plot-output,
      .base-hitting-workspace .html-widget {
        min-width: 0;
        max-width: 100%;
      }
      .base-hitting-workspace .dataTables_wrapper {
        width: 100% !important;
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
        -webkit-overflow-scrolling: touch;
      }
      .base-hitting-workspace .dataTables_wrapper table.dataTable { margin: 0 !important; }
      .base-hitting-workspace .aar-report-header {
        display: grid;
        grid-template-columns: 72px minmax(0, 1fr) 72px;
        gap: 16px;
        align-items: center;
        margin: 12px 0;
        padding: 12px 16px;
        border-radius: var(--base-radius-sm);
        background: var(--base-maroon);
        color: #fff;
      }
      .base-hitting-workspace .aar-report-header img {
        display: block;
        width: 64px;
        height: 50px;
        object-fit: contain;
      }
      .base-hitting-workspace .aar-report-header img:last-child { justify-self: end; }
      .base-hitting-workspace .aar-report-title {
        font-family: var(--base-font-display);
        font-size: 18px;
        font-weight: 700;
        text-align: center;
      }
      .base-hitting-workspace .aar-report-meta {
        margin-top: 3px;
        color: var(--base-gold-bright);
        font-size: 12px;
        font-weight: 650;
        text-align: center;
      }
      .base-hitting-workspace #aar_table tr.aar-swing-row > td {
        background: #B4975A !important;
        color: #501214 !important;
        font-weight: 650;
      }
      .base-hitting-workspace #aar_table tr.aar-take-row > td {
        background: #501214 !important;
        color: #FFFFFF !important;
        font-weight: 650;
      }
      .base-hitting-workspace .base-multi-filter .selectize-input { min-height: 38px; }
      @media (max-width: 900px) {
        .base-hitting-workspace .base-hitting-embedded-layout {
          display: block !important;
        }
        .base-hitting-workspace .base-hitting-sidebar {
          position: static;
          margin-bottom: 16px;
        }
      }
    "))
  )
}

base_wally_hitting_environment <- function(team_rows) {
  if (exists("environment", envir = .base_wally_hitting_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_wally_hitting_state, inherits = FALSE))
  }
  missing <- BASE_WALLY_HITTING_REQUIRED_PACKAGES[
    !vapply(BASE_WALLY_HITTING_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Hitting workspace dependencies are missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(BASE_WALLY_HITTING_FILE)) {
    stop("Wally HittingApp source is unavailable at ", BASE_WALLY_HITTING_FILE)
  }

  workspace <- new.env(parent = globalenv())
  workspace$BASE_HITTING_DATA <- team_rows
  workspace$BASE_HITTING_TEAM_CODE <- TEAM_CONFIG$data_code
  workspace$BASE_HITTING_EMBEDDED <- TRUE
  workspace$BASE_HITTING_TXST_LOGO_PATH <- base_project_path(
    "WallyApps", "HittingApp", "www", "txstlogo.jpeg"
  )
  workspace$BASE_HITTING_BOBCAT_LOGO_PATH <- base_project_path(
    "WallyApps", "HittingApp", "www", "Bobcatlogo.png"
  )
  workspace$BASE_HITTING_REPORT_LOGO_PATH <- base_project_path(
    "WallyApps", "HittingApp", "www", "baseballTS logo gold.png"
  )
  asset_prefix <- "wally-hitting-assets"
  if (!(asset_prefix %in% names(shiny::resourcePaths()))) {
    shiny::addResourcePath(
      asset_prefix,
      normalizePath(base_project_path("WallyApps", "HittingApp", "www"), mustWork = TRUE)
    )
  }
  workspace$BASE_HITTING_ASSET_PREFIX <- asset_prefix
  workspace$BASE_HITTING_THEME <- bslib::bs_theme(
    version = 3,
    primary = TEAM_CONFIG$colors$primary
  )
  workspace$BASE_HITTING_HEAD <- base_hitting_embedded_head()

  prior_warn <- getOption("warn")
  on.exit(options(warn = prior_warn), add = TRUE)
  sys.source(
    BASE_WALLY_HITTING_FILE,
    envir = workspace,
    chdir = TRUE,
    keep.source = FALSE
  )
  if (!inherits(workspace$ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("Wally HittingApp did not expose a usable UI and server.")
  }
  assign("environment", workspace, envir = .base_wally_hitting_state)
  workspace
}

base_team_hitting_workspace_ui <- function() {
  tags$div(
    class = "base-workspace-page base-hitting-host",
    tags$div(
      class = "base-workspace-heading",
      tags$div(
        tags$div(class = "base-eyebrow", "Texas State player development"),
        tags$h1("Hitting"),
        tags$p("Wally's complete hitting workflow, using its season files in WallyApps/HittingApp/data.")
      ),
      tags$div(
        class = "base-source-chip",
        tags$span(class = "home-status-dot"),
        "HittingApp folder CSVs"
      )
    ),
    tags$div(
      id = "base-hitting-loading",
      class = "base-workspace-loading",
      tags$div(class = "base-loading-mark", "B"),
      tags$strong("Preparing Hitting"),
      tags$span("The workspace loads once, when first opened.")
    ),
    shiny::uiOutput("base_team_hitting_app")
  )
}

base_team_hitting_workspace_server <- function(input, output, session) {
  team_rows <- base_load_team_hitting_data()
  workspace <- base_wally_hitting_environment(team_rows)
  base_clear_team_hitting_cache()
  output$base_team_hitting_app <- shiny::renderUI({
    tags$div(class = "base-hitting-workspace", workspace$ui)
  })
  workspace$server(input, output, session)
  shinyjs::hide("base-hitting-loading")
  invisible(workspace)
}
