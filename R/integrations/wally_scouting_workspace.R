# Adapter for Wally's ScoutingApp inside the unified BASE application.
#
# The original report calculations, controls, previews, matchup grid, and PDF
# handlers stay in ScoutingApp.R. BASE supplies only the workspace shell,
# stable paths, dependency checks, and scoped visual language.

BASE_WALLY_SCOUTING_FILE <- base_project_path(
  "WallyApps", "ScoutingApp", "ScoutingApp.R"
)

BASE_WALLY_SCOUTING_REQUIRED_PACKAGES <- c(
  "bslib", "cowplot", "curl", "dplyr", "DT", "ggplot2", "ggplotify",
  "gridExtra", "gtable", "htmltools", "jpeg", "patchwork", "png",
  "purrr", "readr", "scales", "shiny", "stringr", "tibble", "tidyr"
)

.base_wally_scouting_state <- new.env(parent = emptyenv())

base_scouting_season_file <- function() {
  override <- base_env_path("BASE_SCOUTING_SEASON_FILE", "")
  if (nzchar(override)) return(override)
  candidates <- unique(c(
    TEAM_CONFIG$data$ncaa_d1_master_file,
    base_default_ncaa_d1_master_file()
  ))
  available <- candidates[file.exists(candidates)]
  if (length(available)) available[[1]] else candidates[[1]]
}

# Open metadata lazily. Only grouped player menus and filtered player pitches
# cross into R; the national pitch table is never collected in full.
base_scouting_season_source <- function(path = base_scouting_season_file()) {
  dataset <- NULL
  team_cache <- list()
  catalogs <- list()
  cache <- list()
  production_path <- normalizePath(
    base_scouting_season_file(), winslash = "/", mustWork = FALSE
  )
  requested_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  use_runtime_catalogs <- identical(requested_path, production_path) &&
    exists("base_pitcher_catalog", inherits = TRUE) &&
    exists("base_get_hitter_catalog", mode = "function", inherits = TRUE) &&
    exists("base_load_pitcher_rows", mode = "function", inherits = TRUE) &&
    exists("base_load_hitter_rows", mode = "function", inherits = TRUE)
  open <- function() {
    if (!is.null(dataset)) return(dataset)
    if (!file.exists(path)) {
      stop("Season file unavailable: ", path,
           ". Set BASE_SCOUTING_SEASON_FILE to the mounted college Parquet.")
    }
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading the college season requires the arrow package.")
    }
    candidate <- arrow::open_dataset(path, format = "parquet")
    required <- c("Batter", "BatterTeam", "Pitcher", "PitcherTeam")
    missing <- setdiff(required, names(candidate$schema))
    if (length(missing)) stop("Season file is missing columns: ", paste(missing, collapse = ", "))
    dataset <<- candidate
    dataset
  }
  columns <- function(role) {
    if (match.arg(role, c("hitter", "pitcher")) == "hitter") {
      c("BatterTeam", "Batter")
    } else {
      c("PitcherTeam", "Pitcher")
    }
  }
  runtime_catalog <- function(role) {
    role <- match.arg(role, c("hitter", "pitcher"))
    if (role == "hitter") {
      base_get_hitter_catalog() %>%
        dplyr::transmute(
          Team = as.character(.data$BatterTeam),
          Player = as.character(.data$Batter),
          Pitches = suppressWarnings(as.numeric(.data$PitchCount))
        )
    } else {
      base_pitcher_catalog %>%
        dplyr::transmute(
          Team = as.character(.data$PitcherTeam),
          Player = as.character(.data$Pitcher),
          Pitches = suppressWarnings(as.numeric(.data$PitchCount))
        )
    }
  }
  teams <- function(role) {
    role <- match.arg(role, c("hitter", "pitcher"))
    if (!is.null(team_cache[[role]])) return(team_cache[[role]])
    if (use_runtime_catalogs) {
      values <- runtime_catalog(role)$Team
    } else {
      team_col <- columns(role)[[1]]
      query <- dplyr::select(open(), tidyselect::all_of(team_col))
      values <- dplyr::collect(dplyr::distinct(query))[[team_col]]
    }
    values <- sort(unique(as.character(values)))
    values <- values[!is.na(values) & nzchar(trimws(values))]
    team_cache[[role]] <<- values
    values
  }
  catalog <- function(role, team) {
    role <- match.arg(role, c("hitter", "pitcher"))
    cols <- columns(role)
    team <- as.character(team %||% "")[[1]]
    if (is.na(team) || !nzchar(trimws(team))) return(tibble::tibble(
      Team = character(), Player = character(), Pitches = numeric()
    ))
    key <- paste(role, team, sep = "\u001f")
    if (!is.null(catalogs[[key]])) return(catalogs[[key]])
    if (use_runtime_catalogs) {
      rows <- runtime_catalog(role) %>% dplyr::filter(.data$Team == team)
      catalogs[[key]] <<- rows
      return(rows)
    }
    query <- dplyr::filter(open(), .data[[cols[[1]]]] == team)
    query <- dplyr::select(query, tidyselect::all_of(cols))
    query <- dplyr::group_by(query, dplyr::across(tidyselect::all_of(cols)))
    rows <- dplyr::collect(dplyr::summarise(query, Pitches = dplyr::n(), .groups = "drop"))
    names(rows)[match(cols, names(rows))] <- c("Team", "Player")
    rows$Team <- as.character(rows$Team)
    rows$Player <- as.character(rows$Player)
    rows <- rows[!is.na(rows$Team) & nzchar(trimws(rows$Team)) &
                   !is.na(rows$Player) & nzchar(trimws(rows$Player)), , drop = FALSE]
    rows <- rows[order(rows$Team, rows$Player), , drop = FALSE]
    catalogs[[key]] <<- rows
    rows
  }
  load_players <- function(role, team, players) {
    cols <- columns(role)
    players <- sort(unique(as.character(players)))
    players <- players[!is.na(players) & nzchar(players)]
    if (length(team) != 1L || is.na(team) || !nzchar(team) || !length(players)) {
      return(tibble::tibble())
    }
    key <- paste(c(role, team, players), collapse = "\u001f")
    if (!is.null(cache[[key]])) return(cache[[key]])
    if (use_runtime_catalogs) {
      loader <- if (role == "hitter") base_load_hitter_rows else base_load_pitcher_rows
      rows <- dplyr::bind_rows(lapply(players, function(player) loader(team, player)))
    } else {
      query <- dplyr::filter(open(), .data[[cols[[1]]]] == team,
                             .data[[cols[[2]]]] %in% players)
      rows <- tibble::as_tibble(dplyr::collect(query))
    }
    rows$.source_file <- rep(basename(path), nrow(rows))
    # Bound the shared cache by bytes as well as entry count.
    if (as.numeric(object.size(rows)) <= 64 * 1024^2) {
      cache[[key]] <<- rows
      while (length(cache) > 16L || as.numeric(object.size(cache)) > 64 * 1024^2) {
        cache[[1]] <<- NULL
      }
    }
    rows
  }
  list(path = path, teams = teams, catalog = catalog, load_players = load_players)
}

base_scouting_data_dir <- function() {
  configured <- Sys.getenv("BASE_SCOUTING_DATA_DIR", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  base_project_path("WallyApps", "ScoutingApp", "data")
}

base_scouting_team_display_name <- function(team_codes) {
  if (exists("base_team_display_name", mode = "function", inherits = TRUE)) {
    return(base_team_display_name(team_codes))
  }
  as.character(team_codes)
}

base_scouting_embedded_head <- function() {
  htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
    .base-scouting-workspace {
      color: var(--base-ink);
      font-family: var(--base-font-body);
    }
    .base-scouting-workspace .base-scouting-embedded-layout {
      display: grid;
      grid-template-columns: minmax(230px, 270px) minmax(0, 1fr);
      gap: 18px;
      min-height: 760px;
      align-items: start;
    }
    .base-scouting-workspace .base-scouting-sidebar {
      position: sticky;
      top: 72px;
      min-width: 0;
      padding: 20px;
      border: 1px solid var(--base-border);
      border-radius: var(--base-radius);
      background: var(--base-surface);
      box-shadow: var(--base-shadow-sm);
    }
    .base-scouting-workspace .base-scouting-sidebar h4 {
      margin: 0 0 14px;
      color: var(--base-ink);
      font-family: var(--base-font-display);
      font-size: 19px;
      font-weight: 600;
    }
    .base-scouting-workspace .base-scouting-main { min-width: 0; }
    .base-scouting-workspace .nav-tabs {
      display: flex;
      gap: 4px;
      overflow-x: auto;
      margin: 0 0 16px;
      padding: 7px;
      border: 1px solid var(--base-border);
      border-radius: var(--base-radius);
      background: var(--base-surface);
      box-shadow: var(--base-shadow-sm);
      scrollbar-width: thin;
    }
    .base-scouting-workspace .nav-tabs > li > a,
    .base-scouting-workspace .nav-tabs .nav-link {
      border: 0 !important;
      border-radius: var(--base-radius-sm) !important;
      background: transparent !important;
      color: var(--base-muted) !important;
      font-size: 12px;
      font-weight: 650;
      white-space: nowrap;
    }
    .base-scouting-workspace .nav-tabs > li.active > a,
    .base-scouting-workspace .nav-tabs .nav-link.active {
      background: var(--base-maroon) !important;
      color: #fff !important;
      box-shadow: none !important;
    }
    .base-scouting-workspace .report-shell {
      min-width: 0;
      overflow-x: auto;
      padding: 16px;
      border: 1px solid var(--base-border) !important;
      border-radius: var(--base-radius) !important;
      background: var(--base-surface) !important;
      box-shadow: var(--base-shadow-sm);
    }
    .base-scouting-workspace .report-toolbar {
      gap: 12px;
      margin-bottom: 14px;
      padding-bottom: 14px;
      border-bottom: 1px solid var(--base-border);
    }
    .base-scouting-workspace .form-group > label,
    .base-scouting-workspace .control-label {
      color: var(--base-ink);
      font-size: 12px;
      font-weight: 650;
    }
    .base-scouting-workspace .form-control,
    .base-scouting-workspace .selectize-input {
      border-color: var(--base-border-strong);
      border-radius: var(--base-radius-sm);
      background: #fff;
      box-shadow: none;
    }
    .base-scouting-workspace .btn-primary,
    .base-scouting-workspace .btn-success {
      border-color: var(--base-maroon) !important;
      background: var(--base-maroon) !important;
      color: #fff !important;
    }
    .base-scouting-workspace .btn-outline-secondary {
      border-color: var(--base-border-strong) !important;
      color: var(--base-ink) !important;
    }
    .base-scouting-workspace table.dataTable thead th {
      background: var(--base-maroon) !important;
      color: #fff !important;
    }
    .base-scouting-workspace .shiny-plot-output {
      max-width: none;
    }
    .base-scouting-launch {
      display: grid;
      grid-template-columns: minmax(0, 1fr) minmax(360px, .72fr);
      gap: 18px;
      align-items: center;
      min-height: 250px;
      padding: 28px;
      border: 1px solid var(--base-border);
      border-radius: var(--base-radius);
      background:
        radial-gradient(circle at 88% 12%, rgba(180,151,90,.16), transparent 30%),
        linear-gradient(135deg, rgba(80,18,20,.045), rgba(255,255,255,.98));
      box-shadow: var(--base-shadow-sm);
    }
    .base-scouting-launch-copy { max-width: 620px; }
    .base-scouting-launch-mark {
      display: grid;
      width: 54px;
      height: 54px;
      margin-bottom: 14px;
      place-items: center;
      border-radius: 13px;
      background: var(--base-maroon);
      color: var(--base-gold-bright);
      font-family: var(--base-font-display);
      font-size: 25px;
      font-weight: 850;
    }
    .base-scouting-launch h2 {
      margin: 0 0 6px;
      color: var(--base-ink);
      font-family: var(--base-font-display);
      font-size: 28px;
    }
    .base-scouting-launch p {
      margin: 0;
      color: var(--base-muted);
      font-size: 14px;
      line-height: 1.5;
    }
    .base-scouting-launch-controls {
      padding: 18px;
      border: 1px solid var(--base-border);
      border-radius: var(--base-radius-sm);
      background: rgba(255,255,255,.94);
    }
    .base-scouting-launch-controls .form-group { margin-bottom: 10px; }
    .base-scouting-launch-actions {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 8px;
    }
    .base-scouting-launch-actions .btn { min-height: 38px; }
    .base-scouting-launch-status {
      min-height: 18px;
      margin-top: 9px !important;
      font-size: 12px !important;
    }
    @media (max-width: 900px) {
      .base-scouting-launch { grid-template-columns: 1fr; }
      .base-scouting-workspace .base-scouting-embedded-layout {
        display: block;
      }
      .base-scouting-workspace .base-scouting-sidebar {
        position: static;
        margin-bottom: 16px;
      }
    }
  ")))
}

base_wally_scouting_environment <- function(
  data_dir = base_scouting_data_dir(),
  season_source = base_scouting_season_source()
) {
  if (exists("environment", envir = .base_wally_scouting_state, inherits = FALSE)) {
    return(base::get("environment", envir = .base_wally_scouting_state, inherits = FALSE))
  }

  missing <- BASE_WALLY_SCOUTING_REQUIRED_PACKAGES[
    !vapply(BASE_WALLY_SCOUTING_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Opponent Scouting dependencies are missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(BASE_WALLY_SCOUTING_FILE)) {
    stop("Wally ScoutingApp source is unavailable at ", BASE_WALLY_SCOUTING_FILE)
  }

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- file.path(tempdir(), "base-scouting-outputs")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  workspace <- new.env(parent = globalenv())
  workspace$BASE_SCOUTING_EMBEDDED <- TRUE
  workspace$BASE_SCOUTING_SEASON_SOURCE <- season_source
  workspace$BASE_SCOUTING_APP_ROOT <- dirname(BASE_WALLY_SCOUTING_FILE)
  workspace$BASE_SCOUTING_DATA_DIR <- normalizePath(
    data_dir,
    winslash = "/",
    mustWork = FALSE
  )
  workspace$BASE_SCOUTING_OUTPUT_DIR <- normalizePath(
    output_dir,
    winslash = "/",
    mustWork = FALSE
  )
  workspace$BASE_SCOUTING_THEME <- bslib::bs_theme(
    version = 3,
    primary = TEAM_CONFIG$colors$primary
  )
  workspace$BASE_SCOUTING_HEAD <- base_scouting_embedded_head()

  sys.source(
    BASE_WALLY_SCOUTING_FILE,
    envir = workspace,
    chdir = TRUE,
    keep.source = FALSE
  )
  if (!inherits(workspace$ui, c("shiny.tag", "shiny.tag.list", "list")) ||
      !is.function(workspace$server)) {
    stop("Wally ScoutingApp did not expose a usable UI and server.")
  }

  assign("environment", workspace, envir = .base_wally_scouting_state)
  workspace
}

base_opponent_scouting_workspace_ui <- function() {
  tags$div(
    class = "base-workspace-page base-scouting-host",
    base_scouting_embedded_head(),
    tags$div(
      class = "base-workspace-heading",
      tags$div(
        tags$div(class = "base-eyebrow", "Opponent preparation"),
        tags$h1("Opponent Scouting"),
        tags$p(
          "Build hitter cards, pitcher cards, pitch-type sheets, heat maps, and matchup grids."
        )
      ),
      tags$div(
        class = "base-source-chip",
        tags$span(class = "home-status-dot"),
        "Scouting report workspace"
      )
    ),
    tags$section(
      id = "base-scouting-launch",
      class = "base-scouting-launch",
      tags$div(
        class = "base-scouting-launch-copy",
        tags$div(class = "base-scouting-launch-mark", "B"),
        tags$h2("Choose an opponent to begin"),
        tags$p(
          "Select one team to open its scouting workspace. BASE will load that team's player directory first, then only the players you choose."
        )
      ),
      tags$div(
        class = "base-scouting-launch-controls",
        selectizeInput(
          "base_scouting_team", "Opponent team", choices = NULL,
          options = list(placeholder = "Search college teams", maxOptions = 100),
          width = "100%"
        ),
        tags$div(
          class = "base-scouting-launch-actions",
          actionButton("base_scouting_open", "Open team", class = "btn btn-primary"),
          actionButton("base_scouting_open_csv", "Use game CSVs", class = "btn btn-outline-secondary")
        ),
        shiny::uiOutput("base_scouting_launch_status")
      )
    ),
    tags$div(
      id = "base-scouting-loading",
      class = "base-workspace-loading",
      style = "display:none;",
      tags$div(class = "base-loading-mark", "B"),
      tags$strong("Preparing Opponent Scouting"),
      tags$span("Loading the selected workspace. This runs once for the session.")
    ),
    shiny::uiOutput("base_opponent_scouting_app")
  )
}

base_opponent_scouting_workspace_server <- function(
  input,
  output,
  session,
  data_dir = base_scouting_data_dir(),
  season_source = base_scouting_season_source()
) {
  available_teams <- reactiveVal(NULL)
  workspace_started <- reactiveVal(FALSE)

  observe({
    if (!is.null(available_teams())) return()
    teams <- tryCatch(
      # The pitcher catalog is already mounted at BASE startup and contains
      # the complete team directory. Avoid touching the hitter catalog until
      # a team is explicitly selected because its fallback may need to build.
      sort(unique(season_source$teams("pitcher"))),
      error = function(e) structure(character(), error = conditionMessage(e))
    )
    available_teams(teams)
    if (length(teams)) {
      choices <- stats::setNames(teams, base_scouting_team_display_name(teams))
      updateSelectizeInput(session, "base_scouting_team", choices = choices, server = TRUE)
    }
  })

  output$base_scouting_launch_status <- renderUI({
    teams <- available_teams()
    if (is.null(teams)) return(tags$p(class = "base-scouting-launch-status", "Loading team directory…"))
    error <- attr(teams, "error") %||% ""
    if (nzchar(error)) return(tags$p(
      class = "base-scouting-launch-status text-warning",
      "College directory unavailable. You can still use game CSVs."
    ))
    tags$p(
      class = "base-scouting-launch-status",
      paste(format(length(teams), big.mark = ","), "college teams available")
    )
  })

  start_workspace <- function(mode = c("season", "csv")) {
    mode <- match.arg(mode)
    if (isTRUE(workspace_started())) return(invisible(FALSE))
    if (identical(mode, "season")) {
      team <- input$base_scouting_team %||% ""
      if (!nzchar(team)) {
        showNotification("Choose an opponent team first.", type = "warning")
        return(invisible(FALSE))
      }
    }
    workspace_started(TRUE)
    shinyjs::hide("base-scouting-launch")
    shinyjs::show("base-scouting-loading")

    session$onFlushed(function() {
      tryCatch({
        workspace <- base_wally_scouting_environment(data_dir, season_source)
        output$base_opponent_scouting_app <- shiny::renderUI({
          tags$div(class = "base-scouting-workspace", workspace$ui)
        })
        workspace$server(input, output, session)
        session$onFlushed(function() {
          updateRadioButtons(session, "scout_data_source", selected = mode)
          shinyjs::hide("base-scouting-loading")
        }, once = TRUE)
      }, error = function(e) {
        workspace_started(FALSE)
        shinyjs::hide("base-scouting-loading")
        shinyjs::show("base-scouting-launch")
        showNotification(
          paste("Opponent Scouting could not start:", conditionMessage(e)),
          type = "error"
        )
      })
    }, once = TRUE)
    invisible(TRUE)
  }

  observeEvent(input$base_scouting_open, start_workspace("season"), ignoreInit = TRUE)
  observeEvent(input$base_scouting_open_csv, start_workspace("csv"), ignoreInit = TRUE)
  invisible(list(started = workspace_started, teams = available_teams))
}
