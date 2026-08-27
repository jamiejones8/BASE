#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("scripts/dev/run_integration_preview.R", mustWork = TRUE)
}

base_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
setwd(base_root)

local_library <- file.path(base_root, ".baseline-runtime", "R")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(readr)
})

source(file.path(base_root, "team_config.R"), local = FALSE)
BASE_NCAA_D1_SOURCE_LABEL <- "2026 NCAA Division I"
source(file.path(base_root, "R", "performance", "lazy_workspace.R"), local = FALSE)
source(file.path(base_root, "R", "integrations", "wally_pitching_workspace.R"), local = FALSE)
source(file.path(base_root, "R", "integrations", "wally_hitting_workspace.R"), local = FALSE)
source(file.path(base_root, "R", "integrations", "wally_scouting_workspace.R"), local = FALSE)

fixture_path <- file.path(
  base_root, "tests", "fixtures", "wallyapps", "pitching", "data",
  "2026 Season - cleaned.csv"
)
preview_rows <- readr::read_csv(fixture_path, show_col_types = FALSE) |>
  mutate(
    source_file = basename(fixture_path),
    row_in_file = row_number(),
    SeasonGroup = "2026 Season",
    DataSource = BASE_NCAA_D1_SOURCE_LABEL,
    PitcherTeam = coalesce(as.character(.data$PitcherTeam), TEAM_CONFIG$data_code)
  )

hitting_fixture_path <- file.path(
  base_root, "tests", "fixtures", "wallyapps", "hitting", "data",
  "2026 Season - cleaned.csv"
)
preview_hitting_rows <- readr::read_csv(hitting_fixture_path, show_col_types = FALSE) |>
  mutate(
    source_file = basename(hitting_fixture_path),
    row_in_file = row_number(),
    SeasonGroup = "S26",
    DataSource = BASE_NCAA_D1_SOURCE_LABEL,
    BatterTeam = coalesce(as.character(.data$BatterTeam), TEAM_CONFIG$data_code)
  )

addResourcePath("preview-assets", file.path(base_root, "www"))
preview_asset <- function(path) paste0("preview-assets/", basename(path))

nav_click_js <- function(tab_value) {
  sprintf(
    "var link=document.querySelector('.navbar-nav a[data-value=\"%s\"]');if(link){link.click();window.scrollTo({top:0,behavior:'smooth'});}",
    tab_value
  )
}

home_quick_link <- function(label, description, tab_value, number) {
  tags$button(
    type = "button", class = "home-quick-link", onclick = nav_click_js(tab_value),
    tags$span(class = "home-quick-number", number),
    tags$span(class = "home-quick-copy", tags$strong(label), tags$small(description)),
    tags$span(class = "home-quick-arrow", HTML("&rarr;"))
  )
}

placeholder_workspace <- function(eyebrow, title, description) {
  tags$div(
    class = "hub-main base-page base-workspace-landing",
    tags$div(class = "base-eyebrow", eyebrow),
    tags$h2(title),
    tags$p(class = "base-workspace-description", description),
    tags$div(
      class = "base-workspace-tool-grid",
      tags$button(
        type = "button", class = "base-workspace-tool is-pending", disabled = "disabled",
        tags$span(class = "base-tool-eyebrow", "Integration preview"),
        tags$strong("Workspace retained"),
        tags$p("The existing BASE functionality remains in this section while its final integration is completed."),
        tags$span(class = "base-tool-action", "Next integration slice", HTML(" &rarr;"))
      )
    )
  )
}

roster_names <- preview_rows |>
  distinct(Pitcher) |>
  filter(!is.na(Pitcher), nzchar(Pitcher)) |>
  pull(Pitcher)

team_identity <- tags$div(
  class = "home-team-identity",
  tags$div(
    class = "home-team-marks",
    tags$img(src = preview_asset(base_supercat_logo_url()), alt = paste(TEAM_CONFIG$full_name, "logo"), class = "home-team-logo")
  ),
  tags$div(
    tags$div(class = "home-season-line", "Bobcats Analytics & Scouting Engine"),
    tags$h1("Texas State Baseball"),
    tags$p("NCAA Division I analytics, scouting, and team intelligence in one workspace.")
  )
)

preview_home <- tagList(
  tags$head(tags$style(HTML("
    #base-home .content-area { background: transparent; }
    .tab-content > .tab-pane { padding: 0; }
  "))),
  tags$div(
    id = "base-home",
    tags$section(
      id = "base-scoreboard", class = "base-home-hero base-home-hero-empty",
      tags$div(
        class = "base-home-hero-inner",
        team_identity,
        tags$div(
          class = "home-next-game-card is-empty",
          tags$div(class = "home-game-kicker", "Integration preview"),
          tags$div(class = "home-calendar-mark", tags$span("DEV"), tags$strong("26")),
          tags$h2("Unified workspace"),
          tags$p("The real BASE shell with the completed Pitching integration connected to safe fixture data."),
          tags$div(class = "home-schedule-status", tags$span(class = "home-status-dot"), "No production player data is loaded in this preview")
        )
      )
    ),
    tags$div(
      class = "content-area",
      tags$section(
        class = "home-command-section",
        tags$div(
          class = "home-section-heading",
          tags$div(tags$div(class = "base-eyebrow", "BASE command center"), tags$h2("Start with what you need")),
          tags$div(class = "home-data-status", tags$span(class = "home-status-dot"), "Integration preview ready")
        ),
        tags$div(
          class = "home-quick-grid",
          home_quick_link("Postgame Reports", "Pitching, hitting, and catching PDFs", "tab_postgame_reports", "01"),
          home_quick_link("Pitching", "Staff performance, bullpens, trends, and reports", "tab_team_pitching", "02"),
          home_quick_link("Hitting", "Lineups, team trends, and hitter development", "tab_team_hitting", "03"),
          home_quick_link("Opponent Scouting", "Scout NCAA pitchers and hitters", "tab_opponent_scouting", "04"),
          home_quick_link("Defensive Analytics", "Positioning, range, and catcher receiving", "tab_defense", "05"),
          home_quick_link("HomeBASE", "Search and open an individual player snapshot", "tab_homebase", "06"),
          home_quick_link("Data Processing", "Retag, validate, and prepare application data", "tab_data_processing", "07")
        )
      ),
      tags$section(
        class = "home-roster-section",
        tags$div(
          class = "home-section-heading home-roster-heading",
          tags$div(tags$div(class = "base-eyebrow", "Team directory"), tags$h2(TEAM_CONFIG$roster_label)),
          tags$div(
            class = "pos-filters",
            tags$div(class = "pos-pill active", `data-group` = "Pitchers", "Pitchers"),
            tags$div(class = "pos-pill", `data-group` = "Catchers", "Catchers"),
            tags$div(class = "pos-pill", `data-group` = "Infielders", "Infielders"),
            tags$div(class = "pos-pill", `data-group` = "Outfielders", "Outfielders")
          )
        ),
        tags$div(
          class = "roster-grid",
          lapply(seq_along(roster_names), function(index) {
            tags$div(
              class = "player-card", `data-group` = "Pitchers", style = "cursor:pointer;",
              onclick = nav_click_js("tab_homebase"),
              tags$div(class = "p-init", sprintf("%02d", index)),
              tags$div(tags$div(class = "p-name", roster_names[[index]]), tags$div(class = "p-info", "R/R"))
            )
          })
        )
      )
    ),
    tags$script(HTML("
      $(document).on('click', '#base-home .pos-pill', function() {
        $('#base-home .pos-pill').removeClass('active');
        $(this).addClass('active');
        var grp = $(this).data('group');
        $('#base-home .player-card').each(function() { $(this).toggle($(this).data('group') === grp); });
      });
    "))
  )
)

ui <- navbarPage(
  title = tagList(
    tags$img(src = preview_asset(base_supercat_logo_url()), alt = "Texas State SuperCat", class = "base-nav-mark"),
    tags$span(
      class = "base-brand-lockup",
      tags$span(class = "base-brand-name", "BASE"),
      tags$span(class = "base-brand-team", TEAM_CONFIG$organization)
    )
  ),
  id = "base_nav",
  collapsible = TRUE,
  windowTitle = paste(TEAM_CONFIG$full_name, "BASE Integration Preview"),
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "icon", href = preview_asset(base_supercat_logo_url())),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Courier+Prime&family=Source+Sans+3:wght@400;600&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "preview-assets/styles.css?v=19"),
      tags$style(HTML(base_brand_css(include_leaderboards = FALSE)))
    ),
    tags$button(
      id = "base-shell-home", type = "button", onclick = nav_click_js("tab_home"),
      `aria-label` = "Return to BASE Home",
      tags$img(src = preview_asset(base_supercat_logo_url()), alt = ""), tags$span("Home")
    )
  ),
  tabPanel("Home", value = "tab_home", preview_home),
  tabPanel("Postgame Reports", value = "tab_postgame_reports", placeholder_workspace("PDF report generators", "Postgame Reports", "The three existing BASE report generators remain together here.")),
  tabPanel("Pitching", value = "tab_team_pitching", base_team_pitching_workspace_ui()),
  tabPanel("Hitting", value = "tab_team_hitting", base_team_hitting_workspace_ui()),
  tabPanel("Opponent Scouting", value = "tab_opponent_scouting", base_opponent_scouting_workspace_ui()),
  tabPanel("Defense", value = "tab_defense", placeholder_workspace("Positioning, opportunities, and receiving", "Defensive Analytics", "Catcher framing and the broader defensive analytics suite live here.")),
  tabPanel("HomeBASE", value = "tab_homebase", placeholder_workspace("Player search", "HomeBASE", "Search any NCAA player or arrive directly from a Texas State roster card.")),
  tabPanel("Data Processing", value = "tab_data_processing", placeholder_workspace("Prepare, validate, and correct", "Data Processing", "Pitch retagging and internal preparation tools live here."))
)

server <- function(input, output, session) {
  base_lazy_workspace_server(
    input = input, session = session, nav_input = "base_nav", tab_value = "tab_team_pitching",
    initialize = function() {
      workspace <- base_wally_pitching_environment(preview_rows)
      output$base_team_pitching_app <- renderUI({
        tags$div(class = "base-pitching-workspace", workspace$ui)
      })
      workspace$server(input, output, session)
      shinyjs::hide("base-pitching-loading")
    },
    id = "integration_preview_pitching"
  )
  base_lazy_workspace_server(
    input = input, session = session, nav_input = "base_nav", tab_value = "tab_team_hitting",
    initialize = function() {
      workspace <- base_wally_hitting_environment(preview_hitting_rows)
      output$base_team_hitting_app <- renderUI({
        tags$div(class = "base-hitting-workspace", workspace$ui)
      })
      workspace$server(input, output, session)
      shinyjs::hide("base-hitting-loading")
    },
    id = "integration_preview_hitting"
  )
  base_lazy_workspace_server(
    input = input, session = session, nav_input = "base_nav", tab_value = "tab_opponent_scouting",
    initialize = function() base_opponent_scouting_workspace_server(
      input,
      output,
      session,
      data_dir = file.path(base_root, "tests", "fixtures", "wallyapps", "hitting", "data")
    ),
    id = "integration_preview_scouting"
  )
}

options(shiny.autoreload = FALSE)
shiny::runApp(shinyApp(ui = ui, server = server), host = "127.0.0.1", port = 8091, launch.browser = FALSE)
