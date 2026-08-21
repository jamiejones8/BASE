team_analytics_hub_image_src <- local({
  configured <- base_asset_url(TEAM_CONFIG$assets$hub_image)
  image_file <- file.path("www", configured)

  if (!file.exists(image_file)) {
    return(configured)
  }

  paste0(configured, "?v=", as.integer(file.info(image_file)$mtime[[1]]))
})

TEAM_ANALYTICS_PAGE_ID <- "team_analytics_app"

team_analytics_env <- local({
  env <- new.env(parent = globalenv())
  source("leaderboards/app.R", local = env, chdir = TRUE)
  env
})

team_analytics_hub_card <- function() {
  list(
    id = "team_analytics",
    title = paste(TEAM_CONFIG$name, "Analytics"),
    page = TEAM_ANALYTICS_PAGE_ID,
    status = "live",
    image_src = team_analytics_hub_image_src
  )
}

team_analytics_is_page <- function(page) {
  identical(page, TEAM_ANALYTICS_PAGE_ID)
}

team_analytics_embedded_ui <- function() {
  if (exists("embedded_ui", envir = team_analytics_env, inherits = FALSE)) {
    return(tagList(team_analytics_env$embedded_ui))
  }

  tagList(team_analytics_env$ui)
}

team_analytics_bind_parent_server <- function(current_page, input, output, session, hub_page = "hub") {
  initialized <- reactiveVal(FALSE)

  observeEvent(current_page(), {
    if (team_analytics_is_page(current_page()) && !initialized()) {
      session$onFlushed(function() {
        team_analytics_env$server(input, output, session)
        initialized(TRUE)
      }, once = TRUE)
    }

    if (!team_analytics_is_page(current_page())) {
      removeUI(selector = "head #team-analytics-stylesheet", multiple = TRUE, immediate = TRUE)
      removeUI(selector = "head #team-analytics-font-app", multiple = TRUE, immediate = TRUE)
    }
  }, ignoreInit = FALSE)
}
