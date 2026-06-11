whitecaps_hub_image_src <- local({
  image_file <- "www/surf.png"

  if (!file.exists(image_file)) {
    return("surf.png")
  }

  paste0("surf.png?v=", as.integer(file.info(image_file)$mtime[[1]]))
})

WHITECAPS_PAGE_ID <- "whitecaps_app"

whitecaps_env <- local({
  env <- new.env(parent = globalenv())
  source("leaderboards/app.R", local = env, chdir = TRUE)
  env
})

whitecaps_hub_card <- function() {
  list(
    id = "whitecaps_central",
    title = "Whitecaps Analytics",
    page = WHITECAPS_PAGE_ID,
    status = "live",
    image_src = whitecaps_hub_image_src
  )
}

whitecaps_is_page <- function(page) {
  identical(page, WHITECAPS_PAGE_ID)
}

whitecaps_embedded_ui <- function() {
  tagList(whitecaps_env$ui)
}

whitecaps_bind_parent_server <- function(current_page, input, output, session, hub_page = "hub") {
  initialized <- reactiveVal(FALSE)

  observeEvent(input$nav_caps_hub, {
    current_page(hub_page)
  })

  observeEvent(current_page(), {
    if (whitecaps_is_page(current_page()) && !initialized()) {
      session$onFlushed(function() {
        whitecaps_env$server(input, output, session)
        initialized(TRUE)
      }, once = TRUE)
    }

    if (!whitecaps_is_page(current_page())) {
      removeUI(selector = "head #whitecaps-stylesheet", multiple = TRUE, immediate = TRUE)
      removeUI(selector = "head #whitecaps-font-bebas", multiple = TRUE, immediate = TRUE)
    }
  }, ignoreInit = FALSE)
}
