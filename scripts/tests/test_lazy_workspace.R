#!/usr/bin/env Rscript

source("R/performance/lazy_workspace.R", local = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)

calls <- 0L
once <- base_once(function() calls <<- calls + 1L)
if (!isTRUE(once()) || calls != 1L) fail("base_once did not execute its callback.")
if (!identical(once(), FALSE) || calls != 1L) fail("base_once executed more than once.")

retry_calls <- 0L
retry <- base_once(function() {
  retry_calls <<- retry_calls + 1L
  if (retry_calls == 1L) stop("expected first-attempt failure")
})
try(retry(), silent = TRUE)
if (!isTRUE(retry()) || retry_calls != 2L) fail("base_once did not permit retry after failure.")

cache <- base_lru_cache(limit = 2L)
cache$put("a", 1L)
cache$put("b", 2L)
if (!identical(cache$get("a"), 1L)) fail("LRU cache get failed.")
cache$put("c", 3L)
if (cache$has("b") || !cache$has("a") || !cache$has("c")) {
  fail("LRU cache did not evict the least recently used key.")
}
cache$clear()
if (cache$size() != 0L) fail("LRU cache clear failed.")

lazy_calls <- 0L
nested_calls <- 0L
shiny::testServer(
  function(input, output, session) {
    base_lazy_workspace_server(
      input, session, "tab_target",
      initialize = function() {
        lazy_calls <<- lazy_calls + 1L
        output$late_output <- shiny::renderText("ready")
        shiny::observeEvent(input$late_action, {
          nested_calls <<- nested_calls + 1L
        }, ignoreInit = TRUE)
      },
      id = "test_workspace"
    )
  },
  {
    session$setInputs(base_nav = "tab_home")
    session$flushReact()
    if (lazy_calls != 0L) fail("Workspace initialized before its first visit.")
    session$setInputs(base_nav = "tab_target")
    session$flushReact()
    if (lazy_calls != 1L) fail("Workspace did not initialize on first visit.")
    if (!identical(output$late_output, "ready")) {
      fail("A lazily registered output did not become available.")
    }
    session$setInputs(late_action = 1L)
    session$flushReact()
    if (nested_calls != 1L) fail("A lazily registered observer did not run.")
    session$setInputs(base_nav = "tab_home")
    session$setInputs(base_nav = "tab_target")
    session$flushReact()
    if (lazy_calls != 1L) fail("Workspace initialized more than once.")
  }
)

cat("Lazy workspace and bounded cache tests passed.\n")
