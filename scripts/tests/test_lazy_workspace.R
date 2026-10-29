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

byte_cache <- new.env(parent = emptyenv())
assign("small", raw(32), envir = byte_cache)
assign("large", raw(2048), envir = byte_cache)
byte_order <- base_prune_cache_env(byte_cache, c("small", "large"), limit = 4L, max_bytes = 1024)
if (!identical(byte_order, "large") || exists("small", envir = byte_cache, inherits = FALSE)) {
  fail("Byte-bounded cache did not evict the least recently used value.")
}

lazy_calls <- 0L
nested_calls <- 0L
shiny::testServer(
  function(input, output, session) {
    base_lazy_workspace_server(
      input, session, c("tab_target", "tab_alias"),
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
    session$setInputs(base_nav = "tab_alias")
    session$flushReact()
    if (lazy_calls != 1L) fail("Workspace did not initialize from an alternate registered tab.")
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

conditional_calls <- 0L
shiny::testServer(
  function(input, output, session) {
    base_lazy_workspace_server(
      input, session, c("tab_workspace", "tab_reports"),
      initialize = function() conditional_calls <<- conditional_calls + 1L,
      id = "conditional_workspace",
      active_when = function() {
        identical(input$base_nav, "tab_workspace") ||
          (identical(input$base_nav, "tab_reports") && identical(input$report_tab, "Target"))
      }
    )
  },
  {
    session$setInputs(base_nav = "tab_reports", report_tab = "Other")
    session$flushReact()
    if (conditional_calls != 0L) fail("Conditional workspace initialized for an inactive nested tab.")
    session$setInputs(report_tab = "Target")
    session$flushReact()
    if (conditional_calls != 1L) fail("Conditional workspace did not initialize for its nested tab.")
    session$setInputs(base_nav = "tab_workspace")
    session$flushReact()
    if (conditional_calls != 1L) fail("Conditional workspace initialized more than once.")
  }
)

cat("Lazy workspace and bounded cache tests passed.\n")

# Column projection must remain identical to the original boundary matcher,
# including quoted names and avoiding partial matches such as Pitcher/PitcherId.
source_lines <- c('rows$PitcherId', 'select(rows, `Top/Bottom`, `Exit Speed`)',
                  'rows$.private', '# SpinRate is referenced', 'fooRelSpeedbar')
columns <- c('Pitcher', 'PitcherId', 'Top/Bottom', 'Exit Speed', '.private',
             'SpinRate', 'RelSpeed', 'Missing')
legacy_columns <- function(columns, lines) {
  columns[vapply(columns, function(column) {
    any(grepl(paste0('(?<![[:alnum:]_.])\\Q', column,
                     '\\E(?![[:alnum:]_.])'), lines, perl = TRUE))
  }, logical(1))]
}
if (!identical(base_referenced_columns(columns, source_lines),
               legacy_columns(columns, source_lines))) {
  fail('Optimized source discovery changed selected season columns.')
}
if (length(base_referenced_columns(columns, character()))) {
  fail('Empty sources must not select runtime columns.')
}

# Multiple evictions must satisfy both limits and preserve the newest value.
prune_values <- new.env(parent = emptyenv())
for (key in letters[1:4]) assign(key, raw(1024), envir = prune_values)
pruned <- base_prune_cache_env(prune_values, letters[1:4], 3L, 2200)
if (!identical(pruned, c('c', 'd')) ||
    !identical(sort(ls(prune_values)), c('c', 'd'))) {
  fail('Cache pruning did not satisfy combined byte and entry limits.')
}
cat('Runtime column projection and multi-eviction checks passed.\n')
