# Lightweight performance primitives shared by BASE workspaces.

base_once <- function(callback) {
  stopifnot(is.function(callback))
  completed <- FALSE
  running <- FALSE

  function(...) {
    if (completed || running) return(invisible(FALSE))
    running <<- TRUE
    on.exit(running <<- FALSE, add = TRUE)
    tryCatch({
      callback(...)
      completed <<- TRUE
      invisible(TRUE)
    }, error = function(e) {
      completed <<- FALSE
      stop(e)
    })
  }
}

base_lru_cache <- function(limit = 16L) {
  limit <- suppressWarnings(as.integer(limit))
  if (is.na(limit) || limit < 1L) stop("Cache limit must be a positive integer.")

  values <- new.env(parent = emptyenv())
  order <- character()

  has <- function(key) {
    key <- as.character(key)[[1]]
    exists(key, envir = values, inherits = FALSE)
  }

  get <- function(key, default = NULL) {
    key <- as.character(key)[[1]]
    if (!has(key)) return(default)
    order <<- c(setdiff(order, key), key)
    base::get(key, envir = values, inherits = FALSE)
  }

  put <- function(key, value) {
    key <- as.character(key)[[1]]
    assign(key, value, envir = values)
    order <<- c(setdiff(order, key), key)
    while (length(order) > limit) {
      evict <- order[[1]]
      rm(list = evict, envir = values)
      order <<- order[-1]
    }
    invisible(value)
  }

  clear <- function() {
    keys <- ls(values, all.names = TRUE)
    if (length(keys)) rm(list = keys, envir = values)
    order <<- character()
    invisible(TRUE)
  }

  list(
    has = has,
    get = get,
    put = put,
    clear = clear,
    keys = function() order,
    size = function() length(order),
    limit = limit
  )
}

base_lazy_workspace_server <- function(input, session, tab_value, initialize,
                                       id = tab_value, nav_input = "base_nav") {
  stopifnot(
    is.function(initialize),
    length(tab_value) >= 1L,
    all(!is.na(tab_value)),
    all(nzchar(tab_value))
  )
  if (length(id) != 1L) id <- tab_value[[1]]
  stopifnot(!is.na(id), nzchar(id))

  if (is.null(session$userData$base_lazy_workspaces)) {
    session$userData$base_lazy_workspaces <- new.env(parent = emptyenv())
  }
  registry <- session$userData$base_lazy_workspaces
  if (exists(id, envir = registry, inherits = FALSE)) {
    stop("Lazy workspace already registered for this session: ", id)
  }

  status <- shiny::reactiveVal("waiting")
  initialize_once <- base_once(function() {
    status("loading")
    tryCatch({
      initialize()
      status("ready")
      message("Initialized workspace on first visit: ", id)
    }, error = function(e) {
      status("error")
      stop(e)
    })
  })

  observer <- shiny::observeEvent(input[[nav_input]], {
    active_tab <- input[[nav_input]]
    if (is.null(active_tab) || !length(active_tab) || !(active_tab[[1]] %in% tab_value)) return()
    tryCatch(
      initialize_once(),
      error = function(e) {
        message("Workspace initialization failed for ", id, ": ", conditionMessage(e))
        shiny::showNotification(
          paste("Could not initialize", id, ":", conditionMessage(e)),
          type = "error"
        )
      }
    )
  }, ignoreInit = FALSE, ignoreNULL = FALSE, priority = 100)

  handle <- list(
    id = id,
    tab_value = tab_value,
    status = status,
    observer = observer
  )
  assign(id, handle, envir = registry)
  invisible(handle)
}
