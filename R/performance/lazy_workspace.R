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

base_prune_cache_env <- function(values, order, limit, max_bytes = Inf) {
  stopifnot(is.environment(values))
  limit <- suppressWarnings(as.integer(limit))
  max_bytes <- suppressWarnings(as.numeric(max_bytes))
  if (is.na(limit) || limit < 1L) stop("Cache limit must be a positive integer.")
  if (is.na(max_bytes) || max_bytes <= 0) stop("Cache byte limit must be positive.")

  cache_bytes <- function() {
    keys <- intersect(order, ls(values, all.names = TRUE))
    if (!length(keys)) return(0)
    sum(vapply(keys, function(key) as.numeric(object.size(get(key, envir = values, inherits = FALSE))), numeric(1)))
  }

  # Retain the newest value even when a single item exceeds the target. That
  # keeps the current request usable while preventing accumulation around it.
  while (length(order) > 1L && (length(order) > limit || cache_bytes() > max_bytes)) {
    evict <- order[[1]]
    if (exists(evict, envir = values, inherits = FALSE)) rm(list = evict, envir = values)
    order <- order[-1]
  }
  order
}

base_lazy_workspace_server <- function(input, session, tab_value, initialize,
                                       id = tab_value, nav_input = "base_nav",
                                       active_when = NULL) {
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

  is_active <- shiny::reactive({
    if (is.function(active_when)) return(isTRUE(active_when()))
    active_tab <- input[[nav_input]]
    !is.null(active_tab) && length(active_tab) && active_tab[[1]] %in% tab_value
  })

  observer <- shiny::observeEvent(is_active(), {
    if (!isTRUE(is_active())) return()
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
    is_active = is_active,
    observer = observer
  )
  assign(id, handle, envir = registry)
  invisible(handle)
}
