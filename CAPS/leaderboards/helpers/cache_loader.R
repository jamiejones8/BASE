LEADERBOARDS_CACHE_VERSION <- function() {
  1L
}

leaderboards_cache_path <- function(
    cache_dir = get0(
      "WHITE_CAPS_CACHE_DIR",
      inherits = TRUE,
      ifnotfound = file.path(
        get0("WHITE_CAPS_APP_DIR", inherits = TRUE, ifnotfound = "."),
        "cache"
      )
    )
) {
  file.path(cache_dir, "leaderboards_cache.rds")
}

leaderboards_source_signature <- function(file_path, source_label = basename(file_path)) {
  info <- file.info(file_path)

  list(
    file = source_label,
    path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
    size = unname(info$size[[1]]),
    mtime = unname(as.numeric(info$mtime[[1]]))
  )
}

build_leaderboards_cache_bundle <- function(source_file,
                                            processed_raw,
                                            hitting_stats,
                                            pitching_stats_by_tto,
                                            process_stats_by_tto,
                                            pitch_type_breakdown_by_tto,
                                            home_counts) {
  list(
    metadata = list(
      version = LEADERBOARDS_CACHE_VERSION(),
      built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      source = leaderboards_source_signature(source_file$path, source_file$label),
      pitch_tto_choices = c("All Times Through Order", PITCHER_TTO_LEVELS())
    ),
    processed_raw = processed_raw,
    hitting_stats = hitting_stats,
    pitching_stats_by_tto = pitching_stats_by_tto,
    process_stats_by_tto = process_stats_by_tto,
    pitch_type_breakdown_by_tto = pitch_type_breakdown_by_tto,
    home_counts = home_counts
  )
}

save_leaderboards_cache <- function(bundle, cache_path = leaderboards_cache_path()) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, cache_path)
  invisible(cache_path)
}

read_leaderboards_cache <- function(cache_path = leaderboards_cache_path()) {
  if (!file.exists(cache_path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(cache_path),
    error = function(e) {
      warning("⚠️ Unable to read leaderboards cache: ", e$message)
      NULL
    }
  )
}

validate_leaderboards_cache <- function(bundle, active_file) {
  if (is.null(active_file$path) || !nzchar(active_file$path) || !file.exists(active_file$path)) {
    return(list(valid = FALSE, reason = "no active leaderboard source file found"))
  }

  if (is.null(bundle) || !is.list(bundle)) {
    return(list(valid = FALSE, reason = "cache bundle is missing or unreadable"))
  }

  if (is.null(bundle$metadata) || !is.list(bundle$metadata)) {
    return(list(valid = FALSE, reason = "cache metadata is missing"))
  }

  if (!identical(bundle$metadata$version, LEADERBOARDS_CACHE_VERSION())) {
    return(list(valid = FALSE, reason = "cache version does not match current app version"))
  }

  required_components <- c(
    "processed_raw",
    "hitting_stats",
    "pitching_stats_by_tto",
    "process_stats_by_tto",
    "pitch_type_breakdown_by_tto",
    "home_counts"
  )

  missing_components <- required_components[!vapply(required_components, function(name) name %in% names(bundle), logical(1))]
  if (length(missing_components) > 0) {
    return(list(valid = FALSE, reason = paste("cache is missing components:", paste(missing_components, collapse = ", "))))
  }

  expected <- leaderboards_source_signature(active_file$path, active_file$label)
  actual <- bundle$metadata$source

  if (is.null(actual$file) || !identical(actual$file, expected$file)) {
    return(list(valid = FALSE, reason = "cache source file name does not match the active leaderboard source"))
  }

  if (is.null(actual$size) || !identical(as.numeric(actual$size), as.numeric(expected$size))) {
    return(list(valid = FALSE, reason = "cache source file size does not match the active leaderboard source"))
  }

  if (is.null(actual$mtime) || !identical(as.numeric(actual$mtime), as.numeric(expected$mtime))) {
    return(list(valid = FALSE, reason = "cache source file modified time does not match the active leaderboard source"))
  }

  list(valid = TRUE, reason = NULL)
}

load_valid_leaderboards_cache <- function(active_file = pick_active_bundled_file(),
                                          cache_path = leaderboards_cache_path()) {
  bundle <- read_leaderboards_cache(cache_path)
  verdict <- validate_leaderboards_cache(bundle, active_file)

  if (!isTRUE(verdict$valid)) {
    return(list(valid = FALSE, reason = verdict$reason, bundle = NULL))
  }

  list(valid = TRUE, reason = NULL, bundle = bundle)
}
