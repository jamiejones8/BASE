# NCAA Division I AAR benchmarks derived from the parquet-backed percentile
# reference. `sample_n` is the denominator used for each player metric, so a
# weighted mean reconstructs the population rate instead of averaging players
# with very different sample sizes equally.

.base_d1_aar_benchmark_cache <- new.env(parent = emptyenv())

base_d1_aar_reference_path <- function() {
  base_project_path(
    "WallyApps", "PitchingApp", "data",
    "d1_pitch_metric_percentile_reference.csv"
  )
}

base_read_d1_aar_reference <- function(path = base_d1_aar_reference_path()) {
  if (!file.exists(path)) return(tibble::tibble())

  path <- normalizePath(path, mustWork = TRUE)
  mtime <- as.numeric(file.info(path)$mtime)
  cache_key <- paste(path, mtime, sep = "::")
  if (exists(cache_key, envir = .base_d1_aar_benchmark_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .base_d1_aar_benchmark_cache, inherits = FALSE))
  }

  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE) %>% tibble::as_tibble(),
    error = function(e) {
      message("D1 AAR benchmark reference failed: ", conditionMessage(e))
      tibble::tibble()
    }
  )
  assign(cache_key, out, envir = .base_d1_aar_benchmark_cache)
  out
}

base_weighted_d1_aar_metric <- function(reference, metric) {
  required <- c("scope", "metric", "value", "sample_n")
  if (is.null(reference) || !nrow(reference) || !all(required %in% names(reference))) {
    return(NA_real_)
  }

  rows <- reference %>%
    dplyr::filter(
      .data$scope == "overall",
      .data$metric == .env$metric,
      is.finite(.data$value),
      is.finite(.data$sample_n),
      .data$sample_n > 0
    )
  if (!nrow(rows)) return(NA_real_)
  stats::weighted.mean(rows$value, rows$sample_n, na.rm = TRUE)
}

base_d1_aar_benchmarks <- function(path = base_d1_aar_reference_path()) {
  reference <- base_read_d1_aar_reference(path)

  hitting_defaults <- c(whiff = 0.23, chase = 0.24, barrel = 0.17)
  pitching_defaults <- c(
    strike_pct = 0.65,
    zone_pct = 0.50,
    fps_pct = 0.63,
    pre2k_zone = 0.50,
    ea_pct = 0.70,
    put_away_pct = 0.19
  )

  hitting_metrics <- c(
    whiff = "performance_whiff_pct",
    chase = "performance_chase_pct",
    barrel = "performance_barrel_pct"
  )
  pitching_metrics <- c(
    strike_pct = "performance_strike_pct",
    zone_pct = "performance_zone_pct",
    fps_pct = "performance_fps_pct",
    pre2k_zone = "performance_pre2k_zone_pct",
    ea_pct = "performance_ea_pct",
    put_away_pct = "performance_put_away_pct"
  )

  resolve <- function(metric_map, defaults) {
    calculated <- vapply(
      metric_map,
      function(metric) base_weighted_d1_aar_metric(reference, metric),
      numeric(1)
    )
    invalid <- !is.finite(calculated) | calculated < 0 | calculated > 1
    calculated[invalid] <- defaults[names(calculated)[invalid]]
    calculated
  }

  list(
    hitting = resolve(hitting_metrics, hitting_defaults),
    pitching = resolve(pitching_metrics, pitching_defaults),
    source = if (file.exists(path)) normalizePath(path, mustWork = TRUE) else NA_character_
  )
}
