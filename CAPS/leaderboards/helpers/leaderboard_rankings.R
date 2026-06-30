leaderboard_safe_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", x)))
}

rank_metric_leaders <- function(df,
                                entity_col,
                                metric_name,
                                descending = TRUE,
                                n = 10,
                                sort_col = metric_name,
                                filter_fn = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame())
  }

  work <- df
  if (!is.null(filter_fn)) {
    work <- filter_fn(work)
  }

  required_cols <- unique(c(entity_col, metric_name, sort_col))
  if (!all(required_cols %in% names(work))) {
    return(data.frame())
  }

  sort_values <- leaderboard_safe_num(work[[sort_col]])
  keep <- !is.na(sort_values) &
    !is.na(work[[entity_col]]) &
    nzchar(trimws(as.character(work[[entity_col]])))

  work <- work[keep, , drop = FALSE]
  if (!nrow(work)) {
    return(data.frame())
  }

  work[["..sort_value.."]] <- sort_values[keep]
  ord <- if (isTRUE(descending)) {
    order(-work[["..sort_value.."]], as.character(work[[entity_col]]))
  } else {
    order(work[["..sort_value.."]], as.character(work[[entity_col]]))
  }

  work <- utils::head(work[ord, , drop = FALSE], n)
  work[, setdiff(names(work), "..sort_value.."), drop = FALSE]
}

calculate_hitter_ppi_metric <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      dplyr::across(c(`K%`, `BB%`, `Barrel%`), leaderboard_safe_num),
      Z_K = (`K%` - mean(`K%`, na.rm = TRUE)) / stats::sd(`K%`, na.rm = TRUE),
      Z_BB = (`BB%` - mean(`BB%`, na.rm = TRUE)) / stats::sd(`BB%`, na.rm = TRUE),
      Z_Barrel = (`Barrel%` - mean(`Barrel%`, na.rm = TRUE)) / stats::sd(`Barrel%`, na.rm = TRUE),
      RawPPI = (-0.5 * Z_K) + (0.5 * Z_BB) + (0.5 * Z_Barrel),
      HitterPPI = (1 / (1 + exp(-RawPPI))) * 0.9,
      HitterPPI = HitterPPI - 0.15,
      HitterPPI = pmin(pmax(HitterPPI, 0), 1.25),
      HitterPPI = round(HitterPPI, 3)
    ) %>%
    dplyr::select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
}

calculate_pitcher_ppi_metric <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      dplyr::across(c(`K%`, `BB%`, `Barrel%`), leaderboard_safe_num),
      Z_K = (`K%` - mean(`K%`, na.rm = TRUE)) / stats::sd(`K%`, na.rm = TRUE),
      Z_BB = (`BB%` - mean(`BB%`, na.rm = TRUE)) / stats::sd(`BB%`, na.rm = TRUE),
      Z_Barrel = (`Barrel%` - mean(`Barrel%`, na.rm = TRUE)) / stats::sd(`Barrel%`, na.rm = TRUE),
      RawPPI = (1.2 * Z_K) - (0.9 * Z_BB) - (0.9 * Z_Barrel),
      `PPI (ERA)` = round(4.50 - (0.5 * RawPPI), 2)
    ) %>%
    dplyr::select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
}
