# =========================================================
#  BREWSTER WHITECAPS — bundled single-file loader
#  - Loads one active CSV from an explicit configured path
#  - Uses data.table::fread() for speed
# =========================================================

library(dplyr)
library(data.table)

source("helpers/process_data.R", local = TRUE)
source("helpers/metric_helpers.R", local = TRUE)
source("helpers/calculate_pitcher_stats.R", local = TRUE)
source("helpers/calculate_hitter_stats.R", local = TRUE)

SUPPORTED_BUNDLED_EXTENSIONS <- function() {
  c("csv")
}

configured_bundled_source_file <- function() {
  get0(
    "WHITE_CAPS_SOURCE_FILE",
    inherits = TRUE,
    ifnotfound = file.path("..", "test.csv")
  )
}

PREFERRED_BUNDLED_FILE_NAME <- function() {
  basename(configured_bundled_source_file())
}

is_supported_bundled_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  nzchar(ext) && ext %in% SUPPORTED_BUNDLED_EXTENSIONS()
}

safe_count_rows <- function(path) {
  if (!file.exists(path)) {
    return(NA_integer_)
  }

  tryCatch(
    nrow(
      data.table::fread(
        path,
        select = 1L,
        showProgress = FALSE,
        nThread = 1L,
        fill = Inf
      )
    ),
    error = function(e) {
      warning("⚠️ Unable to count rows for ", basename(path), ": ", e$message)
      NA_integer_
    }
  )
}

list_bundled_data_files <- function(
    source_file = configured_bundled_source_file()
) {
  normalized_source <- normalizePath(source_file, winslash = "/", mustWork = FALSE)

  if (!is_supported_bundled_file(normalized_source) || !file.exists(normalized_source)) {
    return(
      tibble::tibble(
        Path = character(),
        File = character(),
        Rows = integer(),
        SizeBytes = numeric(),
        SizeMB = numeric(),
        ModifiedRaw = as.POSIXct(character()),
        Modified = character()
      )
    )
  }

  info <- file.info(normalized_source)
  out <- data.table::data.table(
    Path = normalized_source,
    File = basename(normalized_source),
    Rows = safe_count_rows(normalized_source),
    SizeBytes = info$size,
    SizeMB = round(info$size / (1024^2), 2),
    ModifiedRaw = info$mtime,
    Modified = format(info$mtime, "%Y-%m-%d %H:%M:%S")
  )

  data.table::setorderv(
    out,
    cols = c("ModifiedRaw", "SizeBytes", "File"),
    order = c(-1L, -1L, 1L),
    na.last = TRUE
  )

  tibble::as_tibble(out)
}

pick_active_bundled_file <- function(
    source_file = configured_bundled_source_file()
) {
  files <- list_bundled_data_files(source_file)

  if (!nrow(files)) {
    return(list(path = NULL, label = NULL, file_count = 0L))
  }

  chosen <- files[1, , drop = FALSE]

  list(
    path = chosen$Path[[1]],
    label = chosen$File[[1]],
    file_count = nrow(files)
  )
}

load_bundled_data_file <- function(
    file_path = NULL,
    source_file = configured_bundled_source_file(),
    compute_summaries = FALSE
) {
  active_file <- if (is.null(file_path) || !nzchar(file_path)) {
    pick_active_bundled_file(source_file)
  } else {
    list(
      path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
      label = basename(file_path),
      file_count = NA_integer_
    )
  }

  if (is.null(active_file$path) || !nzchar(active_file$path) || !file.exists(active_file$path)) {
    warning(
      "⚠️ bundled_loader: No configured CSV file was found at ",
      normalizePath(source_file, winslash = "/", mustWork = FALSE),
      "."
    )
    empty <- tibble::tibble()
    return(list(raw = empty, pitching = empty, hitting = empty))
  }

  message("📄 Loading bundled data file: ", active_file$path)

  df <- data.table::fread(
    active_file$path,
    colClasses = "character",
    showProgress = FALSE,
    fill = Inf
  )

  df[, SourceFile := active_file$label]
  combined_df <- as.data.frame(df)

  message(
    "✅ bundled_loader: raw size = ",
    nrow(combined_df), " rows × ", ncol(combined_df), " columns"
  )

  processed <- process_data(combined_df)

  if (!"SourceFile" %in% names(processed)) {
    if (nrow(processed) == nrow(combined_df)) {
      processed$SourceFile <- combined_df$SourceFile
    } else {
      warning(
        "⚠️ process_data() changed row count (combined_df=",
        nrow(combined_df), ", processed=", nrow(processed),
        "). Cannot safely restore SourceFile by row position."
      )
    }
  }

  if (isTRUE(compute_summaries)) {
    team_pitching <- get_team_pitching(processed)
    team_hitting <- get_team_hitting(processed)

    pitching <- calculate_pitcher_stats(team_pitching)
    hitting <- calculate_hitter_stats(team_hitting)
  } else {
    pitching <- NULL
    hitting <- NULL
  }

  list(raw = processed, pitching = pitching, hitting = hitting)
}
