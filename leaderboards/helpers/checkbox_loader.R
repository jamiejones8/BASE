# =========================================================
#  BASE — team leaderboard data loader
#  - Loads the configured college-season source from the repo root
#  - Falls back to CSV parsing helpers only when a CSV source is passed directly
# =========================================================

library(dplyr)
library(data.table)

source("helpers/process_data.R", local = TRUE)
source("helpers/calculate_pitcher_stats.R", local = TRUE)
source("helpers/calculate_hitter_stats.R", local = TRUE)

SUPPORTED_BUNDLED_EXTENSIONS <- function() {
  c("parquet", "csv")
}

is_supported_bundled_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  nzchar(ext) && ext %in% SUPPORTED_BUNDLED_EXTENSIONS()
}

safe_count_rows <- function(path) {
  if (!file.exists(path)) {
    return(NA_integer_)
  }

  if (is_git_lfs_pointer_file(path)) {
    warning("Unable to count rows for ", basename(path), ": file is a Git LFS pointer.")
    return(NA_integer_)
  }

  ext <- tolower(tools::file_ext(path))

  if (identical(ext, "parquet")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      warning("Unable to count rows for ", basename(path), ": the 'arrow' package is not installed.")
      return(NA_integer_)
    }

    return(
      tryCatch(
        nrow(as.data.frame(arrow::read_parquet(path))),
        error = function(e) {
          warning("Unable to count rows for ", basename(path), ": ", e$message)
          NA_integer_
        }
      )
    )
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
      warning("Unable to count rows for ", basename(path), ": ", e$message)
      NA_integer_
    }
  )
}

leaderboards_project_dir <- function() {
  app_dir <- get0(
    "TEAM_ANALYTICS_APP_DIR",
    inherits = TRUE,
    ifnotfound = normalizePath(".", winslash = "/", mustWork = TRUE)
  )

  normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)
}

leaderboards_source_path <- function() {
  source_name <- get0(
    "TEAM_CONFIG",
    inherits = TRUE,
    ifnotfound = list(data = list(season_file = "data/season.csv"))
  )$data$season_file
  normalizePath(
    file.path(leaderboards_project_dir(), source_name),
    winslash = "/",
    mustWork = FALSE
  )
}

is_git_lfs_pointer_file <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  header <- tryCatch(
    readLines(path, n = 1L, warn = FALSE),
    error = function(e) character()
  )

  isTRUE(length(header) > 0) &&
    identical(header[[1]], "version https://git-lfs.github.com/spec/v1")
}

list_bundled_data_files <- function(
    data_dir = get0("TEAM_ANALYTICS_DATA_DIR", inherits = TRUE, ifnotfound = "data")
) {
  files <- leaderboards_source_path()
  files <- files[file.exists(files) & vapply(files, is_supported_bundled_file, logical(1))]

  if (!length(files)) {
    return(
      tibble::tibble(
        Path = character(),
        File = character(),
        Rows = integer(),
        SizeBytes = numeric(),
        SizeMB = numeric(),
        IsLfsPointer = logical(),
        ModifiedRaw = as.POSIXct(character()),
        Modified = character()
      )
    )
  }

  info <- file.info(files)
  out <- data.table::data.table(
    Path = normalizePath(files, winslash = "/", mustWork = FALSE),
    File = basename(files),
    Rows = vapply(files, safe_count_rows, integer(1)),
    SizeBytes = info$size,
    SizeMB = round(info$size / (1024^2), 2),
    IsLfsPointer = vapply(files, is_git_lfs_pointer_file, logical(1)),
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
    data_dir = get0("TEAM_ANALYTICS_DATA_DIR", inherits = TRUE, ifnotfound = "data")
) {
  files <- list_bundled_data_files(data_dir)

  if (!nrow(files)) {
    return(list(path = NULL, label = NULL, file_count = 0L))
  }

  chosen <- files[1, , drop = FALSE]

  if (nrow(files) > 1) {
    warning(
      "Multiple leaderboard source files were found. ",
      "The app will use the most recently updated file: ",
      chosen$File[[1]]
    )
  }

  list(
    path = chosen$Path[[1]],
    label = chosen$File[[1]],
    file_count = nrow(files),
    is_lfs_pointer = isTRUE(chosen$IsLfsPointer[[1]])
  )
}

load_bundled_data_file <- function(
    file_path = NULL,
    data_dir = get0("TEAM_ANALYTICS_DATA_DIR", inherits = TRUE, ifnotfound = "data"),
    compute_summaries = FALSE
) {
  active_file <- if (is.null(file_path) || !nzchar(file_path)) {
    pick_active_bundled_file(data_dir)
  } else {
    list(
      path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
      label = basename(file_path),
      file_count = NA_integer_
    )
  }

  if (is.null(active_file$path) || !nzchar(active_file$path) || !file.exists(active_file$path)) {
    warning("bundled_loader: No leaderboard data source file was found.")
    empty <- tibble::tibble()
    return(list(raw = empty, pitching = empty, hitting = empty))
  }

  if (is_git_lfs_pointer_file(active_file$path)) {
    stop(
      "Leaderboard data source '", active_file$label,
      "' is still a Git LFS pointer. Pull the real file before loading leaderboards."
    )
  }

  message("Loading leaderboard data file: ", active_file$path)

  ext <- tolower(tools::file_ext(active_file$path))
  if (identical(ext, "parquet")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "Reading .parquet requires the 'arrow' package. ",
        "Install it with install.packages('arrow')."
      )
    }

    combined_df <- as.data.frame(arrow::read_parquet(active_file$path))
  } else {
    df <- data.table::fread(
      active_file$path,
      colClasses = "character",
      showProgress = FALSE,
      fill = Inf
    )

    combined_df <- as.data.frame(df)
  }

  combined_df$SourceFile <- rep(active_file$label, nrow(combined_df))

  message(
    "bundled_loader: raw size = ",
    nrow(combined_df), " rows x ", ncol(combined_df), " columns"
  )

  processed <- process_data(combined_df)

  if (!"SourceFile" %in% names(processed)) {
    if (nrow(processed) == nrow(combined_df)) {
      processed$SourceFile <- combined_df$SourceFile
    } else {
      warning(
        "process_data() changed row count (combined_df=",
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
