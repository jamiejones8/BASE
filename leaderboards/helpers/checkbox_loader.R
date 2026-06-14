# =========================================================
#  BREWSTER WHITECAPS — bundled single-file loader
#  - Loads one active CSV from /data
#  - Uses data.table::fread() for speed
# =========================================================

library(dplyr)
library(data.table)

source("helpers/process_data.R", local = TRUE)
source("helpers/calculate_pitcher_stats.R", local = TRUE)
source("helpers/calculate_hitter_stats.R", local = TRUE)

SUPPORTED_BUNDLED_EXTENSIONS <- function() {
  c("csv")
}

PREFERRED_BUNDLED_FILE_NAME <- function() {
  "data.csv"
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
    data_dir = get0("WHITE_CAPS_DATA_DIR", inherits = TRUE, ifnotfound = "data")
) {
  all_files <- list.files(
    data_dir,
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE
  )

  files <- all_files[vapply(all_files, is_supported_bundled_file, logical(1))]

  if (!length(files)) {
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

  info <- file.info(files)
  out <- data.table::data.table(
    Path = normalizePath(files, winslash = "/", mustWork = FALSE),
    File = basename(files),
    Rows = vapply(files, safe_count_rows, integer(1)),
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
    data_dir = get0("WHITE_CAPS_DATA_DIR", inherits = TRUE, ifnotfound = "data")
) {
  files <- list_bundled_data_files(data_dir)

  if (!nrow(files)) {
    return(list(path = NULL, label = NULL, file_count = 0L))
  }

  preferred_name <- PREFERRED_BUNDLED_FILE_NAME()
  preferred_files <- files[files$File == preferred_name, , drop = FALSE]

  if (nrow(preferred_files) > 0) {
    chosen <- preferred_files[1, , drop = FALSE]
  } else {
    chosen <- files[1, , drop = FALSE]
    warning(
      "⚠️ Expected bundled data file '", preferred_name, "' was not found in /data. ",
      "Using ", chosen$File[[1]], " instead."
    )
  }

  if (nrow(files) > 1) {
    if (chosen$File[[1]] == preferred_name) {
      warning(
        "⚠️ Multiple CSV files were found in /data. ",
        "The app is designed for one active file and will use the preferred file: ",
        chosen$File[[1]]
      )
    } else {
      warning(
        "⚠️ Multiple CSV files were found in /data. ",
        "The app is designed for one active file and will use: ",
        chosen$File[[1]]
      )
    }
  }

  list(
    path = chosen$Path[[1]],
    label = chosen$File[[1]],
    file_count = nrow(files)
  )
}

load_bundled_data_file <- function(
    file_path = NULL,
    data_dir = get0("WHITE_CAPS_DATA_DIR", inherits = TRUE, ifnotfound = "data"),
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
    warning("⚠️ bundled_loader: No active CSV file found in /data.")
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
