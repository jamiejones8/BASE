# Query-on-demand access to the partitioned 2026 college runtime dataset.

BASE_RUNTIME_ROOT <- TEAM_CONFIG$data$runtime_root
BASE_PITCHER_DATASET_DIR <- TEAM_CONFIG$data$pitcher_dataset_dir
BASE_PITCHER_CATALOG_FILE <- TEAM_CONFIG$data$pitcher_catalog_file

base_pitcher_catalog <- tryCatch({
  if (!file.exists(BASE_PITCHER_CATALOG_FILE)) {
    stop("pitcher catalog is not mounted at ", BASE_PITCHER_CATALOG_FILE)
  }
  arrow::read_parquet(BASE_PITCHER_CATALOG_FILE) %>%
    tibble::as_tibble() %>%
    mutate(
      PitcherTeam = as.character(PitcherTeam),
      Pitcher = as.character(Pitcher),
      Bucket = as.integer(Bucket),
      PitchCount = as.numeric(PitchCount)
    )
}, error = function(e) {
  message("BASE pitcher catalog unavailable: ", e$message)
  tibble::tibble(
    PitcherTeam = character(), Pitcher = character(),
    Bucket = integer(), PitchCount = numeric()
  )
})

base_pitcher_dataset <- tryCatch({
  if (!dir.exists(BASE_PITCHER_DATASET_DIR)) {
    stop("pitcher dataset is not mounted at ", BASE_PITCHER_DATASET_DIR)
  }
  arrow::open_dataset(
    BASE_PITCHER_DATASET_DIR,
    format = "parquet",
    partitioning = arrow::hive_partition()
  )
}, error = function(e) {
  message("BASE pitcher dataset unavailable: ", e$message)
  NULL
})

.base_pitcher_cache <- new.env(parent = emptyenv())
.base_pitcher_cache_order <- character()
.base_pitcher_cache_limit <- 16L

base_pitcher_cache_put <- function(key, value) {
  assign(key, value, envir = .base_pitcher_cache)
  .base_pitcher_cache_order <<- c(setdiff(.base_pitcher_cache_order, key), key)
  while (length(.base_pitcher_cache_order) > .base_pitcher_cache_limit) {
    evict <- .base_pitcher_cache_order[[1]]
    rm(list = evict, envir = .base_pitcher_cache)
    .base_pitcher_cache_order <<- .base_pitcher_cache_order[-1]
  }
  value
}

base_load_pitcher_rows <- function(team, pitcher) {
  team <- as.character(team)[[1]]
  pitcher <- as.character(pitcher)[[1]]
  key <- paste(team, pitcher, sep = "\u001f")
  if (exists(key, envir = .base_pitcher_cache, inherits = FALSE)) {
    return(get(key, envir = .base_pitcher_cache, inherits = FALSE))
  }
  if (is.null(base_pitcher_dataset)) return(tibble::tibble())

  catalog_row <- base_pitcher_catalog %>%
    filter(PitcherTeam == team, Pitcher == pitcher) %>%
    slice_head(n = 1)
  if (!nrow(catalog_row)) return(tibble::tibble())
  bucket_value <- as.integer(catalog_row$Bucket[[1]])

  rows <- base_pitcher_dataset %>%
    filter(
      Bucket == bucket_value,
      PitcherTeam == team,
      Pitcher == pitcher
    ) %>%
    collect() %>%
    tibble::as_tibble()
  if ("Notes" %in% names(rows)) rows$Notes <- as.character(rows$Notes)
  rows$DataSource <- "2026 College Season"
  message(
    "Loaded player on demand: ", pitcher, " (", team, ") — ",
    format(nrow(rows), big.mark = ","), " pitches from bucket ", bucket_value
  )
  base_pitcher_cache_put(key, rows)
}

# Hitter scouting reuses the same self-contained pitch partitions. A compact
# catalog records which pitcher hash buckets contain each hitter, allowing
# player queries to prune unrelated partitions without duplicating the master.
BASE_HITTER_CATALOG_CACHE_FILE <- TEAM_CONFIG$data$hitter_catalog_cache_file
BASE_HITTER_CATALOG_FILE <- TEAM_CONFIG$data$hitter_catalog_file

.base_hitter_catalog_cache <- NULL
.base_hitter_cache <- new.env(parent = emptyenv())
.base_hitter_cache_order <- character()
.base_hitter_cache_limit <- 24L

base_hitter_cache_put <- function(key, value) {
  assign(key, value, envir = .base_hitter_cache)
  .base_hitter_cache_order <<- c(setdiff(.base_hitter_cache_order, key), key)
  while (length(.base_hitter_cache_order) > .base_hitter_cache_limit) {
    evict <- .base_hitter_cache_order[[1]]
    rm(list = evict, envir = .base_hitter_cache)
    .base_hitter_cache_order <<- .base_hitter_cache_order[-1]
  }
  value
}

base_empty_hitter_catalog <- function() {
  tibble::tibble(
    BatterTeam = character(),
    Batter = character(),
    BatterSide = character(),
    Buckets = character(),
    PitchCount = numeric()
  )
}

base_validate_hitter_catalog <- function(catalog) {
  required <- c("BatterTeam", "Batter", "BatterSide", "Buckets", "PitchCount")
  if (is.null(catalog) || !all(required %in% names(catalog))) {
    return(base_empty_hitter_catalog())
  }
  catalog %>%
    tibble::as_tibble() %>%
    transmute(
      BatterTeam = as.character(BatterTeam),
      Batter = as.character(Batter),
      BatterSide = as.character(BatterSide),
      Buckets = as.character(Buckets),
      PitchCount = suppressWarnings(as.numeric(PitchCount))
    ) %>%
    filter(
      !is.na(BatterTeam), nzchar(BatterTeam),
      !is.na(Batter), nzchar(Batter)
    ) %>%
    distinct(BatterTeam, Batter, .keep_all = TRUE)
}

base_build_hitter_catalog <- function() {
  if (is.null(base_pitcher_dataset) || !dir.exists(BASE_PITCHER_DATASET_DIR)) {
    return(base_empty_hitter_catalog())
  }

  files <- list.files(
    BASE_PITCHER_DATASET_DIR,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(files)) return(base_empty_hitter_catalog())

  message("Building hitter catalog from ", length(files), " local pitch partitions...")
  pieces <- lapply(seq_along(files), function(i) {
    file <- files[[i]]
    bucket_match <- regmatches(file, regexpr("Bucket=[0-9]+", file))
    bucket <- suppressWarnings(as.integer(sub("Bucket=", "", bucket_match, fixed = TRUE)))
    if (!length(bucket) || is.na(bucket)) return(NULL)

    tryCatch({
      arrow::read_parquet(
        file,
        col_select = c("BatterTeam", "Batter", "BatterSide")
      ) %>%
        tibble::as_tibble() %>%
        transmute(
          BatterTeam = as.character(BatterTeam),
          Batter = as.character(Batter),
          BatterSide = as.character(BatterSide),
          Bucket = bucket
        ) %>%
        filter(
          !is.na(BatterTeam), nzchar(BatterTeam),
          !is.na(Batter), nzchar(Batter)
        ) %>%
        count(BatterTeam, Batter, BatterSide, Bucket, name = "PitchCount")
    }, error = function(e) {
      message("Hitter catalog skipped ", basename(file), ": ", conditionMessage(e))
      NULL
    })
  })

  counts <- bind_rows(pieces)
  if (!nrow(counts)) return(base_empty_hitter_catalog())

  side_lookup <- counts %>%
    group_by(BatterTeam, Batter, BatterSide) %>%
    summarise(SideCount = sum(PitchCount), .groups = "drop") %>%
    arrange(BatterTeam, Batter, desc(SideCount), BatterSide) %>%
    group_by(BatterTeam, Batter) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(BatterTeam, Batter, BatterSide)

  catalog <- counts %>%
    group_by(BatterTeam, Batter) %>%
    summarise(
      Buckets = paste(sort(unique(Bucket)), collapse = ","),
      PitchCount = sum(PitchCount),
      .groups = "drop"
    ) %>%
    left_join(side_lookup, by = c("BatterTeam", "Batter")) %>%
    select(BatterTeam, Batter, BatterSide, Buckets, PitchCount) %>%
    arrange(BatterTeam, Batter)

  cache_dir <- dirname(BASE_HITTER_CATALOG_CACHE_FILE)
  tryCatch({
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(catalog, BASE_HITTER_CATALOG_CACHE_FILE)
    message(
      "Saved hitter catalog at ", BASE_HITTER_CATALOG_CACHE_FILE,
      " — ", format(nrow(catalog), big.mark = ","), " hitters"
    )
  }, error = function(e) {
    message("Hitter catalog cache could not be saved: ", conditionMessage(e))
  })

  catalog
}

base_get_hitter_catalog <- function(refresh = FALSE) {
  if (!refresh && !is.null(.base_hitter_catalog_cache)) {
    return(.base_hitter_catalog_cache)
  }

  catalog <- NULL
  if (!refresh && file.exists(BASE_HITTER_CATALOG_FILE)) {
    catalog <- tryCatch(
      arrow::read_parquet(BASE_HITTER_CATALOG_FILE) %>% tibble::as_tibble(),
      error = function(e) {
        message("Derived hitter catalog could not be read: ", conditionMessage(e))
        NULL
      }
    )
  }
  if (!refresh && file.exists(BASE_HITTER_CATALOG_CACHE_FILE)) {
    catalog <- catalog %||% tryCatch(
      readRDS(BASE_HITTER_CATALOG_CACHE_FILE),
      error = function(e) {
        message("Hitter catalog cache could not be read: ", conditionMessage(e))
        NULL
      }
    )
  }
  if (is.null(catalog)) catalog <- base_build_hitter_catalog()

  .base_hitter_catalog_cache <<- base_validate_hitter_catalog(catalog)
  .base_hitter_catalog_cache
}

base_hitter_runtime_columns <- c(
  "PitchUID", "GameID", "GameUID", "Date", "LocalDateTime", "UTCDateTime",
  "Pitcher", "PitcherId", "PitcherTeam", "PitcherThrows",
  "Batter", "BatterId", "BatterTeam", "BatterSide",
  "TaggedPitchType", "AutoPitchType", "PitchCall", "KorBB",
  "TaggedHitType", "PlayResult", "Notes", "Top/Bottom",
  "RelSpeed", "SpinRate", "SpinAxis", "RelHeight", "RelSide", "Extension",
  "InducedVertBreak", "HorzBreak", "PlateLocHeight", "PlateLocSide",
  "ExitSpeed", "Angle", "Direction", "Bearing", "Distance",
  "Inning", "PAofInning", "PitchofPA", "Balls", "Strikes",
  "OutsOnPlay", "RunsScored", "StuffPlus", "LocationPlus",
  "PitchingPlus", "xRV", "xwOBA"
)

base_load_hitter_rows <- function(team, hitter) {
  team <- as.character(team)[[1]]
  hitter <- as.character(hitter)[[1]]
  key <- paste(team, hitter, sep = "\u001f")
  if (exists(key, envir = .base_hitter_cache, inherits = FALSE)) {
    return(get(key, envir = .base_hitter_cache, inherits = FALSE))
  }
  if (is.null(base_pitcher_dataset)) return(tibble::tibble())

  catalog_row <- base_get_hitter_catalog() %>%
    filter(BatterTeam == team, Batter == hitter) %>%
    slice_head(n = 1)
  if (!nrow(catalog_row)) return(tibble::tibble())

  bucket_values <- suppressWarnings(as.integer(strsplit(catalog_row$Buckets[[1]], ",", fixed = TRUE)[[1]]))
  bucket_values <- unique(bucket_values[!is.na(bucket_values)])
  available <- names(base_pitcher_dataset$schema)
  selected <- intersect(base_hitter_runtime_columns, available)

  query <- base_pitcher_dataset
  if (length(bucket_values)) query <- query %>% filter(Bucket %in% bucket_values)
  rows <- query %>%
    filter(BatterTeam == team, Batter == hitter) %>%
    select(tidyselect::all_of(selected)) %>%
    collect() %>%
    tibble::as_tibble()

  if ("Notes" %in% names(rows)) rows$Notes <- as.character(rows$Notes)
  rows$DataSource <- "2026 College Season"
  message(
    "Loaded hitter on demand: ", hitter, " (", team, ") — ",
    format(nrow(rows), big.mark = ","), " pitches across ",
    length(bucket_values), " candidate bucket(s)"
  )
  base_hitter_cache_put(key, rows)
}
