# Query-on-demand access to the joined 2026 defensive alignment runtime.

BASE_DEFENSE_EVENTS_DIR <- TEAM_CONFIG$data$defense_events_dir
BASE_DEFENSE_TEAM_CATALOG_FILE <- TEAM_CONFIG$data$defense_team_catalog_file
BASE_DEFENSE_PLAYER_CATALOG_FILE <- TEAM_CONFIG$data$defense_player_catalog_file
BASE_DEFENSE_BATTER_CATALOG_FILE <- TEAM_CONFIG$data$defense_batter_catalog_file

base_read_defense_catalog <- function(path, empty) {
  tryCatch({
    if (!file.exists(path)) stop("catalog is not mounted at ", path)
    arrow::read_parquet(path) %>% tibble::as_tibble()
  }, error = function(e) {
    message("BASE defensive catalog unavailable: ", conditionMessage(e))
    empty
  })
}

base_defense_team_catalog <- base_read_defense_catalog(
  BASE_DEFENSE_TEAM_CATALOG_FILE,
  tibble::tibble(
    FieldingTeam = character(), Bucket = integer(), Pitches = numeric(),
    Games = numeric(), BallsInPlay = numeric(), TrackedPitches = numeric(),
    MatchedPitches = numeric(), FirstDate = as.Date(character()),
    LastDate = as.Date(character())
  )
) %>%
  mutate(
    FieldingTeam = as.character(FieldingTeam),
    Bucket = as.integer(Bucket)
  )

base_defense_player_catalog <- base_read_defense_catalog(
  BASE_DEFENSE_PLAYER_CATALOG_FILE,
  tibble::tibble(
    FieldingTeam = character(), Bucket = integer(), Position = character(),
    PlayerId = character(), Player = character(), Pitches = numeric(),
    Games = numeric(), BallsInPlay = numeric(), MatchedPitches = numeric(),
    FirstDate = as.Date(character()), LastDate = as.Date(character())
  )
) %>%
  mutate(
    FieldingTeam = as.character(FieldingTeam), Bucket = as.integer(Bucket),
    Position = as.character(Position), PlayerId = as.character(PlayerId),
    Player = as.character(Player)
  )

base_defense_batter_catalog <- base_read_defense_catalog(
  BASE_DEFENSE_BATTER_CATALOG_FILE,
  tibble::tibble(
    FieldingTeam = character(), Bucket = integer(), BatterTeam = character(),
    BatterId = numeric(), Batter = character(), BatterSide = character(),
    Pitches = numeric(), BallsInPlay = numeric(), Games = numeric()
  )
) %>%
  mutate(
    FieldingTeam = as.character(FieldingTeam), Bucket = as.integer(Bucket),
    BatterTeam = as.character(BatterTeam), Batter = as.character(Batter),
    BatterSide = as.character(BatterSide)
  )

base_defense_dataset <- tryCatch({
  if (!dir.exists(BASE_DEFENSE_EVENTS_DIR)) {
    stop("defensive event dataset is not mounted at ", BASE_DEFENSE_EVENTS_DIR)
  }
  arrow::open_dataset(
    BASE_DEFENSE_EVENTS_DIR,
    format = "parquet",
    partitioning = arrow::hive_partition()
  )
}, error = function(e) {
  message("BASE defensive event dataset unavailable: ", conditionMessage(e))
  NULL
})

.base_defense_cache <- new.env(parent = emptyenv())
.base_defense_cache_order <- character()
.base_defense_cache_limit <- 8L

base_defense_cache_put <- function(key, value) {
  assign(key, value, envir = .base_defense_cache)
  .base_defense_cache_order <<- c(setdiff(.base_defense_cache_order, key), key)
  while (length(.base_defense_cache_order) > .base_defense_cache_limit) {
    evict <- .base_defense_cache_order[[1]]
    rm(list = evict, envir = .base_defense_cache)
    .base_defense_cache_order <<- .base_defense_cache_order[-1]
  }
  value
}

base_load_defense_team <- function(team) {
  team <- as.character(team)[[1]]
  if (!nzchar(team) || is.null(base_defense_dataset)) return(tibble::tibble())
  if (exists(team, envir = .base_defense_cache, inherits = FALSE)) {
    return(get(team, envir = .base_defense_cache, inherits = FALSE))
  }

  catalog_row <- base_defense_team_catalog %>%
    filter(FieldingTeam == team) %>%
    slice_head(n = 1)
  if (!nrow(catalog_row)) return(tibble::tibble())
  bucket_value <- as.integer(catalog_row$Bucket[[1]])

  rows <- base_defense_dataset %>%
    filter(Bucket == bucket_value, FieldingTeam == team) %>%
    collect() %>%
    tibble::as_tibble()

  if ("Date" %in% names(rows)) rows$Date <- as.Date(rows$Date)
  rows <- rows %>%
    mutate(
      IsBIP = PitchCall == "InPlay" | (!is.na(ExitSpeed) & is.finite(ExitSpeed)),
      IsOut = IsBIP & PlayResult == "Out",
      IsShift = !is.na(DetectedShift) & DetectedShift != "NoShift",
      HasContactLocation = IsBIP & is.finite(Distance) & is.finite(Bearing),
      LandingLateral = if_else(
        HasContactLocation,
        sin(Bearing * pi / 180) * Distance,
        NA_real_
      ),
      LandingDepth = if_else(
        HasContactLocation,
        cos(Bearing * pi / 180) * Distance,
        NA_real_
      )
    )
  base_defense_cache_put(team, rows)
}

base_defense_available <- function() {
  !is.null(base_defense_dataset) && nrow(base_defense_team_catalog) > 0
}
