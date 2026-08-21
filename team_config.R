# Central team and deployment configuration for BASE.
#
# Every setting can be overridden with an environment variable. This keeps the
# application image reusable: a new team only needs different deployment
# settings and assets, not edits throughout the Shiny application.

base_load_env_file <- function(path = c(".env", file.path("..", ".env"))) {
  existing <- path[file.exists(path)]
  if (!length(existing)) return(invisible(FALSE))
  path <- existing[[1]]
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#") & grepl("=", lines, fixed = TRUE)]
  for (line in lines) {
    split_at <- regexpr("=", line, fixed = TRUE)[1]
    key <- trimws(substr(line, 1, split_at - 1))
    value <- trimws(substr(line, split_at + 1, nchar(line)))
    if (nzchar(key) && !nzchar(Sys.getenv(key, unset = ""))) {
      do.call(Sys.setenv, stats::setNames(list(value), key))
    }
  }
  invisible(TRUE)
}

base_load_env_file()

base_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(trimws(value))) trimws(value) else default
}

base_env_int <- function(name, default = NA_integer_) {
  value <- suppressWarnings(as.integer(base_env(name, "")))
  if (is.na(value)) default else value
}

base_default_season_file <- function() {
  team_code <- base_env("BASE_TEAM_DATA_CODE", "TEX_BOB")
  derived_team_file <- file.path(
    base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"),
    "teams", paste0(team_code, ".parquet")
  )
  bundled_file <- "College26.parquet"
  mounted_file <- "/base-data/College26.runtime.parquet"
  if (file.exists(derived_team_file)) {
    derived_team_file
  } else if (file.exists(bundled_file)) {
    bundled_file
  } else if (file.exists(mounted_file)) {
    mounted_file
  } else {
    bundled_file
  }
}

base_env_bool <- function(name, default = FALSE) {
  value <- tolower(base_env(name, if (default) "true" else "false"))
  value %in% c("1", "true", "yes", "on")
}

base_asset_url <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return("")
  if (grepl("^(https?:)?//|^data:", path)) return(path)
  sub("^www[/\\\\]", "", path)
}

base_asset_file <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) ||
      grepl("^(https?:)?//|^data:", path)) return(NA_character_)
  if (file.exists(path)) return(path)
  candidate <- file.path("www", sub("^www[/\\\\]", "", path))
  if (file.exists(candidate)) candidate else NA_character_
}

TEAM_CONFIG <- list(
  city = base_env("BASE_TEAM_CITY", "San Marcos"),
  name = base_env("BASE_TEAM_NAME", "Bobcats"),
  full_name = base_env("BASE_TEAM_FULL_NAME", "Texas State Bobcats"),
  organization = base_env("BASE_ORGANIZATION", "Texas State Baseball"),
  abbreviation = base_env("BASE_TEAM_ABBR", "TXST"),
  data_code = base_env("BASE_TEAM_DATA_CODE", "TEX_BOB"),
  # Regular expression matched against PitcherTeam/BatterTeam values.
  data_pattern = base_env("BASE_TEAM_DATA_PATTERN", "TEX_BOB|Texas State|Texas St\\.|TXST"),
  # Team report selectors are scoped to Texas State. College scouting pages
  # continue to expose every college team present in the college data source.
  team_scope_only = base_env_bool("BASE_TEAM_SCOPE_ONLY", TRUE),
  mlb_team_id = base_env_int("BASE_MLB_TEAM_ID", NA_integer_),
  stats_api_enabled = base_env_bool("BASE_STATS_API_ENABLED", FALSE),
  league_id = base_env_int("BASE_LEAGUE_ID", NA_integer_),
  sport_id = base_env_int("BASE_SPORT_ID", NA_integer_),
  league_name = base_env("BASE_LEAGUE_NAME", "Pac-12 Conference"),
  season = base_env_int("BASE_SEASON", 2026L),
  season_label = base_env(
    "BASE_SEASON_LABEL",
    paste(base_env_int("BASE_SEASON", 2026L), "College Season")
  ),
  roster_label = base_env("BASE_ROSTER_LABEL", "2026 Texas State roster"),
  competition_level = base_env("BASE_COMPETITION_LEVEL", "NCAA Division I"),
  schedule_timezone = base_env("BASE_SCHEDULE_TIMEZONE", "America/Chicago"),
  colors = list(
    primary = base_env("BASE_PRIMARY_COLOR", "#501214"),
    secondary = base_env("BASE_SECONDARY_COLOR", "#FFFFFF"),
    accent = base_env("BASE_ACCENT_COLOR", "#D7BD8A"),
    background = base_env("BASE_BACKGROUND_COLOR", "#F7F3EC")
  ),
  assets = list(
    logo = base_env("BASE_TEAM_LOGO", "TXST_SuperCat.webp"),
    primary_logo = base_env("BASE_PRIMARY_LOGO", "TXST_Primary.jpg"),
    secondary_logo = base_env("BASE_SECONDARY_LOGO", "TXST_Secondary.png"),
    supercat_logo = base_env("BASE_SUPERCAT_LOGO", "TXST_SuperCat.webp"),
    conference_logo = base_env("BASE_CONFERENCE_LOGO", "pac12logo.webp"),
    scoreboard_logo = base_env("BASE_SCOREBOARD_LOGO", "TXST_Secondary.png"),
    hub_image = base_env("BASE_ANALYTICS_HUB_IMAGE", "TXST_Primary.jpg"),
    leaderboards_logo = base_env("BASE_LEADERBOARDS_LOGO", "TXST_SuperCat.webp"),
    report_logo_url = base_env(
      "BASE_TEAM_REPORT_LOGO_URL",
      "https://www.ncaa.com/sites/default/files/images/logos/schools/bgd/texas-st.svg"
    )
  ),
  data = list(
    season_file = base_env("BASE_SEASON_DATA_FILE", base_default_season_file()),
    runtime_root = base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"),
    pitcher_dataset_dir = base_env(
      "BASE_PITCHER_DATASET_DIR",
      file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "pitchers")
    ),
    pitcher_catalog_file = base_env(
      "BASE_PITCHER_CATALOG_FILE",
      file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "catalogs", "pitchers.parquet")
    ),
    hitter_catalog_file = base_env(
      "BASE_HITTER_CATALOG_FILE",
      file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "catalogs", "hitters.parquet")
    ),
    defense_runtime_root = base_env(
      "BASE_DEFENSE_RUNTIME_ROOT",
      file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
    ),
    defense_events_dir = base_env(
      "BASE_DEFENSE_EVENTS_DIR",
      file.path(
        base_env(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "events"
      )
    ),
    defense_team_catalog_file = base_env(
      "BASE_DEFENSE_TEAM_CATALOG_FILE",
      file.path(
        base_env(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "teams.parquet"
      )
    ),
    defense_player_catalog_file = base_env(
      "BASE_DEFENSE_PLAYER_CATALOG_FILE",
      file.path(
        base_env(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "players.parquet"
      )
    ),
    defense_batter_catalog_file = base_env(
      "BASE_DEFENSE_BATTER_CATALOG_FILE",
      file.path(
        base_env(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "batters.parquet"
      )
    ),
    hitter_catalog_cache_file = base_env(
      "BASE_HITTER_CATALOG_CACHE_FILE",
      file.path(
        if (dir.exists("/base-data")) "/base-data/app_state" else "app_state",
        "hitter-catalog.rds"
      )
    ),
    retag_db_file = base_env(
      "BASE_RETAG_DB_FILE",
      file.path(
        if (dir.exists("/base-data")) "/base-data/app_state" else "app_state",
        "pitch-retags.sqlite"
      )
    ),
    hf_repo_id = base_env("BASE_DATA_REPO_ID", ""),
    hf_repo_path = base_env("BASE_DATA_REPO_PATH", base_env("BASE_SEASON_DATA_FILE", "College26.parquet")),
    swing_model_repo = base_env("BASE_SWING_MODEL_REPO", ""),
    college_file = base_env("BASE_COLLEGE_DATA_FILE", base_default_season_file()),
    college_repo_id = base_env(
      "BASE_COLLEGE_DATA_REPO_ID",
      base_env("BASE_DATA_REPO_ID", "")
    ),
    college_repo_path = base_env("BASE_COLLEGE_DATA_REPO_PATH", base_env("BASE_COLLEGE_DATA_FILE", "College26.parquet")),
    cape_file = base_env("BASE_CAPE_DATA_FILE", "CapeCod26.parquet"),
    heights_file = base_env("BASE_PLAYER_HEIGHTS_FILE", "College26Heights.csv"),
    roster_file = base_env("BASE_ROSTER_FILE", "config/texas_state_roster_2026_reference.csv"),
    schedule_file = base_env("BASE_SCHEDULE_FILE", "config/texas_state_schedule_2026.csv")
  )
)

base_team_matches <- function(x) {
  x <- as.character(x)
  pattern <- TEAM_CONFIG$data_pattern
  !is.na(x) & nzchar(pattern) & grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

base_team_default <- function(values) {
  values <- as.character(values)
  match <- values[base_team_matches(values)]
  if (length(match)) return(match[[1]])
  if (isTRUE(TEAM_CONFIG$team_scope_only)) return(character(0))
  if (length(values)) values[[1]] else character(0)
}

base_team_choices <- function(values) {
  values <- sort(unique(as.character(values)))
  values <- values[!is.na(values) & nzchar(values)]
  if (!isTRUE(TEAM_CONFIG$team_scope_only)) return(values)
  values[base_team_matches(values)]
}

base_brand_footer <- function() {
  paste0(TEAM_CONFIG$full_name, " Analytics · ", format(Sys.Date(), "%Y"))
}

base_team_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$logo)
base_scoreboard_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$scoreboard_logo)
base_team_logo_file <- function() base_asset_file(TEAM_CONFIG$assets$logo)
base_primary_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$primary_logo)
base_secondary_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$secondary_logo)
base_supercat_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$supercat_logo)
base_conference_logo_url <- function() base_asset_url(TEAM_CONFIG$assets$conference_logo)

base_player_key <- function(x) {
  x <- trimws(tolower(as.character(x)))
  comma_name <- !is.na(x) & grepl(",", x, fixed = TRUE)
  if (any(comma_name)) {
    x[comma_name] <- vapply(strsplit(x[comma_name], ",", fixed = TRUE), function(parts) {
      parts <- trimws(parts)
      paste(c(parts[-1], parts[1]), collapse = " ")
    }, character(1))
  }
  gsub("[^a-z0-9]", "", x)
}

base_player_supplement_rows <- function(primary, supplemental, player, role = "Pitcher") {
  if (is.null(supplemental)) return(data.frame())
  if (is.null(primary) || !nrow(primary) || !nrow(supplemental) ||
      !role %in% names(primary) || !role %in% names(supplemental)) {
    return(supplemental[0, , drop = FALSE])
  }

  id_column <- paste0(role, "Id")
  if (id_column %in% names(primary) && id_column %in% names(supplemental)) {
    player_ids <- unique(as.character(primary[[id_column]]))
    player_ids <- player_ids[!is.na(player_ids) & nzchar(player_ids)]
    if (length(player_ids)) {
      by_id <- supplemental[
        !is.na(supplemental[[id_column]]) &
          as.character(supplemental[[id_column]]) %in% player_ids,
        ,
        drop = FALSE
      ]
      if (nrow(by_id)) return(by_id)
    }
  }

  supplemental[
    base_player_key(supplemental[[role]]) %in% base_player_key(player),
    ,
    drop = FALSE
  ]
}

base_brand_css <- function(include_leaderboards = TRUE) {
  css <- paste0(
    ":root{",
    "--base-maroon:", TEAM_CONFIG$colors$primary, ";",
    "--base-maroon-deep:color-mix(in srgb, ", TEAM_CONFIG$colors$primary, " 72%, black);",
    "--base-maroon-soft:color-mix(in srgb, ", TEAM_CONFIG$colors$primary, " 78%, white);",
    "--base-gold:", TEAM_CONFIG$colors$accent, ";",
    "--base-gold-bright:", TEAM_CONFIG$colors$accent, ";",
    "--base-canvas:", TEAM_CONFIG$colors$background, ";",
    "--base-surface:", TEAM_CONFIG$colors$secondary, ";",
    "--navy:", TEAM_CONFIG$colors$primary, ";",
    "--navy-mid:", TEAM_CONFIG$colors$primary, ";",
    "--navy-light:", TEAM_CONFIG$colors$accent, ";",
    "--teal:", TEAM_CONFIG$colors$accent, ";",
    "--teal-light:", TEAM_CONFIG$colors$accent, ";",
    "--white:", TEAM_CONFIG$colors$secondary, ";",
    "--off-white:", TEAM_CONFIG$colors$background, ";",
    "--text-main:var(--base-ink);",
    "}"
  )
  if (include_leaderboards) {
    css <- paste0(
      css,
      ".leaderboards-shell{",
      "--base-navy:", TEAM_CONFIG$colors$primary, ";",
      "--base-navy-mid:", TEAM_CONFIG$colors$primary, ";",
      "--base-teal:", TEAM_CONFIG$colors$accent, ";",
      "--base-text:", TEAM_CONFIG$colors$primary, ";",
      "}",
      ":root{--brand-navy:", TEAM_CONFIG$colors$primary,
      ";--brand-navy-deep:", TEAM_CONFIG$colors$primary,
      ";--brand-teal:", TEAM_CONFIG$colors$accent,
      ";--brand-teal-deep:", TEAM_CONFIG$colors$primary,
      ";--brand-ice:", TEAM_CONFIG$colors$background, ";}"
    )
  }
  css
}
