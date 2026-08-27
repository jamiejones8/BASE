# Central team and deployment configuration for BASE.
#
# Every setting can be overridden with an environment variable. This keeps the
# application image reusable: a new team only needs different deployment
# settings and assets, not edits throughout the Shiny application.

base_current_source_file <- function() {
  frame_files <- vapply(sys.frames(), function(frame) {
    ofile <- frame$ofile
    if (is.null(ofile) || !nzchar(ofile)) return(NA_character_)
    normalizePath(ofile, winslash = "/", mustWork = FALSE)
  }, character(1))

  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files)) frame_files[[length(frame_files)]] else NA_character_
}

base_detect_project_root <- function() {
  source_file <- base_current_source_file()
  candidates <- unique(c(
    if (!is.na(source_file)) dirname(source_file) else NA_character_,
    if (!is.na(source_file)) dirname(dirname(dirname(source_file))) else NA_character_,
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  ))

  for (candidate in candidates) {
    if (is.na(candidate) || !dir.exists(candidate)) next
    if (
      file.exists(file.path(candidate, "README.md")) &&
      file.exists(file.path(candidate, "Dockerfile")) &&
      dir.exists(file.path(candidate, "leaderboards")) &&
      dir.exists(file.path(candidate, "www"))
    ) {
      return(candidate)
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

BASE_PROJECT_ROOT <- base_detect_project_root()

base_project_path <- function(...) {
  normalizePath(file.path(BASE_PROJECT_ROOT, ...), winslash = "/", mustWork = FALSE)
}

base_reference_path <- function(...) {
  base_project_path("data", "reference", ...)
}

base_model_path <- function(...) {
  base_project_path("models", ...)
}

base_www_path <- function(...) {
  base_project_path("www", ...)
}

base_source <- function(relative_path, local = FALSE, chdir = FALSE) {
  source(base_project_path(relative_path), local = local, chdir = chdir)
}

base_env_path <- function(name, default = "") {
  value <- base_env(name, default)
  if (!nzchar(value) || grepl("^(https?:)?//|^data:", value) || startsWith(value, "/")) {
    return(value)
  }
  base_project_path(value)
}

base_load_env_file <- function(path = base_project_path(".env")) {
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

base_is_lfs_pointer <- function(path) {
  if (!file.exists(path)) return(FALSE)
  first_line <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character())
  identical(first_line, "version https://git-lfs.github.com/spec/v1")
}

base_file_override_or_fallback <- function(name, mounted_path, bundled_path) {
  override <- Sys.getenv(name, unset = "")
  if (nzchar(trimws(override))) return(base_env_path(name))
  if (file.exists(mounted_path) && !base_is_lfs_pointer(mounted_path)) mounted_path else bundled_path
}

base_default_season_file <- function() {
  team_code <- base_env("BASE_TEAM_DATA_CODE", "TEX_BOB")
  derived_team_file <- file.path(
    base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"),
    "teams", paste0(team_code, ".parquet")
  )
  bundled_candidates <- c(
    base_project_path("data", "local", "College26.parquet"),
    base_project_path("College26.parquet"),
    base_project_path("data", "local", "texas_state_2027.csv")
  )
  mounted_file <- "/base-data/College26.runtime.parquet"
  if (file.exists(derived_team_file)) {
    derived_team_file
  } else if (any(file.exists(bundled_candidates))) {
    bundled_candidates[file.exists(bundled_candidates)][[1]]
  } else if (file.exists(mounted_file)) {
    mounted_file
  } else {
    bundled_candidates[[1]]
  }
}

base_default_ncaa_d1_master_file <- function() {
  candidates <- c(
    "/base-data/ncaa-d1/2026/master.parquet",
    "/base-data/College26.parquet",
    base_project_path("WallyApps", "D1 Files", "D1 Pitching:Hitting File.parquet"),
    base_project_path("data", "local", "College26.parquet"),
    base_project_path("College26.parquet")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing[[1]] else candidates[[1]]
}

base_default_ncaa_d1_defense_file <- function() {
  candidates <- c(
    "/base-data/ncaa-d1/2026/defense-alignment.parquet",
    base_project_path("WallyApps", "D1 Files", "D1 Defense File.parquet")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing[[1]] else candidates[[1]]
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
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  candidate <- base_project_path(path)
  if (file.exists(candidate)) return(candidate)
  candidate <- base_www_path(sub("^www[/\\\\]", "", path))
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
  roster_label = base_env("BASE_ROSTER_LABEL", "2027 Texas State roster"),
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
    source_contract_file = base_env_path(
      "BASE_DATA_SOURCE_CONTRACT_FILE",
      base_project_path("config", "baseball_data_sources.json")
    ),
    ncaa_d1_master_file = base_env_path(
      "BASE_NCAA_D1_MASTER_FILE",
      base_default_ncaa_d1_master_file()
    ),
    ncaa_d1_defense_file = base_env_path(
      "BASE_NCAA_D1_DEFENSE_FILE",
      base_default_ncaa_d1_defense_file()
    ),
    team_supplement_dataset_dir = base_env_path(
      "BASE_TEAM_SUPPLEMENT_DATASET_DIR",
      "/base-data/supplements/texas-state-pitches"
    ),
    season_file = base_env_path("BASE_SEASON_DATA_FILE", base_default_season_file()),
    runtime_root = base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"),
    pitcher_dataset_dir = base_env_path(
      "BASE_PITCHER_DATASET_DIR",
      file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "pitchers")
    ),
    pitcher_catalog_file = base_env_path(
      "BASE_PITCHER_CATALOG_FILE",
      file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "catalogs", "pitchers.parquet")
    ),
    hitter_catalog_file = base_env_path(
      "BASE_HITTER_CATALOG_FILE",
      file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "catalogs", "hitters.parquet")
    ),
    defense_runtime_root = base_env_path(
      "BASE_DEFENSE_RUNTIME_ROOT",
      file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
    ),
    defense_events_dir = base_env_path(
      "BASE_DEFENSE_EVENTS_DIR",
      file.path(
        base_env_path(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "events"
      )
    ),
    defense_team_catalog_file = base_env_path(
      "BASE_DEFENSE_TEAM_CATALOG_FILE",
      file.path(
        base_env_path(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "teams.parquet"
      )
    ),
    defense_player_catalog_file = base_env_path(
      "BASE_DEFENSE_PLAYER_CATALOG_FILE",
      file.path(
        base_env_path(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "players.parquet"
      )
    ),
    defense_batter_catalog_file = base_env_path(
      "BASE_DEFENSE_BATTER_CATALOG_FILE",
      file.path(
        base_env_path(
          "BASE_DEFENSE_RUNTIME_ROOT",
          file.path(base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"), "defense")
        ),
        "catalogs", "batters.parquet"
      )
    ),
    catcher_framing_reference_file = base_file_override_or_fallback(
      "BASE_CATCHER_FRAMING_REFERENCE_FILE",
      file.path(
        base_env_path("BASE_RUNTIME_ROOT", "/base-data/derived2026"),
        "reference", "d1_catcher_framing_metrics.csv"
      ),
      base_project_path(
        "WallyApps", "DefenseApp", "data", "d1_catcher_framing_metrics.csv"
      )
    ),
    hitter_catalog_cache_file = base_env_path(
      "BASE_HITTER_CATALOG_CACHE_FILE",
      file.path(
        if (dir.exists("/base-data")) "/base-data/app_state" else base_project_path("app_state"),
        "hitter-catalog.rds"
      )
    ),
    retag_db_file = base_env_path(
      "BASE_RETAG_DB_FILE",
      file.path(
        if (dir.exists("/base-data")) "/base-data/app_state" else base_project_path("app_state"),
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
    cape_file = base_file_override_or_fallback(
      "BASE_CAPE_DATA_FILE",
      "/base-data/CapeCod26.parquet",
      base_project_path("data", "external", "CapeCod26.parquet")
    ),
    brewstuff_model_file = base_file_override_or_fallback(
      "BASE_BREWSTUFF_MODEL_FILE",
      "/base-data/models/brewstuff.model",
      base_model_path("shared", "brewstuff.model")
    ),
    scout_models_file = base_file_override_or_fallback(
      "BASE_SCOUT_MODELS_FILE",
      "/base-data/models/pitch_models.rds",
      base_model_path("shared", "pitch_models.rds")
    ),
    pitcher_stuff_model_file = base_file_override_or_fallback(
      "BASE_PITCHER_STUFF_MODEL_FILE",
      "/base-data/models/Stuff+2.rds",
      base_model_path("pitcher", "Stuff+2.rds")
    ),
    pitcher_location_model_file = base_file_override_or_fallback(
      "BASE_PITCHER_LOCATION_MODEL_FILE",
      "/base-data/models/location_plus_model.rds",
      base_model_path("pitcher", "location_plus_model.rds")
    ),
    pitcher_league_stats_file = base_file_override_or_fallback(
      "BASE_PITCHER_LEAGUE_STATS_FILE",
      "/base-data/models/NEW_LeagueStats2.rds",
      base_model_path("pitcher", "NEW_LeagueStats2.rds")
    ),
    pitcher_location_league_stats_file = base_file_override_or_fallback(
      "BASE_PITCHER_LOCATION_LEAGUE_STATS_FILE",
      "/base-data/models/location_plus_league_stats_pitcher.rds",
      base_model_path("pitcher", "location_plus_league_stats_pitcher.rds")
    ),
    xwoba_grid_file = base_file_override_or_fallback(
      "BASE_XWGRID_FILE",
      "/base-data/models/xwoba_grid.rds",
      base_model_path("shared", "xwoba_grid.rds")
    ),
    ncaa_colors_file = base_env_path("BASE_NCAA_COLORS_FILE", base_reference_path("NcaaColors.csv")),
    percentile_table_file = base_env_path("BASE_PERCENTILE_TABLE_FILE", base_reference_path("percentile_table.csv")),
    left_batter_image_file = base_env_path("BASE_LEFT_BATTER_IMAGE_FILE", base_www_path("reference", "left_batter.png")),
    right_batter_image_file = base_env_path("BASE_RIGHT_BATTER_IMAGE_FILE", base_www_path("reference", "right_batter.png")),
    heights_file = base_env_path("BASE_PLAYER_HEIGHTS_FILE", base_reference_path("College26Heights.csv")),
    roster_file = base_env_path("BASE_ROSTER_FILE", base_project_path("config", "texas_state_roster_2027.csv")),
    schedule_file = base_env_path("BASE_SCHEDULE_FILE", base_project_path("config", "texas_state_schedule_2027.csv"))
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
