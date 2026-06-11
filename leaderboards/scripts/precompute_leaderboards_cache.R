get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args, value = TRUE)

  if (!length(match)) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  dirname(normalizePath(sub(file_arg, "", match[[1]]), winslash = "/", mustWork = TRUE))
}

script_dir <- get_script_dir()
app_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

WHITE_CAPS_APP_DIR <- app_dir
WHITE_CAPS_DATA_DIR <- file.path(WHITE_CAPS_APP_DIR, "data")
WHITE_CAPS_CACHE_DIR <- file.path(WHITE_CAPS_APP_DIR, "cache")

whitecaps_asset_path <- function(path = "") {
  path
}

source("helpers/brand_config.R", local = TRUE)
source("helpers/process_data.R", local = TRUE)
source("helpers/pitcher_times_through_order.R", local = TRUE)
source("helpers/calculate_pitcher_stats.R", local = TRUE)
source("helpers/calculate_hitter_stats.R", local = TRUE)
source("helpers/process_calculations.R", local = TRUE)
source("helpers/checkbox_loader.R", local = TRUE)
source("helpers/home_counts.R", local = TRUE)
source("helpers/pitch_type_summary.R", local = TRUE)
source("helpers/cache_loader.R", local = TRUE)

active_file <- pick_active_bundled_file()
if (is.null(active_file$path) || !nzchar(active_file$path) || !file.exists(active_file$path)) {
  stop("No bundled CSV file was found in leaderboards/data.", call. = FALSE)
}

message("📦 Building precomputed leaderboards cache from ", active_file$label)

data_list <- load_bundled_data_file(
  file_path = active_file$path,
  compute_summaries = FALSE
)

processed_raw <- data_list$raw
team_pitching <- get_team_pitching(processed_raw)
team_hitting <- get_team_hitting(processed_raw)
tto_choices <- c("All Times Through Order", PITCHER_TTO_LEVELS())

build_tto_cache <- function(builder) {
  setNames(
    lapply(tto_choices, function(choice) {
      builder(filter_pitcher_times_through_order(team_pitching, choice))
    }),
    tto_choices
  )
}

bundle <- build_leaderboards_cache_bundle(
  source_file = active_file,
  processed_raw = processed_raw,
  hitting_stats = calculate_hitter_stats(team_hitting),
  pitching_stats_by_tto = build_tto_cache(calculate_pitching_stats),
  process_stats_by_tto = build_tto_cache(calculate_pitching_process_stats),
  pitch_type_breakdown_by_tto = build_tto_cache(prepare_pitch_type_breakdown_data),
  home_counts = list(
    hitter_counts = build_home_hitter_counts(team_hitting),
    pitcher_counts = build_home_pitcher_counts(team_pitching),
    pitcher_walks_counts = build_home_pitcher_walks_counts(team_pitching)
  )
)

cache_path <- save_leaderboards_cache(bundle)

cat("Leaderboards cache written to:", cache_path, "\n")
cat("Source data file:", active_file$label, "\n")
cat("Processed rows:", nrow(processed_raw), "\n")
