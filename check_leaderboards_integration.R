get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args, value = TRUE)

  if (!length(match)) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  dirname(normalizePath(sub(file_arg, "", match[[1]]), winslash = "/", mustWork = TRUE))
}

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

base_dir <- get_script_dir()
old_wd <- setwd(base_dir)
on.exit(setwd(old_wd), add = TRUE)

required_files <- c(
  "team_config.R",
  "app.R",
  "leaderboards_embed.R",
  "leaderboards/app.R",
  "leaderboards/helpers/cache_loader.R",
  "leaderboards/helpers/checkbox_loader.R",
  "leaderboards/helpers/home_counts.R",
  "leaderboards/helpers/pitch_type_summary.R",
  "leaderboards/scripts/precompute_leaderboards_cache.R",
  "leaderboards/pages/upload_page.R"
)

missing_files <- required_files[!file.exists(required_files)]
assert_true(!length(missing_files), paste("Missing required file(s):", paste(missing_files, collapse = ", ")))

parse_targets <- c(
  "team_config.R",
  "app.R",
  "leaderboards_embed.R",
  "leaderboards/app.R",
  "leaderboards/helpers/cache_loader.R",
  "leaderboards/helpers/checkbox_loader.R",
  "leaderboards/helpers/home_counts.R",
  "leaderboards/helpers/pitch_type_summary.R",
  "leaderboards/pages/home_page.R",
  "leaderboards/pages/upload_page.R",
  "leaderboards/scripts/precompute_leaderboards_cache.R"
)

invisible(lapply(parse_targets, function(path) parse(file = path)))

app_lines <- readLines("app.R", warn = FALSE)
assert_true(any(grepl('source\\("leaderboards_embed\\.R"\\)', app_lines)), "app.R no longer sources leaderboards_embed.R")
assert_true(any(grepl("team_analytics_hub_card\\(\\)", app_lines)), "app.R no longer registers the team analytics hub card")
assert_true(any(grepl("team_analytics_env\\$server\\(", app_lines)), "app.R no longer binds the embedded leaderboards server")

source("team_config.R", local = FALSE)
source("leaderboards_embed.R")
assert_true(is.function(team_analytics_hub_card), "team_analytics_hub_card() is unavailable")
assert_true(is.function(team_analytics_embedded_ui), "team_analytics_embedded_ui() is unavailable")
assert_true(is.function(team_analytics_bind_parent_server), "team_analytics_bind_parent_server() is unavailable")

loader_env <- new.env(parent = globalenv())
old_leaderboards_wd <- setwd(file.path(base_dir, "leaderboards"))
on.exit(setwd(old_leaderboards_wd), add = TRUE)
source("helpers/checkbox_loader.R", local = loader_env)

files <- loader_env$list_bundled_data_files()
assert_true(nrow(files) == 1L, paste("Expected exactly one CSV in leaderboards/data, found", nrow(files)))

loaded <- loader_env$load_bundled_data_file(compute_summaries = FALSE)
if (nrow(loaded$raw) == 0L) {
  message("Season source is currently empty; verify the configured 2026 college dataset.")
}
assert_true("SourceFile" %in% names(loaded$raw), "Bundled team data is missing SourceFile metadata")

cat("Leaderboards integration check passed.\n")
cat("Active data file:", files$File[[1]], "\n")
cat("Loaded rows:", nrow(loaded$raw), "\n")
