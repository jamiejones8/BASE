get_script_dir <- function() {
  frame_files <- vapply(sys.frames(), function(frame) {
    ofile <- frame$ofile
    if (is.null(ofile) || !nzchar(ofile)) return(NA_character_)
    normalizePath(ofile, winslash = "/", mustWork = TRUE)
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files)) {
    return(dirname(frame_files[[length(frame_files)]]))
  }

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

base_dir <- normalizePath(file.path(get_script_dir(), "..", ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(base_dir)
on.exit(setwd(old_wd), add = TRUE)

required_files <- c(
  "team_config.R",
  "app.R",
  "R/app_main.R",
  "R/integrations/leaderboards_embed.R",
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
  "R/config/team_config.R",
  "R/app_main.R",
  "R/integrations/leaderboards_embed.R",
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

app_lines <- readLines("R/app_main.R", warn = FALSE)
assert_true(any(grepl("leaderboards_embed\\.R", app_lines)), "R/app_main.R no longer sources the leaderboards embed module")
assert_true(any(grepl("team_analytics_hub_card\\(\\)", app_lines)), "R/app_main.R no longer registers the team analytics hub card")
assert_true(any(grepl("team_analytics_env\\$server\\(", app_lines)), "R/app_main.R no longer binds the embedded leaderboards server")

source("team_config.R", local = FALSE)
base_source("R/integrations/leaderboards_embed.R", local = FALSE)
assert_true(is.function(team_analytics_hub_card), "team_analytics_hub_card() is unavailable")
assert_true(is.function(team_analytics_embedded_ui), "team_analytics_embedded_ui() is unavailable")
assert_true(is.function(team_analytics_bind_parent_server), "team_analytics_bind_parent_server() is unavailable")

assert_true(exists("team_analytics_env"), "team_analytics_env is unavailable after loading the embed integration")
files <- team_analytics_env$list_bundled_data_files()
assert_true(nrow(files) == 1L, paste("Expected exactly one CSV in leaderboards/data, found", nrow(files)))

loaded <- team_analytics_env$load_bundled_data_file(compute_summaries = FALSE)
if (nrow(loaded$raw) == 0L) {
  message("Season source is currently empty; verify the configured 2026 college dataset.")
}
assert_true("SourceFile" %in% names(loaded$raw), "Bundled team data is missing SourceFile metadata")

cat("Leaderboards integration check passed.\n")
cat("Active data file:", files$File[[1]], "\n")
cat("Loaded rows:", nrow(loaded$raw), "\n")
