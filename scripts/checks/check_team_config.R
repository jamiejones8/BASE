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

project_root <- normalizePath(file.path(get_script_dir(), "..", ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

source(file.path(project_root, "team_config.R"), local = FALSE)

fail <- function(...) stop(..., call. = FALSE)
check_args <- commandArgs(trailingOnly = TRUE)
if (length(setdiff(check_args, "--build"))) fail("Usage: check_team_config.R [--build]")
build_check <- "--build" %in% check_args
required_text <- c("city", "name", "full_name", "organization",
                   "abbreviation", "data_code", "data_pattern",
                   "league_name", "season_label", "roster_label")

for (field in required_text) {
  value <- TEAM_CONFIG[[field]]
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    fail("TEAM_CONFIG$", field, " must be a non-empty value")
  }
}

for (field in names(TEAM_CONFIG$colors)) {
  value <- TEAM_CONFIG$colors[[field]]
  if (!grepl("^#[0-9A-Fa-f]{6}$", value)) {
    fail("BASE color '", field, "' must use six-digit hex notation: ", value)
  }
}

if (!tolower(tools::file_ext(TEAM_CONFIG$data$season_file)) %in% c("parquet", "csv")) {
  fail("BASE_SEASON_DATA_FILE must be a parquet or CSV file")
}
if (!file.exists(TEAM_CONFIG$data$season_file)) {
  fail("Season data file was not found: ", TEAM_CONFIG$data$season_file)
}

validate_csv <- function(path, required, label) {
  if (!nzchar(path)) return(invisible(NULL))
  if (!file.exists(path)) fail(label, " file was not found: ", path)
  header <- names(utils::read.csv(path, nrows = 1L, check.names = FALSE))
  missing <- setdiff(required, header)
  if (length(missing)) fail(label, " file is missing: ", paste(missing, collapse = ", "))
}

validate_csv(
  TEAM_CONFIG$data$roster_file,
  c("Name", "Pos", "Number", "Bats", "Throws", "pos_type"),
  "Roster"
)
validate_csv(
  TEAM_CONFIG$data$schedule_file,
  c("DateTime", "Opponent", "Venue"),
  "Schedule"
)
validate_csv(
  TEAM_CONFIG$data$heights_file,
  c("tm_name", "team_abbr", "height", "set"),
  "Player heights"
)

if (!file.exists(TEAM_CONFIG$data$team_names_file)) {
  fail("TrackMan team-name file was not found: ", TEAM_CONFIG$data$team_names_file)
}
team_names <- base_team_name_table(refresh = TRUE)
if (!nrow(team_names) || !identical(base_team_display_name("BAY_BEA"), "Baylor Bears")) {
  fail("TrackMan team-name lookup did not resolve BAY_BEA to Baylor Bears")
}
ncaa_team_ids <- utils::read.csv(
  TEAM_CONFIG$data$ncaa_colors_file, nrows = -1L, check.names = FALSE,
  colClasses = "character"
)$team_abbr
missing_team_names <- setdiff(
  unique(ncaa_team_ids[!is.na(ncaa_team_ids) & nzchar(ncaa_team_ids)]),
  team_names$trackman_team_id
)
if (length(missing_team_names)) {
  fail("TrackMan team-name lookup is missing: ", paste(missing_team_names, collapse = ", "))
}

runtime_model_fields <- c(
  "brewstuff_model_file",
  "scout_models_file",
  "pitcher_stuff_model_file",
  "pitcher_location_model_file"
)
model_fields <- c(
  runtime_model_fields,
  "pitcher_league_stats_file",
  "pitcher_location_league_stats_file",
  "xwoba_grid_file"
)
missing_models <- model_fields[!file.exists(unlist(TEAM_CONFIG$data[model_fields]))]
if (build_check) {
  deferred_models <- intersect(missing_models, runtime_model_fields)
  if (length(deferred_models)) {
    cat("Model presence checks deferred until container startup:",
        paste(deferred_models, collapse = ", "), "\n")
  }
  missing_models <- setdiff(missing_models, runtime_model_fields)
}
if (length(missing_models)) {
  details <- vapply(missing_models, function(field) {
    paste0(field, " = ", TEAM_CONFIG$data[[field]])
  }, character(1))
  fail("Configured model file(s) not found: ", paste(details, collapse = ", "))
}
pointer_models <- model_fields[vapply(
  TEAM_CONFIG$data[model_fields],
  base_is_lfs_pointer,
  logical(1)
)]
if (length(pointer_models)) {
  fail("Configured model file(s) are unresolved Git LFS pointers: ", paste(pointer_models, collapse = ", "))
}

resolve_leaderboards_logo_check <- function() {
  configured <- TEAM_CONFIG$assets$leaderboards_logo
  candidates <- c(
    base_asset_file(configured),
    base_project_path("leaderboards", "www", configured)
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates)) candidates[[1]] else NA_character_
}

asset_checks <- c(
  team_logo = base_asset_file(TEAM_CONFIG$assets$logo),
  scoreboard_logo = base_asset_file(TEAM_CONFIG$assets$scoreboard_logo),
  leaderboards_logo = resolve_leaderboards_logo_check()
)
missing_assets <- names(asset_checks)[is.na(asset_checks) | !file.exists(asset_checks)]
if (length(missing_assets)) {
  warning("Configured asset(s) not found: ", paste(missing_assets, collapse = ", "))
}

cat(if (build_check) "BASE build configuration is valid.\n" else "BASE team configuration is valid.\n")
cat("Team:", TEAM_CONFIG$full_name, "\n")
cat("Season:", TEAM_CONFIG$season_label, "\n")
cat("Season data:", TEAM_CONFIG$data$season_file, "\n")
cat("Schedule source:", if (nzchar(TEAM_CONFIG$data$schedule_file)) TEAM_CONFIG$data$schedule_file else "optional Stats API adapter", "\n")
cat("Roster source:", if (nzchar(TEAM_CONFIG$data$roster_file)) TEAM_CONFIG$data$roster_file else "optional Stats API adapter", "\n")
