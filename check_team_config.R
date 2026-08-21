source("team_config.R", local = FALSE)

fail <- function(...) stop(..., call. = FALSE)
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

asset_checks <- c(
  team_logo = base_asset_file(TEAM_CONFIG$assets$logo),
  scoreboard_logo = base_asset_file(TEAM_CONFIG$assets$scoreboard_logo),
  leaderboards_logo = file.path("leaderboards", "www", TEAM_CONFIG$assets$leaderboards_logo)
)
missing_assets <- names(asset_checks)[is.na(asset_checks) | !file.exists(asset_checks)]
if (length(missing_assets)) {
  warning("Configured asset(s) not found: ", paste(missing_assets, collapse = ", "))
}

cat("BASE team configuration is valid.\n")
cat("Team:", TEAM_CONFIG$full_name, "\n")
cat("Season:", TEAM_CONFIG$season_label, "\n")
cat("Season data:", TEAM_CONFIG$data$season_file, "\n")
cat("Schedule source:", if (nzchar(TEAM_CONFIG$data$schedule_file)) TEAM_CONFIG$data$schedule_file else "optional Stats API adapter", "\n")
cat("Roster source:", if (nzchar(TEAM_CONFIG$data$roster_file)) TEAM_CONFIG$data$roster_file else "optional Stats API adapter", "\n")
