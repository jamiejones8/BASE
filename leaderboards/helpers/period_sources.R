leaderboards_period_spec <- function(period = "2026-season", config = TEAM_CONFIG) {
  if (is.null(period) || !length(period) || is.na(period[[1]]) || !nzchar(period[[1]])) {
    period <- "2026-season"
  } else {
    period <- as.character(period[[1]])
  }

  specs <- list(
    `2026-season` = list(
      id = "2026-season", label = "2026 Season", path = config$data$season_file,
      shared_source = TRUE
    ),
    `2026-fall` = list(
      id = "2026-fall", label = "2026 Fall",
      path = file.path(config$data$team_season_import_dir, "2026 Fall - cleaned.csv"),
      shared_source = FALSE
    ),
    `2027-season` = list(
      id = "2027-season", label = "2027 Season",
      path = file.path(config$data$team_season_import_dir, "2027 Season - cleaned.csv"),
      shared_source = FALSE
    )
  )

  if (!period %in% names(specs)) period <- "2026-season"
  specs[[period]]
}
