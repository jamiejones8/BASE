resolve_brand_logo_file <- function() {
  is_lfs_pointer <- function(path) {
    if (!file.exists(path)) {
      return(FALSE)
    }

    header <- tryCatch(
      readLines(path, n = 1L, warn = FALSE),
      error = function(e) character()
    )

    isTRUE(length(header) > 0) &&
      identical(header[[1]], "version https://git-lfs.github.com/spec/v1")
  }

  configured_logo <- get0("TEAM_CONFIG", inherits = TRUE, ifnotfound = list(assets = list()))$assets$leaderboards_logo
  logo_candidates <- unique(c(configured_logo, "base.svg", "base.svg.png", "tslogo.png"))
  logo_candidates <- logo_candidates[!is.na(logo_candidates) & nzchar(logo_candidates)]
  logo_dir <- get0("TEAM_ANALYTICS_WWW_DIR", inherits = TRUE, ifnotfound = "www")
  brand_logo_dir <- get0("TEAM_BRAND_WWW_DIR", inherits = TRUE, ifnotfound = file.path("..", "www"))

  for (logo_name in logo_candidates) {
    brand_logo_path <- file.path(brand_logo_dir, logo_name)
    if (file.exists(brand_logo_path) && !is_lfs_pointer(brand_logo_path)) {
      return(team_brand_asset_path(logo_name))
    }

    logo_path <- file.path(logo_dir, logo_name)
    if (file.exists(logo_path) && !is_lfs_pointer(logo_path)) {
      return(team_analytics_asset_path(logo_name))
    }
  }

  team_analytics_asset_path("tslogo.png")
}

team_cfg <- get0(
  "TEAM_CONFIG",
  inherits = TRUE,
  ifnotfound = list(
    name = "Team",
    full_name = "Team",
    organization = "Team Baseball",
    data_code = "",
    colors = list(primary = "#501214", secondary = "#D7BD8A")
  )
)

APP_TITLE <- paste(team_cfg$full_name, "Analytics")
APP_HOME_TITLE <- "Leaderboards Overview"
APP_COMMAND_CENTER <- paste(team_cfg$full_name, "Performance Snapshot")
APP_FOOTER_TEXT <- paste("Built for", team_cfg$organization)
TEAM_DISPLAY_NAME <- team_cfg$name
ORGANIZATION_DISPLAY_NAME <- team_cfg$organization
BRAND_LOGO_FILE <- resolve_brand_logo_file()
ACTIVE_TEAM_CODE <- team_cfg$data_code
