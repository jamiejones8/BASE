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

  logo_candidates <- c("caps.svg", "caps.svg.png", "tslogo.png")
  logo_dir <- get0("WHITE_CAPS_WWW_DIR", inherits = TRUE, ifnotfound = "www")

  for (logo_name in logo_candidates) {
    logo_path <- file.path(logo_dir, logo_name)
    if (file.exists(logo_path) && !is_lfs_pointer(logo_path)) {
      return(whitecaps_asset_path(logo_name))
    }
  }

  whitecaps_asset_path("tslogo.png")
}

APP_TITLE <- "Brewster Whitecaps Analytics"
APP_HOME_TITLE <- "Welcome to Whitecaps Central"
APP_COMMAND_CENTER <- "Brewster Whitecaps Analytics Command Center"
APP_FOOTER_TEXT <- "Built for Brewster Whitecaps Baseball"
TEAM_DISPLAY_NAME <- "Whitecaps"
ORGANIZATION_DISPLAY_NAME <- "Brewster Whitecaps Baseball"
BRAND_LOGO_FILE <- resolve_brand_logo_file()
ACTIVE_TEAM_CODE <- "BRE_WHI"
