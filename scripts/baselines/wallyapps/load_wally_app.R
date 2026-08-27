wally_baseline_helper_file <- local({
  frame_files <- vapply(sys.frames(), function(frame) {
    ofile <- frame$ofile
    if (is.null(ofile) || !nzchar(ofile)) return(NA_character_)
    normalizePath(ofile, winslash = "/", mustWork = FALSE)
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (!length(frame_files)) stop("Unable to resolve Wally baseline helper path", call. = FALSE)
  frame_files[[length(frame_files)]]
})

wally_baseline_project_root <- normalizePath(
  file.path(dirname(wally_baseline_helper_file), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

get_wally_baseline_root <- function() wally_baseline_project_root

wally_baseline_app_spec <- function(app_name) {
  specs <- list(
    HittingApp = list(source = "HittingApp.R", fixture = "hitting"),
    PitchingApp = list(source = "PitchingApp.R", fixture = "pitching"),
    DefenseApp = list(source = "DefenseApp.R", fixture = "defense")
  )
  spec <- specs[[app_name]]
  if (is.null(spec)) stop("Unknown Wally app: ", app_name, call. = FALSE)
  spec
}

load_wally_baseline_app <- function(app_name) {
  project_root <- get_wally_baseline_root()
  baseline_library <- file.path(project_root, ".baseline-runtime", "R")
  if (!dir.exists(baseline_library)) {
    stop(
      "Baseline R library is missing. Run ",
      "Rscript scripts/baselines/wallyapps/install_r_dependencies.R",
      call. = FALSE
    )
  }
  .libPaths(c(baseline_library, .libPaths()))

  spec <- wally_baseline_app_spec(app_name)
  source_app_dir <- file.path(project_root, "WallyApps", app_name)
  fixture_dir <- file.path(project_root, "tests", "fixtures", "wallyapps", spec$fixture)
  source_file <- file.path(source_app_dir, spec$source)
  if (!file.exists(source_file)) stop("Missing Wally source: ", source_file, call. = FALSE)
  if (!dir.exists(file.path(fixture_dir, "data"))) {
    stop("Missing Wally fixture data: ", fixture_dir, call. = FALSE)
  }

  sandbox <- tempfile(pattern = paste0("wally-baseline-", tolower(app_name), "-"))
  dir.create(sandbox, recursive = TRUE, showWarnings = FALSE)

  copied_source <- file.copy(source_file, file.path(sandbox, spec$source), overwrite = TRUE)
  copied_data <- file.copy(
    file.path(fixture_dir, "data"),
    sandbox,
    recursive = TRUE,
    copy.mode = FALSE,
    copy.date = FALSE
  )
  if (!copied_source || !copied_data) stop("Unable to prepare baseline sandbox", call. = FALSE)

  source_www <- file.path(source_app_dir, "www")
  if (dir.exists(source_www)) {
    file.copy(source_www, sandbox, recursive = TRUE, copy.mode = FALSE, copy.date = FALSE)
  }

  old_wd <- setwd(sandbox)
  old_data_dir <- Sys.getenv("DATA_DIR", unset = NA_character_)
  on.exit({
    setwd(old_wd)
    if (is.na(old_data_dir)) Sys.unsetenv("DATA_DIR") else Sys.setenv(DATA_DIR = old_data_dir)
  }, add = TRUE)
  Sys.unsetenv("DATA_DIR")

  app_env <- new.env(parent = globalenv())
  sys.source(file.path(sandbox, spec$source), envir = app_env)

  list(
    app = app_name,
    env = app_env,
    sandbox = sandbox,
    fixture = fixture_dir,
    cleanup = function() unlink(sandbox, recursive = TRUE, force = TRUE)
  )
}
