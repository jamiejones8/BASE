# Use a bundled project library only when its compiled packages match the
# current R platform. A copied Intel library must not shadow native packages
# on Apple Silicon (or vice versa).
use_vald_local_library <- function(path = ".R-library") {
  if (!dir.exists(path)) return(invisible(FALSE))
  path <- normalizePath(path)
  metadata <- list.files(
    path,
    pattern = "package[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  incompatible <- vapply(metadata, function(meta_path) {
    package_dir <- dirname(dirname(meta_path))
    if (!dir.exists(file.path(package_dir, "libs"))) return(FALSE)
    built <- tryCatch(readRDS(meta_path)$Built$Platform, error = function(e) "")
    nzchar(built) && !identical(built, R.version$platform)
  }, logical(1))
  if (any(incompatible)) {
    message(
      "Skipping incompatible .R-library (",
      basename(dirname(dirname(metadata[which(incompatible)[1]]))),
      " was built for a different platform). Run Rscript install.R to rebuild it."
    )
    return(invisible(FALSE))
  }
  .libPaths(c(path, .libPaths()))
  invisible(TRUE)
}
