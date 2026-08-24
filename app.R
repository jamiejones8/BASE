base_wrapper_file <- local({
  frame_files <- vapply(sys.frames(), function(frame) {
    ofile <- frame$ofile
    if (is.null(ofile) || !nzchar(ofile)) return(NA_character_)
    normalizePath(ofile, winslash = "/", mustWork = FALSE)
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files)) frame_files[[length(frame_files)]] else normalizePath("app.R", winslash = "/", mustWork = FALSE)
})

source(
  file.path(dirname(base_wrapper_file), "R", "app_main.R"),
  local = FALSE
)$value
