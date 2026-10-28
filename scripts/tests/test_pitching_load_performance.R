suppressPackageStartupMessages(library(shiny))

expressions <- parse('WallyApps/PitchingApp/PitchingApp.R')
server_expr <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
  identical(x[[2]], as.name('server')), as.list(expressions))[[1]][[3]]
statements <- as.list(server_expr[[3]])[-1]
load_assignments <- function(names, envir) {
  for (expr in statements) {
    if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% names) eval(expr, envir)
  }
}

env <- new.env(parent = globalenv())
env$`%||%` <- function(x, y) if (is.null(x)) y else x
load_assignments(c('pitch_metric_pitch_type', 'pitch_metric_pitch_type_vec'), env)
legacy <- function(d) {
  out <- rep(NA_character_, nrow(d))
  for (col in c('ModelPitchType', 'ModelPitchTypeRaw', 'PitchType_plot',
                'PitchType', 'TaggedPitchType', 'AutoPitchType')) {
    if (!col %in% names(d)) next
    vals <- vapply(d[[col]], env$pitch_metric_pitch_type, character(1))
    ok <- is.na(out) & !is.na(vals) & nzchar(vals)
    out[ok] <- vals[ok]
  }
  out
}
sample <- data.frame(
  ModelPitchType = c(NA, '', 'undefined', 'FF', 'si', 'unknown', 'Custom'),
  PitchType = factor(c('SL', 'CH', NA, 'CU', 'FC', 'none', 'FS')),
  AutoPitchType = c('SI', 'KC', 'ST', 'CHG', 'FO', '4seam', NA)
)
stopifnot(identical(env$pitch_metric_pitch_type_vec(sample), legacy(sample)),
          identical(env$pitch_metric_pitch_type_vec(sample[FALSE, ]), character()))
large <- sample[rep(seq_len(nrow(sample)), length.out = 32647L), ]
before <- system.time(expected <- legacy(large))[['elapsed']]
after <- system.time(actual <- env$pitch_metric_pitch_type_vec(large))[['elapsed']]
stopifnot(identical(actual, expected))
cat(sprintf('Pitch-type normalization: %.3fs -> %.3fs (32,647 rows, identical results).\n', before, after))

builds <- 0L
paths <- character()
testServer(function(input, output, session) {
  bullpen_report_data <- function() data.frame(pitch = 1)
  bullpen_report_date <- function() as.Date('2026-10-28')
  prepare_bullpen_report_data <- identity
  format_bullpen_pitch_log <- identity
  bullpen_report_pages <- function() stop('Page counting must not compose report plots')
  render_bullpen_report_pdf <- function(d, pitcher, date, path) {
    builds <<- builds + 1L
    paths <<- c(paths, path)
    writeLines('test PDF', path)
  }
  load_assignments(c('split_bullpen_log_pages', 'bullpen_report_page_count',
                     'bullpen_report_pdf_path', 'bullpen_report_pdf_cache'), environment())
}, {
  session$setInputs(bullpen_report_pitcher = 'Test Pitcher', bullpen_report_game = 'Game 1')
  stopifnot(builds == 0L)
  stopifnot(isolate(bullpen_report_page_count()) == 2L, builds == 0L)
  first <- isolate(bullpen_report_pdf_cache())
  stopifnot(builds == 1L, file.exists(first), identical(first, isolate(bullpen_report_pdf_cache())))
  session$setInputs(bullpen_report_game = 'Game 2')
  stopifnot(builds == 1L)
  second <- isolate(bullpen_report_pdf_cache())
  stopifnot(builds == 2L, !file.exists(first), file.exists(second))
})
unlink(paths)
cat('Bullpen PDF builds only on demand and is reused until its inputs change.\n')
