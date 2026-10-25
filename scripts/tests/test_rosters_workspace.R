suppressPackageStartupMessages(library(shiny))
source('team_config.R')
source('R/integrations/wally_depth_chart_workspace.R')
path <- file.path(tempfile('base-rosters-'),'state.rds')
w <- base_rosters_environment(path)
seed <- readRDS(file.path(BASE_DEPTH_ROOT,'data','depth-chart-state.rds'))
stopifnot(identical(readRDS(path),seed), length(w$read_roster('unused')) == 38L)
w$validate_state(seed,vapply(w$read_roster('unused'),`[[`,'','id'))
shiny::testServer(w$server, {
  s <- seed
  session$setInputs(save_state=s)
  saved <- readRDS(path)
  stopifnot(saved$revision == seed$revision+1L)
  expected <- seed; expected$revision <- saved$revision
  stopifnot(identical(saved,expected))
  # A stale client cannot overwrite the saved roster.
  session$setInputs(save_state=s)
  stopifnot(identical(readRDS(path),saved))
})
# Reinitialization must keep edits instead of restoring the bundled snapshot.
w2 <- base_rosters_environment(path)
stopifnot(w2$read_state()$revision == seed$revision+1L)
stopifnot(identical(w2$read_state()$profiles,seed$profiles),identical(w2$read_state()$funding,seed$funding))
unlink(dirname(path),recursive=TRUE)
cat('Imported roster/state fidelity, saves, conflict checks, and persistent initialization passed.\n')
