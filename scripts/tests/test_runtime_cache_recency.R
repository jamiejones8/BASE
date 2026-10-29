#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
source('R/performance/lazy_workspace.R')
source('R/data/source_contract.R')
source('R/data/data_access.R')
source('R/data/defense_data_access.R')

# Exercise the real cache-hit paths without requiring production partitions.
# A frequently revisited selection must survive the next insertion.
for (role in c('pitcher', 'hitter', 'defense')) {
  cache <- get(paste0('.base_', role, '_cache'))
  order_name <- paste0('.base_', role, '_cache_order')
  assign(paste0('.base_', role, '_cache_limit'), 2L)
  put <- get(paste0('base_', role, '_cache_put'))
  key <- if (role == 'defense') 'TEAM' else paste('TEAM', 'Player', sep='\u001f')
  rows <- tibble::tibble(PitchUID='preserved')
  put(key, rows)
  put('other', rows)
  hit <- if (role == 'defense') {
    base_defense_dataset <- list()
    base_load_defense_team('TEAM')
  } else get(paste0('base_load_', role, '_rows'))('TEAM', 'Player')
  stopifnot(identical(hit, rows), identical(tail(get(order_name), 1L), key))
  put('new', rows)
  stopifnot(exists(key, cache, inherits=FALSE), !exists('other', cache, inherits=FALSE))
}
cat('Pitcher, hitter, and defense cache recency checks passed.\n')
