#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
source('team_config.R')
# Exact endpoints, intermediate EV values, below-minimum LA and invalid measurements.
ev <- c(94.9999,95,95,99.9999,99.9999,100,100,104.9999,104.9999,105,105,110,NA,Inf,100)
la <- c(20,7.9999,8,25,25.0001,32,32.0001,32,32.0001,40,40.0001,8,20,20,NA)
expected <- c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE)
stopifnot(identical(base_barrel_evla(ev,la),expected),
          identical(base_barrel_evla(as.character(ev),as.character(la)),expected))
# Check the committed benchmark against its audited population denominator.
meta <- jsonlite::fromJSON('data/reference/barrel_benchmark_provenance.json')
ref <- base_read_d1_aar_reference()
for (metric in names(meta$metrics)) {
  spec <- meta$metrics[[metric]]
  rows <- ref[ref$scope=='overall' & ref$metric==metric,]
  stopifnot(nrow(rows)==spec$players,sum(rows$sample_n)==spec$denominator,
    abs(base_weighted_d1_aar_metric(ref,metric)-spec$weighted_average)<1e-12)
}
cat('Sliding barrel boundaries, missing measurements, and regenerated D1 benchmarks passed.\n')
