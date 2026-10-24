suppressPackageStartupMessages({library(dplyr);library(shiny);library(ggplot2);library(patchwork)})
source('team_config.R')
load_named <- function(path,names,envir) {
  for (expr in parse(path)) if (is.call(expr) && identical(expr[[1]],as.name('<-')) &&
    is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% names) eval(expr,envir)
}
h <- new.env(parent=globalenv())
h$BOBCAT_LOGO_PATH <- base_project_path('WallyApps','HittingApp','www','Bobcatlogo.png')
load_named('WallyApps/HittingApp/HittingApp.R',c('performance_table_metrics','hitting_d1_reference','hitting_d1_lower_better',
  'hitting_d1_percentile','hitting_d1_average_lookup','D1_NON','D1_PCT','shade_cell','severity_rgba_fill',
  'apply_hitting_empirical_shading','HITTING_PERCENTILE_CSS','hitting_percentile_rankings_ui','build_inning_breakdown_plot'),h)
ref <- h$hitting_d1_reference
stopifnot(nrow(ref$values)>1000,all(h$performance_table_metrics %in% ref$averages$metric),
          all(is.finite(h$hitting_d1_average_lookup()[h$performance_table_metrics])))
for (metric in h$performance_table_metrics) {
  values <- ref$values$value[ref$values$metric == metric]
  lo <- h$hitting_d1_percentile(min(values)-1,metric); hi <- h$hitting_d1_percentile(max(values)+1,metric)
  stopifnot(if(metric %in% h$hitting_d1_lower_better) lo==100 && hi==0 else lo==0 && hi==100)
}
nums <- as.data.frame(as.list(setNames(ref$averages$average,ref$averages$metric)))
names(nums) <- ref$averages$metric
fmt <- as.data.frame(lapply(nums,as.character),check.names=FALSE)
shaded <- h$apply_hitting_empirical_shading(nums,fmt)
stopifnot(all(h$performance_table_metrics %in% names(shaded)))
html <- as.character(h$hitting_percentile_rankings_ui(nums,'Test Hitter'))
stopifnot(lengths(regmatches(html,gregexpr('class="cr-percentile-row"',html,fixed=TRUE))) == length(h$performance_table_metrics))
# No fallback 50s: production framing reference must produce distinct extremes.
f <- new.env(parent=globalenv())
load_named('WallyApps/DefenseApp/DefenseApp.R',c('framing_metric_min_chances','add_d1_percentiles'),f)
baseline <- readr::read_csv('WallyApps/DefenseApp/data/d1_catcher_framing_metrics.csv',show_col_types=FALSE)
metrics <- tibble(metric_id=c('strikes_stolen','strikes_stolen','balls_lost','balls_lost'),
  value=c(-1,2,-1,2),chances=100,better=c('high','high','low','low'))
stopifnot(identical(f$add_d1_percentiles(metrics,baseline)$percentile,c(0,100,100,0)))
missing <- f$add_d1_percentiles(mutate(metrics,chances=0L),baseline)
stopifnot(all(is.na(missing$percentile)))
# Title and first header are adjacent rows in a single table.
tbl <- ggpubr::ggtexttable(data.frame(A=1:3,B=4:6),rows=NULL)
g <- base_report_table_grob(tbl)
stopifnot(inherits(g,'gtable'))
p <- base_titled_report_table(tbl,'Test section')
stopifnot(inherits(ggplotGrob(p),'gtable'))
# Team report includes the new orange series.
plot_body <- paste(deparse(body(h$build_inning_breakdown_plot)),collapse='\n')
stopifnot(grepl('IZ Whiff%',plot_body,fixed=TRUE),grepl('#F28E2B',plot_body,fixed=TRUE))
for (file in c('.gitignore','.dockerignore','.railwayignore')) {
  text <- readLines(file)
  stopifnot(all(c('!WallyApps/DefenseApp/data/d1_catcher_framing_metrics.csv',
    '!WallyApps/HittingApp/data/d1_hitting_percentiles.csv','!WallyApps/HittingApp/data/d1_hitting_averages.csv') %in% text))
}
cat('D1 coverage, ranking direction, missing-data behavior, table composition, team-series color, and deployment references passed.\n')
