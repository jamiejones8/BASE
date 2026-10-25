path <- tempfile(fileext='.rds');Sys.setenv(TXST_DEPTH_STATE=path)
source('app.R')
shiny::testServer(server, {
 order <- as.list(c('C','1B','2B','3B','SS','LF','CF','RF','DH'))
 s <- list(profiles=list(),charts=list('2027'=list(C=list('roster-2'),SP=list('roster-17',NULL,NULL,NULL),LongRelief=list('roster-26',NULL,NULL)), '2028'=list(C=list(NULL),SP=list('roster-17',NULL,NULL,NULL))),revision=0,schemaVersion=2,
 departures=list('roster-2'=2028),matchups=list('v LHP'=list('2027'=list(C=list('roster-2')))),lineupOrders=list(General=list('2027'=order),'v LHP'=list('2027'=rev(order))),planEdits=list(General=list('2027'=TRUE)),lineupEdits=list('v LHP'=list('2027'=TRUE)),funding=list(budgets=list('2027'=100000),awards=list('roster-2'=list('2027'=65000))))
 session$setInputs(save_state=s)
 saved <- readRDS(path)
 stopifnot(saved$departures[['roster-2']]==2028,identical(saved$lineupOrders[['v LHP']][['2027']],rev(order)),saved$funding$awards[['roster-2']][['2027']]==65000,saved$charts[['2027']]$C[[1]]=='roster-2')
 s$revision <- 1;s$matchups[['v LHP']][['2028']] <- list(C=list('roster-2'))
 session$setInputs(save_state=s);stopifnot(readRDS(path)$revision==1)
 s$matchups[['v LHP']][['2028']] <- list(C=list(NULL));s$lineupOrders$General[['2027']][[2]] <- 'C'
 session$setInputs(save_state=s);stopifnot(readRDS(path)$revision==1)
})
unlink(path)
cat('Removal history, matchup orders, long relief, and invalid future roster/lineup rejection passed.\n')
