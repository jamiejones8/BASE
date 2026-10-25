path <- tempfile(fileext='.rds')
Sys.setenv(TXST_DEPTH_STATE=path)
source('app.R')
shiny::testServer(server, {
 p <- list(id='recruit-funding',first='Test',last='Offer',entryClass=1,entryYear=2028,status='Offered',positions=list('SS'),recruit=TRUE,offerPackage=5000)
 s <- list(profiles=list(p),charts=list(),revision=0,funding=list(budgets=list('2027'=100000),awards=list('roster-17'=list('2027'=65000))))
 session$setInputs(save_state=s)
 saved <- readRDS(path)
 stopifnot(saved$revision==1,saved$funding$budgets[['2027']]==100000,saved$funding$awards[['roster-17']][['2027']]==65000,saved$profiles[[1]]$offerPackage==5000)
 s$revision <- 1;s$funding$awards[['roster-17']][['2027']] <- -5
 session$setInputs(save_state=s)
 stopifnot(readRDS(path)$revision==1)
 s$funding$awards[['roster-17']][['2027']] <- 0
 s$profiles[[1]]$offerPackage <- -10
 session$setInputs(save_state=s)
 stopifnot(readRDS(path)$revision==1)
 s$profiles[[1]]$offerPackage <- 5000
 session$setInputs(save_state=s)
 stopifnot(readRDS(path)$revision==2,readRDS(path)$funding$awards[['roster-17']][['2027']]==0)
})
unlink(path)
cat('Funding and offer persistence, explicit zero, and invalid amount rejection passed.\n')
