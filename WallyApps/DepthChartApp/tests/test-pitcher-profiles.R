path<-tempfile(fileext='.rds');Sys.setenv(TXST_DEPTH_STATE=path);source('app.R')
shiny::testServer(server, {
 s<-list(profiles=list(list(id='recruit-arm',first='New',last='Pitcher',entryYear=2028,entryClass=1,status='Committed',positions=list('LHP'),throws='L',heightIn=75,releaseHeightFt=6,releaseSideFt=2.1)),charts=list(),revision=0,playerDetails=list('roster-12'=list(positions=list('1B','3B','RHP'),bats='R',throws='R',heightIn=75.5,releaseHeightFt=6.2,releaseSideFt=2.3)))
 session$setInputs(save_state=s);saved<-readRDS(path)
 stopifnot(saved$revision==1,saved$playerDetails[['roster-12']]$releaseHeightFt==6.2,saved$profiles[[1]]$heightIn==75)
 s$revision<-1;s$playerDetails[['roster-12']]$heightIn<-10
 session$setInputs(save_state=s);stopifnot(readRDS(path)$revision==1)
 s$playerDetails[['roster-12']]$heightIn<-NULL;s$playerDetails[['roster-12']]$releaseSideFt<-NULL
 session$setInputs(save_state=s);stopifnot(readRDS(path)$revision==2)
})
unlink(path);cat('Current and recruit pitcher fields persist, reject invalid height, and allow incomplete measurements.\n')
