# Run from ScoutingApp: Rscript tests/test_rick_duplex.R
# Loaded by scripts/tests/test_rick_scouting.R inside the BASE workspace.
d <- pitcher_env$standardize_tm(data.frame(Pitcher='Duplex Test',PitcherThrows='Left',
  BatterSide=c('Right','Left'),TaggedPitchType='Fastball',RelSpeed=90,
  PlateLocSide=0,PlateLocHeight=2,HB=12,IVB=18,RelHeight=6,RelSide=-2,
  Balls=0,Strikes=0,PitchCall='StrikeCalled',PlayResult='Undefined'))
pdf(tempfile(fileext='.pdf'))
shiny::testServer(function(input,output,session) rick_server(input,output,session,reactive(d)), {
  session$setInputs(rick_pitcher='Duplex Test',rick_side='R')
  session$setInputs(rick_editor_change=list(key='Duplex Test|R',notes='RIGHT NOTES',shapes=list()))
  session$setInputs(rick_side='L')
  session$setInputs(rick_editor_change=list(key='Duplex Test|L',notes='LEFT NOTES',shapes=list()))
  path <- output$rick_pdf
  bytes <- readBin(path,'raw',n=file.info(path)$size)
  # PDF page objects are uncompressed even when plot streams are compressed.
  txt <- paste(rawToChar(bytes,multiple=TRUE),collapse='')
  stopifnot(length(gregexpr('/Type /Page\\b',txt,perl=TRUE,useBytes=TRUE)[[1]])==2L)
  file.copy(path,'/tmp/rick-handler-duplex.pdf',overwrite=TRUE)
})
grid.draw(rick_sheet(d[0,],'Duplex Test','L',pitcher_hand='LHP'))
dev.off()
cat('Actual download produces two pages when LHH is selected; empty split renders.\n')
