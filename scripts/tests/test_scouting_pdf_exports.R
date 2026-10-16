#!/usr/bin/env Rscript
source('team_config.R')
suppressPackageStartupMessages(library(dplyr))
source('R/integrations/wally_scouting_workspace.R')
scratch <- tempfile('scouting-pdf-'); dir.create(scratch)
TEAM_CONFIG$data$team_season_import_dir <- scratch
fixture_dir <- 'tests/fixtures/wallyapps/hitting/data'
filename <- '2026 Season - cleaned.csv'
raw <- readr::read_csv(file.path(fixture_dir,filename),show_col_types=FALSE)
saved <- base_import_scouting_file(file.path(fixture_dir,filename),'Opponent.csv')
season_source <- list(
  teams=function(role) 'TEST',
  catalog=function(role,team) data.frame(Team='TEST',Player=unique(if(role=='pitcher')raw$Pitcher else raw$Batter)),
  load_players=function(...) raw
)
file.copy(file.path(fixture_dir,filename),file.path(base_scouting_data_dir(),filename))
w <- base_wally_scouting_environment(base_scouting_data_dir(),season_source)
qa_dir <- Sys.getenv('SCOUTING_PDF_QA_DIR',scratch)
dir.create(qa_dir,recursive=TRUE,showWarnings=FALSE)
pdf_pages <- function(path) {
  bytes <- readBin(path,'raw',n=file.info(path)$size)
  bytes[bytes==as.raw(0)] <- as.raw(32)
  length(regmatches(rawToChar(bytes),gregexpr('/Type /Page\\b',rawToChar(bytes),perl=TRUE,useBytes=TRUE))[[1]])
}
handlers <- c('dl_pdf','dl_pitch_type_pdf','dl_heat_maps_pdf','dl_pitcher_card_pdf','dl_pitcher_bust_pdf','rick_pdf')
for (source_id in c(paste0('upload:',saved$filename),'season','csv')) {
  shiny::testServer(w$server, {
    session$setInputs(main_tab='none',scout_data_source=source_id,
      csv_files=if(source_id=='csv')filename else 'stale-missing.csv',
      scout_team='TEST',scout_season_pitchers=unique(raw$Pitcher),scout_season_hitters=unique(raw$Batter))
    hitters <- unique(std_all()$Batter)
    pitchers <- unique(pitcher_std_all()$PitcherName)
    session$setInputs(player=hitters[1],hitter_order=hitters,
      pitcher_card_pitcher=pitchers[1],pitcher_card_batter_side='Total',
      pitcher_bust_order=pitchers,pitcher_bust_hand='Total',
      heat_maps_hand='Total',rick_pitcher=pitchers[1],rick_side='R')
    for (handler in handlers) {
      session$setInputs(main_tab=c(dl_pdf='Hitter Card',dl_pitch_type_pdf='Pitch Type Tables',dl_heat_maps_pdf='Heat Maps',dl_pitcher_card_pdf='Pitcher Card',dl_pitcher_bust_pdf='Stuff Sheet',rick_pdf='rick_advance')[[handler]],pitch_type_hand='Total')
      cat('Export:',source_id,handler,'\n')
      path <- output[[handler]]
      stopifnot(file.exists(path),file.info(path)$size>1000)
      con <- file(path,'rb'); header <- readChar(con,5,useBytes=TRUE); close(con)
      stopifnot(identical(header,'%PDF-'),!session$isClosed(),
        pdf_pages(path)==if(handler %in% c('dl_pitch_type_pdf','dl_pitcher_card_pdf','rick_pdf'))2L else 1L)
      file.copy(path,file.path(qa_dir,paste0(if(startsWith(source_id,'upload:'))'upload' else source_id,'-',handler,'.pdf')),overwrite=TRUE)
    }
  })
}
# More than one page of hitters must be retained in both reports and both splits.
large <- dplyr::bind_rows(lapply(seq_len(19),function(i) {
  rows <- raw[raw$Batter==raw$Batter[1],]
  rows$Batter <- sprintf('Pagination Hitter %02d',i)
  rows$PitchUID <- paste0(i,'-',seq_len(nrow(rows)))
  rows
}))
large_path <- file.path(scratch,'large.csv'); readr::write_csv(large,large_path)
large_saved <- base_import_scouting_file(large_path,'Large opponent.csv')
shiny::testServer(w$server, {
  session$setInputs(scout_data_source=paste0('upload:',large_saved$filename),main_tab='none')
  session$setInputs(hitter_order=unique(std_all()$Batter),heat_maps_hand='Total')
  for(handler in c('dl_pitch_type_pdf','dl_heat_maps_pdf')) {
    path <- output[[handler]]
    stopifnot(pdf_pages(path)==if(handler=='dl_pitch_type_pdf')4L else 3L)
    file.copy(path,file.path(qa_dir,paste0('large-',handler,'.pdf')),overwrite=TRUE)
  }
})
# Sparse pitch groups must render without failed density calculations.
d <- w$std_cols(raw)
d <- d[is.finite(d$plate_x)&is.finite(d$plate_z)&is.finite(d$ev),][1,,drop=FALSE]
d$pitch_call <- 'StrikeSwinging'
withCallingHandlers({
  for(n in c(1,5)) {
    rows <- d[rep(1,n),]
    for(f in c('damage_by_group_plot','whiff_by_group_plot')) {
      ggplot2::ggplot_build(w[[f]](rows))
    }
  }
},warning=function(e) {
  if(grepl('Computation failed|bandwidth',conditionMessage(e)))stop(e)
})
# Failed rendering must close its device and never publish a partial/error PDF.
failed <- file.path(scratch,'failed.pdf')
before <- grDevices::dev.cur()
result <- w$save_multi_page_pdf(failed,list(grid::textGrob('valid page'),42),8.5,11)
stopifnot(!result$ok,!file.exists(failed),identical(before,grDevices::dev.cur()))
unlink(scratch,recursive=TRUE)
cat('All six scouting PDF downloads passed for uploaded files, national source, and legacy CSV.\n')
