suppressPackageStartupMessages({library(dplyr);library(readr)})
# Exercise the actual BrewStuff feature pipeline with a prediction-boundary spy.
# The production LightGBM model is not needed to check missing-input regressions.
for(expr in parse('R/app_main.R')) if(is.call(expr) && identical(expr[[1]],as.name('<-')) &&
  is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% c('getBrewStuff','scale_Stuff','cleanColumnContents','get_rotated_movement','calculateArmAngle')) eval(expr)
Height26 <- read_csv('data/reference/College26Heights.csv',show_col_types=FALSE)
predict.feature_check <- function(object,newdata,...) {
  stopifnot(ncol(newdata)==13L,all(is.finite(newdata)))
  rep(-.025181704334056,nrow(newdata))
}
model_spy <- structure(list(),class='feature_check')
d <- tibble(Pitcher=c('Leininger, Ryley','Ryley Leininger','Langley, Christopher','Christopher Langley','Chris Langley'),
  PitcherTeam='TEX_BOB',TaggedPitchType='Fastball',PitcherThrows='R',BatterSide='L',
  RelSpeed=91,InducedVertBreak=15,HorzBreak=10,SpinRate=2200,RelHeight=6.1,RelSide=1.8,Extension=5.8,
  PitchCall='StrikeCalled',PlayResult='Undefined')
out <- getBrewStuff(d,model_spy)
stopifnot(nrow(out)==nrow(d),all(is.finite(out$armangle)),all(is.finite(out$Stuff)),all(abs(out$height*12-73)<1e-6))
# An optional real source validates all available target pitches, not just a fixture.
source_path <- Sys.getenv('BASE_STUFF_ALIAS_SOURCE','')
if(nzchar(source_path)) {
  raw <- read_csv(source_path,show_col_types=FALSE) %>% filter(grepl('Leininger|Langley',Pitcher))
  scored <- getBrewStuff(raw,model_spy)
  stopifnot(all(is.finite(scored$Stuff)),all(is.finite(scored$armangle)))
  print(scored %>% count(Pitcher,name='pitches_with_complete_features'))
}
cat('Both TrackMan and display-name aliases produce complete BrewStuff feature vectors.\n')
