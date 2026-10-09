# Run from ScoutingApp: Rscript tests/test_rick_advance.R
# Loaded by scripts/tests/test_rick_scouting.R inside the BASE workspace.
pdf(tempfile(fileext='.pdf'))
raw <- data.frame(Pitcher='Test, Pitcher',PitcherThrows='Right',BatterSide='Right',
  TaggedPitchType=c('Fastball','Fastball','Slider','Slider','Changeup','Fastball'),
  Balls=c(0,1,0,1,2,3),Strikes=c(0,0,2,2,1,2),RelSpeed=c(90,92,80,82,83,91),
  PlateLocSide=c(0,2,0,2,NA,0),PlateLocHeight=c(2,2,2,2,NA,2),
  PitchCall=c('StrikeSwinging','StrikeSwinging','StrikeSwinging','FoulBall','BallCalled','InPlay'),
  PlayResult=c('Undefined','Undefined','Undefined','Undefined','Undefined','Out'),
  InducedVertBreak=c(18,17,0,1,8,19),HorzBreak=c(12,11,-5,-4,14,12),RelHeight=6,RelSide=2,
  ExitSpeed=c(NA,NA,NA,NA,NA,100),Angle=c(NA,NA,NA,NA,NA,0))
d <- pitcher_env$standardize_tm(raw)
x <- rick_prepare(d); tab <- rick_table(x)
f <- tab[tab$Pitch=='Fastball',]; s <- tab[tab$Pitch=='Slider',]
stopifnot(f$`Velocity (max)`=='91.0 (92.0)',f$`Pre2k Usage%`=='67%',f$`IZ%`=='50%',
  f$`2k Usage%`=='33%', f$`2k IZ%`=='100%',f$`IZ Whiff%`=='50%',f$`Chase%`=='100%',
  s$`Pre2k Usage%`=='0%',s$`IZ%`=='-',s$`2k Usage%`=='67%',s$`2k IZ%`=='50%')
stopifnot(tab$`IZ%`[tab$Pitch=='Changeup']=='-')
# Every heatmap renders with no matching events and with a single point.
for(m in c('Location','IZ Whiff','Chase','Exit velocity')) {
  ggplotGrob(rick_heat(x[0,],m)); ggplotGrob(rick_heat(x[6,,drop=FALSE],m))
}
# Missing release, handedness and movement must not crash the D1 comparison.
missing <- d; missing$PitcherThrows <- NA_character_; missing$HB <- NA_real_; missing$IVB <- NA_real_
missing$RelHeight <- NA_real_; missing$RelSide <- NA_real_
invisible(ggplotGrob(rick_movement(missing)))
pdf(tempfile(fileext='.pdf'),width=8.5,height=11)
grid.draw(rick_sheet(d,'Test, Pitcher','R','Attack up.\nFinish away.'));dev.off()
shiny::testServer(function(input,output,session) rick_server(input,output,session,reactive(d)), {
  session$setInputs(rick_pitcher='Test, Pitcher',rick_side='R',rick_notes='')
  stopifnot(!is.null(output$rick_sheet))
  session$setInputs(rick_notes='Test notes'); session$flushReact()
  session$setInputs(rick_side='L'); session$flushReact()
})
cat('Rick Advance Sheet checks passed.\n')
# Positive raw PlateX must land left of center in scatter and density views.
point <- data.frame(PlateX=1,PlateZ=2)
b <- ggplot_build(rick_zone()+geom_point(data=point,aes(PlateX,PlateZ)))
pos <- b$plot$coordinates$transform(b$data[[3]],b$layout$panel_params[[1]])
stopifnot(pos$x < .5)
one <- x[1,,drop=FALSE]; one$PlateX <- 1
h <- ggplot_build(rick_heat(one,'Location'))
# Check the density peak directly through the plotted grid, not color ties.
peak <- which.max(h$plot$data$value)
stopifnot(h$data[[1]]$x[peak] < 0)
stopifnot(rick_percentile(.50,'performance_k_pct') > rick_percentile(.10,'performance_k_pct'),
  rick_percentile(.05,'performance_bb_pct',lower=TRUE) > rick_percentile(.20,'performance_bb_pct',lower=TRUE),
  rick_cell_style(NA_real_,'performance_zone_pct')$fill == '#FFFFFF',
  rick_cell_style(.9,'usage')$fill == '#FFFFFF',
  rick_cell_style(.1,'usage')$fill == '#FFFFFF')
rates <- data.frame(`Usage%`=.75,`IZ%`=.80,check.names=FALSE)
rt <- rick_metric_table(rates,c('usage','performance_zone_pct'))
stopifnot(sum(rt$layout$name=='core-bg')==2L,
  identical(vapply(rt$grobs[which(rt$layout$name=='core-bg')],function(g) g$gp$fill != '#FFFFFF',logical(1)),c(FALSE,TRUE)))
cat('Catcher POV and shaded metric table checks passed.\n')
