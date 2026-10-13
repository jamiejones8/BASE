source('team_config.R')
library(ggplot2)
z <- base_pitch_zone('inches')
r <- BASE_BALL_RADIUS_IN
# Four straight edges, corners, missing positions, and unit conversion.
x <- c(z$xmax+r,z$xmax+r+.01,z$xmin-r,z$xmin-r-.01,0,0,0,0,
       z$xmax+r/sqrt(2), z$xmax+r, NA, 60)
y <- c(30,30,30,30,z$ymax+r,z$ymax+r+.01,z$ymin-r,z$ymin-r-.01,
       z$ymax+r/sqrt(2),z$ymax+r,30,30)
expected <- c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,NA,FALSE)
stopifnot(identical(base_pitch_in_zone(x,y,'inches'),expected),
          identical(base_pitch_in_zone(x/12,y/12),expected))
# Verify every glyph fits inside the full ball footprint, including its outline.
for (shape in c(0:6,8,15:25)) {
  for (symbol in c(TRUE,FALSE)) {
    v <- base_pitch_vertices(shape,symbol)
    stopifnot(max(sqrt(rowSums(v^2))) <= 1+1e-12)
  }
}
d <- data.frame(x=c(0,1),y=c(2,3),s=c('A','B'))
p <- ggplot(d,aes(x,y,shape=s,fill=s))+geom_pitch_location(size=100)+
  scale_shape_manual(values=c(21,24))+coord_fixed(xlim=c(-2,2),ylim=c(0,5))
b <- ggplot_build(p)$data[[1]]
poly <- base_pitch_polygon_data(b,r/12)
for (i in 1:2) {
  v <- poly[poly$group %in% c(i*2,i*2+1),]
  stopifnot(abs(diff(range(v$x))*12-2.90)<1e-10,
            abs(diff(range(v$y))*12-2.90)<1e-10)
}
# Both output engines must support the geometry; reversed scales retain size.
stopifnot(inherits(ggplotGrob(p),'gtable'))
for (plot in list(p,p+scale_x_reverse())) {
  traces <- plotly::ggplotly(plot)$x$data
  stopifnot(length(traces)==4L,
            all(vapply(traces,function(t) diff(range(t$x,na.rm=TRUE))*12<=2.9+1e-10,logical(1))),
            sum(vapply(traces,function(t) abs(diff(range(t$x,na.rm=TRUE))*12-2.9)<1e-10,logical(1)))==2L,
            all(vapply(traces,function(t) length(unique(t$fillcolor))==1L,logical(1))))
}
# The public discipline helpers must delegate to the same boundary rule.
for (file in c('WallyApps/PitchingApp/PitchingApp.R','WallyApps/HittingApp/HittingApp.R',
               'WallyApps/DefenseApp/DefenseApp.R','WallyApps/ScoutingApp/ScoutingApp.R')) {
  env <- new.env(parent=globalenv());env$to_num<-function(x)as.numeric(x)
  for (e in parse(file)) {
    if(is.call(e)&&identical(e[[1]],as.name('<-'))&&is.symbol(e[[2]])&&
       as.character(e[[2]]) %in% c('in_zone_inches','framing_in_zone','derive_zone_inches','zone_h_in','zone_s_in')) eval(e,env)
  }
  for (fn in c('in_zone_inches','framing_in_zone')) if(exists(fn,env,inherits=FALSE)) {
    stopifnot(identical(env[[fn]](x,y),expected %in% TRUE))
  }
  if(exists('derive_zone_inches',env,inherits=FALSE)) {
    stopifnot(identical(env$derive_zone_inches(data.frame(PlateLocSide=x/12,PlateLocHeight=y/12)),expected))
  }
}
cat('Ball boundary, corners, missing coordinates, every shape, PDF grobs and Plotly geometry passed.\n')
