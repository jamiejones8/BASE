# Fit long table headers to their actual PDF or browser panel width.
makeContent.base_report_table_fit <- function(x) {
  tg <- x$table
  for (i in seq_along(tg$grobs)) {
    g <- tg$grobs[[i]]
    if (!inherits(g,"text")) next
    span <- seq.int(tg$layout$l[i],tg$layout$r[i])
    available <- grid::convertWidth(sum(tg$widths[span]),"pt",valueOnly=TRUE)-4
    measured <- grid::convertWidth(grid::grobWidth(g),"pt",valueOnly=TRUE)
    if (is.finite(measured) && measured>available && available>0) {
      gp <- g$gp
      gp$cex <- (if (is.null(gp$cex)) 1 else gp$cex) * available/measured
      tg$grobs[[i]] <- grid::editGrob(g,gp=gp)
    }
  }
  grid::setChildren(x,grid::gList(tg))
}
# Extract the styled table from ggpubr's plotting wrapper, then put its title
# and column headings in the same gtable. No independently centered panels.
base_report_table_grob <- function(x) {
  if (inherits(x, 'gtable') && any(x$layout$name == 'core-fg')) return(x)
  if (inherits(x, 'ggplot')) {
    for (layer in x$layers) {
      found <- base_report_table_grob(layer$geom_params$grob)
      if (!is.null(found)) return(found)
    }
  }
  if (inherits(x, 'gTree')) for (child in x$children) {
    found <- base_report_table_grob(child)
    if (!is.null(found)) return(found)
  }
  NULL
}
base_titled_report_table <- function(tbl, title) {
  tg <- base_report_table_grob(tbl)
  if (is.null(tg)) {
    if (!inherits(tbl, "ggplot")) tbl <- cowplot::ggdraw() + cowplot::draw_grob(
      if (inherits(tbl,"grob")) tbl else grid::textGrob("No data"))
    strip <- cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill='#501214',col=NA)),
      grid::textGrob(title,gp=grid::gpar(col='#B4975A',fontface='bold',fontsize=11))))
    return((strip / (tbl + ggplot2::theme(plot.margin=ggplot2::margin(0,0,0,0)))) +
      patchwork::plot_layout(heights=grid::unit(c(21,1),c('pt','null'))))
  }
  widths <- grid::convertWidth(tg$widths, 'in', valueOnly=TRUE)
  tg$widths <- grid::unit(widths/sum(widths), 'npc')
  tg <- gtable::gtable_add_rows(tg, grid::unit(21, 'pt'), pos=0)
  tg <- gtable::gtable_add_grob(tg, grid::rectGrob(gp=grid::gpar(fill='#501214',col=NA)),
                               t=1,l=1,r=length(tg$widths),name='section-title-bg')
  tg <- gtable::gtable_add_grob(tg, grid::textGrob(title,gp=grid::gpar(col='#B4975A',fontface='bold',fontsize=11)),
                               t=1,l=1,r=length(tg$widths),name='section-title-text')
  # The table begins at the top of its allotted report area; spare space is below.
  registerS3method('makeContent','base_report_table_fit',makeContent.base_report_table_fit,envir=asNamespace('grid'))
  fitted <- grid::gTree(table=tg,cl='base_report_table_fit')
  wrapper <- grid::grobTree(fitted, vp=grid::viewport(y=1,just='top',height=sum(tg$heights)))
  cowplot::ggdraw() + cowplot::draw_grob(wrapper)
}

# Identical physical bounding boxes prevent tall opponent marks crossing the header.
base_bounded_logo_grob <- function(image,x=.98,y=.98) {
  aspect <- dim(image)[2]/dim(image)[1]
  width <- min(.62,.62*aspect);height<-width/aspect
  grid::rasterGrob(image,x=x,y=y,width=grid::unit(width,"in"),height=grid::unit(height,"in"),
    just=c("right","top"),interpolate=TRUE)
}

base_movement_trend_favorable <- function(pitch_type, metric, delta) {
  lower <- (metric == "HB" && pitch_type == "Fastball") ||
    (metric == "IVB" && pitch_type %in% c("Sinker", "Slider", "Curveball", "Sweeper", "Changeup", "Splitter")) ||
    (metric == "Spin" && pitch_type %in% c("Changeup", "Splitter"))
  if (lower) delta < 0 else delta > 0
}

base_movement_report_table <- function(df) {
  trends <- attr(df,"movement_trends")
  tbl <- gridExtra::tableGrob(df,rows=NULL,theme=gridExtra::ttheme_minimal(base_size=9,
    padding=grid::unit(c(5,5),"pt"),core=list(bg_params=list(fill=rep(c("white","#F8F3EA"),length.out=nrow(df)),col="#D0D0D0"))))
  if(is.null(trends)) return(tbl)
  for(j in seq_len(ncol(trends))) for(i in seq_len(nrow(df))) {
    delta <- trends[i,j];if(!is.finite(delta)) next
    digits <- if(colnames(trends)[j]=="Spin") 0 else 1
    delta <- round(delta,digits);if(delta==0) next
    col <- match(colnames(trends)[j],names(df))
    index <- which(tbl$layout$name=="core-fg" & tbl$layout$l==col & tbl$layout$t==i+1)
    if(!length(index)) next
    value <- as.character(df[i,col]);change<-formatC(abs(delta),format="f",digits=digits)
    color <- if(base_movement_trend_favorable(as.character(df$PitchType[i]), colnames(trends)[j], delta)) "#25813E" else "#C83232"
    main<-grid::textGrob(value,gp=grid::gpar(fontsize=8))
    small<-grid::textGrob(change,gp=grid::gpar(fontsize=6,col=color))
    total<-grid::grobWidth(main)+grid::grobWidth(small)+grid::unit(9,"pt")
    start<-(grid::unit(1,"npc")-total)/2
    main<-grid::editGrob(main,x=start,just="left")
    tx<-start+grid::grobWidth(main)+grid::unit(4,"pt")
    triangle<-grid::polygonGrob(x=tx+grid::unit(c(-2.5,2.5,0),"pt"),
      y=grid::unit(.5,"npc")+grid::unit(if(delta>0)c(-2,-2,2) else c(2,2,-2),"pt"),gp=grid::gpar(fill=color,col=NA))
    small<-grid::editGrob(small,x=tx+grid::unit(4,"pt"),just="left")
    tbl$grobs[[index]]<-grid::grobTree(main,triangle,small)
    tbl$widths[col]<-grid::unit.pmax(tbl$widths[col],total+grid::unit(5,"pt"))
  }
  tbl
}

base_team_rankings_grob <- function(rows, population = "All Offenses") {
  n<-nrow(rows)
  grobs<-list(grid::textGrob("Team Rankings",x=.04,y=.97,just="left",gp=grid::gpar(fontface="bold",fontsize=12,col="#501214")),
    grid::textGrob(paste("D1 Percentiles |", population),x=.04,y=.93,just="left",gp=grid::gpar(fontsize=7,col="#555555")))
  if(!n) return(do.call(grid::grobTree,grobs))
  for(i in seq_len(n)) {
    y<-.86-(i-1)*min(.065,.78/n);p<-rows$percentile[i]
    color<-if(!is.finite(p)) "#BFC9CA" else if(p>=75) "#488E3A" else if(p>=50) "#87B78D" else if(p>=25) "#E38484" else "#D94343"
    x<-.38+.39*ifelse(is.finite(p),p/100,0)
    grobs<-c(grobs,list(
      grid::textGrob(rows$label[i],x=.04,y=y,just="left",gp=grid::gpar(fontsize=7)),
      grid::segmentsGrob(x0=.38,x1=.77,y0=y,y1=y,gp=grid::gpar(col="#D6E5E7",lwd=9)),
      grid::segmentsGrob(x0=.38,x1=x,y0=y,y1=y,gp=grid::gpar(col=color,lwd=9)),
      grid::segmentsGrob(x0=.575,x1=.575,y0=y-.012,y1=y+.012,gp=grid::gpar(col="white",lwd=1)),
      grid::circleGrob(x=x,y=y,r=grid::unit(6,"pt"),gp=grid::gpar(fill=color,col=NA)),
      grid::textGrob(if(is.finite(p)) round(p) else "--",x=x,y=y,gp=grid::gpar(fontsize=6,col="white",fontface="bold")),
      grid::textGrob(if(is.finite(rows$value[i])) rows$display[i] else "--",x=.97,y=y,just="right",gp=grid::gpar(fontsize=7))
    ))
  }
  do.call(grid::grobTree,grobs)
}

base_compact_table_widths <- function(tbl,padding=8) {
  for(col in seq_along(tbl$widths)) {
    cells <- which(tbl$layout$l==col & tbl$layout$name %in% c("core-fg","colhead-fg"))
    if(length(cells)) tbl$widths[col] <- do.call(grid::unit.pmax,lapply(tbl$grobs[cells],grid::grobWidth))+grid::unit(padding,"pt")
  }
  tbl
}
