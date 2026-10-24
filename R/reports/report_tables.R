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
