# TrackMan plate coordinates are feet. A pitch is a 2.90-inch ball, not a point.
BASE_BALL_RADIUS_IN <- 1.45
base_pitch_zone <- function(units = c("feet", "inches")) {
  units <- match.arg(units)
  z <- data.frame(xmin = -8.5, xmax = 8.5, ymin = 1.60 * 12, ymax = 3.40 * 12)
  if (units == "feet") z <- z / 12
  z
}
base_pitch_in_zone <- function(x, y, units = c("feet", "inches")) {
  units <- match.arg(units)
  x <- suppressWarnings(as.numeric(as.character(x)))
  y <- suppressWarnings(as.numeric(as.character(y)))
  z <- base_pitch_zone(units)
  radius <- BASE_BALL_RADIUS_IN / if (units == "feet") 12 else 1
  dx <- pmax(z$xmin - x, 0, x - z$xmax)
  dy <- pmax(z$ymin - y, 0, y - z$ymax)
  # Nearest-point distance also handles corners, unlike an expanded rectangle.
  result <- dx^2 + dy^2 <= radius^2 + 1e-12
  result[!is.finite(x) | !is.finite(y)] <- NA
  result
}

# Polygon geometry is expressed in pitch-coordinate units, so both PDFs and
# ggplotly keep the correct ball footprint regardless of panel size. Every
# outcome symbol fits inside the ball diameter without drawing a second glyph.
base_pitch_vertices <- function(shape = 19, symbol = FALSE, inner = .80) {
  theta <- seq(0, 2*pi, length.out = 97)
  circle <- cbind(cos(theta), sin(theta))
  shape <- suppressWarnings(as.integer(shape))
  if (is.na(shape)) shape <- 19L
  if (shape %in% c(0, 15, 22)) p <- rbind(c(-1,-1),c(1,-1),c(1,1),c(-1,1),c(-1,-1))/sqrt(2)
  else if (shape %in% c(2, 17, 24, 6, 25)) {
    a <- c(pi/2, 7*pi/6, 11*pi/6, pi/2)
    p <- cbind(cos(a),sin(a)) * if (shape %in% c(6,25)) -1 else 1
  } else if (shape %in% c(5,18,23)) p <- rbind(c(0,1),c(1,0),c(0,-1),c(-1,0),c(0,1))
  else if (shape %in% c(3,4,8)) {
    arm <- if (inner < .80) .20 else .12
    p <- rbind(c(-arm,-1),c(arm,-1),c(arm,-arm),c(1,-arm),c(1,arm),c(arm,arm),c(arm,1),c(-arm,1),c(-arm,arm),c(-1,arm),c(-1,-arm),c(-arm,-arm),c(-arm,-1)) / sqrt(1+arm^2)
    if (shape %in% c(4,8)) p <- p %*% matrix(c(cos(pi/4),sin(pi/4),-sin(pi/4),cos(pi/4)),2)
  } else p <- circle
  # The border follows the selected shape, never an enclosing circle.
  if (!symbol) return(rbind(p, p[nrow(p):1,,drop=FALSE]*inner,p[1,]))
  p * inner
}
base_pitch_polygon_data <- function(data, radius, component = "both") {
  data <- data[is.finite(data$x) & is.finite(data$y), , drop=FALSE]
  if (!nrow(data)) return(data)
  do.call(rbind, lapply(seq_len(nrow(data)), function(i) {
    row <- data[i,,drop=FALSE]
    shape <- if ("shape" %in% names(row)) row$shape else 19
    parts <- switch(component, outline = FALSE, symbol = TRUE,
                    if (shape == 1) FALSE else c(FALSE,TRUE))
    do.call(rbind, lapply(parts, function(symbol) {
      inner <- if ("stroke" %in% names(row) && is.finite(row$stroke) && row$stroke > 1) .65 else .80
      v <- base_pitch_vertices(shape, symbol, inner) * radius
      out <- row[rep(1,nrow(v)),,drop=FALSE]
      out$x <- row$x + v[,1]; out$y <- row$y + v[,2]
      out$group <- i*2L + as.integer(symbol)
      colour <- if ("colour" %in% names(row)) row$colour else "black"
      fill <- if ("fill" %in% names(row)) row$fill else colour
      out$fill <- if (symbol && shape %in% 21:25 && !is.na(fill)) fill else colour
      out$colour <- NA_character_; out$linewidth <- 0
      out$alpha <- if (symbol && shape %in% c(0,1,2,5,6)) 0 else if ("alpha" %in% names(row)) row$alpha else NA_real_
      out
    }))
  }))
}
# Plotly dispatch receives scaled aesthetics but untransformed data coordinates.
geom2trace.GeomPitchLocation <- function(data, params, p) {
  radius <- if (is.null(params$radius)) BASE_BALL_RADIUS_IN / 12 else params$radius
  expanded <- base_pitch_polygon_data(data, radius, params$component)
  getFromNamespace("geom2trace.GeomPolygon", "plotly")(expanded, params, p)
}
geom_pitch_location <- function(mapping = NULL, data = NULL, ..., units = "feet",
                                coordinate_scale = 1, inherit.aes = TRUE,
                                show.legend = NA, na.rm = FALSE) {
  radius <- BASE_BALL_RADIUS_IN / if (units == "feet") 12 else 1
  if (requireNamespace("plotly", quietly=TRUE)) {
    registerS3method("geom2trace", "GeomPitchLocation", geom2trace.GeomPitchLocation,
                     envir=asNamespace("plotly"))
  }
  geom <- ggplot2::ggproto("GeomPitchLocation", ggplot2::GeomPoint,
    draw_key = function(data, params, size) {
      data$size <- pmin(data$size, 4)
      ggplot2::draw_key_point(data, params, size)
    },
    draw_panel = function(data, panel_params, coord, radius = BASE_BALL_RADIUS_IN/12,
                          component = "both", na.rm = FALSE) {
      expanded <- base_pitch_polygon_data(data, radius, component)
      ggplot2::GeomPolygon$draw_panel(expanded, panel_params, coord)
    })
  # Separate layers preserve outline/fill colours in Plotly, where a single
  # polygon trace can have only one fill colour.
  lapply(c("outline", "symbol"), function(component) {
    ggplot2::layer(mapping=mapping,data=data,stat="identity",geom=geom,
      position="identity",inherit.aes=inherit.aes,
      show.legend=if (component == "outline") FALSE else show.legend,
      params=c(list(...),list(radius=radius*coordinate_scale,
                             component=component,na.rm=na.rm)))
  })
}

# Fit bold pitch numbers inside the ball at draw time, when panel size is known.
# Fit the measured text rectangle to the actual filled shape.
makeContent.base_pitch_label <- function(x) {
  text <- grid::textGrob(x$label, gp = grid::gpar(fontsize=10, fontface="bold", col=x$colour))
  available_w <- grid::convertWidth(grid::unit(x$box_width,"npc"),"points",valueOnly=TRUE)
  available_h <- grid::convertHeight(grid::unit(x$box_height,"npc"),"points",valueOnly=TRUE)
  text_w <- grid::convertWidth(grid::grobWidth(text),"points",valueOnly=TRUE)
  text_h <- grid::convertHeight(grid::grobHeight(text),"points",valueOnly=TRUE)
  shape <- if (is.null(x$shape)) 21 else x$shape
  # Font devices can quantize sizes. Measure candidate glyphs on the actual
  # device instead of assuming that widths scale perfectly from a 10pt font.
  low <- 0; high <- 10 * 2 * max(available_w/text_w, available_h/text_h)
  for (iteration in seq_len(18)) {
    size <- (low + high)/2
    candidate <- grid::editGrob(text, gp=grid::gpar(fontsize=size,fontface="bold",col=x$colour))
    w <- grid::convertWidth(grid::grobWidth(candidate),"points",valueOnly=TRUE)/available_w
    h <- grid::convertHeight(grid::grobHeight(candidate),"points",valueOnly=TRUE)/available_h
    usage <- if (shape %in% c(22,15,0)) max(w,h)/sqrt(2) else if (shape %in% c(24,17,2,25,6)) {
      (h+w/(2*sqrt(3)))/1.5
    } else if (shape %in% c(23,18,5)) (w+h)/2 else sqrt(w*w+h*h)/2
    if (usage <= .80*.98) low <- size else high <- size
  }
  text <- grid::editGrob(text,gp=grid::gpar(fontsize=low,fontface="bold",col=x$colour))
  h <- grid::convertHeight(grid::grobHeight(text),"points",valueOnly=TRUE)/available_h
  offset <- if (shape %in% c(24,17,2,25,6)) (h/2-.80*.98/2) else 0
  if (shape %in% c(25,6)) offset <- -offset
  text <- grid::editGrob(text,x=grid::unit(x$x,"npc"),y=grid::unit(x$y+offset*x$box_height,"npc"))
  grid::setChildren(x,grid::gList(text))
}
geom_pitch_label <- function(mapping=NULL,data=NULL,...,units="feet",inherit.aes=TRUE,
                             show.legend=FALSE,na.rm=TRUE) {
  registerS3method("makeContent","base_pitch_label",makeContent.base_pitch_label,envir=asNamespace("grid"))
  radius <- BASE_BALL_RADIUS_IN / if(units=="feet")12 else 1
  geom <- ggplot2::ggproto("GeomPitchLabel",ggplot2::GeomText,
    default_aes=ggplot2::aes(colour="black",size=3.88,angle=0,hjust=.5,vjust=.5,alpha=NA,family="",fontface=1,lineheight=1.2,shape=21),
    draw_panel=function(data,panel_params,coord,na.rm=TRUE,radius=BASE_BALL_RADIUS_IN/12) {
      data <- data[is.finite(data$x)&is.finite(data$y)&!is.na(data$label),,drop=FALSE]
      if(!nrow(data))return(grid::nullGrob())
      center <- coord$transform(data,panel_params)
      edge <- data; edge$x <- edge$x + radius; edge$y <- edge$y + radius
      edge <- coord$transform(edge,panel_params)
      grobs <- lapply(seq_len(nrow(data)),function(i) grid::gTree(
        x=center$x[i],y=center$y[i],label=as.character(data$label[i]),colour=data$colour[i],
        shape=data$shape[i],box_width=abs(edge$x[i]-center$x[i]),
        box_height=abs(edge$y[i]-center$y[i]),cl="base_pitch_label"))
      grid::gTree(children=do.call(grid::gList,grobs))
    })
  ggplot2::layer(mapping=mapping,data=data,geom=geom,stat="identity",position="identity",
    inherit.aes=inherit.aes,show.legend=show.legend,
    params=c(list(...),list(radius=radius,na.rm=na.rm)))
}
