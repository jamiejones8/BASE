# Compact single-page leaderboard export using the live values and cell colors.
base_write_trout_stat_sheet <- function(file, formatted, shaded, discipline, sort_by, seasons = character()) {
  stopifnot(nrow(formatted) > 0, identical(names(formatted), names(shaded)), nrow(formatted) == nrow(shaded))
  clean <- function(x) {
    x <- gsub("<[^>]*>", "", as.character(x))
    x <- gsub("&nbsp;", " ", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    x[is.na(x) | x %in% c("NA", "NaN", "Inf", "-Inf")] <- "-"
    x
  }
  fill <- function(x, default) {
    found <- regmatches(x, regexpr("rgba\\([^)]+\\)", x))
    if (!length(found) || !nzchar(found)) return(default)
    values <- suppressWarnings(as.numeric(strsplit(gsub("rgba\\(|\\)", "", found), ",")[[1]]))
    if (length(values) != 4 || any(!is.finite(values))) return(default)
    rgb <- values[1:3] * values[4] + 255 * (1 - values[4])
    grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }
  omitted <- if (identical(discipline, "Pitching")) {
    c("pRV", "2k Zone%", "Shut Down Inning%", "Max Velocity")
  } else {
    c("hRV", "90th EV", "EV>95%", "10-35*%", "10-35%", "GB%", "LD%", "FB%",
      "PU%", "Foul Ball%", "LD+FB%", "Airpull%")
  }
  columns <- setdiff(names(formatted), omitted)
  formatted <- formatted[columns]
  shaded <- shaded[columns]
  wrap <- function(label) {
    readable <- c("xwOBAcon" = "xwOBA\ncon", "Z-Contact%" = "Z-\nContact\n%",
                  "IZ-Whiff%" = "IZ-\nWhiff%", "Z-Swing%" = "Z-\nSwing%",
                  "IZWhiff%" = "IZ\nWhiff%")
    if (label %in% names(readable)) return(unname(readable[[label]]))
    words <- strsplit(label, " ", fixed = TRUE)[[1]]
    lines <- character(); line <- ""
    for (word in words) {
      pieces <- substring(word, seq(1, nchar(word), 7), pmin(seq(1, nchar(word), 7)+6, nchar(word)))
      for (piece in pieces) {
        if (nzchar(line) && nchar(paste(line, piece)) <= 7) line <- paste(line, piece)
        else { if (nzchar(line)) lines <- c(lines,line); line <- piece }
      }
    }
    paste(c(lines,line),collapse = "\n")
  }
  headers <- vapply(columns, wrap, character(1))
  header_lines <- max(lengths(strsplit(headers,"\n")))
  # Courier has equal character widths: numeric columns reserve 5-7 characters.
  slots <- pmax(5, pmin(7, nchar(columns)))
  names_clean <- clean(formatted[[1]])
  slots[1] <- max(nchar(c(columns[1],names_clean))) + 1
  slots[1] <- max(slots[1], 18)
  width <- 14; height <- 8.5; margin <- .3; gap <- .035
  font <- min(9, (width-2*margin-length(slots)*gap)*72/(.6*sum(slots)),
              (height-2*margin)*72/(1.65*nrow(formatted)+1.3*header_lines+1))
  widths <- slots * .6 * font/72 + gap
  centers <- margin + cumsum(widths)-widths/2
  row_height <- font*1.65/72
  header_height <- (header_lines*1.3+1)*font/72
  grDevices::pdf(file, width=width, height=height, useDingbats=FALSE, title="Trout Stat Sheet")
  on.exit(grDevices::dev.off(),add=TRUE)
  grid::grid.newpage()
  rect <- function(x,y,w,h,color) grid::grid.rect(x,y,w,h,default.units="inches",
    gp=grid::gpar(fill=color,col=NA))
  text <- function(label,x,y,size=font,just="centre",color="#241D1D") {
    grid::grid.text(label,x,y,default.units="inches",just=just,
      gp=grid::gpar(fontfamily="Courier",fontface="bold",fontsize=size,col=color))
  }
  y <- height-margin-header_height/2
  for (j in seq_along(columns)) {
    rect(centers[j],y,widths[j]-.012,header_height,"#EAE4DC")
    text(headers[j],centers[j],y,color="#501214")
  }
  y <- height-margin-header_height-row_height/2
  for (r in seq_len(nrow(formatted))) {
    for (j in seq_along(columns)) {
      column <- columns[j]
      background <- if (r %% 2) "#F7F5F2" else "#FFFFFF"
      rect(centers[j],y,widths[j]-.012,row_height,fill(as.character(shaded[[column]][r]),background))
      value <- clean(formatted[[column]][r])
      # Rare unusually long values fit without changing the column widths.
      size <- min(font,font*slots[j]/max(1,nchar(value)))
      text(value,if(j==1) margin+gap/2 else centers[j],y,size,
           just=if(j==1) "left" else "centre")
    }
    y <- y-row_height
  }
  invisible(file)
}
