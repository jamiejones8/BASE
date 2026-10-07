# Complete leaderboard export, using the same formatted values and cell colors
# as the live table. Split wide tables into three readable panels per page.
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
  metrics <- names(formatted)[-1]
  panels <- split(metrics, ceiling(seq_along(metrics) / ceiling(length(metrics) / 3)))
  pages <- split(seq_len(nrow(formatted)), ceiling(seq_len(nrow(formatted)) / 10))
  grDevices::pdf(file, width = 14, height = 8.5, useDingbats = FALSE, title = "Trout Stat Sheet")
  on.exit(grDevices::dev.off(), add = TRUE)
  rect <- function(x, y, w, h, color) grid::grid.rect(x, y, w, h, default.units = "inches", gp = grid::gpar(fill = color, col = NA))
  text <- function(label, x, y, size = 8, color = "#241D1D", bold = FALSE, just = "centre") {
    grid::grid.text(label, x, y, default.units = "inches", just = just,
                    gp = grid::gpar(fontfamily = "Helvetica", fontsize = size, col = color,
                                   fontface = if (bold) "bold" else "plain"))
  }
  for (page in seq_along(pages)) {
    rows <- pages[[page]]
    grid::grid.newpage()
    rect(7, 8.08, 14, .84, "#501214")
    text("Trout Stat Sheet", .4, 8.13, 22, "white", TRUE, "left")
    text(paste0(discipline, " | Sorted by ", sort_by, " (highest first)"), .42, 7.79, 10, "#E8DCC3", FALSE, "left")
    context <- paste(as.character(seasons), collapse = " / ")
    text(context, 13.6, 8.1, 10, "white", FALSE, "right")
    text(paste0("Players ", min(rows), "-", max(rows), " of ", nrow(formatted)), 13.6, 7.8, 9, "#E8DCC3", FALSE, "right")
    y <- 7.43
    for (panel in seq_along(panels)) {
      columns <- c(names(formatted)[1], panels[[panel]])
      text(paste0("STATISTICS ", panel, " / ", length(panels)), .4, y, 8, "#501214", TRUE, "left")
      y <- y - .30
      widths <- c(2.1, rep((13.2 - 2.1) / (length(columns) - 1), length(columns) - 1))
      centers <- .4 + cumsum(widths) - widths / 2
      for (j in seq_along(columns)) {
        rect(centers[j], y, widths[j] - .018, .38, "#EAE4DC")
        label <- paste(strwrap(columns[j], width = 11), collapse = "\n")
        text(label, centers[j], y, 7.8, "#501214", TRUE)
      }
      y <- y - .27
      for (r in rows) {
        for (j in seq_along(columns)) {
          column <- columns[j]
          background <- if (match(r, rows) %% 2) "#F7F5F2" else "#FFFFFF"
          colored <- fill(as.character(shaded[[column]][r]), background)
          rect(centers[j], y, widths[j] - .018, .16, colored)
          value <- clean(formatted[[column]][r])
          font_size <- 7.8
          if (j == 1) {
            measured <- graphics::strwidth(value, units = "inches", cex = 7.8 / 12, font = 2)
            if (is.finite(measured) && measured > 1.98) font_size <- 7.8 * 1.98 / measured
          }
          text(value, if (j == 1) .46 else centers[j], y,
               font_size, bold = j == 1, just = if (j == 1) "left" else "centre")
        }
        y <- y - .16
      }
      y <- y - .22
    }
    text("Texas State Baseball | Player Development", .4, .22, 8, "#71665F", FALSE, "left")
    text(paste0("Page ", page, " of ", length(pages)), 13.6, .22, 8, "#71665F", FALSE, "right")
  }
  invisible(file)
}
