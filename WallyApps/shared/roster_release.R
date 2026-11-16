# Read the same saved profiles and imported measurements as Rosters & Recruiting.
# This reader never seeds, changes, or saves the roster state.
roster_release_state_path <- function(roster_root) {
  project <- normalizePath(file.path(roster_root, "..", ".."), mustWork = FALSE)
  Sys.getenv("TXST_DEPTH_STATE", unset = file.path(
    if (dir.exists("/base-data")) "/base-data/app_state" else file.path(project, "app_state"),
    "rosters", "depth-chart-state.rds"))
}

roster_release_current_year <- function(date = Sys.Date()) {
  as.integer(format(date, "%Y")) + as.integer(as.integer(format(date, "%m")) >= 8L)
}

roster_release_signature <- function(root, state_path = roster_release_state_path(root)) {
  paths <- c(file.path(root, "model.R"), file.path(root, "data", c("roster.csv", "pitcher-measurements.json", "depth-chart-state.rds")), state_path)
  info <- file.info(paths)
  paste(paths, info$size, as.numeric(info$mtime), collapse = "|")
}

roster_release_profiles <- function(root, state_path = roster_release_state_path(root)) {
  model <- new.env(parent = asNamespace("utils"))
  sys.source(file.path(root, "model.R"), envir = model)
  roster <- model$read_roster(file.path(root, "data", "roster.csv"))
  path <- if (file.exists(state_path)) state_path else file.path(root, "data", "depth-chart-state.rds")
  state <- readRDS(path)
  players <- c(roster, state$profiles)
  lapply(players, function(p) {
    edits <- if (!isTRUE(p$recruit)) state$playerDetails[[p$id]] else NULL
    # Preserve explicit nulls when a saved edit clears an imported measurement.
    for (key in names(edits)) p[key] <- edits[key]
    if (any(c("heightIn", "releaseHeightFt", "releaseSideFt") %in% names(edits))) p$releaseSource <- "Saved roster profile"
    p$removedFrom <- state$departures[[p$id]]
    p
  })
}

ROSTER_RELEASE_CLASSES <- c("Freshman", "Sophomore", "Junior", "Senior", "5th year")
ROSTER_RELEASE_COLORS <- stats::setNames(c("#ffffff", "#d8dce0", "#cce7b7", "#f7e28e", "#e7a3a0"), ROSTER_RELEASE_CLASSES)

roster_release_points <- function(players, year) {
  finite <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  rows <- lapply(players, function(p) {
    grade <- p$entryClass + as.integer(year) - p$entryYear
    if (length(grade) != 1L || !is.finite(grade) || !grade %in% 1:5 ||
        identical(p$status, "Recruiting") ||
        (!is.null(p$removedFrom) && year >= p$removedFrom) ||
        !any(unlist(p$positions) %in% c("RHP", "LHP", "P")) ||
        !finite(p$heightIn) || !finite(p$releaseHeightFt) ||
        !length(p$throws) || !p$throws %in% c("L", "R")) return(NULL)
    side <- if (finite(p$releaseSideFt) && p$releaseSideFt != 0) abs(p$releaseSideFt) else 1.5
    side <- side * if (p$throws == "L") -1 else 1
    anchor <- (p$heightIn - 10) / 12
    data.frame(id = p$id, Pitcher = paste(p$first, p$last), Class = ROSTER_RELEASE_CLASSES[grade],
      Status = p$status, HeightIn = p$heightIn, RelHeight = p$releaseHeightFt,
      RelSide = side, AnchorY = anchor, ArmAngle = atan2(p$releaseHeightFt - anchor, abs(side)) * 180 / pi,
      Source = if (is.null(p$releaseSource)) "Roster profile measurements" else p$releaseSource,
      SideDefault = !finite(p$releaseSideFt) || p$releaseSideFt == 0, stringsAsFactors = FALSE)
  })
  empty <- data.frame(id = character(), Pitcher = character(), Class = character(), Status = character(),
    HeightIn = numeric(), RelHeight = numeric(), RelSide = numeric(), AnchorY = numeric(), ArmAngle = numeric(),
    Source = character(), SideDefault = logical())
  if (all(vapply(rows, is.null, logical(1)))) empty else do.call(rbind, rows)
}

roster_release_plot <- function(points) {
  p <- plotly::plot_ly()
  if (!nrow(points)) return(p)
  xmax <- max(3, ceiling((max(abs(points$RelSide)) + .45) * 2) / 2)
  yrange <- c(floor((min(points$RelHeight, points$AnchorY) - .3) * 2) / 2,
    ceiling((max(points$RelHeight, points$AnchorY) + .3) * 2) / 2)
  # Arm rays use the exact shoulder anchor, class palette and status line style
  # of the roster client. Equal axis units preserve the inferred angle.
  for (i in seq_len(nrow(points))) {
    d <- points[i, ]
    dash <- switch(d$Status, Offered = "9px,6px", Committed = "3px,5px", "solid")
    for (back in c(TRUE, FALSE)) {
      p <- plotly::add_trace(p, x = c(0, d$RelSide), y = c(d$AnchorY, d$RelHeight),
        type = "scatter", mode = "lines", showlegend = FALSE, hoverinfo = "skip",
        line = list(color = if (back) "#56665c" else ROSTER_RELEASE_COLORS[[d$Class]],
          width = if (back) 5 else 3, dash = dash), opacity = if (back) .55 else 1)
    }
  }
  for (cls in ROSTER_RELEASE_CLASSES) {
    d <- points[points$Class == cls, , drop = FALSE]
    if (!nrow(d)) next
    hover <- paste0(htmltools::htmlEscape(d$Pitcher), "<br>", cls, " · ", d$Status,
      "<br>Release: ", sprintf("%.2f", d$RelHeight), " ft high, ", sprintf("%.2f", d$RelSide), " ft side",
      "<br>Height: ", d$HeightIn, " in · Inferred arm angle: ", sprintf("%.1f", d$ArmAngle), "°",
      "<br>", htmltools::htmlEscape(d$Source), ifelse(d$SideDefault, "<br>Release side uses 1.5 ft default", ""))
    p <- plotly::add_trace(p, x = d$RelSide, y = d$RelHeight, type = "scatter", mode = "markers", name = cls,
      marker = list(size = 12, color = ROSTER_RELEASE_COLORS[[cls]], line = list(color = "#34473c", width = 1.5)),
      text = hover, hoverinfo = "text", showlegend = TRUE)
  }
  placed <- list()
  annotations <- lapply(seq_len(nrow(points)), function(i) {
    d <- points[i, ]; scale <- 1000 / (2 * xmax)
    x <- d$RelSide * scale; y <- d$RelHeight * scale; shift <- 13
    while (any(vapply(placed, function(v) abs(v$x - x) < 120 && abs(v$y - (y + shift)) < 17, logical(1)))) shift <- shift + 18
    placed[[length(placed) + 1L]] <<- list(x = x, y = y + shift)
    list(x = d$RelSide, y = d$RelHeight, text = htmltools::htmlEscape(d$Pitcher),
      showarrow = shift > 15, arrowhead = 0, arrowcolor = "#9ba59f", arrowwidth = 1, standoff = 7,
      ax = 0, ay = -shift, xanchor = if (d$RelSide < 0) "right" else "left", yshift = if (shift > 15) 0 else shift,
      font = list(size = 11, color = "#303c35"), bgcolor = "rgba(255,255,255,.8)", borderpad = 1)
  })
  plotly::layout(p, xaxis = list(title = "Release side (ft) · LHP left / RHP right", range = c(-xmax, xmax),
      zeroline = TRUE, zerolinecolor = "#9ba59f", gridcolor = "#e1e5e2"),
    yaxis = list(title = "Release height (ft)", range = yrange, scaleanchor = "x", scaleratio = 1, gridcolor = "#e1e5e2"),
    annotations = annotations, plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
    legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 95, r = 85, b = 65, l = 85), hovermode = "closest")
}
