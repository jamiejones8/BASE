as_date_safely2 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.character(x)) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dx <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
      if (all(is.na(dx))) dx <- suppressWarnings(lubridate::ymd(x))
      if (!all(is.na(dx))) return(as.Date(dx))
    }
  }
  suppressWarnings(as.Date(x))
}

load_season_phases <- function(path = config_path("season_phases.yml")) {
  cfg <- read_yaml_config(path)
  phases <- cfg$phases %||% list()
  if (length(phases) == 0) {
    return(data.frame(name = character(), start = as.Date(character()), end = as.Date(character())))
  }
  data.frame(
    name = vapply(phases, function(x) as.character(x$name %||% "Unassigned"), character(1)),
    start = as.Date(vapply(phases, function(x) as.character(x$start %||% NA_character_), character(1))),
    end = as.Date(vapply(phases, function(x) as.character(x$end %||% NA_character_), character(1))),
    manual_assignment_only = vapply(phases, function(x) isTRUE(x$manual_assignment_only), logical(1)),
    stringsAsFactors = FALSE
  )
}

assign_season_phase <- function(date, phases = load_season_phases()) {
  d <- as_date_safely2(date)
  out <- rep("Unassigned", length(d))
  if (!is.data.frame(phases) || nrow(phases) == 0) return(out)
  for (i in seq_len(nrow(phases))) {
    if (isTRUE(phases$manual_assignment_only[[i]])) next
    hit <- !is.na(d) & d >= phases$start[[i]] & d <= phases$end[[i]]
    out[hit] <- phases$name[[i]]
  }
  out
}

