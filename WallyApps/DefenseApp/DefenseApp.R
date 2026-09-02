suppressPackageStartupMessages({
  library(shiny)
  if (!isTRUE(get0("BASE_DEFENSE_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
    library(tidyverse)
  }
  library(bslib)
  library(htmltools)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(readr)
  library(shinycssloaders)
})

# -------------------- App paths / assets --------------------
app_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) "")
app_dir <- if (nzchar(app_file) && grepl("DefenseApp\\.R$", app_file)) dirname(app_file) else getwd()
if (!isTRUE(get0("BASE_DEFENSE_EMBEDDED", inherits = FALSE, ifnotfound = FALSE)) &&
    dir.exists(file.path(app_dir, "www"))) {
  shiny::addResourcePath("static", normalizePath(file.path(app_dir, "www"), mustWork = TRUE))
}

# -------------------- Shared helpers --------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

to_num <- function(x) {
  if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
}

to_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

safe_ratio <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

fmt_decimal <- function(x, digits = 3) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

norm_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

find_col <- function(d, candidates) {
  if (is.null(d) || !length(names(d))) return(NA_character_)
  keys <- norm_key(names(d))
  cands <- norm_key(candidates)
  hit <- match(cands, keys)
  hit <- hit[!is.na(hit)]
  if (length(hit)) names(d)[hit[[1]]] else NA_character_
}

col_chr <- function(d, candidates, default = NA_character_) {
  nm <- find_col(d, candidates)
  if (is.na(nm)) rep(default, nrow(d)) else to_chr(d[[nm]])
}

col_num <- function(d, candidates, default = NA_real_) {
  nm <- find_col(d, candidates)
  if (is.na(nm)) rep(default, nrow(d)) else to_num(d[[nm]])
}

col_date <- function(d, candidates) {
  raw <- col_chr(d, candidates, default = NA_character_)
  out <- suppressWarnings(as.Date(raw))
  need <- is.na(out) & !is.na(raw) & nzchar(raw)
  if (any(need)) {
    out[need] <- suppressWarnings(as.Date(raw[need], format = "%m/%d/%Y"))
  }
  need <- is.na(out) & !is.na(raw) & nzchar(raw)
  if (any(need)) {
    out[need] <- suppressWarnings(as.Date(raw[need], format = "%m/%d/%y"))
  }
  out
}

has_text <- function(x, pattern) {
  grepl(pattern, tolower(to_chr(x)), perl = TRUE)
}

infer_season_group_from_file <- function(f) {
  f <- tolower(to_chr(f))
  dplyr::case_when(
    grepl("s25|2025[_ -]?season|season[_ -]?2025", f) ~ "S25",
    grepl("f25|2025[_ -]?fall|fall[_ -]?2025", f) ~ "F25",
    grepl("sq26|2026[_ -]?squads|squads[_ -]?2026", f) ~ "SQ26",
    grepl("s26|2026[_ -]?season|season[_ -]?2026|defense2026|2026", f) ~ "S26",
    TRUE ~ "UNK"
  )
}

season_label <- c(
  "S25" = "2025 Season",
  "F25" = "2025 Fall",
  "SQ26" = "2026 Squads",
  "S26" = "2026 Season",
  "UNK" = "Unassigned"
)

normalize_season <- function(x, source_file) {
  x <- toupper(trimws(to_chr(x)))
  x <- dplyr::case_when(
    x %in% c("S25", "2025 SEASON", "2025_SEASON") ~ "S25",
    x %in% c("F25", "2025 FALL", "2025_FALL") ~ "F25",
    x %in% c("SQ26", "2026 SQUADS", "2026_SQUADS") ~ "SQ26",
    x %in% c("S26", "2026 SEASON", "2026_SEASON") ~ "S26",
    TRUE ~ x
  )
  missing <- is.na(x) | !nzchar(x)
  if (any(missing)) x[missing] <- infer_season_group_from_file(source_file[missing])
  x[!nzchar(x)] <- "UNK"
  x
}

# -------------------- Data load --------------------
load_defense_data <- function() {
  if (exists("BASE_DEFENSE_DATA", inherits = TRUE)) {
    injected <- get("BASE_DEFENSE_DATA", inherits = TRUE)
    if (is.data.frame(injected)) return(tibble::as_tibble(injected))
  }
  data_dir <- Sys.getenv("DATA_DIR", unset = "")
  candidate_dirs <- unique(Filter(
    function(p) nzchar(p) && dir.exists(p),
    c(
      if (nzchar(data_dir)) normalizePath(data_dir, winslash = "/", mustWork = FALSE) else "",
      file.path(app_dir, "data"),
      file.path(app_dir, "Data"),
      "data",
      "Data"
    )
  ))
  candidate_dirs <- unique(normalizePath(candidate_dirs, winslash = "/", mustWork = FALSE))

  rds_candidates <- unique(Filter(nzchar, c(
    if (nzchar(data_dir)) file.path(normalizePath(data_dir, mustWork = FALSE), "data.rds") else "",
    file.path(app_dir, "data", "data.rds"),
    "data/data.rds"
  )))
  rds_path <- rds_candidates[file.exists(rds_candidates)][1]
  if (!is.na(rds_path) && nzchar(rds_path)) return(readRDS(rds_path))

  csvs <- unlist(lapply(candidate_dirs, function(d) list.files(d, pattern = "\\.csv$", full.names = TRUE)))
  csvs <- unique(normalizePath(csvs, winslash = "/", mustWork = FALSE))
  csvs <- csvs[!grepl("^Catchers\\s*-", basename(csvs), ignore.case = TRUE)]
  csvs <- csvs[!grepl("BattedBalls", basename(csvs), ignore.case = TRUE)]
  if (!length(csvs)) {
    warning("No defense CSV files found yet. Add data to DefenseApp/data.")
    return(tibble::tibble(source_file = character(), row_in_file = integer()))
  }

  purrr::map_dfr(csvs, function(path) {
    tryCatch(
      readr::read_csv(
        path,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      ) %>%
        dplyr::mutate(source_file = basename(path), row_in_file = dplyr::row_number()),
      error = function(e) {
        warning("Failed to read ", path, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )
  }) %>%
    readr::type_convert(col_types = readr::cols(.default = readr::col_guess()))
}

load_batted_ball_data <- function() {
  if (exists("BASE_DEFENSE_BATTED_DATA", inherits = TRUE)) {
    injected <- get("BASE_DEFENSE_BATTED_DATA", inherits = TRUE)
    if (is.data.frame(injected)) return(tibble::as_tibble(injected))
  }
  data_dir <- Sys.getenv("DATA_DIR", unset = "")
  candidate_dirs <- unique(Filter(
    function(p) nzchar(p) && dir.exists(p),
    c(
      if (nzchar(data_dir)) normalizePath(data_dir, winslash = "/", mustWork = FALSE) else "",
      file.path(app_dir, "data"),
      file.path(app_dir, "Data"),
      "data",
      "Data"
    )
  ))
  candidate_dirs <- unique(normalizePath(candidate_dirs, winslash = "/", mustWork = FALSE))
  csvs <- unlist(lapply(candidate_dirs, function(d) list.files(d, pattern = "BattedBalls.*\\.csv$", full.names = TRUE, ignore.case = TRUE)))
  csvs <- unique(normalizePath(csvs, winslash = "/", mustWork = FALSE))
  if (!length(csvs)) return(tibble::tibble())

  purrr::map_dfr(csvs, function(path) {
    tryCatch(
      readr::read_csv(
        path,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      ) %>%
        dplyr::mutate(batted_ball_source_file = basename(path)),
      error = function(e) {
        warning("Failed to read ", path, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )
  }) %>%
    readr::type_convert(col_types = readr::cols(.default = readr::col_guess()))
}

attach_batted_ball_data <- function(defense_raw, batted_raw) {
  if (is.null(defense_raw) || !is.data.frame(defense_raw) || !nrow(defense_raw)) return(defense_raw)
  if (is.null(batted_raw) || !is.data.frame(batted_raw) || !nrow(batted_raw)) return(defense_raw)

  possible_keys <- list(
    "PitchUID",
    "PlayID",
    c("GameUID", "PitchNo"),
    c("GameID", "PitchNo")
  )
  key_idx <- which(vapply(possible_keys, function(k) all(k %in% names(defense_raw)) && all(k %in% names(batted_raw)), logical(1)))
  if (!length(key_idx)) return(defense_raw)
  key <- possible_keys[[key_idx[[1]]]]

  enrich_cols <- intersect(
    c(
      key,
      "TaggedHitType", "AutoHitType", "ExitSpeed", "Angle", "Direction", "HitSpinRate",
      "PositionAt110X", "PositionAt110Y", "PositionAt110Z", "Distance", "LastTrackedDistance",
      "Bearing", "HangTime", "MaxHeight", "MeasuredDuration",
      "ContactPositionX", "ContactPositionY", "ContactPositionZ",
      "HitTrajectoryXc0", "HitTrajectoryXc1", "HitTrajectoryXc2", "HitTrajectoryXc3", "HitTrajectoryXc4",
      "HitTrajectoryXc5", "HitTrajectoryXc6", "HitTrajectoryXc7", "HitTrajectoryXc8",
      "HitTrajectoryYc0", "HitTrajectoryYc1", "HitTrajectoryYc2", "HitTrajectoryYc3", "HitTrajectoryYc4",
      "HitTrajectoryYc5", "HitTrajectoryYc6", "HitTrajectoryYc7", "HitTrajectoryYc8",
      "HitTrajectoryZc0", "HitTrajectoryZc1", "HitTrajectoryZc2", "HitTrajectoryZc3", "HitTrajectoryZc4",
      "HitTrajectoryZc5", "HitTrajectoryZc6", "HitTrajectoryZc7", "HitTrajectoryZc8",
      "batted_ball_source_file"
    ),
    names(batted_raw)
  )
  batted_enriched <- batted_raw %>%
    dplyr::select(dplyr::all_of(enrich_cols)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(key)), .keep_all = TRUE) %>%
    dplyr::mutate(batted_ball_matched = TRUE)

  defense_raw %>%
    dplyr::left_join(batted_enriched, by = key)
}

load_catching_data <- function() {
  if (exists("BASE_CATCHING_DATA", inherits = TRUE)) {
    injected <- get("BASE_CATCHING_DATA", inherits = TRUE)
    if (is.data.frame(injected)) return(tibble::as_tibble(injected))
  }
  data_dir <- Sys.getenv("DATA_DIR", unset = "")
  candidate_dirs <- unique(Filter(
    function(p) nzchar(p) && dir.exists(p),
    c(
      if (nzchar(data_dir)) normalizePath(data_dir, winslash = "/", mustWork = FALSE) else "",
      file.path(app_dir, "data"),
      file.path(app_dir, "Data"),
      "data",
      "Data"
    )
  ))
  candidate_dirs <- unique(normalizePath(candidate_dirs, winslash = "/", mustWork = FALSE))
  csvs <- unlist(lapply(candidate_dirs, function(d) list.files(d, pattern = "^Catchers\\s*-.*\\.csv$", full.names = TRUE, ignore.case = TRUE)))
  csvs <- unique(normalizePath(csvs, winslash = "/", mustWork = FALSE))
  if (!length(csvs)) {
    return(tibble::tibble(source_file = character(), row_in_file = integer()))
  }

  purrr::map_dfr(csvs, function(path) {
    tryCatch(
      readr::read_csv(
        path,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      ) %>%
        dplyr::mutate(source_file = basename(path), row_in_file = dplyr::row_number()),
      error = function(e) {
        warning("Failed to read ", path, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )
  }) %>%
    readr::type_convert(col_types = readr::cols(.default = readr::col_guess()))
}

load_d1_catcher_framing_baseline <- function() {
  if (exists("BASE_CATCHER_FRAMING_BASELINE", inherits = TRUE)) {
    injected <- get("BASE_CATCHER_FRAMING_BASELINE", inherits = TRUE)
    if (is.data.frame(injected)) return(tibble::as_tibble(injected))
  }
  path <- file.path(app_dir, "data", "d1_catcher_framing_metrics.csv")
  if (!file.exists(path)) {
    warning("D1 catcher framing baseline not found: ", path)
    return(tibble::tibble(
      metric_id = character(), metric = character(), Catcher = character(),
      value = numeric(), chances = integer(), numerator = numeric(),
      metric_order = integer(), better = character()
    ))
  }
  readr::read_csv(
    path,
    col_types = readr::cols(
      metric_id = readr::col_character(),
      metric = readr::col_character(),
      Catcher = readr::col_character(),
      value = readr::col_double(),
      chances = readr::col_integer(),
      numerator = readr::col_double(),
      metric_order = readr::col_integer(),
      better = readr::col_character()
    ),
    show_col_types = FALSE
  )
}

name_display <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- trimws(s)
    s <- gsub("\\s+", " ", s)
    if (grepl(",", s)) {
      parts <- strsplit(s, ",\\s*")[[1]]
      if (length(parts) >= 2) return(paste0(parts[2], " ", parts[1]))
    }
    s
  }, "", USE.NAMES = FALSE)
}

name_norm <- function(x) tolower(name_display(x))

canon_pitch_call <- function(x) {
  y <- gsub("[^A-Za-z]", "", tolower(to_chr(x)))
  dplyr::case_when(
    y %in% c("strikecalled", "calledstrike") ~ "StrikeCalled",
    y %in% c("ballcalled", "ball", "ballinthedirt", "ballintentional") ~ "BallCalled",
    y %in% c("strikeswinging", "swingingstrike") ~ "StrikeSwinging",
    y %in% c("foulball", "foul", "foulballfieldable", "foulballnotfieldable") ~ "FoulBall",
    y %in% c("inplay", "inplayout", "inplaynoout") ~ "InPlay",
    TRUE ~ as.character(x)
  )
}

normalize_position <- function(x) {
  y <- toupper(trimws(as.character(x)))
  key <- gsub("[^A-Z0-9]", "", y)
  dplyr::case_when(
    key %in% c("1", "1B", "FIRST", "FIRSTBASE", "FIRSTBASEMAN") ~ "1B",
    key %in% c("2", "2B", "SECOND", "SECONDBASE", "SECONDBASEMAN") ~ "2B",
    key %in% c("3", "3B", "THIRD", "THIRDBASE", "THIRDBASEMAN") ~ "3B",
    key %in% c("6", "SS", "SHORT", "SHORTSTOP") ~ "SS",
    key %in% c("7", "LF", "LEFT", "LEFTFIELD", "LEFTFIELDER") ~ "LF",
    key %in% c("8", "CF", "CENTER", "CENTERFIELD", "CENTERFIELDER", "CENTRE", "CENTREFIELD") ~ "CF",
    key %in% c("9", "RF", "RIGHT", "RIGHTFIELD", "RIGHTFIELDER") ~ "RF",
    key %in% c("P", "PITCHER") ~ "P",
    key %in% c("C", "CATCHER") ~ "C",
    TRUE ~ y
  )
}

DEFENSE_POSITIONS <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")
TRACKED_POSITIONS <- c("1B", "2B", "3B", "SS", "LF", "CF", "RF")
INFIELD_POSITIONS <- c("1B", "2B", "3B", "SS")
OUTFIELD_POSITIONS <- c("LF", "CF", "RF")
SHALLOW_AIR_DISTANCE_FT <- 180

is_bip_defense <- function(pitch_call, play_result = NULL) {
  pc <- canon_pitch_call(pitch_call)
  pr <- tolower(to_chr(play_result %||% rep("", length(pc))))
  pc %in% "InPlay" |
    grepl("single|double|triple|home\\s*run|homerun|\\bhr\\b|groundout|flyout|lineout|popout|fielderschoice|error|sacrifice", pr, perl = TRUE)
}

included_bip_result <- function(pitch_call, play_result = NULL) {
  pr <- tolower(to_chr(play_result %||% rep("", length(pitch_call))))
  is_bip_defense(pitch_call, play_result) &
    !grepl("home\\s*run|homerun|\\bhr\\b", pr, perl = TRUE) &
    (
      !nzchar(pr) |
        grepl("single|double|triple|home\\s*run|homerun|\\bhr\\b|\\bout\\b|groundout|flyout|lineout|popout|forceout|fielderschoice|sacrifice|error|reached", pr, perl = TRUE)
    )
}

play_result_bucket <- function(play_result, made_play = NULL) {
  pr <- tolower(to_chr(play_result))
  made <- made_play %||% rep(NA, length(pr))
  dplyr::case_when(
    grepl("\\berror\\b", pr, perl = TRUE) ~ "Error",
    grepl("single|double|triple|home\\s*run|homerun|\\bhr\\b", pr, perl = TRUE) ~ "Hit",
    made %in% TRUE | grepl("\\bout\\b|groundout|flyout|lineout|popout|forceout|caught|sacrifice", pr, perl = TRUE) ~ "Out",
    TRUE ~ "Other"
  )
}

batted_ball_bucket <- function(hit_type, launch_angle = NULL) {
  ht <- tolower(to_chr(hit_type))
  la <- if (is.null(launch_angle)) rep(NA_real_, length(ht)) else to_num(launch_angle)
  dplyr::case_when(
    grepl("ground|gb|chopper|bunt", ht, perl = TRUE) ~ "Ground",
    grepl("pop|popup", ht, perl = TRUE) ~ "Popup",
    grepl("fly|fb", ht, perl = TRUE) ~ "Fly",
    grepl("line|ld|liner", ht, perl = TRUE) ~ "Line",
    is.finite(la) & la < 10 ~ "Ground",
    is.finite(la) & la < 25 ~ "Line",
    is.finite(la) & la < 50 ~ "Fly",
    is.finite(la) ~ "Popup",
    TRUE ~ "Unknown"
  )
}

as_logical_flag <- function(x, default = FALSE) {
  y <- tolower(to_chr(x))
  out <- dplyr::case_when(
    y %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    y %in% c("false", "f", "no", "n", "0") ~ FALSE,
    TRUE ~ default
  )
  out
}

made_play_from_result <- function(play_result, outs_on_play = NULL) {
  pr <- tolower(to_chr(play_result))
  outs <- if (is.null(outs_on_play)) rep(NA_real_, length(pr)) else to_num(outs_on_play)
  dplyr::case_when(
    is.finite(outs) & outs > 0 ~ TRUE,
    grepl("\\bout\\b|groundout|flyout|lineout|popout|forceout|caught|sacrifice", pr, perl = TRUE) ~ TRUE,
    grepl("single|double|triple|home\\s*run|homerun|\\bhr\\b|error|safe|reached", pr, perl = TRUE) ~ FALSE,
    TRUE ~ NA
  )
}

clean_field_from_result <- function(play_result) {
  pr <- tolower(to_chr(play_result))
  dplyr::case_when(
    grepl("\\berror\\b", pr, perl = TRUE) ~ FALSE,
    nzchar(pr) & !pr %in% c("undefined", "na") ~ TRUE,
    TRUE ~ NA
  )
}

coalesce_num_cols <- function(d, candidates) {
  cands <- intersect(candidates, names(d))
  if (!length(cands)) return(rep(NA_real_, nrow(d)))
  vals <- lapply(cands, function(nm) to_num(d[[nm]]))
  out <- vals[[1]]
  if (length(vals) > 1) for (idx in 2:length(vals)) out <- dplyr::coalesce(out, vals[[idx]])
  out
}

coalesce_chr_cols <- function(d, candidates) {
  cands <- intersect(candidates, names(d))
  if (!length(cands)) return(rep("", nrow(d)))
  vals <- lapply(cands, function(nm) to_chr(d[[nm]]))
  out <- vals[[1]]
  if (length(vals) > 1) {
    for (idx in 2:length(vals)) out <- ifelse(nzchar(out), out, vals[[idx]])
  }
  out
}

ball_xy_from_raw <- function(raw) {
  x <- coalesce_num_cols(raw, c(
    "BallX", "LandingX", "HitX", "ContactX", "ProjectedX", "FieldedBallX",
    "BattedBallX", "HitLandingX"
  ))
  y <- coalesce_num_cols(raw, c(
    "BallY", "LandingY", "HitY", "ContactY", "ProjectedY", "FieldedBallY",
    "BattedBallY", "HitLandingY"
  ))

  distance <- coalesce_num_cols(raw, c("Distance", "HitDistance", "LandingDistance", "ProjectedDistance"))
  bearing <- coalesce_num_cols(raw, c("Bearing", "Direction", "HitBearing", "BearingDeg", "BearingDegrees", "SprayAngle"))
  can_bearing <- is.finite(distance) & is.finite(bearing)
  if (any(can_bearing)) {
    rad <- bearing[can_bearing] * pi / 180
    x[can_bearing] <- distance[can_bearing] * cos(rad)
    y[can_bearing] <- distance[can_bearing] * sin(rad)
  }

  list(x = x, y = y)
}

bearing_from_ball_xy <- function(ball_x, ball_y) {
  out <- atan2(ball_y, ball_x) * 180 / pi
  out[!(is.finite(ball_x) & is.finite(ball_y))] <- NA_real_
  out
}

trajectory_first_ground_distance <- function(raw) {
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(numeric(0))
  coef_matrix <- function(axis) {
    cols <- paste0("HitTrajectory", axis, "c", 0:8)
    if (!any(cols %in% names(raw))) return(NULL)
    mat <- sapply(cols, function(nm) if (nm %in% names(raw)) to_num(raw[[nm]]) else rep(NA_real_, nrow(raw)))
    if (is.null(dim(mat))) mat <- matrix(mat, ncol = length(cols))
    mat
  }
  xmat <- coef_matrix("X")
  ymat <- coef_matrix("Y")
  zmat <- coef_matrix("Z")
  if (is.null(xmat) || is.null(ymat) || is.null(zmat)) return(rep(NA_real_, nrow(raw)))

  t_grid <- seq(0, 5, by = 0.01)
  powers <- outer(t_grid, 0:8, `^`)
  eval_row <- function(coefs) {
    coefs[!is.finite(coefs)] <- 0
    as.vector(powers %*% coefs)
  }

  vapply(seq_len(nrow(raw)), function(i) {
    zc <- zmat[i, ]
    if (!any(is.finite(zc))) return(NA_real_)
    z <- eval_row(zc)
    if (!any(is.finite(z))) return(NA_real_)
    above_idx <- which(z > 0.05)
    if (!length(above_idx)) return(ifelse(is.finite(z[[1]]) && z[[1]] <= 0.05, 0, NA_real_))
    start_idx <- above_idx[[1]]
    cross_idx <- which(seq_along(z) > start_idx & z <= 0.05)
    if (!length(cross_idx)) return(NA_real_)
    j <- cross_idx[[1]]
    t0 <- t_grid[[j - 1]]
    t1 <- t_grid[[j]]
    z0 <- z[[j - 1]]
    z1 <- z[[j]]
    frac <- if (is.finite(z0) && is.finite(z1) && z1 != z0) (0.05 - z0) / (z1 - z0) else 0
    t_hit <- t0 + (t1 - t0) * pmin(pmax(frac, 0), 1)
    xp <- sum(ifelse(is.finite(xmat[i, ]), xmat[i, ], 0) * t_hit^(0:8))
    yp <- sum(ifelse(is.finite(ymat[i, ]), ymat[i, ], 0) * t_hit^(0:8))
    dist <- sqrt(xp^2 + yp^2)
    if (is.finite(dist)) dist else NA_real_
  }, numeric(1))
}

opportunity_zone_for_position <- function(pos, bucket, bearing, distance, made_play) {
  n <- length(bucket)
  pos <- rep(pos, n)
  bucket <- to_chr(bucket)
  out <- rep(NA_character_, n)
  has_bearing <- is.finite(bearing)
  made <- made_play %in% TRUE
  is_ground <- bucket == "Ground"
  is_air <- bucket %in% c("Fly", "Line", "Popup")
  is_shallow_air <- is_air & (bucket == "Popup" | (is.finite(distance) & distance <= SHALLOW_AIR_DISTANCE_FT))
  short_caught_air <- is_air & made & is.finite(distance) & distance <= 150

  fair <- has_bearing & dplyr::between(bearing, -45, 45)
  out[pos == "3B" & is_ground & fair & bearing >= -45 & bearing < -22.5] <- "IF 3B"
  out[pos == "SS" & is_ground & fair & bearing >= -22.5 & bearing < 0] <- "IF SS"
  out[pos == "2B" & is_ground & fair & bearing >= 0 & bearing < 22.5] <- "IF 2B"
  out[pos == "1B" & is_ground & fair & bearing >= 22.5 & bearing <= 45] <- "IF 1B"

  out[pos %in% INFIELD_POSITIONS & is_shallow_air] <- paste("Shallow Air", pos[pos %in% INFIELD_POSITIONS & is_shallow_air])

  out[pos == "LF" & is_air & !short_caught_air & fair & bearing >= -45 & bearing < -15] <- "OF LF"
  out[pos == "CF" & is_air & !short_caught_air & fair & bearing >= -15 & bearing <= 15] <- "OF CF"
  out[pos == "RF" & is_air & !short_caught_air & fair & bearing > 15 & bearing <= 45] <- "OF RF"
  out[pos == "LF" & is_air & !short_caught_air & made & has_bearing & bearing < -45] <- "OF LF Foul Out"
  out[pos == "RF" & is_air & !short_caught_air & made & has_bearing & bearing > 45] <- "OF RF Foul Out"

  out
}

position_opportunity_for_row <- function(pos, pitch_call, play_result, bucket, bearing, distance, made_play,
                                         explicit_target = NULL, fhc_ok = TRUE) {
  target <- explicit_target %||% rep(NA_character_, length(bucket))
  has_target <- !is.na(target) & nzchar(target)
  included <- included_bip_result(pitch_call, play_result)
  zone <- opportunity_zone_for_position(pos, bucket, bearing, distance, made_play)
  forced_exclude <- (pos %in% OUTFIELD_POSITIONS & bucket == "Ground") |
    (pos %in% OUTFIELD_POSITIONS & bucket %in% c("Fly", "Line", "Popup") & made_play %in% TRUE & is.finite(distance) & distance <= 150)
  has_spatial_model <- bucket != "Unknown" & is.finite(bearing)
  spatial_opp <- !is.na(zone) & nzchar(zone)
  fallback_opp <- ifelse(has_target, target == pos, fhc_ok)
  opp <- included & !forced_exclude & ifelse(has_spatial_model, spatial_opp, fallback_opp)
  zone <- ifelse(opp & (!nzchar(zone) | is.na(zone)), paste("Unmapped", pos), zone)
  list(opportunity = opp, zone = zone)
}

add_expected_play_probabilities <- function(d) {
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) {
    d$play_probability <- numeric(0)
    return(d)
  }
  if (!"opportunity_zone" %in% names(d)) d$opportunity_zone <- NA_character_
  usable <- d %>%
    dplyr::filter(.data$opportunity %in% TRUE, !is.na(.data$made_play))
  zone_rates <- usable %>%
    dplyr::group_by(.data$season, .data$position, .data$opportunity_zone) %>%
    dplyr::summarise(zone_play_probability = mean(.data$made_play %in% TRUE), .groups = "drop")
  position_rates <- usable %>%
    dplyr::group_by(.data$season, .data$position) %>%
    dplyr::summarise(position_play_probability = mean(.data$made_play %in% TRUE), .groups = "drop")

  d <- d %>%
    dplyr::left_join(zone_rates, by = c("season", "position", "opportunity_zone")) %>%
    dplyr::left_join(position_rates, by = c("season", "position")) %>%
    dplyr::mutate(
      play_probability = dplyr::coalesce(.data$zone_play_probability, .data$position_play_probability),
      play_probability = ifelse(.data$opportunity %in% TRUE, .data$play_probability, 0),
      oaa = dplyr::case_when(
        .data$opportunity %in% TRUE & !is.na(.data$made_play) & is.finite(.data$play_probability) ~ as.numeric(.data$made_play %in% TRUE) - .data$play_probability,
        is.finite(.data$oaa) ~ .data$oaa,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(-zone_play_probability, -position_play_probability)
  d
}

infer_opportunity_position <- function(raw, positions, ball_x = NULL, ball_y = NULL) {
  explicit <- normalize_position(coalesce_chr_cols(raw, c(
    "OpportunityPosition", "OpportunityPos", "HitToPosition", "HitToPos", "BattedBallPosition",
    "TargetPosition", "ResponsiblePosition", "ResponsibleFielderPosition", "FieldedByPosition",
    "FieldedPosition", "FielderPosition", "Position"
  )))
  explicit[!explicit %in% positions] <- NA_character_
  out <- explicit

  fielded_by <- coalesce_chr_cols(raw, c("FieldedBy", "FieldedByName", "ResponsibleFielder", "Fielder", "Defender"))
  need_name_match <- is.na(out) & nzchar(fielded_by)
  if (any(need_name_match)) {
    fielded_norm <- name_norm(fielded_by)
    for (pos in positions) {
      nm <- paste0(pos, "_Name")
      if (nm %in% names(raw)) {
        match_pos <- need_name_match & fielded_norm == name_norm(raw[[nm]])
        out[match_pos] <- pos
      }
    }
  }

  if (!is.null(ball_x) && !is.null(ball_y)) {
    need_nearest <- is.na(out) & is.finite(ball_x) & is.finite(ball_y)
    if (any(need_nearest)) {
      dist_mat <- sapply(positions, function(pos) {
        sx <- to_num(raw[[paste0(pos, "_PositionAtReleaseX")]])
        sy <- to_num(raw[[paste0(pos, "_PositionAtReleaseZ")]])
        sqrt((ball_x - sx)^2 + (ball_y - sy)^2)
      })
      nearest_idx <- max.col(-dist_mat, ties.method = "first")
      nearest <- positions[nearest_idx]
      nearest[!is.finite(rowSums(dist_mat))] <- NA_character_
      out[need_nearest] <- nearest[need_nearest]
    }
  }

  out
}

standardize_defense <- function(raw) {
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    return(tibble::tibble(
      player = character(), position = character(), season = character(), game = character(),
      date = as.Date(character()), team = character(), opponent = character(),
      play_result = character(), batted_ball = character(), direction = character(),
      play_bucket = character(), batted_ball_bucket = character(), bearing = numeric(),
      distance = numeric(), last_tracked_distance = numeric(), first_ground_distance = numeric(),
      batted_ball_matched = logical(),
      opportunity_zone = character(), play_probability = numeric(),
      start_x = numeric(), start_y = numeric(), ball_x = numeric(), ball_y = numeric(),
      route_distance = numeric(), reaction_time = numeric(), first_step = numeric(),
      jump_feet = numeric(), oaa = numeric(), drs = numeric(), made_play = logical(),
      fielded_ball = logical(), clean_field = logical(), putouts = numeric(),
      assists = numeric(), errors = numeric(),
      opportunity = logical(), opportunity_position = character(),
      source_file = character(), row_in_file = integer()
    ))
  }

  source_file <- if ("source_file" %in% names(raw)) raw$source_file else rep(NA_character_, nrow(raw))
  row_in_file <- if ("row_in_file" %in% names(raw)) raw$row_in_file else seq_len(nrow(raw))

  wide_positions <- c("1B", "2B", "3B", "SS", "LF", "CF", "RF")
  wide_has_names <- all(paste0(wide_positions, "_Name") %in% names(raw))
  wide_has_coords <- all(paste0(wide_positions, "_PositionAtReleaseX") %in% names(raw)) &&
    all(paste0(wide_positions, "_PositionAtReleaseZ") %in% names(raw))

  if (wide_has_names && wide_has_coords) {
    season <- normalize_season(
      col_chr(raw, c("SeasonGroup", "Season_Group", "SeasonTag", "Season", "SeasonCode", "Season_Code")),
      source_file
    )
    pitch_call <- col_chr(raw, c("PitchCall"))
    play_result <- col_chr(raw, c("PlayResult", "Result", "Outcome"))
    batted_ball <- col_chr(raw, c("TaggedHitType", "AutoHitType", "BBType", "BattedBallType", "HitType", "ContactType"))
    launch_angle <- col_num(raw, c("Angle", "LaunchAngle", "ExitAngle", "VertAngle", "VerticalAngle"))
    batted_bucket <- batted_ball_bucket(batted_ball, launch_angle)
    ball_xy <- ball_xy_from_raw(raw)
    distance <- coalesce_num_cols(raw, c("Distance", "HitDistance", "LandingDistance", "ProjectedDistance"))
    distance <- ifelse(is.finite(distance), distance, sqrt(ball_xy$x^2 + ball_xy$y^2))
    last_tracked_distance <- col_num(raw, c("LastTrackedDistance", "LastTrackedHitDistance", "FirstPlottedDistance", "FirstBounceDistance"))
    first_ground_distance <- trajectory_first_ground_distance(raw)
    first_ground_distance <- ifelse(is.finite(first_ground_distance), first_ground_distance, last_tracked_distance)
    bearing <- coalesce_num_cols(raw, c("Bearing", "Direction", "HitBearing", "BearingDeg", "BearingDegrees", "SprayAngle"))
    bearing <- ifelse(is.finite(bearing), bearing, bearing_from_ball_xy(ball_xy$x, ball_xy$y))
    batted_ball_matched <- if ("batted_ball_matched" %in% names(raw)) raw$batted_ball_matched %in% TRUE else rep(FALSE, nrow(raw))
    opportunity_position <- infer_opportunity_position(raw, wide_positions, ball_xy$x, ball_xy$y)
    has_target_position <- !is.na(opportunity_position) & nzchar(opportunity_position)
    fhc_col <- col_chr(raw, c("FHC"), default = NA_character_)
    fhc_ok <- if (any(!is.na(fhc_col) & nzchar(fhc_col))) as_logical_flag(fhc_col, default = TRUE) else rep(TRUE, nrow(raw))
    explicit_made <- coalesce_chr_cols(raw, c("MadePlay", "Catch", "IsOut", "Out", "Converted", "SuccessfulPlay"))
    explicit_made_l <- tolower(explicit_made)
    made_play <- dplyr::case_when(
      explicit_made_l %in% c("true", "t", "yes", "y", "1", "out", "caught", "converted") ~ TRUE,
      explicit_made_l %in% c("false", "f", "no", "n", "0", "safe", "not converted") ~ FALSE,
      TRUE ~ made_play_from_result(play_result, col_num(raw, c("OutsOnPlay", "Outs", "FieldingOuts")))
    )
    play_bucket <- play_result_bucket(play_result, made_play)
    explicit_clean <- coalesce_chr_cols(raw, c("CleanField", "FieldedCleanly", "CleanlyFielded", "NoError", "ErrorFree"))
    explicit_clean_l <- tolower(explicit_clean)
    clean_field <- dplyr::case_when(
      explicit_clean_l %in% c("true", "t", "yes", "y", "1", "clean") ~ TRUE,
      explicit_clean_l %in% c("false", "f", "no", "n", "0", "error", "not clean") ~ FALSE,
      TRUE ~ clean_field_from_result(play_result)
    )
    explicit_drs <- col_num(raw, c("DRS", "DefensiveRunsSaved", "Defensive_Runs_Saved", "RunsSaved", "RunValue", "DefensiveRunValue"))

    out <- purrr::map_dfr(wide_positions, function(pos) {
      player <- to_chr(raw[[paste0(pos, "_Name")]])
      route_distance <- col_num(raw, c(paste0(pos, "_RouteDistance"), "RouteDistance", "DistanceCovered", "Distance", "FeetCovered", "RangeDistance"))
      route_distance <- ifelse(
        is.finite(route_distance),
        route_distance,
        sqrt((ball_xy$x - to_num(raw[[paste0(pos, "_PositionAtReleaseX")]]))^2 + (ball_xy$y - to_num(raw[[paste0(pos, "_PositionAtReleaseZ")]]))^2)
      )
      opp_info <- position_opportunity_for_row(
        pos, pitch_call, play_result, batted_bucket, bearing, distance, made_play,
        explicit_target = opportunity_position, fhc_ok = fhc_ok
      )
      opp <- opp_info$opportunity
      pos_putouts <- col_num(raw, c(paste0(pos, "_Putouts"), paste0(pos, "_PO"), "Putouts", "PO"))
      pos_assists <- col_num(raw, c(paste0(pos, "_Assists"), paste0(pos, "_A"), "Assists", "A"))
      pos_errors <- col_num(raw, c(paste0(pos, "_Errors"), paste0(pos, "_E"), "Errors", "E"))
      pos_errors <- ifelse(is.finite(pos_errors), pos_errors, ifelse(opp & clean_field %in% FALSE, 1, ifelse(opp & clean_field %in% TRUE, 0, NA_real_)))
      pos_putouts <- ifelse(is.finite(pos_putouts), pos_putouts, ifelse(opp & clean_field %in% TRUE & !is.finite(pos_assists), 1, 0))
      pos_assists <- ifelse(is.finite(pos_assists), pos_assists, 0)
      tibble::tibble(
        player = ifelse(nzchar(player), player, "Unknown"),
        position = pos,
        season = season,
        game = col_chr(raw, c("GameUID", "GameID", "GameId", "Game")),
        date = col_date(raw, c("Date", "GameDate", "PlayDate", "LocalDate")),
        team = col_chr(raw, c("PitcherTeam", "Team", "FielderTeam", "DefensiveTeam")),
        opponent = col_chr(raw, c("BatterTeam", "Opponent", "OpponentTeam")),
        play_result = play_result,
        batted_ball = batted_ball,
        direction = col_chr(raw, c("DetectedShift", "Direction", "Bearing", "SprayAngle", "HitBearing")),
        play_bucket = play_bucket,
        batted_ball_bucket = batted_bucket,
        bearing = bearing,
        distance = distance,
        last_tracked_distance = last_tracked_distance,
        first_ground_distance = first_ground_distance,
        batted_ball_matched = batted_ball_matched,
        opportunity_zone = opp_info$zone,
        play_probability = NA_real_,
        start_x = to_num(raw[[paste0(pos, "_PositionAtReleaseX")]]),
        start_y = to_num(raw[[paste0(pos, "_PositionAtReleaseZ")]]),
        ball_x = ball_xy$x,
        ball_y = ball_xy$y,
        route_distance = route_distance,
        reaction_time = col_num(raw, c(paste0(pos, "_ReactionTime"), "ReactionTime", "Reaction", "ReadTime", "FirstMoveTime", "TimeToFirstMove")),
        first_step = col_num(raw, c(paste0(pos, "_FirstStep"), "FirstStep", "FirstStepFeet", "Burst", "InitialBurst")),
        jump_feet = col_num(raw, c(paste0(pos, "_JumpFeet"), paste0(pos, "_Jump"), "JumpFeet", "JumpDistance", "Jump", "FeetVsExpected", "JumpPlus")),
        oaa = col_num(raw, c(paste0(pos, "_OAA"), paste0(pos, "_OutsAboveAverage"), "OAA", "OutsAboveAverage")),
        drs = col_num(raw, c(paste0(pos, "_DRS"), paste0(pos, "_DefensiveRunsSaved"), "DRS", "DefensiveRunsSaved", "Defensive_Runs_Saved")) %>%
          dplyr::coalesce(explicit_drs),
        made_play = ifelse(opp, made_play, NA),
        fielded_ball = opp,
        clean_field = ifelse(opp, clean_field, NA),
        putouts = ifelse(opp, pos_putouts, NA_real_),
        assists = ifelse(opp, pos_assists, NA_real_),
        errors = ifelse(opp, pos_errors, NA_real_),
        opportunity = opp,
        opportunity_position = ifelse(has_target_position, opportunity_position, pos),
        source_file = source_file,
        row_in_file = row_in_file
      )
    })

    return(out %>% dplyr::filter(nzchar(player), player != "Unknown"))
  }

  player <- col_chr(raw, c("Player", "Fielder", "FielderName", "Defender", "DefensivePlayer", "Athlete", "Name"))
  position <- col_chr(raw, c("Position", "FielderPosition", "FieldingPosition", "DefensivePosition", "POS"))
  position[!nzchar(position)] <- "Unknown"

  season <- normalize_season(
    col_chr(raw, c("SeasonGroup", "Season_Group", "SeasonTag", "Season", "SeasonCode", "Season_Code")),
    source_file
  )
  game <- col_chr(raw, c("GameID", "GameId", "Game", "GameDate", "EventID", "Event"))
  date <- col_date(raw, c("Date", "GameDate", "PlayDate", "LocalDate"))

  play_result <- col_chr(raw, c("PlayResult", "Result", "Outcome", "PitchCall", "PAResult", "EventResult"))
  batted_ball <- col_chr(raw, c("TaggedHitType", "BBType", "BattedBallType", "HitType", "ContactType"))
  direction <- col_chr(raw, c("Direction", "Bearing", "SprayAngle", "HitBearing", "BearingDeg", "BearingDegrees"))
  launch_angle <- col_num(raw, c("Angle", "LaunchAngle", "ExitAngle", "VertAngle", "VerticalAngle"))
  batted_bucket <- batted_ball_bucket(batted_ball, launch_angle)
  ball_xy <- ball_xy_from_raw(raw)
  distance <- coalesce_num_cols(raw, c("Distance", "HitDistance", "LandingDistance", "ProjectedDistance"))
  distance <- ifelse(is.finite(distance), distance, sqrt(ball_xy$x^2 + ball_xy$y^2))
  last_tracked_distance <- col_num(raw, c("LastTrackedDistance", "LastTrackedHitDistance", "FirstPlottedDistance", "FirstBounceDistance"))
  first_ground_distance <- trajectory_first_ground_distance(raw)
  first_ground_distance <- ifelse(is.finite(first_ground_distance), first_ground_distance, last_tracked_distance)
  bearing <- coalesce_num_cols(raw, c("Bearing", "Direction", "HitBearing", "BearingDeg", "BearingDegrees", "SprayAngle"))
  bearing <- ifelse(is.finite(bearing), bearing, bearing_from_ball_xy(ball_xy$x, ball_xy$y))
  batted_ball_matched <- if ("batted_ball_matched" %in% names(raw)) raw$batted_ball_matched %in% TRUE else rep(FALSE, nrow(raw))
  position <- normalize_position(position)
  opportunity_position <- normalize_position(coalesce_chr_cols(raw, c(
    "OpportunityPosition", "OpportunityPos", "HitToPosition", "HitToPos", "BattedBallPosition",
    "TargetPosition", "ResponsiblePosition", "ResponsibleFielderPosition", "FieldedByPosition",
    "FieldedPosition", "FielderPosition", "Position"
  )))
  opportunity_position[!nzchar(opportunity_position)] <- NA_character_

  explicit_outs <- col_num(raw, c("OutsOnPlay", "Outs", "FieldingOuts"))
  made_play_raw <- col_chr(raw, c("MadePlay", "Catch", "IsOut", "Out", "Converted", "SuccessfulPlay"))
  made_play <- dplyr::case_when(
    tolower(made_play_raw) %in% c("true", "t", "yes", "y", "1", "out", "caught", "converted") ~ TRUE,
    tolower(made_play_raw) %in% c("false", "f", "no", "n", "0", "safe", "not converted") ~ FALSE,
    is.finite(explicit_outs) ~ explicit_outs > 0,
    has_text(play_result, "out|caught|lineout|flyout|groundout|popout|force") ~ TRUE,
    has_text(play_result, "single|double|triple|home run|homerun|error|safe|reached") ~ FALSE,
    TRUE ~ NA
  )
  play_bucket <- play_result_bucket(play_result, made_play)

  opp_raw <- col_chr(raw, c("Opportunity", "DefensiveOpportunity", "IsOpportunity", "InPlay", "BallInPlay"))
  explicit_opp <- tolower(opp_raw) %in% c("true", "t", "yes", "y", "1")
  opp_info <- purrr::map2(
    position,
    seq_along(position),
    function(pos, idx) {
      position_opportunity_for_row(
        pos,
        col_chr(raw[idx, , drop = FALSE], c("PitchCall", "Pitch_Call", "PitchResult")),
        play_result[idx],
        batted_bucket[idx],
        bearing[idx],
        distance[idx],
        made_play[idx],
        explicit_target = opportunity_position[idx],
        fhc_ok = explicit_opp[idx]
      )
    }
  )
  opportunity <- vapply(opp_info, function(x) x$opportunity[[1]], logical(1))
  opportunity_zone <- vapply(opp_info, function(x) x$zone[[1]] %||% NA_character_, character(1))

  explicit_oaa <- col_num(raw, c("OAA", "OutsAboveAverage", "Outs_Above_Average", "ExpectedOutsMinusActual", "ActualMinusExpected"))
  explicit_drs <- col_num(raw, c("DRS", "DefensiveRunsSaved", "Defensive_Runs_Saved", "RunsSaved", "RunValue", "DefensiveRunValue"))
  explicit_clean <- coalesce_chr_cols(raw, c("CleanField", "FieldedCleanly", "CleanlyFielded", "NoError", "ErrorFree"))
  explicit_clean_l <- tolower(explicit_clean)
  clean_field <- dplyr::case_when(
    explicit_clean_l %in% c("true", "t", "yes", "y", "1", "clean") ~ TRUE,
    explicit_clean_l %in% c("false", "f", "no", "n", "0", "error", "not clean") ~ FALSE,
    TRUE ~ clean_field_from_result(play_result)
  )
  putouts <- col_num(raw, c("Putouts", "PO", "PutOuts", "FieldingPutouts"))
  assists <- col_num(raw, c("Assists", "A", "FieldingAssists"))
  errors <- col_num(raw, c("Errors", "E", "FieldingErrors"))
  errors <- ifelse(is.finite(errors), errors, ifelse(opportunity & clean_field %in% FALSE, 1, ifelse(opportunity & clean_field %in% TRUE, 0, NA_real_)))
  putouts <- ifelse(is.finite(putouts), putouts, ifelse(opportunity & clean_field %in% TRUE & !is.finite(assists), 1, 0))
  assists <- ifelse(is.finite(assists), assists, 0)
  catch_probability <- col_num(raw, c("CatchProbability", "OutProbability", "ExpectedOutProbability", "xOut", "ExpectedOuts"))
  catch_probability <- ifelse(is.finite(catch_probability) & catch_probability > 1, catch_probability / 100, catch_probability)
  oaa <- dplyr::case_when(
    is.finite(explicit_oaa) ~ explicit_oaa,
    !is.na(made_play) & is.finite(catch_probability) ~ as.numeric(made_play) - catch_probability,
    TRUE ~ NA_real_
  )

  tibble::tibble(
    player = ifelse(nzchar(player), player, "Unknown"),
    position = position,
    season = season,
    game = game,
    date = date,
    team = col_chr(raw, c("Team", "FielderTeam", "DefensiveTeam")),
    opponent = col_chr(raw, c("Opponent", "OpponentTeam", "BatterTeam")),
    play_result = play_result,
    batted_ball = batted_ball,
    direction = direction,
    play_bucket = play_bucket,
    batted_ball_bucket = batted_bucket,
    bearing = bearing,
    distance = distance,
    last_tracked_distance = last_tracked_distance,
    first_ground_distance = first_ground_distance,
    batted_ball_matched = batted_ball_matched,
    opportunity_zone = opportunity_zone,
    play_probability = NA_real_,
    start_x = col_num(raw, c("StartX", "FielderStartX", "FielderX", "PlayerStartX", "PositionX")),
    start_y = col_num(raw, c("StartY", "FielderStartY", "FielderY", "PlayerStartY", "PositionY")),
    ball_x = ball_xy$x,
    ball_y = ball_xy$y,
    route_distance = col_num(raw, c("RouteDistance", "DistanceCovered", "Distance", "FeetCovered", "RangeDistance")),
    reaction_time = col_num(raw, c("ReactionTime", "Reaction", "ReadTime", "FirstMoveTime", "TimeToFirstMove")),
    first_step = col_num(raw, c("FirstStep", "FirstStepFeet", "Burst", "InitialBurst", "Jump")),
    jump_feet = col_num(raw, c("JumpFeet", "JumpDistance", "Jump", "FeetVsExpected", "JumpPlus")),
    oaa = oaa,
    drs = explicit_drs,
    made_play = made_play,
    fielded_ball = opportunity,
    clean_field = ifelse(opportunity, clean_field, NA),
    putouts = ifelse(opportunity, putouts, NA_real_),
    assists = ifelse(opportunity, assists, NA_real_),
    errors = ifelse(opportunity, errors, NA_real_),
    opportunity = opportunity,
    opportunity_position = opportunity_position,
    source_file = source_file,
    row_in_file = row_in_file
  )
}

raw_df <- load_defense_data()
batted_ball_raw_df <- load_batted_ball_data()
raw_df <- attach_batted_ball_data(raw_df, batted_ball_raw_df)
defense_df <- standardize_defense(raw_df)
defense_df <- add_expected_play_probabilities(defense_df)
catching_raw_df <- load_catching_data()

name_display <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- trimws(s)
    s <- gsub("\\s+", " ", s)
    if (grepl(",", s)) {
      parts <- strsplit(s, ",\\s*")[[1]]
      if (length(parts) >= 2) return(paste0(parts[2], " ", parts[1]))
    }
    s
  }, "", USE.NAMES = FALSE)
}

name_norm <- function(x) tolower(name_display(x))

parse_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  out <- suppressWarnings(as.Date(x))
  need <- is.na(out) & !is.na(x) & nzchar(x)
  if (any(need)) out[need] <- suppressWarnings(as.Date(x[need], format = "%m/%d/%Y"))
  need <- is.na(out) & !is.na(x) & nzchar(x)
  if (any(need)) out[need] <- suppressWarnings(as.Date(x[need], format = "%m/%d/%y"))
  need <- is.na(out) & !is.na(x) & nzchar(x)
  if (any(need)) {
    ymd <- stringr::str_extract(x[need], "(?<!\\d)(\\d{8})(?!\\d)")
    out[need] <- suppressWarnings(as.Date(ymd, format = "%Y%m%d"))
  }
  out
}

extract_date_from_filename <- function(x) {
  parse_date_any(basename(as.character(x)))
}

nz_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

pick_first <- function(cands, in_df) {
  cands <- cands[cands %in% names(in_df)]
  if (length(cands)) cands[[1]] else NA_character_
}

as_named_choices <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  stats::setNames(x, x)
}

canon_pitch_call <- function(x) {
  y <- gsub("[^A-Za-z]", "", tolower(nz_chr(x)))
  dplyr::case_when(
    y %in% c("strikecalled", "calledstrike") ~ "StrikeCalled",
    y %in% c("ballcalled", "ball", "ballinthedirt", "ballintentional") ~ "BallCalled",
    y %in% c("strikeswinging", "swingingstrike") ~ "StrikeSwinging",
    y %in% c("foulball", "foul", "foulballfieldable", "foulballnotfieldable") ~ "FoulBall",
    y %in% c("inplay", "inplayout", "inplaynoout") ~ "InPlay",
    TRUE ~ as.character(x)
  )
}

zone_h_in <- function(h) {
  h <- to_num(h)
  q90 <- suppressWarnings(stats::quantile(abs(h), 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 10) h <- h * 12
  h
}

zone_s_in <- function(s) {
  s <- to_num(s)
  q90 <- suppressWarnings(stats::quantile(abs(s), 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 5) s <- s * 12
  s
}

pitch_colors <- c(
  "Fastball" = "#FF0000",
  "Sinker" = "#FFA500",
  "Changeup" = "#008000",
  "Splitter" = "#000080",
  "Slider" = "#FFFF00",
  "Cutter" = "#000000",
  "Sweeper" = "#FFD700",
  "Curveball" = "#0000FF",
  "Untagged" = "#808080",
  "Undefined" = "#808080"
)
facet_levels <- c("Fastball", "Sinker", "Changeup", "Splitter", "Slider", "Cutter", "Sweeper", "Curveball")
pitch_levels_all <- c(facet_levels, "Untagged", "Undefined")

canonical_pitch_fuzzy <- function(x) {
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    grepl("four|two|fast", y) ~ "Fastball",
    grepl("sink", y) ~ "Sinker",
    grepl("change", y) ~ "Changeup",
    grepl("split", y) ~ "Splitter",
    grepl("slide", y) ~ "Slider",
    grepl("cut", y) ~ "Cutter",
    grepl("sweep", y) ~ "Sweeper",
    grepl("curve|knuckle", y) ~ "Curveball",
    !nzchar(y) | is.na(y) ~ "Undefined",
    grepl("untag|undefined|unknown", y) ~ "Undefined",
    TRUE ~ "Undefined"
  )
}

home_plate_segments <- data.frame(
  x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
  y = c(0.5, 0.5, 0.35, 0.15, 0.35, 0.5),
  xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
  yend = c(0.5, 0.35, 0.15, 0.35, 0.5, 0.5)
)

framing_zone_type <- function(s_in, h_in) {
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  in_top_buffer <- is.finite(abs_s) & is.finite(h_in) & abs_s <= 6.7 & h_in > 38 & h_in <= 46
  in_down_buffer <- is.finite(abs_s) & is.finite(h_in) & abs_s <= 6.7 & h_in >= 14 & h_in < 22
  in_gloveside_buffer <- is.finite(s_in) & is.finite(h_in) & s_in > 6.7 & s_in <= 13.3 & dplyr::between(h_in, 22, 38)
  in_armside_buffer <- is.finite(s_in) & is.finite(h_in) & s_in >= -13.3 & s_in < -6.7 & dplyr::between(h_in, 22, 38)
  in_heart <- is.finite(abs_s) & is.finite(h_in) & abs_s <= 6.7 & dplyr::between(h_in, 22, 38)
  in_zone <- is.finite(abs_s) & is.finite(h_in) & abs_s <= 10.0 & dplyr::between(h_in, 18, 42) & !in_heart
  in_shadow <- is.finite(abs_s) & is.finite(h_in) & (
    (abs_s > 10.0 & abs_s <= 13.3 & dplyr::between(h_in, 18, 42)) |
      (abs_s <= 13.3 & dplyr::between(h_in, 42, 46)) |
      (abs_s <= 13.3 & dplyr::between(h_in, 14, 18))
  )
  dplyr::case_when(
    in_top_buffer ~ "Top Buffer",
    in_gloveside_buffer ~ "Gloveside Buffer",
    in_armside_buffer ~ "Arm Side Buffer",
    in_down_buffer ~ "Bottom Buffer",
    in_heart ~ "Heart",
    in_zone ~ "Zone",
    in_shadow ~ "Shadow",
    TRUE ~ "Chase"
  )
}

framing_zone_levels <- function(mode = c("ball_to_strike", "strike_to_ball")) {
  mode <- match.arg(mode)
  buffer_zones <- c("Top Buffer", "Gloveside Buffer", "Arm Side Buffer", "Bottom Buffer")
  if (mode == "ball_to_strike") c(buffer_zones, "Shadow", "Chase") else c("Heart", buffer_zones, "Zone")
}

framing_in_zone <- function(s_in, h_in) {
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  is.finite(abs_s) & is.finite(h_in) & abs_s <= 10.0 & dplyr::between(h_in, 18, 42)
}

build_custom_game_id <- function(d) {
  if (!nrow(d)) return(character(0))
  game_id <- col_chr(d, c("CustomGameID", "GameUID", "GameID", "GameId", "Game"))
  date_val <- parse_date_any(col_chr(d, c("GameDate", "Date", "UTCDate", "LocalDateTime")))
  date_chr <- ifelse(is.na(date_val), "", format(date_val, "%Y-%m-%d"))
  away <- col_chr(d, c("AwayTeam", "BatterTeam", "OpponentTeam"))
  home <- col_chr(d, c("HomeTeam", "PitcherTeam", "CatcherTeam", "Team"))
  fallback <- ifelse(nzchar(date_chr), paste0(date_chr, ": ", away, " @ ", home), basename(col_chr(d, c("source_file"))))
  ifelse(nzchar(game_id), game_id, fallback)
}

catcher_game_label <- function(gdate, home, away, team, gid = NULL) {
  gdate <- parse_date_any(gdate)
  date_lbl <- ifelse(is.na(gdate), "", format(gdate, "%m-%d-%Y"))
  home <- to_chr(home)
  away <- to_chr(away)
  team <- to_chr(team)
  rel <- dplyr::case_when(
    nzchar(team) & nzchar(home) & team == home ~ "vs.",
    nzchar(team) & nzchar(away) & team == away ~ "@",
    TRUE ~ "vs."
  )
  opp <- dplyr::case_when(
    rel == "vs." & nzchar(away) ~ away,
    rel == "@" & nzchar(home) ~ home,
    nzchar(away) ~ away,
    nzchar(home) ~ home,
    TRUE ~ to_chr(gid)
  )
  trimws(paste(date_lbl, rel, opp))
}

catcher_game_choices <- function(games) {
  if (is.null(games) || !nrow(games)) return(character(0))
  labels <- catcher_game_label(games$gdate, games$home, games$away, games$team, games$gid)
  labels[!nzchar(labels)] <- games$gid[!nzchar(labels)]
  stats::setNames(as.character(games$gid), labels)
}

standardize_catching <- function(d) {
  if (!nrow(d)) return(d)
  if (!"source_file" %in% names(d)) d$source_file <- NA_character_
  if (!"row_in_file" %in% names(d)) d$row_in_file <- seq_len(nrow(d))
  d$Catcher <- col_chr(d, c("Catcher", "CatcherName", "Receiver", "Catcher_Name"))
  d$Catcher[d$Catcher == ""] <- NA_character_
  d$CatcherTeam <- col_chr(d, c("CatcherTeam", "PitcherTeam", "Team", "DefensiveTeam"))
  d$GameDate <- parse_date_any(col_chr(d, c("GameDate", "Date", "UTCDate", "LocalDateTime")))
  d$SeasonGroup <- normalize_season(col_chr(d, c("SeasonGroup", "Season", "SeasonTag", "SeasonCode")), d$source_file)
  d$CustomGameID <- build_custom_game_id(d)
  d$PitcherHand <- toupper(substr(col_chr(d, c("PitcherHand", "PitcherThrows", "Throws", "ThrowingHand")), 1, 1))
  d$PitcherHand <- dplyr::case_when(d$PitcherHand == "L" ~ "LHP", d$PitcherHand == "R" ~ "RHP", TRUE ~ "Unknown")
  d
}

catching_df <- standardize_catching(catching_raw_df)
d1_catcher_framing_baseline <- load_d1_catcher_framing_baseline()

framing_metric_specs <- tibble::tribble(
  ~metric_id, ~metric, ~metric_order, ~metric_type, ~better,
  "strikes_stolen", "Strikes Stolen", 1L, "stolen_rate", "high",
  "balls_lost", "Balls Lost", 2L, "lost_rate", "low",
  "bottom_buffer", "Bottom Buffer", 3L, "called_strike_rate", "high",
  "top_buffer", "Top Buffer", 4L, "called_strike_rate", "high",
  "glove_side_buffer", "Glove Side Buffer", 5L, "called_strike_rate", "high",
  "arm_side_buffer", "Arm Side Buffer", 6L, "called_strike_rate", "high",
  "rhp", "RHP", 7L, "net_rate", "high",
  "lhp", "LHP", 8L, "net_rate", "high",
  "fastballs_sinkers", "Fastballs/Sinkers", 9L, "net_rate", "high",
  "breaking_balls", "Breaking Balls", 10L, "net_rate", "high",
  "soft", "Soft", 11L, "net_rate", "high",
  "lhp_fastballs_sinkers", "LHP Fastballs/Sinkers", 12L, "net_rate", "high",
  "lhp_breaking_balls", "LHP Breaking Balls", 13L, "net_rate", "high",
  "lhp_soft", "LHP Soft", 14L, "net_rate", "high",
  "rhp_fastballs_sinkers", "RHP Fastballs/Sinkers", 15L, "net_rate", "high",
  "rhp_breaking_balls", "RHP Breaking Balls", 16L, "net_rate", "high",
  "rhp_soft", "RHP Soft", 17L, "net_rate", "high"
)

framing_metric_min_chances <- function(metric_id) {
  dplyr::case_when(
    metric_id %in% c("strikes_stolen", "balls_lost", "rhp", "lhp", "fastballs_sinkers", "breaking_balls", "soft") ~ 25L,
    TRUE ~ 10L
  )
}

framing_zone_stats <- function(d, mode = c("ball_to_strike", "strike_to_ball")) {
  mode <- match.arg(mode)
  zones <- framing_zone_levels(mode)
  if (!nrow(d)) {
    return(dplyr::bind_rows(
      tibble::tibble(Zone = zones, Chances = 0L, Strikes = 0L, Pct = NA_real_),
      tibble::tibble(Zone = "Total", Chances = 0L, Strikes = 0L, Pct = NA_real_)
    ))
  }
  sx_in <- zone_s_in(d$plate_x)
  hz_in <- zone_h_in(d$plate_z)
  zone_type <- framing_zone_type(sx_in, hz_in)
  pc <- canon_pitch_call(d$pitch_call)
  called <- pc %in% c("BallCalled", "StrikeCalled")
  in_zone_std <- framing_in_zone(sx_in, hz_in)
  if (mode == "ball_to_strike") {
    idx <- called & !in_zone_std
    strikes <- pc %in% "StrikeCalled" & idx
  } else {
    idx <- called & in_zone_std
    strikes <- pc %in% "BallCalled" & idx
  }
  out <- tibble::tibble(Zone = zone_type, Chance = idx, Strike = strikes) %>%
    dplyr::filter(.data$Chance %in% TRUE) %>%
    dplyr::group_by(.data$Zone) %>%
    dplyr::summarise(Chances = sum(.data$Chance), Strikes = sum(.data$Strike), .groups = "drop")
  out <- tibble::tibble(Zone = zones) %>%
    dplyr::left_join(out, by = "Zone") %>%
    dplyr::mutate(
      Chances = dplyr::coalesce(.data$Chances, 0L),
      Strikes = dplyr::coalesce(.data$Strikes, 0L),
      Pct = dplyr::if_else(.data$Chances > 0, .data$Strikes / .data$Chances, NA_real_)
    )
  total <- tibble::tibble(
    Zone = "Total",
    Chances = sum(out$Chances, na.rm = TRUE),
    Strikes = sum(out$Strikes, na.rm = TRUE)
  ) %>%
    dplyr::mutate(Pct = dplyr::if_else(.data$Chances > 0, .data$Strikes / .data$Chances, NA_real_))
  dplyr::bind_rows(out, total)
}

SEASON_CHOICES <- {
  vals <- unique(defense_df$season)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- unique(c("S26", vals))
  labels <- season_label[vals]
  labels[is.na(labels)] <- vals[is.na(labels)]
  stats::setNames(vals, labels)
}

position_choices <- {
  vals <- sort(unique(defense_df$position))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals)) vals else c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")
}

player_choices <- {
  vals <- sort(unique(defense_df$player))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals)) vals else character(0)
}

batted_ball_choices <- {
  vals <- sort(unique(defense_df$batted_ball))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals)) vals else character(0)
}

# -------------------- Metrics --------------------
summarize_defense <- function(d) {
  if (!nrow(d)) {
    return(tibble::tibble(
      Player = character(), Position = character(), Opportunities = integer(),
      `Fielding %` = character(), OAA = character(), DRS = character()
    ))
  }

  positions_played <- d %>%
    dplyr::filter(!is.na(.data$player), nzchar(.data$player), !is.na(.data$position), nzchar(.data$position)) %>%
    dplyr::distinct(.data$player, .data$position)

  opp_summary <- d %>%
    dplyr::filter(.data$opportunity %in% TRUE) %>%
    dplyr::group_by(player, position) %>%
    dplyr::summarise(
      Opportunities = dplyr::n(),
      Putouts = sum(.data$putouts, na.rm = TRUE),
      Assists = sum(.data$assists, na.rm = TRUE),
      Errors = sum(.data$errors, na.rm = TRUE),
      FieldingPct = safe_ratio(Putouts + Assists, Putouts + Assists + Errors),
      OAA_num = ifelse(any(is.finite(oaa)), sum(oaa, na.rm = TRUE), NA_real_),
      DRS_num = ifelse(any(is.finite(drs)), sum(drs, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )

  positions_played %>%
    dplyr::left_join(opp_summary, by = c("player", "position")) %>%
    dplyr::mutate(
      Opportunities = dplyr::coalesce(.data$Opportunities, 0L),
      FieldingPct = ifelse(
        dplyr::coalesce(.data$Putouts, 0) + dplyr::coalesce(.data$Assists, 0) + dplyr::coalesce(.data$Errors, 0) > 0,
        .data$FieldingPct,
        NA_real_
      )
    ) %>%
    dplyr::arrange(.data$position) %>%
    dplyr::transmute(
      Player = player,
      Position = position,
      Opportunities = Opportunities,
      `Fielding %` = fmt_pct(FieldingPct, 3),
      OAA = fmt_num(OAA_num, 2),
      DRS = fmt_num(DRS_num, 2)
    )
}

summarize_leaderboard <- function(d) {
  if (!nrow(d)) {
    return(tibble::tibble(
      Player = character(), `Position(s)` = character(), Opportunities = integer(),
      `Fielding %` = character(), OAA = character()
    ))
  }

  d %>%
    dplyr::filter(.data$opportunity %in% TRUE, !is.na(.data$player), nzchar(.data$player)) %>%
    dplyr::group_by(.data$player) %>%
    dplyr::summarise(
      Positions = paste(unique(.data$position[!is.na(.data$position) & nzchar(.data$position)]), collapse = ", "),
      Opportunities = dplyr::n(),
      Putouts = sum(.data$putouts, na.rm = TRUE),
      Assists = sum(.data$assists, na.rm = TRUE),
      Errors = sum(.data$errors, na.rm = TRUE),
      FieldingPct = safe_ratio(Putouts + Assists, Putouts + Assists + Errors),
      OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$OAA_num), dplyr::desc(.data$Opportunities)) %>%
    dplyr::transmute(
      Player = name_display(.data$player),
      `Position(s)` = .data$Positions,
      Opportunities = .data$Opportunities,
      `Fielding %` = fmt_decimal(.data$FieldingPct, 3),
      OAA = fmt_num(.data$OAA_num, 2)
    )
}

empty_plot <- function(title, subtitle = "Add TrackMan positioning data to DefenseApp/data to populate this view.") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.56, label = title, fontface = "bold", size = 5, color = "#501214") +
    annotate("text", x = 0.5, y = 0.46, label = subtitle, size = 3.5, color = "#333333") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}

field_base <- function() {
  ggplot() +
    annotate("segment", x = 0, y = 0, xend = -250, yend = 250, color = "#501214", linewidth = 1) +
    annotate("segment", x = 0, y = 0, xend = 250, yend = 250, color = "#501214", linewidth = 1) +
    annotate("path", x = 250 * cos(seq(pi / 4, 3 * pi / 4, length.out = 120)),
             y = 250 * sin(seq(pi / 4, 3 * pi / 4, length.out = 120)), color = "#501214", linewidth = 1) +
    coord_fixed(xlim = c(-270, 270), ylim = c(-20, 280), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_line(color = "grey88", linewidth = 0.25),
      plot.title = element_text(face = "bold", color = "#501214"),
      legend.position = "bottom"
    )
}

spray_xlim <- c(-285, 285)
spray_ylim <- c(0, 500)

make_field_layers <- function(track_width_ft = 15) {
  foul_point <- function(bearing_deg, dist) {
    c(x = dist * sin(bearing_deg * pi / 180), y = dist * cos(bearing_deg * pi / 180))
  }
  point_on_segment_at_radius <- function(p0, p1, radius) {
    dx <- p1[["x"]] - p0[["x"]]
    dy <- p1[["y"]] - p0[["y"]]
    a <- dx^2 + dy^2
    b <- 2 * (p0[["x"]] * dx + p0[["y"]] * dy)
    c0 <- p0[["x"]]^2 + p0[["y"]]^2 - radius^2
    disc <- b^2 - 4 * a * c0
    if (!is.finite(disc) || disc < 0) return(c(x = NA_real_, y = NA_real_))
    roots <- c((-b - sqrt(disc)) / (2 * a), (-b + sqrt(disc)) / (2 * a))
    t <- roots[is.finite(roots) & roots >= 0 & roots <= 1]
    if (!length(t)) return(c(x = NA_real_, y = NA_real_))
    t <- t[[1]]
    c(x = p0[["x"]] + t * dx, y = p0[["y"]] + t * dy)
  }

  lf <- foul_point(-45, 330)
  rf <- foul_point(45, 331)
  lcf_wall <- c(x = -60, y = 399)
  rcf_wall <- c(x = 60, y = 399)

  l342 <- point_on_segment_at_radius(lf, lcf_wall, 342)
  l381 <- point_on_segment_at_radius(lf, lcf_wall, 381)
  r385 <- point_on_segment_at_radius(rcf_wall, rf, 385)
  r344 <- point_on_segment_at_radius(rcf_wall, rf, 344)

  wall <- tibble::tibble(
    label = c("LF 330", "LC 342", "LC 381", "LCF 399", "RCF 399", "RC 385", "RC 344", "RF 331"),
    x = c(lf[["x"]], l342[["x"]], l381[["x"]], lcf_wall[["x"]], rcf_wall[["x"]], r385[["x"]], r344[["x"]], rf[["x"]]),
    y = c(lf[["y"]], l342[["y"]], l381[["y"]], lcf_wall[["y"]], rcf_wall[["y"]], r385[["y"]], r344[["y"]], rf[["y"]])
  ) %>%
    dplyr::mutate(r = sqrt(.data$x^2 + .data$y^2))

  wall_inner <- wall %>% dplyr::mutate(
    r_inner = pmax(.data$r - track_width_ft, 0),
    x = .data$x * .data$r_inner / .data$r,
    y = .data$y * .data$r_inner / .data$r
  )

  track_poly <- dplyr::bind_rows(
    wall %>% dplyr::select(x, y),
    wall_inner %>% dplyr::select(x, y) %>% dplyr::arrange(dplyr::desc(dplyr::row_number()))
  )

  foul_tip <- function(bearing_deg, dist) {
    tibble::tibble(
      x = 0, y = 0,
      xend = dist * sin(bearing_deg * pi / 180),
      yend = dist * cos(bearing_deg * pi / 180)
    )
  }

  half <- 90 / sqrt(2)
  sec <- 90 * sqrt(2)
  bases_diamond <- tibble::tibble(
    x = c(0, half, 0, -half, 0),
    y = c(0, half, sec, half, 0)
  )

  ang <- seq(-45, 45, by = 0.5)
  make_ring <- function(r) {
    tibble::tibble(
      r = r,
      ang = ang,
      x = r * sin(ang * pi / 180),
      y = r * cos(ang * pi / 180)
    ) %>%
      dplyr::filter(abs(.data$ang) >= 4.5)
  }
  ring_distances <- c(seq(200, 450, by = 50), 475)

  list(
    wall = wall,
    track_poly = track_poly,
    foul_lines = dplyr::bind_rows(foul_tip(-45, 330), foul_tip(45, 331)),
    bases_diamond = bases_diamond,
    rings = dplyr::bind_rows(lapply(ring_distances, make_ring)),
    ring_labels = tibble::tibble(x = 0, y = ring_distances, label = paste0(ring_distances, " ft"))
  )
}

default_position_anchor <- function(pos) {
  anchors <- tibble::tribble(
    ~position, ~plot_x, ~plot_y,
    "P", 0, 58,
    "C", 0, 8,
    "1B", 82, 94,
    "2B", 52, 142,
    "3B", -82, 94,
    "SS", -52, 142,
    "LF", -180, 260,
    "CF", 0, 315,
    "RF", 180, 260
  )
  anchors %>% dplyr::filter(.data$position == !!pos) %>% dplyr::slice_head(n = 1)
}

opportunity_spray_plot <- function(d, pos = NULL, title = "Opportunities") {
  fld <- make_field_layers()
  pos <- pos %||% if (nrow(d)) d$position[[1]] else ""
  base <- ggplot() +
    geom_polygon(data = fld$track_poly, aes(x = x, y = y), fill = "white", color = NA) +
    geom_path(data = fld$wall, aes(x = x, y = y), color = "#501214", linewidth = 1.1) +
    geom_segment(data = fld$foul_lines, aes(x = x, y = y, xend = xend, yend = yend), color = "#501214", linewidth = 0.9) +
    geom_path(data = fld$rings, aes(x = x, y = y, group = r), linetype = "dashed", color = "grey45", linewidth = 0.45) +
    geom_text(data = fld$ring_labels, aes(x = x, y = y, label = label), color = "grey35", size = 3.1, vjust = 0.5) +
    geom_path(data = fld$bases_diamond, aes(x = x, y = y), color = "black", linewidth = 0.9) +
    coord_fixed(xlim = spray_xlim, ylim = spray_ylim, expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.title = element_text(face = "bold", color = "#501214", hjust = 0.5),
      legend.position = "bottom"
    )

  anchor <- if (nrow(d) && any(is.finite(d$start_x) & is.finite(d$start_y))) {
    d %>%
      dplyr::filter(is.finite(.data$start_x), is.finite(.data$start_y)) %>%
      dplyr::summarise(plot_x = mean(.data$start_y, na.rm = TRUE), plot_y = mean(.data$start_x, na.rm = TRUE))
  } else {
    default_position_anchor(pos)
  }
  if (nrow(anchor)) {
    base <- base +
      geom_label(
        data = anchor,
        aes(x = plot_x, y = plot_y, label = pos),
        inherit.aes = FALSE,
        fill = "#501214", color = "#B4975A", label.size = 0,
        fontface = "bold", size = 4.4
      )
  }

  total_opps <- sum(d$opportunity %in% TRUE, na.rm = TRUE)
  plot_d <- d %>%
    dplyr::filter(
      .data$opportunity %in% TRUE,
      .data$batted_ball_matched %in% TRUE,
      is.finite(.data$ball_x), is.finite(.data$ball_y)
    ) %>%
    dplyr::mutate(
      plot_x = .data$ball_y,
      plot_y = .data$ball_x,
      result_bucket = factor(.data$play_bucket, levels = c("Out", "Hit", "Error", "Other"))
    )

  if (!nrow(plot_d)) {
    msg <- if (total_opps > 0) {
      sprintf("%s opportunities found, but this file does not include batted-ball coordinates for plotting.", total_opps)
    } else {
      "No opportunities are available for this player and position."
    }
    return(base + annotate("text", x = 0, y = 445, label = msg, color = "#501214", fontface = "bold", size = 4.1))
  }

  ground_segments <- plot_d %>%
    dplyr::filter(.data$batted_ball_bucket == "Ground", is.finite(.data$bearing), is.finite(.data$distance)) %>%
    dplyr::mutate(
      final_distance = sqrt(.data$ball_x^2 + .data$ball_y^2),
      first_distance = dplyr::if_else(is.finite(.data$first_ground_distance), .data$first_ground_distance, 0),
      first_distance = pmin(pmax(.data$first_distance, 0), .data$final_distance),
      bearing_rad = .data$bearing * pi / 180,
      solid_xend = .data$first_distance * sin(.data$bearing_rad),
      solid_yend = .data$first_distance * cos(.data$bearing_rad)
    )

  base +
    geom_segment(
      data = ground_segments %>% dplyr::filter(.data$first_distance > 0.5),
      aes(x = 0, y = 0, xend = solid_xend, yend = solid_yend),
      inherit.aes = FALSE,
      color = "#4A4A4A", linewidth = 0.45, alpha = 0.7
    ) +
    geom_segment(
      data = ground_segments,
      aes(x = solid_xend, y = solid_yend, xend = plot_x, yend = plot_y),
      inherit.aes = FALSE,
      color = "#4A4A4A", linewidth = 0.55, alpha = 0.8, linetype = "dashed"
    ) +
    geom_point(
      data = plot_d,
      aes(x = plot_x, y = plot_y, color = result_bucket, shape = result_bucket),
      size = 3.2, alpha = 0.9, stroke = 1.2
    ) +
    scale_color_manual(
      values = c("Out" = "#0B7A3B", "Hit" = "#B00020", "Error" = "black", "Other" = "#777777"),
      limits = c("Out", "Hit", "Error", "Other"),
      drop = FALSE,
      name = NULL
    ) +
    scale_shape_manual(
      values = c("Out" = 16, "Hit" = 16, "Error" = 4, "Other" = 16),
      limits = c("Out", "Hit", "Error", "Other"),
      drop = FALSE,
      name = NULL
    )
}

range_sector_data <- function(d) {
  if (!nrow(d)) return(d)
  d <- d %>%
    dplyr::filter(
      .data$opportunity %in% TRUE,
      .data$batted_ball_matched %in% TRUE,
      is.finite(.data$start_x), is.finite(.data$start_y),
      is.finite(.data$ball_x), is.finite(.data$ball_y)
    )
  if (!nrow(d)) return(d)

  home_x <- 0
  home_y <- 0
  forward_x <- home_x - d$start_x
  forward_y <- home_y - d$start_y
  forward_len <- sqrt(forward_x^2 + forward_y^2)
  forward_x <- forward_x / forward_len
  forward_y <- forward_y / forward_len

  right_x <- forward_y
  right_y <- -forward_x
  ball_vec_x <- d$ball_x - d$start_x
  ball_vec_y <- d$ball_y - d$start_y

  side <- ball_vec_x * right_x + ball_vec_y * right_y
  forward <- ball_vec_x * forward_x + ball_vec_y * forward_y
  angle <- atan2(side, forward) * 180 / pi
  back_angle <- ifelse(angle >= 0, angle - 180, angle + 180)

  sector <- dplyr::case_when(
    abs(angle) <= 90 & angle < -30 ~ "In Left",
    abs(angle) <= 90 & angle > 30 ~ "In Right",
    abs(angle) <= 90 ~ "In",
    abs(angle) > 90 & back_angle < -30 ~ "Back Left",
    abs(angle) > 90 & back_angle > 30 ~ "Back Right",
    TRUE ~ "Back"
  )

  d$range_x <- side
  d$range_y <- forward
  d$range_angle <- angle
  d$range_sector <- factor(sector, levels = c("In Left", "In", "In Right", "Back Left", "Back", "Back Right"))
  d
}

if_ground_sector_data <- function(d) {
  d <- range_sector_data(d)
  if (!nrow(d)) return(d)
  angle <- pmin(pmax(d$range_angle, -89.999), 89.999)
  sector <- dplyr::case_when(
    angle < -45 ~ "In Far Left",
    angle < 0 ~ "In Left",
    angle < 45 ~ "In Right",
    TRUE ~ "In Far Right"
  )
  d$if_ground_sector <- factor(sector, levels = c("In Far Left", "In Left", "In Right", "In Far Right"))
  d
}

range_sector_polygons <- function(radius) {
  defs <- tibble::tribble(
    ~sector, ~start_deg, ~end_deg,
    "In Left", -90, -30,
    "In", -30, 30,
    "In Right", 30, 90,
    "Back Right", 90, 150,
    "Back", 150, 210,
    "Back Left", 210, 270
  )
  purrr::map_dfr(seq_len(nrow(defs)), function(i) {
    deg <- seq(defs$start_deg[i], defs$end_deg[i], length.out = 25)
    rad <- deg * pi / 180
    tibble::tibble(
      sector = defs$sector[i],
      x = c(0, radius * sin(rad), 0),
      y = c(0, radius * cos(rad), 0)
    )
  }) %>%
    dplyr::mutate(sector = factor(.data$sector, levels = c("In Left", "In", "In Right", "Back Left", "Back", "Back Right")))
}

if_ground_sector_polygons <- function(radius) {
  defs <- tibble::tribble(
    ~sector, ~start_deg, ~end_deg,
    "In Far Left", -90, -45,
    "In Left", -45, 0,
    "In Right", 0, 45,
    "In Far Right", 45, 90
  )
  purrr::map_dfr(seq_len(nrow(defs)), function(i) {
    deg <- seq(defs$start_deg[i], defs$end_deg[i], length.out = 25)
    rad <- deg * pi / 180
    tibble::tibble(
      sector = defs$sector[i],
      x = c(0, radius * sin(rad), 0),
      y = c(0, radius * cos(rad), 0)
    )
  }) %>%
    dplyr::mutate(sector = factor(.data$sector, levels = c("In Far Left", "In Left", "In Right", "In Far Right")))
}

if_ground_oaa_plot <- function(d, title = "IF Ground Ball OAA") {
  total_opps <- sum(d$opportunity %in% TRUE, na.rm = TRUE)
  d <- if_ground_sector_data(d)
  if (!nrow(d)) {
    return(empty_plot(
      title,
      if (total_opps > 0) {
        sprintf("%s opportunities found, but this file does not include batted-ball location needed for wedge placement.", total_opps)
      } else {
        "No infield ground-ball opportunities are available for this selection."
      }
    ))
  }

  point_radius <- sqrt(d$range_x^2 + d$range_y^2)
  radius <- max(60, suppressWarnings(max(point_radius, na.rm = TRUE)) * 1.08)
  sector_levels <- c("In Far Left", "In Left", "In Right", "In Far Right")
  sector_summary <- d %>%
    dplyr::group_by(.data$if_ground_sector) %>%
    dplyr::summarise(
      Opportunities = dplyr::n(),
      OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      tibble::tibble(if_ground_sector = factor(sector_levels, levels = sector_levels)),
      by = "if_ground_sector"
    ) %>%
    dplyr::mutate(
      Opportunities = dplyr::coalesce(.data$Opportunities, 0L),
      OAA_num = dplyr::coalesce(.data$OAA_num, 0)
    )

  polys <- if_ground_sector_polygons(radius) %>%
    dplyr::left_join(sector_summary, by = c("sector" = "if_ground_sector"))
  label_df <- tibble::tibble(
    sector = factor(sector_levels, levels = sector_levels),
    angle = c(-67.5, -22.5, 22.5, 67.5) * pi / 180
  ) %>%
    dplyr::mutate(
      x = radius * 0.55 * sin(.data$angle),
      y = radius * 0.55 * cos(.data$angle)
    ) %>%
    dplyr::left_join(sector_summary, by = c("sector" = "if_ground_sector")) %>%
    dplyr::mutate(
      label = sprintf(
        "%s\nOAA %+0.2f\n%s opp",
        as.character(.data$sector),
        .data$OAA_num,
        format(.data$Opportunities, big.mark = ",", trim = TRUE)
      )
    )
  max_abs_oaa <- suppressWarnings(max(abs(sector_summary$OAA_num), na.rm = TRUE))
  if (!is.finite(max_abs_oaa) || max_abs_oaa <= 0) max_abs_oaa <- 1
  point_d <- d %>%
    dplyr::mutate(
      point_result = factor(
        dplyr::case_when(
          .data$made_play %in% TRUE ~ "Made",
          .data$play_bucket == "Error" ~ "Error",
          TRUE ~ "Missed"
        ),
        levels = c("Made", "Missed", "Error")
      )
    )

  ggplot() +
    geom_polygon(data = polys, aes(x = x, y = y, group = sector, fill = OAA_num), alpha = 0.56, color = "white", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "#501214", linewidth = 0.8) +
    geom_vline(xintercept = 0, color = "#501214", linewidth = 0.8) +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((-45) * pi / 180), yend = radius * cos((-45) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((45) * pi / 180), yend = radius * cos((45) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_point(data = point_d, aes(x = range_x, y = range_y, color = point_result, shape = point_result), size = 2.6, alpha = 0.82) +
    geom_point(aes(x = 0, y = 0), size = 4, color = "#501214") +
    geom_label(data = label_df, aes(x = x, y = y, label = label), fontface = "bold", color = "#501214", fill = "white", label.size = 0, size = 3.7, lineheight = 0.95) +
    annotate("text", x = 0, y = radius * 0.88, label = "IN", fontface = "bold", color = "#501214") +
    scale_fill_gradient2(low = "#501214", mid = "#F7F3EA", high = "#0B7A3B", midpoint = 0, limits = c(-max_abs_oaa, max_abs_oaa), name = "OAA") +
    scale_color_manual(values = c("Made" = "#0B7A3B", "Missed" = "#B00020", "Error" = "black"), name = NULL, drop = FALSE) +
    scale_shape_manual(values = c("Made" = 16, "Missed" = 16, "Error" = 4), name = NULL, drop = FALSE) +
    coord_fixed(xlim = c(-radius, radius), ylim = c(0, radius), expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", color = "#501214", hjust = 0.5), legend.position = "bottom")
}

range_360_plot <- function(d, title = "360 Degree Range") {
  total_opps <- sum(d$opportunity %in% TRUE, na.rm = TRUE)
  d <- range_sector_data(d)
  if (!nrow(d)) {
    return(empty_plot(
      title,
      if (total_opps > 0) {
        sprintf("%s opportunities found, but this file does not include batted-ball location needed for wedge placement.", total_opps)
      } else {
        "No position-specific opportunities are available for this selection."
      }
    ))
  }
  point_radius <- sqrt(d$range_x^2 + d$range_y^2)
  radius <- max(60, suppressWarnings(max(point_radius, na.rm = TRUE)) * 1.08)
  sector_levels <- c("In Left", "In", "In Right", "Back Left", "Back", "Back Right")
  polys <- range_sector_polygons(radius)
  sector_summary <- d %>%
    dplyr::group_by(.data$range_sector) %>%
    dplyr::summarise(
      Opportunities = dplyr::n(),
      OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      tibble::tibble(range_sector = factor(sector_levels, levels = sector_levels)),
      by = "range_sector"
    ) %>%
    dplyr::mutate(
      Opportunities = dplyr::coalesce(.data$Opportunities, 0L),
      OAA_num = dplyr::coalesce(.data$OAA_num, 0)
    )
  polys <- polys %>%
    dplyr::left_join(sector_summary, by = c("sector" = "range_sector"))
  label_df <- tibble::tibble(
    sector = factor(sector_levels, levels = sector_levels),
    angle = c(-60, 0, 60, -120, 180, 120) * pi / 180
  ) %>%
    dplyr::mutate(
    x = radius * 0.54 * sin(.data$angle),
    y = radius * 0.54 * cos(.data$angle)
    ) %>%
    dplyr::left_join(sector_summary, by = c("sector" = "range_sector")) %>%
    dplyr::mutate(
      label = sprintf(
        "%s\nOAA %+0.2f\n%s opp",
        as.character(.data$sector),
        .data$OAA_num,
        format(.data$Opportunities, big.mark = ",", trim = TRUE)
      )
    )
  max_abs_oaa <- suppressWarnings(max(abs(sector_summary$OAA_num), na.rm = TRUE))
  if (!is.finite(max_abs_oaa) || max_abs_oaa <= 0) max_abs_oaa <- 1
  oaa_limits <- c(-max_abs_oaa, max_abs_oaa)
  point_d <- d %>%
    dplyr::mutate(
      point_result = factor(
        dplyr::case_when(
          .data$made_play %in% TRUE ~ "Made",
          .data$play_bucket == "Error" ~ "Error",
          TRUE ~ "Missed"
        ),
        levels = c("Made", "Missed", "Error")
      )
    )

  ggplot() +
    geom_polygon(data = polys, aes(x = x, y = y, group = sector, fill = OAA_num), alpha = 0.56, color = "white", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "#501214", linewidth = 0.8) +
    geom_vline(xintercept = 0, color = "#501214", linewidth = 0.8) +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((-30) * pi / 180), yend = radius * cos((-30) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((30) * pi / 180), yend = radius * cos((30) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((150) * pi / 180), yend = radius * cos((150) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_segment(aes(x = 0, y = 0, xend = radius * sin((210) * pi / 180), yend = radius * cos((210) * pi / 180)), color = "#501214", linewidth = 0.5, linetype = "dashed") +
    geom_point(data = point_d, aes(x = range_x, y = range_y, color = point_result, shape = point_result), size = 2.6, alpha = 0.82) +
    geom_point(aes(x = 0, y = 0), size = 4, color = "#501214") +
    geom_label(data = label_df, aes(x = x, y = y, label = label), fontface = "bold", color = "#501214", fill = "white", label.size = 0, size = 3.7, lineheight = 0.95) +
    annotate("text", x = 0, y = radius * 0.88, label = "IN", fontface = "bold", color = "#501214") +
    annotate("text", x = 0, y = -radius * 0.88, label = "BACK", fontface = "bold", color = "#501214") +
    scale_fill_gradient2(low = "#501214", mid = "#F7F3EA", high = "#0B7A3B", midpoint = 0, limits = oaa_limits, name = "OAA") +
    scale_color_manual(values = c("Made" = "#0B7A3B", "Missed" = "#B00020", "Error" = "black"), name = NULL, drop = FALSE) +
    scale_shape_manual(values = c("Made" = 16, "Missed" = 16, "Error" = 4), name = NULL, drop = FALSE) +
    coord_fixed(xlim = c(-radius, radius), ylim = c(-radius, radius), expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", color = "#501214", hjust = 0.5), legend.position = "bottom")
}

prepare_catcher_receiving_rows <- function(d) {
  if (!nrow(d)) return(d)

  pc_col <- pick_first(c("pitch_call", "PitchCall", "Pitch_Call", "Call", "PitchResult", "Pitch_Result"), d)
  d$pitch_call <- if (!is.na(pc_col)) as.character(d[[pc_col]]) else NA_character_
  d$pitch_call <- canon_pitch_call(d$pitch_call)

  if (all(c("source_file", "row_in_file") %in% names(d))) {
    d <- d %>% dplyr::distinct(.data$source_file, .data$row_in_file, .keep_all = TRUE)
  }

  n_now <- nrow(d)
  get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n_now)

  normalize_plate_coord <- function(vec, colname) {
    v <- to_num(vec)
    q90 <- suppressWarnings(stats::quantile(abs(v), 0.90, na.rm = TRUE))
    if (!is.finite(q90)) q90 <- 0
    is_inches <- !is.na(colname) && grepl("(?i)inch|_in\\b", colname)
    looks_inches <- if (!is.na(colname) && grepl("(?i)z|height", colname)) q90 > 6 else q90 > 3.5
    if (is_inches || looks_inches) v <- v / 12
    v
  }
  coalesce_plate <- function(cands) {
    cands <- intersect(cands, names(d))
    if (!length(cands)) return(rep(NA_real_, n_now))
    mats <- lapply(cands, function(nm) normalize_plate_coord(d[[nm]], nm))
    out <- mats[[1]]
    if (length(mats) > 1) for (k in 2:length(mats)) out <- dplyr::coalesce(out, mats[[k]])
    out
  }

  d$plate_x <- coalesce_plate(c("plate_x", "PlateLocSide", "px", "PlateX", "Plate_X", "PlateLocSideInches"))
  d$plate_z <- coalesce_plate(c("plate_z", "PlateLocHeight", "pz", "PlateZ", "Plate_Z", "PlateLocHeightInches"))
  d$PitchNum <- seq_len(nrow(d))

  pt_chr <- dplyr::coalesce(
    get_chr("PitchType_UNI"),
    get_chr("pitch_type_canon"),
    get_chr("PitchType"),
    get_chr("PitchName"),
    get_chr("TaggedPitchType"),
    get_chr("AutoPitchType")
  )
  pt_chr <- canonical_pitch_fuzzy(pt_chr)
  pt_chr[is.na(pt_chr) | !(pt_chr %in% pitch_levels_all)] <- "Undefined"
  d$PitchType <- factor(pt_chr, levels = pitch_levels_all)

  balls_col <- pick_first(c("Balls", "BallsBeforePitch", "BallCount", "BallsCount", "PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes", "StrikesBeforePitch", "StrikeCount", "StrikesCount", "PitcherStrikes"), d)
  balls <- if (!is.na(balls_col)) to_num(d[[balls_col]]) else rep(NA_real_, n_now)
  strikes <- if (!is.na(strikes_col)) to_num(d[[strikes_col]]) else rep(NA_real_, n_now)
  d$CountStr <- ifelse(is.na(balls) | is.na(strikes), "", sprintf("%s-%s", balls, strikes))

  for (nm in c("Inning", "PAofInning", "PitchofPA", "Pitcher", "Batter")) {
    if (!nm %in% names(d)) d[[nm]] <- NA
  }
  d
}

catcher_framing_subset <- function(d, type = c("ball_to_strike", "strike_to_ball")) {
  type <- match.arg(type)
  if (!nrow(d)) return(d[0, , drop = FALSE])

  px <- to_num(d$plate_x)
  pz <- to_num(d$plate_z)
  sx_in <- zone_s_in(px)
  hz_in <- zone_h_in(pz)
  in_zone_calc <- framing_in_zone(sx_in, hz_in)
  keep <- is.finite(px) & is.finite(pz)

  d <- d[keep, , drop = FALSE]
  in_zone_calc <- in_zone_calc[keep]
  pc <- canon_pitch_call(d$pitch_call)
  x_plot <- px[keep]
  z_plot <- pz[keep]
  if (is.finite(suppressWarnings(max(abs(x_plot), na.rm = TRUE))) && suppressWarnings(max(abs(x_plot), na.rm = TRUE)) > 5) x_plot <- x_plot / 12
  if (is.finite(suppressWarnings(max(abs(z_plot), na.rm = TRUE))) && suppressWarnings(max(abs(z_plot), na.rm = TRUE)) > 10) z_plot <- z_plot / 12
  d$plot_x <- x_plot
  d$plot_z <- z_plot

  if (type == "ball_to_strike") {
    d <- d %>% dplyr::filter(in_zone_calc %in% FALSE, pc %in% "StrikeCalled")
  } else {
    d <- d %>% dplyr::filter(in_zone_calc %in% TRUE, pc %in% "BallCalled")
  }

  if (!nrow(d)) return(d)
  d %>% dplyr::arrange(.data$PitchNum) %>% dplyr::mutate(PitchNumSub = dplyr::row_number())
}

catcher_framing_zone_plot <- function(d, title = NULL, show_pitch_numbers = TRUE) {
  xlim <- c(-2, 2)
  ylim <- c(-0.5, 4.5)
  heart_box <- data.frame(xmin = -6.7 / 12, xmax = 6.7 / 12, ymin = 22 / 12, ymax = 38 / 12)
  zone_box <- data.frame(xmin = -10 / 12, xmax = 10 / 12, ymin = 18 / 12, ymax = 42 / 12)
  shadow_outline <- data.frame(xmin = -13.3 / 12, xmax = 13.3 / 12, ymin = 14 / 12, ymax = 46 / 12)

  base <- ggplot() +
    geom_rect(data = shadow_outline, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linetype = "dashed", linewidth = 0.9) +
    geom_rect(data = zone_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.0) +
    geom_rect(data = heart_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "#0B7A3B", linetype = "dashed", linewidth = 1.0) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_reverse() +
    labs(title = title %||% "Strike Zone", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", color = "#501214"),
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  if (!nrow(d)) return(base)

  d$PitchType <- factor(as.character(d$PitchType), levels = pitch_levels_all)
  d <- d %>% dplyr::filter(is.finite(.data$plot_x), is.finite(.data$plot_z))
  if (!nrow(d)) return(base)

  out <- base %+% d +
    geom_point(aes(x = -plot_x, y = plot_z, fill = PitchType),
               shape = 21, size = 7.4, colour = "black", stroke = 0.9, alpha = 0.95) +
    scale_fill_manual(values = pitch_colors, breaks = facet_levels, limits = facet_levels, drop = FALSE, name = "Pitch Type")
  if (show_pitch_numbers) {
    out <- out +
      geom_text(aes(x = -plot_x, y = plot_z, label = PitchNumSub), size = 4.4, fontface = "bold", color = "white")
  }
  out
}

catcher_framing_legend_plot <- function() {
  labs_df <- data.frame(PitchType = facet_levels, y = seq_along(facet_levels))
  ggplot(labs_df, aes(x = 0.2, y = y)) +
    geom_point(aes(fill = PitchType), shape = 21, size = 4.6, color = "black", stroke = 0.7) +
    geom_text(aes(x = 0.75, label = PitchType), hjust = 0, size = 4.0, color = "black") +
    scale_fill_manual(values = pitch_colors, limits = facet_levels, drop = FALSE) +
    coord_cartesian(xlim = c(0, 3.2), ylim = c(0.5, length(facet_levels) + 0.5), expand = FALSE, clip = "off") +
    theme_void(base_size = 12) +
    theme(legend.position = "none", plot.margin = margin(5, 10, 5, 5), plot.background = element_rect(fill = "white", color = NA))
}

catcher_framing_table <- function(d) {
  if (!nrow(d)) {
    return(tibble::tibble(P = integer(), Inn = integer(), PA = integer(), `PA P#` = integer(), Pitcher = character(), `Pitch type` = character(), Hitter = character(), Count = character(), Result = character()))
  }
  get_chr <- function(nm) if (nm %in% names(d)) nz_chr(d[[nm]]) else rep("", nrow(d))
  last_name <- function(x) {
    x <- nz_chr(x)
    ifelse(grepl(",", x), sub(",.*$", "", x), sub("^.*\\s+", "", x))
  }
  d %>%
    dplyr::mutate(
      P = .data$PitchNumSub,
      PA = .data$PAofInning,
      `PA P#` = .data$PitchofPA,
      Inn = .data$Inning,
      `Pitch type` = as.character(.data$PitchType),
      Pitcher = last_name(get_chr("Pitcher")),
      Hitter = last_name(get_chr("Batter")),
      Count = get_chr("CountStr"),
      Result = get_chr("pitch_call")
    ) %>%
    dplyr::select(P, Inn, PA, `PA P#`, Pitcher, `Pitch type`, Hitter, Count, Result)
}

catcher_stats_table <- function(d, mode) {
  label <- if (mode == "ball_to_strike") "Strikes Stolen" else "Strikes Lost"
  framing_zone_stats(d, mode) %>%
    dplyr::mutate(
      `%` = dplyr::if_else(is.finite(.data$Pct), sprintf("%.1f%%", 100 * .data$Pct), ""),
      Chances = as.integer(.data$Chances),
      Value = as.integer(.data$Strikes)
    ) %>%
    dplyr::select("Zone", "Chances", "Value", "%") %>%
    rlang::set_names(c("Zone", "Chances", label, "%"))
}

compact_catcher_stats_ui <- function(tbl) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(tags$div(class = "cr-compact-empty", "No stats"))
  }
  tags$table(
    class = "cr-compact-table",
    tags$thead(tags$tr(lapply(names(tbl), tags$th))),
    tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
      tags$tr(lapply(tbl[i, , drop = FALSE], function(x) tags$td(as.character(x[[1]]))))
    }))
  )
}

framing_pitch_group <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("Fastball", "Sinker") ~ "Fastballs/Sinkers",
    x %in% c("Cutter", "Slider", "Sweeper", "Curveball") ~ "Breaking Balls",
    x %in% c("Changeup", "Splitter") ~ "Soft",
    TRUE ~ "Other"
  )
}

framing_metric_base <- function(d) {
  if (is.null(d) || !nrow(d)) {
    return(tibble::tibble(
      pitch_call = character(), in_zone = logical(), zone_type = character(),
      PitcherHand = character(), PitchGroup = character(), frame_value = numeric()
    ))
  }
  px <- to_num(d$plate_x)
  pz <- to_num(d$plate_z)
  sx_in <- zone_s_in(px)
  hz_in <- zone_h_in(pz)
  pc <- canon_pitch_call(d$pitch_call)
  pitch_type <- if ("PitchType" %in% names(d)) as.character(d$PitchType) else rep("Undefined", nrow(d))
  pitcher_hand <- if ("PitcherHand" %in% names(d)) as.character(d$PitcherHand) else rep("Unknown", nrow(d))

  out <- tibble::tibble(
    pitch_call = pc,
    valid_location = is.finite(px) & is.finite(pz),
    in_zone = framing_in_zone(sx_in, hz_in),
    zone_type = framing_zone_type(sx_in, hz_in),
    PitcherHand = pitcher_hand,
    PitchGroup = framing_pitch_group(pitch_type)
  ) %>%
    dplyr::filter(.data$valid_location, .data$pitch_call %in% c("StrikeCalled", "BallCalled")) %>%
    dplyr::select(-valid_location)

  out %>%
    dplyr::mutate(
      frame_value = dplyr::case_when(
        .data$pitch_call == "StrikeCalled" & !.data$in_zone ~ 1,
        .data$pitch_call == "BallCalled" & .data$in_zone ~ -1,
        TRUE ~ 0
      )
    )
}

summarise_catcher_percentile_metrics <- function(d) {
  base <- framing_metric_base(d)
  rate_row <- function(metric_id, denom, num) {
    chances <- sum(denom %in% TRUE, na.rm = TRUE)
    numerator <- sum(num[denom %in% TRUE] %in% TRUE, na.rm = TRUE)
    tibble::tibble(metric_id = metric_id, value = safe_ratio(numerator, chances), chances = as.integer(chances), numerator = numerator)
  }
  net_row <- function(metric_id, mask) {
    chances <- sum(mask %in% TRUE, na.rm = TRUE)
    numerator <- sum(base$frame_value[mask %in% TRUE], na.rm = TRUE)
    tibble::tibble(metric_id = metric_id, value = safe_ratio(numerator, chances), chances = as.integer(chances), numerator = numerator)
  }

  if (!nrow(base)) {
    return(framing_metric_specs %>%
             dplyr::mutate(value = NA_real_, chances = 0L, numerator = NA_real_))
  }

  metric_rows <- dplyr::bind_rows(
    rate_row("strikes_stolen", !base$in_zone, base$pitch_call == "StrikeCalled"),
    rate_row("balls_lost", base$in_zone, base$pitch_call == "BallCalled"),
    rate_row("bottom_buffer", base$zone_type == "Bottom Buffer", base$pitch_call == "StrikeCalled"),
    rate_row("top_buffer", base$zone_type == "Top Buffer", base$pitch_call == "StrikeCalled"),
    rate_row("glove_side_buffer", base$zone_type == "Gloveside Buffer", base$pitch_call == "StrikeCalled"),
    rate_row("arm_side_buffer", base$zone_type == "Arm Side Buffer", base$pitch_call == "StrikeCalled"),
    net_row("rhp", base$PitcherHand == "RHP"),
    net_row("lhp", base$PitcherHand == "LHP"),
    net_row("fastballs_sinkers", base$PitchGroup == "Fastballs/Sinkers"),
    net_row("breaking_balls", base$PitchGroup == "Breaking Balls"),
    net_row("soft", base$PitchGroup == "Soft"),
    net_row("lhp_fastballs_sinkers", base$PitcherHand == "LHP" & base$PitchGroup == "Fastballs/Sinkers"),
    net_row("lhp_breaking_balls", base$PitcherHand == "LHP" & base$PitchGroup == "Breaking Balls"),
    net_row("lhp_soft", base$PitcherHand == "LHP" & base$PitchGroup == "Soft"),
    net_row("rhp_fastballs_sinkers", base$PitcherHand == "RHP" & base$PitchGroup == "Fastballs/Sinkers"),
    net_row("rhp_breaking_balls", base$PitcherHand == "RHP" & base$PitchGroup == "Breaking Balls"),
    net_row("rhp_soft", base$PitcherHand == "RHP" & base$PitchGroup == "Soft")
  )

  framing_metric_specs %>%
    dplyr::left_join(metric_rows, by = "metric_id") %>%
    dplyr::arrange(.data$metric_order)
}

add_d1_percentiles <- function(metrics, baseline = d1_catcher_framing_baseline) {
  if (is.null(metrics) || !nrow(metrics)) return(metrics)
  metrics %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      min_chances = framing_metric_min_chances(.data$metric_id),
      d1_average = {
        vals <- baseline$value[baseline$metric_id == .data$metric_id & baseline$chances >= min_chances & is.finite(baseline$value)]
        if (length(vals)) mean(vals, na.rm = TRUE) else NA_real_
      },
      percentile = {
        vals <- baseline$value[baseline$metric_id == .data$metric_id & baseline$chances >= min_chances & is.finite(baseline$value)]
        if (!length(vals) || !is.finite(.data$value) || .data$chances <= 0) {
          NA_real_
        } else if (.data$better == "low") {
          100 * mean(vals >= .data$value, na.rm = TRUE)
        } else {
          100 * mean(vals <= .data$value, na.rm = TRUE)
        }
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentile = pmax(0, pmin(100, round(.data$percentile))))
}

percentile_color <- function(pct) {
  if (!is.finite(pct)) return("#BFC9CA")
  dplyr::case_when(
    pct >= 90 ~ "#1B5E20",
    pct >= 75 ~ "#2E7D32",
    pct >= 60 ~ "#66A96B",
    pct >= 40 ~ "#E3B2B2",
    pct >= 25 ~ "#DF7777",
    TRUE ~ "#B71C1C"
  )
}

format_framing_metric_value <- function(value, metric_type) {
  if (!is.finite(value)) return("")
  if (metric_type == "net_rate") sprintf("%+.1f%%", 100 * value) else sprintf("%.1f%%", 100 * value)
}

percentile_rankings_ui <- function(metrics, catcher_name = NULL) {
  if (is.null(metrics) || !nrow(metrics)) {
    return(div(class = "cr-percentile-card", div(class = "cr-percentile-empty", "No percentile data")))
  }
  catcher_label <- name_display(catcher_name %||% "Catcher")
  rows <- lapply(seq_len(nrow(metrics)), function(i) {
    row <- metrics[i, , drop = FALSE]
    pct <- row$percentile[[1]]
    pct_label <- if (is.finite(pct)) as.character(as.integer(pct)) else "--"
    pct_pos <- if (is.finite(pct)) pct else 50
    color <- percentile_color(pct)
    disabled_class <- if (is.finite(pct)) "" else " cr-percentile-row-empty"
    div(
      class = paste0("cr-percentile-row", disabled_class),
      div(class = "cr-percentile-label", row$metric[[1]]),
      div(
        class = "cr-percentile-track-wrap",
        div(class = "cr-percentile-track"),
        div(class = "cr-percentile-fill", style = sprintf("width:%s%%;background:%s;", pct_pos, color)),
        div(class = "cr-percentile-average"),
        div(class = "cr-percentile-badge", style = sprintf("left:%s%%;background:%s;", pct_pos, color), pct_label)
      ),
      div(class = "cr-percentile-value", format_framing_metric_value(row$value[[1]], row$metric_type[[1]]))
    )
  })
  div(
    class = "cr-percentile-card",
    div(
      class = "cr-percentile-header",
      div(class = "cr-percentile-title", "D1 Percentile Rankings"),
      div(class = "cr-percentile-subtitle", catcher_label)
    ),
    div(
      class = "cr-percentile-scale",
      span("POOR"),
      span("AVERAGE"),
      span("GREAT")
    ),
    div(class = "cr-percentile-rows", tagList(rows))
  )
}

render_catcher_dt <- function(tbl) {
  dt <- DT::datatable(
    tbl,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE)
  )
  if ("Result" %in% names(tbl)) {
    dt <- DT::formatStyle(
      dt,
      "Result",
      backgroundColor = DT::styleEqual(c("StrikeCalled", "BallCalled"), c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)"))
    )
  }
  dt
}

write_catcher_receiving_pdf <- function(d, catcher_name, file) {
  d_left <- catcher_framing_subset(d, "ball_to_strike")
  d_right <- catcher_framing_subset(d, "strike_to_ball")
  gdate <- if ("GameDate" %in% names(d)) d$GameDate[!is.na(d$GameDate)][1] else as.Date(NA)
  hdr_txt <- if (is.na(gdate)) {
    sprintf("%s Catcher Receiving Scorecard", catcher_name)
  } else {
    sprintf("%s Catcher Receiving Scorecard - %s", catcher_name, format(gdate, "%B %d, %Y"))
  }

  open_pdf <- function(path) {
    ok <- FALSE
    tryCatch({ grDevices::cairo_pdf(path, width = 11, height = 8.5); ok <<- TRUE }, error = function(e) {})
    if (!ok) grDevices::pdf(path, width = 11, height = 8.5, useDingbats = FALSE)
  }
  open_pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 3,
    heights = grid::unit.c(grid::unit(0.08, "npc"), grid::unit(0.50, "npc"), grid::unit(0.42, "npc")),
    widths = grid::unit.c(grid::unit(4.75, "in"), grid::unit(1.0, "in"), grid::unit(4.75, "in"))
  )
  grid::pushViewport(grid::viewport(layout = layout))
  draw_in <- function(g, row, col) {
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
    grid::grid.draw(g)
    grid::popViewport()
  }

  draw_in(grid::textGrob(hdr_txt, gp = grid::gpar(fontface = 2, cex = 1.15, col = "#501214")), 1, 1:3)
  draw_in(ggplotGrob(catcher_framing_zone_plot(d_left, "Ball to Strike")), 2, 1)
  draw_in(ggplotGrob(catcher_framing_legend_plot()), 2, 2)
  draw_in(ggplotGrob(catcher_framing_zone_plot(d_right, "Strike to Ball")), 2, 3)

  ttheme <- gridExtra::ttheme_minimal(
    core = list(fg_params = list(cex = 0.78), padding = grid::unit(c(3, 4), "pt")),
    colhead = list(fg_params = list(cex = 0.78, fontface = 2, col = "#B4975A"), bg_params = list(fill = "#501214", col = NA))
  )
  left_tbl <- catcher_framing_table(d_left)
  right_tbl <- catcher_framing_table(d_right)
  if (!nrow(left_tbl)) left_tbl <- tibble::tibble(Message = "No ball-to-strike pitches")
  if (!nrow(right_tbl)) right_tbl <- tibble::tibble(Message = "No strike-to-ball pitches")
  draw_in(gridExtra::tableGrob(left_tbl, rows = NULL, theme = ttheme), 3, 1)
  draw_in(gridExtra::tableGrob(right_tbl, rows = NULL, theme = ttheme), 3, 3)
  grid::popViewport()
}

# -------------------- Theme & CSS --------------------
txst_theme <- bs_theme(
  version = 5,
  primary = "#501214",
  secondary = "#B4975A",
  bootswatch = "flatly"
)

app_title_link <- tags$a(
  href = "#",
  class = "app-title-link",
  "Bobcats Defense Reports"
)

head_css <- htmltools::tags$head(
  htmltools::tags$style(HTML("
    :root { --txst-maroon:#501214; --txst-gold:#B4975A; }

    .navbar,.bslib-navbar,.bslib-page-header,.page-sidebar .navbar,.page-sidebar .navbar .container-fluid{
      background-color:var(--txst-maroon)!important;border:0!important}
    .navbar .navbar-brand,.navbar .nav-link,.bslib-navbar .navbar-brand,.bslib-page-header .navbar-brand,.app-title-link{
      color:var(--txst-gold)!important}
    .navbar,.bslib-navbar{box-shadow:none!important}
    .app-title-link{color:var(--txst-gold);font-weight:700;text-decoration:none}

    body::after{
      content:'';position:fixed;inset:0;
      background-image:url('Bobcatlogo.png');
      background-repeat:no-repeat;background-position:center;background-size:85vmin;
      opacity:0.7;pointer-events:none;z-index:0;
    }
    body::before{
      content:'';position:fixed;
      right:max(16px,env(safe-area-inset-right));bottom:max(16px,env(safe-area-inset-bottom));
      width:clamp(120px,22vmin,360px);height:clamp(120px,22vmin,360px);
      background-image:url('txstlogo.jpeg');
      background-repeat:no-repeat;background-position:right bottom;background-size:contain;
      opacity:0.7;pointer-events:none;z-index:0;
    }

    .container-fluid,.bslib-grid,.page-sidebar,.nav-tabs,.tab-content,.card,.table,.dataTables_wrapper{
      position:relative;z-index:1;
    }
    .tab-content{padding-bottom:clamp(120px,22vmin,360px)}
    .nav-tabs{
      background-color:var(--txst-maroon)!important;border-bottom:2px solid var(--txst-maroon)!important;
      padding:.25rem .5rem;border-radius:.5rem;margin-bottom:.75rem}
    .nav-tabs .nav-link{color:var(--txst-gold)!important;background-color:transparent!important;border:0!important}
    .nav-tabs .nav-link:hover,.nav-tabs .nav-link:focus,.nav-tabs .nav-link.active{
      color:var(--txst-gold)!important;background-color:transparent!important;box-shadow: inset 0 -3px 0 0 var(--txst-gold)}

    .btn-primary{background-color:var(--txst-maroon)!important;border-color:var(--txst-maroon)!important}
    .btn-primary:hover{filter:brightness(0.9)}

    .table, .bslib-card, .card, .defense-panel{
      background-color:rgba(255,255,255,0.9)!important;
      border-radius:8px;
    }
    table th,.table th{background-color:var(--txst-maroon)!important;color:var(--txst-gold)!important}
    .table tbody tr:hover{background-color:rgba(80,18,20,0.08)!important}
    .dt-row-odd{background-color:#f9f9f9!important}
    .dt-row-even{background-color:#ffffff!important}

    .defense-panel{padding:12px 14px;margin-bottom:12px}
    .opportunity-field-panel{padding:0;overflow:hidden}
    .opportunity-toolbar{display:flex;gap:12px;align-items:end;flex-wrap:wrap}
    .metric-strip{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px;margin-bottom:12px}
    .metric-tile{background:rgba(255,255,255,0.94);border:1px solid rgba(80,18,20,0.14);border-radius:8px;padding:10px 12px}
    .metric-label{font-size:.78rem;text-transform:uppercase;color:#501214;font-weight:800}
    .metric-value{font-size:1.6rem;line-height:1.1;color:#222;font-weight:800}
    @media (max-width: 900px){.metric-strip{grid-template-columns:repeat(2,minmax(0,1fr));}}

    .cr-season-layout{align-items:stretch}
    .cr-percentile-column{display:flex}
    .cr-percentile-column > .shiny-html-output{display:flex;width:100%}
    .cr-percentile-card{background:rgba(255,255,255,0.94);border:1px solid rgba(0,117,138,.32);border-radius:8px;padding:12px 14px;margin-bottom:12px;min-height:920px;width:100%;display:flex;flex-direction:column}
    .cr-percentile-header{display:flex;align-items:flex-start;justify-content:space-between;gap:10px;border-bottom:2px solid #078197;padding-bottom:6px;margin-bottom:8px}
    .cr-percentile-title{font-weight:800;color:#222;font-size:1rem;line-height:1.1}
    .cr-percentile-subtitle{font-size:.78rem;font-weight:800;color:#078197;text-align:right;line-height:1.1}
    .cr-percentile-scale{display:grid;grid-template-columns:1fr 1fr 1fr;margin:0 44px 4px 126px;font-size:.62rem;font-weight:800;letter-spacing:.02em}
    .cr-percentile-scale span:nth-child(1){color:#B71C1C;text-align:left}
    .cr-percentile-scale span:nth-child(2){color:#A4BEC1;text-align:center}
    .cr-percentile-scale span:nth-child(3){color:#1B5E20;text-align:right}
    .cr-percentile-rows{flex:1;display:flex;flex-direction:column;justify-content:space-between}
    .cr-percentile-row{display:grid;grid-template-columns:118px minmax(110px,1fr) 42px;gap:7px;align-items:center;min-height:28px}
    .cr-percentile-label{font-size:.72rem;line-height:1.05;text-align:right;color:#444;white-space:normal}
    .cr-percentile-track-wrap{position:relative;height:18px}
    .cr-percentile-track{position:absolute;left:0;right:0;top:7px;height:5px;background:#D8E8E9;border-radius:0}
    .cr-percentile-fill{position:absolute;left:0;top:4px;height:11px;border-radius:0}
    .cr-percentile-average{position:absolute;left:50%;top:3px;width:2px;height:13px;background:rgba(255,255,255,.55)}
    .cr-percentile-badge{position:absolute;top:0;transform:translateX(-50%);min-width:24px;height:18px;border-radius:10px;color:white;font-size:.68rem;font-weight:900;line-height:18px;text-align:center;padding:0 5px}
    .cr-percentile-value{font-size:.68rem;color:#444;text-align:right;white-space:nowrap}
    .cr-percentile-row-empty{opacity:.55}
    .cr-percentile-empty{font-size:.85rem;color:#555}
    .cr-zone-row{display:grid;grid-template-columns:minmax(360px,1fr) minmax(320px,380px);gap:10px;align-items:start;margin-bottom:12px}
    .cr-zone-plot-panel,.cr-zone-table-panel{margin-bottom:0}
    .cr-zone-table-panel{overflow-x:auto}
    .cr-zone-table-panel table{width:100%!important;margin-bottom:0}
    .cr-compact-table{width:auto!important;table-layout:auto;border-collapse:collapse;font-size:.72rem;white-space:nowrap;margin:0}
    .cr-compact-table th,.cr-compact-table td{padding:4px 7px;border-bottom:1px solid rgba(80,18,20,.12);text-align:right}
    .cr-compact-table th:first-child,.cr-compact-table td:first-child{text-align:left}
    .cr-compact-table th{background-color:var(--txst-maroon)!important;color:var(--txst-gold)!important;font-weight:800}
    .cr-compact-empty{font-size:.8rem;color:#555}
    @media (max-width: 1100px){
      .cr-zone-row{grid-template-columns:1fr}
      .cr-percentile-row{grid-template-columns:108px minmax(120px,1fr) 40px}
      .cr-percentile-scale{margin-left:116px}
    }
  "))
)

# BASE supplies a scoped page shell when Defense runs inside the unified app.
if (isTRUE(get0("BASE_DEFENSE_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
  txst_theme <- get0("BASE_DEFENSE_THEME", inherits = FALSE, ifnotfound = txst_theme)
  head_css <- get0("BASE_DEFENSE_HEAD", inherits = FALSE, ifnotfound = head_css)
  app_title_link <- NULL
  base_defense_page <- function(..., title = NULL, sidebar = NULL, theme = NULL) {
    dots <- list(...)
    is_head_item <- vapply(dots, function(item) {
      inherits(item, "shiny.tag") && item$name %in% c("head", "script")
    }, logical(1))
    sidebar_children <- if (inherits(sidebar, "bslib_sidebar")) {
      c(list(sidebar$title), sidebar$children)
    } else list(sidebar)
    htmltools::tagList(
      dots[is_head_item],
      htmltools::tags$div(
        class = "base-defense-embedded-layout",
        htmltools::tags$aside(class = "base-defense-sidebar", sidebar_children),
        htmltools::tags$main(class = "base-defense-main", dots[!is_head_item])
      )
    )
  }
} else {
  base_defense_page <- page_sidebar
}

base_catching_postgame_choices <- character(0)
if (nrow(catching_df) && "Catcher" %in% names(catching_df)) {
  catcher_values <- unique(nz_chr(catching_df$Catcher))
  catcher_values <- catcher_values[nzchar(catcher_values)]
  if (length(catcher_values)) {
    catcher_labels <- name_display(catcher_values)
    catcher_order <- order(catcher_labels)
    base_catching_postgame_choices <- stats::setNames(
      catcher_values[catcher_order],
      catcher_labels[catcher_order]
    )
  }
}

base_catching_postgame_game_choices <- character(0)
if (length(base_catching_postgame_choices)) {
  default_catcher <- unname(base_catching_postgame_choices[[1]])
  default_rows <- catching_df %>% dplyr::filter(.data$Catcher == default_catcher)
  default_games <- tibble::tibble(
    gid = as.character(default_rows$CustomGameID),
    gdate = default_rows$GameDate,
    season = default_rows$SeasonGroup,
    home = coalesce_chr_cols(default_rows, c("HomeTeam")),
    away = coalesce_chr_cols(default_rows, c("AwayTeam")),
    team = coalesce_chr_cols(default_rows, c("CatcherTeam", "PitcherTeam"))
  ) %>%
    dplyr::filter(!is.na(.data$gid), nzchar(.data$gid)) %>%
    dplyr::distinct(.data$gid, .data$gdate, .data$season, .data$home, .data$away, .data$team) %>%
    dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  base_catching_postgame_game_choices <- catcher_game_choices(default_games)
}

base_catching_postgame_ui <- tagList(
  div(
    class = "mb-2 d-flex align-items-center justify-content-between",
    div(
      style = "display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
      selectInput(
        "def_AARCatcher", "Catcher",
        choices = base_catching_postgame_choices,
        selected = if (length(base_catching_postgame_choices)) unname(base_catching_postgame_choices[[1]]) else NULL,
        width = "260px"
      ),
      selectInput(
        "def_AARCatchGame", "Game (most recent first)",
        choices = base_catching_postgame_game_choices,
        selected = if (length(base_catching_postgame_game_choices)) unname(base_catching_postgame_game_choices[[1]]) else NULL,
        width = "320px"
      ),
      downloadButton("def_catcher_framing_pdf", "Download Catcher Receiving (PDF)", class = "btn btn-primary")
    )
  ),
  fluidRow(
    column(
      width = 5,
      div(class = "defense-panel", div(class = "mb-2", tableOutput("def_catcher_ball_to_strike_stats")), plotOutput("def_catcher_ball_to_strike_plot", height = "620px"))
    ),
    column(width = 2, div(class = "defense-panel", plotOutput("def_catcher_pitch_legend", height = "620px", width = "100%"))),
    column(
      width = 5,
      div(class = "defense-panel", div(class = "mb-2", tableOutput("def_catcher_strike_to_ball_stats")), plotOutput("def_catcher_strike_to_ball_plot", height = "620px"))
    )
  ),
  fluidRow(
    column(width = 6, div(class = "defense-panel", style = "width:100%; overflow-x:auto;", DTOutput("def_catcher_ball_to_strike_tbl"))),
    column(width = 6, div(class = "defense-panel", style = "width:100%; overflow-x:auto;", DTOutput("def_catcher_strike_to_ball_tbl")))
  )
)

# -------------------- UI --------------------
ui <- base_defense_page(
  theme = txst_theme,
  title = app_title_link,
  head_css,
  sidebar = sidebar(
    title = "Defense Filters",
    checkboxGroupInput(
      "def_season_groups", "Quick-select seasons",
      choices = SEASON_CHOICES,
      selected = if ("S26" %in% unname(SEASON_CHOICES)) "S26" else unname(SEASON_CHOICES),
      inline = TRUE
    )
  ),
  navset_tab(
    nav_panel(
      "Leaderboard",
      navset_tab(
        nav_panel("Overall", div(class = "defense-panel", withSpinner(DTOutput("leaderboard_overall_table"), type = 4, color = "#501214"))),
        nav_panel("IF", div(class = "defense-panel", withSpinner(DTOutput("leaderboard_if_table"), type = 4, color = "#501214"))),
        nav_panel("OF", div(class = "defense-panel", withSpinner(DTOutput("leaderboard_of_table"), type = 4, color = "#501214")))
      )
    ),
    nav_panel(
      "Opportunities",
      div(
        class = "defense-panel opportunity-toolbar",
        selectInput(
          "opportunity_player",
          "Player",
          choices = player_choices,
          selected = if (length(player_choices)) player_choices[[1]] else NULL,
          width = "260px",
          selectize = TRUE
        ),
        selectInput(
          "opportunity_position",
          "Position",
          choices = position_choices,
          selected = if (length(position_choices)) position_choices[[1]] else NULL,
          width = "220px"
        )
      ),
      div(class = "defense-panel opportunity-field-panel", withSpinner(plotOutput("opportunity_spray", height = "78vh"), type = 4, color = "#501214")),
      div(class = "defense-panel", withSpinner(DTOutput("opportunity_table"), type = 4, color = "#501214"))
    ),
    nav_panel(
      "OF OAA",
      div(
        class = "defense-panel opportunity-toolbar",
        selectInput(
          "of_player",
          "Player",
          choices = player_choices,
          selected = if (length(player_choices)) player_choices[[1]] else NULL,
          width = "260px",
          selectize = TRUE
        ),
        selectInput(
          "range_position",
          "Position",
          choices = position_choices,
          selected = if ("SS" %in% position_choices) "SS" else position_choices[[1]],
          width = "220px"
        )
      ),
      div(class = "defense-panel", withSpinner(plotOutput("range_360", height = "700px"), type = 4, color = "#501214")),
      div(class = "defense-panel", withSpinner(DTOutput("range_sector_table"), type = 4, color = "#501214"))
    ),
    nav_panel(
      "IF OAA",
      div(
        class = "defense-panel opportunity-toolbar",
        selectInput(
          "if_player",
          "Player",
          choices = player_choices,
          selected = if (length(player_choices)) player_choices[[1]] else NULL,
          width = "260px",
          selectize = TRUE
        ),
        selectInput(
          "if_position",
          "Position",
          choices = position_choices,
          selected = if ("SS" %in% position_choices) "SS" else position_choices[[1]],
          width = "220px"
        )
      ),
      navset_tab(
        nav_panel(
          "Ground Balls",
          div(class = "defense-panel", withSpinner(plotOutput("if_ground_oaa_plot", height = "700px"), type = 4, color = "#501214")),
          div(class = "defense-panel", withSpinner(DTOutput("if_ground_oaa_table"), type = 4, color = "#501214"))
        ),
        nav_panel(
          "Air Balls",
          div(class = "defense-panel", withSpinner(plotOutput("if_air_oaa_plot", height = "700px"), type = 4, color = "#501214")),
          div(class = "defense-panel", withSpinner(DTOutput("if_air_oaa_table"), type = 4, color = "#501214"))
        )
      )
    ),
    nav_panel(
      "OAA",
      div(
        class = "defense-panel opportunity-toolbar",
        selectInput(
          "oaa_player",
          "Player",
          choices = player_choices,
          selected = if (length(player_choices)) player_choices[[1]] else NULL,
          width = "260px",
          selectize = TRUE
        ),
        selectInput(
          "oaa_position",
          "Position",
          choices = position_choices,
          selected = if (length(position_choices)) position_choices[[1]] else NULL,
          width = "220px"
        )
      ),
      div(class = "defense-panel", withSpinner(plotOutput("oaa_plot", height = "520px"), type = 4, color = "#501214")),
      div(class = "defense-panel", withSpinner(DTOutput("oaa_table"), type = 4, color = "#501214"))
    ),
    if (!isTRUE(get0("BASE_DEFENSE_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
      nav_panel("Catcher Reports", base_catching_postgame_ui)
    },
    nav_panel(
      "Catcher Season",
      div(
        class = "mb-2 d-flex align-items-center justify-content-between",
        div(
          style = "display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
          selectInput("def_CR_catcher", "Catcher", choices = character(0), selected = NULL, width = "240px"),
          pickerInput(
            "def_CR_games", HTML("Games<br>(auto-selected by season)"),
            choices = character(0), selected = character(0),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = TRUE,
            width = "320px"
          ),
          selectInput("def_CR_pitcher_hand", "Pitcher Handedness", choices = c("All", "LHP", "RHP", "Unknown"), selected = "All", width = "180px"),
          pickerInput(
            "def_CR_pitch_types", "Pitch Types",
            choices = facet_levels,
            selected = facet_levels,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            width = "260px"
          )
        )
      ),
      fluidRow(
        class = "cr-season-layout",
        column(
          width = 4,
          class = "cr-percentile-column",
          uiOutput("cr_percentile_rankings")
        ),
        column(
          width = 8,
          div(
            class = "cr-zone-row",
            div(class = "defense-panel cr-zone-plot-panel", plotOutput("def_cr_ball_to_strike_plot", height = "430px")),
            div(class = "defense-panel cr-zone-table-panel", tableOutput("def_cr_ball_to_strike_stats"))
          ),
          div(
            class = "cr-zone-row",
            div(class = "defense-panel cr-zone-plot-panel", plotOutput("def_cr_strike_to_ball_plot", height = "430px")),
            div(class = "defense-panel cr-zone-table-panel", tableOutput("def_cr_strike_to_ball_stats"))
          )
        )
      )
    )
  )
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  data_all <- reactive(defense_df)

  filtered_data <- reactive({
    d <- data_all()
    if (!nrow(d)) return(d)

    seasons <- input$def_season_groups %||% character(0)

    if (length(seasons)) d <- d %>% dplyr::filter(season %in% seasons)
    d
  })

  scoped_opportunities <- function(position_set = NULL, player = NULL) {
    d <- filtered_data()
    d <- d %>% dplyr::filter(.data$opportunity %in% TRUE)
    if (!is.null(position_set)) d <- d %>% dplyr::filter(.data$position %in% position_set)
    if (!is.null(player) && length(player) && nzchar(player)) d <- d %>% dplyr::filter(.data$player == !!player)
    d
  }

  player_choices_for <- function(position_set = NULL) {
    counts <- scoped_opportunities(position_set) %>%
      dplyr::filter(!is.na(.data$player), nzchar(.data$player)) %>%
      dplyr::count(.data$player, sort = TRUE)
    if (!nrow(counts)) return(character(0))
    stats::setNames(counts$player, paste0(name_display(counts$player), " (", counts$n, ")"))
  }

  update_player_dropdown <- function(input_id, position_set = NULL, selected_value = NULL) {
    choices <- player_choices_for(position_set)
    if (!length(choices)) {
      updateSelectInput(session, input_id, choices = character(0), selected = NULL)
      return(invisible(NULL))
    }
    vals <- unname(choices)
    updateSelectInput(
      session,
      input_id,
      choices = choices,
      selected = if (!is.null(selected_value) && selected_value %in% vals) selected_value else vals[[1]]
    )
  }

  update_position_dropdown <- function(input_id, player_value = NULL, position_set = NULL, selected_value = NULL) {
    counts <- scoped_opportunities(position_set, player_value) %>%
      dplyr::filter(!is.na(.data$position), nzchar(.data$position)) %>%
      dplyr::count(.data$position, sort = TRUE)
    choices <- counts$position
    if (!length(choices)) {
      updateSelectInput(session, input_id, choices = character(0), selected = NULL)
      return(invisible(NULL))
    }
    labels_n <- counts$n[match(choices, counts$position)]
    labels <- ifelse(is.na(labels_n), choices, paste0(choices, " (", labels_n, ")"))
    updateSelectInput(
      session,
      input_id,
      choices = stats::setNames(choices, labels),
      selected = if (!is.null(selected_value) && selected_value %in% choices) selected_value else choices[[1]]
    )
  }

  observe({
    update_player_dropdown("opportunity_player", NULL, isolate(input$opportunity_player))
    update_player_dropdown("of_player", OUTFIELD_POSITIONS, isolate(input$of_player))
    update_player_dropdown("if_player", INFIELD_POSITIONS, isolate(input$if_player))
    update_player_dropdown("oaa_player", NULL, isolate(input$oaa_player))
  })

  observe({
    update_position_dropdown("opportunity_position", input$opportunity_player, NULL, isolate(input$opportunity_position))
    update_position_dropdown("range_position", input$of_player, OUTFIELD_POSITIONS, isolate(input$range_position))
    update_position_dropdown("if_position", input$if_player, INFIELD_POSITIONS, isolate(input$if_position))
    update_position_dropdown("oaa_position", input$oaa_player, NULL, isolate(input$oaa_position))
  })

  opportunity_data <- reactive({
    d <- scoped_opportunities(NULL, input$opportunity_player)
    pos <- input$opportunity_position %||% character(0)
    if (length(pos) && nzchar(pos)) d <- d %>% dplyr::filter(.data$position == !!pos)
    d
  })

  range_data <- reactive({
    d <- scoped_opportunities(OUTFIELD_POSITIONS, input$of_player)
    pos <- input$range_position %||% character(0)
    if (length(pos) && nzchar(pos)) d <- d %>% dplyr::filter(position == !!pos)
    d
  })

  if_base_data <- reactive({
    d <- scoped_opportunities(INFIELD_POSITIONS, input$if_player)
    pos <- input$if_position %||% character(0)
    if (length(pos) && nzchar(pos)) d <- d %>% dplyr::filter(position == !!pos)
    d
  })

  if_ground_data <- reactive({
    if_base_data() %>% dplyr::filter(.data$batted_ball_bucket == "Ground")
  })

  if_air_data <- reactive({
    if_base_data() %>%
      dplyr::filter(.data$batted_ball_bucket %in% c("Fly", "Line", "Popup"), is.finite(.data$distance), .data$distance < 250)
  })

  oaa_page_data <- reactive({
    d <- scoped_opportunities(NULL, input$oaa_player)
    pos <- input$oaa_position %||% character(0)
    if (length(pos) && nzchar(pos)) d <- d %>% dplyr::filter(position == !!pos)
    d
  })

  catcher_receiving_pool <- reactive({
    d <- catching_df
    if (!nrow(d) || !"Catcher" %in% names(d)) return(d[0, , drop = FALSE])
    d %>% dplyr::filter(!is.na(.data$Catcher), nzchar(.data$Catcher))
  })

  catchers_all <- reactive({
    d <- catcher_receiving_pool()
    if (!nrow(d)) return(character(0))
    ch_orig <- unique(nz_chr(d$Catcher))
    ch_disp <- name_display(ch_orig)
    keep <- nzchar(ch_orig)
    ch_orig <- ch_orig[keep]
    ch_disp <- ch_disp[keep]
    if (!length(ch_orig)) return(character(0))
    ord <- order(ch_disp)
    stats::setNames(ch_orig[ord], ch_disp[ord])
  })

  games_for_catcher <- reactive({
    cch <- input$def_AARCatcher
    d <- catcher_receiving_pool()
    if (is.null(cch) || !nzchar(cch) || !nrow(d)) {
      return(tibble::tibble(gid = character(0), gdate = as.Date(character(0)), season = character(0)))
    }
    dd <- d %>% dplyr::filter(.data$Catcher == cch)
    tibble::tibble(
      gid = as.character(dd$CustomGameID),
      gdate = dd$GameDate,
      season = dd$SeasonGroup,
      home = coalesce_chr_cols(dd, c("HomeTeam")),
      away = coalesce_chr_cols(dd, c("AwayTeam")),
      team = coalesce_chr_cols(dd, c("CatcherTeam", "PitcherTeam"))
    ) %>%
      dplyr::filter(!is.na(.data$gid), nzchar(.data$gid)) %>%
      dplyr::distinct(.data$gid, .data$gdate, .data$season, .data$home, .data$away, .data$team) %>%
      dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  })

  games_for_catcher_multi <- reactive({
    cch <- input$def_CR_catcher
    d <- catcher_receiving_pool()
    if (is.null(cch) || !nzchar(cch) || !nrow(d)) {
      return(tibble::tibble(gid = character(0), gdate = as.Date(character(0)), season = character(0)))
    }
    dd <- d %>% dplyr::filter(.data$Catcher == cch)
    tibble::tibble(
      gid = as.character(dd$CustomGameID),
      gdate = dd$GameDate,
      season = dd$SeasonGroup,
      home = coalesce_chr_cols(dd, c("HomeTeam")),
      away = coalesce_chr_cols(dd, c("AwayTeam")),
      team = coalesce_chr_cols(dd, c("CatcherTeam", "PitcherTeam"))
    ) %>%
      dplyr::filter(!is.na(.data$gid), nzchar(.data$gid)) %>%
      dplyr::distinct(.data$gid, .data$gdate, .data$season, .data$home, .data$away, .data$team) %>%
      dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  })

  season_selected_catcher_game_ids <- reactive({
    gf <- games_for_catcher_multi()
    sg <- input$def_season_groups %||% character(0)
    if (!nrow(gf) || !length(sg)) return(character(0))
    sel <- gf$gid[gf$season %in% sg]
    sel <- sel[!is.na(sel) & nzchar(sel)]
    unique(sel)
  })

  observeEvent(catchers_all(), {
    ch <- catchers_all()
    if (!length(ch)) {
      updateSelectInput(session, "def_AARCatcher", choices = character(0), selected = NULL)
      updateSelectInput(session, "def_CR_catcher", choices = character(0), selected = NULL)
      return()
    }
    first_choice <- unname(ch[[1]])
    updateSelectInput(session, "def_AARCatcher", choices = ch, selected = first_choice)
    updateSelectInput(session, "def_CR_catcher", choices = ch, selected = first_choice)
  }, ignoreInit = FALSE)

  observeEvent(input$def_AARCatcher, {
    games <- games_for_catcher()
    choices <- catcher_game_choices(games)
    updateSelectInput(session, "def_AARCatchGame", choices = choices, selected = if (length(choices)) unname(choices[[1]]) else NULL)
  }, ignoreInit = FALSE)

  observeEvent(list(input$def_CR_catcher, input$def_season_groups), {
    games <- games_for_catcher_multi()
    choices <- catcher_game_choices(games)
    updatePickerInput(session, "def_CR_games", choices = choices, selected = season_selected_catcher_game_ids())
  }, ignoreInit = FALSE)

  aar_catch_data <- reactive({
    req(input$def_AARCatcher, input$def_AARCatchGame)
    d <- catcher_receiving_pool() %>%
      dplyr::filter(.data$Catcher == input$def_AARCatcher, .data$CustomGameID == input$def_AARCatchGame)
    validate(need(nrow(d) > 0, "No rows in selected game for this catcher."))
    prepare_catcher_receiving_rows(d)
  })

  cr_catch_data_base <- reactive({
    req(input$def_CR_catcher)
    d <- catcher_receiving_pool() %>% dplyr::filter(.data$Catcher == input$def_CR_catcher)
    sel_games <- input$def_CR_games
    if (is.null(sel_games) || !length(sel_games)) return(d[0, , drop = FALSE])
    d <- d %>% dplyr::filter(.data$CustomGameID %in% sel_games)
    prepare_catcher_receiving_rows(d)
  })

  cr_catch_data <- reactive({
    d <- cr_catch_data_base()
    if (!is.null(input$def_CR_pitcher_hand) && input$def_CR_pitcher_hand != "All" && "PitcherHand" %in% names(d)) {
      d <- d %>% dplyr::filter(.data$PitcherHand == input$def_CR_pitcher_hand)
    }
    pt_sel <- input$def_CR_pitch_types
    if (is.null(pt_sel) || !length(pt_sel)) return(d[0, , drop = FALSE])
    d %>% dplyr::filter(as.character(.data$PitchType) %in% pt_sel)
  })

  catch_ball_to_strike <- reactive(catcher_framing_subset(aar_catch_data(), "ball_to_strike"))
  catch_strike_to_ball <- reactive(catcher_framing_subset(aar_catch_data(), "strike_to_ball"))
  cr_ball_to_strike <- reactive(catcher_framing_subset(cr_catch_data(), "ball_to_strike"))
  cr_strike_to_ball <- reactive(catcher_framing_subset(cr_catch_data(), "strike_to_ball"))

  output$def_catcher_ball_to_strike_plot <- renderPlot({
    validate(need(nrow(catcher_receiving_pool()) > 0, "Add Catchers - (Seasonname).csv to DefenseApp/data to populate catcher reports."))
    catcher_framing_zone_plot(catch_ball_to_strike(), title = "Ball to Strike")
  })
  output$def_catcher_strike_to_ball_plot <- renderPlot({
    validate(need(nrow(catcher_receiving_pool()) > 0, "Add Catchers - (Seasonname).csv to DefenseApp/data to populate catcher reports."))
    catcher_framing_zone_plot(catch_strike_to_ball(), title = "Strike to Ball")
  })
  output$def_catcher_pitch_legend <- renderPlot(catcher_framing_legend_plot())

  output$def_catcher_ball_to_strike_stats <- renderTable(catcher_stats_table(aar_catch_data(), "ball_to_strike"), striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  output$def_catcher_strike_to_ball_stats <- renderTable(catcher_stats_table(aar_catch_data(), "strike_to_ball"), striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  output$def_catcher_ball_to_strike_tbl <- DT::renderDT(render_catcher_dt(catcher_framing_table(catch_ball_to_strike())))
  output$def_catcher_strike_to_ball_tbl <- DT::renderDT(render_catcher_dt(catcher_framing_table(catch_strike_to_ball())))

  output$def_cr_ball_to_strike_plot <- renderPlot({
    validate(need(nrow(catcher_receiving_pool()) > 0, "Add Catchers - (Seasonname).csv to DefenseApp/data to populate catcher season."))
    catcher_framing_zone_plot(cr_ball_to_strike(), title = "Strikes Stolen", show_pitch_numbers = FALSE)
  })
  output$def_cr_strike_to_ball_plot <- renderPlot({
    validate(need(nrow(catcher_receiving_pool()) > 0, "Add Catchers - (Seasonname).csv to DefenseApp/data to populate catcher season."))
    catcher_framing_zone_plot(cr_strike_to_ball(), title = "Strikes Lost", show_pitch_numbers = FALSE)
  })

  output$def_cr_ball_to_strike_stats <- renderTable(catcher_stats_table(cr_catch_data(), "ball_to_strike"), striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  output$def_cr_strike_to_ball_stats <- renderTable(catcher_stats_table(cr_catch_data(), "strike_to_ball"), striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  output$cr_percentile_rankings <- renderUI({
    metrics <- cr_catch_data_base() %>%
      summarise_catcher_percentile_metrics() %>%
      add_d1_percentiles()
    percentile_rankings_ui(metrics, input$def_CR_catcher)
  })

  output$def_catcher_framing_pdf <- downloadHandler(
    filename = function() {
      cch <- input$def_AARCatcher %||% "Catcher"
      last <- if (grepl(",", cch)) sub(",.*$", "", cch) else sub("^.*\\s+", "", trimws(cch))
      d <- tryCatch(aar_catch_data(), error = function(e) NULL)
      gdt <- if (!is.null(d) && nrow(d) && "GameDate" %in% names(d) && any(!is.na(d$GameDate))) {
        format(d$GameDate[!is.na(d$GameDate)][1], "%Y-%m-%d")
      } else {
        format(Sys.Date(), "%Y-%m-%d")
      }
      sprintf("%s_Receiving_%s.pdf", gsub("[^A-Za-z0-9_-]+", "", last), gdt)
    },
    content = function(file) {
      d <- aar_catch_data()
      validate(need(nrow(d) > 0, "No data for catcher receiving."))
      write_catcher_receiving_pdf(d, name_display(input$def_AARCatcher %||% "Catcher"), file)
    }
  )

  output$stats_kpis <- renderUI({
    d <- filtered_data()
    opps <- sum(d$opportunity %in% TRUE, na.rm = TRUE)
    putouts <- sum(d$putouts, na.rm = TRUE)
    assists <- sum(d$assists, na.rm = TRUE)
    errors <- sum(d$errors, na.rm = TRUE)
    fielding_pct <- safe_ratio(putouts + assists, putouts + assists + errors)
    oaa_total <- if (any(is.finite(d$oaa))) sum(d$oaa, na.rm = TRUE) else NA_real_
    drs_total <- if (any(is.finite(d$drs))) sum(d$drs, na.rm = TRUE) else NA_real_

    div(
      class = "metric-strip",
      div(class = "metric-tile", div(class = "metric-label", "Opportunities"), div(class = "metric-value", opps)),
      div(class = "metric-tile", div(class = "metric-label", "Fielding %"), div(class = "metric-value", fmt_pct(fielding_pct, 3))),
      div(class = "metric-tile", div(class = "metric-label", "Total OAA"), div(class = "metric-value", fmt_num(oaa_total, 2))),
      div(class = "metric-tile", div(class = "metric-label", "Total DRS"), div(class = "metric-value", fmt_num(drs_total, 2)))
    )
  })

  output$stats_table <- DT::renderDT({
    tbl <- summarize_defense(filtered_data())
    DT::datatable(
      tbl,
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(pageLength = 25, autoWidth = TRUE)
    )
  })

  output$leaderboard_overall_table <- DT::renderDT({
    DT::datatable(
      summarize_leaderboard(filtered_data()),
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(pageLength = 25, autoWidth = TRUE)
    )
  })

  output$leaderboard_if_table <- DT::renderDT({
    DT::datatable(
      summarize_leaderboard(filtered_data() %>% dplyr::filter(.data$position %in% INFIELD_POSITIONS)),
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(pageLength = 25, autoWidth = TRUE)
    )
  })

  output$leaderboard_of_table <- DT::renderDT({
    DT::datatable(
      summarize_leaderboard(filtered_data() %>% dplyr::filter(.data$position %in% OUTFIELD_POSITIONS)),
      rownames = FALSE,
      class = "stripe hover compact",
      options = list(pageLength = 25, autoWidth = TRUE)
    )
  })

  output$opportunity_spray <- renderPlot({
    player_label <- input$opportunity_player %||% "Player"
    pos_label <- input$opportunity_position %||% "Position"
    opportunity_spray_plot(opportunity_data(), pos_label, sprintf("%s: %s Opportunities", name_display(player_label), pos_label))
  })

  output$opportunity_table <- DT::renderDT({
    d <- opportunity_data()
    tbl <- if (!nrow(d)) {
      tibble::tibble(
        Date = as.Date(character()), Game = character(), Position = character(),
        Result = character(), Type = character(), Zone = character(),
        `Play Prob` = character(), OAA = character()
      )
    } else {
      d %>%
        dplyr::arrange(dplyr::desc(.data$date), .data$game) %>%
        dplyr::transmute(
          Date = .data$date,
          Game = .data$game,
          Position = .data$position,
          Result = .data$play_bucket,
          Type = .data$batted_ball_bucket,
          Zone = .data$opportunity_zone,
          `Play Prob` = fmt_pct(.data$play_probability, 1),
          OAA = fmt_num(.data$oaa, 2)
        )
    }
    DT::datatable(tbl, rownames = FALSE, class = "stripe hover compact", options = list(pageLength = 15, autoWidth = TRUE))
  })

  output$range_360 <- renderPlot({
    player_label <- input$of_player %||% "Player"
    pos_label <- input$range_position %||% "Position"
    range_360_plot(range_data(), sprintf("%s: %s OF OAA", name_display(player_label), pos_label))
  })

  output$range_sector_table <- DT::renderDT({
    raw <- range_data()
    plotted <- range_sector_data(raw)
    if (!nrow(raw)) {
      tbl <- tibble::tibble(Sector = c("In Left", "In", "In Right", "Back Left", "Back", "Back Right"), Opportunities = 0L, `Fielding %` = "", OAA = "", DRS = "")
    } else {
      sector_tbl <- if (nrow(plotted)) {
        plotted %>%
        dplyr::group_by(.data$range_sector) %>%
        dplyr::summarise(
          Opportunities = dplyr::n(),
            Putouts = sum(.data$putouts, na.rm = TRUE),
            Assists = sum(.data$assists, na.rm = TRUE),
            Errors = sum(.data$errors, na.rm = TRUE),
            FieldingPct = safe_ratio(Putouts + Assists, Putouts + Assists + Errors),
          OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), NA_real_),
          DRS_num = ifelse(any(is.finite(.data$drs)), sum(.data$drs, na.rm = TRUE), NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::transmute(
          Sector = as.character(.data$range_sector),
          Opportunities,
          `Fielding %` = fmt_pct(.data$FieldingPct, 3),
          OAA = fmt_num(.data$OAA_num, 2),
          DRS = fmt_num(.data$DRS_num, 2)
        )
      } else {
        tibble::tibble(Sector = character(), Opportunities = integer(), `Fielding %` = character(), OAA = character(), DRS = character())
      }

      unmapped <- raw %>%
        dplyr::filter(!(is.finite(.data$start_x) & is.finite(.data$start_y) & is.finite(.data$ball_x) & is.finite(.data$ball_y))) %>%
        dplyr::summarise(
          Opportunities = dplyr::n(),
          Putouts = sum(.data$putouts, na.rm = TRUE),
          Assists = sum(.data$assists, na.rm = TRUE),
          Errors = sum(.data$errors, na.rm = TRUE),
          FieldingPct = safe_ratio(Putouts + Assists, Putouts + Assists + Errors),
          OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), NA_real_),
          DRS_num = ifelse(any(is.finite(.data$drs)), sum(.data$drs, na.rm = TRUE), NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::filter(.data$Opportunities > 0) %>%
        dplyr::transmute(
          Sector = "Unmapped",
          Opportunities,
          `Fielding %` = fmt_pct(.data$FieldingPct, 3),
          OAA = fmt_num(.data$OAA_num, 2),
          DRS = fmt_num(.data$DRS_num, 2)
        )

      tbl <- dplyr::bind_rows(sector_tbl, unmapped)
    }
    DT::datatable(tbl, rownames = FALSE, class = "stripe hover compact", options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$if_ground_oaa_plot <- renderPlot({
    player_label <- input$if_player %||% "Player"
    pos_label <- input$if_position %||% "Position"
    if_ground_oaa_plot(if_ground_data(), sprintf("%s: %s Ground Ball IF OAA", name_display(player_label), pos_label))
  })

  output$if_ground_oaa_table <- DT::renderDT({
    plotted <- if_ground_sector_data(if_ground_data())
    tbl <- if (!nrow(plotted)) {
      tibble::tibble(Sector = c("In Far Left", "In Left", "In Right", "In Far Right"), Opportunities = 0L, OAA = "")
    } else {
      plotted %>%
        dplyr::group_by(.data$if_ground_sector) %>%
        dplyr::summarise(
          Opportunities = dplyr::n(),
          OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::transmute(
          Sector = as.character(.data$if_ground_sector),
          Opportunities,
          OAA = fmt_num(.data$OAA_num, 2)
        )
    }
    DT::datatable(tbl, rownames = FALSE, class = "stripe hover compact", options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$if_air_oaa_plot <- renderPlot({
    player_label <- input$if_player %||% "Player"
    pos_label <- input$if_position %||% "Position"
    range_360_plot(if_air_data(), sprintf("%s: %s Air Ball IF OAA", name_display(player_label), pos_label))
  })

  output$if_air_oaa_table <- DT::renderDT({
    plotted <- range_sector_data(if_air_data())
    tbl <- if (!nrow(plotted)) {
      tibble::tibble(Sector = c("In Left", "In", "In Right", "Back Left", "Back", "Back Right"), Opportunities = 0L, OAA = "")
    } else {
      plotted %>%
        dplyr::group_by(.data$range_sector) %>%
        dplyr::summarise(
          Opportunities = dplyr::n(),
          OAA_num = ifelse(any(is.finite(.data$oaa)), sum(.data$oaa, na.rm = TRUE), NA_real_),
          .groups = "drop"
        ) %>%
        dplyr::transmute(
          Sector = as.character(.data$range_sector),
          Opportunities,
          OAA = fmt_num(.data$OAA_num, 2)
        )
    }
    DT::datatable(tbl, rownames = FALSE, class = "stripe hover compact", options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  output$oaa_plot <- renderPlot({
    d <- oaa_page_data() %>%
      dplyr::filter(is.finite(oaa)) %>%
      dplyr::group_by(player, position) %>%
      dplyr::summarise(oaa = sum(oaa, na.rm = TRUE), opportunities = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(oaa)) %>%
      dplyr::slice_head(n = 20)
    if (!nrow(d)) return(empty_plot("Outs Above Average"))

    ggplot(d, aes(x = reorder(player, oaa), y = oaa, fill = oaa >= 0)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c(`TRUE` = "#B4975A", `FALSE` = "#501214")) +
      labs(title = "Outs Above Average", x = NULL, y = "OAA") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#501214"))
  })

  output$oaa_table <- DT::renderDT({
    tbl <- summarize_defense(oaa_page_data()) %>%
      dplyr::select(Player, Position, Opportunities, `Fielding %`, OAA, DRS) %>%
      dplyr::arrange(dplyr::desc(OAA))
    DT::datatable(tbl, rownames = FALSE, class = "stripe hover compact", options = list(pageLength = 25))
  })
}
