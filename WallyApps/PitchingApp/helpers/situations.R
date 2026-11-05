# Situations are annotated before pitcher, handedness, or leaderboard filters.
PITCHING_SITUATIONS <- c("Clean inning", "Mid inning", "Early innings", "Mid innings", "Late innings",
  "Up 4+", "Down 4+", "Tight Lead", "Trailing Close", "Tied", "Save Situations",
  "Home", "Away", "Day", "Night")
PITCHING_SITUATION_CONTEXT_COLUMNS <- c("GameUID", "GameID", "CustomGameID", "GameId", "PitchUID",
  "Pitcher", "PitcherTeam", "BatterTeam", "Inning", "Top/Bottom", "InningTopBot", "TopBottom",
  "PAofInning", "PitchofPA", "PitchNo", "PitchCall", "KorBB", "PlayResult", "Strikes", "OutsOnPlay", "Outs", "RunsScored",
  "HomeScoreBefore", "AwayScoreBefore", "HomeTeam", "AwayTeam", "Stadium", "Venue", "Ballpark",
  "Date", "Time", "UTCDateTime", "LocalDateTime", "UTCDate", "UTCTime", "is_bullpen")

pitching_sit_chr <- function(d, column) {
  x <- if (column %in% names(d)) as.character(d[[column]]) else rep(NA_character_, nrow(d))
  x <- trimws(x)
  x[tolower(x) %in% c("", "na", "nan", "null", "undefined")] <- NA_character_
  x
}
pitching_sit_num <- function(d, column) suppressWarnings(as.numeric(pitching_sit_chr(d, column)))
pitching_sit_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
pitching_sit_game <- function(d) {
  key <- rep(NA_character_, nrow(d))
  for (col in c("GameID", "GameUID", "CustomGameID", "GameId")) {
    value <- pitching_sit_chr(d, col)
    key[is.na(key)] <- value[is.na(key)]
  }
  key
}
pitching_sit_half <- function(d) {
  value <- rep(NA_character_, nrow(d))
  for (col in c("Top/Bottom", "InningTopBot", "TopBottom")) {
    x <- tolower(pitching_sit_chr(d, col))
    x <- ifelse(x %in% c("top", "t", "1"), "Top",
      ifelse(x %in% c("bottom", "bot", "b", "2"), "Bottom", NA_character_))
    value[is.na(value)] <- x[is.na(value)]
  }
  value
}
pitching_sit_pitch_key <- function(d) {
  parts <- list(pitching_sit_game(d), pitching_sit_half(d), pitching_sit_num(d, "Inning"),
    pitching_sit_num(d, "PAofInning"), pitching_sit_num(d, "PitchofPA"))
  key <- do.call(paste, c(parts, sep = "\037"))
  valid <- Reduce(`&`, lapply(parts, function(x) !is.na(x)))
  uid <- pitching_sit_chr(d, "PitchUID")
  key[!valid] <- ifelse(!is.na(uid[!valid]), paste0("uid:", uid[!valid]), NA_character_)
  key
}

# USNO solar coordinates + NOAA's apparent-sunset horizon (90.833 degrees).
# https://aa.usno.navy.mil/faq/sun_approx
# https://gml.noaa.gov/grad/solcalc/solareqns.PDF
pitching_sunset_utc <- function(date, latitude, longitude) {
  date <- as.Date(date)
  rad <- pi / 180
  minutes <- 1080 - 4 * longitude
  for (iteration in 1:4) {
    days <- as.numeric(date) + 2440587.5 + minutes / 1440 - 2451545
    g <- ((357.529 + 0.98560028 * days) %% 360) * rad
    q <- (280.459 + 0.98564736 * days) %% 360
    longitude_sun <- (q + 1.915 * sin(g) + 0.020 * sin(2 * g)) * rad
    obliquity <- (23.439 - 0.00000036 * days) * rad
    ra <- (atan2(cos(obliquity) * sin(longitude_sun), cos(longitude_sun)) / rad) %% 360
    decl <- asin(sin(obliquity) * sin(longitude_sun))
    equation_time <- ((q - ra + 180) %% 360 - 180) * 4
    angle <- cos(90.833 * rad) / (cos(latitude * rad) * cos(decl)) - tan(latitude * rad) * tan(decl)
    hour_angle <- suppressWarnings(acos(angle)) / rad
    minutes <- 720 - 4 * longitude + 4 * hour_angle - equation_time
  }
  as.POSIXct(date, tz = "UTC") + minutes * 60
}

pitching_sit_timestamp <- function(d, timezone) {
  result <- rep(NA_real_, nrow(d))
  parse <- function(x, tz) {
    if (inherits(x, "POSIXt")) return(as.numeric(x))
    as.numeric(suppressWarnings(lubridate::parse_date_time(as.character(x),
      orders = c("ymd HMSz", "ymd HMS", "ymd HM", "mdy HMS", "mdy IMS p"), tz = tz, quiet = TRUE)))
  }
  for (col in c("UTCDateTime", "LocalDateTime")) {
    if (!col %in% names(d)) next
    x <- d[[col]]
    utc <- col == "UTCDateTime" | grepl("Z$|[+-][0-9]{2}:?[0-9]{2}$", as.character(x))
    use <- is.na(result) & utc
    if (any(use)) result[use] <- parse(x[use], "UTC")
    for (tz in unique(na.omit(timezone))) {
      use <- is.na(result) & !utc & !is.na(timezone) & timezone == tz
      if (any(use)) result[use] <- parse(x[use], tz)
    }
  }
  if (all(c("UTCDate", "UTCTime") %in% names(d))) {
    use <- is.na(result)
    if (any(use)) result[use] <- parse(paste(as.character(d$UTCDate[use]), as.character(d$UTCTime[use])), "UTC")
  }
  if (all(c("Date", "Time") %in% names(d))) {
    for (tz in unique(na.omit(timezone))) {
      use <- is.na(result) & !is.na(timezone) & timezone == tz
      if (any(use)) result[use] <- parse(paste(as.character(d$Date[use]), as.character(d$Time[use])), tz)
    }
  }
  as.POSIXct(result, origin = "1970-01-01", tz = "UTC")
}

pitching_sit_locations <- function(d, venues, schedule = NULL) {
  n <- nrow(d)
  index <- rep(NA_integer_, n)
  aliases <- strsplit(as.character(venues$aliases), "|", fixed = TRUE)
  lookup <- stats::setNames(rep(seq_len(nrow(venues)), lengths(aliases)),
    pitching_sit_key(unlist(aliases)))
  for (col in c("Stadium", "Venue", "Ballpark")) {
    value <- unname(lookup[pitching_sit_key(pitching_sit_chr(d, col))])
    use <- is.na(index) & !is.na(value)
    index[use] <- value[use]
  }
  # Match schedule dates and opponent codes; do not use the home-team designation
  # to relocate a neutral game to a team's campus.
  if (!is.null(schedule) && nrow(schedule)) {
    parse_dates <- function(x) as.character(as.Date(suppressWarnings(lubridate::parse_date_time(x,
      orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy IM p", "mdy"), tz = "UTC", quiet = TRUE))))
    dates <- parse_dates(as.character(schedule$DateTime))
    date <- parse_dates(pitching_sit_chr(d, "Date"))
    opp <- ifelse(pitching_sit_half(d) == "Top", pitching_sit_chr(d, "AwayTeam"), pitching_sit_chr(d, "HomeTeam"))
    for (i in seq_len(nrow(schedule))) {
      use <- which(is.na(index) & date == dates[i] & !is.na(opp) & opp == schedule$OppAbbr[i])
      venue <- unname(lookup[pitching_sit_key(schedule$Venue[i])])
      if (length(use) && length(venue) && !is.na(venue)) index[use] <- venue
    }
  }
  # Known TXST home games and intrasquads use San Marcos when venue metadata is absent.
  home <- pitching_sit_chr(d, "HomeTeam")
  has_venue <- Reduce(`|`, lapply(c("Stadium", "Venue", "Ballpark"), function(col) !is.na(pitching_sit_chr(d, col))))
  use <- is.na(index) & !has_venue &
    ((!is.na(home) & home == "TEX_BOB") | (is.na(home) & pitching_sit_half(d) == "Top"))
  use[is.na(use)] <- FALSE
  index[use] <- unname(lookup["texasstateuniversity"])
  venues[index, c("city", "latitude", "longitude", "timezone"), drop = FALSE]
}

pitching_annotate_situations <- function(rows, context = NULL, venues, schedule = NULL, score_snapshots = NULL) {
  rows <- tibble::as_tibble(rows)
  n <- nrow(rows)
  if (!n) return(rows)
  # Character conversion preserves mixed export types before compact context is joined.
  frames <- lapply(Filter(function(x) !is.null(x) && nrow(x), list(rows, context)), function(x) {
    x <- as.data.frame(x[, intersect(PITCHING_SITUATION_CONTEXT_COLUMNS, names(x)), drop = FALSE])
    for (col in names(x)) if (inherits(x[[col]], "POSIXt")) x[[col]] <- format(x[[col]], "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    x[] <- lapply(x, as.character)
    x
  })
  all <- dplyr::bind_rows(frames)
  all$.sit_key <- pitching_sit_pitch_key(all)
  all <- all[!is.na(all$.sit_key), , drop = FALSE]
  unique_rows <- all[!duplicated(all$.sit_key), , drop = FALSE]
  at <- match(all$.sit_key, unique_rows$.sit_key)
  for (col in setdiff(names(all), ".sit_key")) {
    missing <- is.na(pitching_sit_chr(unique_rows, col))
    eligible <- which(missing[at] & !is.na(pitching_sit_chr(all, col)))
    eligible <- eligible[!duplicated(at[eligible])]
    unique_rows[[col]][at[eligible]] <- all[[col]][eligible]
  }
  all <- unique_rows
  all$.sit_game <- pitching_sit_game(all)
  all$.sit_half <- pitching_sit_half(all)
  all$.sit_inning <- pitching_sit_num(all, "Inning")
  all$.sit_pa <- pitching_sit_num(all, "PAofInning")
  all$.sit_pn <- pitching_sit_num(all, "PitchofPA")
  all$.sit_half_order <- 2 * all$.sit_inning - ifelse(all$.sit_half == "Top", 1, 0)
  all$.sit_pitcher <- pitching_sit_chr(all, "Pitcher")
  all$.sit_pa_key <- paste(all$.sit_game, all$.sit_half_order, all$.sit_pa, sep = "\037")
  all$.sit_pa_key[is.na(all$.sit_game) | !is.finite(all$.sit_half_order) | !is.finite(all$.sit_pa)] <- NA_character_
  all <- all[order(all$.sit_game, all$.sit_half_order, all$.sit_pa, all$.sit_pn, na.last = TRUE), ]
  all$.sit_entry <- NA_character_
  all$.sit_lead <- NA_real_
  all$.sit_home_score <- all$.sit_away_score <- NA_real_
  if(!is.null(score_snapshots) && nrow(score_snapshots)) {
    snapshot_key<-paste(score_snapshots$GameID,score_snapshots$TopBottom,score_snapshots$Inning,score_snapshots$PAofInning,sep="\037")
    at<-match(paste(all$.sit_game,all$.sit_half,all$.sit_inning,all$.sit_pa,sep="\037"),snapshot_key)
    use<-which(!is.na(at) & !is.finite(pitching_sit_num(all,"HomeScoreBefore")) & !is.finite(pitching_sit_num(all,"AwayScoreBefore")))
    if(!"HomeScoreBefore" %in% names(all)) all$HomeScoreBefore<-NA_real_
    if(!"AwayScoreBefore" %in% names(all)) all$AwayScoreBefore<-NA_real_
    all$HomeScoreBefore[use]<-score_snapshots$HomeScoreBefore[at[use]]
    all$AwayScoreBefore[use]<-score_snapshots$AwayScoreBefore[at[use]]
  }
  all$.sit_terminal <- FALSE
  all$.sit_terminal <- pitching_sit_chr(all, "PitchCall") %in% c("InPlay", "InPlayOut", "InPlayNoOut", "Walk", "HitByPitch") |
    grepl("^(strikeout|k|walk|bb|ibb|hbp)$", tolower(pitching_sit_chr(all, "KorBB"))) |
    pitching_sit_num(all, "Strikes") >= 3
  all$.sit_terminal[is.na(all$.sit_terminal)] <- FALSE
  outs <- pitching_sit_num(all, "OutsOnPlay")
  result <- tolower(pitching_sit_chr(all, "PlayResult"))
  strikeout <- grepl("^(strikeout|k)$", tolower(pitching_sit_chr(all, "KorBB"))) |
    pitching_sit_num(all, "Strikes") >= 3
  inferred_outs <- ifelse(grepl("triple.?play", result), 3,
    ifelse(grepl("double.?play", result), 2,
      ifelse(result %in% c("out", "sacrifice", "sacfly", "sacbunt") | strikeout %in% TRUE, 1, 0)))
  inferred_outs[!all$.sit_terminal | is.na(inferred_outs)] <- 0
  all$.sit_outs <- pmax(ifelse(is.finite(outs), outs, 0), inferred_outs)
  for (game in unique(na.omit(all$.sit_game))) {
    ids <- which(all$.sit_game == game & is.finite(all$.sit_half_order) & is.finite(all$.sit_pa) & is.finite(all$.sit_pn))
    home_runs <- away_runs <- 0
    valid_score <- TRUE
    prior_half <- 0
    prior_pa <- 0
    half_outs <- 3
    prior_pitcher <- NA_character_
    entry <- NA_character_
    for (pa in unique(all$.sit_pa_key[ids])) {
      j <- ids[all$.sit_pa_key[ids] == pa]
      half <- all$.sit_half_order[j[1]]
      pa_no <- all$.sit_pa[j[1]]
      pn <- all$.sit_pn[j]
      new_half <- half != prior_half
      continuous <- if (new_half) half == prior_half + 1 && pa_no == 1 else pa_no == prior_pa + 1
      if (new_half) { valid_score <- valid_score && half_outs >= 3; half_outs <- 0 }
      valid_score <- valid_score && continuous && min(pn) == 1
      # Explicit pre-PA score snapshots can anchor an otherwise partial recording.
      hs <- pitching_sit_num(all[j[1], ], "HomeScoreBefore")
      as <- pitching_sit_num(all[j[1], ], "AwayScoreBefore")
      if (is.finite(hs) && is.finite(as)) { home_runs <- hs; away_runs <- as; valid_score <- TRUE }
      if (valid_score) {
        all$.sit_lead[j] <- if (half %% 2 == 1) home_runs - away_runs else away_runs - home_runs
        all$.sit_home_score[j]<-home_runs;all$.sit_away_score[j]<-away_runs
      }
      for (k in j) {
        pitcher <- all$.sit_pitcher[k]
        change <- new_half || is.na(prior_pitcher) || is.na(pitcher) || pitcher != prior_pitcher
        if (change) {
          entry <- if (pa_no == 1 && all$.sit_pn[k] == 1 && new_half) "Clean inning" else "Mid inning"
          if (new_half && pa_no == 1 && all$.sit_pn[k] != 1) entry <- NA_character_
        }
        all$.sit_entry[k] <- entry
        prior_pitcher <- pitcher
        new_half <- FALSE
      }
      runs <- pitching_sit_num(all[j, ], "RunsScored")
      complete <- identical(sort(unique(as.integer(pn))), seq_len(max(as.integer(pn)))) &&
        all(is.finite(runs)) && (any(all$.sit_terminal[j]) || half_outs + sum(all$.sit_outs[j]) >= 3)
      valid_score <- valid_score && complete
      half_outs <- half_outs + sum(all$.sit_outs[j])
      if (half %% 2 == 1) away_runs <- away_runs + sum(runs, na.rm = TRUE) else home_runs <- home_runs + sum(runs, na.rm = TRUE)
      prior_half <- half
      prior_pa <- pa_no
    }
  }
  match_rows <- match(pitching_sit_pitch_key(rows), all$.sit_key)
  rows$.sit_pa_key <- all$.sit_pa_key[match_rows]
  rows$.sit_entry <- all$.sit_entry[match_rows]
  rows$.sit_lead <- all$.sit_lead[match_rows]
  rows$.sit_home_score<-all$.sit_home_score[match_rows]
  rows$.sit_away_score<-all$.sit_away_score[match_rows]
  rows$.sit_terminal <- all$.sit_terminal[match_rows] %in% TRUE
  rows$.sit_phase <- ifelse(pitching_sit_num(rows, "Inning") %in% 1:3, "Early innings",
    ifelse(pitching_sit_num(rows, "Inning") %in% 4:6, "Mid innings",
      ifelse(pitching_sit_num(rows, "Inning") >= 7, "Late innings", NA_character_)))
  rows$.sit_home_away <- ifelse(pitching_sit_half(rows) == "Top", "Home", ifelse(pitching_sit_half(rows) == "Bottom", "Away", NA_character_))
  metadata <- as.data.frame(rows[, intersect(PITCHING_SITUATION_CONTEXT_COLUMNS, names(rows)), drop = FALSE])
  for (col in names(metadata)) if (inherits(metadata[[col]], "POSIXt"))
    metadata[[col]] <- format(metadata[[col]], "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  metadata[] <- lapply(metadata, as.character)
  for (col in intersect(PITCHING_SITUATION_CONTEXT_COLUMNS, names(all))) {
    if (!col %in% names(metadata)) metadata[[col]] <- all[[col]][match_rows]
    else {
      missing <- is.na(pitching_sit_chr(metadata, col)) & !is.na(match_rows)
      if (any(missing)) metadata[[col]][missing] <- all[[col]][match_rows[missing]]
    }
  }
  loc <- pitching_sit_locations(metadata, venues, schedule)
  timestamp <- pitching_sit_timestamp(metadata, loc$timezone)
  rows$.sit_day_night <- NA_character_
  rows$.sit_sunset <- NA_real_
  for (tz in unique(na.omit(loc$timezone))) {
    j <- which(!is.na(loc$timezone) & loc$timezone == tz & !is.na(timestamp))
    date <- as.Date(format(timestamp[j], "%Y-%m-%d", tz = tz))
    sunset <- as.numeric(pitching_sunset_utc(date, loc$latitude[j], loc$longitude[j]))
    rows$.sit_sunset[j] <- sunset
    rows$.sit_day_night[j] <- ifelse(as.numeric(timestamp[j]) <= sunset - 1800, "Day", "Night")
  }
  bullpen <- tolower(pitching_sit_chr(rows, "is_bullpen")) %in% c("true", "1")
  rows[bullpen, c(".sit_entry", ".sit_phase", ".sit_home_away")] <- NA_character_
  rows$.sit_lead[bullpen] <- NA_real_
  rows
}

pitching_situation_mask <- function(d, situation) {
  if (situation %in% c("Total", "TOTAL")) return(rep(TRUE, nrow(d)))
  lead <- d$.sit_lead
  keep <- switch(situation,
    "Clean inning" = d$.sit_entry == situation, "Mid inning" = d$.sit_entry == situation,
    "Early innings" = d$.sit_phase == situation, "Mid innings" = d$.sit_phase == situation, "Late innings" = d$.sit_phase == situation,
    "Up 4+" = lead >= 4, "Down 4+" = lead <= -4, "Tight Lead" = lead >= 1 & lead <= 3,
    "Trailing Close" = lead <= -1 & lead >= -3, "Tied" = lead == 0,
    "Save Situations" = lead >= 1 & lead <= 3 & pitching_sit_num(d, "Inning") >= 7,
    "Home" = d$.sit_home_away == situation, "Away" = d$.sit_home_away == situation,
    "Day" = d$.sit_day_night == situation, "Night" = d$.sit_day_night == situation,
    stop("Unknown pitching situation: ", situation))
  keep %in% TRUE
}

pitching_situation_pa_last <- function(d, pa_col = "PA_ID") {
  if (".sit_stats" %in% names(d)) d <- d[d$.sit_terminal %in% TRUE, , drop = FALSE]
  d %>% dplyr::group_by(dplyr::across(dplyr::all_of(pa_col))) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
}
pitching_situation_fps <- function(d, pa_col) {
  if (".sit_stats" %in% names(d)) {
    p <- pitching_situation_pa_last(d, pa_col)
    p$fp_strike <- p$.sit_fp_strike
    return(p[, c(pa_col, "fp_strike"), drop = FALSE])
  }
  d %>% dplyr::group_by(dplyr::across(dplyr::all_of(pa_col))) %>%
    dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE), .groups = "drop")
}

pitching_situation_pa_metadata <- function(d) {
  if (!nrow(d)) return(d)
  valid <- !is.na(d$.sit_pa_key) & !is.na(pitching_sit_chr(d, "Pitcher"))
  fields <- c("fp_strike", "n_pitches", "strikes_first3", "early_bip", "any_hbp", "any_barrel", "strikes_pre", "balls_pre", "is_k")
  for (col in fields) d[[paste0(".sit_", col)]] <- NA
  if (!any(valid)) return(d)
  p <- prepare_aar_flags(d[valid, , drop = FALSE])
  p$.sit_player_pa <- paste(p$.sit_pa_key, p$Pitcher, sep = "\037")
  ea <- calc_ea_pa_summary(p, ".sit_player_pa")
  fps <- p %>% dplyr::group_by(.sit_player_pa) %>%
    dplyr::summarise(.sit_fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE), .groups = "drop")
  player_pa <- paste(d$.sit_pa_key, d$Pitcher, sep = "\037")
  d$.sit_fp_strike <- fps$.sit_fp_strike[match(player_pa, fps$.sit_player_pa)]
  for (col in c("n_pitches", "strikes_first3", "early_bip", "any_hbp", "any_barrel"))
    d[[paste0(".sit_", col)]] <- ea[[col]][match(player_pa, ea$.sit_player_pa)]
  p$.sit_is_k <- calc_is_k_pitch(p, ".sit_player_pa", "PitchNum") & p$.sit_terminal
  d$.sit_strikes_pre <- p$StrikesPre[match(d$row_id, p$row_id)]
  d$.sit_balls_pre <- p$BallsPre[match(d$row_id, p$row_id)]
  d$.sit_is_k <- p$.sit_is_k[match(d$row_id, p$row_id)] %in% TRUE
  d
}

pitching_situational_table <- function(d, summarize) {
  labels <- c("Total", PITCHING_SITUATIONS)
  rows <- lapply(labels, function(label) {
    selected <- d[pitching_situation_mask(d, label), , drop = FALSE]
    # Preserve the existing aggregate row; subsets additionally need terminal
    # pitch attribution when a PA crosses a situation boundary.
    selected$.sit_stats <- if (identical(label, "Total")) NULL else TRUE
    stats <- if (nrow(selected)) summarize(selected) else NULL
    values <- if (!is.null(stats)) stats::setNames(stats$value, stats$label) else numeric()
    counts <- attr(stats, "counts")
    if (is.null(counts)) counts <- c(PA = 0, Outs = 0, Pitches = nrow(selected))
    out <- list(Situation = label, IP = sprintf("%d.%d", as.integer(counts["Outs"]) %/% 3,
      as.integer(counts["Outs"]) %% 3), PA = as.integer(counts["PA"]), `Pitches (#)` = as.integer(counts["Pitches"]))
    columns <- c("BAA", "SLG", "OPS", "WHIP", "K/9", "BB/9", "H/9", "wOBA", "wOBAcon", "pRV", "FIP",
      "K%", "BB%", "BB+HBP%", "Barrel%", "GB%", "CSW%", "Whiff%", "IZWhiff%", "Chase%", "FPS%",
      "1-1 Win%", "E&A%", "Strike%", "Zone%", "Pre2k Zone%", "2k Zone%", "Put Away%", "Shut Down Inning%")
    for (col in columns) {
      value <- unname(values[col])
      out[[col]] <- if (length(value) != 1 || !is.finite(value)) "NA" else if (grepl("%", col, fixed = TRUE))
        sprintf("%.0f%%", 100 * value) else if (col %in% c("BAA", "SLG", "OPS", "wOBA", "wOBAcon"))
          sprintf("%.3f", value) else if (col %in% c("K/9", "BB/9", "H/9")) sprintf("%.1f", value) else sprintf("%.2f", value)
    }
    tibble::as_tibble(out)
  })
  raw <- dplyr::bind_rows(rows)
  lower <- c("BAA", "SLG", "OPS", "WHIP", "BB/9", "H/9", "pRV", "FIP", "wOBA", "wOBAcon", "BB%", "BB+HBP%", "Barrel%")
  columns <- setdiff(names(raw), c("Situation", "IP", "PA", "Pitches (#)"))
  shaded <- shade_columns_txst(raw, cols = columns,
    lower_better = lower, percent_cols = columns[grepl("%", columns, fixed = TRUE)])
  attr(shaded, "raw_df") <- raw
  shaded
}

pitching_situation_coverage_note <- function(d) {
  if (!nrow(d)) return("No pitches match the selected filters.")
  score <- sum(!is.finite(d$.sit_lead))
  light <- sum(is.na(d$.sit_day_night))
  paste0("Scores use the start of each PA. Save Situations overlaps Tight Lead. Day ends 30 minutes before local sunset.",
    if (score + light > 0) paste0(" Unclassified: ", score, " pitches lack complete score context; ", light,
      " lack venue/time data. These pitches remain in Total but are omitted from the affected splits.") else "")
}

pitching_situation_file_suffix <- function(situation) {
  if (is.null(situation) || situation %in% c("Total", "TOTAL")) return("")
  situation <- gsub("+", "plus", situation, fixed = TRUE)
  paste0("_", gsub("[^[:alnum:]]+", "_", situation))
}
