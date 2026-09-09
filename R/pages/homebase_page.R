# HomeBASE: roster-first player profiles with optional national lookup.
#
# The overview and recent-game summaries stay lightweight. The original CAPS
# card engine is loaded only when a user asks to generate a hitter or pitcher
# card, preserving fast HomeBASE startup for the active roster.

homebase_name_key <- function(x) {
  x <- as.character(x)
  comma_name <- grepl(",", x, fixed = TRUE)
  x[comma_name] <- vapply(strsplit(x[comma_name], ",", fixed = TRUE), function(parts) {
    if (length(parts) < 2L) return(parts[[1]])
    paste(trimws(parts[[2]]), trimws(parts[[1]]))
  }, character(1))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]", "", tolower(x))
}

homebase_display_name <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("")
  x <- trimws(as.character(x[[1]]))
  if (!grepl(",", x, fixed = TRUE)) return(x)
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2L) x else paste(trimws(parts[[2]]), trimws(parts[[1]]))
}

homebase_team_name <- function(team_code) {
  if (exists("base_team_display_name", mode = "function", inherits = TRUE)) {
    return(base_team_display_name(team_code))
  }
  as.character(team_code)
}

homebase_team_logo_url <- function(team_code) {
  code <- as.character(team_code)
  code <- code[!is.na(code) & nzchar(trimws(code))]
  if (!length(code)) return(NA_character_)
  code <- code[[1]]

  if (exists("base_team_matches", mode = "function", inherits = TRUE) &&
      isTRUE(base_team_matches(code))) {
    for (helper in c("base_supercat_logo_url", "base_team_logo_url")) {
      if (exists(helper, mode = "function", inherits = TRUE)) {
        value <- tryCatch(get(helper, inherits = TRUE)(), error = function(e) NA_character_)
        if (length(value) && !is.na(value[[1]]) && nzchar(value[[1]])) return(value[[1]])
      }
    }
  }

  if (exists("team_palette", mode = "function", inherits = TRUE)) {
    value <- tryCatch(team_palette(code)$logo_url, error = function(e) NA_character_)
    if (length(value) && !is.na(value[[1]]) && nzchar(value[[1]])) return(value[[1]])
  }
  NA_character_
}

homebase_team_monogram <- function(team_name) {
  words <- unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", as.character(team_name)), "\\s+"))
  words <- words[nzchar(words)]
  if (!length(words)) return("TEAM")
  chosen <- if (length(words) == 1L) words else words[c(1L, length(words))]
  toupper(paste0(substr(chosen, 1L, 1L), collapse = ""))
}

homebase_format_game_date <- function(value) {
  value <- suppressWarnings(as.Date(value))
  if (!length(value) || is.na(value[[1]])) return("Date unavailable")
  paste(format(value[[1]], "%b"), as.integer(format(value[[1]], "%d")))
}

homebase_history_file <- function(role = c("hitter", "pitcher")) {
  role <- match.arg(role)
  env_name <- if (role == "hitter") "BASE_HOMEBASE_HITTER_HISTORY_FILE" else "BASE_HOMEBASE_PITCHER_HISTORY_FILE"
  configured <- Sys.getenv(env_name, unset = "")
  if (nzchar(trimws(configured))) return(base_env_path(env_name))
  base_project_path(
    "WallyApps",
    if (role == "hitter") "HittingApp" else "PitchingApp",
    "data",
    "2026 Season - cleaned.csv"
  )
}

homebase_history_label <- function() {
  value <- Sys.getenv("BASE_HOMEBASE_HISTORY_LABEL", unset = "")
  if (nzchar(trimws(value))) trimws(value) else "2026 season"
}

homebase_history_dataset <- local({
  cache <- new.env(parent = emptyenv())
  function(role = c("hitter", "pitcher"), refresh = FALSE) {
    role <- match.arg(role)
    path <- homebase_history_file(role)
    key <- paste(role, normalizePath(path, winslash = "/", mustWork = FALSE), sep = "::")
    if (!isTRUE(refresh) && exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    rows <- if (!file.exists(path)) {
      tibble::tibble()
    } else {
      tryCatch(
        readr::read_csv(
          path,
          col_types = readr::cols(.default = readr::col_character()),
          progress = FALSE,
          show_col_types = FALSE
        ) %>% tibble::as_tibble(),
        error = function(e) {
          message("HomeBASE could not read ", basename(path), ": ", conditionMessage(e))
          tibble::tibble()
        }
      )
    }
    assign(key, rows, envir = cache)
    rows
  }
})

homebase_load_history_rows <- function(role = c("hitter", "pitcher"), player, refresh = FALSE) {
  role <- match.arg(role)
  rows <- homebase_history_dataset(role, refresh = refresh)
  if (!nrow(rows)) return(tibble::tibble())
  player_col <- if (role == "hitter") "Batter" else "Pitcher"
  team_col <- if (role == "hitter") "BatterTeam" else "PitcherTeam"
  if (!all(c(player_col, team_col) %in% names(rows))) return(tibble::tibble())
  keep <- homebase_name_key(rows[[player_col]]) == homebase_name_key(player) &
    !is.na(rows[[team_col]]) & grepl(TEAM_CONFIG$data_pattern, rows[[team_col]], ignore.case = TRUE, perl = TRUE)
  result <- rows[keep, , drop = FALSE]
  if (nrow(result)) result$DataSource <- homebase_history_label()
  result
}

homebase_first_column <- function(data, candidates, default = NA) {
  match <- candidates[candidates %in% names(data)]
  if (!length(match)) return(rep(default, nrow(data)))
  data[[match[[1]]]]
}

homebase_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

homebase_parse_dates <- function(data) {
  raw <- homebase_first_column(
    data,
    c("Date", "GameDate", "LocalDateTime", "UTCDateTime", "DateTime"),
    NA_character_
  )
  raw <- substr(as.character(raw), 1L, 10L)
  parsed <- suppressWarnings(as.Date(raw))
  # TrackMan exports also appear as mm/dd/yyyy.
  missing <- is.na(parsed) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", raw)
  parsed[missing] <- suppressWarnings(as.Date(raw[missing], format = "%m/%d/%Y"))
  parsed
}

homebase_game_key <- function(data, dates = homebase_parse_dates(data)) {
  id <- as.character(homebase_first_column(data, c("GameUID", "GameID", "GameId"), ""))
  id[is.na(id)] <- ""
  date_label <- ifelse(is.na(dates), "Unknown date", format(dates, "%Y-%m-%d"))
  ifelse(nzchar(trimws(id)), paste0(date_label, "::", id), date_label)
}

homebase_latest_date <- function(x) {
  x <- as.Date(x)
  if (!length(x) || all(is.na(x))) return(as.Date(NA))
  max(x, na.rm = TRUE)
}

homebase_first_text <- function(x, fallback = "Opponent") {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[[1]] else fallback
}

homebase_pa_rows <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) return(tibble::tibble())
  d <- tibble::as_tibble(data)
  d$.hb_row <- seq_len(nrow(d))
  d$.hb_date <- homebase_parse_dates(d)
  d$.hb_game <- homebase_game_key(d, d$.hb_date)
  inning <- as.character(homebase_first_column(d, c("Inning"), ""))
  pa_inning <- as.character(homebase_first_column(d, c("PAofInning", "PAOfInning"), ""))
  batter <- as.character(homebase_first_column(d, c("Batter", "BatterName"), ""))
  pitcher <- as.character(homebase_first_column(d, c("Pitcher", "PitcherName"), ""))
  pa_key <- paste(d$.hb_game, inning, pa_inning, batter, pitcher, sep = "::")

  if (!any(nzchar(trimws(pa_inning)))) {
    pitch_of_pa <- homebase_number(homebase_first_column(d, c("PitchofPA", "PitchOfPA"), NA))
    boundary <- is.finite(pitch_of_pa) & pitch_of_pa <= 1
    boundary[is.na(boundary)] <- FALSE
    sequence_id <- ave(boundary, d$.hb_game, FUN = function(x) cumsum(x))
    pa_key <- paste(d$.hb_game, batter, pitcher, sequence_id, sep = "::")
  }

  d$.hb_pa <- pa_key
  d %>%
    dplyr::group_by(.hb_pa) %>%
    dplyr::slice_max(.hb_row, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
}

homebase_classify_pa <- function(pa) {
  if (is.null(pa) || !nrow(pa)) return(tibble::tibble())
  pr <- tolower(trimws(as.character(homebase_first_column(pa, c("PlayResult", "play_result"), ""))))
  kb <- tolower(trimws(as.character(homebase_first_column(pa, c("KorBB", "KOrBB"), ""))))
  pc <- tolower(trimws(as.character(homebase_first_column(pa, c("PitchCall", "pitch_call"), ""))))
  is_hr <- grepl("home.?run|homerun|^hr$", pr)
  is_triple <- grepl("triple", pr)
  is_double <- grepl("double", pr) & !grepl("double.?play", pr)
  is_single <- grepl("single", pr)
  is_hit <- is_single | is_double | is_triple | is_hr
  is_walk <- grepl("walk|base.?on.?balls|^bb$", pr) |
    grepl("walk|base.?on.?balls|^bb$", kb)
  is_hbp <- grepl("hit.?by.?pitch|^hbp$", pr) | grepl("hit.?by.?pitch|^hbp$", kb)
  is_k <- grepl("strike.?out|^k$", pr) | grepl("strike.?out|^k$", kb)
  is_sac <- grepl("sacrifice|sac.?fly|sac.?bunt", pr)
  is_bip <- is_hit | grepl("in.?play|ground|line|fly|pop|field.?out|error|sac", paste(pr, pc))
  tb <- as.numeric(is_single) + 2 * as.numeric(is_double) +
    3 * as.numeric(is_triple) + 4 * as.numeric(is_hr)
  dplyr::mutate(
    pa,
    .hb_hit = is_hit,
    .hb_hr = is_hr,
    .hb_walk = is_walk,
    .hb_hbp = is_hbp,
    .hb_k = is_k,
    .hb_sac = is_sac,
    .hb_bip = is_bip,
    .hb_tb = tb
  )
}

homebase_hitter_metrics <- function(data) {
  pa <- homebase_classify_pa(homebase_pa_rows(data))
  if (!nrow(pa)) return(NULL)
  ab <- sum(!pa$.hb_walk & !pa$.hb_hbp & !pa$.hb_sac, na.rm = TRUE)
  hits <- sum(pa$.hb_hit, na.rm = TRUE)
  walks <- sum(pa$.hb_walk, na.rm = TRUE)
  hbp <- sum(pa$.hb_hbp, na.rm = TRUE)
  ev <- homebase_number(homebase_first_column(pa, c("ExitSpeed", "ExitVelocity", "ev"), NA))
  bip_ev <- ev[pa$.hb_bip & is.finite(ev)]
  pitch_call <- tolower(as.character(homebase_first_column(data, c("PitchCall", "pitch_call"), "")))
  swing <- grepl("swing|foul|in.?play", pitch_call)
  whiff <- grepl("swinging|swing.*strike|whiff", pitch_call)
  px <- homebase_number(homebase_first_column(data, c("PlateLocSide", "PlateX", "plate_x"), NA))
  pz <- homebase_number(homebase_first_column(data, c("PlateLocHeight", "PlateZ", "plate_z"), NA))
  in_zone <- is.finite(px) & is.finite(pz) & abs(px) <= .83 & pz >= 1.5 & pz <= 3.5
  outside <- is.finite(px) & is.finite(pz) & !in_zone
  list(
    PA = nrow(pa),
    AVG = if (ab > 0) hits / ab else NA_real_,
    OBP = if ((ab + walks + hbp) > 0) (hits + walks + hbp) / (ab + walks + hbp) else NA_real_,
    SLG = if (ab > 0) sum(pa$.hb_tb, na.rm = TRUE) / ab else NA_real_,
    `K%` = mean(pa$.hb_k, na.rm = TRUE),
    `BB%` = mean(pa$.hb_walk, na.rm = TRUE),
    `Contact%` = if (sum(swing, na.rm = TRUE) > 0) 1 - sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) else NA_real_,
    `Chase%` = if (sum(outside, na.rm = TRUE) > 0) sum(swing & outside, na.rm = TRUE) / sum(outside, na.rm = TRUE) else NA_real_,
    `HardHit%` = if (length(bip_ev)) mean(bip_ev >= 95) else NA_real_,
    `Avg EV` = if (length(bip_ev)) mean(bip_ev) else NA_real_,
    `Max EV` = if (length(bip_ev)) max(bip_ev) else NA_real_
  )
}

homebase_score_pitcher_rows <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) return(tibble::tibble())
  scored <- tibble::as_tibble(data)

  # HomeBASE must agree with the original CAPS card. That card uses BrewStuff's
  # LightGBM model and its own arm-angle / fastball-difference preparation. The
  # scouting-module scorer has a different feature contract and produced
  # invalid Stuff+ values for these TrackMan rows, so it is intentionally not
  # used here.
  can_score <- all(vapply(
    c("pitcher_summary", "getBrewStuff"),
    exists, logical(1), mode = "function", inherits = TRUE
  )) && exists("model", inherits = TRUE)
  if (can_score) {
    brewstuff <- tryCatch({
      prepared <- pitcher_summary(scored)
      getBrewStuff(prepared, get("model", inherits = TRUE), FALSE)
    }, error = function(e) {
      message("HomeBASE BrewStuff scoring unavailable: ", conditionMessage(e))
      NULL
    })
    if (!is.null(brewstuff) && "Stuff" %in% names(brewstuff)) {
      valid_stuff <- homebase_number(brewstuff$Stuff)
      valid_stuff <- valid_stuff[is.finite(valid_stuff) & valid_stuff >= 50]
      scored$StuffPlus <- if (length(valid_stuff)) mean(valid_stuff) else NA_real_
      scored$StuffPlusPitches <- length(valid_stuff)
    }
  }
  if (!"StuffPlus" %in% names(scored)) scored$StuffPlus <- NA_real_
  if (!"StuffPlusPitches" %in% names(scored)) {
    scored$StuffPlusPitches <- sum(is.finite(homebase_number(scored$StuffPlus)))
  }
  scored
}

homebase_finite_mean <- function(x) {
  x <- homebase_number(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

homebase_pitcher_metrics <- function(data) {
  pa <- homebase_classify_pa(homebase_pa_rows(data))
  if (!nrow(pa) && !nrow(data)) return(NULL)
  pitch_type <- as.character(homebase_first_column(data, c("TaggedPitchType", "PitchType", "AutoPitchType"), ""))
  pitch_type <- pitch_type[!is.na(pitch_type) & nzchar(trimws(pitch_type)) &
                             !tolower(pitch_type) %in% c("undefined", "untagged")]
  usage <- sort(table(pitch_type), decreasing = TRUE)
  primary <- if (length(usage)) names(usage)[[1]] else "—"
  velo <- homebase_number(homebase_first_column(data, c("RelSpeed", "Velocity"), NA))
  all_pitch_types <- tolower(as.character(
    homebase_first_column(data, c("TaggedPitchType", "PitchType", "AutoPitchType"), "")
  ))
  is_fastball <- grepl("fast|four.?seam|sink|two.?seam", all_pitch_types)
  avg_fastball <- if (length(pitch_type)) {
    value <- velo[is_fastball & is.finite(velo)]
    if (length(value)) mean(value) else NA_real_
  } else NA_real_
  pitch_call <- tolower(as.character(homebase_first_column(data, c("PitchCall", "pitch_call"), "")))
  swing <- grepl("swing|foul|in.?play", pitch_call)
  whiff <- grepl("swinging|swing.*strike|whiff", pitch_call)
  px <- homebase_number(homebase_first_column(data, c("PlateLocSide", "PlateX", "plate_x"), NA))
  pz <- homebase_number(homebase_first_column(data, c("PlateLocHeight", "PlateZ", "plate_z"), NA))
  zone_known <- is.finite(px) & is.finite(pz)
  in_zone <- zone_known & abs(px) <= (9.97 / 12) & pz >= (18.29 / 12) & pz <= (44.08 / 12)
  outside <- zone_known & !in_zone
  ev <- homebase_number(homebase_first_column(data, c("ExitSpeed", "ExitVelocity", "ev"), NA))
  angle <- homebase_number(homebase_first_column(data, c("Angle", "LaunchAngle"), NA))
  extension <- homebase_number(homebase_first_column(data, c("Extension", "ReleaseExtension"), NA))
  pitch_call_compact <- gsub("[^a-z]", "", pitch_call)
  is_strike <- pitch_call_compact %in% c(
    "strikecalled", "strikeswinging", "foulball", "foulballfieldable",
    "foulballnotfieldable", "foultip", "inplay", "inplayout", "inplaynoout"
  )
  is_csw <- pitch_call_compact %in% c("strikecalled", "strikeswinging")
  play_result <- tolower(as.character(homebase_first_column(data, c("PlayResult", "play_result"), "")))
  is_bip <- grepl("^in.?play", pitch_call) |
    grepl("single|double|triple|home.?run|ground|fly|line|pop|error|sac|out", play_result)
  barrel_ok <- is_bip & is.finite(ev) & is.finite(angle)
  is_barrel <- barrel_ok & ev >= 95 & angle >= 5 & angle <= 40
  is_hard_hit <- is_bip & is.finite(ev) & ev >= 95
  hit_type <- tolower(as.character(homebase_first_column(data, c("TaggedHitType", "AutoHitType"), "")))
  is_gb <- is_bip & ((is.finite(angle) & angle < 5) | grepl("ground", hit_type))
  pitch_number <- homebase_number(homebase_first_column(data, c("PitchofPA", "PitchOfPA"), NA))
  first_pitch <- is.finite(pitch_number) & pitch_number == 1

  outs_on_play <- homebase_number(homebase_first_column(pa, c("OutsOnPlay"), 0))
  outs_on_play[!is.finite(outs_on_play)] <- 0
  pa_result <- tolower(as.character(homebase_first_column(pa, c("PlayResult", "play_result"), "")))
  outs_on_play[grepl("triple.?play", pa_result)] <- pmax(outs_on_play[grepl("triple.?play", pa_result)], 3)
  outs_on_play[grepl("double.?play", pa_result)] <- pmax(outs_on_play[grepl("double.?play", pa_result)], 2)
  ordinary_out <- grepl("out", pa_result) & !pa$.hb_k
  outs_on_play[ordinary_out] <- pmax(outs_on_play[ordinary_out], 1)
  outs <- sum(outs_on_play, na.rm = TRUE) + sum(pa$.hb_k, na.rm = TRUE)
  innings <- outs / 3
  hits <- sum(pa$.hb_hit, na.rm = TRUE)
  ab <- sum(!pa$.hb_walk & !pa$.hb_hbp & !pa$.hb_sac, na.rm = TRUE)
  walks <- sum(pa$.hb_walk, na.rm = TRUE)
  hbp <- sum(pa$.hb_hbp, na.rm = TRUE)
  strikeouts <- sum(pa$.hb_k, na.rm = TRUE)
  homers <- sum(pa$.hb_hr, na.rm = TRUE)
  runs <- homebase_number(homebase_first_column(pa, c("RunsScored"), 0))
  runs[!is.finite(runs)] <- 0
  total_runs <- sum(runs, na.rm = TRUE)
  total_bases <- sum(pa$.hb_tb, na.rm = TRUE)
  games <- length(unique(homebase_game_key(data)))
  stuff_pitches <- homebase_number(homebase_first_column(data, c("StuffPlusPitches"), NA))
  stuff_pitches <- stuff_pitches[is.finite(stuff_pitches)]
  stuff_pitches <- if (length(stuff_pitches)) max(stuff_pitches) else
    sum(is.finite(homebase_number(homebase_first_column(data, c("StuffPlus"), NA))))
  fb_pitches <- sum(is_fastball & is.finite(velo), na.rm = TRUE)
  known_zone_pitches <- sum(zone_known, na.rm = TRUE)
  swings <- sum(swing, na.rm = TRUE)
  out_of_zone <- sum(outside, na.rm = TRUE)
  bip_evla <- sum(barrel_ok, na.rm = TRUE)
  bip_la <- sum(is_bip & is.finite(angle), na.rm = TRUE)
  woba_den <- sum(!pa$.hb_sac, na.rm = TRUE)
  woba_num <- 0.690 * walks + 0.720 * hbp +
    0.880 * sum(pa$.hb_tb == 1, na.rm = TRUE) +
    1.247 * sum(pa$.hb_tb == 2, na.rm = TRUE) +
    1.578 * sum(pa$.hb_tb == 3, na.rm = TRUE) +
    2.031 * homers
  list(
    Games = games,
    Pitches = nrow(data),
    BF = nrow(pa),
    Outs = outs,
    IP = innings,
    H = hits,
    R = total_runs,
    HR = homers,
    K = strikeouts,
    BB = walks,
    HBP = hbp,
    `R/9` = if (innings > 0) 9 * total_runs / innings else NA_real_,
    `K/9` = if (innings > 0) 9 * strikeouts / innings else NA_real_,
    `BB/9` = if (innings > 0) 9 * walks / innings else NA_real_,
    `H/9` = if (innings > 0) 9 * hits / innings else NA_real_,
    BAA = if (ab > 0) hits / ab else NA_real_,
    SLG = if (ab > 0) total_bases / ab else NA_real_,
    `K%` = if (nrow(pa)) mean(pa$.hb_k, na.rm = TRUE) else NA_real_,
    `BB%` = if (nrow(pa)) mean(pa$.hb_walk, na.rm = TRUE) else NA_real_,
    `K-BB%` = if (nrow(pa)) mean(pa$.hb_k, na.rm = TRUE) - mean(pa$.hb_walk, na.rm = TRUE) else NA_real_,
    `Whiff%` = if (sum(swing, na.rm = TRUE) > 0) sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) else NA_real_,
    `Contact%` = if (swings > 0) 1 - sum(whiff, na.rm = TRUE) / swings else NA_real_,
    `Chase%` = if (sum(outside, na.rm = TRUE) > 0) sum(swing & outside, na.rm = TRUE) / sum(outside, na.rm = TRUE) else NA_real_,
    `CSW%` = if (nrow(data)) mean(is_csw, na.rm = TRUE) else NA_real_,
    `Strike%` = if (nrow(data)) mean(is_strike, na.rm = TRUE) else NA_real_,
    `Zone%` = if (known_zone_pitches > 0) sum(in_zone, na.rm = TRUE) / known_zone_pitches else NA_real_,
    `FPS%` = if (sum(first_pitch, na.rm = TRUE) > 0) sum(is_strike & first_pitch, na.rm = TRUE) / sum(first_pitch, na.rm = TRUE) else NA_real_,
    `Barrel%` = if (sum(barrel_ok, na.rm = TRUE) > 0) sum(is_barrel, na.rm = TRUE) / sum(barrel_ok, na.rm = TRUE) else NA_real_,
    `HardHit%` = if (any(is_bip & is.finite(ev))) mean(is_hard_hit[is_bip & is.finite(ev)], na.rm = TRUE) else NA_real_,
    `GB%` = if (bip_la > 0) sum(is_gb & is.finite(angle), na.rm = TRUE) / bip_la else NA_real_,
    FIP = if (innings > 0) (13 * homers + 3 * (walks + hbp) - 2 * strikeouts) / innings + 3.214 else NA_real_,
    WHIP = if (innings > 0) (walks + hits) / innings else NA_real_,
    wOBA = if (woba_den > 0) woba_num / woba_den else NA_real_,
    `Avg EV` = if (any(is.finite(ev))) mean(ev, na.rm = TRUE) else NA_real_,
    `Avg FB` = avg_fastball,
    Extension = if (any(is_fastball & is.finite(extension))) mean(extension[is_fastball & is.finite(extension)]) else NA_real_,
    `Max Velo` = if (any(is.finite(velo))) max(velo, na.rm = TRUE) else NA_real_,
    `Stuff+` = homebase_finite_mean(homebase_first_column(data, c("StuffPlus"), NA)),
    `Stuff+ pitches` = stuff_pitches,
    `FB pitches` = fb_pitches,
    `Zone pitches` = known_zone_pitches,
    Swings = swings,
    OOZ = out_of_zone,
    `BIP EVLA` = bip_evla,
    `BIP LA` = bip_la,
    Primary = primary,
    Arsenal = length(usage)
  )
}

homebase_pitching_percentile_reference <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- base_project_path("WallyApps", "PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv")
    cached <<- tryCatch(
      readr::read_csv(path, show_col_types = FALSE) %>%
        dplyr::filter(scope == "overall", is.finite(value)) %>%
        dplyr::select(metric, value),
      error = function(e) tibble::tibble(metric = character(), value = numeric())
    )
    cached
  }
})

homebase_empirical_percentile <- function(value, metric, lower_better = FALSE) {
  if (!is.finite(value)) return(NA_real_)
  ref <- homebase_pitching_percentile_reference()
  pool <- ref$value[ref$metric == metric & is.finite(ref$value)]
  if (!length(pool)) return(NA_real_)
  percentile <- if (isTRUE(lower_better)) mean(pool >= value) else mean(pool <= value)
  pmin(99, pmax(1, round(100 * percentile)))
}

homebase_estimated_percentile <- function(value, mean, sd, lower_better = FALSE) {
  if (!is.finite(value) || !is.finite(mean) || !is.finite(sd) || sd <= 0) return(NA_real_)
  percentile <- stats::pnorm(value, mean = mean, sd = sd)
  if (isTRUE(lower_better)) percentile <- 1 - percentile
  pmin(99, pmax(1, round(100 * percentile)))
}

homebase_percentile_rows <- function(data, role = c("hitter", "pitcher")) {
  role <- match.arg(role)
  if (role == "hitter") {
    m <- homebase_hitter_metrics(data)
    if (is.null(m)) return(tibble::tibble())
    specs <- tibble::tribble(
      ~Group, ~Label, ~Key, ~Mean, ~SD, ~LowerBetter, ~Kind,
      "Results & traits", "K%", "K%",              .1920, .0500, TRUE,  "pct",
      "Results & traits", "BB%", "BB%",            .1140, .0500, FALSE, "pct",
      "Results & traits", "SLG", "SLG",            .4410, .1200, FALSE, "dec",
      "Results & traits", "Contact%", "Contact%",  .7710, .0500, FALSE, "pct",
      "Results & traits", "Chase%", "Chase%",      .2420, .0500, TRUE,  "pct",
      "Results & traits", "Max EV", "Max EV",      106.28, 5.62, FALSE, "num"
    )
    specs %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Value = as.numeric(m[[Key]]),
        Percentile = homebase_estimated_percentile(Value, Mean, SD, LowerBetter),
        Display = dplyr::case_when(
          Kind == "pct" ~ homebase_format_rate(Value),
          Kind == "dec" ~ homebase_format_decimal(Value),
          TRUE ~ ifelse(is.finite(Value), sprintf("%.1f", Value), "—")
        )
      ) %>%
      dplyr::ungroup()
  } else {
    m <- homebase_pitcher_metrics(data)
    if (is.null(m)) return(tibble::tibble())
    specs <- tibble::tribble(
      ~Group, ~Label, ~Key, ~Metric, ~LowerBetter, ~Kind, ~SampleKey, ~MinSample,
      "BASE model", "Stuff+", "Stuff+", NA_character_, FALSE, "plus", "Stuff+ pitches", 25,
      "Run prevention", "FIP", "FIP", "performance_fip", TRUE, "num2", "BF", 50,
      "Run prevention", "WHIP", "WHIP", "performance_whip", TRUE, "num2", "BF", 50,
      "Run prevention", "BAA", "BAA", "performance_baa", TRUE, "dec", "BF", 50,
      "Run prevention", "SLG allowed", "SLG", "performance_slg", TRUE, "dec", "BF", 50,
      "Run prevention", "wOBA allowed", "wOBA", "performance_woba", TRUE, "dec", "BF", 50,
      "Run prevention", "K/9", "K/9", "performance_k9", FALSE, "num1", "BF", 50,
      "Run prevention", "BB/9", "BB/9", "performance_bb9", TRUE, "num1", "BF", 50,
      "Run prevention", "H/9", "H/9", "performance_h9", TRUE, "num1", "BF", 50,
      "Run prevention", "K%", "K%", "performance_k_pct", FALSE, "pct", "BF", 50,
      "Run prevention", "BB%", "BB%", "performance_bb_pct", TRUE, "pct", "BF", 50,
      "Pitch execution", "CSW%", "CSW%", "performance_csw_pct", FALSE, "pct", "Pitches", 100,
      "Pitch execution", "Whiff%", "Whiff%", "performance_whiff_pct", FALSE, "pct", "Swings", 25,
      "Pitch execution", "Chase%", "Chase%", "performance_chase_pct", FALSE, "pct", "OOZ", 25,
      "Pitch execution", "Strike%", "Strike%", "performance_strike_pct", FALSE, "pct", "Pitches", 100,
      "Pitch execution", "Zone%", "Zone%", "performance_zone_pct", FALSE, "pct", "Zone pitches", 100,
      "Pitch execution", "FPS%", "FPS%", "performance_fps_pct", FALSE, "pct", "BF", 50,
      "Contact & traits", "Barrel%", "Barrel%", "performance_barrel_pct", TRUE, "pct", "BIP EVLA", 20,
      "Contact & traits", "GB%", "GB%", "performance_gb_pct", FALSE, "pct", "BIP LA", 20,
      "Contact & traits", "Avg EV", "Avg EV", "avg_ev", TRUE, "num1", "BIP EVLA", 20,
      "Contact & traits", "FB velo", "Avg FB", "heater_velocity", FALSE, "num1", "FB pitches", 25,
      "Contact & traits", "Extension", "Extension", "extension", FALSE, "num1", "FB pitches", 25
    )
    specs %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Value = as.numeric(m[[Key]]),
        Sample = as.numeric(m[[SampleKey]]),
        Eligible = is.finite(Sample) && Sample >= MinSample,
        Percentile = if (!Eligible) {
          NA_real_
        } else if (Kind == "plus") {
          homebase_estimated_percentile(Value, 100, 10, FALSE)
        } else {
          homebase_empirical_percentile(Value, Metric, LowerBetter)
        },
        Display = dplyr::case_when(
          Kind == "pct" ~ homebase_format_rate(Value),
          Kind == "dec" ~ homebase_format_decimal(Value),
          Kind == "num2" ~ ifelse(is.finite(Value), sprintf("%.2f", Value), "—"),
          Kind == "plus" ~ ifelse(is.finite(Value), sprintf("%.0f", Value), "—"),
          TRUE ~ ifelse(is.finite(Value), sprintf("%.1f", Value), "—")
        )
      ) %>%
      dplyr::ungroup()
  }
}

homebase_percentile_color <- function(percentile) {
  if (!is.finite(percentile)) return("#A8B0B8")
  if (percentile >= 75) return("#B4232C")
  if (percentile >= 55) return("#C67662")
  if (percentile >= 40) return("#9BB1B8")
  "#5D7EBC"
}

homebase_percentile_card <- function(rows, title, subtitle, estimated = FALSE) {
  if (!nrow(rows)) return(NULL)
  bar_rows <- list()
  previous_group <- NULL
  for (i in seq_len(nrow(rows))) {
    group <- if ("Group" %in% names(rows)) as.character(rows$Group[[i]]) else "Statistics"
    if (!identical(group, previous_group)) {
      bar_rows <- c(bar_rows, list(tags$div(class = "hb-percentile-group", group)))
      previous_group <- group
    }
    percentile <- rows$Percentile[[i]]
    position <- if (is.finite(percentile)) pmin(99, pmax(1, percentile)) else 50
    label <- if (is.finite(percentile)) as.character(as.integer(percentile)) else "—"
    color <- homebase_percentile_color(percentile)
    bar_rows <- c(bar_rows, list(tags$div(
      class = paste0("hb-percentile-row", if (!is.finite(percentile)) " is-empty" else ""),
      tags$span(class = "hb-percentile-label", rows$Label[[i]]),
      tags$div(
        class = "hb-percentile-track-wrap",
        tags$div(class = "hb-percentile-track"),
        tags$div(class = "hb-percentile-fill", style = sprintf("width:%.0f%%; background:%s;", position, color)),
        tags$div(class = "hb-percentile-average"),
        tags$span(class = "hb-percentile-badge", style = sprintf("left:%.0f%%; background:%s;", position, color), label)
      ),
      tags$span(class = "hb-percentile-value", rows$Display[[i]])
    )))
  }
  tags$section(
    class = "hb-percentile-card",
    tags$div(
      class = "hb-percentile-head",
      tags$div(tags$span(subtitle), tags$h3(title)),
      if (estimated) tags$small("Estimated from CAPS D1 benchmarks") else tags$small("D1 reference minimums enforced")
    ),
    tags$div(class = "hb-percentile-scale", tags$span("25th"), tags$span("50th"), tags$span("75th")),
    tags$div(class = "hb-percentile-rows", tagList(bar_rows))
  )
}

homebase_format_rate <- function(x) {
  if (!length(x) || !is.finite(x)) "—" else paste0(round(100 * x, 1), "%")
}

homebase_format_number <- function(x, digits = 1L) {
  if (!length(x) || !is.finite(x)) return("—")
  sprintf(paste0("%.", as.integer(digits), "f"), x)
}

homebase_format_innings <- function(outs) {
  if (!length(outs) || !is.finite(outs)) return("—")
  outs <- max(0L, as.integer(round(outs)))
  paste0(outs %/% 3L, ".", outs %% 3L)
}

homebase_format_decimal <- function(x) {
  if (!length(x) || !is.finite(x)) return("—")
  sub("^0", "", sprintf("%.3f", x))
}

homebase_recent_games <- function(data, role = c("hitter", "pitcher"), limit = 5L) {
  role <- match.arg(role)
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) return(tibble::tibble())
  pitches <- tibble::as_tibble(data)
  pitches$.hb_date <- homebase_parse_dates(pitches)
  pitches$.hb_game <- homebase_game_key(pitches, pitches$.hb_date)
  opponent_col <- if (role == "hitter") c("PitcherTeam") else c("BatterTeam")
  pitches$.hb_opp <- as.character(homebase_first_column(pitches, opponent_col, "Opponent"))
  pa <- homebase_classify_pa(homebase_pa_rows(pitches))
  if (!nrow(pa)) return(tibble::tibble())
  pa$.hb_opp <- as.character(homebase_first_column(pa, opponent_col, "Opponent"))
  runs <- homebase_number(homebase_first_column(pa, c("RunsScored"), 0))
  runs[!is.finite(runs)] <- 0
  pa$.hb_runs <- runs

  if (role == "hitter") {
    recent <- pa %>%
      dplyr::group_by(.hb_game) %>%
      dplyr::summarise(
        Date = homebase_latest_date(.hb_date),
        Opponent = homebase_team_name(homebase_first_text(.hb_opp)),
        PA = dplyr::n(),
        AB = sum(!.hb_walk & !.hb_hbp & !.hb_sac, na.rm = TRUE),
        H = sum(.hb_hit, na.rm = TRUE), HR = sum(.hb_hr, na.rm = TRUE),
        BB = sum(.hb_walk, na.rm = TRUE), K = sum(.hb_k, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    pitch_counts <- pitches %>% dplyr::count(.hb_game, name = "Pitches")
    recent <- pa %>%
      dplyr::group_by(.hb_game) %>%
      dplyr::summarise(
        Date = homebase_latest_date(.hb_date),
        Opponent = homebase_team_name(homebase_first_text(.hb_opp)),
        BF = dplyr::n(), H = sum(.hb_hit, na.rm = TRUE),
        R = sum(.hb_runs, na.rm = TRUE), BB = sum(.hb_walk, na.rm = TRUE),
        K = sum(.hb_k, na.rm = TRUE), .groups = "drop"
      ) %>%
      dplyr::left_join(pitch_counts, by = ".hb_game")
  }
  recent %>%
    dplyr::mutate(.hb_sort = ifelse(is.na(Date), as.Date("1900-01-01"), Date)) %>%
    dplyr::arrange(dplyr::desc(.hb_sort), dplyr::desc(.hb_game)) %>%
    dplyr::slice_head(n = as.integer(limit)) %>%
    dplyr::select(-.hb_sort)
}

homebase_roster_data <- function() {
  path <- TEAM_CONFIG$data$roster_file
  rows <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE) %>% tibble::as_tibble(),
    error = function(e) tibble::tibble()
  )
  required <- c("Name", "Pos", "Number", "Bats", "Throws", "pos_type")
  if (!nrow(rows) || !all(required %in% names(rows))) {
    return(tibble::tibble(
      Name = character(), Pos = character(), Number = character(),
      Bats = character(), Throws = character(), pos_type = character(), Group = character()
    ))
  }
  rows %>%
    dplyr::transmute(
      Name = as.character(Name), Pos = as.character(Pos), Number = as.character(Number),
      Bats = as.character(Bats), Throws = as.character(Throws), pos_type = as.character(pos_type),
      Group = dplyr::case_when(
        pos_type == "Pitcher" ~ "Pitcher",
        pos_type == "Catcher" ~ "Catcher",
        pos_type == "Infielder" ~ "Infielder",
        pos_type == "Outfielder" ~ "Outfielder",
        TRUE ~ "Player"
      )
    ) %>%
    dplyr::arrange(suppressWarnings(as.numeric(Number)), Name)
}

homebase_national_catalog <- function(refresh = FALSE) {
  if (!exists("base_get_hitter_catalog", mode = "function", inherits = TRUE) ||
      !exists("base_pitcher_catalog", inherits = TRUE)) {
    stop("The mounted national player catalogs are unavailable.")
  }
  hitters <- base_get_hitter_catalog(refresh = refresh) %>%
    dplyr::transmute(
      Team = as.character(BatterTeam), RawName = as.character(Batter),
      Name = vapply(Batter, homebase_display_name, character(1)), Role = "Hitter"
    )
  pitchers <- base_pitcher_catalog %>%
    dplyr::transmute(
      Team = as.character(PitcherTeam), RawName = as.character(Pitcher),
      Name = vapply(Pitcher, homebase_display_name, character(1)), Role = "Pitcher"
    )
  result <- dplyr::bind_rows(hitters, pitchers) %>%
    dplyr::filter(!is.na(Team), nzchar(Team), !is.na(RawName), nzchar(RawName))
  if (!nrow(result)) stop("The mounted national player catalogs are empty.")
  result
}

homebase_load_national_rows <- function(role, team, player) {
  role <- match.arg(role, c("hitter", "pitcher"))
  if (role == "hitter") {
    base_load_hitter_rows(team, player)
  } else {
    base_load_pitcher_rows(team, player)
  }
}

homebase_catalog <- function() {
  result <- homebase_national_catalog() %>%
    dplyr::filter(!is.na(Team), nzchar(Team), !is.na(RawName), nzchar(RawName)) %>%
    dplyr::group_by(Team, Name, NameKey = homebase_name_key(Name)) %>%
    dplyr::summarise(Roles = paste(sort(unique(Role)), collapse = " / "), .groups = "drop") %>%
    dplyr::arrange(Name, Team)
  attr(result, "homebase_scope") <- "national"
  result
}

homebase_find_catalog_player <- function(name, team = NULL) {
  key <- homebase_name_key(name)
  national <- homebase_national_catalog()
  if (nrow(national)) {
    if (!is.null(team) && nzchar(team)) national <- national[national$Team == team, , drop = FALSE]
    exact_name <- national$Name == homebase_display_name(name)
    national <- if (any(exact_name, na.rm = TRUE)) {
      national[!is.na(exact_name) & exact_name, , drop = FALSE]
    } else {
      national[homebase_name_key(national$Name) == key, , drop = FALSE]
    }
    hitter_national <- national[national$Role == "Hitter", , drop = FALSE]
    pitcher_national <- national[national$Role == "Pitcher", , drop = FALSE]
    return(list(
      hitter = if (nrow(hitter_national)) tibble::tibble(
        BatterTeam = hitter_national$Team[[1]], Batter = hitter_national$RawName[[1]],
        .homebase_source = "national"
      ) else NULL,
      pitcher = if (nrow(pitcher_national)) tibble::tibble(
        PitcherTeam = pitcher_national$Team[[1]], Pitcher = pitcher_national$RawName[[1]],
        .homebase_source = "national"
      ) else NULL
    ))
  }
  list(hitter = NULL, pitcher = NULL)
}

homebase_metric_tile <- function(label, value, detail = NULL, class = NULL) {
  tags$div(
    class = paste("hb-metric", class %||% ""),
    tags$span(label), tags$strong(value),
    if (!is.null(detail)) tags$small(detail)
  )
}

homebase_pitcher_stat_panel <- function(class, title, subtitle, ...) {
  tags$section(
    class = paste("hb-stat-panel", class),
    tags$div(
      class = "hb-stat-panel-head",
      tags$h4(title),
      tags$span(subtitle)
    ),
    tags$div(class = "hb-stat-panel-grid", ...)
  )
}

homebase_recent_ui <- function(data, role) {
  title <- if (role == "hitter") "Recent games · Hitting" else "Recent games · Pitching"
  if (!nrow(data)) {
    return(tags$section(
      class = "hb-recent-block",
      tags$div(class = "hb-section-title", title),
      tags$p(class = "hb-muted", "No dated game results are available for this player.")
    ))
  }
  rows <- lapply(seq_len(nrow(data)), function(i) {
    game <- data[i, , drop = FALSE]
    date_text <- homebase_format_game_date(game$Date[[1]])
    stats <- if (role == "hitter") {
      c(paste0(game$H, "-", game$AB), paste0(game$HR, " HR"), paste0(game$BB, " BB"), paste0(game$K, " K"))
    } else {
      c(paste0(game$Pitches, " P"), paste0(game$BF, " BF"), paste0(game$K, " K"), paste0(game$BB, " BB"), paste0(game$H, " H"), paste0(game$R, " R"))
    }
    tags$div(
      class = "hb-game-row",
      tags$div(class = "hb-game-identity", tags$strong(date_text), tags$span(paste("vs", game$Opponent[[1]]))),
      tags$div(class = "hb-game-line", lapply(stats, tags$span))
    )
  })
  tags$section(class = "hb-recent-block", tags$div(class = "hb-section-title", title), tagList(rows))
}

homebase_caps_pitcher_report <- function(data, player) {
  required <- c(
    "pcard_build_all", "pcard_movement_plot", "pcard_pitch_metrics_table",
    "pcard_location_plot", "pcard_usage_table", "pcard_pitch_hit_metrics",
    "pcard_usage_by_count"
  )
  missing <- required[!vapply(required, exists, logical(1), mode = "function", inherits = TRUE)]
  if (length(missing)) stop("CAPS pitcher report engine is unavailable.")

  typed <- tibble::as_tibble(data)
  numeric_columns <- intersect(
    c(
      "Balls", "Strikes", "Inning", "PAofInning", "PitchofPA", "OutsOnPlay",
      "RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak", "RelHeight",
      "RelSide", "Extension", "VertApprAngle", "HorzApprAngle", "PlateLocSide",
      "PlateLocHeight", "ExitSpeed", "Angle"
    ),
    names(typed)
  )
  typed[numeric_columns] <- lapply(typed[numeric_columns], homebase_number)

  raw_names <- unique(as.character(typed$Pitcher))
  raw_names <- raw_names[!is.na(raw_names) & nzchar(raw_names)]
  matched <- raw_names[homebase_name_key(raw_names) == homebase_name_key(player)]
  pitcher_raw <- if (length(matched)) matched[[1]] else if (length(raw_names)) raw_names[[1]] else player

  report <- pcard_build_all(typed, pitcher_raw)
  pitcher_data <- report$pitcher_data
  report$p_movement <- pcard_movement_plot(pitcher_data)
  report$pitch_metrics_tbl <- pcard_pitch_metrics_table(pitcher_data)
  report$p_location_lhh <- pcard_location_plot(pitcher_data, "Left")
  report$p_location_rhh <- pcard_location_plot(pitcher_data, "Right")
  report$usage_total <- pcard_usage_table(pitcher_data, "All")
  report$usage_rhh <- pcard_usage_table(pitcher_data, "Right")
  report$usage_lhh <- pcard_usage_table(pitcher_data, "Left")
  report$hit_metrics_tbl <- pcard_pitch_hit_metrics(pitcher_data)
  report$count_usage_tbl <- pcard_usage_by_count(pitcher_data)
  report
}

homebase_caps_pitcher_pages <- c(
  "Summary" = "summary",
  "Movement" = "movement",
  "Pitch metrics" = "pitch_metrics",
  "Locations vs LHH" = "location_lhh",
  "Locations vs RHH" = "location_rhh",
  "Usage by hitter side" = "usage",
  "Results by pitch" = "hit_metrics",
  "Usage by count" = "count_usage"
)

homebase_draw_caps_pitcher_page <- function(report, page = "summary") {
  page <- page %||% "summary"
  if (identical(page, "summary")) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit(c(0.46, 0.54), "null"))))
    grid::pushViewport(grid::viewport(layout.pos.row = 1))
    pcard_draw_header(pcard_format_pitcher_name(report$pitcher_raw), report$team_abbr)
    grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2))
    pcard_draw_boxscore(report$box_stats)
    grid::popViewport(2)
  } else if (identical(page, "movement")) {
    pcard_draw_movement_page(report$p_movement)
  } else if (identical(page, "pitch_metrics")) {
    pcard_draw_pitch_metrics_page(report$pitch_metrics_tbl)
  } else if (identical(page, "location_lhh")) {
    pcard_draw_location_lhh_page(report$p_location_lhh)
  } else if (identical(page, "location_rhh")) {
    pcard_draw_location_rhh_page(report$p_location_rhh)
  } else if (identical(page, "usage")) {
    pcard_draw_usage_page(report$usage_total, report$usage_rhh, report$usage_lhh)
  } else if (identical(page, "hit_metrics")) {
    pcard_draw_hit_metrics_page(report$hit_metrics_tbl)
  } else {
    pcard_draw_count_usage_page(report$count_usage_tbl)
  }
  invisible(NULL)
}

homebase_save_caps_pitcher_pdf <- function(file, report) {
  grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page in unname(homebase_caps_pitcher_pages)) {
    homebase_draw_caps_pitcher_page(report, page)
  }
  invisible(file)
}

homebase_build_caps_pitcher_card <- function(data, player) {
  required <- c("pitcher_summary", "build_pitcher_card_page")
  missing <- required[!vapply(required, exists, logical(1), mode = "function", inherits = TRUE)]
  if (length(missing)) stop("The original CAPS pitcher-card engine is unavailable.")
  game <- pitcher_summary(tibble::as_tibble(data)) %>%
    dplyr::mutate(
      TaggedPitchType = if (exists("canonicalize_pitch", mode = "function", inherits = TRUE)) {
        canonicalize_pitch(TaggedPitchType)
      } else TaggedPitchType,
      row_id = dplyr::row_number()
    )
  build_pitcher_card_page(game, homebase_display_name(player))
}

homebase_page_ui <- function() {
  roster <- homebase_roster_data()
  roster_choices <- if (nrow(roster)) {
    stats::setNames(roster$Name, paste0("#", roster$Number, "  ", roster$Name, " · ", roster$Pos))
  } else character()

  tags$div(
    class = "hub-main base-workspace-page homebase-workspace",
    tags$div(
      class = "base-workspace-heading hb-heading",
      tags$div(
        tags$div(class = "base-eyebrow", "Player intelligence"),
        tags$h1("HomeBASE"),
        tags$p(paste0("Current roster and opponent profiles using the ", homebase_history_label(), " baseline."))
      ),
      tags$div(class = "base-source-chip", tags$span(class = "home-status-dot"), TEAM_CONFIG$roster_label)
    ),
    tags$section(
      class = "hb-picker",
      radioButtons(
        "hb_scope", NULL,
        choices = c("Our roster" = "roster", "College player search" = "opponent"),
        selected = "roster", inline = TRUE
      ),
      conditionalPanel(
        "input.hb_scope === 'roster'",
        selectizeInput(
          "hb_roster_player", NULL, choices = c("Select a player…" = "", roster_choices), selected = "",
          options = list(placeholder = "Search the active roster", maxOptions = 50), width = "100%"
        )
      ),
      conditionalPanel(
        "input.hb_scope === 'opponent'",
        tags$div(
          class = "hb-opponent-picker",
          selectizeInput(
            "hb_opponent_player", NULL, choices = NULL,
            options = list(placeholder = "Search any player in the national season file", maxOptions = 20), width = "100%"
          ),
          actionButton("hb_load_opponent", "Open player", class = "btn btn-primary")
        ),
        uiOutput("hb_opponent_source_note")
      )
    ),
    uiOutput("hb_empty_state"),
    uiOutput("hb_profile_header"),
    tags$div(
      class = "hb-profile-grid",
      uiOutput("hb_overview"),
      uiOutput("hb_recent")
    ),
    uiOutput("hb_percentiles"),
    uiOutput("hb_card_studio"),
    tags$div(class = "hub-footer", base_brand_footer())
  )
}

homebase_page_server <- function(input, output, session, requested_player = NULL) {
  selected_profile <- reactiveVal(NULL)
  national_catalog <- reactiveVal(NULL)
  opponent_catalog_scope <- reactiveVal(NULL)
  hitter_card_plot <- reactiveVal(NULL)
  pitcher_card_page <- reactiveVal(NULL)

  roster <- homebase_roster_data()

  load_profile <- function(name, team = NULL, roster_row = NULL) {
    is_roster_player <- !is.null(roster_row) && nrow(roster_row) > 0
    if (is_roster_player) {
      hitter_data <- homebase_load_history_rows("hitter", name)
      pitcher_data <- homebase_load_history_rows("pitcher", name)
      resolved_team <- TEAM_CONFIG$abbreviation
      source_label <- homebase_history_label()
    } else {
      matches <- homebase_find_catalog_player(name, team)
      hitter_data <- if (!is.null(matches$hitter)) {
        if (identical(matches$hitter$.homebase_source[[1]] %||% "", "national")) {
          homebase_load_national_rows("hitter", matches$hitter$BatterTeam[[1]], matches$hitter$Batter[[1]])
        } else {
          base_load_hitter_rows(matches$hitter$BatterTeam[[1]], matches$hitter$Batter[[1]])
        }
      } else tibble::tibble()
      pitcher_data <- if (!is.null(matches$pitcher)) {
        if (identical(matches$pitcher$.homebase_source[[1]] %||% "", "national")) {
          homebase_load_national_rows("pitcher", matches$pitcher$PitcherTeam[[1]], matches$pitcher$Pitcher[[1]])
        } else {
          base_load_pitcher_rows(matches$pitcher$PitcherTeam[[1]], matches$pitcher$Pitcher[[1]])
        }
      } else tibble::tibble()
      national_rows_found <- nrow(hitter_data) > 0 || nrow(pitcher_data) > 0
      resolved_team <- team %||%
        if (!is.null(matches$hitter)) matches$hitter$BatterTeam[[1]] else
        if (!is.null(matches$pitcher)) matches$pitcher$PitcherTeam[[1]] else TEAM_CONFIG$abbreviation
      source_label <- paste(homebase_history_label(), "national season file")
    }
    pitcher_scored <- homebase_score_pitcher_rows(pitcher_data)
    selected_profile(list(
      name = homebase_display_name(name), team = resolved_team,
      roster = roster_row, hitter = hitter_data, pitcher = pitcher_data,
      pitcher_scored = pitcher_scored,
      source_label = source_label
    ))
    hitter_card_plot(NULL)
    pitcher_card_page(NULL)
    invisible(nrow(hitter_data) > 0 || nrow(pitcher_data) > 0)
  }

  observeEvent(input$hb_roster_player, {
    req(nzchar(input$hb_roster_player))
    row <- roster[homebase_name_key(roster$Name) == homebase_name_key(input$hb_roster_player), , drop = FALSE]
    if (!nrow(row)) row <- NULL
    load_profile(input$hb_roster_player, roster_row = row)
  }, ignoreInit = TRUE)

  observeEvent(input$hb_scope, {
    if (!identical(input$hb_scope, "opponent") || !is.null(national_catalog())) return()
    catalog <- tryCatch(homebase_catalog(), error = function(e) {
      showNotification(paste("National player directory unavailable:", conditionMessage(e)), type = "warning")
      opponent_catalog_scope("unavailable")
      tibble::tibble()
    })
    national_catalog(catalog)
    if (!identical(opponent_catalog_scope(), "unavailable")) {
      opponent_catalog_scope(attr(catalog, "homebase_scope") %||% "national")
    }
    if (nrow(catalog)) {
      values <- paste(catalog$Team, catalog$Name, sep = "||")
      labels <- paste0(catalog$Name, " — ", homebase_team_name(catalog$Team), " · ", catalog$Roles)
      updateSelectizeInput(session, "hb_opponent_player", choices = stats::setNames(values, labels), server = TRUE)
    }
  }, ignoreInit = TRUE)

  output$hb_opponent_source_note <- renderUI({
    scope <- opponent_catalog_scope()
    if (is.null(scope)) return(NULL)
    label <- if (identical(scope, "national")) {
      paste(homebase_history_label(), "national season file · all available college players")
    } else {
      "National season file is not mounted. College player search is unavailable."
    }
    tags$p(class = "hb-picker-note", label)
  })

  observeEvent(input$hb_load_opponent, {
    req(input$hb_opponent_player, nzchar(input$hb_opponent_player))
    parts <- strsplit(input$hb_opponent_player, "||", fixed = TRUE)[[1]]
    req(length(parts) >= 2L)
    load_profile(parts[[2]], team = parts[[1]])
  })

  if (is.function(requested_player)) {
    observeEvent(requested_player(), {
      name <- requested_player()
      req(name, nzchar(name))
      updateRadioButtons(session, "hb_scope", selected = "roster")
      updateSelectizeInput(session, "hb_roster_player", selected = name)
      row <- roster[homebase_name_key(roster$Name) == homebase_name_key(name), , drop = FALSE]
      load_profile(name, roster_row = if (nrow(row)) row else NULL)
    }, ignoreInit = FALSE)
  }

  output$hb_empty_state <- renderUI({
    if (!is.null(selected_profile())) return(NULL)
    tags$section(
      class = "hb-empty",
      tags$div(class = "hb-empty-mark", "B"),
      tags$div(tags$strong("Select a player"), tags$p("Start with the active roster, or switch to college player search for anyone in the national season file."))
    )
  })

  output$hb_profile_header <- renderUI({
    profile <- selected_profile()
    if (is.null(profile)) return(NULL)
    row <- profile$roster
    number <- if (!is.null(row) && nrow(row)) paste0("#", row$Number[[1]]) else "NCAA"
    pos <- if (!is.null(row) && nrow(row)) row$Pos[[1]] else "Player"
    handedness <- if (!is.null(row) && nrow(row)) paste0("B/T: ", row$Bats[[1]], "/", row$Throws[[1]]) else NULL
    roles <- c(if (nrow(profile$hitter)) "Hitter", if (nrow(profile$pitcher)) "Pitcher")
    team_name <- homebase_team_name(profile$team)
    logo_url <- homebase_team_logo_url(profile$team)
    tags$section(
      class = "hb-player-band",
      tags$div(
        class = paste("hb-team-logo-shell", if (is.na(logo_url)) "is-fallback" else ""),
        tags$span(class = "hb-team-monogram", homebase_team_monogram(team_name), `aria-hidden` = "true"),
        if (!is.na(logo_url)) tags$img(
          class = "hb-team-logo", src = logo_url, alt = paste(team_name, "logo"),
          onerror = "this.style.display='none';this.parentElement.classList.add('is-fallback');"
        ),
        tags$span(class = "hb-player-number", number)
      ),
      tags$div(
        class = "hb-player-name",
        tags$span(paste(pos, team_name, sep = " · ")),
        tags$h2(profile$name),
        tags$div(class = "hb-player-tags", lapply(roles, function(role) tags$span(role)), if (!is.null(handedness)) tags$span(handedness))
      ),
      tags$div(
        class = "hb-sample",
        tags$span("Tracked sample"),
        tags$strong(format(nrow(profile$hitter) + nrow(profile$pitcher), big.mark = ",")),
        tags$small(paste(profile$source_label, "pitches"))
      )
    )
  })

  output$hb_overview <- renderUI({
    profile <- selected_profile()
    if (is.null(profile)) return(NULL)
    sections <- list()
    if (nrow(profile$hitter)) {
      m <- homebase_hitter_metrics(profile$hitter)
      sections <- c(sections, list(tags$section(
        class = "hb-overview-card",
        tags$div(class = "hb-section-head", tags$div(tags$span("Hitter profile"), tags$h3("Season hitting")), tags$span(class = "hb-role-chip", "BAT")),
        tags$div(
          class = "hb-metric-grid",
          homebase_metric_tile("PA", format(m$PA, big.mark = ",")),
          homebase_metric_tile("AVG", homebase_format_decimal(m$AVG)),
          homebase_metric_tile("OBP", homebase_format_decimal(m$OBP)),
          homebase_metric_tile("SLG", homebase_format_decimal(m$SLG)),
          homebase_metric_tile("K%", homebase_format_rate(m$`K%`)),
          homebase_metric_tile("BB%", homebase_format_rate(m$`BB%`)),
          homebase_metric_tile("Hard-hit", homebase_format_rate(m$`HardHit%`)),
          homebase_metric_tile("Avg EV", if (is.finite(m$`Avg EV`)) sprintf("%.1f", m$`Avg EV`) else "—")
        )
      )))
    }
    if (nrow(profile$pitcher)) {
      m <- homebase_pitcher_metrics(profile$pitcher_scored %||% profile$pitcher)
      sections <- c(sections, list(tags$section(
        class = "hb-overview-card",
        tags$div(class = "hb-section-head", tags$div(tags$span("Pitcher profile"), tags$h3("Season pitching")), tags$span(class = "hb-role-chip", "PITCH")),
        tags$div(class = "hb-pitcher-stat-board",
          homebase_pitcher_stat_panel("is-workload", "Workload", "Season volume",
            homebase_metric_tile("Games", format(m$Games, big.mark = ",")),
            homebase_metric_tile("Innings", homebase_format_innings(m$Outs)),
            homebase_metric_tile("Pitches", format(m$Pitches, big.mark = ",")),
            homebase_metric_tile("Batters faced", format(m$BF, big.mark = ",")),
            homebase_metric_tile("Hits", format(m$H, big.mark = ",")),
            homebase_metric_tile("Runs", format(m$R, big.mark = ","))
          ),
          homebase_pitcher_stat_panel("is-results", "Results", "Run prevention & outcomes",
            homebase_metric_tile("R/9", homebase_format_number(m$`R/9`, 2)),
            homebase_metric_tile("FIP", homebase_format_number(m$FIP, 2), class = "is-key"),
            homebase_metric_tile("WHIP", homebase_format_number(m$WHIP, 2), class = "is-key"),
            homebase_metric_tile("BAA", homebase_format_decimal(m$BAA)),
            homebase_metric_tile("SLG allowed", homebase_format_decimal(m$SLG)),
            homebase_metric_tile("wOBA allowed", homebase_format_decimal(m$wOBA)),
            homebase_metric_tile("K/9", homebase_format_number(m$`K/9`, 1)),
            homebase_metric_tile("BB/9", homebase_format_number(m$`BB/9`, 1)),
            homebase_metric_tile("K%", homebase_format_rate(m$`K%`)),
            homebase_metric_tile("BB%", homebase_format_rate(m$`BB%`)),
            homebase_metric_tile("K-BB%", homebase_format_rate(m$`K-BB%`), class = "is-key"),
            homebase_metric_tile("Home runs", format(m$HR, big.mark = ","))
          ),
          homebase_pitcher_stat_panel("is-execution", "Command & miss", "How pitches earn strikes",
            homebase_metric_tile("Strike%", homebase_format_rate(m$`Strike%`)),
            homebase_metric_tile("Zone%", homebase_format_rate(m$`Zone%`)),
            homebase_metric_tile("First-pitch strike", homebase_format_rate(m$`FPS%`)),
            homebase_metric_tile("CSW%", homebase_format_rate(m$`CSW%`), class = "is-key"),
            homebase_metric_tile("Whiff%", homebase_format_rate(m$`Whiff%`), class = "is-key"),
            homebase_metric_tile("Chase%", homebase_format_rate(m$`Chase%`)),
            homebase_metric_tile("Contact%", homebase_format_rate(m$`Contact%`))
          ),
          homebase_pitcher_stat_panel("is-contact", "Contact quality", "Damage allowed on contact",
            homebase_metric_tile("Barrel%", homebase_format_rate(m$`Barrel%`), class = "is-key"),
            homebase_metric_tile("Hard-hit%", homebase_format_rate(m$`HardHit%`), class = "is-key"),
            homebase_metric_tile("Ground-ball%", homebase_format_rate(m$`GB%`)),
            homebase_metric_tile("Average EV", homebase_format_number(m$`Avg EV`, 1), "mph")
          ),
          homebase_pitcher_stat_panel("is-traits", "Pitch profile", "Arsenal shape & model",
            homebase_metric_tile("Stuff+", homebase_format_number(m$`Stuff+`, 0), "CAPS BrewStuff", class = "is-model"),
            homebase_metric_tile("Average fastball", homebase_format_number(m$`Avg FB`, 1), "mph"),
            homebase_metric_tile("Max velocity", homebase_format_number(m$`Max Velo`, 1), "mph"),
            homebase_metric_tile("Extension", homebase_format_number(m$Extension, 1), "ft"),
            homebase_metric_tile("Primary pitch", m$Primary),
            homebase_metric_tile("Arsenal", m$Arsenal, "tracked pitch types")
          )
        )
      )))
    }
    if (!length(sections)) {
      sections <- list(tags$section(class = "hb-overview-card", tags$h3("Player profile"), tags$p(class = "hb-muted", "Roster identity is available, but no matching season pitch data was found yet.")))
    }
    tags$div(class = "hb-overview-stack", tagList(sections))
  })

  output$hb_recent <- renderUI({
    profile <- selected_profile()
    if (is.null(profile)) return(NULL)
    blocks <- list()
    if (nrow(profile$hitter)) blocks <- c(blocks, list(homebase_recent_ui(homebase_recent_games(profile$hitter, "hitter"), "hitter")))
    if (nrow(profile$pitcher)) blocks <- c(blocks, list(homebase_recent_ui(homebase_recent_games(profile$pitcher, "pitcher"), "pitcher")))
    if (!length(blocks)) blocks <- list(tags$section(class = "hb-recent-block", tags$div(class = "hb-section-title", "Recent games"), tags$p(class = "hb-muted", "Game performance will appear when tracked season data is available.")))
    tags$div(class = "hb-recent-stack", tagList(blocks))
  })

  output$hb_percentiles <- renderUI({
    profile <- selected_profile()
    if (is.null(profile)) return(NULL)
    cards <- list()
    if (nrow(profile$hitter)) {
      cards <- c(cards, list(homebase_percentile_card(
        homebase_percentile_rows(profile$hitter, "hitter"),
        "Hitter stats", "D1 percentiles", estimated = TRUE
      )))
    }
    if (nrow(profile$pitcher)) {
      cards <- c(cards, list(homebase_percentile_card(
        homebase_percentile_rows(profile$pitcher_scored %||% profile$pitcher, "pitcher"),
        "Pitcher statistics", "BASE models + D1 percentiles", estimated = FALSE
      )))
    }
    if (!length(cards)) return(NULL)
    tags$section(
      class = "hb-percentile-section",
      tags$div(class = "hb-studio-heading", tags$span("At a glance"), tags$h2(paste(homebase_history_label(), "percentile rankings"))),
      tags$div(class = paste("hb-percentile-grid", if (length(cards) == 1L) "is-single" else ""), tagList(cards))
    )
  })

  output$hb_card_studio <- renderUI({
    profile <- selected_profile()
    if (is.null(profile) || (!nrow(profile$hitter) && !nrow(profile$pitcher))) return(NULL)
    panes <- list()
    if (nrow(profile$hitter)) {
      panes <- c(panes, list(tags$section(
        class = "hb-card-pane",
        tags$div(class = "hb-card-toolbar", tags$div(tags$span("Wally"), tags$h3("Hitter card")), tags$div(actionButton("hb_generate_hitter", "Generate hitter card", class = "btn btn-primary"), downloadButton("hb_download_hitter", "Download PDF", class = "btn btn-outline-secondary"))),
        if (is.null(hitter_card_plot())) tags$p(class = "hb-card-placeholder", "Generate the Wally hitter card from this player's full tracked sample.") else plotOutput("hb_hitter_card", height = "850px")
      )))
    }
    if (nrow(profile$pitcher)) {
      panes <- c(panes, list(tags$section(
        class = "hb-card-pane",
        tags$div(class = "hb-card-toolbar", tags$div(tags$span("CAPS"), tags$h3("Complete pitcher card")), tags$div(actionButton("hb_generate_pitcher", "Generate full card", class = "btn btn-primary"), downloadButton("hb_download_pitcher", "Download PDF", class = "btn btn-outline-secondary"), downloadButton("hb_download_pitcher_png", "Download PNG", class = "btn btn-outline-secondary"))),
        if (is.null(pitcher_card_page())) tags$p(class = "hb-card-placeholder", "Generate the original single-page CAPS card from this player's full tracked sample.") else imageOutput("hb_pitcher_card", width = "100%", height = "auto")
      )))
    }
    tags$section(class = "hb-card-studio", tags$div(class = "hb-studio-heading", tags$span("Card studio"), tags$h2("Scouting cards")), tagList(panes))
  })

  observeEvent(input$hb_generate_hitter, {
    profile <- selected_profile()
    req(profile, nrow(profile$hitter) > 0)
    withProgress(message = "Building Wally hitter card", value = 0.2, {
      engine <- base_wally_scouting_environment()
      standardized <- engine$std_cols(profile$hitter)
      display <- paste0(profile$name, engine$batter_side_suffix(standardized))
      plot <- tryCatch(engine$build_card(standardized, display), error = function(e) {
        showNotification(paste("Hitter card could not be generated:", conditionMessage(e)), type = "error")
        NULL
      })
      hitter_card_plot(plot)
    })
  })

  observeEvent(input$hb_generate_pitcher, {
    profile <- selected_profile()
    req(profile, nrow(profile$pitcher) > 0)
    withProgress(message = "Building CAPS pitcher card", value = 0.2, {
      page <- tryCatch(
        homebase_build_caps_pitcher_card(profile$pitcher, profile$name),
        error = function(e) {
          showNotification(paste("Pitcher card could not be generated:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      pitcher_card_page(page)
    })
  })

  output$hb_hitter_card <- renderPlot({ req(hitter_card_plot()); hitter_card_plot() }, res = 100)
  output$hb_pitcher_card <- renderImage({
    req(pitcher_card_page())
    outfile <- tempfile(fileext = ".png")
    draw_card_to_png(pitcher_card_page(), outfile,
      width = 1200, height = 1200, units = "px", res = 96, dpi = 96
    )
    list(src = outfile, contentType = "image/png", width = "100%", height = "auto", alt = "Complete CAPS pitcher card")
  }, deleteFile = TRUE)

  output$hb_download_hitter <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", selected_profile()$name), "_Wally_Hitter_Card.pdf"),
    content = function(file) {
      req(hitter_card_plot())
      engine <- base_wally_scouting_environment()
      result <- engine$save_card_pdf(file, hitter_card_plot(), engine$LETTER_W, engine$LETTER_H)
      if (!isTRUE(result$ok)) stop(result$err %||% "CAPS hitter card export failed.")
    }
  )

  output$hb_download_pitcher <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", selected_profile()$name), "_CAPS_Pitcher_Card.pdf"),
    content = function(file) {
      req(pitcher_card_page())
      draw_cards_to_pdf(list(pitcher_card_page()), file, width = 12.5, height = 12.5, dpi = 300)
    }
  )

  output$hb_download_pitcher_png <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", selected_profile()$name), "_CAPS_Pitcher_Card.png"),
    content = function(file) {
      req(pitcher_card_page())
      draw_card_to_png(pitcher_card_page(), file,
        width = 12.5, height = 12.5, units = "in", res = 300, dpi = 300
      )
    }
  )
}
