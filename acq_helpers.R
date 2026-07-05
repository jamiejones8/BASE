

library(rvest)

# ── MLB API helpers ───────────────────────────────────────────────────────────

safe1_mlb <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x[1])
}

fetch_player_bio_mlb <- function(player_id) {
  resp <- httr::GET(
    paste0("https://statsapi.mlb.com/api/v1/people/", player_id),
    query = list(hydrate = "education"),
    httr::add_headers("User-Agent" = "Mozilla/5.0"),
    httr::timeout(10)
  )
  if (httr::status_code(resp) != 200) return(NULL)
  raw <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  p <- raw$people
  if (is.null(p) || nrow(p) == 0) return(NULL)
  college_name  <- safe1_mlb(tryCatch(p$`education.colleges`[[1]]$name[1],      error = function(e) NULL))
  college_state <- safe1_mlb(tryCatch(p$`education.colleges`[[1]]$stateProv[1], error = function(e) NULL))
  hs_name       <- safe1_mlb(tryCatch(p$`education.highschools`[[1]]$name[1],   error = function(e) NULL))
  tibble::tibble(
    player_id     = as.integer(player_id),
    full_name     = as.character(p$fullName[1]),
    first_name    = as.character(p$firstName[1]),
    last_name     = as.character(p$lastName[1]),
    birth_date    = as.character(p$birthDate[1]),
    age           = as.integer(p$currentAge[1]),
    birth_city    = as.character(p$birthCity[1]),
    birth_country = as.character(p$birthCountry[1]),
    height        = as.character(p$height[1]),
    weight        = as.character(p$weight[1]),
    throws        = as.character(p$pitchHand.code[1]),
    bats          = as.character(p$batSide.code[1]),
    position      = as.character(p$primaryPosition.abbreviation[1]),
    college       = college_name,
    college_state = college_state,
    hs_name       = hs_name
  )
}

flatten_game_pbp_mlb <- function(gamePk) {
  resp <- httr::GET(
    paste0("https://statsapi.mlb.com/api/v1/game/", gamePk, "/playByPlay"),
    httr::add_headers("User-Agent" = "Mozilla/5.0")
  )
  raw   <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  plays <- raw$allPlays
  if (is.null(plays) || nrow(plays) == 0) return(NULL)
  purrr::map_dfr(seq_len(nrow(plays)), function(i) {
    events <- plays$playEvents[[i]]
    if (is.null(events) || nrow(events) == 0) return(NULL)
    events %>%
      dplyr::mutate(
        gamePk            = gamePk,
        atBatIndex        = plays$atBatIndex[i],
        inning            = plays$about.inning[i],
        halfInning        = plays$about.halfInning[i],
        batter_id         = plays$matchup.batter.id[i],
        batter_name       = plays$matchup.batter.fullName[i],
        bat_side          = plays$matchup.batSide.code[i],
        pitcher_id        = plays$matchup.pitcher.id[i],
        pitcher_name      = plays$matchup.pitcher.fullName[i],
        pitch_hand        = plays$matchup.pitchHand.code[i],
        result_event      = plays$result.event[i],
        result_event_type = plays$result.eventType[i],
        result_rbi        = plays$result.rbi[i],
        is_out            = plays$result.isOut[i],
        is_scoring_play   = plays$about.isScoringPlay[i],
        end_count_balls   = plays$count.balls[i],
        end_count_strikes = plays$count.strikes[i],
        end_count_outs    = plays$count.outs[i],
        men_on_base       = plays$matchup.splits.menOnBase[i]
      )
  })
}

fetch_schedule_mlb <- function(league_id,
                               start = "2026-05-01",
                               end   = format(Sys.Date(), "%Y-%m-%d")) {
  resp <- httr::GET(
    "https://statsapi.mlb.com/api/v1/schedule",
    query = list(leagueId = league_id, season = 2026, gameType = "R",
                 startDate = start, endDate = end, sportId = 22),
    httr::add_headers("User-Agent" = "Mozilla/5.0")
  )
  raw <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  if (is.null(raw$dates) || length(raw$dates) == 0) return(tibble::tibble())
  dplyr::bind_rows(raw$dates$games) %>%
    dplyr::filter(status.detailedState %in% c("Final", "Completed Early",
                                               "Game Over", "Postponed"))
}

fetch_pitching_mlb <- function(league_id, season = 2026) {
  resp <- httr::GET(
    "https://statsapi.mlb.com/api/v1/stats",
    query = list(stats = "season", leagueId = league_id, season = season,
                 group = "pitching", gameType = "R", playerPool = "ALL", limit = 500),
    httr::add_headers("User-Agent" = "Mozilla/5.0")
  )
  httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    .$stats %>% .$splits %>% .[[1]] %>%
    dplyr::mutate(league_id = league_id)
}

fetch_hitting_mlb <- function(league_id, season = 2026) {
  resp <- httr::GET(
    "https://statsapi.mlb.com/api/v1/stats",
    query = list(stats = "season", leagueId = league_id, season = season,
                 group = "hitting", gameType = "R", playerPool = "ALL", limit = 500),
    httr::add_headers("User-Agent" = "Mozilla/5.0")
  )
  httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    .$stats %>% .$splits %>% .[[1]] %>%
    dplyr::mutate(league_id = league_id)
}

clean_pitching_mlb <- function(df) {
  df %>%
    dplyr::select(
      player_id = player.id, player_name = player.fullName,
      team_name = team.name, league_name = league.name, league_id,
      position = position.abbreviation,
      G = stat.gamesPlayed, GS = stat.gamesStarted,
      IP = stat.inningsPitched, ERA = stat.era, WHIP = stat.whip,
      K9 = stat.strikeoutsPer9Inn, BB9 = stat.walksPer9Inn,
      KBB = stat.strikeoutWalkRatio, HR9 = stat.homeRunsPer9,
      K = stat.strikeOuts, BB = stat.baseOnBalls,
      H = stat.hits, HR = stat.homeRuns, ER = stat.earnedRuns,
      HBP = stat.hitBatsmen, BF = stat.battersFaced
    ) %>%
    dplyr::mutate(
      dplyr::across(c(G, GS, K, BB, H, HR, ER, HBP, BF), as.integer),
      dplyr::across(c(ERA, WHIP, K9, BB9, KBB, HR9), as.numeric),
      IP = as.numeric(IP)
    )
}

clean_hitting_mlb <- function(df) {
  df %>%
    dplyr::select(
      player_id = player.id, player_name = player.fullName,
      team_name = team.name, league_name = league.name, league_id,
      position = position.abbreviation,
      G = stat.gamesPlayed, AB = stat.atBats, H = stat.hits,
      R = stat.runs, `2B` = stat.doubles, `3B` = stat.triples,
      HR = stat.homeRuns, RBI = stat.rbi, BB = stat.baseOnBalls,
      SO = stat.strikeOuts, HBP = stat.hitByPitch, SF = stat.sacFlies,
      SB = stat.stolenBases, CS = stat.caughtStealing,
      TB = stat.totalBases, AVG = stat.avg, OBP = stat.obp,
      SLG = stat.slg, OPS = stat.ops
    ) %>%
    dplyr::mutate(
      dplyr::across(c(G, AB, H, R, `2B`, `3B`, HR, RBI, BB,
                      SO, HBP, SF, SB, CS, TB), as.integer),
      dplyr::across(c(AVG, OBP, SLG, OPS), as.numeric)
    )
}

build_pitch_level_mlb <- function(pbp) {
  pl <- pbp %>%
    dplyr::filter(isPitch == TRUE, !is.na(pitcher_id)) %>%
    dplyr::mutate(
      pitch_type = details.type.code,
      velo       = as.numeric(pitchData.startSpeed),
      ivb        = as.numeric(pitchData.breaks.breakVerticalInduced),
      hb_pov     = as.numeric(pitchData.breaks.breakHorizontal),
      rel_height = as.numeric(pitchData.coordinates.z0),
      rel_side   = as.numeric(pitchData.coordinates.x0),
      extension  = as.numeric(pitchData.extension),
      is_strike  = details.isStrike == TRUE | details.isInPlay == TRUE,
      is_swing   = details.call.description %in% c(
                     "Swinging Strike", "Foul", "In Play, Out(s)",
                     "In Play, Run(s)", "In Play, No Out",
                     "Swinging Strike (Blocked)", "Foul Tip"),
      is_whiff   = details.call.description %in% c(
                     "Swinging Strike", "Swinging Strike (Blocked)")
    )

  first_pitches <- pl %>%
    dplyr::group_by(gamePk, atBatIndex, pitcher_id) %>%
    dplyr::slice_min(pitchNumber, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(gamePk, atBatIndex, pitcher_id) %>%
    dplyr::mutate(is_fps = TRUE)

  pl %>%
    dplyr::left_join(first_pitches, by = c("gamePk", "atBatIndex", "pitcher_id")) %>%
    dplyr::mutate(
      is_fps        = dplyr::coalesce(is_fps, FALSE),
      is_fps_strike = is_fps & is_strike
    )
}

build_pa_results_mlb <- function(pbp) {
  pbp %>%
    dplyr::filter(!is.na(pitcher_id)) %>%
    dplyr::group_by(gamePk, atBatIndex, pitcher_id, pitcher_name, pitch_hand) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(gamePk, atBatIndex, pitcher_id, pitcher_name, pitch_hand,
                  result_event_type, result_rbi, is_out)
}

build_pitcher_season_mlb <- function(pa_results, pitcher_teams, player_bios) {
  outs_df <- pa_results %>%
    dplyr::group_by(pitcher_id) %>%
    dplyr::summarise(total_outs = sum(as.integer(is_out), na.rm = TRUE),
                     .groups = "drop")

  pa_results %>%
    dplyr::group_by(pitcher_id, pitcher_name, pitch_hand) %>%
    dplyr::summarise(
      G   = dplyr::n_distinct(gamePk),
      K   = sum(result_event_type == "strikeout",    na.rm = TRUE),
      BB  = sum(result_event_type == "walk",         na.rm = TRUE),
      HBP = sum(result_event_type == "hit_by_pitch", na.rm = TRUE),
      H   = sum(result_event_type %in% c("single","double","triple","home_run"),
                na.rm = TRUE),
      HR  = sum(result_event_type == "home_run",     na.rm = TRUE),
      ER  = sum(result_rbi,                          na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(outs_df, by = "pitcher_id") %>%
    dplyr::mutate(
      IP_dec = total_outs / 3,
      IP     = floor(IP_dec) + (total_outs %% 3) / 10,
      ERA    = round(ifelse(IP_dec > 0, (ER  / IP_dec) * 9,  NA), 2),
      WHIP   = round(ifelse(IP_dec > 0, (BB + H) / IP_dec,   NA), 2),
      FIP    = round(ifelse(IP_dec > 0,
                            ((13*HR + 3*(BB+HBP) - 2*K) / IP_dec) + 3.10, NA), 2),
      K9     = round(ifelse(IP_dec > 0, (K  / IP_dec) * 9,   NA), 1),
      BB9    = round(ifelse(IP_dec > 0, (BB / IP_dec) * 9,   NA), 1),
      KBB    = round(ifelse(BB > 0,      K / BB,             NA), 2),
      HR9    = round(ifelse(IP_dec > 0, (HR / IP_dec) * 9,   NA), 1)
    ) %>%
    dplyr::left_join(
      pitcher_teams %>%
        dplyr::select(pitcher_id, team_name, league_name) %>%
        dplyr::distinct(pitcher_id, .keep_all = TRUE),
      by = "pitcher_id"
    ) %>%
    dplyr::left_join(
      player_bios %>%
        dplyr::transmute(pitcher_id = as.integer(player_id), age, college) %>%
        dplyr::distinct(pitcher_id, .keep_all = TRUE),
      by = "pitcher_id"
    )
}

build_pitch_metrics_mlb <- function(pitch_level) {
  fps <- pitch_level %>%
    dplyr::filter(is_fps == TRUE, !is.na(pitch_type), pitch_type != "") %>%
    dplyr::group_by(pitcher_id, pitch_type) %>%
    dplyr::summarise(
      FPS_pct = round(sum(is_strike, na.rm = TRUE) / dplyr::n() * 100, 1),
      .groups = "drop"
    )

  pitch_level %>%
    dplyr::filter(!is.na(pitch_type), pitch_type != "") %>%
    dplyr::group_by(pitcher_id, pitcher_name, pitch_type) %>%
    dplyr::summarise(
      N          = dplyr::n(),
      Velo       = round(mean(velo,       na.rm = TRUE), 1),
      iVB        = round(mean(ivb,        na.rm = TRUE), 1),
      HB         = round(mean(hb_pov,     na.rm = TRUE), 1),
      Strike_pct = round(mean(is_strike,  na.rm = TRUE) * 100, 1),
      Whiff_pct  = round(sum(is_whiff,    na.rm = TRUE) /
                         max(sum(is_swing), 1) * 100, 1),
      Rel_Ht     = round(mean(rel_height, na.rm = TRUE), 2),
      Rel_Side   = round(mean(rel_side,   na.rm = TRUE), 2),
      Ext        = round(mean(extension,  na.rm = TRUE), 2),
      .groups    = "drop"
    ) %>%
    dplyr::left_join(fps, by = c("pitcher_id", "pitch_type"))
}

build_movement_avg_mlb <- function(pitch_level) {
  pitch_level %>%
    dplyr::filter(!is.na(hb_pov), !is.na(ivb),
                  !is.na(pitch_type), pitch_type != "") %>%
    dplyr::group_by(pitcher_id, pitch_type) %>%
    dplyr::filter(dplyr::n() >= 5) %>%
    dplyr::summarise(
      pfx_x = mean(hb_pov, na.rm = TRUE),
      pfx_z = mean(ivb,    na.rm = TRUE),
      velo  = round(mean(velo, na.rm = TRUE), 1),
      N     = dplyr::n(),
      .groups = "drop"
    )
}

build_pitcher_teams_mlb <- function(pitch_level, all_games) {
  pitch_level %>%
    dplyr::filter(!is.na(pitcher_id)) %>%
    dplyr::distinct(gamePk, pitcher_id, pitcher_name, halfInning) %>%
    dplyr::left_join(
      all_games %>%
        dplyr::select(gamePk,
                      home_team     = teams.home.team.name,
                      away_team     = teams.away.team.name,
                      source_league),
      by = "gamePk"
    ) %>%
    dplyr::mutate(team_name = ifelse(halfInning == "top", away_team, home_team)) %>%
    dplyr::group_by(pitcher_id, pitcher_name) %>%
    dplyr::summarise(
      team_name   = names(sort(table(team_name),     decreasing = TRUE))[1],
      league_name = names(sort(table(source_league), decreasing = TRUE))[1],
      .groups     = "drop"
    ) %>%
    dplyr::distinct(pitcher_id, .keep_all = TRUE)
}

# ── NECBL scraper helpers ──────────────────────────────────────────────────────
# Recovered from update_necbl.R (previously a standalone local script, run
# manually to regenerate necbl_pitching.parquet / necbl_hitting.parquet).
# Wired in here so the "Run updates" button can call it live.

clean_val <- function(x) {
  x <- as.character(x)
  x[stringr::str_detect(x, "^-$|^'?-'?$")] <- NA_character_
  x
}

clean_name <- function(x) {
  stringr::str_trim(stringr::str_replace_all(x, "[\r\n\\s]+", " ")) %>%
    stringr::str_replace("^[A-Z]\\s+", "") %>%
    stringr::str_trim()
}

normalize_team <- function(x) {
  stringr::str_to_lower(stringr::str_trim(x)) %>%
    stringr::str_replace_all("[^a-z]", "")
}

fetch_necbl_hitting <- function() {
  resp <- httr::GET(
    "https://necbl.com/sports/bsb/2026/players?pos=h&sort=avg",
    httr::add_headers(
      "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.5",
      "Referer"         = "https://necbl.com/sports/bsb/2026/players"
    )
  )

  page <- httr::content(resp, as = "text", encoding = "UTF-8") %>% rvest::read_html()

  page %>%
    rvest::html_nodes("table") %>% .[[1]] %>%
    rvest::html_table(fill = TRUE, convert = FALSE) %>%
    dplyr::rename(player_name = Name) %>%
    dplyr::mutate(
      league_name = "NECBL",
      player_name = clean_name(player_name),
      dplyr::across(c(gp, ab, h, rbi, bb, `2b`, `3b`, hr, xbh,
                      k, hbp, sf, sh, hdp, go, fo, pa),
                    ~ suppressWarnings(as.integer(clean_val(.)))),
      dplyr::across(c(avg, obp, slg),
                    ~ suppressWarnings(as.numeric(clean_val(.))))
    ) %>%
    dplyr::filter(!is.na(player_name), player_name != "",
                  player_name != "Name", !is.na(ab), ab > 0)
}

read_necbl_pitching <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    message("NECBL pitching CSV not found: ", path)
    return(tibble::tibble())
  }

  raw <- readr::read_csv(path, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))

  names(raw) <- stringr::str_to_lower(names(raw)) %>%
    stringr::str_replace_all("[^a-z0-9]", "_")
  names(raw)[names(raw) == "k_9"] <- "k9"

  raw %>%
    dplyr::rename(player_name = name) %>%
    dplyr::mutate(
      league_name = "NECBL",
      player_name = clean_name(player_name),
      dplyr::across(dplyr::any_of(c("w","l","app","gs","sv","h","r","er",
                                     "bb","k","hr","bf","wp","hbp")),
                    ~ suppressWarnings(as.integer(clean_val(.)))),
      dplyr::across(dplyr::any_of(c("era","ip","k9","whip")),
                    ~ suppressWarnings(as.numeric(clean_val(.))))
    ) %>%
    dplyr::filter(!is.na(player_name), player_name != "",
                  player_name != "name", !is.na(app))
}

fetch_necbl_team_slugs <- function() {
  page <- httr::content(
    httr::GET("https://necbl.com/sports/bsb/2026/teams",
              httr::add_headers("User-Agent" = "Mozilla/5.0")),
    as = "text", encoding = "UTF-8"
  ) %>% rvest::read_html()

  page %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href") %>%
    .[stringr::str_detect(., "/sports/bsb/2026/teams/[^?]+$")] %>%
    unique() %>% stringr::str_remove("^/")
}

fetch_necbl_roster <- function(team_slug) {
  resp <- httr::GET(
    paste0("https://necbl.com/", team_slug),
    query = list(view = "roster"),
    httr::add_headers("User-Agent" = "Mozilla/5.0"),
    httr::timeout(10)
  )
  if (httr::status_code(resp) != 200) return(NULL)

  page <- httr::content(resp, as = "text", encoding = "UTF-8") %>% rvest::read_html()

  table <- tryCatch(
    page %>% rvest::html_nodes("table") %>% .[[2]] %>% rvest::html_table(fill = TRUE),
    error = function(e) NULL
  )
  if (is.null(table) || nrow(table) == 0) return(NULL)

  team_name <- team_slug %>% stringr::str_extract("[^/]+$") %>% stringr::str_to_title()

  table %>%
    dplyr::mutate(
      team_name   = team_name,
      league_name = "NECBL",
      school      = stringr::str_extract(Hometown, "(?<=/ ).*$") %>% stringr::str_trim(),
      hometown    = stringr::str_extract(Hometown, "^[^/]+") %>% stringr::str_trim(),
      dplyr::across(dplyr::everything(), as.character)
    ) %>%
    dplyr::select(
      number = `#`, name = Name, position = Position,
      year = Year, status = Status, height = Height,
      weight = Weight, bats = Bats, throws = Throws,
      dob = DOB, hometown, school, team_name, league_name
    )
}

# ── NWL scraper helpers ─────────────────────────────────────────────────────────

NWL_SEASON_ID <- 26

fetch_nwl_hitting <- function(season_id = NWL_SEASON_ID) {
  resp <- httr::GET(
    paste0("https://scorebook.northwoodsleague.com/api/statistics/batting/", season_id),
    httr::add_headers("User-Agent" = "Mozilla/5.0",
                       "Referer"    = "https://northwoodsleague.com/"),
    httr::timeout(15)
  )
  raw   <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  stats <- raw$statistics$types$batting$stats
  tibble::as_tibble(stats) %>%
    dplyr::mutate(league_name = "Northwoods League", league_id = season_id)
}

fetch_nwl_pitching <- function(season_id = NWL_SEASON_ID) {
  resp <- httr::GET(
    paste0("https://scorebook.northwoodsleague.com/api/statistics/pitching/", season_id),
    httr::add_headers("User-Agent" = "Mozilla/5.0",
                       "Referer"    = "https://northwoodsleague.com/"),
    httr::timeout(15)
  )
  raw   <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  stats <- raw$statistics$types$pitching$stats
  tibble::as_tibble(stats) %>%
    dplyr::mutate(league_name = "Northwoods League", league_id = season_id)
}

fetch_nwl_bio <- function(player_id) {
  resp <- httr::GET(
    paste0("https://scorebook.northwoodsleague.com/api/player/", player_id),
    httr::add_headers("User-Agent" = "Mozilla/5.0",
                       "Referer"    = "https://northwoodsleague.com/"),
    httr::timeout(10)
  )
  if (httr::status_code(resp) != 200) return(NULL)
  p <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>% .$player
  if (is.null(p)) return(NULL)
  tibble::tibble(
    player_id   = as.integer(player_id),
    full_name   = paste(p$firstname, p$lastname),
    first_name  = as.character(p$firstname),
    last_name   = as.character(p$lastname),
    position    = as.character(p$position),
    bats_throws = as.character(p$bats_throws),
    height      = as.character(p$height),
    weight      = as.integer(p$weight),
    college     = as.character(p$college),
    class       = as.character(p$class),
    hometown    = as.character(p$hometown)
  )
}

# ── Run update functions ──────────────────────────────────────────────────────

run_mlb_update <- function() {

  existing_pitch_level <- acq_pitch_level
  existing_pa_results  <- tryCatch(
    read_parquet("pa_results.parquet"),
    error = function(e) tibble::tibble()
  )
  existing_player_bios <- acq_player_bios

  dl_games  <- fetch_schedule_mlb(5536)
  app_games <- fetch_schedule_mlb(120)

  all_games <- dplyr::bind_rows(
    dl_games  %>% dplyr::mutate(source_league = "MLB Draft League"),
    app_games %>% dplyr::mutate(source_league = "Appalachian League")
  )

  existing_game_pks <- existing_pitch_level %>%
    dplyr::distinct(gamePk) %>% dplyr::pull(gamePk)
  new_game_pks <- setdiff(all_games$gamePk, existing_game_pks)
  message("New games to process: ", length(new_game_pks))

  if (length(new_game_pks) > 0) {
    new_pbp <- purrr::map_dfr(new_game_pks, function(pk) {
      Sys.sleep(0.3)
      tryCatch(flatten_game_pbp_mlb(pk), error = function(e) {
        message("Failed PBP: ", pk); NULL
      })
    })

    if (nrow(new_pbp) > 0) {
      new_pitch_level <- build_pitch_level_mlb(new_pbp)
      new_pa_results  <- build_pa_results_mlb(new_pbp)
      pitch_level     <- dplyr::bind_rows(existing_pitch_level, new_pitch_level)
      pa_results      <- dplyr::bind_rows(existing_pa_results,  new_pa_results)

      new_player_ids <- new_pbp %>%
        dplyr::filter(isPitch == TRUE) %>%
        dplyr::select(player_id = pitcher_id) %>%
        dplyr::bind_rows(new_pbp %>% dplyr::select(player_id = batter_id)) %>%
        dplyr::filter(!is.na(player_id)) %>%
        dplyr::distinct(player_id) %>%
        dplyr::filter(!player_id %in% existing_player_bios$player_id)

      if (nrow(new_player_ids) > 0) {
        message("Fetching ", nrow(new_player_ids), " new bios...")
        new_bios <- purrr::map_dfr(new_player_ids$player_id, function(id) {
          Sys.sleep(0.3)
          tryCatch(fetch_player_bio_mlb(id), error = function(e) {
            message("Failed bio: ", id); NULL
          })
        })
        player_bios <- dplyr::bind_rows(existing_player_bios, new_bios) %>%
          dplyr::distinct(player_id, .keep_all = TRUE)
      } else {
        player_bios <- existing_player_bios
      }
    } else {
      message("No PBP data returned.")
      pitch_level <- existing_pitch_level
      pa_results  <- existing_pa_results
      player_bios <- existing_player_bios
    }
  } else {
    message("No new games.")
    pitch_level <- existing_pitch_level
    pa_results  <- existing_pa_results
    player_bios <- existing_player_bios
  }

  message("Fetching season stats...")
  pitching_all <- dplyr::bind_rows(
    clean_pitching_mlb(fetch_pitching_mlb(5536)),
    clean_pitching_mlb(fetch_pitching_mlb(120))
  )
  hitting_all <- dplyr::bind_rows(
    clean_hitting_mlb(fetch_hitting_mlb(5536)),
    clean_hitting_mlb(fetch_hitting_mlb(120))
  )

  season_player_ids <- dplyr::bind_rows(
    pitching_all %>% dplyr::select(player_id),
    hitting_all  %>% dplyr::select(player_id)
  ) %>%
    dplyr::distinct(player_id) %>%
    dplyr::filter(!is.na(player_id), !player_id %in% player_bios$player_id)

  if (nrow(season_player_ids) > 0) {
    message("Fetching ", nrow(season_player_ids), " season stat bios...")
    new_season_bios <- purrr::map_dfr(season_player_ids$player_id, function(id) {
      Sys.sleep(0.3)
      tryCatch(fetch_player_bio_mlb(id), error = function(e) {
        message("Failed bio: ", id); NULL
      })
    })
    player_bios <- dplyr::bind_rows(player_bios, new_season_bios) %>%
      dplyr::distinct(player_id, .keep_all = TRUE)
  }

  bio_join <- player_bios %>%
    dplyr::select(player_id, age, college, college_state, hs_name,
                  height, weight, throws, bats, birth_country)

  pitching_all <- pitching_all %>% dplyr::left_join(bio_join, by = "player_id")
  hitting_all  <- hitting_all  %>% dplyr::left_join(bio_join, by = "player_id")

  message("Building derived tables...")
  pitcher_teams  <- build_pitcher_teams_mlb(pitch_level, all_games)
  pitcher_season <- build_pitcher_season_mlb(pa_results, pitcher_teams, player_bios)
  pitch_metrics  <- build_pitch_metrics_mlb(pitch_level)
  movement_avg   <- build_movement_avg_mlb(pitch_level)

  write_parquet(pitch_level,    "pitch_level.parquet")
  write_parquet(pa_results,     "pa_results.parquet")
  write_parquet(pitcher_season, "pitcher_season.parquet")
  write_parquet(pitch_metrics,  "pitch_metrics.parquet")
  write_parquet(movement_avg,   "movement_avg.parquet")
  write_parquet(pitching_all,   "pitching_all.parquet")
  write_parquet(hitting_all,    "hitting_all.parquet")
  write_parquet(player_bios,    "player_bios.parquet")

  message("Pushing to HF dataset repo...")
  for (f in c("pitch_level.parquet", "pa_results.parquet",
              "pitcher_season.parquet", "pitch_metrics.parquet",
              "movement_avg.parquet", "pitching_all.parquet",
              "hitting_all.parquet", "player_bios.parquet")) {
    push_file_to_hf(f, f, paste("MLB update —", format(Sys.time())))
  }

  acq_pitch_level    <<- pitch_level
  acq_pitcher_season <<- pitcher_season
  acq_pitch_metrics  <<- pitch_metrics
  acq_movement_avg   <<- movement_avg
  acq_pitching_all   <<- pitching_all
  acq_hitting_all    <<- hitting_all
  acq_player_bios    <<- player_bios

  message("MLB update complete.")
}

run_necbl_update <- function(necbl_csv_path = NULL) {

  message("Fetching NECBL hitting...")
  necbl_hitting <- fetch_necbl_hitting()

  message("Reading NECBL pitching...")
  necbl_pitching <- if (!is.null(necbl_csv_path) && file.exists(necbl_csv_path)) {
    read_necbl_pitching(necbl_csv_path)
  } else {
    message("No NECBL CSV uploaded — keeping existing pitching data.")
    acq_necbl_pitching
  }

  message("Fetching NECBL rosters...")
  team_slugs    <- fetch_necbl_team_slugs()
  necbl_rosters <- purrr::map_dfr(team_slugs, function(slug) {
    Sys.sleep(0.5)
    tryCatch(fetch_necbl_roster(slug), error = function(e) {
      message("Failed roster: ", slug); NULL
    })
  })
  message("Roster rows: ", nrow(necbl_rosters))

  necbl_rosters_clean <- necbl_rosters %>%
    dplyr::filter(!is.na(name), name != "") %>%
    dplyr::mutate(
      last_name = stringr::str_to_lower(stringr::str_trim(
                    stringr::str_replace(name, "^\\S+\\s+", ""))),
      team_norm = normalize_team(team_name)
    )

  roster_join <- necbl_rosters_clean %>%
    dplyr::select(last_name, team_norm, full_name = name, position,
                  year, school, hometown, bats, throws, height, weight,
                  roster_team = team_name)

  necbl_pitching_full <- necbl_pitching %>%
    dplyr::mutate(
      last_name = stringr::str_to_lower(stringr::str_trim(player_name)),
      team_norm = normalize_team(team)
    ) %>%
    dplyr::left_join(roster_join, by = c("last_name", "team_norm"),
                     relationship = "many-to-many") %>%
    dplyr::group_by(player_name, team) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  necbl_hitting_full <- necbl_hitting %>%
    dplyr::mutate(
      last_name = stringr::str_to_lower(stringr::str_trim(player_name)),
      team_norm = normalize_team(Team)
    ) %>%
    dplyr::left_join(roster_join, by = c("last_name", "team_norm"),
                     relationship = "many-to-many") %>%
    dplyr::group_by(player_name, Team) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  message("Pitching matched: ", sum(!is.na(necbl_pitching_full$school)),
          " / ", nrow(necbl_pitching_full))
  message("Hitting matched:  ", sum(!is.na(necbl_hitting_full$school)),
          " / ", nrow(necbl_hitting_full))

  write_parquet(necbl_pitching_full, "necbl_pitching.parquet")
  write_parquet(necbl_hitting_full,  "necbl_hitting.parquet")

  push_file_to_hf("necbl_pitching.parquet", "necbl_pitching.parquet",
                  paste("NECBL update —", format(Sys.time())))
  push_file_to_hf("necbl_hitting.parquet",  "necbl_hitting.parquet",
                  paste("NECBL update —", format(Sys.time())))

  acq_necbl_pitching <<- necbl_pitching_full
  acq_necbl_hitting  <<- necbl_hitting_full

  message("NECBL update complete.")
}

run_nwl_update <- function() {

  message("Fetching NWL stats...")
  nwl_hitting  <- fetch_nwl_hitting()
  nwl_pitching <- fetch_nwl_pitching()

  message("NWL hitters: ",  nrow(nwl_hitting))
  message("NWL pitchers: ", nrow(nwl_pitching))

  bio_cache_local <- "nwl_bios.parquet"
  pull_file_from_hf("nwl_bios.parquet", bio_cache_local)

  existing_nwl_bios <- tryCatch(
    read_parquet(bio_cache_local),
    error = function(e) { message("No existing NWL bios cache."); tibble::tibble() }
  )

  all_nwl_ids <- dplyr::bind_rows(
    nwl_hitting  %>% dplyr::select(player_id),
    nwl_pitching %>% dplyr::select(player_id)
  ) %>%
    dplyr::distinct(player_id) %>%
    dplyr::filter(!is.na(player_id))

  new_nwl_ids <- all_nwl_ids %>%
    dplyr::filter(!player_id %in% existing_nwl_bios$player_id)

  message("New NWL player bios to fetch: ", nrow(new_nwl_ids))

  if (nrow(new_nwl_ids) > 0) {
    new_nwl_bios <- purrr::map_dfr(new_nwl_ids$player_id, function(id) {
      Sys.sleep(0.2)
      tryCatch(fetch_nwl_bio(id), error = function(e) {
        message("Failed NWL bio: ", id); NULL
      })
    })
    nwl_bios <- dplyr::bind_rows(existing_nwl_bios, new_nwl_bios) %>%
      dplyr::distinct(player_id, .keep_all = TRUE)
  } else {
    nwl_bios <- existing_nwl_bios
  }

  bio_cols <- nwl_bios %>%
    dplyr::select(player_id, college, class, hometown, height, weight, bats_throws)

  nwl_hitting_full  <- nwl_hitting  %>% dplyr::left_join(bio_cols, by = "player_id")
  nwl_pitching_full <- nwl_pitching %>% dplyr::left_join(bio_cols, by = "player_id")

  write_parquet(nwl_bios,          "nwl_bios.parquet")
  write_parquet(nwl_hitting_full,  "nwl_hitting.parquet")
  write_parquet(nwl_pitching_full, "nwl_pitching.parquet")

  push_file_to_hf("nwl_bios.parquet",     "nwl_bios.parquet",
                  paste("NWL update —", format(Sys.time())))
  push_file_to_hf("nwl_hitting.parquet",  "nwl_hitting.parquet",
                  paste("NWL update —", format(Sys.time())))
  push_file_to_hf("nwl_pitching.parquet", "nwl_pitching.parquet",
                  paste("NWL update —", format(Sys.time())))

  acq_nwl_hitting  <<- nwl_hitting_full
  acq_nwl_pitching <<- nwl_pitching_full

  message("NWL update complete.")
}