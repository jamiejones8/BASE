library(shiny)
library(htmltools)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(grid)
library(magick)
library(readr)
library(workflows)
library(parsnip)
library(recipes)
library(tune)
library(xgboost)
library(base64enc)
library(ggridges)
library(arrow)
library(shinyBS)
library(shinyjs)
library(DT)
library(glue)
library(stringr)
library(tibble)
library(reactable)

source("team_config.R", local = FALSE)
source("data_access.R", local = FALSE)
source("defense_data_access.R", local = FALSE)
source("defense_attribution.R", local = FALSE)
source("pitch_retags.R", local = FALSE)

BASE_RETAG_STORAGE_STATUS <- base_retag_storage_status()
message(
  if (isTRUE(BASE_RETAG_STORAGE_STATUS$ok)) {
    paste("Persistent pitch retag storage ready at", BASE_RETAG_STORAGE_STATUS$path)
  } else {
    paste("Persistent pitch retag storage unavailable:", BASE_RETAG_STORAGE_STATUS$message)
  }
)

combine  <- dplyr::combine
slice    <- dplyr::slice
between  <- dplyr::between
first    <- dplyr::first
last     <- dplyr::last

source("scout_app.R")
source("leaderboards_embed.R")
source("cape_pitcher_page.R")
source("hitter_scouting_page.R")
source("defense_page.R")
source("Pitcher_Card.R")
# ══════════════════════════════════════════════════════════════════════════════
# HF HUB WRITE-BACK HELPER — now points at a Dataset repo, not the Space repo
# Dataset repos don't trigger Space rebuilds on commit, so ineligible list
# changes no longer restart the app.
# ══════════════════════════════════════════════════════════════════════════════

HF_DATA_REPO_ID   <- TEAM_CONFIG$data$hf_repo_id
HF_DATA_REPO_TYPE <- "dataset"
SEASON_DATA_FILE      <- TEAM_CONFIG$data$season_file
SEASON_DATA_REPO_ID   <- base_env("BASE_SEASON_DATA_REPO_ID",
                                  HF_DATA_REPO_ID)
SEASON_DATA_REPO_PATH <- base_env("BASE_SEASON_DATA_REPO_PATH",
                                  TEAM_CONFIG$data$hf_repo_path)

push_file_to_hf <- function(local_path, repo_path,
                            commit_message = paste("Update", repo_path),
                            repo_id = HF_DATA_REPO_ID) {

  if (!nzchar(repo_id)) {
    message("HF dataset repository is not configured — skipping push for ", repo_path)
    return(invisible(FALSE))
  }

  token <- Sys.getenv("write_token")
  if (!nzchar(token)) {
    message("HF write token not found — skipping push for ", repo_path)
    return(invisible(FALSE))
  }

  if (!file.exists(local_path)) {
    message("Local file not found, cannot push: ", local_path)
    return(invisible(FALSE))
  }

  file_content <- readBin(local_path, "raw", file.info(local_path)$size)
  encoded      <- base64enc::base64encode(file_content)

  url <- glue::glue(
    "https://huggingface.co/api/datasets/{repo_id}/commit/main"
  )

  body <- list(
    summary = commit_message,
    files = list(
      list(
        path     = repo_path,
        content  = encoded,
        encoding = "base64"
      )
    )
  )

  resp <- httr::POST(
    url,
    httr::add_headers(
      Authorization  = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(resp) >= 200 && httr::status_code(resp) < 300) {
    message("Pushed to HF dataset: ", repo_path)
    return(invisible(TRUE))
  } else {
    message("HF push failed (", httr::status_code(resp), "): ",
            httr::content(resp, as = "text", encoding = "UTF-8"))
    return(invisible(FALSE))
  }
}

pull_file_from_hf <- function(repo_path, local_path, repo_id = HF_DATA_REPO_ID) {
  if (!nzchar(repo_id)) {
    message("HF dataset repository is not configured — using local ", local_path)
    return(invisible(FALSE))
  }
  token_candidates <- c(
    Sys.getenv("BASE_DATA_TOKEN", unset = ""),
    Sys.getenv("HF_TOKEN", unset = "")
  )
  token_candidates <- token_candidates[nzchar(token_candidates)]
  token <- if (length(token_candidates)) token_candidates[[1]] else ""
  message(
    "HF read credential for ", repo_id, ": ",
    if (nzchar(token)) "present" else "missing"
  )
  url <- glue::glue(
    "https://huggingface.co/datasets/{repo_id}/resolve/main/{repo_path}"
  )
  tmp_path <- tempfile(tmpdir = dirname(local_path),
                       pattern = "hf_pull_",
                       fileext = paste0(".", tools::file_ext(local_path)))
  download_timeout <- base_env_int("BASE_DATA_DOWNLOAD_TIMEOUT", 900L)

  resp <- tryCatch({
    if (nzchar(token)) {
      httr::GET(
        url,
        httr::add_headers(Authorization = paste("Bearer", token)),
        httr::write_disk(tmp_path, overwrite = TRUE),
        httr::timeout(download_timeout)
      )
    } else {
      httr::GET(
        url,
        httr::write_disk(tmp_path, overwrite = TRUE),
        httr::timeout(download_timeout)
      )
    }
  }, error = function(e) NULL)

  if (is.null(resp) || httr::http_error(resp)) {
    if (file.exists(tmp_path)) unlink(tmp_path)
    status <- if (is.null(resp)) "request error" else httr::status_code(resp)
    message(
      "HF pull failed for ", repo_path, " (status: ", status,
      ") — using local fallback if present."
    )
    return(invisible(FALSE))
  }

  ok <- file.rename(tmp_path, local_path)
  if (!ok) {
    ok <- file.copy(tmp_path, local_path, overwrite = TRUE)
    unlink(tmp_path)
  }
  if (!ok) {
    message("HF pull succeeded but could not update local file for ", repo_path)
    return(invisible(FALSE))
  }

  message("Pulled from HF dataset: ", repo_path)
  return(invisible(TRUE))
}

pull_season_data_from_hf <- function() {
  pull_file_from_hf(SEASON_DATA_REPO_PATH, SEASON_DATA_FILE, repo_id = SEASON_DATA_REPO_ID)
}

push_season_data_to_hf <- function(local_path = SEASON_DATA_FILE,
                                   commit_message = paste("Update", SEASON_DATA_REPO_PATH)) {
  push_file_to_hf(local_path, SEASON_DATA_REPO_PATH,
                  commit_message = commit_message,
                  repo_id = SEASON_DATA_REPO_ID)
}


parse_base_schedule_times <- function(schedule) {
  if ("DateTime" %in% names(schedule)) {
    raw_times <- trimws(as.character(schedule$DateTime))
  } else if ("Date" %in% names(schedule)) {
    game_clock <- if ("Time" %in% names(schedule)) {
      trimws(as.character(schedule$Time))
    } else {
      rep("12:00 PM", nrow(schedule))
    }
    raw_times <- paste(trimws(as.character(schedule$Date)), game_clock)
  } else {
    return(as.POSIXct(character(), tz = TEAM_CONFIG$schedule_timezone))
  }

  parsed <- suppressWarnings(lubridate::parse_date_time(
    raw_times,
    orders = c(
      "ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p",
      "mdy HMS", "mdy HM", "mdy IMS p", "mdy IM p", "mdy",
      "Ymd HMS", "Ymd HM"
    ),
    tz = TEAM_CONFIG$schedule_timezone,
    quiet = TRUE
  ))
  as.POSIXct(parsed, tz = TEAM_CONFIG$schedule_timezone)
}

read_next_game_from_schedule <- function(path = TEAM_CONFIG$data$schedule_file) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  schedule <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
  has_datetime <- "DateTime" %in% names(schedule) || "Date" %in% names(schedule)
  if (is.null(schedule) || !has_datetime || !"Opponent" %in% names(schedule)) {
    message("BASE_SCHEDULE_FILE must contain Opponent plus DateTime or Date/Time columns")
    return(NULL)
  }

  game_times <- parse_base_schedule_times(schedule)
  active_rows <- rep(TRUE, nrow(schedule))
  if ("Status" %in% names(schedule)) {
    active_rows <- !tolower(trimws(as.character(schedule$Status))) %in%
      c("cancelled", "canceled", "final", "completed", "postponed")
  }
  upcoming <- which(active_rows & !is.na(game_times) & game_times >= Sys.time())
  if (!length(upcoming)) return(NULL)
  i <- upcoming[which.min(game_times[upcoming])]
  value_or <- function(column, default) {
    if (!column %in% names(schedule) || is.na(schedule[[column]][i]) ||
        !nzchar(trimws(as.character(schedule[[column]][i])))) default else schedule[[column]][i]
  }
  bool_or <- function(column, default = TRUE) {
    value <- tolower(trimws(as.character(value_or(column, default))))
    if (value %in% c("true", "t", "1", "yes", "home", "h")) TRUE
    else if (value %in% c("false", "f", "0", "no", "away", "a")) FALSE
    else default
  }
  game_time <- game_times[i]

  list(
    opponent = as.character(schedule$Opponent[i]),
    venue = as.character(value_or("Venue", "Venue TBD")),
    is_home = bool_or("IsHome", TRUE),
    datetime = game_time,
    time_str = format(game_time, "%A, %B %d · %I:%M %p"),
    ms = as.numeric(game_time) * 1000,
    wins = as.integer(value_or("TeamWins", 0L)),
    losses = as.integer(value_or("TeamLosses", 0L)),
    opp_wins = as.integer(value_or("OppWins", 0L)),
    opp_losses = as.integer(value_or("OppLosses", 0L)),
    opp_abbr = as.character(value_or("OppAbbr", "OPP"))
  )
}

fetch_next_team_game <- function() {
  configured_game <- read_next_game_from_schedule()
  if (!is.null(configured_game)) return(configured_game)
  if (!isTRUE(TEAM_CONFIG$stats_api_enabled) || is.na(TEAM_CONFIG$mlb_team_id)) return(NULL)

  resp <- tryCatch(
    httr::GET(paste0(
      "https://statsapi.mlb.com/api/v1/schedule",
      "?sportId=", TEAM_CONFIG$sport_id,
      "&leagueId=", TEAM_CONFIG$league_id,
      "&teamId=", TEAM_CONFIG$mlb_team_id,
      "&startDate=", format(Sys.Date(), "%Y-%m-%d"),
      "&endDate=",   format(Sys.Date() + 30, "%Y-%m-%d"),
      "&hydrate=team,venue"
    ), httr::timeout(10)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::http_error(resp)) return(NULL)

  sched <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
  if (length(sched$dates) == 0) return(NULL)

  for (d in sched$dates) {
    g <- d$games[[1]]
    if (g$status$abstractGameState %in% c("Preview", "Live")) {
      is_home <- g$teams$home$team$id == TEAM_CONFIG$mlb_team_id

      opponent <- if (is_home) g$teams$away$team$name else g$teams$home$team$name

      # Venue: always the home team's venue
      venue <- g$venue$name

      # Record: configured team's side
      team_side <- if (is_home) g$teams$home else g$teams$away
      wins   <- team_side$leagueRecord$wins
      losses <- team_side$leagueRecord$losses

      # Opponent record
      opp_side <- if (is_home) g$teams$away else g$teams$home
      opp_wins   <- opp_side$leagueRecord$wins
      opp_losses <- opp_side$leagueRecord$losses

      game_dt_utc <- as.POSIXct(g$gameDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      game_dt_est <- lubridate::with_tz(game_dt_utc, "America/New_York")

      
      teams_resp <- tryCatch(
        httr::GET(paste0("https://statsapi.mlb.com/api/v1/teams?leagueId=",
                         TEAM_CONFIG$league_id), httr::timeout(10)),
        error = function(e) NULL
      )
      opp_abbr <- if (!is.null(teams_resp) && !httr::http_error(teams_resp)) {
        td  <- jsonlite::fromJSON(httr::content(teams_resp, "text", encoding="UTF-8"), simplifyVector=TRUE)$teams
        row <- td[td$name == opponent, ]
        if (nrow(row) > 0) row$abbreviation[1] else "OPP"
      } else "OPP"

      return(list(
        opponent   = opponent,
        venue      = venue,
        is_home    = is_home,
        datetime   = game_dt_est,
        time_str   = format(game_dt_est, "%A, %B %d · %I:%M %p"),
        ms         = as.numeric(game_dt_utc) * 1000,
        wins       = wins,
        losses     = losses,
        opp_wins   = opp_wins,
        opp_losses = opp_losses,
        opp_abbr   = opp_abbr
      ))
    }
  }
  return(NULL)
}

message("Fetching next game for ", TEAM_CONFIG$full_name, "...")
next_game <- tryCatch(fetch_next_team_game(), error = function(e) NULL)

NEXT_GAME_OPPONENT <- next_game$opponent   %||% "TBD"
NEXT_GAME_TIME_STR <- next_game$time_str   %||% "TBD"
NEXT_GAME_LOCATION <- next_game$venue      %||% "TBD"
NEXT_GAME_DT       <- next_game$datetime   %||% (Sys.time() + 86400)
NEXT_GAME_IS_HOME  <- next_game$is_home    %||% TRUE
TEAM_WINS          <- next_game$wins       %||% 0L
TEAM_LOSSES        <- next_game$losses     %||% 0L
OPP_WINS           <- next_game$opp_wins   %||% 0L
OPP_LOSSES         <- next_game$opp_losses %||% 0L
TEAM_STREAK        <- "--"
next_game_ms <- function() as.numeric(NEXT_GAME_DT) * 1000                      
# Postgame Pitcher Reports -> BrewSummaryCard card engine + tab (replaces the
# old generate_pitcher_pdf flow). Requires brewstuff.model, College26Heights.csv,
# percentile_table.csv, NcaaColors.csv, left_batter.png, right_batter.png.

# ============================================================================
# CARD ENGINE (inlined — was pitcher_report_card.R)
# ============================================================================
# Card engine for the BASE Postgame Pitcher Reports tab.
# Trimmed to the packages the card builder actually needs (the standalone app
# loaded many more for its Shiny UI / data tooling).
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(png)
library(lightgbm)
library(readr)
library(sysfonts)

options(shiny.maxRequestSize = 10000000 * 1024^2)
pdf(file = NULL)
Sys.setenv(TZ='EST')

model <- lgb.load('brewstuff.model')
Height26 <- tryCatch(
  read_csv(TEAM_CONFIG$data$heights_file, show_col_types = FALSE),
  error = function(e) tibble(
    tm_name = character(), team_abbr = character(),
    height = numeric(), set = numeric()
  )
)

percentiledata <- read_csv("percentile_table.csv")

get_percentile <- function(value, stat, pitch_type, percentiledata) {
  if (is.na(value)) return(NA_real_)
  
  df <- percentiledata %>%
    dplyr::filter(
      Stat == stat,
      TaggedPitchType == pitch_type
    ) %>%
    dplyr::arrange(Value)
  
  if (nrow(df) == 0) return(NA_real_)
  
  # Interpolate percentile
  p <- approx(
    x = df$Value,
    y = df$Percentile,
    xout = value,
    rule = 2  # clamp outside range
  )$y
  
  return(p)
}

percentile_color <- function(p) {
  if (is.na(p)) return(c(tok$bg_card, tok$text_body))

  t <- max(0, min(1, p / 100))
  soft_ramp <- colorRamp(c(tok$navy, tok$bg_card, tok$cardinal))
  bg <- soft_ramp(t)

  # WCAG relative luminance — switch to white text only when bg is too dark
  # for tok$text_primary to read clearly.
  rgb_norm <- bg / 255
  rgb_lin  <- ifelse(rgb_norm <= 0.03928,
                     rgb_norm / 12.92,
                     ((rgb_norm + 0.055) / 1.055) ^ 2.4)
  bg_lum   <- 0.2126 * rgb_lin[1] + 0.7152 * rgb_lin[2] + 0.0722 * rgb_lin[3]

  fg_col <- if (bg_lum < 0.45) "#FFFFFF" else tok$text_primary
  bg_col <- rgb(bg[1], bg[2], bg[3], maxColorValue = 255)

  c(bg_col, fg_col)
}


# percentile_table <- apply_percentile_calcs(College26)

# percentile_lookup <- percentile_table %>%
#   dplyr::group_by(Pitch, metric) %>%
#   dplyr::summarise(
#     value = list(value),
#     percentile = list(percentile),
#     .groups = "drop"
#   )

# percentile_lookup <- split(
#   percentile_lookup,
#   list(percentile_lookup$Pitch, percentile_lookup$metric),
#   drop = TRUE
# )

getBrewStuff <- function(game, final_model, bullpen = FALSE,
                         height_override = NULL, set_override = NULL) {

  BREW_STUFF_MEAN <- -0.025181704334056
  BREW_STUFF_SD   <-  0.0146306446044836

  game <- game %>% left_join(Height26, by = c("Pitcher" = "tm_name", "PitcherTeam" = "team_abbr"))

  # Fallback height for pitchers missing from College26Heights — lets us still
  # estimate arm angle for unlisted pitchers when the user supplies a height.
  if (!is.null(height_override) && length(height_override) == 1 &&
      !is.na(height_override) && is.finite(height_override)) {
    game <- game %>% mutate(height = coalesce(height, height_override))
  }

  # User-provided set position overrides whatever (if anything) is in the CSV.
  # Clamped to [-1, 1] feet from rubber center (+ve = 3B side).
  if (!is.null(set_override) && length(set_override) == 1 &&
      !is.na(set_override) && is.finite(set_override)) {
    set_val <- max(-1, min(1, set_override))
    if (!"set" %in% names(game)) game$set <- NA_real_
    game$set <- set_val
  }

  if(bullpen) {
    game <- game %>%
      cleanColumnContents() %>%
      calculateArmAngle() %>%
      get_rotated_movement(ivb_col = "InducedVertBreak", hb_col = "HorzBreak") %>%
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType %in% c("Changeup") ~ "ChangeUp",
          TaggedPitchType %in% c("FourSeamFastBall") ~ "Fastball",
          TaggedPitchType %in% c("OneSeamFastBall","TwoSeamFastBall") ~ "Sinker",
          TaggedPitchType %in% c('Other') ~ "Undefined",
          TRUE ~ TaggedPitchType
        )
      ) %>%
      filter(!is.na(PitcherThrows) & PitcherThrows %in% c("Left", "Right")) %>%
      mutate(
        armangle = round(armangle, 1),
        RelSpeed = round(RelSpeed, 1),
        InducedVertBreak = round(InducedVertBreak, 1),
        HorzBreak = round(HorzBreak, 1),
        HorzBreak = ifelse(PitcherThrows == "Left", HorzBreak * -1, HorzBreak),
        SpinRate = round(SpinRate, 1),
        RelHeight = round(RelHeight, 1),
        Extension = round(Extension, 1)
      ) %>%
      ungroup()
  } else {
    game <- game %>%
      cleanColumnContents() %>%
      calculateArmAngle() %>%
      get_rotated_movement(ivb_col = "InducedVertBreak", hb_col = "HorzBreak") %>%
      filter(
        !is.na(PitcherThrows) & !is.na(BatterSide) &
          PitcherThrows %in% c("Left", "Right") &
          BatterSide %in% c("Left", "Right")
      ) %>%
      mutate(
        armangle = round(armangle, 1),
        RelSpeed = round(RelSpeed, 1),
        InducedVertBreak = round(InducedVertBreak, 1),
        HorzBreak = round(HorzBreak, 1),
        HorzBreak = ifelse(PitcherThrows == "Left", HorzBreak * -1, HorzBreak),
        SpinRate = round(SpinRate, 1),
        RelHeight = round(RelHeight, 1),
        Extension = round(Extension, 1),
        opp_hand = ifelse(PitcherThrows != BatterSide, 1, 0)
      ) %>%
      ungroup()
  }

  game <- game %>%
    group_by(Pitcher) %>%
    mutate(
      is_fastball = TaggedPitchType %in% c("Fastball", "FourSeamFastBall",
                                           "TwoSeamFastBall", "Sinker",
                                           "Four-Seam", "Two-Seam Fastball")
    ) %>%
    group_by(Pitcher, TaggedPitchType) %>%
    mutate(pitch_count = n()) %>%
    group_by(Pitcher) %>%
    mutate(
      # Most-used fastball if the pitcher throws one, else the most-used pitch
      # overall. Computed as a single length-1 value per group so an all-
      # "Undefined" (no fastball) pitcher can't trip case_when's size check.
      primary_fb = {
        cand <- if (any(is_fastball)) {
          fb <- which(is_fastball)
          TaggedPitchType[fb][which.max(pitch_count[fb])]
        } else {
          TaggedPitchType[which.max(pitch_count)]
        }
        if (length(cand) == 0) NA_character_ else cand[1]
      }
    ) %>%
    mutate(
      fb_rel_speed = round(mean(RelSpeed[TaggedPitchType == primary_fb], na.rm = TRUE), 1),
      fb_ivb = round(mean(InducedVertBreak[TaggedPitchType == primary_fb], na.rm = TRUE), 1),
      fb_hb = round(mean(HorzBreak[TaggedPitchType == primary_fb], na.rm = TRUE), 1),

      velo_diff = round(ifelse(is.na(primary_fb), 0, RelSpeed - fb_rel_speed), 1),
      ivb_diff = round(ifelse(is.na(primary_fb), 0, InducedVertBreak - fb_ivb), 1),
      hb_diff = round(ifelse(is.na(primary_fb), 0, HorzBreak - fb_hb), 1),

      ivb_adj = round(InducedVertBreak * (RelHeight / mean(RelHeight, na.rm = TRUE)), 1),
      hb_adj = round(HorzBreak * (Extension / mean(Extension, na.rm = TRUE)), 1)
    ) %>%
    ungroup()

  feature_vars <- c("RelSpeed", "InducedVertBreak", "HorzBreak",
                    "SpinRate", "RelHeight", "armangle", "Extension", "velo_diff",
                    "ivb_diff", "hb_diff", "ivb_adj", "hb_adj")

  complete_rows <- complete.cases(game[, feature_vars])
  game_complete <- game[complete_rows, ]

  game_na <- if(any(!complete_rows)) {
    na_rows <- game[!complete_rows, ]
    na_rows$rv <- NA
    na_rows$Stuff <- NA
    if(bullpen) {
      na_rows$rv_vs_R <- NA
      na_rows$rv_vs_L <- NA
      na_rows$Stuff_vs_R <- NA
      na_rows$Stuff_vs_L <- NA
    }
    na_rows
  } else {
    empty_df <- game_complete[0, ]
    empty_df$Stuff <- numeric()
    if(bullpen) {
      empty_df$rv_vs_R <- numeric()
      empty_df$rv_vs_L <- numeric()
      empty_df$Stuff_vs_R <- numeric()
      empty_df$Stuff_vs_L <- numeric()
    }
    empty_df
  }

  if(nrow(game_complete) > 0) {
    if(bullpen) {
      game_vs_righties <- game_complete
      game_vs_righties$opp_hand <- ifelse(game_vs_righties$PitcherThrows == "Left", 1, 0)

      game_vs_lefties <- game_complete
      game_vs_lefties$opp_hand <- ifelse(game_vs_lefties$PitcherThrows == "Right", 1, 0)

      game_complete$rv_vs_R <- predict(final_model, as.matrix(cbind(
        game_vs_righties$RelSpeed,
        game_vs_righties$InducedVertBreak,
        game_vs_righties$HorzBreak,
        game_vs_righties$SpinRate,
        game_vs_righties$RelHeight,
        game_vs_righties$Extension,
        game_vs_righties$velo_diff,
        game_vs_righties$ivb_diff,
        game_vs_righties$hb_diff,
        game_vs_righties$ivb_adj,
        game_vs_righties$hb_adj,
        game_vs_righties$armangle,
        game_vs_righties$opp_hand
      )))

      game_complete$rv_vs_L <- predict(final_model, as.matrix(cbind(
        game_vs_lefties$RelSpeed,
        game_vs_lefties$InducedVertBreak,
        game_vs_lefties$HorzBreak,
        game_vs_lefties$SpinRate,
        game_vs_lefties$RelHeight,
        game_vs_lefties$Extension,
        game_vs_lefties$velo_diff,
        game_vs_lefties$ivb_diff,
        game_vs_lefties$hb_diff,
        game_vs_lefties$ivb_adj,
        game_vs_lefties$hb_adj,
        game_vs_lefties$armangle,
        game_vs_lefties$opp_hand
      )))

      game_complete$Stuff_vs_R <- scale_Stuff(game_complete$rv_vs_R, BREW_STUFF_MEAN, BREW_STUFF_SD)
      game_complete$Stuff_vs_L <- scale_Stuff(game_complete$rv_vs_L, BREW_STUFF_MEAN, BREW_STUFF_SD)
      game_complete$Stuff <- (game_complete$Stuff_vs_R + game_complete$Stuff_vs_L) / 2
    } else {
      game_complete$rv <- predict(final_model, as.matrix(cbind(
        game_complete$RelSpeed,
        game_complete$InducedVertBreak,
        game_complete$HorzBreak,
        game_complete$SpinRate,
        game_complete$RelHeight,
        game_complete$Extension,
        game_complete$velo_diff,
        game_complete$ivb_diff,
        game_complete$hb_diff,
        game_complete$ivb_adj,
        game_complete$hb_adj,
        game_complete$armangle,
        game_complete$opp_hand
      )))
      game_complete$Stuff <- scale_Stuff(game_complete$rv, BREW_STUFF_MEAN, BREW_STUFF_SD)
    }
  } else {
    # Predict block was skipped (no complete rows — usually because armangle
    # is NA for every row when height is missing). Add the Stuff/rv columns
    # so game_complete and game_na have matching schemas for the bind below,
    # and downstream code that references Stuff doesn't blow up.
    game_complete$rv    <- numeric()
    game_complete$Stuff <- numeric()
    if (bullpen) {
      game_complete$rv_vs_R    <- numeric()
      game_complete$rv_vs_L    <- numeric()
      game_complete$Stuff_vs_R <- numeric()
      game_complete$Stuff_vs_L <- numeric()
    }
  }

  game_complete <- dplyr::bind_rows(game_complete, game_na)
  return(game_complete)
}

scale_Stuff <- function(raw_score, model_mean, model_sd) {
  scaled_score <- (raw_score - model_mean) / model_sd
  result <- 100 - (scaled_score * 10)
  return(result)
}

cleanColumnContents <- function(df) {
  
  df <- df %>%
    mutate(
      TaggedPitchType = case_when(
        TaggedPitchType %in% c("Changeup") ~ "ChangeUp",
        TaggedPitchType %in% c("FourSeamFastBall") ~ "Fastball",
        TaggedPitchType %in% c("OneSeamFastBall","TwoSeamFastBall") ~ "Sinker",
        TaggedPitchType %in% c('Other') ~ "Undefined", TRUE ~ TaggedPitchType
      )
    ) %>%
    mutate(PitchCall = ifelse(PitchCall %in% c('BallIntentional','ballCalled','BallinDirt','BalIntentional'), 'BallCalled', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('FoulBallNotFieldable','FoulBallFieldable','FouldBallNotFieldable'), 'FoulBall', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('inPlay','Inplay','InPLay','InPlay'), PlayResult, PitchCall),
           PitchCall = ifelse(PitchCall %in% c('StrkeSwinging'), 'StrikeSwinging', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('SrikeCalled','StriekC',"StrikeC'alled"), 'StrikeCalled', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('Error','error','FieldersChoice','Sacrifice','Fielderschoice'), 'Out', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('homerun','Homerun'), 'HomeRun', PitchCall),
           PitchCall = ifelse(PitchCall %in% c('SIngle'), 'Single', PitchCall))
  
}

get_rotated_movement <- function(data, ivb_col = "IVB", hb_col = "HB", pitcher_throws_col = "PitcherThrows", arm_angle_col = "armangle") {
  
  # Calculate rotated movement based on each pitcher's arm angle and handedness
  data <- data %>%
    mutate(
      is_lefty = !!sym(pitcher_throws_col) == "Left",
      # Calculate rotation angle for each row
      rotation_angle = ifelse(
        is_lefty,
        -(90 - !!sym(arm_angle_col)) * (pi / 180),  # Clockwise for lefties
        (90 - !!sym(arm_angle_col)) * (pi / 180)    # Counterclockwise for righties
      ),
      # Apply rotation matrix using the actual column names
      rotated_HB = !!sym(hb_col) * cos(rotation_angle) - !!sym(ivb_col) * sin(rotation_angle),
      rotated_IVB = !!sym(hb_col) * sin(rotation_angle) + !!sym(ivb_col) * cos(rotation_angle)
    ) %>%
    # Remove temporary columns
    select(-is_lefty, -rotation_angle)
  
  return(data)
}

calculateArmAngle <- function(df) {
  # Input validation
  required_cols <- c("RelHeight", "RelSide", "height")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("DataFrame is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # `set` shifts the assumed standing position on the rubber (feet from center,
  # +ve = 3B side, -ve = 1B side). Default 0 = middle of rubber.
  if (!"set" %in% names(df)) df$set <- 0

  df <- df %>%
    mutate(
      set_offset = ifelse(is.na(set), 0, set),
      shoulder_height = height * 0.7,
      vertical_distance = RelHeight - shoulder_height,
      adjusted_RelSide = RelSide - set_offset,
      side_eff = ifelse(PitcherThrows == "Left", -adjusted_RelSide, adjusted_RelSide),
      armangle = atan2(vertical_distance, side_eff) * (180 / pi)
    ) %>%
    select(-shoulder_height, -vertical_distance, -adjusted_RelSide, -set_offset, -side_eff)

  return(df)
}

getaccelbreak <- function(df,release_extension_col = "Extension", vx0_col = "vx0", vy0_col = "vy0",vz0_col = "vz0",ax_col = "ax0", ay_col = "ay0",az_col = "az0"){
  # Store the original class to return the same type
  is_data_table <- inherits(df, "data.table")
  # Constants
  MOUND_DISTANCE <- 60.5
  STATCAST_INITIAL_MEASUREMENT <- 50.0
  Z_CONSTANT <- 32.174
  HOME_PLATE_HEIGHT <- 17/12
  # Compute release position
  df$release_pos_y <- MOUND_DISTANCE - df[[release_extension_col]]
  # Compute release time
  df$release_time <- (-df[[vy0_col]] -
                        sqrt((df[[vy0_col]]^2) -
                               (2 * df[[ay_col]] * (STATCAST_INITIAL_MEASUREMENT - df$release_pos_y)))) / df[[ay_col]]
  # Compute release velocities
  df$vxR <- df[[vx0_col]] + (df[[ax_col]] * df$release_time)
  df$vyR <- df[[vy0_col]] + (df[[ay_col]] * df$release_time)
  df$vzR <- df[[vz0_col]] + (df[[az_col]] * df$release_time)
  # Compute time to home plate
  df$tf <- (-df$vyR - sqrt(df$vyR^2 - 2 * df[[ay_col]] * (df$release_pos_y - HOME_PLATE_HEIGHT))) / df[[ay_col]]
  df$vertaccel <- ((2 * (df$InducedVertBreak/12))/(df$tf * df$tf))
  df$horzaccel <- ((2 * (df$HorzBreak/12))/(df$tf * df$tf))
  
  intermediate_cols <- c("temp_x", "temp_y", "temp_z", "handedness_factor",
                         "horz_mag", "horz_x", "horz_y", "horz_z",
                         "vert_x", "vert_y", "vert_z",
                         "tang_x", "tang_y", "tang_z")
  
  columns_to_keep <- setdiff(names(df), intermediate_cols)
  # Handle the different object types
  if (is_data_table) {
    # Create a character vector for data.table syntax
    df <- df[, columns_to_keep, with = FALSE]
  } else {
    # For regular data.frame
    df <- df[, columns_to_keep, drop = FALSE]
  }
  return(df)
}

pitcher_summary <- function(tmilb){
  tmilb %>%
    select(any_of(c("Date","GameUID","HomeTeam","AwayTeam","BatterTeam","Tilt","ExitSpeed")),
           Batter,Pitcher,PitcherTeam,PlayResult,PitchCall,TaggedPitchType,RelSpeed,SpinRate,
           Extension,PlateLocSide,PlateLocHeight,RelSide,RelHeight,ax0,ay0,az0,vx0,
           vz0,vy0,pfxx,pfxz,InducedVertBreak,HorzBreak,SpinAxis,PitcherThrows,BatterSide,
           PitchofPA,VertApprAngle,AutoPitchType, KorBB, OutsOnPlay, TaggedHitType) %>%
    mutate(
      PlateLocSide = suppressWarnings(as.numeric(PlateLocSide)),
      PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
      RelSpeed = suppressWarnings(as.numeric(RelSpeed)),
      SpinRate = suppressWarnings(as.numeric(SpinRate)),
      Extension = suppressWarnings(as.numeric(Extension)),
      RelSide = suppressWarnings(as.numeric(RelSide)),
      RelHeight = suppressWarnings(as.numeric(RelHeight)),
      ax0 = suppressWarnings(as.numeric(ax0)),
      ay0 = suppressWarnings(as.numeric(ay0)),
      az0 = suppressWarnings(as.numeric(az0)),
      vx0 = suppressWarnings(as.numeric(vx0)),
      vy0 = suppressWarnings(as.numeric(vy0)),
      vz0 = suppressWarnings(as.numeric(vz0)),
      pfxx = suppressWarnings(as.numeric(pfxx)),
      pfxz = suppressWarnings(as.numeric(pfxz)),
      InducedVertBreak = suppressWarnings(as.numeric(InducedVertBreak)),
      HorzBreak = suppressWarnings(as.numeric(HorzBreak)),
      SpinAxis = suppressWarnings(as.numeric(SpinAxis)),
      PitchofPA = suppressWarnings(as.numeric(PitchofPA)),
      OutsOnPlay = suppressWarnings(as.numeric(OutsOnPlay)),
      is_strike_swinging = ifelse(PitchCall == "StrikeSwinging", TRUE, FALSE)
    ) %>%
    mutate("Hit" = case_when(PlayResult %in% c("Single","Double","Triple","HomeRun") ~ TRUE,TRUE ~ FALSE),
         "CallStrike" = case_when(PitchCall %in% c("StrikeCalled") ~ TRUE, TRUE ~ FALSE),
         "Whiff" = case_when(PitchCall %in% c("StrikeSwinging") ~ TRUE, TRUE ~ FALSE),
         "CSW" = CallStrike + Whiff,
         "Contact" = case_when(PitchCall %in% c("FoulBall","FoulBallNotFieldable","InPlay") ~ TRUE, TRUE ~ FALSE),
         "GB" = case_when(TaggedHitType %in% c('GroundBall') ~ TRUE, TRUE ~ FALSE),
         "LD" = case_when(TaggedHitType %in% c('LineDrive') ~ TRUE, TRUE ~ FALSE),
         "FB" = case_when(TaggedHitType %in% c ("FlyBall") ~ TRUE, TRUE ~ FALSE),
         "PopU" = case_when(TaggedHitType %in% c ("Popup") ~ TRUE, TRUE ~ FALSE),
         "Swing" = Whiff + Contact,
         "BBE" = GB + LD + FB + PopU,
        # "HardHit" = ifelse(ExitSpeed >= 95,TRUE,FALSE),
         "Ball" = case_when(PitchCall %in% c("BallCalled","BallinDirt") ~ TRUE, TRUE ~ FALSE),
         "Single" = case_when(PlayResult %in% c("Single") ~ TRUE, TRUE ~ FALSE),
         "Double" = case_when(PlayResult %in% c("Double") ~ TRUE, TRUE ~ FALSE),
         "Triple" = case_when(PlayResult %in% c("Triple") ~ TRUE, TRUE ~ FALSE),
         "HR" = case_when(PlayResult %in% c("HomeRun") ~ TRUE, TRUE ~ FALSE),
         "Sac" = case_when(PlayResult %in% c("Sacrifice") ~ TRUE, TRUE ~ FALSE),
         "HBP" = case_when(PitchCall %in% c("HitByPitch") ~ TRUE, TRUE ~ FALSE),
         "Error" = case_when(PlayResult %in% c("Error") ~ TRUE, TRUE ~ FALSE),
         "FC"= case_when(PlayResult %in% c("FieldersChoice") ~ TRUE, TRUE ~ FALSE),
         "Out" = case_when(PlayResult %in% c ("Out") ~ TRUE, TRUE ~ FALSE),
         "BIP" = Single + Double + Triple + HR + Sac + Error + Out + FC,
        #  "Count" = paste0(Balls,"-",Strikes),
        #  "BSituation" = ifelse(Balls > Strikes,"Ahead",NA),
        #  "BSituation" = ifelse(Balls < Strikes,"Behind",BSituation),
        #  "BSituation" = ifelse(Balls == Strikes,"Even",BSituation),
        #  "PSituation" = ifelse(Balls < Strikes,"Ahead",NA),
        #  "PSituation" = ifelse(Balls > Strikes,"Behind",PSituation),
        #  "PSituation" = ifelse(Balls == Strikes,"Even",PSituation),
         "Strikeout" = ifelse(KorBB == "Strikeout",TRUE,FALSE),
         "Walk" = ifelse(KorBB == "Walk",TRUE,FALSE),
         "Zone" = case_when(between(PlateLocSide,-.825,.825) & between(PlateLocHeight,1.45,3.45) ~ TRUE, TRUE ~ FALSE),
         "AB" = Strikeout + BIP - Sac,
         "PA" = Strikeout + BIP + Walk + HBP,
         "Strike" = CallStrike + Contact + Whiff
    )
}

left_batter <- png::readPNG("left_batter.png")
right_batter <- png::readPNG("right_batter.png")

read_input_file <- function(datapath, original_name) {
  ext <- tolower(tools::file_ext(original_name))
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading .parquet requires the 'arrow' package. ",
           "Install it with install.packages('arrow').")
    }
    as.data.frame(arrow::read_parquet(datapath))
  } else {
    # readr (not base read.csv) so time/date columns parse to the same
    # hms/Date/POSIXct classes the configured TrackMan season source uses. With base read.csv
    # they come back as <character>, and the later bind_rows() against the
    # season fails on a <hms> vs <character> mismatch (Time/Tilt/UTCTime) —
    # combine_with_manual() swallows that error and the upload is silently
    # dropped, so the new game never reaches the team/date/pitcher selectors.
    as.data.frame(readr::read_csv(datapath, show_col_types = FALSE))
  }
}

# ============================================================================
# Mockup card design (ported from R/mockup_card.R)
# Visual tokens, palette, theme, and grob builders used by the layout in
# observeEvent(input$update1).
# ============================================================================

have_showtext <- requireNamespace("showtext", quietly = TRUE) &&
                 requireNamespace("sysfonts", quietly = TRUE)
#font_sans <- "sans"
#font_mono <- "sans"
if (have_showtext) {
  sysfonts::font_add_google("Arimo",         "arimo")
  sysfonts::font_add_google("Courier Prime", "cprime")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 96)
  font_sans <- "arimo"
  font_mono <- "cprime"
}

# C

tok <- list(
  bg_page      = "#FFFFFF",
  bg_card      = "#FAFAFB",
  bg_card_alt  = "#F4F4F6",
  border       = "#EAEAEE",
  text_primary = "#16161B",
  text_body    = "#36363F",
  text_2nd     = "#5F5F6B",
  text_3rd     = "#8B8B96",
  navy         = TEAM_CONFIG$colors$primary,
  cardinal     = TEAM_CONFIG$colors$accent,
  cardinal_glow= TEAM_CONFIG$colors$accent,
  success      = "#1B8A4D",
  success_soft = "#E1F4E9",
  danger       = "#C03029",
  danger_soft  = "#F7DEDC",
  zero_axis    = "#5F5F6B"
)

ncaa_colors <- tryCatch(
  readr::read_csv("NcaaColors.csv", show_col_types = FALSE),
  error = function(e) NULL
)

team_palette <- function(team_abbr) {
  fallback_label <- if (length(team_abbr) && !is.na(team_abbr) &&
                        base_team_matches(team_abbr)) TEAM_CONFIG$full_name else team_abbr
  fallback <- list(primary = TEAM_CONFIG$colors$primary,
                   secondary = TEAM_CONFIG$colors$accent,
                   label = fallback_label, logo_url = NA_character_)
  if (is.null(ncaa_colors) || length(team_abbr) == 0 ||
      is.na(team_abbr) || team_abbr == "") return(fallback)
  if (base_team_matches(team_abbr)) {
    fallback$logo_url <- TEAM_CONFIG$assets$report_logo_url
    return(fallback)
  }
  row <- ncaa_colors[ncaa_colors$team_abbr == team_abbr, ]
  if (nrow(row) == 0) return(fallback)
  list(primary   = row$`Primary Color`[1],
       secondary = row$`Secondary Color`[1],
       label     = row$Team[1],
       logo_url  = row$NCAA_img[1])
}

team_logo_cache <- new.env(parent = emptyenv())

fetch_team_logo <- function(team_abbr, logo_url) {
  key <- if (length(team_abbr) == 0 || is.na(team_abbr)) "" else team_abbr
  if (nzchar(key) && exists(key, envir = team_logo_cache, inherits = FALSE)) {
    return(get(key, envir = team_logo_cache, inherits = FALSE))
  }
  if (length(logo_url) == 0 || is.na(logo_url) || !nzchar(logo_url)) {
    return(NULL)
  }
  img <- tryCatch({
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    if (requireNamespace("rsvg", quietly = TRUE)) {
      rsvg::rsvg_png(logo_url, tmp, width = 512)
      png::readPNG(tmp)
    } else if (requireNamespace("magick", quietly = TRUE)) {
      mi <- magick::image_read(logo_url)
      mi <- magick::image_resize(mi, "512x512")
      magick::image_write(mi, tmp, format = "png")
      png::readPNG(tmp)
    } else NULL
  }, error = function(e) {
    message("[logo] fetch failed for ", team_abbr, ": ", conditionMessage(e))
    NULL
  })
  if (nzchar(key)) assign(key, img, envir = team_logo_cache)
  img
}

format_pitcher_name <- function(nm) {
  vapply(as.character(nm), function(n) {
    if (length(n) == 0 || is.na(n) || !grepl(",", n)) return(as.character(n))
    parts <- strsplit(n, ",\\s*")[[1]]
    if (length(parts) >= 2) paste(trimws(parts[2]), trimws(parts[1])) else as.character(n)
  }, character(1), USE.NAMES = FALSE)
}

parse_pitcher_value <- function(v) {
  if (length(v) == 0 || is.na(v) || !nzchar(v)) {
    return(list(name = NA_character_, team = NA_character_))
  }
  parts <- strsplit(v, "::", fixed = TRUE)[[1]]
  list(name = parts[1],
       team = if (length(parts) >= 2) parts[2] else NA_character_)
}

ordinal_suffix <- function(n) {
  if (n %% 100 %in% 11:13) return("th")
  switch(as.character(n %% 10),
         "1" = "st", "2" = "nd", "3" = "rd", "th")
}

format_pretty_date <- function(d) {
  d <- as.Date(d)
  day <- as.integer(format(d, "%d"))
  paste0(format(d, "%B "), day, ordinal_suffix(day))
}

pitch_pal <- c(
  "Four-Seam"        = "#D94A3F",
  "Two-Seam Fastball"= "#B83A95",
  "Sinker"           = "#D17A2A",
  "Cutter"           = "#C9A82E",
  "Splitter"         = "#7FD9A2",
  "Changeup"         = "#3DA84B",
  "Slider"           = "#2C7AB8",
  "Sweeper"          = "#B566B5",
  "Curveball"        = "#8E2EA0",
  "Knuckle Curve"    = "#7A2A52",
  "Slurve"           = "#3F8A66",
  "Knuckle Ball"     = "#3FA3A3",
  "Eephus"           = "#9A9AA0",
  "Fastball"         = "#5FA8C9",
  "Slow Curve"       = "#B8B8BE",
  "Screwball"        = "#C96A95"
)

canonicalize_pitch <- function(x) {
  case_when(
    x %in% c("Fastball", "FourSeamFastBall", "FF","FastBall") ~ "Four-Seam",
    x %in% c("TwoSeamFastBall", "OneSeamFastBall", "Sinker", "SI") ~ "Sinker",
    x %in% c("ChangeUp", "CH") ~ "Changeup",
    x %in% c("KnuckleCurve", "KC") ~ "Curveball",
    x %in% c("CutFastBall", "FC") ~ "Cutter",
    x %in% c("SL") ~ "Slider",
    x %in% c("CU") ~ "Curveball",
    x %in% c("FS") ~ "Splitter",
    TRUE ~ x
  )
}

# Build a palette that always covers every type present in the data —
# unknowns fall back to a neutral grey so scale_*_manual never crashes.
pal_for <- function(types, palette = pitch_pal, default = "#888888") {
  types <- unique(as.character(types))
  types <- types[!is.na(types)]
  unknown <- setdiff(types, names(palette))
  if (length(unknown)) {
    palette <- c(palette, setNames(rep(default, length(unknown)), unknown))
  }
  palette
}

theme_wildcats <- function(base = 14) {
  theme_minimal(base_size = base, base_family = font_sans) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#EAEAEE", linewidth = 0.4),
      axis.text        = element_text(colour = "#5F5F6B",
                                      family = font_mono, size = base - 2,
                                      face = "bold"),
      axis.title       = element_text(colour = "#5F5F6B", size = base - 2),
      plot.title       = element_text(colour = "#5F5F6B", size = base,
                                      face = "plain", hjust = 0,
                                      margin = margin(b = 8)),
      legend.position  = "none",
      plot.margin      = margin(12, 12, 12, 12)
    )
}

header_grob_fn <- function(pitcher_name, subtitle,
                           team_label   = toupper(TEAM_CONFIG$full_name),
                           banner_color = tok$navy,
                           accent_color = tok$cardinal_glow,
                           logo         = NULL) {
  logo_x  <- 22
  logo_sz <- 64
  text_x  <- if (!is.null(logo)) logo_x + logo_sz + 14 else logo_x

  children <- list(rectGrob(gp = gpar(fill = banner_color, col = NA)))
  if (!is.null(logo)) {
    children[[length(children) + 1]] <- rasterGrob(
      logo,
      x = unit(logo_x, "pt"), y = unit(0.5, "npc"),
      width = unit(logo_sz, "pt"), height = unit(logo_sz, "pt"),
      just = c("left", "centre"), interpolate = TRUE
    )
  }
  children <- c(children, list(
    textGrob(pitcher_name,
             x = unit(text_x, "pt"), y = unit(1, "npc") - unit(20, "pt"),
             just = c("left","top"),
             gp = gpar(col = accent_color, fontsize = 42,
                       fontface = "bold", fontfamily = font_sans)),
    rectGrob(x = unit(text_x, "pt"), y = unit(1, "npc") - unit(60, "pt"),
             width = unit(28, "pt"), height = unit(2.5, "pt"),
             just = c("left","top"),
             gp = gpar(fill = accent_color, col = NA)),
    textGrob(subtitle,
             x = unit(text_x, "pt"), y = unit(1, "npc") - unit(72, "pt"),
             just = c("left","top"),
             gp = gpar(col = accent_color, fontsize = 16,
                       fontface = "bold", fontfamily = font_sans))
  ))
  gTree(children = do.call(gList, children))
}

box_grob_fn <- function(box_row) {
  cols <- names(box_row)
  vals <- as.character(unlist(box_row))
  n    <- length(cols)
  cell_w <- 1 / n
  children <- list(rectGrob(gp = gpar(fill = tok$bg_card, col = tok$border)))
  for (i in seq_len(n)) {
    cx <- (i - 0.5) * cell_w
    children[[length(children) + 1]] <- textGrob(
      toupper(cols[i]),
      x = unit(cx, "npc"), y = unit(0.72, "npc"),
      gp = gpar(col = tok$text_2nd, fontsize = 12,
                fontface = "plain", fontfamily = font_sans))
    children[[length(children) + 1]] <- textGrob(
      vals[i],
      x = unit(cx, "npc"), y = unit(0.32, "npc"),
      gp = gpar(col = tok$text_primary, fontsize = 26,
                fontface = "bold", fontfamily = font_mono))
  }
  gTree(children = do.call(gList, children))
}

footer_grob_fn <- function() {
  gTree(children = gList(
    rectGrob(gp = gpar(fill = tok$bg_page, col = NA)),
    textGrob(TEAM_CONFIG$full_name,
             x = 0.02, y = 0.5, hjust = 0,
             gp = gpar(col = tok$text_3rd, fontsize = 9,
                       fontfamily = font_sans, fontface = "bold")),
    textGrob("Pitching Summary Card",
             x = 0.5, y = 0.5, hjust = 0.5,
             gp = gpar(col = tok$text_3rd, fontsize = 9,
                       fontfamily = font_sans, fontface = "bold")),
    textGrob(format(Sys.Date(), "Generated %Y-%m-%d"),
             x = 0.98, y = 0.5, hjust = 1,
             gp = gpar(col = tok$text_3rd, fontsize = 9,
                       fontfamily = font_sans))
  ))
}

# Full-width legend explaining the location-plot outcome markers. Drawn as its
# own card row so it can't be clipped by the narrow location panels.
markers_legend_grob <- function() {
  key_gp <- gpar(fill = "#5F5F6B", col = "black", lwd = 1.4)
  txt_gp <- gpar(col = tok$text_2nd, fontsize = 11,
                 fontface = "bold", fontfamily = font_sans)
  gTree(children = gList(
    rectGrob(gp = gpar(fill = tok$bg_page, col = NA)),
    # Hard Hit — diamond
    pointsGrob(x = unit(0.40, "npc"), y = unit(0.5, "npc"),
               pch = 22, size = unit(11, "pt"), gp = key_gp),
    textGrob("Hard Hit (95+ EV)", x = unit(0.415, "npc"), y = unit(0.5, "npc"),
             hjust = 0, gp = txt_gp),
    # Whiff — square
    pointsGrob(x = unit(0.565, "npc"), y = unit(0.5, "npc"),
               pch = 23, size = unit(11, "pt"), gp = key_gp),
    textGrob("Whiff", x = unit(0.58, "npc"), y = unit(0.5, "npc"),
             hjust = 0, gp = txt_gp)
  ))
}

build_arsenal_grob <- function(arsenal_full, percentiledata) {
  
  display_names <- c("Type","Usage","FPS","Velo","Top","Spin",
                     "IVB","HB","RelH","RelS","Ext","AA","CSW","Whiff","Chase",
                     "Stuff+ vL","Stuff+ vR","Stuff+")
  
  names(arsenal_full) <- display_names
  
  nrow_ars <- nrow(arsenal_full)
  ncol_ars <- ncol(arsenal_full)
  
  fg_mat <- matrix(tok$text_body, nrow_ars, ncol_ars)
  bg_mat <- matrix(tok$bg_card,   nrow_ars, ncol_ars)
  ff_mat <- matrix(font_mono,     nrow_ars, ncol_ars)
  
  ff_mat[, 1] <- font_sans
  
  fontsize_mat <- matrix(12, nrow_ars, ncol_ars)
  hjust_mat    <- matrix(0.5, nrow_ars, ncol_ars)
  x_mat        <- matrix(0.5, nrow_ars, ncol_ars)
  
  # Color pitch names
  for (i in seq_len(nrow_ars)) {
    nm <- arsenal_full[[1]][i]
    if (nm %in% names(pitch_pal)) {
      fg_mat[i, 1] <- unname(pitch_pal[nm])
    }
  }
  
  fontsize_mat[, 1] <- 14
  
  # Columns to apply gradients
  gradient_cols <- c("Velo", "Top", "Ext", "CSW", "Whiff", "Chase")
  
  # Mapping to percentile table stats
  stat_map <- c(
    "Velo"  = "AvgVelo",
    "Top"   = "TopVelo",
    "Ext"   = "Ext",
    "CSW"   = "CSW",
    "Whiff" = "Whiff",
    "Chase" = "Chase"
  )
  
  # Helper to parse numeric values
  parse_stat <- function(x) {
    s <- gsub("[^0-9.-]", "", as.character(x))
    suppressWarnings(as.numeric(s))
  }
  
  # ---- APPLY PERCENTILE COLORING ----
  for (lbl in gradient_cols) {
    
    j <- which(display_names == lbl)
    if (!length(j)) next
    
    stat_name <- stat_map[[lbl]]
    
    for (i in seq_len(nrow_ars)) {
      
      pitch_type <- as.character(arsenal_full[[1]][i])
      
      if (pitch_type == "Total") next
      
      val <- parse_stat(arsenal_full[[j]][i])
      
      pctl <- get_percentile(
        value = val,
        stat = stat_name,
        pitch_type = pitch_type,
        percentiledata = percentiledata
      )
      
      cols <- percentile_color(pctl)

      bg_mat[i, j] <- cols[1]
      fg_mat[i, j] <- cols[2]
    }
  }

  # Stuff+ columns are already z-scored to mean = 100, sd = 10, so we can
  # convert each value directly to a percentile via the normal CDF instead
  # of looking up peer benchmarks. Includes the Total row.
  stuff_lbls <- c("Stuff+ vL", "Stuff+ vR", "Stuff+")
  stuff_idx  <- which(display_names %in% stuff_lbls)
  for (j in stuff_idx) {
    for (i in seq_len(nrow_ars)) {
      val  <- parse_stat(arsenal_full[[j]][i])
      pctl <- if (is.na(val)) NA else pnorm((val - 100) / 10) * 100
      cols <- percentile_color(pctl)
      bg_mat[i, j] <- cols[1]
      fg_mat[i, j] <- cols[2]
    }
  }

  # Emphasize Total row, but leave Stuff+ cells alone so their gradient text
  # color survives.
  total_idx <- which(arsenal_full[[1]] == "Total")
  if (length(total_idx)) {
    non_colored <- setdiff(seq_len(ncol_ars), c(1, stuff_idx))
    fg_mat[total_idx, non_colored] <- tok$text_primary
  }
  
  # ---- TABLE THEME ----
  ars_theme <- gridExtra::ttheme_minimal(
    core = list(
      bg_params = list(fill = bg_mat, col = tok$border),
      fg_params = list(col = fg_mat,
                       fontfamily = ff_mat,
                       fontsize = fontsize_mat,
                       fontface = "bold",
                       hjust = hjust_mat,
                       x = x_mat)
    ),
    colhead = list(
      bg_params = list(fill = tok$bg_card_alt, col = tok$border),
      fg_params = list(col = tok$text_2nd,
                       fontfamily = font_sans,
                       fontsize = 11,
                       fontface = "bold")
    )
  )
  
  ars_grob <- gridExtra::tableGrob(arsenal_full, rows = NULL, theme = ars_theme)
  
  ars_grob$heights <- grid::unit(rep(1, nrow(ars_grob)), "null")
  
  # Slight horizontal padding
  ars_grob$widths <- ars_grob$widths * 0.95
  
  # Tighten Usage column
  usage_idx <- which(display_names == "Usage")
  if (length(usage_idx)) {
    ars_grob$widths[usage_idx] <- ars_grob$widths[usage_idx] * 0.99
  }
  
  return(ars_grob)
}

mockup_break_plot <- function(game) {
  game <- game %>% mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType))
  # getBrewStuff flips HorzBreak sign for lefties (needed as a model input); undo
  # it here only for the movement plot so LHP show their true horizontal break.
  if ("PitcherThrows" %in% names(game)) {
    game <- game %>% mutate(
      HorzBreak = ifelse(PitcherThrows == "Left", -HorzBreak, HorzBreak))
  }
  avg_break <- game %>%
    group_by(TaggedPitchType) %>%
    summarise(HorzBreak = mean(HorzBreak, na.rm = TRUE),
              InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
              .groups = "drop")

  pitcher_hand <- {
    h <- unique(game$PitcherThrows); h <- h[!is.na(h)][1]
    if (is.null(h)) NA_character_ else h
  }
  fac <- if (!is.na(pitcher_hand) && pitcher_hand == "Left") -1 else 1
  arm_len <- 32

  arm_lines <- if ("armangle" %in% names(game)) {
    game %>%
      group_by(TaggedPitchType) %>%
      summarise(arm = mean(armangle, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(arm)) %>%
      mutate(angle_rad = arm * (pi / 180),
             x_end = arm_len * cos(angle_rad) * fac,
             y_end = arm_len * sin(angle_rad))
  } else NULL

  arm_overall <- if ("armangle" %in% names(game)) {
    suppressWarnings(mean(game$armangle, na.rm = TRUE))
  } else NaN
  arm_subtitle <- if (is.finite(arm_overall)) {
    sprintf("Est. Arm Angle: %.1f°", round(arm_overall, 1))
  } else "Est. Arm Angle: --"

  p <- ggplot(game, aes(HorzBreak, InducedVertBreak, colour = TaggedPitchType)) +
    geom_vline(xintercept = 0, colour = tok$zero_axis,
               linetype = "dotted", linewidth = 0.4) +
    geom_hline(yintercept = 0, colour = tok$zero_axis,
               linetype = "dotted", linewidth = 0.4) +
    geom_point(aes(fill = TaggedPitchType),
               shape = 21, size = 3, stroke = 0.4,
               colour = "black", alpha = 0.85) +
    geom_point(data = avg_break, aes(fill = TaggedPitchType),
               shape = 21, size = 5.5, stroke = 1.4, colour = "white")

  if (!is.null(arm_lines) && nrow(arm_lines) > 0) {
    p <- p + geom_segment(data = arm_lines,
                          aes(x = 0, y = 0, xend = x_end, yend = y_end,
                              colour = TaggedPitchType),
                          linetype = "dashed", linewidth = 0.9,
                          inherit.aes = FALSE)
  }

  p +
    scale_colour_manual(values = pal_for(game$TaggedPitchType)) +
    scale_fill_manual(values = pal_for(game$TaggedPitchType)) +
    scale_x_continuous(breaks = seq(-20, 20, 10)) +
    scale_y_continuous(breaks = seq(-20, 20, 10)) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(title = "PITCH MOVEMENT", subtitle = arm_subtitle,
         x = "HB", y = "IVB") +
    theme_wildcats() +
    theme(
      # Extra left padding shifts the panel rightward so it sits between the
      # location plots and the usage plot more comfortably.
      plot.margin   = margin(12, 12, 12, 60),
      plot.title    = element_text(colour = tok$text_2nd, size = 13,
                                   face = "bold", hjust = 0.5,
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(colour = tok$text_2nd, size = 10,
                                   face = "italic", hjust = 0.5,
                                   margin = margin(b = 6)),
      axis.title.x  = element_text(colour = tok$text_2nd, size = 11,
                                   face = "bold",
                                   margin = margin(t = 4)),
      axis.title.y  = element_text(colour = tok$text_2nd, size = 11,
                                   face = "bold",
                                   margin = margin(r = 4)),
      plot.title.position = "panel"
    )
}

mockup_plate_plot <- function(game, side) {
  game <- game %>% mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType))

  # Outcome marker: hard-hit balls in play (95+ EV, no fouls) = diamond,
  # whiffs = square, everything else = circle. Outcome markers are drawn larger
  # than ordinary pitch dots so they stand out.
  has_ev    <- "ExitSpeed" %in% names(game)
  has_whiff <- "Whiff" %in% names(game)
  has_bbe   <- "BBE" %in% names(game)
  game <- game %>%
    mutate(
      .whiff   = if (has_whiff) (!is.na(Whiff) & Whiff) else FALSE,
      .hardhit = if (has_ev && has_bbe)
                   (BBE > 0 & !is.na(ExitSpeed) & ExitSpeed >= 95) else FALSE,
      outcome  = factor(
        case_when(.hardhit ~ "Hard Hit", .whiff ~ "Whiff", TRUE ~ "Other"),
        levels = c("Other", "Whiff", "Hard Hit")
      )
    )

  batter_img <- if (side == "L") left_batter else right_batter
  home_plate <- data.frame(
    x = c(0.6, -0.6, -0.7083, 0, 0.7083),
    y = c(0.5, 0.5, 0.25, 0, 0.25)
  )
  p <- ggplot(game, aes(-PlateLocSide, PlateLocHeight, colour = TaggedPitchType))
  if (!is.null(batter_img)) {
    if (side == "L") {
      p <- p + annotation_custom(rasterGrob(batter_img,
                                            width  = unit(0.65, "npc"),
                                            height = unit(1.4,  "npc")),
                                 xmin = 0.7, xmax = 3.0, ymin = 1, ymax = 5)
    } else {
      p <- p + annotation_custom(rasterGrob(batter_img,
                                            width  = unit(0.65, "npc"),
                                            height = unit(1.4,  "npc")),
                                 xmin = -3.0, xmax = -0.7, ymin = 1, ymax = 5)
    }
  }
  p +
    # Ordinary pitches first, then plate + strike zone, then the whiff / hard-hit
    # markers on top so they sit in front of the zone and the other dots.
    geom_point(data = ~ dplyr::filter(.x, outcome == "Other"),
               aes(fill = TaggedPitchType, shape = outcome, size = outcome,
                   stroke = outcome),
               colour = "black", alpha = 0.9) +
    geom_polygon(data = home_plate, aes(x, y),
                 fill = NA, colour = "#5F5F6B", linewidth = 0.7,
                 inherit.aes = FALSE) +
    annotate("rect", xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.6,
             fill = NA, colour = "#36363F", linewidth = 0.8) +
    geom_point(data = ~ dplyr::filter(.x, outcome %in% c("Whiff", "Hard Hit")),
               aes(fill = TaggedPitchType, shape = outcome, size = outcome,
                   stroke = outcome),
               colour = "black", alpha = 0.9) +
    scale_colour_manual(values = pal_for(game$TaggedPitchType)) +
    scale_fill_manual(values = pal_for(game$TaggedPitchType)) +
    # Only Whiff / Hard Hit appear in the legend; ordinary pitches stay circles.
    # Explicit limits keep all three levels in the domain so the Hard Hit /
    # Whiff legend keys always render, even when the data has none of them.
    scale_shape_manual(values = c("Other" = 21, "Whiff" = 23, "Hard Hit" = 22),
                       limits = c("Other", "Whiff", "Hard Hit"),
                       breaks = c("Hard Hit", "Whiff"), name = NULL,
                       drop = FALSE) +
    scale_size_manual(values = c("Other" = 2.6, "Whiff" = 4.6, "Hard Hit" = 4.6),
                      limits = c("Other", "Whiff", "Hard Hit"),
                      guide = "none", drop = FALSE) +
    scale_discrete_manual(aesthetics = "stroke",
                          values = c("Other" = 0.4, "Whiff" = 0.8, "Hard Hit" = 0.8),
                          limits = c("Other", "Whiff", "Hard Hit"),
                          guide = "none", drop = FALSE) +
    # Legend is drawn separately as its own card row (markers_legend_grob), so
    # the narrow location panel doesn't clip it.
    guides(fill = "none", colour = "none", shape = "none") +
    coord_fixed() +
    xlim(-3, 3) + ylim(0, 5) +
    labs(title = sprintf("PITCH LOCATION  ·  %sHB", side),
         subtitle = "Catcher's View", x = NULL, y = NULL) +
    theme_void(base_family = font_sans) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      legend.position  = "none",
      plot.title = element_text(colour = "#5F5F6B", size = 13,
                                face = "bold", hjust = 0,
                                margin = margin(b = 1, l = 8)),
      plot.subtitle = element_text(colour = "#8B8B96", size = 9,
                                   face = "italic", hjust = 0,
                                   margin = margin(b = 4, l = 8)),
      plot.margin = margin(8, 8, 8, 8)
    )
}

mockup_usage_plot <- function(game) {
  usage_df <- game %>%
    mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType)) %>%
    filter(BatterSide %in% c("Left", "Right")) %>%
    count(TaggedPitchType, BatterSide, name = "count") %>%
    rename(pitch_name = TaggedPitchType, side = BatterSide) %>%
    mutate(side = ifelse(side == "Left", "LHB", "RHB")) %>%
    group_by(side) %>%
    mutate(
      pct    = count / sum(count) * 100,
      signed = ifelse(side == "LHB", -pct, pct)
    ) %>%
    ungroup()

  if (nrow(usage_df) == 0) {
    return(ggplot() + theme_void() + labs(title = "USAGE  ·  L / R"))
  }

  pitch_order <- usage_df %>%
    group_by(pitch_name) %>%
    summarise(total = sum(count), .groups = "drop") %>%
    arrange(-total) %>%
    pull(pitch_name)
  usage_df <- usage_df %>%
    mutate(pitch_name = factor(pitch_name, levels = rev(pitch_order)))

  x_lim <- 100
  ggplot(usage_df, aes(signed, pitch_name, fill = pitch_name)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = sprintf("%.0f%%", pct), x = signed,
                  hjust = ifelse(pct > 90,
                                 ifelse(side == "LHB", -0.1, 1.1),
                                 ifelse(side == "LHB",  1.1, -0.1))),
              family = font_mono, size = 3.6,
              colour = "#36363F", fontface = "bold") +
    scale_fill_manual(values = pal_for(usage_df$pitch_name)) +
    scale_x_continuous(
      limits = c(-x_lim, x_lim), expand = expansion(mult = 0.06),
      breaks = seq(-100, 100, 25),
      labels = function(b) paste0(abs(b), "%")
    ) +
    geom_vline(xintercept = 0, colour = "#FFFFFF", linewidth = 1.4) +
    annotate("text", x = -x_lim/2, y = length(pitch_order) + 0.8,
             label = "LHB",
             family = font_sans, fontface = "bold", size = 4.2,
             colour = "#5F5F6B") +
    annotate("text", x =  x_lim/2, y = length(pitch_order) + 0.8,
             label = "RHB",
             family = font_sans, fontface = "bold", size = 4.2,
             colour = "#5F5F6B") +
    coord_cartesian(clip = "off") +
    labs(title = "USAGE  ·  L / R", x = NULL, y = NULL) +
    theme_wildcats() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "#EAEAEE", linewidth = 0.4),
      axis.text.x        = element_text(colour = "#5F5F6B",
                                        family = font_mono, size = 10,
                                        face = "bold"),
      axis.text.y        = element_blank(),
      axis.ticks         = element_blank(),
      plot.title         = element_text(colour = "#5F5F6B", size = 13,
                                        face = "bold", hjust = 0.5,
                                        margin = margin(b = 18))
    )
}

# Mockup-format arsenal: per-pitch row + Total row, with Stuff+ vs L / vs R / overall
arsenal_summary <- function(game, height_override = NULL, set_override = NULL) {
  game <- getBrewStuff(game, model, FALSE,
                       height_override = height_override,
                       set_override = set_override) %>%
    mutate(Stuff = ifelse(Stuff < 50, NA, Stuff),
           TaggedPitchType = canonicalize_pitch(TaggedPitchType))

  per_pitch <- game %>%
    group_by(TaggedPitchType) %>%
    summarise(
      N        = n(),
      Usage    = sprintf("%.1f%%(%d)", N / nrow(game) * 100, N),
      FPS      = sprintf("%.1f%%", mean(Strike[PitchofPA == 1], na.rm = TRUE) * 100),
      Velo     = sprintf("%.1f", round(mean(RelSpeed, na.rm = TRUE), 1)),
      Top      = sprintf("%.1f", round(max(RelSpeed, na.rm = TRUE), 1)),
      Spin     = format(round(mean(SpinRate, na.rm = TRUE)), big.mark = ","),
      IVB      = sprintf("%.1f", round(mean(InducedVertBreak, na.rm = TRUE), 1)),
      HB       = sprintf("%.1f", round(mean(HorzBreak, na.rm = TRUE), 1)),
      RelH     = sprintf("%.1f", round(mean(RelHeight, na.rm = TRUE), 1)),
      RelS     = sprintf("%.1f", round(mean(RelSide, na.rm = TRUE), 1)),
      Ext      = sprintf("%.1f", round(mean(Extension, na.rm = TRUE), 1)),
      AA       = sprintf("%.1f°", round(mean(armangle, na.rm = TRUE), 1)),
      CSW      = sprintf("%.1f%%", (sum(Whiff, na.rm = TRUE) +
                                    sum(CallStrike, na.rm = TRUE)) / N * 100),
      Whiff    = sprintf("%.1f%%", sum(Whiff, na.rm = TRUE) /
                                   max(sum(Swing, na.rm = TRUE), 1) * 100),
      Chase    = sprintf("%.1f%%", sum(Swing[!Zone], na.rm = TRUE) /
                                   max(sum(!Zone, na.rm = TRUE), 1) * 100),
      StuffPvL = as.character(round(mean(Stuff[BatterSide == "Left"],  na.rm = TRUE), 0)),
      StuffPvR = as.character(round(mean(Stuff[BatterSide == "Right"], na.rm = TRUE), 0)),
      StuffP   = as.character(round(mean(Stuff, na.rm = TRUE), 0)),
      .groups  = "drop"
    ) %>%
    arrange(-N) %>%
    select(Type = TaggedPitchType, Usage, FPS, Velo, Top, Spin,
           IVB, HB, RelH, RelS, Ext, AA, CSW, Whiff, Chase, StuffPvL, StuffPvR, StuffP)

  total <- game %>%
    summarise(
      Type     = "Total",
      Usage    = sprintf("100.0%%(%d)", n()),
      FPS      = sprintf("%.1f%%", mean(Strike[PitchofPA == 1], na.rm = TRUE) * 100),
      Velo     = "—",
      Top      = sprintf("%.1f", round(max(RelSpeed, na.rm = TRUE), 1)),
      Spin     = "—",
      IVB      = "—",
      HB       = "—",
      RelH     = sprintf("%.1f", round(mean(RelHeight, na.rm = TRUE), 1)),
      RelS     = sprintf("%.1f", round(mean(RelSide, na.rm = TRUE),  1)),
      Ext      = sprintf("%.1f", round(mean(Extension, na.rm = TRUE), 1)),
      AA       = sprintf("%.1f°", round(mean(armangle, na.rm = TRUE), 1)),
      CSW      = sprintf("%.1f%%", (sum(Whiff, na.rm = TRUE) +
                                    sum(CallStrike, na.rm = TRUE)) / n() * 100),
      Whiff    = sprintf("%.1f%%", sum(Whiff, na.rm = TRUE) /
                                   max(sum(Swing, na.rm = TRUE), 1) * 100),
      Chase    = sprintf("%.1f%%", sum(Swing[!Zone], na.rm = TRUE) /
                                   max(sum(!Zone, na.rm = TRUE), 1) * 100),
      StuffPvL = as.character(round(mean(Stuff[BatterSide == "Left"],  na.rm = TRUE), 0)),
      StuffPvR = as.character(round(mean(Stuff[BatterSide == "Right"], na.rm = TRUE), 0)),
      StuffP   = as.character(round(mean(Stuff, na.rm = TRUE), 0))
    )

  bind_rows(per_pitch, total) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ ifelse(
      is.na(.x) | .x %in% c("NA", "NaN", "—") |
        grepl("NaN|Inf", .x, ignore.case = FALSE),
      "--", .x)))
}

# ============================================================================
# End mockup card design
# ============================================================================


boxscore_summary <- function(game) {

  # ExitSpeed is optional in the source data; HardHit% falls back to "--".
  has_ev <- "ExitSpeed" %in% names(game)

  game_stats <- game %>%
    # mutate(
    #   Hit = PlayResult %in% c("Single","Double","Triple","HomeRun"),
    #   Whiff = PitchCall %in% c("swinging_strike", "swinging_strike_blocked"),
    #   Contact = PitchCall %in% c("foul", "foul_tip", "foul_bunt"),
    #   Single = PlayResult == "Single",
    #   Double = PlayResult == "Double",
    #   Triple = PlayResult == "Triple",
    #   HR = PlayResult == "HomeRun",
    #   Sac = PlayResult == "Sacrifice",
    #   Error = PlayResult == "Error",
    #   FC = PlayResult == "FieldersChoice",
    #   Out = PlayResult == "Out",
    #   BIP = Single | Double | Triple | HR | Sac | Error | Out | FC,
    #   Strikeout = KorBB == "Strikeout",
    #   Walk = KorBB == "Walk",
    #   AB = Strikeout | BIP & !Sac
    # ) %>%
    group_by(Pitcher) %>%
    summarize(
      total_outs = (sum(OutsOnPlay, na.rm = TRUE) + sum(Strikeout, na.rm = TRUE)),
      IP      = floor(total_outs / 3) + ((total_outs %% 3) / 10),
      AB      = sum(AB, na.rm = TRUE),
      Hits    = sum(Hit, na.rm = TRUE),
      K       = sum(Strikeout, na.rm = TRUE),
      BB      = sum(Walk, na.rm = TRUE),
      `K/BB`  = paste0(K, "/", BB),
      bbe     = sum(BBE, na.rm = TRUE),
      gb      = sum(GB,  na.rm = TRUE),
      `GB%`   = ifelse(bbe > 0,
                       sprintf("%.1f%%", gb / bbe * 100),
                       "--"),
      n_pitch = n(),
      `Zone%`   = ifelse(n_pitch > 0,
                         sprintf("%.1f%%", sum(Zone, na.rm = TRUE) / n_pitch * 100),
                         "--"),
      `Strike%` = ifelse(n_pitch > 0,
                         sprintf("%.1f%%", sum(Strike, na.rm = TRUE) / n_pitch * 100),
                         "--"),
      # Hard-hit = batted ball in play (no fouls) at 95+ mph EV, over all BBE.
      hardhit   = if (has_ev) sum(BBE > 0 & !is.na(ExitSpeed) & ExitSpeed >= 95, na.rm = TRUE) else 0,
      `HardHit%` = if (has_ev && bbe > 0)
                     sprintf("%.1f%%", hardhit / bbe * 100) else "--"
    ) %>%
    select(Pitcher, IP, AB, Hits, `K/BB`, `GB%`, `Zone%`, `Strike%`, `HardHit%`) %>%
    rename("Pitcher ID" = Pitcher)
  
  return(game_stats)
}


# ============================================================================
# Card assembly (extracted from the standalone app's observeEvent so the BASE
# pitcher tab can reuse it). Returns a grid gTree ("page") ready to draw.
#   game            : one pitcher's pitches, already run through pitcher_summary(),
#                     canonicalize_pitch(), with a row_id column (current_pitches()).
#   pitcher_display : pre-formatted display name (e.g. "First Last").
# ============================================================================
build_pitcher_card_page <- function(game, pitcher_display,
                                     height_override = NULL, set_override = NULL) {

  # Stuff+ enrichment for charts (table builder runs this internally too)
  game_with_stuff <- getBrewStuff(game, model, FALSE,
                                  height_override = height_override,
                                  set_override = set_override) %>%
    mutate(Stuff = ifelse(Stuff < 50, NA, Stuff),
           TaggedPitchType = canonicalize_pitch(TaggedPitchType))

  arsenal_full <- arsenal_summary(game, height_override = height_override,
                                  set_override = set_override)

  # Subtitle: throwing hand · (vs. opponent | date range) · pitch count
  throws_label <- {
    th <- unique(game$PitcherThrows)
    th <- th[!is.na(th)][1]
    if (is.null(th) || is.na(th)) "" else paste0(toupper(substr(th, 1, 1)), "HP")
  }

  dates <- if ("Date" %in% names(game)) {
    sort(unique(suppressWarnings(as.Date(game$Date[!is.na(game$Date)]))))
  } else as.Date(character(0))

  n_games <- if ("GameUID" %in% names(game)) {
    length(unique(game$GameUID[!is.na(game$GameUID)]))
  } else length(dates)

  pitcher_team_abbr <- {
    pt <- unique(game$PitcherTeam); pt <- pt[!is.na(pt)][1]
    if (is.null(pt)) "" else pt
  }
  pal <- team_palette(pitcher_team_abbr)

  opp_str <- {
    ot <- unique(game$BatterTeam); ot <- ot[!is.na(ot)][1]
    if (is.null(ot) || is.na(ot)) "" else team_palette(ot)$label
  }

  when_str <- if (n_games == 1 && nzchar(opp_str)) {
    if (length(dates) == 1) {
      paste0(format_pretty_date(dates[1]), "  ·  vs. ", opp_str)
    } else paste0("vs. ", opp_str)
  } else if (length(dates) >= 2) {
    paste0("from ", format_pretty_date(min(dates)),
           " to ",  format_pretty_date(max(dates)))
  } else if (length(dates) == 1) {
    format_pretty_date(dates[1])
  } else ""

  parts <- c(throws_label, when_str, paste0(nrow(game), " pitches"))
  subtitle <- paste(parts[nzchar(parts)], collapse = "  ·  ")

  team_logo <- fetch_team_logo(pitcher_team_abbr, pal$logo_url)

  header_g <- header_grob_fn(pitcher_display, subtitle,
                             team_label   = pal$label,
                             banner_color = pal$primary,
                             accent_color = pal$secondary,
                             logo         = team_logo)

  boxscore_data <- boxscore_summary(game)
  if ("Pitcher ID" %in% names(boxscore_data)) {
    boxscore_data <- boxscore_data %>% select(-`Pitcher ID`)
  }
  box_g <- box_grob_fn(boxscore_data)

  break_p <- mockup_break_plot(game_with_stuff)
  lhb_p   <- mockup_plate_plot(game_with_stuff %>% filter(BatterSide == "Left"),  "L")
  rhb_p   <- mockup_plate_plot(game_with_stuff %>% filter(BatterSide == "Right"), "R")
  usage_p <- mockup_usage_plot(game_with_stuff)

  arsenal_g <- build_arsenal_grob(arsenal_full, percentiledata)
  markers_g <- markers_legend_grob()
  footer_g  <- footer_grob_fn()

  plate_col <- arrangeGrob(lhb_p, rhb_p, ncol = 1, heights = c(1, 1))
  main_row  <- arrangeGrob(plate_col, break_p, usage_p, ncol = 3, widths = c(2, 5, 3))

  card <- arrangeGrob(
    header_g, box_g, main_row, markers_g, arsenal_g, footer_g,
    ncol = 1,
    heights = c(0.115, 0.080, 0.440, 0.030, 0.310, 0.025)
  )

  gTree(children = gList(
    rectGrob(gp = gpar(fill = tok$bg_page, col = NA)),
    card
  ))
}

# Draw a built card page to a PNG file. dpi/res default to screen-preview; pass
# dpi = 300, res = 300, units = "in", width/height = 12.5 for download quality.
draw_card_to_png <- function(page, file,
                             width = 1200, height = 1200, units = "px",
                             res = 96, dpi = 96) {
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_opts(dpi = dpi)
  grDevices::png(file, width = width, height = height, units = units,
                 res = res, bg = tok$bg_page, type = "cairo")
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_begin()
  on.exit({
    if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_end()
    grDevices::dev.off()
  }, add = TRUE, after = FALSE)
  grid::grid.draw(page)
  invisible(file)
}

# Draw one or more built card pages to a multi-page PDF (one card per page).
# Pages are square to match the 1:1 card layout (e.g. game card, then season card).
draw_cards_to_pdf <- function(pages, file, width = 12.5, height = 12.5, dpi = 300) {
  pages <- Filter(Negate(is.null), pages)
  if (length(pages) == 0) return(invisible(file))
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_opts(dpi = dpi)
  grDevices::cairo_pdf(file, width = width, height = height,
                       onefile = TRUE, bg = tok$bg_page)
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_begin()
  on.exit({
    if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_end()
    grDevices::dev.off()
  }, add = TRUE, after = FALSE)
  for (i in seq_along(pages)) {
    if (i > 1) grid::grid.newpage()
    grid::grid.draw(pages[[i]])
  }
  invisible(file)
}

# ============================================================================
# CARD TAB UI + SERVER (inlined — was pitcher_card_tab.R)
# ============================================================================
# ============================================================================
# BASE "Postgame Pitcher Reports" tab — BrewSummaryCard, embedded.
#
# Provides:
#   pitcher_card_ui()                         -> the page UI (drop into page_content)
#   pitcher_card_server(input, output, session) -> the server logic (call once)
#
# All input/output IDs are prefixed `pc_` so they don't collide with the BASE
# app's existing pitcher-report inputs. Depends on pitcher_report_card.R being
# sourced first (model, pitcher_summary, build_pitcher_card_page, tok, etc.).
# ============================================================================
library(plotly)

pitcher_card_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-report-page",
      tags$h2("Postgame Pitcher Report Card",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 16px;"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxInput("pc_manual_enabled", "Upload single-game CSV", value = FALSE),
          conditionalPanel(
            condition = "input.pc_manual_enabled",
            fileInput("pc_manual_csv", "Game CSV:", accept = c(".csv", ".parquet"),
                      buttonLabel = "Browse", placeholder = "No file selected"),
            helpText("Appended to the configured season data for this session.")
          ),
          uiOutput("pc_team_ui"),
          uiOutput("pc_dates_ui"),
          uiOutput("pc_pitcher_ui"),
          radioButtons("pc_pitch_src", "Pitch Type Source:",
                       choices = c("Tagged" = "tagged", "Auto (backup)" = "auto"),
                       selected = "tagged", inline = TRUE),
          tags$label("Enter Pitcher Height (only used for pitchers not in the height CSV)",
                     style = "font-weight:bold; margin-top:8px;"),
          helpText("Arm angle = atan2(RelHeight - shoulder, RelSide), shoulder = 70% of height. ",
                   "Without a height the arm-angle estimate (and Stuff+) can't be computed, so set ",
                   "it here for pitchers missing from the height CSV."),
          fluidRow(
            column(6, numericInput("pc_height_ft", "ft", value = NA,
                                   min = 4, max = 8, step = 1)),
            column(6, numericInput("pc_height_in", "in", value = NA,
                                   min = 0, max = 11, step = 1))
          ),
          tags$label("Set Position on Rubber (overrides height CSV)",
                     style = "font-weight:bold; margin-top:8px;"),
          helpText("Feet from rubber center: -1 = toward 1B, 0 = middle, +1 = toward 3B. ",
                   "Leave at 0 to use the coded value (if any)."),
          tags$div(
            style = "margin-top:6px;",
            tags$div(
              style = "position:relative; height:40px; padding:0 4px;",
              tags$div(style = paste(
                "position:relative; width:100%; height:34px; top:50%;",
                "transform:translateY(-50%); background:#fff;",
                "border:1px solid #444; border-radius:2px;",
                "display:flex; align-items:center; justify-content:space-between;",
                "padding:0 8px; box-sizing:border-box;"
              ),
                tags$span("← 1B", style = "font-size:12px; font-weight:bold; color:#333; white-space:nowrap;"),
                tags$span("3B →", style = "font-size:12px; font-weight:bold; color:#333; white-space:nowrap;")
              ),
              tags$div(style = paste(
                "position:absolute; left:50%; top:50%; width:1px; height:38px;",
                "background:#444; transform:translate(-50%, -50%);"
              ))
            ),
            sliderInput("pc_set_pos", NULL, min = -1, max = 1, value = 0, step = 0.1,
                        ticks = FALSE)
          ),
          actionButton("pc_update1", "Make/Update Card", icon("plus"),
                       class = "btn-success btn-block"),
          actionButton("pc_reset_pitches", "Reset Pitch Tags",
                       class = "btn-secondary btn-block"),
          downloadButton("pc_downloadPlot", "Download PDF (Game + Season)",
                         class = "btn-info btn-block"),
          downloadButton("pc_downloadPng", "Download PNG (1:1)",
                         class = "btn-info btn-block")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Card",
                     div(style = "max-width: 900px; width: 100%; margin: 0 auto;",
                         imageOutput("pc_combinedPlot", width = "100%", height = "auto"))),
            tabPanel("Pitch Retag",
                     fluidRow(
                       column(4, selectizeInput("pc_filter_pitch", "Filter to Pitch Types:",
                                                choices = c("All"), multiple = TRUE,
                                                options = list(plugins = list("remove_button")))),
                       column(4, selectInput("pc_new_pitch_type", "New Pitch Type:",
                                             choices = c("Four-Seam","Sinker","Cutter","Slider",
                                                         "Sweeper","Curveball","Knuckle Curve",
                                                         "Slurve","Changeup","Splitter",
                                                         "Knuckle Ball","Eephus","Slow Curve",
                                                         "Screwball","Two-Seam Fastball","Fastball"))),
                       column(4, br(),
                              actionButton("pc_apply_retag", "Apply To Selected",
                                           class = "btn-primary btn-block"),
                              actionButton("pc_delete_pitches", "Delete Selected",
                                           class = "btn-danger btn-block"))
                     ),
                     fluidRow(
                       column(3, sliderInput("pc_filter_velo", "Velo (mph)",
                                             min = 40, max = 110, value = c(40, 110), step = 0.5)),
                       column(3, sliderInput("pc_filter_spin", "Spin (rpm)",
                                             min = 0, max = 4500, value = c(0, 4500), step = 50)),
                       column(3, sliderInput("pc_filter_ivb", "IVB (in)",
                                             min = -35, max = 35, value = c(-35, 35), step = 0.5)),
                       column(3, sliderInput("pc_filter_hb", "HB (in)",
                                             min = -35, max = 35, value = c(-35, 35), step = 0.5))
                     ),
                     helpText("Lasso-select pitches on the plot, then Apply To Selected (retag) or ",
                              "Delete Selected. Sliders narrow the visible pitches. Click Make Card to redraw."),
                     textOutput("pc_selection_info"),
                     plotlyOutput("pc_retag_plot", height = "650px"),
                     tags$hr(),
                     fluidRow(
                       column(8, selectInput("pc_delete_class", "Delete Whole Pitch Class:",
                                             choices = character(0))),
                       column(4, br(),
                              actionButton("pc_delete_class_btn", "Delete Class",
                                           class = "btn-danger btn-block"))
                     ),
                     helpText("Removes every pitch with the selected tag."))
          )
        )
      )
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}

pitcher_card_server <- function(input, output, session) {

  # Configured team season, optionally with this page's manual single-game CSV appended.
  master_data <- reactive({
    combine_with_manual(season_data, input$pc_manual_enabled, input$pc_manual_csv)
  })

  current_pitches  <- reactiveVal(NULL)
  selected_points  <- reactiveVal(NULL)
  card_page        <- reactiveVal(NULL)
  season_card_page <- reactiveVal(NULL)

  # ft + in -> decimal feet; NULL when both blank so the override is skipped.
  height_override_dec <- reactive({
    ft   <- suppressWarnings(as.numeric(input$pc_height_ft))
    inch <- suppressWarnings(as.numeric(input$pc_height_in))
    if (is.na(ft) && is.na(inch)) return(NULL)
    if (is.na(ft)) ft <- 0
    if (is.na(inch)) inch <- 0
    val <- ft + inch / 12
    if (!is.finite(val) || val <= 0) NULL else val
  })

  set_override_val <- reactive({
    v <- suppressWarnings(as.numeric(input$pc_set_pos))
    if (length(v) == 0 || is.na(v) || !is.finite(v) || v == 0) NULL else v
  })

  # Team / date / pitcher selectors driven by the shared master_data
  # (configured season + optional manual single-game CSV) — same source as
  # the hitter reports.
  output$pc_team_ui <- renderUI({
    if (is.null(master_data()) || nrow(master_data()) == 0) return(base_data_notice())
    teams <- base_team_choices(master_data()$PitcherTeam)
    if (!length(teams)) return(base_data_notice())
    selectInput("pc_team", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = base_team_default(teams))
  })

  output$pc_dates_ui <- renderUI({
    req(!is.null(master_data()), input$pc_team)
    dates <- master_data() %>%
      filter(PitcherTeam == input$pc_team) %>%
      mutate(d = as.Date(as.character(Date))) %>%
      pull(d) %>% unique() %>% sort(decreasing = TRUE)
    selectInput("pc_dates", "Select Date(s):",
                choices  = as.character(dates),
                selected = as.character(dates[1]),
                multiple = TRUE, selectize = TRUE)
  })

  output$pc_pitcher_ui <- renderUI({
    req(!is.null(master_data()), input$pc_team, input$pc_dates)
    pitchers <- master_data() %>%
      filter(PitcherTeam == input$pc_team,
             as.character(as.Date(as.character(Date))) %in% input$pc_dates) %>%
      pull(Pitcher) %>% unique() %>% sort()
    req(length(pitchers) > 0)
    selectInput("pc_pitcher", "Select Pitcher:",
                choices = setNames(pitchers, format_pitcher_name(pitchers)))
  })

  load_pitcher_pitches <- function() {
    req(!is.null(master_data()), input$pc_team, input$pc_dates, input$pc_pitcher)
    pp <- master_data() %>%
      filter(PitcherTeam == input$pc_team,
             as.character(as.Date(as.character(Date))) %in% input$pc_dates,
             Pitcher == input$pc_pitcher) %>%
      apply_pitch_source(input$pc_pitch_src) %>%
      pitcher_summary() %>%
      mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType),
             row_id = row_number())
    current_pitches(pp)
    selected_points(NULL)
  }

  # Full-season pitches for the selected pitcher, used
  # for the season page of the PDF. Independent of the date selector and of any
  # retag/delete edits applied to the game card.
  load_pitcher_pitches_season <- function() {
    req(!is.null(master_data()), input$pc_team, input$pc_pitcher)
    master_data() %>%
      filter(PitcherTeam == input$pc_team, Pitcher == input$pc_pitcher) %>%
      apply_pitch_source(input$pc_pitch_src) %>%
      pitcher_summary() %>%
      mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType),
             row_id = row_number())
  }

  observeEvent(list(input$pc_pitcher, input$pc_dates, input$pc_pitch_src),
               { load_pitcher_pitches() }, ignoreInit = TRUE)
  observeEvent(input$pc_reset_pitches, { load_pitcher_pitches() })

  # Keep filter + delete-class dropdowns in sync with the pitcher's pitch types.
  observe({
    d <- current_pitches()
    cur <- isolate(input$pc_filter_pitch)
    if (is.null(d) || nrow(d) == 0) {
      updateSelectizeInput(session, "pc_filter_pitch", choices = c("All"),
                           selected = character(0))
      updateSelectInput(session, "pc_delete_class", choices = character(0))
      return()
    }
    types <- sort(unique(d$TaggedPitchType))
    keep  <- cur[cur %in% c("All", types)]
    updateSelectizeInput(session, "pc_filter_pitch",
                         choices = c("All", types), selected = keep)
    updateSelectInput(session, "pc_delete_class", choices = types)
  })

  output$pc_retag_plot <- plotly::renderPlotly({
    d <- current_pitches()
    req(d)
    flt <- input$pc_filter_pitch
    if (length(flt) > 0 && !"All" %in% flt) d <- d %>% filter(TaggedPitchType %in% flt)
    in_range <- function(x, rng) {
      if (length(rng) != 2) return(rep(TRUE, length(x)))
      is.na(x) | (x >= rng[1] & x <= rng[2])
    }
    d <- d %>% filter(
      in_range(RelSpeed,         input$pc_filter_velo),
      in_range(SpinRate,         input$pc_filter_spin),
      in_range(InducedVertBreak, input$pc_filter_ivb),
      in_range(HorzBreak,        input$pc_filter_hb)
    )
    if (nrow(d) == 0) {
      return(plotly::plot_ly() %>% plotly::layout(title = "No pitches match the current filter."))
    }
    spin_str <- ifelse(is.na(d$SpinRate), "--", format(round(d$SpinRate), big.mark = ","))
    tilt_str <- if ("Tilt" %in% names(d)) ifelse(is.na(d$Tilt) | d$Tilt == "", "--", as.character(d$Tilt)) else rep("--", nrow(d))
    hover_text <- paste0(
      "Pitch: ", d$TaggedPitchType,
      "<br>Velo: ", sprintf("%.1f", d$RelSpeed),
      "<br>Spin: ", spin_str, " rpm",
      "<br>Tilt: ", tilt_str,
      "<br>IVB: ",  sprintf("%.1f", d$InducedVertBreak),
      "<br>HB: ",   sprintf("%.1f", d$HorzBreak),
      "<br>#: ",    d$row_id
    )
    plotly::plot_ly(
      data = d, x = ~HorzBreak, y = ~InducedVertBreak,
      color = ~TaggedPitchType, colors = pal_for(d$TaggedPitchType),
      customdata = ~row_id, text = hover_text, hoverinfo = "text",
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.85), source = "pc_retagplot"
    ) %>% plotly::layout(
      title = "Pitch Movement (lasso select to highlight pitches)",
      dragmode = "lasso",
      xaxis = list(title = "Horizontal Break (in)", range = c(-35, 35),
                   zerolinecolor = "rgba(0,0,0,0.25)"),
      yaxis = list(title = "Induced Vertical Break (in)", range = c(-35, 35),
                   scaleanchor = "x", scaleratio = 1, zerolinecolor = "rgba(0,0,0,0.25)")
    ) %>% plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("autoScale2d","hoverClosestCartesian",
                                 "hoverCompareCartesian","toggleSpikelines")
      ) %>% plotly::event_register("plotly_selected")
  })

  observe({
    ed <- tryCatch(suppressWarnings(plotly::event_data("plotly_selected", source = "pc_retagplot")),
                   error = function(e) NULL)
    if (is.null(ed) || !is.data.frame(ed) || nrow(ed) == 0 || is.null(ed$customdata)) {
      selected_points(NULL); return()
    }
    ids <- suppressWarnings(as.integer(ed$customdata))
    ids <- ids[!is.na(ids)]
    selected_points(if (length(ids)) ids else NULL)
  })

  output$pc_selection_info <- renderText({
    s <- selected_points()
    if (is.null(s) || length(s) == 0) "No pitches selected. Use the box/lasso tool on the plot."
    else paste(length(s), "pitches selected.")
  })

  observeEvent(input$pc_apply_retag, {
    req(current_pitches(), input$pc_new_pitch_type)
    s <- selected_points()
    if (is.null(s) || length(s) == 0) {
      showNotification("Select pitches on the plot first.", type = "warning"); return()
    }
    d <- current_pitches()
    rows <- which(d$row_id %in% s)
    if (length(rows) == 0) return()
    d$TaggedPitchType[rows] <- input$pc_new_pitch_type
    current_pitches(d); selected_points(NULL)
    showNotification(sprintf("Retagged %d pitches as %s.", length(rows), input$pc_new_pitch_type),
                     type = "message")
  })

  observeEvent(input$pc_delete_pitches, {
    req(current_pitches())
    s <- selected_points()
    if (is.null(s) || length(s) == 0) {
      showNotification("Select pitches on the plot first.", type = "warning"); return()
    }
    d <- current_pitches()
    keep <- !(d$row_id %in% s)
    if (sum(!keep) == 0) return()
    current_pitches(d[keep, , drop = FALSE]); selected_points(NULL)
    showNotification(sprintf("Deleted %d pitches.", sum(!keep)), type = "message")
  })

  observeEvent(input$pc_delete_class_btn, {
    req(current_pitches(), input$pc_delete_class)
    d <- current_pitches()
    keep <- d$TaggedPitchType != input$pc_delete_class
    removed <- sum(!keep, na.rm = TRUE)
    if (removed == 0) {
      showNotification(sprintf("No pitches tagged %s.", input$pc_delete_class), type = "warning")
      return()
    }
    current_pitches(d[keep, , drop = FALSE]); selected_points(NULL)
    showNotification(sprintf("Deleted %d pitches tagged %s.", removed, input$pc_delete_class),
                     type = "message")
  })

  observeEvent(input$pc_update1, {
    req(current_pitches())
    game <- current_pitches()
    if (nrow(game) == 0) {
      showNotification("No data available for the selected pitcher.", type = "warning"); return()
    }
    pitcher_display <- format_pitcher_name(input$pc_pitcher)
    page <- build_pitcher_card_page(game, pitcher_display,
                                    height_override = height_override_dec(),
                                    set_override    = set_override_val())
    card_page(page)

    # Season card (all of this pitcher's configured-season pitches) for the PDF's 2nd page.
    season <- tryCatch(load_pitcher_pitches_season(), error = function(e) NULL)
    season_page <- if (!is.null(season) && nrow(season) > 0) {
      tryCatch(
        build_pitcher_card_page(season, pitcher_display,
                                height_override = height_override_dec(),
                                set_override    = set_override_val()),
        error = function(e) { message("season card error: ", e$message); NULL }
      )
    } else NULL
    season_card_page(season_page)
  })

  output$pc_combinedPlot <- renderImage({
    req(card_page())
    outfile <- tempfile(fileext = ".png")
    draw_card_to_png(card_page(), outfile,
                     width = 1200, height = 1200, units = "px", res = 96, dpi = 96)
    list(src = outfile, contentType = "image/png",
         width = "100%", height = "auto", alt = "Pitching Summary Card")
  }, deleteFile = TRUE)

  # 2-page PDF: page 1 = selected-game card, page 2 = full-season card.
  output$pc_downloadPlot <- downloadHandler(
    filename = function() {
      if (is.null(input$pc_pitcher)) return("Pitcher Report.pdf")
      opp <- most_common_opponent(current_pitches(), "BatterTeam")
      paste0(report_base_name(input$pc_pitcher, input$pc_dates, opp, "Pitcher"), ".pdf")
    },
    content = function(file) {
      req(card_page())
      draw_cards_to_pdf(list(card_page(), season_card_page()), file,
                        width = 12.5, height = 12.5, dpi = 300)
    }
  )

  # PNG export of the game card, square (1:1) at 300 dpi.
  output$pc_downloadPng <- downloadHandler(
    filename = function() {
      if (is.null(input$pc_pitcher)) return("Pitcher Report.png")
      opp <- most_common_opponent(current_pitches(), "BatterTeam")
      paste0(report_base_name(input$pc_pitcher, input$pc_dates, opp, "Pitcher"), ".png")
    },
    content = function(file) {
      req(card_page())
      draw_card_to_png(card_page(), file,
                       width = 12.5, height = 12.5, units = "in", res = 300, dpi = 300)
    }
  )
}

# ============================================================================
# SEASON PITCHER SUMMARY CARD tab (College26)
# ============================================================================
# Full-season summary card loaded on demand from the partitioned College26
# runtime dataset. Reuses the BrewSummaryCard engine
# (build_pitcher_card_page / draw_card_to_png / draw_cards_to_pdf). No date
# selector or retag flow — the card aggregates every pitch the selected pitcher
# threw in the College26 season. All IDs are prefixed `spc_`.
#   season_pitcher_card_ui()                         -> the page UI
#   season_pitcher_card_server(input, output, session) -> the server logic
# ============================================================================
season_pitcher_card_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-report-page",
      tags$h2("Season Pitcher Summary Card",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 16px;"),
      if (!nrow(base_pitcher_catalog))
        tags$div(class = "alert alert-warning",
                 "College season-card data is unavailable. Check BASE_RUNTIME_ROOT."),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("spc_team_ui"),
          uiOutput("spc_pitcher_ui"),
          checkboxInput(
            "spc_include_cape",
            "Include matched 2026 Cape Cod League pitches",
            value = TRUE
          ),
          uiOutput("spc_source_note"),
          radioButtons("spc_pitch_src", "Pitch Type Source:",
                       choices = c("Tagged" = "tagged", "Auto (backup)" = "auto"),
                       selected = "tagged", inline = TRUE),
          tags$label("Enter Pitcher Height (only used for pitchers not in the height CSV)",
                     style = "font-weight:bold; margin-top:8px;"),
          helpText("Arm angle = atan2(RelHeight - shoulder, RelSide), shoulder = 70% of height. ",
                   "Without a height the arm-angle estimate (and Stuff+) can't be computed, so set ",
                   "it here for pitchers missing from the height CSV."),
          fluidRow(
            column(6, numericInput("spc_height_ft", "ft", value = NA,
                                   min = 4, max = 8, step = 1)),
            column(6, numericInput("spc_height_in", "in", value = NA,
                                   min = 0, max = 11, step = 1))
          ),
          tags$label("Set Position on Rubber (overrides height CSV)",
                     style = "font-weight:bold; margin-top:8px;"),
          helpText("Feet from rubber center: -1 = toward 1B, 0 = middle, +1 = toward 3B. ",
                   "Leave at 0 to use the coded value (if any)."),
          sliderInput("spc_set_pos", NULL, min = -1, max = 1, value = 0, step = 0.1,
                      ticks = FALSE),
          actionButton("spc_update1", "Make/Update Card", icon("plus"),
                       class = "btn-success btn-block"),
          downloadButton("spc_downloadPng", "Download PNG (1:1)",
                         class = "btn-info btn-block"),
          downloadButton("spc_downloadPdf", "Download PDF",
                         class = "btn-info btn-block")
        ),
        mainPanel(
          div(style = "max-width: 900px; width: 100%; margin: 0 auto;",
              imageOutput("spc_combinedPlot", width = "100%", height = "auto"))
        )
      )
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}

season_pitcher_card_server <- function(input, output, session) {

  spc_card_page <- reactiveVal(NULL)

  # ft + in -> decimal feet; NULL when both blank so the override is skipped.
  height_override_dec <- reactive({
    ft   <- suppressWarnings(as.numeric(input$spc_height_ft))
    inch <- suppressWarnings(as.numeric(input$spc_height_in))
    if (is.na(ft) && is.na(inch)) return(NULL)
    if (is.na(ft)) ft <- 0
    if (is.na(inch)) inch <- 0
    val <- ft + inch / 12
    if (!is.finite(val) || val <= 0) NULL else val
  })

  set_override_val <- reactive({
    v <- suppressWarnings(as.numeric(input$spc_set_pos))
    if (length(v) == 0 || is.na(v) || !is.finite(v) || v == 0) NULL else v
  })

  output$spc_team_ui <- renderUI({
    if (!length(college26_team_choices)) {
      return(base_data_notice(paste0(
        "No ", TEAM_CONFIG$season, " college pitcher data is loaded yet. ",
        "Populate ", TEAM_CONFIG$data$college_file, " to enable this selector."
      )))
    }
    configured_team <- college26_team_choices[
      base_team_matches(unname(college26_team_choices))
    ]
    selectInput("spc_team", "Select Team:",
                choices  = college26_team_choices,
                selected = if (length(configured_team)) unname(configured_team[1]) else unname(college26_team_choices[1]))
  })

  output$spc_pitcher_ui <- renderUI({
    req(input$spc_team)
    pitchers <- college26_pitchers_by_team[[input$spc_team]]
    req(length(pitchers) > 0)
    selectInput("spc_pitcher", "Select Pitcher:", choices = pitchers)
  })

  output$spc_source_note <- renderUI({
    req(input$spc_team, input$spc_pitcher)
    primary_rows <- base_load_pitcher_rows(input$spc_team, input$spc_pitcher)
    college_n <- nrow(primary_rows)
    cape_n <- nrow(base_player_supplement_rows(
      primary_rows,
      cape26_data,
      input$spc_pitcher,
      role = "Pitcher"
    ))
    tags$p(
      paste0(
        format(college_n, big.mark = ","), " college pitches",
        if (cape_n > 0L) paste0(" + ", format(cape_n, big.mark = ","), " matched Cape pitches")
        else "; no matched Cape pitches"
      ),
      style = "font-size:12px; color:#5F6B7A; margin-top:-6px;"
    )
  })

  # Every College26 pitch for the selected pitcher, run through the same
  # preprocessing the game card uses.
  load_pitcher_pitches_season <- function() {
    req(input$spc_team, input$spc_pitcher)
    selected_data <- base_load_pitcher_rows(input$spc_team, input$spc_pitcher)
    req(nrow(selected_data) > 0)

    if (isTRUE(input$spc_include_cape)) {
      selected_data <- base_add_player_supplement(
        selected_data,
        cape26_data,
        input$spc_pitcher,
        role = "Pitcher"
      )
    }

    selected_data %>%
      apply_pitch_source(input$spc_pitch_src) %>%
      pitcher_summary() %>%
      mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType),
             row_id = row_number())
  }

  observeEvent(input$spc_update1, {
    game <- tryCatch(load_pitcher_pitches_season(), error = function(e) NULL)
    if (is.null(game) || nrow(game) == 0) {
      showNotification("No data available for the selected pitcher.", type = "warning"); return()
    }
    pitcher_display <- format_pitcher_name(input$spc_pitcher)
    page <- tryCatch(
      build_pitcher_card_page(game, pitcher_display,
                              height_override = height_override_dec(),
                              set_override    = set_override_val()),
      error = function(e) {
        showNotification(paste("Card error:", e$message), type = "error"); NULL
      }
    )
    spc_card_page(page)
  })

  output$spc_combinedPlot <- renderImage({
    req(spc_card_page())
    outfile <- tempfile(fileext = ".png")
    draw_card_to_png(spc_card_page(), outfile,
                     width = 1200, height = 1200, units = "px", res = 96, dpi = 96)
    list(src = outfile, contentType = "image/png",
         width = "100%", height = "auto", alt = "Season Pitching Summary Card")
  }, deleteFile = TRUE)

  output$spc_downloadPng <- downloadHandler(
    filename = function() {
      if (is.null(input$spc_pitcher)) return("Season Pitcher Card.png")
      paste0(format_name(input$spc_pitcher), " - Season (Pitcher) Report.png")
    },
    content = function(file) {
      req(spc_card_page())
      draw_card_to_png(spc_card_page(), file,
                       width = 12.5, height = 12.5, units = "in", res = 300, dpi = 300)
    }
  )

  output$spc_downloadPdf <- downloadHandler(
    filename = function() {
      if (is.null(input$spc_pitcher)) return("Season Pitcher Card.pdf")
      paste0(format_name(input$spc_pitcher), " - Season (Pitcher) Report.pdf")
    },
    content = function(file) {
      req(spc_card_page())
      draw_cards_to_pdf(list(spc_card_page()), file, width = 12.5, height = 12.5, dpi = 300)
    }
  )
}


base_media_server <- function(input, output, session) {

  cm_selected <- reactiveVal(NULL)
  cm_selection_uids <- reactiveVal(NULL)
  cm_source_summary <- reactiveVal(NULL)

  observe({
    req(nrow(base_pitcher_catalog) > 0)
    player_choices <- base_pitcher_catalog %>%
      filter(!is.na(Pitcher), !is.na(PitcherTeam)) %>%
      distinct(Pitcher, PitcherTeam) %>%
      mutate(
        display = paste0(pcard_format_pitcher_name(Pitcher), " — ", PitcherTeam),
        value   = paste(Pitcher, PitcherTeam, sep = "||")
      ) %>%
      arrange(display)
    choices_vec <- setNames(player_choices$value, player_choices$display)
    updateSelectizeInput(session, "cm_player_search", choices = choices_vec, server = TRUE, selected = "")
  })

  observeEvent(input$cm_load, {
    req(input$cm_player_search, nzchar(input$cm_player_search))
    parts    <- strsplit(input$cm_player_search, "\\|\\|")[[1]]
    raw_p    <- parts[1]
    raw_team <- parts[2]
    player_data <- base_load_pitcher_rows(raw_team, raw_p)
    if (!nrow(player_data)) {
      showNotification("No 2026 college rows were found for that player.", type = "warning")
      return()
    }
    college_n <- nrow(player_data)
    if (isTRUE(input$cm_include_cape)) {
      player_data <- base_add_player_supplement(
        player_data,
        cape26_data,
        raw_p,
        role = "Pitcher"
      )
    }
    cape_n <- sum(player_data$DataSource == "2026 Cape Cod League", na.rm = TRUE)
    pc <- tryCatch(
      pcard_build_all(player_data, raw_p),
      error = function(e) { showNotification(paste("Card build failed:", e$message), type = "error"); NULL }
    )
    cm_selected(pc)
    cm_source_summary(list(college = college_n, cape = cape_n))
    cm_selection_uids(NULL)
  })

  output$cm_source_note <- renderUI({
    counts <- cm_source_summary()
    if (is.null(counts)) return(NULL)
    tags$p(
      paste0(
        format(counts$college, big.mark = ","), " college pitches",
        if (counts$cape > 0L) paste0(" + ", format(counts$cape, big.mark = ","), " Cape pitches")
        else "; no matched Cape pitches"
      ),
      style = "color:#5F6B7A; font-size:12px; margin-top:-14px; margin-bottom:18px;"
    )
  })

  output$cm_missing_msg <- renderUI({
    if (is.null(cm_selected())) {
      tags$p("Search for a player above, then click Load Player.", style = "color:#8B8B96; font-size:13px;")
    } else NULL
  })

  output$cm_header_plot <- renderPlot({
    req(cm_selected())
    pcard_draw_header_page(cm_selected()$pitcher_raw, cm_selected()$team_abbr)
  })

  output$cm_boxscore_plot <- renderPlot({
    req(cm_selected())
    pcard_draw_boxscore_page(cm_selected()$box_stats)
  })

  raw_prepped_data <- reactive({
    req(cm_selected())
    pcard_prep_filterable_data(cm_selected()$pitcher_data)
  })

  CM_FILTER_CATALOG <- c(
    "Pitch Type"            = "pitch_type",
    "Previous Pitch Type"   = "prev_pitch",
    "Count Situation"       = "count",
    "Batted Ball"           = "batted_ball",
    "Times Through Order"   = "tto",
    "vs. Team"              = "vs_team",
    "Velocity"              = "velo",
    "Exit Velocity"         = "exit_velo",
    "Inning"                = "inning",
    "Date Range"            = "date"
  )

  cm_active_filter_keys <- reactiveVal(character(0))

  observe({
    remaining <- CM_FILTER_CATALOG[!CM_FILTER_CATALOG %in% cm_active_filter_keys()]
    updateSelectizeInput(session, "cm_filter_picker",
                        choices = c("(blank)" = "", remaining), selected = "")
  })

  observeEvent(input$cm_filter_picker, {
    req(nzchar(input$cm_filter_picker))
    cm_active_filter_keys(union(cm_active_filter_keys(), input$cm_filter_picker))
  })

  observeEvent(input$cm_filter_remove_key, {
    req(nzchar(input$cm_filter_remove_key))
    cm_active_filter_keys(setdiff(cm_active_filter_keys(), input$cm_filter_remove_key))
  })

  output$cm_active_filters_ui <- renderUI({
    keys <- cm_active_filter_keys()
    if (length(keys) == 0) return(NULL)
    d <- raw_prepped_data()

    widget_for <- function(key) {
      close_btn <- tags$span("✕",
        onclick = sprintf("Shiny.setInputValue('cm_filter_remove_key', '%s', {priority:'event'})", key),
        style = "cursor:pointer; color:#8B8B96; margin-left:8px; font-size:12px;")

      wrap <- function(label_ui, input_ui) {
        tags$div(style = "margin-top:14px; padding-top:12px; border-top:1px solid #F0F0F2;",
                 tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                          label_ui, close_btn),
                 input_ui)
      }

      switch(key,
        pitch_type = wrap(tags$b("Pitch Type"),
          checkboxGroupInput("cm_filter_pitch_type", NULL,
                             choices = sort(unique(na.omit(d$TaggedPitchType_clean))), inline = TRUE)),
        prev_pitch = wrap(tags$b("Previous Pitch Type"),
          checkboxGroupInput("cm_filter_prev_pitch", NULL,
                             choices = sort(unique(na.omit(d$PrevPitchType))), inline = TRUE)),
        count = wrap(tags$b("Count Situation"),
          checkboxGroupInput("cm_filter_count", NULL,
                             choices = c("Early","Ahead","Even","3-2","Kill/Putaway"), inline = TRUE)),
        batted_ball = wrap(tags$b("Batted Ball"),
          checkboxGroupInput("cm_filter_batted_ball", NULL,
                             choices = c("Ground Ball","Line Drive","Fly Ball","Pop Up","Bunt"), inline = TRUE)),
        tto = wrap(tags$b("Times Through Order"),
          checkboxGroupInput("cm_filter_tto", NULL, choices = c("1st","2nd","3rd+"), inline = TRUE)),
        vs_team = wrap(tags$b("vs. Team"),
          if ("BatterTeam" %in% names(d))
            checkboxGroupInput("cm_filter_vs_team", NULL,
                               choices = sort(unique(na.omit(d$BatterTeam))), inline = TRUE)
          else tags$p("Not available in this data.", style = "font-size:12px; color:#8B8B96;")),
        velo = wrap(tags$b("Velocity (mph)"),
          tags$div(style = "display:flex; gap:8px; align-items:center;",
                   numericInput("cm_filter_velo_min", NULL, value = NA, width = "100px"),
                   tags$span("to"),
                   numericInput("cm_filter_velo_max", NULL, value = NA, width = "100px"))),
        exit_velo = wrap(tags$b("Exit Velocity (mph)"),
          tags$div(style = "display:flex; gap:8px; align-items:center;",
                   numericInput("cm_filter_exit_velo_min", NULL, value = NA, width = "100px"),
                   tags$span("to"),
                   numericInput("cm_filter_exit_velo_max", NULL, value = NA, width = "100px"))),
        inning = wrap(tags$b("Inning"),
          tags$div(style = "display:flex; gap:8px; align-items:center;",
                   numericInput("cm_filter_inning_min", NULL, value = NA, width = "80px"),
                   tags$span("to"),
                   numericInput("cm_filter_inning_max", NULL, value = NA, width = "80px"))),
        date = wrap(tags$b("Date Range"),
          if ("Date" %in% names(d)) dateRangeInput("cm_filter_date", NULL, width = "260px")
          else tags$p("Not available in this data.", style = "font-size:12px; color:#8B8B96;"))
      )
    }

    tagList(lapply(keys, widget_for))
  })

  cm_applied_filters <- reactiveVal(list())

  observeEvent(input$cm_filter_apply, {
    cm_applied_filters(list(
      pitch_type      = input$cm_filter_pitch_type,
      prev_pitch      = input$cm_filter_prev_pitch,
      count_bucket    = input$cm_filter_count,
      batted_ball     = input$cm_filter_batted_ball,
      tto             = input$cm_filter_tto,
      vs_team         = input$cm_filter_vs_team,
      velo_range      = if (!is.null(input$cm_filter_velo_min) && !is.na(input$cm_filter_velo_min) &&
                            !is.null(input$cm_filter_velo_max) && !is.na(input$cm_filter_velo_max))
                          c(input$cm_filter_velo_min, input$cm_filter_velo_max) else NULL,
      exit_velo_range = if (!is.null(input$cm_filter_exit_velo_min) && !is.na(input$cm_filter_exit_velo_min) &&
                            !is.null(input$cm_filter_exit_velo_max) && !is.na(input$cm_filter_exit_velo_max))
                          c(input$cm_filter_exit_velo_min, input$cm_filter_exit_velo_max) else NULL,
      inning_range    = if (!is.null(input$cm_filter_inning_min) && !is.na(input$cm_filter_inning_min) &&
                            !is.null(input$cm_filter_inning_max) && !is.na(input$cm_filter_inning_max))
                          c(input$cm_filter_inning_min, input$cm_filter_inning_max) else NULL,
      date_range      = input$cm_filter_date
    ))
  })

  observeEvent(input$cm_filter_reset, {
    cm_active_filter_keys(character(0))
    cm_applied_filters(list())
  })

  filtered_pitcher_data <- reactive({
    pcard_apply_filters(raw_prepped_data(), cm_applied_filters())
  })

  output$cm_movement_plotly <- renderPlotly({
    req(cm_selected())
    pcard_movement_plotly(filtered_pitcher_data(), palette = pcard_pitch_colors)
  })

  observeEvent(plotly::event_data("plotly_selected", source = "cm_movement_src"),
              ignoreNULL = FALSE, {
    sel <- plotly::event_data("plotly_selected", source = "cm_movement_src")
    cm_selection_uids(if (is.null(sel) || nrow(sel) == 0) NULL else sel$customdata)
  })

  observeEvent(input$cm_clear_selection, {
    cm_selection_uids(NULL)
    plotlyProxy("cm_movement_plotly", session) %>%
      plotlyProxyInvoke("relayout", list(selections = list()))
  })

  active_pitcher_data <- reactive({
    d <- filtered_pitcher_data()
    uids <- cm_selection_uids()
    if (is.null(uids)) d else d %>% filter(pitch_uid %in% uids)
  })

  output$cm_pitch_metrics_dt <- renderDT({
    pcard_datatable(pcard_pitch_metrics_table(active_pitcher_data()))
  })

  output$cm_location_lhh_plot <- renderPlot({
    pcard_draw_location_lhh_page(pcard_location_plot(active_pitcher_data(), "Left"))
  })

  output$cm_location_rhh_plot <- renderPlot({
    pcard_draw_location_rhh_page(pcard_location_plot(active_pitcher_data(), "Right"))
  })

  output$cm_usage_overall_dt <- renderDT({
    pcard_datatable(pcard_usage_table(active_pitcher_data(), "All"))
  })

  output$cm_usage_rhh_dt <- renderDT({
    pcard_datatable(pcard_usage_table(active_pitcher_data(), "Right"))
  })

  output$cm_usage_lhh_dt <- renderDT({
    pcard_datatable(pcard_usage_table(active_pitcher_data(), "Left"))
  })

  output$cm_hit_metrics_dt <- renderDT({
    pcard_datatable(pcard_pitch_hit_metrics(active_pitcher_data()))
  })

  output$cm_count_usage_dt <- renderDT({
    pcard_datatable(pcard_usage_by_count(active_pitcher_data()))
  })

  output$cm_heatmap_pitch_ui <- renderUI({
    req(cm_selected())
    selectInput("cm_heatmap_pitch", "Pitch Type:",
                choices = c("All", cm_selected()$pitch_types), selected = "All", width = "200px")
  })

  output$cm_heatmap_lhh_plot <- renderPlot({
    req(cm_selected(), input$cm_heatmap_pitch)
    pcard_density_heatmap(active_pitcher_data(), pitch_type = input$cm_heatmap_pitch, side = "Left")
  })

  output$cm_heatmap_rhh_plot <- renderPlot({
    req(cm_selected(), input$cm_heatmap_pitch)
    pcard_density_heatmap(active_pitcher_data(), pitch_type = input$cm_heatmap_pitch, side = "Right")
  })
}




         
standings <- tryCatch(fetch_standings(), error = function(e) NULL)

fetch_team_roster <- function() {
  if (!isTRUE(TEAM_CONFIG$stats_api_enabled) || is.na(TEAM_CONFIG$mlb_team_id)) return(NULL)
  resp <- tryCatch(
    httr::GET(paste0("https://statsapi.mlb.com/api/v1/teams/",
                     TEAM_CONFIG$mlb_team_id, "/roster?season=", TEAM_CONFIG$season,
                     "&hydrate=person"),
              httr::timeout(10)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::http_error(resp)) return(NULL)

  raw <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
  if (length(raw$roster) == 0) return(NULL)

  do.call(rbind, lapply(raw$roster, function(p) {
    data.frame(
      Name     = p$person$fullName,
      Pos      = p$position$abbreviation,
      pos_type = p$position$type,
      Number   = p$jerseyNumber,
      Bats     = p$person$batSide$code %||% "",
      Throws   = p$person$pitchHand$code %||% "",
      stringsAsFactors = FALSE
    )
  }))
}

empty_roster <- function() {
  data.frame(Name = character(), Pos = character(), Number = character(),
             Bats = character(), Throws = character(), pos_type = character(),
             stringsAsFactors = FALSE)
}

read_configured_roster <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  roster <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  required <- c("Name", "Pos", "Number", "Bats", "Throws", "pos_type")
  if (is.null(roster) || !all(required %in% names(roster))) return(NULL)
  as.data.frame(roster[, required])
}

message("Loading roster for ", TEAM_CONFIG$full_name, "...")
roster_raw <- read_configured_roster(TEAM_CONFIG$data$roster_file)
if (is.null(roster_raw)) {
  roster_raw <- tryCatch(fetch_team_roster(), error = function(e) NULL)
}

if (!is.null(roster_raw)) {
  roster_pitchers   <- roster_raw %>% filter(pos_type == "Pitcher")   %>% select(Name, Pos, Number, Bats, Throws)
  roster_catchers   <- roster_raw %>% filter(pos_type == "Catcher")   %>% select(Name, Pos, Number, Bats, Throws)
  roster_infielders <- roster_raw %>% filter(pos_type == "Infielder") %>% select(Name, Pos, Number, Bats, Throws)
  roster_outfielders <- roster_raw %>% filter(pos_type == "Outfielder") %>% select(Name, Pos, Number, Bats, Throws)
} else {
  message("Roster unavailable; set BASE_ROSTER_FILE to provide a college roster")
  empty_group <- empty_roster()[, c("Name", "Pos", "Number", "Bats", "Throws")]
  roster_pitchers <- roster_catchers <- roster_infielders <- roster_outfielders <- empty_group
}


pcard_report_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      #pcard-page { max-width: 1100px; margin: 0 auto; padding: 0 24px 40px; }
      #pcard-page .pcard-panel {
        background: #fff; border: 1px solid #EAEAEE; border-radius: 10px;
        padding: 12px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(12,35,64,0.04);
      }
      #pcard-page .pcard-panel img { width: 100%; height: auto; display: block; }
      #pcard-page .pcard-row { display: flex; gap: 20px; margin-bottom: 0; }
      #pcard-page .pcard-row .pcard-panel { flex: 1; margin-bottom: 20px; min-width: 0; }
      #pcard-page .pcard-section-label {
        font-size: 11px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase;
        color: #5F5F6B; margin: 4px 0 10px;
      }
    "))),
    tags$div(
      class = "hub-main",
      tags$div(
        id = "pcard-page",
        uiOutput("pcard_missing_msg"),

        tags$div(class = "pcard-panel", plotOutput("pcard_header_plot", height = "100px")),
        tags$div(class = "pcard-panel", plotOutput("pcard_boxscore_plot", height = "100px")),

        tags$div(class = "pcard-section-label", "Pitch Movement"),
        tags$div(class = "pcard-panel", style = "max-width: 520px; margin-left: auto; margin-right: auto;",
                 plotOutput("pcard_movement_plot", height = "480px")),

        tags$div(class = "pcard-section-label", "Pitch Metrics"),
        tags$div(class = "pcard-panel", plotOutput("pcard_hit_metrics_plot", height = "320px")),

        tags$div(class = "pcard-section-label", "Usage by Count Situation"),
        tags$div(class = "pcard-panel", plotOutput("pcard_count_usage_plot", height = "320px")),

        tags$div(class = "pcard-section-label", "Pitch Location Density"),
        tags$div(
          class = "pcard-panel",
          style = "display:flex; gap:16px; align-items:end; margin-bottom:16px;",
          uiOutput("pcard_heatmap_pitch_ui"),
          selectInput("pcard_heatmap_side", "Hitter Side:",
                      choices = c("All","Left","Right"), selected = "All", width = "160px")
        ),
        tags$div(class = "pcard-panel", plotOutput("pcard_heatmap_plot", height = "420px"))
      )
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}                   
# ==========================================
# SHARED HELPERS
# ==========================================
format_name <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) paste(trimws(parts[2]), trimws(parts[1])) else name
}

# Pitch-type source toggle: when src == "auto", use AutoPitchType in place of
# TaggedPitchType (falling back to Tagged where Auto is blank/NA). Used as a
# backup for pitches tagged "Undefined". No-op if AutoPitchType isn't present.
apply_pitch_source <- function(df, src) {
  if (!identical(src, "auto") || !"AutoPitchType" %in% names(df)) return(df)
  df %>% mutate(
    TaggedPitchType = ifelse(
      is.na(AutoPitchType) | trimws(as.character(AutoPitchType)) == "",
      TaggedPitchType, as.character(AutoPitchType))
  )
}

# Export filename base, e.g. "June_13_Owen_Jenkins_Catcher_Report".
# `date` may be one or several dates (the most recent is used).
# Map team code(s) -> full NcaaColors team name (falls back to the raw code).
team_display_name <- function(abbr) {
  vapply(as.character(abbr), function(t) team_palette(t)$label, character(1))
}

base_data_notice <- function(message = NULL) {
  if (is.null(message)) {
    message <- paste0(
      "No ", TEAM_CONFIG$season, " Texas State pitch data is loaded yet. ",
      "Add TrackMan data to ", TEAM_CONFIG$data$season_file, "."
    )
  }
  tags$div(
    class = "base-data-notice",
    style = paste0(
      "padding:12px 14px; border-left:4px solid ", TEAM_CONFIG$colors$accent,
      "; background:", TEAM_CONFIG$colors$background,
      "; color:", TEAM_CONFIG$colors$primary, "; font-size:13px;"
    ),
    message
  )
}

# Export filename base, e.g. "Johnny Nuanez - June 13 vs Bourne (Pitcher) Report".
# `date` may be one or several dates (most recent is used); `opponent` is the
# opposing team code (resolved to its full name).
report_base_name <- function(player, date, opponent, role) {
  d <- suppressWarnings(max(as.Date(as.character(date))))
  date_part <- if (length(d) == 0 || is.na(d)) "" else paste0(" - ", format(d, "%B %d"))
  opp_part  <- if (length(opponent) == 0 || is.na(opponent) || !nzchar(as.character(opponent)))
                 "" else paste0(" vs ", unname(team_display_name(opponent)))
  paste0(format_name(player), date_part, opp_part, " (", role, ") Report")
}

# Most-frequent opposing-team code in `df` (column `opp_col`), or NA if none.
most_common_opponent <- function(df, opp_col) {
  if (is.null(df) || !opp_col %in% names(df)) return(NA_character_)
  v <- as.character(df[[opp_col]])
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}

# Stack report page PNGs into a single PNG file (for the .png export).
combine_pngs <- function(paths, file) {
  imgs     <- magick::image_read(unlist(paths))
  combined <- if (length(imgs) > 1) magick::image_append(imgs, stack = TRUE) else imgs
  magick::image_write(combined, path = file, format = "png")
}

draw_grid_table <- function(df,
                            title        = NULL,
                            y_top        = 0.95,
                            x_center     = 0.5,
                            row_h        = 0.028,
                            table_width  = 0.85,
                            header_bg    = TEAM_CONFIG$colors$primary,
                            zebra_bg     = "#f0f4f8",
                            alt_row_bg   = NULL,
                            title_cex    = 0.75,
                            header_cex   = 0.60,
                            cell_cex     = 0.58,
                            color_matrix = NULL) {
  df[] <- lapply(df, function(x) ifelse(is.na(x), "-", as.character(x)))
  headers    <- names(df)
  n_cols     <- length(headers)
  col_widths <- rep(table_width / n_cols, n_cols)
  x_start    <- x_center - sum(col_widths) / 2
  x_pos      <- c(x_start, x_start + cumsum(col_widths[-n_cols]))
  y_cursor   <- y_top

  if (!is.null(title)) {
    grid.text(title, x = x_center, y = y_cursor,
              gp = gpar(fontface = "bold", cex = title_cex, col = TEAM_CONFIG$colors$primary))
    y_cursor <- y_cursor - row_h * 0.9
  }

  for (i in seq_along(headers)) {
    grid.rect(x = x_pos[i], y = y_cursor,
              width = col_widths[i] * 0.98, height = row_h,
              just = c("left","top"),
              gp = gpar(fill = header_bg, col = "black", lwd = 0.5))
    grid.text(headers[i],
              x = x_pos[i] + col_widths[i] * 0.49,
              y = y_cursor - row_h * 0.5,
              gp = gpar(col = "white", cex = header_cex, fontface = "bold"))
  }
  y_cursor <- y_cursor - row_h

  for (r in seq_len(nrow(df))) {
    for (i in seq_along(headers)) {
      bg <- if (!is.null(color_matrix)) {
        color_matrix[r, i]
      } else if (!is.null(alt_row_bg) && r == 2) {
        alt_row_bg
      } else {
        if (r %% 2 == 0) zebra_bg else "white"
      }
      grid.rect(x = x_pos[i], y = y_cursor,
                width = col_widths[i] * 0.98, height = row_h,
                just = c("left","top"),
                gp = gpar(fill = bg, col = "grey80", lwd = 0.3))
      grid.text(as.character(df[[i]][r]),
                x = x_pos[i] + col_widths[i] * 0.49,
                y = y_cursor - row_h * 0.5,
                gp = gpar(cex = cell_cex, fontface = "bold"))
    }
    y_cursor <- y_cursor - row_h
  }
  invisible(y_cursor)
}

# ==========================================
# CATCHER - HELPER FUNCTIONS
# ==========================================
prep_catcher_data <- function(raw, team, src = "tagged") {
  raw <- raw %>%
    filter(CatcherTeam == team) %>%
    apply_pitch_source(src) %>%
    mutate(
      TaggedPitchType = case_when(
        TaggedPitchType %in% c("Fastball","FourSeamFastBall","Four-Seam","OneSeamFastball",
                                "FourSeamFastball","Sinker","TwoSeamFastball","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","CurveBall","Slider","Sweeper","Slurve")     ~ "Breaking Ball",
        TaggedPitchType %in% c("ChangeUp","Changeup","Splitter")                        ~ "Offspeed",
        TRUE ~ TaggedPitchType
      )
    )

  dfFraming <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, BatterSide,
           PitchCall, PlateLocHeight, PlateLocSide, Balls, Strikes, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PlateLocHeight), !is.na(PlateLocSide),
           PitchCall %in% c("BallCalled","StrikeCalled"))

  dfThrowing <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, KorBB, PitchCall,
           Strikes, OutsOnPlay, PopTime, ExchangeTime, TimeToBase, ThrowSpeed,
           BasePositionX, BasePositionY, BasePositionZ, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PopTime), PitchCall %in% c("BallCalled","StrikeCalled","StrikeSwinging"))

  list(framing = dfFraming, throwing = dfThrowing)
}

overallStealInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    summarise(
      `Caught Stealing` = sum(OutsOnPlay == 1, na.rm = TRUE),
      `Stolen Bases`    = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`             = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`        = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time`   = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`        = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`     = round(mean(ThrowSpeed,   na.rm = TRUE), 1)
    )
}

overallStealByPitch <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    group_by(`Pitch Type` = TaggedPitchType) %>%
    summarise(
      `CS`            = sum(OutsOnPlay == 1, na.rm = TRUE),
      `SB`            = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`           = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`      = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time` = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`      = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`   = round(mean(ThrowSpeed,   na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(CS))
}

overallFramingInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0)
    ) %>%
    summarise(
      `Strikes Won`  = sum(PhysicalZone == 0 & PitchCall == "StrikeCalled", na.rm = TRUE),
      `Strikes Lost` = sum(PhysicalZone == 1 & PitchCall == "BallCalled",   na.rm = TRUE),
      `Ratio`        = round(`Strikes Won` / `Strikes Lost`, 2)
    )
}

gameStealInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date) %>%
    mutate(
      `Caught Stealing` = if_else(OutsOnPlay == 1, "Yes", "No"),
      `Pop Time`        = round(PopTime,      2),
      `Exchange Time`   = round(ExchangeTime, 2),
      `Air Time`        = round(TimeToBase,   2),
      `Throw Speed`     = round(ThrowSpeed,   1)
    ) %>%
    rename(Pitch = TaggedPitchType) %>%
    select(Inning, Batter, Pitch, `Caught Stealing`, `Pop Time`, `Exchange Time`, `Air Time`, `Throw Speed`)
}

gameFramingInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date,
           PitchCall %in% c("StrikeCalled","BallCalled")) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      Count          = paste0(Balls, "-", Strikes),
      `Plate Height` = round(PlateLocHeight, 2),
      `Plate Side`   = round(PlateLocSide,   2)
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    rename(Pitch = TaggedPitchType) %>%
    mutate(`#` = row_number()) %>%
    select(`#`, Inning, Batter, Pitch, `Strike Outcome`, Count, `Plate Height`, `Plate Side`)
}

framingPlotData <- function(catcher, df, game_date = NULL) {
  out <- df %>% filter(Catcher == catcher, PitchCall %in% c("StrikeCalled","BallCalled"))
  if (!is.null(game_date)) out <- out %>% filter(as.Date(Date) == game_date)
  out <- out %>%
    mutate(
      PhysicalZone     = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      `Plate Height` = PlateLocHeight,
      `Plate Side`   = PlateLocSide
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    select(-Catcher)

  if (!is.null(game_date)) out <- out %>% mutate(`#` = row_number())
  out
}

plot_framing <- function(plot_df, outcome_filter, plot_title) {
  df_filtered <- plot_df %>% filter(`Strike Outcome` == outcome_filter)
  pt_color    <- ifelse(outcome_filter == "Won", "#00840D", "#E1463E")
  has_numbers <- "#" %in% names(df_filtered)

  p <- ggplot() +
    geom_polygon(data = data.frame(x = c(-0.708, 0.708, 0.708, 0, -0.708),
                                   y = c(0, 0, 0.25, 0.5, 0.25)),
                 aes(x = x, y = y), fill = "grey90", color = "black") +
    annotate("rect", xmin = -0.83083, xmax = 0.83083, ymin = 1.5, ymax = 3.3775,
             fill = NA, color = "black", linewidth = 1) +
    geom_point(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`),
               color = pt_color, size = ifelse(has_numbers, 6, 3), alpha = 0.9)

  if (has_numbers) {
    p <- p + geom_text(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`, label = `#`),
                       color = "white", size = 2.5, fontface = "bold")
  }

  p +
    xlim(-1.5, 1.5) + ylim(0, 3.75) +
    coord_fixed() +
    theme_minimal() +
    labs(title = plot_title, x = "Horizontal (ft)", y = "Height (ft)") +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

generate_catcher_pdf <- function(game_framing, game_throwing, season_framing, season_throwing,
                                  catcher, game_date, output_file, logo_path = NULL) {

  catcher_name <- format_name(catcher)

  g_steal        <- gameStealInfo(catcher, game_throwing, game_date)
  g_framing      <- gameFramingInfo(catcher, game_framing, game_date)
  g_frame_coords <- framingPlotData(catcher, game_framing, game_date)
  g_won_p        <- plot_framing(g_frame_coords, "Won",  paste0(game_date, " - Strikes Won"))
  g_lost_p       <- plot_framing(g_frame_coords, "Lost", paste0(game_date, " - Strikes Lost"))

  s_steal        <- overallStealInfo(catcher, season_throwing)
  s_steal_pitch  <- overallStealByPitch(catcher, season_throwing)
  s_framing      <- overallFramingInfo(catcher, season_framing)
  s_frame_coords <- framingPlotData(catcher, season_framing)
  s_won_p        <- plot_framing(s_frame_coords, "Won",  "Season - Strikes Won")
  s_lost_p       <- plot_framing(s_frame_coords, "Lost", "Season - Strikes Lost")

  logo_grob <- tryCatch({
    img <- magick::image_read(logo_path)
    img <- magick::image_resize(img, "x100")
    rasterGrob(as.raster(img), interpolate = TRUE)
  }, error = function(e) nullGrob())

  pdf(output_file, width = 11, height = 15)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  tryCatch({
    # PAGE 1 - GAME
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "- Postgame Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = TEAM_CONFIG$colors$primary))
    grid.text(as.character(game_date), x = 0.5, y = 0.05,
              gp = gpar(cex = 0.85, col = TEAM_CONFIG$colors$primary))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    GAP      <- 0.03
    PLOT_TOP <- 0.34   # framing location plots live in the band below this

    # Each table is positioned off the previous table's actual bottom so they
    # never overlap regardless of how many rows the game produced.
    y_cur <- draw_grid_table(g_steal,
                    title = "Game Throwing - Steal Attempts",
                    y_top = 0.90, x_center = 0.5, row_h = 0.022,
                    table_width = 0.88, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    # Strike log length scales with pitches caught; shrink row height so the
    # whole table fits between the steal table and the plot band.
    fram_top   <- y_cur - GAP
    n_fram     <- max(nrow(g_framing), 1)
    row_h_fram <- min(0.020, max(0.010, (fram_top - PLOT_TOP) / (n_fram + 2)))
    draw_grid_table(g_framing,
                    title = "Game Framing - Strike Log",
                    y_top = fram_top, x_center = 0.5, row_h = row_h_fram,
                    table_width = 0.75, header_cex = 0.68, cell_cex = 0.68, title_cex = 0.90)

    grid.text("Game Framing - Strike Locations", x = 0.5, y = PLOT_TOP,
              gp = gpar(fontface = "bold", cex = 0.90, col = TEAM_CONFIG$colors$primary))

    pushViewport(viewport(x = 0.27, y = PLOT_TOP - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(g_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = PLOT_TOP - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(g_lost_p, newpage = FALSE)
    popViewport()

    grid.text(paste("Data: TrackMan |", TEAM_CONFIG$full_name, "Analytics"),
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

    # PAGE 2 - SEASON
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "- Season Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = TEAM_CONFIG$colors$primary))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    y_cur <- draw_grid_table(s_steal,
                    title = "Season Throwing - Overall",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    y_cur <- draw_grid_table(s_steal_pitch,
                    title = "Season Throwing - By Pitch Type",
                    y_top = y_cur - GAP, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    y_cur <- draw_grid_table(s_framing,
                    title = "Season Framing - Strike Summary",
                    y_top = y_cur - GAP, x_center = 0.5, row_h = 0.022,
                    table_width = 0.40, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    plot_top2 <- y_cur - GAP
    grid.text("Season Framing - Strike Locations", x = 0.5, y = plot_top2,
              gp = gpar(fontface = "bold", cex = 0.90, col = TEAM_CONFIG$colors$primary))

    pushViewport(viewport(x = 0.27, y = plot_top2 - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(s_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = plot_top2 - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(s_lost_p, newpage = FALSE)
    popViewport()

    grid.text(paste("Data: TrackMan |", TEAM_CONFIG$full_name, "Analytics"),
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

  }, error = function(e) message("PDF generation error: ", conditionMessage(e)))
}

# ==========================================
# PITCHER - MODELS (retained for compatibility; card tab no longer uses them)
# ==========================================
pitcher_model        <- readRDS("PitcherModels/Stuff+2.rds")
league_stats         <- readRDS("PitcherModels/NEW_LeagueStats2.rds")
xgb_fit              <- readRDS("PitcherModels/location_plus_model.rds")
league_stats_pitcher <- readRDS("PitcherModels/location_plus_league_stats_pitcher.rds")

# ==========================================
# HITTER DATA SOURCE + REPORT HELPERS (from basetest.R)
# ==========================================
download_from_hf_dataset <- function(repo_id, filename, token) {
  if (!nzchar(repo_id)) stop("HF dataset repository is not configured")
  url  <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filename)
  tmp  <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  auth <- if (nzchar(token)) {
    httr::add_headers(Authorization = paste("Bearer", token))
  } else {
    httr::add_headers()
  }
  resp <- httr::GET(url, auth, httr::write_disk(tmp, overwrite = TRUE),
                    httr::timeout(120))
  if (httr::http_error(resp)) stop("Failed to download ", filename, ": ", httr::status_code(resp))
  tmp
}

read_data_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("arrow package required for parquet files but is not installed")
    arrow::read_parquet(path)
  } else if (ext %in% c("csv", "txt")) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Unsupported file type: ", ext)
  }
}

# Keep the complete season source on the mounted volume, but materialize only
# columns referenced by the current R application. This preserves every source
# field for future features without forcing all 201 columns into RAM at once.
read_season_runtime_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext != "parquet") return(read_data_file(path))

  dataset <- arrow::open_dataset(path, format = "parquet")
  available <- names(dataset$schema)
  r_files <- list.files(
    ".", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE
  )
  source_lines <- unlist(lapply(r_files, function(file) {
    tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  }), use.names = FALSE)

  referenced <- available[vapply(available, function(column) {
    pattern <- paste0(
      "(?<![[:alnum:]_.])\\Q", column, "\\E(?![[:alnum:]_.])"
    )
    any(grepl(pattern, source_lines, perl = TRUE))
  }, logical(1))]

  core_columns <- c(
    "PitchUID", "GameID", "GameUID", "Date", "LocalDateTime", "UTCDateTime",
    "Pitcher", "PitcherId", "PitcherTeam", "PitcherThrows",
    "Batter", "BatterId", "BatterTeam", "BatterSide",
    "Catcher", "CatcherId", "CatcherTeam", "CatcherThrows",
    "TaggedPitchType", "AutoPitchType", "PitchCall", "KorBB",
    "TaggedHitType", "PlayResult", "Notes", "Top/Bottom",
    "RelSpeed", "SpinRate", "SpinAxis", "RelHeight", "RelSide", "Extension",
    "InducedVertBreak", "HorzBreak", "PlateLocHeight", "PlateLocSide",
    "ExitSpeed", "Angle", "Direction", "Inning", "PAofInning", "PitchofPA",
    "Balls", "Strikes", "OutsOnPlay", "RunsScored"
  )
  selected <- intersect(available, unique(c(core_columns, referenced)))
  if (!length(selected)) stop("No usable season columns were selected")

  message(
    "Reading ", length(selected), " of ", length(available),
    " mounted season columns; the full source remains intact on disk."
  )
  dataset %>%
    dplyr::select(tidyselect::all_of(selected)) %>%
    dplyr::collect()
}

message("Loading master game data...")
# The BASE deployment reads its mounted bucket path directly. A configured
# remote repository remains an optional fallback for other deployments only.
if (!file.exists(SEASON_DATA_FILE)) {
  invisible(pull_season_data_from_hf())
}

season_data <- tryCatch({
  df <- read_season_runtime_data(SEASON_DATA_FILE)
  if (nrow(df) == 0) {
    df <- df %>%
      mutate(across(any_of(c(
        "PitcherTeam", "BatterTeam", "CatcherTeam", "Pitcher", "Batter", "Catcher",
        "PitcherThrows", "BatterSide", "CatcherThrows", "TaggedPitchType",
        "AutoPitchType", "PitchCall", "KorBB", "TaggedHitType", "PlayResult",
        "GameID", "GameUID", "Top/Bottom"
      )), as.character)) %>%
      mutate(across(any_of(c(
        "PitcherId", "BatterId", "CatcherId", "OutsOnPlay", "RunsScored",
        "RelSpeed", "SpinRate", "SpinAxis", "RelHeight", "RelSide", "Extension",
        "InducedVertBreak", "HorzBreak", "PlateLocHeight", "PlateLocSide",
        "ExitSpeed", "Angle", "Direction", "Inning", "PAofInning", "PitchofPA",
        "Balls", "Strikes"
      )), ~ suppressWarnings(as.numeric(.x))))
  }
  if ("Notes" %in% names(df)) df$Notes <- as.character(df$Notes)
  df$DataSource <- "2026 College Season"
  message("Loaded ", SEASON_DATA_FILE, " — rows: ", nrow(df))
  df
}, error = function(e) {
  message(SEASON_DATA_FILE, " load failed: ", e$message)
  NULL
})

master_last_updated <- if (!is.null(season_data)) format(Sys.time(), "%b %d, %Y at %I:%M %p") else "unavailable"
message("Season data rows: ", if (!is.null(season_data)) nrow(season_data) else 0)

# ----------------------------------------------------------------------------
# The full College26 master stays in private Space storage for future features.
# Current all-college scouting loads one selected pitcher from hash-partitioned
# runtime data, while season_data contains only the configured team's rows.
# ----------------------------------------------------------------------------
COLLEGE26_FILE      <- TEAM_CONFIG$data$college_file
COLLEGE26_REPO_ID   <- base_env("COLLEGE26_REPO_ID", TEAM_CONFIG$data$college_repo_id)
COLLEGE26_REPO_PATH <- base_env("COLLEGE26_REPO_PATH", TEAM_CONFIG$data$college_repo_path)

college26_data <- NULL

# CapeCod26 remains a separate supplemental source. It is joined only after a
# player is selected, so Cape teams never replace or hide college affiliations.
cape26_data <- tryCatch({
  df <- read_data_file(TEAM_CONFIG$data$cape_file)
  if ("Notes" %in% names(df)) df$Notes <- as.character(df$Notes)
  df$DataSource <- "2026 Cape Cod League"
  message("Loaded ", TEAM_CONFIG$data$cape_file, " — rows: ", nrow(df))
  df
}, error = function(e) {
  message(TEAM_CONFIG$data$cape_file, " load failed: ", e$message)
  NULL
})

base_add_player_supplement <- function(primary, supplemental, player, role = "Pitcher") {
  if (is.null(primary) || !nrow(primary) || is.null(supplemental) || !nrow(supplemental) ||
      !role %in% names(primary) || !role %in% names(supplemental)) {
    return(primary)
  }

  extra <- base_player_supplement_rows(primary, supplemental, player, role)
  if (!nrow(extra)) return(primary)
  extra[[role]] <- as.character(player)[[1]]

  tryCatch(
    dplyr::bind_rows(primary, extra),
    error = function(e) {
      message("Cape supplement bind failed for ", player, ": ", e$message)
      primary
    }
  )
}

# Precompute the Season Pitcher Card selector choices once, at startup, so the
# renderUI selectors never re-scan the large College26 table:
#   college26_team_choices     : named vector (display name -> team abbr)
#   college26_pitchers_by_team : list keyed by team abbr, each a named vector
#                                (formatted pitcher name -> raw Pitcher value)
college26_team_choices     <- character(0)
college26_pitchers_by_team <- list()
if (nrow(base_pitcher_catalog) > 0) {
  .c26_teams <- sort(unique(base_pitcher_catalog$PitcherTeam))
  .c26_teams <- .c26_teams[!is.na(.c26_teams)]
  college26_team_choices <- setNames(.c26_teams, unname(team_display_name(.c26_teams)))

  .c26_tp <- base_pitcher_catalog %>%
    filter(!is.na(PitcherTeam), !is.na(Pitcher)) %>%
    distinct(PitcherTeam, Pitcher) %>%
    arrange(Pitcher)
  college26_pitchers_by_team <- lapply(
    split(.c26_tp$Pitcher, .c26_tp$PitcherTeam),
    function(p) setNames(p, format_pitcher_name(p))
  )
  rm(.c26_teams, .c26_tp)
  message("College26 selectors ready: ", length(college26_team_choices), " teams")
}

# ----------------------------------------------------------------------------
# Manual single-game CSV support. Each report page has a toggle + uploader; when
# enabled the uploaded game is appended to the configured season so its date(s)
# show up in the team/date selectors. The configured season remains unchanged.
# ----------------------------------------------------------------------------
align_manual_to_season <- function(manual, season) {
  common <- intersect(names(manual), names(season))
  for (col in common) {
    target <- season[[col]]
    src    <- as.character(manual[[col]])
    manual[[col]] <- tryCatch({
      if      (inherits(target, "Date"))    as.Date(src)
      else if (inherits(target, "POSIXct")) as.POSIXct(src, tz = "UTC")
      else if (inherits(target, "hms"))     hms::as_hms(src)
      else if (is.numeric(target))          suppressWarnings(as.numeric(src))
      else if (is.logical(target))          as.logical(src)
      else if (is.character(target))        src
      else manual[[col]]
    # Last resort: a typed all-NA column so bind_rows() can never choke on a
    # class clash (e.g. an empty column readr guessed as <lgl> vs season <dbl>).
    }, error = function(e) target[rep(NA_integer_, length(src))])
  }
  manual
}

combine_with_manual <- function(season, enabled, file) {
  if (is.null(season)) return(season)
  if (!isTRUE(enabled) || is.null(file)) return(season)
  manual <- tryCatch(read_input_file(file$datapath, file$name), error = function(e) {
    message("Manual game CSV read failed: ", e$message); NULL
  })
  if (is.null(manual) || nrow(manual) == 0) return(season)
  manual <- align_manual_to_season(manual, season)
  tryCatch(dplyr::bind_rows(season, manual), error = function(e) {
    message("Manual game bind failed: ", e$message); season
  })
}

pdf_to_pngs <- function(pdf_path, dpi = 200) {
  img <- magick::image_read_pdf(pdf_path, density = dpi)
  w   <- as.character(round(8.5 * dpi))
  h   <- as.character(round(11  * dpi))
  img <- magick::image_resize(img, paste0(w, "x", h, "!"))
  png_paths <- vapply(seq_along(img), function(i) {
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img[i], path = tmp, format = "png")
    tmp
  }, character(1))
  png_paths
}

png_to_base64 <- function(path) {
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}

report_viewer_ui <- function(png_paths, pdf_path, download_id_pdf, download_id_png) {
  n_pages  <- length(png_paths)
  img_tags <- lapply(seq_along(png_paths), function(i) {
    tags$div(
      style = "margin-bottom: 16px;",
      tags$p(
        style = "font-size: 11px; color: #888; margin-bottom: 4px;",
        paste0("Page ", i, " of ", n_pages)
      ),
      tags$img(
        src   = png_to_base64(png_paths[[i]]),
        style = paste0(
          "width: 100%;",
          "aspect-ratio: 8.5 / 11;",
          "object-fit: contain;",
          "border: 0.5px solid #ddd;",
          "border-radius: 6px;",
          "display: block;",
          "background: #fff;"
        )
      )
    )
  })

  tagList(
    tags$div(
      style = "display: flex; gap: 12px; margin-bottom: 16px;",
      downloadButton(download_id_pdf, "Download PDF",
                     class = "btn btn-success", style = "width: 160px;"),
      downloadButton(download_id_png, "Download PNG(s)",
                     class = "btn btn-outline-secondary", style = "width: 160px;")
    ),
    tags$div(
      style = paste0(
        "max-height: 900px;",
        "overflow-y: auto;",
        "border: 0.5px solid #ddd;",
        "border-radius: 8px;",
        "padding: 16px;",
        "background: #f8f8f8;"
      ),
      do.call(tagList, img_tags)
    )
  )
}

base_media_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      #cm-page { max-width: 1180px; margin: 0 auto; padding: 0 0 40px; }
      #cm-page .pcard-panel {
        background: var(--base-surface); border: 1px solid var(--base-border); border-radius: var(--base-radius);
        padding: 16px; margin-bottom: 18px; box-shadow: var(--base-shadow-sm);
      }
      #cm-page .pcard-panel img { width: 100%; height: auto; display: block; }
      #cm-page .pcard-row { display: flex; gap: 20px; }
      #cm-page .pcard-row .pcard-panel { flex: 1; min-width: 0; }
      #cm-page .pcard-section-label {
        font-family: var(--base-font-display); font-size: 17px; font-weight: 600;
        letter-spacing: 1px; text-transform: uppercase;
        color: var(--base-muted); margin: 28px 0 12px; padding-bottom: 9px;
        border-bottom: 1px solid var(--base-border);
      }
    "))),
    tags$div(
      class = "hub-main base-page base-media-page",
      tags$h2("BASE Media",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 16px;"),
      tags$p("Search any pitcher in the configured college database.",
             style = "color:#5F6B7A; font-size:14px; margin-bottom:20px;"),

      tags$div(
        class = "base-control-grid base-player-search",
        style = "display: grid; grid-template-columns: 3fr 1fr; gap: 16px; align-items: end; margin-bottom: 24px;",
        tags$div(
          tags$label("Search Player:", style = "font-size:13px; font-weight:600; color:#5F5F6B;"),
          selectizeInput("cm_player_search", NULL, choices = NULL,
                        options = list(placeholder = "Type a name...", maxOptions = 15),
                        width = "100%")
        ),
        actionButton("cm_load", "Load Player", class = "btn btn-primary")
      ),
      checkboxInput(
        "cm_include_cape",
        "Include matched 2026 Cape Cod League pitches",
        value = TRUE
      ),
      uiOutput("cm_source_note"),

      tags$div(
        class = "pcard-panel",
        tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                 tags$span("Filters", style = "font-size:13px; font-weight:700; color:var(--navy);"),
                 tags$div(style = "display:flex; gap:8px;",
                          actionButton("cm_filter_reset", "Reset", class = "btn btn-sm btn-outline-secondary"),
                          actionButton("cm_filter_apply", "Apply Filters", class = "btn btn-sm btn-primary"))),

        tags$div(style = "margin-top:10px;",
                 selectizeInput("cm_filter_picker", NULL, choices = NULL,
                               options = list(placeholder = "Search filters to add...",
                                              onInitialize = I('function() { this.setValue(""); }')),
                               width = "320px")),

        uiOutput("cm_active_filters_ui")
      ),

      tags$div(
        id = "cm-page",
        uiOutput("cm_missing_msg"),

        tags$div(class = "pcard-panel", plotOutput("cm_header_plot", height = "100px")),
        tags$div(class = "pcard-panel", plotOutput("cm_boxscore_plot", height = "100px")),

        tags$div(class = "pcard-section-label", "Pitch Movement & Location Density"),
        tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                 tags$span("Box- or lasso-select pitches on the movement plot to filter every table below. Pitch type filter applies to the density maps.",
                           style = "font-size:12px; color:#8B8B96;"),
                 tags$div(style = "display:flex; gap:10px; align-items:center;",
                          uiOutput("cm_heatmap_pitch_ui"),
                          actionButton("cm_clear_selection", "Clear Selection", class = "btn btn-sm btn-outline-secondary"))),
        tags$div(class = "pcard-row",
          tags$div(class = "pcard-panel", style = "flex:1;",
                   plotOutput("cm_heatmap_lhh_plot", height = "420px")),
          tags$div(class = "pcard-panel", style = "flex:1.3;",
                   plotlyOutput("cm_movement_plotly", height = "420px")),
          tags$div(class = "pcard-panel", style = "flex:1;",
                   plotOutput("cm_heatmap_rhh_plot", height = "420px"))
        ),

        tags$div(class = "pcard-section-label", "Pitch Metrics"),
        tags$div(class = "pcard-panel", DTOutput("cm_pitch_metrics_dt")),

        tags$div(class = "pcard-section-label", "Pitch Location"),
        tags$div(class = "pcard-row",
          tags$div(class = "pcard-panel", plotOutput("cm_location_lhh_plot", height = "420px")),
          tags$div(class = "pcard-panel", plotOutput("cm_location_rhh_plot", height = "420px"))
        ),

        tags$div(class = "pcard-section-label", "Usage"),
        tags$div(class = "pcard-row",
          tags$div(class = "pcard-panel", DTOutput("cm_usage_overall_dt")),
          tags$div(class = "pcard-panel", DTOutput("cm_usage_rhh_dt")),
          tags$div(class = "pcard-panel", DTOutput("cm_usage_lhh_dt"))
        ),

        tags$div(class = "pcard-section-label", "Hit Metrics by Pitch"),
        tags$div(class = "pcard-panel", DTOutput("cm_hit_metrics_dt")),

        tags$div(class = "pcard-section-label", "Usage by Count Situation"),
        tags$div(class = "pcard-panel", DTOutput("cm_count_usage_dt"))
      )
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}                      
# ==========================================
# HITTER CONSTANTS & HELPERS
# ==========================================
hitter_pitch_colors <- c(
  "Four Seam" = "red",    "Sinker"    = "orange",
  "Slider"    = "gold",   "Curveball" = "blue",
  "Changeup"  = "green3", "Cutter"    = "#8B4513",
  "Splitter"  = "mediumpurple3"
)

hitter_strike_zone <- tibble(
  PlateLocSide   = c(-0.8303, -0.8303, 0.8303, 0.8303, -0.8303),
  PlateLocHeight = c(1.5, 3.3775, 3.3775, 1.5, 1.5)
)

hitter_home_plate <- data.frame(
  x = c(-0.708, 0.708, 0.708, 0, -0.708),
  y = c(0, 0, 0.25, 0.5, 0.25)
)

clean_hitter_pitch_type <- function(pt) {
  case_when(
    pt %in% c("Fastball","FourSeamFastBall","Four-Seam") ~ "Four Seam",
    pt %in% c("Sinker","TwoSeamFastBall")               ~ "Sinker",
    pt == "Slider"                                       ~ "Slider",
    pt == "Curveball"                                    ~ "Curveball",
    pt %in% c("ChangeUp","Changeup")                     ~ "Changeup",
    pt == "Cutter"                                       ~ "Cutter",
    pt == "Splitter"                                     ~ "Splitter",
    TRUE ~ NA_character_
  )
}

build_color_matrix_hitter <- function(df, benchmarks, lower_is_better = c()) {
  get_color <- function(value, bottom, top, flip = FALSE) {
    value <- suppressWarnings(as.numeric(gsub("%", "", value)))
    if (is.na(value)) return("white")
    normalized <- pmax(0, pmin(1, (value - bottom) / (top - bottom)))
    if (flip) normalized <- 1 - normalized
    colorRamp(c("#E1463E","#CDCD00","#00840D"))(normalized) %>%
      { rgb(.[1], .[2], .[3], maxColorValue = 255) }
  }
  color_matrix <- matrix("white", nrow = nrow(df), ncol = ncol(df))
  for (i in seq_along(names(df))) {
    col_name <- names(df)[i]
    if (col_name %in% names(benchmarks)) {
      bounds <- benchmarks[[col_name]]
      flip   <- col_name %in% lower_is_better
      for (r in seq_len(nrow(df)))
        color_matrix[r, i] <- get_color(df[[i]][r], bounds[1], bounds[2], flip)
    }
  }
  color_matrix
}

hitter_season_benchmarks <- list(
  AVG=c(0.200,0.360), OBP=c(0.290,0.450), SLG=c(0.339,0.667),
  `K%`=c(9.8,26.5), `BB%`=c(6.3,18.8), `Whiff%`=c(12.8,30.7), `HardHit%`=c(20,55)
)
hitter_lower_is_better <- c("K%","Whiff%")

hitter_split_bench_map <- list(
  "Fastball" = list(`Swing%`=c(37.2,52.2), `Contact%`=c(72,90), `Chase%`=c(14,29), `Barrel%`=c(6,30),  `IZ Whiff%`=c(6,23),  `HardHit%`=c(18,56), `Avg EV`=c(80,92)),
  "Breaker"  = list(`Swing%`=c(30,48),     `Contact%`=c(56,82), `Chase%`=c(15,35), `Barrel%`=c(0,30),  `IZ Whiff%`=c(8,29),  `HardHit%`=c(9,50),  `Avg EV`=c(77,90)),
  "Offspeed" = list(`Swing%`=c(36,54),     `Contact%`=c(56,81), `Chase%`=c(20,39), `Barrel%`=c(4,30),  `IZ Whiff%`=c(10,34), `HardHit%`=c(14,54), `Avg EV`=c(79,91))
)
hitter_split_lower_is_better <- c("Chase%","IZ Whiff%")

build_split_color_matrix_hitter <- function(df, bench_map, lower_is_better = c()) {
  color_matrix <- matrix("white", nrow = nrow(df), ncol = ncol(df))
  for (r in seq_len(nrow(df))) {
    pitch_type <- df$`Pitch Type`[r]
    benchmarks <- bench_map[[pitch_type]]
    if (!is.null(benchmarks)) {
      row_colors <- build_color_matrix_hitter(df[r,,drop=FALSE], benchmarks, lower_is_better)
      color_matrix[r, ] <- row_colors[1, ]
    }
  }
  color_matrix
}

# ==========================================
# SWING DECISION MODELS
# ==========================================
message("HITTER_TOKEN present: ", nchar(Sys.getenv("HITTER_TOKEN")) > 0)
sd_models <- tryCatch({
  token <- Sys.getenv("HITTER_TOKEN")
  repo  <- TEAM_CONFIG$data$swing_model_repo
  if (!nzchar(repo)) stop("BASE_SWING_MODEL_REPO is not configured")
  message("Downloading swing decision models...")
  list(
    model_take  = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Take.ubj",  token)),
    model_swing = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Swing.ubj", token)),
    encodings   = readRDS(download_from_hf_dataset(repo,  "encodings.rds",        token))
  )
}, error = function(e) { message("Swing decision models not loaded: ", e$message); NULL })
message("sd_models loaded: ", !is.null(sd_models))

sd_features <- c("PlateLocHeight","PlateLocSide","count_state_enc","pitch_type_enc")

recode_pitch_type_model <- function(x) {
  case_when(
    x %in% c("Fastball","Four-Seam","FourSeamFastBall","FourSeamFastball") ~ "FF",
    x %in% c("Sinker","TwoSeamFastBall","TwoSeamFastball")                 ~ "SI",
    x == "Cutter"                     ~ "FC",
    x %in% c("Curveball","CurveBall") ~ "CU",
    x == "Slider"                     ~ "SL",
    x == "Sweeper"                    ~ "SW",
    x %in% c("ChangeUp","Changeup")   ~ "CH",
    x == "Splitter"                   ~ "FS",
    TRUE ~ "Other"
  )
}

score_pitches_xrv <- function(df, models = sd_models) {
  if (is.null(models)) return(df %>% mutate(xRV_swing=NA_real_, xRV_take=NA_real_, xRV_diff=NA_real_))
  enc <- models$encodings
  scored <- df %>%
    mutate(
      PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
      PlateLocSide   = suppressWarnings(as.numeric(PlateLocSide)),
      Balls = as.integer(Balls), Strikes = as.integer(Strikes),
      count_state = paste0(Balls, "-", Strikes),
      count_state = ifelse(!count_state %in% c("0-0","0-1","0-2","1-0","1-1","1-2",
                                               "2-0","2-1","2-2","3-0","3-1","3-2"), NA, count_state),
      pitch_type_model = recode_pitch_type_model(TaggedPitchType),
      count_state_enc  = match(count_state, enc$count_state),
      pitch_type_enc   = match(pitch_type_model, enc$pitch_type)
    )
  valid <- !is.na(scored$PlateLocHeight) & !is.na(scored$PlateLocSide) &
           !is.na(scored$count_state_enc) & !is.na(scored$pitch_type_enc) &
           scored$pitch_type_model != "Other"
  scored$xRV_swing <- NA_real_; scored$xRV_take <- NA_real_
  if (any(valid)) {
    feature_frame <- scored[valid, sd_features, drop = FALSE] %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
    feature_matrix <- data.matrix(feature_frame)
    keep <- stats::complete.cases(feature_matrix)

    if (any(keep)) {
      dmat <- xgb.DMatrix(feature_matrix[keep, , drop = FALSE])
      scored_rows <- which(valid)[keep]
      scored$xRV_swing[scored_rows] <- predict(models$model_swing, dmat)
      scored$xRV_take[scored_rows]  <- predict(models$model_take,  dmat)
    }
  }
  scored %>% mutate(xRV_diff = xRV_swing - xRV_take) %>%
    select(-pitch_type_model, -count_state_enc, -pitch_type_enc)
}

summarise_xrv_swdec <- function(scored_df) {
  scored_df %>% filter(!is.na(xRV_diff)) %>%
    mutate(ModelShouldSwing = xRV_diff > 0,
           DidSwing = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
           GoodxDec = (ModelShouldSwing & DidSwing) | (!ModelShouldSwing & !DidSwing)) %>%
    summarise(Total=n(), GoodDec=sum(GoodxDec,na.rm=TRUE),
              SwingPitches=sum(ModelShouldSwing,na.rm=TRUE),
              ActualSwings=sum(DidSwing,na.rm=TRUE), .groups="drop") %>%
    mutate(`xSwDec%`    = paste0(round(GoodDec      / pmax(Total,1)*100, 1), "%"),
           `Mdl Swing%` = paste0(round(SwingPitches / pmax(Total,1)*100, 1), "%"),
           `Act Swing%` = paste0(round(ActualSwings / pmax(Total,1)*100, 1), "%")) %>%
    select(`xSwDec%`, `Mdl Swing%`, `Act Swing%`)
}

make_swdec_plot <- function(df, plot_title) {
  dec_colors <- c("Good Swing"="#00840D","Good Take"="#5BBF6A","Bad Swing"="#E1463E","Bad Take"="#F4A49E")
  dec_shapes <- c("Good Swing"=17,"Good Take"=21,"Bad Swing"=25,"Bad Take"=21)
  ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="grey85", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1.2) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,shape=DecLabel), size=6.5, color="black", alpha=0.3) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,color=DecLabel,shape=DecLabel), size=5.5, alpha=0.92) +
    scale_color_manual(values=dec_colors, name=NULL, guide=guide_legend(override.aes=list(size=3.5))) +
    scale_shape_manual(values=dec_shapes, name=NULL, guide=guide_legend(override.aes=list(size=3.5))) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title, subtitle="\u25b2 Swing  \u25cf Take  |  Green = Good  Red = Bad", x=NULL, y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color=TEAM_CONFIG$colors$primary),
          plot.subtitle=element_text(hjust=0.5,size=7,color="grey50"),
          axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(),
          legend.position="bottom", legend.text=element_text(size=7.5),
          legend.key.size=unit(0.35,"cm"), legend.spacing.x=unit(0.2,"cm"))
}

make_swdec_heatmap <- function(df, plot_title) {
  bin_w <- 0.30
  binned <- df %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    mutate(bx = round(PlateLocSide/bin_w)*bin_w, by = round(PlateLocHeight/bin_w)*bin_w) %>%
    group_by(bx, by) %>%
    summarise(GoodPct = mean(SwDec==1, na.rm=TRUE), N=n(), .groups="drop") %>%
    filter(N >= 2)
  ggplot() +
    geom_tile(data=binned, aes(x=bx, y=by, fill=GoodPct), width=bin_w*0.97, height=bin_w*0.97) +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0.70, limits=c(0,1), guide="none") +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="grey85", color="black", linewidth=0.8, inherit.aes=FALSE) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1.2, inherit.aes=FALSE) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title, subtitle="Red = Good Decisions  |  Blue = Bad Decisions", x=NULL, y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color=TEAM_CONFIG$colors$primary),
          plot.subtitle=element_text(hjust=0.5,size=7,color="grey50"),
          axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())
}

# ==========================================
# HITTER PDF
# ==========================================
generate_hitter_pdf <- function(game_data, season_data, selected_hitter, output_file,
                                active_models = sd_models) {
  hitter_name <- format_name(selected_hitter)

  coerce_numerics <- function(df) {
  df %>% mutate(
    PlateLocSide   = suppressWarnings(as.numeric(PlateLocSide)),
    PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
    ExitSpeed      = suppressWarnings(as.numeric(ExitSpeed)),
    Angle          = suppressWarnings(as.numeric(Angle)),
    Balls          = suppressWarnings(as.integer(Balls)),
    Strikes        = suppressWarnings(as.integer(Strikes)),
    OutsOnPlay     = suppressWarnings(as.numeric(OutsOnPlay)),
    PitchofPA      = suppressWarnings(as.integer(PitchofPA))   # <-- add
  )
}

  # Overwrite the parameters in-place so every downstream call
  # (make_split_stats_hitter, make_density_plots_hitter, etc.)
  # sees numeric columns — not just the per-batter slices.
  game_data   <- coerce_numerics(game_data)
  season_data <- coerce_numerics(season_data)

  dedup <- function(df) {
    key_cols <- intersect(c("GameID","Batter","Inning","Balls","Strikes","Outs",
                            "PitchCall","TaggedPitchType","PlateLocHeight","PlateLocSide"), names(df))
    df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  }

  # No coerce_numerics() needed here anymore — game_data/season_data already coerced above
  game_hitter   <- game_data   %>% filter(Batter == selected_hitter) %>%
                   dedup() %>%
                   score_pitches_xrv(models = active_models)

  season_hitter <- season_data %>% filter(Batter == selected_hitter) %>%
                   dedup() %>%
                   score_pitches_xrv(models = active_models)

  logo_grob <- tryCatch({
    logo_file <- base_team_logo_file()
    if (is.na(logo_file)) stop("Configured team logo was not found")
    img <- magick::image_read(logo_file)
    img <- magick::image_resize(img, "x100")
    grid::rasterGrob(as.raster(img), interpolate=TRUE)
  }, error=function(e) grid::nullGrob())

  counting_stats <- game_hitter %>%
    mutate(
      IsWhiff = PitchCall == "StrikeSwinging",
      IsSwing = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsChase = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip") &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsBall  = PitchCall == "BallCalled" &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsLast  = PitchCall %in% c("InPlay","HitByPitch")
    ) %>%
    summarise(
      PA       = sum(IsLast, na.rm=TRUE),
      Hits     = sum(IsLast & PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm=TRUE),
      `K's`    = sum(IsLast & PitchCall == "StrikeSwinging" & Strikes == 2, na.rm=TRUE) +
                 sum(IsLast & PitchCall == "StrikeCalled"   & Strikes == 2, na.rm=TRUE),
      `BB's`   = sum(IsLast & PitchCall == "BallCalled" & Balls == 3, na.rm=TRUE),
      `2B`     = sum(IsLast & PlayResult == "Double", na.rm=TRUE),
      `3B`     = sum(IsLast & PlayResult == "Triple", na.rm=TRUE),
      HR       = sum(IsLast & PlayResult == "HomeRun", na.rm=TRUE),
      `Whiff%` = paste0(round(sum(IsWhiff,na.rm=TRUE)/pmax(sum(IsSwing,na.rm=TRUE),1)*100,1),"%"),
      `Chase%` = paste0(round(sum(IsChase,na.rm=TRUE)/pmax(sum(IsBall+IsChase,na.rm=TRUE),1)*100,1),"%")
    )

  swing_decisions <- game_hitter %>%
    mutate(
      InZone   = PlateLocHeight>=1.5 & PlateLocHeight<=3.3775 & abs(PlateLocSide)<=0.8303,
      DidSwing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","FoulTip","InPlay"),
      SwDec = case_when(
        InZone  & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 1,
        !InZone & PitchCall == "BallCalled"                                                     ~ 1,
        !InZone & PitchCall == "StrikeCalled" & Strikes < 2                                     ~ 1,
        PitchCall == "HitByPitch"                                                               ~ 1,
        InZone  & PitchCall == "StrikeCalled"                                                   ~ 0,
        !InZone & PitchCall == "StrikeCalled" & Strikes == 2                                    ~ 0,
        !InZone & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 0,
        TRUE ~ NA_real_),
      DecLabel = case_when(
        SwDec==1 & DidSwing ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_)
    ) %>%
    filter(!is.na(SwDec), !is.na(PlateLocHeight), !is.na(PlateLocSide))

  overall_swdec <- swing_decisions %>%
    summarise(Good=sum(SwDec==1), Total=n()) %>%
    mutate(` `="Overall", SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(` `, SwDec, `SwDec%`)

  swdec_by_pitch <- swing_decisions %>%
    mutate(PitchTypeGroup=case_when(
      TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
      TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
      TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(PitchTypeGroup)) %>%
    group_by(PitchTypeGroup) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(`Pitch Type`=PitchTypeGroup, SwDec, `SwDec%`)

  game_swdec_plot <- make_swdec_plot(swing_decisions, "Game Swing Decisions by Location")

  game_xrv_overall <- if (!is.null(active_models) && any(!is.na(game_hitter$xRV_diff)))
    summarise_xrv_swdec(game_hitter) %>% mutate(` `="Overall") %>% select(` `, everything()) else NULL

  game_xrv_by_pitch <- if (!is.null(active_models) && any(!is.na(game_hitter$xRV_diff))) {
    game_hitter %>%
      mutate(PitchTypeGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchTypeGroup)) %>%
      group_by(`Pitch Type`=PitchTypeGroup) %>%
      group_modify(~summarise_xrv_swdec(.x)) %>% ungroup()
  } else NULL

  stats_by_pitch <- game_hitter %>%
    mutate(
      TaggedPitchType_clean = clean_hitter_pitch_type(TaggedPitchType),
      IsSwing     = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsWhiff     = PitchCall == "StrikeSwinging",
      InZone      = abs(PlateLocSide)<=0.8303 & PlateLocHeight>=1.5 & PlateLocHeight<=3.3775,
      IsZoneWhiff = IsWhiff & InZone, IsZoneSwing = IsSwing & InZone,
      IsChase     = IsSwing & !InZone, IsOutZone = !InZone,
      IsHardHit   = PitchCall=="InPlay" & !is.na(ExitSpeed) & ExitSpeed>=95
    ) %>%
    filter(!is.na(TaggedPitchType_clean)) %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      `#`         = n(),
      `Swing%`    = round(sum(IsSwing)/n()*100,1),
      `Whiff%`    = round(ifelse(sum(IsSwing)==0,NA,sum(IsWhiff)/sum(IsSwing)*100),1),
      `IZ Whiff%` = round(ifelse(sum(IsZoneSwing)==0,NA,sum(IsZoneWhiff)/sum(IsZoneSwing)*100),1),
      `Chase%`    = round(ifelse(sum(IsOutZone)==0,NA,sum(IsChase)/sum(IsOutZone)*100),1),
      `Hard Hit%` = round(ifelse(sum(!is.na(ExitSpeed))==0,NA,sum(IsHardHit)/sum(!is.na(ExitSpeed))*100),1),
      `Avg EV`    = round(mean(ExitSpeed,na.rm=TRUE),1),
      `Avg LA`    = round(mean(Angle,na.rm=TRUE),1),
      .groups="drop") %>%
    rename(Type=TaggedPitchType_clean) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.),NA,.)))

  contact_shapes <- c("Take"=21,"Whiff"=4,"In Play"=22,"Hard Hit"=23)

  zone_hitter <- game_hitter %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    mutate(TaggedPitchType_clean=clean_hitter_pitch_type(TaggedPitchType),
           ContactType=case_when(
             ExitSpeed>=95                                                ~ "Hard Hit",
             PitchCall=="StrikeSwinging"                                  ~ "Whiff",
             PitchCall %in% c("StrikeCalled","BallCalled")               ~ "Take",
             PitchCall=="InPlay"                                          ~ "In Play",
             TRUE ~ "Take")) %>%
    filter(!is.na(TaggedPitchType_clean))

  zone_plot <- ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="white", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1) +
    geom_point(data=zone_hitter,
               aes(x=PlateLocSide, y=PlateLocHeight, fill=TaggedPitchType_clean, shape=ContactType),
               size=5, alpha=0.90, color="black", stroke=0.8) +
    geom_text(data=zone_hitter,
              aes(x=PlateLocSide, y=PlateLocHeight, label=PitchofPA),
              size=2.4, color="white", fontface="bold") +
    scale_fill_manual(values=hitter_pitch_colors, drop=TRUE) +
    scale_shape_manual(values=contact_shapes, drop=FALSE) +
    facet_wrap(~TaggedPitchType_clean, nrow=1) +
    labs(title="Strike Zone by Pitch Type", x=NULL, y=NULL) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() + theme_minimal() +
    theme(plot.title=element_text(hjust=0.5,size=10,face="bold"),
          strip.text=element_text(size=8,face="bold"),
          axis.text=element_blank(), axis.ticks=element_blank(),
          panel.grid=element_blank(), legend.position="bottom",
          legend.title=element_text(size=8,face="bold"),
          legend.text=element_text(size=8)) +
    guides(fill="none", shape=guide_legend(title="Contact Type"))

  season_stats <- season_hitter %>%
    mutate(
      IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsWhiff   = PitchCall=="StrikeSwinging",
      IsHardHit = PitchCall=="InPlay" & !is.na(ExitSpeed) & ExitSpeed>=95,
      IsBIP     = PitchCall=="InPlay" & !is.na(ExitSpeed),
      IsHit     = PlayResult %in% c("Single","Double","Triple","HomeRun"),
      Is2B=PlayResult=="Double", Is3B=PlayResult=="Triple", IsHR=PlayResult=="HomeRun",
      IsK=KorBB=="Strikeout", IsBB=KorBB=="Walk", IsHBP=PitchCall=="HitByPitch",
      IsLastPitch = KorBB %in% c("Strikeout","Walk") | PitchCall %in% c("InPlay","HitByPitch"),
      IsPA = IsLastPitch & (KorBB %in% c("Strikeout","Walk") | PitchCall %in% c("InPlay","HitByPitch")),
      IsAB = IsPA & !IsBB & !IsHBP & !PlayResult %in% c("Sacrifice","SacrificeFly")
    ) %>%
    summarise(
      PA=sum(IsPA,na.rm=TRUE), AB=sum(IsAB,na.rm=TRUE),
      H=sum(IsHit & IsLastPitch,na.rm=TRUE), `2B`=sum(Is2B & IsLastPitch,na.rm=TRUE),
      `3B`=sum(Is3B & IsLastPitch,na.rm=TRUE), HR=sum(IsHR & IsLastPitch,na.rm=TRUE),
      BB=sum(IsBB & IsLastPitch,na.rm=TRUE), K=sum(IsK & IsLastPitch,na.rm=TRUE),
      HBP=sum(IsHBP,na.rm=TRUE), Swings=sum(IsSwing,na.rm=TRUE),
      Whiffs=sum(IsWhiff,na.rm=TRUE), BIP=sum(IsBIP,na.rm=TRUE), HH=sum(IsHardHit,na.rm=TRUE)
    ) %>%
    mutate(
      AVG=sprintf("%.3f",round(H/pmax(AB,1),3)),
      OBP=sprintf("%.3f",round((H+BB+HBP)/pmax(AB+BB+HBP,1),3)),
      SLG=sprintf("%.3f",round((H+`2B`+2*`3B`+3*HR)/pmax(AB,1),3)),
      `K%`=paste0(round(K/pmax(PA,1)*100,1),"%"),
      `BB%`=paste0(round(BB/pmax(PA,1)*100,1),"%"),
      `Whiff%`=paste0(round(Whiffs/pmax(Swings,1)*100,1),"%"),
      `HardHit%`=paste0(round(HH/pmax(BIP,1)*100,1),"%")
    ) %>%
    select(AVG,OBP,SLG,`K%`,`BB%`,`Whiff%`,`HardHit%`)

  season_color_matrix <- build_color_matrix_hitter(season_stats, hitter_season_benchmarks, hitter_lower_is_better)

  season_swing_decisions <- season_hitter %>%
    mutate(
      InZone   = PlateLocHeight>=1.5 & PlateLocHeight<=3.3775 & abs(PlateLocSide)<=0.8303,
      DidSwing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","FoulTip","InPlay"),
      SwDec=case_when(
        InZone  & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 1,
        !InZone & PitchCall=="BallCalled"                                                       ~ 1,
        !InZone & PitchCall=="StrikeCalled" & Strikes<2                                         ~ 1,
        PitchCall=="HitByPitch"                                                                 ~ 1,
        InZone  & PitchCall=="StrikeCalled"                                                     ~ 0,
        !InZone & PitchCall=="StrikeCalled" & Strikes==2                                        ~ 0,
        !InZone & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 0,
        TRUE ~ NA_real_),
      DecLabel=case_when(
        SwDec==1 & DidSwing ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_)
    ) %>%
    filter(!is.na(SwDec), !is.na(PlateLocHeight), !is.na(PlateLocSide))

  season_overall_swdec <- season_swing_decisions %>%
    summarise(Good=sum(SwDec==1), Total=n()) %>%
    mutate(` `="Overall", SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(` `, SwDec, `SwDec%`) %>%
    bind_rows(data.frame(` `="League Avg", SwDec="-", `SwDec%`=73.0, check.names=FALSE))

  season_swdec_by_pitch <- season_swing_decisions %>%
    mutate(PitchTypeGroup=case_when(
      TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
      TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
      TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(PitchTypeGroup)) %>%
    group_by(PitchTypeGroup) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(`Pitch Type`=PitchTypeGroup, SwDec, `SwDec%`)

  season_swdec_plot <- make_swdec_heatmap(season_swing_decisions, "Season Swing Decisions by Location")

  season_xrv_overall <- if (!is.null(active_models) && any(!is.na(season_hitter$xRV_diff)))
    summarise_xrv_swdec(season_hitter) %>% mutate(` `="Overall") %>% select(` `, everything()) else NULL

  make_split_stats_hitter <- function(data, hand) {
    data %>%
      filter(Batter==selected_hitter, PitcherThrows==hand) %>%
      mutate(
        PitchGroup=case_when(
          TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
          TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaker",
          TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
          TRUE ~ NA_character_),
        IsSwing=PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
        IsContact=PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
        IsOutZone=!is.na(PlateLocSide)&!is.na(PlateLocHeight)&
          (abs(PlateLocSide)>0.8303|PlateLocHeight<1.5|PlateLocHeight>3.3775),
        IsChase=IsSwing&IsOutZone,
        InZone=!is.na(PlateLocSide)&!is.na(PlateLocHeight)&
          abs(PlateLocSide)<=0.8303&PlateLocHeight>=1.5&PlateLocHeight<=3.3775,
        IsWhiff=PitchCall=="StrikeSwinging",
        IsZoneSwing=IsSwing&InZone, IsZoneWhiff=IsWhiff&InZone,
        IsHardHit=PitchCall=="InPlay"&!is.na(ExitSpeed)&ExitSpeed>=95,
        IsBarrel=PitchCall=="InPlay"&!is.na(ExitSpeed)&!is.na(Angle)&ExitSpeed>=95&Angle>=10&Angle<=35,
        IsBIP=PitchCall=="InPlay"&!is.na(ExitSpeed)
      ) %>%
      filter(!is.na(PitchGroup)) %>%
      group_by(`Pitch Type`=PitchGroup) %>%
      summarise(
        `Swing%`   =paste0(round(sum(IsSwing,na.rm=TRUE)/n()*100,1),"%"),
        `Contact%` =paste0(round(ifelse(sum(IsSwing,na.rm=TRUE)==0,NA,sum(IsContact,na.rm=TRUE)/sum(IsSwing,na.rm=TRUE)*100),1),"%"),
        `Chase%`   =paste0(round(ifelse(sum(IsOutZone,na.rm=TRUE)==0,0,sum(IsChase,na.rm=TRUE)/sum(IsOutZone,na.rm=TRUE)*100),1),"%"),
        `Barrel%`  =paste0(round(ifelse(sum(IsBIP,na.rm=TRUE)==0,NA,sum(IsBarrel,na.rm=TRUE)/sum(IsBIP,na.rm=TRUE)*100),1),"%"),
        `IZ Whiff%`=paste0(round(ifelse(sum(IsZoneSwing,na.rm=TRUE)==0,NA,sum(IsZoneWhiff,na.rm=TRUE)/sum(IsZoneSwing,na.rm=TRUE)*100),1),"%"),
        `HardHit%` =paste0(round(ifelse(sum(IsBIP,na.rm=TRUE)==0,NA,sum(IsHardHit,na.rm=TRUE)/sum(IsBIP,na.rm=TRUE)*100),1),"%"),
        `Avg EV`   =round(mean(ExitSpeed[IsBIP],na.rm=TRUE),1),
        `Avg LA`   =round(mean(Angle[IsBIP],na.rm=TRUE),1),
        .groups="drop") %>%
      arrange(factor(`Pitch Type`,levels=c("Fastball","Breaker","Offspeed"))) %>%
      mutate(across(everything(),~ifelse(is.na(.)|.=="NA%","-",as.character(.))))
  }

  rhp_stats <- make_split_stats_hitter(season_data, "Right")
  lhp_stats <- make_split_stats_hitter(season_data, "Left")
  rhp_color_matrix <- build_split_color_matrix_hitter(rhp_stats, hitter_split_bench_map, hitter_split_lower_is_better)
  lhp_color_matrix <- build_split_color_matrix_hitter(lhp_stats, hitter_split_bench_map, hitter_split_lower_is_better)

  make_density_plots_hitter <- function(data, hand) {
    split_data <- data %>%
      filter(Batter==selected_hitter, PitcherThrows==hand) %>%
      mutate(PitchGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaker",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchGroup), !is.na(PlateLocSide), !is.na(PlateLocHeight))
    lapply(c("Fastball","Breaker","Offspeed"), function(grp) {
      grp_data   <- split_data %>% filter(PitchGroup==grp)
      whiff_data <- grp_data  %>% filter(PitchCall=="StrikeSwinging")
      hh_data    <- grp_data  %>% filter(PitchCall=="InPlay",!is.na(ExitSpeed),ExitSpeed>=95)
      if (nrow(grp_data)<1)
        return(ggplot()+theme_void()+labs(title=grp)+theme(plot.title=element_text(hjust=0.5,size=13,face="bold")))
      p <- ggplot(grp_data, aes(x=PlateLocSide,y=PlateLocHeight)) +
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE,interpolate=TRUE) +
        scale_fill_gradient(low="lightblue",high="red") +
        geom_path(data=hitter_strike_zone,aes(x=PlateLocSide,y=PlateLocHeight),color="black",linewidth=1,inherit.aes=FALSE) +
        geom_polygon(data=hitter_home_plate,aes(x=x,y=y),fill="white",color="black",linewidth=0.8,inherit.aes=FALSE)
      if (nrow(whiff_data)>0)
        p <- p+geom_point(data=whiff_data,aes(x=PlateLocSide,y=PlateLocHeight),shape=4,size=2.5,color="black",stroke=1,inherit.aes=FALSE)
      if (nrow(hh_data)>0)
        p <- p+geom_point(data=hh_data,aes(x=PlateLocSide,y=PlateLocHeight),shape=23,size=2.5,color="white",fill=NA,stroke=1,inherit.aes=FALSE)
      p+xlim(-2.5,2.5)+ylim(0,5)+coord_fixed()+labs(title=grp)+theme_minimal()+
        theme(legend.position="none",plot.title=element_text(hjust=0.5,size=13,face="bold"),
              axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid=element_blank())
    }) %>% setNames(c("Fastball","Breaker","Offspeed"))
  }

  density_rhp <- make_density_plots_hitter(season_data, "Right")
  density_lhp <- make_density_plots_hitter(season_data, "Left")

  pdf(output_file, width=8.5, height=11)
  on.exit(try(dev.off(), silent=TRUE), add=TRUE)

  page_header_hitter <- function(name, subtitle) {
    grid.rect(x=0,y=1,width=1,height=0.06,just=c("left","top"),gp=gpar(fill=TEAM_CONFIG$colors$primary,col=NA))
    grid.text(name,     x=0.03,y=0.978,just="left",gp=gpar(col="white",   fontface="bold",cex=1.2))
    grid.text(subtitle, x=0.03,y=0.948,just="left",gp=gpar(col=TEAM_CONFIG$colors$accent, cex=1.0))
    pushViewport(viewport(x=0.96,y=0.965,width=0.07,height=0.08,just=c("center","center")))
    grid.draw(logo_grob); popViewport()
  }
  page_footer_hitter <- function() {
    grid.text(paste("Data: TrackMan |", TEAM_CONFIG$full_name, "Analytics"),
              x=0.5,y=0.02,gp=gpar(cex=0.55,col=TEAM_CONFIG$colors$primary,fontface="italic"))
  }

  tryCatch({
    # PAGE 1 — GAME
    grid.newpage()
    page_header_hitter(hitter_name, "Postgame Hitter Report")
    grid.text("Game Stats", x=0.5,y=0.89,gp=gpar(fontface="bold",cex=1,col=TEAM_CONFIG$colors$primary))
    draw_grid_table(counting_stats, y_top=0.865, x_center=0.5, row_h=0.018, cell_cex=0.90)
    pushViewport(viewport(x=0.5,y=0.82,width=0.96,height=0.30,just=c("center","top")))
    print(zone_plot, newpage=FALSE); popViewport()
    draw_grid_table(stats_by_pitch, title="Stats by Pitch Type",
                    y_top=0.490, x_center=0.5, row_h=0.026, title_cex=0.90, header_cex=0.80, cell_cex=0.80)
    grid.lines(x=c(0.03,0.97), y=c(0.300,0.300), gp=gpar(col=TEAM_CONFIG$colors$accent, lwd=1))
    grid.text("Swing Decisions", x=0.5, y=0.290, gp=gpar(fontface="bold", cex=0.90, col=TEAM_CONFIG$colors$primary))
    grid.text("Game", x=0.25, y=0.290, gp=gpar(fontface="bold", cex=0.75, col=TEAM_CONFIG$colors$primary))
    pushViewport(viewport(x=0.12, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(game_swdec_plot + theme(plot.title=element_blank(),plot.subtitle=element_blank(),legend.position="none"),
          newpage=FALSE); popViewport()
    draw_grid_table(overall_swdec, title="Overall",
                    y_top=0.260, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    draw_grid_table(swdec_by_pitch, title="By Pitch",
                    y_top=0.180, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    if (!is.null(active_models) && !is.null(game_xrv_overall))
      draw_grid_table(game_xrv_overall, title="xRV",
                      y_top=0.075, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    grid.text("Season", x=0.75, y=0.290, gp=gpar(fontface="bold", cex=0.75, col=TEAM_CONFIG$colors$primary))
    pushViewport(viewport(x=0.62, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(season_swdec_plot + theme(plot.title=element_blank(),plot.subtitle=element_blank()),
          newpage=FALSE); popViewport()
    draw_grid_table(season_overall_swdec, title="Overall",
                    y_top=0.260, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60,
                    title_cex=0.82, alt_row_bg="grey80")
    draw_grid_table(season_swdec_by_pitch, title="By Pitch",
                    y_top=0.180, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    if (!is.null(active_models) && !is.null(season_xrv_overall))
      draw_grid_table(season_xrv_overall, title="xRV",
                      y_top=0.075, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    page_footer_hitter()

    # PAGE 2 — SEASON
    grid.newpage()
    page_header_hitter(hitter_name, paste(TEAM_CONFIG$season_label, "Report"))
    draw_grid_table(season_stats, title="Season Stats",
                    y_top=0.900, x_center=0.5, row_h=0.020, table_width=0.65,
                    header_cex=0.80, cell_cex=0.80, title_cex=0.90, color_matrix=season_color_matrix)
    grid.text("Location Density vs. RHP", x=0.5,y=0.83,gp=gpar(fontface="bold",cex=1,col=TEAM_CONFIG$colors$primary))
    grid.text("X = Whiff  |  Diamond = Hard Hit (95+ EV)", x=0.5,y=0.695,gp=gpar(cex=0.58,col="grey40",fontface="italic"))
    pushViewport(viewport(x=0.17,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Offspeed"]],newpage=FALSE); popViewport()
    grid.text("Stats vs. RHP by Pitch Type", x=0.5,y=0.60,gp=gpar(fontface="bold",cex=1,col=TEAM_CONFIG$colors$primary))
    draw_grid_table(rhp_stats, y_top=0.59, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=rhp_color_matrix)
    grid.text("Location Density vs. LHP", x=0.5,y=0.50,gp=gpar(fontface="bold",cex=1,col=TEAM_CONFIG$colors$primary))
    pushViewport(viewport(x=0.17,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Offspeed"]],newpage=FALSE); popViewport()
    grid.text("Stats vs. LHP by Pitch Type", x=0.5,y=0.28,gp=gpar(fontface="bold",cex=1,col=TEAM_CONFIG$colors$primary))
    draw_grid_table(lhp_stats, y_top=0.27, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=lhp_color_matrix)
    page_footer_hitter()
  }, error=function(e) message("Hitter PDF error: ", e$message))
}

# ==========================================
# HUB UI
# ==========================================
BASE_NAV_TABS <- c(
  hub                = "tab_home",
  catcher            = "tab_catcher",
  hitter             = "tab_hitter",
  hitter_scouting    = "tab_hitter_scouting",
  defense            = "tab_defense",
  pitcher            = "tab_pitcher",
  pitcher_player     = "tab_pitcher_player",
  pitcher_mock       = "tab_pcard_mock",
  team_analytics_app = "tab_leaderboards",
  season_pitcher     = "tab_season_pitcher",
  base_media         = "tab_base_media"
)

base_nav_click_js <- function(target) {
  tab_value <- unname(BASE_NAV_TABS[target])
  if (!length(tab_value) || is.na(tab_value)) return("")
  sprintf(
    "var navLink=document.querySelector(\".navbar-nav a[data-value='%s']\");if(navLink){navLink.click();}",
    tab_value
  )
}

apps <- list(
  list(id = "catcher",          title = "Catcher Reports",          page = "catcher",        status = "live"),
  list(id = "hitter",           title = "Postgame Hitter Reports",  page = "hitter",         status = "live"),
  list(id = "hitter_scouting",  title = "Hitter Scouting",         page = "hitter_scouting", status = "live", image_src = "hitter_scouting.png"),
  list(id = "pitcher",          title = "Postgame Pitcher Reports", page = "pitcher",        status = "live"),
  list(id = "pitcher_player",   title = "Pitcher Scouting", page = "pitcher_player", status = "live", image_src = "pitcher_scouting.png"),
  list(id = "defense",          title = "Defensive Analytics", page = "defense", status = "live", image_src = "TXST_Primary.jpg"),
  team_analytics_hub_card(),
  list(id = "umpire",           title = "Umpire Reports",           page = NULL,             status = "live")
)

make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class  <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"
  card_image  <- if (!is.null(app$image_src)) app$image_src else paste0(app$id, ".png")

  if (!is.null(app$page) && app$status == "live") {
    onclick_js <- base_nav_click_js(app$page)
    tags$div(
      onclick = onclick_js,
      class   = card_class,
      style   = "cursor: pointer;",
      tags$img(src = card_image, class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", ">")
        )
      )
    )
  } else {
    tags$a(
      href   = if (!is.null(app$url)) app$url else "#",
      target = "_blank",
      class  = card_class,
      tags$img(src = card_image, class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", ">")
        )
      )
    )
  }
}

hub_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-hub-page",
      tags$div(class = "section-label", "Applications"),
      tags$div(class = "app-grid", lapply(apps, make_card)),
      tags$div(class = "section-label", style = "margin-top: 40px;",
               paste(TEAM_CONFIG$season_label, "Roster")),
      tags$div(
        class = "standings-wrapper",
        tags$div(class = "standings-division-label", "Catchers"),
        tableOutput("roster_catchers"),
        tags$div(class = "standings-division-label", "Infielders"),
        tableOutput("roster_infielders"),
        tags$div(class = "standings-division-label", "Outfielders"),
        tableOutput("roster_outfielders"),
        tags$div(class = "standings-division-label", "Pitchers"),
        tableOutput("roster_pitchers")
      )
    ),
    tags$div(
      class = "hub-footer",
      base_brand_footer()
    )
  )
}

catcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-generator-page",
      tags$h2("Catcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(
        class = "base-control-grid",
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          class = "base-control-panel",
          tags$h4("Select Team & Game", style = "color: var(--navy); margin-bottom: 12px;"),
          checkboxInput("catcher_manual_enabled", "Upload single-game CSV", value = FALSE),
          conditionalPanel(
            condition = "input.catcher_manual_enabled",
            fileInput("catcher_manual_csv", "Game CSV:", accept = c(".csv", ".parquet"),
                      buttonLabel = "Browse", placeholder = "No file selected"),
            helpText("Appended to the configured season data for this session.")
          ),
          uiOutput("catcher_team_select_ui"),
          uiOutput("catcher_date_ui")
        ),
        tags$div(
          tags$h4("Select Catcher", style = "color: var(--navy); margin-bottom: 12px;"),
          uiOutput("catcher_select_ui"),
          radioButtons("catcher_pitch_src", "Pitch Type Source:",
                       choices = c("Tagged" = "tagged", "Auto (backup)" = "auto"),
                       selected = "tagged", inline = TRUE)
        )
      ),
      tags$div(class = "base-action-row", style = "display:flex; gap:12px; align-items:center;",
        actionButton("generate_catcher", "Generate Report", class = "btn btn-primary", style = "width:200px;"),
        downloadButton("download_catcher_all", "Download All (PDF)",
                       class = "btn btn-outline-primary", style = "width:200px;")),
      br(),
      uiOutput("catcher_status"), br(),
      uiOutput("catcher_report_ui")
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}

hitter_ui <- function() {
  tagList(
    tags$div(class="hub-main base-page base-generator-page",
      tags$h2("Hitter Report Generator",
              style="font-family:var(--font-head);color:var(--navy);margin-bottom:24px;"),
      tags$div(class = "base-control-grid", style="display:grid;grid-template-columns:1fr 1fr;gap:32px;margin-bottom:32px;",
        tags$div(class = "base-control-panel",
          tags$h4("Select Team & Game(s)", style="color:var(--navy);margin-bottom:12px;"),
          checkboxInput("hitter_manual_enabled", "Upload single-game CSV", value = FALSE),
          conditionalPanel(
            condition = "input.hitter_manual_enabled",
            fileInput("hitter_manual_csv", "Game CSV:", accept = c(".csv", ".parquet"),
                      buttonLabel = "Browse", placeholder = "No file selected"),
            helpText("Appended to the configured season data for this session.")),
          uiOutput("hitter_team_select_ui"),
          uiOutput("hitter_dates_ui")),
        tags$div(
          tags$h4("Select Player", style="color:var(--navy);margin-bottom:12px;"),
          uiOutput("hitter_select_ui"),
          radioButtons("hitter_pitch_src", "Pitch Type Source:",
                       choices = c("Tagged" = "tagged", "Auto (backup)" = "auto"),
                       selected = "tagged", inline = TRUE))
      ),
      tags$div(class = "base-action-row", style = "display:flex; gap:12px; align-items:center;",
        actionButton("generate_hitter","Generate Report",class="btn btn-primary",style="width:200px;"),
        downloadButton("download_hitter_all","Download All (PDF)",
                       class="btn btn-outline-primary", style="width:200px;")),
      br(),
      uiOutput("hitter_status"), br(),
      uiOutput("hitter_report_ui")
    ),
    tags$div(class="hub-footer", base_brand_footer())
  )
}

# Old PDF-based pitcher_ui() is retained but no longer routed to; the pitcher
# page now renders pitcher_card_ui() (BrewSummaryCard). Kept for reference.
pitcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$h2("Pitcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          tags$h4("Game CSV", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("pitcher_game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("pitcher_select", "Select Pitcher:", choices = NULL),
          tags$h4("Manual Overrides", style = "color: var(--navy); margin-top: 16px; margin-bottom: 8px;"),
          tags$p("Leave blank to use Trackman values.",
                 style = "font-size: 0.82rem; color: var(--text-muted); margin-bottom: 10px;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px;",
            numericInput("manual_pitches", "Pitches", value = NA, min = 0),
            numericInput("manual_ks",      "K's",     value = NA, min = 0),
            numericInput("manual_bbs",     "BB's",    value = NA, min = 0)
          ),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 8px;",
            numericInput("manual_hits", "Hits", value = NA, min = 0),
            numericInput("manual_runs", "ER",   value = NA, min = 0)
          )
        ),
        tags$div(
          tags$h4("Season CSVs", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("pitcher_season_csvs", "Upload Season CSVs:", accept = ".csv", multiple = TRUE,
                    buttonLabel = "Browse", placeholder = "No files selected")
        )
      ),
      actionButton("generate_pitcher", "Generate Report",
                   class = "btn btn-primary", style = "width: 200px;"),
      br(), br(),
      uiOutput("pitcher_status"),
      br(),
      uiOutput("pitcher_download_ui")
    ),
    tags$div(class = "hub-footer",
             base_brand_footer())
  )
}

# Resolve an opponent logo from www/<ABBR>.png, then the NCAA team metadata.
opponent_logo <- function(abbr) {
  direct <- paste0(toupper(as.character(abbr)), ".png")
  if (file.exists(file.path("www", direct))) return(direct)
  logo_url <- team_palette(as.character(abbr))$logo_url
  if (length(logo_url) && !is.na(logo_url) && nzchar(logo_url)) {
    return(logo_url)
  }
  base_team_logo_url()
}

home_quick_link <- function(label, description, target, number) {
  tags$button(
    type = "button",
    class = "home-quick-link",
    onclick = base_nav_click_js(target),
    tags$span(class = "home-quick-number", number),
    tags$span(
      class = "home-quick-copy",
      tags$strong(label),
      tags$small(description)
    ),
    tags$span(class = "home-quick-arrow", HTML("&rarr;"))
  )
}

home_tab_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      #base-home .content-area { background: transparent; }
      #base-home .section-label {
        font-size: 15px; font-weight: 600; letter-spacing: 1.2px;
        text-transform: uppercase; color: var(--base-muted); margin-bottom: 16px;
      }
      .tab-content > .tab-pane { padding: 0; }
      .tab-pane[data-value='tab_leaderboards'] .navbar { display: none !important; }
      .tab-pane[data-value='tab_leaderboards'] .navbar-default { display: none !important; }
      .navbar-nav > li > a[data-value='tab_pcard_mock'] { display: none !important; }
    "))),
    tags$div(
      id = "base-home",
      uiOutput("scoreboard_hero"),
      tags$div(
        class = "content-area",
        tags$section(
          class = "home-command-section",
          tags$div(
            class = "home-section-heading",
            tags$div(
              tags$div(class = "base-eyebrow", "BASE command center"),
              tags$h2("Start with what you need")
            ),
            tags$div(
              class = "home-data-status",
              tags$span(class = "home-status-dot"),
              paste(TEAM_CONFIG$season, "college data ready")
            )
          ),
          tags$div(
            class = "home-quick-grid",
            home_quick_link("Pitcher Reports", "Postgame pitch and outing analysis", "pitcher", "01"),
            home_quick_link("Pitcher Scouting", "Search the full college player pool", "pitcher_player", "02"),
            home_quick_link("Hitter Scouting", "Analyze opposing-hitter tendencies", "hitter_scouting", "03"),
            home_quick_link("Defensive Analytics", "Explore positioning and batted-ball context", "defense", "04"),
            home_quick_link("Leaderboards", "Team rankings and performance trends", "team_analytics_app", "05"),
            home_quick_link("Catcher Reports", "Receiving and game-management reports", "catcher", "06")
          )
        ),
        tags$section(
          class = "home-roster-section",
          tags$div(
            class = "home-section-heading home-roster-heading",
            tags$div(
              tags$div(class = "base-eyebrow", "Team directory"),
              tags$h2(TEAM_CONFIG$roster_label)
            ),
            tags$div(
              class = "pos-filters",
              tags$div(class = "pos-pill active", `data-group` = "Pitchers",    "Pitchers"),
              tags$div(class = "pos-pill",        `data-group` = "Catchers",    "Catchers"),
              tags$div(class = "pos-pill",        `data-group` = "Infielders",  "Infielders"),
              tags$div(class = "pos-pill",        `data-group` = "Outfielders", "Outfielders")
            )
          ),
          uiOutput("home_roster_grid")
        )
      ),
      tags$script(HTML("
        $(document).on('click', '#base-home .pos-pill', function() {
          $('#base-home .pos-pill').removeClass('active');
          $(this).addClass('active');
          var grp = $(this).data('group');
          $('#base-home .player-card').each(function() {
            $(this).toggle($(this).data('group') === grp);
          });
        });
      "))
    )
  )
}

# ==========================================
# UI
# ==========================================
ui <- navbarPage(
  title       = tagList(
    tags$img(
      src = base_supercat_logo_url(),
      alt = "Texas State SuperCat",
      class = "base-nav-mark"
    ),
    tags$span(
      class = "base-brand-lockup",
      tags$span(class = "base-brand-name", "BASE"),
      tags$span(class = "base-brand-team", TEAM_CONFIG$organization)
    )
  ),
  id          = "base_nav",
  collapsible = TRUE,
  windowTitle = paste(TEAM_CONFIG$full_name, "BASE"),
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "icon", href = base_supercat_logo_url()),
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Courier+Prime&family=Source+Sans+3:wght@400;600&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=18"),
      tags$style(HTML(base_brand_css(include_leaderboards = FALSE))),
      tags$style(HTML("
        #base-splash {
          position: fixed; inset: 0; z-index: 9999;
          display: flex; align-items: center; justify-content: center;
          flex-direction: column; overflow: hidden;
          background:
            radial-gradient(circle at 50% 42%, rgba(215,189,138,0.16), transparent 18rem),
            linear-gradient(145deg, var(--base-maroon-deep), var(--base-maroon) 68%, #64272a);
          transition: opacity 0.45s ease;
        }
        #base-splash::before {
          position: absolute; inset: 22px; border: 1px solid rgba(215,189,138,0.22);
          content: ''; pointer-events: none;
        }
        #base-splash.fade-out { opacity: 0; pointer-events: none; }
        #splash-logo {
          width: 132px; height: 132px;
          display: flex; align-items: center; justify-content: center;
          animation: markIn 0.8s cubic-bezier(.2,.75,.25,1) forwards;
          opacity: 0;
        }
        #splash-logo img {
          width: 124px; height: 124px; object-fit: contain;
          filter: drop-shadow(0 12px 22px rgba(0,0,0,0.28));
        }
        #splash-title {
          color: white; font-family: var(--base-font-display); font-size: 34px;
          font-weight: 600; letter-spacing: 8px; margin-top: 18px; margin-left: 8px;
          animation: fadeUp 0.65s ease 0.35s forwards; opacity: 0; text-transform: uppercase;
        }
        #splash-sub {
          color: var(--base-gold-bright); font-size: 11px; font-weight: 600;
          letter-spacing: 2.2px; margin-top: 8px;
          animation: fadeUp 0.65s ease 0.55s forwards; opacity: 0;
          text-align: center; text-transform: uppercase;
        }
        #splash-progress {
          width: 112px; height: 2px; margin-top: 26px; overflow: hidden;
          background: rgba(255,255,255,0.18); opacity: 0;
          animation: fadeUp 0.5s ease 0.7s forwards;
        }
        #splash-progress::after {
          display: block; width: 42%; height: 100%;
          background: var(--base-gold-bright); content: '';
          animation: loadSweep 1.2s ease-in-out 0.75s infinite alternate;
        }
        @keyframes markIn {
          from { transform: translateY(12px) scale(0.88); opacity: 0; }
          to { transform: translateY(0) scale(1); opacity: 1; }
        }
        @keyframes fadeUp {
          from { transform: translateY(8px); opacity: 0; }
          to   { transform: translateY(0);    opacity: 1; }
        }
        @keyframes loadSweep {
          from { transform: translateX(-105%); }
          to { transform: translateX(245%); }
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            $('#base-splash').addClass('fade-out');
            setTimeout(function() { $('#base-splash').remove(); }, 700);
          }, 2800);
        });
      "))
    ),
    tags$div(
      id = "base-splash",
      tags$div(id = "splash-logo", tags$img(src = base_supercat_logo_url())),
      tags$div(id = "splash-title", "BASE"),
      tags$div(id = "splash-sub", TEAM_CONFIG$organization),
      tags$div(id = "splash-progress", role = "progressbar", `aria-label` = "Loading BASE")
    )
  ),
  tabPanel("Home",             value = "tab_home",           home_tab_ui()),
  tabPanel("Pitcher Reports",  value = "tab_pitcher",        pitcher_card_ui()),
  tabPanel("Hitter Reports",   value = "tab_hitter",         hitter_ui()),
  tabPanel("Catcher Reports",  value = "tab_catcher",        catcher_ui()),
  tabPanel("Pitcher Card",     value = "tab_season_pitcher", season_pitcher_card_ui()),
  tabPanel("Pitcher Scouting", value = "tab_pitcher_player", cape_pitcher_player_page_ui()),
  tabPanel("Hitter Scouting",  value = "tab_hitter_scouting", hitter_scouting_page_ui()),
  tabPanel("Defensive Analytics", value = "tab_defense", defense_page_ui()),
  tabPanel("Leaderboards",     value = "tab_leaderboards",   team_analytics_embedded_ui()),
  tabPanel("Umpire Reports",   value = "tab_umpire",
    tags$div(class = "hub-main base-page base-placeholder-page",
      tags$div(class = "base-placeholder-panel",
        tags$div(class = "base-eyebrow", "In development"),
        tags$h2("Umpire Reports"),
        tags$p("This workspace is being prepared for a future BASE release.")))),
  tabPanel("BASE Media", value = "tab_base_media", base_media_ui()),
  tabPanel("Pitcher Card (Mock)", value = "tab_pcard_mock", pcard_report_ui())
)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  observeEvent(input$nav_to, {
    target <- unname(BASE_NAV_TABS[input$nav_to])
    if (length(target) && !is.na(target)) updateNavbarPage(session, "base_nav", selected = target)
  })
  team_analytics_env$server(input, output, session)

  catcher_data <- reactive({
    combine_with_manual(season_data, input$catcher_manual_enabled, input$catcher_manual_csv)
  })

  hitter_data <- reactive({
    combine_with_manual(season_data, input$hitter_manual_enabled, input$hitter_manual_csv)
  })

  output$hub_header_ui <- renderUI({ NULL })
  output$page_content  <- renderUI({ NULL })

  # Reactive next game (re-fetches every 30 min)
  next_game_reactive <- reactivePoll(
    intervalMillis = 1800000,
    session        = session,
    checkFunc      = function() Sys.time(),
    valueFunc      = function() {
      tryCatch(fetch_next_team_game(), error = function(e) NULL)
    }
  )

  # Scoreboard hero
  output$scoreboard_hero <- renderUI({
    ng <- next_game_reactive()

    team_identity <- tags$div(
      class = "home-team-identity",
      tags$div(
        class = "home-team-marks",
        tags$img(
          src = base_conference_logo_url(),
          alt = TEAM_CONFIG$league_name,
          class = "home-team-logo"
        ),
        tags$span(class = "home-team-logo-divider", `aria-hidden` = "true"),
        tags$img(
          src = base_scoreboard_logo_url(),
          alt = paste(TEAM_CONFIG$full_name, "logo"),
          class = "home-team-logo"
        )
      ),
      tags$div(
        tags$div(
          class = "home-season-line",
          "Bobcats Analytics & Scouting Engine"
        ),
        tags$h1("Texas State Baseball"),
        tags$p(
          paste(
            TEAM_CONFIG$competition_level,
            "analytics, scouting, and team intelligence in one workspace."
          )
        )
      )
    )

    if (is.null(ng)) {
      return(tags$section(
        id = "base-scoreboard",
        class = "base-home-hero base-home-hero-empty",
        tags$div(
          class = "base-home-hero-inner",
          team_identity,
          tags$div(
            class = "home-next-game-card is-empty",
            tags$div(class = "home-game-kicker", "Next game"),
            tags$div(class = "home-calendar-mark", tags$span(format(Sys.Date(), "%b")), tags$strong("—")),
            tags$h2("Schedule pending"),
            tags$p("Upcoming matchup details will appear here automatically as soon as a future game is added to the team schedule."),
            tags$div(
              class = "home-schedule-status",
              tags$span(class = "home-status-dot"),
              "Watching for the next scheduled game"
            )
          )
        )
      ))
    }

    opponent   <- ng$opponent   %||% "TBD"
    time_str   <- ng$time_str   %||% "Time TBD"
    venue      <- ng$venue      %||% "Venue TBD"
    wins       <- ng$wins       %||% 0L
    losses     <- ng$losses     %||% 0L
    opp_wins   <- ng$opp_wins   %||% 0L
    opp_losses <- ng$opp_losses %||% 0L
    opp_abbr   <- ng$opp_abbr   %||% "OPP"
    is_home    <- ng$is_home    %||% TRUE
    ms         <- if (!is.null(ng) && !is.na(ng$ms)) ng$ms else (as.numeric(Sys.time()) + 86400) * 1000

    countdown_segments <- list(
      list(ids = c("cd-d1", "cd-d2"), label = "Days"),
      list(ids = c("cd-h1", "cd-h2"), label = "Hours"),
      list(ids = c("cd-m1", "cd-m2"), label = "Minutes"),
      list(ids = c("cd-s1", "cd-s2"), label = "Seconds")
    )

    tags$section(
      id = "base-scoreboard",
      class = "base-home-hero has-game",
      tags$div(
        class = "base-home-hero-inner",
        team_identity,
        tags$div(
          class = "home-next-game-card",
          tags$div(
            class = "home-game-card-header",
            tags$div(
              tags$div(class = "home-game-kicker", "Next game"),
              tags$div(class = "home-game-date", time_str)
            ),
            tags$span(class = "home-game-location-badge", if (isTRUE(is_home)) "Home" else "Away")
          ),
          tags$div(
            class = "home-matchup",
            tags$div(
              class = "home-matchup-team",
              tags$img(src = base_scoreboard_logo_url(), alt = TEAM_CONFIG$full_name),
              tags$div(
                tags$strong(TEAM_CONFIG$abbreviation),
                tags$small(paste0(wins, "–", losses))
              )
            ),
            tags$div(class = "home-matchup-versus", if (isTRUE(is_home)) "VS" else "AT"),
            tags$div(
              class = "home-matchup-team opponent",
              tags$div(
                tags$strong(opponent),
                tags$small(paste0(opp_wins, "–", opp_losses))
              ),
              tags$img(src = opponent_logo(opp_abbr), alt = paste(opponent, "logo"))
            )
          ),
          tags$div(class = "home-game-venue", tags$span("Venue"), tags$strong(venue)),
          tags$div(
            class = "home-countdown",
            lapply(countdown_segments, function(segment) {
              tags$div(
                class = "home-countdown-unit",
                tags$div(
                  class = "home-countdown-digits",
                  tags$span(id = segment$ids[[1]], "0"),
                  tags$span(id = segment$ids[[2]], "0")
                ),
                tags$small(segment$label)
              )
            })
          )
        )
      ),
      tags$script(HTML(paste0(
        "(function(){",
        "if(window.baseHomeCountdown){clearInterval(window.baseHomeCountdown);}",
        "var target=", format(ms, scientific = FALSE, trim = TRUE), ";",
        "function pad(n){return String(Math.max(0,Math.floor(n))).padStart(2,'0');}",
        "function put(a,b,v){var x=document.getElementById(a),y=document.getElementById(b);if(x&&y){x.textContent=v[0];y.textContent=v[1];}}",
        "function tick(){var diff=Math.max(0,target-Date.now());",
        "put('cd-d1','cd-d2',pad(diff/86400000));",
        "put('cd-h1','cd-h2',pad((diff%86400000)/3600000));",
        "put('cd-m1','cd-m2',pad((diff%3600000)/60000));",
        "put('cd-s1','cd-s2',pad((diff%60000)/1000));}",
        "tick();window.baseHomeCountdown=setInterval(tick,1000);",
        "})();"
      )))
    )
  })

  # Roster grid
  make_roster_badge <- function(number) {
    trimws(ifelse(is.null(number), "", as.character(number)))
  }

  make_handedness_label <- function(bats, throws) {
    bats <- trimws(ifelse(is.null(bats), "", as.character(bats)))
    throws <- trimws(ifelse(is.null(throws), "", as.character(throws)))

    if (!nzchar(bats) && !nzchar(throws)) {
      return("")
    }

    paste0(bats, "/", throws)
  }

  make_player_card <- function(name, pos, number, bats, throws, group, visible) {
    click_js <- if (group == "Pitchers") {
      sprintf(
        "Shiny.setInputValue('roster_pitcher_click', %s, {priority:'event'});",
        jsonlite::toJSON(name, auto_unbox = TRUE)
      )
    } else NULL

    tags$div(
      class        = "player-card",
      `data-group` = group,
      style        = paste0(if (!visible) "display:none;" else "",
                            if (!is.null(click_js)) "cursor:pointer;" else ""),
      onclick      = click_js,
      tags$div(class = "p-init", make_roster_badge(number)),
      tags$div(
        tags$div(class = "p-name", name),
        if (nzchar(make_handedness_label(bats, throws))) {
          tags$div(class = "p-info", make_handedness_label(bats, throws))
        }
      )
    )
  }

  output$home_roster_grid <- renderUI({
    roster_count <- sum(vapply(
      list(roster_pitchers, roster_catchers, roster_infielders, roster_outfielders),
      nrow,
      integer(1)
    ))
    if (roster_count == 0L) {
      return(tags$p(
        "Roster unavailable. Configure BASE_ROSTER_FILE to load the college roster.",
        style = "color:#8B8B96; font-size:13px;"
      ))
    }
    tags$div(
      class = "roster-grid",
      tagList(
        mapply(make_player_card, roster_pitchers$Name,    roster_pitchers$Pos,    roster_pitchers$Number,    roster_pitchers$Bats,    roster_pitchers$Throws,    "Pitchers",    TRUE,  SIMPLIFY = FALSE),
        mapply(make_player_card, roster_catchers$Name,    roster_catchers$Pos,    roster_catchers$Number,    roster_catchers$Bats,    roster_catchers$Throws,    "Catchers",    FALSE, SIMPLIFY = FALSE),
        mapply(make_player_card, roster_infielders$Name,  roster_infielders$Pos,  roster_infielders$Number,  roster_infielders$Bats,  roster_infielders$Throws,  "Infielders",  FALSE, SIMPLIFY = FALSE),
        mapply(make_player_card, roster_outfielders$Name, roster_outfielders$Pos, roster_outfielders$Number, roster_outfielders$Bats, roster_outfielders$Throws, "Outfielders", FALSE, SIMPLIFY = FALSE)
      )
    )
  })

pcard_selected <- reactiveVal(NULL)

  observeEvent(input$roster_pitcher_click, {
    req(!is.null(season_data), input$roster_pitcher_click)
    clicked_display <- input$roster_pitcher_click

    updateNavbarPage(session, "base_nav", selected = "tab_pcard_mock")   # NEW — navigate immediately

    team_pitchers <- season_data %>%
      filter(base_team_matches(PitcherTeam)) %>%
      distinct(Pitcher)

    norm_name <- function(x) trimws(tolower(x))

    matched <- team_pitchers %>%
      filter(norm_name(pcard_format_pitcher_name(Pitcher)) == norm_name(clicked_display))

    if (nrow(matched) == 0) {
      showNotification(
        paste0("No Trackman data found yet for ", clicked_display, "."),
        type = "warning"
      )
      pcard_selected(NULL)
      return()
    }

    pc <- tryCatch(
      pcard_build_all(season_data, matched$Pitcher[1]),
      error = function(e) {
        showNotification(paste("Pitcher card build failed:", e$message), type = "error")
        NULL
      }
    )
    pcard_selected(pc)
  }, ignoreInit = TRUE)

  output$pcard_missing_msg <- renderUI({
    if (is.null(pcard_selected())) {
      tags$p("Click a pitcher on the Home roster to load their card.",
             style = "color:#8B8B96; font-size:13px;")
    } else NULL
  })

  output$pcard_header_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_header_page(pcard_selected()$pitcher_raw)
  })

  output$pcard_boxscore_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_boxscore_page(pcard_selected()$box_stats)
  })

  output$pcard_movement_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_movement_page(pcard_selected()$p_movement)
  })

  output$pcard_pitch_metrics_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_pitch_metrics_page(pcard_selected()$pitch_metrics_tbl)
  })

  output$pcard_location_lhh_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_location_lhh_page(pcard_selected()$p_location_lhh)
  })

  output$pcard_location_rhh_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_location_rhh_page(pcard_selected()$p_location_rhh)
  })

  output$pcard_usage_overall_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_single_table_page(pcard_selected()$usage_total, "Overall")
  })

  output$pcard_usage_rhh_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_single_table_page(pcard_selected()$usage_rhh, "RHH")
  })

  output$pcard_usage_lhh_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_single_table_page(pcard_selected()$usage_lhh, "LHH")
  })
  output$pcard_hit_metrics_plot <- renderPlot({
    req(pcard_selected())
    tryCatch({
      pcard_draw_hit_metrics_page(pcard_selected()$hit_metrics_tbl)
    }, error = function(e) {
      grid::grid.newpage()
      grid::grid.text(paste("Hit metrics error:", e$message),
                      gp = grid::gpar(col = "red", fontsize = 12))
    })
  })

  output$pcard_count_usage_plot <- renderPlot({
    req(pcard_selected())
    pcard_draw_count_usage_page(pcard_selected()$count_usage_tbl)
  })

  output$pcard_heatmap_pitch_ui <- renderUI({
    req(pcard_selected())
    choices <- c("All", pcard_selected()$pitch_types)
    selectInput("pcard_heatmap_pitch", "Pitch Type:", choices = choices, selected = "All", width = "220px")
  })

  output$pcard_heatmap_plot <- renderPlot({
    req(pcard_selected(), input$pcard_heatmap_pitch, input$pcard_heatmap_side)
    pcard_density_heatmap(
      pcard_selected()$pitcher_data,
      pitch_type = input$pcard_heatmap_pitch,
      side       = input$pcard_heatmap_side
    )
  })

  # ==========================================
  # CATCHER SERVER LOGIC
  # ==========================================
  output$catcher_team_select_ui <- renderUI({
    if (is.null(catcher_data()) || nrow(catcher_data()) == 0) return(base_data_notice())
    teams <- base_team_choices(catcher_data()$CatcherTeam)
    if (!length(teams)) return(base_data_notice())
    selectInput("catcher_team_select", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = base_team_default(teams))
  })

  output$catcher_date_ui <- renderUI({
    req(!is.null(catcher_data()), input$catcher_team_select)
    dates <- catcher_data() %>%
      filter(CatcherTeam == input$catcher_team_select) %>%
      mutate(d = as.Date(as.character(Date))) %>%
      pull(d) %>% unique() %>% sort(decreasing = TRUE)
    selectInput("catcher_date", "Select Game Date:",
                choices = as.character(dates), selected = as.character(dates[1]))
  })

  output$catcher_select_ui <- renderUI({
    req(!is.null(catcher_data()), input$catcher_team_select, input$catcher_date)
    catchers <- catcher_data() %>%
      filter(CatcherTeam == input$catcher_team_select,
             as.character(as.Date(as.character(Date))) == input$catcher_date) %>%
      pull(Catcher) %>% unique() %>% sort()
    req(length(catchers) > 0)
    selectInput("catcher_name", "Select Catcher:", choices = catchers)
  })

  catcher_pdf_path  <- reactiveVal(NULL)
  catcher_png_paths <- reactiveVal(NULL)
  catcher_opp       <- reactiveVal(NA_character_)

  observeEvent(input$generate_catcher, {
    req(input$catcher_name, input$catcher_team_select, input$catcher_date, !is.null(catcher_data()))
    output$catcher_status <- renderUI({ div(style = "color:orange;font-weight:bold;", "Generating report...") })
    tryCatch({
      team      <- input$catcher_team_select
      game_date <- as.Date(input$catcher_date)
      md        <- catcher_data()
      game_raw  <- md %>% filter(as.Date(as.character(Date)) == game_date)
      catcher_opp(most_common_opponent(game_raw %>% filter(CatcherTeam == team), "BatterTeam"))
      game_dat   <- prep_catcher_data(game_raw, team, input$catcher_pitch_src)
      season_dat <- prep_catcher_data(md %>% filter(as.Date(as.character(Date)) <= game_date), team, input$catcher_pitch_src)
      tmp_pdf <- tempfile(fileext = ".pdf")
      generate_catcher_pdf(
        game_framing    = game_dat$framing    %>% mutate(Date = as.Date(as.character(Date))),
        game_throwing   = game_dat$throwing   %>% mutate(Date = as.Date(as.character(Date))),
        season_framing  = season_dat$framing  %>% mutate(Date = as.Date(as.character(Date))),
        season_throwing = season_dat$throwing %>% mutate(Date = as.Date(as.character(Date))),
        catcher     = input$catcher_name,
        game_date   = game_date,
        output_file = tmp_pdf,
        logo_path   = base_team_logo_file()
      )
      catcher_pdf_path(tmp_pdf)
      output$catcher_status <- renderUI({ div(style = "color:orange;font-weight:bold;", "Rendering pages...") })
      catcher_png_paths(pdf_to_pngs(tmp_pdf))
      output$catcher_status <- renderUI({ div(style = "color:green;font-weight:bold;", "\u2713 Report ready!") })
    }, error = function(e) {
      message("catcher ERROR: ", e$message)
      output$catcher_status <- renderUI({ div(style = "color:red;", paste("Error:", e$message)) })
    })
  })

  output$catcher_report_ui <- renderUI({
    req(catcher_png_paths(), catcher_pdf_path())
    report_viewer_ui(catcher_png_paths(), catcher_pdf_path(),
                     "download_catcher_pdf", "download_catcher_png")
  })

  output$download_catcher_pdf <- downloadHandler(
    filename = function() paste0(report_base_name(input$catcher_name, input$catcher_date, catcher_opp(), "Catcher"), ".pdf"),
    content  = function(file) { req(catcher_pdf_path()); file.copy(catcher_pdf_path(), file, overwrite = TRUE) }
  )

  output$download_catcher_png <- downloadHandler(
    filename = function() paste0(report_base_name(input$catcher_name, input$catcher_date, catcher_opp(), "Catcher"), ".png"),
    content  = function(file) { req(catcher_png_paths()); combine_pngs(catcher_png_paths(), file) }
  )

  output$download_catcher_all <- downloadHandler(
    filename = function() {
      d <- suppressWarnings(as.Date(input$catcher_date))
      paste0(unname(team_display_name(input$catcher_team_select)),
             " - ", format(d, "%B %d"), " Catcher Reports.zip")
    },
    content = function(file) {
      req(!is.null(catcher_data()), input$catcher_team_select, input$catcher_date)
      md         <- catcher_data()
      team       <- input$catcher_team_select
      game_date  <- as.Date(input$catcher_date)
      game_raw   <- md %>% filter(as.Date(as.character(Date)) == game_date)
      season_raw <- md %>% filter(as.Date(as.character(Date)) <= game_date)
      opp        <- most_common_opponent(game_raw %>% filter(CatcherTeam == team), "BatterTeam")
      gd <- prep_catcher_data(game_raw,   team, input$catcher_pitch_src)
      sd <- prep_catcher_data(season_raw, team, input$catcher_pitch_src)
      catchers <- sort(unique(c(gd$framing$Catcher, gd$throwing$Catcher)))
      req(length(catchers) > 0)
      tmp_dir   <- file.path(tempdir(), paste0("catcher_all_", as.integer(Sys.time())))
      dir.create(tmp_dir, showWarnings = FALSE)
      out_files <- character(0)
      for (c_name in catchers) {
        fn <- file.path(tmp_dir, paste0(report_base_name(c_name, game_date, opp, "Catcher"), ".pdf"))
        ok <- tryCatch({
          generate_catcher_pdf(
            game_framing    = gd$framing    %>% mutate(Date = as.Date(as.character(Date))),
            game_throwing   = gd$throwing   %>% mutate(Date = as.Date(as.character(Date))),
            season_framing  = sd$framing    %>% mutate(Date = as.Date(as.character(Date))),
            season_throwing = sd$throwing   %>% mutate(Date = as.Date(as.character(Date))),
            catcher     = c_name,
            game_date   = game_date,
            output_file = fn,
            logo_path   = base_team_logo_file()
          )
          TRUE
        }, error = function(e) { message("skip catcher ", c_name, ": ", e$message); FALSE })
        if (ok) out_files <- c(out_files, fn)
      }
      req(length(out_files) > 0)
      zip(file, files = out_files, flags = "-j")
    }
  )

  # ==========================================
  # HITTER SERVER
  # ==========================================
  output$hitter_team_select_ui <- renderUI({
    if (is.null(hitter_data()) || nrow(hitter_data()) == 0) return(base_data_notice())
    teams <- base_team_choices(hitter_data()$BatterTeam)
    if (!length(teams)) return(base_data_notice())
    selectInput("hitter_team_select", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = base_team_default(teams))
  })

  output$hitter_dates_ui <- renderUI({
    req(!is.null(hitter_data()), input$hitter_team_select)
    dates <- hitter_data() %>%
      filter(BatterTeam == input$hitter_team_select) %>%
      mutate(d = as.Date(as.character(Date))) %>%
      pull(d) %>% unique() %>% sort(decreasing = TRUE)
    selectInput("hitter_dates", "Select Date(s):",
                choices   = as.character(dates),
                selected  = as.character(dates[1]),
                multiple  = TRUE,
                selectize = TRUE)
  })

  output$hitter_select_ui <- renderUI({
    req(!is.null(hitter_data()), input$hitter_team_select, input$hitter_dates)
    hitters <- hitter_data() %>%
      filter(BatterTeam == input$hitter_team_select,
             as.character(as.Date(as.character(Date))) %in% input$hitter_dates) %>%
      pull(Batter) %>% unique() %>% sort()
    req(length(hitters) > 0)
    selectInput("selected_hitter", "Select Hitter:", choices = hitters)
  })

  hitter_pdf_path  <- reactiveVal(NULL)
  hitter_png_paths <- reactiveVal(NULL)
  hitter_opp       <- reactiveVal(NA_character_)

  observeEvent(input$generate_hitter, {
    req(input$selected_hitter, input$hitter_team_select, input$hitter_dates, !is.null(hitter_data()))
    output$hitter_status <- renderUI({ div(style = "color:orange;font-weight:bold;", "Generating report...") })
    tryCatch({
      selected_dates <- as.Date(input$hitter_dates)
      team           <- input$hitter_team_select
      md             <- hitter_data()
      game_data   <- md %>% filter(as.Date(as.character(Date)) %in% selected_dates,   BatterTeam == team) %>% apply_pitch_source(input$hitter_pitch_src)
      season_data <- md %>% filter(as.Date(as.character(Date)) <= max(selected_dates), BatterTeam == team) %>% apply_pitch_source(input$hitter_pitch_src)
      hitter_opp(most_common_opponent(game_data %>% filter(Batter == input$selected_hitter), "PitcherTeam"))
      tmp_pdf <- tempfile(fileext = ".pdf")
      generate_hitter_pdf(
        game_data       = game_data,
        season_data     = season_data,
        selected_hitter = input$selected_hitter,
        output_file     = tmp_pdf,
        active_models   = sd_models
      )
      hitter_pdf_path(tmp_pdf)
      output$hitter_status <- renderUI({ div(style = "color:orange;font-weight:bold;", "Rendering pages...") })
      hitter_png_paths(pdf_to_pngs(tmp_pdf))
      output$hitter_status <- renderUI({ div(style = "color:green;font-weight:bold;", "\u2713 Report ready!") })
    }, error = function(e) {
      message("hitter ERROR: ", e$message)
      output$hitter_status <- renderUI({ div(style = "color:red;", paste("Error:", e$message)) })
    })
  })

  output$hitter_report_ui <- renderUI({
    req(hitter_png_paths(), hitter_pdf_path())
    report_viewer_ui(hitter_png_paths(), hitter_pdf_path(),
                     "download_hitter_pdf", "download_hitter_png")
  })

  output$download_hitter_pdf <- downloadHandler(
    filename = function() paste0(report_base_name(input$selected_hitter, input$hitter_dates, hitter_opp(), "Hitter"), ".pdf"),
    content  = function(file) { req(hitter_pdf_path()); file.copy(hitter_pdf_path(), file, overwrite = TRUE) }
  )

  output$download_hitter_png <- downloadHandler(
    filename = function() paste0(report_base_name(input$selected_hitter, input$hitter_dates, hitter_opp(), "Hitter"), ".png"),
    content  = function(file) { req(hitter_png_paths()); combine_pngs(hitter_png_paths(), file) }
  )

  output$download_hitter_all <- downloadHandler(
    filename = function() {
      d <- suppressWarnings(max(as.Date(input$hitter_dates)))
      paste0(unname(team_display_name(input$hitter_team_select)),
             " - ", format(d, "%B %d"), " Hitter Reports.zip")
    },
    content = function(file) {
      req(!is.null(hitter_data()), input$hitter_team_select, input$hitter_dates)
      team           <- input$hitter_team_select
      selected_dates <- as.Date(input$hitter_dates)
      base_df    <- hitter_data() %>% filter(BatterTeam == team)
      game_all   <- base_df %>% filter(as.Date(as.character(Date)) %in% selected_dates)    %>% apply_pitch_source(input$hitter_pitch_src)
      season_all <- base_df %>% filter(as.Date(as.character(Date)) <= max(selected_dates)) %>% apply_pitch_source(input$hitter_pitch_src)
      hitters <- sort(unique(game_all$Batter))
      req(length(hitters) > 0)
      tmp_dir   <- file.path(tempdir(), paste0("hitter_all_", as.integer(Sys.time())))
      dir.create(tmp_dir, showWarnings = FALSE)
      out_files <- character(0)
      for (h in hitters) {
        opp <- most_common_opponent(game_all %>% filter(Batter == h), "PitcherTeam")
        fn  <- file.path(tmp_dir, paste0(report_base_name(h, selected_dates, opp, "Hitter"), ".pdf"))
        ok  <- tryCatch({
          generate_hitter_pdf(
            game_data       = game_all,
            season_data     = season_all,
            selected_hitter = h,
            output_file     = fn,
            active_models   = sd_models
          )
          TRUE
        }, error = function(e) { message("skip hitter ", h, ": ", e$message); FALSE })
        if (ok) out_files <- c(out_files, fn)
      }
      req(length(out_files) > 0)
      zip(file, files = out_files, flags = "-j")
    }
  )

  # ==========================================
  # CAPE PITCHER PLAYER PAGE
  # ==========================================
  cape_pitcher_player_page_server(
    input,
    output,
    session,
    catalog_data = base_pitcher_catalog,
    player_loader = base_load_pitcher_rows,
    supplement_data = cape26_data
  )

  # ==========================================
  # HITTER SCOUTING PAGE
  # ==========================================
  hitter_scouting_page_server(
    input,
    output,
    session,
    catalog_loader = base_get_hitter_catalog,
    player_loader = base_load_hitter_rows,
    supplement_data = cape26_data
  )

  # ==========================================
  # DEFENSIVE ANALYTICS PAGE
  # ==========================================
  defense_page_server(input, output, session)

  # ==========================================
  # PITCHER SERVER LOGIC -> BrewSummaryCard
  # ==========================================
  pitcher_card_server(input, output, session)

  # ==========================================
  # SEASON PITCHER CARD SERVER (College26)
  # ==========================================
  season_pitcher_card_server(input, output, session)

  base_media_server(input, output, session)

}

      
shinyApp(ui = ui, server = server)
