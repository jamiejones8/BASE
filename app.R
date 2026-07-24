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

combine  <- dplyr::combine
slice    <- dplyr::slice
between  <- dplyr::between
first    <- dplyr::first
last     <- dplyr::last

source("scout_app.R")
source("leaderboards_embed.R")
source("cape_pitcher_page.R")
source("Pitcher_Card.R")
# ══════════════════════════════════════════════════════════════════════════════
# HF HUB WRITE-BACK HELPER — now points at a Dataset repo, not the Space repo
# Dataset repos don't trigger Space rebuilds on commit, so ineligible list
# changes no longer restart the app.
# ══════════════════════════════════════════════════════════════════════════════

HF_DATA_REPO_ID   <- "BrewsterWhitecapsMAC/acq-board-data"
HF_DATA_REPO_TYPE <- "dataset"
SEASON_DATA_FILE      <- "CapeCod26.parquet"
SEASON_DATA_REPO_ID   <- Sys.getenv("CAPE_DATA_REPO_ID", unset = HF_DATA_REPO_ID)
SEASON_DATA_REPO_PATH <- Sys.getenv("CAPE_DATA_REPO_PATH", unset = SEASON_DATA_FILE)

push_file_to_hf <- function(local_path, repo_path,
                            commit_message = paste("Update", repo_path),
                            repo_id = HF_DATA_REPO_ID) {

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
  token <- Sys.getenv("write_token")
  url <- glue::glue(
    "https://huggingface.co/datasets/{repo_id}/resolve/main/{repo_path}"
  )
  tmp_path <- tempfile(tmpdir = dirname(local_path),
                       pattern = "hf_pull_",
                       fileext = paste0(".", tools::file_ext(local_path)))

  resp <- tryCatch({
    if (nzchar(token)) {
      httr::GET(
        url,
        httr::add_headers(Authorization = paste("Bearer", token)),
        httr::write_disk(tmp_path, overwrite = TRUE),
        httr::timeout(15)
      )
    } else {
      httr::GET(
        url,
        httr::write_disk(tmp_path, overwrite = TRUE),
        httr::timeout(15)
      )
    }
  }, error = function(e) NULL)

  if (is.null(resp) || httr::http_error(resp)) {
    if (file.exists(tmp_path)) unlink(tmp_path)
    message("HF pull failed for ", repo_path, " — using local fallback if present.")
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


fetch_next_whitecaps_game <- function() {
  resp <- tryCatch(
    httr::GET(paste0(
      "https://statsapi.mlb.com/api/v1/schedule",
      "?sportId=22",
      "&leagueId=565",
      "&teamId=6096",
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
      is_home <- g$teams$home$team$id == 6096

      opponent <- if (is_home) g$teams$away$team$name else g$teams$home$team$name

      # Venue: always the home team's venue
      venue <- g$venue$name

      # Record: Brewster's side
      brew_side <- if (is_home) g$teams$home else g$teams$away
      wins   <- brew_side$leagueRecord$wins
      losses <- brew_side$leagueRecord$losses

      # Opponent record
      opp_side <- if (is_home) g$teams$away else g$teams$home
      opp_wins   <- opp_side$leagueRecord$wins
      opp_losses <- opp_side$leagueRecord$losses

      game_dt_utc <- as.POSIXct(g$gameDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      game_dt_est <- lubridate::with_tz(game_dt_utc, "America/New_York")

      
      teams_resp <- tryCatch(
        httr::GET("https://statsapi.mlb.com/api/v1/teams?leagueId=565", httr::timeout(10)),
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

message("Fetching next Whitecaps game...")
next_game <- tryCatch(fetch_next_whitecaps_game(), error = function(e) NULL)

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
# Card engine for the CAPS Postgame Pitcher Reports tab.
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
Height26 <- read_csv("College26Heights.csv")

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
    # hms/Date/POSIXct classes the CapeCod26 parquet uses. With base read.csv
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
  navy         = "#0C2340",
  cardinal     = "#C8102E",
  cardinal_glow= "#E2374B",
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
  fallback <- list(primary = "#0C2340", secondary = "#C8102E",
                   label = team_abbr, logo_url = NA_character_)
  if (is.null(ncaa_colors) || length(team_abbr) == 0 ||
      is.na(team_abbr) || team_abbr == "") return(fallback)
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
                           team_label   = "BREWSTER WHITECAPS",
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
    textGrob("Brewster Whitecaps",
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
# Card assembly (extracted from the standalone app's observeEvent so the CAPS
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
# CAPS "Postgame Pitcher Reports" tab — BrewSummaryCard, embedded.
#
# Provides:
#   pitcher_card_ui()                         -> the page UI (drop into page_content)
#   pitcher_card_server(input, output, session) -> the server logic (call once)
#
# All input/output IDs are prefixed `pc_` so they don't collide with the CAPS
# app's existing pitcher-report inputs. Depends on pitcher_report_card.R being
# sourced first (model, pitcher_summary, build_pitcher_card_page, tok, etc.).
# ============================================================================
library(plotly)

pitcher_card_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 16px;",
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
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
            helpText("Appended to the CapeCod26 season so its date appears below. ",
                     "Season stats still use CapeCod26.")
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
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

pitcher_card_server <- function(input, output, session) {

  # CapeCod26 season, optionally with this page's manual single-game CSV appended.
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
  # (CapeCod26 season + optional manual single-game CSV) — same source as
  # the hitter reports.
  output$pc_team_ui <- renderUI({
    req(!is.null(master_data()))
    teams <- sort(unique(master_data()$PitcherTeam))
    selectInput("pc_team", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = teams[grepl("BRE|Brewster", teams, ignore.case = TRUE)][1])
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

  # Full-season pitches for the selected pitcher (all dates in CapeCod26), used
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

    # Season card (all of this pitcher's CapeCod26 pitches) for the PDF's 2nd page.
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
# Full-season summary card built from college26.parquet (loaded in
# preprocessing above as `college26_data`). Reuses the BrewSummaryCard engine
# (build_pitcher_card_page / draw_card_to_png / draw_cards_to_pdf). No date
# selector or retag flow — the card aggregates every pitch the selected pitcher
# threw in the College26 season. All IDs are prefixed `spc_`.
#   season_pitcher_card_ui()                         -> the page UI
#   season_pitcher_card_server(input, output, session) -> the server logic
# ============================================================================
season_pitcher_card_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 16px;",
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
      tags$h2("Season Pitcher Summary Card",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 16px;"),
      if (is.null(college26_data))
        tags$div(class = "alert alert-warning",
                 "College26 season data is unavailable — check that college26.parquet ",
                 "loaded from the acq-board-data dataset at startup."),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("spc_team_ui"),
          uiOutput("spc_pitcher_ui"),
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
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
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
    req(length(college26_team_choices) > 0)
    brew <- college26_team_choices[grepl("BRE|Brewster", college26_team_choices,
                                         ignore.case = TRUE)]
    selectInput("spc_team", "Select Team:",
                choices  = college26_team_choices,
                selected = if (length(brew)) unname(brew[1]) else unname(college26_team_choices[1]))
  })

  output$spc_pitcher_ui <- renderUI({
    req(input$spc_team)
    pitchers <- college26_pitchers_by_team[[input$spc_team]]
    req(length(pitchers) > 0)
    selectInput("spc_pitcher", "Select Pitcher:", choices = pitchers)
  })

  # Every College26 pitch for the selected pitcher, run through the same
  # preprocessing the game card uses.
  load_pitcher_pitches_season <- function() {
    req(!is.null(college26_data), input$spc_team, input$spc_pitcher)
    college26_data %>%
      filter(PitcherTeam == input$spc_team, Pitcher == input$spc_pitcher) %>%
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


caps_media_server <- function(input, output, session) {

  cm_selected <- reactiveVal(NULL)
  cm_selection_uids <- reactiveVal(NULL)

  observe({
    req(!is.null(college26_data))
    player_choices <- college26_data %>%
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
    req(!is.null(college26_data), input$cm_player_search, nzchar(input$cm_player_search))
    parts    <- strsplit(input$cm_player_search, "\\|\\|")[[1]]
    raw_p    <- parts[1]
    raw_team <- parts[2]
    pc <- tryCatch(
      pcard_build_all(college26_data %>% filter(PitcherTeam == raw_team), raw_p),
      error = function(e) { showNotification(paste("Card build failed:", e$message), type = "error"); NULL }
    )
    cm_selected(pc)
    cm_selection_uids(NULL)
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
    updateSelectizeInput(session, "cm_filter_picker", choices = c("" = "", remaining), selected = "")
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

fetch_whitecaps_roster <- function() {
  resp <- tryCatch(
    httr::GET("https://statsapi.mlb.com/api/v1/teams/6096/roster?season=2026&hydrate=person",
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

message("Fetching Whitecaps roster...")
roster_raw <- tryCatch(fetch_whitecaps_roster(), error = function(e) NULL)

if (!is.null(roster_raw)) {
  roster_pitchers   <- roster_raw %>% filter(pos_type == "Pitcher")   %>% select(Name, Pos, Number, Bats, Throws)
  roster_catchers   <- roster_raw %>% filter(pos_type == "Catcher")   %>% select(Name, Pos, Number, Bats, Throws)
  roster_infielders <- roster_raw %>% filter(pos_type == "Infielder") %>% select(Name, Pos, Number, Bats, Throws)
  roster_outfielders <- roster_raw %>% filter(pos_type == "Outfielder") %>% select(Name, Pos, Number, Bats, Throws)
} else {
  message("Roster fetch failed — using hardcoded fallback")
  roster_pitchers <- data.frame(
    Name = c("Charlie Willcox","Nate Harris","Logan Eisenrich","Ethan Grim","Zach Kmatz",
             "Jordan Martin","Finbar O'Brien","Landon Mack","Joshua Whritenour",
             "Schuyler Sandford","Jordan Regulski","Carter Williams","Maverick Rizy",
             "Frank Menendez","Tommy Conley","Sebastian Santos-Olsen","Trent Collier",
             "Charlie West","Nate Smithburg","Tye Briscoe"),
    Pos    = c(rep("RHP",13),"LHP","LHP","LHP","LHP","LHP","LHP","LHP"),
    Number = rep("", 20),
    Bats   = rep("", 20),
    Throws = rep("", 20),
    stringsAsFactors = FALSE
  )
  roster_catchers <- data.frame(
    Name = c("Owen Jenkins","Jacob Lee","Jimmy Janicki"),
    Pos = c("C","C","C"), Number = rep("", 3), Bats = rep("", 3), Throws = rep("", 3), stringsAsFactors = FALSE
  )
  roster_infielders <- data.frame(
    Name = c("Dalton Wentz","Brendan Lawson","Pete Daniel","Nicholas Partida",
             "Will Moore","Dane Harvey","Petey Craska","Jamie Laskofski",
             "Landon Penfield","Jacob Lambdin","Jett Kenady","Alexander Peck"),
    Pos = c("MINF","SS","SS","SS","INF","1B","1B","SS","3B","SS","SS","SS"),
    Number = rep("", 12), Bats = rep("", 12), Throws = rep("", 12), stringsAsFactors = FALSE
  )
  roster_outfielders <- data.frame(
    Name = c("Adam Magpoc","Brody DeLamielleure","Michael Torres","Frank Carney",
             "Terrence Kiel II","Jay Abernathy","Blaine Brown","Cash Strayer","Eric Hines"),
    Pos = rep("OF", 9), Number = rep("", 9), Bats = rep("", 9), Throws = rep("", 9), stringsAsFactors = FALSE
  )
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
        style = "margin-bottom: 16px;",
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
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
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
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
                            header_bg    = "#0C2340",
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
              gp = gpar(fontface = "bold", cex = title_cex, col = "#0C2340"))
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
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
    grid.text(as.character(game_date), x = 0.5, y = 0.05,
              gp = gpar(cex = 0.85, col = "#0C2340"))
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
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = PLOT_TOP - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(g_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = PLOT_TOP - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(g_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

    # PAGE 2 - SEASON
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "- Season Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
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
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = plot_top2 - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(s_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = plot_top2 - 0.02, width = 0.42, height = 0.24, just = c("center","top")))
    print(s_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
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
# HITTER DATA SOURCE + REPORT HELPERS (from capstest.R)
# ==========================================
download_from_hf_dataset <- function(repo_id, filename, token) {
  url  <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filename)
  tmp  <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  resp <- httr::GET(url,
                    httr::add_headers(Authorization = paste("Bearer", token)),
                    httr::write_disk(tmp, overwrite = TRUE),
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

message("Loading master game data...")
# Pull the latest deployed season parquet from the configured dataset repo,
# then fall back to the local copy if the remote fetch is unavailable.
invisible(pull_season_data_from_hf())

season_data <- tryCatch({
  df <- arrow::read_parquet(SEASON_DATA_FILE)
  message("Loaded ", SEASON_DATA_FILE, " — rows: ", nrow(df))
  df
}, error = function(e) {
  message(SEASON_DATA_FILE, " load failed: ", e$message)
  NULL
})

master_last_updated <- if (!is.null(season_data)) format(Sys.time(), "%b %d, %Y at %I:%M %p") else "unavailable"
message("Season data rows: ", if (!is.null(season_data)) nrow(season_data) else 0)

# ----------------------------------------------------------------------------
# College26 season pitch data (large) for the Season Pitcher Card tab. Pulled
# once at startup straight from the private acq-board-data HF dataset (to a
# tempfile, Bearer-auth) and read from there — no local copy is kept.
# ----------------------------------------------------------------------------
COLLEGE26_FILE      <- "College26.parquet"
COLLEGE26_REPO_ID   <- Sys.getenv("COLLEGE26_REPO_ID",   unset = HF_DATA_REPO_ID)
COLLEGE26_REPO_PATH <- Sys.getenv("COLLEGE26_REPO_PATH", unset = COLLEGE26_FILE)

message("Loading College26 season data from HF dataset ", COLLEGE26_REPO_ID, "...")
college26_data <- tryCatch({
  path <- download_from_hf_dataset(COLLEGE26_REPO_ID, COLLEGE26_REPO_PATH,
                                   Sys.getenv("write_token"))
  df <- arrow::read_parquet(path)
  message("Loaded ", COLLEGE26_REPO_PATH, " from HF — rows: ", nrow(df))
  df
}, error = function(e) {
  message(COLLEGE26_REPO_PATH, " HF load failed: ", e$message)
  NULL
})

# Precompute the Season Pitcher Card selector choices once, at startup, so the
# renderUI selectors never re-scan the large College26 table:
#   college26_team_choices     : named vector (display name -> team abbr)
#   college26_pitchers_by_team : list keyed by team abbr, each a named vector
#                                (formatted pitcher name -> raw Pitcher value)
college26_team_choices     <- character(0)
college26_pitchers_by_team <- list()
if (!is.null(college26_data)) {
  .c26_teams <- sort(unique(college26_data$PitcherTeam))
  .c26_teams <- .c26_teams[!is.na(.c26_teams)]
  college26_team_choices <- setNames(.c26_teams, unname(team_display_name(.c26_teams)))

  .c26_tp <- college26_data %>%
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
# enabled the uploaded game is appended to the CapeCod26 season so its date(s)
# show up in the team/date selectors. The season itself stays CapeCod26.
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

caps_media_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      #cm-page { max-width: 1100px; margin: 0 auto; padding: 0 24px 40px; }
      #cm-page .pcard-panel {
        background: #fff; border: 1px solid #EAEAEE; border-radius: 10px;
        padding: 12px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(12,35,64,0.04);
      }
      #cm-page .pcard-panel img { width: 100%; height: auto; display: block; }
      #cm-page .pcard-row { display: flex; gap: 20px; }
      #cm-page .pcard-row .pcard-panel { flex: 1; min-width: 0; }
      #cm-page .pcard-section-label {
        font-size: 11px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase;
        color: #5F5F6B; margin: 4px 0 10px;
      }
    "))),
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 16px;",
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
      tags$h2("CAPS Media",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 16px;"),
      tags$p("Search any pitcher in the College26 database.",
             style = "color:#5F6B7A; font-size:14px; margin-bottom:20px;"),

      tags$div(
        style = "display: grid; grid-template-columns: 3fr 1fr; gap: 16px; align-items: end; margin-bottom: 24px;",
        tags$div(
          tags$label("Search Player:", style = "font-size:13px; font-weight:600; color:#5F5F6B;"),
          selectizeInput("cm_player_search", NULL, choices = NULL,
                        options = list(placeholder = "Type a name...", maxOptions = 15),
                        width = "100%")
        ),
        actionButton("cm_load", "Load Player", class = "btn btn-primary")
      ),

      tags$div(
        class = "pcard-panel",
        tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                 tags$span("Filters", style = "font-size:13px; font-weight:700; color:#0C2340;"),
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
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
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
  repo  <- "BrewsterWhitecapsMAC/swing-decision-models"
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
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color="#0C2340"),
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
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color="#0C2340"),
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
    img <- magick::image_read("www/logo1.png")
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
    grid.rect(x=0,y=1,width=1,height=0.06,just=c("left","top"),gp=gpar(fill="#0C2340",col=NA))
    grid.text(name,     x=0.03,y=0.978,just="left",gp=gpar(col="white",   fontface="bold",cex=1.2))
    grid.text(subtitle, x=0.03,y=0.948,just="left",gp=gpar(col="#9DC2EA", cex=1.0))
    pushViewport(viewport(x=0.96,y=0.965,width=0.07,height=0.08,just=c("center","center")))
    grid.draw(logo_grob); popViewport()
  }
  page_footer_hitter <- function() {
    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x=0.5,y=0.02,gp=gpar(cex=0.55,col="#0C2340",fontface="italic"))
  }

  tryCatch({
    # PAGE 1 — GAME
    grid.newpage()
    page_header_hitter(hitter_name, "Postgame Hitter Report")
    grid.text("Game Stats", x=0.5,y=0.89,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(counting_stats, y_top=0.865, x_center=0.5, row_h=0.018, cell_cex=0.90)
    pushViewport(viewport(x=0.5,y=0.82,width=0.96,height=0.30,just=c("center","top")))
    print(zone_plot, newpage=FALSE); popViewport()
    draw_grid_table(stats_by_pitch, title="Stats by Pitch Type",
                    y_top=0.490, x_center=0.5, row_h=0.026, title_cex=0.90, header_cex=0.80, cell_cex=0.80)
    grid.lines(x=c(0.03,0.97), y=c(0.300,0.300), gp=gpar(col="#9DC2EA", lwd=1))
    grid.text("Swing Decisions", x=0.5, y=0.290, gp=gpar(fontface="bold", cex=0.90, col="#0C2340"))
    grid.text("Game", x=0.25, y=0.290, gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
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
    grid.text("Season", x=0.75, y=0.290, gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
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
    page_header_hitter(hitter_name, "2026 Season Report")
    draw_grid_table(season_stats, title="Season Stats",
                    y_top=0.900, x_center=0.5, row_h=0.020, table_width=0.65,
                    header_cex=0.80, cell_cex=0.80, title_cex=0.90, color_matrix=season_color_matrix)
    grid.text("Location Density vs. RHP", x=0.5,y=0.83,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    grid.text("X = Whiff  |  Diamond = Hard Hit (95+ EV)", x=0.5,y=0.695,gp=gpar(cex=0.58,col="grey40",fontface="italic"))
    pushViewport(viewport(x=0.17,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Offspeed"]],newpage=FALSE); popViewport()
    grid.text("Stats vs. RHP by Pitch Type", x=0.5,y=0.60,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(rhp_stats, y_top=0.59, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=rhp_color_matrix)
    grid.text("Location Density vs. LHP", x=0.5,y=0.50,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    pushViewport(viewport(x=0.17,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Offspeed"]],newpage=FALSE); popViewport()
    grid.text("Stats vs. LHP by Pitch Type", x=0.5,y=0.28,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(lhp_stats, y_top=0.27, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=lhp_color_matrix)
    page_footer_hitter()
  }, error=function(e) message("Hitter PDF error: ", e$message))
}

# ==========================================
# HUB UI
# ==========================================
apps <- list(
  list(id = "catcher",          title = "Catcher Reports",          page = "catcher",        status = "live"),
  list(id = "hitter",           title = "Postgame Hitter Reports",  page = "hitter",         status = "live"),
  list(id = "pitcher",          title = "Postgame Pitcher Reports", page = "pitcher",        status = "live"),
  list(id = "pitcher_player",   title = "Cape Pitcher Scout", page = "pitcher_player", status = "live", image_src = "pitcher_scouting.png"),
  whitecaps_hub_card(),
  list(id = "umpire",           title = "Umpire Reports",           page = NULL,             status = "live")
)

make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class  <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"
  card_image  <- if (!is.null(app$image_src)) app$image_src else paste0(app$id, ".png")

  if (!is.null(app$page) && app$status == "live") {
    onclick_js <- paste0("Shiny.setInputValue('nav_to', '", app$page, "', {priority: 'event'})")
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
      class = "hub-main",
      tags$div(class = "section-label", "Applications"),
      tags$div(class = "app-grid", lapply(apps, make_card)),
      tags$div(class = "section-label", style = "margin-top: 40px;",
               "2025 Roster"),
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
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

catcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button("< Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
      tags$h2("Catcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          tags$h4("Select Team & Game", style = "color: var(--navy); margin-bottom: 12px;"),
          checkboxInput("catcher_manual_enabled", "Upload single-game CSV", value = FALSE),
          conditionalPanel(
            condition = "input.catcher_manual_enabled",
            fileInput("catcher_manual_csv", "Game CSV:", accept = c(".csv", ".parquet"),
                      buttonLabel = "Browse", placeholder = "No file selected"),
            helpText("Appended to the CapeCod26 season so its date appears below. ",
                     "Season stats still use CapeCod26.")
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
      tags$div(style = "display:flex; gap:12px; align-items:center;",
        actionButton("generate_catcher", "Generate Report", class = "btn btn-primary", style = "width:200px;"),
        downloadButton("download_catcher_all", "Download All (PDF)",
                       class = "btn btn-outline-primary", style = "width:200px;")),
      br(),
      uiOutput("catcher_status"), br(),
      uiOutput("catcher_report_ui")
    ),
    tags$div(class = "hub-footer",
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

hitter_ui <- function() {
  tagList(
    tags$div(class="hub-main",
      tags$div(style="margin-bottom:24px;",
        tags$button("← Back to Hub",
                    onclick="Shiny.setInputValue('nav_to','hub',{priority:'event'})",
                    class="btn btn-outline-secondary btn-sm")),
      tags$h2("Hitter Report Generator",
              style="font-family:var(--font-head);color:var(--navy);margin-bottom:24px;"),
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:32px;margin-bottom:32px;",
        tags$div(
          tags$h4("Select Team & Game(s)", style="color:var(--navy);margin-bottom:12px;"),
          checkboxInput("hitter_manual_enabled", "Upload single-game CSV", value = FALSE),
          conditionalPanel(
            condition = "input.hitter_manual_enabled",
            fileInput("hitter_manual_csv", "Game CSV:", accept = c(".csv", ".parquet"),
                      buttonLabel = "Browse", placeholder = "No file selected"),
            helpText("Appended to the CapeCod26 season so its date appears below. ",
                     "Season stats still use CapeCod26.")),
          uiOutput("hitter_team_select_ui"),
          uiOutput("hitter_dates_ui")),
        tags$div(
          tags$h4("Select Player", style="color:var(--navy);margin-bottom:12px;"),
          uiOutput("hitter_select_ui"),
          radioButtons("hitter_pitch_src", "Pitch Type Source:",
                       choices = c("Tagged" = "tagged", "Auto (backup)" = "auto"),
                       selected = "tagged", inline = TRUE))
      ),
      tags$div(style = "display:flex; gap:12px; align-items:center;",
        actionButton("generate_hitter","Generate Report",class="btn btn-primary",style="width:200px;"),
        downloadButton("download_hitter_all","Download All (PDF)",
                       class="btn btn-outline-primary", style="width:200px;")),
      br(),
      uiOutput("hitter_status"), br(),
      uiOutput("hitter_report_ui")
    ),
    tags$div(class="hub-footer", paste0("Brewster Whitecaps Analytics · ",format(Sys.Date(),"%Y")))
  )
}

# Old PDF-based pitcher_ui() is retained but no longer routed to; the pitcher
# page now renders pitcher_card_ui() (BrewSummaryCard). Kept for reference.
pitcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button("< Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
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
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

# Map team API abbreviations to logo filenames
ccbl_logo <- function(abbr) {
  map <- c(
    BRE = "BRE.png", BOU = "BOU.png", CHA = "CHA.png",
    COT = "COT.png", FAL = "FAL.png", HAR = "HAR.png",
    HYA = "HYA.png", ORL = "ORL.png", WAR = "WAR.png",
    YD  = "YD.png"
  )
  unname(map[abbr]) %||% "BRE.png"
}

home_tab_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      .navbar { background-color: #0C2340 !important; border-bottom: none; }
      .navbar-brand { color: #fff !important; font-family: 'Oswald', sans-serif;
                      font-size: 18px; letter-spacing: 2px; }
      .navbar-nav > li > a {
        color: rgba(255,255,255,0.65) !important;
        font-size: 12px; font-weight: 500; letter-spacing: 0.4px;
        padding: 14px 14px !important;
        border-bottom: 2px solid transparent;
      }
      .navbar-nav > li > a:hover  { color: #fff !important; }
      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:hover,
      .navbar-nav > li.active > a:focus {
        color: #fff !important; border-bottom: 2px solid #C8102E;
        background: transparent !important;
      }
      .navbar-nav > li > a:focus { background: transparent !important; }
      .nav-tabs { display: none; }
      #caps-home .content-area { padding: 24px 32px; background: #f8f9fb; }
      #caps-home .section-label {
        font-size: 10px; font-weight: 600; letter-spacing: 1.5px;
        text-transform: uppercase; color: #5F5F6B; margin-bottom: 14px;
      }
      #caps-home .pos-filters { display: flex; gap: 6px; margin-bottom: 14px; }
      #caps-home .pos-pill {
        font-size: 11px; font-weight: 600; padding: 4px 14px;
        border-radius: 20px; cursor: pointer;
        border: 1px solid #DADADA; background: #fff; color: #5F5F6B;
      }
      #caps-home .pos-pill.active {
        background: #0C2340; color: #fff; border-color: #0C2340;
      }
      #caps-home .roster-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
        gap: 8px;
      }
      #caps-home .player-card {
        background: #fff; border: 0.5px solid #EAEAEE;
        border-radius: 8px; padding: 10px 13px;
        display: flex; align-items: center; gap: 10px;
      }
      #caps-home .p-init {
        width: 44px; height: 44px; border-radius: 50%;
        background: #E6F1FB; color: #185FA5;
        font-size: 14px; font-weight: 700;
        display: flex; align-items: center; justify-content: center; flex-shrink: 0;
      }
      #caps-home .p-name { font-size: 12px; font-weight: 600; color: #16161B; }
      #caps-home .p-info { font-size: 10px; color: #8B8B96; margin-top: 2px; }
      .tab-content > .tab-pane { padding: 0; }
      .tab-pane[data-value='tab_leaderboards'] .navbar { display: none !important; }
      .tab-pane[data-value='tab_leaderboards'] .navbar-default { display: none !important; }
      .navbar-nav > li > a[data-value='tab_pcard_mock'] { display: none !important; }
    "))),
    tags$div(
      id = "caps-home",
      uiOutput("scoreboard_hero"),
      tags$div(
        class = "content-area",
        tags$div(class = "section-label", "2026 Roster"),
        tags$div(
          class = "pos-filters",
          tags$div(class = "pos-pill active", `data-group` = "Pitchers",    "Pitchers"),
          tags$div(class = "pos-pill",        `data-group` = "Catchers",    "Catchers"),
          tags$div(class = "pos-pill",        `data-group` = "Infielders",  "Infielders"),
          tags$div(class = "pos-pill",        `data-group` = "Outfielders", "Outfielders")
        ),
        uiOutput("home_roster_grid")
      ),
      tags$script(HTML("
        $(document).on('click', '#caps-home .pos-pill', function() {
          $('#caps-home .pos-pill').removeClass('active');
          $(this).addClass('active');
          var grp = $(this).data('group');
          $('#caps-home .player-card').each(function() {
            $(this).toggle($(this).data('group') === grp);
          });
        });
      "))
    )
  )
}

# ============================================================================
# ACQUISITIONS BOARD (integrated from AcquisitionsApp3.R)
# Self-contained: globals + scoped UI builder (acq_board_ui) + server module
# (acq_board_server). Wired into the navbarPage as the "Acquisitions Board"
# tab and called once from the main server.
# ============================================================================

# ── 1. Pull latest data from HF dataset repo, then load ────────────────────
ACQ_DATA_FILES <- c(
  "pitch_level.parquet", "pa_results.parquet", "pitcher_season.parquet",
  "pitch_metrics.parquet", "movement_avg.parquet", "pitching_all.parquet",
  "hitting_all.parquet", "player_bios.parquet",
  "necbl_pitching.parquet", "necbl_hitting.parquet",
  "nwl_pitching.parquet", "nwl_hitting.parquet"
)

pull_acq_data_from_hf <- function() {
  for (f in ACQ_DATA_FILES) {
    tryCatch({
      pull_file_from_hf(f, f, repo_id = HF_DATA_REPO_ID)
      message("[acq] Pulled latest ", f, " from HF dataset repo")
    }, error = function(e) {
      message("[acq] Pull failed for ", f, ", using bundled copy: ", e$message)
    })
  }
}

invisible(pull_acq_data_from_hf())

acq_pitch_level    <- read_parquet("pitch_level.parquet")
acq_pitcher_season <- read_parquet("pitcher_season.parquet")
acq_pitch_metrics  <- read_parquet("pitch_metrics.parquet")
acq_movement_avg   <- read_parquet("movement_avg.parquet")
acq_pitching_all   <- read_parquet("pitching_all.parquet")
acq_hitting_all    <- read_parquet("hitting_all.parquet")
acq_player_bios    <- read_parquet("player_bios.parquet")
acq_necbl_pitching <- read_parquet("necbl_pitching.parquet")
acq_necbl_hitting  <- read_parquet("necbl_hitting.parquet")
acq_nwl_pitching   <- read_parquet("nwl_pitching.parquet")
acq_nwl_hitting    <- read_parquet("nwl_hitting.parquet")

message("[acq] Data loaded.")

# ── 2. Harmonize data ────────────────────────────────────────────────────────
# Wrapped in a function so it can be re-run after "Run updates" refreshes the
# raw tables (acq_pitch_level, acq_necbl_pitching, etc. via <<-). Without this,
# the combined acq_all_pitchers/acq_all_hitters tables the UI actually reads

# from only ever get built once, at app startup.
acq_rebuild_combined <- function() {

  acq_pitcher_season_tagged <- acq_pitcher_season %>%
    mutate(
      has_pbp    = TRUE,
      source_key = as.character(pitcher_id),
      class_year = NA_character_
    ) %>%
    rename(ERA_approx = ERA) %>%
    left_join(
      acq_pitching_all %>%
        distinct(player_id, .keep_all = TRUE) %>%
        select(player_id, ERA_real = ERA),
      by = c("pitcher_id" = "player_id")
    ) %>%
    mutate(ERA = coalesce(ERA_real, ERA_approx)) %>%
    select(-ERA_real, -ERA_approx)

  acq_necbl_pitchers_clean <- acq_necbl_pitching %>%
    mutate(source_key = paste(player_name, team, "NECBL")) %>%
    mutate(
      pitcher_name = coalesce(full_name, player_name),
      pitch_hand   = throws,
      team_name    = team,
      IP_dec       = suppressWarnings(as.numeric(ip)),
      K9           = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (k  / IP_dec) * 9, NA), 1),
      BB9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (bb / IP_dec) * 9, NA), 1),
      KBB          = round(ifelse(!is.na(bb) & bb > 0, k / bb, NA), 2),
      HR9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (coalesce(hr,0L) / IP_dec) * 9, NA), 1),
      FIP          = round(ifelse(!is.na(IP_dec) & IP_dec > 0,
                                  ((13*coalesce(hr,0L) + 3*(coalesce(bb,0L)+coalesce(hbp,0L)) -
                                     2*coalesce(k,0L)) / IP_dec) + 3.10, NA), 2),
      ERA          = suppressWarnings(as.numeric(era)),
      WHIP         = suppressWarnings(as.numeric(whip)),
      IP           = IP_dec,
      age          = NA_integer_,
      college      = school,
      class_year   = year,
      has_pbp      = FALSE
    ) %>%
    select(pitcher_name, pitch_hand, age, class_year, college,
           team_name, league_name, G = app, IP, ERA, FIP, WHIP,
           K9, BB9, KBB, HR9, has_pbp, source_key)
  acq_nwl_pitchers_clean <- acq_nwl_pitching %>%
    mutate(
      pitcher_name = paste(firstname, lastname),
      pitch_hand   = str_extract(coalesce(bats_throws, ""), "(?<=/).$"),
      team_name    = team_abv,
      IP_dec       = suppressWarnings(as.numeric(IP)),
      ERA_num      = suppressWarnings(as.numeric(ERA)),
      WHIP_num     = suppressWarnings(as.numeric(WHIP)),
      K9_num       = suppressWarnings(as.numeric(`K/9`)),
      BB9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (BB / IP_dec) * 9, NA), 1),
      KBB          = round(ifelse(!is.na(BB) & BB > 0, K / BB, NA), 2),
      HR9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (HR / IP_dec) * 9, NA), 1),
      FIP          = round(ifelse(!is.na(IP_dec) & IP_dec > 0,
                                  ((13*coalesce(HR,0L) + 3*(coalesce(BB,0L)+coalesce(HB,0L)) -
                                      2*coalesce(K,0L)) / IP_dec) + 3.10, NA), 2),
      age          = NA_integer_,
      class_year   = class,
      has_pbp      = FALSE,
      source_key   = paste(pitcher_name, team_abv, "Northwoods League")
    ) %>%
    select(pitcher_name, pitch_hand, age, class_year, college,
           team_name, league_name, G, IP = IP_dec,
           ERA = ERA_num, FIP, WHIP = WHIP_num,
           K9 = K9_num, BB9, KBB, HR9, has_pbp, source_key)

  acq_all_pitchers <<- bind_rows(
    acq_pitcher_season_tagged %>%
      select(pitcher_name, pitch_hand, age, class_year, college,
             team_name, league_name, G, IP, ERA, FIP, WHIP,
             K9, BB9, KBB, HR9, has_pbp, source_key),
    acq_necbl_pitchers_clean,
    acq_nwl_pitchers_clean
  )

acq_hitting_all_clean <- acq_hitting_all %>%
    mutate(source_key = as.character(player_id), class_year = NA_character_, K = SO,
           Bats = if ("bats" %in% names(acq_hitting_all)) bats else NA_character_) %>%
    select(player_name, position, age, class_year, college,
           team_name, league_name, G, AB, H, R,
           `2B`, `3B`, HR, RBI, BB, K, SB, AVG, OBP, SLG, OPS, Bats, source_key)

  acq_necbl_hitting_clean <- acq_necbl_hitting %>%
    mutate(source_key = paste(player_name, Team, "NECBL")) %>%
    mutate(
      player_name = coalesce(full_name, player_name),
      team_name   = Team,
      age         = NA_integer_,
      class_year  = year,
      college     = school,
      R           = NA_integer_,
      SB          = NA_integer_,
      OPS         = round(coalesce(obp, 0) + coalesce(slg, 0), 3),
      AVG         = avg,
      OBP         = obp,
      SLG         = slg,
      Bats        = if ("bats" %in% names(acq_necbl_hitting)) bats else NA_character_
    ) %>%
    select(player_name, position, age, class_year, college,
           team_name, league_name, G = gp, AB = ab, H = h, R,
           `2B` = `2b`, `3B` = `3b`, HR = hr, RBI = rbi,
           BB = bb, K = k, SB, AVG, OBP, SLG, OPS, Bats, source_key)

  acq_nwl_hitting_clean <- acq_nwl_hitting %>%
    mutate(
      player_name = paste(firstname, lastname), team_name = team_abv,
      age = NA_integer_, class_year = class,
      AVG = suppressWarnings(as.numeric(AVG)),
      OBP = suppressWarnings(as.numeric(OBP)),
      SLG = suppressWarnings(as.numeric(SLG)),
      OPS = suppressWarnings(as.numeric(OPS)),
      Bats = if ("bats_throws" %in% names(acq_nwl_hitting))
               stringr::str_extract(coalesce(bats_throws, ""), "^[A-Za-z]") else NA_character_,
      source_key  = paste(player_name, team_abv, "Northwoods League")
    ) %>%
    select(player_name, position, age, class_year, college,
           team_name, league_name, G, AB, H, R,
           `2B`, `3B`, HR, RBI, BB, K, SB, AVG, OBP, SLG, OPS, Bats, source_key)

  acq_all_hitters <<- bind_rows(acq_hitting_all_clean, acq_necbl_hitting_clean, acq_nwl_hitting_clean)
  message("[acq] Rebuilt combined tables — ", nrow(acq_all_pitchers), " pitchers, ",
          nrow(acq_all_hitters), " hitters.")
}

acq_age_or_class <- function(age, class_year) {
  dplyr::case_when(
    !is.na(class_year) & class_year != "" ~ class_year,
    !is.na(age)                           ~ as.character(age),
    TRUE                                  ~ "—"
  )
}

acq_rebuild_combined()


# ── 3. Constants ─────────────────────────────────────────────────────────────
ACQ_NAVY  <- "#0D2B56"
ACQ_TEAL  <- "#00827F"
ACQ_WHITE <- "#FFFFFF"

ACQ_PITCH_COLORS <- c(
  "FF" = "#D22D49", "SI" = "#FE9D00", "FC" = "#933F2C",
  "SL" = "#EEE716", "ST" = "#DDB33A", "CU" = "#00D1ED",
  "KC" = "#3025CE", "CH" = "#1DBE3A", "FS" = "#4AFF89",
  "FA" = "#D22D49", "CS" = "#00D1ED"
)

ACQ_ALL_LEAGUES <- c("All", "MLB Draft League", "Appalachian League",
                     "NECBL", "Northwoods League")

ACQ_INELIG_FILE   <- "ineligible_pitchers.csv"
ACQ_INELIG_FILE_H <- "ineligible_hitters.csv"

# Pull the latest copy from the dataset repo at startup, before reading
invisible(pull_file_from_hf("ineligible_pitchers.csv", ACQ_INELIG_FILE))
invisible(pull_file_from_hf("ineligible_hitters.csv",  ACQ_INELIG_FILE_H))


source("acq_helpers.R", local = TRUE)
      

acq_load_ineligible <- function() {
  if (!file.exists(ACQ_INELIG_FILE)) return(character(0))
  df <- read_csv(ACQ_INELIG_FILE, show_col_types = FALSE)
  if ("source_key" %in% names(df)) df$source_key
  else if ("pitcher_id" %in% names(df)) as.character(df$pitcher_id)
  else character(0)
}

acq_save_ineligible <- function(keys) {
  tibble(source_key = as.character(keys)) %>% write_csv(ACQ_INELIG_FILE)
  push_file_to_hf(ACQ_INELIG_FILE, "ineligible_pitchers.csv",
                  paste("Update ineligible pitchers —", length(keys), "total"))
}

acq_load_ineligible_h <- function() {
  if (!file.exists(ACQ_INELIG_FILE_H)) return(character(0))
  df <- read_csv(ACQ_INELIG_FILE_H, show_col_types = FALSE)
  if ("source_key" %in% names(df)) df$source_key
  else if ("player_id" %in% names(df)) as.character(df$player_id)
  else character(0)
}

acq_save_ineligible_h <- function(keys) {
  tibble(source_key = as.character(keys)) %>% write_csv(ACQ_INELIG_FILE_H)
  push_file_to_hf(ACQ_INELIG_FILE_H, "ineligible_hitters.csv",
                  paste("Update ineligible hitters —", length(keys), "total"))
}

acq_dt_header_js <- function() {
  JS(glue(
    "function(settings, json) {{
       $(this.api().table().header()).css({{
         'background-color':'{ACQ_TEAL}', 'color':'{ACQ_WHITE}'
       }});
     }}"
  ))
}

# ── 4. Scoped CSS ────────────────────────────────────────────────────────────
# Every selector is namespaced to #acq-app (or the two modal ids) so the
# board's full-screen / dark styling can't bleed into the rest of the CAPS app.
acq_app_css <- glue("
  #acq-app * {{ box-sizing: border-box; }}
  #acq-app {{ background:{ACQ_NAVY}; color:{ACQ_WHITE};
              font-family:Arial,sans-serif; }}

  /* Shell */
  #acq-app .app-shell {{ display:flex; height:calc(100vh - 70px); }}

  /* Sidebar */
  #acq-app .sidebar {{
    width:200px; min-width:200px; height:100%;
    background:#061B38; border-right:1px solid #1A3A5C;
    display:flex; flex-direction:column; flex-shrink:0;
  }}
  #acq-app .sidebar-logo {{ padding:16px 14px 12px; border-bottom:1px solid #1A3A5C; }}
  #acq-app .sidebar-logo-title {{ font-size:13px; font-weight:bold; color:{ACQ_WHITE}; }}
  #acq-app .sidebar-logo-sub {{ font-size:11px; color:#6B8CAE; margin-top:2px; }}

  #acq-app .nav-section {{
    font-size:10px; font-weight:bold; letter-spacing:.07em;
    text-transform:uppercase; color:#4A6B8A; padding:14px 14px 4px;
  }}
  #acq-app .nav-item {{
    display:flex; align-items:center; gap:9px;
    padding:8px 14px; font-size:13px; cursor:pointer;
    color:#8BAAC8; border-left:3px solid transparent; transition:all .15s;
  }}
  #acq-app .nav-item:hover {{ color:{ACQ_WHITE}; background:#0D2B56; }}
  #acq-app .nav-item.active {{
    color:{ACQ_TEAL}; background:#0A2240;
    border-left-color:{ACQ_TEAL}; font-weight:bold;
  }}
  #acq-app .nav-item i {{ font-size:16px; }}
  #acq-app .inelig-badge {{
    margin-left:auto; font-size:9px; font-weight:bold;
    background:#7B2020; color:#FFB3B3; padding:1px 6px; border-radius:10px;
  }}

  #acq-app .sidebar-bottom {{ margin-top:auto; border-top:1px solid #1A3A5C; padding:10px 14px; }}
  #acq-app .sidebar-action {{
    display:flex; align-items:center; gap:7px;
    font-size:12px; color:#6B8CAE; cursor:pointer; padding:6px 0;
  }}
  #acq-app .sidebar-action:hover {{ color:{ACQ_WHITE}; }}
  #acq-app .sidebar-action i {{ font-size:14px; }}

  /* Main */
  #acq-app .main-area {{ flex:1; display:flex; flex-direction:column; height:100%; overflow:hidden; }}
  #acq-app .main-header {{
    padding:12px 20px; border-bottom:1px solid #1A3A5C;
    display:flex; align-items:center; justify-content:space-between;
    background:#0A2240; flex-shrink:0;
  }}
  #acq-app .main-header-title {{ font-size:15px; font-weight:bold; color:{ACQ_WHITE}; }}
  #acq-app .header-filters {{ display:flex; gap:8px; align-items:center; }}
  #acq-app .content-area {{ flex:1; overflow-y:auto; padding:16px 20px; }}

  /* Filter bar */
  #acq-app .filter-bar {{ display:flex; gap:10px; align-items:flex-end; margin-bottom:14px; flex-wrap:wrap; }}
  #acq-app .filter-group {{ display:flex; flex-direction:column; gap:4px; }}
  #acq-app .filter-label {{ font-size:10px; color:#6B8CAE; text-transform:uppercase; letter-spacing:.05em; }}
  #acq-app .filter-bar select, #acq-app .filter-bar input {{
    background:#0F3366; color:{ACQ_WHITE}; border:1px solid #1A3A5C;
    border-radius:4px; padding:5px 8px; font-size:12px;
  }}
  #acq-app .filter-bar input[type=number] {{ width:80px; }}
  #acq-app .btn-apply {{
    background:{ACQ_TEAL}; color:{ACQ_WHITE}; border:none;
    padding:6px 14px; border-radius:4px; font-size:12px; cursor:pointer;
    font-weight:bold; align-self:flex-end;
  }}
  #acq-app .btn-apply:hover {{ opacity:.85; }}

  /* Action buttons */
  #acq-app .action-bar {{ display:flex; gap:8px; margin-bottom:12px; flex-wrap:wrap; }}
  #acq-app .btn-action {{
    display:flex; align-items:center; gap:5px;
    background:#0F3366; color:{ACQ_WHITE}; border:1px solid #1A3A5C;
    padding:5px 12px; border-radius:4px; font-size:12px; cursor:pointer;
  }}
  #acq-app .btn-action:hover {{ border-color:{ACQ_TEAL}; color:{ACQ_TEAL}; }}
  #acq-app .btn-action.danger {{ border-color:#7B2020; color:#FFB3B3; }}
  #acq-app .btn-action.danger:hover {{ background:#4A1010; }}
  #acq-app .btn-action.restore {{ border-color:#1A5276; color:#7EC8E3; }}
  #acq-app .btn-top10 {{
    display:flex; align-items:center; gap:5px;
    background:{ACQ_TEAL}; color:{ACQ_WHITE}; border:none;
    padding:5px 12px; border-radius:4px; font-size:12px; cursor:pointer;
    font-weight:bold; margin-left:auto;
  }}
  #acq-app .btn-top10:hover {{ opacity:.85; }}

  /* Tables (board + both modals) */
  #acq-app .dataTables_wrapper, #acq-app .dataTables_info, #acq-app .dataTables_paginate,
  #pitcher_modal .dataTables_wrapper, #pitcher_modal .dataTables_info, #pitcher_modal .dataTables_paginate,
  #top10_modal .dataTables_wrapper, #top10_modal .dataTables_info, #top10_modal .dataTables_paginate {{ color:{ACQ_WHITE} !important; }}
  #acq-app .dataTables_filter label, #acq-app .dataTables_length label,
  #pitcher_modal .dataTables_filter label, #pitcher_modal .dataTables_length label,
  #top10_modal .dataTables_filter label, #top10_modal .dataTables_length label {{ color:{ACQ_WHITE}; }}
  #acq-app .dataTables_filter input, #acq-app .dataTables_length select,
  #pitcher_modal .dataTables_filter input, #pitcher_modal .dataTables_length select,
  #top10_modal .dataTables_filter input, #top10_modal .dataTables_length select {{
    background:#0F3366; color:{ACQ_WHITE}; border:1px solid #1A3A5C;
  }}
  #acq-app table.dataTable tbody tr, #pitcher_modal table.dataTable tbody tr, #top10_modal table.dataTable tbody tr {{
    background:#0F3366 !important; color:{ACQ_WHITE} !important;
  }}
  #acq-app table.dataTable tbody tr:hover, #pitcher_modal table.dataTable tbody tr:hover, #top10_modal table.dataTable tbody tr:hover {{
    background:#1A4A6C !important; cursor:pointer;
  }}
  #acq-app table.dataTable tbody tr.selected td, #pitcher_modal table.dataTable tbody tr.selected td, #top10_modal table.dataTable tbody tr.selected td {{
    background:{ACQ_TEAL} !important; color:{ACQ_WHITE} !important;
  }}
  #acq-app table.dataTable thead th, #pitcher_modal table.dataTable thead th, #top10_modal table.dataTable thead th {{
    background:{ACQ_TEAL}; color:{ACQ_WHITE};
  }}

  /* Modals */
  #pitcher_modal .modal-content, #top10_modal .modal-content {{
    background:#0A2040; color:{ACQ_WHITE}; border:2px solid {ACQ_TEAL};
  }}
  #pitcher_modal .modal-header, #top10_modal .modal-header {{ background:{ACQ_TEAL}; border-bottom:none; }}
  #pitcher_modal .modal-title, #top10_modal .modal-title {{ color:{ACQ_WHITE} !important; font-weight:bold; }}
  #pitcher_modal .close, #top10_modal .close {{ color:{ACQ_WHITE} !important; opacity:1 !important; }}
  #pitcher_modal .modal-footer, #top10_modal .modal-footer {{ background:#0A2040; border-top:1px solid {ACQ_TEAL}; }}

  /* Misc */
  #acq-app .info-bar {{ color:#8BAAC8; font-size:13px; margin-bottom:12px; }}
  #acq-app .no-pbp-badge, #pitcher_modal .no-pbp-badge {{
    display:inline-block; background:#2A3A50; color:#8BAAC8;
    font-size:11px; padding:2px 8px; border-radius:3px; margin-left:8px;
  }}
  #acq-app label, #pitcher_modal label, #top10_modal label {{ color:{ACQ_WHITE}; }}
  #acq-app hr, #pitcher_modal hr, #top10_modal hr {{ border-color:{ACQ_TEAL}; opacity:.4; }}
  #acq-app select, #pitcher_modal select, #top10_modal select {{ background:#0F3366; color:{ACQ_WHITE}; border:1px solid #1A3A5C; }}

  /* Pos buttons */
  #acq-app .pos-btn, #top10_modal .pos-btn {{
    background:#0F3366; color:{ACQ_WHITE};
    border:1px solid #1A3A5C; margin:2px; padding:4px 10px;
    border-radius:4px; cursor:pointer; font-size:12px; display:inline-block;
  }}
  #acq-app .pos-btn.active, #top10_modal .pos-btn.active {{
    background:{ACQ_TEAL}; color:{ACQ_NAVY}; font-weight:bold; border-color:{ACQ_TEAL};
  }}

  /* Page sections (hidden by default) */
  #acq-app .page {{ display:none; }}
  #acq-app .page.active {{ display:block; }}

  /* Scrollbar */
  #acq-app ::-webkit-scrollbar {{ width:6px; height:6px; }}
  #acq-app ::-webkit-scrollbar-track {{ background:#061B38; }}
  #acq-app ::-webkit-scrollbar-thumb {{ background:#1A3A5C; border-radius:3px; }}
  #acq-app ::-webkit-scrollbar-thumb:hover {{ background:{ACQ_TEAL}; }}

  #acq-app button.nav-item {{
    background:none; border-top:none; border-right:none;
    border-bottom:none; border-left:3px solid transparent;
    width:100%; text-align:left; border-radius:0;
    display:flex; align-items:center; gap:9px;
    padding:8px 14px; font-size:13px; cursor:pointer; color:#8BAAC8;
  }}
  #acq-app button.nav-item:hover {{ color:{ACQ_WHITE}; background:#0D2B56; }}
  #acq-app button.nav-item.active {{
    color:{ACQ_TEAL}; background:#0A2240;
    border-left-color:{ACQ_TEAL}; font-weight:bold;
  }}
")

# ── 5. UI builder ────────────────────────────────────────────────────────────
acq_board_ui <- function() {
  tagList(
    tags$head(
      tags$style(HTML(acq_app_css)),
      tags$link(
        rel  = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/tabler-icons.min.css"
      )
    ),

    div(id = "acq-app",
      div(class = "app-shell",

          # ── Sidebar ────────────────────────────────────────────────────────
          div(class = "sidebar",
              div(class = "sidebar-logo",
                  div(class = "sidebar-logo-title", "Brewster Whitecaps"),
                  div(class = "sidebar-logo-sub",   "Player Acquisitions")
              ),
              div(class = "nav-section", "Scouting"),
              actionButton("nav_pitchers",   tagList(tags$i(class="ti ti-ball-baseball"), " Pitchers"),
                           class = "nav-item active"),
              actionButton("nav_hitters",    tagList(tags$i(class="ti ti-run"),           " Hitters"),
                           class = "nav-item"),
              actionButton("nav_top10",      tagList(tags$i(class="ti ti-trophy"),        " Top 10"),
                           class = "nav-item"),
              actionButton("nav_ineligible", tagList(tags$i(class="ti ti-ban"),           " Ineligible"),
                           class = "nav-item"),
              div(class = "sidebar-bottom",
                  actionButton("open_run_updates",
                               tagList(tags$i(class = "ti ti-refresh"), " Run updates"),
                               class = "sidebar-action")
              )
          ),
          # ── Main area ───────────────────────────────────────────────────────
          div(class = "main-area",
              div(class = "main-header",
                  div(class = "main-header-title", textOutput("page_title", inline = TRUE)),
                  div(class = "header-filters", uiOutput("header_subtitle"))
              ),
              div(class = "content-area",

                  # ── Pitchers page ─────────────────────────────────────────
                  div(id = "page_pitchers", class = "page active",
                      div(class = "filter-bar",
                          div(class = "filter-group",
                              div(class = "filter-label", "League"),
                              selectInput("pbp_league", NULL,
                                          choices = ACQ_ALL_LEAGUES, selected = "All",
                                          width = "150px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Hand"),
                              selectInput("pbp_hand", NULL,
                                          choices = c("All","R","L"), selected = "All",
                                          width = "80px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Min IP"),
                              numericInput("min_pitches", NULL, value = 0, min = 0, step = 1,
                                           width = "70px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Max Age"),
                              numericInput("max_age_p", NULL, value = 99, min = 18, step = 1,
                                           width = "70px")
                          ),
                          actionButton("apply_pbp_filter", "Apply", class = "btn-apply")
                      ),
                      div(class = "action-bar",
                          actionButton("view_pitcher",   tagList(tags$i(class="ti ti-chart-line"), " View profile"),
                                       class = "btn-action"),
                          actionButton("remove_pitcher", tagList(tags$i(class="ti ti-ban"), " Mark ineligible"),
                                       class = "btn-action danger"),
                          actionButton("open_top10",     tagList(tags$i(class="ti ti-trophy"), " Top 10"),
                                       class = "btn-top10")
                      ),
                      DTOutput("pbp_pitcher_table")
                  ),

                  # ── Hitters page ──────────────────────────────────────────
                  div(id = "page_hitters", class = "page",
                      div(class = "filter-bar",
                          div(class = "filter-group",
                              div(class = "filter-label", "League"),
                              selectInput("season_league_h", NULL,
                                          choices = ACQ_ALL_LEAGUES, selected = "All",
                                          width = "150px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Min AB"),
                              numericInput("min_ab", NULL, value = 0, min = 0, step = 5,
                                           width = "70px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Bats"),
                              selectInput("season_hand_h", NULL,
                                          choices = c("All","R","L","S"), selected = "All",
                                          width = "80px")
                          ),
                          div(class = "filter-group",
                              div(class = "filter-label", "Max Age"),
                              numericInput("max_age_h", NULL, value = 99, min = 18, step = 1,
                                           width = "70px")
                          ),
                          actionButton("apply_season_h", "Apply", class = "btn-apply")
                      ),
                      div(class = "action-bar",
                          actionButton("remove_hitter", tagList(tags$i(class="ti ti-ban"), " Mark ineligible"),
                                       class = "btn-action danger"),
                          actionButton("open_top10_h",  tagList(tags$i(class="ti ti-trophy"), " Top 10"),
                                       class = "btn-top10")
                      ),
                      DTOutput("season_hitter_table")
                  ),

                  # ── Top 10 page ───────────────────────────────────────────
                  div(id = "page_top10", class = "page",
                      tabsetPanel(id = "top10_inline_tabs",
                                  tabPanel("Hitting",
                                           br(),
                                           fluidRow(
                                             column(3, selectInput("top10_league_h", "League:", choices = ACQ_ALL_LEAGUES,
                                                                   selected = "All", width = "100%")),
                                             column(3, numericInput("top10_min_ab", "Min AB:", value = 20,
                                                                    min = 0, step = 5, width = "100%")),
                                             column(3, numericInput("top10_max_age_h", "Max Age:", value = 99,
                                                                    min = 18, step = 1, width = "100%")),
                                             column(3, selectInput("top10_stat_h", "Sort by:",
                                                                   choices = c("OPS","AVG","OBP","SLG","HR","RBI","SB"),
                                                                   selected = "OPS", width = "100%"))
                                           ),
                                           div(style = "margin-bottom:10px;",
                                               tags$p("Position:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                                               actionButton("hpos_All","All",class="pos-btn active"),
                                               actionButton("hpos_C",  "C",  class="pos-btn"),
                                               actionButton("hpos_1B", "1B", class="pos-btn"),
                                               actionButton("hpos_2B", "2B", class="pos-btn"),
                                               actionButton("hpos_3B", "3B", class="pos-btn"),
                                               actionButton("hpos_SS", "SS", class="pos-btn"),
                                               actionButton("hpos_LF", "LF", class="pos-btn"),
                                               actionButton("hpos_CF", "CF", class="pos-btn"),
                                               actionButton("hpos_RF", "RF", class="pos-btn"),
                                               actionButton("hpos_DH", "DH", class="pos-btn")
                                           ),
                                           div(style = "margin-bottom:10px;",
                               tags$p("Bats:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                               actionButton("bats_All","All", class="pos-btn active"),
                               actionButton("bats_R",  "R",   class="pos-btn"),
                               actionButton("bats_L",  "L",   class="pos-btn"),
                               actionButton("bats_S",  "S",   class="pos-btn")
                           ),
                                           DTOutput("top10_hitting_table")
                                  ),
                                  tabPanel("Pitching",
                                           br(),
                                           fluidRow(
                                             column(3, selectInput("top10_league_p", "League:", choices = ACQ_ALL_LEAGUES,
                                                                   selected = "All", width = "100%")),
                                             column(3, numericInput("top10_min_ip", "Min IP:", value = 5,
                                                                    min = 0, step = 1, width = "100%")),
                                             column(3, selectInput("top10_stat_p", "Sort by:",
                                                                   choices = c("ERA","FIP","WHIP","K9","BB9","KBB"),
                                                                   selected = "ERA", width = "100%")),
                                             column(3, numericInput("top10_max_age_p", "Max Age:", value = 99,
                                                                    min = 18, step = 1, width = "100%"))
                                           ),
                                           div(style = "margin-bottom:6px;",
                                               tags$p("Role:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                                               actionButton("prole_All","All",class="pos-btn active"),
                                               actionButton("prole_SP", "SP", class="pos-btn"),
                                               actionButton("prole_RP", "RP", class="pos-btn")
                                           ),
                                           div(style = "margin-bottom:10px;",
    tags$p("Hand:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
    actionButton("phand_All","All", class="pos-btn active"),
    actionButton("phand_R",  "RHP", class="pos-btn"),
    actionButton("phand_L",  "LHP", class="pos-btn")
),
tags$p("Click a pitcher's row to view profile.",
       style = "color:#6B8CAE; font-size:11px; margin-bottom:8px;"),
DTOutput("top10_pitching_table")
                                  )
                      )
                  ),

                  # ── Ineligible page ───────────────────────────────────────
                  div(id = "page_ineligible", class = "page",
                      tabsetPanel(id = "inelig_tabs",
                                  tabPanel("Pitchers",
                                           br(),
                                           div(class = "action-bar",
                                               actionButton("restore_pitcher",
                                                            tagList(tags$i(class="ti ti-circle-check"), " Restore selected"),
                                                            class = "btn-action restore")
                                           ),
                                           tags$p(textOutput("inelig_subtitle"),
                                                  style = "color:#8BAAC8; font-size:12px; margin-bottom:8px;"),
                                           DTOutput("inelig_table")
                                  ),
                                  tabPanel("Hitters",
                                           br(),
                                           div(class = "action-bar",
                                               actionButton("restore_hitter",
                                                            tagList(tags$i(class="ti ti-circle-check"), " Restore selected"),
                                                            class = "btn-action restore")
                                           ),
                                           tags$p(textOutput("inelig_h_subtitle"),
                                                  style = "color:#8BAAC8; font-size:12px; margin-bottom:8px;"),
                                           DTOutput("inelig_hitter_table")
                                  )
                      )
                  )
              )
          )
      )
    ),

    # ── Pitcher profile modal ────────────────────────────────────────────────
    bsModal(
      id = "pitcher_modal", title = "Pitcher Profile",
      trigger = NULL, size = "large",
      uiOutput("modal_info"),
      uiOutput("modal_body")
    ),

    # ── Top 10 modal ─────────────────────────────────────────────────────────
    bsModal(
      id = "top10_modal", title = "Top 10 Leaderboards",
      trigger = NULL, size = "large",
      tabsetPanel(id = "top10_modal_tabs",
                  tabPanel("Hitting",
                           br(),
                           fluidRow(
                             column(3, selectInput("top10m_league_h", "League:", choices = ACQ_ALL_LEAGUES,
                                                   selected = "All", width = "100%")),
                             column(3, numericInput("top10m_min_ab", "Min AB:", value = 20,
                                                    min = 0, step = 5, width = "100%")),
                             column(3, numericInput("top10m_max_age_h", "Max Age:", value = 99,
                                                    min = 18, step = 1, width = "100%")),
                             column(3, selectInput("top10m_stat_h", "Sort by:",
                                                   choices = c("OPS","AVG","OBP","SLG","HR","RBI","SB"),
                                                   selected = "OPS", width = "100%"))
                           ),
                           div(style = "margin-bottom:10px;",
                               actionButton("hpos_m_All","All",class="pos-btn active"),
                               actionButton("hpos_m_C",  "C",  class="pos-btn"),
                               actionButton("hpos_m_1B", "1B", class="pos-btn"),
                               actionButton("hpos_m_2B", "2B", class="pos-btn"),
                               actionButton("hpos_m_3B", "3B", class="pos-btn"),
                               actionButton("hpos_m_SS", "SS", class="pos-btn"),
                               actionButton("hpos_m_LF", "LF", class="pos-btn"),
                               actionButton("hpos_m_CF", "CF", class="pos-btn"),
                               actionButton("hpos_m_RF", "RF", class="pos-btn"),
                               actionButton("hpos_m_DH", "DH", class="pos-btn")
                           ),
                           div(style = "margin-bottom:10px;",
                               actionButton("bats_m_All","All", class="pos-btn active"),
                               actionButton("bats_m_R",  "R",   class="pos-btn"),
                               actionButton("bats_m_L",  "L",   class="pos-btn"),
                               actionButton("bats_m_S",  "S",   class="pos-btn")
                           ),
                           DTOutput("top10m_hitting_table")
                  ),
                  tabPanel("Pitching",
                           br(),
                           fluidRow(
                             column(3, selectInput("top10m_league_p", "League:", choices = ACQ_ALL_LEAGUES,
                                                   selected = "All", width = "100%")),
                             column(3, numericInput("top10m_min_ip", "Min IP:", value = 5,
                                                    min = 0, step = 1, width = "100%")),
                             column(3, selectInput("top10m_stat_p", "Sort by:",
                                                   choices = c("ERA","FIP","WHIP","K9","BB9","KBB"),
                                                   selected = "ERA", width = "100%")),
                             column(3, numericInput("top10m_max_age_p", "Max Age:", value = 99,
                                                    min = 18, step = 1, width = "100%"))
                           ),
                           div(style = "margin-bottom:6px;",
                               actionButton("prole_m_All","All",class="pos-btn active"),
                               actionButton("prole_m_SP", "SP", class="pos-btn"),
                               actionButton("prole_m_RP", "RP", class="pos-btn")
                           ),
                           div(style = "margin-bottom:10px;",
                               actionButton("phand_m_All","All", class="pos-btn active"),
                               actionButton("phand_m_R",  "RHP", class="pos-btn"),
                               actionButton("phand_m_L",  "LHP", class="pos-btn")
                           ),
                           DTOutput("top10m_pitching_table")
                  )
      )
    ),

    bsModal(
      id = "run_updates_modal", title = "Run Updates",
      trigger = NULL, size = "medium",

      tags$p(
        "Pulls fresh stats from MLB, NECBL, and Northwoods League and rebuilds all data.",
        style = "color:#8BAAC8; font-size:13px; margin-bottom:16px;"
      ),

      tags$div(
        style = "margin-bottom:16px;",
        tags$p(
          "NECBL pitching CSV — download from necbl.com first, then upload here:",
          style = "font-size:12px; color:#8BAAC8; margin-bottom:4px;"
        ),
        fileInput("necbl_csv_upload", NULL,
                  accept      = ".csv",
                  buttonLabel = "Browse",
                  placeholder = "No file selected")
      ),

      actionButton("run_updates_btn", "Start Update",
                   class = "btn-apply", style = "width:100%;")
    )

  ) 
} 

# ── 6. Server module ─────────────────────────────────────────────────────────
acq_board_server <- function(input, output, session) {

  data_version <- reactiveVal(0)
  ineligible   <- reactiveVal(acq_load_ineligible())
  ineligible_h <- reactiveVal(acq_load_ineligible_h())
  selected_key <- reactiveVal(NULL)
  current_page <- reactiveVal("pitchers")

  h_pos    <- reactiveVal(c("All"))
  p_role   <- reactiveVal("All")
  p_hand   <- reactiveVal("All")
  h_pos_m  <- reactiveVal(c("All"))
  p_role_m <- reactiveVal("All")
  p_hand_m <- reactiveVal("All")
  h_bats   <- reactiveVal("All")
  h_bats_m <- reactiveVal("All")

  h_positions <- c("All","C","1B","2B","3B","SS","LF","CF","RF","DH")
  p_roles     <- c("All","SP","RP")
  p_hands     <- c("All","R","L")

  set_active_btn <- function(group, selected, ids) {
    for (id in ids) {
      val <- sub(paste0("^", group), "", id)
      if (val == selected) {
        runjs(glue("$('#{id}').addClass('active').css({{'background-color':'{ACQ_TEAL}','color':'{ACQ_NAVY}','font-weight':'bold','border-color':'{ACQ_TEAL}'}});"))
      } else {
        runjs(glue("$('#{id}').removeClass('active').css({{'background-color':'#0F3366','color':'{ACQ_WHITE}','font-weight':'normal','border-color':'#1A3A5C'}});"))
      }
    }
  }

toggle_position <- function(current, clicked) {
  if (clicked == "All") return("All")
  current <- setdiff(current, "All")
  if (clicked %in% current) {
    current <- setdiff(current, clicked)
  } else {
    current <- c(current, clicked)
  }
  if (length(current) == 0) current <- "All"
  current
}

set_active_btns <- function(group, selected, ids) {
  for (id in ids) {
    val <- sub(paste0("^", group), "", id)
    if (val %in% selected) {
      runjs(glue("$('#{id}').addClass('active').css({{'background-color':'{ACQ_TEAL}','color':'{ACQ_NAVY}','font-weight':'bold','border-color':'{ACQ_TEAL}'}});"))
    } else {
      runjs(glue("$('#{id}').removeClass('active').css({{'background-color':'#0F3366','color':'{ACQ_WHITE}','font-weight':'normal','border-color':'#1A3A5C'}});"))
    }
  }
}
  
  lapply(h_positions, function(pos) {
    observeEvent(input[[paste0("hpos_", pos)]], {
      new_val <- toggle_position(h_pos(), pos)
      h_pos(new_val)
      set_active_btns("hpos_", new_val, paste0("hpos_", h_positions))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("hpos_m_", pos)]], {
      new_val <- toggle_position(h_pos_m(), pos)
      h_pos_m(new_val)
      set_active_btns("hpos_m_", new_val, paste0("hpos_m_", h_positions))
    }, ignoreInit = TRUE)
  })

  lapply(p_roles, function(role) {
    observeEvent(input[[paste0("prole_", role)]], {
      p_role(role); set_active_btn("prole_", role, paste0("prole_", p_roles))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("prole_m_", role)]], {
      p_role_m(role); set_active_btn("prole_m_", role, paste0("prole_m_", p_roles))
    }, ignoreInit = TRUE)
  })

  lapply(p_hands, function(hand) {
    observeEvent(input[[paste0("phand_", hand)]], {
      p_hand(hand); set_active_btn("phand_", hand, paste0("phand_", p_hands))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("phand_m_", hand)]], {
      p_hand_m(hand); set_active_btn("phand_m_", hand, paste0("phand_m_", p_hands))
    }, ignoreInit = TRUE)
  })

  h_bats_choices <- c("All","R","L","S")

  lapply(h_bats_choices, function(b) {
    observeEvent(input[[paste0("bats_", b)]], {
      h_bats(b); set_active_btn("bats_", b, paste0("bats_", h_bats_choices))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("bats_m_", b)]], {
      h_bats_m(b); set_active_btn("bats_m_", b, paste0("bats_m_", h_bats_choices))
    }, ignoreInit = TRUE)
  })
  # ── Sidebar navigation ──────────────────────────────────────────────────
  nav_pages <- c("pitchers","hitters","top10","ineligible")

  switch_page <- function(page) {
    current_page(page)
    for (p in nav_pages) {
      if (p == page) {
        runjs(glue("$('#page_{p}').addClass('active');"))
        runjs(glue("$('#nav_{p}').addClass('active');"))
      } else {
        runjs(glue("$('#page_{p}').removeClass('active');"))
        runjs(glue("$('#nav_{p}').removeClass('active');"))
      }
    }
  }

  observeEvent(input$nav_pitchers,   { switch_page("pitchers") },   ignoreInit = TRUE)
  observeEvent(input$nav_hitters,    { switch_page("hitters") },    ignoreInit = TRUE)
  observeEvent(input$nav_top10,      { switch_page("top10") },      ignoreInit = TRUE)
  observeEvent(input$nav_ineligible, { switch_page("ineligible") }, ignoreInit = TRUE)
  observeEvent(input$open_run_updates, {
    toggleModal(session, "run_updates_modal", toggle = "open")
  })

observeEvent(input$run_updates_btn, {

  showModal(modalDialog(
    title = "Updating data...",
    tags$div(
      tags$p("This will take 1-3 minutes. Please don't close this window.",
             style = "margin-bottom:12px;"),
      tags$p("Starting...")
    ),
    footer = NULL,
    size = "m"
  ))

  result <- tryCatch({

    update_progress <- function(msg) {
      removeModal()
      showModal(modalDialog(
        title = "Updating data...",
        tags$div(
          tags$p("This will take 1-3 minutes. Please don't close this window.",
                 style = "margin-bottom:12px;"),
          tags$p(msg)
        ),
        footer = NULL,
        size = "m"
      ))
    }

    necbl_csv_path <- if (!is.null(input$necbl_csv_upload)) {
      input$necbl_csv_upload$datapath
    } else {
      NULL
    }

    update_progress("Step 1/3: Updating MLB Draft League + Appalachian League...")
    run_mlb_update()

    update_progress("Step 2/3: Updating NECBL...")
    run_necbl_update(necbl_csv_path)

    update_progress("Step 3/3: Updating Northwoods League...")
    run_nwl_update()

    update_progress("Rebuilding combined tables...")
    acq_rebuild_combined()
    data_version(data_version() + 1)

    list(success = TRUE)

  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })

  removeModal()

  if (isTRUE(result$success)) {
    showModal(modalDialog(
      title = "Update complete",
      tags$p("Data has been refreshed. Reload the app to see the latest numbers."),
      footer = modalButton("Close")
    ))
  } else {
    showModal(modalDialog(
      title = "Update failed",
      tags$p(paste("Error:", result$message)),
      footer = modalButton("Close")
    ))
  }
})
  observeEvent(input$open_top10,   { toggleModal(session, "top10_modal", toggle = "open") })
  observeEvent(input$open_top10_h, { toggleModal(session, "top10_modal", toggle = "open") })

  # ── Page title + subtitle ───────────────────────────────────────────────
  output$page_title <- renderText({
    switch(current_page(),
           pitchers   = "Pitchers",
           hitters    = "Hitters",
           top10      = "Top 10 Leaderboards",
           ineligible = "Ineligible Players"
    )
  })

  output$header_subtitle <- renderUI({
    switch(current_page(),
           pitchers   = tags$span(textOutput("pbp_subtitle", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           hitters    = tags$span(textOutput("season_h_subtitle", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           ineligible = tags$span(textOutput("inelig_nav_total", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           NULL
    )
  })

  output$inelig_nav_badge <- renderUI({
    n <- length(ineligible()) + length(ineligible_h())
    if (n > 0) tags$span(n, class = "inelig-badge")
  })

  output$inelig_nav_total <- renderText({
    glue("{length(ineligible())} pitchers · {length(ineligible_h())} hitters")
  })

  # ── Pitchers ────────────────────────────────────────────────────────────
  filtered_pitchers <- reactive({
    df <- acq_all_pitchers %>% filter(!source_key %in% ineligible())

    if (input$pbp_league != "All")
      df <- df %>% filter(league_name == input$pbp_league)
    if (input$pbp_hand != "All")
      df <- df %>% filter(pitch_hand == input$pbp_hand)
    if (!is.na(input$min_pitches) && input$min_pitches > 0)
      df <- df %>% filter(!is.na(IP) & IP >= input$min_pitches)
    if (!is.na(input$max_age_p) && input$max_age_p < 99)
      df <- df %>% filter(is.na(age) | age <= input$max_age_p)

    df %>% arrange(ERA)
  }) %>% bindEvent(input$apply_pbp_filter, ineligible(), data_version(), ignoreNULL = FALSE)

  output$pbp_subtitle <- renderText(glue("{nrow(filtered_pitchers())} pitchers"))

  output$pbp_pitcher_table <- renderDT({
    filtered_pitchers() %>%
      mutate(Profile = ifelse(has_pbp, "✓", "—"),
             `Age/Yr` = acq_age_or_class(age, class_year)) %>%
      select(Name = pitcher_name, Hand = pitch_hand, `Age/Yr`,
             School = college, Team = team_name, League = league_name,
             Profile, G, IP, ERA, FIP, WHIP,
             `K/9` = K9, `BB/9` = BB9, `K/BB` = KBB, `HR/9` = HR9) %>%
      datatable(selection = "multiple", rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                               initComplete = acq_dt_header_js()),
                class = "display compact") %>%
      formatRound(c("ERA","FIP","WHIP","K/9","BB/9","K/BB","HR/9"), 2)
  })

  observeEvent(input$view_pitcher, {
    row <- input$pbp_pitcher_table_rows_selected
    if (length(row) == 0) { showNotification("Select a pitcher first.", type="warning"); return() }
    selected_key(filtered_pitchers()$source_key[row[1]])
    toggleModal(session, "pitcher_modal", toggle = "open")
  })

  observeEvent(input$remove_pitcher, {
    rows <- input$pbp_pitcher_table_rows_selected
    if (length(rows) == 0) { showNotification("Select at least one pitcher.", type="warning"); return() }
    keys  <- filtered_pitchers()$source_key[rows]
    new_inelig <- unique(c(ineligible(), keys))
    ineligible(new_inelig); acq_save_ineligible(new_inelig)
    dataTableProxy("pbp_pitcher_table") %>% selectRows(NULL)
    showNotification(glue("{length(keys)} pitcher(s) marked ineligible."), type="warning", duration=5)
  })

  # ── Hitters ─────────────────────────────────────────────────────────────
filtered_hitters <- reactive({
    df <- acq_all_hitters %>% filter(!source_key %in% ineligible_h())

    if (input$season_league_h != "All")
      df <- df %>% filter(league_name == input$season_league_h)
    if (!is.na(input$max_age_h) && input$max_age_h < 99)
      df <- df %>% filter(is.na(age) | age <= input$max_age_h)
    if (input$season_hand_h != "All")
      df <- df %>% filter(Bats == input$season_hand_h)          # NEW

    df %>% filter(!is.na(AB), AB >= input$min_ab) %>% arrange(desc(OPS))
  }) %>% bindEvent(input$apply_season_h, ineligible_h(), data_version(), ignoreNULL = FALSE)

  output$season_h_subtitle <- renderText(glue("{nrow(filtered_hitters())} hitters"))

  output$season_hitter_table <- renderDT({
    filtered_hitters() %>%
      mutate(`Age/Yr` = acq_age_or_class(age, class_year)) %>%
      select(Name = player_name, Bats, `Age/Yr`, School = college,     # Bats added
             Team = team_name, League = league_name, Pos = position,
             G, AB, H, R, `2B`, `3B`, HR, RBI, BB, K, SB,
             AVG, OBP, SLG, OPS) %>%
      datatable(selection = "multiple", rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                               initComplete = acq_dt_header_js()),
                class = "display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  })

  observeEvent(input$remove_hitter, {
    rows <- input$season_hitter_table_rows_selected
    if (length(rows) == 0) { showNotification("Select at least one hitter.", type="warning"); return() }
    keys  <- filtered_hitters()$source_key[rows]
    new_inelig <- unique(c(ineligible_h(), keys))
    ineligible_h(new_inelig); acq_save_ineligible_h(new_inelig)
    dataTableProxy("season_hitter_table") %>% selectRows(NULL)
    showNotification(glue("{length(keys)} hitter(s) marked ineligible."), type="warning", duration=5)
  })

  # ── Ineligible ──────────────────────────────────────────────────────────
  output$inelig_subtitle   <- renderText(glue("{length(ineligible())} pitchers marked ineligible"))
  output$inelig_h_subtitle <- renderText(glue("{length(ineligible_h())} hitters marked ineligible"))

  output$inelig_table <- renderDT({
    keys <- ineligible()
    if (length(keys) == 0) return(datatable(tibble(Message="No pitchers marked ineligible."),
                                            rownames=FALSE, options=list(dom="t", initComplete=acq_dt_header_js()), class="display compact"))
    acq_all_pitchers %>%
      filter(source_key %in% keys) %>%
      mutate(`Age/Yr` = acq_age_or_class(age, class_year)) %>%
      select(Name=pitcher_name, Hand=pitch_hand, `Age/Yr`,
             Team=team_name, League=league_name, School=college,
             G, IP, ERA, FIP, WHIP) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="t", pageLength=50, initComplete=acq_dt_header_js()),
                class="display compact") %>%
      formatRound(c("ERA","FIP","WHIP"), 2)
  })

  observeEvent(input$restore_pitcher, {
    row <- input$inelig_table_rows_selected
    if (length(row) == 0) { showNotification("Select a pitcher to restore.", type="warning"); return() }
    keys <- ineligible()
    df   <- acq_all_pitchers %>% filter(source_key %in% keys)
    key  <- df$source_key[row]; name <- df$pitcher_name[row]
    new_inelig <- keys[keys != key]
    ineligible(new_inelig); acq_save_ineligible(new_inelig)
    dataTableProxy("inelig_table") %>% selectRows(NULL)
    showNotification(glue("{name} restored."), type="message", duration=4)
  })

  output$inelig_hitter_table <- renderDT({
    keys <- ineligible_h()
    if (length(keys) == 0) return(datatable(tibble(Message="No hitters marked ineligible."),
                                            rownames=FALSE, options=list(dom="t", initComplete=acq_dt_header_js()), class="display compact"))
    acq_all_hitters %>%
      filter(source_key %in% keys) %>%
      mutate(`Age/Yr` = acq_age_or_class(age, class_year)) %>%
      select(Name=player_name, Pos=position, `Age/Yr`,
             Team=team_name, League=league_name, School=college,
             G, AB, AVG, OBP, SLG, OPS) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="t", pageLength=50, initComplete=acq_dt_header_js()),
                class="display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  })

  observeEvent(input$restore_hitter, {
    row <- input$inelig_hitter_table_rows_selected
    if (length(row) == 0) { showNotification("Select a hitter to restore.", type="warning"); return() }
    keys <- ineligible_h()
    df   <- acq_all_hitters %>% filter(source_key %in% keys)
    key  <- df$source_key[row]; name <- df$player_name[row]
    new_inelig <- keys[keys != key]
    ineligible_h(new_inelig); acq_save_ineligible_h(new_inelig)
    dataTableProxy("inelig_hitter_table") %>% selectRows(NULL)
    showNotification(glue("{name} restored."), type="message", duration=4)
  })

  # ── Top 10 ──────────────────────────────────────────────────────────────
  top10_pitcher_data <- function(league_in, min_ip, max_age, stat, role, hand) {
  data_version()  
  df <- acq_all_pitchers %>%
    filter(!source_key %in% ineligible()) %>%
    filter(!is.na(ERA), !is.na(IP), IP >= min_ip)

  if (league_in != "All") df <- df %>% filter(league_name == league_in)
  if (!is.na(max_age) && max_age < 99)
    df <- df %>% filter(is.na(age) | age <= max_age)

  if (role %in% c("SP","RP")) {
    gs_join <- acq_pitching_all %>%
      select(player_id, GS) %>% distinct(player_id, .keep_all=TRUE) %>%
      mutate(source_key = as.character(player_id))
    df <- df %>% left_join(gs_join %>% select(source_key, GS), by="source_key")
    if (role == "SP") df <- df %>% filter(!is.na(GS), GS/G >= 0.5)
    else              df <- df %>% filter(is.na(GS) | GS/G < 0.5)
  }

  if (hand != "All") df <- df %>% filter(pitch_hand == hand)

  asc_stats <- c("ERA","FIP","WHIP","BB9")
  df <- if (stat %in% asc_stats) arrange(df, .data[[stat]]) else
    arrange(df, desc(.data[[stat]]))

  df %>%
    slice_head(n = 10) %>%
    mutate(Rank = row_number(), `Age/Yr` = acq_age_or_class(age, class_year))
}

render_top10_pitcher_dt <- function(data) {
  data %>%
    select(Rank, Name=pitcher_name, Hand=pitch_hand,
           Team=team_name, League=league_name, `Age/Yr`,
           School=college, G, IP, ERA, FIP, WHIP,
           `K/9`=K9, `BB/9`=BB9, `K/BB`=KBB) %>%
    datatable(selection = "single", rownames=FALSE,
              options=list(dom="t", pageLength=10, initComplete=acq_dt_header_js()),
              class="display compact") %>%
    formatRound(c("ERA","FIP","WHIP","K/9","BB/9","K/BB"), 2)
}

  top10_hitter_dt <- function(league_in, min_ab, max_age, stat, pos, hand) {
    data_version()  
    df <- acq_all_hitters %>%
      filter(!source_key %in% ineligible_h()) %>%
      filter(position != "P", !is.na(OPS), !is.na(AB), AB >= min_ab)

    if (league_in != "All") df <- df %>% filter(league_name == league_in)
    if (!is.na(max_age) && max_age < 99)
      df <- df %>% filter(is.na(age) | age <= max_age)
    if (!("All" %in% pos)) df <- df %>% filter(position %in% pos)
    if (hand != "All") df <- df %>% filter(Bats == hand)          # NEW

    df %>%
      arrange(desc(.data[[stat]])) %>%
      slice_head(n = 10) %>%
      mutate(Rank = row_number(), `Age/Yr` = acq_age_or_class(age, class_year)) %>%
      select(Rank, Name=player_name, Pos=position, Bats,          # Bats added
             Team=team_name, League=league_name, `Age/Yr`,
             School=college, G, AB, AVG, OBP, SLG, OPS, HR, RBI, SB) %>%
      datatable(rownames=FALSE,
                options=list(dom="t", pageLength=10, initComplete=acq_dt_header_js()),
                class="display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  }

output$top10_hitting_table  <- renderDT(top10_hitter_dt(
    input$top10_league_h, input$top10_min_ab, input$top10_max_age_h,
    input$top10_stat_h, h_pos(), h_bats()))

  output$top10m_hitting_table  <- renderDT(top10_hitter_dt(
    input$top10m_league_h, input$top10m_min_ab, input$top10m_max_age_h,
    input$top10m_stat_h, h_pos_m(), h_bats_m()))

  top10_pitchers_inline <- reactive({
    top10_pitcher_data(input$top10_league_p, input$top10_min_ip, input$top10_max_age_p,
                        input$top10_stat_p, p_role(), p_hand())
  })
  top10_pitchers_modal <- reactive({
    top10_pitcher_data(input$top10m_league_p, input$top10m_min_ip, input$top10m_max_age_p,
                        input$top10m_stat_p, p_role_m(), p_hand_m())
  })

  output$top10_pitching_table  <- renderDT(render_top10_pitcher_dt(top10_pitchers_inline()))
  output$top10m_pitching_table <- renderDT(render_top10_pitcher_dt(top10_pitchers_modal()))

  # Click a row -> open profile directly, no button needed
  observeEvent(input$top10_pitching_table_rows_selected, {
    row <- input$top10_pitching_table_rows_selected
    req(row)
    selected_key(top10_pitchers_inline()$source_key[row[1]])
    toggleModal(session, "pitcher_modal", toggle = "open")
  }, ignoreNULL = TRUE)

  observeEvent(input$top10m_pitching_table_rows_selected, {
    row <- input$top10m_pitching_table_rows_selected
    req(row)
    selected_key(top10_pitchers_modal()$source_key[row[1]])
    toggleModal(session, "top10_modal", toggle = "close")
    toggleModal(session, "pitcher_modal", toggle = "open")
  }, ignoreNULL = TRUE)

  # ── Modal ───────────────────────────────────────────────────────────────
  output$modal_info <- renderUI({
    key <- selected_key(); req(key)
    p   <- acq_all_pitchers %>% filter(source_key == key); req(nrow(p) > 0)
    tags$div(
      tags$h3(p$pitcher_name, style=glue("color:{ACQ_TEAL}; margin:0 0 4px 0;")),
      if (!p$has_pbp) tags$span("No PBP data", class="no-pbp-badge"),
      tags$p(glue(
        "{coalesce(p$pitch_hand,'—')}HP  |  {acq_age_or_class(p$age, p$class_year)}  |  ",
        "School: {ifelse(is.na(p$college),'—',p$college)}  |  ",
        "Team: {ifelse(is.na(p$team_name),'—',p$team_name)}  |  ",
        "League: {ifelse(is.na(p$league_name),'—',p$league_name)}"
      ), class="info-bar")
    )
  })

  output$modal_body <- renderUI({
    key <- selected_key(); req(key)
    p   <- acq_all_pitchers %>% filter(source_key == key); req(nrow(p) > 0)
    if (p$has_pbp) {
      tagList(
        tags$hr(),
        plotOutput("movement_plot", height="500px"),
        tags$hr(),
        tags$h4("Pitch metrics", style=glue("color:{ACQ_TEAL}; margin-top:0;")),
        DTOutput("pitch_metrics_table")
      )
    } else {
      tagList(
        tags$hr(),
        tags$h4("Season stats", style=glue("color:{ACQ_TEAL}; margin-top:0;")),
        tags$table(
          class="table", style="color:white; width:auto;",
          tags$tr(tags$th("G"),tags$th("IP"),tags$th("ERA"),tags$th("FIP"),
                  tags$th("WHIP"),tags$th("K/9"),tags$th("BB/9"),tags$th("K/BB"),tags$th("HR/9")),
          tags$tr(
            tags$td(p$G), tags$td(round(p$IP,1)), tags$td(round(p$ERA,2)),
            tags$td(round(p$FIP,2)), tags$td(round(p$WHIP,2)), tags$td(round(p$K9,1)),
            tags$td(round(p$BB9,1)), tags$td(round(p$KBB,2)), tags$td(round(p$HR9,1))
          )
        )
      )
    }
  })

output$movement_plot <- renderPlot({
    key <- selected_key(); req(key)
    p   <- acq_all_pitchers %>% filter(source_key == key); req(p$has_pbp)
    pid <- as.integer(p$source_key)

    ind_raw  <- acq_pitch_level %>%
      filter(pitcher_id == pid, !is.na(hb_pov), !is.na(ivb),
             !is.na(pitch_type), pitch_type != "")
    mean_raw <- acq_movement_avg %>% filter(pitcher_id == pid)
    req(nrow(ind_raw) > 0)

    ind_data  <- ind_raw  %>% transmute(x = hb_pov, y = ivb, type = pitch_type)
    mean_data <- mean_raw %>% transmute(x = pfx_x, y = pfx_z, type = pitch_type, velo = velo)

    pcard_movement_plot_generic(
      ind_data, mean_data, palette = ACQ_PITCH_COLORS, na_color = "#AAAAAA",
      point_size = 3, mean_size = 12, label_size = 4
    ) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#F7FAF8", color = NA),
        panel.grid.major = element_line(color = "#DCE8DF", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        text             = element_text(color = ACQ_WHITE),
        axis.text        = element_text(color = "#444444"),
        axis.title       = element_text(color = "#666666", size = 11),
        plot.title       = element_text(color = ACQ_TEAL, face = "bold", size = 16,
                                        hjust = 0.5, margin = margin(b = 10)),
        legend.position  = "none"
      )
  }, bg = "white")

  output$pitch_metrics_table <- renderDT({
    key <- selected_key(); req(key)
    p   <- acq_all_pitchers %>% filter(source_key == key); req(p$has_pbp)
    pid <- as.integer(p$source_key)
    acq_pitch_metrics %>%
      filter(pitcher_id == pid, N >= 5) %>%
      arrange(desc(N)) %>%
      select(Pitch=pitch_type, N, Velo, iVB=iVB, HB,
             `Strike%`=Strike_pct, `FPS%`=FPS_pct, `Whiff%`=Whiff_pct,
             `Rel Ht`=Rel_Ht, `Rel Side`=Rel_Side, Ext) %>%
      datatable(rownames=FALSE,
                options=list(dom="t", pageLength=15, initComplete=acq_dt_header_js()),
                class="display compact") %>%
      formatRound(c("Velo","iVB","HB","Strike%","FPS%","Whiff%","Rel Ht","Rel Side","Ext"), 1)
  })
}

# ==========================================
# UI
# ==========================================
ui <- navbarPage(
  title       = "CAPS",
  id          = "caps_nav",
  collapsible = TRUE,
  windowTitle = "Brewster Whitecaps CAPS",
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Courier+Prime&family=Source+Sans+3:wght@400;600&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=8"),
      tags$style(HTML("
        #caps-splash {
          position: fixed; inset: 0; background: #0C2340; z-index: 9999;
          display: flex; align-items: center; justify-content: center;
          flex-direction: column; transition: opacity 0.6s ease;
        }
        #caps-splash.fade-out { opacity: 0; pointer-events: none; }
        #splash-logo {
          width: 100px; height: 100px; border-radius: 50%; background: white;
          display: flex; align-items: center; justify-content: center;
          animation: scaleIn 1.2s ease forwards; opacity: 0; overflow: hidden;
        }
        #splash-logo img { width: 90px; height: 90px; object-fit: contain; }
        #splash-title {
          color: white; font-family: 'Oswald', sans-serif; font-size: 28px;
          letter-spacing: 6px; margin-top: 20px;
          animation: fadeUp 1s ease 0.8s forwards; opacity: 0; text-transform: uppercase;
        }
        #splash-sub {
          color: #9DC2EA; font-size: 12px; letter-spacing: 2px; margin-top: 8px;
          animation: fadeUp 1s ease 1.2s forwards; opacity: 0; text-align: center;
        }
        @keyframes scaleIn {
          0%   { transform: scale(0.3); opacity: 0; }
          70%  { transform: scale(1.1); opacity: 1; }
          100% { transform: scale(1);   opacity: 1; }
        }
        @keyframes fadeUp {
          from { transform: translateY(12px); opacity: 0; }
          to   { transform: translateY(0);    opacity: 1; }
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            $('#caps-splash').addClass('fade-out');
            setTimeout(function() { $('#caps-splash').remove(); }, 700);
          }, 2800);
        });
      "))
    ),
    tags$div(
      id = "caps-splash",
      tags$div(id = "splash-logo", tags$img(src = "logo1.png")),
      tags$div(id = "splash-title", "C.A.P.S."),
      tags$div(id = "splash-sub",   "Centralized Application Platform for Staff")
    )
  ),
  tabPanel("Home",             value = "tab_home",           home_tab_ui()),
  tabPanel("Catcher Reports",  value = "tab_catcher",        catcher_ui()),
  tabPanel("Hitter Reports",   value = "tab_hitter",         hitter_ui()),
  tabPanel("Pitcher Reports",  value = "tab_pitcher",        pitcher_card_ui()),
  tabPanel("Season Pitcher Card", value = "tab_season_pitcher", season_pitcher_card_ui()),
  tabPanel("Cape Pitcher Scout", value = "tab_pitcher_player", cape_pitcher_player_page_ui()),
  tabPanel("Acquisitions Board", value = "tab_acq_board",    acq_board_ui()),
  tabPanel("Leaderboards",     value = "tab_leaderboards",   whitecaps_embedded_ui()),
  tabPanel("Umpire Reports",   value = "tab_umpire",
    tags$div(style = "padding: 40px 32px;",
      tags$h3("Umpire Reports", style = "color: #0C2340;"),
      tags$p("Coming soon.", style = "color: #5F5F6B;"))),
  tabPanel("Pitcher Card (Mock)", value = "tab_pcard_mock", pcard_report_ui()),
  tabPanel("CAPS Media", value = "tab_caps_media", caps_media_ui())
)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  nav_map <- c(
    hub            = "tab_home",
    catcher        = "tab_catcher",
    hitter         = "tab_hitter",
    pitcher        = "tab_pitcher",
    pitcher_player = "tab_pitcher_player",
    pitcher_mock   = "tab_pcard_mock", 
    whitecaps_app  = "tab_leaderboards"
  )

  observeEvent(input$nav_to, {
    target <- nav_map[input$nav_to]
    if (!is.na(target)) updateNavbarPage(session, "caps_nav", selected = target)
  })
  whitecaps_env$server(input, output, session)

  observeEvent(input$nav_caps_hub, {
    updateNavbarPage(session, "caps_nav", selected = "tab_home")
  })

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
      tryCatch(fetch_next_whitecaps_game(), error = function(e) NULL)
    }
  )

  # Scoreboard hero
  output$scoreboard_hero <- renderUI({
    ng <- next_game_reactive()

    opponent   <- ng$opponent   %||% "TBD"
    time_str   <- ng$time_str   %||% "TBD"
    venue      <- ng$venue      %||% "TBD"
    wins       <- ng$wins       %||% 0L
    losses     <- ng$losses     %||% 0L
    opp_wins   <- ng$opp_wins   %||% 0L
    opp_losses <- ng$opp_losses %||% 0L
    opp_abbr   <- ng$opp_abbr   %||% "OPP"
    ms         <- if (!is.null(ng) && !is.na(ng$ms)) ng$ms else (as.numeric(Sys.time()) + 86400) * 1000

    tags$div(
      style = "background:#071828; border-bottom:3px solid #0a3a5a; font-family:'Bebas Neue',sans-serif;",
      tags$div(
        style = "background:#0C2340; padding:8px 20px; display:flex; align-items:center; justify-content:space-between; border-bottom:2px solid #0a3a5a;",
        tags$span(style = "color:#fff; font-size:22px; letter-spacing:5px;",
          "C", tags$span(style = "color:#2DD4BF;", "A"), "PS"),
        tags$span(style = "color:rgba(255,255,255,0.35); font-size:10px; letter-spacing:2px;",
          "CENTRALIZED APPLICATION PLATFORM FOR STAFF"),
        tags$span(style = "color:rgba(255,255,255,0.35); font-size:10px; letter-spacing:2px;",
          "CAPE COD BASEBALL LEAGUE")
      ),
      tags$div(
        style = "padding:20px 28px;",
        tags$div(
          style = "display:grid; grid-template-columns:1fr auto 1fr; align-items:center; gap:16px; margin-bottom:18px;",
          tags$div(
            style = "display:flex; align-items:center; gap:14px;",
            tags$img(src = "BRE.png", style = "width:56px; height:56px; object-fit:contain;"),
            tags$div(
              tags$div(style = "color:#2DD4BF; font-size:11px; letter-spacing:2px;", "Brewster"),
              tags$div(style = "color:#fff; font-size:26px; letter-spacing:2px; line-height:1;", "WHITECAPS"),
              tags$div(style = "color:#fff; font-size:11px; letter-spacing:1px;",
                paste0(wins, " \u2013 ", losses))
            )
          ),
          tags$div(style = "color:rgba(255,255,255,0.15); font-size:24px; text-align:center;", "@"),
          tags$div(
            style = "display:flex; align-items:center; justify-content:flex-end; gap:14px;",
            tags$div(
              style = "text-align:right;",
              tags$div(style = "color:#2DD4BF; font-size:11px; letter-spacing:2px;",
                gsub(" .*", "", opponent)),
              tags$div(style = "color:#fff; font-size:26px; letter-spacing:2px; line-height:1;",
                toupper(gsub(".* ", "", opponent))),
              tags$div(style = "color:#fff; font-size:11px; letter-spacing:1px;",
                paste0(opp_wins, " \u2013 ", opp_losses))
            ),
            tags$img(src = ccbl_logo(opp_abbr), style = "width:56px; height:56px; object-fit:contain;")
          )
        ),
        tags$div(style = "height:1px; background:#0a3a5a; margin-bottom:14px;"),
        tags$div(style = "color:#2DD4BF; font-size:10px; letter-spacing:4px; text-align:center; margin-bottom:10px;",
          "GAME STARTS IN"),
        tags$div(
          style = "display:flex; justify-content:center; align-items:stretch; gap:0;",
          lapply(list(
            list(id1="cd-d1", id2="cd-d2", unit="DAYS", border=TRUE),
            list(id1="cd-h1", id2="cd-h2", unit="HRS",  border=TRUE),
            list(id1="cd-m1", id2="cd-m2", unit="MIN",  border=TRUE),
            list(id1="cd-s1", id2="cd-s2", unit="SEC",  border=FALSE)
          ), function(seg) {
            tagList(
              tags$div(
                style = paste0("display:flex; flex-direction:column; align-items:center; padding:0 10px;",
                  if (seg$border) " border-right:1px solid #0a3a5a;" else ""),
                tags$div(
                  style = "display:flex; gap:4px; margin-bottom:4px;",
                  tags$div(
                    style = "background:#040e1a; border:1px solid #0a3a5a; border-radius:3px; width:38px; height:54px; display:flex; align-items:center; justify-content:center; position:relative;",
                    tags$div(style = "position:absolute; left:0; right:0; top:50%; height:1px; background:#071828; z-index:2;"),
                    tags$span(id = seg$id1, style = "font-family:'Share Tech Mono',monospace; font-size:36px; font-weight:700; color:#2DD4BF; position:relative; z-index:1;", "0")
                  ),
                  tags$div(
                    style = "background:#040e1a; border:1px solid #0a3a5a; border-radius:3px; width:38px; height:54px; display:flex; align-items:center; justify-content:center; position:relative;",
                    tags$div(style = "position:absolute; left:0; right:0; top:50%; height:1px; background:#071828; z-index:2;"),
                    tags$span(id = seg$id2, style = "font-family:'Share Tech Mono',monospace; font-size:36px; font-weight:700; color:#2DD4BF; position:relative; z-index:1;", "0")
                  )
                ),
                tags$div(style = "color:rgba(45,212,191,0.4); font-size:9px; letter-spacing:2px;", seg$unit)
              ),
              if (seg$border) tags$div(
                style = "display:flex; flex-direction:column; align-items:center; justify-content:center; padding:0 6px; padding-bottom:20px; gap:6px;",
                tags$div(style = "width:5px; height:5px; background:#2DD4BF; border-radius:50%; opacity:0.5;"),
                tags$div(style = "width:5px; height:5px; background:#2DD4BF; border-radius:50%; opacity:0.5;")
              )
            )
          })
        ),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-top:14px; padding-top:12px; border-top:1px solid #0a3a5a;",
          tags$div(
            style = "text-align:center;",
            tags$div(style = "color:rgba(45,212,191,0.4); font-size:9px; letter-spacing:2px; margin-bottom:2px;", "FIRST PITCH"),
            tags$div(style = "color:#fff; font-size:13px; letter-spacing:1px; font-family:'Bebas Neue',sans-serif;", time_str)
          ),
          tags$div(
            style = "text-align:center;",
            tags$div(style = "color:rgba(45,212,191,0.4); font-size:9px; letter-spacing:2px; margin-bottom:2px;", "VENUE"),
            tags$div(style = "color:#fff; font-size:13px; letter-spacing:1px; font-family:'Bebas Neue',sans-serif;", venue)
          )
        )
      ),
      tags$div(
        id = "sb-bulbs",
        style = "display:flex; justify-content:center; gap:6px; padding:10px 20px; background:#0C2340; border-top:2px solid #0a3a5a;"
      ),
      tags$script(HTML(sprintf("
        (function() {
          var row = document.getElementById('sb-bulbs');
          for(var i=0;i<40;i++){
            var b=document.createElement('div');
            b.style.cssText='width:8px;height:8px;border-radius:50%%;background:#0a3a5a;display:inline-block;';
            row.appendChild(b);
          }
          var bulbs=row.querySelectorAll('div'),frame=0;
          setInterval(function(){
            bulbs.forEach(function(b,i){ b.style.background=(i+frame)%%4===0?'#2DD4BF':'#0a3a5a'; });
            frame++;
          }, 300);
          var target=%.0f;
          function pad(n){ return String(Math.floor(n)).padStart(2,'0'); }
          function tick(){
            var diff=Math.max(0,target-Date.now());
            var d=pad(Math.floor(diff/86400000));
            var h=pad(Math.floor((diff%%86400000)/3600000));
            var m=pad(Math.floor((diff%%3600000)/60000));
            var s=pad(Math.floor((diff%%60000)/1000));
            document.getElementById('cd-d1').textContent=d[0];
            document.getElementById('cd-d2').textContent=d[1];
            document.getElementById('cd-h1').textContent=h[0];
            document.getElementById('cd-h2').textContent=h[1];
            document.getElementById('cd-m1').textContent=m[0];
            document.getElementById('cd-m2').textContent=m[1];
            document.getElementById('cd-s1').textContent=s[0];
            document.getElementById('cd-s2').textContent=s[1];
          }
          tick(); setInterval(tick,1000);
        })();
      ", ms)))
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

    updateNavbarPage(session, "caps_nav", selected = "tab_pcard_mock")   # NEW — navigate immediately

    brew_pitchers <- season_data %>%
      filter(grepl("BRE|Brewster", PitcherTeam, ignore.case = TRUE)) %>%
      distinct(Pitcher)

    norm_name <- function(x) trimws(tolower(x))

    matched <- brew_pitchers %>%
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
    req(!is.null(catcher_data()))
    teams <- sort(unique(catcher_data()$CatcherTeam))
    selectInput("catcher_team_select", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = teams[grepl("BRE|Brewster", teams, ignore.case = TRUE)][1])
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
        logo_path   = "www/logo1.png"
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
            logo_path   = "www/logo1.png"
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
    req(!is.null(hitter_data()))
    teams <- sort(unique(hitter_data()$BatterTeam))
    selectInput("hitter_team_select", "Select Team:",
                choices  = setNames(teams, team_display_name(teams)),
                selected = teams[grepl("BRE|Brewster", teams, ignore.case = TRUE)][1])
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
  cape_pitcher_player_page_server(input, output, session, source_data = season_data)

  # ==========================================
  # PITCHER SERVER LOGIC -> BrewSummaryCard
  # ==========================================
  pitcher_card_server(input, output, session)

  # ==========================================
  # SEASON PITCHER CARD SERVER (College26)
  # ==========================================
  season_pitcher_card_server(input, output, session)

  # ==========================================
  # ACQUISITIONS BOARD SERVER (AcquisitionsApp3)
  # ==========================================
  acq_board_server(input, output, session)

  caps_media_server(input, output, session)

}

      
shinyApp(ui = ui, server = server)
