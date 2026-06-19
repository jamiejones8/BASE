library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(rsconnect)
library(baseballr)
library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(janitor)
library(ggthemes)
library(ggpubr)
library(jsonlite)
library(utils)
library(grid)
library(gridExtra)
library(png)
library(lightgbm)
library(httr)
library(jpeg)
library(zoo)
library(gtable)
library(lightgbm)
library(sysfonts)

options(shiny.maxRequestSize = 10000000 * 1024^2)
pdf(file = NULL)
Sys.setenv(TZ='EST')

model <- lgb.load('college26.model')
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
  soft_ramp <- colorRamp(c("#0C234B", tok$bg_card, "#AB0520"))
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

getCollegeStuff <- function(game, final_model, bullpen = FALSE,
                            height_override = NULL, set_override = NULL) {

  COLLEGE_STUFF_MEAN <- -0.0252977442132292
  COLLEGE_STUFF_SD   <-  0.0144571851458667

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
    game <- getaccelbreak(game) %>%
      cleanColumnContents() %>%
      calculateArmAngle() %>%
      get_rotated_movement(ivb_col = "vertaccel", hb_col = "horzaccel") %>%
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
        vertaccel = round(vertaccel, 1),
        horzaccel = round(horzaccel, 1),
        horzaccel = ifelse(PitcherThrows == "Left", horzaccel * -1, horzaccel),
        SpinRate = round(SpinRate, 1),
        RelHeight = round(RelHeight, 1),
        Extension = round(Extension, 1)
      ) %>%
      ungroup()
  } else {
    game <- getaccelbreak(game) %>%
      cleanColumnContents() %>%
      calculateArmAngle() %>%
      get_rotated_movement(ivb_col = "vertaccel", hb_col = "horzaccel") %>%
      filter(
        !is.na(PitcherThrows) & !is.na(BatterSide) &
          PitcherThrows %in% c("Left", "Right") &
          BatterSide %in% c("Left", "Right")
      ) %>%
      mutate(
        armangle = round(armangle, 1),
        RelSpeed = round(RelSpeed, 1),
        vertaccel = round(vertaccel, 1),
        horzaccel = round(horzaccel, 1),
        horzaccel = ifelse(PitcherThrows == "Left", horzaccel * -1, horzaccel),
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
      has_fastball = any(is_fastball),
      primary_fb = case_when(
        has_fastball ~ {
          fastball_counts <- pitch_count[is_fastball]
          max_fb_count <- max(fastball_counts, na.rm = TRUE)
          first(TaggedPitchType[is_fastball & pitch_count == max_fb_count])
        },
        !has_fastball ~ {
          max_pitch_count <- max(pitch_count, na.rm = TRUE)
          first(TaggedPitchType[pitch_count == max_pitch_count])
        },
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      fb_rel_speed = round(mean(RelSpeed[TaggedPitchType == primary_fb], na.rm = TRUE), 1),
      fb_vertaccel = round(mean(vertaccel[TaggedPitchType == primary_fb], na.rm = TRUE), 1),
      fb_horzaccel = round(mean(horzaccel[TaggedPitchType == primary_fb], na.rm = TRUE), 1),

      velo_diff = round(ifelse(is.na(primary_fb), 0, RelSpeed - fb_rel_speed), 1),
      vertaccel_diff = round(ifelse(is.na(primary_fb), 0, vertaccel - fb_vertaccel), 1),
      horzaccel_diff = round(ifelse(is.na(primary_fb), 0, horzaccel - fb_horzaccel), 1),

      vertaccel_adj = round(rotated_IVB, 1),
      horzaccel_adj = round(rotated_HB, 1),
      horzaccel_adj = ifelse(PitcherThrows == "Left", horzaccel_adj * -1, horzaccel_adj)
    ) %>%
    ungroup()

  feature_vars <- c("RelSpeed", "vertaccel", "horzaccel",
                    "SpinRate", "RelHeight", "armangle", "Extension", "velo_diff",
                    "vertaccel_diff", "horzaccel_diff", "vertaccel_adj", "horzaccel_adj")

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
        game_vs_righties$vertaccel,
        game_vs_righties$horzaccel,
        game_vs_righties$SpinRate,
        game_vs_righties$RelHeight,
        game_vs_righties$Extension,
        game_vs_righties$velo_diff,
        game_vs_righties$vertaccel_diff,
        game_vs_righties$horzaccel_diff,
        game_vs_righties$vertaccel_adj,
        game_vs_righties$horzaccel_adj,
        game_vs_righties$armangle,
        game_vs_righties$opp_hand
      )))

      game_complete$rv_vs_L <- predict(final_model, as.matrix(cbind(
        game_vs_lefties$RelSpeed,
        game_vs_lefties$vertaccel,
        game_vs_lefties$horzaccel,
        game_vs_lefties$SpinRate,
        game_vs_lefties$RelHeight,
        game_vs_lefties$Extension,
        game_vs_lefties$velo_diff,
        game_vs_lefties$vertaccel_diff,
        game_vs_lefties$horzaccel_diff,
        game_vs_lefties$vertaccel_adj,
        game_vs_lefties$horzaccel_adj,
        game_vs_lefties$armangle,
        game_vs_lefties$opp_hand
      )))

      game_complete$Stuff_vs_R <- scale_Stuff(game_complete$rv_vs_R, COLLEGE_STUFF_MEAN, COLLEGE_STUFF_SD)
      game_complete$Stuff_vs_L <- scale_Stuff(game_complete$rv_vs_L, COLLEGE_STUFF_MEAN, COLLEGE_STUFF_SD)
      game_complete$Stuff <- (game_complete$Stuff_vs_R + game_complete$Stuff_vs_L) / 2
    } else {
      game_complete$rv <- predict(final_model, as.matrix(cbind(
        game_complete$RelSpeed,
        game_complete$vertaccel,
        game_complete$horzaccel,
        game_complete$SpinRate,
        game_complete$RelHeight,
        game_complete$Extension,
        game_complete$velo_diff,
        game_complete$vertaccel_diff,
        game_complete$horzaccel_diff,
        game_complete$vertaccel_adj,
        game_complete$horzaccel_adj,
        game_complete$armangle,
        game_complete$opp_hand
      )))
      game_complete$Stuff <- scale_Stuff(game_complete$rv, COLLEGE_STUFF_MEAN, COLLEGE_STUFF_SD)
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
    select(any_of(c("Date","GameUID","HomeTeam","AwayTeam","BatterTeam","Tilt")),
           Batter,Pitcher,PitcherTeam,PlayResult,PitchCall,TaggedPitchType,RelSpeed,SpinRate,
           Extension,PlateLocSide,PlateLocHeight,RelSide,RelHeight,ax0,ay0,az0,vx0,
           vz0,vy0,pfxx,pfxz,InducedVertBreak,HorzBreak,SpinAxis,PitcherThrows,BatterSide,
           PitchofPA,VertApprAngle,AutoPitchType, KorBB, OutsOnPlay, TaggedHitType) %>%
    mutate(
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
    read.csv(datapath)
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
  navy         = "#0C234B",
  cardinal     = "#AB0520",
  cardinal_glow= "#FF1F3D",
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
  fallback <- list(primary = "#0C234B", secondary = "#AB0520",
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
                           team_label   = "ARIZONA PITCHING",
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
    textGrob("Arizona Baseball",
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
    geom_point(aes(fill = TaggedPitchType),
               shape = 21, size = 2.6, stroke = 0.4,
               colour = "black", alpha = 0.9) +
    geom_polygon(data = home_plate, aes(x, y),
                 fill = NA, colour = "#5F5F6B", linewidth = 0.7,
                 inherit.aes = FALSE) +
    annotate("rect", xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.6,
             fill = NA, colour = "#36363F", linewidth = 0.8) +
    scale_colour_manual(values = pal_for(game$TaggedPitchType)) +
    scale_fill_manual(values = pal_for(game$TaggedPitchType)) +
    coord_fixed() +
    xlim(-3, 3) + ylim(0, 5) +
    labs(title = sprintf("PITCH LOCATION  ·  %sHB", side), x = NULL, y = NULL) +
    theme_void(base_family = font_sans) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      legend.position  = "none",
      plot.title = element_text(colour = "#5F5F6B", size = 13,
                                face = "bold", hjust = 0,
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
  game <- getCollegeStuff(game, model, FALSE,
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
      IP     = floor(total_outs / 3) + ((total_outs %% 3) / 10),
      AB     = sum(AB, na.rm = TRUE),
      Hits   = sum(Hit, na.rm = TRUE),
      K      = sum(Strikeout, na.rm = TRUE),
      BB     = sum(Walk, na.rm = TRUE),
      HR     = sum(HR, na.rm = TRUE),
      bbe    = sum(BBE, na.rm = TRUE),
      gb     = sum(GB,  na.rm = TRUE),
      `GB%`  = ifelse(bbe > 0,
                      sprintf("%.1f%%", gb / bbe * 100),
                      "--"),
      Whiffs = sum(Whiff, na.rm = TRUE)
    ) %>%
    select(-total_outs, -bbe, -gb) %>%
    select(Pitcher, IP, AB, Hits, K, BB, HR, `GB%`, Whiffs) %>%
    rename("Pitcher ID" = Pitcher)
  
  return(game_stats)
}

ui <- fluidPage(
  theme = bs_theme(bg = "#ffffff", fg = "#333333", primary = "#428bca"),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400;700&display=swap", 
              rel = "stylesheet"),
    tags$style(HTML("
      * { font-family: 'Roboto Condensed', sans-serif !important; }
    "))
  ),
  titlePanel("Create Summary Card"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("incsv", "Input File (CSV or Parquet)",
                accept = c(".csv", ".parquet"), multiple = FALSE),
      actionButton("update", "Load Pitchers", icon("magnifying-glass"),
                   class = "btn-primary btn-block"),
      selectizeInput("pitcher", "Pitcher Name:", NULL),
      selectInput("game", "Game:", choices = c("All Games" = "all")),
      tags$div(
        style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
        tags$label("Enter Pitcher Height (Will only work for Pitchers not in Height CSV)",
                   style = "margin:0; font-weight:bold;"),
        bslib::tooltip(
          tags$span(icon("circle-info"),
                    style = "color:#666; cursor:help;"),
          paste("Use this only when the pitcher isn't in College26Heights.csv.",
                "Arm angle is estimated as atan2(RelHeight - shoulder, RelSide),",
                "where shoulder = 70% of total height. No height = no arm-angle estimate,",
                "so providing one here lets unlisted pitchers still get an Est. Arm Angle."),
          placement = "right"
        )
      ),
      fluidRow(
        column(6, numericInput("height_ft", "ft", value = NA,
                               min = 4, max = 8, step = 1)),
        column(6, numericInput("height_in", "in", value = NA,
                               min = 0, max = 11, step = 1))
      ),
      tags$div(
        style = "display:flex; align-items:center; gap:6px; margin-top:8px; margin-bottom:4px;",
        tags$label("Set Position on Rubber (overrides Height CSV)",
                   style = "margin:0; font-weight:bold;"),
        bslib::tooltip(
          tags$span(icon("circle-info"),
                    style = "color:#666; cursor:help;"),
          paste("Arm angle assumes the pitcher stands in the middle of the rubber.",
                "If they stand off-center, set this to their position in feet from rubber center:",
                "-1 = 1ft toward 1B, 0 = middle, +1 = 1ft toward 3B.",
                "Leave at 0 to use the value coded in College26Heights.csv (if any)."),
          placement = "right"
        )
      ),
      tags$div(
        style = "margin-top:6px;",
        # Overhead view of the rubber, sized to match the slider's track.
        # ion.rangeSlider pads the track only slightly, so use a small inset.
        tags$div(
          style = "position:relative; height:40px; padding:0 4px;",
          # Rubber (white slab) — spans the full slider track width
          tags$div(style = paste(
            "position:relative; width:100%; height:34px; top:50%;",
            "transform:translateY(-50%); background:#fff;",
            "border:1px solid #444; border-radius:2px;",
            "display:flex; align-items:center; justify-content:space-between;",
            "padding:0 8px; box-sizing:border-box;"
          ),
            tags$span("← 1B", style = paste(
              "font-size:12px; font-weight:bold; color:#333;",
              "white-space:nowrap;"
            )),
            tags$span("3B →", style = paste(
              "font-size:12px; font-weight:bold; color:#333;",
              "white-space:nowrap;"
            ))
          ),
          # Center tick
          tags$div(style = paste(
            "position:absolute; left:50%; top:50%; width:1px; height:38px;",
            "background:#444; transform:translate(-50%, -50%);"
          ))
        ),
        sliderInput("set_pos", NULL, min = -1, max = 1, value = 0, step = 0.1,
                    ticks = FALSE)
      ),
      actionButton("update1", "Make/Update Card", icon("plus"),
                   class = "btn-success btn-block"),
      actionButton("reset_pitches", "Reset Pitch Tags",
                   class = "btn-secondary btn-block"),
      downloadButton("downloadPlot", "Download Card", class = "btn-info btn-block")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Card",
                 div(style = "max-width: 800px; width: 100%; margin: 0 auto;",
                     imageOutput("combinedPlot", width = "100%", height = "auto"))),
        tabPanel("Pitch Retag",
                 fluidRow(
                   column(4, selectizeInput("filter_pitch", "Filter to Pitch Types:",
                                            choices = c("All"), multiple = TRUE,
                                            options = list(plugins = list("remove_button")))),
                   column(4, selectInput("new_pitch_type", "New Pitch Type:",
                                         choices = c("Four-Seam","Sinker","Cutter","Slider",
                                                     "Sweeper","Curveball","Knuckle Curve",
                                                     "Slurve","Changeup","Splitter",
                                                     "Knuckle Ball","Eephus","Slow Curve",
                                                     "Screwball","Two-Seam Fastball","Fastball"))),
                   column(4, br(),
                          actionButton("apply_retag", "Apply To Selected",
                                       class = "btn-primary btn-block"),
                          actionButton("delete_pitches", "Delete Selected",
                                       class = "btn-danger btn-block"))
                 ),
                 fluidRow(
                   column(3, sliderInput("filter_velo", "Velo (mph)",
                                         min = 40, max = 110,
                                         value = c(40, 110), step = 0.5)),
                   column(3, sliderInput("filter_spin", "Spin (rpm)",
                                         min = 0, max = 4500,
                                         value = c(0, 4500), step = 50)),
                   column(3, sliderInput("filter_ivb", "IVB (in)",
                                         min = -35, max = 35,
                                         value = c(-35, 35), step = 0.5)),
                   column(3, sliderInput("filter_hb", "HB (in)",
                                         min = -35, max = 35,
                                         value = c(-35, 35), step = 0.5))
                 ),
                 helpText("Lasso-select pitches on the plot, then Apply To Selected (retag) or Delete Selected (remove). Sliders narrow the visible pitches. Click Make Card to redraw."),
                 textOutput("selection_info"),
                 plotlyOutput("retag_plot", height = "650px"),
                 tags$hr(),
                 fluidRow(
                   column(8, selectInput("delete_class", "Delete Whole Pitch Class:",
                                         choices = character(0))),
                   column(4, br(),
                          actionButton("delete_class_btn", "Delete Class",
                                       class = "btn-danger btn-block"))
                 ),
                 helpText("Removes every pitch with the selected tag — useful for classes whose pitches have NA break and don't appear on the plot."))
      )
    )
  )
)

server <- function(input, output, session) {
  game_data <- reactiveVal()

  current_pitches <- reactiveVal(NULL)

  # Combine ft + in into decimal feet (e.g. 6'3" -> 6.25). Returns NULL when
  # both inputs are blank so getCollegeStuff knows to skip the override.
  height_override_dec <- reactive({
    ft <- suppressWarnings(as.numeric(input$height_ft))
    inch <- suppressWarnings(as.numeric(input$height_in))
    if (is.na(ft) && is.na(inch)) return(NULL)
    if (is.na(ft)) ft <- 0
    if (is.na(inch)) inch <- 0
    val <- ft + inch / 12
    if (!is.finite(val) || val <= 0) NULL else val
  })

  # NULL when slider is at 0 (use the per-pitcher value from the heights CSV).
  # Any non-zero slider value overrides whatever's coded in the CSV.
  set_override_val <- reactive({
    v <- suppressWarnings(as.numeric(input$set_pos))
    if (length(v) == 0 || is.na(v) || !is.finite(v) || v == 0) NULL else v
  })

  observeEvent(input$update, {
    req(input$incsv)
    df <- read_input_file(input$incsv$datapath, input$incsv$name)
    pname <- pitcher_summary(df)
    game_data(pname)
    if(nrow(pname) == 0) {
      showNotification("No pitchers found for the selected game.", type = "warning")
    } else {
      pchoices <- pname %>%
        distinct(Pitcher, PitcherTeam) %>%
        arrange(Pitcher)
      team_labels <- vapply(pchoices$PitcherTeam,
                            function(t) team_palette(t)$label,
                            character(1))
      vec <- setNames(
        paste(pchoices$Pitcher, pchoices$PitcherTeam, sep = "::"),
        paste0(format_pitcher_name(pchoices$Pitcher), " - ", team_labels)
      )
      updateSelectizeInput(session, "pitcher", "Pitcher Name:", choices = vec)
      game_data(pname)
    }
  })

  selected_points <- reactiveVal(NULL)

  # When the user picks a different pitcher, load their pitches into the editor
  load_pitcher_pitches <- function() {
    req(game_data(), input$pitcher)
    sel <- parse_pitcher_value(input$pitcher)
    if (is.na(sel$name)) {
      current_pitches(NULL)
      return()
    }
    pp <- game_data() %>%
      filter(Pitcher == sel$name, PitcherTeam == sel$team)

    g_sel <- input$game
    if (!is.null(g_sel) && nzchar(g_sel) && g_sel != "all" &&
        "GameUID" %in% names(pp)) {
      pp <- pp %>% filter(GameUID == g_sel)
    }

    pp <- pp %>%
      mutate(TaggedPitchType = canonicalize_pitch(TaggedPitchType),
             row_id = row_number())
    current_pitches(pp)
    selected_points(NULL)
  }

  # Refresh the game dropdown whenever the pitcher changes
  observeEvent(input$pitcher, {
    req(game_data(), input$pitcher)
    sel <- parse_pitcher_value(input$pitcher)
    if (is.na(sel$name)) {
      updateSelectInput(session, "game",
                        choices = c("All Games" = "all"), selected = "all")
      return()
    }
    pp_all <- game_data() %>%
      filter(Pitcher == sel$name, PitcherTeam == sel$team)
    if ("GameUID" %in% names(pp_all) && nrow(pp_all) > 0) {
      have_date <- "Date" %in% names(pp_all)
      have_opp  <- "BatterTeam" %in% names(pp_all)
      keep_cols <- c("GameUID",
                     if (have_date) "Date",
                     if (have_opp)  "BatterTeam")
      g_df <- pp_all %>%
        distinct(across(all_of(keep_cols)))
      if (have_date) g_df <- g_df %>% arrange(Date)
      labels <- vapply(seq_len(nrow(g_df)), function(i) {
        d <- if (have_date && !is.na(g_df$Date[i]))
          format_pretty_date(g_df$Date[i]) else "?"
        o <- if (have_opp && !is.na(g_df$BatterTeam[i]))
          team_palette(g_df$BatterTeam[i])$label else "?"
        paste0(d, "  ·  vs ", o)
      }, character(1))
      choices <- c("All Games" = "all",
                   setNames(as.character(g_df$GameUID), labels))
    } else {
      choices <- c("All Games" = "all")
    }
    updateSelectInput(session, "game", choices = choices, selected = "all")
  }, ignoreInit = TRUE)

  # Reload pitches whenever pitcher or game selection changes
  observeEvent(list(input$pitcher, input$game), {
    load_pitcher_pitches()
  }, ignoreInit = TRUE)

  observeEvent(input$reset_pitches, { load_pitcher_pitches() })

  # Keep the filter dropdown + delete-class dropdown in sync with the pitcher's
  # current pitch types
  observe({
    d <- current_pitches()
    cur <- isolate(input$filter_pitch)
    if (is.null(d) || nrow(d) == 0) {
      updateSelectizeInput(session, "filter_pitch", choices = c("All"),
                           selected = character(0))
      updateSelectInput(session, "delete_class", choices = character(0))
      return()
    }
    types <- sort(unique(d$TaggedPitchType))
    keep  <- cur[cur %in% c("All", types)]
    updateSelectizeInput(session, "filter_pitch",
                         choices = c("All", types), selected = keep)
    updateSelectInput(session, "delete_class", choices = types)
  })

  output$retag_plot <- plotly::renderPlotly({
    d <- current_pitches()
    req(d)
    flt <- input$filter_pitch
    if (length(flt) > 0 && !"All" %in% flt) {
      d <- d %>% filter(TaggedPitchType %in% flt)
    }
    in_range <- function(x, rng) {
      if (length(rng) != 2) return(rep(TRUE, length(x)))
      is.na(x) | (x >= rng[1] & x <= rng[2])
    }
    d <- d %>% filter(
      in_range(RelSpeed,        input$filter_velo),
      in_range(SpinRate,        input$filter_spin),
      in_range(InducedVertBreak, input$filter_ivb),
      in_range(HorzBreak,        input$filter_hb)
    )
    if (nrow(d) == 0) {
      return(plotly::plot_ly() %>% plotly::layout(
        title = "No pitches match the current filter."))
    }
    spin_str <- ifelse(is.na(d$SpinRate), "--",
                       format(round(d$SpinRate), big.mark = ","))
    tilt_str <- if ("Tilt" %in% names(d)) {
      ifelse(is.na(d$Tilt) | d$Tilt == "", "--", as.character(d$Tilt))
    } else rep("--", nrow(d))
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
      data       = d,
      x          = ~HorzBreak,
      y          = ~InducedVertBreak,
      color      = ~TaggedPitchType,
      colors     = pal_for(d$TaggedPitchType),
      customdata = ~row_id,
      text       = hover_text,
      hoverinfo  = "text",
      type       = "scatter",
      mode       = "markers",
      marker     = list(size = 10, opacity = 0.85),
      source     = "retagplot"
    ) %>% plotly::layout(
      title  = "Pitch Movement (lasso select to highlight pitches)",
      dragmode = "lasso",
      xaxis  = list(title = "Horizontal Break (in)", range = c(-35, 35),
                    zerolinecolor = "rgba(0,0,0,0.25)"),
      yaxis  = list(title = "Induced Vertical Break (in)", range = c(-35, 35),
                    scaleanchor = "x", scaleratio = 1,
                    zerolinecolor = "rgba(0,0,0,0.25)"),
      legend = list(orientation = "h", y = -0.18)
    ) %>% plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("autoScale2d","hoverClosestCartesian",
                                 "hoverCompareCartesian","toggleSpikelines")
    )
  })

  observe({
    ed <- tryCatch(plotly::event_data("plotly_selected", source = "retagplot"),
                   error = function(e) NULL)
    if (is.null(ed) || !is.data.frame(ed) || nrow(ed) == 0 ||
        is.null(ed$customdata)) {
      selected_points(NULL)
      return()
    }
    ids <- suppressWarnings(as.integer(ed$customdata))
    ids <- ids[!is.na(ids)]
    selected_points(if (length(ids)) ids else NULL)
  })

  output$selection_info <- renderText({
    s <- selected_points()
    if (is.null(s) || length(s) == 0)
      "No pitches selected. Use the box/lasso tool on the plot."
    else paste(length(s), "pitches selected.")
  })

  observeEvent(input$apply_retag, {
    req(current_pitches(), input$new_pitch_type)
    s <- selected_points()
    if (is.null(s) || length(s) == 0) {
      showNotification("Select pitches on the plot first.", type = "warning")
      return()
    }
    d <- current_pitches()
    rows <- which(d$row_id %in% s)
    if (length(rows) == 0) return()
    d$TaggedPitchType[rows] <- input$new_pitch_type
    current_pitches(d)
    selected_points(NULL)
    showNotification(sprintf("Retagged %d pitches as %s.",
                             length(rows), input$new_pitch_type),
                     type = "message")
  })

  observeEvent(input$delete_pitches, {
    req(current_pitches())
    s <- selected_points()
    if (is.null(s) || length(s) == 0) {
      showNotification("Select pitches on the plot first.", type = "warning")
      return()
    }
    d <- current_pitches()
    keep <- !(d$row_id %in% s)
    removed <- sum(!keep)
    if (removed == 0) return()
    current_pitches(d[keep, , drop = FALSE])
    selected_points(NULL)
    showNotification(sprintf("Deleted %d pitches.", removed), type = "message")
  })

  observeEvent(input$delete_class_btn, {
    req(current_pitches(), input$delete_class)
    d <- current_pitches()
    keep <- d$TaggedPitchType != input$delete_class
    removed <- sum(!keep, na.rm = TRUE)
    if (removed == 0) {
      showNotification(sprintf("No pitches tagged %s.", input$delete_class),
                       type = "warning")
      return()
    }
    current_pitches(d[keep, , drop = FALSE])
    selected_points(NULL)
    showNotification(sprintf("Deleted %d pitches tagged %s.",
                             removed, input$delete_class),
                     type = "message")
  })

  combinedPlot <- reactiveVal()
  
  observeEvent(input$update1, {
    req(current_pitches())
    game <- current_pitches()

    if (nrow(game) == 0) {
      showNotification("No data available for the selected pitcher.", type = "warning")
      return()
    }

    # Stuff+ enrichment for charts (table builder runs this internally too)
    game_with_stuff <- getCollegeStuff(game, model, FALSE,
                                       height_override = height_override_dec(),
                                       set_override = set_override_val()) %>%
      mutate(Stuff = ifelse(Stuff < 50, NA, Stuff),
             TaggedPitchType = canonicalize_pitch(TaggedPitchType))

    arsenal_full <- arsenal_summary(game, height_override = height_override_dec(),
                                    set_override = set_override_val())

    # Headline Stuff+ — pull from Total row
    stuff_overall <- arsenal_full$StuffP[arsenal_full$Type == "Total"]
    if (length(stuff_overall) == 0 || is.na(stuff_overall) ||
        stuff_overall %in% c("NA", "NaN", "—", "--")) stuff_overall <- "--"

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

    pitcher_display <- format_pitcher_name(parse_pitcher_value(input$pitcher)$name)

    team_logo <- fetch_team_logo(pitcher_team_abbr, pal$logo_url)

    # Build all grobs
    header_g <- header_grob_fn(pitcher_display, subtitle,
                               team_label   = pal$label,
                               banner_color = pal$primary,
                               accent_color = pal$secondary,
                               logo         = team_logo)

    boxscore_data <- boxscore_summary(game)
    if ("Pitcher ID" %in% names(boxscore_data)) {
      boxscore_data <- boxscore_data %>% select(-`Pitcher ID`)
    }
    boxscore_data$`Stuff+` <- stuff_overall
    box_g <- box_grob_fn(boxscore_data)

    break_p <- mockup_break_plot(game_with_stuff)
    lhb_p   <- mockup_plate_plot(game_with_stuff %>% filter(BatterSide == "Left"),  "L")
    rhb_p   <- mockup_plate_plot(game_with_stuff %>% filter(BatterSide == "Right"), "R")
    usage_p <- mockup_usage_plot(game_with_stuff)

    arsenal_g <- build_arsenal_grob(arsenal_full,percentiledata)
    footer_g  <- footer_grob_fn()

    plate_col <- arrangeGrob(lhb_p, rhb_p, ncol = 1, heights = c(1, 1))
    main_row  <- arrangeGrob(plate_col, break_p, usage_p, ncol = 3, widths = c(2, 5, 3))

    card <- arrangeGrob(
      header_g, box_g, main_row, arsenal_g, footer_g,
      ncol = 1,
      heights = c(0.115, 0.080, 0.450, 0.330, 0.025)
    )

    page <- gTree(children = gList(
      rectGrob(gp = gpar(fill = tok$bg_page, col = NA)),
      card
    ))

    combinedPlot(page)

    output$combinedPlot <- renderImage({
      outfile <- tempfile(fileext = ".png")
      showtext::showtext_opts(dpi = 96)
      grDevices::png(outfile,
                     width = 1200, height = 1200, units = "px",
                     res = 96, bg = tok$bg_page, type = "cairo")
      showtext::showtext_begin()
      on.exit({
        showtext::showtext_end()
        grDevices::dev.off()
      }, add = TRUE, after = FALSE)
      grid::grid.draw(combinedPlot())
      list(src = outfile, contentType = "image/png",
           width = "100%", height = "auto",
           alt = "Pitching Summary Card")
    }, deleteFile = TRUE)

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0("Arizona_Pitcher_",
               gsub("[^A-Za-z0-9]+", "_", parse_pitcher_value(input$pitcher)$name),
               ".png")
      },
      content = function(file) {
        # Match renderPlot's font pipeline: Cairo device + showtext hook.
        # ggsave's `type=cairo` is not honored on Windows, so open the device
        # manually and bracket with showtext_begin/end.
        showtext::showtext_opts(dpi = 300)
        on.exit(showtext::showtext_opts(dpi = 96), add = TRUE)

        grDevices::png(file,
                       width = 12.5, height = 12.5, units = "in",
                       res = 300, bg = tok$bg_page, type = "cairo")
        showtext::showtext_begin()
        on.exit({
          showtext::showtext_end()
          grDevices::dev.off()
        }, add = TRUE, after = FALSE)

        grid::grid.draw(combinedPlot())
      }
    )
  })
}

shinyApp(ui = ui, server = server)