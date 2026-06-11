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

source("scout_app.R")
source("leaderboards_embed.R")

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
  # Title shows "Name - Team" when a team label is available.
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
               pch = 23, size = unit(11, "pt"), gp = key_gp),
    textGrob("Hard Hit (95+ EV)", x = unit(0.415, "npc"), y = unit(0.5, "npc"),
             hjust = 0, gp = txt_gp),
    # Whiff — square
    pointsGrob(x = unit(0.565, "npc"), y = unit(0.5, "npc"),
               pch = 22, size = unit(11, "pt"), gp = key_gp),
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
    scale_shape_manual(values = c("Other" = 21, "Whiff" = 22, "Hard Hit" = 23),
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
          fileInput("pc_incsv", "Input File (CSV or Parquet)",
                    accept = c(".csv", ".parquet"), multiple = FALSE),
          actionButton("pc_update", "Load Pitchers", icon("magnifying-glass"),
                       class = "btn-primary btn-block"),
          selectizeInput("pc_pitcher", "Pitcher Name:", NULL),
          selectInput("pc_game", "Game:", choices = c("All Games" = "all")),
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
          downloadButton("pc_downloadPlot", "Download Card", class = "btn-info btn-block")
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

  game_data       <- reactiveVal()
  current_pitches <- reactiveVal(NULL)
  selected_points <- reactiveVal(NULL)
  card_page       <- reactiveVal(NULL)

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

  observeEvent(input$pc_update, {
    req(input$pc_incsv)
    df    <- read_input_file(input$pc_incsv$datapath, input$pc_incsv$name)
    pname <- pitcher_summary(df)
    game_data(pname)
    if (nrow(pname) == 0) {
      showNotification("No pitchers found in the file.", type = "warning")
    } else {
      pchoices <- pname %>%
        distinct(Pitcher, PitcherTeam) %>%
        arrange(Pitcher)
      team_labels <- vapply(pchoices$PitcherTeam,
                            function(t) team_palette(t)$label, character(1))
      # Choice label is "Name - Team"; value encodes both as "Name::Team".
      vec <- setNames(
        paste(pchoices$Pitcher, pchoices$PitcherTeam, sep = "::"),
        paste0(format_pitcher_name(pchoices$Pitcher), " - ", team_labels)
      )
      updateSelectizeInput(session, "pc_pitcher", "Pitcher Name:", choices = vec)
    }
  })

  load_pitcher_pitches <- function() {
    req(game_data(), input$pc_pitcher)
    sel <- parse_pitcher_value(input$pc_pitcher)
    if (is.na(sel$name)) { current_pitches(NULL); return() }
    pp <- game_data() %>% filter(Pitcher == sel$name, PitcherTeam == sel$team)

    g_sel <- input$pc_game
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

  # Game dropdown follows the selected pitcher.
  observeEvent(input$pc_pitcher, {
    req(game_data(), input$pc_pitcher)
    sel <- parse_pitcher_value(input$pc_pitcher)
    if (is.na(sel$name)) {
      updateSelectInput(session, "pc_game", choices = c("All Games" = "all"), selected = "all")
      return()
    }
    pp_all <- game_data() %>% filter(Pitcher == sel$name, PitcherTeam == sel$team)
    if ("GameUID" %in% names(pp_all) && nrow(pp_all) > 0) {
      have_date <- "Date" %in% names(pp_all)
      have_opp  <- "BatterTeam" %in% names(pp_all)
      keep_cols <- c("GameUID", if (have_date) "Date", if (have_opp) "BatterTeam")
      g_df <- pp_all %>% distinct(across(all_of(keep_cols)))
      if (have_date) g_df <- g_df %>% arrange(Date)
      labels <- vapply(seq_len(nrow(g_df)), function(i) {
        d <- if (have_date && !is.na(g_df$Date[i])) format_pretty_date(g_df$Date[i]) else "?"
        o <- if (have_opp && !is.na(g_df$BatterTeam[i])) team_palette(g_df$BatterTeam[i])$label else "?"
        paste0(d, "  ·  vs ", o)
      }, character(1))
      choices <- c("All Games" = "all", setNames(as.character(g_df$GameUID), labels))
    } else {
      choices <- c("All Games" = "all")
    }
    updateSelectInput(session, "pc_game", choices = choices, selected = "all")
  }, ignoreInit = TRUE)

  observeEvent(list(input$pc_pitcher, input$pc_game), { load_pitcher_pitches() },
               ignoreInit = TRUE)
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
    )
  })

  observe({
    ed <- tryCatch(plotly::event_data("plotly_selected", source = "pc_retagplot"),
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
    pitcher_display <- format_pitcher_name(parse_pitcher_value(input$pc_pitcher)$name)
    page <- build_pitcher_card_page(game, pitcher_display,
                                    height_override = height_override_dec(),
                                    set_override    = set_override_val())
    card_page(page)
  })

  output$pc_combinedPlot <- renderImage({
    req(card_page())
    outfile <- tempfile(fileext = ".png")
    draw_card_to_png(card_page(), outfile,
                     width = 1200, height = 1200, units = "px", res = 96, dpi = 96)
    list(src = outfile, contentType = "image/png",
         width = "100%", height = "auto", alt = "Pitching Summary Card")
  }, deleteFile = TRUE)

  output$pc_downloadPlot <- downloadHandler(
    filename = function() {
      paste0("Whitecaps_Pitcher_",
             gsub("[^A-Za-z0-9]+", "_",
                  if (is.null(input$pc_pitcher)) "card"
                  else parse_pitcher_value(input$pc_pitcher)$name),
             ".png")
    },
    content = function(file) {
      req(card_page())
      draw_card_to_png(card_page(), file,
                       width = 12.5, height = 12.5, units = "in", res = 300, dpi = 300)
    }
  )
}


standings <- tryCatch(fetch_standings(), error = function(e) NULL)

# -- Roster --
roster_catchers <- data.frame(
  Name = c("Owen Jenkins","Jacob Lee","Jimmy Janicki"),
  Pos  = c("C","C","C/3B"),
  `B/T`= c("R/R","R/R","R/R"),
  Ht   = c("6'2","6'2","6'3"),
  Wt   = c(215,205,224),
  College = c("Kentucky","VCU","Troy"),
  check.names = FALSE
)
roster_infielders <- data.frame(
  Name = c("Dalton Wentz","Brendan Lawson","Pete Daniel","Nicholas Partida",
           "Will Moore","Dane Harvey","Petey Craska","Jamie Laskofski",
           "Landon Penfield","Jacob Lambdin","Jett Kenady","Alexander Peck"),
  Pos  = c("MINF","SS/3B","SS","SS","INF","1B","1B","SS/3B/2B","3B","SS","SS","SS"),
  `B/T`= c("S/R","L/R","R/R","R/R","L/R","L/R","L/R","L/R","L/R","R/R","R/R","R/R"),
  Ht   = c("6'2","6'3","6'3","6'0","5'9","6'5","6'0","6'3","6'1","5'10","6'2","6'4"),
  Wt   = c(215,215,180,180,170,270,250,185,210,185,185,205),
  College = c("Wake Forest","Florida","VT","Texas A&M","Indiana","Ohio State",
              "North Alabama","William & Mary","Charleston","Duke","Cal","Arkansas"),
  check.names = FALSE
)
roster_outfielders <- data.frame(
  Name = c("Adam Magpoc","Brody DeLamielleure","Michael Torres","Frank Carney",
           "Terrence Kiel II","Jay Abernathy","Blaine Brown","Cash Strayer","Eric Hines"),
  Pos  = c("2B/OF","COF","OF","OF","OF","OF/2B","OF/LHP","OF","OF"),
  `B/T`= c("S/R","R/R","L/L","L/R","R/R","L/R","L/L","L/R","R/R"),
  Ht   = c("5'10","6'1","5'11","5'10","6'0","5'10","6'3","6'2","6'3"),
  Wt   = c(170,195,185,180,185,177,180,195,223),
  College = c("Boston College","FSU","Miami","UC Irvine","Texas A&M",
              "Tennessee","Tennessee","Florida","Alabama"),
  check.names = FALSE
)
roster_pitchers <- data.frame(
  Name = c("Charlie Willcox","Nate Harris","Logan Eisenrich","Ethan Grim","Zach Kmatz",
           "Jordan Martin","Finbar O'Brien","Landon Mack","Joshua Whritenour",
           "Schuyler Sandford","Jordan Regulski","Carter Williams","Maverick Rizy",
           "Frank Menendez","Tommy Conley","Sebastian Santos-Olsen","Trent Collier",
           "Charlie West","Nate Smithburg","Tye Briscoe"),
  Pos  = c(rep("RHP",13),"LHP","LHP","LHP","LHP","LHP","LHP","LHP"),
  Ht   = c("6'3","6'4","6'4","6'0","6'3","6'5","6'3","6'1","6'2","6'6","6'3","6'2","6'9",
           "6'1","6'2","6'3","6'2","5'11","6'2","6'0"),
  Wt   = c(210,225,200,190,215,215,205,200,190,210,175,210,250,
           215,205,215,241,182,257,205),
  College = c("Georgia Tech","Kentucky","VT","VT","Oregon State","Arkansas","Gonzaga",
              "Tennessee","Florida","Florida","Duke","Midland JUCO","LSU",
              "Miami","St Johns","Miami","Oklahoma","UConn","Oklahoma","Arkansas"),
  check.names = FALSE
)

# ==========================================
# SHARED HELPERS
# ==========================================
format_name <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) paste(trimws(parts[2]), trimws(parts[1])) else name
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
prep_catcher_data <- function(raw, team) {
  raw <- raw %>%
    filter(CatcherTeam == team) %>%
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

    draw_grid_table(g_steal,
                    title = "Game Throwing - Steal Attempts",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.88, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(g_framing,
                    title = "Game Framing - Strike Log",
                    y_top = 0.77, x_center = 0.5, row_h = 0.020,
                    table_width = 0.75, header_cex = 0.68, cell_cex = 0.68, title_cex = 0.90)

    grid.text("Game Framing - Strike Locations", x = 0.5, y = 0.46,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
    print(g_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
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

    draw_grid_table(s_steal,
                    title = "Season Throwing - Overall",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_steal_pitch,
                    title = "Season Throwing - By Pitch Type",
                    y_top = 0.78, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_framing,
                    title = "Season Framing - Strike Summary",
                    y_top = 0.63, x_center = 0.5, row_h = 0.022,
                    table_width = 0.40, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    grid.text("Season Framing - Strike Locations", x = 0.5, y = 0.54,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
    print(s_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
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
# HITTER - CONSTANTS & HELPERS
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
    pt %in% c("Fastball", "FourSeamFastBall", "Four-Seam") ~ "Four Seam",
    pt %in% c("Sinker", "TwoSeamFastBall")                 ~ "Sinker",
    pt == "Slider"    ~ "Slider",
    pt == "Curveball" ~ "Curveball",
    pt %in% c("ChangeUp", "Changeup") ~ "Changeup",
    pt == "Cutter"    ~ "Cutter",
    pt == "Splitter"  ~ "Splitter",
    TRUE ~ NA_character_
  )
}

build_color_matrix_hitter <- function(df, benchmarks, lower_is_better = c()) {
  get_color <- function(value, bottom, top, flip = FALSE) {
    value <- suppressWarnings(as.numeric(gsub("%", "", value)))
    if (is.na(value)) return("white")
    normalized <- (value - bottom) / (top - bottom)
    normalized <- pmax(0, pmin(1, normalized))
    if (flip) normalized <- 1 - normalized
    colorRamp(c("#E1463E", "#CDCD00", "#00840D"))(normalized) %>%
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
  AVG        = c(0.200, 0.360), OBP  = c(0.290, 0.450), SLG  = c(0.339, 0.667),
  `K%`       = c(9.8,  26.5),  `BB%`= c(6.3,  18.8),
  `Whiff%`   = c(12.8, 30.7),  `HardHit%` = c(20, 55)
)
hitter_lower_is_better <- c("K%", "Whiff%")

hitter_fastball_split_benchmarks <- list(
  `Swing%` = c(37.2,52.2), `Contact%` = c(72,90), `Chase%` = c(14,29),
  `Barrel%`= c(6,30),      `IZ Whiff%`= c(6,23),  `HardHit%`= c(18,56), `Avg EV`= c(80,92))
hitter_breaker_split_benchmarks <- list(
  `Swing%` = c(30,48),     `Contact%` = c(56,82), `Chase%` = c(15,35),
  `Barrel%`= c(0,30),      `IZ Whiff%`= c(8,29),  `HardHit%`= c(9,50),  `Avg EV`= c(77,90))
hitter_offspeed_split_benchmarks <- list(
  `Swing%` = c(36,54),     `Contact%` = c(56,81), `Chase%` = c(20,39),
  `Barrel%`= c(4,30),      `IZ Whiff%`= c(10,34), `HardHit%`= c(14,54), `Avg EV`= c(79,91))
hitter_split_lower_is_better <- c("Chase%", "IZ Whiff%")
hitter_split_bench_map <- list(
  "Fastball" = hitter_fastball_split_benchmarks,
  "Breaker"  = hitter_breaker_split_benchmarks,
  "Offspeed" = hitter_offspeed_split_benchmarks)

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
# HITTER - SWING DECISION MODELS
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

message("HITTER_TOKEN present: ", nchar(Sys.getenv("HITTER_TOKEN")) > 0)
sd_models <- tryCatch({
  token <- Sys.getenv("HITTER_TOKEN")
  repo  <- "BrewsterWhitecapsMAC/swing-decision-models"
  message("Downloading swing decision models from HF dataset: ", repo)
  list(
    model_take  = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Take.ubj",  token)),
    model_swing = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Swing.ubj", token)),
    encodings   = readRDS(download_from_hf_dataset(repo,  "encodings.rds",        token))
  )
}, error = function(e) { message("Swing decision models not loaded: ", e$message); NULL })
message("sd_models loaded: ", !is.null(sd_models))

sd_features <- c("PlateLocHeight", "PlateLocSide", "count_state_enc", "pitch_type_enc")

brewster_roster <- c(
  "French, Anderson", "Jenkins, Owen", "Lee, Jacob",
  "Wentz, Dalton", "Lawson, Brendan", "Daniel, Pete", "Partida, Nicholas",
  "Moore, Will", "Craska, Petey", "Laskofski, Jamie", "Penfield, Landon",
  "Rhine, Will", "Lambdin, Jake",
  "Magpoc, Adam", "DeLamielleure, Brody", "Torres, Michael", "Carney, Frankie",
  "Daniel, Conlan", "Kiel II, Terrence", "Abernathy, Jay", "Brown, Blaine",
  "Strayer, Cash"
)

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
      Balls       = as.integer(Balls),
      Strikes     = as.integer(Strikes),
      count_state = paste0(Balls, "-", Strikes),
      count_state = ifelse(!count_state %in% c("0-0","0-1","0-2","1-0","1-1","1-2",
                                               "2-0","2-1","2-2","3-0","3-1","3-2"),
                           NA, count_state),
      pitch_type_model = recode_pitch_type_model(TaggedPitchType),
      count_state_enc  = match(count_state, enc$count_state),
      pitch_type_enc   = match(pitch_type_model, enc$pitch_type)
    )
  valid <- !is.na(scored$PlateLocHeight) & !is.na(scored$PlateLocSide) &
    !is.na(scored$count_state_enc) & !is.na(scored$pitch_type_enc) &
    scored$pitch_type_model != "Other"
  scored$xRV_swing <- NA_real_; scored$xRV_take <- NA_real_
  if (any(valid)) {
    dmat <- xgb.DMatrix(as.matrix(scored[valid, sd_features]))
    scored$xRV_swing[valid] <- predict(models$model_swing, dmat)
    scored$xRV_take[valid]  <- predict(models$model_take,  dmat)
  }
  scored %>% mutate(xRV_diff = xRV_swing - xRV_take) %>%
    select(-pitch_type_model, -count_state_enc, -pitch_type_enc)
}

plot_xrv_diff_heatmap <- function(count_label=NULL, pitch_label="FF", title_prefix="", models=sd_models) {
  if (is.null(models)) return(ggplot() + theme_void())
  enc <- models$encodings
  grid_df <- expand.grid(PlateLocHeight=seq(0.75,4.25,by=0.20), PlateLocSide=seq(-2.0,2.0,by=0.20))
  grid_df$count_state_enc <- if (!is.null(count_label)) match(count_label, enc$count_state) else 1L
  grid_df$pitch_type_enc  <- match(pitch_label, enc$pitch_type)
  if (any(is.na(grid_df$count_state_enc)) || any(is.na(grid_df$pitch_type_enc)))
    return(ggplot() + theme_void())
  dmat <- xgb.DMatrix(as.matrix(grid_df[, sd_features]))
  grid_df$xRV_swing <- predict(models$model_swing, dmat)
  grid_df$xRV_take  <- predict(models$model_take,  dmat)
  grid_df$diff      <- grid_df$xRV_swing - grid_df$xRV_take
  subtitle <- if (!is.null(count_label)) paste0(title_prefix,"Count: ",count_label," | Pitch: ",pitch_label) else paste0(title_prefix,"Pitch: ",pitch_label)
  ggplot(grid_df, aes(x=PlateLocSide, y=PlateLocHeight, fill=diff)) +
    geom_tile() +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, name="Swing−Take\nxRV") +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide, y=PlateLocHeight), inherit.aes=FALSE, color="black", linewidth=1) +
    annotate("rect", xmin=-0.7083, xmax=0.7083, ymin=0.0, ymax=0.15, fill="grey70", color="black") +
    coord_fixed() + xlim(-2.5,2.5) + ylim(0,5) +
    labs(subtitle=subtitle, x=NULL, y=NULL) +
    theme_minimal(base_size=9) +
    theme(plot.subtitle=element_text(hjust=0.5,size=7.5), panel.grid=element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank(),
          legend.key.size=unit(0.35,"cm"), legend.text=element_text(size=6),
          legend.title=element_text(size=6.5))
}

summarise_xrv_swdec <- function(scored_df) {
  scored_df %>% filter(!is.na(xRV_diff)) %>%
    mutate(ModelShouldSwing = xRV_diff > 0,
           DidSwing = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
           GoodxDec = (ModelShouldSwing & DidSwing) | (!ModelShouldSwing & !DidSwing)) %>%
    summarise(Total=n(), GoodDec=sum(GoodxDec,na.rm=TRUE),
              SwingPitches=sum(ModelShouldSwing,na.rm=TRUE),
              ActualSwings=sum(DidSwing,na.rm=TRUE), .groups="drop") %>%
    mutate(`xSwDec%`    = paste0(round(GoodDec      /pmax(Total,1)*100,1),"%"),
           `Mdl Swing%` = paste0(round(SwingPitches /pmax(Total,1)*100,1),"%"),
           `Act Swing%` = paste0(round(ActualSwings /pmax(Total,1)*100,1),"%")) %>%
    select(`xSwDec%`, `Mdl Swing%`, `Act Swing%`)
}

make_swdec_plot <- function(df, plot_title) {
  dec_colors <- c("Good Swing"="#00840D","Good Take"="#5BBF6A","Bad Swing"="#E1463E","Bad Take"="#F4A49E")
  dec_shapes <- c("Good Swing"=17,"Good Take"=21,"Bad Swing"=25,"Bad Take"=21)
  ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="grey85", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1.2) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,shape=DecLabel),
               size=6.5, color="black", alpha=0.3) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,color=DecLabel,shape=DecLabel),
               size=5.5, alpha=0.92) +
    scale_color_manual(values=dec_colors, name=NULL,
                       guide=guide_legend(override.aes=list(size=3.5))) +
    scale_shape_manual(values=dec_shapes, name=NULL,
                       guide=guide_legend(override.aes=list(size=3.5))) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title,
         subtitle="▲ Swing  ● Take  |  Green = Good  Red = Bad",
         x=NULL, y=NULL) +
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
    mutate(
      bx = round(PlateLocSide   / bin_w) * bin_w,
      by = round(PlateLocHeight / bin_w) * bin_w
    ) %>%
    group_by(bx, by) %>%
    summarise(GoodPct = mean(SwDec == 1, na.rm=TRUE), N = n(), .groups="drop") %>%
    filter(N >= 2)

  ggplot() +
    geom_tile(data=binned, aes(x=bx, y=by, fill=GoodPct),
              width=bin_w*0.97, height=bin_w*0.97) +
    scale_fill_gradient2(low="blue", mid="white", high="red",
                         midpoint=0.70, limits=c(0,1), guide="none") +
    geom_polygon(data=hitter_home_plate, aes(x=x, y=y),
                 fill="grey85", color="black", linewidth=0.8, inherit.aes=FALSE) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide, y=PlateLocHeight),
              color="black", linewidth=1.2, inherit.aes=FALSE) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title,
         subtitle="Red = Good Decisions  |  Blue = Bad Decisions",
         x=NULL, y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color="#0C2340"),
          plot.subtitle=element_text(hjust=0.5,size=7,color="grey50"),
          axis.text=element_blank(), axis.ticks=element_blank(),
          panel.grid=element_blank(), legend.position="none")
}

# ==========================================
# HITTER - PDF GENERATION
# ==========================================
generate_hitter_pdf <- function(game_data, season_data, selected_hitter, output_file,
                                 active_models = sd_models) {

  hitter_name <- format_name(selected_hitter)

  dedup <- function(df) {
    key_cols <- intersect(c("GameID","Batter","Inning","Balls","Strikes","Outs",
                            "PitchCall","TaggedPitchType","PlateLocHeight","PlateLocSide"), names(df))
    df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  }

  game_hitter   <- game_data   %>% filter(Batter == selected_hitter) %>% dedup() %>% score_pitches_xrv(models=active_models)
  season_hitter <- season_data %>% filter(Batter == selected_hitter) %>% dedup() %>% score_pitches_xrv(models=active_models)

  logo_grob <- tryCatch({
    img <- magick::image_read("www/logo1.png")
    img <- magick::image_resize(img, "x100")
    grid::rasterGrob(as.raster(img), interpolate=TRUE)
  }, error=function(e) grid::nullGrob())

  # -- Game Stats --
  counting_stats <- game_hitter %>%
    mutate(
      IsWhiff  = PitchCall == "StrikeSwinging",
      IsSwing  = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsChase  = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip") &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsBall   = PitchCall == "BallCalled" &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsLast   = if ("is_last_pitch_of_PA" %in% names(.)) is_last_pitch_of_PA == TRUE else
        PitchCall %in% c("InPlay","HitByPitch")
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
      DecLabel   = case_when(
        SwDec==1 & DidSwing  ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing  ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_),
      CountLabel = paste0(Balls,"-",Strikes)
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

  swdec_by_count <- swing_decisions %>%
    mutate(CountType=case_when(
      Balls-Strikes>=1 ~ "Hitter Ahead", Strikes-Balls>=1 ~ "Pitcher Ahead",
      Balls==Strikes   ~ "Even",         TRUE             ~ "Neutral")) %>%
    group_by(CountType) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(Count=CountType, SwDec, `SwDec%`)

  game_swdec_plot <- make_swdec_plot(swing_decisions, "Game Swing Decisions by Location")

  game_xrv_overall <- if (!is.null(active_models) && any(!is.na(game_hitter$xRV_diff))) {
    summarise_xrv_swdec(game_hitter) %>% mutate(` `="Overall") %>% select(` `, everything())
  } else NULL

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
      IsChase     = IsSwing & !InZone, IsOutZone   = !InZone,
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
             ExitSpeed>=95                                ~ "Hard Hit",
             PitchCall=="StrikeSwinging"                 ~ "Whiff",
             PitchCall %in% c("StrikeCalled","BallCalled") ~ "Take",
             PitchCall=="InPlay"                         ~ "In Play",
             TRUE ~ "Take")) %>%
    filter(!is.na(TaggedPitchType_clean))

  zone_plot <- ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="white", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1) +
    geom_point(data=zone_hitter,
               aes(x=PlateLocSide,y=PlateLocHeight,fill=TaggedPitchType_clean,shape=ContactType),
               size=3, alpha=0.90, color="black", stroke=0.8) +
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

  # -- Season Stats --
  season_stats <- season_hitter %>%
    mutate(
      IsSwing    = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsWhiff    = PitchCall=="StrikeSwinging",
      IsHardHit  = PitchCall=="InPlay" & !is.na(ExitSpeed) & ExitSpeed>=95,
      IsBIP      = PitchCall=="InPlay" & !is.na(ExitSpeed),
      IsHit      = PlayResult %in% c("Single","Double","Triple","HomeRun"),
      Is2B=PlayResult=="Double", Is3B=PlayResult=="Triple", IsHR=PlayResult=="HomeRun",
      IsK=KorBB=="Strikeout", IsBB=KorBB=="Walk", IsHBP=PitchCall=="HitByPitch",
      IsLastPitch = if ("is_last_pitch_of_PA" %in% names(.)) is_last_pitch_of_PA==TRUE else
        (KorBB %in% c("Strikeout","Walk") | PitchCall %in% c("InPlay","HitByPitch")),
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

  # -- Season Swing Decisions --
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
        SwDec==1 & DidSwing  ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing  ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_),
      CountLabel=paste0(Balls,"-",Strikes)
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

  season_swdec_by_count <- season_swing_decisions %>%
    mutate(CountType=case_when(
      Balls-Strikes>=1 ~ "Hitter Ahead", Strikes-Balls>=1 ~ "Pitcher Ahead",
      Balls==Strikes ~ "Even", TRUE ~ "Neutral")) %>%
    group_by(CountType) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(Count=CountType, SwDec, `SwDec%`)

  season_swdec_plot <- make_swdec_heatmap(season_swing_decisions, "Season Swing Decisions by Location")

  season_xrv_overall <- if (!is.null(active_models) && any(!is.na(season_hitter$xRV_diff))) {
    summarise_xrv_swdec(season_hitter) %>% mutate(` `="Overall") %>% select(` `, everything())
  } else NULL

  season_xrv_by_pitch <- if (!is.null(active_models) && any(!is.na(season_hitter$xRV_diff))) {
    season_hitter %>%
      mutate(PitchTypeGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchTypeGroup)) %>%
      group_by(`Pitch Type`=PitchTypeGroup) %>%
      group_modify(~summarise_xrv_swdec(.x)) %>% ungroup()
  } else NULL

  # -- RHP/LHP Stats --
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

  # -- Density Plots --
  make_density_plots_hitter <- function(data, hand) {
    split_data <- data %>%
      filter(Batter==selected_hitter, PitcherThrows==hand) %>%
      mutate(PitchGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaker",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchGroup), !is.na(PlateLocSide), !is.na(PlateLocHeight))
    group_levels <- c("Fastball","Breaker","Offspeed")
    plots <- lapply(group_levels, function(grp) {
      grp_data   <- split_data %>% filter(PitchGroup==grp)
      whiff_data <- grp_data  %>% filter(PitchCall=="StrikeSwinging")
      hh_data    <- grp_data  %>% filter(PitchCall=="InPlay",!is.na(ExitSpeed),ExitSpeed>=95)
      if (nrow(grp_data)<1)
        return(ggplot()+theme_void()+labs(title=grp)+
                 theme(plot.title=element_text(hjust=0.5,size=13,face="bold")))
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
    })
    setNames(plots, group_levels)
  }

  density_rhp <- make_density_plots_hitter(season_data, "Right")
  density_lhp <- make_density_plots_hitter(season_data, "Left")

  # -- Draw PDF --
  pdf(output_file, width=11, height=15)
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

    # PAGE 1 - GAME
    grid.newpage()
    page_header_hitter(hitter_name, "Postgame Hitter Report")

    grid.text("Game Stats", x=0.5,y=0.89,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(counting_stats, y_top=0.865, x_center=0.5, row_h=0.018, cell_cex=0.90)

    pushViewport(viewport(x=0.5,y=0.82,width=0.96,height=0.30,just=c("center","top")))
    print(zone_plot, newpage=FALSE); popViewport()

    draw_grid_table(stats_by_pitch, title="Stats by Pitch Type",
                    y_top=0.490, x_center=0.5, row_h=0.026, title_cex=0.90, header_cex=0.80, cell_cex=0.80)

    grid.lines(x=c(0.03,0.97), y=c(0.300,0.300), gp=gpar(col="#9DC2EA", lwd=1))
    grid.text("Swing Decisions", x=0.5, y=0.290,
              gp=gpar(fontface="bold", cex=0.90, col="#0C2340"))

    grid.text("Game", x=0.25, y=0.290,
              gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
    pushViewport(viewport(x=0.12, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(game_swdec_plot +
            theme(plot.title=element_blank(), plot.subtitle=element_blank(),
                  legend.position="none"),
          newpage=FALSE); popViewport()

    draw_grid_table(overall_swdec,  title="Overall",
                    y_top=0.260, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    draw_grid_table(swdec_by_pitch, title="By Pitch",
                    y_top=0.180, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    if (!is.null(active_models) && !is.null(game_xrv_overall))
      draw_grid_table(game_xrv_overall, title="xRV",
                      y_top=0.075, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)

    grid.text("Season", x=0.75, y=0.290,
              gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
    pushViewport(viewport(x=0.62, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(season_swdec_plot +
            theme(plot.title=element_blank(), plot.subtitle=element_blank()),
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

    # PAGE 2 - SEASON
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

  }, error=function(e) message("PDF error: ", e$message))
}

# ==========================================
# HUB UI
# ==========================================
apps <- list(
  list(id = "catcher",          title = "Catcher Reports",          page = "catcher",        status = "live"),
  list(id = "hitter",           title = "Postgame Hitter Reports",  page = "hitter",         status = "live"),
  list(id = "pitcher",          title = "Postgame Pitcher Reports", page = "pitcher",        status = "live"),
  list(id = "pitcher_scouting", title = "Pitcher Scouting",         page = "scout_pitching", status = "live"),
  list(id = "hitter_scouting",  title = "Hitter Scouting",          page = "scout_hitting",  status = "live"),
  list(id = "acquisitions",     title = "Acquisitions",             page = "scout_acq",      status = "live"),
  list(id = "player_grades",    title = "Player Grades",            page = "scout_grades",   status = "live"),
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
          tags$h4("Postgame Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("team_select", "Select Team:", choices = NULL),
          selectInput("game_date_select", "Select Game Date:", choices = NULL),
          selectInput("catcher_name", "Select Catcher:", choices = NULL),
          actionButton("generate_catcher", "Generate Report", class = "btn btn-primary w-100")
        ),
        tags$div(
          tags$h4("Season Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("season_csvs", "Upload Season CSVs:", accept = ".csv", multiple = TRUE,
                    buttonLabel = "Browse", placeholder = "No files selected"),
          selectInput("season_team_select", "Select Team:", choices = NULL)
        )
      ),
      uiOutput("catcher_status"),
      br(),
      uiOutput("catcher_download_ui")
    ),
    tags$div(class = "hub-footer",
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

hitter_ui <- function() {
  tagList(
    tags$div(class="hub-main",
      tags$div(style="margin-bottom: 24px;",
        tags$button("< Back to Hub", onclick="Shiny.setInputValue('nav_to','hub',{priority:'event'})", class="btn btn-outline-secondary btn-sm")),
      tags$h2("Hitter Report Generator", style="font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(style="display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          tags$h4("Game CSV", style="color: var(--navy); margin-bottom: 12px;"),
          fileInput("hitter_game_csv", "Upload Game CSV:", accept=".csv", buttonLabel="Browse", placeholder="No file selected"),
          tags$h4("Season CSVs", style="color: var(--navy); margin-bottom: 12px;"),
          fileInput("hitter_season_csvs", "Upload Season CSVs:", accept=".csv", multiple=TRUE, buttonLabel="Browse", placeholder="No files selected")),
        tags$div(
          tags$h4("Select Player", style="color: var(--navy); margin-bottom: 12px;"),
          uiOutput("hitter_team_select_ui"),
          uiOutput("hitter_select_ui"))
      ),
      actionButton("generate_hitter", "Generate Report", class="btn btn-primary", style="width: 200px;"),
      br(), br(), uiOutput("hitter_status"), br(), uiOutput("hitter_download_ui")),
    tags$div(class="hub-footer", paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
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

# ==========================================
# UI
# ==========================================
ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=7"),
    tags$style(HTML("
      #caps-splash {
        position: fixed;
        inset: 0;
        background: #0C2340;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        transition: opacity 0.6s ease;
      }
      #caps-splash.fade-out {
        opacity: 0;
        pointer-events: none;
      }
      #splash-logo {
        width: 100px;
        height: 100px;
        border-radius: 50%;
        background: white;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: scaleIn 1.2s ease forwards;
        opacity: 0;
        overflow: hidden;
      }
      #splash-logo img {
        width: 90px;
        height: 90px;
        object-fit: contain;
      }
      #splash-title {
        color: white;
        font-family: 'Oswald', sans-serif;
        font-size: 28px;
        letter-spacing: 6px;
        margin-top: 20px;
        animation: fadeUp 1s ease 0.8s forwards;
        opacity: 0;
        text-transform: uppercase;
      }
      #splash-sub {
        color: #9DC2EA;
        font-size: 12px;
        letter-spacing: 2px;
        margin-top: 8px;
        animation: fadeUp 1s ease 1.2s forwards;
        opacity: 0;
        text-align: center;
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
      @media (max-width: 1024px) {
        .hub-main {
          padding: 0 16px;
        }
        .app-grid {
          grid-template-columns: repeat(2, 1fr) !important;
        }
        div[style*='grid-template-columns: 1fr 1fr'] {
          grid-template-columns: 1fr !important;
        }
        div[style*='grid-template-columns: 1fr 1fr 1fr'] {
          grid-template-columns: 1fr 1fr !important;
        }
        .btn {
          min-height: 44px !important;
          font-size: 15px !important;
        }
        input, select {
          min-height: 44px !important;
          font-size: 15px !important;
        }
        .form-control {
          font-size: 15px !important;
        }
        .standings-wrapper {
          overflow-x: auto;
        }
        .hub-header {
          padding: 14px 16px !important;
        }
        .hub-header h1 {
          font-size: 20px !important;
        }
        .team-logo {
          height: 42px !important;
        }
        .card-title {
          font-size: 13px !important;
        }
      }
      @media (max-width: 768px) {
        .app-grid {
          grid-template-columns: repeat(2, 1fr) !important;
        }
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        setTimeout(function() {
          $('#caps-splash').addClass('fade-out');
          setTimeout(function() {
            $('#caps-splash').remove();
          }, 700);
        }, 2800);
      });
    "))
  ),

  tags$div(
    id = "caps-splash",
    tags$div(
      id = "splash-logo",
      tags$img(src = "logo1.png")
    ),
    tags$div(id = "splash-title", "C.A.P.S."),
    tags$div(id = "splash-sub", "Centralized Application Platform for Staff")
  ),

  uiOutput("hub_header_ui"),

  uiOutput("page_content")
)
# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  current_page <- reactiveVal("hub")

  observeEvent(input$nav_to, {
    current_page(input$nav_to)
  })

  scout_server(input, output, session)
  whitecaps_bind_parent_server(current_page, input, output, session)
  pitcher_card_server(input, output, session)

  output$hub_header_ui <- renderUI({
    if (whitecaps_is_page(current_page())) {
      return(NULL)
    }

    tags$div(
      class = "hub-header",
      tags$div(
        class = "header-text",
        tags$h1("Brewster Whitecaps"),
        tags$p("C.A.P.S. - Centralized Application Platform for Staff")
      ),
      tags$img(src = "logo1.png", class = "team-logo")
    )
  })

  output$page_content <- renderUI({
    if      (current_page() == "hub")            hub_ui()
    else if (current_page() == "catcher")        catcher_ui()
    else if (current_page() == "hitter")         hitter_ui()
    else if (current_page() == "pitcher")        pitcher_card_ui()
    else if (current_page() == "scout_pitching") scout_pitching_ui()
    else if (current_page() == "scout_hitting")  scout_hitting_ui()
    else if (current_page() == "scout_acq")      scout_acq_ui()
    else if (current_page() == "scout_grades")   scout_grades_ui()
    else if (whitecaps_is_page(current_page()))  whitecaps_embedded_ui()
  })

output$roster_catchers    <- renderTable({ roster_catchers },    striped = TRUE, hover = TRUE, width = "100%", digits = 0)
output$roster_infielders  <- renderTable({ roster_infielders },  striped = TRUE, hover = TRUE, width = "100%", digits = 0)
output$roster_outfielders <- renderTable({ roster_outfielders }, striped = TRUE, hover = TRUE, width = "100%", digits = 0)
output$roster_pitchers    <- renderTable({ roster_pitchers },    striped = TRUE, hover = TRUE, width = "100%", digits = 0)
  # ==========================================
  # CATCHER SERVER LOGIC
  # ==========================================
  raw_game <- reactive({
    req(input$game_csv)
    read_csv(input$game_csv$datapath, show_col_types = FALSE)
  })

  observe({
    req(raw_game())
    teams <- sort(unique(raw_game()$CatcherTeam))
    updateSelectInput(session, "team_select", choices = teams)
  })

  game_data_catcher <- reactive({
    req(raw_game(), input$team_select)
    prep_catcher_data(raw_game(), input$team_select)
  })

  observe({
    req(game_data_catcher())
    dates <- sort(unique(as.Date(game_data_catcher()$framing$Date)), decreasing = TRUE)
    updateSelectInput(session, "game_date_select", choices = as.character(dates))
  })

  observe({
    req(game_data_catcher())
    catchers <- sort(unique(game_data_catcher()$framing$Catcher))
    updateSelectInput(session, "catcher_name", choices = catchers)
  })

  raw_season_catcher <- reactive({
    req(input$season_csvs)
    bind_rows(lapply(input$season_csvs$datapath, function(f) {
      read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
    })) %>% type.convert(as.is = TRUE)
  })

  observe({
    req(raw_season_catcher())
    teams <- sort(unique(raw_season_catcher()$CatcherTeam))
    updateSelectInput(session, "season_team_select", choices = teams)
  })

  season_data_catcher <- reactive({
    req(raw_season_catcher(), input$season_team_select)
    prep_catcher_data(raw_season_catcher(), input$season_team_select)
  })

  catcher_pdf_path <- reactiveVal(NULL)

  observeEvent(input$generate_catcher, {
    req(input$catcher_name, game_data_catcher(), season_data_catcher(), input$game_date_select)
    output$catcher_status <- renderUI({
      div(style = "color: orange; font-weight: bold;", "Generating report...")
    })
    tryCatch({
      tmp_pdf   <- tempfile(fileext = ".pdf")
      game_date <- as.Date(input$game_date_select)
      generate_catcher_pdf(
        game_framing    = game_data_catcher()$framing  %>% mutate(Date = as.Date(as.character(Date))),
        game_throwing   = game_data_catcher()$throwing %>% mutate(Date = as.Date(as.character(Date))),
        season_framing  = season_data_catcher()$framing  %>% mutate(Date = as.Date(as.character(Date))),
        season_throwing = season_data_catcher()$throwing %>% mutate(Date = as.Date(as.character(Date))),
        catcher     = input$catcher_name,
        game_date   = game_date,
        output_file = tmp_pdf,
        logo_path   = "www/logo1.png"
      )
      catcher_pdf_path(tmp_pdf)
      output$catcher_status <- renderUI({
        div(style = "color: green; font-weight: bold;", "✓ Report ready!")
      })
    }, error = function(e) {
      output$catcher_status <- renderUI({
        div(style = "color: red;", paste("Error:", e$message))
      })
    })
  })

  output$catcher_download_ui <- renderUI({
    req(catcher_pdf_path())
    downloadButton("download_catcher_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_catcher_pdf <- downloadHandler(
    filename = function() paste0(gsub(", ", "_", input$catcher_name), "_CatcherReport.pdf"),
    content  = function(file) { req(catcher_pdf_path()); file.copy(catcher_pdf_path(), file, overwrite = TRUE) }
  )

  # ==========================================
  # HITTER SERVER LOGIC
  # ==========================================
  raw_hitter_game <- reactive({
    req(input$hitter_game_csv)
    read.csv(input$hitter_game_csv$datapath, stringsAsFactors = FALSE)
  })

  raw_hitter_season <- reactive({
    req(input$hitter_season_csvs)
    lapply(input$hitter_season_csvs$datapath, function(f) {
      read.csv(f, stringsAsFactors = FALSE, colClasses = "character") %>%
        select(-any_of("GameForeignID"))
    }) %>% bind_rows() %>% type.convert(as.is = TRUE)
  })

  output$hitter_team_select_ui <- renderUI({
    req(input$hitter_game_csv, input$hitter_season_csvs)
    teams <- tryCatch(
      sort(unique(c(raw_hitter_game()$BatterTeam, raw_hitter_season()$BatterTeam))),
      error = function(e) character(0)
    )
    req(length(teams) > 0)
    selectInput("hitter_team_select", "Select Team:", choices = teams)
  })

  output$hitter_select_ui <- renderUI({
    req(input$hitter_game_csv, input$hitter_season_csvs, input$hitter_team_select)
    game_h <- tryCatch(
      raw_hitter_game() %>%
        filter(BatterTeam == input$hitter_team_select) %>%
        pull(Batter) %>% unique(),
      error = function(e) character(0)
    )
    season_h <- tryCatch(
      raw_hitter_season() %>%
        filter(BatterTeam == input$hitter_team_select) %>%
        pull(Batter) %>% unique(),
      error = function(e) character(0)
    )
    hitters <- sort(unique(c(game_h, season_h)))
    req(length(hitters) > 0)
    selectInput("selected_hitter", "Select Hitter:", choices = hitters)
  })

  hitter_pdf_path <- reactiveVal(NULL)

  observeEvent(input$generate_hitter, {
    req(input$selected_hitter, input$hitter_team_select, raw_hitter_game(), raw_hitter_season())
    output$hitter_status <- renderUI({
      div(style = "color: orange; font-weight: bold;", "Generating report...")
    })
    tryCatch({
      tmp_pdf <- tempfile(fileext = ".pdf")
      generate_hitter_pdf(
        game_data       = raw_hitter_game() %>% filter(BatterTeam == input$hitter_team_select),
        season_data     = raw_hitter_season() %>% filter(BatterTeam == input$hitter_team_select),
        selected_hitter = input$selected_hitter,
        output_file     = tmp_pdf,
        active_models   = sd_models
      )
      hitter_pdf_path(tmp_pdf)
      output$hitter_status <- renderUI({
        div(style = "color: green; font-weight: bold;", "✓ Report ready!")
      })
    }, error = function(e) {
      message("hitter ERROR: ", e$message)
      output$hitter_status <- renderUI({
        div(style = "color: red;", paste("Error:", e$message))
      })
    })
  })

  output$hitter_download_ui <- renderUI({
    req(hitter_pdf_path())
    downloadButton("download_hitter_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_hitter_pdf <- downloadHandler(
    filename = function() paste0(gsub(", ", "_", input$selected_hitter), "_HitterReport.pdf"),
    content  = function(file) { req(hitter_pdf_path()); file.copy(hitter_pdf_path(), file, overwrite = TRUE) }
  )

  # ==========================================
  # PITCHER SERVER LOGIC -> BrewSummaryCard card
  # ==========================================
  # (Old PDF-based pitcher server logic removed; the card tab is bound above via
  #  pitcher_card_server(input, output, session).)
}

shinyApp(ui = ui, server = server)
