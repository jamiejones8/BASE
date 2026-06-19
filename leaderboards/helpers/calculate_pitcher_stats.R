# =========================================================
#  CALCULATE PITCHING STATS — FINAL (TXST CatsDen v3, Hardened)
#  Fixes barrel filtering for numeric coercion / NA issues
# =========================================================
library(dplyr)
library(tidyr)

calculate_pitching_stats <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Pitcher = character(),
      Pitches = numeric(),
      PA = numeric(),
      `K%` = numeric(),
      `BB%` = numeric(),
      BABIP = numeric(),
      `Barrel%` = numeric(),
      MaxVelo = numeric(),
      `Whiff%` = numeric(),
      `CSW%` = numeric()
    ))
  }
  
  # ---- Ensure numeric safety for key metrics ----
  df <- df %>%
    mutate(
      ExitSpeed   = suppressWarnings(as.numeric(ExitSpeed)),
      Angle       = suppressWarnings(as.numeric(Angle)),
      RelSpeed    = suppressWarnings(as.numeric(RelSpeed)),
      PitchofPA   = suppressWarnings(as.numeric(PitchofPA))
    ) %>%
    filter(!is.na(Pitcher))
  
  # ----------------------------- #
  #          OUTCOME STATS        #
  # ----------------------------- #
  
  # --- Total pitches ---
  total_pitches <- df %>%
    count(Pitcher, name = "Pitches")
  
  # --- Plate appearances (count first pitch of each PA) ---
  pa_counts <- df %>%
    filter(!is.na(PitchofPA), PitchofPA == 1) %>%
    count(Pitcher, name = "PA")
  
  # ---- Strikeouts ----
  k_df <- df %>%
    filter(KorBB == "Strikeout") %>%
    count(Pitcher, name = "K")
  
  # ---- Walks ----
  bb_df <- df %>%
    filter(KorBB == "Walk") %>%
    count(Pitcher, name = "BB")

  # ---- Batted-ball outcomes ----
  batted_ball_df <- df %>%
    group_by(Pitcher) %>%
    summarise(
      BBE = sum(is_batted_ball_event(PitchCall), na.rm = TRUE),
      BABIPDen = sum(is_batted_ball_event(PitchCall) & PlayResult != "HomeRun", na.rm = TRUE),
      HitsInPlay = sum(is_batted_ball_event(PitchCall) & is_hit_in_play(PlayResult), na.rm = TRUE),
      Barrels = sum(is_batted_ball_event(PitchCall) & is_brewster_barrel(ExitSpeed, Angle), na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Max Velocity (max release speed per pitcher) ----
  velo_df <- df %>%
    group_by(Pitcher) %>%
    summarise(MaxVelo = max(RelSpeed, na.rm = TRUE), .groups = "drop") %>%
    mutate(MaxVelo = ifelse(is.infinite(MaxVelo), NA_real_, MaxVelo))
  
  # ---- Whiff % (misses ÷ swings) ----
  swing_labels <- pitch_swing_calls()
  swings <- df %>%
    filter(PitchCall %in% swing_labels)
  misses <- df %>%
    filter(PitchCall == "StrikeSwinging")
  
  swing_counts <- swings %>%
    count(Pitcher, name = "Swings")
  miss_counts <- misses %>%
    count(Pitcher, name = "Misses")
  
  whiff_df <- swing_counts %>%
    left_join(miss_counts, by = "Pitcher") %>%
    mutate(
      Misses = replace_na(Misses, 0),
      `Whiff%` = round(ifelse(Swings > 0, Misses / Swings * 100, 0), 1)
    ) %>%
    select(Pitcher, `Whiff%`)
  
  # ---- CSW% (Called Strikes + Whiffs per Pitch) ----
  csw_df <- df %>%
    mutate(CSWFlag = PitchCall %in% c("StrikeCalled", "StrikeSwinging")) %>%
    group_by(Pitcher) %>%
    summarise(
      CSW = sum(CSWFlag, na.rm = TRUE),
      Total = n(),
      `CSW%` = round(ifelse(Total > 0, CSW / Total * 100, 0), 1),
      .groups = "drop"
    ) %>%
    select(Pitcher, `CSW%`)
  
  # ----------------------------- #
  #         MERGE & FINALIZE      #
  # ----------------------------- #
  
  stats <- total_pitches %>%
    left_join(pa_counts, by = "Pitcher") %>%
    left_join(k_df, by = "Pitcher") %>%
    left_join(bb_df, by = "Pitcher") %>%
    left_join(batted_ball_df, by = "Pitcher") %>%
    left_join(velo_df, by = "Pitcher") %>%
    left_join(whiff_df, by = "Pitcher") %>%
    left_join(csw_df, by = "Pitcher") %>%
    mutate(across(c(Pitches, PA, K, BB, BBE, BABIPDen, HitsInPlay, Barrels), ~ replace_na(., 0))) %>%
    mutate(
      `K%`      = round(ifelse(PA > 0, K / PA * 100, 0), 1),
      `BB%`     = round(ifelse(PA > 0, BB / PA * 100, 0), 1),
      BABIP     = ifelse(BABIPDen > 0, round(HitsInPlay / BABIPDen, 3), NA_real_),
      `Barrel%` = round(ifelse(BBE > 0, Barrels / BBE * 100, 0), 1)
    ) %>%
    select(
      Pitcher, Pitches, PA,
      `K%`, `BB%`, BABIP, `Barrel%`, MaxVelo, `Whiff%`, `CSW%`
    )
  
  message("✅ Pitching outcome stats calculated for ", nrow(stats), " pitchers (includes fixed Barrel%).")
  return(stats)
}

calculate_pitcher_stats <- function(df) {
  calculate_pitching_stats(df)
}
