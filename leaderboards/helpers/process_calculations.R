# =========================================================
#  CALCULATE PITCHING PROCESS STATS — FINAL (CatsDen v2, Count-Aware)
# =========================================================
library(dplyr)

calculate_pitching_process_stats <- function(df) {
  # ---- Handle empty dataset ----
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Pitcher = character(),
      Pitches = numeric(),
      `Strike%` = numeric(),
      `Zone%` = numeric(),
      `FirstPitchStrike%` = numeric(),
      `EarlyAhead%` = numeric()
    ))
  }
  
  # ---- Define what counts as a strike ----
  # Only count strikes that actually change the count
  # (called or swinging strikes; exclude 2-strike fouls)
  strike_calls <- c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")

  required_pa_cols <- c("GameID", "Inning", "Top/Bottom", "PAofInning")
  for (col in required_pa_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }

  if (!"PAofGame" %in% names(df)) {
    df <- df %>%
      mutate(PAofGame = paste(GameID, Inning, `Top/Bottom`, PAofInning, sep = "_"))
  }
  
  # ---- Total pitches per pitcher ----
  total_pitches <- df %>%
    count(Pitcher, name = "Pitches")
  
  # ---- Strike % ----
  strike_df <- df %>%
    filter(PitchCall %in% strike_calls) %>%
    count(Pitcher, name = "Strikes") %>%
    right_join(total_pitches, by = "Pitcher") %>%
    mutate(
      Strikes = tidyr::replace_na(Strikes, 0),
      `Strike%` = round(Strikes / Pitches * 100, 1)
    ) %>%
    select(Pitcher, `Strike%`)
  
  # ---- Zone % (corrected and stable) ----
  zone_df <- df %>%
    mutate(
      PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
      PlateLocSide   = suppressWarnings(as.numeric(PlateLocSide)),
      PlateLocHeight_in = ifelse(PlateLocHeight < 10, PlateLocHeight * 12, PlateLocHeight),
      PlateLocSide_in   = ifelse(abs(PlateLocSide) < 5, PlateLocSide * 12, PlateLocSide),
      valid_loc = !is.na(PlateLocHeight_in) & !is.na(PlateLocSide_in),
      InZoneFlag = valid_loc &
        PlateLocHeight_in >= 18.29 & PlateLocHeight_in <= 44.08 &
        PlateLocSide_in >= -9.97 & PlateLocSide_in <= 9.97
    ) %>%
    filter(valid_loc) %>%  # Only pitches with real location data
    group_by(Pitcher) %>%
    summarise(
      LocPitches = n(),
      ZonePitches = sum(InZoneFlag, na.rm = TRUE),
      `Zone%` = round(ZonePitches / LocPitches * 100, 1),
      .groups = "drop"
    )
  
  # ---- First Pitch Strike % ----
  fps_df <- df %>%
    mutate(PitchofPA = suppressWarnings(as.numeric(PitchofPA))) %>%
    filter(PitchofPA == 1) %>%
    group_by(Pitcher) %>%
    summarise(
      PA = n(),
      FirstPitchStrikes = sum(PitchCall %in% strike_calls, na.rm = TRUE),
      `FirstPitchStrike%` = round(FirstPitchStrikes / PA * 100, 1),
      .groups = "drop"
    ) %>%
    select(Pitcher, `FirstPitchStrike%`)
  
  # ---- Early & Ahead % ----
  early_ahead_df <- df %>%
    group_split(PAofGame) %>%
    lapply(function(pa) {
      pitcher <- pa$Pitcher[1]
      # Keep pitch ordering numeric to match Python parity (prevents "10" sorting before "2").
      pa$PitchofPA_num <- suppressWarnings(as.numeric(pa$PitchofPA))
      pa <- pa[order(pa$PitchofPA_num, na.last = TRUE), ]
      
      # at least 2 strikes in first 3 pitches
      first_three <- head(pa$PitchCall, 3)
      cond_a <- sum(first_three %in% strike_calls, na.rm = TRUE) >= 2
      
      # ended in <=3 pitches and not a barrel or HBP
      cond_b <- nrow(pa) <= 3
      if ("PlayResult" %in% names(pa)) {
        last_result <- tail(pa$PlayResult, 1)
        last_pitch_call <- tail(pa$PitchCall, 1)
        if (last_result %in% "Barrel" || last_pitch_call %in% "HitByPitch") {
          cond_b <- FALSE
        }
      }
      
      success <- cond_a || cond_b
      data.frame(Pitcher = pitcher, EA = success)
    }) %>%
    bind_rows() %>%
    group_by(Pitcher) %>%
    summarise(`EarlyAhead%` = round(mean(EA, na.rm = TRUE) * 100, 1), .groups = "drop")
  
  # ---- Merge all process stats ----
  stats <- total_pitches %>%
    left_join(strike_df, by = "Pitcher") %>%
    left_join(zone_df, by = "Pitcher") %>%
    left_join(fps_df, by = "Pitcher") %>%
    left_join(early_ahead_df, by = "Pitcher") %>%
    mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0)))
  
  # ---- Final clean output ----
  stats <- stats %>%
    select(Pitcher, Pitches, `Strike%`, `Zone%`, `FirstPitchStrike%`, `EarlyAhead%`) %>%
    arrange(desc(`Strike%`))
  
  message("Pitching process stats calculated for ", nrow(stats), " pitchers.")
  return(stats)
}
