# =========================================================
#  TXST BASEBALL ANALYTICS — Hitter Stat Calculations (Stable & Display-Friendly, PA aligned)
#  - PAs and PA-ending outcomes both derived from the SAME pa_index built off PitchofPA == 1
#  - Output schema: Batter, PA, wOBA, BABIP, K%, BB%, Barrel%, Contact%, Z-Contact%, Z-Swing%, Chase%, EV>95%, MaxEV, P90EV
# =========================================================
library(dplyr)
library(stringr)

calculate_hitter_stats <- function(df,
                                   wBB = 0.69, wHBP = 0.72,
                                   w1B = 0.89, w2B = 1.27, w3B = 1.62, wHR = 2.10) {
  # --------- 0) Normalize column names ----------
  names(df) <- gsub("\\.", "/", names(df))
  
  # --------- 1) Ensure all expected columns exist ----------
  needed_cols <- c(
    "Batter", "PitchCall", "KorBB", "PlayResult", "ExitSpeed", "Angle",
    "TaggedHitType", "Direction", "BatterSide", "PlateLocHeight", "PlateLocSide",
    "GameID", "Inning", "Top/Bottom", "PAofInning", "PitchofPA"
    # NOTE: we no longer assume a Zone column exists in the CSV; we compute it here
  )
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  
  # --------- 2) Sanitize names and numerics ----------
  if (!"Batter" %in% names(df) || all(is.na(df$Batter))) df$Batter <- "Unknown"
  
  df <- df %>%
    mutate(
      Batter_display = str_squish(as.character(Batter)),
      Batter_group   = str_squish(tolower(Batter_display)),
      PitchCall      = as.character(PitchCall),
      KorBB          = as.character(KorBB),
      PlayResult     = as.character(PlayResult),
      TaggedHitType  = as.character(TaggedHitType),
      `Top/Bottom`   = as.character(`Top/Bottom`),
      ExitSpeed      = suppressWarnings(as.numeric(ExitSpeed)),
      Angle          = suppressWarnings(as.numeric(Angle)),
      PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
      PlateLocSide   = suppressWarnings(as.numeric(PlateLocSide)),
      Inning         = suppressWarnings(as.integer(Inning)),
      PAofInning     = suppressWarnings(as.integer(PAofInning)),
      PitchofPA      = suppressWarnings(as.integer(PitchofPA))
    )

  loc <- normalize_plate_location_feet(df$PlateLocHeight, df$PlateLocSide)
  df$PlateLocHeight <- loc$height
  df$PlateLocSide <- loc$side
  
  # --------- 3) Zone logic — replicate xRV grid exactly ----------
  # Assumes PlateLocHeight / PlateLocSide are in FEET (which matches your example)
  # Zones 1–9 = strike zone, 11–14 = chase / out of zone, 0 = everything else.
  
  df <- df %>%
    mutate(
      ZoneNum = dplyr::case_when(
        # Missing locations → no zone
        is.na(PlateLocHeight) | is.na(PlateLocSide) ~ NA_integer_,
        
        # In-zone 1–9
        PlateLocSide >= -0.83 & PlateLocSide <= -0.27 &
          PlateLocHeight <= 3.5   & PlateLocHeight >= 2.867 ~ 1L,
        PlateLocSide >= -0.27 & PlateLocSide <= 0.27 &
          PlateLocHeight <= 3.5   & PlateLocHeight >= 2.867 ~ 2L,
        PlateLocSide >= 0.27  & PlateLocSide <= 0.83 &
          PlateLocHeight <= 3.5   & PlateLocHeight >= 2.867 ~ 3L,
        
        PlateLocSide >= -0.83 & PlateLocSide <= -0.27 &
          PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 4L,
        PlateLocSide >= -0.27 & PlateLocSide <= 0.27 &
          PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 5L,
        PlateLocSide >= 0.27  & PlateLocSide <= 0.83 &
          PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 6L,
        
        PlateLocSide >= -0.83 & PlateLocSide <= -0.27 &
          PlateLocHeight >= 1.6   & PlateLocHeight <= 2.34  ~ 7L,
        PlateLocSide >= -0.27 & PlateLocSide <= 0.27 &
          PlateLocHeight >= 1.6   & PlateLocHeight <= 2.34  ~ 8L,
        PlateLocSide >= 0.27  & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.6   & PlateLocHeight <= 2.34  ~ 9L,
        
        # Out-of-zone buckets (11–14), matching your xRV pipeline
        PlateLocSide <= -0.82 & PlateLocHeight >= 2.55              ~ 11L,
        PlateLocSide <= 0     & PlateLocHeight >= 3.5               ~ 11L,
        PlateLocSide >= 0.82  & PlateLocHeight >= 2.55              ~ 12L,
        PlateLocSide >= 0     & PlateLocHeight >= 2.5               ~ 12L,
        PlateLocSide <= -0.82 & PlateLocHeight <= 2.55              ~ 13L,
        PlateLocSide <= 0     & PlateLocHeight <= 1.6               ~ 13L,
        PlateLocSide >= 0.83  & PlateLocHeight <= 2.55              ~ 14L,
        PlateLocSide >= 0     & PlateLocHeight <= 1.6               ~ 14L,
        
        TRUE ~ 0L
      ),
      Zone = dplyr::case_when(
        is.na(ZoneNum)          ~ NA_character_,
        ZoneNum %in% 1:9        ~ "InZone",
        ZoneNum %in% c(11:14,0) ~ "OutZone",
        TRUE                    ~ "OutZone"
      )
    )
  
  # --------- 4) Build a single PA index ----------
  df <- df %>%
    arrange(
      Batter_group,
      dplyr::coalesce(as.character(GameID), ""),
      dplyr::coalesce(Inning, 0L),
      dplyr::coalesce(`Top/Bottom`, ""),
      dplyr::coalesce(PAofInning, 0L),
      dplyr::coalesce(PitchofPA, 0L),
      dplyr::row_number()
    ) %>%
    group_by(Batter_group) %>%
    mutate(
      pa_index = cumsum(dplyr::coalesce(PitchofPA == 1L, FALSE))
    ) %>%
    ungroup()
  
  # --------- 5) Pitch-level flags ----------
  df <- df %>%
    mutate(
      Swing    = PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable",
                                  "FoulBallFieldable", "InPlay"),
      Contact  = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable","InPlay"),
      Whiff    = PitchCall == "StrikeSwinging",
      ZSwing   = (Zone == "InZone") & Swing,
      ZContact = (Zone == "InZone") & Contact,
      OOPitch  = (Zone == "OutZone"),
      OOSwing  = OOPitch & Swing,
      BBE      = PitchCall == "InPlay",
      BarrelFlag = BBE & is_brewster_barrel(ExitSpeed, Angle),
      HardHit  = BBE & !is.na(ExitSpeed) & ExitSpeed >= 95,
      AirBall  = BBE & (TaggedHitType %in% c("LineDrive", "FlyBall", "Popup"))
    )
  
  # --------- 6) Plate Appearances + PA-ending outcomes ----------
  pa_summary <- df %>%
    filter(pa_index > 0) %>%
    group_by(Batter_group) %>%
    summarise(PA = max(pa_index), .groups = "drop")
  
  pa_last <- df %>%
    filter(pa_index > 0) %>%
    group_by(Batter_group, pa_index) %>%
    slice_max(dplyr::coalesce(PitchofPA, 0L), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      Batter_group, pa_index,
      isK   = KorBB == "Strikeout",
      isBB  = KorBB == "Walk",
      isHBP = PitchCall == "HitByPitch",
      is1B  = PlayResult == "Single",
      is2B  = PlayResult == "Double",
      is3B  = PlayResult == "Triple",
      isHR  = PlayResult == "HomeRun"
    )
  
  pa_events <- pa_last %>%
    group_by(Batter_group) %>%
    summarise(
      K  = sum(isK,   na.rm = TRUE),
      BB = sum(isBB,  na.rm = TRUE),
      HBP= sum(isHBP, na.rm = TRUE),
      `1B`= sum(is1B, na.rm = TRUE),
      `2B`= sum(is2B, na.rm = TRUE),
      `3B`= sum(is3B, na.rm = TRUE),
      HR = sum(isHR,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(pa_summary, by = "Batter_group") %>%
    mutate(across(c(K, BB, HBP, `1B`, `2B`, `3B`, HR), ~ dplyr::coalesce(.x, 0L))) %>%
    mutate(
      wOBA_num = wBB * BB + wHBP * HBP + w1B * `1B` + w2B * `2B` + w3B * `3B` + wHR * HR,
      wOBA = ifelse(PA > 0, wOBA_num / PA, NA_real_)
    ) %>%
    select(Batter_group, PA, wOBA, K, BB)
  
  # --------- 7) Pitch-level rates ----------
  pitch_rates <- df %>%
    group_by(Batter_group) %>%
    summarise(
      InZonePitches = sum(Zone == "InZone", na.rm = TRUE),
      ZSwings       = sum(ZSwing,  na.rm = TRUE),
      ZContacts     = sum(ZContact,na.rm = TRUE),
      OOPitches     = sum(OOPitch, na.rm = TRUE),
      OOSwings      = sum(OOSwing, na.rm = TRUE),
      Swings        = sum(Swing,   na.rm = TRUE),
      Contacts      = sum(Contact, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Z-Contact%`     = ifelse(ZSwings > 0, round(ZContacts / ZSwings * 100, 1), 0),
      `Z-Swing%` = ifelse(InZonePitches > 0, round(ZSwings / InZonePitches * 100, 1), 0),
      `Chase%`         = ifelse(OOPitches > 0, round(OOSwings / OOPitches * 100, 1), 0),
      `Contact%`       = ifelse(Swings > 0, round(Contacts / Swings * 100, 1), 0)
    ) %>%
    select(Batter_group, `Contact%`, `Z-Contact%`, `Z-Swing%`, `Chase%`)
  
  # --------- 8) Batted-ball metrics ----------
  bbe_rates <- df %>%
    group_by(Batter_group) %>%
    summarise(
      BBECount = sum(BBE, na.rm = TRUE),
      BABIPDen = sum(BBE & PlayResult != "HomeRun", na.rm = TRUE),
      HitsInPlay = sum(BBE & is_hit_in_play(PlayResult), na.rm = TRUE),
      Barrels  = sum(BarrelFlag, na.rm = TRUE),
      HardHits = sum(HardHit,    na.rm = TRUE),
      MaxEV    = {
        bbe_ev <- ExitSpeed[PitchCall == "InPlay" & !is.na(ExitSpeed)]
        if (length(bbe_ev)) max(bbe_ev) else NA_real_
      },
      P90EV    = {
        bbe_ev <- ExitSpeed[PitchCall == "InPlay" & !is.na(ExitSpeed)]
        if (length(bbe_ev)) as.numeric(stats::quantile(bbe_ev, 0.9, na.rm = TRUE)) else NA_real_
      },
      .groups  = "drop"
    ) %>%
    mutate(
      BABIP     = ifelse(BABIPDen > 0, round(HitsInPlay / BABIPDen, 3), NA_real_),
      `EV>95%`  = ifelse(BBECount > 0, round(HardHits / BBECount * 100, 1), 0),
      `Barrel%` = ifelse(BBECount > 0, round(Barrels  / BBECount * 100, 1), 0)
    ) %>%
    select(Batter_group, BABIP, `EV>95%`, MaxEV, P90EV, `Barrel%`)
  
  # --------- 9) Combine and restore display names ----------
  out <- pa_events %>%
    left_join(pitch_rates, by = "Batter_group") %>%
    left_join(bbe_rates,   by = "Batter_group") %>%
    mutate(
      `K%`  = ifelse(PA > 0, round((K  / PA) * 100, 1), 0),
      `BB%` = ifelse(PA > 0, round((BB / PA) * 100, 1), 0)
    ) %>%
    left_join(df %>% distinct(Batter_group, Batter_display), by = "Batter_group") %>%
    mutate(Batter = Batter_display) %>%
    select(
      Batter, PA, wOBA, BABIP,
      `K%`, `BB%`, `Barrel%`,
      `Contact%`, `Z-Contact%`, `Z-Swing%`, `Chase%`,
      `EV>95%`, MaxEV, P90EV
    )
  
  out
}
