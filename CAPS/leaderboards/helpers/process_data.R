# =========================================================
#  TXST BASEBALL — process_data.R (Unified, Team-Aware)
# =========================================================
library(dplyr)

process_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    message("⚠️ No data provided to process_data()")
    return(tibble())
  }
  
  # ---- Normalize column names (match Python: dots -> underscores) ----
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\.", "_", names(df))
  
  # ---- Normalize Pitcher column ----
  if ("player_name" %in% names(df) && !"Pitcher" %in% names(df)) {
    df <- df %>% rename(Pitcher = player_name)
  }
  if ("pitcher_name" %in% names(df) && !"Pitcher" %in% names(df)) {
    df <- df %>% rename(Pitcher = pitcher_name)
  }
  if ("pitcher" %in% names(df) && !"Pitcher" %in% names(df)) {
    df <- df %>% rename(Pitcher = pitcher)
  }
  
  # ---- Normalize pitcher handedness column ----
  if ("pitcher_throws" %in% names(df) && !"PitcherThrows" %in% names(df)) {
    df <- df %>% rename(PitcherThrows = pitcher_throws)
  }
  if ("Pitcher_Throws" %in% names(df) && !"PitcherThrows" %in% names(df)) {
    df <- df %>% rename(PitcherThrows = Pitcher_Throws)
  }
  if ("throws" %in% names(df) && !"PitcherThrows" %in% names(df)) {
    df <- df %>% rename(PitcherThrows = throws)
  }
  
  # ---- Normalize team columns ----
  if ("team" %in% names(df) && !"PitcherTeam" %in% names(df)) {
    df <- df %>% rename(PitcherTeam = team)
  }
  if ("batter_team" %in% names(df) && !"BatterTeam" %in% names(df)) {
    df <- df %>% rename(BatterTeam = batter_team)
  }
  
  # ---- Columns to keep for CatsDen tables ----
  cols_to_keep <- c(
    "Date", "Pitcher", "PitcherTeam",
    "Batter", "BatterTeam",
    "PitchCall", "KorBB", "PlayResult",
    "OutsOnPlay", "RunsScored",
    "ExitSpeed", "Angle", "RelSpeed",
    "VertRelAngle", "HorzRelAngle",
    "SpinRate", "SpinAxis",
    "HorzBreak", "InducedVertBreak",
    "Extension", "EffectiveVelo",
    "TaggedHitType", "TaggedPitchType", "Direction",
    "GameID", "Inning", "Top/Bottom", "PAofInning", "PitchofPA",
    "PlateLocHeight", "PlateLocSide", "BatterSide", "PitcherThrows"
  )
  
  
  df <- df[, intersect(cols_to_keep, names(df)), drop = FALSE]
  
  # ---- Add missing columns ----
  for (col in cols_to_keep) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  
  # ---- Add PAofGame if missing ----
  if (!"PAofGame" %in% names(df)) {
    df <- df %>%
      mutate(PAofGame = paste(GameID, Inning, `Top/Bottom`, PAofInning, sep = "_"))
  }
  
  # ---- Ensure numeric coordinates ----
  for (c in c("PlateLocHeight", "PlateLocSide")) {
    if (c %in% names(df)) {
      df[[c]] <- suppressWarnings(as.numeric(df[[c]]))
    }
  }
  
  # ---- Derive strike zone flag (InZone / OutZone) ----
  if (all(c("PlateLocHeight", "PlateLocSide") %in% names(df))) {
    h <- df$PlateLocHeight
    s <- df$PlateLocSide
    
    if (any(!is.na(h)) && max(h, na.rm = TRUE) < 10) h <- h * 12
    if (any(!is.na(s)) && max(abs(s), na.rm = TRUE) < 5) s <- s * 12
    
    df$Zone <- ifelse(
      h >= 18.29 & h <= 44.08 & s >= -9.97 & s <= 9.97,
      "InZone", "OutZone"
    )
  }
  
  message("✅ Data processed successfully with ", nrow(df), " rows.")
  df
}

# =========================================================
#  HELPER FUNCTIONS — Filter Active Team Data by Role
# =========================================================

filter_active_team <- function(df, team_column) {
  if (!team_column %in% names(df)) {
    message("⚠️ Missing team column — returning all data for debugging.")
    return(df)
  }

  df %>% filter(.data[[team_column]] == ACTIVE_TEAM_CODE)
}

get_team_pitching <- function(df) {
  filter_active_team(df, "PitcherTeam")
}

get_team_hitting <- function(df) {
  filter_active_team(df, "BatterTeam")
}
