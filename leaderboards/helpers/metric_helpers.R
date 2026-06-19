safe_metric_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_plate_location_feet <- function(height, side) {
  height_num <- safe_metric_num(height)
  side_num <- safe_metric_num(side)

  list(
    height = ifelse(!is.na(height_num) & abs(height_num) > 10, height_num / 12, height_num),
    side = ifelse(!is.na(side_num) & abs(side_num) > 5, side_num / 12, side_num)
  )
}

pitch_swing_calls <- function() {
  c("StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
}

pitch_csw_calls <- function() {
  c("StrikeCalled", "StrikeSwinging")
}

pitch_strike_calls <- function() {
  c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
}

is_batted_ball_event <- function(pitch_call) {
  !is.na(pitch_call) & as.character(pitch_call) == "InPlay"
}

is_hit_in_play <- function(play_result) {
  !is.na(play_result) & as.character(play_result) %in% c("Single", "Double", "Triple")
}

is_brewster_barrel <- function(exit_speed, angle) {
  ev <- safe_metric_num(exit_speed)
  la <- safe_metric_num(angle)

  !is.na(ev) & !is.na(la) &
    ((ev >= 95 & la >= 5 & la <= 30) |
       (ev >= 105 & la >= 5 & la <= 40))
}

safe_zscore <- function(x) {
  values <- safe_metric_num(x)
  sigma <- stats::sd(values, na.rm = TRUE)

  if (is.na(sigma) || sigma == 0) {
    return(rep(0, length(values)))
  }

  (values - mean(values, na.rm = TRUE)) / sigma
}

calculate_hitter_ppi <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      dplyr::across(c(`K%`, `BB%`, `Barrel%`), safe_metric_num),
      Z_K = safe_zscore(`K%`),
      Z_BB = safe_zscore(`BB%`),
      Z_Barrel = safe_zscore(`Barrel%`),
      RawPPI = (-0.5 * Z_K) + (0.5 * Z_BB) + (0.5 * Z_Barrel),
      HitterPPI = (1 / (1 + exp(-RawPPI))) * 0.9,
      HitterPPI = HitterPPI - 0.15,
      HitterPPI = pmin(pmax(HitterPPI, 0), 1.25),
      HitterPPI = round(HitterPPI, 3)
    ) %>%
    dplyr::select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
}

calculate_pitcher_ppi <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      dplyr::across(c(`K%`, `BB%`, `Barrel%`), safe_metric_num),
      Z_K = safe_zscore(`K%`),
      Z_BB = safe_zscore(`BB%`),
      Z_Barrel = safe_zscore(`Barrel%`),
      RawPPI = (1.2 * Z_K) - (0.9 * Z_BB) - (0.9 * Z_Barrel),
      `PPI (ERA)` = round(4.50 - (0.5 * RawPPI), 2)
    ) %>%
    dplyr::select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
}
