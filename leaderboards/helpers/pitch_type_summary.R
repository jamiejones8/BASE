library(dplyr)
library(stringr)

is_pitch_type_breakdown_summary <- function(df) {
  required_cols <- c("Pitcher", "PitchType", "Pitches", "Whiff%", "CSW%", "GB%", "Zone%")
  !is.null(df) && all(required_cols %in% names(df))
}

prepare_pitch_type_breakdown_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  if (is_pitch_type_breakdown_summary(df)) {
    return(
      df %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        dplyr::select(Pitcher, PitchType, Pitches, `Whiff%`, `CSW%`, `GB%`, `Zone%`) %>%
        dplyr::arrange(Pitcher, desc(Pitches))
    )
  }

  safe_num <- function(x) suppressWarnings(as.numeric(x))
  safe_round <- function(x, d = 1) suppressWarnings(round(safe_num(x), d))

  names(df) <- trimws(names(df))

  df <- df %>%
    mutate(
      Pitcher = str_squish(as.character(Pitcher)),
      TaggedPitchType = str_squish(as.character(TaggedPitchType)),
      TaggedHitTypeNorm = str_to_upper(
        str_replace_all(str_squish(as.character(TaggedHitType)), "\\s+", "")
      ),
      TaggedPitchType = ifelse(
        is.na(TaggedPitchType) | TaggedPitchType == "",
        NA_character_,
        TaggedPitchType
      ),
      TaggedPitchType = str_to_title(TaggedPitchType),
      TaggedPitchType = str_replace_all(TaggedPitchType, "\\s+", ""),
      PlateLocHeight = safe_num(PlateLocHeight),
      PlateLocSide = safe_num(PlateLocSide)
    )

  loc <- normalize_plate_location_feet(df$PlateLocHeight, df$PlateLocSide)
  df$PlateLocHeight <- loc$height
  df$PlateLocSide <- loc$side

  df <- df %>%
    mutate(
      InZone = case_when(
        is.na(PlateLocHeight) | is.na(PlateLocSide) ~ NA,
        PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.6 & PlateLocHeight <= 3.5 ~ TRUE,
        TRUE ~ FALSE
      )
    )

  swing_calls <- pitch_swing_calls()
  whiff_calls <- c("StrikeSwinging")
  csw_calls <- pitch_csw_calls()

  df %>%
    filter(!is.na(Pitcher), Pitcher != "", !is.na(TaggedPitchType)) %>%
    group_by(Pitcher, TaggedPitchType) %>%
    summarise(
      Pitches = n(),
      Swings = sum(PitchCall %in% swing_calls, na.rm = TRUE),
      Whiffs = sum(PitchCall %in% whiff_calls, na.rm = TRUE),
      InPlay = sum(PitchCall == "InPlay", na.rm = TRUE),
      GroundBalls = sum(
        PitchCall == "InPlay" & TaggedHitTypeNorm == "GROUNDBALL",
        na.rm = TRUE
      ),
      `Whiff%` = if_else(Swings > 0, (Whiffs / Swings) * 100, NA_real_),
      `CSW%` = mean(PitchCall %in% csw_calls, na.rm = TRUE) * 100,
      `GB%` = if_else(InPlay > 0, (GroundBalls / InPlay) * 100, NA_real_),
      ZoneDen = sum(!is.na(InZone)),
      `Zone%` = if_else(
        ZoneDen > 0,
        (sum(InZone %in% TRUE, na.rm = TRUE) / ZoneDen) * 100,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    rename(PitchType = TaggedPitchType) %>%
    mutate(
      `Whiff%` = safe_round(`Whiff%`, 1),
      `CSW%` = safe_round(`CSW%`, 1),
      `GB%` = safe_round(`GB%`, 1),
      `Zone%` = safe_round(`Zone%`, 1)
    ) %>%
    select(Pitcher, PitchType, Pitches, `Whiff%`, `CSW%`, `GB%`, `Zone%`) %>%
    arrange(Pitcher, desc(Pitches))
}
