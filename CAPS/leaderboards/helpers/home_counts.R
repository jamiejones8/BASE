library(dplyr)

build_home_hitter_counts <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  names(df) <- gsub("\\.", "/", names(df))
  needed_cols <- c("Batter", "PlayResult", "KorBB", "PitchCall", "GameID", "Inning", "Top/Bottom", "PAofInning", "PitchofPA")
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  df <- df %>%
    mutate(
      Batter = trimws(as.character(Batter)),
      PlayResult = as.character(PlayResult),
      KorBB = as.character(KorBB),
      PitchCall = as.character(PitchCall),
      Inning = suppressWarnings(as.integer(Inning)),
      `Top/Bottom` = as.character(`Top/Bottom`),
      PAofInning = suppressWarnings(as.integer(PAofInning)),
      PitchofPA = suppressWarnings(as.integer(PitchofPA))
    ) %>%
    filter(!is.na(Batter), Batter != "") %>%
    group_by(Batter) %>%
    arrange(
      dplyr::coalesce(as.character(GameID), ""),
      dplyr::coalesce(Inning, 0L),
      dplyr::coalesce(`Top/Bottom`, ""),
      dplyr::coalesce(PAofInning, 0L),
      dplyr::coalesce(PitchofPA, 0L),
      .by_group = TRUE
    ) %>%
    mutate(pa_index = cumsum(dplyr::coalesce(PitchofPA == 1L, FALSE))) %>%
    ungroup()

  pa_summary <- df %>%
    filter(pa_index > 0) %>%
    group_by(Batter) %>%
    summarise(PA = max(pa_index), .groups = "drop")

  pa_last <- df %>%
    filter(pa_index > 0) %>%
    group_by(Batter, pa_index) %>%
    slice_max(dplyr::coalesce(PitchofPA, 0L), n = 1, with_ties = FALSE) %>%
    ungroup()

  pa_events <- pa_last %>%
    group_by(Batter) %>%
    summarise(
      `1B` = sum(PlayResult == "Single", na.rm = TRUE),
      `2B` = sum(PlayResult == "Double", na.rm = TRUE),
      `3B` = sum(PlayResult == "Triple", na.rm = TRUE),
      HR = sum(PlayResult == "HomeRun", na.rm = TRUE),
      Hits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
      Walks = sum(KorBB == "Walk", na.rm = TRUE),
      HBP = sum(PitchCall == "HitByPitch", na.rm = TRUE),
      .groups = "drop"
    )

  pa_summary %>%
    left_join(pa_events, by = "Batter") %>%
    mutate(
      `1B` = dplyr::coalesce(`1B`, 0L),
      `2B` = dplyr::coalesce(`2B`, 0L),
      `3B` = dplyr::coalesce(`3B`, 0L),
      HR = dplyr::coalesce(HR, 0L),
      Hits = dplyr::coalesce(Hits, 0L),
      Walks = dplyr::coalesce(Walks, 0L),
      HBP = dplyr::coalesce(HBP, 0L),
      AB = pmax(PA - Walks - HBP, 0L),
      TB = `1B` + (2L * `2B`) + (3L * `3B`) + (4L * HR),
      OBP = ifelse(PA > 0, (Hits + Walks + HBP) / PA, NA_real_),
      SLG = ifelse(AB > 0, TB / AB, NA_real_),
      OPS = OBP + SLG
    )
}

build_home_pitcher_counts <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  names(df) <- gsub("\\.", "/", names(df))
  needed_cols <- c("Pitcher", "KorBB", "OutsOnPlay", "RunsScored", "GameID", "Inning", "Top/Bottom", "PAofInning", "PitchofPA")
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  df <- df %>%
    mutate(
      Pitcher = trimws(as.character(Pitcher)),
      KorBB = as.character(KorBB),
      OutsOnPlay = suppressWarnings(as.integer(OutsOnPlay)),
      RunsScored = suppressWarnings(as.numeric(RunsScored)),
      Inning = suppressWarnings(as.integer(Inning)),
      `Top/Bottom` = as.character(`Top/Bottom`),
      PAofInning = suppressWarnings(as.integer(PAofInning)),
      PitchofPA = suppressWarnings(as.integer(PitchofPA))
    ) %>%
    filter(!is.na(Pitcher), Pitcher != "") %>%
    group_by(Pitcher) %>%
    arrange(
      dplyr::coalesce(as.character(GameID), ""),
      dplyr::coalesce(Inning, 0L),
      dplyr::coalesce(`Top/Bottom`, ""),
      dplyr::coalesce(PAofInning, 0L),
      dplyr::coalesce(PitchofPA, 0L),
      .by_group = TRUE
    ) %>%
    mutate(pa_index = cumsum(dplyr::coalesce(PitchofPA == 1L, FALSE))) %>%
    ungroup()

  pa_summary <- df %>%
    filter(pa_index > 0) %>%
    group_by(Pitcher) %>%
    summarise(PA = max(pa_index), .groups = "drop")

  runs_summary <- df %>%
    group_by(Pitcher) %>%
    summarise(
      RunsAllowed = sum(dplyr::coalesce(RunsScored, 0), na.rm = TRUE),
      .groups = "drop"
    )

  pa_last <- df %>%
    filter(pa_index > 0) %>%
    group_by(Pitcher, pa_index) %>%
    slice_max(dplyr::coalesce(PitchofPA, 0L), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      is_k = KorBB == "Strikeout",
      outs_on_play = dplyr::coalesce(OutsOnPlay, 0L),
      outs_recorded = ifelse(is_k & outs_on_play < 1L, 1L, outs_on_play)
    )

  pa_events <- pa_last %>%
    group_by(Pitcher) %>%
    summarise(
      Strikeouts = sum(is_k, na.rm = TRUE),
      Walks = sum(KorBB == "Walk", na.rm = TRUE),
      Outs = sum(outs_recorded, na.rm = TRUE),
      .groups = "drop"
    )

  pa_summary %>%
    left_join(pa_events, by = "Pitcher") %>%
    left_join(runs_summary, by = "Pitcher") %>%
    mutate(
      Strikeouts = dplyr::coalesce(Strikeouts, 0L),
      Walks = dplyr::coalesce(Walks, 0L),
      Outs = dplyr::coalesce(Outs, 0L),
      RunsAllowed = dplyr::coalesce(RunsAllowed, 0),
      IP = paste0(Outs %/% 3L, ".", Outs %% 3L),
      ERA = ifelse(Outs > 0, round((RunsAllowed * 27) / Outs, 2), NA_real_)
    )
}

build_home_pitcher_walks_counts <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  names(df) <- gsub("\\.", "/", names(df))
  needed_cols <- c("Pitcher", "KorBB", "PitchofPA")
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  df %>%
    mutate(
      Pitcher = trimws(as.character(Pitcher)),
      KorBB = as.character(KorBB),
      PitchofPA = suppressWarnings(as.numeric(PitchofPA))
    ) %>%
    filter(!is.na(Pitcher), Pitcher != "") %>%
    group_by(Pitcher) %>%
    summarise(
      PA = sum(PitchofPA == 1, na.rm = TRUE),
      Walks = sum(KorBB == "Walk", na.rm = TRUE),
      .groups = "drop"
    )
}
