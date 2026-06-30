# =========================================================
#  TXST BASEBALL ANALYTICS — Strength of Competition Helpers
#  Builds opponent terciles + split wOBA tables (scrimmages)
#  - PAs defined using PitchofPA == 1 (consistent with CatsDen)
#  - wOBA computed from PA-ending outcomes using hitter weights
#
#  UPDATE:
#   - Preserves BatterTeam / PitcherTeam in the FINAL output tables
#     so pages can filter to TEX_BOB cleanly.
#   - Adds wOBA_Total splits vs LHP and vs RHP (overall totals)
#     to the hitters output table (right of wOBA_Total).
# =========================================================
library(dplyr)
library(stringr)
library(tidyr)

safe_num <- function(x) suppressWarnings(as.numeric(x))

# ---- Normalize throw-hand labels to L/R ----
normalize_throw_hand <- function(x) {
  x <- toupper(str_squish(as.character(x)))

  dplyr::case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in% c("R", "RH", "RHP", "RIGHT", "RIGHTHANDED", "RIGHT-HANDED") ~ "R",
    x %in% c("L", "LH", "LHP", "LEFT", "LEFTHANDED", "LEFT-HANDED") ~ "L",
    str_detect(x, "^R") ~ "R",
    str_detect(x, "^L") ~ "L",
    TRUE ~ NA_character_
  )
}

# ---- Create PA index within a group using PitchofPA == 1 ----
make_pa_index <- function(df, group_col) {
  g <- rlang::sym(group_col)
  
  df %>%
    mutate(
      PitchofPA = suppressWarnings(as.integer(PitchofPA))
    ) %>%
    arrange(
      !!g,
      dplyr::coalesce(as.character(.data$GameID), ""),
      dplyr::coalesce(.data$Inning, 0L),
      dplyr::coalesce(as.character(.data$`Top/Bottom`), ""),
      dplyr::coalesce(.data$PAofInning, 0L),
      dplyr::coalesce(.data$PitchofPA, 0L),
      dplyr::row_number()
    ) %>%
    group_by(!!g) %>%
    mutate(pa_index = cumsum(dplyr::coalesce(.data$PitchofPA == 1L, FALSE))) %>%
    ungroup()
}

# ---- Collapse to one row per PA (last pitch of PA) ----
pa_last_rows <- function(df, group_col) {
  g <- rlang::sym(group_col)
  
  df %>%
    filter(.data$pa_index > 0) %>%
    group_by(!!g, .data$pa_index) %>%
    slice_max(dplyr::coalesce(.data$PitchofPA, 0L), n = 1, with_ties = FALSE) %>%
    ungroup()
}

# ---- Compute wOBA from PA-ending outcomes (same weights as hitter file) ----
compute_woba_from_pa_last <- function(pa_last,
                                      wBB = 0.69, wHBP = 0.72,
                                      w1B = 0.89, w2B = 1.27, w3B = 1.62, wHR = 2.10) {
  pa_last %>%
    transmute(
      isK   = .data$KorBB == "Strikeout",
      isBB  = .data$KorBB == "Walk",
      isHBP = .data$PitchCall == "HitByPitch",
      is1B  = .data$PlayResult == "Single",
      is2B  = .data$PlayResult == "Double",
      is3B  = .data$PlayResult == "Triple",
      isHR  = .data$PlayResult == "HomeRun"
    ) %>%
    summarise(
      PA  = n(),
      BB  = sum(isBB,  na.rm = TRUE),
      HBP = sum(isHBP, na.rm = TRUE),
      `1B`= sum(is1B,  na.rm = TRUE),
      `2B`= sum(is2B,  na.rm = TRUE),
      `3B`= sum(is3B,  na.rm = TRUE),
      HR  = sum(isHR,  na.rm = TRUE),
      wOBA_num = wBB * BB + wHBP * HBP + w1B * `1B` + w2B * `2B` + w3B * `3B` + wHR * HR,
      wOBA = ifelse(PA > 0, wOBA_num / PA, NA_real_)
    ) %>%
    select(PA, wOBA)
}

# ---- Assign Top/Middle/Bottom thirds (Top = best) ----
assign_terciles <- function(metric, higher_is_better = TRUE) {
  metric <- safe_num(metric)
  if (all(is.na(metric))) return(rep(NA_character_, length(metric)))
  
  r <- rank(metric, na.last = "keep", ties.method = "average")
  if (!higher_is_better) r <- rank(-metric, na.last = "keep", ties.method = "average")
  
  n <- sum(!is.na(r))
  if (n == 0) return(rep(NA_character_, length(r)))
  
  terc <- rep(NA_integer_, length(r))
  terc[!is.na(r)] <- pmin(3L, pmax(1L, ceiling(3 * r[!is.na(r)] / n)))
  
  dplyr::case_when(
    terc == 3L ~ "Top",
    terc == 2L ~ "Middle",
    terc == 1L ~ "Bottom",
    TRUE ~ NA_character_
  )
}

empty_soc_hitters <- function() {
  tibble::tibble(
    Hitter = character(),
    PA_Total = numeric(),
    wOBA_Total = numeric(),
    `wOBA vs LHP` = numeric(),
    `wOBA vs RHP` = numeric(),
    PA_vs_Top = numeric(),
    `wOBA vs Top` = numeric(),
    PA_vs_Middle = numeric(),
    `wOBA vs Middle` = numeric(),
    PA_vs_Bottom = numeric(),
    `wOBA vs Bottom` = numeric(),
    BatterTeam = character()
  )
}

empty_soc_pitchers <- function() {
  tibble::tibble(
    Pitcher = character(),
    PA_Allowed_Total = numeric(),
    wOBA_Allowed_Total = numeric(),
    PA_vs_Top = numeric(),
    `wOBA Allowed vs Top` = numeric(),
    PA_vs_Middle = numeric(),
    `wOBA Allowed vs Middle` = numeric(),
    PA_vs_Bottom = numeric(),
    `wOBA Allowed vs Bottom` = numeric(),
    PitcherTeam = character()
  )
}

ensure_split_columns <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_real_
    }
  }
  df
}

# =========================================================
#  MAIN: Build Strength-of-Competition Tables
# =========================================================
build_strength_of_comp_tables <- function(df,
                                          min_pa_qualify = 10,
                                          wBB = 0.69, wHBP = 0.72,
                                          w1B = 0.89, w2B = 1.27, w3B = 1.62, wHR = 2.10) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(hitters = empty_soc_hitters(), pitchers = empty_soc_pitchers()))
  }
  
  # normalize column names a bit (match your hitter file behavior)
  names(df) <- gsub("\\.", "/", names(df))
  
  # ensure needed columns exist
  needed <- c(
    "Batter","BatterTeam","Pitcher","PitcherTeam","PitchCall","KorBB","PlayResult",
    "GameID","Inning","Top/Bottom","PAofInning","PitchofPA",
    "PitcherThrows"
  )
  for (col in needed) if (!col %in% names(df)) df[[col]] <- NA
  
  # sanitize names
  df <- df %>%
    mutate(
      Batter  = str_squish(as.character(.data$Batter)),
      Pitcher = str_squish(as.character(.data$Pitcher)),
      BatterTeam  = str_squish(as.character(.data$BatterTeam)),
      PitcherTeam = str_squish(as.character(.data$PitcherTeam)),
      PitchCall  = as.character(.data$PitchCall),
      KorBB      = as.character(.data$KorBB),
      PlayResult = as.character(.data$PlayResult),
      Inning     = suppressWarnings(as.integer(.data$Inning)),
      PAofInning = suppressWarnings(as.integer(.data$PAofInning)),
      PitchofPA  = suppressWarnings(as.integer(.data$PitchofPA)),
      `Top/Bottom` = as.character(.data$`Top/Bottom`),
      PitcherThrows = normalize_throw_hand(.data$PitcherThrows)
    ) %>%
    filter(!is.na(Batter), Batter != "", !is.na(Pitcher), Pitcher != "")

  if (nrow(df) == 0) {
    return(list(hitters = empty_soc_hitters(), pitchers = empty_soc_pitchers()))
  }
  
  # -------------------------------------------------------
  # TEAM MAPS (so final summary tables can filter by team)
  # Pick the most frequent team label per player to avoid
  # duplicate joins if a player appears under multiple teams.
  # -------------------------------------------------------
  batter_team_map <- df %>%
    filter(!is.na(BatterTeam), BatterTeam != "") %>%
    count(Batter, BatterTeam, name = "n", sort = TRUE) %>%
    group_by(Batter) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(Batter, BatterTeam)
  
  pitcher_team_map <- df %>%
    filter(!is.na(PitcherTeam), PitcherTeam != "") %>%
    count(Pitcher, PitcherTeam, name = "n", sort = TRUE) %>%
    group_by(Pitcher) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(Pitcher, PitcherTeam)
  
  # ----------------------------
  # HITter overall wOBA (tier hitters)
  # + overall totals vs LHP / RHP
  # ----------------------------
  df_batter <- make_pa_index(df, "Batter")
  batter_pa_last <- pa_last_rows(df_batter, "Batter")
  
  hitter_overall_base <- batter_pa_last %>%
    group_by(Batter) %>%
    group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
    ungroup() %>%
    filter(PA >= min_pa_qualify) %>%
    rename(PA_Total = PA, wOBA_Total = wOBA)
  
  # Compute overall wOBA vs LHP / vs RHP using the same weights,
  # but only for hitters that qualified overall (keeps table stable).
  hitter_lhp <- batter_pa_last %>%
    filter(.data$PitcherThrows == "L") %>%
    semi_join(hitter_overall_base %>% select(Batter), by = "Batter") %>%
    group_by(Batter) %>%
    group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
    ungroup() %>%
    transmute(Batter, wOBA_vs_LHP = wOBA)
  
  hitter_rhp <- batter_pa_last %>%
    filter(.data$PitcherThrows == "R") %>%
    semi_join(hitter_overall_base %>% select(Batter), by = "Batter") %>%
    group_by(Batter) %>%
    group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
    ungroup() %>%
    transmute(Batter, wOBA_vs_RHP = wOBA)
  
  hitter_overall <- hitter_overall_base %>%
    left_join(hitter_lhp, by = "Batter") %>%
    left_join(hitter_rhp, by = "Batter") %>%
    mutate(HitterTier = assign_terciles(wOBA_Total, higher_is_better = TRUE))
  
  # ----------------------------
  # PITcher overall wOBA allowed (tier pitchers; lower is better)
  # ----------------------------
  df_pitcher <- make_pa_index(df, "Pitcher")
  pitcher_pa_last <- pa_last_rows(df_pitcher, "Pitcher")
  
  pitcher_overall <- pitcher_pa_last %>%
    group_by(Pitcher) %>%
    group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
    ungroup() %>%
    filter(PA >= min_pa_qualify) %>%
    mutate(PitcherTier = assign_terciles(wOBA, higher_is_better = FALSE)) %>%
    rename(PA_Allowed_Total = PA, wOBA_Allowed_Total = wOBA)
  
  # ----------------------------
  # Build tiered PA-last tables for splits
  # ----------------------------
  
  hitter_base <- hitter_overall %>%
    select(Batter, PA_Total, wOBA_Total, wOBA_vs_LHP, wOBA_vs_RHP)

  # HITTER splits: join pitcher tiers onto batter pa_last
  batter_pa_last_tiered <- batter_pa_last %>%
    inner_join(pitcher_overall %>% select(Pitcher, PitcherTier), by = "Pitcher") %>%
    inner_join(hitter_base, by = "Batter")

  hitter_split_cols <- c(
    "PA_Split_vs_Top", "wOBA_Split_vs_Top",
    "PA_Split_vs_Middle", "wOBA_Split_vs_Middle",
    "PA_Split_vs_Bottom", "wOBA_Split_vs_Bottom"
  )

  hitter_splits <- if (nrow(batter_pa_last_tiered) == 0) {
    hitter_base %>% select(Batter)
  } else {
    batter_pa_last_tiered %>%
      group_by(Batter, PitcherTier) %>%
      group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
      ungroup() %>%
      rename(PA_Split = PA, wOBA_Split = wOBA) %>%
      tidyr::pivot_wider(
        names_from = PitcherTier,
        values_from = c(PA_Split, wOBA_Split),
        names_glue = "{.value}_vs_{PitcherTier}"
      )
  }

  hitters_vs_pitcher_tiers <- hitter_base %>%
    left_join(hitter_splits, by = "Batter") %>%
    ensure_split_columns(hitter_split_cols) %>%
    transmute(
      Hitter = Batter,
      PA_Total,
      wOBA_Total,
      `wOBA vs LHP` = wOBA_vs_LHP,
      `wOBA vs RHP` = wOBA_vs_RHP,
      PA_vs_Top    = PA_Split_vs_Top,    `wOBA vs Top`    = wOBA_Split_vs_Top,
      PA_vs_Middle = PA_Split_vs_Middle, `wOBA vs Middle` = wOBA_Split_vs_Middle,
      PA_vs_Bottom = PA_Split_vs_Bottom, `wOBA vs Bottom` = wOBA_Split_vs_Bottom
    ) %>%
    arrange(desc(`wOBA vs Top`), desc(wOBA_Total)) %>%
    left_join(batter_team_map, by = c("Hitter" = "Batter"))
  
  pitcher_base <- pitcher_overall %>%
    select(Pitcher, PA_Allowed_Total, wOBA_Allowed_Total)

  # PITCHER splits: join hitter tiers onto pitcher pa_last
  pitcher_pa_last_tiered <- pitcher_pa_last %>%
    inner_join(hitter_overall %>% select(Batter, HitterTier), by = "Batter") %>%
    inner_join(pitcher_base, by = "Pitcher")

  pitcher_split_cols <- c(
    "PA_Split_vs_Top", "wOBA_Allowed_Split_vs_Top",
    "PA_Split_vs_Middle", "wOBA_Allowed_Split_vs_Middle",
    "PA_Split_vs_Bottom", "wOBA_Allowed_Split_vs_Bottom"
  )

  pitcher_splits <- if (nrow(pitcher_pa_last_tiered) == 0) {
    pitcher_base %>% select(Pitcher)
  } else {
    pitcher_pa_last_tiered %>%
      group_by(Pitcher, HitterTier) %>%
      group_modify(~ compute_woba_from_pa_last(.x, wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
      ungroup() %>%
      rename(PA_Split = PA, wOBA_Allowed_Split = wOBA) %>%
      tidyr::pivot_wider(
        names_from = HitterTier,
        values_from = c(PA_Split, wOBA_Allowed_Split),
        names_glue = "{.value}_vs_{HitterTier}"
      )
  }

  pitchers_vs_hitter_tiers <- pitcher_base %>%
    left_join(pitcher_splits, by = "Pitcher") %>%
    ensure_split_columns(pitcher_split_cols) %>%
    transmute(
      Pitcher,
      PA_Allowed_Total, wOBA_Allowed_Total,
      PA_vs_Top    = PA_Split_vs_Top,    `wOBA Allowed vs Top`    = wOBA_Allowed_Split_vs_Top,
      PA_vs_Middle = PA_Split_vs_Middle, `wOBA Allowed vs Middle` = wOBA_Allowed_Split_vs_Middle,
      PA_vs_Bottom = PA_Split_vs_Bottom, `wOBA Allowed vs Bottom` = wOBA_Allowed_Split_vs_Bottom
    ) %>%
    arrange(`wOBA Allowed vs Top`, wOBA_Allowed_Total) %>%
    left_join(pitcher_team_map, by = "Pitcher")
  
  list(
    hitters  = hitters_vs_pitcher_tiers,
    pitchers = pitchers_vs_hitter_tiers
  )
}
