library(dplyr)
library(stringr)

PITCHER_TTO_LEVELS <- function() {
  c(
    "1st Time Through Order",
    "2nd Time Through Order",
    "3rd Time Through Order",
    "4th+ Time Through Order"
  )
}

tag_pitcher_times_through_order <- function(df) {
  if (is.null(df)) return(df)

  if (nrow(df) == 0) {
    df$PitcherAppearanceId <- character()
    df$AppearancePA <- integer()
    df$PitcherTTO <- character()
    return(df)
  }

  needed_cols <- c("GameID", "Pitcher", "Inning", "Top/Bottom", "PAofInning", "PitchofPA")
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  if (!"PAofGame" %in% names(df)) {
    df <- df %>%
      mutate(PAofGame = paste(GameID, Inning, `Top/Bottom`, PAofInning, sep = "_"))
  }

  cleaned <- df %>%
    mutate(
      GameID = as.character(.data$GameID),
      Pitcher = str_squish(as.character(.data$Pitcher)),
      Inning = suppressWarnings(as.integer(.data$Inning)),
      `Top/Bottom` = str_squish(as.character(.data$`Top/Bottom`)),
      PAofInning = suppressWarnings(as.integer(.data$PAofInning)),
      PitchofPA = suppressWarnings(as.integer(.data$PitchofPA)),
      half_inning_order = dplyr::case_when(
        .data$`Top/Bottom` == "Top" ~ 1L,
        .data$`Top/Bottom` == "Bottom" ~ 2L,
        TRUE ~ 9L
      ),
      row_in_source = dplyr::row_number()
    )

  tagged <- cleaned %>%
    filter(
      !is.na(.data$GameID), .data$GameID != "",
      !is.na(.data$Pitcher), .data$Pitcher != ""
    ) %>%
    arrange(
      .data$GameID,
      .data$Inning,
      .data$half_inning_order,
      .data$PAofInning,
      .data$PitchofPA,
      .data$row_in_source
    ) %>%
    group_by(.data$GameID) %>%
    mutate(
      appearance_number = cumsum(dplyr::coalesce(.data$Pitcher != lag(.data$Pitcher), TRUE))
    ) %>%
    ungroup() %>%
    group_by(.data$GameID, .data$Pitcher, .data$appearance_number) %>%
    mutate(
      AppearancePA = match(.data$PAofGame, unique(.data$PAofGame)),
      PitcherAppearanceId = paste(.data$GameID, .data$Pitcher, .data$appearance_number, sep = "__"),
      PitcherTTO = dplyr::case_when(
        .data$AppearancePA <= 9L ~ "1st Time Through Order",
        .data$AppearancePA <= 18L ~ "2nd Time Through Order",
        .data$AppearancePA <= 27L ~ "3rd Time Through Order",
        TRUE ~ "4th+ Time Through Order"
      )
    ) %>%
    ungroup() %>%
    select(row_in_source, PitcherAppearanceId, AppearancePA, PitcherTTO)

  cleaned %>%
    left_join(tagged, by = "row_in_source") %>%
    select(-any_of(c("half_inning_order", "row_in_source")))
}

filter_pitcher_times_through_order <- function(df, selection = "All Times Through Order") {
  tagged <- tag_pitcher_times_through_order(df)

  if (
    is.null(selection) ||
    !nzchar(selection) ||
    identical(selection, "All Times Through Order")
  ) {
    return(tagged)
  }

  tagged %>%
    filter(.data$PitcherTTO == selection)
}
