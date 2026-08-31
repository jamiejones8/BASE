#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rvest)
  library(tidyr)
})

source(file.path("JucoStatsApp", "scripts", "scrape_njcaa_stats.R"))

first_non_empty <- function(x) {
  x <- unique(na.omit(as.character(x)))
  x <- x[nzchar(trimws(x))]
  if (length(x) == 0) NA_character_ else x[[1]]
}

collapse_non_empty <- function(x) {
  x <- unique(na.omit(as.character(x)))
  x <- x[nzchar(trimws(x))]
  if (length(x) == 0) NA_character_ else paste(x, collapse = "/")
}

normalize_class <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  case_when(
    grepl("RS|RED\\s*SHIRT|REDSHIRT", raw) & grepl("FR|FRESH", raw) ~ "RS FR",
    grepl("RS|RED\\s*SHIRT|REDSHIRT", raw) & grepl("SO|SOPH", raw) ~ "RS SO",
    grepl("FR|FRESH", raw) ~ "FR",
    grepl("SO|SOPH", raw) ~ "SO",
    TRUE ~ NA_character_
  )
}

normalize_hand <- function(x, allowed = c("L", "R", "S")) {
  raw <- toupper(trimws(as.character(x)))
  out <- ifelse(raw %in% allowed, raw, NA_character_)
  out
}

parse_bats_throws <- function(...) {
  text <- paste(..., collapse = " ")
  text <- toupper(gsub("\\s+", " ", text))
  match <- regmatches(text, regexpr("\\b[LRSH]/[LRH]\\b", text, perl = TRUE))
  if (length(match) == 0 || match == "") {
    match <- regmatches(text, regexpr("\\b[LRSH]\\s*/\\s*[LRH]\\b", text, perl = TRUE))
  }
  if (length(match) == 0 || match == "") {
    return(c(bats = NA_character_, throws = NA_character_))
  }
  value <- gsub("\\s+", "", match[[1]])
  parts <- strsplit(value, "/", fixed = TRUE)[[1]]
  bats <- ifelse(parts[[1]] %in% c("L", "R", "S"), parts[[1]], NA_character_)
  throws <- ifelse(parts[[2]] %in% c("L", "R"), parts[[2]], NA_character_)
  c(bats = bats, throws = throws)
}

position_tokens <- function(x) {
  raw <- toupper(as.character(x))
  raw <- gsub("OUTFIELD|OUTFIELDER", "OF", raw)
  raw <- gsub("INFIELD|INFIELDER|INF", "IF", raw)
  raw <- gsub("[^A-Z0-9/ -]+", " ", raw)
  unique(unlist(strsplit(raw, "[/ ,;-]+")))
}

has_any_position <- function(x, tokens) {
  vapply(x, function(value) any(position_tokens(value) %in% tokens), logical(1))
}

position_bucket_string <- function(position_raw) {
  buckets <- character()
  tokens <- position_tokens(position_raw)
  if (any(tokens %in% c("OF", "LF", "CF", "RF"))) buckets <- c(buckets, "Outfield")
  if (any(tokens %in% c("1B", "2B", "3B", "SS", "IF"))) buckets <- c(buckets, "Infield")
  if (any(tokens %in% c("1B", "3B"))) buckets <- c(buckets, "Corner Infield")
  if (any(tokens %in% c("SS", "2B"))) buckets <- c(buckets, "MIF")
  if (any(tokens %in% c("C"))) buckets <- c(buckets, "Catcher")
  if (length(buckets) == 0) NA_character_ else paste(unique(buckets), collapse = "|")
}

extract_roster_profiles <- function(manifest_path) {
  if (!file.exists(manifest_path)) return(tibble())

  manifest <- read_csv(manifest_path, show_col_types = FALSE)
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    source_file <- manifest$file[[i]]
    if (!file.exists(source_file)) return(NULL)

    doc <- read_html_source(source_file)
    tables <- html_table(doc, fill = TRUE)
    parsed <- lapply(tables, function(tbl) {
      names(tbl) <- clean_names(names(tbl))
      if (!all(c("name", "position", "year") %in% names(tbl))) return(NULL)
      tbl %>%
        as_tibble() %>%
        mutate(across(everything(), as.character)) %>%
        rowwise() %>%
        mutate(
          slash_bats = parse_bats_throws(c_across(everything()))[["bats"]],
          slash_throws = parse_bats_throws(c_across(everything()))[["throws"]]
        ) %>%
        ungroup() %>%
        transmute(
          program_name = manifest$program_name[[i]],
          player_name = clean_player_name(name),
          class_raw = year,
          position_raw = position,
          bats = coalesce(if ("bats" %in% names(.)) bats else NA_character_, slash_bats),
          throws = coalesce(if ("throws" %in% names(.)) throws else NA_character_, slash_throws)
        )
    })

    bind_rows(parsed)
  })

  bind_rows(rows)
}

extract_stat_profiles <- function(stats_path) {
  if (!file.exists(stats_path)) return(tibble())

  read_csv(stats_path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    transmute(
      program_name,
      player_name = clean_player_name(player_name),
      class_raw = yr,
      position_raw = pos,
      bats = NA_character_,
      throws = case_when(
        grepl("\\bLHP\\b", toupper(pos)) ~ "L",
        grepl("\\bRHP\\b", toupper(pos)) ~ "R",
        TRUE ~ NA_character_
      )
    )
}

args <- commandArgs(trailingOnly = TRUE)
stats_path <- if (length(args) >= 1) args[[1]] else file.path("JucoStatsApp", "data", "juco_player_stats_inventory_sample.csv")
manifest_path <- if (length(args) >= 2) args[[2]] else file.path("JucoStatsApp", "data", "raw_njcaa", "sources", "2025-26", "source_manifest_rebuilt.csv")
output_path <- if (length(args) >= 3) args[[3]] else file.path("JucoStatsApp", "data", "player_profiles.csv")

profiles <- bind_rows(
  extract_roster_profiles(manifest_path),
  extract_stat_profiles(stats_path)
) %>%
  filter(!is.na(program_name), !is.na(player_name), player_name != "", !is_non_player_name(player_name)) %>%
  mutate(
    player_name = clean_player_name(player_name),
    class = normalize_class(class_raw),
    bats = normalize_hand(bats, c("L", "R", "S")),
    throws = normalize_hand(throws, c("L", "R"))
  ) %>%
  group_by(program_name, player_name) %>%
  summarise(
    class = first_non_empty(class),
    bats = first_non_empty(bats),
    throws = first_non_empty(throws),
    position_raw = collapse_non_empty(position_raw),
    position_buckets = position_bucket_string(position_raw),
    .groups = "drop"
  ) %>%
  arrange(program_name, player_name)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(profiles, output_path, na = "")
message("Wrote ", nrow(profiles), " player profile rows to ", output_path)
