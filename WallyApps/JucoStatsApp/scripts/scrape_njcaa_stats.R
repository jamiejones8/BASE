#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rvest)
  library(xml2)
})

default_registry <- file.path("JucoStatsApp", "data", "program_registry.csv")
default_output <- file.path("JucoStatsApp", "data", "juco_player_stats_latest.csv")

parse_args <- function(args) {
  out <- list(
    season = "2025-26",
    division = "1",
    max_pages = 30L,
    base_url = "https://njcaa.prestosports.com",
    registry = default_registry,
    output = default_output,
    manifest = NA_character_,
    fixture_dir = NA_character_
  )

  for (arg in args) {
    if (!grepl("^--", arg)) next
    kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- kv[[1]]
    value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else "TRUE"
    key <- gsub("-", "_", key)
    if (key == "max_pages") value <- as.integer(value)
    out[[key]] <- value
  }

  out
}

normalize_key <- function(x) {
  x <- tolower(trimws(x))
  gsub("[^a-z0-9]+", "", x)
}

clean_names <- function(x) {
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", tolower(x))
  x[x == ""] <- paste0("col_", which(x == ""))
  make.unique(x, sep = "_")
}

clean_stat_value <- function(x) {
  if (!is.character(x)) return(x)
  x <- trimws(x)
  x[x %in% c("", "-", "--")] <- NA_character_
  suppressWarnings({
    numeric_x <- as.numeric(gsub(",", "", x))
  })
  if (sum(!is.na(numeric_x)) == 0) return(x)
  numeric_x
}

clean_text_value <- function(x) {
  if (!is.character(x)) return(x)
  gsub("\\s+", " ", trimws(x))
}

clean_player_name <- function(x) {
  x <- clean_text_value(x)
  if (!is.character(x)) return(x)
  x <- sub("^(.+?)\\s+[0-9]+\\s*\\1$", "\\1", x)
  x
}

is_non_player_name <- function(x) {
  value <- tolower(clean_text_value(x))
  grepl("^[0-9]+$", value) | value %in% c(
    "name", "player", "totals", "total", "opponent", "opponents",
    "team", "team totals", "overall", "conference", "conf",
    "non-conference", "non conference", "home", "away", "neutral",
    "exhibition", "division", "vs ranked"
  )
}

normalize_stat_columns <- function(stats) {
  identity_cols <- c(
    "name", "player_name", "team", "program_name", "stats_team_name",
    "source_group", "njcaa_region", "state", "yr", "pos", "stat_type",
    "source_url", "scraped_at"
  )

  stats %>%
    mutate(across(-any_of(identity_cols), clean_stat_value))
}

parse_pair_part <- function(x, part = c("first", "second")) {
  part <- match.arg(part)
  if (!is.character(x)) x <- as.character(x)
  pattern <- if (part == "first") "^\\s*([0-9.]+).*$" else "^[^-]+-\\s*([0-9.]+).*$"
  parsed <- ifelse(grepl(pattern, x), sub(pattern, "\\1", x), NA_character_)
  suppressWarnings(as.numeric(parsed))
}

coalesce_stat_column <- function(stats, target, candidates) {
  candidates <- candidates[candidates %in% names(stats)]
  if (length(candidates) == 0) return(stats)

  values <- stats[[candidates[[1]]]]
  if (length(candidates) > 1) {
    for (candidate in candidates[-1]) {
      values <- dplyr::coalesce(values, stats[[candidate]])
    }
  }

  if (target %in% names(stats)) {
    stats[[target]] <- dplyr::coalesce(stats[[target]], values)
  } else {
    stats[[target]] <- values
  }

  stats
}

standardize_stat_aliases <- function(stats) {
  if ("gp_gs" %in% names(stats)) {
    stats$gp_from_gp_gs <- parse_pair_part(stats$gp_gs, "first")
    stats$gs_from_gp_gs <- parse_pair_part(stats$gp_gs, "second")
  }
  if ("app_gs" %in% names(stats)) {
    stats$app_from_app_gs <- parse_pair_part(stats$app_gs, "first")
    stats$gs_from_app_gs <- parse_pair_part(stats$app_gs, "second")
  }
  if ("w_l" %in% names(stats)) {
    stats$w_from_w_l <- parse_pair_part(stats$w_l, "first")
    stats$l_from_w_l <- parse_pair_part(stats$w_l, "second")
  }
  if ("sb_att" %in% names(stats)) {
    sb <- parse_pair_part(stats$sb_att, "first")
    attempts <- parse_pair_part(stats$sb_att, "second")
    stats$sb_from_sb_att <- sb
    stats$cs_from_sb_att <- attempts - sb
  }

  stats <- coalesce_stat_column(stats, "g", c("g", "gp", "gp_from_gp_gs"))
  stats <- coalesce_stat_column(stats, "app", c("app", "gp", "app_from_app_gs"))
  stats <- coalesce_stat_column(stats, "gs", c("gs", "gs_from_gp_gs", "gs_from_app_gs"))
  stats <- coalesce_stat_column(stats, "w", c("w", "w_from_w_l"))
  stats <- coalesce_stat_column(stats, "l", c("l", "l_from_w_l"))
  stats <- coalesce_stat_column(stats, "k", c("k", "so"))
  stats <- coalesce_stat_column(stats, "obp", c("obp", "ob"))
  stats <- coalesce_stat_column(stats, "fpct", c("fpct", "fld", "f"))
  stats <- coalesce_stat_column(stats, "tc", c("tc", "c"))
  stats <- coalesce_stat_column(stats, "sb", c("sb", "sb_from_sb_att"))
  stats <- coalesce_stat_column(stats, "cs", c("cs", "cs_from_sb_att"))

  if (all(c("obp", "slg") %in% names(stats))) {
    derived_ops <- stats$obp + stats$slg
    if ("ops" %in% names(stats)) {
      stats$ops <- dplyr::coalesce(stats$ops, derived_ops)
    } else {
      stats$ops <- derived_ops
    }
  }

  stats
}

build_url <- function(base_url, season, division, pos, page, sort) {
  sprintf(
    "%s/sports/bsb/%s/div%s/players?pos=%s&r=%s&sort=%s&view=",
    sub("/+$", "", base_url),
    season,
    division,
    pos,
    page,
    sort
  )
}

read_html_source <- function(source) {
  if (file.exists(source)) {
    html_text <- paste(readLines(source, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    if (requireNamespace("curl", quietly = TRUE)) {
      handle <- curl::new_handle(
        useragent = "Mozilla/5.0 (compatible; JucoStatsApp/0.1; +local scouting use)",
        httpheader = c(Accept = "text/html,application/xhtml+xml")
      )
      response <- curl::curl_fetch_memory(source, handle = handle)
      html_text <- rawToChar(response$content)
    } else {
      html_text <- paste(readLines(source, warn = FALSE), collapse = "\n")
    }
  }

  if (grepl("Just a moment|Performing security verification|security service to protect against malicious bots|cf_chl|challenge-platform|Enable JavaScript and cookies", html_text, ignore.case = TRUE)) {
    stop(
      "The source returned a Cloudflare/browser challenge instead of stats HTML. ",
      "Use an approved browser-capable source, a Presto/NJCAA export, or saved HTML fixtures.",
      call. = FALSE
    )
  }

  if (grepl("Site has been disabled per owner request", html_text, ignore.case = TRUE)) {
    stop(
      "The source host returned a disabled-site page. ",
      "Try --base-url=https://njcaa.prestosports.com or update the source registry.",
      call. = FALSE
    )
  }

  read_html(html_text)
}

detect_stat_type <- function(tbl, fallback = NA_character_) {
  cols <- names(tbl)
  if (all(c("era", "ip") %in% cols)) return("pitching")
  if (all(c("tc", "po", "a", "e") %in% cols)) return("fielding")
  if (all(c("c", "po", "a", "e") %in% cols)) return("fielding")
  if (all(c("ab", "h", "avg") %in% cols)) return("hitting")
  if (all(c("player", "era", "ip") %in% cols)) return("pitching")
  if (all(c("player", "c", "po", "a", "e") %in% cols)) return("fielding")
  if (all(c("player", "avg", "ab", "h") %in% cols)) return("hitting")
  if (!is.na(fallback)) return(fallback)
  "unknown"
}

promote_player_name_column <- function(tbl) {
  cols <- names(tbl)
  if ("name" %in% cols || "player" %in% cols || ncol(tbl) == 0) return(tbl)
  first_col <- cols[[1]]
  is_blank_header <- grepl("^(col|x)_?[0-9]+$", first_col)
  is_player_stats <- detect_stat_type(tbl, NA_character_) != "unknown"
  is_game_log <- any(c("date", "opponent", "score", "result") %in% cols)
  if (is_blank_header && is_player_stats && !is_game_log) {
    names(tbl)[[1]] <- "name"
  }
  tbl
}

extract_player_tables <- function(source, stat_type = NA_character_, source_url = source, team_name = NA_character_, source_kind = NA_character_) {
  doc <- read_html_source(source)
  tables <- html_table(doc, fill = TRUE)
  if (length(tables) == 0) {
    return(tibble())
  }

  normalized_tables <- lapply(tables, function(tbl) {
    names(tbl) <- clean_names(names(tbl))
    promote_player_name_column(tbl)
  })

  if (!is.na(source_kind) && source_kind == "sidearm_stats") {
    sidearm_indexes <- which(vapply(
      normalized_tables,
      function(tbl) nrow(tbl) > 0 && "player" %in% names(tbl) && detect_stat_type(tbl, NA_character_) != "unknown",
      logical(1)
    ))

    seen <- character()
    player_table_indexes <- integer()
    for (index in sidearm_indexes) {
      table_stat_type <- detect_stat_type(normalized_tables[[index]], NA_character_)
      if (!table_stat_type %in% seen) {
        player_table_indexes <- c(player_table_indexes, index)
        seen <- c(seen, table_stat_type)
      }
    }
  } else {
    requested_stat_type <- if (!is.na(source_kind) && source_kind %in% c("hitting", "pitching", "fielding")) {
      source_kind
    } else {
      stat_type
    }

    matching_stat_indexes <- which(vapply(
      normalized_tables,
      function(tbl) {
        "name" %in% names(tbl) &&
          nrow(tbl) > 0 &&
          ("team" %in% names(tbl) || !is.na(team_name)) &&
          detect_stat_type(tbl, NA_character_) == requested_stat_type
      },
      logical(1)
    ))

    player_table_indexes <- if (length(matching_stat_indexes) > 0) {
      matching_stat_indexes[[1]]
    } else {
      which(vapply(
        normalized_tables,
        function(tbl) nrow(tbl) > 0 && "name" %in% names(tbl) && ("team" %in% names(tbl) || !is.na(team_name)),
        logical(1)
      ))
    }
  }

  if (length(player_table_indexes) == 0) {
    return(tibble())
  }

  identity_cols <- c("name", "team", "yr", "pos", "stat_type", "source_url", "scraped_at")

  parsed_tables <- lapply(player_table_indexes, function(index) {
    tbl <- normalized_tables[[index]] %>% as_tibble()
    if (nrow(tbl) == 0) return(NULL)
    if (!"name" %in% names(tbl) && "player" %in% names(tbl)) {
      tbl$name <- tbl$player
      tbl$player <- NULL
    }
    if (!"team" %in% names(tbl)) {
      tbl$team <- team_name
    }

    tbl %>%
      filter(!is.na(name), name != "", !grepl("^#", name)) %>%
      mutate(
        across(any_of(c("name", "team", "yr", "pos")), clean_text_value),
        name = clean_player_name(name),
        across(-any_of(identity_cols), as.character),
        stat_type = detect_stat_type(tbl, stat_type),
        source_url = source_url,
        scraped_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      ) %>%
      filter(!is_non_player_name(name))
  })

  bind_rows(parsed_tables[!vapply(parsed_tables, is.null, logical(1))])
}

scrape_stat_type <- function(base_url, season, division, pos, stat_type, sort, max_pages) {
  pages <- vector("list", max_pages + 1L)

  for (page in seq.int(0L, max_pages)) {
    url <- build_url(base_url, season, division, pos, page, sort)
    message("Scraping ", stat_type, " page ", page, ": ", url)
    tbl <- extract_player_tables(url, stat_type, url)
    if (nrow(tbl) == 0) {
      if (page == 0L) {
        warning("No rows found for ", stat_type, " page 0.")
      }
      break
    }
    pages[[page + 1L]] <- tbl
    Sys.sleep(0.75)
  }

  bind_rows(pages)
}

read_fixture_dir <- function(fixture_dir, manifest_path = NA_character_) {
  files <- list.files(fixture_dir, pattern = "\\.html?$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    stop("No .html files found in fixture_dir: ", fixture_dir, call. = FALSE)
  }

  manifest <- tibble(file = character(), source_url = character(), team = character(), stats_team_name = character())
  if (!is.na(manifest_path) && file.exists(manifest_path)) {
    manifest <- read_csv(manifest_path, show_col_types = FALSE) %>%
      mutate(file = normalizePath(file, mustWork = FALSE))
    files <- files[normalizePath(files, mustWork = FALSE) %in% manifest$file]
  }

  bind_rows(lapply(files, function(path) {
    filename <- tolower(basename(path))
    stat_type <- if (grepl("pitch", filename)) {
      "pitching"
    } else if (grepl("field", filename)) {
      "fielding"
    } else {
      "hitting"
    }

    source_meta <- manifest %>% filter(file == normalizePath(path, mustWork = FALSE)) %>% slice_head(n = 1)
    source_url <- if (nrow(source_meta) == 1 && !is.na(source_meta$source_url)) source_meta$source_url else path
    team_name <- if (nrow(source_meta) == 1 && "team" %in% names(source_meta) && !is.na(source_meta$team)) {
      source_meta$team
    } else if (nrow(source_meta) == 1 && "stats_team_name" %in% names(source_meta) && !is.na(source_meta$stats_team_name)) {
      source_meta$stats_team_name
    } else {
      NA_character_
    }

    source_kind <- if (nrow(source_meta) == 1 && "source_kind" %in% names(source_meta) && !is.na(source_meta$source_kind)) {
      source_meta$source_kind
    } else {
      NA_character_
    }

    extract_player_tables(path, stat_type, source_url, team_name, source_kind)
  }))
}

filter_to_registry <- function(stats, registry_path) {
  registry <- read_csv(registry_path, show_col_types = FALSE) %>%
    filter(include) %>%
    mutate(team_key = normalize_key(stats_team_name))

  stats %>%
    mutate(
      team = as.character(team),
      team_key = normalize_key(team)
    ) %>%
    inner_join(
      registry %>% select(program_name, stats_team_name, source_group, njcaa_region, state, team_key),
      by = "team_key"
    ) %>%
    rename(player_name = name) %>%
    select(
      stat_type,
      player_name,
      team,
      program_name,
      source_group,
      njcaa_region,
      state,
      everything(),
      -team_key
    ) %>%
    distinct()
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  stats <- if (!is.na(args$fixture_dir)) {
    read_fixture_dir(args$fixture_dir, args$manifest)
  } else {
    bind_rows(
      scrape_stat_type(args$base_url, args$season, args$division, "h", "hitting", "avg", args$max_pages),
      scrape_stat_type(args$base_url, args$season, args$division, "p", "pitching", "era", args$max_pages),
      scrape_stat_type(args$base_url, args$season, args$division, "f", "fielding", "pb", args$max_pages)
    )
  }

  stats <- stats %>%
    normalize_stat_columns() %>%
    standardize_stat_aliases()
  filtered <- filter_to_registry(stats, args$registry)
  dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
  write_csv(filtered, args$output, na = "")
  message("Wrote ", nrow(filtered), " rows to ", args$output)
}

if (sys.nframe() == 0) {
  main()
}
