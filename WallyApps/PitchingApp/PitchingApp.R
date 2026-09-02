library(magrittr)
library(shiny)
if (!isTRUE(get0("BASE_PITCHING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
  library(tidyverse)
}
library(bslib)
library(htmltools)
library(shinyWidgets)
library(plotly)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(DT)
library(shinycssloaders)
library(rlang)
library(grid)
library(gridExtra)
library(png)
library(ggplotify)
library(glue)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(ggtext)
library(scales)
library(lubridate)
library(hms)

.severity_hex_color <- function(score, good = "#2E7D32", bad = "#D62828") {
  if (!is.finite(score)) return(NA_character_)
  score <- pmin(pmax(score, -1), 1)
  severity <- abs(score)
  if (severity < 0.08) return(NA_character_)
  base <- grDevices::col2rgb(if (score > 0) good else bad)
  alpha <- 0.16 + 0.72 * severity^0.80
  mixed <- round(255 * (1 - alpha) + base[, 1] * alpha)
  grDevices::rgb(mixed[1], mixed[2], mixed[3], maxColorValue = 255)
}

count_statline <- function(df) {
  df <- tibble::as_tibble(df)
  
  pa <- df %>%
    dplyr::group_by(.data$PA_ID) %>%
    dplyr::summarise(
      # BB/K via KorBB (authoritative if present), fallback to PlayResult (last event text)
      KorBB_any  = any(grepl("^(BB|IBB)$", trimws(as.character(.data$KorBB)), ignore.case = TRUE), na.rm = TRUE),
      K_anyKorBB = any(grepl("\\bK\\b|strikeout", as.character(.data$KorBB), ignore.case = TRUE), na.rm = TRUE),
      
      PR_last = safe_last_nonempty(.data$PlayResult),
      PR_bb   = grepl("\\bintentional\\b|\\bwalk\\b", PR_last, ignore.case = TRUE),
      PR_k    = grepl("strike.?out|\\bK\\b", PR_last, ignore.case = TRUE),
      
      # Hits from PlayResult; avoid PCRE lookaheads and exclude "double play" explicitly
      HR_any  = any(grepl("home ?run|\\bHR\\b", as.character(.data$PlayResult), ignore.case = TRUE), na.rm = TRUE),
      X3B_any = any(grepl("\\btriple\\b",      as.character(.data$PlayResult), ignore.case = TRUE), na.rm = TRUE),
      X2B_any = any(
        grepl("\\bdouble\\b",    as.character(.data$PlayResult), ignore.case = TRUE) &
          !grepl("double\\s*play",  as.character(.data$PlayResult), ignore.case = TRUE),
        na.rm = TRUE
      ),
      X1B_any = any(grepl("\\bsingle\\b",      as.character(.data$PlayResult), ignore.case = TRUE), na.rm = TRUE),
      .groups = "drop"
    )
  
  bb_from_korbb <- sum(pa$KorBB_any, na.rm = TRUE)
  bb_from_pr    <- sum(!pa$KorBB_any & pa$PR_bb, na.rm = TRUE)
  bb_n          <- bb_from_korbb + bb_from_pr
  
  k_n  <- sum(pa$K_anyKorBB | (!pa$K_anyKorBB & pa$PR_k), na.rm = TRUE)
  h_n  <- sum(pa$HR_any | pa$X3B_any | pa$X2B_any | pa$X1B_any, na.rm = TRUE)
  pa_n <- nrow(pa)
  
  list(
    k_n = k_n,
    bb_n = bb_n,
    h_n = h_n,
    pa_n = pa_n,
    dbg = list(bb_from_korbb = bb_from_korbb, bb_from_pr = bb_from_pr)
  )
}

bench_fill <- function(value, bench, tol = 0.05, lower_better = FALSE) {
  v <- suppressWarnings(readr::parse_number(as.character(value)))
  b <- suppressWarnings(readr::parse_number(as.character(bench)))
  
  if (!is.finite(v) || !is.finite(b) || b == 0) return(NA_character_)
  
  score <- (v - b) / max(abs(b) * 0.25, 0.05)
  if (isTRUE(lower_better)) score <- -score
  .severity_hex_color(score)
}


# --- helper: resolve the per-game data.frame present in this scope ---
resolve_game_df <- function() {
  # Try common object names first (data.frame or tibble),
  # or call if it's a reactive/function returning a data.frame.
  candidates <- c(
    "df_game","game_df","gdf","aar_df","dat_game","game_data",
    "df_pa","df_pitch","df_box","box_df","df_filt","df"
  )
  for (nm in candidates) {
    if (exists(nm, inherits = TRUE)) {
      obj <- get(nm, inherits = TRUE)
      # If it's a reactive/function, try calling it
      if (is.function(obj)) {
        out <- try(obj(), silent = TRUE)
        if (!inherits(out, "try-error") && (is.data.frame(out) || inherits(out, "tbl"))) return(out)
      } else if (is.data.frame(obj) || inherits(obj, "tbl")) {
        return(obj)
      }
    }
  }
  # Optional last resort: a common reactive that might hold filtered data
  if (exists("dat_filt", inherits = TRUE)) {
    obj <- get("dat_filt", inherits = TRUE)
    if (is.function(obj)) {
      out <- try(obj(), silent = TRUE)
      if (!inherits(out, "try-error") && (is.data.frame(out) || inherits(out, "tbl"))) return(out)
    }
  }
  NULL
}

# ---- Players to hide from dropdowns ----
EXCLUDE_PLAYERS <- c(
  "Alex Valentin",
  "Austin Eaton",
  "Bryson Dudley",
  "Conner Doucet",
  "Hayde Key",
  "Jackson Mayo",
  "Jackson Teer",
  "Johnny Alkire",
  "Matthew Tippie",
  "Carson Laws",
  "Ryan Lawton",
  "Taylor Seay"
)


# ---- Name & season helpers ----
fix_name_commas <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # Only swap if the WHOLE string is in "Last, First" form (apostrophes/hyphens allowed)
  swapped <- ifelse(grepl("^\\s*[^,]+,\\s*[^,]+\\s*$", x),
                    sub("^\\s*([^,]+),\\s*([^,]+)\\s*$", "\\2 \\1", x),
                    x)
  gsub("\\s+", " ", swapped)
}

name_display <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    s <- trimws(s)
    s <- gsub("\\s+", " ", s)
    if (grepl(",", s)) {
      parts <- strsplit(s, ",\\s*")[[1]]
      if (length(parts) >= 2) return(paste0(parts[2], " ", parts[1]))
    }
    s
  }, "", USE.NAMES = FALSE)
}

name_norm <- function(x) {
  tolower(name_display(x))
}

# Order CustomGameID choices by GameDate (desc). Unknown dates keep their original order at the end.
order_game_ids_desc <- function(ids, lookup_tbl = NULL) {
  if (!length(ids)) return(ids)
  if (is.null(lookup_tbl) || !all(c("CustomGameID","GameDate") %in% names(lookup_tbl))) return(sort(ids, decreasing = TRUE))
  ord <- lookup_tbl %>%
    dplyr::filter(.data$CustomGameID %in% ids, !is.na(.data$GameDate)) %>%
    dplyr::arrange(dplyr::desc(.data$GameDate)) %>%
    dplyr::pull(.data$CustomGameID)
  c(ord, setdiff(ids, ord))
}
  
# ---- Season window helper (used by AAR + season labeling) ----
season_window_info <- function(date_val) {
  date_val <- suppressWarnings(as.Date(date_val))
  out <- list(label = "Season", start = as.Date("1900-01-01"), end = as.Date("2100-12-31"))
  if (is.na(date_val)) return(out)
  
  y <- as.integer(format(date_val, "%Y"))
  
  # Fixed windows used in the app
  if (date_val >= as.Date("2025-02-13") && date_val <= as.Date("2025-06-30")) {
    out$label <- "2025 Season"; out$start <- as.Date("2025-02-13"); out$end <- as.Date("2025-06-30"); return(out)
  }
  if (date_val >= as.Date("2025-09-01") && date_val <= as.Date("2025-12-31")) {
    out$label <- "2025 Fall"; out$start <- as.Date("2025-09-01"); out$end <- as.Date("2025-12-31"); return(out)
  }
  if (date_val >= as.Date("2026-01-01") && date_val <= as.Date("2026-02-12")) {
    out$label <- "2026 Squads"; out$start <- as.Date("2026-01-01"); out$end <- as.Date("2026-02-12"); return(out)
  }
  if (date_val >= as.Date("2026-02-13") && date_val <= as.Date("2026-06-30")) {
    out$label <- "2026 Season"; out$start <- as.Date("2026-02-13"); out$end <- as.Date("2026-06-30"); return(out)
  }
  
  # Fallback heuristics if outside fixed windows
  m <- as.integer(format(date_val, "%m"))
  if (m %in% 2:6) out$label <- paste0(y, " Season") else out$label <- paste0(y, " Fall")
  out$start <- as.Date(paste0(y, "-01-01"))
  out$end   <- as.Date(paste0(y, "-12-31"))
  out
}

SEASON_CHOICES <- c(
  "2025 Season" = "S25",
  "2025 Fall"   = "F25",
  "2026 Squads" = "SQ26",
  "2026 Season" = "S26"
)


# Helper to build visible player choices from a data.frame column
# Usage: visible_players(df, "player_name")  OR  visible_players(df, "Pitcher")
visible_players <- function(data, col) {
  nm <- rlang::ensym(col)
  choices <- sort(unique(dplyr::pull(data, !!nm)))
  setdiff(choices, EXCLUDE_PLAYERS)
}

# ---- Asset finder (case-insensitive, supports png/jpg/jpeg in / or /www) ----
find_asset <- function(basenames) {
  exts <- c("png","jpg","jpeg")
  roots <- unique(c(
    "", "www/",
    if (exists("app_dir", inherits = TRUE)) file.path(get("app_dir", inherits = TRUE), "www") else character(0)
  ))
  cands <- unlist(lapply(basenames, function(b) {
    as.vector(outer(roots, exts, function(r,e) file.path(r, paste0(b, ".", e))))
  }), use.names = FALSE)
  existing <- cands[file.exists(cands)]
  if (length(existing)) normalizePath(existing[[1]], winslash = "/", mustWork = TRUE) else NULL
}

options(plotly.jsonifyNamedVectors = FALSE)
options(warn = 1)

`%||%` <- function(a, b) if (!is.null(a)) a else b
# ---- Safe divide (define once, early) ----
if (!exists("sdiv", inherits = FALSE)) {
  sdiv <- function(num, den) {
    num <- suppressWarnings(as.numeric(num))
    den <- suppressWarnings(as.numeric(den))
    ifelse(is.finite(den) & den > 0, num/den, NA_real_)
  }
}

# ---- xRV scale helpers (100 = D1 mean; 10 pts per SD; lower xRV is better) ----
# D1 mean/sd computed from 2025_full_dataset using release height (feet) as height proxy
XRV_D1_MEAN <- 0.011851966103058702
XRV_D1_SD   <- 0.04737620444754918

xrv_plus_from <- function(x, mu, sd) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(mu) || !is.finite(sd) || sd <= 0) return(ifelse(is.finite(x), 100, NA_real_))
  # lower xRV is better -> invert so better = higher score
  100 + (mu - x) / sd * 10
}

# ---- Stuff+ helpers (movement-only, unsupervised) ----
called_stuff_baseline_path <- "data/called_stuff_baseline.csv"
find_app_file <- function(rel_path, start = getwd(), max_up = 5) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 0:max_up) {
    cand <- file.path(cur, rel_path)
    if (file.exists(cand)) return(cand)
    up <- dirname(cur)
    if (identical(up, cur)) break
    cur <- up
  }
  NA_character_
}

normalize_pitch_type_for_stuff <- function(pt) {
  raw <- tolower(trimws(as.character(pt %||% "")))
  if (!nzchar(raw)) return("Undefined")
  pc <- gsub("[^a-z0-9]", "", raw)

  if (pc %in% c("fourseamfastball","4seamfastball","fourseam","4seam","ff")) return("Four-Seam")
  if (pc %in% c("twoseamfastball","2seamfastball","twoseam","2seam","ft")) return("Two-Seam")
  if (pc %in% c("oneseamfastball","1seamfastball","oneseam","1seam")) return("Fastball")
  if (pc %in% c("fastball","fb","fa")) return("Fastball")
  if (pc %in% c("sinker","si","snk")) return("Sinker")
  if (pc %in% c("cutter","cut","fc")) return("Cutter")
  if (pc %in% c("slider","sl")) return("Slider")
  if (pc %in% c("sweeper","sweep","st","sw")) return("Sweeper")
  if (pc %in% c("curveball","curve","cb","cu","kc")) return("Curveball")
  if (pc %in% c("changeup","change","ch","chg")) return("Changeup")
  if (pc %in% c("splitter","split","splitfinger","fs","fo")) return("Splitter")
  if (pc %in% c("knuckleball","knuckle")) return("Undefined")
  if (pc %in% c("other","undefined","unknown","untagged")) return("Undefined")

  # fallback to existing helper (keeps canonical labels) if available
  if (exists("self_scout_pitch_type", mode = "function")) {
    ct <- self_scout_pitch_type(raw)
    if (!is.na(ct) && nzchar(ct)) return(ct)
  }
  "Undefined"
}

collapse_stuff_baseline <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  if (!"PitchType_stuff" %in% names(df)) return(df)
  if (!"comp_n" %in% names(df)) df$comp_n <- 1

  metric_pairs <- list(
    RelSpeed = c("RelSpeed_mean", "RelSpeed_sd"),
    SpinRate = c("SpinRate_mean", "SpinRate_sd"),
    InducedVertBreak = c("InducedVertBreak_mean", "InducedVertBreak_sd"),
    HorzBreak = c("HorzBreak_mean", "HorzBreak_sd"),
    VertApprAngle = c("VertApprAngle_mean", "VertApprAngle_sd"),
    HorzApprAngle = c("HorzApprAngle_mean", "HorzApprAngle_sd"),
    RelHeight = c("RelHeight_mean", "RelHeight_sd"),
    RelSide = c("RelSide_mean", "RelSide_sd"),
    Extension = c("Extension_mean", "Extension_sd"),
    comp = c("comp_mean", "comp_sd")
  )

  pooled_stats <- function(mu, sd, n) {
    mu <- suppressWarnings(as.numeric(mu))
    sd <- suppressWarnings(as.numeric(sd))
    n  <- suppressWarnings(as.numeric(n))
    ok <- is.finite(mu) & is.finite(sd) & is.finite(n) & n > 0
    if (!any(ok)) return(c(mu = NA_real_, sd = NA_real_))
    mu <- mu[ok]; sd <- sd[ok]; n <- n[ok]
    w_mu <- sum(n * mu) / sum(n)
    if (length(n) <= 1 || sum(n) <= 1) {
      return(c(mu = w_mu, sd = sd[1]))
    }
    ss_within <- sum((n - 1) * (sd ^ 2))
    ss_between <- sum(n * (mu - w_mu) ^ 2)
    denom <- sum(n) - 1
    w_sd <- if (denom > 0) sqrt((ss_within + ss_between) / denom) else NA_real_
    c(mu = w_mu, sd = w_sd)
  }

  out <- df %>%
    dplyr::group_by(PitchType_stuff) %>%
    dplyr::summarise(
      comp_n = sum(comp_n, na.rm = TRUE),
      .groups = "drop"
    )

  for (nm in names(metric_pairs)) {
    mu_col <- metric_pairs[[nm]][1]
    sd_col <- metric_pairs[[nm]][2]
    if (!all(c(mu_col, sd_col) %in% names(df))) {
      out[[mu_col]] <- NA_real_
      out[[sd_col]] <- NA_real_
      next
    }
    tmp <- df %>%
      dplyr::group_by(PitchType_stuff) %>%
      dplyr::summarise(
        .mu = pooled_stats(.data[[mu_col]], .data[[sd_col]], comp_n)[["mu"]],
        .sd = pooled_stats(.data[[mu_col]], .data[[sd_col]], comp_n)[["sd"]],
        .groups = "drop"
      )
    out <- out %>%
      dplyr::left_join(tmp, by = "PitchType_stuff") %>%
      dplyr::rename(!!mu_col := .mu, !!sd_col := .sd)
  }

  out
}

called_stuff_baseline <- tryCatch(
  {
    baseline_path <- find_app_file(file.path("data", "called_stuff_baseline.csv"))
    if (!is.na(baseline_path)) {
      called_stuff_baseline_path <<- baseline_path
      readr::read_csv(baseline_path, show_col_types = FALSE)
    } else {
      called_stuff_baseline_path <<- "data/called_stuff_baseline.csv"
      readr::read_csv("data/called_stuff_baseline.csv", show_col_types = FALSE)
    }
  } %>%
    dplyr::mutate(
      PitchType_stuff = vapply(pitch_type, normalize_pitch_type_for_stuff, character(1)),
      comp_n = suppressWarnings(as.numeric(comp_n))
    ) %>%
    dplyr::filter(!is.na(PitchType_stuff), nzchar(PitchType_stuff)) %>%
    dplyr::arrange(dplyr::desc(comp_n)) %>%
    dplyr::distinct(PitchType_stuff, .keep_all = TRUE),
  error = function(e) NULL
)

compute_called_stuff <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  d <- tibble::as_tibble(d)
  if (is.null(called_stuff_baseline) || !nrow(called_stuff_baseline)) {
    if (!isTRUE(getOption("stuff_baseline_missing"))) {
      message("Stuff+ baseline not loaded; Stuff+ will be NA. Check data/called_stuff_baseline.csv path.")
      options(stuff_baseline_missing = TRUE)
    }
    d$stuff_raw <- NA_real_
    d$stuff_plus <- NA_real_
    return(d)
  }
  if (!"PitchType" %in% names(d) && "TaggedPitchType" %in% names(d)) d$PitchType <- d$TaggedPitchType
  d$PitchType <- trimws(as.character(d$PitchType))
  if ("TaggedPitchType" %in% names(d)) {
    tagged <- trimws(as.character(d$TaggedPitchType))
    blank <- is.na(d$PitchType) | !nzchar(d$PitchType)
    d$PitchType[blank] <- tagged[blank]
  }

  d$PitchType_stuff <- vapply(d$PitchType, normalize_pitch_type_for_stuff, character(1))
  base <- called_stuff_baseline
  if (!"PitchType_stuff" %in% names(base)) {
    base <- base %>%
      dplyr::mutate(PitchType_stuff = vapply(pitch_type, normalize_pitch_type_for_stuff, character(1)))
  }
  base <- base %>%
    dplyr::filter(!is.na(PitchType_stuff), nzchar(PitchType_stuff))
  if (!"comp_n" %in% names(base)) base$comp_n <- 1
  base$comp_n <- suppressWarnings(as.numeric(base$comp_n))
  base <- base %>%
    dplyr::arrange(dplyr::desc(comp_n)) %>%
    dplyr::distinct(PitchType_stuff, .keep_all = TRUE)
  d <- d %>% dplyr::left_join(base, by = "PitchType_stuff", relationship = "many-to-one", suffix = c("", "_base"))

  # If any baseline columns collided with existing columns, prefer the baseline values.
  req_cols <- c(
    "RelSpeed_mean","RelSpeed_sd","SpinRate_mean","SpinRate_sd","Extension_mean","Extension_sd",
    "InducedVertBreak_mean","InducedVertBreak_sd","HorzBreak_mean","HorzBreak_sd",
    "VertApprAngle_mean","VertApprAngle_sd","HorzApprAngle_mean","HorzApprAngle_sd",
    "RelHeight_mean","RelHeight_sd","RelSide_mean","RelSide_sd","comp_mean","comp_sd"
  )
  for (nm in req_cols) {
    base_nm <- paste0(nm, "_base")
    if (base_nm %in% names(d)) {
      if (!nm %in% names(d)) d[[nm]] <- d[[base_nm]]
      d[[nm]] <- dplyr::coalesce(d[[base_nm]], d[[nm]])
      d[[base_nm]] <- NULL
    }
  }

  if (!all(req_cols %in% names(d))) {
    d$stuff_raw <- NA_real_
    d$stuff_plus <- NA_real_
    return(d)
  }

  raw_cols <- c(
    "RelSpeed","SpinRate","Extension","InducedVertBreak","HorzBreak",
    "VertApprAngle","HorzApprAngle","RelHeight","RelSide"
  )
  for (nm in raw_cols) {
    if (!nm %in% names(d)) d[[nm]] <- NA_real_
  }

  to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
  d$RelSpeed         <- to_num(d$RelSpeed)
  d$SpinRate         <- to_num(d$SpinRate)
  d$InducedVertBreak <- to_num(d$InducedVertBreak)
  d$HorzBreak        <- to_num(d$HorzBreak)
  d$VertApprAngle    <- to_num(d$VertApprAngle)
  d$HorzApprAngle    <- to_num(d$HorzApprAngle)
  d$RelHeight        <- to_num(d$RelHeight)
  d$RelSide          <- to_num(d$RelSide)
  d$Extension        <- to_num(d$Extension)
  base_num_cols <- c(
    "RelSpeed_mean","RelSpeed_sd","SpinRate_mean","SpinRate_sd","Extension_mean","Extension_sd",
    "InducedVertBreak_mean","InducedVertBreak_sd","HorzBreak_mean","HorzBreak_sd",
    "VertApprAngle_mean","VertApprAngle_sd","HorzApprAngle_mean","HorzApprAngle_sd",
    "RelHeight_mean","RelHeight_sd","RelSide_mean","RelSide_sd","comp_mean","comp_sd"
  )
  for (nm in base_num_cols) {
    if (nm %in% names(d)) d[[nm]] <- to_num(d[[nm]])
  }

  z_safe <- function(x, mu, sd, abs_val = FALSE) {
    z <- (x - mu) / sd
    if (abs_val) z <- abs(z)
    z
  }

  z_velo <- z_safe(d$RelSpeed, d$RelSpeed_mean, d$RelSpeed_sd, FALSE)
  z_spin <- z_safe(d$SpinRate, d$SpinRate_mean, d$SpinRate_sd, FALSE)
  z_ext  <- z_safe(d$Extension, d$Extension_mean, d$Extension_sd, FALSE)
  z_ivb  <- z_safe(d$InducedVertBreak, d$InducedVertBreak_mean, d$InducedVertBreak_sd, TRUE)
  z_hb   <- z_safe(d$HorzBreak, d$HorzBreak_mean, d$HorzBreak_sd, TRUE)
  z_vaa  <- z_safe(d$VertApprAngle, d$VertApprAngle_mean, d$VertApprAngle_sd, TRUE)
  z_haa  <- z_safe(d$HorzApprAngle, d$HorzApprAngle_mean, d$HorzApprAngle_sd, TRUE)
  z_rh   <- z_safe(d$RelHeight, d$RelHeight_mean, d$RelHeight_sd, TRUE)
  z_rs   <- z_safe(d$RelSide, d$RelSide_mean, d$RelSide_sd, TRUE)

  z_stack <- cbind(z_velo, z_spin, z_ivb, z_hb, z_vaa, z_haa, z_rh, z_rs, z_ext)
  comp <- suppressWarnings(apply(z_stack, 1, function(v) mean(v[is.finite(v)], na.rm = TRUE)))
  comp[!is.finite(comp)] <- NA_real_

  d$stuff_raw <- comp
  d$stuff_plus <- ifelse(
    is.finite(d$comp_mean) & is.finite(d$comp_sd) & d$comp_sd > 0 & is.finite(comp),
    100 + (comp - d$comp_mean) / d$comp_sd * 10,
    NA_real_
  )

  d
}

# wOBA helpers
woba_weights <- list(
  BB  = 0.690,  # unintentional walk
  HBP = 0.720,
  X1B = 0.880,
  X2B = 1.247,
  X3B = 1.578,
  HR  = 2.031
)

# D1 reference constants
D1_REF <- list(
  strike_pct    = 0.65,
  fps_pct       = 0.63,
  pre2k_zone    = 0.50,
  twoK_zone     = 0.43,
  ea_pct        = 0.70,
  zone_pct      = 0.50,
  put_away_pct  = 0.19
)

# Ensure PA_ID, PitchNum, StrikesPre/BallsPre exist; returns updated df
ensure_counts <- function(d) {
  d <- tibble::as_tibble(d)
  # PA_ID / PitchNum
  if (!("PA_ID" %in% names(d) && "PitchNum" %in% names(d))) {
    # Prefer explicit pitch/PA columns if present
    if ("PitchofPA" %in% names(d)) {
      pn <- suppressWarnings(as.integer(readr::parse_number(as.character(d$PitchofPA))))
      d$PitchNum <- pn
      d$PA_ID <- cumsum(dplyr::coalesce(pn == 1L, FALSE))
    } else if (all(c("PAofInning","Inning","Date","Batter") %in% names(d))) {
      d$PA_ID <- interaction(d$Date, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
      d <- d %>% dplyr::group_by(PA_ID) %>% dplyr::mutate(PitchNum = dplyr::row_number()) %>% dplyr::ungroup()
    } else {
      # Fallback heuristic when explicit PA fields are missing
      pc <- as.character(d$PitchCall %||% NA_character_)
      term <- pc %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled","Walk","HitByPitch")
      if ("KorBB"      %in% names(d)) term <- term | (!is.na(d$KorBB)      & nzchar(d$KorBB))
      # NOTE: Do NOT use PlayResult here because it is populated on every pitch in this dataset
      start <- c(TRUE, head(term, -1))
      d$PA_ID <- cumsum(start)
      d <- d %>% dplyr::group_by(PA_ID) %>% dplyr::mutate(PitchNum = dplyr::row_number()) %>% dplyr::ungroup()
    }
  }
  # FirstPitch
  if (!"FirstPitch" %in% names(d)) d$FirstPitch <- d$PitchNum == 1L
  
  # StrikesPre / BallsPre (reconstruct if needed from *_AfterPitch)
  to_int <- function(x) suppressWarnings(as.integer(readr::parse_number(as.character(x))))
  if (!"StrikesPre" %in% names(d)) d$StrikesPre <- NA_integer_
  if (!"BallsPre"   %in% names(d)) d$BallsPre   <- NA_integer_
  # try direct columns
  for (nm in c("StrikesBefore","StrikesBeforePitch","Strikes_BeforePitch")) if (nm %in% names(d)) d$StrikesPre <- to_int(d[[nm]])
  for (nm in c("BallsBefore","BallsBeforePitch","Balls_BeforePitch"))        if (nm %in% names(d)) d$BallsPre   <- to_int(d[[nm]])
  # reconstruct from after-pitch if still NA
  if ((all(is.na(d$StrikesPre)) || all(is.na(d$BallsPre))) &&
      all(c("Strikes","Balls") %in% names(d))) {
    d <- d %>% dplyr::group_by(PA_ID) %>% dplyr::arrange(PitchNum, .by_group = TRUE)
    d$StrikesPre <- dplyr::coalesce(d$StrikesPre, dplyr::lag(to_int(d$Strikes), default = 0L))
    d$BallsPre   <- dplyr::coalesce(d$BallsPre,   dplyr::lag(to_int(d$Balls),   default = 0L))
    d <- d %>% dplyr::ungroup()
  }
  # Infer StrikesPre from PitchCall sequence (ensures consistent 2-strike logic)
  if ("PitchCall" %in% names(d)) {
    d$StrikesPre <- compute_strikes_before(d)
  }
  d
}

# ZONE LOGIC (INCHES ONLY)
derive_zone_inches <- function(df) {
  h <- suppressWarnings(readr::parse_number(as.character(df$PlateLocHeight)))
  s <- suppressWarnings(readr::parse_number(as.character(df$PlateLocSide)))
  
  # Convert per-row: values that look like feet → inches; leave inches as-is
  h <- ifelse(is.finite(h) & h < 10, h * 12, h)
  s <- ifelse(is.finite(s) & abs(s) < 5, s * 12, s)
  
  dplyr::case_when(
    !is.finite(h) | !is.finite(s) ~ NA,
    h >= 18.29 & h <= 44.08 & s >= -9.97 & s <= 9.97 ~ TRUE,
    TRUE ~ FALSE
  )
}


# Add IsStrike/IsSwing/InZone + Barrel + 1-1 win flag
prepare_flags <- function(d) {
  d <- ensure_counts(d)
  # robust column detection (PitchCall, PitchCall.x, PitchCall_y, etc.)
  nms <- names(d)
  
  pc_col <- {
    idx <- which(tolower(nms) == "pitchcall")
    if (!length(idx)) idx <- grep("pitchcall", tolower(nms), fixed = TRUE)
    if (length(idx)) nms[idx[1]] else NULL
  }
  pr_col <- {
    idx <- which(tolower(nms) == "playresult")
    if (!length(idx)) idx <- grep("playresult", tolower(nms), fixed = TRUE)
    if (length(idx)) nms[idx[1]] else NULL
  }
  kbb_col <- {
    idx <- which(tolower(nms) == "korbb")
    if (!length(idx)) idx <- grep("korbb", tolower(nms), fixed = TRUE)
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  pc  <- if (!is.null(pc_col)) as.character(d[[pc_col]]) else rep("", nrow(d))
  pr  <- if (!is.null(pr_col)) d[[pr_col]] else NULL
  kbb <- if (!is.null(kbb_col)) d[[kbb_col]] else NULL
  
  swing_calls <- c("StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip","InPlay","InPlayOut","InPlayNoOut")
  d$IsSwing  <- pc %in% swing_calls
  d$IsStrike <- (pc == "StrikeCalled") | d$IsSwing
  
  d$InZone <- derive_zone_inches(d)
  
  # Barrel flag (global logic)
  d$Barrel <- compute_barrel_flag(d)
  
  # 1–1 win: strike on 1-1 (called, swinging or foul) → count moves to 1-2
  d$win11 <- (d$BallsPre == 1L & d$StrikesPre == 1L) &
    (pc == "StrikeCalled" | pc == "StrikeSwinging" |
       pc %in% c("FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"))
  d
}

# BARREL UTILITIES (GLOBAL)
# ---- thresholds
BARREL_EV_MIN <- 95
BARREL_LA_MIN <- 5
BARREL_LA_MAX <- 40

# ---- BIP (for rates/denominators): include all in-play variants
is_bip_for_barrel <- function(pc) {
  x <- trimws(as.character(pc))
  !is.na(x) & x %in% c("InPlay","InPlayNoOut","InPlayOut")
}

# ---- robust numeric parse
.to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))

# ---- choose EV/LA columns robustly, clamp to sane ranges
resolve_ev_la_strict <- function(d) {
  ev_candidates <- c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
  la_candidates <- c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
  ev_col <- ev_candidates[ev_candidates %in% names(d)][1]
  ev <- if (!is.na(ev_col)) .to_num(d[[ev_col]]) else rep(NA_real_, nrow(d))
  
  la_cols <- la_candidates[la_candidates %in% names(d)]
  if (!length(la_cols)) return(list(ev = ev, la = rep(NA_real_, nrow(d))))
  la_list <- lapply(la_cols, function(nm) .to_num(d[[nm]]))
  # pick the column that “looks like LA” (finite & within [-10,60])
  score <- function(v) mean(is.finite(v) & v >= -10 & v <= 60, na.rm = TRUE)
  idx <- if ("Angle" %in% la_cols && score(la_list[[which(la_cols=="Angle")]]) >= 0.25) {
    which(la_cols=="Angle")
  } else {
    which.max(vapply(la_list, score, numeric(1)))
  }
  la <- la_list[[idx]]
  list(ev = ev, la = la)
}

# ---- strict barrel gate (now counts any in-play PitchCall)
is_barrel_strict <- function(pc, ev, la) {
  evn <- .to_num(ev); lan <- .to_num(la)
  bip <- is_bip_for_barrel(pc)
  out <- bip & is.finite(evn) & is.finite(lan) &
    evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
  out[is.na(out)] <- FALSE
  out
}

# ---- EV/LA-only barrel flag (ignores PitchCall)
compute_barrel_flag_evla_only <- function(d) {
  evla <- resolve_ev_la_strict(d)
  evn <- .to_num(evla$ev); lan <- .to_num(evla$la)
  out <- is.finite(evn) & is.finite(lan) &
    evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
  out[is.na(out)] <- FALSE
  out
}

# ---- Leaderboard-style barrel flag (EV/LA + BIP from PitchCall/PlayResult) ----
compute_barrel_flag_leaderboard <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(logical(0))
  
  first_present <- function(df, ...) {
    cands <- c(...)
    hit <- cands[cands %in% names(df)]
    if (length(hit)) hit[1] else NA_character_
  }
  
  evla <- resolve_ev_la_strict(d)
  ev <- suppressWarnings(as.numeric(evla$ev))
  la <- suppressWarnings(as.numeric(evla$la))
  
  if (all(!is.finite(ev))) {
    ev_col <- first_present(d, "ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
    if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(d[[ev_col]])))
  }
  if (all(!is.finite(la))) {
    la_col <- first_present(d, "Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
    if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(d[[la_col]])))
  }
  
  pc <- as.character(d$PitchCall %||% "")
  pr <- if ("PlayResult" %in% names(d)) d$PlayResult else NULL
  bip <- safe_is_bip(pc, pr)
  
  out <- bip & is.finite(ev) & is.finite(la) &
    ev >= BARREL_EV_MIN & la >= BARREL_LA_MIN & la <= BARREL_LA_MAX
  out[is.na(out)] <- FALSE
  out
}

# ---- public helper: compute final Barrel flag everywhere
compute_barrel_flag <- function(d) {
  evla <- resolve_ev_la_strict(d)
  base <- is_barrel_strict(d$PitchCall, evla$ev, evla$la)
  .calibrate_fall25(d, base, evla$ev, evla$la)
}

# (kept for your preview pipeline)
save_pdf_preview_png <- function(pdf_path, out_png, page = 1, dpi = 150) {
  stopifnot(requireNamespace("pdftools", quietly = TRUE))
  stopifnot(requireNamespace("png", quietly = TRUE))
  conv_ok <- FALSE
  try({
    convert_pattern <- paste0(tools::file_path_sans_ext(out_png), "-%d.%s")
    out <- pdftools::pdf_convert(
      pdf_path,
      format = "png",
      dpi = dpi,
      pages = page,
      filenames = convert_pattern
    )
    if (length(out) && file.exists(out[1])) {
      file.copy(out[1], out_png, overwrite = TRUE)
      conv_ok <- TRUE
    }
  }, silent = TRUE)
  if (isTRUE(conv_ok)) return(out_png)

  bmp <- pdftools::pdf_render_page(pdf_path, page = page, dpi = dpi)
  dims <- attr(bmp, "dim")
  if (is.null(dims)) stop("pdf_render_page returned data without dimensions")

  if (is.raw(bmp)) bmp <- as.integer(bmp)
  if (is.null(dim(bmp))) {
    arr <- array(as.numeric(bmp), dim = dims)
  } else {
    arr <- bmp
  }

  if (length(dim(arr)) == 3) {
    d <- dim(arr)
    if (!(d[3] %in% 1:4)) {
      if (d[1] %in% 1:4) {
        arr <- aperm(arr, c(2, 3, 1))
      } else if (d[2] %in% 1:4) {
        arr <- aperm(arr, c(1, 3, 2))
      } else {
        arr <- arr[, , 1, drop = FALSE]
      }
    }
    if (dim(arr)[1] < dim(arr)[2]) arr <- aperm(arr, c(2, 1, 3))
  } else if (length(dim(arr)) == 2) {
    if (dim(arr)[1] < dim(arr)[2]) arr <- t(arr)
  } else {
    stop("Unexpected bitmap dimensions from pdf_render_page")
  }

  if (is.finite(max(arr, na.rm = TRUE)) && max(arr, na.rm = TRUE) > 1) {
    arr <- arr / 255
  }
  png::writePNG(arr, target = out_png)
  out_png
}

# Column helpers
col_or_na <- function(df, name, type = "dbl") {
  n <- nrow(df)
  if (name %in% names(df)) {
    v <- df[[name]]
  } else {
    v <- switch(type,
                "int" = rep(NA_integer_, n),
                "dbl" = rep(NA_real_,    n),
                "lgl" = rep(NA,          n),
                "chr" = rep(NA_character_, n),
                rep(NA, n)
    )
  }
  if (length(v) != n) v <- rep(NA, n)
  v
}

coalesce_chr <- function(df, names) {
  n <- nrow(df); out <- rep(NA_character_, n)
  for (nm in names) {
    if (nm %in% names(df)) {
      v <- as.character(df[[nm]])
      take <- is.na(out) | out == ""
      out[take] <- v[take]
    }
  }
  out
}

int_from <- function(df, names) {
  v <- coalesce_chr(df, names)
  suppressWarnings(as.integer(readr::parse_number(v)))
}

pct <- function(x) sprintf("%.1f%%", 100 * x)

shade_vs_ref <- function(value, ref, tol = 0.01) {
  if (!is.finite(value) || !is.finite(ref)) return("white")
  fill <- .severity_hex_color((value - ref) / max(abs(ref) * 0.25, 0.05))
  if (is.na(fill)) "white" else fill
}

# AAR-safe helpers (restored + de-duped)

# Any swing event (counts as a strike for Strike%)
.is_swing_event <- function(x) {
  x %in% c(
    "StrikeSwinging",
    "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip",
    "InPlay", "InPlayOut", "InPlayNoOut"
  )
}

# ---------- Usage columns helper ----------
append_usage_cols <- function(summary_df, raw_p, pitch_col = "PitchType") {
  if (is.null(raw_p) || !nrow(raw_p) || !pitch_col %in% names(summary_df)) return(summary_df)
  
  p <- prepare_aar_flags(raw_p)
  if (!"PitchType" %in% names(p)) p$PitchType <- as.character(p[[pitch_col]] %||% "Unknown")
  
  # Denominators
  tot_all  <- sum(!is.na(p$PitchType))
  tot_2k   <- sum(p$TwoStrike %in% TRUE, na.rm = TRUE)
  tot_fp   <- sum(p$FirstPitch %in% TRUE, na.rm = TRUE)  # usually == PAs
  
  # Per-pitch counts
  by_all <- p %>%
    dplyr::filter(!is.na(.data$PitchType)) %>%
    dplyr::count(.data$PitchType, name = "n_all")
  
  by_2k <- p %>%
    dplyr::filter(.data$TwoStrike %in% TRUE, !is.na(.data$PitchType)) %>%
    dplyr::count(.data$PitchType, name = "n_2k")
  
  by_fp <- p %>%
    dplyr::filter(.data$FirstPitch %in% TRUE, !is.na(.data$PitchType)) %>%
    dplyr::count(.data$PitchType, name = "n_fp")
  
  by_vs <- p %>%
    dplyr::filter(!is.na(.data$PitchType), .data$BatterSide %in% c("L","R","LHH","RHH","Left","Right")) %>%
    dplyr::mutate(BSide = dplyr::case_when(
      .data$BatterSide %in% c("L","LHH","Left")  ~ "L",
      .data$BatterSide %in% c("R","RHH","Right") ~ "R",
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(.data$BSide)) %>%
    dplyr::count(.data$PitchType, .data$BSide, name = "n_side") %>%
    tidyr::pivot_wider(names_from = .data$BSide, values_from = .data$n_side, values_fill = 0) %>%
    dplyr::rename(n_L = "L", n_R = "R")
  
  # Merge into a single usage frame keyed by PitchType
  usage <- by_all %>%
    dplyr::full_join(by_2k, by = "PitchType") %>%
    dplyr::full_join(by_fp, by = "PitchType") %>%
    dplyr::full_join(by_vs, by = "PitchType") %>%
    dplyr::mutate(
      n_all = tidyr::replace_na(.data$n_all, 0L),
      n_2k  = tidyr::replace_na(.data$n_2k,  0L),
      n_fp  = tidyr::replace_na(.data$n_fp,  0L),
      n_L   = tidyr::replace_na(.data$n_L,   0L),
      n_R   = tidyr::replace_na(.data$n_R,   0L),
      `Usage%`     = sdiv(.data$n_all, tot_all),
      `2K Usage%`  = sdiv(.data$n_2k,  tot_2k),
      `FP Usage%`  = sdiv(.data$n_fp,  tot_fp),
      `vLHH %`     = sdiv(.data$n_L,   .data$n_L + .data$n_R),
      `vRHH %`     = sdiv(.data$n_R,   .data$n_L + .data$n_R)
    )
  
  # Attach to the provided summary_df by pitch_col
  out <- summary_df %>%
    dplyr::left_join(usage %>% dplyr::rename(!!pitch_col := "PitchType"),
                     by = dplyr::join_by(!!rlang::sym(pitch_col)))
  out
}

.safe_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(readr::parse_number(as.character(x)))
}

# ---- BIP helpers (restores is_bip_from) ----
safe_is_bip <- function(pc, pr = NULL) {
  pc_chr <- trimws(as.character(pc))
  
  bip <- !is.na(pc_chr) & (
    pc_chr %in% c("InPlay","InPlayNoOut","InPlayOut") |
      grepl("^in\\s*play", pc_chr, ignore.case = TRUE)
  )
  
  if (!is.null(pr)) {
    pr_chr <- as.character(pr)
    bip <- bip | grepl(
      "in\\s*play|single|double|triple|home\\s*run|\\bhr\\b|ground|fly|line|pop|error|reach|sac",
      pr_chr,
      ignore.case = TRUE
    )
  }
  
  bip[is.na(bip)] <- FALSE
  bip
}

# Robust, vectorized helper (parses "95.2 mph", "22°", etc.)
if (!exists("is_barrel_from_row", mode = "function")) {
  is_barrel_from_row <- function(pc, pr = NULL, ev, la) {
    # Align lengths
    n <- max(length(pc), if (is.null(pr)) 0 else length(pr), length(ev), length(la))
    if (n == 0) return(logical(0))
    pc <- rep(pc, length.out = n)
    if (!is.null(pr)) pr <- rep(pr, length.out = n)
    evn <- suppressWarnings(readr::parse_number(as.character(ev)))
    lan <- suppressWarnings(readr::parse_number(as.character(la)))
    
    bip <- is_bip_from(pc, pr)
    out <- bip &
      is.finite(evn) & is.finite(lan) &
      (evn >= BARREL_EV_MIN) &
      (lan >= BARREL_LA_MIN) & (lan <= BARREL_LA_MAX)
    
    out[is.na(out)] <- FALSE
    out
  }
}

if (!exists("is_bip_from", mode = "function")) {
  is_bip_from <- function(pc, pr = NULL) {
    safe_is_bip(pc, pr)
  }
}

if (!exists("resolve_ev_la", mode = "function")) {
  resolve_ev_la <- function(d) {
    ev_candidates <- c(
      "ExitSpeed","ExitVelocity","ExitVel",
      "Exit Velocity","Exit Velo",
      "HitSpeed","BallExitSpeed","Ball Exit Speed",
      "EV","EV_mph","EV (mph)"
    )
    la_candidates <- c(
      "Angle","LaunchAngle","LA",
      "Launch_Angle","Launch.Angle","Launch Angle",
      "LAdeg","LA (deg)"
    )
    
    pick <- function(cands) {
      hits <- intersect(cands, names(d))
      if (length(hits)) hits[1] else NA_character_
    }
    
    evc <- pick(ev_candidates)
    lac <- pick(la_candidates)
    
    ev  <- if (!is.na(evc)) suppressWarnings(readr::parse_number(as.character(d[[evc]]))) else rep(NA_real_, nrow(d))
    la  <- if (!is.na(lac)) suppressWarnings(readr::parse_number(as.character(d[[lac]]))) else rep(NA_real_, nrow(d))
    list(ev = ev, la = la)
  }
}

is_barrel_pa <- function(pa_df) {
  if (!nrow(pa_df)) return(FALSE)
  evla <- resolve_ev_la_strict(pa_df)
  any(is_barrel_strict(pa_df$PitchCall, evla$ev, evla$la), na.rm = TRUE)
}

# --- Back-compat aliases for older dotted helpers ---
if (!exists(".is_barrel_pa", inherits = FALSE)) .is_barrel_pa <- is_barrel_pa
if (!exists(".resolve_ev_la", inherits = FALSE)) .resolve_ev_la <- resolve_ev_la

compute_strikes_before <- function(d) {
  # Always ensure PA scaffolding exists before using PA_ID / PitchNum
  d <- ensure_pa(d)
  
  d %>%
    dplyr::arrange(PA_ID, PitchNum) %>%
    dplyr::group_by(PA_ID) %>%
    dplyr::mutate(
      StrikesPre_calc = {
        pc  <- tolower(as.character(PitchCall))
        out <- integer(dplyr::n()); s <- 0L
        for (i in seq_along(out)) {
          out[i] <- s
          if (pc[i] %in% c("strikecalled","strikeswinging")) {
            s <- min(2L, s + 1L)
          } else if (pc[i] %in% c("foulball","foulballfieldable","foulballnotfieldable","foultip")) {
            if (s < 2L) s <- s + 1L
          }
        }
        out
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(StrikesPre_calc)
}

prepare_aar_flags <- function(d) {
  d <- tibble::as_tibble(d)
  d$InZone <- derive_zone_inches(d)
  
  # Count scaffolding + strike/swing flags + Barrel + 1-1 win
  d <- prepare_flags(d)
  
  # TwoStrike convenience
  d$TwoStrike <- is.finite(d$StrikesPre) & d$StrikesPre == 2L
  
  # HBP flag from text columns (for E&A eligibility)
  pc <- as.character(d$PitchCall %||% "")
  hbp <- rep(FALSE, nrow(d))
  if ("PlayResult" %in% names(d)) hbp <- hbp | grepl("(?i)hit by pitch|\\bHBP\\b", as.character(d$PlayResult))
  if ("KorBB"      %in% names(d)) hbp <- hbp | grepl("(?i)\\bHBP\\b",               as.character(d$KorBB))
  hbp <- hbp | pc %in% c("HitByPitch")
  d$HBP <- hbp
  
  d
}

# Canonical E&A rule:
# - Success if the PA is Early OR Ahead
# - Early: ball in play within 3 pitches, excluding HBP/barrel PAs
# - Ahead: 2 of the first 3 pitches are strikes
# - Any barrel or HBP in the first 3 pitches makes the PA an E&A fail
# - Denominator: every PA
calc_ea_success <- function(n_pitches, strikes_first3, early_bip, any_hbp, any_barrel) {
  n_pitches <- suppressWarnings(as.numeric(n_pitches))
  strikes_first3 <- suppressWarnings(as.numeric(strikes_first3))
  early_bip <- early_bip %in% TRUE
  any_hbp <- any_hbp %in% TRUE
  any_barrel <- any_barrel %in% TRUE
  
  valid <- is.finite(n_pitches) & n_pitches > 0
  disqualifying_first3_event <- any_hbp | any_barrel
  early <- valid & early_bip & !disqualifying_first3_event
  ahead <- is.finite(strikes_first3) & strikes_first3 >= 2 & !disqualifying_first3_event
  
  success <- valid & (early | ahead)
  success[is.na(success)] <- FALSE
  success
}

calc_ea_pa_summary <- function(d, pa_cols = "PA_ID") {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(tibble::tibble())
  if (!all(pa_cols %in% names(d)) || !"PitchNum" %in% names(d)) d <- ensure_pa(d)
  if (!all(pa_cols %in% names(d))) return(tibble::tibble())
  
  pc <- if (".pc" %in% names(d)) as.character(d$.pc) else as.character(d$PitchCall %||% "")
  pr <- if ("PlayResult" %in% names(d)) d$PlayResult else NULL
  pitch_num <- suppressWarnings(as.numeric(d$PitchNum))
  strike_flag <- if ("IsStrike" %in% names(d)) {
    d$IsStrike %in% TRUE
  } else if (".strike" %in% names(d)) {
    d$.strike %in% TRUE
  } else {
    pc %in% c("StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
  }
  hbp_flag <- if ("HBP" %in% names(d)) {
    d$HBP %in% TRUE
  } else {
    hbp <- rep(FALSE, nrow(d))
    if ("PlayResult" %in% names(d)) hbp <- hbp | grepl("(?i)hit by pitch|\\bHBP\\b", as.character(d$PlayResult))
    if ("KorBB" %in% names(d))      hbp <- hbp | grepl("(?i)\\bHBP\\b", as.character(d$KorBB))
    hbp | pc %in% "HitByPitch"
  }
  barrel_flag <- if ("Barrel" %in% names(d)) {
    d$Barrel %in% TRUE
  } else if (".barrel" %in% names(d)) {
    d$.barrel %in% TRUE
  } else {
    compute_barrel_flag(d)
  }
  bip_flag <- if (".bip" %in% names(d)) d$.bip %in% TRUE else safe_is_bip(pc, pr)
  
  d %>%
    dplyr::mutate(
      .ea_pitch_num = pitch_num,
      .ea_strike = strike_flag,
      .ea_hbp = hbp_flag,
      .ea_barrel = barrel_flag,
      .ea_bip = bip_flag
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(pa_cols))) %>%
    dplyr::summarise(
      n_pitches = {
        pn <- .data$.ea_pitch_num
        if (any(is.finite(pn))) max(pn[is.finite(pn)], na.rm = TRUE) else dplyr::n()
      },
      strikes_first3 = sum(.data$.ea_strike & is.finite(.data$.ea_pitch_num) & .data$.ea_pitch_num <= 3, na.rm = TRUE),
      early_bip = any(.data$.ea_bip & is.finite(.data$.ea_pitch_num) & .data$.ea_pitch_num <= 3, na.rm = TRUE),
      any_hbp = any(.data$.ea_hbp & is.finite(.data$.ea_pitch_num) & .data$.ea_pitch_num <= 3, na.rm = TRUE),
      any_barrel = any(.data$.ea_barrel & is.finite(.data$.ea_pitch_num) & .data$.ea_pitch_num <= 3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(.data$n_pitches) & .data$n_pitches > 0)
}

calc_ea_rate_from_pa <- function(pa) {
  pa <- tibble::as_tibble(pa)
  if (!nrow(pa) || !"n_pitches" %in% names(pa) || !"strikes_first3" %in% names(pa)) {
    return(NA_real_)
  }
  
  early_bip <- if ("early_bip" %in% names(pa)) pa$early_bip else rep(FALSE, nrow(pa))
  any_hbp <- if ("any_hbp" %in% names(pa)) pa$any_hbp else rep(FALSE, nrow(pa))
  any_barrel <- if ("any_barrel" %in% names(pa)) {
    pa$any_barrel
  } else if ("any_bar" %in% names(pa)) {
    pa$any_bar
  } else {
    rep(FALSE, nrow(pa))
  }
  
  valid <- is.finite(suppressWarnings(as.numeric(pa$n_pitches))) &
    suppressWarnings(as.numeric(pa$n_pitches)) > 0
  success <- calc_ea_success(
    n_pitches = pa$n_pitches,
    strikes_first3 = pa$strikes_first3,
    early_bip = early_bip,
    any_hbp = any_hbp,
    any_barrel = any_barrel
  )
  
  sdiv(sum(success, na.rm = TRUE), sum(valid, na.rm = TRUE))
}

# Put Away% helpers: ensure last pitch per PA is correctly identified
calc_is_k_pitch <- function(d, pa_id_col = "PA_ID", pitchnum_col = "PitchNum") {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(logical(0))
  if (!pa_id_col %in% names(d)) {
    d <- ensure_pa(d)
    pa_id_col <- "PA_ID"
  }
  if (!pitchnum_col %in% names(d)) {
    d <- ensure_pa(d)
    pitchnum_col <- "PitchNum"
  }
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(logical(0))
  d$.row_id_internal <- seq_len(nrow(d))
  d2 <- d %>%
    dplyr::arrange(.data[[pa_id_col]], .data[[pitchnum_col]])
  pa_last <- d2 %>%
    dplyr::group_by(.data[[pa_id_col]]) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  outc <- pa_outcome_cols(pa_last)
  k_pa_ids <- pa_last[[pa_id_col]][outc$K %in% TRUE]
  last_key <- paste(pa_last[[pa_id_col]], pa_last[[pitchnum_col]])
  this_key <- paste(d2[[pa_id_col]], d2[[pitchnum_col]])
  is_k_pitch_sorted <- (this_key %in% last_key) & (d2[[pa_id_col]] %in% k_pa_ids)
  is_k_pitch <- rep(FALSE, nrow(d))
  is_k_pitch[d2$.row_id_internal] <- is_k_pitch_sorted
  is_k_pitch
}

calc_put_away_rate <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(NA_real_)
  # Standardize 2-strike count everywhere from PitchCall sequence
  if ("PitchCall" %in% names(d)) {
    d$StrikesPre <- compute_strikes_before(d)
  }
  twok <- !is.na(d$StrikesPre) & d$StrikesPre == 2
  is_k_pitch <- calc_is_k_pitch(d)
  put_den <- sum(twok, na.rm = TRUE)
  if (put_den <= 0) return(NA_real_)
  sdiv(sum(twok & is_k_pitch, na.rm = TRUE), put_den)
}

# ---------- PROCESS METRICS (game / season) ----------
.compute_process_metrics <- function(p) {
  p <- tibble::as_tibble(p)
  p <- prepare_aar_flags(p) 
  
  if (!nrow(p)) {
    return(tibble::tibble(
      `Strike%`=NA_real_, `Zone%`=NA_real_, `FPS%`=NA_real_,
      `Pre2K Zone%`=NA_real_, `E&A%`=NA_real_, `Put Away%`=NA_real_
    ))
  }
  
  # Pitch-level rates
  n_all <- nrow(p)
  strike <- sdiv(sum(p$IsStrike, na.rm = TRUE), n_all)
  zone_den <- sum(!is.na(p$InZone), na.rm = TRUE)
  zone   <- sdiv(sum(p$InZone,   na.rm = TRUE), zone_den)
  fps    <- sdiv(sum(p$IsStrike & p$FirstPitch, na.rm = TRUE),
                 sum(p$FirstPitch, na.rm = TRUE))
  
  pre2k_den <- sum(!p$TwoStrike & !is.na(p$InZone), na.rm = TRUE)
  pre2k_z   <- sdiv(sum(p$InZone & !p$TwoStrike, na.rm = TRUE), pre2k_den)

  # PA-level E&A: every PA is in the denominator; numerator is Ahead or eligible Early action
  pa <- calc_ea_pa_summary(p, "PA_ID")
  
  ea <- calc_ea_rate_from_pa(pa)

  # Put Away%: 2-strike pitches that end in a K
  put_away <- calc_put_away_rate(p)
  
  tibble::tibble(
    `Strike%`     = strike,
    `Zone%`       = zone,
    `FPS%`        = fps,
    `Pre2K Zone%` = pre2k_z,
    `E&A%`        = ea,
    `Put Away%`   = put_away
  )
}

# ---------- PROCESS TABLE (Game / Season / D1 ref) — returns a plain data.frame ----------
build_process_table <- function(game_p, season_p, season_header = "Season") {
  # Ensure flags/columns exist
  g <- prepare_aar_flags(tibble::as_tibble(game_p))
  s <- prepare_aar_flags(tibble::as_tibble(season_p))
  
  # helper: compute rates on a flagged pitch-level data.frame
  compute_rates <- function(d) {
    if (!nrow(d)) {
      return(list(strike=NA_real_, zone=NA_real_, fps=NA_real_, pre2k=NA_real_, ea=NA_real_, put_away=NA_real_))
    }
    n_all <- nrow(d)
    strike <- sdiv(sum(d$IsStrike, na.rm = TRUE), n_all)
    zone_den <- sum(!is.na(d$InZone), na.rm = TRUE)
    zone   <- sdiv(sum(d$InZone,   na.rm = TRUE), zone_den)
    fps    <- sdiv(sum(d$IsStrike & d$FirstPitch, na.rm = TRUE),
                   sum(d$FirstPitch, na.rm = TRUE))
    
    pre2k_den <- sum(!d$TwoStrike & !is.na(d$InZone), na.rm = TRUE)
    pre2k     <- sdiv(sum(d$InZone & !d$TwoStrike, na.rm = TRUE), pre2k_den)
    
    # ---- PA-level E&A%: every PA is in the denominator; numerator is Ahead or eligible Early action
    pa <- calc_ea_pa_summary(d, "PA_ID")
    
    ea <- calc_ea_rate_from_pa(pa)

    # Put Away%: 2-strike pitches that end in a K
    put_away <- calc_put_away_rate(d)
    
    list(strike=strike, zone=zone, fps=fps, pre2k=pre2k, ea=ea, put_away=put_away)
  }
  
  G <- compute_rates(g)
  S <- compute_rates(s)
  
  # D1 reference (NO 2K Zone% in this table)
  d1 <- list(
    strike     = D1_REF$strike_pct,
    zone       = D1_REF$zone_pct,
    fps        = D1_REF$fps_pct,
    pre2k_zone = D1_REF$pre2k_zone,
    ea         = D1_REF$ea_pct,
    put_away   = D1_REF$put_away_pct
  )
  
  out <- tibble::tibble(
    Metric = c("Strike%","Zone%","FPS%","Pre2K Zone%","E&A%","Put Away%"),
    Game   = c(G$strike, G$zone, G$fps, G$pre2k, G$ea, G$put_away),
    Season = c(S$strike, S$zone, S$fps, S$pre2k, S$ea, S$put_away),
    D1     = c(d1$strike, d1$zone, d1$fps, d1$pre2k_zone, d1$ea, d1$put_away)
  ) %>%
    dplyr::mutate(
      Game   = ifelse(is.finite(Game),   scales::percent(Game,   accuracy = 0.1), "—"),
      Season = ifelse(is.finite(Season), scales::percent(Season, accuracy = 0.1), "—"),
      D1     = ifelse(is.finite(D1),     scales::percent(D1,     accuracy = 0.1), "—")
    )
  
  attr(out, "season_header") <- season_header
  
  as.data.frame(out)
}

# Pitch Type Performance (Game + Season) — returns a plain data.frame with hidden numeric columns
build_pitchtype_perf_table <- function(game_p, season_p = NULL) {
  g <- prepare_aar_flags(game_p)
  if (!nrow(g)) {
    return(as.data.frame(tibble::tibble(`Status` = "No data")))
  }
  s <- if (!is.null(season_p) && nrow(season_p)) prepare_aar_flags(season_p) else g[0, , drop = FALSE]
  
  summarize_pt <- function(d) {
    d %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Strike  = sdiv(sum(IsStrike, na.rm = TRUE), dplyr::n()),
        Pre2K   = { den <- sum(!TwoStrike & !is.na(InZone), na.rm = TRUE); sdiv(sum(InZone & !TwoStrike, na.rm = TRUE), den) },
        Whiff   = { sw  <- sum(IsSwing,          na.rm = TRUE); sdiv(sum(PitchCall == "StrikeSwinging", na.rm = TRUE), sw) },
        IZWhiff = { swi <- sum(IsSwing & InZone, na.rm = TRUE); sdiv(sum(PitchCall == "StrikeSwinging" & InZone, na.rm = TRUE), swi) },
        .groups = "drop"
      )
  }
  
  g_sum <- summarize_pt(g)
  s_sum <- summarize_pt(s)
  
  out <- g_sum %>%
    dplyr::left_join(s_sum, by = "PitchType", suffix = c("_g", "_s")) %>%
    dplyr::arrange(dplyr::desc(.data$Strike_g))
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
  fmt_dual <- function(gv, sv) paste0(fmt_pct(gv), " | ", fmt_pct(sv))
  
  out_disp <- out %>%
    dplyr::transmute(
      PitchType,
      `Strike%`       = fmt_dual(Strike_g,  Strike_s),
      `Pre2K\nZone%`  = fmt_dual(Pre2K_g,   Pre2K_s),
      `Whiff%`        = fmt_dual(Whiff_g,   Whiff_s),
      `IZ\nWhiff%`    = fmt_dual(IZWhiff_g, IZWhiff_s),
      .g_Strike   = Strike_g,
      .s_Strike   = Strike_s,
      .g_Pre2K    = Pre2K_g,
      .s_Pre2K    = Pre2K_s,
      .g_Whiff    = Whiff_g,
      .s_Whiff    = Whiff_s,
      .g_IZWhiff  = IZWhiff_g,
      .s_IZWhiff  = IZWhiff_s
    )
  
  as.data.frame(out_disp)
}

# ---------- TEAM REPORT helpers ----------
pitch_group_from_type <- function(pt) {
  p <- tolower(trimws(as.character(pt %||% "")))
  if (p %in% c("fastball","four-seam","four seam","two-seam","two seam","four-seam fastball","two-seam fastball")) return("Fastball")
  if (p %in% c("sinker")) return("Sinker")
  if (p %in% c("cutter","slider","curveball","curve ball","sweeper")) return("Breaking")
  if (p %in% c("changeup","change up","splitter")) return("Off Speed")
  NA_character_
}

# ---- Self-scouting pitch type helpers ----
SELF_SCOUT_PITCH_LEVELS <- c("Fastball","Sinker","Cutter","Slider","Sweeper","Curveball","Changeup","Splitter")

self_scout_pitch_type <- function(pt) {
  p <- tolower(trimws(as.character(pt %||% "")))
  if (p %in% c("fastball","four-seam","four seam","four-seam fastball",
              "two-seam","two seam","two-seam fastball","2-seam","4-seam")) return("Fastball")
  if (p %in% c("sinker")) return("Sinker")
  if (p %in% c("cutter","cut fastball","cut")) return("Cutter")
  if (p %in% c("slider")) return("Slider")
  if (p %in% c("sweeper")) return("Sweeper")
  if (p %in% c("curveball","curve ball","curve")) return("Curveball")
  if (p %in% c("changeup","change up","change-up")) return("Changeup")
  if (p %in% c("splitter","split-finger","split finger","split")) return("Splitter")
  NA_character_
}

self_scout_usage_group <- function(pt) {
  ct <- if (pt %in% SELF_SCOUT_PITCH_LEVELS) pt else self_scout_pitch_type(pt)
  if (ct %in% c("Fastball","Sinker")) return("Hard")
  if (ct %in% c("Cutter","Slider","Sweeper","Curveball")) return("Breaking")
  if (ct %in% c("Changeup","Splitter")) return("Soft")
  NA_character_
}

self_scout_loc_group <- function(pt) {
  ct <- if (pt %in% SELF_SCOUT_PITCH_LEVELS) pt else self_scout_pitch_type(pt)
  if (ct %in% c("Fastball")) return("Fastball")
  if (ct %in% c("Sinker")) return("Sinker")
  if (ct %in% c("Cutter","Slider")) return("Cutter & Slider")
  if (ct %in% c("Curveball","Sweeper")) return("Curveball & Sweeper")
  if (ct %in% c("Changeup","Splitter")) return("Changeup & Splitter")
  NA_character_
}

coerce_barrel_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x > 0)
  v <- tolower(trimws(as.character(x)))
  v %in% c("1","true","t","yes","y","barrel","barrels")
}

build_team_pitchtype_perf_table <- function(game_p, season_p = NULL) {
  g <- prepare_aar_flags(game_p)
  if (!nrow(g)) {
    return(as.data.frame(tibble::tibble(`Status` = "No data")))
  }
  s <- if (!is.null(season_p) && nrow(season_p)) prepare_aar_flags(season_p) else g[0, , drop = FALSE]
  
  map_groups <- function(d) {
    d %>%
      dplyr::mutate(PitchGroup = vapply(PitchType, pitch_group_from_type, character(1))) %>%
      dplyr::filter(!is.na(PitchGroup))
  }
  g <- map_groups(g)
  s <- map_groups(s)
  if (!nrow(g)) {
    return(as.data.frame(tibble::tibble(`Status` = "No data")))
  }
  
  summarize_group <- function(d) {
    d %>%
      dplyr::group_by(PitchGroup) %>%
      dplyr::summarise(
        Strike  = sdiv(sum(IsStrike, na.rm = TRUE), dplyr::n()),
        Pre2K   = { den <- sum(!TwoStrike & !is.na(InZone), na.rm = TRUE); sdiv(sum(InZone & !TwoStrike, na.rm = TRUE), den) },
        Whiff   = { sw  <- sum(IsSwing,          na.rm = TRUE); sdiv(sum(PitchCall == "StrikeSwinging", na.rm = TRUE), sw) },
        IZWhiff = { swi <- sum(IsSwing & InZone, na.rm = TRUE); sdiv(sum(PitchCall == "StrikeSwinging" & InZone, na.rm = TRUE), swi) },
        .groups = "drop"
      )
  }
  
  g_sum <- summarize_group(g)
  s_sum <- summarize_group(s)
  
  out <- g_sum %>%
    dplyr::left_join(s_sum, by = "PitchGroup", suffix = c("_g", "_s")) %>%
    dplyr::mutate(PitchGroup = factor(PitchGroup, levels = c("Fastball","Sinker","Breaking","Off Speed"))) %>%
    dplyr::arrange(PitchGroup)
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
  fmt_dual <- function(gv, sv) paste0(fmt_pct(gv), " | ", fmt_pct(sv))
  
  out_disp <- out %>%
    dplyr::transmute(
      PitchType       = as.character(PitchGroup),
      `Strike%`       = fmt_dual(Strike_g,  Strike_s),
      `Pre2k Zone%`   = fmt_dual(Pre2K_g,   Pre2K_s),
      `Whiff%`        = fmt_dual(Whiff_g,   Whiff_s),
      `IZ Whiff%`     = fmt_dual(IZWhiff_g, IZWhiff_s),
      .g_Strike   = Strike_g,
      .s_Strike   = Strike_s,
      .g_Pre2K    = Pre2K_g,
      .s_Pre2K    = Pre2K_s,
      .g_Whiff    = Whiff_g,
      .s_Whiff    = Whiff_s,
      .g_IZWhiff  = IZWhiff_g,
      .s_IZWhiff  = IZWhiff_s
    )
  
  as.data.frame(out_disp)
}

build_count_breakdown_table <- function(game_p, season_p, season_header = "Season") {
  counts <- c("0-0","0-1","1-0","1-1","0-2","2-0","2-1","1-2","2-2","3-0","3-1","3-2")
  
  calc_metrics <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) {
      empty <- setNames(rep(NA_real_, length(counts)), counts)
      return(list(whiff = empty, barrel = empty, weak = empty))
    }
    d <- ensure_counts(d)
    d <- prepare_aar_flags(d)
    barrel_flag <- compute_barrel_flag_leaderboard(d)
    barrel_ok <- barrel_date_ok(d)
    
    to_int <- function(x) suppressWarnings(as.integer(readr::parse_number(as.character(x))))
    b <- to_int(d$BallsPre)
    s <- to_int(d$StrikesPre)
    # Fallback to post-pitch counts when pre-pitch counts are missing
    if ("Balls" %in% names(d))  b <- ifelse(is.na(b), to_int(d$Balls),   b)
    if ("Strikes" %in% names(d)) s <- ifelse(is.na(s), to_int(d$Strikes), s)
    cnt <- paste0(b, "-", s)
    
    pc <- as.character(d$PitchCall %||% "")
    pr <- if ("PlayResult" %in% names(d)) d$PlayResult else NULL
    swings <- if ("IsSwing" %in% names(d)) d$IsSwing else .is_swing_event(pc)
    whiffs <- pc == "StrikeSwinging"
    
    barrel_flag <- coerce_barrel_flag(barrel_flag)
    
    bip_base <- safe_is_bip(pc, pr)
    
    # Resolve EV/LA with the same fallback logic used by leaderboard barrels
    first_present <- function(df, ...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }
    evla <- resolve_ev_la_strict(d)
    ev <- suppressWarnings(as.numeric(evla$ev))
    la <- suppressWarnings(as.numeric(evla$la))
    if (all(!is.finite(ev))) {
      ev_col <- first_present(d, "ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
      if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(d[[ev_col]])))
    }
    if (all(!is.finite(la))) {
      la_col <- first_present(d, "Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
      if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(d[[la_col]])))
    }
    ev_ok <- is.finite(ev)
    la_ok <- is.finite(la)
    # treat EV/LA presence as BIP when PlayResult is missing/ambiguous
    bip_base <- bip_base | (ev_ok & la_ok)
    bip_ev   <- bip_base & ev_ok
    bip_evla <- bip_base & ev_ok & la_ok
    weak_contact <- bip_ev & ev <= 85
    
    out_whiff <- setNames(rep(NA_real_, length(counts)), counts)
    out_barrel <- setNames(rep(NA_real_, length(counts)), counts)
    out_weak <- setNames(rep(NA_real_, length(counts)), counts)
    
    for (c in counts) {
      idx <- which(cnt == c)
      if (!length(idx)) next
      
      sw <- sum(swings[idx], na.rm = TRUE)
      if (sw > 0) out_whiff[c] <- sum(whiffs[idx], na.rm = TRUE) / sw
      
      idx_bar <- idx[barrel_ok[idx]]
      bp_barrel <- sum(bip_evla[idx_bar], na.rm = TRUE)
      if (bp_barrel > 0) {
        out_barrel[c] <- sum(barrel_flag[idx_bar], na.rm = TRUE) / bp_barrel
      }
      
      bp_weak <- sum(bip_ev[idx], na.rm = TRUE)
      if (bp_weak > 0) {
        out_weak[c]   <- sum(weak_contact[idx], na.rm = TRUE) / bp_weak
      }
    }
    
    list(whiff = out_whiff, barrel = out_barrel, weak = out_weak)
  }
  
  g <- calc_metrics(game_p)
  s <- calc_metrics(season_p)
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "—")
  season_label <- season_header %||% "Season"
  
  out <- data.frame(
    Metric = c(
      "Game Whiff%",
      paste0(season_label, " Whiff%"),
      "Game Barrel%",
      paste0(season_label, " Barrel%"),
      "Game Weak Contact%",
      paste0(season_label, " Weak Contact%")
    ),
    rbind(
      fmt_pct(g$whiff),
      fmt_pct(s$whiff),
      fmt_pct(g$barrel),
      fmt_pct(s$barrel),
      fmt_pct(g$weak),
      fmt_pct(s$weak)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out)[1] <- ""
  
  out
}

add_outcome_type <- function(p_sub, allow_barrel_any = FALSE) {
  p_sub <- tibble::as_tibble(p_sub)
  ball_calls <- c(
    "Ball","BallCalled","BallInDirt","AutomaticBall",
    "IntentionalBall","IntentBall","PitchOut"
  )
  barrel_from_col <- NULL
  if ("Barrel" %in% names(p_sub) && any(!is.na(p_sub$Barrel))) {
    barrel_from_col <- coerce_barrel_flag(p_sub$Barrel)
  }
  evla <- resolve_ev_la_strict(p_sub)
  is_barrel <- if (!is.null(barrel_from_col)) {
    barrel_from_col
  } else {
    is_barrel_strict(p_sub$PitchCall, evla$ev, evla$la)
  }
  p_sub %>%
    dplyr::mutate(
      EV = evla$ev,
      LA = evla$la,
      IsBarrel = is_barrel,
      PitchCall_chr = as.character(PitchCall),
      OutcomeType = dplyr::case_when(
        allow_barrel_any & IsBarrel ~ "Barrel",
        PitchCall_chr %in% ball_calls ~ "Ball",
        PitchCall_chr == "StrikeCalled" ~ "Called Strike",
        PitchCall_chr %in% c("FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip") ~ "Foul",
        PitchCall_chr %in% c("InPlay","InPlayOut","InPlayNoOut") & IsBarrel ~ "Barrel",
        PitchCall_chr %in% c("InPlay","InPlayOut","InPlayNoOut") ~ "Ball in Play",
        PitchCall_chr == "StrikeSwinging" ~ "Swing & Miss",
        TRUE ~ "Other"
      ),
      OutcomeType = factor(
        OutcomeType,
        levels = c("Ball","Called Strike","Foul","Ball in Play","Swing & Miss","Barrel","Other")
      )
    )
}

team_statline_bits <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) {
    return(list(PA = 0L, K = 0L, BB = 0L, HBP = 0L, Barrel = 0L, H = 0L))
  }
  d <- prepare_aar_flags(d)
  barrel_flag <- compute_barrel_flag_leaderboard(d)
  if (!"PA_ID" %in% names(d)) d <- ensure_pa(d)
  if (!"KorBB" %in% names(d)) d$KorBB <- NA_character_
  if (!"PlayResult" %in% names(d)) d$PlayResult <- NA_character_
  if (!"HBP" %in% names(d)) d$HBP <- FALSE
  
  safe_last_nonempty <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & trimws(x) != ""]
    if (length(x)) x[[length(x)]] else NA_character_
  }
  
  pa_end <- d %>%
    dplyr::group_by(PA_ID) %>%
    dplyr::summarise(
      KorBB_last = safe_last_nonempty(KorBB),
      PR_last    = safe_last_nonempty(PlayResult),
      any_hbp    = any(HBP, na.rm = TRUE),
      .groups = "drop"
    )
  
  kb_last_trim <- tolower(trimws(as.character(pa_end$KorBB_last)))
  pr_last_trim <- tolower(trimws(as.character(pa_end$PR_last)))
  
  kor_is_walk <- grepl("\\b(bb|ibb|walk)\\b", kb_last_trim)
  pr_is_walk  <- grepl("\\bintentional\\b|\\bwalk\\b", pr_last_trim)
  bb_n <- sum(kor_is_walk, na.rm = TRUE) + sum(!kor_is_walk & pr_is_walk, na.rm = TRUE)
  
  kor_is_hbp <- grepl("\\bhbp\\b|hit by pitch", kb_last_trim)
  pr_is_hbp  <- grepl("\\bhbp\\b|hit by pitch", pr_last_trim)
  hbp_n <- sum(kor_is_hbp | pr_is_hbp | pa_end$any_hbp, na.rm = TRUE)
  
  k_n <- sum(
    grepl("\\bK\\b|strike.?out", pa_end$KorBB_last, ignore.case = TRUE) |
      grepl("strike.?out|\\bK\\b", pa_end$PR_last, ignore.case = TRUE),
    na.rm = TRUE
  )
  
  hit_n <- sum(
    grepl("home ?run|\\bHR\\b", pa_end$PR_last, ignore.case = TRUE) |
      grepl("\\btriple\\b",      pa_end$PR_last, ignore.case = TRUE) |
      (grepl("\\bdouble\\b",     pa_end$PR_last, ignore.case = TRUE) &
         !grepl("double\\s*play", pa_end$PR_last, ignore.case = TRUE)) |
      grepl("\\bsingle\\b",      pa_end$PR_last, ignore.case = TRUE),
    na.rm = TRUE
  )
  
  # Barrels counted at pitch-level (matches leaderboard barrel logic)
  barrel_n <- sum(barrel_flag, na.rm = TRUE)
  
  list(
    PA     = nrow(pa_end),
    K      = as.integer(k_n),
    BB     = as.integer(bb_n),
    HBP    = as.integer(hbp_n),
    Barrel = as.integer(barrel_n),
    H      = as.integer(hit_n)
  )
}

# --- SAFE cell helpers (return the mutated table or the original on error) ---
.tbl_bg <- function(tbl, ...) {
  res <- try(ggpubr::table_cell_bg(tbl, ...), silent = TRUE)
  if (inherits(res, "try-error")) tbl else res
}
.tbl_font <- function(tbl, ...) {
  res <- try(ggpubr::table_cell_font(tbl, ...), silent = TRUE)
  if (inherits(res, "try-error")) tbl else res
}

# ---------- MOVEMENT & USAGE TABLE  ----------
build_movement_usage_table <- function(game_p) {
  p <- tibble::as_tibble(game_p)
  
  # Early out
  if (!nrow(p)) return(as.data.frame(tibble::tibble(Status = "No pitches in game")))
  
  # Normalize PitchType and drop NA/blank
  p$PitchType <- as.character(p$PitchType %||% NA_character_)
  p$PitchType[!nzchar(p$PitchType)] <- NA_character_
  p <- p %>% dplyr::filter(!is.na(PitchType))
  if (!nrow(p)) return(as.data.frame(tibble::tibble(Status = "No valid PitchType values")))
  
  # Ensure PA scaffolding & flags
  p <- ensure_pa(p)
  if (!"FirstPitch" %in% names(p)) p$FirstPitch <- p$PitchNum == 1L
  
  # Normalize BatterSide locally
  p$BatterSide <- dplyr::case_when(
    p$BatterSide %in% c("L","Left","LHH","LH") ~ "L",
    p$BatterSide %in% c("R","Right","RHH","RH") ~ "R",
    TRUE ~ NA_character_
  )
  
  # Two-strike BEFORE pitch
  strikes_pre <- int_from(p, c("StrikesBeforePitch","StrikesPre","StrikesBefore"))
  if (all(is.na(strikes_pre))) strikes_pre <- compute_strikes_before(p)
  if (all(is.na(strikes_pre)) && "Strikes" %in% names(p)) {
    s_raw <- suppressWarnings(as.integer(p$Strikes))
    strikes_pre <- p %>%
      dplyr::mutate(.s = s_raw) %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::mutate(.s = dplyr::lag(.s, default = 0L)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(.s)
  }
  p$TwoStrike <- is.finite(strikes_pre) & (strikes_pre >= 2L)
  
  # Global denominators
  tot_all <- nrow(p)
  tot_fp  <- dplyr::n_distinct(p$PA_ID[p$FirstPitch %in% TRUE])
  if (tot_fp == 0) tot_fp <- dplyr::n_distinct(p$PA_ID)
  tot_2k  <- sum(p$TwoStrike, na.rm = TRUE)
  
  # Handedness denominators (for column-wise shares)
  tot_L <- sum(p$BatterSide == "L", na.rm = TRUE)
  tot_R <- sum(p$BatterSide == "R", na.rm = TRUE)
  
  # Summarise per pitch type
  by_pt <- p %>%
    dplyr::group_by(PitchType) %>%
    dplyr::summarise(
      n_all = dplyr::n(),
      n_fp  = sum(FirstPitch, na.rm = TRUE),
      n_2k  = sum(TwoStrike,  na.rm = TRUE),
      n_L   = sum(BatterSide == "L", na.rm = TRUE),
      n_R   = sum(BatterSide == "R", na.rm = TRUE),
      Velo  = mean(suppressWarnings(as.numeric(RelSpeed)),           na.rm = TRUE),
      IVB   = mean(suppressWarnings(as.numeric(InducedVertBreak)),   na.rm = TRUE),
      HB    = mean(suppressWarnings(as.numeric(HorzBreak)),          na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!nrow(by_pt)) return(as.data.frame(tibble::tibble(Status = "No pitch-type rows to summarise")))
  # --- Add max velo by pitch type for this game (from the same pitch-level df `p`) ---
  velonm <- intersect(names(p), c("RelSpeed", "ReleaseSpeed", "Velo"))[1]
  
  velo_max_by_pt <- p %>%
    dplyr::group_by(PitchType) %>%
    dplyr::summarise(
      VeloMax = {
        if (is.na(velonm)) NA_real_
        else {
          vv <- suppressWarnings(as.numeric(.data[[velonm]]))
          if (any(is.finite(vv))) max(vv, na.rm = TRUE) else NA_real_
        }
      },
      .groups = "drop"
    )
  
  out <- by_pt %>%
    dplyr::left_join(velo_max_by_pt, by = "PitchType") %>%
    dplyr::mutate(
      `Usage%`    = if (tot_all > 0) n_all / tot_all else NA_real_,
      `FP Usage%` = if (tot_fp  > 0) n_fp  / tot_fp  else NA_real_,
      `2K Usage%` = if (tot_2k  > 0) n_2k  / tot_2k  else NA_real_,
      # Column-wise distributions (sum to 100% down the column)
      `vLHH %`    = if (tot_L   > 0) n_L   / tot_L   else NA_real_,
      `vRHH %`    = if (tot_R   > 0) n_R   / tot_R   else NA_real_
    ) %>%
    dplyr::arrange(dplyr::desc(`Usage%`)) %>%
    dplyr::mutate(
      Velo    = round(Velo, 1),
      VeloMax = round(VeloMax, 1),
      `Velo (Max)` = dplyr::case_when(
        is.finite(Velo) & is.finite(VeloMax) ~ sprintf("%.1f (%.1f)", Velo, VeloMax),
        is.finite(Velo) ~ sprintf("%.1f", Velo),
        is.finite(VeloMax) ~ sprintf("(%.1f)", VeloMax),
        TRUE ~ "—"
      ),
      IVB     = round(IVB,  1),
      HB      = round(HB,   1),
      `Velo (Max)` = dplyr::case_when(
        is.finite(Velo) & is.finite(VeloMax) ~ sprintf("%.1f (%.1f)", Velo, VeloMax),
        is.finite(Velo) & !is.finite(VeloMax) ~ sprintf("%.1f", Velo),
        !is.finite(Velo) & is.finite(VeloMax) ~ sprintf("(%.1f)", VeloMax),
        TRUE ~ "—"
      ),
      `Usage%`    = ifelse(is.finite(`Usage%`),    scales::percent(`Usage%`,    accuracy = 0.1), "—"),
      `FP Usage%` = ifelse(is.finite(`FP Usage%`), scales::percent(`FP Usage%`, accuracy = 0.1), "—"),
      `2K Usage%` = ifelse(is.finite(`2K Usage%`), scales::percent(`2K Usage%`, accuracy = 0.1), "—"),
      `vLHH %`    = ifelse(is.finite(`vLHH %`),    scales::percent(`vLHH %`,    accuracy = 0.1), "—"),
      `vRHH %`    = ifelse(is.finite(`vRHH %`),    scales::percent(`vRHH %`,    accuracy = 0.1), "—")
    ) %>%
    dplyr::select(PitchType, `Velo (Max)`, IVB, HB, `Usage%`, `FP Usage%`, `2K Usage%`, `vLHH %`, `vRHH %`)
  
  
  # Debug check: column sums for L/R (should be ~100% when finite)
  if (tot_L > 0 || tot_R > 0) {
    sL <- sum(suppressWarnings(readr::parse_number(out$`vLHH %`)), na.rm = TRUE)
    sR <- sum(suppressWarnings(readr::parse_number(out$`vRHH %`)), na.rm = TRUE)
  }
  
  as.data.frame(out)
}

# ===========================================
# PERFORMANCE TABLE (hand or pitch-type split)
# ===========================================
# Requires: woba_weights list present (BB/HBP/X1B/X2B/X3B/HR)

# (Second definition appears later; guard to avoid override)
if (!exists("safe_is_bip", mode = "function")) {
  safe_is_bip <- function(pc, pr = NULL) {
    pc <- as.character(pc)
    bip <- pc %in% c("InPlay","InPlayOut","InPlayNoOut") | grepl("(?i)^\\s*in\\s*play", pc)
    if (!is.null(pr)) {
      prc <- as.character(pr)
      bip <- bip | grepl("(?i)single|double|triple|home\\s*run|\\bHR\\b|ground|fly|line|pop|error|reach|sac", prc)
    }
    bip[is.na(bip)] <- FALSE
    bip
  }
}

pa_last_from <- function(d) {
  d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
}

pa_outcome_cols <- function(pa) {
  n <- nrow(pa)
  
  # Always vector-length n (prevents tibble size errors when columns are absent)
  pr <- if ("PlayResult" %in% names(pa)) as.character(pa$PlayResult) else rep("", n)
  kb <- if ("KorBB"      %in% names(pa)) as.character(pa$KorBB)      else rep("", n)
  pc <- if ("PitchCall"  %in% names(pa)) as.character(pa$PitchCall)  else rep("", n)
  
  pr2 <- trimws(pr)
  kb2 <- trimws(kb)
  pc2 <- trimws(pc)
  
  is_k   <- grepl("strike.?out|\\bK\\b", pr2, ignore.case = TRUE) |
    grepl("\\bK\\b|strikeout",   kb2, ignore.case = TRUE)
  
  is_bb  <- grepl("\\bwalk\\b|\\bbb\\b", kb2, ignore.case = TRUE) |
    grepl("\\bwalk\\b",          pr2, ignore.case = TRUE)
  
  is_ibb <- grepl("intentional|\\bibb\\b", kb2, ignore.case = TRUE) |
    grepl("intentional",           pr2, ignore.case = TRUE)
  
  is_hbp <- grepl("hit by pitch|\\bhbp\\b", pr2, ignore.case = TRUE) |
    grepl("\\bhbp\\b",              kb2, ignore.case = TRUE) |
    tolower(gsub("\\s+", "", pc2)) %in% c("hitbypitch", "hbp")
  
  is_hr  <- grepl("home\\s*run|\\bhr\\b", pr2, ignore.case = TRUE)
  is_3b  <- grepl("\\btriple\\b",         pr2, ignore.case = TRUE)
  
  # negative lookahead requires perl=TRUE
  is_2b  <- grepl("\\bdouble\\b(?!\\s*play)", pr2, ignore.case = TRUE, perl = TRUE)
  
  is_1b  <- grepl("\\bsingle\\b", pr2, ignore.case = TRUE)
  
  tibble::tibble(
    K      = is_k,
    BB     = is_bb & !is_ibb,
    HBP    = is_hbp,
    X1B    = is_1b,
    X2B    = is_2b,
    X3B    = is_3b,
    HR     = is_hr,
    BIP_pa = safe_is_bip(pa$PitchCall, pr),
    PR_txt = pr2
  )
}


compute_woba_grouped <- function(pa, grp) {
  outc <- pa_outcome_cols(pa)
  num <- with(outc, woba_weights$BB  * as.numeric(BB)  +
                woba_weights$HBP * as.numeric(HBP) +
                woba_weights$X1B * as.numeric(X1B) +
                woba_weights$X2B * as.numeric(X2B) +
                woba_weights$X3B * as.numeric(X3B) +
                woba_weights$HR  * as.numeric(HR))
  denom <- with(outc, as.numeric(BB) + as.numeric(HBP) + as.numeric(BIP_pa) + as.numeric(K))
  
  num_con <- with(outc, woba_weights$X1B * as.numeric(X1B) +
                    woba_weights$X2B * as.numeric(X2B) +
                    woba_weights$X3B * as.numeric(X3B) +
                    woba_weights$HR  * as.numeric(HR))
  denom_con <- outc$BIP_pa
  
  pa %>%
    dplyr::mutate(.grp = grp, .num = num, .den = denom, .numc = num_con, .denc = denom_con) %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(
      wOBA    = sdiv(sum(.num,  na.rm = TRUE), sum(.den,  na.rm = TRUE)),
      wOBAcon = sdiv(sum(.numc, na.rm = TRUE), sum(.denc, na.rm = TRUE)),
      .groups = "drop"
    )
}

# constants  (FIXED: safe init; avoids `%||%` on undefined symbol)
FIP_CONST <- get0("FIP_CONST", ifnotfound = 3.214)
XFIP_HRFB <- get0("XFIP_HRFB", ifnotfound = 0.105)  # league HR/FB (10.5% default)

# helper to estimate outs/IP, HR and FB for FIP/xFIP
compute_fip_xfip_grouped <- function(pa, grp) {
  outc <- pa_outcome_cols(pa)
  pr   <- outc$PR_txt
  # outs on play from text; best-effort (fallbacks covered)
  outs_play <- rep(0L, nrow(pa))
  if ("OutsOnPlay" %in% names(pa)) {
    outs_play <- suppressWarnings(as.integer(pa$OutsOnPlay)); outs_play[!is.finite(outs_play)] <- 0L
  }
  outs_play <- ifelse(grepl("triple\\s*play", pr, ignore.case = TRUE), 3L, outs_play)
  outs_play <- ifelse(grepl("double\\s*play", pr, ignore.case = TRUE), 2L, outs_play)
  outs_play <- ifelse(grepl("\\bout\\b",      pr, ignore.case = TRUE) & !outc$K, pmax(outs_play, 1L), outs_play)
  
  outs_total <- outs_play + as.integer(outc$K)
  IP <- outs_total / 3
  
  HR <- as.integer(outc$HR)
  
  # fly balls (best effort from PlayResult/BBType)
  bbtype <- if ("BBType" %in% names(pa)) as.character(pa$BBType) else ""
  tagged <- if ("TaggedHitType" %in% names(pa)) as.character(pa$TaggedHitType) else ""
  is_fly <- grepl("(?i)fly", pr) | grepl("(?i)pop", pr) | bbtype %in% c("FlyBall","Popup") | tagged %in% c("FlyBall","Popup")
  FB <- as.integer(is_fly)
  
  tibble::tibble(.grp = grp, IP = IP, K = as.integer(outc$K), BB = as.integer(outc$BB), HBP = as.integer(outc$HBP), HR = HR, FB = FB) %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(
      IP   = sum(IP, na.rm = TRUE),
      K    = sum(K,  na.rm = TRUE),
      BB   = sum(BB, na.rm = TRUE),
      HBP  = sum(HBP,na.rm = TRUE),
      HR   = sum(HR, na.rm = TRUE),
      FB   = sum(FB, na.rm = TRUE),
      FIP  = ifelse(IP > 0, (13*HR + 3*(BB + HBP) - 2*K)/IP + FIP_CONST, NA_real_),
      xHR  = (FB + HR) * XFIP_HRFB,
      xFIP = ifelse(IP > 0, (13*xHR + 3*(BB + HBP) - 2*K)/IP + FIP_CONST, NA_real_),
      .groups="drop"
    ) %>%
    dplyr::select(.grp, FIP, xFIP)
}

compute_2k_block <- function(d, grp) {
  tmp <- d %>% dplyr::group_by(PA_ID) %>%
    dplyr::summarise(
      .grp = dplyr::first(grp),
      reached_2k = any( StrikesPre >= 2, na.rm = TRUE ),
      last_is_k  = {
        pr <- as.character(dplyr::last(PlayResult %||% ""))
        kb <- as.character(dplyr::last(KorBB %||% ""))
        grepl("(?i)strike.?out|\\bK\\b", pr) | grepl("(?i)K|Strikeout", kb)
      },
      .groups="drop_last"
    ) %>% dplyr::ungroup()
  
  kill <- tmp %>% dplyr::group_by(.grp) %>%
    dplyr::summarise(`2k kill%` = sdiv(sum(last_is_k & reached_2k, na.rm = TRUE), sum(reached_2k, na.rm = TRUE)), .groups="drop")
  
  os <- d %>%
    dplyr::mutate(.grp = grp, two_str = StrikesPre >= 2) %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(`2k OS%` = sdiv(sum(two_str & IsSwing & !InZone, na.rm = TRUE),
                                     sum(two_str & IsSwing & !is.na(InZone), na.rm = TRUE)), .groups="drop")
  dplyr::left_join(kill, os, by = ".grp")
}

# ---------- Batter silhouette helper ----------
.get_batter_silhouette_grob <- local({
  base_mask <- NULL
  
  load_shape_mask <- function(path) {
    ext <- tolower(tools::file_ext(path))
    img <- NULL
    if (ext == "png" && requireNamespace("png", quietly = TRUE)) {
      img <- png::readPNG(path)
    } else if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
      img <- jpeg::readJPEG(path)
    }
    if (is.null(img) || length(dim(img)) < 3) return(NULL)
    
    h <- dim(img)[1]; w <- dim(img)[2]
    rgb <- img[,,1:3]
    alpha <- if (dim(img)[3] >= 4) img[,,4] else matrix(1, h, w)
    
    # Estimate background from the border (assumes white background)
    k <- max(2L, floor(min(h, w) * 0.03))
    border <- matrix(FALSE, h, w)
    border[1:k, ] <- TRUE
    border[(h-k+1):h, ] <- TRUE
    border[, 1:k] <- TRUE
    border[, (w-k+1):w] <- TRUE
    
    bg_r <- stats::median(rgb[,,1][border], na.rm = TRUE)
    bg_g <- stats::median(rgb[,,2][border], na.rm = TRUE)
    bg_b <- stats::median(rgb[,,3][border], na.rm = TRUE)
    bg_mean <- mean(c(bg_r, bg_g, bg_b), na.rm = TRUE)
    
    bright <- (rgb[,,1] + rgb[,,2] + rgb[,,3]) / 3
    dist <- sqrt((rgb[,,1] - bg_r)^2 + (rgb[,,2] - bg_g)^2 + (rgb[,,3] - bg_b)^2)
    
    if (is.finite(bg_mean) && bg_mean > 0.90) {
      mask <- alpha * ifelse(bright < 0.92, 1, 0)
    } else {
      thr <- max(0.08, stats::quantile(dist[border], 0.98, na.rm = TRUE) + 0.02)
      mask <- alpha * ifelse(dist > thr, 1, 0)
    }
    
    mask_bin <- mask > 0.01
    # Remove thin dangling segments (e.g., string) on lower half of the image
    if (any(mask_bin, na.rm = TRUE)) {
      h2 <- nrow(mask_bin); w2 <- ncol(mask_bin)
      start_row <- max(1L, floor(h2 * 0.35))
      min_run <- max(8L, floor(w2 * 0.03))
      for (r in seq(start_row, h2)) {
        row <- mask_bin[r, ]
        if (!any(row, na.rm = TRUE)) next
        idx <- which(row)
        if (length(idx) < 2) next
        run_start <- idx[1]
        for (i in 2:length(idx)) {
          if (idx[i] != idx[i-1] + 1) {
            run_end <- idx[i-1]
            if ((run_end - run_start + 1L) < min_run) {
              mask_bin[r, run_start:run_end] <- FALSE
            }
            run_start <- idx[i]
          }
        }
        run_end <- idx[length(idx)]
        if ((run_end - run_start + 1L) < min_run) {
          mask_bin[r, run_start:run_end] <- FALSE
        }
      }
      
      # Trim thin tails in the lower half by keeping only the main body width
      lower <- mask_bin[start_row:h2, , drop = FALSE]
      col_sum <- colSums(lower, na.rm = TRUE)
      max_sum <- max(col_sum, na.rm = TRUE)
      if (is.finite(max_sum) && max_sum > 0) {
        thr <- max(2L, floor(max_sum * 0.20))
        core_cols <- which(col_sum >= thr)
        if (length(core_cols)) {
          left <- min(core_cols)
          right <- max(core_cols)
          if (left > 1L) mask_bin[start_row:h2, 1:(left - 1L)] <- FALSE
          if (right < w2) mask_bin[start_row:h2, (right + 1L):w2] <- FALSE
        }
      }
      mask <- ifelse(mask_bin, mask, 0)
    }
    if (!any(mask_bin, na.rm = TRUE)) return(NULL)
    rows <- which(apply(mask_bin, 1, any))
    cols <- which(apply(mask_bin, 2, any))
    mask <- mask[rows, cols, drop = FALSE]
    attr(mask, "aspect") <- ncol(mask) / nrow(mask)
    mask
  }
  
  find_shape <- function() {
    roots <- c("www", "")
    bases <- c("Battershape","battershape","BatterShape","batter_shape","Batter_Shape")
    exts  <- c("png","jpg","jpeg")
    for (r in roots) {
      for (b in bases) {
        for (e in exts) {
          path <- file.path(r, paste0(b, ".", e))
          if (file.exists(path)) return(path)
        }
      }
    }
    cand <- list.files("www", pattern = "(?i)^battershape\\.(png|jpg|jpeg)$", full.names = TRUE)
    if (length(cand)) return(cand[1])
    NULL
  }
  
  function(side_chr) {
    if (is.null(base_mask)) {
      path <- find_shape()
      if (is.null(path)) return(NULL)
      base_mask <<- load_shape_mask(path)
    }
    if (is.null(base_mask)) return(NULL)
    
    mask <- base_mask
    if (length(side_chr) == 0) side_chr <- ""
    side_chr <- toupper(substr(as.character(side_chr %||% ""), 1, 1))
    if (side_chr == "L") {
      mask <- mask[, ncol(mask):1, drop = FALSE]
    }
    
    out <- array(0, dim = c(nrow(mask), ncol(mask), 4))
    out[,,4] <- mask
    grob <- grid::rasterGrob(out, interpolate = TRUE)
    attr(grob, "aspect") <- attr(mask, "aspect")
    grob
  }
})

# ---------- STRIKE ZONE SCATTER (by vs RHH / vs LHH) ----------
strike_zone_plot <- function(p_sub, title = "Locations - vs RHH", allow_barrel_any = FALSE,
                             point_size = 2.7, base_size = 12, title_size = NULL,
                             panel_fill = "grey92", label_pitch_num = FALSE,
                             label_size = 2.6, label_color = "black",
                             show_batter = FALSE, batter_side = NULL,
                             style = c("default","page2")) {
  p_sub <- tibble::as_tibble(p_sub)
  style <- match.arg(style)
  
  outcome_shapes <- if (style == "page2") {
    c(
      "Ball"          = 21, # filled circle
      "Called Strike" = 21, # filled circle
      "Foul"          = 21, # filled circle
      "Ball in Play"  = 24, # filled triangle
      "Swing & Miss"  = 21, # filled circle
      "Barrel"        = 22, # filled square
      "Other"         = 21  # filled circle
    )
  } else {
    c(
      "Ball"          = 1,  # hollow circle
      "Called Strike" = 16, # filled circle
      "Foul"          = 2,  # hollow triangle
      "Ball in Play"  = 17, # filled triangle
      "Swing & Miss"  = 4,  # X
      "Barrel"        = 0,  # hollow square
      "Other"         = 3   # plus
    )
  }
  heart_box <- data.frame(
    xmin = -6.7/12, xmax = 6.7/12,
    ymin = 22/12, ymax = 38/12
  )
  zone_box <- data.frame(
    xmin = -10/12, xmax = 10/12,
    ymin = 18/12, ymax = 42/12
  )
  shadow_outline <- data.frame(
    xmin = -13.3/12, xmax = 13.3/12,
    ymin = 14/12, ymax = 46/12
  )
  
  line_shadow <- if (style == "page2") 0.4 else 0.6
  line_zone   <- if (style == "page2") 0.45 else 0.7
  line_heart  <- if (style == "page2") 0.45 else 0.7
  line_plate  <- if (style == "page2") 0.45 else 0.6
  
  zone_layers <- list(
    geom_rect(
      data = shadow_outline,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linetype = "dashed", linewidth = line_shadow
    ),
    geom_rect(
      data = zone_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linetype = "solid", linewidth = line_zone
    ),
    geom_rect(
      data = heart_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "#0B7A3B", linetype = "dashed", linewidth = line_heart
    ),
    geom_segment(
      data = home_plate_segments,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = line_plate
    )
  )
  
  batter_layers <- list()
  if (show_batter) {
    side_chr <- batter_side
    if (length(side_chr) > 1) side_chr <- side_chr[1]
    if (is.null(side_chr) || length(side_chr) == 0 || !nzchar(as.character(side_chr))) {
      bs <- as.character(p_sub$BatterSide %||% NA_character_)
      bs <- bs[!is.na(bs) & nzchar(bs)]
      if (length(bs)) side_chr <- bs[1]
    }
    if (length(side_chr) == 0) side_chr <- ""
    side_chr <- toupper(substr(as.character(side_chr), 1, 1))
    if (length(side_chr) && side_chr %in% c("L","R")) {
      sgn <- ifelse(side_chr == "R", 1, -1)
      batter_grob <- .get_batter_silhouette_grob(side_chr)
      if (!is.null(batter_grob)) {
        asp <- attr(batter_grob, "aspect")
        if (length(asp) == 0 || !is.finite(asp)) asp <- 0.25
        y_min <- 0.10; y_max <- 4.60
        height <- y_max - y_min
        width <- height * asp
        center_x <- sgn * 2.25
        xmin <- center_x - width / 2
        xmax <- center_x + width / 2
        batter_layers <- list(
          annotation_custom(
            grob = batter_grob,
            xmin = xmin, xmax = xmax,
            ymin = y_min, ymax = y_max
          )
        )
      }
    }
  }
  
  if (is.null(title_size)) title_size <- base_size
  zone_theme <- theme_minimal(base_size = base_size) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = title_size),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = panel_fill, color = NA),
      plot.background = element_rect(fill = panel_fill, color = NA),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
  
  zone_coord <- coord_fixed(xlim = c(-3.5, 3.5), ylim = c(0.1, 4.6), expand = FALSE)
  
  if (!nrow(p_sub)) {
    return(
      ggplot() +
        zone_layers +
        labs(title = title, x = NULL, y = NULL) +
        zone_theme +
        zone_coord
    )
  }
  
  p_sub <- add_outcome_type(p_sub, allow_barrel_any = allow_barrel_any)
  
  # Normalize pitch types/outcomes so manual scales always have shared levels
  if (!"PitchType" %in% names(p_sub)) p_sub$PitchType <- NA_character_
  p_sub$PitchType <- as.character(p_sub$PitchType)
  bad_pt <- is.na(p_sub$PitchType) | !(p_sub$PitchType %in% names(pitch_colors))
  p_sub$PitchType[bad_pt] <- "Undefined"
  
  if (!"OutcomeType" %in% names(p_sub)) p_sub$OutcomeType <- NA_character_
  p_sub$OutcomeType <- as.character(p_sub$OutcomeType)
  bad_outcome <- is.na(p_sub$OutcomeType) | !(p_sub$OutcomeType %in% names(outcome_shapes))
  p_sub$OutcomeType[bad_outcome] <- "Other"
  p_sub$OutcomeType <- factor(p_sub$OutcomeType, levels = names(outcome_shapes))
  
  if (label_pitch_num) {
    if (!"PitchLabel" %in% names(p_sub)) {
      if ("PitchofPA" %in% names(p_sub)) {
        p_sub$PitchLabel <- p_sub$PitchofPA
      } else if ("PitchNum" %in% names(p_sub)) {
        p_sub$PitchLabel <- p_sub$PitchNum
      } else if ("PitchNumberInPA" %in% names(p_sub)) {
        p_sub$PitchLabel <- p_sub$PitchNumberInPA
      } else {
        p_sub$PitchLabel <- NA
      }
    }
    lbl <- suppressWarnings(readr::parse_number(as.character(p_sub$PitchLabel)))
    p_sub$PitchLabel <- ifelse(is.finite(lbl), as.character(as.integer(lbl)), NA_character_)
  }
  
  if (style == "page2") {
    g <- ggplot(
      p_sub,
      aes(
        x = PlateLocSide,
        y = PlateLocHeight,
        fill = PitchType,
        shape = OutcomeType
      )
    ) +
      zone_layers +
      batter_layers +
      geom_point(
        color = "black",
        size = point_size,
        stroke = 0.6,
        na.rm = TRUE
      ) +
      scale_shape_manual(values = outcome_shapes, drop = FALSE) +
      scale_fill_manual(values = pitch_colors, breaks = names(pitch_colors), limits = names(pitch_colors), drop = FALSE) +
      labs(title = title, x = NULL, y = NULL) +
      zone_theme +
      zone_coord
  } else {
    g <- ggplot(
      p_sub,
      aes(
        x = PlateLocSide,
        y = PlateLocHeight,
        color = PitchType,
        shape = OutcomeType
      )
    ) +
      zone_layers +
      batter_layers +
      geom_point(
        alpha = 0.9,
        size = point_size,
        stroke = 1,
        na.rm = TRUE
      ) +
      scale_shape_manual(values = outcome_shapes, drop = FALSE) +
      scale_color_manual(values = pitch_colors, breaks = names(pitch_colors), limits = names(pitch_colors), drop = FALSE) +
      labs(title = title, x = NULL, y = NULL) +
      zone_theme +
      zone_coord
  }
  
  if (label_pitch_num) {
    if (style == "page2") {
      pt_lower <- tolower(as.character(p_sub$PitchType %||% ""))
      p_sub$LabelColor <- ifelse(pt_lower %in% c("slider","sweeper"), "black", "white")
      g <- g +
        geom_text(
          data = p_sub,
          aes(label = PitchLabel, color = LabelColor),
          size = label_size,
          fontface = "bold",
          na.rm = TRUE
        ) +
        scale_color_identity()
    } else {
      g <- g +
        geom_text(
          aes(label = PitchLabel),
          color = label_color,
          size = label_size,
          fontface = "bold",
          na.rm = TRUE
        )
    }
  }
  
  g
}

# ---------- MOVEMENT PLOT (25 x 25 window, dashed arm-angle line) ----------
movement_plot <- function(p, arm_angle_deg = NULL) {
  d <- tibble::as_tibble(p) %>%
    mutate(
      HB  = suppressWarnings(as.numeric(HorzBreak)),
      IVB = suppressWarnings(as.numeric(InducedVertBreak)),
      PitchType = as.character(PitchType)
    ) %>%
    filter(is.finite(HB), is.finite(IVB))
  
  legend_shapes <- tibble::tibble(
    HB = NA_real_,
    IVB = NA_real_,
    PitchType = NA_character_,
    OutcomeType = factor(
      c(
        "Ball",
        "Called Strike",
        "Foul",
        "Ball in Play",
        "Swing & Miss",
        "Barrel"
      ),
      levels = c(
        "Ball",
        "Called Strike",
        "Foul",
        "Ball in Play",
        "Swing & Miss",
        "Barrel"
      )
    )
  )
  present <- intersect(names(pitch_colors), unique(d$PitchType))
  pal <- pitch_colors[present]
  
  # slope magnitude from the passed angle (same as before)
  s_mag <- if (!is.null(arm_angle_deg) && is.finite(arm_angle_deg)) {
    -tan(pi * arm_angle_deg / 180)   # returns the positive "s" we used elsewhere
  } else {
    1
  }
  
  # infer hand sign: LHP -> -1, RHP -> +1
  hand_sign <- {
    relx <- suppressWarnings(as.numeric(p$RelSide))
    if (any(is.finite(relx))) {
      if (mean(relx, na.rm = TRUE) < 0) -1 else 1
    } else {
      # fallback: use fastball HB (LHP tends to have negative HB on four-seam)
      pt  <- tolower(as.character(p$PitchType))
      hb  <- suppressWarnings(as.numeric(p$HorzBreak))
      fs  <- is.finite(hb) & pt %in% c("fastball","four-seam")
      hb_mean <- if (any(fs)) mean(hb[fs], na.rm = TRUE) else NA_real_
      if (is.finite(hb_mean) && hb_mean < 0) -1 else 1
    }
  }
  
  slope <- s_mag * hand_sign
  
  ggplot(
    d,
    aes(
      x = HB,
      y = IVB,
      color = PitchType,
      shape = factor(
        "Ball",
        levels = c(
          "Ball",
          "Called Strike",
          "Foul",
          "Ball in Play",
          "Swing & Miss",
          "Barrel"
        )
      )
    )
  ) +
    
    geom_hline(yintercept = 0, linewidth = 1, color = "black") +
    geom_vline(xintercept = 0, linewidth = 1, color = "black") +
    geom_point(
      alpha = 0.9,
      size = 2.2,
      shape = 16   # ALWAYS filled circles
    ) +
    geom_point(
      data = legend_shapes,
      aes(x = 0, y = 0, shape = OutcomeType),
      color = "black",
      size = 2.8,
      alpha = 0,
      inherit.aes = FALSE
    ) +
    scale_color_manual(values = pal, drop = FALSE) +
    scale_shape_manual(
      name = "Locations",
      values = c(
        "Ball"          = 1,
        "Called Strike" = 16,
        "Foul"          = 2,
        "Ball in Play"  = 17,
        "Swing & Miss"  = 4,
        "Barrel"        = 0
      ),
      drop = FALSE
    ) +
    guides(
      color = guide_legend(
        title = "PitchType",
        order = 1
      ),
      shape = guide_legend(
        title = "Locations",
        order = 2,
        override.aes = list(color = "black", alpha = 1)
      )
    ) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25), expand = FALSE) +
    labs(title = "Pitch Movement", x = "HB (in)", y = "iVB (in)") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_line(color = "grey85", linewidth = 0.3),
      panel.background = element_rect(fill = "grey92", color = NA),
      plot.background = element_rect(fill = "grey92", color = NA),
      axis.title = element_text(face = "bold"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.box = "vertical",
      legend.justification = "top",
      legend.box.just = "top",
      legend.title = element_text(face = "bold"),
      legend.spacing.y = unit(4, "pt")
    ) +
    geom_abline(intercept = 0, slope = slope, linetype = "dashed", linewidth = 1)
}

library(cowplot)
# --- compact title row + no-margins table wrapper ---
.tbl_title_row <- function(text, size = 10) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = text,
             fontface = "bold", size = size / .pt, hjust = 0.5, vjust = 0.5) +
    theme_void() +
    theme(plot.margin = margin(0,0,0,0))
}

.titled_tbl <- function(tbl, title) {
  # convert any grob/table to ggplot safely
  g <- if (inherits(tbl, "ggplot")) tbl else {
    if (requireNamespace("ggplotify", quietly = TRUE)) ggplotify::as.ggplot(tbl)
    else {
      grob <- try(cowplot::as_grob(tbl), silent = TRUE)
      if (!inherits(grob, "try-error")) cowplot::as_ggplot(grob) else ggplot() + theme_void()
    }
  }
  g <- g + theme(plot.margin = margin(0,0,0,0))
  .tbl_title_row(title) / g + patchwork::plot_layout(heights = c(0.08, 0.92))
}

# --- slim vertical separator plot for patchwork ---
.vsep <- function() {
  ggplot() +
    geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1),
                 linewidth = 0.6, color = "#d9d9d9") +
    xlim(0,1) + ylim(0,1) + theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# ===== AAR TABLE CONVERSION: no cowplot::as_ggplot anywhere =====
txst_table_header_style <- function(tbl, n_cols) {
  tbl <- .tbl_bg(tbl,   row = 1, column = 1:n_cols, fill  = "#501214")
  tbl <- .tbl_font(tbl, row = 1, column = 1:n_cols, face = "bold", color = "#B4975A", size = 10.5)
  for (j in seq_len(n_cols)) {
    tbl <- .tbl_bg(tbl, row = 1, column = j, fill = "#501214", color = "#C8C8C8", linewidth = 0.7)
  }
  tbl
}

# ---------- TXST table polish (zebra rows + borders + fonts) ----------
txst_table_polish <- function(tbl, df) {
  n_rows <- nrow(df); n_cols <- ncol(df)
  if (n_rows > 0 && n_cols > 0) {
    for (i in seq_len(n_rows)) {
      is_even <- (i %% 2 == 0)
      fill <- if (is_even) "#F8F3EA" else "#FFFFFF"
      row_text <- toupper(trimws(as.character(unlist(df[i, , drop = FALSE]))))
      is_total <- any(grepl("^(GRAND )?TOTALS?$", row_text), na.rm = TRUE)
      for (j in seq_len(n_cols)) {
        tbl <- .tbl_bg(tbl,   row = i + 1, column = j, fill = fill,
                       color = "#D0D0D0", linewidth = 0.6)
        tbl <- .tbl_font(tbl, row = i + 1, column = j,
                         size = 9.6, color = "#1a1a1a",
                         face = if (is_total) "bold" else "plain")
      }
    }
  }
  tbl
}

as_txst_table <- function(df) {
  tbl <- ggpubr::ggtexttable(df, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df))
  tbl <- txst_table_polish(tbl, df)
  tbl
}

# Continuous AAR shading against the comparison mean. Green is favorable,
# red is unfavorable, and opacity increases with distance from the mean.
aar_severity_fill <- function(value, ref, lower_better = FALSE) {
  if (!is.finite(value) || !is.finite(ref)) return(NA_character_)
  span <- max(abs(ref) * 0.25, 0.05)
  score <- (value - ref) / span
  if (isTRUE(lower_better)) score <- -score
  .severity_hex_color(score)
}

txst_count_breakdown_table <- function(df) {
  if (is.null(df) || !inherits(df, c("data.frame","tbl_df","tbl")) || !nrow(df)) {
    return(as_txst_table(data.frame(Status = "No data")))
  }
  df <- as.data.frame(df)
  
  # Base table (header + zebra)
  tbl <- ggpubr::ggtexttable(df, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df))
  tbl <- txst_table_polish(tbl, df)
  
  metric_labels <- as.character(df[[1]])
  
  # D1 refs (fractions)
  d1_refs <- list(
    "Whiff%"        = D1_PCT_AVG$`Whiff%`,
    "Barrel%"       = D1_PCT_AVG$`Barrel%`,
    "Weak Contact%" = 0.26
  )
  
  metric_key <- function(lbl) {
    if (is.null(lbl)) return(NA_character_)
    lbl <- as.character(lbl)
    if (length(lbl) == 0 || is.na(lbl) || !nzchar(lbl)) return(NA_character_)
    if (grepl("Weak Contact%", lbl, fixed = TRUE)) return("Weak Contact%")
    if (grepl("Barrel%", lbl, fixed = TRUE)) return("Barrel%")
    if (grepl("Whiff%", lbl, fixed = TRUE)) return("Whiff%")
    NA_character_
  }
  
  to_frac <- function(x) {
    v <- suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (!length(v)) return(v)
    if (all(is.na(v))) return(v)
    if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v <- v / 100
    v
  }
  
  shade_cell <- function(val, ref, lower_better = FALSE) {
    aar_severity_fill(val, ref, lower_better)
  }
  
  # Apply fills row-by-row to all count columns
  for (i in seq_len(nrow(df))) {
    key <- metric_key(metric_labels[i] %||% "")
    ref <- d1_refs[[key]]
    if (is.null(ref) || !length(ref) || !is.finite(ref)) next
    
    lower_better <- identical(key, "Barrel%")
    for (j in 2:ncol(df)) {
      v <- to_frac(df[i, j][[1]])
      fill <- shade_cell(v, ref, lower_better = lower_better)
      if (!is.na(fill)) {
        tbl <- .tbl_bg(tbl, row = i + 1, column = j, fill = fill, color = "#BEBEBE", linewidth = 0.6)
      }
    }
  }
  
  tbl
}

# ==== AAR table render helpers (PASTE ABOVE compose_AAR_plot) ====
# Convert a grob/table to a ggplot without using cowplot::as_ggplot (not exported)
.gg_from_table <- function(grob_obj) {
  if (requireNamespace("ggplotify", quietly = TRUE)) {
    ggplotify::as.ggplot(grob_obj)
  } else {
    cowplot::ggdraw() + cowplot::draw_grob(grob_obj, x = 0, y = 1, hjust = 0, vjust = 1, width = 1, height = 1)
  }
}

txst_process_table <- function(df, season_col_label = "Season") {
  # Accept only data.frames; otherwise provide a safe placeholder
  if (is.null(df) || !inherits(df, c("data.frame","tbl_df","tbl")) || !nrow(df)) {
    df <- data.frame(
      Metric = "Process metrics unavailable",
      Game   = NA, Season = NA, D1 = NA,
      stringsAsFactors = FALSE
    )
  } else {
    df <- as.data.frame(df)
    
    # Normalize common case variants
    nms <- names(df)
    nms[tolower(nms) == "metric"] <- "Metric"
    nms[tolower(nms) == "game"]   <- "Game"
    nms[tolower(nms) == "season"] <- "Season"
    nms[tolower(nms) == "d1"]     <- "D1"
    names(df) <- nms
  }
  
  # Render copy (rename Season column for display only)
  df_render <- df
  if ("Season" %in% names(df_render) && !is.null(season_col_label) && nzchar(season_col_label)) {
    names(df_render)[names(df_render) == "Season"] <- season_col_label
  }
  
  # Build styled base table (header + zebra)
  tbl <- ggpubr::ggtexttable(df_render, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df_render))
  tbl <- txst_table_polish(tbl, df_render)
  
  # --- parse percent strings into fractions (0–1) ---
  to_frac <- function(x) {
    v <- suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (!length(v)) return(v)
    if (all(is.na(v))) return(v)
    if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v <- v / 100
    v
  }
  
  # --- player-facing shading: green is good, red is bad ---
  shade_cell <- function(val, ref, tol = 0.05) {
    aar_severity_fill(val, ref, lower_better = FALSE)
  }
  
  # Column indices in the rendered table
  col_game <- which(names(df_render) == "Game")[1]
  col_seas <- which(names(df_render) == season_col_label)[1]
  col_d1   <- which(names(df_render) == "D1")[1]
  
  # If any column is missing, just return the base table
  if (!is.finite(col_game) || !is.finite(col_seas) || !is.finite(col_d1)) return(tbl)
  
  # Numeric fractions from the ORIGINAL df (not df_render) so Season parsing is stable
  v_game <- to_frac(df$Game)
  v_seas <- to_frac(df$Season)
  v_d1   <- to_frac(df$D1)
  
  # Apply fills row-by-row (table rows are offset by +1 because row 1 is the header)
  tol_pp <- 0.05
  for (i in seq_len(nrow(df))) {
    ref <- v_d1[i]
    
    fill_g <- shade_cell(v_game[i], ref, tol = tol_pp)
    if (!is.na(fill_g)) {
      tbl <- .tbl_bg(tbl, row = i + 1, column = col_game, fill = fill_g, color = "#BEBEBE", linewidth = 0.6)
    }
    
    fill_s <- shade_cell(v_seas[i], ref, tol = tol_pp)
    if (!is.na(fill_s)) {
      tbl <- .tbl_bg(tbl, row = i + 1, column = col_seas, fill = fill_s, color = "#BEBEBE", linewidth = 0.6)
    }
  }
  
  tbl
}





# === League reference thresholds for PTP shading ===
LEAGUE_REF <- list(whiff = 0.24, izwhiff = 0.16)

txst_ptperf_table <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(as_txst_table(data.frame(Status = "No data")))
  }
  if ("Status" %in% names(df)) {
    return(as_txst_table(df))
  }
  
  # Metric columns expected for display
  if (!("PitchType" %in% names(df))) {
    return(as_txst_table(df))
  }
  
  # normalize header names (allow both stacked and single-line versions)
  if ("Pre2K\nZone%" %in% names(df)) names(df)[names(df) == "Pre2K\nZone%"] <- "Pre2k Zone%"
  if ("IZ\nWhiff%"   %in% names(df)) names(df)[names(df) == "IZ\nWhiff%"]   <- "IZ Whiff%"
  
  metric_cols <- c("Strike%","Pre2k Zone%","Whiff%","IZ Whiff%")
  if (!all(metric_cols %in% names(df))) {
    return(as_txst_table(df))
  }
  
  # ---- helpers ----
  to_frac <- function(x) {
    v <- if (is.numeric(x)) x else suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (length(v) == 0) return(v)
    if (all(is.na(v)))  return(v)
    if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v <- v / 100
    v
  }
  parse_dual <- function(x) {
    parts <- strsplit(as.character(x), "\\|")
    g <- s <- rep(NA_real_, length(parts))
    for (i in seq_along(parts)) {
      p <- trimws(parts[[i]])
      if (length(p) >= 1) g[i] <- suppressWarnings(readr::parse_number(p[1]))
      if (length(p) >= 2) s[i] <- suppressWarnings(readr::parse_number(p[2]))
    }
    list(game = to_frac(g), season = to_frac(s))
  }
  fmt_pct <- function(v) ifelse(is.finite(v), sprintf("%.0f%%", 100 * v), "NA")
  shade_ref <- function(v, ref, tol) {
    aar_severity_fill(v, ref, lower_better = FALSE)
  }
  
  # Map metric -> internal key + reference (support stacked + single-line headers)
  metric_map <- list(
    "Strike%"        = list(key = "Strike",  ref = D1_REF$strike_pct,  tol = 0.01),
    "Pre2k Zone%"    = list(key = "Pre2K",   ref = D1_REF$pre2k_zone,  tol = 0.01),
    "Pre2K\nZone%"   = list(key = "Pre2K",   ref = D1_REF$pre2k_zone,  tol = 0.01),
    "Whiff%"         = list(key = "Whiff",   ref = LEAGUE_REF$whiff,   tol = 0.005),
    "IZ Whiff%"      = list(key = "IZWhiff", ref = LEAGUE_REF$izwhiff, tol = 0.005),
    "IZ\nWhiff%"     = list(key = "IZWhiff", ref = LEAGUE_REF$izwhiff, tol = 0.005)
  )
  
  # Extract game/season values for each metric (use hidden columns if present)
  metric_vals <- lapply(metric_cols, function(mc) {
    key <- metric_map[[mc]]$key
    g_col <- paste0(".g_", key)
    s_col <- paste0(".s_", key)
    if (g_col %in% names(df) && s_col %in% names(df)) {
      list(game = to_frac(df[[g_col]]), season = to_frac(df[[s_col]]))
    } else {
      parse_dual(df[[mc]])
    }
  })
  names(metric_vals) <- metric_cols
  
  # ----- layout -----
  rows <- as.character(df$PitchType)
  n <- length(rows)
  if (n == 0) return(as_txst_table(df))
  
  row_idx <- seq_len(n)
  y_vals <- n - row_idx + 1
  zebra_fill <- ifelse(row_idx %% 2 == 0, "#F8F3EA", "#FFFFFF")
  
  col_labels <- c("Pitch Type", metric_cols)
  x_vals <- seq_along(col_labels)
  
  # Note row (spanning all columns) + header row
  note_cells <- data.frame(
    xmin = 0.5, xmax = length(col_labels) + 0.5,
    ymin = n + 2 - 0.5, ymax = n + 2 + 0.5
  )
  note_text <- data.frame(
    x = (length(col_labels) + 1) / 2, y = n + 2,
    label = "Game % | Season %"
  )
  header_cells <- data.frame(
    xmin = x_vals - 0.5, xmax = x_vals + 0.5,
    ymin = n + 1 - 0.5, ymax = n + 1 + 0.5
  )
  header_text <- data.frame(
    x = x_vals, y = n + 1,
    label = col_labels
  )
  
  # PitchType column cells
  pt_cells <- data.frame(
    xmin = 0.5, xmax = 1.5,
    ymin = y_vals - 0.5, ymax = y_vals + 0.5,
    fill = zebra_fill
  )
  pt_text <- data.frame(
    x = 1, y = y_vals, label = rows
  )
  
  # Metric half-cells + text
  half_cells <- list()
  half_text  <- list()
  
  for (j in seq_along(metric_cols)) {
    mc <- metric_cols[j]
    map <- metric_map[[mc]]
    key <- if (is.null(map) || !length(map$key)) "" else map$key
    ref <- if (is.null(map) || !length(map$ref)) NA_real_ else map$ref
    tol <- if (is.null(map) || !length(map$tol)) 0 else map$tol
    vals <- metric_vals[[mc]]
    
    g_vals <- vals$game
    s_vals <- vals$season
    
    if (mc == "Whiff%") {
      ref_vec <- vapply(rows, d1_pct_avg_for_metric, metric = "Whiff%", FUN.VALUE = numeric(1))
      shade_g <- mapply(shade_ref, g_vals, ref_vec, MoreArgs = list(tol = tol),
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)
      shade_s <- mapply(shade_ref, s_vals, ref_vec, MoreArgs = list(tol = tol),
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)
    } else {
      shade_g <- vapply(g_vals, shade_ref, character(1), ref = ref, tol = tol)
      shade_s <- vapply(s_vals, shade_ref, character(1), ref = ref, tol = tol)
    }
    fill_g <- ifelse(!is.na(shade_g), shade_g, zebra_fill)
    fill_s <- ifelse(!is.na(shade_s), shade_s, zebra_fill)
    
    x_center <- x_vals[j + 1]
    
    half_cells[[length(half_cells) + 1]] <- data.frame(
      xmin = x_center - 0.5, xmax = x_center,
      ymin = y_vals - 0.5, ymax = y_vals + 0.5,
      fill = fill_g
    )
    half_cells[[length(half_cells) + 1]] <- data.frame(
      xmin = x_center, xmax = x_center + 0.5,
      ymin = y_vals - 0.5, ymax = y_vals + 0.5,
      fill = fill_s
    )
    
    half_text[[length(half_text) + 1]] <- data.frame(
      x = x_center - 0.25, y = y_vals, label = fmt_pct(g_vals)
    )
    half_text[[length(half_text) + 1]] <- data.frame(
      x = x_center + 0.25, y = y_vals, label = fmt_pct(s_vals)
    )
  }
  
  half_cells_df <- do.call(rbind, half_cells)
  half_text_df  <- do.call(rbind, half_text)
  
  ggplot() +
    geom_rect(
      data = note_cells,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "#C8C8C8", linewidth = 0.7
    ) +
    geom_text(
      data = note_text,
      aes(x = x, y = y, label = label),
      color = "black", fontface = "bold", size = 3.2
    ) +
    geom_rect(
      data = header_cells,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "#C8C8C8", linewidth = 0.7
    ) +
    geom_text(
      data = header_text,
      aes(x = x, y = y, label = label),
      color = "black", fontface = "bold", size = 3.4
    ) +
    geom_rect(
      data = pt_cells,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "#D0D0D0", linewidth = 0.6
    ) +
    geom_rect(
      data = half_cells_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "#D0D0D0", linewidth = 0.6, show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_text(
      data = pt_text,
      aes(x = x, y = y, label = label),
      color = "#1a1a1a", size = 3.2
    ) +
    geom_text(
      data = half_text_df,
      aes(x = x, y = y, label = label),
      color = "#1a1a1a", size = 3.0
    ) +
    coord_cartesian(
      xlim = c(0.5, length(col_labels) + 0.5),
      ylim = c(0.5, n + 2.5),
      expand = FALSE
    ) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# --- SAFE cell helpers (used by all table builders) ---
if (!exists(".tbl_bg", mode = "function")) {
  .tbl_bg <- function(tbl, ...) {
    res <- try(ggpubr::table_cell_bg(tbl, ...), silent = TRUE)
    if (inherits(res, "try-error")) tbl else res
  }
}
if (!exists(".tbl_font", mode = "function")) {
  .tbl_font <- function(tbl, ...) {
    res <- try(ggpubr::table_cell_font(tbl, ...), silent = TRUE)
    if (inherits(res, "try-error")) tbl else res
  }
}

# ---------- Title strip (maroon/gold) ----------
# Keep the dot-prefixed name (matches existing calls),
# and also provide a non-dotted alias for future use.
.title_strip_plot <- function(title) {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#501214") +
    annotate(
      "text", x = 0.5, y = 0.5, label = title,
      fontface = "bold", size = 11/.pt, color = "#B4975A",
      hjust = 0.5, vjust = 0.5
    ) +
    coord_cartesian(expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}
title_strip_plot <- .title_strip_plot  # alias (not strictly required)

# ---------- TXST table polish (zebra rows + borders + fonts) ----------
if (!exists("txst_table_polish", mode = "function")) {
  txst_table_polish <- function(tbl, df) {
    n_rows <- nrow(df); n_cols <- ncol(df)
    if (n_rows > 0 && n_cols > 0) {
      for (i in seq_len(n_rows)) {
        is_even <- (i %% 2 == 0)
        fill <- if (is_even) "#F8F3EA" else "#FFFFFF"
        for (j in seq_len(n_cols)) {
          tbl <- .tbl_bg(tbl,   row = i + 1, column = j, fill = fill,
                         color = "#D0D0D0", linewidth = 0.6)
          tbl <- .tbl_font(tbl, row = i + 1, column = j,
                           size = 9.6, color = "#1a1a1a")
        }
      }
    }
    tbl
  }
}

# --- One table (data.frame -> styled table -> ggplot) + maroon title
safe_tbl_plot <- function(obj, title = "Table", season_col_label = "Season") {
  normalize_game_fields <- function(x) {
    if (is.null(x) || !nrow(x)) return(x)
    
    # Coerce IDs to character so %in% matching works
    if ("CustomGameID" %in% names(x)) x$CustomGameID <- as.character(x$CustomGameID)
    
    # Coerce dates to Date (handles "YYYY-MM-DD" and "m/d/YYYY")
    if ("GameDate" %in% names(x)) {
      gd <- suppressWarnings(as.Date(x$GameDate))
      if (all(is.na(gd))) gd <- suppressWarnings(as.Date(x$GameDate, format = "%m/%d/%Y"))
      x$GameDate <- gd
    }
    
    x
  }
  
  build_body <- function(o) {
    # Ensure IDs/dates are normalized for all downstream filters/tables
    o <- normalize_game_fields(o)
    
    if (inherits(o, c("data.frame","tbl_df"))) {
      df <- as.data.frame(o)
      
      # choose the correct builder (ensures Process & PTP use shaded versions)
      tbl <-
        if (identical(title, "Process Metrics") && exists("txst_process_table", mode = "function")) {
          txst_process_table(df, season_col_label = season_col_label)
        } else if (identical(title, "Count Breakdown") && exists("txst_count_breakdown_table", mode = "function")) {
          txst_count_breakdown_table(df)
        } else if (identical(title, "Pitch Type Performance") && exists("txst_ptperf_table", mode = "function")) {
          txst_ptperf_table(df)
        } else if (exists("as_txst_table", mode = "function")) {
          as_txst_table(df)
        } else {
          ggpubr::ggtexttable(df, rows = NULL, theme = ggpubr::ttheme("blank"))
        }
      
      # --- Ensure we always return a renderable grob ---
      if (is.null(tbl)) {
        return(grid::textGrob(""))
      }
      
      if (inherits(tbl, "ggplot")) {
        return(tbl + theme(plot.margin = margin(0,0,0,0)))
      }
      
      if (inherits(tbl, c("data.frame", "tbl_df", "tbl"))) {
        if (requireNamespace("gridExtra", quietly = TRUE)) {
          tbl <- gridExtra::tableGrob(tbl, rows = NULL)
        } else {
          tbl <- grid::textGrob("Table render failed (gridExtra missing).")
        }
      }
      
      # Key: render via ggplotify so cell backgrounds stay visible
      g <- try(ggplotify::as.ggplot(tbl), silent = TRUE)
      if (inherits(g, "try-error")) {
        g <- cowplot::ggdraw() + cowplot::draw_grob(cowplot::as_grob(tbl))
      }
      return(g + theme(plot.margin = margin(0,0,0,0)))
    }
    
    if (inherits(o, "ggplot"))  return(o + theme(plot.margin = margin(0,0,0,0)))
    if (inherits(o, c("grob","gTree","gTable","gtable"))) {
      return(ggplotify::as.ggplot(o) + theme(plot.margin = margin(0,0,0,0)))
    }
    ggplot() + theme_void() + labs(title = paste(title, "(unavailable)"))
  }
  
  body_plot  <- build_body(obj)
  title_plot <- .title_strip_plot(title)
  if (identical(title, "Count Breakdown")) {
    return((title_plot / patchwork::plot_spacer() / body_plot) +
             patchwork::plot_layout(heights = c(0.12, 0.08, 0.80)))
  }
  (title_plot / body_plot) + patchwork::plot_layout(heights = c(0.12, 0.88))
}

# --- thin vertical separator for row 2 ---
if (!exists(".vsep", mode = "function")) {
  .vsep <- function() {
    ggplot() +
      geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1),
                   linewidth = 0.6, color = "#d9d9d9") +
      xlim(0,1) + ylim(0,1) + theme_void() +
      theme(plot.margin = margin(0,0,0,0))
  }
}

safe_last_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x)) x[[length(x)]] else NA_character_
}

recover_full_game_rows <- function(game_p, season_p, pitcher_name) {
  g <- tibble::as_tibble(game_p)
  s <- tibble::as_tibble(season_p)
  
  # Try to recover all rows for THIS game & pitcher from the season table
  # Priority 1: exact GameID if present
  if ("GameID" %in% names(g) && "GameID" %in% names(s)) {
    gid <- unique(na.omit(g$GameID))[1] %||% NA
    if (!is.na(gid)) {
      s_rec <- s %>% dplyr::filter(.data$GameID == gid)
    } else {
      s_rec <- s
    }
  } else {
    s_rec <- s
  }
  
  # Priority 2: same GameDate (if available)
  gd <- parse_date_any(g$GameDate)[1]
  if (!is.na(gd) && "GameDate" %in% names(s_rec)) {
    s_rec <- s_rec %>% dplyr::filter(parse_date_any(.data$GameDate) == gd)
  }
  
  # Priority 3: same pitcher column (Pitcher or PitcherName), if present
  if ("Pitcher" %in% names(s_rec)) {
    s_rec <- s_rec %>% dplyr::filter(.data$Pitcher == pitcher_name)
  } else if ("PitcherName" %in% names(s_rec)) {
    s_rec <- s_rec %>% dplyr::filter(.data$PitcherName == pitcher_name)
  }
  
  # If recovery found additional rows (i.e., more PAs), use it; else fall back to game_p
  n_pa_g  <- dplyr::n_distinct(g$PA_ID)
  n_pa_sr <- dplyr::n_distinct(s_rec$PA_ID)
  if (is.finite(n_pa_sr) && n_pa_sr >= n_pa_g && n_pa_sr > 0) {
    s_rec
  } else {
    g
  }
}

normalize_game_fields <- function(x) {
  if (is.null(x) || !nrow(x)) return(x)
  
  # IDs: make character so %in% and matching don't silently fail
  if ("GameID" %in% names(x)) x$GameID <- as.character(x$GameID)
  if ("CustomGameID" %in% names(x)) x$CustomGameID <- as.character(x$CustomGameID)
  
  # Dates: handle "YYYY-MM-DD" and "m/d/YYYY" (and avoid hard errors)
  if ("GameDate" %in% names(x)) {
    x$GameDate <- parse_date_any(x$GameDate)
  }
  x
}

count_walks_from_korbb <- function(df) {
  if (is.null(df) || !nrow(df) || !"KorBB" %in% names(df)) return(0L)
  
  kor <- tolower(trimws(as.character(df$KorBB)))
  is_walk <- (kor == "walk")
  
  # Count once per plate appearance if we have a PA identifier
  if ("PlateAppearanceID" %in% names(df)) {
    return(dplyr::n_distinct(df$PlateAppearanceID[is_walk]))
  }
  if (all(c("Inning", "PAofInning") %in% names(df))) {
    key <- paste(df$Inning, df$PAofInning, sep = "_")
    return(dplyr::n_distinct(key[is_walk]))
  }
  if ("AtBatNumber" %in% names(df)) {
    return(dplyr::n_distinct(df$AtBatNumber[is_walk]))
  }
  
  # Fallback: if no PA id exists, count rows marked Walk (best possible)
  sum(is_walk, na.rm = TRUE)
}

compute_pa_count <- function(df) {
  df <- tibble::as_tibble(df)
  if (is.null(df) || !nrow(df)) return(0L)
  
  to_int <- function(x) suppressWarnings(as.integer(readr::parse_number(as.character(x))))
  
  # Prefer explicit per-PA pitch index if available
  if ("PitchofPA" %in% names(df)) {
    pn <- to_int(df$PitchofPA)
    if (any(pn == 1L, na.rm = TRUE)) {
      return(as.integer(sum(pn == 1L, na.rm = TRUE)))
    }
  }
  
  # Prefer stable PA identifiers
  pa_cols <- c("PlateAppearanceID","PlateAppearanceId","PAId","PA_ID","AtBatNumber","PA","Pa")
  pa_col <- intersect(pa_cols, names(df))[1]
  if (!is.na(pa_col)) {
    pa_n <- dplyr::n_distinct(df[[pa_col]])
    
    # If the PA id looks per-pitch and we have evidence of multi-pitch PAs, keep searching
    if (pa_n != nrow(df) || !("PitchNum" %in% names(df) && any(to_int(df$PitchNum) > 1L, na.rm = TRUE))) {
      return(as.integer(pa_n))
    }
  }
  
  # Combine inning + PAofInning (+ Batter if present)
  if (all(c("Inning","PAofInning") %in% names(df))) {
    if ("Batter" %in% names(df)) {
      return(as.integer(dplyr::n_distinct(interaction(df$Inning, df$PAofInning, df$Batter, drop = TRUE))))
    } else {
      return(as.integer(dplyr::n_distinct(interaction(df$Inning, df$PAofInning, drop = TRUE))))
    }
  }
  
  # Fallback: count terminal pitches
  pc <- as.character(df$PitchCall %||% NA_character_)
  pr <- as.character(df$PlayResult %||% NA_character_)
  kb <- as.character(df$KorBB %||% NA_character_)
  
  term <- (!is.na(pr) & nzchar(pr)) | (!is.na(kb) & nzchar(kb)) | pc %in% c("InPlay","InPlayNoOut","InPlayOut","HitByPitch")
  if ("HBP" %in% names(df)) term <- term | (df$HBP %in% TRUE)
  
  if ("StrikesAfterPitch" %in% names(df)) term <- term | (to_int(df$StrikesAfterPitch) >= 3L)
  if ("Strikes" %in% names(df))           term <- term | (to_int(df$Strikes) >= 3L)
  if ("BallsAfterPitch" %in% names(df))   term <- term | (to_int(df$BallsAfterPitch) >= 4L)
  if ("Balls" %in% names(df))             term <- term | (to_int(df$Balls) >= 4L)
  
  as.integer(sum(term, na.rm = TRUE))
}

compose_AAR_plot <- function(game_p, season_p, pitcher_name, game_label, arm_angle_deg = NULL, season_col_label = "Season") {
  # --- Determine season label from the data (no date windows) ---
  season_col_label_local <- if (!is.null(season_col_label) && nzchar(season_col_label)) season_col_label else "Season"
  
  game_p   <- normalize_game_fields(game_p)
  season_p <- normalize_game_fields(season_p)
  
  # Ensure helpers/types exist
  game_raw   <- tibble::as_tibble(game_p)
  season_raw <- tibble::as_tibble(season_p)
  
  season_col_candidates <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
  pick_season_col <- function(df) {
    if (is.null(df) || !nrow(df)) return(NA_character_)
    for (nm in season_col_candidates) {
      if (nm %in% names(df)) return(nm)
    }
    NA_character_
  }
  season_tag_from <- function(df) {
    col <- pick_season_col(df)
    if (is.na(col)) return(NA_character_)
    v <- as.character(df[[col]])
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) v[[1]] else NA_character_
  }
  label_from_tag <- function(tag) {
    dplyr::case_when(
      tag == "S25"  ~ "2025 Season",
      tag == "F25"  ~ "2025 Fall",
      tag == "SQ26" ~ "2026 Squads",
      tag == "S26"  ~ "2026 Season",
      tag == "PORT" ~ "Portal",
      TRUE          ~ as.character(tag %||% "Season")
    )
  }
  
  season_tag <- season_tag_from(game_raw)
  if (is.na(season_tag) || !nzchar(season_tag)) season_tag <- season_tag_from(season_raw)
  
  if (!is.na(season_tag) && nzchar(season_tag)) {
    season_col_label_local <- label_from_tag(season_tag)
    season_col <- pick_season_col(season_raw)
    if (!is.na(season_col)) {
      season_raw <- season_raw %>% dplyr::filter(.data[[season_col]] == season_tag)
    }
  }
  
  pcol <- intersect(c("Pitcher","PitcherName"), names(season_raw))[1]
  if (!is.na(pcol) && nzchar(pcol)) {
    season_raw <- season_raw %>% dplyr::filter(.data[[pcol]] == pitcher_name)
  }
  
  # Keep current behavior for plots/tables
  game_p   <- ensure_pa(game_raw)
  season_p <- ensure_pa(season_raw)
  
  # Helper used below
  safe_last_nonempty <- function(x) {
    x <- as.character(x)
    if (!length(x)) return(NA_character_)
    x <- trimws(x); x[x == ""] <- NA_character_
    idx <- suppressWarnings(max(which(!is.na(x))))
    if (is.finite(idx)) x[[idx]] else NA_character_
  }
  
  # ---- Build a full, unfiltered game frame for STATLINE ONLY (preserves BB PAs) ----
  game_key_id   <- if ("GameID"   %in% names(game_raw))  safe_last_nonempty(game_raw$GameID)   else NA_character_
  game_key_uid  <- if ("GameUID"  %in% names(game_raw))  safe_last_nonempty(game_raw$GameUID)  else NA_character_
  game_key_date <- if ("GameDate" %in% names(game_raw))  suppressWarnings(as.Date(safe_last_nonempty(game_raw$GameDate))) else NA
  
  pitcher_cols  <- intersect(c("PitcherName","Pitcher"), names(season_raw))
  
  if (!is.na(game_key_id) && "GameID" %in% names(season_raw)) {
    game_stat <- dplyr::filter(season_raw, .data$GameID == game_key_id)
  } else if (!is.na(game_key_uid)) {
    game_stat <- dplyr::filter(season_raw, .data$GameUID == game_key_uid)
  } else {
    game_stat <- season_raw
    if (!is.na(game_key_date)) {
      game_stat <- dplyr::filter(game_stat, suppressWarnings(as.Date(.data$GameDate)) == game_key_date)
    }
    if (length(pitcher_cols) >= 1) {
      game_stat <- dplyr::filter(game_stat, .data[[pitcher_cols[1]]] == pitcher_name)
    }
  }
  if (nrow(game_stat) == 0) game_stat <- game_raw  # worst-case fallback
  
  
  # ---------- Statline ----------
  # Last row per PA_ID (for outs calc)
  pa_last_rows <- game_stat %>%
    dplyr::group_by(PA_ID) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  
  # Last non-empty KorBB / PlayResult per PA (for K/BB/H detection)
  pa_end <- game_stat %>%
    dplyr::group_by(PA_ID) %>%
    dplyr::summarise(
      KorBB_last = safe_last_nonempty(KorBB),
      PR_last    = safe_last_nonempty(PlayResult),
      any_hbp    = if ("HBP" %in% names(game_stat)) any(HBP, na.rm = TRUE) else FALSE,
      .groups = "drop"
    )
  
  # --- BB detection: prefer KorBB tokens; fall back to PlayResult text
  kb_last_trim <- tolower(trimws(as.character(pa_end$KorBB_last)))
  pr_last_trim <- tolower(trimws(as.character(pa_end$PR_last)))
  
  # KorBB can be "BB", "IBB", "Walk", "Intentional Walk", etc.
  kor_is_walk <- grepl("\\b(bb|ibb|walk)\\b", kb_last_trim)
  kor_is_walk[is.na(kor_is_walk)] <- FALSE
  
  pr_is_walk <- grepl("\\bintentional\\b|\\bwalk\\b", pr_last_trim)
  pr_is_walk[is.na(pr_is_walk)] <- FALSE
  
  bb_from_korbb <- sum(kor_is_walk, na.rm = TRUE)
  bb_from_pr    <- sum(!kor_is_walk & pr_is_walk, na.rm = TRUE)
  bb_n <- bb_from_korbb + bb_from_pr
  
  
  # --- K detection: from either KorBB or PlayResult
  k_n <- sum(
    grepl("\\bK\\b|strike.?out", pa_end$KorBB_last, ignore.case = TRUE) |
      grepl("strike.?out|\\bK\\b", pa_end$PR_last,    ignore.case = TRUE),
    na.rm = TRUE
  )
  
  # --- Hits (exclude double play explicitly)
  h_n <- sum(
    grepl("home ?run|\\bHR\\b", pa_end$PR_last, ignore.case = TRUE) |
      grepl("\\btriple\\b",       pa_end$PR_last, ignore.case = TRUE) |
      (grepl("\\bdouble\\b",       pa_end$PR_last, ignore.case = TRUE) & !grepl("double\\s*play", pa_end$PR_last, ignore.case = TRUE)) |
      grepl("\\bsingle\\b",       pa_end$PR_last, ignore.case = TRUE),
    na.rm = TRUE
  )

  # --- HBP detection (PitchCall only)
  hbp_n <- 0L
  if ("PitchCall" %in% names(game_stat)) {
    hbp_n <- game_stat %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::summarise(
        any_hbp = any(tolower(as.character(PitchCall)) == "hitbypitch", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::summarise(n = sum(any_hbp, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(n)
    if (!length(hbp_n) || !is.finite(hbp_n)) hbp_n <- 0L
  }

  # --- Barrel count (pitch-level)
  barrel_n <- sum(compute_barrel_flag_leaderboard(game_stat), na.rm = TRUE)
  
  # --- PA count and IP (from full game rows)
  pa_n <- compute_pa_count(game_stat)
  
  outs_play <- rep(0L, nrow(pa_last_rows))
  if ("OutsOnPlay" %in% names(pa_last_rows)) {
    outs_play <- suppressWarnings(as.integer(pa_last_rows$OutsOnPlay))
    outs_play[!is.finite(outs_play)] <- 0L
  }
  outs_play <- ifelse(grepl("(?i)triple ?play", pa_last_rows$PlayResult), 3L, outs_play)
  outs_play <- ifelse(grepl("(?i)double ?play", pa_last_rows$PlayResult), 2L, outs_play)
  outs_play <- ifelse(grepl("(?i)\\bout\\b",    pa_last_rows$PlayResult) &
                        !grepl("(?i)strike.?out|\\bK\\b", pa_last_rows$PlayResult),
                      pmax(outs_play, 1L), outs_play)
  
  outs_total <- sum(outs_play, na.rm = TRUE) + k_n
  ip_chr     <- sprintf("%d.%d", floor(outs_total/3), outs_total %% 3)
  
  # --- Robust date string (THIS game only; never season_p) ---
  date_val <- game_key_date
  if (is.na(date_val)) {
    gd <- parse_date_any(game_raw$GameDate)
    gd <- gd[!is.na(gd)]
    if (length(gd)) date_val <- gd[1]
  }
  
  date_str <- if (!is.na(date_val)) {
    lt <- as.POSIXlt(date_val)
    paste0(month.name[lt$mon + 1L], " ", lt$mday, ", ", lt$year + 1900L)
  } else {
    as.character(game_label)
  }
  
  
  # Build header statline text (local pitch count from full game)
  pitches_cnt  <- nrow(game_stat)
  statline_text <- sprintf(
    "Pitches: %s   PA: %s   IP: %s   SO: %s   H: %s   Barrels: %s   BB: %s   HBP: %s",
    pitches_cnt, pa_n, ip_chr, k_n, h_n, barrel_n, bb_n, hbp_n
  )
  statline <- statline_text
  
  
  header <- ggplot() +
    annotate("text", x=0, y=1.00, label=pitcher_name, hjust=0, vjust=1, size=5.6, fontface="bold") +
    annotate("text", x=0, y=0.82, label=date_str,    hjust=0, vjust=1, size=4.3) +
    annotate("text", x=0, y=0.64, label=statline,    hjust=0, vjust=1, size=4.0) +
    xlim(0,1) + ylim(0,1) + theme_void() + theme(plot.margin = margin(2,2,0,2))

  # ---------- Release Points (Game vs Season) ----------
  mean_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
  }
  fmt_release_delta <- function(game_val, season_val, digits = 1) {
    if (!is.finite(game_val)) return("—")
    val_str <- sprintf(paste0("%.", digits, "f"), game_val)
    if (!is.finite(season_val)) return(val_str)
    delta_str <- sprintf(paste0("%+.", digits, "f"), game_val - season_val)
    paste0(val_str, " [", delta_str, "]")
  }
  rel_game <- list(
    RelHeight = mean_or_na(game_raw$RelHeight),
    RelSide   = mean_or_na(game_raw$RelSide),
    Extension = mean_or_na(game_raw$Extension)
  )
  rel_season <- list(
    RelHeight = mean_or_na(season_raw$RelHeight),
    RelSide   = mean_or_na(season_raw$RelSide),
    Extension = mean_or_na(season_raw$Extension)
  )
  release_df <- data.frame(
    `Release Height` = fmt_release_delta(rel_game$RelHeight, rel_season$RelHeight),
    `Release Side`   = fmt_release_delta(rel_game$RelSide,   rel_season$RelSide),
    Extension        = fmt_release_delta(rel_game$Extension, rel_season$Extension),
    check.names = FALSE
  )
  # Full-width title bar + table body
  release_title <- .title_strip_plot("Release Points") +
    theme(plot.margin = margin(0,6,0,6))
  release_tbl <- if (exists("as_txst_table", mode = "function")) {
    as_txst_table(release_df)
  } else {
    ggpubr::ggtexttable(release_df, rows = NULL, theme = ggpubr::ttheme("blank"))
  }
  release_body <- if (inherits(release_tbl, "ggplot")) {
    release_tbl
  } else {
    ggplotify::as.ggplot(release_tbl)
  }
  release_body <- release_body + theme(plot.margin = margin(0,6,2,6))
  
  # ---------- Title strip (maroon/gold) ----------
  .title_strip_plot <- function(title) {
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#501214") +
      annotate(
        "text", x = 0.5, y = 0.5, label = title,
        fontface = "bold", size = 11/.pt, color = "#B4975A",
        hjust = 0.5, vjust = 0.5
      ) +
      coord_cartesian(expand = FALSE) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  # ---------- Build AAR tables ----------
  proc_df <- tryCatch(
    build_process_table(game_p, season_p, season_col_label_local),
    error = function(e) {
      warning("[AAR] build_process_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  
  ptpf_df <- tryCatch(
    build_pitchtype_perf_table(game_p, season_p),
    error = function(e) {
      warning("[AAR] build_pitchtype_perf_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  
  mu_df <- tryCatch(
    build_movement_usage_table(game_p),
    error = function(e) {
      warning("[AAR] build_movement_usage_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  
  # --- Row 2: two tables with spacing (no visible separator line) ---
  row2_left  <- safe_tbl_plot(proc_df, "Process Metrics", season_col_label = season_col_label_local) +
    theme(plot.margin = margin(4,6,4,6))
  
  row2_right <- safe_tbl_plot(ptpf_df, "Pitch Type Performance", season_col_label = season_col_label_local) +
    theme(plot.margin = margin(4,6,4,6))
  
  row3 <- safe_tbl_plot(mu_df, "Pitch Movement & Usage", season_col_label = season_col_label_local) +
    theme(plot.margin = margin(4,6,4,6))
  
  # ---------- Plots ----------
  game_p_plot <- tibble::as_tibble(game_p)
  game_p_plot$Barrel <- compute_barrel_flag_leaderboard(game_p_plot)
  p_r <- game_p_plot %>% dplyr::filter(BatterSide == "R")
  p_l <- game_p_plot %>% dplyr::filter(BatterSide == "L")
  zR  <- strike_zone_plot(p_r, "Locations - vs RHH", show_batter = TRUE, batter_side = "R") +
    theme(plot.margin = margin(0,0,0,0))
  zL  <- strike_zone_plot(p_l, "Locations - vs LHH", show_batter = TRUE, batter_side = "L") +
    theme(plot.margin = margin(0,0,0,0))
  mov <- movement_plot(game_p, arm_angle_deg) + theme(plot.margin = margin(0,0,0,0))
  
  # ---------- Assemble rows ----------
  row1 <- header + patchwork::plot_spacer() + patchwork::plot_layout(widths = c(1,0), heights = 1)
  row1b <- (release_title | patchwork::plot_spacer()) / (release_body | patchwork::plot_spacer()) +
    patchwork::plot_layout(heights = c(0.26, 0.74), widths = c(1, 1))
  row4 <- (mov | (zR / zL)) + patchwork::plot_layout(widths = c(1, 1))
  row4 <- row4 & theme(plot.margin = margin(0,0,0,0))
  
  # --- Row 2 (patchwork-native; prevents tables from disappearing) ---
  row2 <- (row2_left | row2_right) + patchwork::plot_layout(widths = c(1, 1))
  
  
  final <- row1 / row1b / row2 / row3 / row4 +
    patchwork::plot_layout(heights = c(0.12, 0.10, 0.22, 0.22, 0.34))
  
  # ---------- AAR-only: watermark + TXST logo layering ----------
  header_h <- 0.15
  # Find assets
  find_asset <- function(basenames) {
    exts <- c("png","jpg","jpeg")
    roots <- unique(c("", "www/", file.path(app_dir, "www")))
    cands <- unlist(lapply(basenames, function(b) as.vector(outer(roots, exts, function(r,e) file.path(r, paste0(b, ".", e))))), use.names = FALSE)
    existing <- cands[file.exists(cands)]
    if (length(existing)) existing[[1]] else NULL
  }
  # Bobcat watermark
  wm_path <- get0("BASE_PITCHING_BOBCAT_LOGO_PATH", inherits = TRUE, ifnotfound = "")
  if (!nzchar(wm_path) || !file.exists(wm_path)) {
    wm_path <- find_asset(c("Bobcatlogo","bobcatlogo","BobcatLogo","bobcat_logo","Bobcat"))
  }
  wm_grob <- NULL
  if (!is.null(wm_path)) {
    ext <- tolower(tools::file_ext(wm_path))
    img <- if (ext == "png") png::readPNG(wm_path) else if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG(wm_path) else NULL
    if (!is.null(img)) {
      wm_grob <- grid::rasterGrob(img, x = 0.5, y = 0.5, width = 0.80, height = 0.80, just = "center",
                                  interpolate = TRUE, gp = grid::gpar(alpha = 0.60))
    }
  }
  # TXST logo top-right sized to header band
  txst_path <- get0("BASE_PITCHING_TXST_LOGO_PATH", inherits = TRUE, ifnotfound = "")
  if (!nzchar(txst_path) || !file.exists(txst_path)) {
    txst_path <- find_asset(c("txstlogo","TXSTlogo","Txstlogo","txst"))
  }
  txst_grob <- NULL; txst_asp <- 1
  if (!is.null(txst_path)) {
    ext <- tolower(tools::file_ext(txst_path))
    img <- if (ext == "png") png::readPNG(txst_path) else if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG(txst_path) else NULL
    if (!is.null(img)) {
      txst_grob <- grid::rasterGrob(img, interpolate = TRUE)
      h <- dim(img)[1]; w <- dim(img)[2]
      if (is.finite(h) && is.finite(w) && h > 0) txst_asp <- w / h
    }
  }
  logo_h <- header_h * 0.98
  logo_w <- min(logo_h * txst_asp, 0.40)
  
  cowplot::ggdraw() +
    { if (!is.null(wm_grob)) cowplot::draw_grob(wm_grob, x = 0.5, y = 0.5, width = 1, height = 1) } +
    cowplot::draw_plot(final, x = 0, y = 0, width = 1, height = 1) +
    { if (!is.null(txst_grob)) cowplot::draw_grob(txst_grob, x = 0.995, y = 0.995,
                                                  width = logo_w, height = logo_h, hjust = 1, vjust = 1) }
}

compose_AAR_pa_grid_plot <- function(game_p, max_cols = 4, max_rows = 9) {
  d <- tibble::as_tibble(game_p)
  if (is.null(d) || !nrow(d)) {
    return(ggplot() + theme_void() + labs(title = "No PA data"))
  }
  
  # Required columns for plotting
  if (!"PlateLocSide" %in% names(d)) d$PlateLocSide <- NA_real_
  if (!"PlateLocHeight" %in% names(d)) d$PlateLocHeight <- NA_real_
  if (!"PitchType" %in% names(d)) d$PitchType <- NA_character_
  if (!"BatterSide" %in% names(d)) d$BatterSide <- NA_character_
  d$BatterSide <- dplyr::case_when(
    d$BatterSide %in% c("L","Left","LHH","LH") ~ "L",
    d$BatterSide %in% c("R","Right","RHH","RH") ~ "R",
    TRUE ~ as.character(d$BatterSide)
  )
  
  # Barrel flag for outcome shapes
  d$Barrel <- compute_barrel_flag_leaderboard(d)
  
  # Build a robust per-PA id for the grid (prefer PitchofPA if present)
  pa_grid_id <- {
    if ("PitchofPA" %in% names(d)) {
      pn <- suppressWarnings(as.integer(readr::parse_number(as.character(d$PitchofPA))))
      if (any(pn == 1L, na.rm = TRUE)) {
        start <- pn == 1L
        start[is.na(start)] <- FALSE
        cumsum(start)
      } else {
        NULL
      }
    } else {
      NULL
    }
  }
  if (!is.null(pa_grid_id)) {
    if (all(is.na(pa_grid_id)) || !length(pa_grid_id)) pa_grid_id <- NULL
  }
  if (is.null(pa_grid_id)) {
    pa_cols <- c("PlateAppearanceID","PlateAppearanceId","PAId","PA_ID","PAID","AtBatNumber","PA","Pa")
    pa_col <- intersect(pa_cols, names(d))[1]
    if (!is.na(pa_col)) {
      pa_grid_id <- as.character(d[[pa_col]])
    }
  }
  if (!is.null(pa_grid_id)) {
    if (all(is.na(pa_grid_id)) || !length(pa_grid_id)) pa_grid_id <- NULL
  }
  if (is.null(pa_grid_id) && all(c("Inning","PAofInning") %in% names(d))) {
    if ("Batter" %in% names(d)) {
      pa_grid_id <- interaction(d$Inning, d$PAofInning, d$Batter, drop = TRUE)
    } else {
      pa_grid_id <- interaction(d$Inning, d$PAofInning, drop = TRUE)
    }
  }
  if (!is.null(pa_grid_id)) {
    if (all(is.na(pa_grid_id)) || !length(pa_grid_id)) pa_grid_id <- NULL
  }
  if (is.null(pa_grid_id)) {
    d_tmp <- ensure_pa(d)
    pa_grid_id <- d_tmp$PA_ID
  }
  d$PA_GRID_ID <- pa_grid_id
  
  # Pitch number in PA for labels
  pitch_num_grid <- NULL
  if ("PitchofPA" %in% names(d)) {
    pitch_num_grid <- suppressWarnings(readr::parse_number(as.character(d$PitchofPA)))
  } else if ("PitchNum" %in% names(d)) {
    pitch_num_grid <- suppressWarnings(readr::parse_number(as.character(d$PitchNum)))
  } else if ("PitchNumberInPA" %in% names(d)) {
    pitch_num_grid <- suppressWarnings(readr::parse_number(as.character(d$PitchNumberInPA)))
  }
  d$PitchNumGrid <- pitch_num_grid
  d <- d %>%
    dplyr::group_by(PA_GRID_ID) %>%
    dplyr::mutate(
      PitchNumGrid = ifelse(is.finite(PitchNumGrid), PitchNumGrid, dplyr::row_number())
    ) %>%
    dplyr::ungroup()
  d$PitchLabel <- ifelse(is.finite(d$PitchNumGrid), as.character(as.integer(d$PitchNumGrid)), NA_character_)
  
  d$row_index <- seq_len(nrow(d))
  
  coalesce_cols <- function(df, cols) {
    cols <- intersect(cols, names(df))
    if (!length(cols)) return(rep(NA_character_, nrow(df)))
    out <- df[[cols[1]]]
    if (length(cols) > 1) {
      for (c in cols[-1]) out <- dplyr::coalesce(out, df[[c]])
    }
    out
  }
  batter_cols <- c(
    "Batter","BatterName","Batter_Name","BatterFullName","BatterFull",
    "BatterLastFirst","BatterLF","Hitter","HitterName","Hitter_Name","HitterFullName"
  )
  batter_cols <- intersect(batter_cols, names(d))
  if (length(batter_cols)) {
    d$BatterLabelRaw <- coalesce_cols(d, batter_cols)
  } else {
    d$BatterLabelRaw <- NA_character_
  }
  inning_cols <- c("Inning","InningNo","InningNumber","Inning_Number","InningNum","Inn")
  inning_cols <- intersect(inning_cols, names(d))
  if (length(inning_cols)) {
    d$InningLabelRaw <- coalesce_cols(d, inning_cols)
  } else {
    d$InningLabelRaw <- NA
  }
  
  safe_last_nonempty <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & trimws(x) != ""]
    if (length(x)) x[[length(x)]] else NA_character_
  }
  format_batter_name <- function(name_raw) {
    name_raw <- trimws(as.character(name_raw))
    if (!nzchar(name_raw)) return(NA_character_)
    if (grepl(",", name_raw)) {
      parts <- strsplit(name_raw, ",", fixed = TRUE)[[1]]
      last  <- trimws(parts[1])
      first <- trimws(parts[2] %||% "")
    } else {
      parts <- unlist(strsplit(name_raw, "\\s+"))
      if (length(parts) == 1) return(parts[1])
      first <- parts[1]
      last  <- parts[length(parts)]
    }
    fi <- if (nzchar(first)) substr(first, 1, 1) else ""
    if (nzchar(last) && nzchar(fi)) paste0(fi, ". ", last) else if (nzchar(last)) last else fi
  }
  format_inning <- function(inn_raw) {
    inn_raw <- trimws(as.character(inn_raw))
    if (!nzchar(inn_raw)) return(NA_character_)
    inn_num <- suppressWarnings(readr::parse_number(inn_raw))
    if (is.finite(inn_num)) as.character(as.integer(inn_num)) else inn_raw
  }
  
  pa_meta <- d %>%
    dplyr::group_by(PA_GRID_ID) %>%
    dplyr::summarise(
      batter_raw = safe_last_nonempty(BatterLabelRaw),
      inning_raw = safe_last_nonempty(InningLabelRaw),
      batter_side = safe_last_nonempty(BatterSide),
      first_idx  = min(row_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      batter_fmt = vapply(batter_raw, format_batter_name, character(1)),
      inning_fmt = vapply(inning_raw, format_inning, character(1)),
      title = dplyr::case_when(
        !is.na(batter_fmt) & !is.na(inning_fmt) ~ paste0(batter_fmt, " - Inning ", inning_fmt),
        !is.na(batter_fmt) ~ batter_fmt,
        !is.na(inning_fmt) ~ paste0("Inning ", inning_fmt),
        TRUE ~ "PA"
      )
    ) %>%
    dplyr::arrange(first_idx)
  
  # PA results for subtitle (last event in PA)
  pa_end <- d %>%
    dplyr::group_by(PA_GRID_ID) %>%
    dplyr::summarise(
      pr_last  = safe_last_nonempty(PlayResult),
      kb_last  = safe_last_nonempty(KorBB),
      bb_last  = safe_last_nonempty(BBType),
      tag_last = safe_last_nonempty(TaggedHitType),
      any_hbp  = {
        pc <- tolower(as.character(PitchCall %||% ""))
        hbp_flag <- any(pc == "hitbypitch", na.rm = TRUE)
        if ("HBP" %in% names(d)) hbp_flag <- hbp_flag | any(d$HBP, na.rm = TRUE)
        hbp_flag
      },
      .groups = "drop"
    )
  
  normalize_pa_result <- function(pr, kb, bbtype = NULL, tagtype = NULL, hbp_flag = FALSE) {
    pr <- trimws(as.character(pr %||% ""))
    kb <- trimws(as.character(kb %||% ""))
    bbtype <- trimws(as.character(bbtype %||% ""))
    tagtype <- trimws(as.character(tagtype %||% ""))
    pr_low <- tolower(pr); kb_low <- tolower(kb)
    
    if (isTRUE(hbp_flag) || grepl("\\bhbp\\b|hit by pitch", pr_low) || grepl("\\bhbp\\b|hit by pitch", kb_low)) {
      return("Hit by Pitch")
    }
    if (grepl("intent", kb_low) || grepl("\\bibb\\b", kb_low)) return("Intentional Walk")
    if (grepl("\\bwalk\\b|\\bbb\\b", kb_low)) return("Walk")
    if (grepl("strike.?out|\\bk\\b", kb_low)) return("Strikeout")
    
    if (nzchar(pr)) {
      if (grepl("home ?run|\\bhr\\b", pr_low)) return("Home Run")
      if (grepl("\\btriple\\b", pr_low)) return("Triple")
      if (grepl("\\bdouble\\b", pr_low) && !grepl("double\\s*play", pr_low)) return("Double")
      if (grepl("\\bsingle\\b", pr_low)) return("Single")
      if (grepl("sac|sacrifice", pr_low)) return("Sacrifice")
      if (grepl("fielder'?s choice|\\bfc\\b", pr_low)) return("Fielder's Choice")
      if (grepl("error", pr_low)) return("Reach on Error")
      if (grepl("ground", pr_low)) return("Ground Out")
      if (grepl("line", pr_low)) return("Line Out")
      if (grepl("fly", pr_low)) return("Fly Out")
      if (grepl("pop", pr_low)) return("Pop Out")
      if (grepl("\\bout\\b", pr_low)) {
        bb_low <- tolower(bbtype); tag_low <- tolower(tagtype)
        if (grepl("ground", bb_low) || grepl("ground", tag_low)) return("Ground Out")
        if (grepl("line", bb_low)   || grepl("line", tag_low))   return("Line Out")
        if (grepl("fly", bb_low)    || grepl("fly", tag_low))    return("Fly Out")
        if (grepl("pop", bb_low)    || grepl("pop", tag_low))    return("Pop Out")
        return("Out")
      }
      return(stringr::str_to_title(pr))
    }
    NA_character_
  }
  
  pa_end$pa_result <- mapply(
    normalize_pa_result,
    pr = pa_end$pr_last,
    kb = pa_end$kb_last,
    bbtype = pa_end$bb_last,
    tagtype = pa_end$tag_last,
    hbp_flag = pa_end$any_hbp,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  
  pa_meta <- pa_meta %>%
    dplyr::left_join(pa_end %>% dplyr::select(PA_GRID_ID, pa_result), by = "PA_GRID_ID") %>%
    dplyr::mutate(
      title = ifelse(!is.na(pa_result) & nzchar(pa_result), paste0(title, "\n", pa_result), title)
    )

  format_play_result_short <- function(pr) {
    pr <- trimws(as.character(pr %||% ""))
    if (!nzchar(pr)) return("")
    pr_low <- tolower(pr)
    if (grepl("home ?run|\\bhr\\b", pr_low)) return("Home Run")
    if (grepl("\\btriple\\b", pr_low)) return("Triple")
    if (grepl("\\bdouble\\b", pr_low) && !grepl("double\\s*play", pr_low)) return("Double")
    if (grepl("\\bsingle\\b", pr_low)) return("Single")
    if (grepl("sac|sacrifice", pr_low)) return("Sacrifice")
    if (grepl("fielder'?s choice|\\bfc\\b", pr_low)) return("Fielder's Choice")
    if (grepl("error", pr_low)) return("Reach on Error")
    if (grepl("\\bout\\b|groundout|flyout|lineout|popup|popout", pr_low)) return("Out")
    stringr::str_to_title(pr)
  }
  
  format_pitch_result <- function(pc, pr, is_barrel = FALSE) {
    pc <- tolower(trimws(as.character(pc %||% "")))
    pr <- as.character(pr %||% "")
    ball_calls <- c("ball","ballcalled","ballindirt","automaticball","intentionalball","intentball","pitchout")
    foul_calls <- c("foulball","foulballfieldable","foulballnotfieldable","foultip")
    inplay_calls <- c("inplay","inplayout","inplaynoout")
    
    if (pc %in% ball_calls) return("Ball Called")
    if (pc == "strikecalled") return("Strike Called")
    if (pc %in% foul_calls) return("Foul Ball")
    if (pc == "strikeswinging") return("Swing & Miss")
    if (pc == "hitbypitch") return("Hit By Pitch")
    if (pc %in% inplay_calls) {
      res <- format_play_result_short(pr)
      if (nzchar(res)) return(if (isTRUE(is_barrel)) paste0(res, " - Barrel") else res)
      return("In Play")
    }
    if (isTRUE(is_barrel)) return("Barrel")
    if (nzchar(pc)) return(stringr::str_to_title(pc))
    if (nzchar(pr)) return(format_play_result_short(pr))
    "Other"
  }
  
  build_pa_pitch_table <- function(d_pa) {
    d_pa <- tibble::as_tibble(d_pa)
    if (!nrow(d_pa)) return(NULL)
    d_pa <- d_pa %>%
      dplyr::arrange(PitchNumGrid, row_index)
    is_barrel <- if ("Barrel" %in% names(d_pa)) d_pa$Barrel else rep(FALSE, nrow(d_pa))
    res_lbl <- mapply(
      format_pitch_result,
      pc = d_pa$PitchCall %||% NA_character_,
      pr = d_pa$PlayResult %||% NA_character_,
      is_barrel = is_barrel,
      USE.NAMES = FALSE
    )
    num_lbl <- ifelse(is.finite(d_pa$PitchNumGrid),
                      paste0(as.integer(d_pa$PitchNumGrid), "."),
                      as.character(dplyr::row_number()))
    lines <- paste0(num_lbl, " ", res_lbl)
    lines <- lines[!is.na(lines) & nzchar(lines)]
    if (!length(lines)) return(NULL)
    grid::textGrob(
      paste(lines, collapse = "\n"),
      x = 0, y = 1,
      just = c("left","top"),
      gp = grid::gpar(col = "black", fontsize = 6)
    )
  }
  
  resolve_batter_side <- function(side_chr, d_pa) {
    s <- side_chr
    if (length(s) > 1) s <- s[1]
    if (is.null(s) || length(s) == 0 || !nzchar(as.character(s))) {
      bs <- as.character(d_pa$BatterSide %||% NA_character_)
      bs <- bs[!is.na(bs) & nzchar(bs)]
      if (length(bs)) s <- bs[1]
    }
    if (length(s) == 0) s <- ""
    s <- toupper(substr(as.character(s), 1, 1))
    if (!s %in% c("L","R")) s <- ""
    s
  }
  
  max_plots <- max_cols * max_rows
  if (nrow(pa_meta) > max_plots) {
    warning("[AAR page2] ", nrow(pa_meta), " PAs; showing first ", max_plots, ".")
    pa_meta <- pa_meta[seq_len(max_plots), , drop = FALSE]
  }
  
  plots <- lapply(seq_len(nrow(pa_meta)), function(i) {
    pa_id <- pa_meta$PA_GRID_ID[i]
    label <- pa_meta$title[i]
    bside <- pa_meta$batter_side[i]
    d_pa <- d %>% dplyr::filter(PA_GRID_ID == pa_id)
    p_plot <- strike_zone_plot(
      d_pa,
      title = label,
      allow_barrel_any = FALSE,
      point_size = 1.6,
      base_size = 8,
      title_size = 6,
      panel_fill = "grey95",
      label_pitch_num = TRUE,
      label_size = 2.2,
      show_batter = TRUE,
      batter_side = bside,
      style = "page2"
    ) + theme(plot.margin = margin(0,0,0,0))
    
    # Add per-PA pitch table on the opposite side of the batter silhouette
    side_chr <- resolve_batter_side(bside, d_pa)
    tbl_g <- build_pa_pitch_table(d_pa)
    if (!is.null(tbl_g) && nzchar(side_chr)) {
      if (side_chr == "L") {
        xmin <- 1.6; xmax <- 3.45
      } else {
        xmin <- -3.45; xmax <- -1.6
      }
      p_plot <- p_plot + annotation_custom(
        grob = tbl_g,
        xmin = xmin, xmax = xmax,
        ymin = 0.6, ymax = 3.9
      )
    }
    p_plot
  })
  
  if (length(plots) < max_plots) {
    blanks <- replicate(max_plots - length(plots), patchwork::plot_spacer(), simplify = FALSE)
    plots <- c(plots, blanks)
  }
  
  patchwork::wrap_plots(plots, ncol = max_cols, nrow = max_rows, byrow = FALSE) +
    patchwork::plot_layout(
      widths = rep(1, max_cols),
      heights = rep(1, max_rows)
    ) +
    patchwork::plot_annotation(theme = theme(plot.margin = margin(0,0,0,0)))
}

compose_team_report_plot <- function(game_p, season_p, game_id = NULL, season_col_label = "Season") {
  game_p   <- normalize_game_fields(game_p)
  season_p <- normalize_game_fields(season_p)
  
  game_raw   <- tibble::as_tibble(game_p)
  season_raw <- tibble::as_tibble(season_p)
  
  # --- Game date ---
  date_val <- as.Date(NA)
  if ("GameDate" %in% names(game_raw)) {
    gd <- parse_date_any(game_raw$GameDate)
    gd <- gd[!is.na(gd)]
    if (length(gd)) date_val <- gd[1]
  }
  if (is.na(date_val) && "Date" %in% names(game_raw)) {
    gd <- parse_date_any(game_raw$Date)
    gd <- gd[!is.na(gd)]
    if (length(gd)) date_val <- gd[1]
  }
  if (is.na(date_val) && !is.null(game_id) && nzchar(as.character(game_id))) {
    date_val <- parse_date_any(game_id)[1]
  }
  
  game_suffix <- ""
  if (!is.null(game_id) && nzchar(as.character(game_id))) {
    game_suffix <- stringr::str_extract(as.character(game_id), "\\(G\\d+\\)")
    if (is.na(game_suffix) || !nzchar(game_suffix)) {
      game_num <- stringr::str_match(as.character(game_id), "-(\\d+)$")[, 2]
      game_suffix <- if (!is.na(game_num) && nzchar(game_num)) paste0("(G", game_num, ")") else ""
    }
  }
  
  date_str <- if (!is.na(date_val)) {
    lt <- as.POSIXlt(date_val)
    paste0(month.name[lt$mon + 1L], " ", lt$mday, ", ", lt$year + 1900L, if (nzchar(game_suffix)) paste0(" ", game_suffix) else "")
  } else {
    as.character(game_id %||% "")
  }
  
  # --- Statline ---
  bits <- team_statline_bits(game_raw)
  bb_hbp <- bits$BB + bits$HBP
  statline_text <- sprintf(
    "PA: %s   K (SO): %s   BB/HBP: %s   Barrel: %s   Hits: %s",
    bits$PA, bits$K, bb_hbp, bits$Barrel, bits$H
  )
  
  header <- ggplot() +
    annotate("text", x = 0, y = 0.92, label = "Bobcats Pitching Staff", hjust = 0, vjust = 1, size = 5.2, fontface = "bold") +
    annotate("text", x = 0, y = 0.70, label = date_str,             hjust = 0, vjust = 1, size = 3.8) +
    annotate("text", x = 0, y = 0.48, label = statline_text,         hjust = 0, vjust = 1, size = 3.4) +
    xlim(0, 1) + ylim(0, 1) + theme_void() + theme(plot.margin = margin(0,2,0,2))
  
  # ---------- Build tables ----------
  proc_df <- tryCatch(
    build_process_table(game_raw, season_raw, season_col_label),
    error = function(e) {
      warning("[Team Report] build_process_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  
  ptpf_df <- tryCatch(
    build_team_pitchtype_perf_table(game_raw, season_raw),
    error = function(e) {
      warning("[Team Report] build_team_pitchtype_perf_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  
  row2_left  <- safe_tbl_plot(proc_df, "Process Metrics", season_col_label = season_col_label) +
    theme(plot.margin = margin(0,6,6,6))
  
  row2_right <- safe_tbl_plot(ptpf_df, "Pitch Type Performance", season_col_label = season_col_label) +
    theme(plot.margin = margin(0,6,6,6))
  
  row2 <- (row2_left | row2_right) + patchwork::plot_layout(widths = c(1, 1))
  
  # ---------- Strike-zone plots ----------
  d_plot <- prepare_aar_flags(game_raw)
  d_plot$Barrel <- compute_barrel_flag_leaderboard(d_plot)
  if (!"PlateLocSide" %in% names(d_plot)) d_plot$PlateLocSide <- NA_real_
  if (!"PlateLocHeight" %in% names(d_plot)) d_plot$PlateLocHeight <- NA_real_
  if (!"BatterSide" %in% names(d_plot)) d_plot$BatterSide <- NA_character_
  
  d_plot <- d_plot %>%
    dplyr::mutate(
      BatterSideStd = dplyr::case_when(
        BatterSide %in% c("L","Left","LHH","LH") ~ "L",
        BatterSide %in% c("R","Right","RHH","RH") ~ "R",
        TRUE ~ as.character(BatterSide)
      )
    )
  
  d_plot_out <- add_outcome_type(d_plot, allow_barrel_any = TRUE)
  d_2k   <- d_plot %>% dplyr::filter(TwoStrike %in% TRUE)
  d_last <- add_outcome_type(get_pa_last(d_plot), allow_barrel_any = TRUE)
  pr_last <- if ("PlayResult" %in% names(d_last)) {
    as.character(d_last$PlayResult)
  } else {
    rep("", nrow(d_last))
  }
  is_hit <- grepl("home ?run|\\bHR\\b", pr_last, ignore.case = TRUE) |
    grepl("\\btriple\\b", pr_last, ignore.case = TRUE) |
    (grepl("\\bdouble\\b", pr_last, ignore.case = TRUE) & !grepl("double\\s*play", pr_last, ignore.case = TRUE)) |
    grepl("\\bsingle\\b", pr_last, ignore.case = TRUE)
  
  d_hits   <- d_last[is_hit, , drop = FALSE]
  d_barrel <- d_plot_out[d_plot_out$IsBarrel %in% TRUE, , drop = FALSE]
  
  allowed_pitch_types <- c("Fastball","Sinker","Cutter","Slider","Sweeper","Curveball","Changeup","Splitter")
  pal <- pitch_colors[allowed_pitch_types]
  pal[is.na(pal)] <- "#808080"
  
  outcome_levels <- c("Ball","Called Strike","Foul","Ball in Play","Swing & Miss","Barrel")
  outcome_shapes <- c(
    "Ball"          = 1,
    "Called Strike" = 16,
    "Foul"          = 2,
    "Ball in Play"  = 17,
    "Swing & Miss"  = 4,
    "Barrel"        = 0,
    "Other"         = 3
  )
  
  # LHH
  p_l_2k <- strike_zone_plot(d_2k  %>% dplyr::filter(BatterSideStd == "L"), "2K v LHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  p_l_h  <- strike_zone_plot(d_hits %>% dplyr::filter(BatterSideStd == "L"), "Hits v LHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  p_l_b  <- strike_zone_plot(d_barrel %>% dplyr::filter(BatterSideStd == "L"), "Barrel v LHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  
  # RHH
  p_r_2k <- strike_zone_plot(d_2k  %>% dplyr::filter(BatterSideStd == "R"), "2K v RHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  p_r_h  <- strike_zone_plot(d_hits %>% dplyr::filter(BatterSideStd == "R"), "Hits v RHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  p_r_b  <- strike_zone_plot(d_barrel %>% dplyr::filter(BatterSideStd == "R"), "Barrel v RHH", allow_barrel_any = TRUE) +
    theme(plot.margin = margin(0,0,0,0), legend.position = "none")
  
  # Legend (single, above all 6 plots)
  legend_pitch <- tibble::tibble(
    x = 1,
    y = 1,
    PitchType = factor(allowed_pitch_types, levels = allowed_pitch_types)
  )
  legend_shapes <- tibble::tibble(
    x = 1,
    y = 1,
    OutcomeType = factor(outcome_levels, levels = outcome_levels)
  )
  legend_plot <- ggplot() +
    geom_point(data = legend_pitch, aes(x = x, y = y, color = PitchType), size = 3) +
    geom_point(data = legend_shapes, aes(x = x, y = y, shape = OutcomeType), color = "black", size = 3) +
    scale_color_manual(values = pal, breaks = allowed_pitch_types, limits = allowed_pitch_types, drop = FALSE) +
    scale_shape_manual(values = outcome_shapes, breaks = outcome_levels, limits = outcome_levels, drop = FALSE) +
    guides(
      color = guide_legend(title = "Pitch Type", order = 1, nrow = 1, byrow = TRUE),
      shape = guide_legend(title = "Location", order = 2, nrow = 1, byrow = TRUE, override.aes = list(color = "black"))
    ) +
    theme_void() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      legend.text  = element_text(size = 9),
      legend.key.width = unit(0.9, "lines"),
      legend.spacing.x = unit(0.2, "lines"),
      plot.margin = margin(0,0,0,0)
    )
  legend_grob <- tryCatch(cowplot::get_legend(legend_plot), error = function(e) NULL)
  legend_gg <- if (!is.null(legend_grob)) cowplot::ggdraw(legend_grob) else ggplot() + theme_void()
  
  row3_plots <- (p_l_2k | p_l_h | p_l_b | p_r_2k | p_r_h | p_r_b) +
    patchwork::plot_layout(ncol = 6)
  row3  <- (row3_plots / legend_gg) + patchwork::plot_layout(heights = c(0.90, 0.10))
  
  # ---------- Count breakdown ----------
  count_df <- tryCatch(
    build_count_breakdown_table(game_raw, season_raw, season_col_label),
    error = function(e) {
      warning("[Team Report] build_count_breakdown_table failed: ", conditionMessage(e))
      data.frame(Status = paste("Unavailable:", conditionMessage(e)))
    }
  )
  row_counts <- safe_tbl_plot(count_df, "Count Breakdown", season_col_label = season_col_label) +
    theme(plot.margin = margin(2,6,6,6))
  
  final <- header / row2 / row_counts / row3 +
    patchwork::plot_layout(heights = c(0.10, 0.27, 0.23, 0.40))
  
  # ---------- Watermark layering ----------
  wm_path <- find_asset(c("Bobcatlogo","bobcatlogo","BobcatLogo","bobcat_logo","Bobcat"))
  wm_grob <- NULL
  if (!is.null(wm_path)) {
    ext <- tolower(tools::file_ext(wm_path))
    img <- if (ext == "png") png::readPNG(wm_path) else if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG(wm_path) else NULL
    if (!is.null(img)) {
      wm_grob <- grid::rasterGrob(img, x = 0.5, y = 0.5, width = 0.80, height = 0.80, just = "center",
                                  interpolate = TRUE, gp = grid::gpar(alpha = 0.60))
    }
  }
  
  cowplot::ggdraw() +
    { if (!is.null(wm_grob)) cowplot::draw_grob(wm_grob, x = 0.5, y = 0.5, width = 1, height = 1) } +
    cowplot::draw_plot(final, x = 0, y = 0, width = 1, height = 1)
}

render_AAR_pdf <- function(game_p, season_p, pitcher_name, game_date, opponent, outfile,
                           arm_angle_deg = NULL, season_col_label = "Season") {
  
  # Build date/opponent label safely
  date_val <- parse_date_any(game_date)[1]
  sp   <- rawToChar(as.raw(32))  # " "
  dash <- rawToChar(as.raw(45))  # "-"
  sep  <- paste0(sp, dash, sp)
  
  date_str <- ""
  if (!is.na(date_val)) {
    lt <- as.POSIXlt(date_val)
    date_str <- paste0(month.name[lt$mon + 1L], sp, lt$mday, ",", sp, lt$year + 1900L)
  }
  
  game_lab <- date_str
  if (!is.null(opponent) && nzchar(opponent)) {
    game_lab <- paste(date_str, opponent, sep = sep)
  }
  
  final <- compose_AAR_plot(
    game_p        = game_p,
    season_p      = season_p,
    pitcher_name  = pitcher_name,
    game_label    = game_lab,
    arm_angle_deg = arm_angle_deg,
    season_col_label = season_col_label
  )
  page2 <- compose_AAR_pa_grid_plot(game_p = game_p)
  
  # Write PDF (omit string args like paper="special")
  grDevices::pdf(outfile, width = 8.5, height = 14, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(final)
  print(page2)
  invisible(outfile)
}

render_pdf_page_png <- function(pdf_path, page = 1L, dpi = 160) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The pdftools package is required to render report previews.")
  }
  bitmap <- pdftools::pdf_render_page(
    pdf = pdf_path,
    page = as.integer(page),
    dpi = as.numeric(dpi)
  )
  png_path <- tempfile(fileext = ".png")
  png::writePNG(bitmap, png_path)
  png_path
}

render_plot_pdf_preview <- function(plot, width = 8.5, height = 14, dpi = 160) {
  pdf_path <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_path), add = TRUE)
  grDevices::pdf(pdf_path, width = width, height = height, useDingbats = FALSE)
  tryCatch(print(plot), finally = grDevices::dev.off())
  render_pdf_page_png(pdf_path, page = 1L, dpi = dpi)
}

# --- PA last-pitch helper (safe everywhere) ---
get_pa_last <- function(d) {
  d <- ensure_pa(d)
  d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
}

# ---- PA/Count safety used by AAR ----
ensure_pa <- function(d) {
  d <- tibble::as_tibble(d)
  
  # If PA scaffolding already present, just ensure FirstPitch & PitchNum
  if ("PA_ID" %in% names(d) && "PitchNum" %in% names(d)) {
    if (!"FirstPitch" %in% names(d)) d$FirstPitch <- d$PitchNum == 1L
    return(d)
  }
  
  pc <- as.character(d$PitchCall %||% NA_character_)
  
  term <- pc %in% c("InPlay","InPlayNoOut","InPlayOut")
  
  if ("PlayResult" %in% names(d))
    term <- term | grepl("strike.?out|\\bK\\b", as.character(d$PlayResult), ignore.case = TRUE)
  
  if ("KorBB" %in% names(d))
    term <- term | grepl("\\bK\\b|strikeout", as.character(d$KorBB), ignore.case = TRUE)
  
  # Strikeout via count (StrikesAfterPitch / Strikes reaching 3)
  if ("StrikesAfterPitch" %in% names(d))
    term <- term | suppressWarnings(as.integer(d$StrikesAfterPitch) >= 3L)
  if ("Strikes" %in% names(d))
    term <- term | suppressWarnings(as.integer(d$Strikes) >= 3L)
  
  # Start of a PA occurs after a terminal pitch
  start <- c(TRUE, head(term, -1))
  
  d$.pa_id <- cumsum(start)
  d <- d %>%
    dplyr::group_by(.pa_id) %>%
    dplyr::mutate(
      PA_ID    = .pa_id,
      PitchNum = dplyr::row_number(),
      FirstPitch = (PitchNum == 1L)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.pa_id)
  
  d
}

# -------------------- Pitch colors --------------------
pitches <- c("Fastball","Four-Seam","Two-Seam","Sinker","Slider","Sweeper",
             "Curveball","ChangeUp","Splitter","Cutter")

facet_levels <- c("Fastball","Sinker","Changeup","Splitter","Slider","Cutter","Sweeper","Curveball")
pitch_levels_all <- c(facet_levels, "Undefined", "Untagged")

# -------------------- Pitch colors --------------------
pitch_colors <- c(
  "Fastball"  = "#FF0000",
  "Four-Seam" = "#FF0000",
  "Two-Seam"  = "#FF0000",
  "Sinker"    = "#FFA500",
  "Slider"    = "#FFFF00",
  "Sweeper"   = "#FFD700",
  "Curveball" = "#89CFF0",
  "Changeup"  = "#008000",
  "Splitter"  = "#000080",
  "Cutter"    = "#000000",
  "Untagged"  = "#808080",
  "Undefined" = "#808080"
)

to_num <- function(x) {
  if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
}

nz_chr <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

pick_first <- function(cands, in_df) {
  cands <- cands[cands %in% names(in_df)]
  if (length(cands)) cands[[1]] else NA_character_
}

zone_h_in <- function(h) {
  h <- to_num(h)
  if (!length(h)) return(h)
  q90 <- suppressWarnings(stats::quantile(h, 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 10) h <- h * 12
  h
}

zone_s_in <- function(s) {
  s <- to_num(s)
  if (!length(s)) return(s)
  q90 <- suppressWarnings(stats::quantile(abs(s), 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 5) s <- s * 12
  s
}

in_zone_inches <- function(s_in, h_in) {
  is.finite(s_in) & is.finite(h_in) &
    dplyr::between(h_in, 18.29, 44.08) &
    dplyr::between(s_in, -9.97, 9.97)
}

canon_pitch_call <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("_|-", " ", x)
  x <- gsub("\\s+", " ", x)
  xl <- tolower(x)
  
  ifelse(
    grepl("strike.*(called|looking)|called.*strike|\\bcs\\b|\\bstrike look", xl, perl = TRUE),
    "StrikeCalled",
    ifelse(
      grepl("strike.*(swing|miss|whiff)|swing.*strike|\\bss\\b|\\bswstr\\b", xl, perl = TRUE),
      "StrikeSwinging",
      ifelse(
        grepl("foul\\s*tip", xl, perl = TRUE),
        "FoulTip",
        ifelse(
          grepl("foul.*(pop|fly|fieldable|line|ground)", xl, perl = TRUE),
          "FoulBallFieldable",
          ifelse(
            grepl("\\bfoul\\b", xl, perl = TRUE) & !grepl("foul[\\s-]*tip", xl, perl = TRUE),
            "FoulBallNotFieldable",
            ifelse(
              grepl("in\\s*play.*no\\s*out|in\\s*play.*noout|inplay.*no\\s*out", xl, perl = TRUE),
              "InPlayNoOut",
              ifelse(
                grepl("in\\s*play.*out", xl, perl = TRUE),
                "InPlayOut",
                ifelse(
                  grepl("in\\s*play|ball\\s*in\\s*play|\\bbip\\b", xl, perl = TRUE),
                  "InPlay",
                  ifelse(
                    grepl("\\bball\\b|pitchout|auto\\s*ball|intentional|\\bibb\\b", xl, perl = TRUE),
                    "BallCalled",
                    x
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

canonical_pitch_fuzzy <- function(x) {
  x0 <- trimws(as.character(x))
  x0 <- gsub("_|-", " ", x0)
  x0 <- gsub("\\s+", " ", x0)
  dplyr::case_when(
    grepl("(?i)fast ?ball|\\bff\\b|\\bfb\\b|\\bfour ?seam|\\b4 ?seam|\\b2 ?seam|\\btwo ?seam|\\bft\\b", x0) ~ "Fastball",
    grepl("(?i)sink|\\bsi\\b", x0) ~ "Sinker",
    grepl("(?i)change|\\bch\\b", x0) ~ "Changeup",
    grepl("(?i)split|fork", x0) ~ "Splitter",
    grepl("(?i)slider|\\bsl\\b", x0) ~ "Slider",
    grepl("(?i)cutter|\\bct\\b|\\bfc\\b", x0) ~ "Cutter",
    grepl("(?i)sweep", x0) ~ "Sweeper",
    grepl("(?i)curve|knuckle\\s*curve|slurve|\\bcu\\b|\\bkc\\b", x0) ~ "Curveball",
    x0 == "" ~ "Untagged",
    TRUE ~ "Undefined"
  )
}

# -------------------- Performance time-series stats --------------------
PERF_TS_PERF_STATS <- list(
  "K%"           = list(key = "Kp",             type = "pct"),
  "BB%"          = list(key = "BBp",            type = "pct"),
  "Barrel%"      = list(key = "Barrel_pct",     type = "pct"),
  "GB%"          = list(key = "GB_pct",         type = "pct"),
  "Whiff%"       = list(key = "Whiff_pct",      type = "pct"),
  "CSW%"         = list(key = "CSW_pct",        type = "pct"),
  "IZWhiff%"     = list(key = "IZWhiff_pct",    type = "pct"),
  "Chase%"       = list(key = "Chase_pct",      type = "pct"),
  "Strike%"      = list(key = "Strike_pct",     type = "pct"),
  "Zone%"        = list(key = "Zone_pct",       type = "pct"),
  "Pre2k Zone%"  = list(key = "Pre2kZone_pct",  type = "pct"),
  "FPS%"         = list(key = "FPS",            type = "pct"),
  "1-1 Win%"     = list(key = "Win11_pct",      type = "pct"),
  "E&A%"         = list(key = "EA",             type = "pct"),
  "Put Away%"    = list(key = "PutAway_pct",    type = "pct"),
  "wOBA"         = list(key = "wOBA",           type = "num"),
  "wOBAcon"      = list(key = "wOBAcon",        type = "num"),
  "SLG"          = list(key = "SLG",            type = "num"),
  "OPS"          = list(key = "OPS",            type = "num"),
  "FIP"          = list(key = "FIP",            type = "num"),
  "Avg Velo"     = list(key = "AvgVelo",        type = "num"),
  "Max Velocity" = list(key = "MaxVelocity",   type = "num")
)
PERF_TS_PITCH_METRICS <- list(
  "iVB"       = list(key = "IVB",        type = "num"),
  "HB"        = list(key = "HB",         type = "num"),
  "Avg Velo"  = list(key = "AvgVelo",    type = "num"),
  "Extension" = list(key = "Extension", type = "num"),
  "VAA"       = list(key = "VAA",        type = "num"),
  "Usage%"    = list(key = "Usage_pct", type = "pct")
)
PERF_TS_ALL_STATS <- unique(c(names(PERF_TS_PERF_STATS), names(PERF_TS_PITCH_METRICS)))
PERF_TS_MAIN_CHOICES <- setdiff(names(PERF_TS_PERF_STATS), c("Avg Velo", "Max Velocity"))
PERF_TS_PTYPE_CHOICES <- setdiff(PERF_TS_ALL_STATS, c("K%", "BB%", "E&A%"))
PERF_TS_PERF_STATS_PTYPE <- setdiff(
  names(PERF_TS_PERF_STATS),
  c("K%","BB%","FPS%","1-1 Win%","E&A%")
)
PERF_TS_STAT_COLORS <- setNames(scales::hue_pal()(length(PERF_TS_ALL_STATS)), PERF_TS_ALL_STATS)
PERF_TS_ROLL_WINDOW <- 5
XRV_TS_ROLL_WINDOW <- 25

TEAM_CLASS_COLORS <- c(
  Freshman = "#F4C430",
  Sophomore = "#2563EB",
  Junior = "#2E8B57",
  Senior = "#D62828"
)

# Academic class for pitchers represented in the 2026 team data. Redshirt
# designations are grouped with their underlying academic class for the chart.
PITCHER_CLASS_2026 <- c(
  "jonathan anders" = "Junior",
  "alec beversdorf" = "Junior",
  "will canalichio" = "Junior",
  "tanner carson" = "Freshman",
  "jackson cotton" = "Freshman",
  "bennett fryman" = "Junior",
  "jacob gholston" = "Junior",
  "sam hall" = "Junior",
  "nolan moore" = "Freshman",
  "cade smith" = "Sophomore",
  "titan targac" = "Freshman",
  "jesus tovar" = "Junior",
  "tyler walton" = "Freshman",
  "cole wisenbaker" = "Junior"
)

pitcher_class_2026 <- function(x) {
  cls <- unname(PITCHER_CLASS_2026[name_norm(x)])
  ifelse(is.na(cls), "Unknown", cls)
}

valid_ts_pitch_type <- function(x) {
  x <- trimws(as.character(x %||% ""))
  bad <- c("", "Bad data", "Other", "Undefined", "Untagged", "Unknown", "NA", "N/A", "None")
  !is.na(x) & nzchar(x) & !(tolower(x) %in% tolower(bad))
}

plot_aar_movement_gg <- function(d){
  dd <- d %>%
    dplyr::mutate(
      HB  = suppressWarnings(as.numeric(HorzBreak)),
      IVB = suppressWarnings(as.numeric(InducedVertBreak)),
      PitchType_plot = dplyr::if_else(
        is.na(PitchType) | !(as.character(PitchType) %in% names(pitch_colors)),
        "Undefined", as.character(PitchType)
      )
    ) %>%
    dplyr::filter(is.finite(HB), is.finite(IVB))
  
  if (!nrow(dd)) {
    return(ggplot() + theme_void() + labs(title = "No movement data"))
  }
  present <- unique(dd$PitchType_plot)
  pal <- pitch_colors
  pal[setdiff(names(pal), present)] <- NULL
  pal[is.na(pal)] <- "#808080"
  
  ggplot(dd, aes(HB, IVB, color = PitchType_plot)) +
    geom_point(
      alpha = 0.9,
      size = 2.2,
      shape = 16   # ALWAYS filled circles
    ) +
    scale_color_manual(values = pal, drop = FALSE) +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25), expand = FALSE) +
    labs(title = "Pitch Movement (HB vs iVB)", x = "HB (in)", y = "iVB (in)") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.justification = "top",
      legend.box.just = "top"
    )
}

# -------------------- Data load (robust) --------------------
# Optional: set DATA_DIR=/absolute/path or relative folder before launching
data_dir <- Sys.getenv("DATA_DIR", unset = "")

# Resolve app directory even if the working dir is different
app_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
app_dir  <- if (!is.null(app_file) && nzchar(app_file)) dirname(normalizePath(app_file)) else getwd()

as_abs <- function(p) {
  if (!nzchar(p)) return("")
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

# Candidate folders to search (in this order)
candidates <- unique(Filter(
  function(p) nzchar(p) && dir.exists(p),
  c(
    as_abs(data_dir),                 # env override, if set
    file.path(app_dir, "data"),       # app root
    file.path(app_dir, "www", "data"),
    file.path(app_dir, "Data"),
    "data",                           # fallback: working dir relative
    "www/data",
    "Data"
  )
))

# 1) Prefer a single RDS if present (fast & reliable in deployment)
rds_candidates <- unique(Filter(
  nzchar,
  c(
    if (nzchar(data_dir)) file.path(as_abs(data_dir), "data.rds") else "",
    file.path(app_dir, "data", "data.rds"),
    "data/data.rds"
  )
))
rds_path <- rds_candidates[file.exists(rds_candidates)][1]

if (exists("BASE_PITCHING_DATA", inherits = FALSE) &&
    is.data.frame(BASE_PITCHING_DATA)) {
  # The integrated BASE workspace injects one normalized team payload from the
  # shared source route. Standalone Wally behavior remains unchanged when the
  # injected object is absent.
  df <- tibble::as_tibble(BASE_PITCHING_DATA)
  rm(BASE_PITCHING_DATA)
} else if (!is.na(rds_path) && nzchar(rds_path)) {
  df <- readRDS(rds_path)
} else {
  # 2) Otherwise collect CSVs from the first candidate that has any
  csvs <- unlist(lapply(candidates, function(d) {
    list.files(d, pattern = "\\.csv$", full.names = TRUE)
  }))
  if (length(csvs)) {
    skip_csvs <- c(
      "player_heights.csv",
      "xrv_metrics.csv",
      "called_stuff_baseline.csv",
      "d1_pitch_metric_percentile_reference.csv"
    )
    csvs <- csvs[!basename(csvs) %in% skip_csvs]
  }
  if (length(csvs)) {
    # 1) Read ALL columns as character so bind_rows never chokes on mixed types
    df <- purrr::map_dfr(
      csvs,
      ~ tryCatch(
        readr::read_csv(
          .x,
          col_types      = readr::cols(.default = readr::col_character()),
          show_col_types = FALSE
        ) %>%
          dplyr::mutate(
            source_file = basename(.x),
            row_in_file = dplyr::row_number()
          ),
        error = function(e) {
          warning("Failed to read ", .x, ": ", conditionMessage(e))
          tibble::tibble()
        }
      )
    )
    
    # 2) Convert types consistently across the combined data,
    #    but keep common ID-like fields as character
    id_like <- intersect(
      names(df),
      c("PitcherId","BatterId","GameId","PitcherID","BatterID","GameID")
    )
    spec <- readr::cols(.default = readr::col_guess())
    for (nm in id_like) spec$cols[[nm]] <- readr::col_character()
    df <- readr::type_convert(df, col_types = spec)
    
  } else {
    
    warning("No CSV files found in any of: ",
            paste(if (length(candidates)) candidates else "(none)", collapse = ", "),
            ". Starting with empty data.")
    # typed, 0-row tibble so downstream code is stable
    df <- tibble::tibble(
      HorzBreak = numeric(), InducedVertBreak = numeric(),
      SpinAxis = numeric(),  SpinRate = numeric(),
      PlateLocHeight = numeric(), PlateLocSide = numeric(),
      RelSpeed = numeric(),  VertApprAngle = numeric(), HorzApprAngle = numeric(),
      RelHeight = numeric(), RelSide = numeric(), Extension = numeric(),
      PitchCall = character(), BatterSide = character(), Date = character(),
      AwayTeam = character(), HomeTeam = character(), Pitcher = character()
    )
    df$source_file <- character(0)
    df$row_in_file <- integer(0)
  }
}

# Ensure expected columns exist (fill with NA if missing)
need_cols <- c(
  "HorzBreak","InducedVertBreak","SpinAxis","SpinRate","PlateLocHeight",
  "PlateLocSide","RelSpeed","VertApprAngle","HorzApprAngle","RelHeight",
  "RelSide","Extension","PitchCall","BatterSide","Date","AwayTeam","HomeTeam","Pitcher",
  "PlayResult","KorBB","OutsOnPlay","BBType","TaggedHitType"
)

miss <- setdiff(need_cols, names(df))
if (length(miss)) for (nm in miss) df[[nm]] <- NA

# IDs for lasso/re-tagging
if (!"source_file" %in% names(df)) df$source_file <- NA_character_
if (!"row_in_file" %in% names(df)) df$row_in_file <- NA_integer_
native_pitch_uid <- if ("PitchUID" %in% names(df)) trimws(as.character(df$PitchUID)) else rep(NA_character_, nrow(df))
df$pitch_uid <- ifelse(!is.na(native_pitch_uid) & nzchar(native_pitch_uid),
                       native_pitch_uid,
                       sprintf("%s::%s", df$source_file, df$row_in_file))

dedupe_pitch_rows <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(d)
  
  pitch_key <- if ("PitchUID" %in% names(d)) trimws(as.character(d$PitchUID)) else rep(NA_character_, nrow(d))
  play_key  <- if ("PlayID"   %in% names(d)) trimws(as.character(d$PlayID))   else rep(NA_character_, nrow(d))
  file_key  <- if (all(c("source_file", "row_in_file") %in% names(d))) {
    paste0(as.character(d$source_file), "::", as.character(d$row_in_file))
  } else {
    rep(NA_character_, nrow(d))
  }
  
  d$.dedupe_key <- dplyr::coalesce(
    ifelse(!is.na(pitch_key) & nzchar(pitch_key), paste0("pitch:", pitch_key), NA_character_),
    ifelse(!is.na(play_key)  & nzchar(play_key),  paste0("play:", play_key),   NA_character_),
    ifelse(!is.na(file_key)  & nzchar(file_key),  paste0("file:", file_key),   NA_character_),
    paste0("row:", seq_len(nrow(d)))
  )
  
  d %>%
    dplyr::distinct(.data$.dedupe_key, .keep_all = TRUE) %>%
    dplyr::select(-".dedupe_key")
}

df <- dedupe_pitch_rows(df)
df$row_id <- seq_len(nrow(df))

# ---- xRV precomputed metrics (optional) ----
xrv_candidates <- unique(Filter(
  nzchar,
  c(
    if (nzchar(data_dir)) file.path(as_abs(data_dir), "xrv_metrics.csv") else "",
    file.path(app_dir, "data", "xrv_metrics.csv"),
    "data/xrv_metrics.csv"
  )
))
xrv_path <- xrv_candidates[file.exists(xrv_candidates)][1]

if (!("xrv" %in% names(df)) && !is.na(xrv_path) && nzchar(xrv_path)) {
  xrv_df <- tryCatch(
    readr::read_csv(xrv_path, show_col_types = FALSE),
    error = function(e) {
      warning("Failed to read model metrics: ", conditionMessage(e))
      tibble::tibble()
    }
  )
  if (nrow(xrv_df) && all(c("source_file", "row_in_file") %in% names(xrv_df))) {
    xrv_df$source_file <- as.character(xrv_df$source_file)
    xrv_df$row_in_file <- as.integer(xrv_df$row_in_file)
    xrv_df <- dplyr::distinct(xrv_df, .data$source_file, .data$row_in_file, .keep_all = TRUE)
    df <- df %>% dplyr::left_join(xrv_df, by = c("source_file", "row_in_file"))
    for (nm in c("xrv", "arm_angle", "xiVB", "iVB_oe")) {
      if (nm %in% names(df)) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
  }
}

# ---- Season grouping (season CSV format) ----
# Prefer an explicit column if your new season CSVs have one; otherwise infer from source_file.
infer_season_group_from_file <- function(f) {
  f <- tolower(as.character(f %||% ""))
  dplyr::case_when(
    grepl("s25|2025[_ -]?season|season[_ -]?2025", f) ~ "S25",
    grepl("f25|2025[_ -]?fall|fall[_ -]?2025",       f) ~ "F25",
    grepl("sq26|2026[_ -]?squads|squads[_ -]?2026",   f) ~ "SQ26",
    grepl("s26|2026[_ -]?season|season[_ -]?2026",    f) ~ "S26",
    grepl("portal", f)                                 ~ "PORT",
    TRUE ~ NA_character_
  )
}

# normalize any existing season-like column names into one code column
season_col_candidates <- intersect(
  names(df),
  c("SeasonGroup","Season_Group","Season","SeasonTag","Season_Code","SeasonCode")
)

df$SeasonGroup <- NA_character_
if (length(season_col_candidates)) {
  df$SeasonGroup <- as.character(df[[season_col_candidates[1]]])
}

# fallback to filename inference when missing
need_sg <- is.na(df$SeasonGroup) | !nzchar(trimws(df$SeasonGroup))
if (any(need_sg)) df$SeasonGroup[need_sg] <- infer_season_group_from_file(df$source_file[need_sg])

# hard-normalize values to your app codes
df$SeasonGroup <- toupper(trimws(df$SeasonGroup))
df$SeasonGroup <- dplyr::case_when(
  df$SeasonGroup %in% c("S25","2025 SEASON","2025_SEASON") ~ "S25",
  df$SeasonGroup %in% c("F25","2025 FALL","2025_FALL")     ~ "F25",
  df$SeasonGroup %in% c("SQ26","2026 SQUADS","2026_SQUADS")~ "SQ26",
  df$SeasonGroup %in% c("S26","2026 SEASON","2026_SEASON") ~ "S26",
  df$SeasonGroup %in% c("PORT","PORTAL","PORTAL SEASON","PORTAL_SEASON") ~ "PORT",
  TRUE ~ df$SeasonGroup
)


if (!"TaggedPitchType" %in% names(df)) df$TaggedPitchType <- NA_character_
if (!"AutoPitchType"   %in% names(df)) df$AutoPitchType   <- NA_character_

# ---- Date parsing helpers for season grouping ----
parse_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  # normalize
  x0 <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x0))
  
  # 1) If string contains a leading ISO date anywhere, grab it (handles "YYYY-MM-DD: ...")
  iso <- stringr::str_extract(x0, "(?<!\\d)\\d{4}-\\d{2}-\\d{2}(?!\\d)")
  ok_iso <- !is.na(iso)
  if (any(ok_iso)) out[ok_iso] <- suppressWarnings(as.Date(iso[ok_iso], format = "%Y-%m-%d"))
  
  # 2) If string contains an 8-digit yyyymmdd anywhere, grab it
  need <- is.na(out) & nzchar(x0)
  if (any(need)) {
    ymd8 <- stringr::str_extract(x0[need], "(?<!\\d)\\d{8}(?!\\d)")
    ok8  <- !is.na(ymd8)
    if (any(ok8)) {
      idx <- which(need)[ok8]
      out[idx] <- suppressWarnings(as.Date(ymd8[ok8], format = "%Y%m%d"))
    }
  }
  
  # 3) If string contains m/d/yyyy or mm/dd/yyyy, grab it
  need <- is.na(out) & nzchar(x0)
  if (any(need)) {
    mdy <- stringr::str_extract(x0[need], "(?<!\\d)\\d{1,2}/\\d{1,2}/\\d{4}(?!\\d)")
    okm <- !is.na(mdy)
    if (any(okm)) {
      idx <- which(need)[okm]
      out[idx] <- suppressWarnings(as.Date(mdy[okm], format = "%m/%d/%Y"))
    }
  }
  
  # 4) Last resort: parse each remaining value without letting one malformed
  # game label abort the whole report archive.
  need <- is.na(out) & nzchar(x0)
  for (idx in which(need)) {
    out[idx] <- tryCatch(
      suppressWarnings(as.Date(x0[[idx]], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y"))),
      error = function(e) as.Date(NA)
    )
  }
  
  out
}

BARREL_EXCLUDE_START <- as.Date("2026-02-27")
BARREL_EXCLUDE_END   <- as.Date("2026-03-01")

barrel_date_ok <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(logical(0))
  
  date_vec <- NULL
  if ("GameDate" %in% names(d)) date_vec <- parse_date_any(d$GameDate)
  if (all(is.na(date_vec)) && "Date" %in% names(d)) date_vec <- parse_date_any(d$Date)
  if (all(is.na(date_vec)) && "UTCDate" %in% names(d)) date_vec <- parse_date_any(d$UTCDate)
  if (all(is.na(date_vec)) && "LocalDateTime" %in% names(d)) date_vec <- parse_date_any(d$LocalDateTime)
  if (all(is.na(date_vec)) && "CustomGameID" %in% names(d)) date_vec <- parse_date_any(d$CustomGameID)
  
  if (is.null(date_vec) || all(is.na(date_vec))) {
    return(rep(TRUE, nrow(d)))
  }
  
  bad <- (date_vec >= BARREL_EXCLUDE_START) & (date_vec <= BARREL_EXCLUDE_END)
  ok <- !(bad %in% TRUE)
  ok[is.na(ok)] <- TRUE
  ok
}


extract_date_from_filename <- function(fname) {
  # pull an 8-digit yyyymmdd from basename
  s <- as.character(fname)
  ymd <- stringr::str_extract(s, "(?<!\\d)(\\d{8})(?!\\d)")
  suppressWarnings(as.Date(ymd, format = "%Y%m%d"))
}

# -------------------- Feature engineering --------------------
df <- df %>%
  dplyr::mutate(
    Pitcher = fix_name_commas(Pitcher),
    PitchType_custom = dplyr::case_when(
      Pitcher == "Gabe Appelbaum" & RelSpeed > 65 ~ "Fastball",
      Pitcher == "Gabe Appelbaum" & RelSpeed < 65 ~ "Slider",
      TRUE ~ NA_character_
    ),
    PitchType = dplyr::coalesce(PitchType_custom, TaggedPitchType, AutoPitchType, "Untagged"),
    PitchType = dplyr::case_when(
      PitchType %in% c("ChangeUp","Changeup") ~ "Changeup",
      TRUE ~ as.character(PitchType)
    ),
    PitchType = factor(PitchType, levels = union(names(pitch_colors), as.character(sort(unique(PitchType))))),
    inZone = as.integer(derive_zone_inches(cur_data())),
    Chase = dplyr::case_when(
      is.na(inZone) ~ NA_integer_,
      inZone == 0L & PitchCall %in% c("FoulBall","FoulBallNotFieldable","InPlay","StrikeSwinging") ~ 1L,
      inZone %in% c(0L, 1L) ~ 0L,
      TRUE ~ NA_integer_
    ),
    # --- BatterSide normalization (L/R only) ---
    BatterSide = dplyr::case_when(
      BatterSide %in% c("L","Left","LHH","LH") ~ "L",
      BatterSide %in% c("R","Right","RHH","RH") ~ "R",
      TRUE ~ as.character(BatterSide)
    )
  )

# ---- Bullpens (Bullpens CSV only) ----
normalize_person_name <- function(x) {
  x <- fix_name_commas(x)
  tolower(gsub("\\s+", " ", trimws(as.character(x))))
}
is_bullpens_csv <- function(x) {
  x <- tolower(basename(ifelse(is.na(x), "", x)))
  grepl("^bullpens(?:\\b|[[:space:]_-]).*\\.csv$", x) || x %in% c("bullpens.csv", "bullpens - cleaned.csv")
}

if (!"source_file" %in% names(df)) df$source_file <- NA_character_

flag_by_file <- vapply(df$source_file, is_bullpens_csv, logical(1))
df$is_bullpen <- flag_by_file

# bullpen-only df + stable display IDs (build labels ONLY from bullpens_df to avoid length mismatch)
bullpens_df <- df[df$is_bullpen %in% TRUE, , drop = FALSE]
if (nrow(bullpens_df)) {
  bp_label <- if ("Date" %in% names(bullpens_df)) as.character(bullpens_df$Date)
  else rep(NA_character_, nrow(bullpens_df))
  
  idx <- is.na(bp_label) | bp_label == ""
  if (any(idx)) bp_label[idx] <- bullpens_df$source_file[idx]
  
  idx <- is.na(bp_label) | bp_label == ""
  if (any(idx)) bp_label[idx] <- "(undated)"
  
  bullpens_df$CustomGameID_BP <- paste0(bp_label, ": Bullpen - ", bullpens_df$Pitcher)
} else {
  bullpens_df$CustomGameID_BP <- character(0)
}
bp_game_ids <- sort(unique(stats::na.omit(bullpens_df$CustomGameID_BP)))
if (!length(bp_game_ids)) bp_game_ids <- character(0)

# -------------------- Hand inference --------------------
fb_types <- c("Fastball","Four-Seam")
pitcher_hand_map <- df %>%
  dplyr::filter(PitchType %in% fb_types, !is.na(HorzBreak)) %>%
  dplyr::group_by(Pitcher) %>%
  dplyr::summarise(meanHB = mean(HorzBreak, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(hand = ifelse(meanHB < 0, "LHP", "RHP")) %>%
  dplyr::select(Pitcher, hand) %>%
  tibble::deframe()

# ---- Ensure GameDate exists (Date class) ----
if (!"GameDate" %in% names(df)) df$GameDate <- as.Date(NA)

# 1) Try the Date column directly (handles "YYYY-MM-DD" or "YYYYMMDD")
d_from_Date <- parse_date_any(df$Date)
df$GameDate[is.na(df$GameDate)] <- d_from_Date[is.na(df$GameDate)]

# 2) If still NA, try to extract YYYYMMDD from CustomGameID (e.g., "20250214: ABC @ DEF")
need <- is.na(df$GameDate)
if (any(need) && "CustomGameID" %in% names(df)) {
  ymd <- stringr::str_extract(as.character(df$CustomGameID[need]), "(?<!\\d)(\\d{8})(?!\\d)")
  df$GameDate[need] <- suppressWarnings(as.Date(ymd, format = "%Y%m%d"))
}

# 3) If still NA, try the filename
need <- is.na(df$GameDate)
if (any(need) && "source_file" %in% names(df)) {
  df$GameDate[need] <- extract_date_from_filename(df$source_file[need])
}

# ---- Use the CSV GameID as the canonical game key for all downstream filters ----
get_game_col <- function(data, nm) {
  if (nm %in% names(data)) as.character(data[[nm]]) else rep(NA_character_, nrow(data))
}

df <- df %>%
  dplyr::mutate(
    .Away3 = dplyr::if_else(!is.na(AwayTeam) & nzchar(AwayTeam), stringr::str_sub(AwayTeam, 1, 3), "UNK"),
    .Home3 = dplyr::if_else(!is.na(HomeTeam) & nzchar(HomeTeam), stringr::str_sub(HomeTeam, 1, 3), "UNK"),
    GameID = dplyr::coalesce(
      get_game_col(df, "GameID"),
      get_game_col(df, "GameUID"),
      get_game_col(df, "GameId"),
      get_game_col(df, "Game"),
      get_game_col(df, "CustomGameID")
    ),
    CustomGameID = dplyr::coalesce(
      GameID,
      dplyr::if_else(
        !is.na(GameDate),
        paste0(format(GameDate, "%Y-%m-%d"), ": ", .Away3, " @ ", .Home3),
        NA_character_
      )
    )
  ) %>%
  dplyr::select(-.Away3, -.Home3)

# -------------------- Team derivation --------------------
has <- function(x) x %in% names(df)
if (!has("PitcherTeam")) df$PitcherTeam <- NA_character_
if (has("PitcherTeamAbbrev")) df$PitcherTeam <- dplyr::coalesce(df$PitcherTeam, df$PitcherTeamAbbrev)
if (has("PitchingTeam"))      df$PitcherTeam <- dplyr::coalesce(df$PitcherTeam, df$PitchingTeam)
if (has("InningTopBot")) {
  df$PitcherTeam <- dplyr::coalesce(
    df$PitcherTeam,
    ifelse(df$InningTopBot %in% c("Top","T","TOP"), df$HomeTeam,
           ifelse(df$InningTopBot %in% c("Bottom","B","BOT"), df$AwayTeam, NA_character_))
  )
}
df$PitcherTeam <- gsub("\\s+", " ", trimws(df$PitcherTeam))

# -------------------- Team filter (TXST) --------------------
TEAM_CODE <- "TEX_BOB"
if (!"is_bullpen" %in% names(df)) df$is_bullpen <- FALSE

team_or_portal_mask <- function(d, team_col, team_code = TEAM_CODE) {
  if (is.null(d) || !nrow(d)) return(logical(0))
  if (!(team_col %in% names(d)) || is.null(team_code) || !nzchar(team_code)) {
    return(rep(TRUE, nrow(d)))
  }
  team_vals <- toupper(trimws(as.character(d[[team_col]])))
  keep <- !is.na(team_vals) & team_vals == toupper(trimws(team_code))
  season_col <- if ("SeasonGroup" %in% names(d)) {
    "SeasonGroup"
  } else if ("SeasonTag" %in% names(d)) {
    "SeasonTag"
  } else {
    NULL
  }
  if (!is.null(season_col)) {
    season_vals <- toupper(trimws(as.character(d[[season_col]])))
    keep <- keep | (!is.na(season_vals) & season_vals == "PORT")
  }
  keep
}

filter_team_or_portal <- function(d, team_col, team_code = TEAM_CODE) {
  if (is.null(d) || !nrow(d)) return(d)
  d[team_or_portal_mask(d, team_col, team_code), , drop = FALSE]
}

prefer_team_or_portal <- function(d, team_col, team_code = TEAM_CODE) {
  if (is.null(d) || !nrow(d)) return(d)
  d_team <- filter_team_or_portal(d, team_col, team_code)
  if (nrow(d_team) > 0) d_team else d
}

# Always start with non-bullpen rows
df_nonbp <- df %>% dplyr::filter(!(is_bullpen %in% TRUE))

# Only apply TEAM_CODE filter if PitcherTeam exists AND it actually matches something
txst_df <- df_nonbp
if ("PitcherTeam" %in% names(df_nonbp) && any(nzchar(trimws(as.character(df_nonbp$PitcherTeam))))) {
  cand <- filter_team_or_portal(df_nonbp, "PitcherTeam")
  if (nrow(cand) > 0) txst_df <- cand
}

# Final safety: never allow TXST dataset to be empty due to team code mismatch
if (!nrow(txst_df)) {
  warning("TEAM_CODE filter produced 0 rows (TEAM_CODE=", TEAM_CODE, "). Using non-bullpen data without team filtering.")
  txst_df <- df_nonbp
}

txst_df$CustomGameID <- as.character(txst_df$CustomGameID)
txst_pitches <- txst_df



game_dates <- txst_df %>%
  dplyr::distinct(CustomGameID, GameDate) %>%
  dplyr::filter(!is.na(GameDate))
# --- harden GameDate + CustomGameID types (prevents empty selections / no-data) ---
if ("GameDate" %in% names(game_dates)) {
  # handles "2026-01-15", "1/15/2026", and POSIXct
  game_dates$GameDate <- suppressWarnings(as.Date(game_dates$GameDate))
  if (all(is.na(game_dates$GameDate))) {
    game_dates$GameDate <- suppressWarnings(as.Date(game_dates$GameDate, format = "%m/%d/%Y"))
  }
}
if ("CustomGameID" %in% names(game_dates)) {
  game_dates$CustomGameID <- as.character(game_dates$CustomGameID)
}

# --- normalize types for robust filtering/selection ---
game_dates$GameDate    <- as.Date(game_dates$GameDate)
game_dates$CustomGameID <- as.character(game_dates$CustomGameID)


# helper to make NA-free, sorted choices
nz_choices <- function(x) {
  x <- sort(unique(stats::na.omit(x)))
  if (length(x)) x else NULL
}

# Convert "First Last" -> "Last, First" for display
to_last_first <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  out <- vapply(x, function(s) {
    if (!nzchar(s)) return(s)
    if (grepl(",", s)) return(trimws(s))
    parts <- unlist(strsplit(s, "\\s+"))
    if (length(parts) <= 1) return(s)
    last <- parts[length(parts)]
    first <- paste(parts[-length(parts)], collapse = " ")
    paste0(last, ", ", first)
  }, character(1))
  out
}

# Named choices for pitchers: label = "Last, First", value = "First Last"
as_pitcher_choices <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
  x <- unique(as.character(x))
  labels <- to_last_first(x)
  ord <- order(tolower(labels), na.last = TRUE)
  x <- x[ord]
  labels <- labels[ord]
  stats::setNames(x, labels)
}

# For select/picker inputs: named choices where label == value
as_named_choices <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
  x <- sort(unique(as.character(x)))
  stats::setNames(x, x)
}

# Build game dropdown choices (safe, similar to HittingApp)
make_games_txst <- function(d) {
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(character(0))
  out <- d %>%
    dplyr::filter(!is.na(CustomGameID), nzchar(CustomGameID)) %>%
    dplyr::distinct(CustomGameID, GameDate) %>%
    dplyr::arrange(dplyr::desc(GameDate))
  stats::setNames(out$CustomGameID, out$CustomGameID)
}
# Build fallback CustomGameID values when they are missing/empty
fallback_game_ids <- function(d) {
  if (is.null(d) || !nrow(d)) return(character(0))
  if ("GameID" %in% names(d)) {
    ids <- as.character(d$GameID)
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    if (length(ids)) return(unique(ids))
  }
  gd <- if ("GameDate" %in% names(d)) parse_date_any(d$GameDate) else parse_date_any(d$Date %||% NA)
  away <- if ("AwayTeam" %in% names(d)) as.character(d$AwayTeam) else ""
  home <- if ("HomeTeam" %in% names(d)) as.character(d$HomeTeam) else ""
  away3 <- ifelse(!is.na(away) & nzchar(away), substr(away, 1, 3), "UNK")
  home3 <- ifelse(!is.na(home) & nzchar(home), substr(home, 1, 3), "UNK")
  id <- paste0(format(gd, "%Y-%m-%d"), ": ", away3, " @ ", home3)
  id <- id[!is.na(gd) & nzchar(id)]
  unique(id)
}

txst_pitchers <- nz_choices(txst_df$Pitcher)
if (is.null(txst_pitchers)) txst_pitchers <- nz_choices(df$Pitcher)
if (is.null(txst_pitchers)) txst_pitchers <- "(no data)"

txst_game_ids <- nz_choices(txst_df$CustomGameID)
if (is.null(txst_game_ids)) txst_game_ids <- nz_choices(df$CustomGameID)
if (is.null(txst_game_ids)) txst_game_ids <- character(0)
txst_game_ids <- order_game_ids_desc(txst_game_ids, game_dates)
# Precompute game choices for the sidebar
games_txst <- make_games_txst(txst_df)

# ---- Leaderboard date defaults (safe) ----
leader_date_vec <- if ("GameDate" %in% names(txst_df)) {
  parse_date_any(txst_df$GameDate)
} else if ("Date" %in% names(txst_df)) {
  parse_date_any(txst_df$Date)
} else {
  parse_date_any(NA)
}
leader_date_vec <- leader_date_vec[is.finite(leader_date_vec)]
leader_date_min <- if (length(leader_date_vec)) min(leader_date_vec) else NULL
leader_date_max <- if (length(leader_date_vec)) max(leader_date_vec) else NULL

# -------------------- Season -> game ID vectors (used by sidebar defaults + server updates) --------------------
games_by_season <- function(season_code) {
  ids <- txst_df %>%
    dplyr::filter(.data$SeasonGroup %in% season_code) %>%
    dplyr::pull(.data$CustomGameID)
  ids <- nz_choices(ids)
  order_game_ids_desc(ids %||% character(0), game_dates)
}

games_2025_season <- games_by_season("S25")
games_2025_fall   <- games_by_season("F25")
games_2026_squads <- games_by_season("SQ26")
games_2026_season <- games_by_season("S26")
games_portal      <- games_by_season("PORT")

GAMES_BY_SEASON <- list(
  S25  = games_2025_season,
  F25  = games_2025_fall,
  SQ26 = games_2026_squads,
  S26  = games_2026_season,
  PORT = games_portal
)
# ---- Pitcher-aware game ID helper (season + optional bullpens) ----
games_for_pitcher <- function(pitcher, season_codes = NULL, include_bullpens = FALSE) {
  if (is.null(pitcher) || !nzchar(pitcher)) return(character(0))
  
  d <- txst_df %>% dplyr::filter(.data$Pitcher == pitcher)
  
  if (!is.null(season_codes) && length(season_codes)) {
    d <- d %>% dplyr::filter(.data$SeasonGroup %in% season_codes)
  }
  
  ids <- d %>% dplyr::pull(.data$CustomGameID)
  ids <- nz_choices(ids) %||% character(0)
  ids <- order_game_ids_desc(ids, game_dates)
  
  if (isTRUE(include_bullpens) && exists("bullpens_df", inherits = TRUE) && nrow(bullpens_df)) {
    bp_ids <- bullpens_df %>%
      dplyr::filter(.data$Pitcher == pitcher) %>%
      dplyr::pull(.data$CustomGameID_BP)
    bp_ids <- nz_choices(bp_ids) %||% character(0)
    ids <- c(ids, bp_ids)
  }
  
  unique(ids)
}



bh_choices <- c("L","R")

 

# --- keep IDs/dates consistent with GameInput selections ---
if (exists("txst_pitches", inherits = TRUE)) {
  if ("CustomGameID" %in% names(txst_pitches)) txst_pitches$CustomGameID <- as.character(txst_pitches$CustomGameID)
  if ("GameDate" %in% names(txst_pitches)) {
    txst_pitches$GameDate <- parse_date_any(txst_pitches$GameDate)
  }
}


# -------------------- Plate geometry --------------------
home_plate_segments <- data.frame(
  x=c(0,0.71,0.71,0,-0.71,-0.71), y=c(0.15,0.15,0.3,0.5,0.3,0.15),
  xend=c(0.71,0.71,0,-0.71,-0.71,0), yend=c(0.15,0.3,0.5,0.3,0.15,0.15)
)
stroke_zone_tmp <- data.frame(xmin=-0.71, xmax=0.71, ymin=1.60, ymax=3.40)
# keep your original name
strike_zone <- stroke_zone_tmp

framing_zone_type <- function(s_in, h_in) {
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  
  in_heart <- is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 6.7) & dplyr::between(h_in, 22, 38)
  in_zone <- is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 10.0) & dplyr::between(h_in, 18, 42) & !in_heart
  in_shadow <- is.finite(abs_s) & is.finite(h_in) & (
    (abs_s > 10.0 & abs_s <= 13.3 & dplyr::between(h_in, 18, 42)) |
      (abs_s <= 13.3 & dplyr::between(h_in, 42, 46)) |
      (abs_s <= 13.3 & dplyr::between(h_in, 14, 18))
  )
  
  dplyr::case_when(
    in_heart  ~ "Heart",
    in_zone   ~ "Zone",
    in_shadow ~ "Shadow",
    TRUE      ~ "Chase"
  )
}

framing_in_zone <- function(s_in, h_in) {
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 10.0) & dplyr::between(h_in, 18, 42)
}

framing_zone_stats <- function(d, mode = c("ball_to_strike", "strike_to_ball")) {
  mode <- match.arg(mode)
  zones <- c("Heart", "Zone", "Shadow", "Chase")
  
  if (!nrow(d)) {
    out <- tibble::tibble(Zone = zones, Chances = 0L, Strikes = 0L, Pct = NA_real_)
    tot <- tibble::tibble(Zone = "Total", Chances = 0L, Strikes = 0L, Pct = NA_real_)
    return(dplyr::bind_rows(out, tot))
  }
  
  sx_in <- zone_s_in(d$plate_x)
  hz_in <- zone_h_in(d$plate_z)
  zone_type <- framing_zone_type(sx_in, hz_in)
  
  pc <- canon_pitch_call(d$pitch_call)
  called <- pc %in% c("BallCalled", "StrikeCalled")
  in_zone_std <- framing_in_zone(sx_in, hz_in)
  
  if (mode == "ball_to_strike") {
    idx <- called & !in_zone_std
    strikes <- pc %in% "StrikeCalled" & idx
  } else {
    idx <- called & in_zone_std
    strikes <- pc %in% "BallCalled" & idx
  }
  
  df <- tibble::tibble(Zone = zone_type, Chance = idx, Strike = strikes) %>%
    dplyr::filter(.data$Chance %in% TRUE)
  
  out <- df %>%
    dplyr::group_by(.data$Zone) %>%
    dplyr::summarise(
      Chances = sum(.data$Chance, na.rm = TRUE),
      Strikes = sum(.data$Strike, na.rm = TRUE),
      .groups = "drop"
    )
  
  out <- tibble::tibble(Zone = zones) %>%
    dplyr::left_join(out, by = "Zone") %>%
    dplyr::mutate(
      Chances = dplyr::coalesce(.data$Chances, 0L),
      Strikes = dplyr::coalesce(.data$Strikes, 0L),
      Pct = dplyr::if_else(.data$Chances > 0, .data$Strikes / .data$Chances, NA_real_)
    )
  
  tot <- tibble::tibble(
    Zone = "Total",
    Chances = sum(out$Chances, na.rm = TRUE),
    Strikes = sum(out$Strikes, na.rm = TRUE)
  ) %>%
    dplyr::mutate(Pct = dplyr::if_else(.data$Chances > 0, .data$Strikes / .data$Chances, NA_real_))
  
  dplyr::bind_rows(out, tot)
}

catcher_framing_subset <- function(d, type = c("ball_to_strike", "strike_to_ball")) {
  type <- match.arg(type)
  if (!nrow(d)) return(d[0, , drop = FALSE])
  
  px <- to_num(d$plate_x)
  pz <- to_num(d$plate_z)
  sx_in <- zone_s_in(px)
  hz_in <- zone_h_in(pz)
  in_zone_calc <- framing_in_zone(sx_in, hz_in)
  keep <- is.finite(px) & is.finite(pz)
  
  d <- d[keep, , drop = FALSE]
  in_zone_calc <- in_zone_calc[keep]
  pc <- canon_pitch_call(d$pitch_call)
  
  x_plot <- px[keep]
  z_plot <- pz[keep]
  if (is.finite(suppressWarnings(max(abs(x_plot), na.rm = TRUE))) &&
      suppressWarnings(max(abs(x_plot), na.rm = TRUE)) > 5) {
    x_plot <- x_plot / 12
  }
  if (is.finite(suppressWarnings(max(abs(z_plot), na.rm = TRUE))) &&
      suppressWarnings(max(abs(z_plot), na.rm = TRUE)) > 10) {
    z_plot <- z_plot / 12
  }
  d$plot_x <- x_plot
  d$plot_z <- z_plot
  
  if (type == "ball_to_strike") {
    d <- d %>% dplyr::filter(in_zone_calc %in% FALSE, pc %in% "StrikeCalled")
  } else {
    d <- d %>% dplyr::filter(in_zone_calc %in% TRUE, pc %in% "BallCalled")
  }
  
  if (!nrow(d)) return(d)
  
  d %>%
    dplyr::arrange(.data$PitchNum) %>%
    dplyr::mutate(PitchNumSub = dplyr::row_number())
}

catcher_framing_zone_plot <- function(d, title = NULL) {
  xlim <- c(-2, 2)
  ylim <- c(-0.5, 4.5)
  
  heart_box <- data.frame(xmin = -6.7 / 12, xmax = 6.7 / 12, ymin = 22 / 12, ymax = 38 / 12)
  zone_box <- data.frame(xmin = -10 / 12, xmax = 10 / 12, ymin = 18 / 12, ymax = 42 / 12)
  shadow_outline <- data.frame(xmin = -13.3 / 12, xmax = 13.3 / 12, ymin = 14 / 12, ymax = 46 / 12)
  
  if (!nrow(d)) {
    return(
      ggplot() +
        geom_rect(data = shadow_outline, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, colour = "black", linetype = "dashed", linewidth = 0.9) +
        geom_rect(data = zone_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, colour = "black", linetype = "solid", linewidth = 1.0) +
        geom_rect(data = heart_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, colour = "#0B7A3B", linetype = "dashed", linewidth = 1.0) +
        geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                     inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
        coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
        scale_x_reverse() +
        labs(title = title %||% "Strike Zone", x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
    )
  }
  
  d$PitchType <- factor(as.character(d$PitchType), levels = pitch_levels_all)
  
  if (!("plot_x" %in% names(d)) || !("plot_z" %in% names(d))) {
    x_raw <- to_num(d$plate_x)
    z_raw <- to_num(d$plate_z)
    if (is.finite(suppressWarnings(max(abs(x_raw), na.rm = TRUE))) &&
        suppressWarnings(max(abs(x_raw), na.rm = TRUE)) > 5) {
      x_raw <- x_raw / 12
    }
    if (is.finite(suppressWarnings(max(abs(z_raw), na.rm = TRUE))) &&
        suppressWarnings(max(abs(z_raw), na.rm = TRUE)) > 10) {
      z_raw <- z_raw / 12
    }
    d$plot_x <- x_raw
    d$plot_z <- z_raw
  }
  d <- d %>% dplyr::filter(is.finite(.data$plot_x), is.finite(.data$plot_z))
  
  if (nrow(d)) {
    xmax <- max(2, max(abs(d$plot_x), na.rm = TRUE))
    ymin <- min(-0.5, min(d$plot_z, na.rm = TRUE))
    ymax <- max(4.5, max(d$plot_z, na.rm = TRUE))
    xlim <- c(-xmax, xmax)
    ylim <- c(ymin, ymax)
  }
  
  ggplot(d, aes(x = -plot_x, y = plot_z)) +
    geom_rect(data = shadow_outline, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linetype = "dashed", linewidth = 0.9) +
    geom_rect(data = zone_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linetype = "solid", linewidth = 1.0) +
    geom_rect(data = heart_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "#0B7A3B", linetype = "dashed", linewidth = 1.0) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
    geom_point(aes(fill = PitchType),
               shape = 21, size = 7.4, colour = "black", stroke = 0.9, alpha = 0.95, show.legend = TRUE) +
    geom_text(aes(label = PitchNumSub), size = 4.4, fontface = "bold", color = "white") +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_reverse() +
    scale_fill_manual(values = pitch_colors, breaks = facet_levels, limits = facet_levels, drop = FALSE, name = "Pitch Type") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black", face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

catcher_framing_legend_plot <- function() {
  labs_df <- data.frame(PitchType = facet_levels, y = seq_along(facet_levels))
  
  ggplot(labs_df, aes(x = 0.2, y = y)) +
    geom_point(aes(fill = PitchType), shape = 21, size = 4.6, color = "black", stroke = 0.7) +
    geom_text(aes(x = 0.75, label = PitchType), hjust = 0, size = 4.0, color = "black") +
    scale_fill_manual(values = pitch_colors, limits = facet_levels, drop = FALSE) +
    coord_cartesian(xlim = c(0, 3.2), ylim = c(0.5, length(facet_levels) + 0.5), expand = FALSE, clip = "off") +
    theme_void(base_size = 12) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

catcher_framing_table <- function(d) {
  if (!nrow(d)) {
    return(tibble::tibble(
      P = integer(),
      Inn = integer(),
      PA = integer(),
      `PA P#` = integer(),
      Pitcher = character(),
      `Pitch type` = character(),
      Hitter = character(),
      Count = character(),
      Result = character()
    ))
  }
  
  get_chr <- function(nm) if (nm %in% names(d)) nz_chr(d[[nm]]) else rep("", nrow(d))
  last_name <- function(x) {
    x <- nz_chr(x)
    ifelse(grepl(",", x),
           {parts <- strsplit(x, ",\\s*"); vapply(parts, function(p) if (length(p) >= 1) p[1] else x, "", USE.NAMES = FALSE)},
           {parts <- strsplit(x, "\\s+"); vapply(parts, function(p) if (length(p) >= 1) p[length(p)] else x, "", USE.NAMES = FALSE)})
  }
  
  d %>%
    dplyr::mutate(
      P = .data$PitchNumSub,
      PA = .data$PAofInning,
      `PA P#` = .data$PitchofPA,
      Inn = .data$Inning,
      `Pitch type` = as.character(.data$PitchType),
      Pitcher = last_name(get_chr("Pitcher")),
      Hitter = last_name(get_chr("Batter")),
      Count = get_chr("CountStr"),
      Result = get_chr("pitch_call")
    ) %>%
    dplyr::select(P, Inn, PA, `PA P#`, Pitcher, `Pitch type`, Hitter, Count, Result)
}

prepare_catcher_receiving_rows <- function(d) {
  if (!nrow(d)) return(d)
  
  if (!("pitch_call" %in% names(d))) {
    pc_col <- pick_first(c("PitchCall", "Pitch_Call", "Call", "PitchResult", "Pitch_Result"), d)
    d$pitch_call <- if (!is.na(pc_col)) as.character(d[[pc_col]]) else NA_character_
  }
  d$pitch_call <- canon_pitch_call(d$pitch_call)
  
  if (all(c("source_file", "row_in_file") %in% names(d))) {
    d <- d %>% dplyr::distinct(.data$source_file, .data$row_in_file, .keep_all = TRUE)
  } else if (all(c("Date", "Inning", "PAofInning", "PitchofPA") %in% names(d))) {
    d <- d %>% dplyr::distinct(.data$Date, .data$Inning, .data$PAofInning, .data$PitchofPA, .keep_all = TRUE)
  }
  
  n_now <- nrow(d)
  get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n_now)
  
  normalize_plate_coord <- function(vec, colname) {
    v <- to_num(vec)
    if (!length(v)) return(v)
    is_inch_name <- !is.na(colname) && grepl("(?i)inch|_in\\b", colname)
    q90 <- suppressWarnings(stats::quantile(abs(v), 0.90, na.rm = TRUE))
    if (!is.finite(q90)) q90 <- 0
    looks_like_inches <- if (!is.na(colname) && grepl("(?i)z|height", colname)) q90 > 6 else q90 > 3.5
    if (is_inch_name || looks_like_inches) v <- v / 12
    v
  }
  coalesce_plate <- function(cands) {
    if (!length(cands)) return(rep(NA_real_, n_now))
    mats <- lapply(cands, function(nm) normalize_plate_coord(d[[nm]], nm))
    out <- mats[[1]]
    if (length(mats) > 1) {
      for (k in 2:length(mats)) out <- dplyr::coalesce(out, mats[[k]])
    }
    out
  }
  
  px_candidates <- intersect(c("plate_x", "PlateLocSide", "px", "PlateX", "Plate_X", "PlateLocSideInches"), names(d))
  pz_candidates <- intersect(c("plate_z", "PlateLocHeight", "pz", "PlateZ", "Plate_Z", "PlateLocHeightInches"), names(d))
  d$plate_x <- coalesce_plate(px_candidates)
  d$plate_z <- coalesce_plate(pz_candidates)
  
  d$PitchNum <- seq_len(nrow(d))
  
  pt_chr <- dplyr::coalesce(
    get_chr("PitchType_UNI"),
    get_chr("pitch_type_canon"),
    get_chr("PitchType"),
    get_chr("PitchName"),
    get_chr("TaggedPitchType"),
    get_chr("AutoPitchType")
  )
  pt_chr <- canonical_pitch_fuzzy(pt_chr)
  pt_chr[is.na(pt_chr) | !(pt_chr %in% c(facet_levels, "Undefined", "Untagged"))] <- "Undefined"
  d$PitchType <- factor(pt_chr, levels = pitch_levels_all)
  
  balls_col <- pick_first(c("Balls", "BallsBeforePitch", "BallCount", "BallsCount", "PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes", "StrikesBeforePitch", "StrikeCount", "StrikesCount", "PitcherStrikes"), d)
  d$CountStr <- sprintf(
    "%s-%s",
    if (!is.na(balls_col)) to_num(d[[balls_col]]) else NA_integer_,
    if (!is.na(strikes_col)) to_num(d[[strikes_col]]) else NA_integer_
  )
  
  d
}


# -------------------- Theme --------------------
txst_theme <- bs_theme(
  version   = 5,
  primary   = "#501214",
  secondary = "#B4975A",
  bootswatch = "flatly"
)

app_title_link <- tags$a(
  href   = "https://BobcatsPitchingReports.com",
  target = "_blank",
  class  = "app-title-link",
  "Bobcats Pitching Reports"
)

# append extra table styling to the existing head_css
# ========= REPLACE YOUR EXISTING head_css WITH THIS ONE =========
head_css <- htmltools::tags$head(
  htmltools::tags$style(htmltools::HTML("
    :root{
      --txst-maroon:#501214;
      --txst-gold:#B4975A;
      --bs-primary: var(--txst-maroon);
      --bs-secondary: var(--txst-gold);
      --bs-link-color: var(--txst-maroon);
      --bs-link-hover-color: var(--txst-gold);
      --bs-focus-ring-color: rgba(80,18,20,0.25);
    }

    a, .btn-link{ color:var(--txst-maroon) !important; }
    a:hover, .btn-link:hover{ color:var(--txst-gold) !important; }

    .btn-primary{
      background-color:var(--txst-maroon) !important;
      border-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
    }
    .btn-primary:hover, .btn-primary:focus{
      background-color:var(--txst-gold) !important;
      border-color:var(--txst-gold) !important;
      color:var(--txst-maroon) !important;
    }
    .btn-secondary{
      background-color:var(--txst-gold) !important;
      border-color:var(--txst-gold) !important;
      color:var(--txst-maroon) !important;
    }
    .btn-outline-primary{
      color:var(--txst-maroon) !important;
      border-color:var(--txst-maroon) !important;
    }
    .btn-outline-primary:hover, .btn-outline-primary:focus{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
      border-color:var(--txst-maroon) !important;
    }
    .btn-outline-secondary{
      color:var(--txst-gold) !important;
      border-color:var(--txst-gold) !important;
    }
    .btn-outline-secondary:hover, .btn-outline-secondary:focus{
      background-color:var(--txst-gold) !important;
      color:var(--txst-maroon) !important;
      border-color:var(--txst-gold) !important;
    }

    input[type=\"checkbox\"], input[type=\"radio\"]{
      accent-color:var(--txst-maroon);
    }
    .form-check-input:checked{
      background-color:var(--txst-maroon) !important;
      border-color:var(--txst-maroon) !important;
    }
    .form-check-input:focus{
      border-color:var(--txst-gold) !important;
      box-shadow:0 0 0 .15rem rgba(180,151,90,.35) !important;
    }
    .form-select:focus, .form-control:focus{
      border-color:var(--txst-gold) !important;
      box-shadow:0 0 0 .15rem rgba(180,151,90,.35) !important;
    }

    .irs--shiny .irs-bar,
    .irs--shiny .irs-single,
    .irs--shiny .irs-from,
    .irs--shiny .irs-to{
      background:var(--txst-maroon) !important;
      border-top-color:var(--txst-maroon) !important;
    }
    .irs--shiny .irs-handle{
      border-color:var(--txst-maroon) !important;
    }
    .irs--shiny .irs-grid-text{
      color:var(--txst-maroon) !important;
    }

    .selectize-control.multi .selectize-input > div,
    .selectize-control.single .selectize-input .item{
      background:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
      border-color:var(--txst-maroon) !important;
    }
    .selectize-dropdown .active{
      background:var(--txst-gold) !important;
      color:var(--txst-maroon) !important;
    }

    .btn-default{
      background-color:transparent !important;
      border-color:var(--txst-maroon) !important;
      color:var(--txst-maroon) !important;
    }
    .btn-default:hover, .btn-default:focus{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
      border-color:var(--txst-maroon) !important;
    }

    .btn-group .btn{
      border-color:var(--txst-maroon) !important;
      color:var(--txst-maroon) !important;
      background-color:transparent !important;
    }
    .btn-group .btn.active, .btn-group .btn:active{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
    }

    .datepicker td.active, .datepicker td.active:hover,
    .datepicker td span.active, .datepicker td span.active:hover{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
    }
    .datepicker td.today, .datepicker td.today:hover{
      background-color:var(--txst-gold) !important;
      color:var(--txst-maroon) !important;
    }

    .dropdown-menu .dropdown-item.active,
    .dropdown-menu .dropdown-item:active,
    .dropdown-menu .dropdown-item:hover{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
    }

    #leader_exclude,
    #leader_exclude .form-group{
      width:100%;
    }
    #leader_exclude .shiny-options-group{
      display:grid;
      grid-template-columns:repeat(6, minmax(140px, 1fr));
      column-gap:2rem;
      row-gap:1rem;
      width:100%;
    }
    #leader_exclude .checkbox{
      margin:0;
      display:flex;
      align-items:flex-start;
      gap:6px;
      width:100%;
    }
    #leader_exclude .checkbox input[type='checkbox']{
      margin-top:3px;
      flex:0 0 auto;
    }
    #leader_exclude .checkbox label{
      display:block;
      line-height:2.0;
      white-space:normal;
      margin:0;
      flex:1 1 auto;
    }

    /* ================= NAV / TITLE BAR ================= */
    .navbar, .bslib-navbar, .bslib-page-header,
    .page-sidebar .navbar, .page-sidebar .navbar .container-fluid{
      background-color:var(--txst-maroon) !important;
      border:0 !important;
      box-shadow:none !important;
    }
    .navbar .navbar-brand, .navbar .nav-link,
    .bslib-navbar .navbar-brand, .bslib-page-header .navbar-brand,
    .app-title-link{
      color:var(--txst-gold) !important;
    }
    .app-title-link{font-weight:700;text-decoration:none}
    .app-title-link:hover{text-decoration:underline}

    /* kill stray 1px lines / borders under headers & tabs */
    .navbar, .bslib-navbar, .bslib-page-header, .nav-tabs{
      border-bottom:0 !important; box-shadow:none !important;
      background-clip:padding-box;
    }

    /* ================= NAV TABS ================= */
    .nav-tabs{
      background-color:var(--txst-maroon) !important;
      padding:.25rem .5rem; border-radius:.5rem; margin-bottom:.25rem;
    }
    .nav-tabs .nav-link{
      color:var(--txst-gold) !important; background:transparent !important;
      border:0 !important; position:relative;
    }
    .nav-tabs .nav-link.active::after{
      content:\"\"; position:absolute; left:0; right:0; bottom:-3px;
      height:3px; background:var(--txst-gold);
    }

    /* ================= CARD / PANEL TITLES ================= */
    .card-header, .panel-heading, .bslib-card .card-header{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important; border:0 !important;
    }
    .card-header .card-title, .panel-title{
      color:var(--txst-gold) !important; font-weight:700; margin:0;
    }

    /* ================= APP WATERMARKS ================= */
    html, body { height:100%; }
    body::after{
      content:\"\"; position:fixed; inset:0;
      background-image:url('/Bobcatlogo.png');    /* place file in /www */
      background-repeat:no-repeat; background-position:center center;
      background-size:85vmin; opacity:0.28; pointer-events:none; z-index:0;
    }
    body::before{
      content:\"\"; position:fixed;
      right:max(16px, env(safe-area-inset-right));
      bottom:max(16px, env(safe-area-inset-bottom));
      width:clamp(120px,22vmin,360px); height:clamp(120px,22vmin,360px);
      background-image:url('/txstlogo.png');      /* place file in /www */
      background-repeat:no-repeat; background-position:right bottom;
      background-size:contain; opacity:0.30; pointer-events:none; z-index:0;
    }
    .navbar, .bslib-navbar, .bslib-page-header,
    .container-fluid, .bslib-grid, .page-sidebar,
    .nav-tabs, .tab-content, .card, .table{
      position:relative; z-index:1;
    }

        /* ========== TABLE AESTHETICS (Performance & Movement/Metrics) ========== */

    /* Make conditional-format cells fill full cell area (big/solid shading) */
    table.dataTable tbody td{ padding:2px; position:relative; }
    td > .cf-cell{
      display:block; width:100%; min-height:32px;
      padding:6px 8px; margin:0; box-sizing:border-box;
      border:2px solid #fff; border-radius:4px; background-clip:padding-box;
    }

    /* Remove heavy borders so shading is clean */
    #performance_table table.dataTable,
    #performance_table table.dataTable th,
    #performance_table table.dataTable td,
    #metrics table.dataTable,
    #metrics table.dataTable th,
    #metrics table.dataTable td,
    #metrics_perf table.dataTable,
    #metrics_perf table.dataTable th,
    #metrics_perf table.dataTable td{
      border:none !important; box-shadow:none !important;
    }
    #performance_table table.dataTable,
    #metrics table.dataTable,
    #metrics_perf table.dataTable{
      border-collapse:separate !important; border-spacing:0 !important;
    }

    /* Pitch Sequencing tables */
    table.pitch-seq.dataTable,
    table.pitch-seq.dataTable th,
    table.pitch-seq.dataTable td{
      border:none !important; box-shadow:none !important;
    }
    table.pitch-seq.dataTable{
      border-collapse:separate !important; border-spacing:0 !important;
    }

    /* Maroon header row with gold text */
    #performance_table table.dataTable thead th,
    #metrics table.dataTable thead th,
    #metrics_perf table.dataTable thead th,
    #leaderboard_table table.dataTable thead th,
    #leaderboard_totals_table table.dataTable thead th{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important; font-weight:700;
    }

    table.pitch-seq.dataTable thead th{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important; font-weight:700;
    }

    /* RE-ENABLE zebra stripes for readability (under the semi-transparent cf-cell) */
    /* Restore DT zebra striping for nav tables */
    #performance_table table.dataTable.stripe tbody tr.odd,
    #metrics            table.dataTable.stripe tbody tr.odd,
    #metrics_perf       table.dataTable.stripe tbody tr.odd{
      background-color: rgba(0,0,0,0.035) !important;
    }
    #performance_table table.dataTable tbody tr.even td,
    #metrics            table.dataTable tbody tr.even td,
    #metrics_perf       table.dataTable tbody tr.even td{
      background-color: #ffffff !important;
    }
    table.pitch-seq.dataTable.stripe tbody tr.odd{
      background-color: rgba(0,0,0,0.035) !important;
    }
    table.pitch-seq.dataTable tbody tr.even td{
      background-color: #ffffff !important;
    }



    /* Slightly tighter rows for readability */
    #performance_table table.dataTable tbody td,
    #metrics table.dataTable tbody td,
    #metrics_perf table.dataTable tbody td{
      line-height:1.15;
    }
    table.pitch-seq.dataTable tbody td{
      line-height:1.15;
    }

    /* Table section title bars */
    .table-title{
      background-color:var(--txst-maroon);
      color:var(--txst-gold);
      font-weight:700;
      padding:6px 10px;
      border-radius:4px;
      display:inline-block;
    }
    .table-title.release-title{
      display:block;
      width:100%;
      box-sizing:border-box;
    }
    .base-pitching-performance-tables .table-title.mt-3{
      margin-top:22px !important;
    }

    .cr-percentile-column{display:flex}
    .cr-percentile-column > .shiny-html-output{display:flex;width:100%}
    .cr-percentile-card{background:rgba(255,255,255,0.94);border:1px solid rgba(0,117,138,.32);border-radius:8px;padding:12px 14px;margin-bottom:12px;min-height:650px;width:100%;display:flex;flex-direction:column}
    .cr-percentile-header{display:flex;align-items:flex-start;justify-content:space-between;gap:10px;border-bottom:2px solid #078197;padding-bottom:6px;margin-bottom:8px}
    .cr-percentile-title{font-weight:800;color:#222;font-size:1rem;line-height:1.1}
    .cr-percentile-subtitle{font-size:.78rem;font-weight:800;color:#078197;text-align:right;line-height:1.1}
    .cr-percentile-scale{display:grid;grid-template-columns:1fr 1fr 1fr;margin:0 58px 4px 106px;font-size:.62rem;font-weight:800;letter-spacing:.02em}
    .cr-percentile-scale span:nth-child(1){color:#3464B8;text-align:left}
    .cr-percentile-scale span:nth-child(2){color:#A4BEC1;text-align:center}
    .cr-percentile-scale span:nth-child(3){color:#D9252E;text-align:right}
    .cr-percentile-rows{flex:1;display:flex;flex-direction:column;justify-content:space-between}
    .cr-percentile-row{display:grid;grid-template-columns:98px minmax(220px,1fr) 56px;gap:9px;align-items:center;min-height:28px}
    .cr-percentile-label{font-size:.72rem;line-height:1.05;text-align:right;color:#444;white-space:normal}
    .cr-percentile-track-wrap{position:relative;height:18px}
    .cr-percentile-track{position:absolute;left:0;right:0;top:7px;height:5px;background:#D8E8E9;border-radius:0}
    .cr-percentile-fill{position:absolute;left:0;top:4px;height:11px;border-radius:0}
    .cr-percentile-average{position:absolute;left:50%;top:3px;width:2px;height:13px;background:rgba(255,255,255,.55)}
    .cr-percentile-badge{position:absolute;top:0;transform:translateX(-50%);min-width:24px;height:18px;border-radius:10px;color:white;font-size:.68rem;font-weight:900;line-height:18px;text-align:center;padding:0 5px}
    .cr-percentile-value{font-size:.68rem;color:#444;text-align:right;white-space:nowrap}
    .cr-percentile-row-empty{opacity:.55}
    .cr-percentile-empty{font-size:.85rem;color:#555}
    .performance-percentile-card{min-height:700px}
    .performance-percentile-card .cr-percentile-rows{justify-content:flex-start;gap:6px}
    .performance-percentile-card .cr-percentile-row{min-height:21px}
    .performance-percentile-card .cr-percentile-scale{margin-bottom:8px}
    .performance-timeseries-panel{min-height:700px}
    .staff-percentile-card{min-height:760px}
    .staff-percentile-card .cr-percentile-rows{justify-content:flex-start;gap:6px}
    .staff-percentile-card .cr-percentile-row{min-height:21px}
    .staff-percentile-card .cr-percentile-scale{margin-bottom:8px}
    .pm-section{
      border-top:1px solid #e3e3e3;
      margin-top:14px;
      padding-top:10px;
    }
    .pm-pitch-details{
      border-top:1px solid #e7e7e7;
      padding:8px 0;
    }
    .pm-pitch-details summary{
      cursor:pointer;
      color:var(--txst-maroon);
      font-weight:800;
      list-style:none;
    }
    .pm-pitch-details summary::-webkit-details-marker{
      display:none;
    }
    .pm-empty{
      color:#777;
      font-style:italic;
      padding:10px 0;
    }
    @media (max-width: 1100px){
      .cr-percentile-row{grid-template-columns:108px minmax(120px,1fr) 40px}
      .cr-percentile-scale{margin-left:116px}
    }

    /* Release Points table: keep metrics tight to title bar */
    #team_release_points_table table.dataTable{
      margin-top:0 !important;
    }

    /* Self-scouting tables (performance-table style without ID coupling) */
    table.self-scout-table.dataTable,
    table.self-scout-table.dataTable th,
    table.self-scout-table.dataTable td{
      border:none !important; box-shadow:none !important;
    }
    table.self-scout-table.dataTable{
      border-collapse:separate !important; border-spacing:0 !important;
    }
    table.self-scout-table.dataTable thead th{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important; font-weight:700;
    }
    table.self-scout-table.dataTable.stripe tbody tr.odd{
      background-color: rgba(0,0,0,0.035) !important;
    }
    table.self-scout-table.dataTable tbody tr.even td{
      background-color: #ffffff !important;
    }
    table.self-scout-table.dataTable tbody td{
      line-height:1.15;
    }

    /* Self-scouting heatmap grid */
    .self-heatmap-grid{
      display:flex;
      flex-wrap:wrap;
      gap:12px;
      align-items:flex-start;
    }
    .self-heatmap-col{
      flex:1 1 220px;
      min-width:220px;
    }
    .self-heatmap-title{
      font-weight:700;
      text-align:center;
      margin-bottom:6px;
    }

    /* Global DT zebra rows (all tables) */
    table.dataTable tbody tr.odd td{
      background-color: rgba(0,0,0,0.035) !important;
    }
    table.dataTable tbody tr.even td{
      background-color: #ffffff !important;
    }
  "))
)

app_title_link <- htmltools::tags$a(
  href = "https://bobcatspitchingreports.com",
  target = "_blank",
  class = "app-title-link",
  "Bobcats Pitching Reports"
)

# -------------------- Conditional-formatting helpers (9 buckets, 60% alpha, abs-% for rate columns) --------------------
parse_num <- function(v) {
  if (is.numeric(v)) v else suppressWarnings(readr::parse_number(as.character(v)))
}

# Hazen percent rank (avoids 0%/100% in tiny samples)
percent_rank_dir <- function(x, higher_is_better = TRUE) {
  v  <- parse_num(x)
  ok <- is.finite(v)
  p  <- rep(NA_real_, length(v))
  n  <- sum(ok)
  if (n >= 1) {
    r <- rank(v[ok], ties.method = "average")
    p[ok] <- (r - 0.5) / n
    if (!higher_is_better) p[ok] <- 1 - p[ok]
    p[ok] <- pmin(pmax(p[ok], 0), 1)
  }
  p
}

bucket_color <- local({
  rgba <- function(hex, alpha) {
    rgb <- grDevices::col2rgb(hex)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  function(p) {
    if (is.na(p)) return(NA_character_)
    p <- pmin(pmax(p, 0), 1)
    severity <- abs((p - 0.5) * 2)
    if (severity < 0.08) return(NA_character_)
    alpha <- 0.16 + 0.72 * severity^0.80
    rgba(if (p >= 0.5) "#E33434" else "#5D7EBC", alpha)
  }
})

# -------------------- Conditional-formatting helpers (GREEN/RED vs D1 avg) --------------------
parse_num <- function(v) {
  if (is.numeric(v)) v else suppressWarnings(readr::parse_number(as.character(v)))
}

# rgba helper (keeps zebra visible under your .cf-cell)
.alpha_rgba <- function(hex, a = 0.65) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
}

CF_GREEN <- .alpha_rgba("#E33434", a = 0.30)
CF_RED   <- .alpha_rgba("#5D7EBC", a = 0.30)

.severity_fill <- function(score, palette = c("coach", "player")) {
  palette <- match.arg(palette)
  if (!is.finite(score)) return(NA_character_)
  score <- pmin(pmax(score, -1), 1)
  severity <- abs(score)
  if (severity < 0.08) return(NA_character_)
  alpha <- 0.16 + 0.72 * severity^0.80
  colors <- if (identical(palette, "player")) c(good = "#2E7D32", bad = "#D62828") else c(good = "#E33434", bad = "#5D7EBC")
  .alpha_rgba(if (score > 0) colors[["good"]] else colors[["bad"]], a = alpha)
}

.severity_text <- function(fill) {
  alpha <- suppressWarnings(as.numeric(sub(".*,(0?\\.[0-9]+)\\)$", "\\1", fill)))
  ifelse(!is.na(fill) & is.finite(alpha) & alpha >= 0.58, "#FFFFFF", "#391315")
}

# ---- D1 averages (PERCENT stats) as FRACTIONS ----
D1_PCT_AVG <- list(
  `K%`          = 0.193,
  `BB%`         = 0.113,
  `BB+HBP%`     = 0.130,
  `Barrel%`     = 0.174,
  `CSW%`        = 0.275,
  `GB%`         = 0.420,
  `Strike%`     = 0.605,
  `FPS%`        = 0.575,
  `1-1 win%`    = 0.620,
  `Pre2k Zone%` = 0.460,
  `Pre2K Zone%` = 0.460,
  `Zone%`       = 0.456,
  `E&A%`        = 0.700,
  `Put Away%`   = 0.190,
  `Chase%`      = 0.210,
  `Whiff%`      = 0.240,
  `IZWhiff%`    = 0.157,
  `IZ Whiff%`   = 0.157
)

# ---- D1 averages by pitch type (fractions) ----
D1_PCT_AVG_BY_PITCH <- list(
  `Whiff%` = c(
    "Fastball"           = 0.18,
    "Sinker"             = 0.14,
    "Slider/Sweeper"     = 0.32,
    "Curveball"          = 0.29,
    "Changeup/Splitter"  = 0.32,
    "Cutter"             = 0.27
  ),
  `Chase%` = c(
    "Fastball"           = 0.18,
    "Sinker"             = 0.20,
    "Slider/Sweeper"     = 0.24,
    "Curveball"          = 0.19,
    "Changeup/Splitter"  = 0.27,
    "Cutter"             = 0.26
  )
)

normalize_pitch_type_for_d1 <- function(pt) {
  if (is.null(pt)) return(NA_character_)
  if (!is.character(pt)) pt <- as.character(pt)
  raw <- tolower(trimws(pt %||% ""))
  if (!nzchar(raw)) return(NA_character_)
  
  # If already a canonical label, keep it
  if (raw %in% tolower(SELF_SCOUT_PITCH_LEVELS)) {
    return(SELF_SCOUT_PITCH_LEVELS[match(raw, tolower(SELF_SCOUT_PITCH_LEVELS))])
  }
  
  # Use existing helper when possible
  ct <- self_scout_pitch_type(raw)
  if (!is.na(ct) && nzchar(ct)) return(ct)
  
  # Fallback: common pitch codes
  if (raw %in% c("ff","fa","4-seam","4 seam","fourseam","four-seam","four seam")) return("Fastball")
  if (raw %in% c("ft","2-seam","2 seam","two-seam","two seam","2seam")) return("Fastball")
  if (raw %in% c("si","sinker","snk")) return("Sinker")
  if (raw %in% c("fc","cut","cutter")) return("Cutter")
  if (raw %in% c("sl","slider")) return("Slider")
  if (raw %in% c("st","sw","sweep","sweeper")) return("Sweeper")
  if (raw %in% c("cu","kc","cb","curve","curveball","curve ball")) return("Curveball")
  if (raw %in% c("ch","chg","change","changeup","change-up")) return("Changeup")
  if (raw %in% c("fs","fo","split","splitter","split-finger","split finger")) return("Splitter")
  
  NA_character_
}

d1_pitch_bucket <- function(pt) {
  p <- normalize_pitch_type_for_d1(pt)
  if (is.na(p) || !nzchar(p)) return(NA_character_)
  
  if (p == "Fastball") return("Fastball")
  if (p == "Sinker") return("Sinker")
  if (p %in% c("Slider","Sweeper")) return("Slider/Sweeper")
  if (p == "Curveball") return("Curveball")
  if (p %in% c("Changeup","Splitter")) return("Changeup/Splitter")
  if (p == "Cutter") return("Cutter")
  
  NA_character_
}

d1_pct_avg_for_metric <- function(metric, pitch_type = NULL) {
  metric <- as.character(metric %||% "")
  if (metric %in% c("Whiff%","Chase%")) {
    bucket <- d1_pitch_bucket(pitch_type)
    ref_map <- D1_PCT_AVG_BY_PITCH[[metric]]
    if (is.null(ref_map) || !length(ref_map)) return(D1_PCT_AVG[[metric]])
    if (is.na(bucket) || !nzchar(bucket) || !(bucket %in% names(ref_map))) {
      return(D1_PCT_AVG[[metric]])
    }
    ref <- ref_map[[bucket]]
    if (is.finite(ref)) return(ref)
  }
  D1_PCT_AVG[[metric]]
}

# ---- non-% rules (absolute thresholds) ----
# NOTE: follow exactly what you wrote (pitcher-context: lower wOBA/FIP is "better")
ABS_RULES <- list(
  `BAA`     = list(green_max = 0.220, red_min = 0.300),
  `wOBA`    = list(green_max = 0.264, red_min = 0.464),
  `wOBAcon` = list(green_max = 0.295, red_min = 0.495),
  `FIP`     = list(green_max = 4.08,  red_min = 6.08),
  `WHIP`    = list(green_max = 1.20,  red_min = 1.60),
  `K/9`     = list(green_min = 10.0,  red_max = 6.5),
  `BB/9`    = list(green_max = 3.0,   red_min = 5.0),
  `H/9`     = list(green_max = 7.0,   red_min = 10.0),
  `pRV`     = list(green_max = 0.00,  red_min = 5.00),
  # Pitching context: lower is better (green below avg, red above) with +/- .100 band
  `SLG`     = list(green_max = 0.441 - 0.100, red_min = 0.441 + 0.100),
  `OPS`     = list(green_max = 0.825 - 0.100, red_min = 0.825 + 0.100),
  
  # Movement/Metrics-only
  `Extension` = list(green_min = 6.81, red_max = 4.81),
  `Rel Ht`    = list(no_min = 5.20, no_max = 5.99),  # outside this range => GREEN
  `RelHt`     = list(no_min = 5.20, no_max = 5.99),
  `VAA`       = list(green_min = -4.41, red_max = -6.41)
)

# Convert a parsed numeric into:
# - percent FRACTION if it looks like % points (e.g., "19.3" means 19.3%)
# - otherwise keep as-is
.as_fraction_if_percentish <- function(v) {
  if (!is.finite(v)) return(NA_real_)
  # If value is > 1.5 we assume it’s percent-points like 19.3 (not 0.193)
  if (v > 1.5) return(v / 100)
  v
}

# 5 percentage-point band around D1 avg (AAR-style)
.fill_pct_vs_d1 <- function(value, avg_frac, band_pp = 5, palette = "coach") {
  v <- .as_fraction_if_percentish(parse_num(value))
  if (!is.finite(v) || !is.finite(avg_frac) || avg_frac <= 0) return(NA_character_)
  v_pp <- v * 100
  avg_pp <- avg_frac * 100
  .severity_fill((v_pp - avg_pp) / band_pp, palette)
}

# Lower-is-better variant
.fill_pct_vs_d1_lower <- function(value, avg_frac, band_pp = 5, palette = "coach") {
  v <- .as_fraction_if_percentish(parse_num(value))
  if (!is.finite(v) || !is.finite(avg_frac) || avg_frac <= 0) return(NA_character_)
  v_pp <- v * 100
  avg_pp <- avg_frac * 100
  .severity_fill((avg_pp - v_pp) / band_pp, palette)
}
# Absolute rules for non-% columns
.fill_abs_rule <- function(value, rule, palette = "coach") {
  v <- parse_num(value)
  if (!is.finite(v)) return(NA_character_)
  
  # Special: Rel Ht => within [no_min, no_max] = no color; outside = GREEN
  if (!is.null(rule$no_min) && !is.null(rule$no_max)) {
    if (v >= rule$no_min && v <= rule$no_max) return(NA_character_)
    span <- max((rule$no_max - rule$no_min) / 2, .Machine$double.eps)
    distance <- if (v < rule$no_min) rule$no_min - v else v - rule$no_max
    return(.severity_fill(distance / span, palette))
  }

  if (!is.null(rule$green_min) && !is.null(rule$red_max)) {
    midpoint <- (rule$green_min + rule$red_max) / 2
    span <- max(abs(rule$green_min - rule$red_max) / 2, .Machine$double.eps)
    return(.severity_fill((v - midpoint) / span, palette))
  }
  if (!is.null(rule$green_max) && !is.null(rule$red_min)) {
    midpoint <- (rule$green_max + rule$red_min) / 2
    span <- max(abs(rule$red_min - rule$green_max) / 2, .Machine$double.eps)
    return(.severity_fill((midpoint - v) / span, palette))
  }
  
  NA_character_
}

# RPM rule depends on PitchType row
.fill_rpm_by_pitch <- function(value, pitch_type_chr, palette = "coach") {
  v <- parse_num(value)
  if (!is.finite(v)) return(NA_character_)
  pt <- tolower(trimws(as.character(pitch_type_chr %||% "")))
  
  fb_sink <- pt %in% tolower(c("fastball","four-seam","four seam","two-seam","two seam","sinker"))
  sl_cb_sw_ct <- pt %in% tolower(c("slider","curveball","curve ball","sweeper","cutter"))
  
  if (fb_sink) {
    return(.severity_fill((v - mean(c(2386, 1986))) / ((2386 - 1986) / 2), palette))
  }
  if (sl_cb_sw_ct) {
    return(.severity_fill((v - mean(c(2558, 2158))) / ((2558 - 2158) / 2), palette))
  }
  NA_character_
}

shade_columns_txst <- function(out_df,
                               cols = NULL,
                               cols_to_color = NULL,
                               lower_better = character(0),
                               percent_cols = character(0),
                               palette = c("coach", "player"),
                               ...) {
  palette <- match.arg(palette)
  # --- compatibility shim (supports both cols= and cols_to_color=) ---
  if (is.null(cols_to_color)) cols_to_color <- cols
  if (is.null(cols_to_color)) cols_to_color <- character(0)
  
  # keep everything constrained to columns that actually exist
  cols_to_color <- intersect(cols_to_color, names(out_df))
  percent_cols  <- intersect(percent_cols,  names(out_df))
  lower_better  <- intersect(lower_better,  names(out_df))
  
  # in case the existing body references `cols` instead of `cols_to_color`
  cols <- cols_to_color
  
  # --- DEBUG: prove whether we are hanging here ---
 
  
  # --- SAFETY: if there are no columns to shade, do nothing ---
  if (!length(cols_to_color)) return(out_df)
  
  out <- out_df
  if (is.null(out) || !nrow(out)) return(out)
  
  # detect pitch type column for RPM logic
  pt_col <- intersect(names(out), c("PitchType","Pitch Type","Pitch"))[1]
  if (is.na(pt_col) || !nzchar(pt_col)) pt_col <- NA_character_
  
  for (nm in names(out)) {
    # don’t shade label columns
    if (nm %in% c("PitchType","Pitch Type","Pitch","Hand","BatterSide","Split","Metric")) next
    
    # decide rule type
    is_pct_rule <- nm %in% names(D1_PCT_AVG)
    abs_rule    <- ABS_RULES[[nm]]
    
    # rpm-ish column?
    is_rpm_col <- grepl("(?i)spin|rpm", nm)
    
    txt <- as.character(out[[nm]])
    
    fills <- rep(NA_character_, length(txt))
    
    if (is_pct_rule) {
      if (!is.na(pt_col) && nm %in% c("Whiff%","Chase%")) {
        pitch_vals <- out[[pt_col]]
        avg_vec <- vapply(pitch_vals, d1_pct_avg_for_metric, metric = nm, FUN.VALUE = numeric(1))
        if (nm %in% lower_better) {
          fills <- mapply(.fill_pct_vs_d1_lower, txt, avg_vec, MoreArgs = list(palette = palette), SIMPLIFY = TRUE, USE.NAMES = FALSE)
        } else {
          fills <- mapply(.fill_pct_vs_d1, txt, avg_vec, MoreArgs = list(palette = palette), SIMPLIFY = TRUE, USE.NAMES = FALSE)
        }
      } else {
        avg <- D1_PCT_AVG[[nm]]
        if (nm %in% lower_better) {
          fills <- vapply(txt, .fill_pct_vs_d1_lower, avg_frac = avg, palette = palette, FUN.VALUE = character(1))
        } else {
          fills <- vapply(txt, .fill_pct_vs_d1, avg_frac = avg, palette = palette, FUN.VALUE = character(1))
        }
      }
    } else if (!is.null(abs_rule)) {
      fills <- vapply(txt, .fill_abs_rule, rule = abs_rule, palette = palette, FUN.VALUE = character(1))
    } else if (is_rpm_col && !is.na(pt_col)) {
      pitch_vals <- out[[pt_col]]
      fills <- mapply(.fill_rpm_by_pitch, txt, pitch_vals, MoreArgs = list(palette = palette), SIMPLIFY = TRUE, USE.NAMES = FALSE)
    } else {
      next
    }
    
    # numeric value for proper sorting (DataTables reads data-order)
    order_val <- suppressWarnings(readr::parse_number(as.character(txt)))
    order_attr <- ifelse(is.finite(order_val), as.character(order_val), "")
    
    text_colors <- .severity_text(fills)
    out[[nm]] <- ifelse(
      is.na(fills),
      sprintf("<span class='cf-cell' data-order='%s'>%s</span>", order_attr, txt),
      sprintf("<span class='cf-cell' data-order='%s' style='background-color:%s;color:%s;font-weight:700'>%s</span>", order_attr, fills, text_colors, txt)
    )
  }
  
  out
}


# BASE supplies a scoped theme/head when this file is hosted as a workspace.
# The standalone app retains its original title, theme, and page structure.
if (isTRUE(get0("BASE_PITCHING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
  txst_theme <- get0(
    "BASE_PITCHING_THEME",
    inherits = FALSE,
    ifnotfound = txst_theme
  )
  head_css <- get0(
    "BASE_PITCHING_HEAD",
    inherits = FALSE,
    ifnotfound = head_css
  )
  app_title_link <- NULL
  base_pitching_page <- function(..., title = NULL, sidebar = NULL, theme = NULL) {
    dots <- list(...)
    is_head_item <- vapply(dots, function(item) {
      inherits(item, "shiny.tag") && item$name %in% c("head", "script")
    }, logical(1))
    sidebar_children <- if (inherits(sidebar, "bslib_sidebar")) {
      c(list(sidebar$title), sidebar$children)
    } else {
      list(sidebar)
    }
    htmltools::tagList(
      dots[is_head_item],
      htmltools::tags$div(
        class = "base-pitching-embedded-layout",
        htmltools::tags$aside(
          class = "base-pitching-sidebar",
          sidebar_children
        ),
        htmltools::tags$main(
          class = "base-pitching-main",
          dots[!is_head_item]
        )
      )
    )
  }
} else {
  base_pitching_page <- page_sidebar
}

base_pitching_postgame_ui <- tagList(
  navset_tab(
    id = "aar_tabs",
    nav_panel(
      "AAR Builder",
      div(
        class = "base-report-builder",
        div(
          class = "base-report-controls",
          selectInput("aar_pitcher", "Pitcher:",
                      choices = as_pitcher_choices(setdiff(txst_pitchers, EXCLUDE_PLAYERS)),
                      selected = (setdiff(txst_pitchers, EXCLUDE_PLAYERS))[1]),
          selectInput("aar_game", "Game:", choices = NULL, multiple = FALSE, selectize = FALSE),
          textInput("aar_opp", "Opponent label (optional):", placeholder = "vs UTA"),
          downloadButton("aar_dl", "Download AAR PDF")
        ),
        if (isTRUE(get0("BASE_PITCHING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
          div(
            class = "base-report-controls base-report-controls-team",
            checkboxGroupInput(
              "pitch_aar_season_groups", "Quick-select seasons",
              choices = SEASON_CHOICES, selected = "S26", inline = TRUE
            )
          )
        },
        div(
          class = "base-report-preview base-report-preview-portrait",
          div(class = "table-title mb-1", "AAR Page 1"),
          imageOutput("aar_preview", height = "2400px"),
          div(class = "table-title mt-3 mb-1", "AAR Page 2"),
          imageOutput("aar_preview_page2", height = "2400px")
        )
      )
    ),
    nav_panel(
      "Recent AARs",
      div(class = "table-title mb-1", "Most Recent AARs"),
      uiOutput("aar_recent_list_ui")
    )
  )
)

# -------------------- UI --------------------
ui <- base_pitching_page(
  theme = txst_theme,
  title = app_title_link, 
  head_css,
  tags$head(tags$style(HTML("
    table.dataTable tbody tr.base-total-row td,
    table.dataTable tbody tr.base-total-row td * {
      font-weight: 800 !important;
    }
    .base-report-builder {
      display: grid;
      gap: 18px;
      width: 100%;
      min-width: 0;
    }
    .base-report-controls {
      display: grid;
      grid-template-columns: repeat(3, minmax(180px, 1fr)) auto;
      gap: 12px;
      align-items: end;
      padding: 14px;
      border: 1px solid rgba(80,18,20,.14);
      border-radius: 10px;
      background: rgba(255,255,255,.92);
    }
    .base-report-controls-team {
      grid-template-columns: minmax(260px, 1fr) auto;
    }
    .base-report-controls .form-group { margin-bottom: 0; }
    .base-report-preview {
      width: 100%;
      min-width: 0;
      overflow-x: auto;
      padding: 12px;
      border: 1px solid rgba(80,18,20,.14);
      border-radius: 10px;
      background: #e9e6e1;
    }
    .base-report-preview .shiny-image-output {
      height: auto !important;
      margin: 0 auto 18px;
      background: #fff;
      box-shadow: 0 6px 18px rgba(30,22,18,.16);
    }
    .base-report-preview-portrait .shiny-image-output { width: 1050px !important; }
    .base-report-preview-landscape .shiny-image-output { width: 1180px !important; }
    .base-report-preview .shiny-image-output img {
      display: block;
      width: 100% !important;
      height: auto !important;
    }
    @media (max-width: 900px) {
      .base-report-controls,
      .base-report-controls-team { grid-template-columns: 1fr; }
    }
  "))),
  tags$script(HTML("
    function baseMarkPitchingTotalRows(scope){
      $(scope || document).find('table.dataTable').addBack('table.dataTable').find('tbody tr').each(function(){
        var isTotal=$(this).find('td').toArray().some(function(cell){
          return /^(GRAND\\s+)?TOTALS?$/.test($(cell).text().trim().toUpperCase());
        });
        $(this).toggleClass('base-total-row', isTotal);
      });
    }
    $(document).on('draw.dt', function(e){ baseMarkPitchingTotalRows(e.target); });
    function baseKeepPitchingTabVisible(link){
      var $link=$(link), nav=$link.closest('.nav-tabs')[0];
      if(!nav) return;
      var left=$link.position().left+nav.scrollLeft;
      var target=Math.max(0,left-(nav.clientWidth-$link.outerWidth())/2);
      nav.scrollTo({left:target,behavior:'smooth'});
    }
    $(document).on('shown.bs.tab','a[data-bs-toggle=\"tab\"],a[data-toggle=\"tab\"]',function(e){
      var txt=$(e.target).text().trim();
      document.body.classList.toggle('aar-active', txt==='AAR');
      baseKeepPitchingTabVisible(e.target);
    });
    $(function(){ window.setTimeout(function(){
      var active=$('.nav-tabs .nav-link.active').text().trim();
      document.body.classList.toggle('aar-active', active==='AAR');
      $('.base-pitching-main .nav-tabs .nav-link.active').each(function(){baseKeepPitchingTabVisible(this);});
      baseMarkPitchingTotalRows(document);
    },150); });
  ")),
  sidebar = sidebar(
    title = "Select Pitcher/Game",
    selectInput("PitcherInput", "Select Pitcher",
                choices  = as_pitcher_choices(setdiff(txst_pitchers, EXCLUDE_PLAYERS)),
                selected = (setdiff(txst_pitchers, EXCLUDE_PLAYERS))[1],
                selectize = TRUE
    )
    ,
    checkboxGroupInput(
      "season_groups", "Quick-select seasons",
      choices  = SEASON_CHOICES,
      selected = "S26",
      inline   = TRUE
    ),
    checkboxInput("Bullpens", "Bullpens", value = FALSE),
    pickerInput("GameInput", HTML("Select Game<br>(Selects all by default)"),
                choices = games_txst, selected = games_txst,
                options = list(`actions-box` = TRUE), multiple = TRUE),
    pickerInput("BatterHand", HTML("Select Batter Hand<br>(Selects both by default)"),
                choices = bh_choices, selected = bh_choices,
                options = list(`actions-box` = TRUE), multiple = TRUE)
  ),
  
  navset_tab(
    nav_panel(
      id = "main_tabs",
      title = "Performance",
      div(
        class = "mb-2",
        shinyWidgets::radioGroupButtons(
          inputId  = "perf_split",
          label    = "Split performance by",
          choices  = c("Batter Handedness" = "hand", "Pitch Type" = "ptype"),
          selected = "hand",
          justified = TRUE,
          size      = "sm"
        )
      ),
      div(
        class = "base-pitching-performance-tables",
        column(
          6,
          div(class = "table-title mb-1", "Traditional Stats"),
          withSpinner(DTOutput("performance_traditional_table"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Process Stats"),
          withSpinner(DTOutput("performance_process_table"), type = 4, color = "#501214")
        ),
        column(
          6,
          div(class = "table-title mb-1", "Modern Stats"),
          withSpinner(DTOutput("performance_modern_table"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Performance"),
          withSpinner(DTOutput("performance_results_table"), type = 4, color = "#501214")
        )
      ),
      div(
        class = "base-pitching-performance-details",
        column(
          6,
          div(class = "cr-percentile-column mt-3", withSpinner(uiOutput("performance_percentiles"), type = 4, color = "#501214"))
        ),
        column(
          6,
          div(class = "performance-timeseries-panel mt-3",
              div(class = "table-title mb-1", "Time Series"),
              div(class = "mb-2",
                  strong("Performance Stats"),
                  selectizeInput(
                    "perf_ts_stats",
                    NULL,
                    choices  = PERF_TS_MAIN_CHOICES,
                    selected = "wOBA",
                    multiple = TRUE,
                    options = list(
                      plugins = list("remove_button"),
                      placeholder = "Choose performance metrics"
                    ),
                    width = "100%"
                  )
              ),
              div(class = "mb-2",
                  strong("Performance Stats by Pitch Type"),
                  uiOutput("perf_ts_ptype_perf_boxes")
              ),
              div(class = "mb-2",
                  strong("View"),
                  shinyWidgets::radioGroupButtons(
                    inputId  = "perf_ts_mode",
                    label    = NULL,
                    choices  = c("Rolling" = "roll", "Game by Game" = "game"),
                    selected = "roll",
                    justified = TRUE,
                    size      = "sm"
                  )
              ),
              withSpinner(plotOutput("perf_ts_plot", height = "560px", width = "100%"), type = 4, color = "#501214")
          )
        )
      )
    ),
    nav_panel(
      title = "Pitch Metrics",
      fluidRow(
        column(6, div(class = "cr-percentile-column", withSpinner(uiOutput("pitch_metrics_percentiles"), type = 4, color = "#501214"))),
        column(6, withSpinner(plotlyOutput("pitch_metrics_plot", height = "650px", width = "100%"), type = 4, color = "#501214"))
      ),
      div(style = "font-size: 11px; color: #666; margin-top: 4px;",
          textOutput("pitch_metrics_debug")
      ),
      fluidRow(
        column(6, withSpinner(plotlyOutput("extension_plot", height = "450px", width = "100%"), type = 4, color = "#501214")),
        column(6, withSpinner(plotlyOutput("release_plot",   height = "450px", width = "100%"), type = 4, color = "#501214"))
      ),
      br(),
      div(class = "table-title mb-1", "Metrics"),
      withSpinner(DTOutput("metrics"), type = 4, color = "#501214"),
      div(class = "table-title mt-3 mb-1", "Performance"),
      withSpinner(DTOutput("metrics_perf"), type = 4, color = "#501214")
    ),
    nav_panel(
      title = "Season Summary",
      div(
        class = "base-season-summary-page",
        div(
          class = "base-season-summary-header",
          uiOutput("season_summary_header_ui"),
          downloadButton("season_summary_pdf", "Download Season Summary PDF")
        ),
        div(
          class = "base-season-summary-section",
          div(class = "table-title mb-1", "Split Summary"),
          withSpinner(DTOutput("season_summary_split_table"), type = 4, color = "#501214")
        ),
        div(
          class = "base-season-summary-hands",
          div(
            class = "base-season-summary-hand-panel",
            div(class = "table-title mb-1", "vs. Left-Handed Hitters"),
            withSpinner(plotOutput("season_summary_usage_lhh", height = "230px", width = "100%"), type = 4, color = "#501214"),
            div(class = "base-season-summary-table-label", "Pitch Type Performance"),
            withSpinner(DTOutput("season_summary_hand_perf_lhh"), type = 4, color = "#501214")
          ),
          div(
            class = "base-season-summary-hand-panel",
            div(class = "table-title mb-1", "vs. Right-Handed Hitters"),
            withSpinner(plotOutput("season_summary_usage_rhh", height = "230px", width = "100%"), type = 4, color = "#501214"),
            div(class = "base-season-summary-table-label", "Pitch Type Performance"),
            withSpinner(DTOutput("season_summary_hand_perf_rhh"), type = 4, color = "#501214")
          )
        ),
        div(
          class = "base-season-summary-section",
          div(class = "table-title mb-1", "Pitch Type Summary"),
          withSpinner(DTOutput("season_summary_pitch_table"), type = 4, color = "#501214")
        )
      )
    ),
    nav_panel(
      title = "Pitch Decay",
      navset_tab(
        nav_panel(
          title = "Season",
          div(class = "table-title mb-1", "Heater Velocity"),
          withSpinner(plotlyOutput("pitch_decay_velocity", height = "360px", width = "100%"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Times Through the Order"),
          withSpinner(DTOutput("pitch_decay_table"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Single Game",
          uiOutput("pitch_decay_game_picker"),
          div(class = "table-title mb-1", "Heater Velocity"),
          withSpinner(plotlyOutput("pitch_decay_single_velocity", height = "360px", width = "100%"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Times Through the Order"),
          withSpinner(DTOutput("pitch_decay_single_table"), type = 4, color = "#501214")
        )
      )
    ),
    nav_panel(
      title = "Locations",
      # height is now dynamic from the server (see output$locations height fn)
      withSpinner(plotOutput("locations", width = "100%"), type = 4, color = "#501214")
    ),
    nav_panel(
      title = "Whiffs / Chases / Called Strikes / Barrels",
      uiOutput("outcome_location_tabs")
    ),
    nav_panel(
      title = "Stuff+",
      navset_tab(
        nav_panel(
          title = "Season Summary",
          uiOutput("xrv_season_header"),
          div(class = "table-title mb-1", "Stats"),
          withSpinner(DTOutput("xrv_season_stats"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Movement & Usage"),
          fluidRow(
            column(6, withSpinner(plotlyOutput("xrv_season_movement", height = "420px", width = "100%"), type = 4, color = "#501214")),
            column(
              6,
              div(class = "table-title mb-1", "Pitch Usage vLHH"),
              withSpinner(plotlyOutput("xrv_season_usage_lhh", height = "200px", width = "100%"), type = 4, color = "#501214"),
              div(class = "table-title mt-2 mb-1", "Pitch Usage vRHH"),
              withSpinner(plotlyOutput("xrv_season_usage_rhh", height = "200px", width = "100%"), type = 4, color = "#501214")
            )
          ),
          div(class = "table-title mt-3 mb-1", "Pitch Summary"),
          withSpinner(DTOutput("xrv_season_pitch_table"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Stuff+ Rolling Average"),
          withSpinner(plotlyOutput("xrv_season_ts", height = "330px", width = "100%"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Game Summary",
          fluidRow(
            column(
              6,
              selectInput("xrv_game_pitcher", "Pitcher:", choices = character(0), selected = NULL)
            ),
            column(
              6,
              selectInput("xrv_game_game", "Game:", choices = character(0), selected = NULL)
            )
          ),
          uiOutput("xrv_game_header"),
          div(class = "table-title mb-1", "Stats"),
          withSpinner(DTOutput("xrv_game_stats"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Movement & Usage"),
          fluidRow(
            column(6, withSpinner(plotlyOutput("xrv_game_movement", height = "420px", width = "100%"), type = 4, color = "#501214")),
            column(
              6,
              div(class = "table-title mb-1", "Pitch Usage vLHH"),
              withSpinner(plotlyOutput("xrv_game_usage_lhh", height = "200px", width = "100%"), type = 4, color = "#501214"),
              div(class = "table-title mt-2 mb-1", "Pitch Usage vRHH"),
              withSpinner(plotlyOutput("xrv_game_usage_rhh", height = "200px", width = "100%"), type = 4, color = "#501214")
            )
          ),
          div(class = "table-title mt-3 mb-1", "Pitch Summary"),
          withSpinner(DTOutput("xrv_game_pitch_table"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Stuff+ Rolling Average"),
          withSpinner(plotlyOutput("xrv_game_ts", height = "330px", width = "100%"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Staff Leaderboard",
          sliderInput(
            "xrv_leader_min_pitches",
            "Minimum pitches to qualify",
            min = 0, max = 200, value = 0, step = 25
          ),
          div(class = "table-title mb-1", "Pitch Type Filter"),
          uiOutput("xrv_leader_pitch_filter"),
          div(class = "table-title mt-2 mb-1", "Totals (All Pitches)"),
          withSpinner(DTOutput("xrv_leader_totals"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Pitch Leaderboard"),
          withSpinner(DTOutput("xrv_leader_pitch_table"), type = 4, color = "#501214")
        )
      )
    ),
    if (!isTRUE(get0("BASE_PITCHING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
      nav_panel("AAR", base_pitching_postgame_ui)
    },
    nav_panel(
      title = "Bullpens",
      value = "bullpens",
      navset_tab(
        id = "bullpen_tabs",
        nav_panel(
          title = "Accumulating",
          div(
            class = "mb-3",
            pickerInput(
              "bullpen_accum_games",
              "Bullpen Sessions",
              choices = character(0),
              selected = character(0),
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          withSpinner(plotlyOutput("bullpen_accum_movement", height = "650px", width = "100%"), type = 4, color = "#501214"),
          div(class = "table-title mt-3 mb-1", "Pitch Metrics"),
          withSpinner(DTOutput("bullpen_accum_metrics"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Reports",
          sidebarLayout(
            sidebarPanel(
              selectInput("bullpen_report_game", "Bullpen Session:", choices = character(0), selectize = FALSE),
              downloadButton("bullpen_report_pdf", "Download Bullpen PDF")
            ),
            mainPanel(
              div(class = "table-title mb-1", "Bullpen Report"),
              uiOutput("bullpen_report_preview_ui")
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Leaderboard",
      div(
        class = "mb-2",
        fluidRow(
          column(
            6,
            checkboxGroupInput(
              "leader_seasons", "Seasons",
              choices  = SEASON_CHOICES,
              selected = "S26",
              inline   = TRUE
            )
          ),
          column(
            3,
            checkboxGroupInput(
              "leader_hand", "Batter Hand",
              choices  = c("v LHH" = "L", "v RHH" = "R"),
              selected = c("L","R"),
              inline   = TRUE
            )
          ),
          column(
            12,
            selectizeInput(
              "leader_exclude", "Exclude Pitchers",
              choices = character(0),
              selected = character(0),
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Type a pitcher name to exclude"
              ),
              width = "100%"
            )
          ),
          column(
            3,
            dateRangeInput(
              "leader_dates", "Date range",
              start = leader_date_min,
              end   = leader_date_max,
              min   = leader_date_min,
              max   = leader_date_max
            )
          ),
          column(
            3,
            sliderInput(
              "leader_min_pa", "Minimum PA",
              min = 0, max = 50, value = 0, step = 5
            )
          )
        )
      ),
      div(
        class = "mb-2 d-flex gap-2",
        downloadButton("leader_results_pdf", "Download results pdf"),
        downloadButton("leader_process_pdf", "Download Process pdf")
      ),
      withSpinner(DTOutput("leaderboard_table"), type = 4, color = "#501214"),
      div(
        class = "mt-2",
        withSpinner(DTOutput("leaderboard_totals_table"), type = 4, color = "#501214")
      )
    ),
    nav_panel(
      title = "Team Report",
      div(
        class = "base-report-builder",
        div(
          class = "base-report-controls base-report-controls-team",
          selectInput(
            "team_game",
            "Game:",
            choices = games_txst,
            selected = if (length(games_txst)) games_txst[[1]] else NULL,
            multiple = FALSE,
            selectize = FALSE
          ),
          downloadButton("team_report_dl", "Download Team Report PDF")
        ),
        div(
          class = "base-report-preview base-report-preview-landscape",
          imageOutput("team_report_preview", height = "1000px")
        )
      )
    ),
    nav_panel(
      title = "Team Trends",
      div(
        class = "mb-2",
        fluidRow(
          column(
            6,
            checkboxGroupInput(
              "team_trends_seasons", "Seasons",
              choices  = SEASON_CHOICES,
              selected = "S26",
              inline   = TRUE
            )
          ),
          column(
            3,
            dateRangeInput(
              "team_trends_dates", "Date range",
              start = leader_date_min,
              end   = leader_date_max,
              min   = leader_date_min,
              max   = leader_date_max
            )
          )
        )
      ),
      navset_tab(
        nav_panel(
          title = "Performance",
          div(class = "table-title mb-1", "Team Trends"),
          div(class = "mb-2",
              strong("Performance Stats"),
              selectizeInput(
                "team_ts_stats",
                NULL,
                choices  = PERF_TS_MAIN_CHOICES,
                selected = "wOBA",
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  placeholder = "Choose staff metrics"
                ),
                width = "100%"
              )
          ),
          div(class = "mb-2",
              strong("Performance Stats by Pitch Type"),
              uiOutput("team_ts_ptype_perf_boxes")
          ),
          div(
            class = "base-team-trends-dashboard",
            div(class = "cr-percentile-column", withSpinner(uiOutput("team_staff_percentiles"), type = 4, color = "#501214")),
            div(
              class = "base-team-trends-charts",
              withSpinner(plotlyOutput("team_ts_roll", height = "330px", width = "100%"), type = 4, color = "#501214"),
              withSpinner(plotlyOutput("team_ts_game", height = "330px", width = "100%"), type = 4, color = "#501214")
            )
          )
        ),
        nav_panel(
          title = "Release Points",
          div(class = "table-title release-title mb-0", "Release Points"),
          withSpinner(plotlyOutput("team_release_points_plot", height = "520px", width = "100%"), type = 4, color = "#501214"),
          withSpinner(DTOutput("team_release_points_table"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Game Report",
          div(class = "mb-2", uiOutput("team_release_game_date_ui")),
          div(class = "table-title mb-1", "Game Report"),
          withSpinner(DTOutput("team_release_game_table"), type = 4, color = "#501214")
        ),
        nav_panel(
          title = "Player Report",
          div(class = "mb-2", uiOutput("team_release_player_ui")),
          div(class = "table-title mb-1", "Player Report"),
          withSpinner(DTOutput("team_release_player_table"), type = 4, color = "#501214")
        )
      )
    )
  )
)

# BARREL UTILITIES (GLOBAL)
# ---- thresholds
BARREL_EV_MIN <- 95
BARREL_LA_MIN <- 5
BARREL_LA_MAX <- 40

# ---- BIP (for rates/denominators): include all in-play variants
is_bip_for_barrel <- function(pc) {
  x <- trimws(as.character(pc))
  !is.na(x) & x %in% c("InPlay","InPlayNoOut","InPlayOut")
}

# ---- robust numeric parse
.to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))

# ---- choose EV/LA columns robustly, clamp to sane ranges
resolve_ev_la_strict <- function(d) {
  ev_candidates <- c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
  la_candidates <- c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
  ev_col <- ev_candidates[ev_candidates %in% names(d)][1]
  ev <- if (!is.na(ev_col)) .to_num(d[[ev_col]]) else rep(NA_real_, nrow(d))
  
  la_cols <- la_candidates[la_candidates %in% names(d)]
  if (!length(la_cols)) return(list(ev = ev, la = rep(NA_real_, nrow(d))))
  la_list <- lapply(la_cols, function(nm) .to_num(d[[nm]]))
  # pick the column that “looks like LA” (finite & within [-10,60])
  score <- function(v) mean(is.finite(v) & v >= -10 & v <= 60, na.rm = TRUE)
  idx <- if ("Angle" %in% la_cols && score(la_list[[which(la_cols=="Angle")]]) >= 0.25) {
    which(la_cols=="Angle")
  } else {
    which.max(vapply(la_list, score, numeric(1)))
  }
  la <- la_list[[idx]]
  list(ev = ev, la = la)
}

# ---- strict barrel gate (now counts any in-play PitchCall)
is_barrel_strict <- function(pc, ev, la) {
  evn <- .to_num(ev); lan <- .to_num(la)
  bip <- is_bip_for_barrel(pc)
  out <- bip & is.finite(evn) & is.finite(lan) &
    evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
  out[is.na(out)] <- FALSE
  out
}

# ---- Calibrate within Fall '25 selections to your exact season-level rates.
# Outside Fall '25 we use strict barrels (so 2025/2026 seasons won’t be stuck at 0%).
.calibrate_fall25 <- function(d, base_barrel, ev, la) {
  n <- nrow(d)
  if (!n) return(base_barrel)
  
  out <- as.logical(base_barrel)
  out[is.na(out)] <- FALSE
  
  # Only calibrate if a mapping exists
  if (!exists("FALL_2025_BARREL_PCT", inherits = TRUE)) return(out)
  
  if (!all(c("Pitcher","GameDate","PitchCall") %in% names(d))) return(out)
  
  gd <- parse_date_any(d$GameDate)
  in_fall <- !is.na(gd) & gd >= as.Date("2025-09-01") & gd <= as.Date("2025-12-31")
  if (!any(in_fall)) return(out)
  
  pitchers <- intersect(names(FALL_2025_BARREL_PCT), unique(as.character(d$Pitcher[in_fall])))
  if (!length(pitchers)) return(out)
  
  to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
  evn <- to_num(ev)
  lan <- to_num(la)
  
  for (p in pitchers) {
    target <- FALL_2025_BARREL_PCT[[p]]
    if (!is.finite(target)) next
    
    idx_bip <- which(in_fall & as.character(d$Pitcher) == p & is_bip_for_barrel(d$PitchCall))
    if (!length(idx_bip)) next
    
    tgt_n <- round(target * length(idx_bip))
    
    # reset this pitcher's fall BIP rows, then re-pick
    out[idx_bip] <- FALSE
    if (tgt_n <= 0) next
    
    score <- evn + 10 * pmax(0, 1 - pmin(abs(lan - 27) / 20, 1))
    ord <- idx_bip[order(score[idx_bip], decreasing = TRUE, na.last = NA)]
    pick <- head(ord, tgt_n)
    
    # fallback if score is unusable
    if (length(pick) < tgt_n) {
      pick <- c(pick, head(setdiff(idx_bip, pick), tgt_n - length(pick)))
    }
    
    if (length(pick)) out[pick] <- TRUE
  }
  
  out
}

# ===== COMPAT SHIM: calibrate_fall25_barrels (accepts both old & new signatures) =====
# Works with either:
#   calibrate_fall25_barrels(d)
#   calibrate_fall25_barrels(d, d$EV, d$LA, d$BIP_exact, d$Barrel_strict)
.cal_fall25_impl_prior <- if (exists("calibrate_fall25_barrels", mode = "function")) calibrate_fall25_barrels else NULL

calibrate_fall25_barrels <- function(d, EV = NULL, LA = NULL, BIP_exact = NULL, Barrel_strict = NULL, ...) {
  d <- tibble::as_tibble(d)
  n <- nrow(d)
  if (!n) return(logical(0))
  
  # If no legacy extras were provided AND we have a prior one-arg implementation, delegate.
  if (missing(EV) && missing(LA) && missing(BIP_exact) && missing(Barrel_strict) &&
      !is.null(.cal_fall25_impl_prior)) {
    return(.cal_fall25_impl_prior(d))
  }
  
  # --- Build EV/LA (use provided vectors when present; else resolve from data) ---
  to_num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
  if (is.null(EV) || length(EV) != n || is.null(LA) || length(LA) != n) {
    if (exists("resolve_ev_la_strict", mode = "function")) {
      evla <- resolve_ev_la_strict(d); EV <- evla$ev; LA <- evla$la
    } else {
      EV <- rep(NA_real_, n); LA <- rep(NA_real_, n)
    }
  } else {
    EV <- to_num(EV); LA <- to_num(LA)
  }
  
  # --- Build BIP indicator (legacy BIP_exact if supplied; else from PitchCall) ---
  if (is.null(BIP_exact) || length(BIP_exact) != n) {
    pc <- trimws(as.character(d$PitchCall %||% ""))
    BIP_exact <- !is.na(pc) & pc %in% c("InPlay","InPlayNoOut","InPlayOut")
  } else {
    BIP_exact <- as.logical(BIP_exact); BIP_exact[is.na(BIP_exact)] <- FALSE
  }
  
  # --- Base output (optionally start from strict barrels if provided, otherwise FALSE) ---
  out <- rep(FALSE, n)
  if (!is.null(Barrel_strict) && length(Barrel_strict) == n) {
    out <- as.logical(Barrel_strict); out[is.na(out)] <- FALSE
  }
  
  # --- Restrict to Fall '25 window ---
  if (!all(c("GameDate","Pitcher") %in% names(d))) return(out)
  gd <- suppressWarnings(as.Date(d$GameDate))
  in_fall <- !is.na(gd) & gd >= as.Date("2025-09-01") & gd <= as.Date("2025-12-31")
  if (!any(in_fall)) return(out)
  
  
  # --- Calibrate per pitcher to exact FALL_2025_BARREL_PCT of strict BIP ---
  pitchers <- intersect(names(FALL_2025_BARREL_PCT), unique(as.character(d$Pitcher[in_fall])))
  if (!length(pitchers)) return(out)
  
  for (p in pitchers) {
    idx <- which(in_fall & BIP_exact & as.character(d$Pitcher) == p)
    if (!length(idx)) next
    
    target <- FALL_2025_BARREL_PCT[[p]]
    if (!is.finite(target)) next
    
    n_bip <- length(idx)
    n_bar <- round(n_bip * target)
    if (n_bar <= 0) { out[idx] <- FALSE; next }
    
    evn <- to_num(EV[idx])
    lan <- to_num(LA[idx])
    
    # Rank by EV + bonus for proximity to 27° (±20° window)
    la_bonus <- ifelse(is.finite(lan), pmax(0, 1 - pmin(abs(lan - 27) / 20, 1)), 0)
    score <- ifelse(is.finite(evn), evn, -Inf) + 10 * la_bonus
    
    ord <- order(score, decreasing = TRUE, na.last = NA)
    pick <- if (length(ord) >= n_bar) idx[ord[seq_len(n_bar)]] else idx[ord]
    
    # Fallback if EV/LA are all NA/−Inf: use earliest BIP rows to reach the count
    if (length(pick) < n_bar) {
      need <- n_bar - length(pick)
      fallback <- setdiff(idx, pick)
      if (length(fallback)) pick <- c(pick, head(fallback, need))
    }
    
    # Overwrite any prior selections for this pitcher's BIP rows, then mark picks
    out[idx] <- FALSE
    if (length(pick)) out[pick] <- TRUE
  }
  
  out
}


# ---- public helper: compute final Barrel flag everywhere
compute_barrel_flag <- function(d) {
  evla <- resolve_ev_la_strict(d)
  base <- is_barrel_strict(d$PitchCall, evla$ev, evla$la)
  .calibrate_fall25(d, base, evla$ev, evla$la)
}

# -------------------- Server --------------------
server <- function(input, output, session){

  last_pitcher <- reactiveVal(NULL)
  # Prefer TXST-filtered rows, but fall back to non-bullpen data if TXST filter drops a pitcher.
  get_pitcher_rows <- function() {
    req(input$PitcherInput)
    d <- txst_df %>% dplyr::filter(.data$Pitcher == input$PitcherInput)
    if (!nrow(d)) {
      d <- df_nonbp %>% dplyr::filter(.data$Pitcher == input$PitcherInput)
    }
    d
  }
  
  # Non-bullpen game IDs for the selected pitcher (all seasons)
  game_ids_for_pitcher <- reactive({
    req(input$PitcherInput)
    
    d <- get_pitcher_rows()
    
    # If your txst_df contains bullpen rows, exclude them safely
    if ("is_bullpen" %in% names(d)) {
      d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))
    }
    
    ids <- d %>% dplyr::pull(.data$CustomGameID)
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    ids <- sort(unique(ids))

    # Fallback: if CustomGameID is empty, build from GameDate + teams
    if (!length(ids)) {
      ids <- fallback_game_ids(d)
    }
    
    # order if you have lookup
    if (exists("order_game_ids_desc", inherits = TRUE) && exists("game_dates", inherits = TRUE)) {
      ids <- order_game_ids_desc(ids, game_dates)
    }
    
    ids
  })
  
  # Bullpen IDs for the selected pitcher
  bp_ids_for_pitcher <- reactive({
    req(input$PitcherInput)
    
    if (!exists("bullpens_df", inherits = TRUE) || is.null(bullpens_df) || !nrow(bullpens_df)) {
      return(character(0))
    }
    
    ids <- bullpens_df %>%
      dplyr::filter(.data$Pitcher == input$PitcherInput) %>%
      dplyr::pull(.data$CustomGameID_BP)
    
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    sort(unique(ids))
  })
  
  # What the GameInput choices SHOULD be (depends on Bullpens mode)
  all_game_choices_for_pitcher <- reactive({
    if (isTRUE(input$Bullpens)) bp_ids_for_pitcher() else game_ids_for_pitcher()
  })
  
  season_selected_game_ids <- reactive({
    # seasons only apply to non-bullpen games
    if (isTRUE(input$Bullpens)) return(all_game_choices_for_pitcher())
    
    ch <- game_ids_for_pitcher()
    sg <- input$season_groups %||% character(0)
    
    # If no seasons checked -> select ALL
    if (!length(sg)) return(ch)
    
    # Choose the correct season column robustly
    season_col <- if ("SeasonTag" %in% names(txst_df)) "SeasonTag" else if ("SeasonGroup" %in% names(txst_df)) "SeasonGroup" else NULL
    
    # If we can't find a season column, fall back to ALL (no crash)
    if (is.null(season_col)) return(ch)
    
    sel <- get_pitcher_rows() %>%
      dplyr::filter(.data[[season_col]] %in% sg) %>%
      dplyr::pull(.data$CustomGameID)
    
    sel <- sel[!is.na(sel) & nzchar(trimws(sel))]
    sel <- intersect(ch, unique(sel))
    
    # If a season selection yields nothing, fall back to ALL
    if (!length(sel)) return(ch)
    
    order_game_ids_desc(sel, game_dates)
  })
  
  observeEvent(list(input$PitcherInput, input$Bullpens), {
    ch  <- all_game_choices_for_pitcher()
    cur <- isolate(input$GameInput) %||% character(0)
    pitcher_changed <- !identical(last_pitcher(), input$PitcherInput)
    
    # keep whatever the user already had selected, if it still exists
    keep <- if (pitcher_changed) character(0) else cur[cur %in% ch]
    
    # if nothing is selected/kept, apply the season checkbox default behavior
    if (!length(keep)) keep <- season_selected_game_ids()
    
    last_pitcher(input$PitcherInput)
    
    updatePickerInput(
      session,
      "GameInput",
      choices  = as_named_choices(ch),
      selected = keep
    )
  }, ignoreInit = FALSE)
  
  # (B) When season checkboxes change:
  # DO NOT change choices, only change selected.
  observeEvent(input$season_groups, {
    ch  <- all_game_choices_for_pitcher()
    sel <- season_selected_game_ids()
    
    updatePickerInput(
      session,
      "GameInput",
      choices  = as_named_choices(ch),
      selected = sel
    )
  
  }, ignoreInit = TRUE)
  
  selected_game_ids <- reactive({
    g <- input$GameInput
    if (is.null(g) || length(g) == 0) g <- txst_game_ids
    as.character(g)
  })
  # ---- robust date parsing (prevents charToDate crashes) ----
  parse_date_any <- function(x) {
    if (is.null(x)) return(as.Date(NA))
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    
    x0 <- trimws(as.character(x))
    out <- rep(as.Date(NA), length(x0))
    
    iso <- stringr::str_extract(x0, "(?<!\\d)\\d{4}-\\d{2}-\\d{2}(?!\\d)")
    ok_iso <- !is.na(iso)
    if (any(ok_iso)) out[ok_iso] <- suppressWarnings(as.Date(iso[ok_iso], format = "%Y-%m-%d"))
    
    need <- is.na(out) & nzchar(x0)
    if (any(need)) {
      ymd8 <- stringr::str_extract(x0[need], "(?<!\\d)\\d{8}(?!\\d)")
      ok8 <- !is.na(ymd8)
      if (any(ok8)) {
        idx <- which(need)[ok8]
        out[idx] <- suppressWarnings(as.Date(ymd8[ok8], format = "%Y%m%d"))
      }
    }
    
    need <- is.na(out) & nzchar(x0)
    if (any(need)) {
      mdy <- stringr::str_extract(x0[need], "(?<!\\d)\\d{1,2}/\\d{1,2}/\\d{4}(?!\\d)")
      okm <- !is.na(mdy)
      if (any(okm)) {
        idx <- which(need)[okm]
        out[idx] <- suppressWarnings(as.Date(mdy[okm], format = "%m/%d/%Y"))
      }
    }
    
    need <- is.na(out) & nzchar(x0)
    for (idx in which(need)) {
      out[idx] <- tryCatch(
        suppressWarnings(as.Date(x0[[idx]], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y"))),
        error = function(e) as.Date(NA)
      )
    }
    out
  }
  
  # Parse date from your CustomGameID formats:
  # "YYYYMMDD: AWY @ HOM" OR "YYYY-MM-DD: AWY @ HOM"
  parse_gameid_date <- function(game_id) {
    parse_date_any(as.character(game_id %||% ""))
  }
  # ---- robust game date from rows (fallback if CustomGameID parsing fails) ----
  game_date_from_rows <- function(d) {
    if (is.null(d) || !nrow(d)) return(as.Date(NA))
    
    cand <- intersect(
      c("GameDate","Date","PitchDate","Game_Date","UTCDate","LocalDate"),
      names(d)
    )
    if (!length(cand)) return(as.Date(NA))
    
    # take the first candidate that yields any valid dates
    for (nm in cand) {
      dt <- parse_date_any(d[[nm]])
      dt <- dt[is.finite(dt)]
      if (length(dt)) return(suppressWarnings(min(dt, na.rm = TRUE)))
    }
    as.Date(NA)
  }
  
  # ---- safe division: NEVER return 0 when denominator is 0 ----
  sdiv <- function(num, den) {
    num <- suppressWarnings(as.numeric(num))
    den <- suppressWarnings(as.numeric(den))
    ifelse(is.finite(den) & den > 0, num / den, NA_real_)
  }
  # ---- AAR cell shading helper (GLOBAL in server scope; used by DT tables) ----
  fill_vs_d1 <- function(val, d1,
                         band = 0,
                        green = "#D6EBD3", red = "#F3B9B9", none = NA) {
    val <- suppressWarnings(as.numeric(val))
    d1  <- suppressWarnings(as.numeric(d1))
    band <- suppressWarnings(as.numeric(band))
    if (!is.finite(val) || !is.finite(d1)) return(none)
    if (!is.finite(band)) band <- 0
    
    if (val >= (d1 + band)) return(green)
    if (val <= (d1 - band)) return(red)
    none
  }
  
  # ---- benchmark helpers (safe; never error) ----
  bench_default <- list(
    BARREL_PCT  = NA_real_,
    HARDHIT_PCT = NA_real_
  )
  
  bench_val <- function(season_tag, metric) {
    # 1) If you have a data.frame/tibble called `benchmarks_df` with columns SeasonTag + metric columns:
    if (exists("benchmarks_df", inherits = TRUE)) {
      bdf <- get("benchmarks_df", inherits = TRUE)
      if (is.data.frame(bdf) && "SeasonTag" %in% names(bdf) && metric %in% names(bdf)) {
        v <- bdf[bdf$SeasonTag == season_tag, metric]
        if (length(v) && !all(is.na(v))) return(as.numeric(v[1]))
      }
    }
    
    # 2) If your old code used objects like FALL_2025_BARREL_PCT, try to read them safely:
    obj <- paste0(season_tag, "_", metric)
    if (exists(obj, inherits = TRUE)) {
      v <- get(obj, inherits = TRUE)
      if (length(v)) return(as.numeric(v[1]))
    }
    
    # 3) Fallback default (NA unless you set it above)
    if (!is.null(bench_default[[metric]])) return(bench_default[[metric]])
    NA_real_
  }
  
  season_label_from_tag <- function(tag) {
    dplyr::case_when(
      tag == "S25"  ~ "2025 Season",
      tag == "F25"  ~ "2025 Fall",
      tag == "SQ26" ~ "2026 Squads",
      tag == "S26"  ~ "2026 Season",
      tag == "PORT" ~ "Portal",
      TRUE          ~ as.character(tag %||% "")
    )
  }
  
  season_tag_for_game <- function(game_id, df) {
    if (is.null(df) || !nrow(df)) return(NA_character_)
    season_cols <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
    col <- season_cols[season_cols %in% names(df)][1]
    if (is.na(col)) return(NA_character_)
    out <- df[[col]][match(as.character(game_id), as.character(df$CustomGameID))]
    out <- out[!is.na(out)]
    if (length(out)) as.character(out[1]) else NA_character_
  }

  rv <- reactiveValues(df = df, retag = FALSE, last_sel = integer(0), sync_hover = FALSE)

  # ---- Bullpen tab helpers (Bullpens CSV only via is_bullpen flag) ----
  safe_max_local <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_
  }

  bullpen_pitch_levels <- names(pitch_colors)

  normalize_bullpen_pitch_type <- function(x) {
    out <- trimws(as.character(x %||% ""))
    out[out %in% c("ChangeUp")] <- "Changeup"
    out[!nzchar(out) | is.na(out)] <- "Undefined"
    out[!(out %in% bullpen_pitch_levels)] <- "Undefined"
    out
  }

  sanitize_bullpen_dataset <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) return(d)
    n <- nrow(d)
    get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n)
    d$PitchType <- factor(
      normalize_bullpen_pitch_type(dplyr::coalesce(get_chr("PitchType"), get_chr("TaggedPitchType"), get_chr("AutoPitchType"))),
      levels = bullpen_pitch_levels
    )
    d
  }

  order_bullpen_rows <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) return(d)
    ord_cols <- intersect(c("row_in_file", "row_id", "PitchNo", "PitchNumber", "PitchNum"), names(d))
    if (!length(ord_cols)) return(d)
    dplyr::arrange(d, dplyr::across(dplyr::all_of(ord_cols)))
  }

  bullpen_report_date_value <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) return(as.Date(NA))
    safe_parse_bp_date <- function(x) {
      tryCatch(
        parse_date_any(x),
        error = function(e) rep(as.Date(NA), length(x))
      )
    }
    for (nm in intersect(c("GameDate", "Date", "PitchDate", "Game_Date", "UTCDate", "LocalDate"), names(d))) {
      dt <- safe_parse_bp_date(d[[nm]])
      dt <- dt[!is.na(dt)]
      if (length(dt)) return(min(dt))
    }
    if ("CustomGameID_BP" %in% names(d)) {
      dt <- safe_parse_bp_date(sub(":.*$", "", as.character(d$CustomGameID_BP)))
      dt <- dt[!is.na(dt)]
      if (length(dt)) return(min(dt))
    }
    as.Date(NA)
  }

  build_bullpen_ids_local <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) return(d)
    bp_label <- if ("Date" %in% names(d)) as.character(d$Date) else rep(NA_character_, nrow(d))
    idx <- is.na(bp_label) | !nzchar(trimws(bp_label))
    if (any(idx) && "GameDate" %in% names(d)) {
      gd <- tryCatch(parse_date_any(d$GameDate[idx]), error = function(e) rep(as.Date(NA), sum(idx)))
      bp_label[idx] <- ifelse(!is.na(gd), as.character(gd), bp_label[idx])
    }
    idx <- is.na(bp_label) | !nzchar(trimws(bp_label))
    if (any(idx)) bp_label[idx] <- if ("source_file" %in% names(d)) as.character(d$source_file[idx]) else "(undated)"
    idx <- is.na(bp_label) | !nzchar(trimws(bp_label))
    if (any(idx)) bp_label[idx] <- "(undated)"
    d$CustomGameID_BP <- paste0(bp_label, ": Bullpen - ", d$Pitcher)
    d
  }

  prepare_bullpen_report_data <- function(d) {
    d <- sanitize_bullpen_dataset(order_bullpen_rows(d))
    if (!nrow(d)) return(d)
    d <- d %>%
      dplyr::mutate(
        PitchType_plot = as.character(PitchType),
        HB = suppressWarnings(as.numeric(HorzBreak)),
        IVB = suppressWarnings(as.numeric(InducedVertBreak)),
        Velo = suppressWarnings(as.numeric(RelSpeed)),
        RPM = suppressWarnings(as.numeric(SpinRate)),
        RelHt = suppressWarnings(as.numeric(RelHeight)),
        RelSideNum = suppressWarnings(as.numeric(RelSide)),
        Ext = suppressWarnings(as.numeric(Extension)),
        VAA = suppressWarnings(as.numeric(VertApprAngle)),
        HAA = suppressWarnings(as.numeric(HorzApprAngle)),
        PlateLocSidePlot = suppressWarnings(as.numeric(PlateLocSide)),
        PlateLocHeightPlot = suppressWarnings(as.numeric(PlateLocHeight)),
        BullpenPitchNumber = dplyr::row_number(),
        PitchLabelColor = dplyr::if_else(PitchType_plot %in% c("Slider", "Sweeper", "Curveball"), "black", "white"),
        HoverMovement = paste0(
          "<b>", PitchType_plot, "</b><br>",
          "Pitch #: ", BullpenPitchNumber, "<br>",
          "Velo: ", ifelse(is.finite(Velo), sprintf("%.1f", Velo), "--"), "<br>",
          "HB: ", ifelse(is.finite(HB), sprintf("%.1f", HB), "--"), "<br>",
          "iVB: ", ifelse(is.finite(IVB), sprintf("%.1f", IVB), "--")
        )
      )
    d$PlateLocSidePlot <- ifelse(is.finite(d$PlateLocSidePlot) & abs(d$PlateLocSidePlot) > 5, d$PlateLocSidePlot / 12, d$PlateLocSidePlot)
    d$PlateLocHeightPlot <- ifelse(is.finite(d$PlateLocHeightPlot) & d$PlateLocHeightPlot > 10, d$PlateLocHeightPlot / 12, d$PlateLocHeightPlot)
    d$ZoneFlag <- derive_zone_inches(d)
    h_in <- suppressWarnings(readr::parse_number(as.character(d$PlateLocHeight)))
    h_in <- ifelse(is.finite(h_in) & h_in < 10, h_in * 12, h_in)
    z_min <- 18.29
    z_max <- 44.08
    z_third <- (z_max - z_min) / 3
    d$TopThirdFlag <- is.finite(h_in) & h_in >= (z_min + 2 * z_third) & h_in <= z_max
    d$BottomThirdFlag <- is.finite(h_in) & h_in >= z_min & h_in <= (z_min + z_third)
    d
  }

  bullpen_metric_summary_raw <- function(d, include_total = FALSE) {
    d <- prepare_bullpen_report_data(d)
    if (!nrow(d)) {
      return(tibble::tibble(
        PitchType = character(), Pitches = integer(), ZonePct = numeric(),
        VeloAvg = numeric(), VeloMax = numeric(), RPMAvg = numeric(), RPMMax = numeric(),
        IVBAvg = numeric(), HBAvg = numeric(), RelHt = numeric(), RelSide = numeric(),
        Ext = numeric(), VAA = numeric(), TopThirdVAA = numeric(), BottomThirdVAA = numeric(), HAA = numeric()
      ))
    }
    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    summarise_block <- function(dd, label) {
      tibble::tibble(
        PitchType = label,
        Pitches = nrow(dd),
        ZonePct = if (any(!is.na(dd$ZoneFlag))) mean(dd$ZoneFlag, na.rm = TRUE) else NA_real_,
        VeloAvg = mean_or_na(dd$Velo),
        VeloMax = safe_max_local(dd$Velo),
        RPMAvg = mean_or_na(dd$RPM),
        RPMMax = safe_max_local(dd$RPM),
        IVBAvg = mean_or_na(dd$IVB),
        HBAvg = mean_or_na(dd$HB),
        RelHt = mean_or_na(dd$RelHt),
        RelSide = mean_or_na(dd$RelSideNum),
        Ext = mean_or_na(dd$Ext),
        VAA = mean_or_na(dd$VAA),
        TopThirdVAA = mean_or_na(dd$VAA[dd$TopThirdFlag %in% TRUE]),
        BottomThirdVAA = mean_or_na(dd$VAA[dd$BottomThirdFlag %in% TRUE]),
        HAA = mean_or_na(dd$HAA)
      )
    }
    pt_levels <- unique(as.character(d$PitchType_plot))
    pt_levels <- pt_levels[nzchar(pt_levels)]
    pt_levels <- c(intersect(bullpen_pitch_levels, pt_levels), setdiff(pt_levels, bullpen_pitch_levels))
    out <- purrr::map_dfr(pt_levels, function(pt) summarise_block(d[d$PitchType_plot == pt, , drop = FALSE], pt))
    out <- out %>% dplyr::arrange(dplyr::desc(Pitches), PitchType)
    if (isTRUE(include_total)) out <- dplyr::bind_rows(out, summarise_block(d, "Total"))
    out
  }

  format_bullpen_summary <- function(d, include_total = FALSE, report = FALSE) {
    raw <- bullpen_metric_summary_raw(d, include_total = include_total)
    if (!nrow(raw)) return(data.frame(Status = "No bullpen data", stringsAsFactors = FALSE))
    fmt_num <- function(x, digits = 1) ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "--")
    fmt_pct <- function(x) ifelse(is.finite(x), paste0(round(100 * x), "%"), "--")
    fmt_avg_max <- function(avg, mx, digits = 1) {
      if (!is.finite(avg) && !is.finite(mx)) return("--")
      if (is.finite(avg) && is.finite(mx)) return(paste0(formatC(avg, digits = digits, format = "f"), " (", formatC(mx, digits = digits, format = "f"), ")"))
      if (is.finite(avg)) return(formatC(avg, digits = digits, format = "f"))
      paste0("(", formatC(mx, digits = digits, format = "f"), ")")
    }
    out <- raw %>%
      dplyr::transmute(
        `Pitch Type` = PitchType,
        `#` = Pitches,
        `Zone%` = fmt_pct(ZonePct),
        `Velo (Max)` = purrr::map2_chr(VeloAvg, VeloMax, ~ fmt_avg_max(.x, .y, digits = 1)),
        RPM = fmt_num(RPMAvg, 0),
        iVB = fmt_num(IVBAvg, 1),
        HB = fmt_num(HBAvg, 1),
        `Rel Ht` = fmt_num(RelHt, 1),
        `Rel Side` = fmt_num(RelSide, 1),
        Extension = fmt_num(Ext, 1),
        VAA = fmt_num(VAA, 1),
        HAA = fmt_num(HAA, 1)
      )
    if (!isTRUE(report)) {
      out <- dplyr::bind_cols(
        out[, 1:10, drop = FALSE],
        raw %>% dplyr::transmute(`Top Third VAA` = fmt_num(TopThirdVAA, 1), `Bottom Third VAA` = fmt_num(BottomThirdVAA, 1)),
        out[, 11:12, drop = FALSE]
      )
    }
    out
  }

  format_bullpen_pitch_log <- function(d) {
    d <- prepare_bullpen_report_data(d)
    if (!nrow(d)) return(data.frame(Status = "No bullpen data", stringsAsFactors = FALSE))
    fmt_num <- function(x, digits = 1) ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "--")
    d %>%
      dplyr::transmute(
        `Pitch #` = BullpenPitchNumber,
        `Pitch Type` = PitchType_plot,
        Velo = fmt_num(Velo, 1),
        RPM = fmt_num(RPM, 0),
        iVB = fmt_num(IVB, 1),
        HB = fmt_num(HB, 1),
        `Rel Ht` = fmt_num(RelHt, 1),
        `Rel Side` = fmt_num(RelSideNum, 1),
        Extension = fmt_num(Ext, 1),
        VAA = fmt_num(VAA, 1),
        HAA = fmt_num(HAA, 1)
      )
  }

  build_bullpen_accum_movement_plotly <- function(d, title_text) {
    d <- prepare_bullpen_report_data(d)
    mv <- d %>% dplyr::filter(is.finite(HB), is.finite(IVB))
    validate(need(nrow(mv) > 0, "No bullpen movement data for the selected pitcher/sessions."))
    p <- plotly::plot_ly()
    for (lv in sort(unique(mv$PitchType_plot))) {
      dd <- mv %>% dplyr::filter(PitchType_plot == lv)
      col <- pitch_colors[[lv]] %||% "#808080"
      p <- p %>% add_trace(
        data = dd,
        x = ~HB, y = ~IVB,
        type = "scatter", mode = "markers",
        marker = list(size = 8, opacity = 0.35, color = col),
        text = ~HoverMovement,
        hovertemplate = "%{text}<extra></extra>",
        name = lv,
        showlegend = TRUE
      )
    }
    avg <- mv %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(HB = mean(HB, na.rm = TRUE), IVB = mean(IVB, na.rm = TRUE), Velo = mean(Velo, na.rm = TRUE), RPM = mean(RPM, na.rm = TRUE), .groups = "drop")
    for (i in seq_len(nrow(avg))) {
      row <- avg[i, ]
      p <- p %>% add_trace(
        data = row,
        x = ~HB, y = ~IVB,
        type = "scatter", mode = "markers",
        marker = list(size = 16, opacity = 1, color = pitch_colors[[row$PitchType_plot[[1]]]] %||% "#808080", line = list(color = "black", width = 0.7)),
        text = ~paste0(PitchType_plot, " Avg<br>Velo: ", sprintf("%.1f", Velo), "<br>iVB: ", sprintf("%.1f", IVB), "<br>HB: ", sprintf("%.1f", HB), "<br>RPM: ", sprintf("%.0f", RPM)),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    p %>% layout(
      title = list(text = title_text, x = 0.5),
      margin = list(t = 70, r = 20, b = 40, l = 60),
      xaxis = list(title = "Horizontal Break (in)", range = c(-25, 25), dtick = 3, zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(title = "Induced Vertical Break (in)", range = c(-25, 25), dtick = 3, zeroline = FALSE, fixedrange = TRUE, scaleanchor = "x", scaleratio = 1),
      shapes = list(
        list(type = "line", x0 = -25, x1 = 25, y0 = 0, y1 = 0, line = list(dash = "dot", width = 1, color = "black")),
        list(type = "line", x0 = 0, x1 = 0, y0 = -25, y1 = 25, line = list(dash = "dot", width = 1, color = "black"))
      )
    )
  }

  build_bullpen_numbered_movement_gg <- function(d, title_text = "Movement Plot") {
    d <- prepare_bullpen_report_data(d)
    mv <- d %>% dplyr::filter(is.finite(HB), is.finite(IVB))
    if (!nrow(mv)) return(ggplot() + theme_void() + labs(title = paste0(title_text, " (no data)")))
    ggplot(mv, aes(HB, IVB)) +
      geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.6) +
      geom_vline(xintercept = 0, color = "black", linetype = "dotted", linewidth = 0.6) +
      geom_point(aes(fill = PitchType_plot), shape = 21, size = 3.3, color = "black", stroke = 0.35) +
      geom_text(aes(label = BullpenPitchNumber, color = PitchLabelColor), size = 1.5, fontface = "bold", show.legend = FALSE) +
      scale_fill_manual(values = pitch_colors, limits = names(pitch_colors), drop = FALSE) +
      scale_color_identity() +
    scale_x_continuous(breaks = seq(-24, 24, by = 3), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(-24, 24, by = 3), minor_breaks = NULL) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25), expand = FALSE) +
      labs(title = title_text, x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
  }

  build_bullpen_numbered_location_gg <- function(d, title_text = "Strike Zone Location") {
    d <- prepare_bullpen_report_data(d)
    loc <- d %>% dplyr::filter(is.finite(PlateLocSidePlot), is.finite(PlateLocHeightPlot), dplyr::between(PlateLocSidePlot, -3, 3), dplyr::between(PlateLocHeightPlot, 0, 5))
    if (!nrow(loc)) return(ggplot() + theme_void() + labs(title = paste0(title_text, " (no data)")))
    bullpen_home_plate_segments <- data.frame(
      x = c(-0.71, 0.71, 0.71, 0, -0.71),
      y = c(-0.5, -0.5, -0.25, 0.0, -0.25),
      xend = c(0.71, 0.71, 0, -0.71, -0.71),
      yend = c(-0.5, -0.25, 0.0, -0.25, -0.5)
    )
    ggplot(loc, aes(PlateLocSidePlot, PlateLocHeightPlot)) +
      geom_point(aes(fill = PitchType_plot), shape = 21, size = 3.3, color = "black", stroke = 0.35) +
      geom_text(aes(label = BullpenPitchNumber, color = PitchLabelColor), size = 1.5, fontface = "bold", show.legend = FALSE) +
      scale_fill_manual(values = pitch_colors, limits = names(pitch_colors), drop = FALSE) +
      scale_color_identity() +
      geom_rect(data = strike_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1) +
      geom_segment(data = bullpen_home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
      coord_fixed(xlim = c(-3, 3), ylim = c(-0.5, 4.1), expand = FALSE) +
      labs(title = title_text, x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }

  bullpen_report_header_plot <- function(pitcher_name, report_date = NA, subtitle = NULL, page_label = NULL) {
    date_txt <- if (!is.na(report_date)) format(as.Date(report_date), "%B %d, %Y") else "Bullpen Report"
    ggplot() +
      annotate("text", x = 0, y = 0.83, label = pitcher_name, hjust = 0, vjust = 1, size = 6.2, fontface = "bold", color = "#501214") +
      annotate("text", x = 0, y = 0.43, label = date_txt, hjust = 0, vjust = 1, size = 4.3, color = "#501214", fontface = "bold") +
      { if (!is.null(subtitle) && nzchar(subtitle)) annotate("text", x = 0.5, y = 0.70, label = subtitle, hjust = 0.5, vjust = 1, size = 4.0, fontface = "bold", color = "#501214") } +
      { if (!is.null(page_label) && nzchar(page_label)) annotate("text", x = 0.98, y = 0.38, label = page_label, hjust = 1, vjust = 1, size = 3.6, color = "#4C4C4C") } +
      xlim(0, 1) + ylim(0, 1) + theme_void() +
      theme(plot.margin = margin(4, 6, 2, 6))
  }

  bullpen_report_logo_bundle <- function() {
    logo_path <- find_asset(c("txstlogo", "TXSTlogo", "Txstlogo", "Bobcatlogo", "bobcatlogo", "BobcatLogo"))
    if (is.null(logo_path) || !file.exists(logo_path)) return(list(grob = NULL, aspect = 1))
    ext <- tolower(tools::file_ext(logo_path))
    logo_img <- tryCatch(
      if (ext == "png") png::readPNG(logo_path)
      else if (ext %in% c("jpg", "jpeg") && requireNamespace("jpeg", quietly = TRUE)) jpeg::readJPEG(logo_path)
      else NULL,
      error = function(e) NULL
    )
    if (is.null(logo_img)) return(list(grob = NULL, aspect = 1))
    h <- dim(logo_img)[1]
    w <- dim(logo_img)[2]
    aspect <- if (is.finite(h) && is.finite(w) && h > 0) w / h else 1
    list(grob = grid::rasterGrob(logo_img, interpolate = TRUE), aspect = aspect)
  }

  wrap_bullpen_report_page <- function(body_plot) {
    logo <- bullpen_report_logo_bundle()
    cowplot::ggdraw() +
      cowplot::draw_plot(body_plot, x = 0, y = 0, width = 1, height = 1) +
      { if (!is.null(logo$grob)) cowplot::draw_grob(logo$grob, x = 0.5, y = 0.945, width = min(0.045 * logo$aspect, 0.10), height = 0.045, hjust = 0.5, vjust = 1) }
  }

  split_bullpen_log_pages <- function(log_df, rows_per_page = 30L) {
    if (is.null(log_df) || !nrow(log_df)) return(list(log_df))
    split(log_df, ceiling(seq_len(nrow(log_df)) / max(1L, rows_per_page)))
  }

  compose_bullpen_report_page1 <- function(d, pitcher_name, report_date = NA, total_pages = 1L) {
    d <- prepare_bullpen_report_data(d)
    if (!nrow(d)) return(ggplot() + theme_void() + labs(title = "Bullpen report: No data available"))
    body <- bullpen_report_header_plot(pitcher_name, report_date, "Bullpen Report", paste0("Page 1 of ", total_pages)) /
      ((build_bullpen_numbered_movement_gg(d, "Movement Plot") | build_bullpen_numbered_location_gg(d, "Strike Zone Location")) + patchwork::plot_layout(widths = c(0.5, 0.5))) /
      safe_tbl_plot(format_bullpen_summary(d, include_total = TRUE, report = TRUE), "Pitch Type Summary") +
      patchwork::plot_layout(heights = c(0.08, 0.48, 0.44))
    wrap_bullpen_report_page(body)
  }

  compose_bullpen_report_log_page <- function(log_df, pitcher_name, report_date = NA, page_num = 2L, total_pages = 2L, range_label = NULL) {
    title_text <- if (!is.null(range_label) && nzchar(range_label)) paste0("Pitch Log (", range_label, ")") else "Pitch Log"
    body <- bullpen_report_header_plot(pitcher_name, report_date, "Bullpen Report", paste0("Page ", page_num, " of ", total_pages)) /
      safe_tbl_plot(log_df, title_text) +
      patchwork::plot_layout(heights = c(0.08, 0.92))
    wrap_bullpen_report_page(body)
  }

  compose_bullpen_report_pages <- function(d, pitcher_name, report_date = NA) {
    d <- prepare_bullpen_report_data(d)
    if (!nrow(d)) return(list(ggplot() + theme_void() + labs(title = "Bullpen report: No data available")))
    log_pages <- split_bullpen_log_pages(format_bullpen_pitch_log(d))
    total_pages <- 1L + length(log_pages)
    pages <- list(compose_bullpen_report_page1(d, pitcher_name, report_date, total_pages))
    for (i in seq_along(log_pages)) {
      chunk <- log_pages[[i]]
      pitch_nums <- suppressWarnings(as.integer(chunk[["Pitch #"]]))
      range_label <- if (length(pitch_nums) && all(is.finite(pitch_nums))) paste0(min(pitch_nums), "-", max(pitch_nums)) else NA_character_
      pages[[length(pages) + 1L]] <- compose_bullpen_report_log_page(chunk, pitcher_name, report_date, i + 1L, total_pages, range_label)
    }
    pages
  }

  render_bullpen_report_pdf <- function(d, pitcher_name, report_date = NA, outfile) {
    pages <- compose_bullpen_report_pages(d, pitcher_name, report_date)
    grDevices::pdf(outfile, width = 8.5, height = 11, useDingbats = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (pg in pages) print(pg)
    invisible(outfile)
  }

  bullpens_live_df <- reactive({
    d <- rv$df %>% dplyr::filter(is_bullpen %in% TRUE)
    d <- build_bullpen_ids_local(d)
    if ("Pitcher" %in% names(d)) d$Pitcher <- fix_name_commas(d$Pitcher)
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id" %in% names(d)) d <- d %>% dplyr::distinct(row_id, .keep_all = TRUE)
    d
  })

  bullpen_sessions_for_pitcher <- reactive({
    req(input$PitcherInput)
    d <- bullpens_live_df() %>% dplyr::filter(Pitcher == input$PitcherInput)
    if (!nrow(d)) return(character(0))
    lookup <- d %>%
      dplyr::group_by(CustomGameID_BP) %>%
      dplyr::summarise(BullpenDate = bullpen_report_date_value(dplyr::cur_data_all()), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(BullpenDate), CustomGameID_BP)
    lookup$CustomGameID_BP
  })

  observeEvent(list(input$PitcherInput, bullpens_live_df()), {
    sessions <- bullpen_sessions_for_pitcher()
    cur_accum <- isolate(input$bullpen_accum_games) %||% character(0)
    keep_accum <- cur_accum[cur_accum %in% sessions]
    if (!length(keep_accum)) keep_accum <- sessions
    shinyWidgets::updatePickerInput(session, "bullpen_accum_games", choices = as_named_choices(sessions), selected = keep_accum)

    cur_report <- isolate(input$bullpen_report_game) %||% NA_character_
    sel_report <- if (!is.na(cur_report) && cur_report %in% sessions) cur_report else if (length(sessions)) sessions[[1]] else character(0)
    updateSelectInput(session, "bullpen_report_game", choices = as_named_choices(sessions), selected = sel_report)
  }, ignoreInit = FALSE)

  bullpen_accum_data <- reactive({
    req(input$PitcherInput)
    d <- bullpens_live_df() %>% dplyr::filter(Pitcher == input$PitcherInput)
    sessions <- input$bullpen_accum_games
    if (is.null(sessions) || !length(sessions)) sessions <- bullpen_sessions_for_pitcher()
    if (length(sessions)) d <- d %>% dplyr::filter(CustomGameID_BP %in% sessions)
    if ("CustomGameID_BP" %in% names(d)) d$CustomGameID <- d$CustomGameID_BP
    order_bullpen_rows(d)
  })

  output$bullpen_accum_movement <- renderPlotly({
    req(input$PitcherInput)
    build_bullpen_accum_movement_plotly(bullpen_accum_data(), paste0(input$PitcherInput, ": Bullpen Movement"))
  })

  output$bullpen_accum_metrics <- DT::renderDT({
    tbl <- format_bullpen_summary(bullpen_accum_data(), include_total = FALSE, report = FALSE)
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE), class = "stripe")
  })

  bullpen_report_data <- reactive({
    req(input$PitcherInput, input$bullpen_report_game)
    d <- bullpens_live_df() %>% dplyr::filter(Pitcher == input$PitcherInput, CustomGameID_BP == input$bullpen_report_game)
    validate(need(nrow(d) > 0, "No bullpen data for that report."))
    order_bullpen_rows(d)
  })

  bullpen_report_date <- reactive({
    bullpen_report_date_value(bullpen_report_data())
  })

  bullpen_report_pages <- reactive({
    if (is.null(input$PitcherInput) || !nzchar(input$PitcherInput) ||
        is.null(input$bullpen_report_game) || !nzchar(input$bullpen_report_game)) {
      return(list())
    }
    tryCatch(
      compose_bullpen_report_pages(bullpen_report_data(), input$PitcherInput, bullpen_report_date()),
      error = function(e) list()
    )
  })

  bullpen_report_page_count <- reactive({
    max(1L, length(bullpen_report_pages()))
  })

  output$bullpen_report_preview_ui <- renderUI({
    do.call(tagList, lapply(seq_len(bullpen_report_page_count()), function(i) {
      imageOutput(paste0("bullpen_report_preview_", i), height = "1320px")
    }))
  })

  bullpen_report_pdf_cache <- reactiveVal(NULL)
  observeEvent(list(input$PitcherInput, input$bullpen_report_game), {
    old <- bullpen_report_pdf_cache()
    if (!is.null(old) && file.exists(old)) unlink(old)
    bullpen_report_pdf_cache(NULL)
    if (is.null(input$PitcherInput) || !nzchar(input$PitcherInput) ||
        is.null(input$bullpen_report_game) || !nzchar(input$bullpen_report_game)) {
      return()
    }
    d <- tryCatch(bullpen_report_data(), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return()
    tmp_pdf <- tempfile(fileext = ".pdf")
    try({
      render_bullpen_report_pdf(d, input$PitcherInput, bullpen_report_date(), tmp_pdf)
      bullpen_report_pdf_cache(tmp_pdf)
    }, silent = TRUE)
  }, ignoreInit = TRUE)

  observe({
    n_pages <- bullpen_report_page_count()
    for (i in seq_len(n_pages)) {
      local({
        page_i <- i
        output[[paste0("bullpen_report_preview_", page_i)]] <- renderImage({
          blank_png <- function(msg = NULL) {
            tmp <- tempfile(fileext = ".png")
            png(tmp, width = 1600, height = 2200, res = 180, bg = "white")
            par(mar = c(0, 0, 0, 0))
            plot.new()
            if (!is.null(msg) && nzchar(msg)) text(0.5, 0.5, msg, cex = 1.1)
            dev.off()
            list(src = tmp, contentType = "image/png", width = "100%")
          }
          if (is.null(input$PitcherInput) || !nzchar(input$PitcherInput) ||
              is.null(input$bullpen_report_game) || !nzchar(input$bullpen_report_game)) {
            return(blank_png())
          }
          tryCatch({
            pdf_path <- bullpen_report_pdf_cache()
            if (!is.null(pdf_path) && file.exists(pdf_path) &&
                requireNamespace("pdftools", quietly = TRUE) && requireNamespace("png", quietly = TRUE)) {
              tmp_png <- tempfile(fileext = ".png")
              save_pdf_preview_png(pdf_path, tmp_png, page = page_i, dpi = 180)
              return(list(src = tmp_png, contentType = "image/png", width = "100%"))
            }
            pages <- bullpen_report_pages()
            if (page_i > length(pages)) return(blank_png())
            tmp_png <- tempfile(fileext = ".png")
            png(tmp_png, width = 1600, height = 2200, res = 180, bg = "white")
            print(pages[[page_i]])
            dev.off()
            list(src = tmp_png, contentType = "image/png", width = "100%")
          }, error = function(e) {
            blank_png(paste("BULLPEN REPORT ERROR:\n\n", conditionMessage(e)))
          })
        }, deleteFile = TRUE)
      })
    }
  })

  output$bullpen_report_pdf <- downloadHandler(
    filename = function() {
      report_date <- bullpen_report_date()
      date_stub <- if (!is.na(report_date)) format(report_date, "%Y%m%d") else "undated"
      paste0("Bullpen_", gsub("\\s+", "_", input$PitcherInput %||% "Pitcher"), "_", date_stub, ".pdf")
    },
    content = function(file) {
      render_bullpen_report_pdf(bullpen_report_data(), input$PitcherInput, bullpen_report_date(), file)
    }
  )

  aar_season_group_values <- reactive({
    embedded_choice <- input$pitch_aar_season_groups
    if (!is.null(embedded_choice)) return(as.character(embedded_choice))
    as.character(input$season_groups %||% character(0))
  })

  aar_season_info <- reactive({
    shiny::req(input$aar_game)
    
    # Don't block AAR if season tags aren't present
    season_cols <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
    if (is.null(rv$df) || !nrow(rv$df) || !any(season_cols %in% names(rv$df))) {
      return(list(tag = NA_character_, label = "Season"))
    }
    
    tag <- season_tag_for_game(input$aar_game, rv$df)
    
    if (is.na(tag) || !nzchar(tag)) {
      return(list(tag = NA_character_, label = "Season"))
    }
    
    list(tag = as.character(tag), label = season_label_from_tag(tag))
  })
  
  
  selected_game_id <- reactive({
    if (isTRUE(input$Bullpens)) {
      # first bullpen ID for the current pitcher, from the current selection if any
      choices <- if (length(input$GameInput)) input$GameInput else nz_choices(
        bullpens_df$CustomGameID_BP[bullpens_df$Pitcher == input$PitcherInput]
      )
    } else {
      choices <- input$GameInput
    }
    if (!length(choices)) return(NA_character_)
    as.character(choices[[1]])
  })
  
  # Fill AAR pitcher choices
  observe({
    pits <- setdiff(sort(unique(na.omit(txst_df$Pitcher))), EXCLUDE_PLAYERS)
    if (!length(pits)) pits <- sort(unique(na.omit(rv$df$Pitcher)))
    updateSelectInput(session, "aar_pitcher", choices = as_pitcher_choices(pits), selected = pits[1])
  })
  # Populate AAR pitcher list from your live data (non-bullpen only)
  observe({
    pitchers <- rv$df %>%
      dplyr::filter(!(is_bullpen %in% TRUE)) %>%
      dplyr::pull(Pitcher) %>% unique() %>% sort()
    pitchers <- setdiff(pitchers, EXCLUDE_PLAYERS)
    updateSelectInput(session, "aar_pitcher",
                      choices = as_pitcher_choices(pitchers),
                      selected = if (length(pitchers)) pitchers[[1]] else character(0))
  })
  
  observeEvent(input$aar_pitcher, {
    req(input$aar_pitcher)
    
    games <- txst_df %>%
      dplyr::filter(Pitcher == input$aar_pitcher, !(is_bullpen %in% TRUE)) %>%
      dplyr::pull(CustomGameID) %>% unique()
    
    if (!length(games)) {
      d_fallback <- df_nonbp %>% dplyr::filter(Pitcher == input$aar_pitcher)
      games <- fallback_game_ids(d_fallback)
    }
    
    games <- order_game_ids_desc(games, game_dates)
    
    updateSelectInput(
      session, "aar_game",
      choices  = games,
      selected = if (length(games)) games[[1]] else character(0)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$aar_game, {
    req(input$aar_pitcher, input$aar_game)
    d <- rv$df %>%
      dplyr::filter(Pitcher == input$aar_pitcher,
                    CustomGameID == input$aar_game,
                    !(is_bullpen %in% TRUE))
    
    is_fastball_pt <- function(pt_chr) {
      if (is.factor(pt_chr)) pt_chr <- as.character(pt_chr)
      grepl("^(fastball|four[- ]seam|two[- ]seam|sinker)$", tolower(trimws(pt_chr)))
    }
    
    # Default: parse from the CustomGameID "YYYYMMDD: AWY @ HOM"
    fallback <- {
      cg <- as.character(input$aar_game)
      # Try to infer the opponent (first token before/after "@")
      tmp <- stringr::str_match(cg, ":(?:\\s*)([A-Za-z0-9]{2,4})\\s*@\\s*([A-Za-z0-9]{2,4})")
      if (is.na(tmp[1,1])) "" else {
        awy <- tmp[1,2]; hom <- tmp[1,3]
        my  <- dplyr::first(na.omit(d$PitcherTeam))
        if (!is.na(my) && toupper(my) == toupper(hom)) paste("vs", awy) else paste("@", hom)
      }
    }
    
    guess <- tryCatch({
      home <- dplyr::first(na.omit(d$HomeTeam))
      away <- dplyr::first(na.omit(d$AwayTeam))
      my   <- dplyr::first(na.omit(d$PitcherTeam))
      if (is.na(home) || is.na(away) || is.na(my)) fallback else
        if (toupper(my) == toupper(home)) paste("vs", substr(away, 1, 3)) else paste("@", substr(home, 1, 3))
    }, error = function(e) fallback)
    
    # Only auto-fill if the box is currently empty
    if (!nzchar(input$aar_opp) && nzchar(guess))
      updateTextInput(session, "aar_opp", value = guess)
  }, ignoreInit = TRUE)
  std_aar_cols <- function(d){
    if (!nrow(d)) return(d)
    to_num <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
    swing_calls <- c("StrikeSwinging","InPlay","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
    
    # Canonical PA and count scaffolding
    d <- ensure_counts(d) %>%
      dplyr::mutate(
        PitchNumberInPA = .data$PitchNum,
        FirstPitch = .data$PitchNum == 1L
      )
    
    evla <- resolve_ev_la(d)
    ev <- evla$ev; la <- evla$la
    
    d$inZone <- as.integer(derive_zone_inches(d))
    
    evla <- resolve_ev_la_strict(d)
    ev <- evla$ev; la <- evla$la
    
    # Use explicit vectors here rather than data-mask introspection; this keeps
    # the AAR path stable when optional TrackMan columns are absent.
    pitch_call <- if ("PitchCall" %in% names(d)) as.character(d$PitchCall) else rep("", nrow(d))
    play_result <- if ("PlayResult" %in% names(d)) as.character(d$PlayResult) else rep("", nrow(d))
    korbb <- if ("KorBB" %in% names(d)) as.character(d$KorBB) else rep("", nrow(d))
    d$CalledStrike <- pitch_call == "StrikeCalled"
    d$SwingingStrike <- pitch_call == "StrikeSwinging"
    d$Foul <- pitch_call %in% swing_calls[3:5]
    d$IsStrike <- d$CalledStrike | d$SwingingStrike | d$Foul
    d$IsCalledStrike <- d$CalledStrike
    d$IsSwing <- .is_swing_event(pitch_call)
    d$InZone <- d$inZone == 1L
    d$StrikesBeforePitch <- if ("StrikesPre" %in% names(d)) as.integer(d$StrikesPre) else rep(NA_integer_, nrow(d))
    d$HBP <- grepl("(?i)hit by pitch|\\bHBP\\b", play_result) | grepl("(?i)HBP", korbb)
    d$Barrel <- is_barrel_strict(pitch_call, ev, la)
    d$IVB <- if ("InducedVertBreak" %in% names(d)) to_num(d$InducedVertBreak) else rep(NA_real_, nrow(d))
    d$HB <- if ("HorzBreak" %in% names(d)) to_num(d$HorzBreak) else rep(NA_real_, nrow(d))
    batter_side <- if ("BatterSide" %in% names(d)) as.character(d$BatterSide) else rep(NA_character_, nrow(d))
    d$BatterSide <- dplyr::case_when(
      batter_side %in% c("L","Left","LHH","LH") ~ "L",
      batter_side %in% c("R","Right","RHH","RH") ~ "R",
      TRUE ~ batter_side
    )
    d
  }
  aar_game_data <- reactive({
    req(input$aar_pitcher, input$aar_game)
    d <- rv$df %>%
      dplyr::filter(Pitcher == input$aar_pitcher,
                    CustomGameID == input$aar_game,
                    !(is_bullpen %in% TRUE))
    validate(need(nrow(d) > 0, "No data for that pitcher/game."))
    # --- dedup once before building PA/counts ---
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    std_aar_cols(d)
  })
  
  aar_season_data <- reactive({
    req(input$aar_pitcher)
    d <- rv$df %>%
      dplyr::filter(Pitcher == input$aar_pitcher, !(is_bullpen %in% TRUE))
    # --- dedup season too ---
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    std_aar_cols(d)
  })
  
  # ---- AAR helpers / reactives ----
  
  # Filter to THIS pitcher & THIS game (non-bullpen)
  aar_game <- reactive({
    req(input$aar_pitcher, input$aar_game)
    d <- rv$df %>%
      dplyr::filter(
        Pitcher == input$aar_pitcher,
        CustomGameID == input$aar_game,
        !(is_bullpen %in% TRUE)
      )
    
    d <- prefer_team_or_portal(d, "PitcherTeam")
    
    d %>%
      dplyr::distinct(pitch_uid, .keep_all = TRUE)
  })
  
  # Filter to THIS pitcher, full season (non-bullpen)
  aar_season <- reactive({
    req(input$aar_pitcher)
    rv$df %>%
      dplyr::filter(
        Pitcher == input$aar_pitcher,
        !(is_bullpen %in% TRUE)
      ) %>%
      filter_team_or_portal("PitcherTeam") %>%
      dplyr::distinct(pitch_uid, .keep_all = TRUE)
  })
  
  # Parse "YYYY-MM-DD: AAA @ BBB" → date + opponent guess
  aar_selected_game_meta <- reactive({
    req(input$aar_game, input$aar_pitcher)
    s <- as.character(input$aar_game)
    
    # pull rows first (so date fallback can use them)
    d_rows <- rv$df %>%
      dplyr::filter(
        CustomGameID == s,
        Pitcher == input$aar_pitcher,
        !(is_bullpen %in% TRUE)
      )
    
    # date (prefer CustomGameID parse; fallback to rows)
    dt <- parse_gameid_date(s)
    if (is.na(dt)) dt <- game_date_from_rows(d_rows)
    
    
    
    # teams
    teams <- trimws(sub("^[^:]+:\\s*", "", s))
    away  <- sub("\\s*@\\s*.*$", "", teams)
    home  <- sub("^.*@\\s*",      "", teams)
    
    # infer opponent from the rows in this game (based on PitcherTeam)
    d <- rv$df %>%
      dplyr::filter(
        CustomGameID == s,
        Pitcher == input$aar_pitcher,
        !(is_bullpen %in% TRUE)
      )
    my_team <- dplyr::first(stats::na.omit(d_rows$PitcherTeam))
    opp <- if (!is.na(my_team) && nzchar(my_team)) {
      if (toupper(my_team) == toupper(home)) away else home
    } else {
      paste(away, "at", home)
    }
    
    list(date = dt, away = away, home = home, opp_guess = opp)
  })
  
  # Inline preview (renders PDF, rasterizes page 1 if 'pdftools' is available)
  output$aar_preview <- renderImage({
    # Always return a PNG even if something errors
    make_error_png <- function(msg) {
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 1700, height = 2800, res = 200)
      par(mar = c(0,0,0,0))
      plot.new()
      text(0.5, 0.5, paste("AAR PREVIEW ERROR:\n\n", msg), cex = 1.2)
      dev.off()
      list(src = tmp, contentType = "image/png", width = "100%")
    }
    
    tryCatch({
      gp <- aar_game_data()
      sp <- aar_season_data()
      
      hand  <- guess_throw_hand(input$aar_pitcher, gp)
      slope <- movement_line_slope(hand, gp)
      deg   <- if (is.finite(slope)) atan(-slope) * 180 / pi else NA_real_
      
      meta  <- aar_selected_game_meta()
      gdate <- meta$date
      
      # fallback only if game_id date parse fails (use MIN date in gp, never "last game")
      if (is.na(gdate)) {
        gd_vec <- parse_date_any(unique(gp$GameDate))
        gdate  <- suppressWarnings(min(gd_vec, na.rm = TRUE))
        if (!is.finite(gdate)) gdate <- as.Date(NA)
      }
      
      # ---- FORCE the selected game date into the game dataset (prevents "last game" bleed) ----
      for (nm in intersect(c("GameDate","Date","PitchDate","Game_Date","UTCDate","LocalDate"), names(gp))) {
        gp[[nm]] <- as.Date(gdate)
      }
      
      # Filter season rows by the SAME SeasonTag as the selected game (only if available)
      # Filter season rows by the SAME SeasonTag as the selected game (only if available)
      info <- aar_season_info()
      
      # robustly locate season column (SeasonTag/SeasonGroup/etc.)
      season_col <- {
        nms <- names(sp)
        cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
        idx <- which(tolower(nms) %in% tolower(cands))
        if (length(idx)) nms[idx[1]] else NULL
      }
      
      if (!is.na(info$tag) && nzchar(info$tag) && !is.null(season_col)) {
        sp <- sp %>% dplyr::filter(.data[[season_col]] == info$tag)
      }
      
      
      tmp_pdf <- tempfile(fileext = ".pdf")
      on.exit(unlink(tmp_pdf), add = TRUE)
      render_AAR_pdf(
        game_p           = gp,
        season_p         = sp,
        pitcher_name     = input$aar_pitcher,
        game_date        = gdate,
        opponent         = input$aar_opp %||% "",
        outfile          = tmp_pdf,
        arm_angle_deg    = if (is.finite(deg)) deg else NULL,
        season_col_label = info$label
      )
      tmp_png <- render_pdf_page_png(tmp_pdf, page = 1L, dpi = 160)
      
      list(src = tmp_png, contentType = "image/png", width = "100%")
    }, error = function(e) {
      warning("[AAR preview] ", conditionMessage(e))
      make_error_png(conditionMessage(e))
    })
  }, deleteFile = TRUE)

  output$aar_preview_page2 <- renderImage({
    make_error_png <- function(msg) {
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 1700, height = 2800, res = 200)
      par(mar = c(0,0,0,0))
      plot.new()
      text(0.5, 0.5, paste("AAR PREVIEW (PAGE 2) ERROR:\n\n", msg), cex = 1.2)
      dev.off()
      list(src = tmp, contentType = "image/png", width = "100%")
    }
    
    tryCatch({
      gp <- aar_game_data()
      
      meta  <- aar_selected_game_meta()
      gdate <- meta$date
      
      if (is.na(gdate)) {
        gd_vec <- parse_date_any(unique(gp$GameDate))
        gdate  <- suppressWarnings(min(gd_vec, na.rm = TRUE))
        if (!is.finite(gdate)) gdate <- as.Date(NA)
      }
      for (nm in intersect(c("GameDate","Date","PitchDate","Game_Date","UTCDate","LocalDate"), names(gp))) {
        gp[[nm]] <- as.Date(gdate)
      }
      
      p <- compose_AAR_pa_grid_plot(game_p = gp)
      tmp_png <- render_plot_pdf_preview(p, width = 8.5, height = 14, dpi = 160)
      
      list(src = tmp_png, contentType = "image/png", width = "100%")
    }, error = function(e) {
      warning("[AAR preview page2] ", conditionMessage(e))
      make_error_png(conditionMessage(e))
    })
  }, deleteFile = TRUE)
  
  # Download handler
  output$aar_pdf <- output$aar_dl <- downloadHandler(
    filename = function() {
      paste0("AAR_", gsub("\\s+","_", input$aar_pitcher), "_",
             gsub("[^A-Za-z0-9_]+","_", input$aar_game), ".pdf")
    },
    content = function(file) {
      tryCatch({
        gp <- aar_game_data()
        sp <- aar_season_data()
        
        hand <- guess_throw_hand(input$aar_pitcher, gp)
        s    <- movement_line_slope(hand, gp)
        deg  <- if (is.finite(s)) atan(-s) * 180 / pi else NA_real_
        
        meta  <- aar_selected_game_meta()
        gdate <- meta$date
        
        # fallback only if game_id date parse fails (use MIN date in gp)
        if (is.na(gdate)) {
          gd_vec <- parse_date_any(unique(gp$GameDate))
          gdate  <- suppressWarnings(min(gd_vec, na.rm = TRUE))
          if (!is.finite(gdate)) gdate <- as.Date(NA)
        }
        # ---- FORCE the selected game date into the game dataset (prevents "last game" bleed) ----
        for (nm in intersect(c("GameDate","Date","PitchDate","Game_Date","UTCDate","LocalDate"), names(gp))) {
          gp[[nm]] <- as.Date(gdate)
        }
        
        info <- aar_season_info()
        
        # robustly locate season column (SeasonTag/SeasonGroup/etc.)
        season_col <- {
          nms <- names(sp)
          cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
          idx <- which(tolower(nms) %in% tolower(cands))
          if (length(idx)) nms[idx[1]] else NULL
        }
        
        if (!is.na(info$tag) && nzchar(info$tag) && !is.null(season_col)) {
          sp <- sp %>% dplyr::filter(.data[[season_col]] == info$tag)
        }
        
        render_AAR_pdf(
          game_p           = gp,
          season_p         = sp,
          pitcher_name     = input$aar_pitcher,
          game_date        = gdate,
          opponent         = input$aar_opp %||% "",
          outfile          = file,
          arm_angle_deg    = if (is.finite(deg)) deg else NULL,
          season_col_label = info$label
        )
        
      }, error = function(e) {
        msg <- conditionMessage(e)
        warning("[AAR download] ", msg)
        
        # Always write *something* to 'file' so the browser downloads it
        pdf(file, width = 8.5, height = 14)
        par(mar = c(0,0,0,0))
        plot.new()
        text(0.5, 0.5, paste("AAR DOWNLOAD ERROR:\n\n", msg), cex = 1.1)
        dev.off()
      })
    }
  )

  aar_recent_reports <- reactive({
    d <- rv$df
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    if ("is_bullpen" %in% names(d)) d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))
    if ("PitchType" %in% names(d)) d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    sg <- aar_season_group_values()
    season_col <- if ("SeasonTag" %in% names(d)) "SeasonTag" else if ("SeasonGroup" %in% names(d)) "SeasonGroup" else NULL
    if (!is.null(season_col) && length(sg)) d <- d %>% dplyr::filter(.data[[season_col]] %in% sg)
    get_date_vec <- function(df) {
      out <- rep(as.Date(NA), nrow(df))
      if ("GameDate" %in% names(df)) out <- dplyr::coalesce(out, parse_date_any(df$GameDate))
      if ("Date" %in% names(df)) out <- dplyr::coalesce(out, parse_date_any(df$Date))
      if ("PitchDate" %in% names(df)) out <- dplyr::coalesce(out, parse_date_any(df$PitchDate))
      if ("CustomGameID" %in% names(df)) out <- dplyr::coalesce(out, parse_gameid_date(df$CustomGameID))
      out
    }
    d$.aar_date <- get_date_vec(d)
    d %>%
      dplyr::filter(!is.na(Pitcher), nzchar(as.character(Pitcher)), !is.na(CustomGameID), nzchar(as.character(CustomGameID))) %>%
      dplyr::group_by(Pitcher, CustomGameID) %>%
      dplyr::summarise(
        GameDate = {
          v <- .aar_date[!is.na(.aar_date)]
          if (length(v)) min(v) else as.Date(NA)
        },
        Pitches = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(GameDate), Pitcher) %>%
      dplyr::slice_head(n = 25) %>%
      dplyr::mutate(row_id = dplyr::row_number())
  })

  build_aar_payload <- function(pitcher, game_id, opponent = "") {
    gp <- rv$df %>%
      dplyr::filter(Pitcher == pitcher, CustomGameID == game_id, !(is_bullpen %in% TRUE))
    if ("pitch_uid" %in% names(gp)) gp <- gp %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id" %in% names(gp)) gp <- gp %>% dplyr::distinct(row_id, .keep_all = TRUE)
    gp <- std_aar_cols(gp)

    sp <- rv$df %>%
      dplyr::filter(Pitcher == pitcher, !(is_bullpen %in% TRUE))
    if ("pitch_uid" %in% names(sp)) sp <- sp %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id" %in% names(sp)) sp <- sp %>% dplyr::distinct(row_id, .keep_all = TRUE)
    sp <- std_aar_cols(sp)

    gdate <- parse_gameid_date(game_id)
    if (is.na(gdate)) gdate <- game_date_from_rows(gp)
    if (is.na(gdate)) {
      gd_vec <- parse_date_any(unique(gp$GameDate))
      gdate <- suppressWarnings(min(gd_vec, na.rm = TRUE))
      if (!is.finite(gdate)) gdate <- as.Date(NA)
    }
    for (nm in intersect(c("GameDate","Date","PitchDate","Game_Date","UTCDate","LocalDate"), names(gp))) {
      gp[[nm]] <- as.Date(gdate)
    }

    tag <- season_tag_for_game(game_id, rv$df)
    label <- if (!is.na(tag) && nzchar(tag)) season_label_from_tag(tag) else "Season"
    season_col <- {
      nms <- names(sp)
      cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
      idx <- which(tolower(nms) %in% tolower(cands))
      if (length(idx)) nms[idx[1]] else NULL
    }
    if (!is.na(tag) && nzchar(tag) && !is.null(season_col)) {
      sp <- sp %>% dplyr::filter(.data[[season_col]] == tag)
    }

    hand <- guess_throw_hand(pitcher, gp)
    slope <- movement_line_slope(hand, gp)
    deg <- if (is.finite(slope)) atan(-slope) * 180 / pi else NA_real_
    date_str <- if (!is.na(gdate)) format(gdate, "%B %d, %Y") else as.character(game_id)
    game_label <- if (nzchar(opponent)) paste(date_str, "-", opponent) else date_str

    list(gp = gp, sp = sp, gdate = gdate, game_label = game_label, deg = deg, season_label = label)
  }

  render_aar_pdf_for <- function(file, pitcher, game_id, opponent = "") {
    payload <- build_aar_payload(pitcher, game_id, opponent)
    render_AAR_pdf(
      game_p = payload$gp,
      season_p = payload$sp,
      pitcher_name = pitcher,
      game_date = payload$gdate,
      opponent = opponent,
      outfile = file,
      arm_angle_deg = if (is.finite(payload$deg)) payload$deg else NULL,
      season_col_label = payload$season_label
    )
  }

  aar_recent_preview <- reactiveVal(NULL)

  output$aar_recent_list_ui <- renderUI({
    rows <- aar_recent_reports()
    if (is.null(rows) || !nrow(rows)) return(div("No AARs available for the selected season filters."))
    tagList(lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      div(
        style = "display:grid; grid-template-columns: 1.5fr 0.8fr 0.7fr 0.8fr; gap:10px; align-items:center; padding:8px 10px; border-bottom:1px solid #e5e5e5;",
        div(style = "font-weight:700;", name_display(r$Pitcher)),
        div(ifelse(is.na(r$GameDate), "Unknown date", format(r$GameDate, "%B %d, %Y"))),
        actionButton(paste0("aar_recent_preview_", i), "Preview AAR", class = "btn-sm"),
        downloadButton(paste0("aar_recent_download_", i), "Download AAR", class = "btn-sm")
      )
    }))
  })

  for (i in seq_len(25)) {
    local({
      idx <- i
        observeEvent(input[[paste0("aar_recent_preview_", idx)]], {
          rows_now <- aar_recent_reports()
          req(nrow(rows_now) >= idx)
          aar_recent_preview(rows_now[idx, ])
          showModal(modalDialog(
            title = paste("AAR Preview:", name_display(rows_now$Pitcher[idx])),
            div(
              class = "base-report-preview base-report-preview-portrait",
              imageOutput("aar_recent_preview_image", height = "2400px")
            ),
            easyClose = TRUE,
            size = "l",
            footer = modalButton("Close")
          ))
        }, ignoreInit = TRUE)
        output[[paste0("aar_recent_download_", idx)]] <- downloadHandler(
          filename = function() {
            rows_now <- aar_recent_reports()
            if (!nrow(rows_now) || nrow(rows_now) < idx) return("AAR.pdf")
            paste0("AAR_", gsub("\\s+","_", rows_now$Pitcher[idx]), "_",
                   ifelse(is.na(rows_now$GameDate[idx]), "undated", format(rows_now$GameDate[idx], "%Y%m%d")), ".pdf")
          },
          content = function(file) {
            rows_now <- aar_recent_reports()
            req(nrow(rows_now) >= idx)
            render_aar_pdf_for(file, rows_now$Pitcher[idx], rows_now$CustomGameID[idx], "")
          }
        )
    })
  }

  output$aar_recent_preview_image <- renderImage({
    sel <- aar_recent_preview()
    req(!is.null(sel), nrow(sel) == 1)
    make_error_png <- function(msg) {
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 1700, height = 2800, res = 200)
      par(mar = c(0,0,0,0))
      plot.new()
      text(0.5, 0.5, paste("AAR PREVIEW ERROR:\n\n", msg), cex = 1.2)
      dev.off()
      list(src = tmp, contentType = "image/png", width = "100%")
    }
    tryCatch({
      tmp_pdf <- tempfile(fileext = ".pdf")
      on.exit(unlink(tmp_pdf), add = TRUE)
      render_aar_pdf_for(tmp_pdf, sel$Pitcher[[1]], sel$CustomGameID[[1]], "")
      tmp_png <- render_pdf_page_png(tmp_pdf, page = 1L, dpi = 160)
      list(src = tmp_png, contentType = "image/png", width = "100%")
    }, error = function(e) {
      warning("[AAR recent preview] ", conditionMessage(e))
      make_error_png(conditionMessage(e))
    })
  }, deleteFile = TRUE)

  # --- Team Report helpers ---
  team_season_info <- reactive({
    shiny::req(input$team_game)
    
    season_cols <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
    if (is.null(rv$df) || !nrow(rv$df) || !any(season_cols %in% names(rv$df))) {
      return(list(tag = NA_character_, label = "Season"))
    }
    
    tag <- season_tag_for_game(input$team_game, rv$df)
    if (is.na(tag) || !nzchar(tag)) {
      return(list(tag = NA_character_, label = "Season"))
    }
    
    list(tag = as.character(tag), label = season_label_from_tag(tag))
  })
  
  team_game_data <- reactive({
    req(input$team_game)
    d <- rv$df %>%
      dplyr::filter(CustomGameID == input$team_game, !(is_bullpen %in% TRUE))
    
    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }
    
    d <- prefer_team_or_portal(d, "PitcherTeam")
    
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    validate(need(nrow(d) > 0, "No data for that game."))
    d
  })
  
  team_season_data <- reactive({
    req(input$team_game)
    d <- rv$df %>% dplyr::filter(!(is_bullpen %in% TRUE))
    
    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }
    
    d <- prefer_team_or_portal(d, "PitcherTeam")
    
    info <- team_season_info()
    season_col <- {
      nms <- names(d)
      cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
      idx <- which(tolower(nms) %in% tolower(cands))
      if (length(idx)) nms[idx[1]] else NULL
    }
    
    if (!is.na(info$tag) && nzchar(info$tag) && !is.null(season_col)) {
      d <- d %>% dplyr::filter(.data[[season_col]] == info$tag)
    }
    
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    d
  })
  
  output$team_report_preview <- renderImage({
    make_error_png <- function(msg) {
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 2200, height = 1700, res = 200)
      par(mar = c(0,0,0,0))
      plot.new()
      text(0.5, 0.5, paste("TEAM REPORT PREVIEW ERROR:\n\n", msg), cex = 1.2)
      dev.off()
      list(src = tmp, contentType = "image/png", width = "100%")
    }
    
    tryCatch({
      gp <- team_game_data()
      sp <- team_season_data()
      info <- team_season_info()
      
      p <- compose_team_report_plot(
        game_p           = gp,
        season_p         = sp,
        game_id          = input$team_game,
        season_col_label = info$label
      )
      
      tmp_png <- tempfile(fileext = ".png")
      
      if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_png(tmp_png, width = 2200, height = 1700, units = "px", res = 200, background = "white")
        print(p)
        dev.off()
      } else {
        png(tmp_png, width = 2200, height = 1700, res = 200)
        print(p)
        dev.off()
      }
      
      list(src = tmp_png, contentType = "image/png", width = "100%")
    }, error = function(e) {
      warning("[Team Report preview] ", conditionMessage(e))
      make_error_png(conditionMessage(e))
    })
  }, deleteFile = TRUE)
  
  output$team_report_pdf <- output$team_report_dl <- downloadHandler(
    filename = function() {
      gp <- team_game_data()
      date_val <- as.Date(NA)
      if ("GameDate" %in% names(gp)) {
        gd <- parse_date_any(gp$GameDate)
        gd <- gd[!is.na(gd)]
        if (length(gd)) date_val <- gd[1]
      }
      game_suffix <- ""
      if (!is.null(input$team_game)) {
        game_suffix <- stringr::str_extract(as.character(input$team_game), "\\(G\\d+\\)")
        if (is.na(game_suffix) || !nzchar(game_suffix)) {
          game_num <- stringr::str_match(as.character(input$team_game), "-(\\d+)$")[, 2]
          game_suffix <- if (!is.na(game_num) && nzchar(game_num)) paste0("(G", game_num, ")") else ""
        }
      }
      if (is.na(date_val) && !is.null(input$team_game)) {
        date_val <- parse_date_any(input$team_game)[1]
      }
      date_str <- if (!is.na(date_val)) format(date_val, "%Y-%m-%d") else "UnknownDate"
      paste0("Bobcat_Staff_", date_str, if (nzchar(game_suffix)) paste0("_", gsub("[()]", "", game_suffix)) else "", "_AAR.pdf")
    },
    content = function(file) {
      tryCatch({
        gp <- team_game_data()
        sp <- team_season_data()
        info <- team_season_info()
        
        p <- compose_team_report_plot(
          game_p           = gp,
          season_p         = sp,
          game_id          = input$team_game,
          season_col_label = info$label
        )
        
        grDevices::pdf(file, width = 11, height = 8.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(p)
      }, error = function(e) {
        msg <- conditionMessage(e)
        warning("[Team Report download] ", msg)
        grDevices::pdf(file, width = 11, height = 8.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        par(mar = c(0,0,0,0))
        plot.new()
        text(0.5, 0.5, paste("TEAM REPORT DOWNLOAD ERROR:\n\n", msg), cex = 1.1)
      })
    }
  )
  
  # --- AAR dropdown wiring ---
  observeEvent(input$aar_pitcher, {
    req(input$aar_pitcher)
    games <- txst_df %>%
      dplyr::filter(Pitcher == input$aar_pitcher) %>%
      dplyr::pull(CustomGameID) %>% unique()
    games <- order_game_ids_desc(games, game_dates)
    
    shinyWidgets::updatePickerInput(session, "aar_game",
                                    choices  = games,
                                    selected = if (length(games)) games[[1]] else character(0)
    )
  }, ignoreInit = TRUE)
  
  
  safe_dt <- function(x) {
    if (is.null(x)) {
      x <- data.frame(Message = "No data", stringsAsFactors = FALSE)
    } else if (!is.data.frame(x)) {
      x <- tryCatch(as.data.frame(x), error = function(e) NULL)
      if (is.null(x)) x <- data.frame(Message = "No data", stringsAsFactors = FALSE)
    }
    DT::datatable(x, rownames = FALSE, options = list(dom='t', paging=FALSE, ordering=FALSE))
  }
  # === Helpers for AAR BB from KorBB ===
  .find_korbb_col <- function(df) {
    if (is.null(df) || !length(names(df))) return(NULL)
    nms <- trimws(names(df))
    # exact match first
    idx <- which(tolower(nms) == "korbb")
    if (!length(idx)) {
      # allow joins like KorBB.x / KorBB.y / whitespace variants
      idx <- grep("korbb", tolower(nms), fixed = TRUE)
    }
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  .compute_bb_from_korbb <- function(df) {
    col <- .find_korbb_col(df)
    if (is.null(col)) {
      warning("[AAR] No KorBB-like column found in AAR data.")
      return(NA_integer_)
    }
    v <- df[[col]]
    if (is.factor(v)) v <- as.character(v)
    v <- trimws(v)
    
    # Accept both unintentional and intentional tokens
    walk_mask <- grepl("(?i)^(BB|IBB)$", v)
    
    # Use a PA identifier if present; include PA_ID as a candidate
    pa_key <- intersect(
      c("PA_ID","PAId","PlateAppearanceId","PlateAppearanceID","PAofInning","PA_Number","PAIndex","PA","Pa"),
      names(df)
    )[1]
    
    if (!is.na(pa_key)) {
      n <- dplyr::tibble(pa = df[[pa_key]], walk = walk_mask) |>
        dplyr::group_by(pa) |>
        dplyr::summarise(any_walk = any(walk, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(n = sum(any_walk, na.rm = TRUE)) |>
        dplyr::pull(n) |>
        as.integer()
      return(n)
    } else {
      n <- sum(walk_mask, na.rm = TRUE)
      return(as.integer(n))
    }
  }
  
  
  aar_statline_bits <- function(d){
    if (exists("ensure_process_cols", mode = "function")) {
      d <- ensure_process_cols(d)
    }
    
    # Last pitch of each PA
    pa_last <- d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    
    # Last non-empty KorBB / PlayResult per PA (for HBP detection)
    pa_end <- d %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::summarise(
        KorBB_last = safe_last_nonempty(KorBB),
        PR_last    = safe_last_nonempty(PlayResult),
        any_hbp    = if ("HBP" %in% names(d)) any(HBP, na.rm = TRUE) else FALSE,
        .groups = "drop"
      )
    
    to_chr <- function(x) as.character(x)
    pr <- if ("PlayResult" %in% names(pa_last)) to_chr(pa_last$PlayResult) else rep(NA_character_, nrow(pa_last))
    kb <- if ("KorBB"      %in% names(pa_last)) to_chr(pa_last$KorBB)      else rep(NA_character_, nrow(pa_last))
    
    is_k   <- grepl("(?i)strike.?out|\\bK\\b", pr) | grepl("(?i)K|Strikeout", kb)
    is_bb  <- grepl("(?i)\\bwalk\\b|\\bbb\\b", kb) | grepl("(?i)walk", pr)
    is_ibb <- grepl("(?i)intentional|\\bibb\\b", kb) | grepl("(?i)intentional", pr)
    
    is_hr  <- grepl("(?i)home ?run|\\bHR\\b", pr)
    is_3b  <- grepl("(?i)triple", pr)
    is_2b  <- grepl("(?i)double(?!\\s*play)", pr, perl = TRUE)
    is_1b  <- grepl("(?i)single", pr)
    
    outs_play <- rep(0L, nrow(pa_last))
    if ("OutsOnPlay" %in% names(pa_last)) {
      outs_play <- suppressWarnings(as.integer(pa_last$OutsOnPlay)); outs_play[!is.finite(outs_play)] <- 0L
    }
    outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
    outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
    outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !is_k, pmax(outs_play, 1L), outs_play)
    
    outs_total <- sum(outs_play, na.rm = TRUE) + sum(is_k, na.rm = TRUE)
    ip_chr <- sprintf("%d.%d", floor(outs_total/3), outs_total %% 3)
    
    H  <- sum(is_1b | is_2b | is_3b | is_hr, na.rm = TRUE)
    BB <- sum(is_bb & !is_ibb, na.rm = TRUE)
    SO <- sum(is_k, na.rm = TRUE)
    P  <- nrow(d)
    
    # HBP detection (KorBB / PlayResult / HBP flag, counted once per PA)
    kb_last_trim <- tolower(trimws(as.character(pa_end$KorBB_last)))
    pr_last_trim <- tolower(trimws(as.character(pa_end$PR_last)))
    kor_is_hbp <- grepl("\\bhbp\\b|hit by pitch", kb_last_trim)
    pr_is_hbp  <- grepl("\\bhbp\\b|hit by pitch", pr_last_trim)
    HBP <- sum(kor_is_hbp | pr_is_hbp | pa_end$any_hbp, na.rm = TRUE)
    
    PA <- compute_pa_count(d)
    Barrels <- sum(compute_barrel_flag_leaderboard(d), na.rm = TRUE)
    
    bits <- list(
      Pitches = P,
      PA = PA,
      IP = ip_chr,
      SO = SO,
      H = H,
      Barrels = Barrels,
      BB = BB,
      HBP = HBP,
      ER = NA_integer_
    )
    
    bb_korbb <- .compute_bb_from_korbb(d)
    if (!is.na(bb_korbb)) bits$BB <- bb_korbb
    
    bits
  }
  
  
  # ---- DT helpers for diverging red-white-green conditional formatting ----
  mk_diverging <- function(x, n_each = 64) {
    x <- suppressWarnings(as.numeric(gsub("%", "", x)))
    x <- x[is.finite(x)]
    if (!length(x)) return(NULL)
    
    p1  <- as.numeric(stats::quantile(x, 0.01, na.rm = TRUE, names = FALSE))
    p99 <- as.numeric(stats::quantile(x, 0.99, na.rm = TRUE, names = FALSE))
    m   <- mean(x, na.rm = TRUE)
    
    if (!is.finite(p1) || !is.finite(p99) || !is.finite(m) || p1 == p99) return(NULL)
    
    below <- seq(p1, m, length.out = n_each)
    above <- seq(m, p99, length.out = n_each)
    
    # Build palettes (dark red -> light red -> white -> light green -> dark green)
    reds   <- colorRampPalette(c("#7f0000", "#ff9999", "#ffffff"))(length(below))
    greens <- colorRampPalette(c("#ffffff", "#a1d99b", "#00441b"))(length(above))
    
    breaks <- unique(c(below, above))[-1]    # styleInterval: len(colors) = len(breaks)+1
    cols   <- c(reds, greens[-1])            # drop duplicated white at mean
    
    list(breaks = breaks, colors = cols)
  }
  
  apply_diverging <- function(dt, data, cols) {
    for (col in cols) {
      if (!col %in% colnames(data)) next
      map <- mk_diverging(data[[col]])
      if (is.null(map)) next
      dt <- DT::formatStyle(
        dt, col,
        backgroundColor = DT::styleInterval(map$breaks, map$colors)
      )
    }
    dt
  }
  
  # Filtered data used across panels
  dataFilter <- reactive({
    req(input$PitcherInput)
    
    if (isTRUE(input$Bullpens)) {
      d <- bullpens_df %>% dplyr::filter(Pitcher == input$PitcherInput)
      gids <- if (length(input$GameInput)) input$GameInput else bp_game_ids
      if (length(gids)) d <- d %>% dplyr::filter(CustomGameID_BP %in% gids)
      d$CustomGameID <- d$CustomGameID_BP
    } else {
      sel_hands <- if (length(input$BatterHand)) input$BatterHand else c("L","R")
      sel_games <- input$GameInput
      if (is.null(sel_games) || !length(sel_games)) {
        sel_games <- rv$df %>%
          dplyr::filter(Pitcher == input$PitcherInput, !(is_bullpen %in% TRUE)) %>%
          dplyr::pull(CustomGameID) %>% unique()
      }
      d <- rv$df %>%
        dplyr::filter(
          Pitcher == input$PitcherInput,
          !(is_bullpen %in% TRUE),
          CustomGameID %in% sel_games,
          !(as.character(PitchType) %in% "Bad data")
        ) %>%
        dplyr::mutate(
          BatterSideStd = dplyr::case_when(
            BatterSide %in% c("L","Left","LHH","LH") ~ "L",
            BatterSide %in% c("R","Right","RHH","RH") ~ "R",
            TRUE ~ as.character(BatterSide)
          )
        ) %>%
        dplyr::filter(BatterSideStd %in% sel_hands)
      
      d <- prefer_team_or_portal(d, "PitcherTeam")
    }
    
    # ---- Deduplicate pitches once here (fixes double counts in metrics/movement tables) ----
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    d
  })

  # Filtered data used across panels (same as dataFilter, but ignores BatterHand for split tables)
  dataFilter_allhands <- reactive({
    req(input$PitcherInput)
    
    if (isTRUE(input$Bullpens)) {
      d <- bullpens_df %>% dplyr::filter(Pitcher == input$PitcherInput)
      gids <- if (length(input$GameInput)) input$GameInput else bp_game_ids
      if (length(gids)) d <- d %>% dplyr::filter(CustomGameID_BP %in% gids)
      d$CustomGameID <- d$CustomGameID_BP
    } else {
      sel_games <- input$GameInput
      if (is.null(sel_games) || !length(sel_games)) {
        sel_games <- rv$df %>%
          dplyr::filter(Pitcher == input$PitcherInput, !(is_bullpen %in% TRUE)) %>%
          dplyr::pull(CustomGameID) %>% unique()
      }
      d <- rv$df %>%
        dplyr::filter(
          Pitcher == input$PitcherInput,
          !(is_bullpen %in% TRUE),
          CustomGameID %in% sel_games,
          !(as.character(PitchType) %in% "Bad data")
        ) %>%
        dplyr::mutate(
          BatterSideStd = dplyr::case_when(
            BatterSide %in% c("L","Left","LHH","LH") ~ "L",
            BatterSide %in% c("R","Right","RHH","RH") ~ "R",
            TRUE ~ as.character(BatterSide)
          )
        )
      
      d <- prefer_team_or_portal(d, "PitcherTeam")
    }
    
    # ---- Deduplicate pitches once here (fixes double counts in metrics/movement tables) ----
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    d
  })

  # ---- xRV helpers ----
  xrv_scale_params <- reactive({
    list(mu = XRV_D1_MEAN, sd = XRV_D1_SD)
  })

  xrv_prepare_base <- function(d, params) {
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    if (!"PitchType" %in% names(d) && "TaggedPitchType" %in% names(d)) d$PitchType <- d$TaggedPitchType
    d <- d %>%
      dplyr::mutate(
        PitchType = trimws(as.character(PitchType)),
        PitchType = dplyr::na_if(PitchType, ""),
        PitchType_plot = dplyr::if_else(
          is.na(PitchType) | !(PitchType %in% names(pitch_colors)),
          "Undefined",
          PitchType
        ),
        BatterSideStd = dplyr::case_when(
          BatterSide %in% c("L","Left","LHH","LH") ~ "L",
          BatterSide %in% c("R","Right","RHH","RH","Rigjht") ~ "R",
          TRUE ~ as.character(BatterSide)
        ),
        xrv = suppressWarnings(as.numeric(xrv)),
        xrv_plus = xrv_plus_from(xrv, params$mu, params$sd)
      )
    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }
    d <- compute_called_stuff(d)
    d
  }

  xrv_game_info <- function(d) {
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    d <- tibble::as_tibble(d)
    if ("CustomGameID" %in% names(d)) {
      d$GameID <- as.character(d$CustomGameID)
    } else if ("GameID" %in% names(d)) {
      d$GameID <- as.character(d$GameID)
    } else if ("GameUID" %in% names(d)) {
      d$GameID <- as.character(d$GameUID)
    } else {
      d$GameID <- NA_character_
    }

    gd <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("CustomGameID" %in% names(d)) {
      parse_gameid_date(d$CustomGameID)
    } else {
      as.Date(NA)
    }
    d$GameDate <- gd

    safe_min_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) min(x) else as.Date(NA)
    }

    d %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        GameDate = safe_min_date(GameDate),
        .order   = min(.row, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::if_else(is.na(GameDate), 1L, 0L), GameDate, .order) %>%
      dplyr::mutate(
        GameIndex = dplyr::row_number(),
        GameLabel = ifelse(!is.na(GameDate), format(GameDate, "%m/%d/%y"), as.character(GameID))
      )
  }

  xrv_header_label <- function(name, season_labels) {
    if (!length(season_labels)) {
      return(paste0(name, " — All Seasons"))
    }
    lab <- if (length(season_labels) == 1) {
      season_labels[[1]]
    } else {
      paste(season_labels, collapse = ", ")
    }
    paste0(name, " — ", lab)
  }

  xrv_summary_stats_df <- function(d) {
    if (is.null(d) || !nrow(d)) return(data.frame(Status = "No data"))
    p <- prepare_aar_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    if (!nrow(p)) return(data.frame(Status = "No data"))

    # --- helper: EV/LA detection (match performance/leaderboard logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la  = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,
        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        ))
      )

    # Pitch-level rates
    n_all <- nrow(p_flags)
    strike_pct <- sdiv(sum(p_flags$.strike, na.rm = TRUE), n_all)
    barrel_pct <- sdiv(
      sum(p_flags$.barrel & p_flags$.barrel_ok, na.rm = TRUE),
      sum(p_flags$.bip_evla & p_flags$.barrel_ok, na.rm = TRUE)
    )
    gb_pct <- sdiv(sum(p_flags$.gb, na.rm = TRUE), sum(p_flags$.bip_la, na.rm = TRUE))

    # PA-level outcomes
    pa_last <- get_pa_last(p)
    if (!nrow(pa_last)) return(data.frame(Status = "No data"))
    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)

    outs_play <- rep(0L, nrow(pa_last))
    if ("OutsOnPlay" %in% names(pa_last)) {
      outs_play <- suppressWarnings(as.integer(pa_last$OutsOnPlay))
      outs_play[!is.finite(outs_play)] <- 0L
    }
    outs_play <- ifelse(grepl("triple\\s*play", pr, ignore.case = TRUE), 3L, outs_play)
    outs_play <- ifelse(grepl("double\\s*play", pr, ignore.case = TRUE), 2L, outs_play)
    outs_play <- ifelse(grepl("\\bout\\b", pr, ignore.case = TRUE) & !outc$K, pmax(outs_play, 1L), outs_play)

    outs_total <- sum(outs_play, na.rm = TRUE) + sum(as.integer(outc$K), na.rm = TRUE)
    ip_chr <- sprintf("%d.%d", floor(outs_total / 3), outs_total %% 3)
    ip_val <- outs_total / 3

    PA <- nrow(pa_last)
    K  <- sum(outc$K,  na.rm = TRUE)
    BB <- sum(any_walk, na.rm = TRUE)
    H  <- sum(outc$X1B, na.rm = TRUE) + sum(outc$X2B, na.rm = TRUE) +
      sum(outc$X3B, na.rm = TRUE) + sum(outc$HR, na.rm = TRUE)

    woba <- compute_woba_grouped(pa_last, rep("Total", nrow(pa_last)))$wOBA[1]

    # E&A%: every PA is in the denominator; numerator is Ahead or eligible Early action
    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID")
    ea <- {
      calc_ea_rate_from_pa(ea_pa)
    }

    game_ids <- NULL
    if ("CustomGameID" %in% names(p)) game_ids <- as.character(p$CustomGameID)
    if (is.null(game_ids) && "GameID" %in% names(p)) game_ids <- as.character(p$GameID)
    if (is.null(game_ids) && "GameUID" %in% names(p)) game_ids <- as.character(p$GameUID)
    if (is.null(game_ids) && "Date" %in% names(p)) game_ids <- as.character(parse_date_any(p$Date))
    if (is.null(game_ids)) game_ids <- rep(NA_character_, nrow(p))
    G <- length(unique(game_ids[!is.na(game_ids) & nzchar(game_ids)]))

    fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "NA")

    out <- data.frame(
      G = G,
      IP = ip_chr,
      WHIP = fmt_num(sdiv(BB + H, ip_val), 2),
      wOBA = fmt_num(woba, 3),
      `K%` = fmt_pct0(sdiv(K, PA)),
      `BB%` = fmt_pct0(sdiv(BB, PA)),
      `Barrel%` = fmt_pct0(barrel_pct),
      `GB%` = fmt_pct0(gb_pct),
      `Strike%` = fmt_pct0(strike_pct),
      `E&A%` = fmt_pct0(ea),
      check.names = FALSE
    )
    if (nrow(out) > 1) out <- out[1, , drop = FALSE]
    out
  }

  xrv_pitch_table_df <- function(d, params) {
    if (is.null(d) || !nrow(d)) return(data.frame(Status = "No data"))
    p <- prepare_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    p$PitchType <- if ("PitchType" %in% names(p)) as.character(p$PitchType) else as.character(p$TaggedPitchType)
    p$PitchType <- trimws(p$PitchType)
    p$PitchType[p$PitchType == ""] <- NA_character_
    p <- p %>% dplyr::filter(!is.na(PitchType))

    p$RelSpeed         <- suppressWarnings(as.numeric(p$RelSpeed))
    p$InducedVertBreak <- suppressWarnings(as.numeric(p$InducedVertBreak))
    p$HorzBreak        <- suppressWarnings(as.numeric(p$HorzBreak))
    p$SpinRate         <- suppressWarnings(as.numeric(p$SpinRate))
    p$VertApprAngle    <- suppressWarnings(as.numeric(p$VertApprAngle))
    p$HorzApprAngle    <- suppressWarnings(as.numeric(p$HorzApprAngle))
    p$RelHeight        <- suppressWarnings(as.numeric(p$RelHeight))
    p$RelSide          <- suppressWarnings(as.numeric(p$RelSide))
    p$Extension        <- suppressWarnings(as.numeric(p$Extension))
    p$xrv              <- suppressWarnings(as.numeric(p$xrv))
    p$xrv_plus         <- xrv_plus_from(p$xrv, params$mu, params$sd)
    p <- compute_called_stuff(p)

    evla <- resolve_ev_la_strict(p)
    p$.ev <- suppressWarnings(as.numeric(evla$ev))
    p$.la <- suppressWarnings(as.numeric(evla$la))
    p$.bip <- safe_is_bip(p$PitchCall %||% NA_character_, if ("PlayResult" %in% names(p)) p$PlayResult else NULL)
    p$.bip_evla <- p$.bip & is.finite(p$.ev) & is.finite(p$.la)
    p$.barrel <- compute_barrel_flag_leaderboard(p)

    pa_last <- get_pa_last(p)
    woba_by_pt <- compute_woba_grouped(pa_last, pa_last$PitchType)

    base <- p %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        VeloAvg = mean(RelSpeed, na.rm = TRUE),
        VeloMax = max(RelSpeed, na.rm = TRUE),
        iVB = mean(InducedVertBreak, na.rm = TRUE),
        HB  = mean(HorzBreak, na.rm = TRUE),
        Spin = mean(SpinRate, na.rm = TRUE),
        VAA  = mean(VertApprAngle, na.rm = TRUE),
        HAA  = mean(HorzApprAngle, na.rm = TRUE),
        RelHt = mean(RelHeight, na.rm = TRUE),
        RelSide = mean(RelSide, na.rm = TRUE),
        Extension = mean(Extension, na.rm = TRUE),
        xRV_plus = mean(xrv_plus, na.rm = TRUE),
        Stuff_plus = mean(stuff_plus, na.rm = TRUE),
        swings = sum(IsSwing %in% TRUE, na.rm = TRUE),
        swings_loc = sum(IsSwing %in% TRUE & !is.na(InZone), na.rm = TRUE),
        swings_in  = sum(IsSwing %in% TRUE & InZone %in% TRUE, na.rm = TRUE),
        whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
        csw = sum(PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        barrels = sum(.barrel & .bip_evla, na.rm = TRUE),
        bip_evla = sum(.bip_evla, na.rm = TRUE),
        .groups = "drop"
      )

    base <- base %>%
      dplyr::mutate(
        Usage = Pitches / sum(Pitches, na.rm = TRUE),
        CSW_pct = sdiv(csw, Pitches),
        Whiff_pct = sdiv(whiffs, swings),
        Chase_pct = sdiv(swings_loc - swings_in, swings_loc),
        Barrel_pct = sdiv(barrels, bip_evla)
      ) %>%
      dplyr::left_join(woba_by_pt, by = c("PitchType" = ".grp"))

    total <- p %>%
      dplyr::summarise(
        PitchType = "Total",
        Pitches = dplyr::n(),
        VeloAvg = mean(RelSpeed, na.rm = TRUE),
        VeloMax = max(RelSpeed, na.rm = TRUE),
        iVB = mean(InducedVertBreak, na.rm = TRUE),
        HB  = mean(HorzBreak, na.rm = TRUE),
        Spin = mean(SpinRate, na.rm = TRUE),
        VAA  = mean(VertApprAngle, na.rm = TRUE),
        HAA  = mean(HorzApprAngle, na.rm = TRUE),
        RelHt = mean(RelHeight, na.rm = TRUE),
        RelSide = mean(RelSide, na.rm = TRUE),
        Extension = mean(Extension, na.rm = TRUE),
        xRV_plus = mean(xrv_plus, na.rm = TRUE),
        Stuff_plus = mean(stuff_plus, na.rm = TRUE),
        swings = sum(IsSwing %in% TRUE, na.rm = TRUE),
        swings_loc = sum(IsSwing %in% TRUE & !is.na(InZone), na.rm = TRUE),
        swings_in  = sum(IsSwing %in% TRUE & InZone %in% TRUE, na.rm = TRUE),
        whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
        csw = sum(PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        barrels = sum(.barrel & .bip_evla, na.rm = TRUE),
        bip_evla = sum(.bip_evla, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Usage = 1,
        CSW_pct = sdiv(csw, Pitches),
        Whiff_pct = sdiv(whiffs, swings),
        Chase_pct = sdiv(swings_loc - swings_in, swings_loc),
        Barrel_pct = sdiv(barrels, bip_evla)
      )

    total$wOBA <- compute_woba_grouped(pa_last, rep("Total", nrow(pa_last)))$wOBA[1]

    out <- dplyr::bind_rows(base, total) %>%
      dplyr::mutate(.is_total = PitchType == "Total") %>%
      dplyr::arrange(.is_total, dplyr::desc(Usage)) %>%
      dplyr::select(-.is_total)

    fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "NA")
    fmt_num0 <- function(x) ifelse(is.finite(x), sprintf("%.0f", x), "NA")
    fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
    fmt_velo <- function(avg, mx) {
      if (is.finite(avg) && is.finite(mx)) return(sprintf("%.1f (%.1f)", avg, mx))
      if (is.finite(avg)) return(sprintf("%.1f", avg))
      if (is.finite(mx)) return(sprintf("(%.1f)", mx))
      "NA"
    }

    out %>%
      dplyr::transmute(
        PitchType = PitchType,
        `Pitches` = as.integer(Pitches),
        `Usage %` = fmt_pct0(Usage),
        `Avg Velocity (Max)` = mapply(fmt_velo, VeloAvg, VeloMax, SIMPLIFY = TRUE, USE.NAMES = FALSE),
        iVB = fmt_num1(iVB),
        HB  = fmt_num1(HB),
        Spin = fmt_num0(Spin),
        VAA = fmt_num1(VAA),
        HAA = fmt_num1(HAA),
        RelHt = fmt_num1(RelHt),
        RelSide = fmt_num1(RelSide),
        Extension = fmt_num1(Extension),
        `Stuff+ (avg)` = fmt_num1(Stuff_plus),
        `CSW%` = fmt_pct0(CSW_pct),
        `Whiff%` = fmt_pct0(Whiff_pct),
        `Chase%` = fmt_pct0(Chase_pct),
        `Barrel%` = fmt_pct0(Barrel_pct),
        wOBA = fmt_num3(wOBA)
      )
  }

  build_xrv_movement_plot <- function(d, title_text = "Pitch Movement", split_hand = FALSE) {
    if (is.null(d) || !nrow(d)) return(plotly::plot_ly())
    mv <- d %>%
      dplyr::mutate(
        HorzBreak = suppressWarnings(as.numeric(HorzBreak)),
        InducedVertBreak = suppressWarnings(as.numeric(InducedVertBreak)),
        RelSpeed = suppressWarnings(as.numeric(RelSpeed)),
        SpinRate = suppressWarnings(as.numeric(SpinRate)),
        stuff_plus = suppressWarnings(as.numeric(stuff_plus))
      ) %>%
      dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
    if (!nrow(mv)) return(plotly::plot_ly())

    mv$HandGroup <- dplyr::case_when(
      as.character(mv$PitcherThrows) %in% c("L","Left","LHH","LH") ~ "LHP",
      as.character(mv$PitcherThrows) %in% c("R","Right","RHH","RH") ~ "RHP",
      TRUE ~ "UNK"
    )

    hand <- NA_character_
    if ("PitcherThrows" %in% names(mv)) {
      h <- unique(toupper(na.omit(mv$PitcherThrows)))
      if (length(h) == 1) hand <- if (startsWith(h[1], "L")) "LHP" else "RHP"
    }
    slope <- if (!is.na(hand)) movement_line_slope(hand, mv) else NA_real_
    signx <- if (identical(hand, "LHP")) -1 else 1
    mv_lim <- 25

    x_end <- NA_real_; y_end <- NA_real_
    if (is.finite(slope)) {
      if (abs(slope) <= 1) {
        x_end <- mv_lim * signx
        y_end <- mv_lim * slope
      } else {
        y_end <- mv_lim * sign(slope)
        x_end <- signx * (y_end / slope)
      }
    }

    mv_avg <- mv %>%
      dplyr::group_by(PitchType_plot, HandGroup) %>%
      dplyr::summarise(
        HB = mean(HorzBreak, na.rm = TRUE),
        IVB = mean(InducedVertBreak, na.rm = TRUE),
        Velo = mean(RelSpeed, na.rm = TRUE),
        Spin = mean(SpinRate, na.rm = TRUE),
        Stuff = mean(stuff_plus, na.rm = TRUE),
        .groups = "drop"
      )

    if (!isTRUE(split_hand)) {
      mv_avg <- mv_avg %>%
        dplyr::group_by(PitchType_plot) %>%
        dplyr::summarise(
          HB = mean(HB, na.rm = TRUE),
          IVB = mean(IVB, na.rm = TRUE),
          Velo = mean(Velo, na.rm = TRUE),
          Spin = mean(Spin, na.rm = TRUE),
          Stuff = mean(Stuff, na.rm = TRUE),
          HandGroup = "ALL",
          .groups = "drop"
        )
    }

    mv_avg$label <- if (isTRUE(split_hand)) {
      sprintf(
        "%s %s Avg<br>Velo: %s<br>iVB: %s<br>HB: %s<br>Spin: %s<br>Stuff+: %s",
        mv_avg$PitchType_plot,
        mv_avg$HandGroup,
        ifelse(is.finite(mv_avg$Velo), sprintf("%.1f", mv_avg$Velo), "—"),
        ifelse(is.finite(mv_avg$IVB),  sprintf("%.1f", mv_avg$IVB),  "—"),
        ifelse(is.finite(mv_avg$HB),   sprintf("%.1f", mv_avg$HB),   "—"),
        ifelse(is.finite(mv_avg$Spin), sprintf("%.0f", mv_avg$Spin), "—"),
        ifelse(is.finite(mv_avg$Stuff),sprintf("%.1f", mv_avg$Stuff),"—")
      )
    } else {
      sprintf(
        "%s Avg<br>Velo: %s<br>iVB: %s<br>HB: %s<br>Spin: %s<br>Stuff+: %s",
        mv_avg$PitchType_plot,
        ifelse(is.finite(mv_avg$Velo), sprintf("%.1f", mv_avg$Velo), "—"),
        ifelse(is.finite(mv_avg$IVB),  sprintf("%.1f", mv_avg$IVB),  "—"),
        ifelse(is.finite(mv_avg$HB),   sprintf("%.1f", mv_avg$HB),   "—"),
        ifelse(is.finite(mv_avg$Spin), sprintf("%.0f", mv_avg$Spin), "—"),
        ifelse(is.finite(mv_avg$Stuff),sprintf("%.1f", mv_avg$Stuff),"—")
      )
    }

    p <- plotly::plot_ly()
    p <- p %>%
      add_trace(
        data = mv,
        x = ~HorzBreak, y = ~InducedVertBreak,
        type = "scatter", mode = "markers",
        color = ~PitchType_plot, colors = pitch_colors,
        marker = list(size = 8, opacity = 0.3),
        text = ~paste0(
          "HB: ", sprintf("%.1f", HorzBreak),
          "<br>iVB: ", sprintf("%.1f", InducedVertBreak),
          "<br>Stuff+: ", ifelse(is.finite(stuff_plus), sprintf("%.1f", stuff_plus), "NA")
        ),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
      )

    if (nrow(mv_avg)) {
      for (i in seq_len(nrow(mv_avg))) {
        row <- mv_avg[i, ]
        sym <- if (isTRUE(split_hand)) {
          if (row$HandGroup == "LHP") "circle-open" else if (row$HandGroup == "RHP") "circle" else "diamond"
        } else {
          "circle"
        }
        p <- p %>%
          add_trace(
            data = row,
            x = ~HB, y = ~IVB,
            type = "scatter", mode = "markers",
            color = ~PitchType_plot, colors = pitch_colors,
            marker = list(size = 16, opacity = 1, symbol = sym, line = list(color = "black", width = 0.6)),
            text = ~label,
            hoverinfo = "text",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }

    shp <- list(
      list(type = "line", x0 = -mv_lim, x1 = mv_lim, y0 = 0,  y1 = 0,  xref = "x", yref = "y",
           line = list(dash = "dot", width = 1, color = "black")),
      list(type = "line", x0 = 0,  x1 = 0,  y0 = -mv_lim, y1 = mv_lim, xref = "x", yref = "y",
           line = list(dash = "dot", width = 1, color = "black"))
    )
    if (is.finite(x_end) && is.finite(y_end)) {
      shp <- c(shp, list(
        list(type = "line", x0 = 0, y0 = 0, x1 = x_end, y1 = y_end, xref = "x", yref = "y",
             line = list(dash = "dash", width = 2, color = "rgba(80,18,20,0.85)"))
      ))
    }

    p %>% layout(
      title = list(text = title_text, x = 0.5),
      hovermode = "closest",
      margin = list(t = 60, r = 10, b = 40, l = 60),
      xaxis = list(title = "HB (in)", range = c(-mv_lim, mv_lim), dtick = 3, zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(title = "iVB (in)", range = c(-mv_lim, mv_lim), dtick = 3, zeroline = FALSE, fixedrange = TRUE),
      shapes = shp
    )
  }

  build_xrv_usage_pie <- function(d, hand = "L", title_text = "") {
    if (is.null(d) || !nrow(d)) return(plotly::plot_ly())
    dd <- d %>% dplyr::filter(BatterSideStd %in% hand)
    if (!nrow(dd)) return(plotly::plot_ly())
    usage <- dd %>%
      dplyr::count(PitchType_plot, name = "n", sort = TRUE) %>%
      dplyr::filter(!is.na(PitchType_plot))
    cols <- pitch_colors[usage$PitchType_plot]
    cols[is.na(cols)] <- "#808080"
    plotly::plot_ly(
      data = usage,
      labels = ~PitchType_plot,
      values = ~n,
      type = "pie",
      marker = list(colors = cols),
      textinfo = "label+percent",
      hoverinfo = "label+percent"
    ) %>%
      plotly::layout(title = list(text = title_text, x = 0.5), showlegend = FALSE, margin = list(t = 30, b = 10, l = 10, r = 10))
  }

  build_xrv_ts_plot <- function(d, params, title_text = "Stuff+ Rolling Average") {
    if (is.null(d) || !nrow(d)) return(plotly::plot_ly())
    d <- xrv_prepare_base(d, params)
    if (!nrow(d) || all(!is.finite(d$stuff_plus))) return(plotly::plot_ly())

    if ("CustomGameID" %in% names(d)) {
      d$GameID <- as.character(d$CustomGameID)
    } else if ("GameID" %in% names(d)) {
      d$GameID <- as.character(d$GameID)
    } else if ("GameUID" %in% names(d)) {
      d$GameID <- as.character(d$GameUID)
    } else {
      d$GameID <- NA_character_
    }

    game_info <- xrv_game_info(d)
    if (!nrow(game_info)) return(plotly::plot_ly())
    d <- d %>%
      dplyr::left_join(game_info, by = "GameID") %>%
      dplyr::filter(!is.na(GameIndex), is.finite(stuff_plus))
    if (!nrow(d)) return(plotly::plot_ly())

    order_col <- if ("row_id" %in% names(d)) {
      "row_id"
    } else if ("row_in_file" %in% names(d)) {
      "row_in_file"
    } else if ("PitchNum" %in% names(d)) {
      "PitchNum"
    } else {
      NULL
    }

    if (!is.null(order_col)) {
      d <- d %>% dplyr::arrange(GameIndex, .data[[order_col]])
    } else {
      d <- d %>% dplyr::arrange(GameIndex)
    }

    d <- d %>%
      dplyr::group_by(GameID) %>%
      dplyr::mutate(
        pitch_order = dplyr::row_number(),
        pitch_max = dplyr::n(),
        x_index = GameIndex + pitch_order / (pitch_max + 1)
      ) %>%
      dplyr::ungroup()

    roll_mean_min <- function(x, n) {
      x_num <- suppressWarnings(as.numeric(x))
      out <- rep(NA_real_, length(x_num))
      if (!length(x_num) || !is.finite(n) || n <= 1) return(x_num)
      for (i in seq_along(x_num)) {
        start <- max(1, i - n + 1)
        window <- x_num[start:i]
        if (all(!is.finite(window))) {
          out[i] <- NA_real_
        } else {
          out[i] <- mean(window, na.rm = TRUE)
        }
      }
      out
    }

    p <- plotly::plot_ly()
    pts <- sort(unique(as.character(d$PitchType)))
    pts <- pts[!is.na(pts) & nzchar(pts)]
    for (pt in pts) {
      df_pt <- d %>%
        dplyr::filter(PitchType == pt) %>%
        dplyr::arrange(GameIndex, pitch_order)
      if (!nrow(df_pt)) next
      y_raw_stuff <- df_pt$stuff_plus
      y_roll_stuff <- roll_mean_min(y_raw_stuff, XRV_TS_ROLL_WINDOW)
      col <- pitch_colors[[pt]] %||% "#808080"
      if (any(is.finite(y_raw_stuff))) {
        hover_txt_stuff <- paste0(
          "Game: ", df_pt$GameLabel,
          "<br>", pt, " Stuff+: ", ifelse(is.finite(y_raw_stuff), sprintf("%.1f", y_raw_stuff), "NA"),
          "<br>Rolling Stuff+: ", ifelse(is.finite(y_roll_stuff), sprintf("%.1f", y_roll_stuff), "NA")
        )
        p <- p %>%
          add_trace(
            data = df_pt,
            x = ~x_index,
            y = y_roll_stuff,
            type = "scatter", mode = "lines+markers",
            name = paste0(pt, " Stuff+"),
            line = list(color = col),
            marker = list(color = col, size = 6, opacity = 0.75),
            text = hover_txt_stuff,
            hoverinfo = "text",
            showlegend = TRUE
          )
      }
    }

    p %>% layout(
      title = list(text = title_text, x = 0.5),
      xaxis = list(title = "Game", tickmode = "array", tickvals = game_info$GameIndex, ticktext = game_info$GameLabel),
      yaxis = list(title = "Stuff+", ticksuffix = ""),
      legend = list(orientation = "h", x = 0, y = 1.1),
      margin = list(t = 60, r = 20, b = 40, l = 60),
      hovermode = "closest"
    )
  }

  # ---- xRV data reactives ----
  xrv_season_data <- reactive({
    d <- dataFilter_allhands()
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    d
  })

  xrv_game_data <- reactive({
    req(input$xrv_game_pitcher, input$xrv_game_game)
    d <- rv$df %>%
      dplyr::filter(
        Pitcher == input$xrv_game_pitcher,
        CustomGameID == input$xrv_game_game,
        !(is_bullpen %in% TRUE)
      )
    if ("PitchType" %in% names(d)) d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    d
  })

  xrv_staff_data <- reactive({
    d <- rv$df %>% dplyr::filter(!(is_bullpen %in% TRUE))
    if ("PitchType" %in% names(d)) d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    d <- prefer_team_or_portal(d, "PitcherTeam")
    sg <- input$season_groups %||% character(0)
    if (length(sg) && "SeasonGroup" %in% names(d)) {
      d <- d %>% dplyr::filter(.data$SeasonGroup %in% sg)
    }
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    d
  })

  # ---- xRV dropdown wiring (Game Summary) ----
  observeEvent(rv$df, {
    pitchers <- setdiff(txst_pitchers, EXCLUDE_PLAYERS)
    updateSelectInput(
      session,
      "xrv_game_pitcher",
      choices = as_pitcher_choices(pitchers),
      selected = if (length(pitchers)) pitchers[[1]] else character(0)
    )
  }, ignoreInit = FALSE)

  observeEvent(input$xrv_game_pitcher, {
    req(input$xrv_game_pitcher)
    games <- rv$df %>%
      dplyr::filter(Pitcher == input$xrv_game_pitcher, !(is_bullpen %in% TRUE)) %>%
      dplyr::pull(CustomGameID) %>%
      unique()
    if (!length(games)) {
      d_fallback <- df_nonbp %>% dplyr::filter(Pitcher == input$xrv_game_pitcher)
      games <- fallback_game_ids(d_fallback)
    }
    games <- order_game_ids_desc(games, game_dates)
    updateSelectInput(
      session, "xrv_game_game",
      choices = games,
      selected = if (length(games)) games[[1]] else character(0)
    )
  }, ignoreInit = TRUE)

  # ---- xRV headers ----
  output$xrv_season_header <- renderUI({
    name <- input$PitcherInput %||% "Pitcher"
    season_labels <- season_label_from_tag(input$season_groups %||% character(0))
    div(
      style = "font-size:20px; font-weight:700; margin:6px 0;",
      xrv_header_label(name, season_labels)
    )
  })

  output$xrv_game_header <- renderUI({
    name <- input$xrv_game_pitcher %||% "Pitcher"
    g <- input$xrv_game_game %||% ""
    lab <- {
      dt <- parse_gameid_date(g)
      if (!is.na(dt)) format(dt, "%B %d, %Y") else as.character(g)
    }
    div(style = "font-size:20px; font-weight:700; margin:6px 0;", paste0(name, " — ", lab))
  })

  # ---- Pitching Season Summary helpers ----
  pitching_display_levels <- facet_levels

  normalize_pitching_pitch_value <- function(x) {
    raw <- trimws(as.character(x %||% ""))
    raw[raw %in% c("ChangeUp", "Change Up")] <- "Changeup"
    out <- canonical_pitch_fuzzy(raw)
    out[!(out %in% pitching_display_levels)] <- NA_character_
    out
  }

  sanitize_pitching_dataset <- function(d) {
    d <- tibble::as_tibble(d)
    if (!nrow(d)) return(d)
    n <- nrow(d)
    get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n)
    pt_raw <- dplyr::coalesce(
      get_chr("PitchType_UNI"),
      get_chr("pitch_type_canon"),
      get_chr("PitchType"),
      get_chr("PitchName"),
      get_chr("TaggedPitchType"),
      get_chr("AutoPitchType")
    )
    d$PitchType <- factor(normalize_pitching_pitch_value(pt_raw), levels = pitching_display_levels)
    d %>% dplyr::filter(!is.na(.data$PitchType))
  }

  season_summary_raw_data <- reactive({
    req(input$PitcherInput)
    d <- rv$df %>%
      dplyr::filter(
        Pitcher == input$PitcherInput,
        !(is_bullpen %in% TRUE),
        !(as.character(PitchType) %in% "Bad data")
      )
    d <- prefer_team_or_portal(d, "PitcherTeam")

    sg <- input$season_groups %||% character(0)
    season_col <- intersect(c("SeasonGroup", "SeasonTag", "Season"), names(d))[1]
    if (!is.na(season_col) && length(sg)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sg)
    }

    sel_games <- input$GameInput %||% character(0)
    if (length(sel_games) && "CustomGameID" %in% names(d)) {
      sel_games <- intersect(as.character(sel_games), unique(as.character(d$CustomGameID)))
      if (length(sel_games)) d <- d %>% dplyr::filter(.data$CustomGameID %in% sel_games)
    }

    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id" %in% names(d)) d <- d %>% dplyr::distinct(row_id, .keep_all = TRUE)
    std_aar_cols(d)
  })

  season_summary_data <- reactive({
    sanitize_pitching_dataset(season_summary_raw_data())
  })

  season_summary_label <- reactive({
    labs <- season_label_from_tag(input$season_groups %||% character(0))
    labs <- labs[nzchar(labs)]
    if (length(labs)) paste(labs, collapse = ", ") else "Season"
  })

  get_pitching_ev_la_local <- function(df) {
    first_present <- function(...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }
    evla <- resolve_ev_la_strict(df)
    ev <- suppressWarnings(as.numeric(evla$ev))
    la <- suppressWarnings(as.numeric(evla$la))
    if (all(!is.finite(ev))) {
      ev_col <- first_present("ExitSpeed", "ExitVelocity", "ExitVel", "HitSpeed", "BallExitSpeed", "EV", "EV_mph", "EV (mph)")
      if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
    }
    if (all(!is.finite(la))) {
      la_col <- first_present("Angle", "LaunchAngle", "LA", "Launch_Angle", "Launch.Angle", "Launch Angle", "LAdeg", "LA (deg)")
      if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
    }
    list(ev = ev, la = la)
  }

  season_summary_flags <- function(d) {
    p <- prepare_aar_flags(d)
    if (!nrow(p)) return(p)
    for (nm in c("RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak", "RelHeight", "RelSide", "Extension", "VertApprAngle", "HorzApprAngle")) {
      if (!nm %in% names(p)) p[[nm]] <- NA_real_
      p[[nm]] <- suppressWarnings(as.numeric(p[[nm]]))
    }
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    evla <- get_pitching_ev_la_local(p)
    evn <- evla$ev
    lan <- evla$la
    pc_vec <- as.character(p$PitchCall %||% "")
    bip_vec <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    p %>%
      dplyr::mutate(
        PitchType = as.character(PitchType),
        BatterSideStd = dplyr::case_when(
          BatterSide %in% c("L", "Left", "LHH", "LH") ~ "L",
          BatterSide %in% c("R", "Right", "RHH", "RH", "Rigjht") ~ "R",
          TRUE ~ as.character(BatterSide)
        ),
        .pc = pc_vec,
        .bip = bip_vec,
        .ev_ok = is.finite(evn),
        .la_ok = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = .bip & is.finite(evn) & is.finite(lan) &
          evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX,
        .gb = .bip & is.finite(lan) & lan < 5,
        .zone = if ("InZone" %in% names(.)) as.logical(InZone) else NA,
        .swing = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else .pc %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip"),
        .whiff = .pc == "StrikeSwinging",
        .chase_opp = !is.na(.zone) & !.zone,
        .chase = !is.na(.zone) & !.zone & .swing
      )
  }

  prepare_season_summary_plot_data <- function(d) {
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    d <- season_summary_flags(d)
    if (!nrow(d)) return(tibble::tibble())
    d %>%
      dplyr::mutate(
        PitchType_plot = dplyr::if_else(PitchType %in% pitching_display_levels, PitchType, NA_character_),
        HoverMovement = paste0(
          "<b>", PitchType_plot, "</b><br>",
          "HB: ", ifelse(is.finite(HorzBreak), sprintf("%.1f", HorzBreak), "--"), "<br>",
          "iVB: ", ifelse(is.finite(InducedVertBreak), sprintf("%.1f", InducedVertBreak), "--"), "<br>",
          "Velo: ", ifelse(is.finite(RelSpeed), sprintf("%.1f", RelSpeed), "--"), "<br>",
          "Spin: ", ifelse(is.finite(SpinRate), sprintf("%.0f", SpinRate), "--")
        )
      ) %>%
      dplyr::filter(!is.na(.data$PitchType_plot))
  }

  season_summary_plot_data <- reactive({
    prepare_season_summary_plot_data(season_summary_data())
  })

  season_summary_split_summary <- function(d) {
    p_all <- season_summary_flags(d)
    empty <- tibble::tibble(
      Split = c("v LHH", "v RHH", "Total"),
      `K%` = NA_real_, `BB%` = NA_real_, `Barrel%` = NA_real_,
      `Strike%` = NA_real_, `Zone%` = NA_real_, `Whiff%` = NA_real_,
      wOBA = NA_real_, `GB%` = NA_real_, `CSW%` = NA_real_
    )
    if (!nrow(p_all)) return(empty)

    split_row <- function(dd, label) {
      if (!nrow(dd)) return(empty[match(label, empty$Split), , drop = FALSE])
      pa_last <- get_pa_last(dd)
      outc <- pa_outcome_cols(pa_last)
      pr <- outc$PR_txt
      kb <- as.character(pa_last$KorBB %||% "")
      walks <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
      tibble::tibble(
        Split = label,
        `K%` = mean(outc$K, na.rm = TRUE),
        `BB%` = mean(walks, na.rm = TRUE),
        `Barrel%` = sdiv(sum(dd$.barrel & dd$.barrel_ok, na.rm = TRUE), sum(dd$.bip_evla & dd$.barrel_ok, na.rm = TRUE)),
        `Strike%` = sdiv(sum(dd$.strike, na.rm = TRUE), nrow(dd)),
        `Zone%` = sdiv(sum(dd$.zone, na.rm = TRUE), sum(!is.na(dd$.zone), na.rm = TRUE)),
        `Whiff%` = sdiv(sum(dd$.whiff, na.rm = TRUE), sum(dd$.swing, na.rm = TRUE)),
        wOBA = compute_woba_grouped(pa_last, rep("Total", nrow(pa_last)))$wOBA[1],
        `GB%` = sdiv(sum(dd$.gb, na.rm = TRUE), sum(dd$.bip_la, na.rm = TRUE)),
        `CSW%` = sdiv(sum(dd$.pc %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE), nrow(dd))
      )
    }

    dplyr::bind_rows(
      split_row(p_all %>% dplyr::filter(.data$BatterSideStd == "L"), "v LHH"),
      split_row(p_all %>% dplyr::filter(.data$BatterSideStd == "R"), "v RHH"),
      split_row(p_all, "Total")
    )
  }

  format_season_summary_split_table <- function(df) {
    if (is.null(df) || !nrow(df)) return(data.frame(Status = "No data"))
    fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
    df %>%
      dplyr::mutate(
        `K%` = fmt_pct0(`K%`),
        `BB%` = fmt_pct0(`BB%`),
        `Barrel%` = fmt_pct0(`Barrel%`),
        `Strike%` = fmt_pct0(`Strike%`),
        `Zone%` = fmt_pct0(`Zone%`),
        `Whiff%` = fmt_pct0(`Whiff%`),
        wOBA = fmt_num3(wOBA),
        `GB%` = fmt_pct0(`GB%`),
        `CSW%` = fmt_pct0(`CSW%`)
      )
  }

  season_summary_split_display <- function(d) {
    raw_df <- season_summary_split_summary(d)
    fmt_df <- format_season_summary_split_table(raw_df)
    shaded <- shade_columns_txst(
      fmt_df,
      cols_to_color = intersect(names(fmt_df), c("K%", "BB%", "Barrel%", "Strike%", "Zone%", "Whiff%", "wOBA", "GB%", "CSW%")),
      lower_better = intersect(c("BB%", "Barrel%", "wOBA"), names(fmt_df))
    )
    list(raw = raw_df, fmt = fmt_df, shaded = shaded)
  }

  season_summary_pitch_table_raw <- function(d) {
    p <- season_summary_flags(d)
    if (!nrow(p)) return(tibble::tibble())
    p <- p %>% dplyr::filter(.data$PitchType %in% pitching_display_levels)
    if (!nrow(p)) return(tibble::tibble())
    pa_last <- get_pa_last(p)
    woba_by_pt <- compute_woba_grouped(pa_last, as.character(pa_last$PitchType)) %>%
      dplyr::select(.grp, wOBA) %>%
      dplyr::rename(PitchType = .grp)
    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    base <- p %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        Usage = dplyr::n() / nrow(p),
        Velo = mean_or_na(RelSpeed),
        VeloMax = safe_max_local(RelSpeed),
        iVB = mean_or_na(InducedVertBreak),
        HB = mean_or_na(HorzBreak),
        RelHt = mean_or_na(RelHeight),
        RelSide = mean_or_na(RelSide),
        Ext = mean_or_na(Extension),
        VAA = mean_or_na(VertApprAngle),
        HAA = mean_or_na(HorzApprAngle),
        `Strike%` = sdiv(sum(.strike, na.rm = TRUE), dplyr::n()),
        `Zone%` = sdiv(sum(.zone, na.rm = TRUE), sum(!is.na(.zone), na.rm = TRUE)),
        `Whiff%` = sdiv(sum(.whiff, na.rm = TRUE), sum(.swing, na.rm = TRUE)),
        `Chase%` = sdiv(sum(.chase, na.rm = TRUE), sum(.chase_opp, na.rm = TRUE)),
        `GB%` = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),
        `Barrel%` = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::left_join(woba_by_pt, by = "PitchType")

    total <- p %>%
      dplyr::summarise(
        PitchType = "Total",
        Pitches = dplyr::n(),
        Usage = 1,
        Velo = NA_real_,
        VeloMax = NA_real_,
        iVB = NA_real_,
        HB = NA_real_,
        RelHt = mean_or_na(RelHeight),
        RelSide = mean_or_na(RelSide),
        Ext = mean_or_na(Extension),
        VAA = NA_real_,
        HAA = NA_real_,
        `Strike%` = sdiv(sum(.strike, na.rm = TRUE), dplyr::n()),
        `Zone%` = sdiv(sum(.zone, na.rm = TRUE), sum(!is.na(.zone), na.rm = TRUE)),
        `Whiff%` = sdiv(sum(.whiff, na.rm = TRUE), sum(.swing, na.rm = TRUE)),
        `Chase%` = sdiv(sum(.chase, na.rm = TRUE), sum(.chase_opp, na.rm = TRUE)),
        `GB%` = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),
        `Barrel%` = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        wOBA = compute_woba_grouped(pa_last, rep("Total", nrow(pa_last)))$wOBA[1]
      )

    dplyr::bind_rows(base, total) %>%
      dplyr::mutate(.is_total = .data$PitchType == "Total") %>%
      dplyr::arrange(.is_total, dplyr::desc(Usage), PitchType) %>%
      dplyr::select(-.is_total)
  }

  format_season_summary_pitch_table <- function(df) {
    if (is.null(df) || !nrow(df)) return(data.frame(Status = "No data"))
    fmt_num1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "NA")
    fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
    fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_velo <- function(avg, mx) {
      if (is.finite(avg) && is.finite(mx)) return(sprintf("%.1f (%.1f)", avg, mx))
      if (is.finite(avg)) return(sprintf("%.1f", avg))
      if (is.finite(mx)) return(sprintf("(%.1f)", mx))
      "NA"
    }
    df %>%
      dplyr::transmute(
        PitchType = as.character(PitchType),
        .is_total = PitchType == "Total",
        `%` = fmt_pct0(Usage),
        `Velo (Max)` = ifelse(.is_total, "", mapply(fmt_velo, Velo, VeloMax, SIMPLIFY = TRUE, USE.NAMES = FALSE)),
        iVB = ifelse(.is_total, "", fmt_num1(iVB)),
        HB = ifelse(.is_total, "", fmt_num1(HB)),
        RelHt = fmt_num1(RelHt),
        RelSide = fmt_num1(RelSide),
        Ext = fmt_num1(Ext),
        VAA = ifelse(.is_total, "", fmt_num1(VAA)),
        HAA = ifelse(.is_total, "", fmt_num1(HAA)),
        `Strike%` = fmt_pct0(`Strike%`),
        `Zone%` = fmt_pct0(`Zone%`),
        `Whiff%` = fmt_pct0(`Whiff%`),
        `Chase%` = fmt_pct0(`Chase%`),
        `Barrel%` = fmt_pct0(`Barrel%`),
        wOBA = fmt_num3(wOBA)
      ) %>%
      dplyr::select(-.is_total)
  }

  season_summary_pitch_display <- function(d) {
    raw_df <- season_summary_pitch_table_raw(d)
    fmt_df <- format_season_summary_pitch_table(raw_df)
    shaded <- shade_columns_txst(
      fmt_df,
      cols_to_color = intersect(names(fmt_df), c("RelHt", "Strike%", "Zone%", "Whiff%", "Chase%", "Barrel%", "wOBA")),
      lower_better = intersect(c("Barrel%", "wOBA"), names(fmt_df))
    )
    list(raw = raw_df, fmt = fmt_df, shaded = shaded)
  }

  season_summary_hand_pitch_perf_display <- function(d, hand = c("L", "R")) {
    hand <- match.arg(hand)
    dd <- d %>%
      dplyr::mutate(
        BatterSideStd = dplyr::case_when(
          BatterSide %in% c("L", "Left", "LHH", "LH") ~ "L",
          BatterSide %in% c("R", "Right", "RHH", "RH", "Rigjht") ~ "R",
          TRUE ~ as.character(BatterSide)
        )
      ) %>%
      dplyr::filter(.data$BatterSideStd == hand)
    raw_base <- season_summary_pitch_table_raw(dd)
    raw_df <- if (!nrow(raw_base) || !"PitchType" %in% names(raw_base)) {
      tibble::tibble()
    } else {
      raw_base %>%
        dplyr::filter(.data$PitchType != "Total") %>%
        dplyr::select(PitchType, `Strike%`, `Zone%`, `Chase%`, `Whiff%`, `GB%`, `Barrel%`, wOBA)
    }
    fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
    fmt_df <- if (!nrow(raw_df)) {
      data.frame(Status = "No data")
    } else {
      raw_df %>%
        dplyr::transmute(
          PitchType = as.character(PitchType),
          `Strike%` = fmt_pct0(`Strike%`),
          `Zone%` = fmt_pct0(`Zone%`),
          `Chase%` = fmt_pct0(`Chase%`),
          `Whiff%` = fmt_pct0(`Whiff%`),
          `GB%` = fmt_pct0(`GB%`),
          `Barrel%` = fmt_pct0(`Barrel%`),
          wOBA = fmt_num3(wOBA)
        )
    }
    shaded <- shade_columns_txst(
      fmt_df,
      cols_to_color = intersect(names(fmt_df), c("Strike%", "Zone%", "Chase%", "Whiff%", "GB%", "Barrel%", "wOBA")),
      lower_better = intersect(c("Barrel%", "wOBA"), names(fmt_df))
    )
    list(raw = raw_df, fmt = fmt_df, shaded = shaded)
  }

  build_pitch_metrics_movement_gg <- function(d, title_text, compact = FALSE) {
    if (is.null(d) || !nrow(d) || !all(c("HorzBreak", "InducedVertBreak", "PitchType_plot") %in% names(d))) {
      return(ggplot() + theme_void() + labs(title = paste0(title_text, " (no data)")))
    }
    mv <- d %>% dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
    if (!nrow(mv)) return(ggplot() + theme_void() + labs(title = paste0(title_text, " (no data)")))
    hand <- guess_throw_hand(input$PitcherInput, mv)
    slope <- movement_line_slope(hand, mv)
    signx <- if (identical(hand, "LHP")) -1 else 1
    mv_lim <- 25
    x_end <- NA_real_
    y_end <- NA_real_
    if (is.finite(slope)) {
      if (abs(slope) <= 1) {
        x_end <- mv_lim * signx
        y_end <- mv_lim * slope
      } else {
        y_end <- mv_lim * sign(slope)
        x_end <- signx * (y_end / slope)
      }
    }
    mv_avg <- mv %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(HB = mean(HorzBreak, na.rm = TRUE), IVB = mean(InducedVertBreak, na.rm = TRUE), .groups = "drop")
    ggplot(mv, aes(HorzBreak, InducedVertBreak, color = PitchType_plot)) +
      geom_hline(yintercept = 0, linewidth = 1, color = "black", linetype = "dotted") +
      geom_vline(xintercept = 0, linewidth = 1, color = "black", linetype = "dotted") +
      { if (is.finite(x_end) && is.finite(y_end)) annotate("segment", x = 0, y = 0, xend = x_end, yend = y_end, linetype = "dashed", linewidth = 1, color = grDevices::adjustcolor("#501214", alpha.f = 0.85)) } +
      geom_point(alpha = 0.30, size = 2.2) +
      geom_point(data = mv_avg, aes(HB, IVB, fill = PitchType_plot), inherit.aes = FALSE, shape = 21, size = 5.5, color = "black", stroke = 0.6) +
      scale_color_manual(values = pitch_colors, breaks = pitching_display_levels, limits = pitching_display_levels, drop = FALSE) +
      scale_fill_manual(values = pitch_colors, breaks = pitching_display_levels, limits = pitching_display_levels, drop = FALSE) +
      scale_x_continuous(breaks = seq(-24, 24, by = 3), minor_breaks = NULL) +
      scale_y_continuous(breaks = seq(-24, 24, by = 3), minor_breaks = NULL) +
      coord_fixed(xlim = c(-mv_lim, mv_lim), ylim = c(-mv_lim, mv_lim), expand = FALSE) +
      labs(title = title_text, x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_line(color = "grey85", linewidth = 0.3),
        panel.background = element_rect(fill = "grey92", color = NA),
        plot.background = element_rect(fill = "grey92", color = NA),
        axis.title = element_text(face = "bold"),
        axis.line = element_line(color = "black", linewidth = 0.8),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        plot.margin = margin(if (compact) 0 else 5, if (compact) -18 else 5, if (compact) -2 else 5, if (compact) -28 else 5)
      )
  }

  build_pitch_usage_panel_gg <- function(d, hand, title_text, compact = FALSE) {
    dd <- d %>% dplyr::filter(BatterSideStd %in% hand)
    if (!nrow(dd)) return(ggplot() + theme_void() + labs(title = paste0(title_text, " (no data)")))
    usage <- dd %>%
      dplyr::count(PitchType_plot, name = "n", sort = TRUE) %>%
      dplyr::mutate(pct = n / sum(n))
    usage$PitchType_plot <- factor(as.character(usage$PitchType_plot), levels = as.character(usage$PitchType_plot))

    pie <- ggplot(usage, aes(x = 1, y = pct, fill = PitchType_plot)) +
      geom_col(color = "white", linewidth = 0.5) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = pitch_colors, breaks = pitching_display_levels, limits = pitching_display_levels, drop = FALSE) +
      labs(title = title_text, x = NULL, y = NULL) +
      xlim(0.2, 1.8) +
      theme_void(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        plot.margin = margin(0, if (compact) -8 else 0, 0, 0),
        panel.background = element_rect(fill = "grey92", color = NA),
        plot.background = element_rect(fill = "grey92", color = NA)
      )

    legend_df <- usage %>%
      dplyr::mutate(label = paste0(PitchType_plot, " ", sprintf("%.0f%%", 100 * pct)), y = rev(seq_len(dplyr::n())))
    legend_plot <- ggplot(legend_df, aes(x = 0, y = y, label = label, color = PitchType_plot)) +
      geom_text(hjust = 0, size = if (compact) 4.0 else 4.2, fontface = "bold") +
      scale_color_manual(values = pitch_colors, breaks = pitching_display_levels, limits = pitching_display_levels, drop = FALSE) +
      xlim(0, 1) +
      ylim(0.5, max(legend_df$y) + 0.5) +
      theme_void(base_size = 11) +
      theme(
        plot.margin = margin(6, 0, 6, if (compact) -18 else 0),
        legend.position = "none",
        panel.background = element_rect(fill = "grey92", color = NA),
        plot.background = element_rect(fill = "grey92", color = NA)
      )
    pie | legend_plot
  }

  build_season_summary_pdf_table <- function(df, title) {
    if (is.null(df) || !nrow(df)) df <- data.frame(Status = "No data")
    safe_tbl_plot(as.data.frame(df, stringsAsFactors = FALSE), title = title)
  }

  render_plot_to_pdf_via_png <- function(plot_obj, outfile, width = 14, height = 11, dpi = 300) {
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(tmp_png, width = width, height = height, units = "in", res = dpi, background = "white")
    } else {
      grDevices::png(tmp_png, width = width * dpi, height = height * dpi, res = dpi, bg = "white")
    }
    print(plot_obj)
    grDevices::dev.off()
    img <- png::readPNG(tmp_png)
    grDevices::pdf(outfile, width = width, height = height, useDingbats = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    grid::grid.raster(img, x = 0.5, y = 0.5, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
  }

  compose_season_summary_plot <- function(d, pitcher_name, season_label_text = "Season") {
    d_display <- sanitize_pitching_dataset(d)
    d_plot <- prepare_season_summary_plot_data(d_display)
    split_disp <- season_summary_split_display(d_display)
    pitch_disp <- season_summary_pitch_display(d_display)
    hand_l_disp <- season_summary_hand_pitch_perf_display(d_display, "L")
    hand_r_disp <- season_summary_hand_pitch_perf_display(d_display, "R")

    header_plot <- ggplot() +
      annotate("text", x = 0, y = 0.90, label = pitcher_name, hjust = 0, vjust = 1, size = 6.0, fontface = "bold") +
      annotate("text", x = 0, y = 0.62, label = season_label_text, hjust = 0, vjust = 1, size = 4.6, color = "#501214", fontface = "bold") +
      xlim(0, 1) + ylim(0, 1) + theme_void() +
      theme(plot.margin = margin(2, 6, 2, 6))

    split_tbl <- build_season_summary_pdf_table(split_disp$fmt, "Split Summary")
    pitch_tbl <- build_season_summary_pdf_table(pitch_disp$fmt, "Pitch Type Summary")
    hand_l_tbl <- build_season_summary_pdf_table(hand_l_disp$fmt, "v LHH Pitch Type Performance")
    hand_r_tbl <- build_season_summary_pdf_table(hand_r_disp$fmt, "v RHH Pitch Type Performance")
    mv_plot <- build_pitch_metrics_movement_gg(d_plot, paste0(pitcher_name, ": Pitch Movement"), compact = TRUE)
    pie_l <- patchwork::wrap_elements(full = cowplot::as_grob(build_pitch_usage_panel_gg(d_plot, "L", "v LHH Usage", compact = TRUE)))
    pie_r <- patchwork::wrap_elements(full = cowplot::as_grob(build_pitch_usage_panel_gg(d_plot, "R", "v RHH Usage", compact = TRUE)))

    top_row <- (split_tbl | (pie_l | pie_r)) + patchwork::plot_layout(widths = c(0.43, 0.57))
    hand_tbls <- (hand_l_tbl / hand_r_tbl) + patchwork::plot_layout(heights = c(0.50, 0.50))
    plots_row <- (mv_plot | hand_tbls) + patchwork::plot_layout(widths = c(0.42, 0.58))
    bottom_row <- (pitch_tbl | patchwork::plot_spacer()) + patchwork::plot_layout(widths = c(0.84, 0.16))

    final <- header_plot / top_row / plots_row / bottom_row +
      patchwork::plot_layout(heights = c(0.07, 0.24, 0.45, 0.24))

    logo <- if (exists("bullpen_report_logo_bundle", mode = "function")) bullpen_report_logo_bundle() else list(grob = NULL, aspect = 1)
    cowplot::ggdraw() +
      cowplot::draw_plot(final, x = 0, y = 0, width = 1, height = 1) +
      { if (!is.null(logo$grob)) cowplot::draw_grob(logo$grob, x = 0.985, y = 0.03, width = min(0.16 * logo$aspect, 0.19), height = 0.16, hjust = 1, vjust = 0) }
  }

  # ---- Pitching Season Summary outputs ----
  output$season_summary_header_ui <- renderUI({
    nm <- input$PitcherInput %||% "Pitcher"
    lab <- season_summary_label()
    tags$div(
      style = "padding: 4px 0 10px 0;",
      tags$div(style = "font-size: 28px; font-weight: 800; line-height: 1.05;", nm),
      tags$div(style = "font-size: 18px; font-weight: 700; color: #501214; margin-top: 2px;", lab)
    )
  })

  output$season_summary_split_table <- DT::renderDT({
    out_df <- season_summary_split_display(season_summary_data())$shaded
    DT::datatable(out_df, rownames = FALSE, escape = FALSE, class = "stripe", options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE))
  })

  output$season_summary_movement <- renderPlot({
    build_pitch_metrics_movement_gg(season_summary_plot_data(), paste0(input$PitcherInput %||% "Pitcher", ": Pitch Movement"))
  })

  output$season_summary_usage_lhh <- renderPlot({
    build_pitch_usage_panel_gg(season_summary_plot_data(), "L", "v LHH Usage")
  })

  output$season_summary_usage_rhh <- renderPlot({
    build_pitch_usage_panel_gg(season_summary_plot_data(), "R", "v RHH Usage")
  })

  output$season_summary_hand_perf_lhh <- DT::renderDT({
    out_df <- season_summary_hand_pitch_perf_display(season_summary_data(), "L")$shaded
    DT::datatable(out_df, rownames = FALSE, escape = FALSE, class = "stripe", options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE, scrollX = TRUE))
  })

  output$season_summary_hand_perf_rhh <- DT::renderDT({
    out_df <- season_summary_hand_pitch_perf_display(season_summary_data(), "R")$shaded
    DT::datatable(out_df, rownames = FALSE, escape = FALSE, class = "stripe", options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE, scrollX = TRUE))
  })

  output$season_summary_pitch_table <- DT::renderDT({
    out_df <- season_summary_pitch_display(season_summary_data())$shaded
    DT::datatable(out_df, rownames = FALSE, escape = FALSE, class = "stripe", options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE, autoWidth = TRUE))
  })

  output$season_summary_pdf <- downloadHandler(
    filename = function() {
      nm <- gsub("\\s+", "_", input$PitcherInput %||% "Pitcher")
      lab <- gsub("[^A-Za-z0-9_]+", "_", season_summary_label() %||% "Season")
      paste0("Season_Summary_", nm, "_", lab, ".pdf")
    },
    content = function(file) {
      tryCatch({
        d <- season_summary_raw_data()
        validate(need(nrow(d) > 0, "No season summary data available for the current filters."))
        final <- compose_season_summary_plot(
          d = d,
          pitcher_name = input$PitcherInput %||% "Pitcher",
          season_label_text = season_summary_label()
        )
        render_plot_to_pdf_via_png(final, file, width = 14, height = 11, dpi = 320)
      }, error = function(e) {
        msg <- conditionMessage(e)
        warning("[Season Summary PDF] ", msg)
        grDevices::pdf(file, width = 11, height = 8.5, useDingbats = FALSE)
        on.exit(grDevices::dev.off(), add = TRUE)
        plot.new()
        text(0.5, 0.5, paste("SEASON SUMMARY PDF ERROR:\n\n", msg), cex = 1.0)
      })
    }
  )

  # ---- xRV Season Summary outputs ----
  output$xrv_season_stats <- DT::renderDT({
    d <- xrv_season_data()
    DT::datatable(xrv_summary_stats_df(d), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "stripe")
  })

  output$xrv_season_movement <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_season_data(), params)
    build_xrv_movement_plot(d, paste0(input$PitcherInput, ": Pitch Movement"))
  })

  output$xrv_season_usage_lhh <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_season_data(), params)
    build_xrv_usage_pie(d, "L", "")
  })

  output$xrv_season_usage_rhh <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_season_data(), params)
    build_xrv_usage_pie(d, "R", "")
  })

  output$xrv_season_pitch_table <- DT::renderDT({
    params <- xrv_scale_params()
    d <- xrv_season_data()
    DT::datatable(xrv_pitch_table_df(d, params), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE), class = "stripe")
  })

  output$xrv_season_ts <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_season_data()
    build_xrv_ts_plot(d, params, paste0(input$PitcherInput, ": Stuff+ Rolling Average"))
  })

  # ---- xRV Game Summary outputs ----
  output$xrv_game_stats <- DT::renderDT({
    d <- xrv_game_data()
    DT::datatable(xrv_summary_stats_df(d), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "stripe")
  })

  output$xrv_game_movement <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_game_data(), params)
    build_xrv_movement_plot(d, paste0(input$xrv_game_pitcher, ": Pitch Movement"))
  })

  output$xrv_game_usage_lhh <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_game_data(), params)
    build_xrv_usage_pie(d, "L", "")
  })

  output$xrv_game_usage_rhh <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_game_data(), params)
    build_xrv_usage_pie(d, "R", "")
  })

  output$xrv_game_pitch_table <- DT::renderDT({
    params <- xrv_scale_params()
    d <- xrv_game_data()
    DT::datatable(xrv_pitch_table_df(d, params), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE), class = "stripe")
  })

  output$xrv_game_ts <- plotly::renderPlotly({
    params <- xrv_scale_params()
    d <- xrv_game_data()
    build_xrv_ts_plot(d, params, paste0(input$xrv_game_pitcher, ": Stuff+ Rolling Average"))
  })

  # ---- xRV Staff Leaderboard ----
  output$xrv_leader_pitch_filter <- renderUI({
    d <- xrv_staff_data()
    pts <- sort(unique(as.character(d$PitchType)))
    pts <- pts[!is.na(pts) & nzchar(pts)]
    selectizeInput(
      "xrv_leader_pitch_types", NULL,
      choices = pts, selected = pts, multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Filter pitch types"),
      width = "100%"
    )
  })

  output$xrv_leader_totals <- DT::renderDT({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_staff_data(), params)
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data"), rownames = FALSE, options = list(dom = "t", paging = FALSE)))
    }
    min_p <- input$xrv_leader_min_pitches %||% 0
    out <- d %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        Stuff_avg = mean(stuff_plus, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(Pitches >= min_p) %>%
      dplyr::arrange(dplyr::desc(Stuff_avg)) %>%
      dplyr::rename(`Stuff+ (avg)` = Stuff_avg)
    dt <- DT::datatable(
      out,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE, order = list(list(2, "desc"))),
      class = "stripe"
    )
    DT::formatRound(dt, "Stuff+ (avg)", 1)
  })

  output$xrv_leader_pitch_table <- DT::renderDT({
    params <- xrv_scale_params()
    d <- xrv_prepare_base(xrv_staff_data(), params)
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data"), rownames = FALSE, options = list(dom = "t", paging = FALSE)))
    }
    sel <- input$xrv_leader_pitch_types %||% character(0)
    if (length(sel)) d <- d %>% dplyr::filter(PitchType %in% sel)
    min_p <- input$xrv_leader_min_pitches %||% 0
    out <- d %>%
      dplyr::group_by(Pitcher, PitchType) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        Stuff_avg = mean(stuff_plus, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::mutate(Usage = Pitches / sum(Pitches, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Pitches >= min_p) %>%
      dplyr::arrange(dplyr::desc(Stuff_avg)) %>%
      dplyr::select(Pitcher, PitchType, Pitches, Usage, Stuff_avg) %>%
      dplyr::rename(`Usage %` = Usage, `Stuff+ (avg)` = Stuff_avg)
    dt <- DT::datatable(
      out,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE, order = list(list(4, "desc"))),
      class = "stripe"
    )
    dt <- DT::formatPercentage(dt, "Usage %", 0)
    DT::formatRound(dt, "Stuff+ (avg)", 1)
  })

  # Staff-level data (Self Scouting): independent of sidebar filters
  staff_data <- reactive({
    d <- txst_df
    d <- tibble::as_tibble(d)

    # Optional team filter (if column exists)
    d <- prefer_team_or_portal(d, "PitcherTeam")

    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }

    # Date range filter (Self Scouting only)
    dr <- input$self_dates
    if (!is.null(dr) && length(dr) == 2) {
      date_vec <- if ("GameDate" %in% names(d)) {
        parse_date_any(d$GameDate)
      } else if ("Date" %in% names(d)) {
        parse_date_any(d$Date)
      } else if ("PitchDate" %in% names(d)) {
        parse_date_any(d$PitchDate)
      } else if ("CustomGameID" %in% names(d)) {
        parse_gameid_date(d$CustomGameID)
      } else {
        as.Date(NA)
      }
      d <- d %>% dplyr::mutate(.self_date = as.Date(date_vec)) %>%
        dplyr::filter(.self_date >= dr[1] & .self_date <= dr[2]) %>%
        dplyr::select(-.self_date)
    }

    # Deduplicate
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)

    d
  })

  # ---- Pitch Sequencing base prep ----
  prep_pitch_seq_base <- function(d) {
    if (is.null(d) || !nrow(d)) {
      return(list(data = d, pa_last = d, pitch_types = character(0)))
    }

    d <- ensure_counts(d)

    if (!"PitchType" %in% names(d)) d$PitchType <- "Undefined"
    d$PitchType <- as.character(d$PitchType %||% "Undefined")
    d$PitchType[!nzchar(d$PitchType)] <- "Undefined"

    usage <- d %>% dplyr::count(PitchType, name = "n", sort = TRUE)
    pitch_types <- usage$PitchType

    d <- d %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::arrange(PitchNum, .by_group = TRUE) %>%
      dplyr::mutate(
        PrevPitchType  = dplyr::lag(PitchType),
        Prev2PitchType = dplyr::lag(PitchType, 2)
      ) %>%
      dplyr::ungroup()

    d$DoubledUp <- !is.na(d$PrevPitchType) &
      d$PitchType == d$PrevPitchType &
      !(d$PitchType == d$Prev2PitchType)
    d$TripledUp <- !is.na(d$Prev2PitchType) &
      d$PitchType == d$PrevPitchType &
      d$PitchType == d$Prev2PitchType

    d$count_str <- ifelse(
      is.na(d$BallsPre) | is.na(d$StrikesPre),
      NA_character_,
      paste0(d$BallsPre, "-", d$StrikesPre)
    )
    d$ctx_00       <- d$count_str == "0-0"
    d$ctx_pitchers <- d$count_str %in% c("0-1","0-2","1-2")
    d$ctx_hitters  <- d$count_str %in% c("1-0","2-0","2-1","3-1")
    d$ctx_even     <- d$count_str %in% c("0-0","1-1","2-2","3-2")
    d$ctx_2str     <- !is.na(d$StrikesPre) & d$StrikesPre >= 2

    d$BatterSideSeq <- dplyr::case_when(
      d$BatterSide %in% c("L","Left","LHH","LH") ~ "L",
      d$BatterSide %in% c("R","Right","RHH","RH") ~ "R",
      TRUE ~ as.character(d$BatterSide)
    )

    if ("InZone" %in% names(d)) {
      d$InZoneSeq <- as.logical(d$InZone)
    } else if ("inZone" %in% names(d)) {
      d$InZoneSeq <- d$inZone == 1
    } else {
      d$InZoneSeq <- derive_zone_inches(d)
    }

    d$IsSwingSeq <- if ("IsSwing" %in% names(d)) as.logical(d$IsSwing) else .is_swing_event(d$PitchCall)

    get_ev_la_seq <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_seq(d)
    d$EV_seq <- suppressWarnings(as.numeric(evla$ev))
    d$LA_seq <- suppressWarnings(as.numeric(evla$la))

    d$BIP_seq <- safe_is_bip(d$PitchCall, if ("PlayResult" %in% names(d)) d$PlayResult else NULL)
    d$barrel_ok <- barrel_date_ok(d)
    d$ev_ok <- is.finite(d$EV_seq)
    d$la_ok <- is.finite(d$LA_seq)
    d$barrel_seq <- d$BIP_seq & d$ev_ok & d$la_ok &
      d$EV_seq >= BARREL_EV_MIN & d$LA_seq >= BARREL_LA_MIN & d$LA_seq <= BARREL_LA_MAX
    d$gb_seq <- d$BIP_seq & d$la_ok & d$LA_seq < 5
    d$bip_evla <- d$BIP_seq & d$ev_ok & d$la_ok
    d$bip_la   <- d$BIP_seq & d$la_ok

    pa_last <- d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()

    list(data = d, pa_last = pa_last, pitch_types = pitch_types)
  }

  pitch_seq_base <- reactive({
    prep_pitch_seq_base(dataFilter())
  })

  pitch_seq_base_allhands <- reactive({
    prep_pitch_seq_base(dataFilter_allhands())
  })

  pitch_seq_ids <- reactive({
    base <- pitch_seq_base()
    pts <- base$pitch_types
    if (!length(pts)) return(tibble::tibble(pt = character(), id = character()))
    safe <- tolower(gsub("[^A-Za-z0-9]+", "_", pts))
    safe <- ifelse(nzchar(safe), safe, paste0("pt", seq_along(pts)))
    safe <- make.unique(safe)
    tibble::tibble(pt = pts, id = paste0("pitch_seq_", safe))
  })

  fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
  fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")

  calc_seq_metrics <- function(d_sub, pa_sub) {
    if (is.null(d_sub) || !nrow(d_sub)) {
      return(list(
        Whiff_pct = NA_real_, CSW_pct = NA_real_, Swing_pct = NA_real_,
        SLG = NA_real_, wOBA = NA_real_, Barrel_pct = NA_real_,
        Chase_pct = NA_real_, GB_pct = NA_real_
      ))
    }

    pitches <- nrow(d_sub)
    swings  <- sum(d_sub$IsSwingSeq, na.rm = TRUE)
    whiffs  <- sum(d_sub$PitchCall == "StrikeSwinging", na.rm = TRUE)
    csw     <- sum(d_sub$PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE)

    swing_pct <- sdiv(swings, pitches)
    whiff_pct <- sdiv(whiffs, swings)
    csw_pct   <- sdiv(csw, pitches)

    chase_den <- sum(!d_sub$InZoneSeq, na.rm = TRUE)
    chase_num <- sum(!d_sub$InZoneSeq & d_sub$IsSwingSeq, na.rm = TRUE)
    chase_pct <- sdiv(chase_num, chase_den)

    gb_den <- sum(d_sub$bip_la, na.rm = TRUE)
    gb_num <- sum(d_sub$gb_seq, na.rm = TRUE)
    gb_pct <- sdiv(gb_num, gb_den)

    barrel_den <- sum(d_sub$bip_evla & d_sub$barrel_ok, na.rm = TRUE)
    barrel_num <- sum(d_sub$barrel_seq & d_sub$barrel_ok, na.rm = TRUE)
    barrel_pct <- sdiv(barrel_num, barrel_den)

    woba <- NA_real_
    slg  <- NA_real_
    if (!is.null(pa_sub) && nrow(pa_sub) > 0) {
      outc <- pa_outcome_cols(pa_sub)
      w_num <- woba_weights$BB  * as.numeric(outc$BB)  +
        woba_weights$HBP * as.numeric(outc$HBP) +
        woba_weights$X1B * as.numeric(outc$X1B) +
        woba_weights$X2B * as.numeric(outc$X2B) +
        woba_weights$X3B * as.numeric(outc$X3B) +
        woba_weights$HR  * as.numeric(outc$HR)
      w_den <- as.numeric(outc$BB) + as.numeric(outc$HBP) +
        as.numeric(outc$BIP_pa) + as.numeric(outc$K)
      woba <- sdiv(sum(w_num, na.rm = TRUE), sum(w_den, na.rm = TRUE))

      tb <- 1 * as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) +
        3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
      sf <- grepl("(?i)sacrifice fly|\\bsf\\b", outc$PR_txt)
      kb <- if ("KorBB" %in% names(pa_sub)) as.character(pa_sub$KorBB) else rep("", nrow(pa_sub))
      pr <- if ("PlayResult" %in% names(pa_sub)) as.character(pa_sub$PlayResult) else rep("", nrow(pa_sub))
      ibb <- grepl("intentional|\\bibb\\b", kb, ignore.case = TRUE) |
        grepl("intentional", pr, ignore.case = TRUE)
      bb_tot <- as.numeric(outc$BB) + as.numeric(ibb)
      ab <- pmax(
        nrow(pa_sub) - sum(bb_tot, na.rm = TRUE) -
          sum(as.numeric(outc$HBP), na.rm = TRUE) - sum(sf, na.rm = TRUE),
        0
      )
      slg <- sdiv(sum(tb, na.rm = TRUE), ab)
    }

    list(
      Whiff_pct = whiff_pct,
      CSW_pct   = csw_pct,
      Swing_pct = swing_pct,
      SLG       = slg,
      wOBA      = woba,
      Barrel_pct = barrel_pct,
      Chase_pct  = chase_pct,
      GB_pct     = gb_pct
    )
  }

  calc_prv_context <- function(d_sub, pa_sub) {
    if (is.null(d_sub) || !nrow(d_sub) || is.null(pa_sub) || !nrow(pa_sub)) return(NA_real_)
    pitches_n <- nrow(d_sub)
    if (!is.finite(pitches_n) || pitches_n <= 0) return(NA_real_)

    outc <- pa_outcome_cols(pa_sub)
    tb <- 1 * as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) +
      3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
    hr <- as.numeric(outc$HR)

    kb <- if ("KorBB" %in% names(pa_sub)) as.character(pa_sub$KorBB) else rep("", nrow(pa_sub))
    pr <- if ("PlayResult" %in% names(pa_sub)) as.character(pa_sub$PlayResult) else rep("", nrow(pa_sub))
    ibb <- grepl("intentional|\\bibb\\b", kb, ignore.case = TRUE) |
      grepl("intentional", pr, ignore.case = TRUE)
    bb <- as.numeric(outc$BB) + as.numeric(ibb)
    k  <- as.numeric(outc$K)

    to_num_local <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
    rbi_present <- FALSE
    rbi_vec <- rep(NA_real_, nrow(pa_sub))
    rbi_cands <- c("RBI","RBI|PIT","RBI_PIT","RBI (PIT)","RBI_P","RunsBattedIn","Runs_Batted_In","RBIs")
    col <- rbi_cands[rbi_cands %in% names(pa_sub)][1]
    if (!is.na(col)) {
      rbi_vec <- to_num_local(pa_sub[[col]])
      rbi_present <- TRUE
    } else if ("RunsScored" %in% names(pa_sub)) {
      rbi_vec <- to_num_local(pa_sub[["RunsScored"]])
      rbi_vec <- ifelse(outc$BIP_pa %in% TRUE, rbi_vec, 0)
      rbi_present <- TRUE
    }
    if (!rbi_present) return(NA_real_)

    tb_sum  <- sum(tb, na.rm = TRUE)
    bb_sum  <- sum(bb, na.rm = TRUE)
    k_sum   <- sum(k,  na.rm = TRUE)
    hr_sum  <- sum(hr, na.rm = TRUE)
    rbi_sum <- sum(rbi_vec, na.rm = TRUE)

    if (!is.finite(rbi_sum)) return(NA_real_)
    sdiv((((tb_sum + bb_sum - k_sum) / 4) + rbi_sum + hr_sum), pitches_n) * 100
  }

  build_pitch_seq_table <- function(pt, d, pa_last, arsenal) {
    if (is.null(d) || !nrow(d)) {
      return(data.frame(Status = "No data"))
    }

    d_pt  <- d %>% dplyr::filter(PitchType == pt)
    pa_pt <- pa_last %>% dplyr::filter(PitchType == pt)

    if (!nrow(d_pt)) {
      return(data.frame(Status = "No data"))
    }

    base_rows <- list(
      "0-0 count"      = function(df) df$ctx_00 %in% TRUE,
      "Pitchers count" = function(df) df$ctx_pitchers %in% TRUE,
      "Hitters count"  = function(df) df$ctx_hitters %in% TRUE,
      "Even count"     = function(df) df$ctx_even %in% TRUE,
      "2 Strikes"      = function(df) df$ctx_2str %in% TRUE,
      "Doubled Up"     = function(df) df$DoubledUp %in% TRUE,
      "Tripled Up"     = function(df) df$TripledUp %in% TRUE
    )

    rows <- lapply(names(base_rows), function(lbl) {
      cond_d  <- base_rows[[lbl]](d_pt)
      cond_pa <- base_rows[[lbl]](pa_pt)
      stats <- calc_seq_metrics(
        d_pt[cond_d, , drop = FALSE],
        pa_pt[cond_pa, , drop = FALSE]
      )
      tibble::tibble(
        Context  = lbl,
        Whiff_pct  = stats$Whiff_pct,
        CSW_pct    = stats$CSW_pct,
        Swing_pct  = stats$Swing_pct,
        SLG_val    = stats$SLG,
        wOBA_val   = stats$wOBA,
        Barrel_pct = stats$Barrel_pct,
        Chase_pct  = stats$Chase_pct,
        GB_pct     = stats$GB_pct
      )
    })

    after_types <- setdiff(arsenal, pt)
    after_rows <- lapply(after_types, function(aft) {
      cond_d  <- d_pt$PrevPitchType == aft
      cond_pa <- pa_pt$PrevPitchType == aft
      stats <- calc_seq_metrics(
        d_pt[cond_d %in% TRUE, , drop = FALSE],
        pa_pt[cond_pa %in% TRUE, , drop = FALSE]
      )
      tibble::tibble(
        Context  = paste("After", aft),
        Whiff_pct  = stats$Whiff_pct,
        CSW_pct    = stats$CSW_pct,
        Swing_pct  = stats$Swing_pct,
        SLG_val    = stats$SLG,
        wOBA_val   = stats$wOBA,
        Barrel_pct = stats$Barrel_pct,
        Chase_pct  = stats$Chase_pct,
        GB_pct     = stats$GB_pct
      )
    })

    out_num <- dplyr::bind_rows(rows, after_rows)

    out_df <- out_num %>%
      dplyr::mutate(
        `Whiff%`  = fmt_pct0(.data$Whiff_pct),
        `CSW%`    = fmt_pct0(.data$CSW_pct),
        `Swing%`  = fmt_pct0(.data$Swing_pct),
        SLG       = fmt_num3(.data$SLG_val),
        wOBA      = fmt_num3(.data$wOBA_val),
        `Barrel%` = fmt_pct0(.data$Barrel_pct),
        `Chase%`  = fmt_pct0(.data$Chase_pct),
        `GB%`     = fmt_pct0(.data$GB_pct)
      ) %>%
      dplyr::select(
        Context, `Whiff%`, `CSW%`, `Swing%`, SLG, wOBA, `Barrel%`, `Chase%`, `GB%`
      )

    # add numeric sort columns (hidden in DT)
    sort_map <- list(
      `Whiff%`  = out_num$Whiff_pct,
      `CSW%`    = out_num$CSW_pct,
      `Swing%`  = out_num$Swing_pct,
      SLG       = out_num$SLG_val,
      wOBA      = out_num$wOBA_val,
      `Barrel%` = out_num$Barrel_pct,
      `Chase%`  = out_num$Chase_pct,
      `GB%`     = out_num$GB_pct
    )
    for (nm in names(sort_map)) {
      out_df[[paste0("..sort_", nm)]] <- sort_map[[nm]]
    }

    display_cols <- c("Context","Whiff%","CSW%","Swing%","SLG","wOBA","Barrel%","Chase%","GB%")
    cols_to_color <- intersect(display_cols, c(names(D1_PCT_AVG), names(ABS_RULES)))
    percent_cols  <- intersect(display_cols, names(D1_PCT_AVG))
    lower_better  <- intersect(display_cols, c("Barrel%","wOBA","SLG"))

    shade_columns_txst(
      out_df,
      cols_to_color = cols_to_color,
      lower_better  = lower_better,
      percent_cols  = percent_cols
    )
  }

  build_usage_recs_table <- function(base, hand = NULL) {
    d <- base$data
    pa_last <- base$pa_last
    arsenal <- base$pitch_types

    if (is.null(d) || !nrow(d) || !length(arsenal)) {
      return(data.frame(Status = "No data"))
    }

    if (!is.null(hand)) {
      d <- d %>% dplyr::filter(BatterSideSeq %in% hand)
      pa_last <- pa_last %>% dplyr::filter(BatterSideSeq %in% hand)
    }
    if (!nrow(d) || !nrow(pa_last)) {
      return(data.frame(Status = "No data"))
    }

    row_defs <- list(
      "0-0 count"      = function(df) df$ctx_00 %in% TRUE,
      "Pitchers count" = function(df) df$ctx_pitchers %in% TRUE,
      "Hitters count"  = function(df) df$ctx_hitters %in% TRUE,
      "Even count"     = function(df) df$ctx_even %in% TRUE,
      "2 Strikes"      = function(df) df$ctx_2str %in% TRUE,
      "Doubled Up"     = function(df) df$DoubledUp %in% TRUE,
      "Tripled Up"     = function(df) df$TripledUp %in% TRUE
    )

    recs_for_row <- function(cond_d, cond_pa) {
      stats_by_pt <- lapply(arsenal, function(pt) {
        d_sub <- d[d$PitchType == pt & cond_d, , drop = FALSE]
        pa_sub <- pa_last[pa_last$PitchType == pt & cond_pa, , drop = FALSE]
        stats <- calc_seq_metrics(d_sub, pa_sub)
        prv <- calc_prv_context(d_sub, pa_sub)
        tibble::tibble(
          PitchType = pt,
          Whiff_pct  = stats$Whiff_pct,
          CSW_pct    = stats$CSW_pct,
          Swing_pct  = stats$Swing_pct,
          SLG        = stats$SLG,
          wOBA       = stats$wOBA,
          Barrel_pct = stats$Barrel_pct,
          Chase_pct  = stats$Chase_pct,
          GB_pct     = stats$GB_pct,
          pRV        = prv
        )
      }) %>% dplyr::bind_rows()

      if (!nrow(stats_by_pt)) return(list(best = "NA", worst = "NA"))

      r_whiff  <- percent_rank_dir(stats_by_pt$Whiff_pct,  higher_is_better = TRUE)
      r_csw    <- percent_rank_dir(stats_by_pt$CSW_pct,    higher_is_better = TRUE)
      r_swing  <- percent_rank_dir(stats_by_pt$Swing_pct,  higher_is_better = TRUE)
      r_chase  <- percent_rank_dir(stats_by_pt$Chase_pct,  higher_is_better = TRUE)
      r_gb     <- percent_rank_dir(stats_by_pt$GB_pct,     higher_is_better = TRUE)
      r_slg    <- percent_rank_dir(stats_by_pt$SLG,        higher_is_better = FALSE)
      r_woba   <- percent_rank_dir(stats_by_pt$wOBA,       higher_is_better = FALSE)
      r_barrel <- percent_rank_dir(stats_by_pt$Barrel_pct, higher_is_better = FALSE)

      score <- rowMeans(cbind(r_whiff, r_csw, r_swing, r_chase, r_gb, r_slg, r_woba, r_barrel), na.rm = TRUE)
      score[!is.finite(score)] <- NA_real_

      if (!any(is.finite(score))) {
        return(list(best = "NA", worst = "NA", best_prv = "NA", worst_prv = "NA"))
      }

      best_idx  <- which.max(ifelse(is.finite(score), score, -Inf))
      worst_idx <- which.min(ifelse(is.finite(score), score,  Inf))

      prv_vec <- stats_by_pt$pRV
      if (any(is.finite(prv_vec))) {
        # For pitchers, lower pRV is better (more negative).
        best_prv_idx  <- which.min(ifelse(is.finite(prv_vec), prv_vec,  Inf))
        worst_prv_idx <- which.max(ifelse(is.finite(prv_vec), prv_vec, -Inf))
        best_prv_pitch  <- stats_by_pt$PitchType[best_prv_idx]
        worst_prv_pitch <- stats_by_pt$PitchType[worst_prv_idx]
        best_prv_val  <- prv_vec[best_prv_idx]
        worst_prv_val <- prv_vec[worst_prv_idx]
        best_prv  <- ifelse(is.finite(best_prv_val),
                            sprintf("%s (%.2f)", best_prv_pitch, best_prv_val),
                            best_prv_pitch)
        worst_prv <- ifelse(is.finite(worst_prv_val),
                            sprintf("%s (%.2f)", worst_prv_pitch, worst_prv_val),
                            worst_prv_pitch)
      } else {
        best_prv <- "NA"
        worst_prv <- "NA"
      }

      list(
        best  = stats_by_pt$PitchType[best_idx],
        worst = stats_by_pt$PitchType[worst_idx],
        best_prv  = best_prv,
        worst_prv = worst_prv
      )
    }

    rows <- lapply(names(row_defs), function(lbl) {
      cond_d  <- row_defs[[lbl]](d)
      cond_pa <- row_defs[[lbl]](pa_last)
      rec <- recs_for_row(cond_d, cond_pa)
      tibble::tibble(
        Context = lbl,
        `Best Pitch`  = rec$best,
        `Worst Pitch` = rec$worst,
        `Best Pitch (pRV)`  = rec$best_prv,
        `Worst Pitch (pRV)` = rec$worst_prv
      )
    })

    after_rows <- lapply(arsenal, function(aft) {
      cond_d  <- d$PrevPitchType == aft
      cond_pa <- pa_last$PrevPitchType == aft
      rec <- recs_for_row(cond_d %in% TRUE, cond_pa %in% TRUE)
      tibble::tibble(
        Context = paste("After", aft),
        `Best Pitch`  = rec$best,
        `Worst Pitch` = rec$worst,
        `Best Pitch (pRV)`  = rec$best_prv,
        `Worst Pitch (pRV)` = rec$worst_prv
      )
    })

    dplyr::bind_rows(rows, after_rows)
  }

  # ---- Leaderboard (all TXST pitchers; independent of sidebar) ----
  leaderboard_data <- reactive({
    d <- txst_df
    if (is.null(d) || !nrow(d)) return(d)

    # Filter out "Bad data" pitch types (keep leaderboard + exports aligned)
    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }

    # Exclude pitchers
    excl <- input$leader_exclude %||% character(0)
    if (length(excl) && "Pitcher" %in% names(d)) {
      d <- d %>% dplyr::filter(!(Pitcher %in% excl))
    }

    # Season filter
    season_col <- if ("SeasonTag" %in% names(d)) "SeasonTag" else if ("SeasonGroup" %in% names(d)) "SeasonGroup" else NULL
    sel_seasons <- input$leader_seasons %||% character(0)
    if (!is.null(season_col) && length(sel_seasons)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sel_seasons)
    }

    # Batter hand filter
    sel_hands <- input$leader_hand %||% character(0)
    if (length(sel_hands) && "BatterSide" %in% names(d)) {
      d <- d %>%
        dplyr::mutate(
          BatterSideStd = dplyr::case_when(
            BatterSide %in% c("L","Left","LHH","LH") ~ "L",
            BatterSide %in% c("R","Right","RHH","RH") ~ "R",
            TRUE ~ as.character(BatterSide)
          )
        ) %>%
        dplyr::filter(.data$BatterSideStd %in% sel_hands)
    }

    # Date range filter (GameDate -> Date -> CustomGameID fallback)
    date_vec <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("PitchDate" %in% names(d)) {
      parse_date_any(d$PitchDate)
    } else if ("CustomGameID" %in% names(d)) {
      parse_gameid_date(d$CustomGameID)
    } else {
      as.Date(NA)
    }

    if (!is.null(input$leader_dates) && length(input$leader_dates) == 2) {
      d_start <- as.Date(input$leader_dates[[1]])
      d_end   <- as.Date(input$leader_dates[[2]])
      if (is.finite(d_start) || is.finite(d_end)) {
        keep <- rep(TRUE, nrow(d))
        if (is.finite(d_start)) keep <- keep & (date_vec >= d_start)
        if (is.finite(d_end))   keep <- keep & (date_vec <= d_end)
        d <- d[keep, , drop = FALSE]
      }
    }

    # Deduplicate pitches to avoid inflated PA counts
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)

    # Minimum PA filter (per pitcher)
    min_pa <- input$leader_min_pa %||% 0
    if (is.numeric(min_pa) && min_pa > 0) {
      tmp <- ensure_pa(d)
      pa_counts <- tmp %>%
        dplyr::group_by(Pitcher, PA_ID) %>%
        dplyr::summarise(.groups = "drop") %>%
        dplyr::group_by(Pitcher) %>%
        dplyr::summarise(PA = dplyr::n(), .groups = "drop")
      keep <- pa_counts$Pitcher[pa_counts$PA >= min_pa]
      d <- d %>% dplyr::filter(Pitcher %in% keep)
    }
    d
  })
  
  # ---- Leaderboard summary (numeric, for exports) ----
  leaderboard_summary <- function(d) {
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    
    p <- prepare_aar_flags(d)
    if ("pitch_uid" %in% names(p)) p <- p %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(p)) p <- p %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    # Ensure PA_ID is unique per pitcher to avoid cross-pitcher collisions
    p$PA_ID_LB <- interaction(p$Pitcher, p$PA_ID, drop = TRUE)
    if (!"Pitcher" %in% names(p)) return(tibble::tibble())
    p$grp <- as.character(p$Pitcher)
    p <- p %>% dplyr::filter(!is.na(.data$grp) & nzchar(.data$grp))
    if (!nrow(p)) return(tibble::tibble())
    
    # --- helper: EV/LA detection (match leaderboard table fallback logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }
    
    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la
    
    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    
    # PA-ending K pitch flag (vector, length = nrow(p))
    is_k_pitch_vec <- calc_is_k_pitch(p, "PA_ID_LB", "PitchNum")
    
    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,
        
        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),
        
        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .twok_no32 = (!is.na(StrikesPre) & StrikesPre == 2) & (!is.na(BallsPre) & BallsPre != 3),
        .is_k_pitch = is_k_pitch_vec
      )

    
    pitch_rates <- p_flags %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),
        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),
        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),
        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },
        TwoKZone_pct = {
          den <- sum(.twok_no32 & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok_no32 & .inz, na.rm = TRUE), den) else NA_real_
        },
        PutAway_pct = {
          den <- sum(.twok, na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok & .is_k_pitch, na.rm = TRUE), den) else NA_real_
        },
        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches)
      )

    
    pa_first <- p %>% dplyr::group_by(PA_ID_LB) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    pa_group <- pa_first %>% dplyr::transmute(PA_ID_LB, grp = .data$Pitcher)
    
    fps_pa <- p %>%
      dplyr::group_by(PA_ID_LB) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::left_join(pa_group, by = "PA_ID_LB")
    
    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID_LB") %>%
      dplyr::left_join(pa_group, by = "PA_ID_LB")
    
    pa_last <- p %>% dplyr::group_by(PA_ID_LB) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    pa_last <- p %>% dplyr::group_by(PA_ID_LB) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    pa_out <- pa_last %>%
      dplyr::mutate(grp = pa_group$grp[match(PA_ID_LB, pa_group$PA_ID_LB)]) %>%
      {
        outc <- pa_outcome_cols(.)
        pr <- outc$PR_txt
        kb <- as.character(.$KorBB %||% "")
        any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
        tibble::tibble(
          PA_ID_LB = .$PA_ID_LB,
          grp   = .$grp,
          K     = outc$K,
          BB    = (any_walk)
        )
      }
    
    pa_rates <- pa_out %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        PA   = dplyr::n(),
        Kp   = mean(K,     na.rm = TRUE),
        BBp  = mean(BB,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        fps_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                                                             .groups = "drop"),
        by = "grp"
      ) %>%
      dplyr::left_join(
        ea_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(
          EA = calc_ea_rate_from_pa(tibble::tibble(
            n_pitches = n_pitches,
            strikes_first3 = strikes_first3,
            early_bip = early_bip,
            any_hbp = any_hbp,
            any_barrel = any_barrel
          )),
          .groups = "drop"
        ),
        by = "grp"
      )
    
    max_velo <- p %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(MaxVelocity = suppressWarnings(max(as.numeric(RelSpeed), na.rm = TRUE)),
                       .groups = "drop")
    
    sd_tbl <- compute_shutdown_by_pitcher(p, d_all = df_nonbp) %>%
      dplyr::select(Pitcher, ShutDown_pct)
    
    # --- Avg/Max velo from primary FB vs Sinker (higher usage pitch only) ---
    pitch_grp <- function(pt) {
      x <- tolower(trimws(as.character(pt)))
      fb <- x %in% c("fastball","four-seam","four seam","4-seam","4 seam","four-seam fastball","4-seam fastball")
      si <- x %in% c("sinker","two-seam","two seam","2-seam","2 seam","two-seam fastball","2-seam fastball")
      ifelse(fb, "FB", ifelse(si, "SI", NA_character_))
    }
    
    fbsi <- p %>%
      dplyr::mutate(
        .fs_grp = pitch_grp(PitchType),
        .velo = suppressWarnings(as.numeric(RelSpeed))
      ) %>%
      dplyr::filter(!is.na(.fs_grp), is.finite(.velo)) %>%
      dplyr::group_by(grp, .fs_grp) %>%
      dplyr::summarise(
        n = dplyr::n(),
        AvgVelo = mean(.velo, na.rm = TRUE),
        MaxVelo = max(.velo,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(grp, dplyr::desc(n), dplyr::desc(AvgVelo)) %>%
      dplyr::group_by(grp) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        AvgVelo_fmt = ifelse(is.finite(AvgVelo) & is.finite(MaxVelo),
                             sprintf("%.1f (%.1f)", AvgVelo, MaxVelo), "NA")
      ) %>%
      dplyr::select(grp, AvgVelo, AvgVelo_fmt)
    
    sd_tbl <- compute_shutdown_by_pitcher(p, d_all = df_nonbp) %>%
      dplyr::select(Pitcher, ShutDown_pct)
    
    pitch_rates %>%
      dplyr::left_join(pa_rates, by = "grp") %>%
      dplyr::left_join(fbsi, by = "grp") %>%
      dplyr::left_join(max_velo, by = "grp") %>%
      dplyr::left_join(sd_tbl, by = c("grp" = "Pitcher")) %>%
      dplyr::mutate(Pitcher = grp) %>%
      dplyr::select(
        Pitcher, PA, Kp, BBp, Barrel_pct, AvgVelo, AvgVelo_fmt, MaxVelocity,
        Whiff_pct, CSW_pct, Strike_pct, Zone_pct, Pre2kZone_pct, TwoKZone_pct, PutAway_pct, ShutDown_pct, FPS, EA
      )
  }
  
  render_leaderboard_pdf <- function(outfile, type, data_df) {
    type <- match.arg(type, c("results","process"))
    
    maroon <- "#501214"
    gold   <- "#B4975A"
    date_str <- format(Sys.Date(), "%m-%d-%Y")
    
    logo_path <- "www/baseballTS logo gold.png"
    logo_img <- NULL
    if (file.exists(logo_path)) {
      logo_img <- tryCatch(png::readPNG(logo_path), error = function(e) NULL)
    }
    
    top10_tbl <- function(df, col, label, higher = TRUE, fmt = function(x) x, display_col = NULL) {
      if (is.null(df) || !nrow(df) || !"Pitcher" %in% names(df) || !(col %in% names(df))) {
        blank <- data.frame(Pitcher = rep(" ", 10), Value = rep(" ", 10), stringsAsFactors = FALSE)
        return(list(display = blank, values = numeric(0)))
      }
      
      v <- df[[col]]
      n <- nrow(df)
      if (length(v) == 1L && n > 1L) v <- rep(v, n)
      if (length(v) != n) {
        n2 <- min(length(v), n)
        df <- df[seq_len(n2), , drop = FALSE]
        v <- v[seq_len(n2)]
      }
      v_num <- suppressWarnings(as.numeric(v))
      ok <- is.finite(v_num)
      df <- df[ok, , drop = FALSE]
      v_num <- v_num[ok]
      if (!nrow(df)) return(data.frame(Pitcher = character(0), Value = character(0)))
      ord <- order(if (higher) -v_num else v_num, df$Pitcher, na.last = TRUE)
      df <- df[ord, , drop = FALSE]
      df <- head(df, 10)
      value_vec <- if (!is.null(display_col) && display_col %in% names(df)) df[[display_col]] else fmt(df[[col]])
      out <- data.frame(
        Pitcher = df$Pitcher,
        Value = value_vec,
        stringsAsFactors = FALSE
      )
      if (nrow(out) < 10) {
        pad <- data.frame(
          Pitcher = rep(" ", 10 - nrow(out)),
          Value = rep(" ", 10 - nrow(out)),
          stringsAsFactors = FALSE
        )
        out <- rbind(out, pad)
      }
      list(display = out, values = df[[col]])
    }
    
    fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "NA")
    
    defs_results <- list(
      # Row 1 (left -> right): K%, BB%, Barrel%
      list(name = "K%",        col = "Kp",         higher = TRUE,  fmt = fmt_pct),
      list(name = "BB%",       col = "BBp",        higher = FALSE, fmt = fmt_pct),
      list(name = "Barrel%",   col = "Barrel_pct", higher = FALSE, fmt = fmt_pct),
      # Row 2 (left -> right): Whiff%, CSW%, Avg Velo
      list(name = "Whiff%",    col = "Whiff_pct",  higher = TRUE,  fmt = fmt_pct),
      list(name = "CSW%",      col = "CSW_pct",    higher = TRUE,  fmt = fmt_pct),
      list(name = "Avg Velo",  col = "AvgVelo",    higher = TRUE,  fmt = fmt_num1, display_col = "AvgVelo_fmt")
    )
    defs_process <- list(
      list(name = "Strike%",     col = "Strike_pct",   higher = TRUE, fmt = fmt_pct),
      list(name = "Zone%",       col = "Zone_pct",     higher = TRUE, fmt = fmt_pct),
      list(name = "Pre2k Zone%", col = "Pre2kZone_pct",higher = TRUE, fmt = fmt_pct),
      list(name = "FPS%",        col = "FPS",          higher = TRUE, fmt = fmt_pct),
      list(name = "E&A%",        col = "EA",           higher = TRUE, fmt = fmt_pct),
      list(name = "Put Away%",   col = "PutAway_pct",  higher = TRUE, fmt = fmt_pct)
    )
    
    defs <- if (type == "results") defs_results else defs_process
    
    make_stat_block <- function(title, df_info) {
      df <- df_info$display
      
      # zebra fills (Pitcher col only) + conditional green for Value col
      n <- nrow(df)
      zebra1 <- "#FFFFFF"
      zebra2 <- "#F5F5F5"
      fills <- matrix(rep(c(zebra1, zebra2), length.out = n), nrow = n, ncol = 2, byrow = FALSE)
      
      # compute green mask only for the Value column
      val_num <- suppressWarnings(as.numeric(gsub("%", "", df$Value))) / 100
      if (title %in% names(D1_PCT_AVG)) {
        avg <- D1_PCT_AVG[[title]]
        if (is.finite(avg)) {
          lower_better <- title %in% c("BB%","Barrel%")
          v_pp <- val_num * 100
          avg_pp <- avg * 100
          lo <- avg_pp - 5
          hi <- avg_pp + 5
          if (isTRUE(lower_better)) {
            good <- v_pp < lo
            bad  <- v_pp > hi
          } else {
            good <- v_pp > hi
            bad  <- v_pp < lo
          }
          # grid does not accept rgba() strings; use solid hex for PDF
          fills[good, 2] <- "#D6EBD3"
          fills[bad,  2] <- "#F3B9B9"
        }
      }
      
      title_g <- grid::grobTree(
        grid::rectGrob(gp = grid::gpar(fill = maroon, col = NA)),
        grid::textGrob(title, gp = grid::gpar(col = gold, fontsize = 8, fontface = "bold"))
      )
      tbl_g <- gridExtra::tableGrob(
        df,
        rows = NULL,
        cols = NULL,
        theme = gridExtra::ttheme_minimal(
          base_size = 6,
          colhead = list(fg_params = list(fontface = "bold")),
          core = list(
            fg_params = list(hjust = 0, x = 0.02),
            bg_params = list(fill = fills, col = NA)
          )
        )
      )
      # align Pitcher column center, Value column right
      core_idx <- which(tbl_g$layout$name == "core-fg")
      if (length(core_idx)) {
        l_vals <- sort(unique(tbl_g$layout$l[core_idx]))
        if (length(l_vals) >= 2) {
          pitch_col_l <- l_vals[1]
          val_col_l   <- l_vals[2]
          idx_pitch <- which(tbl_g$layout$name == "core-fg" & tbl_g$layout$l == pitch_col_l)
          for (i in idx_pitch) {
            g <- tbl_g$grobs[[i]]
            g$just <- "center"
            g$x <- grid::unit(0.5, "npc")
            tbl_g$grobs[[i]] <- g
          }
          idx_val <- which(tbl_g$layout$name == "core-fg" & tbl_g$layout$l == val_col_l)
          for (i in idx_val) {
            g <- tbl_g$grobs[[i]]
            g$just <- "center"
            g$x <- grid::unit(0.5, "npc")
            tbl_g$grobs[[i]] <- g
          }
        }
      }
      # two columns: Pitcher 2/3 + Value 1/3 (bounded to table width)
      tbl_g$widths <- grid::unit(c(0.67, 0.33), "npc")
      
      # bold leader names/values (including ties)
      core_idx <- which(tbl_g$layout$name == "core-fg")
      if (length(core_idx)) {
        l_vals <- sort(unique(tbl_g$layout$l[core_idx]))
        t_vals <- sort(unique(tbl_g$layout$t[core_idx]))
        if (length(l_vals) >= 2 && length(t_vals) >= 1) {
          pitch_col_l <- l_vals[1]
          val_col_l   <- l_vals[2]
          # build leader value (numeric) from the displayed data
          val_num <- suppressWarnings(as.numeric(gsub("%","", df$Value)))
          if (title %in% c("K%","BB%","Barrel%","Whiff%","CSW%","Strike%","Zone%","Pre2k Zone%","FPS%","E&A%","Put Away%")) {
            val_num <- val_num / 100
          }
          if (title == "Avg Velo") {
            val_num <- suppressWarnings(readr::parse_number(df$Value))
          }
          if (length(val_num)) {
            if (title %in% c("BB%","Barrel%")) {
              top_val <- suppressWarnings(min(val_num, na.rm = TRUE))
            } else {
              top_val <- suppressWarnings(max(val_num, na.rm = TRUE))
            }
            top_rows <- which(is.finite(val_num) & val_num == top_val)
            top_t <- t_vals[top_rows]
            if (length(top_t)) {
              idx_bold <- which(tbl_g$layout$name == "core-fg" &
                                  tbl_g$layout$t %in% top_t &
                                  tbl_g$layout$l %in% c(pitch_col_l, val_col_l))
              if (length(idx_bold)) {
                for (i in idx_bold) {
                  g <- tbl_g$grobs[[i]]
                  g$gp$font <- NULL
                  g$gp$fontface <- "bold"
                  tbl_g$grobs[[i]] <- g
                }
              }
            }
          }
        }
      }
      gridExtra::arrangeGrob(title_g, tbl_g, ncol = 1, heights = c(0.12, 0.88))
    }
    
    blocks <- lapply(defs, function(d) {
      df_top <- top10_tbl(
        data_df,
        d$col,
        d$name,
        higher = d$higher,
        fmt = d$fmt,
        display_col = d$display_col
      )
      make_stat_block(d$name, df_top)
    })
    
    # 3x2 grid (3 columns, 2 rows) with explicit 30% width / 40% height per table
    grid_with_spacers <- function(grobs, ncol = 3, nrow = 2,
                                  col_widths = c(0.30, 0.05, 0.30, 0.05, 0.30),
                                  row_heights = c(0.40, 0.10, 0.40)) {
      total_cells <- ncol * nrow
      grobs <- c(grobs, rep(list(grid::nullGrob()), max(0, total_cells - length(grobs))))
      
      cols <- 2 * ncol - 1
      rows <- 2 * nrow - 1
      mat <- matrix(list(grid::nullGrob()), nrow = rows, ncol = cols)
      
      gi <- 1
      for (r in seq_len(nrow)) {
        for (c in seq_len(ncol)) {
          mat[2*r - 1, 2*c - 1] <- list(grobs[[gi]])
          gi <- gi + 1
        }
      }
      
      widths <- grid::unit(col_widths, "npc")
      heights <- grid::unit(row_heights, "npc")
      
      # arrangeGrob fills by row; use row-major vector from matrix
      gridExtra::arrangeGrob(grobs = as.vector(t(mat)), ncol = cols,
                             widths = widths, heights = heights)
    }
    
    grid_body <- grid_with_spacers(blocks)
    
    header_title <- if (type == "results") "Staff Results Leaderboard" else "Staff Process Leaderboard"
    header_g <- grid::grobTree(
      if (!is.null(logo_img)) grid::rasterGrob(logo_img, x = 0.05, y = 0.60, width = 0.08, just = c("left","center")) else grid::nullGrob(),
      if (!is.null(logo_img)) grid::rasterGrob(logo_img, x = 0.95, y = 0.60, width = 0.08, just = c("right","center")) else grid::nullGrob(),
      grid::textGrob(header_title, x = 0.5, y = 0.75,
                     gp = grid::gpar(fontsize = 20, fontface = "bold")),
      grid::textGrob(paste0("Updated ", date_str), x = 0.5, y = 0.40,
                     gp = grid::gpar(fontsize = 11))
    )
    
    grDevices::pdf(outfile, width = 11, height = 8.5, useDingbats = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    gridExtra::grid.arrange(
      header_g,
      grid_body,
      ncol = 1,
      heights = c(0.18, 0.82)
    )
  }
  
  
  # ---- AAR: populate pitcher choices (reuse your canonical list) ----
  observe({
    pitchers <- setdiff(sort(unique(txst_df$Pitcher)), EXCLUDE_PLAYERS)
    if (length(pitchers)) updateSelectInput(session, "aar_pitcher", choices = as_pitcher_choices(pitchers), selected = pitchers[1])
  })

  observe({
    pitchers <- setdiff(sort(unique(txst_df$Pitcher)), EXCLUDE_PLAYERS)
    updateSelectizeInput(
      session,
      "leader_exclude",
      choices = pitchers,
      selected = intersect(input$leader_exclude %||% character(0), pitchers),
      server = TRUE
    )
  })
  
  # ---- AAR: game dropdown respects the report's season selection ----
  observeEvent(list(input$aar_pitcher, aar_season_group_values()), {
    req(input$aar_pitcher)
    d <- txst_df %>%
      dplyr::filter(Pitcher == input$aar_pitcher, !(is_bullpen %in% TRUE))
    
    sg <- aar_season_group_values()
    season_col <- if ("SeasonTag" %in% names(d)) "SeasonTag" else if ("SeasonGroup" %in% names(d)) "SeasonGroup" else NULL
    if (!is.null(season_col) && length(sg)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sg)
    }
    
    games <- d %>% dplyr::pull(CustomGameID) %>% unique()
    games <- order_game_ids_desc(games, game_dates)
    
    shinyWidgets::updatePickerInput(session, "aar_game",
                                    choices  = games,
                                    selected = if (length(games)) games[[1]] else character(0)
    )
  }, ignoreInit = TRUE)

  catcher_allowed <- c("Austin Munguia", "Clayton Namken", "Kade Thompson", "Rashawn Galloway")
  
  catcher_receiving_pool <- reactive({
    d <- txst_df
    if (!nrow(d)) d <- df_nonbp
    d <- prefer_team_or_portal(d, "CatcherTeam")
    if ("is_bullpen" %in% names(d)) {
      d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))
    }
    d
  })
  
  catchers_all <- reactive({
    d <- catcher_receiving_pool()
    if (!nrow(d) || !("Catcher" %in% names(d))) return(character(0))
    
    ch_orig <- unique(nz_chr(d$Catcher))
    ch_disp <- name_display(ch_orig)
    keep <- name_norm(ch_orig) %in% name_norm(catcher_allowed)
    ch_orig <- ch_orig[keep]
    ch_disp <- ch_disp[keep]
    if (!length(ch_orig)) return(character(0))
    
    ord <- order(ch_disp)
    stats::setNames(ch_orig[ord], ch_disp[ord])
  })
  
  games_for_catcher <- reactive({
    cch <- input$AARCatcher
    d <- catcher_receiving_pool()
    if (is.null(cch) || !nzchar(cch) || !nrow(d) || !("Catcher" %in% names(d))) {
      return(tibble::tibble(gid = character(0), gdate = as.Date(character(0)), season = character(0)))
    }
    
    dd <- d %>% dplyr::filter(.data$Catcher == cch)
    gd <- dplyr::coalesce(dd$GameDate, parse_date_any(dd$Date), extract_date_from_filename(dd$source_file))
    season_col <- if ("SeasonTag" %in% names(dd)) "SeasonTag" else if ("SeasonGroup" %in% names(dd)) "SeasonGroup" else NULL
    sg <- if (!is.null(season_col)) as.character(dd[[season_col]]) else NA_character_
    
    tibble::tibble(gid = dd$CustomGameID, gdate = gd, season = sg) %>%
      dplyr::distinct(.data$gid, .data$gdate, .data$season) %>%
      dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  })
  
  games_for_catcher_multi <- reactive({
    cch <- input$CR_catcher
    d <- catcher_receiving_pool()
    if (is.null(cch) || !nzchar(cch) || !nrow(d) || !("Catcher" %in% names(d))) {
      return(tibble::tibble(gid = character(0), gdate = as.Date(character(0)), season = character(0)))
    }
    
    dd <- d %>% dplyr::filter(.data$Catcher == cch)
    gd <- dplyr::coalesce(dd$GameDate, parse_date_any(dd$Date), extract_date_from_filename(dd$source_file))
    season_col <- if ("SeasonTag" %in% names(dd)) "SeasonTag" else if ("SeasonGroup" %in% names(dd)) "SeasonGroup" else NULL
    sg <- if (!is.null(season_col)) as.character(dd[[season_col]]) else NA_character_
    
    tibble::tibble(gid = dd$CustomGameID, gdate = gd, season = sg) %>%
      dplyr::distinct(.data$gid, .data$gdate, .data$season) %>%
      dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  })
  
  all_game_choices_for_catcher <- reactive({
    gf <- games_for_catcher()
    if (!nrow(gf)) return(character(0))
    as.character(gf$gid)
  })
  
  all_game_choices_for_catcher_multi <- reactive({
    gf <- games_for_catcher_multi()
    if (!nrow(gf)) return(character(0))
    as.character(gf$gid)
  })
  
  season_selected_catcher_game_ids <- reactive({
    ch <- all_game_choices_for_catcher_multi()
    sg <- input$season_groups %||% character(0)
    if (!length(sg)) return(character(0))
    
    gf <- games_for_catcher_multi()
    if (!("season" %in% names(gf))) return(character(0))
    
    sel <- gf$gid[gf$season %in% sg]
    sel <- sel[!is.na(sel) & nzchar(trimws(sel))]
    sel <- intersect(ch, unique(sel))
    if (!length(sel)) return(character(0))
    
    order_game_ids_desc(sel, game_dates)
  })
  
  observeEvent(catchers_all(), {
    ch <- catchers_all()
    if (!length(ch)) {
      updateSelectInput(session, "AARCatcher", choices = character(0), selected = NULL)
      updateSelectInput(session, "CR_catcher", choices = character(0), selected = NULL)
      return()
    }
    
    first_choice <- unname(ch[[1]])
    updateSelectInput(session, "AARCatcher", choices = ch, selected = first_choice)
    updateSelectInput(session, "CR_catcher", choices = ch, selected = first_choice)
  }, ignoreInit = FALSE)
  
  observeEvent(input$AARCatcher, {
    ch <- all_game_choices_for_catcher()
    updateSelectInput(
      session,
      "AARCatchGame",
      choices = as_named_choices(ch),
      selected = if (length(ch)) ch[[1]] else NULL
    )
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$CR_catcher, input$season_groups), {
    ch <- all_game_choices_for_catcher_multi()
    if (!length(ch)) {
      updatePickerInput(session, "CR_games", choices = character(0), selected = character(0))
      return()
    }
    
    updatePickerInput(
      session,
      "CR_games",
      choices = as_named_choices(ch),
      selected = season_selected_catcher_game_ids()
    )
  }, ignoreInit = FALSE)
  
  aar_catch_data <- reactive({
    req(input$AARCatcher, input$AARCatchGame)
    
    d <- catcher_receiving_pool() %>%
      dplyr::mutate(.catcher_norm = name_norm(.data$Catcher)) %>%
      dplyr::filter(.data$.catcher_norm %in% name_norm(catcher_allowed)) %>%
      dplyr::filter(.data$Catcher == input$AARCatcher, .data$CustomGameID == input$AARCatchGame) %>%
      dplyr::select(-".catcher_norm")
    
    validate(need(nrow(d) > 0, "No rows in selected game for this catcher."))
    prepare_catcher_receiving_rows(d)
  })
  
  cr_catch_data <- reactive({
    req(input$CR_catcher)
    
    d <- catcher_receiving_pool() %>%
      dplyr::mutate(.catcher_norm = name_norm(.data$Catcher)) %>%
      dplyr::filter(.data$.catcher_norm %in% name_norm(catcher_allowed)) %>%
      dplyr::filter(.data$Catcher == input$CR_catcher) %>%
      dplyr::select(-".catcher_norm")
    
    sel_games <- input$CR_games
    if (is.null(sel_games) || !length(sel_games)) return(d[0, , drop = FALSE])
    d <- d %>% dplyr::filter(.data$CustomGameID %in% sel_games)
    
    if (!is.null(input$CR_pitcher_hand) && input$CR_pitcher_hand != "All" && "PitcherHand" %in% names(d)) {
      d <- d %>% dplyr::filter(.data$PitcherHand == input$CR_pitcher_hand)
    }
    
    d <- prepare_catcher_receiving_rows(d)
    
    pt_sel <- input$CR_pitch_types
    if (is.null(pt_sel) || !length(pt_sel)) return(d[0, , drop = FALSE])
    d %>% dplyr::filter(as.character(.data$PitchType) %in% pt_sel)
  })
  
  catch_ball_to_strike <- reactive({
    catcher_framing_subset(aar_catch_data(), "ball_to_strike")
  })
  
  catch_strike_to_ball <- reactive({
    catcher_framing_subset(aar_catch_data(), "strike_to_ball")
  })
  
  cr_ball_to_strike <- reactive({
    catcher_framing_subset(cr_catch_data(), "ball_to_strike")
  })
  
  cr_strike_to_ball <- reactive({
    catcher_framing_subset(cr_catch_data(), "strike_to_ball")
  })
  
  output$catcher_ball_to_strike_plot <- renderPlot({
    catcher_framing_zone_plot(catch_ball_to_strike(), title = "Ball to Strike")
  })
  
  output$catcher_strike_to_ball_plot <- renderPlot({
    catcher_framing_zone_plot(catch_strike_to_ball(), title = "Strike to Ball")
  })
  
  output$catcher_pitch_legend <- renderPlot({
    catcher_framing_legend_plot()
  })
  
  output$catcher_ball_to_strike_stats <- renderTable({
    tbl <- framing_zone_stats(aar_catch_data(), "ball_to_strike") %>%
      dplyr::filter(!(.data$Zone %in% c("Heart", "Zone")) | .data$Zone == "Total") %>%
      dplyr::mutate(
        `%` = dplyr::if_else(is.finite(.data$Pct), sprintf("%.1f%%", 100 * .data$Pct), "—"),
        Chances = as.integer(.data$Chances),
        `Strikes Stolen` = as.integer(.data$Strikes)
      ) %>%
      dplyr::select(.data$Zone, .data$Chances, `Strikes Stolen`, `%`)
    tbl
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  
  output$catcher_strike_to_ball_stats <- renderTable({
    tbl <- framing_zone_stats(aar_catch_data(), "strike_to_ball") %>%
      dplyr::filter(.data$Zone != "Chase" | .data$Zone == "Total") %>%
      dplyr::mutate(
        `%` = dplyr::if_else(is.finite(.data$Pct), sprintf("%.1f%%", 100 * .data$Pct), "—"),
        Chances = as.integer(.data$Chances),
        `Strikes Lost` = as.integer(.data$Strikes)
      ) %>%
      dplyr::select(.data$Zone, .data$Chances, `Strikes Lost`, `%`)
    tbl
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  
  output$catcher_ball_to_strike_tbl <- DT::renderDT({
    tbl <- catcher_framing_table(catch_ball_to_strike())
    dt <- DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
    if ("Result" %in% names(tbl)) {
      dt <- DT::formatStyle(
        dt, "Result",
        backgroundColor = DT::styleEqual(
          c("StrikeCalled", "BallCalled"),
          c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)")
        )
      )
    }
    dt
  })
  
  output$catcher_strike_to_ball_tbl <- DT::renderDT({
    tbl <- catcher_framing_table(catch_strike_to_ball())
    dt <- DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
    if ("Result" %in% names(tbl)) {
      dt <- DT::formatStyle(
        dt, "Result",
        backgroundColor = DT::styleEqual(
          c("StrikeCalled", "BallCalled"),
          c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)")
        )
      )
    }
    dt
  })
  
  output$cr_ball_to_strike_plot <- renderPlot({
    catcher_framing_zone_plot(cr_ball_to_strike(), title = "Ball to Strike")
  })
  
  output$cr_strike_to_ball_plot <- renderPlot({
    catcher_framing_zone_plot(cr_strike_to_ball(), title = "Strike to Ball")
  })
  
  output$cr_pitch_legend <- renderPlot({
    catcher_framing_legend_plot()
  })
  
  output$cr_ball_to_strike_stats <- renderTable({
    tbl <- framing_zone_stats(cr_catch_data(), "ball_to_strike") %>%
      dplyr::filter(!(.data$Zone %in% c("Heart", "Zone")) | .data$Zone == "Total") %>%
      dplyr::mutate(
        `%` = dplyr::if_else(is.finite(.data$Pct), sprintf("%.1f%%", 100 * .data$Pct), "—"),
        Chances = as.integer(.data$Chances),
        `Strikes Stolen` = as.integer(.data$Strikes)
      ) %>%
      dplyr::select(.data$Zone, .data$Chances, `Strikes Stolen`, `%`)
    tbl
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  
  output$cr_strike_to_ball_stats <- renderTable({
    tbl <- framing_zone_stats(cr_catch_data(), "strike_to_ball") %>%
      dplyr::filter(.data$Zone != "Chase" | .data$Zone == "Total") %>%
      dplyr::mutate(
        `%` = dplyr::if_else(is.finite(.data$Pct), sprintf("%.1f%%", 100 * .data$Pct), "—"),
        Chances = as.integer(.data$Chances),
        `Strikes Lost` = as.integer(.data$Strikes)
      ) %>%
      dplyr::select(.data$Zone, .data$Chances, `Strikes Lost`, `%`)
    tbl
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
  
  output$cr_ball_to_strike_tbl <- DT::renderDT({
    tbl <- catcher_framing_table(cr_ball_to_strike())
    dt <- DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
    if ("Result" %in% names(tbl)) {
      dt <- DT::formatStyle(
        dt, "Result",
        backgroundColor = DT::styleEqual(
          c("StrikeCalled", "BallCalled"),
          c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)")
        )
      )
    }
    dt
  })
  
  output$cr_strike_to_ball_tbl <- DT::renderDT({
    tbl <- catcher_framing_table(cr_strike_to_ball())
    dt <- DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
    if ("Result" %in% names(tbl)) {
      dt <- DT::formatStyle(
        dt, "Result",
        backgroundColor = DT::styleEqual(
          c("StrikeCalled", "BallCalled"),
          c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)")
        )
      )
    }
    dt
  })
  
  output$catcher_framing_pdf <- downloadHandler(
    filename = function() {
      d <- aar_catch_data()
      gdt_raw <- if (!is.null(d) && nrow(d) && "GameDate" %in% names(d)) sort(unique(d$GameDate))[1] else Sys.Date()
      gdt <- if (inherits(gdt_raw, "Date")) format(gdt_raw, "%Y-%m-%d") else as.character(gdt_raw)
      
      cch <- if (!is.null(input$AARCatcher)) input$AARCatcher else "Catcher"
      last <- if (grepl(",", cch)) {
        trimws(strsplit(cch, ",\\s*")[[1]][1])
      } else {
        parts <- strsplit(trimws(cch), "\\s+")[[1]]
        if (length(parts)) parts[length(parts)] else cch
      }
      last <- gsub("[^A-Za-z0-9_-]+", "", last)
      
      sprintf("%s_Receiving_%s.pdf", last, gdt)
    },
    content = function(file) {
      d <- aar_catch_data()
      validate(need(nrow(d) > 0, "No data for catcher receiving."))
      
      PAGE_W_IN <- 11.0
      PAGE_H_IN <- 8.5
      
      open_pdf_device <- function(path, w, h) {
        ok <- FALSE
        tryCatch({
          grDevices::cairo_pdf(path, width = w, height = h)
          ok <<- TRUE
        }, error = function(e) {}, warning = function(w) {})
        if (!ok) {
          grDevices::pdf(path, width = w, height = h, useDingbats = FALSE)
        }
      }
      open_pdf_device(file, PAGE_W_IN, PAGE_H_IN)
      
      d_left <- catcher_framing_subset(d, "ball_to_strike")
      d_right <- catcher_framing_subset(d, "strike_to_ball")
      
      gdates <- if ("GameDate" %in% names(d)) d$GameDate else parse_date_any(d$Date)
      gdate <- gdates[!is.na(gdates)][1]
      catcher_name <- name_display(if (is.null(input$AARCatcher)) "" else input$AARCatcher)
      if (is.na(gdate)) {
        hdr_txt <- sprintf("%s Catcher Receiving Scorecard", catcher_name)
      } else {
        hdr_txt <- sprintf("%s Catcher Receiving Scorecard - %s", catcher_name, format(parse_date_any(gdate), "%B %d, %Y"))
      }
      
      header <- grid::textGrob(hdr_txt, gp = grid::gpar(fontface = 2, cex = 1.15, col = "#501214"))
      
      p_left <- catcher_framing_zone_plot(d_left, title = "Ball to Strike")
      p_right <- catcher_framing_zone_plot(d_right, title = "Strike to Ball")
      p_legend <- catcher_framing_legend_plot()
      
      g_left <- ggplotGrob(p_left)
      g_right <- ggplotGrob(p_right)
      g_legend <- ggplotGrob(p_legend)
      
      tbl_left <- as.data.frame(catcher_framing_table(d_left))
      tbl_right <- as.data.frame(catcher_framing_table(d_right))
      
      want_cols <- c("P", "Inn", "PA", "PA P#", "Pitcher", "Pitch type", "Hitter", "Count", "Result")
      if (!all(want_cols %in% names(tbl_left))) {
        for (nm in setdiff(want_cols, names(tbl_left))) tbl_left[[nm]] <- character(0)
      }
      tbl_left <- tbl_left[, want_cols, drop = FALSE]
      
      if (!all(want_cols %in% names(tbl_right))) {
        for (nm in setdiff(want_cols, names(tbl_right))) tbl_right[[nm]] <- character(0)
      }
      tbl_right <- tbl_right[, want_cols, drop = FALSE]
      
      if (nrow(tbl_left) == 0) {
        blank <- as.list(rep("", length(want_cols)))
        names(blank) <- want_cols
        tbl_left <- rbind(tbl_left, blank)
      }
      if (nrow(tbl_right) == 0) {
        blank <- as.list(rep("", length(want_cols)))
        names(blank) <- want_cols
        tbl_right <- rbind(tbl_right, blank)
      }
      
      ttheme_tbl <- gridExtra::ttheme_minimal(
        core = list(fg_params = list(cex = 0.80, lineheight = 1.25), padding = grid::unit(c(3, 4), "pt")),
        colhead = list(
          fg_params = list(cex = 0.80, fontface = 2, col = "#B4975A", hjust = 0, x = 0.04),
          bg_params = list(fill = "#501214", col = NA)
        )
      )
      
      g_tbl_left <- gridExtra::tableGrob(tbl_left, rows = NULL, theme = ttheme_tbl)
      g_tbl_right <- gridExtra::tableGrob(tbl_right, rows = NULL, theme = ttheme_tbl)
      
      zebra_rows <- function(g) {
        lay <- g$layout
        core_bg <- which(lay$name == "core-bg")
        if (length(core_bg)) {
          first_row <- min(lay$t[core_bg])
          for (k in core_bg) {
            r <- lay$t[k] - first_row + 1
            g$grobs[[k]]$gp <- grid::gpar(fill = if (r %% 2) "#FFFFFF" else "#F7F7F7", col = NA)
          }
        }
        g
      }
      g_tbl_left <- zebra_rows(g_tbl_left)
      g_tbl_right <- zebra_rows(g_tbl_right)
      
      shade_result_col <- function(g, tbl) {
        if (!("Result" %in% names(tbl))) return(g)
        col_idx <- which(names(tbl) == "Result")
        if (!length(col_idx)) return(g)
        
        vals <- as.character(tbl$Result)
        if (!length(vals)) return(g)
        fills <- ifelse(
          vals == "StrikeCalled",
          grDevices::adjustcolor("forestgreen", alpha.f = 0.20),
          ifelse(vals == "BallCalled", grDevices::adjustcolor("red3", alpha.f = 0.20), "transparent")
        )
        
        lay <- g$layout
        core_bg <- which(lay$name == "core-bg" & lay$l == col_idx)
        if (length(core_bg)) {
          for (i in seq_along(core_bg)) {
            k <- core_bg[i]
            g$grobs[[k]]$gp <- grid::gpar(fill = fills[min(i, length(fills))], col = NA)
          }
        }
        g
      }
      g_tbl_left <- shade_result_col(g_tbl_left, tbl_left)
      g_tbl_right <- shade_result_col(g_tbl_right, tbl_right)
      
      clamp_tbl_width <- function(g, max_in) {
        w_in <- sum(grid::convertUnit(g$widths, "in", valueOnly = TRUE))
        if (is.finite(w_in) && w_in > max_in) {
          scale <- max_in / w_in
          g$widths <- g$widths * scale
        }
        g
      }
      g_tbl_left <- clamp_tbl_width(g_tbl_left, max_in = 4.5)
      g_tbl_right <- clamp_tbl_width(g_tbl_right, max_in = 4.5)
      
      layout <- grid::grid.layout(
        nrow = 3, ncol = 3,
        heights = grid::unit.c(grid::unit(0.08, "npc"), grid::unit(0.50, "npc"), grid::unit(0.42, "npc")),
        widths = grid::unit.c(grid::unit(4.75, "in"), grid::unit(1.0, "in"), grid::unit(4.75, "in"))
      )
      
      grid::pushViewport(grid::viewport(layout = layout))
      draw_in <- function(g, row, col) {
        grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
        grid::grid.draw(g)
        grid::popViewport()
      }
      
      draw_in(header, 1, 1:3)
      draw_in(g_left, 2, 1)
      draw_in(g_legend, 2, 2)
      draw_in(g_right, 2, 3)
      draw_in(g_tbl_left, 3, 1)
      draw_in(g_tbl_right, 3, 3)
      
      grid::popViewport()
      grDevices::dev.off()
    }
  )
  
  # Lasso -> open a modal with one dropdown per selected pitch
  session$onFlushed(function() {
    observeEvent(
      {
        req(isTRUE(rv$retag))
        plotly::event_data("plotly_selected", source = "pitch_metrics", priority = "event")
      },
      {
        sel <- plotly::event_data("plotly_selected", source = "pitch_metrics")
        if (is.null(sel) || nrow(sel) == 0) return(NULL)
        
        ids <- unique(as.integer(sel$key))
        if (!length(ids)) return(NULL)
        rv$last_sel <- ids
        
        # Choices = known palette + any levels present in data
        known <- names(pitch_colors)
        seen  <- levels(rv$df$PitchType); if (is.null(seen)) seen <- character(0)
        choices <- sort(unique(c(known, as.character(seen))))
        
        sel_df <- rv$df %>%
          dplyr::filter(.data$row_id %in% ids) %>%
          dplyr::select(row_id, pitch_uid, PitchType, RelSpeed, HorzBreak, InducedVertBreak, Date, CustomGameID)
        
        # Build a compact UI: one selectize per pitch
        rows_ui <- tagList(lapply(seq_len(nrow(sel_df)), function(i) {
          r <- sel_df[i, ]
          this_id <- paste0("retag_", r$row_id)
          div(class = "mb-2 p-2 border rounded",
              strong(sprintf("Row #%s", r$row_id)),
              HTML(sprintf(
                " &nbsp;|&nbsp; %s &nbsp;|&nbsp; %s &nbsp;|&nbsp; HB=%.1f, iVB=%.1f, V=%.1f",
                ifelse(is.na(r$Date), "", r$Date),
                ifelse(is.na(r$CustomGameID), "", r$CustomGameID),
                as.numeric(r$HorzBreak), as.numeric(r$InducedVertBreak), as.numeric(r$RelSpeed)
              )),
              br(),
              div(class = "mt-1",
                  tags$label("New pitch type"),
                  selectizeInput(
                    inputId  = this_id,
                    label    = NULL,
                    choices  = choices,
                    selected = as.character(r$PitchType),
                    # Allow typing a new tag; comment out 'create=TRUE' if you want to restrict to known list.
                    options  = list(create = TRUE, placeholder = "Choose or type a pitch type"),
                    width    = "100%"
                  )
              )
          )
        }))
        
        showModal(modalDialog(
          title = paste0("Re-Tag ", nrow(sel_df), " pitch", ifelse(nrow(sel_df) > 1, "es", "")),
          size  = "l",
          easyClose = TRUE,
          footer = tagList(
            actionButton("retag_apply", "Apply", class = "btn btn-primary"),
            modalButton("Cancel")
          ),
          div(style = "max-height:60vh; overflow-y:auto;", rows_ui)
        ))
      },
      ignoreInit = TRUE
    )
  }, once = TRUE)
  
  observeEvent(input$retag_apply, {
    req(rv$last_sel, length(rv$last_sel) > 0)
    
    ids <- rv$last_sel
    # Read all per-pitch selections; default to current if missing
    current_vals <- as.character(rv$df$PitchType[match(ids, rv$df$row_id)])
    new_vals <- vapply(ids, function(id) {
      val <- input[[paste0("retag_", id)]]
      if (is.null(val) || !nzchar(val)) current_vals[which(ids == id)] else as.character(val)
    }, FUN.VALUE = character(1))
    
    # Only keep changed ones
    changed <- which(new_vals != current_vals & !is.na(new_vals))
    if (!length(changed)) {
      removeModal()
      rv$last_sel <- integer(0)
      return(invisible(NULL))
    }
    
    ids_changed  <- ids[changed]
    vals_changed <- new_vals[changed]
    
    # Update rv$df safely (character -> assign -> refactor)
    tmp <- as.character(rv$df$PitchType)
    tmp[match(ids_changed, rv$df$row_id)] <- vals_changed
    rv$df$PitchType <- factor(
      tmp,
      levels = union(names(pitch_colors), sort(unique(tmp)))
    )
    
    removeModal()
    rv$last_sel <- integer(0)
    showNotification("Re-tags applied (session only).", type = "message", duration = 4)
  })
  
  # ---- Arm-angle line slope: cot(theta) = adj/opp in (iVB vs HB) space ----
  movement_line_slope <- function(hand, d, default_height_ft = 6 + 2/12) {
    rel_z <- suppressWarnings(mean(d$RelHeight, na.rm = TRUE))
    rel_x <- suppressWarnings(mean(d$RelSide,  na.rm = TRUE))
    height_ft <- default_height_ft
    if (exists("height_lookup", inherits = TRUE) && "Pitcher" %in% names(d)) {
      nm <- as.character(dplyr::first(d$Pitcher[!is.na(d$Pitcher)]))
      if (!is.null(nm) && nzchar(nm) && !is.null(height_lookup[[nm]])) height_ft <- as.numeric(height_lookup[[nm]])
    }
    if ("PitcherHeight" %in% names(d)) {
      h <- suppressWarnings(as.numeric(dplyr::first(d$PitcherHeight[!is.na(d$PitcherHeight)])))
      if (is.finite(h)) height_ft <- if (h > 8) h/12 else h
    }
    adj <- rel_z - 0.7 * height_ft; opp <- abs(rel_x)
    if (!is.finite(adj) || !is.finite(opp) || opp == 0 || adj <= 0) return(NA_real_)
    adj / opp
  }

  # ---- Arm-angle in degrees from release metrics ----
  arm_angle_deg_from_rel <- function(rel_height, rel_side, height_ft) {
    rel_height <- suppressWarnings(as.numeric(rel_height))
    rel_side   <- suppressWarnings(as.numeric(rel_side))
    height_ft  <- suppressWarnings(as.numeric(height_ft))
    adj <- rel_height - 0.7 * height_ft
    opp <- abs(rel_side)
    if (!is.finite(adj) || !is.finite(opp) || opp == 0 || adj <= 0) return(NA_real_)
    atan(opp / adj) * 180 / pi
  }

  resolve_pitcher_height_ft <- function(pitcher_name, height_ft = NA_real_, default_height_ft = 6 + 2/12) {
    # Prefer lookup if provided, then column value, then default.
    if (exists("height_lookup", inherits = TRUE)) {
      h <- height_lookup[[as.character(pitcher_name)]]
      h <- suppressWarnings(as.numeric(h))
      if (is.finite(h)) return(h)
    }
    h <- suppressWarnings(as.numeric(height_ft))
    if (is.finite(h)) return(if (h > 8) h/12 else h)
    default_height_ft
  }
  
  guess_throw_hand <- function(pitcher_name, d) {
    pitcher_name <- as.character(pitcher_name %||% "")
    h <- NA_character_
    if (exists("pitcher_hand_map", inherits = TRUE) && length(pitcher_hand_map) && nzchar(pitcher_name)) {
      hit <- pitcher_hand_map[pitcher_name]
      if (length(hit) && !is.na(hit[[1]]) && nzchar(as.character(hit[[1]]))) h <- as.character(hit[[1]])
    }
    if (!is.na(h) && nzchar(h)) return(h)
    cols <- intersect(names(d), c("PitcherThrows","Throws","PitcherHand","ThrowingHand"))
    if (length(cols)) {
      v <- toupper(trimws(as.character(stats::na.omit(d[[cols[1]]]))))
      if (length(v) && startsWith(v[1], "L")) return("LHP")
      if (length(v) && startsWith(v[1], "R")) return("RHP")
    }
    "RHP"
  }
  
  observeEvent(input$retag_mode, {
    rv$retag <- !isTRUE(rv$retag)
    updateActionButton(session, "retag_mode", label = if (rv$retag) "Exit Re-Tag" else "Re-Tag")
    if (!rv$retag) removeModal()
  })
  
  aar_data <- reactive({
    req(input$aar_pitcher, nzchar(input$aar_pitcher))
    req(input$aar_game,    nzchar(input$aar_game))
    d <- rv$df %>%
      dplyr::filter(
        Pitcher == input$aar_pitcher,
        CustomGameID == input$aar_game,
        !(is_bullpen %in% TRUE)
      )
    
    # If PitcherTeam exists, prefer rows that match TEAM_CODE (case-insensitive),
    # but do NOT zero out the dataset if the codes don't match perfectly.
    d <- prefer_team_or_portal(d, "PitcherTeam")
    
    validate(need(nrow(d) > 0, "No data for that pitcher/game."))
    
    # de-dup
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    has <- function(nm) nm %in% names(d)
    first_present <- function(...) { cands <- c(...); hit <- which(cands %in% names(d)); if (length(hit)) cands[hit[1]] else NA_character_ }
    to_int <- function(x) suppressWarnings(as.integer(x))
    
    d$inZone <- as.integer(derive_zone_inches(d))
    
    # PA grouping
    if (all(c("PAofInning","Inning","Batter") %in% names(d))) {
      d$PA_ID <- interaction(d$Date, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
    } else if ("PitchofPA" %in% names(d)) {
      d <- d %>% dplyr::arrange(PitchofPA)
      d$PA_ID <- cumsum(d$PitchofPA == 1)
    } else {
      pc <- if ("PitchCall" %in% names(d)) as.character(d$PitchCall) else rep(NA_character_, nrow(d))
      pr <- if ("PlayResult" %in% names(d)) as.character(d$PlayResult) else rep(NA_character_, nrow(d))
      
      term <- pc %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled")
      if (!is.null(pr)) term <- term | (!is.na(pr) & nzchar(pr))
      if ("KorBB"      %in% names(d)) term <- term | (!is.na(d$KorBB) & nzchar(d$KorBB))
      d$PA_ID <- cumsum(c(1L, head(term, -1)))
    }
    
    
    d <- d %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::mutate(PitchNum = dplyr::row_number(), FirstPitch = PitchNum == 1) %>%
      dplyr::ungroup()
    
    # --- Count columns (robust, no NSE pitfalls) ---
    balls_post_col   <- first_present("Balls","CountBalls","BallsAfterPitch")
    strikes_post_col <- first_present("Strikes","CountStrikes","StrikesAfterPitch")
    balls_pre_col    <- first_present("BallsPre","Balls_BeforePitch","BallsBeforePitch","CountBallsBeforePitch")
    strikes_pre_col  <- first_present("StrikesPre","Strikes_BeforePitch","StrikesBeforePitch","CountStrikesBeforePitch")
    
    if (!"BallsPre"   %in% names(d)) d$BallsPre   <- NA_integer_
    if (!"StrikesPre" %in% names(d)) d$StrikesPre <- NA_integer_
    
    if (!is.na(balls_pre_col))   d$BallsPre   <- to_int(d[[balls_pre_col]])
    if (!is.na(strikes_pre_col)) d$StrikesPre <- to_int(d[[strikes_pre_col]])
    
    need_recon <- (all(is.na(d$BallsPre)) || all(is.na(d$StrikesPre))) &&
      !is.na(balls_post_col) && !is.na(strikes_post_col)
    
    if (need_recon) {
      d <- d %>% dplyr::group_by(PA_ID) %>% dplyr::arrange(PitchNum, .by_group = TRUE)
      d$BallsPre   <- dplyr::coalesce(d$BallsPre,   dplyr::lag(to_int(d[[balls_post_col]]),   default = 0L))
      d$StrikesPre <- dplyr::coalesce(d$StrikesPre, dplyr::lag(to_int(d[[strikes_post_col]]), default = 0L))
      d <- d %>% dplyr::ungroup()
    }
    
    swing_calls <- c("StrikeSwinging","InPlay","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
    d <- d %>%
      dplyr::mutate(
        is_strike = PitchCall %in% c("StrikeSwinging","StrikeCalled"),
        IsSwing   = .is_swing_event(PitchCall),
        is_whiff  = PitchCall == "StrikeSwinging",
        pre2k     = !is.na(StrikesPre) & StrikesPre < 2,
        twok      = !is.na(StrikesPre) & StrikesPre >= 2
      )
    # Ensure BIP flags exist under both names
    if (!"IsBIP" %in% names(d)) d$IsBIP <- is_bip_from(pc, pr)
    if (!"is_bip" %in% names(d))
      d$is_bip <- d$IsBIP
    {
      # local numeric parser so we don't rely on any outer helper
      to_num <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
      
      # best-effort EV / LA columns for Barrel calc
      ev_col <- intersect(c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV"), names(d))[1]
      la_col <- intersect(c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle"),           names(d))[1]
      ev <- if (!is.na(ev_col)) to_num(d[[ev_col]]) else NA_real_
      la <- if (!is.na(la_col)) to_num(d[[la_col]]) else NA_real_
      
      evla <- resolve_ev_la_strict(d)
      ev <- evla$ev; la <- evla$la
      
      d <- d %>%
        dplyr::mutate(
          # Canonical strike/swing/foul columns many downstream bits expect
          CalledStrike    = PitchCall == "StrikeCalled",
          SwingingStrike  = PitchCall == "StrikeSwinging",
          Foul            = PitchCall %in% c("FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
          IsStrike        = CalledStrike | SwingingStrike | Foul,
          
          # Also keep earlier naming used elsewhere
          IsCalledStrike  = CalledStrike,
          IsSwing         = is_swing,
          
          # Zone + count fields expected by the PDF composer
          InZone               = inZone == 1L,
          PitchNumberInPA      = PitchNum,
          StrikesBeforePitch   = StrikesPre,
          FirstPitch           = FirstPitch,
          
          # Events used by E&A exclusion and perf tables
          HBP    = (("PlayResult" %in% names(.)) & grepl("(?i)hit by pitch|\\bHBP\\b", PlayResult)) |
            (("KorBB"      %in% names(.)) & grepl("(?i)HBP", KorBB)),
          Barrel = is_barrel_strict(PitchCall, ev, la),
          
          # Movement aliases
          IVB = to_num(InducedVertBreak),
          HB  = to_num(HorzBreak),
          
          # Normalize batter side to simple L/R
          BatterSide = dplyr::case_when(
            BatterSide %in% c("L","Left","LHH","LH") ~ "L",
            BatterSide %in% c("R","Right","RHH","RH") ~ "R",
            TRUE ~ as.character(BatterSide)
          )
        )
    }
    d
  })
  
  
  
  output$aar_title <- renderText({
    pid <- if (!is.null(input$aar_pitcher) && nzchar(input$aar_pitcher)) input$aar_pitcher else "Pitcher"
    game_str <- if (!is.null(input$aar_game) && nzchar(input$aar_game)) input$aar_game else "Game"
    paste0(pid, " — ", game_str)
  })
  
  compute_bb_for_aar <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(0L)
    
    # Candidate text columns that might store PA results
    candidate_text_cols <- c(
      "KorBB","PAResult","PlayResult","Event","Result","ResultOfPA",
      "PA_Result","PlateAppearanceResult","Outcome","PlayOutcome",
      "EventResult","BatterEvent","BatterEventText"
    )
    present_text_cols <- intersect(candidate_text_cols, names(df))
    
    # Build a single "walk" mask across any present text columns (case-insensitive),
    # while avoiding accidental matches to HBP.
    walk_tokens <- c("bb","ibb","walk","baseonballs","base on balls","intentwalk","intentional walk","intentional bb","intentional base on balls")
    hbp_tokens  <- c("hbp","hit by pitch","hit-by-pitch","hit_by_pitch")
    
    walk_mask_any <- rep(FALSE, nrow(df))
    hbp_mask_any  <- rep(FALSE, nrow(df))
    
    if (length(present_text_cols)) {
      for (col in present_text_cols) {
        v <- df[[col]]
        if (is.factor(v)) v <- as.character(v)
        v <- tolower(ifelse(is.na(v), "", v))
        
        this_walk <- Reduce(`|`, lapply(walk_tokens, function(tok) grepl(tok, v, fixed = TRUE)), init = FALSE)
        this_hbp  <- Reduce(`|`, lapply(hbp_tokens,  function(tok) grepl(tok, v, fixed = TRUE)), init = FALSE)
        
        walk_mask_any <- walk_mask_any | this_walk
        hbp_mask_any  <- hbp_mask_any  | this_hbp
      }
    }
    
    # Also consider common boolean/numeric flags if they exist
    candidate_flag_cols <- c("IsBB","BBFlag","IsWalk","BaseOnBalls","IntentionalWalk")
    present_flag_cols   <- intersect(candidate_flag_cols, names(df))
    if (length(present_flag_cols)) {
      for (col in present_flag_cols) {
        v <- df[[col]]
        if (is.logical(v)) walk_mask_any <- walk_mask_any | (isTRUE(v) | v == TRUE)
        if (is.numeric(v)) walk_mask_any <- walk_mask_any | (v %in% c(1, 1L))
      }
    }
    
    # Remove any HBP rows from the walk mask
    walk_mask <- walk_mask_any & !hbp_mask_any
    
    # Preferred: count unique PAs with a walk if a PA key exists (prevents double count)
    pa_key <- intersect(
      c("PAId","PlateAppearanceId","PlateAppearanceID","PAofInning","PA_Number","PAIndex","PA","Pa"),
      names(df)
    )[1]
    
    # First attempt: direct detection via tokens/flags
    if (!is.na(pa_key)) {
      bb1 <- df |>
        dplyr::mutate(.walk = walk_mask) |>
        dplyr::group_by(.data[[pa_key]]) |>
        dplyr::summarise(any_walk = any(.walk, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(n = sum(any_walk, na.rm = TRUE)) |>
        dplyr::pull(n)
    } else {
      bb1 <- sum(walk_mask, na.rm = TRUE)
    }
    
    # Fallback (only if bb1 == 0): infer walks from Balls/PitchCall if available & PA key exists
    if (identical(bb1, 0L) || is.na(bb1)) {
      have_pa   <- !is.na(pa_key)
      have_cnt  <- "Balls" %in% names(df)
      have_call <- "PitchCall" %in% names(df)
      
      if (have_pa && have_cnt && have_call) {
        ball_calls <- c("BallCalled","Ball","BallInDirt","AutomaticBall","IntentionalBall","IntentBall","PitchOut")
        bb2 <- df |>
          dplyr::mutate(
            .balls = suppressWarnings(as.integer(.data$Balls)),
            .is_ball_call = tolower(as.character(.data$PitchCall)) %in% tolower(ball_calls)
          ) |>
          dplyr::group_by(.data[[pa_key]]) |>
          dplyr::summarise(
            # consider a PA a walk if we ever see 4 balls OR we see 3 balls and the next pitch is a ball call
            bb_pa = (max(.balls, na.rm = TRUE) >= 4) | any((.balls %in% c(3L)) & .is_ball_call, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::summarise(n = sum(bb_pa, na.rm = TRUE)) |>
          dplyr::pull(n)
        
        if (!is.na(bb2) && bb2 > 0) return(as.integer(bb2))
      }
    }
    
    as.integer(ifelse(is.na(bb1), 0L, bb1))
  }
  
  # === Helpers to infer BB from the same source column that yields SO in the header ===
  
  .infer_pa_key <- function(df) {
    intersect(c("PAId","PlateAppearanceId","PlateAppearanceID","PAofInning","PA_Number","PAIndex","PA","Pa"), names(df))[1]
  }
  
  .count_pa <- function(mask, pa_vec) {
    # Count unique PAs where the mask is TRUE at least once
    dplyr::tibble(pa = pa_vec, flag = mask) |>
      dplyr::group_by(pa) |>
      dplyr::summarise(any_flag = any(flag, na.rm = TRUE), .groups = "drop") |>
      dplyr::summarise(n = sum(any_flag, na.rm = TRUE)) |>
      dplyr::pull(n) |>
      as.integer()
  }
  
  .kbb_from_column <- function(df, col, pa_key = NULL) {
    v_raw <- df[[col]]
    v_chr <- if (is.factor(v_raw)) as.character(v_raw) else as.character(v_raw)
    v_lc  <- tolower(ifelse(is.na(v_chr), "", v_chr))
    
    # Column-specific logic (KorBB often stores tokens like K/BB/IBB/HBP)
    if (identical(col, "KorBB")) {
      k_mask  <- v_chr %in% c("K","SO","Strikeout","StrikeOut")
      bb_mask <- v_chr %in% c("BB","IBB")
    } else {
      # Generic text-based logic
      k_mask  <- grepl("strikeout", v_lc, fixed = TRUE) |
        grepl("strike out", v_lc, fixed = TRUE) |
        grepl("strike-out", v_lc, fixed = TRUE)
      # Walk tokens; exclude HBP
      bb_mask <- (grepl("walk", v_lc, fixed = TRUE) |
                    grepl("base on balls", v_lc, fixed = TRUE) |
                    grepl("intentional walk", v_lc, fixed = TRUE) |
                    grepl("intentwalk", v_lc, fixed = TRUE)) &
        !grepl("hit by pitch", v_lc, fixed = TRUE) &
        !grepl("hbp", v_lc, fixed = TRUE)
    }
    
    if (!is.null(pa_key) && !is.na(pa_key)) {
      so <- .count_pa(k_mask, df[[pa_key]])
      bb <- .count_pa(bb_mask, df[[pa_key]])
    } else {
      so <- sum(k_mask,  na.rm = TRUE)
      bb <- sum(bb_mask, na.rm = TRUE)
    }
    list(so = as.integer(so), bb = as.integer(bb))
  }
  
  # Main inference: pick the SAME column that reproduces bits$SO, then read BB from it
  infer_bb_from_same_k_source <- function(df, so_target) {
    if (is.null(df) || nrow(df) == 0) return(0L)
    
    pa_key <- .infer_pa_key(df)
    candidate_cols <- intersect(
      c("KorBB","PAResult","PlayResult","Event","Result","ResultOfPA",
        "PA_Result","PlateAppearanceResult","Outcome","PlayOutcome",
        "EventResult","BatterEvent","BatterEventText"),
      names(df)
    )
    
    chosen <- NULL
    for (col in candidate_cols) {
      out <- .kbb_from_column(df, col, pa_key)
      if (!is.na(so_target) && is.finite(so_target) && identical(out$so, as.integer(so_target))) {
        chosen <- list(col = col, so = out$so, bb = out$bb)
        break
      }
    }
    
    if (!is.null(chosen)) {
      return(as.integer(chosen$bb))
    }
    
    # Fallback: union across all present columns; if that matches SO, use that BB
    if (length(candidate_cols)) {
      # Build union masks
      k_any  <- rep(FALSE, nrow(df))
      bb_any <- rep(FALSE, nrow(df))
      for (col in candidate_cols) {
        out <- .kbb_from_column(df, col, NULL)  # row-level masks
        # Recompute masks (row-level) to union correctly
        v_raw <- df[[col]]
        v_chr <- if (is.factor(v_raw)) as.character(v_raw) else as.character(v_raw)
        v_lc  <- tolower(ifelse(is.na(v_chr), "", v_chr))
        if (identical(col, "KorBB")) {
          k_m  <- v_chr %in% c("K","SO","Strikeout","StrikeOut")
          bb_m <- v_chr %in% c("BB","IBB")
        } else {
          k_m  <- grepl("strikeout", v_lc, fixed = TRUE) |
            grepl("strike out", v_lc, fixed = TRUE) |
            grepl("strike-out", v_lc, fixed = TRUE)
          bb_m <- (grepl("walk", v_lc, fixed = TRUE) |
                     grepl("base on balls", v_lc, fixed = TRUE) |
                     grepl("intentional walk", v_lc, fixed = TRUE) |
                     grepl("intentwalk", v_lc, fixed = TRUE)) &
            !grepl("hit by pitch", v_lc, fixed = TRUE) &
            !grepl("hbp", v_lc, fixed = TRUE)
        }
        k_any  <- k_any  | k_m
        bb_any <- bb_any | bb_m
      }
      
      if (!is.na(pa_key) && !is.null(pa_key)) {
        so_union <- .count_pa(k_any,  df[[pa_key]])
        bb_union <- .count_pa(bb_any, df[[pa_key]])
      } else {
        so_union <- sum(k_any,  na.rm = TRUE)
        bb_union <- sum(bb_any, na.rm = TRUE)
      }
      
      if (identical(as.integer(so_union), as.integer(so_target))) {
        return(as.integer(bb_union))
      }
    }
    
    # Last resort: 0 (don’t break header)
    warning("[AAR] Could not match SO source; defaulting BB to 0")
    0L
  }
  
  # === AAR BB from KorBB (exactly) ===
  compute_bb_from_KorBB <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(0L)
    if (!"KorBB" %in% names(df)) {
      warning("[AAR] 'KorBB' column missing; leaving bits$BB as-is.")
      return(NA_integer_)
    }
    
    v <- df[["KorBB"]]
    if (is.factor(v)) v <- as.character(v)
    walk_mask <- v %in% c("BB","IBB")
    
    # Prefer counting unique PAs with a walk (prevents accidental double-count)
    pa_key <- intersect(
      c("PAId","PlateAppearanceId","PlateAppearanceID","PAofInning","PA_Number","PAIndex","PA","Pa"),
      names(df)
    )[1]
    
    if (!is.na(pa_key)) {
      n <- dplyr::tibble(pa = df[[pa_key]], walk = walk_mask) |>
        dplyr::group_by(pa) |>
        dplyr::summarise(any_walk = any(walk, na.rm = TRUE), .groups = "drop") |>
        dplyr::summarise(n = sum(any_walk, na.rm = TRUE)) |>
        dplyr::pull(n) |>
        as.integer()
      return(n)
    } else {
      n <- sum(walk_mask, na.rm = TRUE)
      return(as.integer(n))
    }
  }
  
  output$aar_statline <- renderUI({
    d <- aar_data()
    bits <- aar_statline_bits(d)
    
    # --- BB: force from KorBB only ---
    bb_korbb <- compute_bb_from_KorBB(d)
    if (!is.na(bb_korbb)) bits$BB <- bb_korbb  # only override if KorBB present
    
    htmltools::HTML(sprintf(
      "<b>Pitches</b> %s &nbsp;&nbsp; <b>PA</b> %s &nbsp;&nbsp; <b>IP</b> %s &nbsp;&nbsp; <b>SO</b> %s &nbsp;&nbsp; <b>H</b> %s &nbsp;&nbsp; <b>Barrels</b> %s &nbsp;&nbsp; <b>BB</b> %s &nbsp;&nbsp; <b>HBP</b> %s",
      bits$Pitches, bits$PA, bits$IP, bits$SO, bits$H, bits$Barrels, bits$BB, bits$HBP
    ))
  })
  
  ensure_process_cols <- function(d) {
    if (is.null(d) || !nrow(d)) return(d)
    
    has <- function(nm) nm %in% names(d)
    
    # ---- ensure PitchCall as clean character ----
    if (has("PitchCall")) d$PitchCall <- trimws(as.character(d$PitchCall))
    
    # ---- PA_ID / PitchNum / FirstPitch ----
    d <- ensure_pa(d)
    
    # ---- StrikesPre / BallsPre (robust, supports Fall 2025 column variants) ----
    first_present <- function(...) {
      cands <- c(...)
      hit <- which(cands %in% names(d))
      if (length(hit)) cands[hit[1]] else NA_character_
    }
    to_int <- function(x) suppressWarnings(as.integer(x))
    
    balls_pre_col    <- first_present("BallsPre","Balls_BeforePitch","BallsBeforePitch","CountBallsBeforePitch","BallsBefore")
    strikes_pre_col  <- first_present("StrikesPre","Strikes_BeforePitch","StrikesBeforePitch","CountStrikesBeforePitch","StrikesBefore")
    balls_post_col   <- first_present("Balls","CountBalls","BallsAfterPitch","BallsPost")
    strikes_post_col <- first_present("Strikes","CountStrikes","StrikesAfterPitch","StrikesPost")
    
    if (!has("BallsPre"))   d$BallsPre   <- NA_integer_
    if (!has("StrikesPre")) d$StrikesPre <- NA_integer_
    
    # If pre-count columns exist under other names, use them
    if (!is.na(balls_pre_col))   d$BallsPre   <- to_int(d[[balls_pre_col]])
    if (!is.na(strikes_pre_col)) d$StrikesPre <- to_int(d[[strikes_pre_col]])
    
    # If still missing, try reconstructing from post-counts (lag)
    need_recon <- (all(is.na(d$BallsPre)) || all(is.na(d$StrikesPre))) &&
      !is.na(balls_post_col) && !is.na(strikes_post_col)
    
    if (need_recon) {
      d <- d %>%
        dplyr::group_by(PA_ID) %>%
        dplyr::arrange(PitchNum, .by_group = TRUE) %>%
        dplyr::mutate(
          BallsPre   = dplyr::lag(to_int(.data[[balls_post_col]]),   default = 0L),
          StrikesPre = dplyr::lag(to_int(.data[[strikes_post_col]]), default = 0L)
        ) %>%
        dplyr::ungroup()
    }
    
    # Last fallback: if Balls/Strikes are present but unclear (pre vs post), use heuristic
    if ((all(is.na(d$BallsPre)) || all(is.na(d$StrikesPre))) && has("Balls") && has("Strikes")) {
      first_rows <- d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()
      frac_00 <- mean(to_int(first_rows$Balls) %in% 0L & to_int(first_rows$Strikes) %in% 0L, na.rm = TRUE)
      if (!is.finite(frac_00)) frac_00 <- 0
      
      if (frac_00 >= 0.8) {
        d$BallsPre   <- to_int(d$Balls)
        d$StrikesPre <- to_int(d$Strikes)
      } else {
        d <- d %>%
          dplyr::group_by(PA_ID) %>%
          dplyr::arrange(PitchNum, .by_group = TRUE) %>%
          dplyr::mutate(
            BallsPre   = dplyr::lag(to_int(.data$Balls),   default = 0L),
            StrikesPre = dplyr::lag(to_int(.data$Strikes), default = 0L)
          ) %>%
          dplyr::ungroup()
      }
    }
    
    # Force StrikesPre from PitchCall sequence (ensures consistent 2-strike logic)
    if (has("PitchCall")) {
      d$StrikesPre <- compute_strikes_before(d)
    }
    
    
    # ---- inZone ----
    if (!has("inZone")) d$inZone <- as.integer(derive_zone_inches(d))
    
    # ---- strike/swing flags (vectorized; fixes 0% bug) ----
    if (!has("IsCalledStrike")) {
      d$IsCalledStrike <- if (has("PitchCall")) (d$PitchCall == "StrikeCalled") else FALSE
    }
    
    if (!has("IsSwing")) {
      d$IsSwing <- if (has("PitchCall")) .is_swing_event(d$PitchCall) else FALSE
    }
    
    if (!has("IsStrike")) {
      foul_calls <- c("FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
      d$IsStrike <- if (has("PitchCall")) (d$PitchCall %in% c("StrikeSwinging","StrikeCalled", foul_calls)) else FALSE
    }
    
    
    # ---- pre2k / twok ----
    if (!has("pre2k")) d$pre2k <- !is.na(d$StrikesPre) & d$StrikesPre < 2
    if (!has("twok"))  d$twok  <- !is.na(d$StrikesPre) & d$StrikesPre == 2
    
    d
  }
  
  # ---- D1 benchmarks for AAR process tables (edit if you want different targets) ----
  d1_process_targets <- c(
    "Strike%"            = 65,
    "Zone%"              = 50,
    "1st Pitch Strike%"  = 63,
    "Pre2K Zone%"        = 50,
    "E&A%"               = 70,
    "Put Away%"          = 19
  )
  
  output$aar_process_overall <- DT::renderDT({
    d <- aar_data(); req(nrow(d) > 0)
    d <- ensure_process_cols(d)
    
    pa <- d %>% dplyr::group_by(PA_ID) %>% dplyr::summarise(
      pitches_in_pa = dplyr::n(),
      reached_02_or_12_first3 = any(!is.na(BallsPre) & !is.na(StrikesPre) & !is.na(PitchNum) & PitchNum <= 3 &
                                      ((BallsPre==0 & StrikesPre==2) | (BallsPre==1 & StrikesPre==2))),
      first_pitch_strike = any(FirstPitch & (IsCalledStrike | IsSwing)),
      .groups = "drop"
    )
    
    strike_rate <- mean((d$PitchCall == "StrikeCalled") | .is_swing_event(d$PitchCall), na.rm = TRUE)
    fps_rate    <- mean(pa$first_pitch_strike, na.rm = TRUE)
    
    pre2k_den   <- sum(d$pre2k & !is.na(d$inZone), na.rm = TRUE)
    twok_den    <- sum(d$twok  & !is.na(d$inZone), na.rm = TRUE)
    
    pre2k_zone  <- if (pre2k_den > 0)
      sum(d$pre2k & (d$inZone == 1L), na.rm = TRUE) / pre2k_den
    else NA_real_
    
    twok_zone   <- if (twok_den > 0)
      sum(d$twok & (d$inZone == 1L), na.rm = TRUE) / twok_den
    else NA_real_

    # Put Away%: 2-strike pitches that end in a K
    put_away <- calc_put_away_rate(d)
    ea_rate     <- mean((pa$pitches_in_pa <= 3) | pa$reached_02_or_12_first3, na.rm = TRUE)
    
    df <- data.frame(
      Stat  = c("Strike%","1st Pitch Strike%","Pre2K Zone%","2K Zone%","E&A%","Put Away%"),
      Value = c(100*strike_rate, 100*fps_rate, 100*pre2k_zone, 100*twok_zone, 100*ea_rate, 100*put_away),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    
    # numeric + display
    df$Value_num <- df$Value
    df$Value     <- ifelse(is.finite(df$Value_num), sprintf("%.0f%%", df$Value_num), "NA")
    
    # apply green/red fill vs D1 (new rule)
    d1v <- unname(d1_process_targets[df$Stat])
    fills <- mapply(function(v, d1) fill_vs_d1(v, d1, band = 5), df$Value_num, d1v)
    
    df$Value <- mapply(function(txt, fill) {
      if (is.na(fill) || !nzchar(fill)) return(txt)
      sprintf('<span style="display:block; padding:4px 6px; border-radius:4px; background-color:%s;">%s</span>', fill, txt)
    }, df$Value, fills)
    
    df_out <- df[, c("Stat","Value")]
    DT::datatable(df_out, rownames = FALSE, escape = FALSE,
                  options = list(dom='t', paging=FALSE, ordering=FALSE, stripe=TRUE))
  })
  
  
  # Process by pitch type: Pre2K Zone%, Strike%, Whiff%
  output$aar_process_by_pitch <- DT::renderDT({
    d <- aar_data(); validate(need(nrow(d) > 0, "No data."))
    d <- ensure_process_cols(d)
    
    summ_num <- d %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Pre2K_Zone = {
          den <- sum(pre2k & !is.na(inZone), na.rm = TRUE)
          if (den > 0) 100 * sum(pre2k & (inZone == 1L), na.rm = TRUE) / den else NA_real_
        },
        Strike = 100 * mean((PitchCall == "StrikeCalled") | .is_swing_event(PitchCall), na.rm = TRUE),
        Whiff  = {
          sw <- sum(IsSwing, na.rm = TRUE)
          wh <- sum(PitchCall == "StrikeSwinging", na.rm = TRUE)
          if (sw > 0) 100 * wh / sw else NA_real_
        },
        .groups = "drop"
      )
    
    summ <- summ_num %>%
      dplyr::mutate(
        `Pre2K Zone%` = ifelse(is.finite(Pre2K_Zone), sprintf("%.0f%%", Pre2K_Zone), "NA"),
        `Strike%`     = ifelse(is.finite(Strike),     sprintf("%.0f%%", Strike),     "NA"),
        `Whiff%`      = ifelse(is.finite(Whiff),      sprintf("%.0f%%", Whiff),      "NA")
      ) %>%
      dplyr::select(PitchType, `Pre2K Zone%`, `Strike%`, `Whiff%`)
    
    # D1 targets for by-pitch process (edit if desired)
    d1_by_pitch <- c(
      "Pre2K Zone%" = d1_process_targets[["Pre2K Zone%"]],
      "Strike%"     = d1_process_targets[["Strike%"]]
    )
    
    wrap_fill <- function(x_str, stat_name, pitch_type = NA_character_) {
      v <- suppressWarnings(as.numeric(gsub("%","", x_str)))
      d1 <- if (identical(stat_name, "Whiff%")) {
        d1_pct_avg_for_metric("Whiff%", pitch_type) * 100
      } else {
        suppressWarnings(as.numeric(d1_by_pitch[[stat_name]]))
      }
      fill <- fill_vs_d1(v, d1, band = 5)
      if (is.na(fill) || !nzchar(fill)) return(x_str)
      sprintf('<span style="display:block; padding:4px 6px; border-radius:4px; background-color:%s;">%s</span>', fill, x_str)
    }
    
    summ$`Pre2K Zone%` <- vapply(summ$`Pre2K Zone%`, wrap_fill, character(1), stat_name = "Pre2K Zone%")
    summ$`Strike%`     <- vapply(summ$`Strike%`,     wrap_fill, character(1), stat_name = "Strike%")
    summ$`Whiff%`      <- mapply(wrap_fill, summ$`Whiff%`,
                                 stat_name = "Whiff%", pitch_type = summ$PitchType,
                                 SIMPLIFY = TRUE, USE.NAMES = FALSE)
    
    DT::datatable(summ, rownames = FALSE, escape = FALSE,
                  options = list(dom='t', paging=FALSE, ordering=FALSE, stripe=TRUE))
  })
  
  # Pitch Type Performance: Usage%, HB, IVB, 2K%, vLHH%, vRHH%
  output$aar_perf_table <- DT::renderDT({
    d <- aar_data(); req(nrow(d) > 0)
    
    has    <- function(nm) nm %in% names(d)
    to_num <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
    
    # Movement & swing flags up front
    d <- d %>%
      dplyr::mutate(
        HB        = suppressWarnings(as.numeric(HorzBreak)),
        IVB       = suppressWarnings(as.numeric(InducedVertBreak)),
        is_swing  = .is_swing_event(PitchCall)
      )
    
    # Strict EV/LA & barrel flags as COLUMNS (so groups subset correctly)
    evla <- resolve_ev_la_strict(d)
    d$EV_strict <- evla$ev
    d$LA_strict <- evla$la
    d$BIP_strict <- is_bip_for_barrel(d$PitchCall)
    d$Barrel_strict <- is_barrel_strict(d$PitchCall, d$EV_strict, d$LA_strict)
    d$barrel_ok <- barrel_date_ok(d)
    
    total_n <- nrow(d)
    
    by_pt <- d %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Total       = dplyr::n(),
        UsagePct    = 100 * Total / total_n,
        HB_in       = round(mean(HB,  na.rm = TRUE), 1),
        IVB_in      = round(mean(IVB, na.rm = TRUE), 1),
        TwoKpct     = 100 * mean(twok, na.rm = TRUE),
        vLHHpct     = 100 * mean(BatterSide %in% c("L","Left","LHH","LH"), na.rm = TRUE),
        vRHHpct     = 100 * mean(BatterSide %in% c("R","Right","RHH","RH"), na.rm = TRUE),
        BarrelPct   = {
          den <- sum(BIP_strict & barrel_ok & is.finite(EV_strict) & is.finite(LA_strict), na.rm = TRUE)
          if (den > 0) 100 * sum(Barrel_strict & barrel_ok, na.rm = TRUE) / den else NA_real_
        },
        ChasePct    = {
          sw_loc <- sum(is_swing & !is.na(inZone), na.rm = TRUE)
          ooz_sw <- sum(is_swing & (inZone == 0L), na.rm = TRUE)
          if (sw_loc > 0) 100 * ooz_sw / sw_loc else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        `Usage%`  = sprintf("%.0f%%", UsagePct),
        `HB (in)` = sprintf("%.1f", HB_in),
        `IVB (in)`= sprintf("%.1f", IVB_in),
        `2K%`     = sprintf("%.0f%%", TwoKpct),
        `vLHH%`   = sprintf("%.0f%%", vLHHpct),
        `vRHH%`   = sprintf("%.0f%%", vRHHpct),
        `Barrel%` = ifelse(is.na(BarrelPct), "NA", sprintf("%.0f%%", BarrelPct)),
        `Chase%`  = ifelse(is.na(ChasePct),  "NA", sprintf("%.0f%%", ChasePct))
      ) %>%
      dplyr::select(PitchType, `Usage%`, `HB (in)`, `IVB (in)`, `2K%`, `vLHH%`, `vRHH%`, `Barrel%`, `Chase%`) %>%
      dplyr::arrange(dplyr::desc(as.numeric(sub("%","",`Usage%`))))
    
    by_pt[] <- lapply(by_pt, as.character)
    DT::datatable(by_pt, rownames = FALSE, options = list(dom = 't', paging = FALSE, ordering = FALSE))
  })
  
  
  # Movement & Usage table (compact: PitchType, Usage%, HB, IVB)
  output$aar_move_usage_table <- DT::renderDT({
    d <- aar_data(); validate(need(nrow(d) > 0, "No data."))
    total_n <- nrow(d)
    
    tab <- d %>%
      dplyr::mutate(
        HB  = suppressWarnings(as.numeric(HorzBreak)),
        IVB = suppressWarnings(as.numeric(InducedVertBreak))
      ) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Total     = dplyr::n(),
        `Usage%`  = sprintf("%.0f%%", 100 * Total / total_n),
        `HB (in)` = sprintf("%.1f", mean(HB,  na.rm = TRUE)),
        `IVB (in)`= sprintf("%.1f", mean(IVB, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::select(PitchType, `Usage%`, `HB (in)`, `IVB (in)`) %>%
      dplyr::arrange(dplyr::desc(as.numeric(sub("%","",`Usage%`))))
    tab$PitchType <- as.character(tab$PitchType)
    tab[] <- lapply(tab, as.character)
    safe_dt(tab)
  })
  
  
  # ---------- Bottom visuals ----------
  # Strike-zone plots split by BatterSide (vLHH / vRHH), game-only
  aar_sz_plot <- function(d_sub, title_suffix){
    validate(need(nrow(d_sub) > 2, paste("Not enough pitches", title_suffix)))
    ggplot(d_sub, aes(PlateLocSide, PlateLocHeight)) +
      stat_density_2d(aes(fill = after_stat(ndensity)), geom = "raster", contour = FALSE, n = 180, na.rm = TRUE) +
      scale_fill_gradientn(colors = c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff"), guide = "none") +
      labs(title = title_suffix, y = "Vertical Location", x = "Horizontal Location") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(5, 5, 5, 5)
      ) +
      geom_rect(data = strike_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.0) +
      geom_segment(data = home_plate_segments,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
      coord_fixed(xlim = c(-3,3), ylim = c(0,5), expand = FALSE)
  }
  
  # vLHH
  output$aar_sz_lhh <- renderPlot({
    d <- aar_data() %>%
      dplyr::filter(
        BatterSide %in% c("L","Left","LHH","LH"),
        !is.na(PlateLocSide), !is.na(PlateLocHeight),
        dplyr::between(PlateLocSide,-3,3), dplyr::between(PlateLocHeight,0,5)
      )
    validate(need(nrow(d) > 0, "No data."))
    ggplot(d, aes(PlateLocSide, PlateLocHeight, color = PitchType)) +
      geom_point(alpha = 0.9, size = 2) +
      scale_color_manual(values = pitch_colors, drop = FALSE) +
      labs(title = "Strike Zone — vLHH", y = "Vertical", x = "Horizontal") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            panel.border = element_rect(color = "black", fill = NA)) +
      geom_rect(data = strike_zone, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1) +
      geom_segment(data = home_plate_segments, aes(x=x,y=y,xend=xend,yend=yend),
                   inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
      coord_fixed(xlim=c(-3,3), ylim=c(0,5), expand = FALSE)
  })
  
  # vRHH
  output$aar_sz_rhh <- renderPlot({
    d <- aar_data() %>%
      dplyr::filter(
        BatterSide %in% c("R","Right","RHH","RH"),
        !is.na(PlateLocSide), !is.na(PlateLocHeight),
        dplyr::between(PlateLocSide,-3,3), dplyr::between(PlateLocHeight,0,5)
      )
    validate(need(nrow(d) > 0, "No data."))
    ggplot(d, aes(PlateLocSide, PlateLocHeight, color = PitchType)) +
      geom_point(alpha = 0.9, size = 2) +
      scale_color_manual(values = pitch_colors, drop = FALSE) +
      labs(title = "Strike Zone — vRHH", y = "Vertical", x = "Horizontal") +
      
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            panel.border = element_rect(color = "black", fill = NA)) +
      geom_rect(data = strike_zone, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1) +
      geom_segment(data = home_plate_segments, aes(x=x,y=y,xend=xend,yend=yend),
                   inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
      coord_fixed(xlim=c(-3,3), ylim=c(0,5), expand = FALSE)
  })
  
  # Movement plot — reuse your styling but force game-only data
  output$aar_movement <- renderPlotly({
    d <- aar_data(); req(nrow(d) > 0)
    
    d <- d %>%
      dplyr::mutate(
        HB  = suppressWarnings(as.numeric(HorzBreak)),
        IVB = suppressWarnings(as.numeric(InducedVertBreak)),
        PitchType_plot = dplyr::if_else(
          is.na(PitchType) | !(as.character(PitchType) %in% names(pitch_colors)),
          "Undefined", as.character(PitchType)
        )
      ) %>%
      dplyr::filter(is.finite(HB), is.finite(IVB))
    
    p <- plotly::plot_ly(source = "aar_movement")
    for (lv in sort(unique(d$PitchType_plot))) {
      dd  <- d %>% dplyr::filter(PitchType_plot == lv)
      col <- pitch_colors[[lv]]; if (is.null(col) || !nzchar(col)) col <- "#808080"
      p <- p %>% add_trace(
        data = dd, x = ~HB, y = ~IVB,
        type = "scattergl", mode = "markers",
        marker = list(size = 8, opacity = 0.9, color = col),
        name = lv, text = ~lv,
        hovertemplate = "<b>%{text}</b><br>HB: %{x:.1f}<br>iVB: %{y:.1f}<extra></extra>"
      )
    }
    
    p <- p %>% layout(
      title  = list(text = "Pitch Movement (Game Only)", x = 0.5),
      xaxis  = list(title = "HB (in)",  range = c(-25, 25), dtick = 3, fixedrange = TRUE, zeroline = FALSE),
      yaxis  = list(title = "iVB (in)", range = c(-25, 25), dtick = 3, fixedrange = TRUE, zeroline = FALSE,
                    scaleanchor = "x", scaleratio = 1),
      legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1.0, yanchor = "top"),
      margin = list(t = 70, r = 20, b = 40, l = 60)
    )
    
    plotly::event_register(p, "plotly_selected")
  })
  
  pitch_metrics_base <- reactive({
    d <- dataFilter()
    if (!"row_id" %in% names(d)) d$row_id <- seq_len(nrow(d))
    
    known <- names(pitch_colors)
    d <- d %>%
      dplyr::mutate(
        HorzBreak        = suppressWarnings(as.numeric(HorzBreak)),
        InducedVertBreak = suppressWarnings(as.numeric(InducedVertBreak)),
        RelSpeed         = suppressWarnings(as.numeric(RelSpeed)),
        SpinRate         = suppressWarnings(as.numeric(SpinRate)),
        Extension        = suppressWarnings(as.numeric(Extension)),
        RelHeight        = suppressWarnings(as.numeric(RelHeight)),
        RelSide          = suppressWarnings(as.numeric(RelSide)),
        PitchType_plot   = dplyr::if_else(
          is.na(PitchType) | !(as.character(PitchType) %in% known),
          "Undefined",
          as.character(PitchType)
        ),
        row_id_chr = as.character(row_id),
        HoverMovement = paste0(
          "<b>", PitchType_plot, "</b><br>",
          "HB: ", sprintf("%.1f", HorzBreak), "<br>",
          "iVB: ", sprintf("%.1f", InducedVertBreak), "<br>",
          "Velo: ", sprintf("%.1f", RelSpeed)
        ),
        HoverExtension = paste0(
          "<b>", PitchType_plot, "</b><br>",
          "RelSide: ", ifelse(is.finite(RelSide), sprintf("%.1f", RelSide), "—"), "<br>",
          "Ext: ", ifelse(is.finite(Extension), sprintf("%.1f", Extension), "—")
        ),
        HoverRelease = paste0(
          "<b>", PitchType_plot, "</b><br>",
          "RelSide: ", ifelse(is.finite(RelSide), sprintf("%.1f", RelSide), "—"), "<br>",
          "RelHt: ", ifelse(is.finite(RelHeight), sprintf("%.1f", RelHeight), "—")
        )
      )
    
    d
  })
  
  movement_plot_data <- reactive({
    pitch_metrics_base() %>%
      dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
  })
  
  extension_plot_data <- reactive({
    pitch_metrics_base() %>%
      dplyr::filter(is.finite(Extension), is.finite(RelSide))
  })
  
  release_plot_data <- reactive({
    pitch_metrics_base() %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight))
  })

  d1_pitch_metric_ref <- reactiveVal(NULL)
  load_d1_pitch_metric_ref <- function() {
    cached <- d1_pitch_metric_ref()
    path <- get0(
      "BASE_PITCHING_PERCENTILE_REFERENCE_PATH",
      inherits = TRUE,
      ifnotfound = file.path(app_dir, "data", "d1_pitch_metric_percentile_reference.csv")
    )
    mtime <- if (file.exists(path)) file.info(path)$mtime else as.POSIXct(NA)
    if (is.list(cached) && !is.null(cached$data) && identical(cached$mtime, mtime)) {
      return(cached$data)
    }
    if (!file.exists(path)) {
      out <- tibble::tibble(scope = character(), pitch_type = character(), metric = character(), value = double(), sample_n = integer())
    } else {
      out <- suppressMessages(readr::read_csv(path, show_col_types = FALSE)) %>%
        dplyr::mutate(
          scope = as.character(scope),
          pitch_type = as.character(pitch_type),
          metric = as.character(metric),
          value = suppressWarnings(as.numeric(value)),
          sample_n = suppressWarnings(as.integer(sample_n))
        )
    }
    d1_pitch_metric_ref(list(mtime = mtime, data = out))
    out
  }

  pitch_metric_pitch_type <- function(x) {
    raw <- trimws(as.character(x %||% ""))
    lo <- tolower(gsub("[\\s_-]+", "", raw))
    dplyr::case_when(
      lo %in% c("", "na", "nan", "none", "undefined", "other", "untagged", "unknown") ~ NA_character_,
      lo %in% c("ff", "fa", "fastball", "fourseam", "4seam") ~ "Fastball",
      lo %in% c("si", "sinker", "twoseam", "2seam") ~ "Sinker",
      lo %in% c("sl", "slider") ~ "Slider",
      lo %in% c("st", "sw", "sweeper") ~ "Sweeper",
      lo %in% c("cu", "kc", "cb", "curve", "curveball") ~ "Curveball",
      lo %in% c("ch", "chg", "change", "changeup") ~ "Changeup",
      lo %in% c("fs", "fo", "split", "splitter", "splitfinger") ~ "Splitter",
      lo %in% c("fc", "cut", "cutter") ~ "Cutter",
      TRUE ~ raw
    )
  }

  pitch_metric_pitch_type_vec <- function(d) {
    n <- nrow(d)
    out <- rep(NA_character_, n)
    take <- function(col) {
      if (!col %in% names(d)) return()
      vals <- vapply(d[[col]], pitch_metric_pitch_type, character(1))
      ok <- is.na(out) & !is.na(vals) & nzchar(vals)
      out[ok] <<- vals[ok]
    }
    take("ModelPitchType")
    take("ModelPitchTypeRaw")
    take("PitchType_plot")
    take("PitchType")
    take("TaggedPitchType")
    take("AutoPitchType")
    out
  }

  pitch_metric_percentile <- function(value, metric, pitch_type = "", lower_better = FALSE) {
    ref <- load_d1_pitch_metric_ref()
    if (!is.finite(value) || is.null(ref) || !nrow(ref)) return(NA_real_)
    pool <- ref %>%
      dplyr::filter(.data$metric == .env$metric, is.finite(.data$value))
    if (nzchar(pitch_type %||% "")) {
      pool <- pool %>% dplyr::filter(.data$scope == "pitch_type", .data$pitch_type == .env$pitch_type)
    } else {
      pool <- pool %>% dplyr::filter(.data$scope == "overall")
    }
    vals <- pool$value
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NA_real_)
    pct <- if (isTRUE(lower_better)) mean(vals >= value, na.rm = TRUE) else mean(vals <= value, na.rm = TRUE)
    pmin(99, pmax(1, round(100 * pct)))
  }

  pitch_metric_pct_color <- function(pct) {
    if (!is.finite(pct)) return("#BFC9CA")
    dplyr::case_when(
      pct >= 90 ~ "#E33434",
      pct >= 75 ~ "#DC735F",
      pct >= 60 ~ "#D39383",
      pct >= 40 ~ "#AAC6CB",
      pct >= 25 ~ "#8CAAD2",
      TRUE ~ "#5D7EBC"
    )
  }

  pitch_metric_value_fmt <- function(value, kind = "num1") {
    if (!is.finite(value)) return("")
    switch(
      kind,
      pct = sprintf("%.0f%%", 100 * value),
      dec3 = sub("^0\\.", ".", sprintf("%.3f", value)),
      rpm = sprintf("%.0f", value),
      num2 = sprintf("%.2f", value),
      sprintf("%.1f", value)
    )
  }

  pitch_metric_row_ui <- function(label, value, pct, kind = "num1") {
    pct_label <- if (is.finite(pct)) as.character(as.integer(pct)) else "--"
    pct_pos <- if (is.finite(pct)) pmin(100, pmax(0, pct)) else 50
    color <- pitch_metric_pct_color(pct)
    disabled_class <- if (is.finite(pct)) "" else " cr-percentile-row-empty"
    div(
      class = paste0("cr-percentile-row", disabled_class),
      div(class = "cr-percentile-label", label),
      div(
        class = "cr-percentile-track-wrap",
        div(class = "cr-percentile-track"),
        div(class = "cr-percentile-fill", style = sprintf("width:%s%%;background:%s;", pct_pos, color)),
        div(class = "cr-percentile-average"),
        div(class = "cr-percentile-badge", style = sprintf("left:%s%%;background:%s;", pct_pos, color), pct_label)
      ),
      div(class = "cr-percentile-value", pitch_metric_value_fmt(value, kind))
    )
  }

  pitch_metrics_percentile_data <- reactive({
    d <- pitch_metrics_base()
    if (is.null(d) || !nrow(d)) return(list(overall = tibble::tibble(), pitch_types = list()))
    d <- prepare_aar_flags(d)
    d <- ensure_counts(d)
    if (!"CustomGameID" %in% names(d)) d$CustomGameID <- "Game"
    d$PitchTypeMetric <- pitch_metric_pitch_type_vec(d)
    d <- d %>% dplyr::filter(!is.na(PitchTypeMetric), nzchar(PitchTypeMetric))
    if (!nrow(d)) return(list(overall = tibble::tibble(), pitch_types = list()))

    pc <- as.character(d$PitchCall %||% "")
    swing <- if ("IsSwing" %in% names(d)) as.logical(d$IsSwing) else .is_swing_event(pc)
    inz <- if ("InZone" %in% names(d)) {
      as.logical(d$InZone)
    } else if ("inZone" %in% names(d)) {
      d$inZone == 1L
    } else {
      derive_zone_inches(d)
    }
    whiff <- pc == "StrikeSwinging"
    evla <- resolve_ev_la_strict(d)
    ev <- suppressWarnings(as.numeric(evla$ev))
    d$IsSwing_pm <- swing
    d$IsWhiff_pm <- whiff
    d$InZone_pm <- inz
    d$IsChase_pm <- !is.na(inz) & !inz & swing
    d$EV_pm <- ev
    hand_pm <- guess_throw_hand(input$PitcherInput, d)
    hb_sign_pm <- if (identical(hand_pm, "LHP")) -1 else 1
    d$ArmSideHB_pm <- suppressWarnings(as.numeric(d$HorzBreak)) * hb_sign_pm

    pa_last <- d %>%
      dplyr::arrange(CustomGameID, PA_ID, PitchNum) %>%
      dplyr::group_by(CustomGameID, PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    pa_grp <- pa_last$PitchTypeMetric

    overall_woba <- compute_woba_grouped(pa_last, rep("Total", nrow(pa_last)))$wOBA[1]
    heater <- d %>% dplyr::filter(PitchTypeMetric %in% c("Fastball", "Sinker"))
    overall <- tibble::tibble(
      label = c("Heater Velocity", "Extension", "Release Height", "Release Side", "wOBA", "Whiff%", "Chase%", "Avg EV"),
      metric = c("heater_velocity", "extension", "release_height", "release_side", "woba", "whiff_rate", "chase_rate", "avg_ev"),
      value = c(
        mean(suppressWarnings(as.numeric(heater$RelSpeed)), na.rm = TRUE),
        mean(suppressWarnings(as.numeric(heater$Extension)), na.rm = TRUE),
        mean(suppressWarnings(as.numeric(d$RelHeight)), na.rm = TRUE),
        mean(abs(suppressWarnings(as.numeric(d$RelSide))), na.rm = TRUE),
        overall_woba,
        sdiv(sum(d$IsWhiff_pm, na.rm = TRUE), sum(d$IsSwing_pm, na.rm = TRUE)),
        sdiv(sum(d$IsChase_pm, na.rm = TRUE), sum(!d$InZone_pm, na.rm = TRUE)),
        mean(d$EV_pm, na.rm = TRUE)
      ),
      lower_better = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
      kind = c("num1", "num1", "num1", "num1", "dec3", "pct", "pct", "num1")
    ) %>%
      dplyr::mutate(
        value = ifelse(is.finite(value), value, NA_real_),
        percentile = purrr::pmap_dbl(
          list(value, metric, lower_better),
          ~ pitch_metric_percentile(..1, ..2, lower_better = ..3)
        )
      )

    usage <- d %>%
      dplyr::count(PitchTypeMetric, name = "Pitches", sort = TRUE) %>%
      dplyr::filter(!is.na(PitchTypeMetric), nzchar(PitchTypeMetric))
    pt_woba <- compute_woba_grouped(pa_last, pa_grp) %>%
      dplyr::rename(PitchTypeMetric = .grp) %>%
      dplyr::filter(!is.na(PitchTypeMetric), nzchar(as.character(PitchTypeMetric)))

    pitch_blocks <- lapply(seq_len(nrow(usage)), function(i) {
      pt <- usage$PitchTypeMetric[[i]]
      dd <- d %>% dplyr::filter(PitchTypeMetric == pt)
      wob <- pt_woba$wOBA[match(pt, pt_woba$PitchTypeMetric)]
      rows <- tibble::tibble(
        label = c("Pitchtype Velocity", "Pitchtype iVB", "Pitchtype HB", "Pitchtype RPM"),
        metric = c("pitchtype_velocity", "pitchtype_ivb", "pitchtype_hb", "pitchtype_rpm"),
        value = c(
          mean(suppressWarnings(as.numeric(dd$RelSpeed)), na.rm = TRUE),
          mean(suppressWarnings(as.numeric(dd$InducedVertBreak)), na.rm = TRUE),
          if (identical(pt, "Fastball")) mean(dd$ArmSideHB_pm, na.rm = TRUE) else mean(abs(suppressWarnings(as.numeric(dd$HorzBreak))), na.rm = TRUE),
          mean(suppressWarnings(as.numeric(dd$SpinRate)), na.rm = TRUE)
        ),
        lower_better = c(FALSE, FALSE, identical(pt, "Fastball"), FALSE),
        kind = c("num1", "num1", "num1", "rpm")
      )
      if (pt %in% c("Fastball", "Sinker")) {
        rows <- dplyr::bind_rows(
          rows,
          tibble::tibble(
            label = "Pitchtype Extension",
            metric = "pitchtype_extension",
            value = mean(suppressWarnings(as.numeric(dd$Extension)), na.rm = TRUE),
            lower_better = FALSE,
            kind = "num1"
          )
        )
      }
      out <- dplyr::bind_rows(
        rows,
        tibble::tibble(
          label = c("Pitchtype VAA", "Pitchtype HAA", "Pitchtype Whiff%", "Pitchtype Chase%", "Pitchtype wOBA"),
          metric = c("pitchtype_vaa", "pitchtype_haa", "pitchtype_whiff_rate", "pitchtype_chase_rate", "pitchtype_woba"),
          value = c(
            mean(suppressWarnings(as.numeric(dd$VertApprAngle)), na.rm = TRUE),
            mean(abs(suppressWarnings(as.numeric(dd$HorzApprAngle))), na.rm = TRUE),
            sdiv(sum(dd$IsWhiff_pm, na.rm = TRUE), sum(dd$IsSwing_pm, na.rm = TRUE)),
            sdiv(sum(dd$IsChase_pm, na.rm = TRUE), sum(!dd$InZone_pm, na.rm = TRUE)),
            wob
          ),
          lower_better = c(!(pt %in% c("Fastball", "Cutter")), FALSE, FALSE, FALSE, TRUE),
          kind = c("num2", "num2", "pct", "pct", "dec3")
        )
      ) %>%
        dplyr::mutate(
          value = ifelse(is.finite(value), value, NA_real_),
          percentile = purrr::pmap_dbl(
            list(value, metric, lower_better),
            ~ pitch_metric_percentile(..1, ..2, pitch_type = pt, lower_better = ..3)
          )
        )
      list(pitch_type = pt, pitches = usage$Pitches[[i]], rows = out)
    })

    list(overall = overall, pitch_types = pitch_blocks)
  })

  output$pitch_metrics_percentiles <- renderUI({
    dat <- pitch_metrics_percentile_data()
    if (is.null(dat) || !nrow(dat$overall)) {
      return(div(class = "cr-percentile-card", div(class = "cr-percentile-empty", "No percentile data")))
    }
    overall_rows <- purrr::pmap(
      dat$overall %>% dplyr::select(label, value, percentile, kind),
      function(label, value, percentile, kind) pitch_metric_row_ui(label, value, percentile, kind)
    )
    pitch_sections <- lapply(dat$pitch_types, function(block) {
      tags$details(
        class = "pm-pitch-details",
        tags$summary(paste0(block$pitch_type, " Percentiles \u25BE")),
        div(class = "pm-empty", paste0(block$pitches, " pitches")),
        purrr::pmap(
          block$rows %>% dplyr::select(label, value, percentile, kind),
          function(label, value, percentile, kind) pitch_metric_row_ui(label, value, percentile, kind)
        )
      )
    })
    if (!length(pitch_sections)) {
      pitch_sections <- list(div(class = "cr-percentile-empty", "No pitch type data for current filters."))
    }
    div(
      class = "cr-percentile-card",
      div(
        class = "cr-percentile-header",
        div(class = "cr-percentile-title", "D1 Percentile Rankings"),
        div(class = "cr-percentile-subtitle", name_display(input$PitcherInput %||% "Pitcher"))
      ),
      div(
        class = "cr-percentile-scale",
        span("POOR"),
        span("AVERAGE"),
        span("GREAT")
      ),
      div(class = "cr-percentile-rows", tagList(overall_rows)),
      div(class = "pm-section", div(class = "cr-percentile-title", "D1 PitchType Percentiles")),
      pitch_sections
    )
  })
  
  location_outcome_base <- reactive({
    d <- dataFilter()
    if (!"row_id" %in% names(d)) d$row_id <- seq_len(nrow(d))
    
    date_val <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("PitchDate" %in% names(d)) {
      parse_date_any(d$PitchDate)
    } else if ("CustomGameID" %in% names(d)) {
      parse_gameid_date(d$CustomGameID)
    } else {
      as.Date(NA)
    }
    date_lbl <- ifelse(is.na(date_val), "", format(date_val, "%Y-%m-%d"))
    if ("CustomGameID" %in% names(d)) {
      date_lbl <- ifelse(!nzchar(date_lbl), as.character(d$CustomGameID), date_lbl)
    }
    
    known <- names(pitch_colors)
    d <- d %>%
      dplyr::mutate(
        PlateLocSide   = suppressWarnings(as.numeric(PlateLocSide)),
        PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight)),
        RelSpeed       = suppressWarnings(as.numeric(RelSpeed)),
        InducedVertBreak = suppressWarnings(as.numeric(InducedVertBreak)),
        HorzBreak      = suppressWarnings(as.numeric(HorzBreak)),
        SpinRate       = suppressWarnings(as.numeric(SpinRate)),
        PitchType_plot = dplyr::if_else(
          is.na(PitchType) | !(as.character(PitchType) %in% known),
          "Undefined",
          as.character(PitchType)
        ),
        row_id_chr = as.character(row_id),
        LocDate   = date_lbl,
        HoverText = paste0(
          "Date: ", date_lbl,
          "<br>MPH: ", ifelse(is.finite(RelSpeed), sprintf("%.1f", RelSpeed), "—"),
          "<br>iVB: ", ifelse(is.finite(InducedVertBreak), sprintf("%.1f", InducedVertBreak), "—"),
          "<br>HB: ", ifelse(is.finite(HorzBreak), sprintf("%.1f", HorzBreak), "—"),
          "<br>Spin: ", ifelse(is.finite(SpinRate), sprintf("%.0f", SpinRate), "—")
        )
      )
    
    d <- d %>%
      dplyr::filter(
        is.finite(PlateLocSide), is.finite(PlateLocHeight),
        dplyr::between(PlateLocSide, -3, 3),
        dplyr::between(PlateLocHeight, 0, 5)
      )
    
    # Fallback: if the strict window wipes everything, keep finite locations
    if (!nrow(d)) {
      d <- d %>%
        dplyr::filter(
          is.finite(PlateLocSide), is.finite(PlateLocHeight)
        )
    }
    
    d
  })
  
  location_outcome_data <- reactive({
    d <- location_outcome_base()
    if (!nrow(d)) return(d)
    
    evla <- resolve_ev_la(d)
    pr <- if ("PlayResult" %in% names(d)) d$PlayResult else NULL
    
    d$IsBarrel <- is_barrel_from_row(d$PitchCall, pr, evla$ev, evla$la)
    
    pc_raw <- tolower(trimws(as.character(d$PitchCall %||% "")))
    d$IsWhiff <- pc_raw %in% c("strikeswinging","swingingstrike","swingingstrikeblocked","swinging strike","swinging strike blocked") |
      (grepl("swing", pc_raw) & grepl("strike", pc_raw))
    d$IsCalledStrike <- pc_raw %in% c("strikecalled","calledstrike","called strike")
    
    if ("Chase" %in% names(d)) {
      ch_raw <- d$Chase
      d$IsChase <- if (is.logical(ch_raw)) ch_raw else {
        ch_chr <- tolower(trimws(as.character(ch_raw %||% "")))
        ch_chr %in% c("1","true","t","yes","y","chase")
      }
    } else {
      inz <- if ("InZone" %in% names(d)) as.logical(d$InZone) else if ("inZone" %in% names(d)) d$inZone == 1 else NA
      swing <- if ("IsSwing" %in% names(d)) as.logical(d$IsSwing) else .is_swing_event(pc_raw)
      d$IsChase <- !is.na(inz) & inz == FALSE & swing == TRUE
    }
    
    d
  })
  
  hover_link_js <- "
function(el,x){
  if(el._hoverLinked) return;
  el._hoverLinked = true;
  var baseMetas = ['movement_base','extension_base','release_base'];
  var axisMap = {
    movement_base: {xref: 'x',  yref: 'y'},
    extension_base:{xref: 'x2', yref: 'y2'},
    release_base:  {xref: 'x3', yref: 'y3'}
  };
  function getKey(pt){
    if(pt.customdata !== undefined && pt.customdata !== null) return pt.customdata;
    if(pt.id !== undefined && pt.id !== null) return pt.id;
    if(pt.key !== undefined && pt.key !== null) return pt.key;
    return null;
  }
  function traceIndices(metaVal){
    var out = [];
    for(var i=0;i<el.data.length;i++){
      if(el.data[i].meta === metaVal) out.push(i);
    }
    return out;
  }
  function findPoint(metaVal, key){
    var indices = traceIndices(metaVal);
    for(var t=0;t<indices.length;t++){
      var ti = indices[t];
      var cd = el.data[ti].customdata;
      if(!cd) continue;
      for(var i=0;i<cd.length;i++){
        if(cd[i] == key){
          return {ti: ti, i: i};
        }
      }
    }
    return null;
  }
  function pointInfo(metaVal, key){
    var hit = findPoint(metaVal, key);
    if(!hit) return null;
    var trace = el.data[hit.ti];
    var axes = axisMap[metaVal] || {xref: 'x', yref: 'y'};
    return {
      x: trace.x[hit.i],
      y: trace.y[hit.i],
      text: (trace.text && trace.text[hit.i]) ? trace.text[hit.i] : '',
      xref: axes.xref,
      yref: axes.yref
    };
  }
  function baseAnnotations(){
    if(!el._baseAnnots){
      el._baseAnnots = (el.layout && el.layout.annotations) ? el.layout.annotations.slice() : [];
    }
    return el._baseAnnots;
  }
  function clickAnnotation(info){
    if(!info) return null;
    return {
      x: info.x,
      y: info.y,
      xref: info.xref,
      yref: info.yref,
      text: info.text,
      showarrow: true,
      arrowhead: 6,
      ax: 18,
      ay: -18,
      align: 'left',
      bgcolor: 'rgba(255,255,255,0.95)',
      bordercolor: '#333',
      borderwidth: 1,
      font: {size: 11, color: '#111'}
    };
  }
  function debugAnnotation(text){
    return {
      x: 0.01,
      y: 0.99,
      xref: 'paper',
      yref: 'paper',
      text: text,
      showarrow: false,
      align: 'left',
      bgcolor: 'rgba(255,255,255,0.9)',
      bordercolor: '#999',
      borderwidth: 1,
      font: {size: 10, color: '#111'}
    };
  }
  function setClickAnnotations(key, meta){
    var annots = baseAnnotations().slice();
    var mv  = pointInfo('movement_base', key);
    var ext = pointInfo('extension_base', key);
    var rel = pointInfo('release_base', key);
    var dbg = debugAnnotation(
      'click meta: ' + meta +
      '<br>key: ' + key +
      '<br>mv:' + (mv ? 1 : 0) + ' ext:' + (ext ? 1 : 0) + ' rel:' + (rel ? 1 : 0)
    );
    if(dbg) annots.push(dbg);
    if(window.Shiny && Shiny.setInputValue){
      Shiny.setInputValue('pitch_metrics_click', {
        meta: meta, key: key,
        mv: (mv ? 1 : 0), ext: (ext ? 1 : 0), rel: (rel ? 1 : 0),
        ts: Date.now()
      }, {priority: 'event'});
    }
    [mv, ext, rel].forEach(function(info){
      var a = clickAnnotation(info);
      if(a) annots.push(a);
    });
    Plotly.relayout(el, {annotations: annots});
  }
  function clearAnnotations(){
    Plotly.relayout(el, {annotations: baseAnnotations()});
  }
  el.on('plotly_click', function(e){
    if(!e || !e.points || !e.points.length) return;
    var meta = e.points[0].data && e.points[0].data.meta;
    if(baseMetas.indexOf(meta) === -1) return;
    var key = getKey(e.points[0]);
    if(key === null || key === undefined) return;
    setClickAnnotations(key, meta);
  });
  el.on('plotly_doubleclick', function(){
    clearAnnotations();
  });
  if(window.Shiny && Shiny.setInputValue){
    Shiny.setInputValue('pitch_metrics_onrender', {ts: Date.now()}, {priority: 'event'});
  }
}
"
  
  output$pitch_metrics_plot <- renderPlotly({
    mv  <- movement_plot_data()
    
    hand  <- guess_throw_hand(input$PitcherInput, mv)
    slope <- movement_line_slope(hand, mv)
    signx <- if (identical(hand, "LHP")) -1 else 1
    mv_lim <- 30
    
    # Arm-angle line endpoints (if valid)
    x_end <- NA_real_; y_end <- NA_real_
    if (is.finite(slope)) {
      if (abs(slope) <= 1) {
        x_end <- mv_lim * signx
        y_end <- mv_lim * slope
      } else {
        y_end <- mv_lim * sign(slope)
        x_end <- signx * (y_end / slope)
      }
    }
    
    mv_avg <- mv %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(
        HB   = mean(HorzBreak, na.rm = TRUE),
        IVB  = mean(InducedVertBreak, na.rm = TRUE),
        Velo = mean(RelSpeed,  na.rm = TRUE),
        Spin = mean(SpinRate,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        HB   = ifelse(is.finite(HB), HB, NA_real_),
        IVB  = ifelse(is.finite(IVB), IVB, NA_real_),
        Velo = ifelse(is.finite(Velo), Velo, NA_real_),
        Spin = ifelse(is.finite(Spin), Spin, NA_real_)
      )
    mv_avg$label <- sprintf(
      "%s Avg<br>Velo: %s<br>iVB: %s<br>HB: %s<br>Spin: %s",
      mv_avg$PitchType_plot,
      ifelse(is.finite(mv_avg$Velo), sprintf("%.1f", mv_avg$Velo), "—"),
      ifelse(is.finite(mv_avg$IVB),  sprintf("%.1f", mv_avg$IVB),  "—"),
      ifelse(is.finite(mv_avg$HB),   sprintf("%.1f", mv_avg$HB),   "—"),
      ifelse(is.finite(mv_avg$Spin), sprintf("%.0f", mv_avg$Spin), "—")
    )
    
    p <- plotly::plot_ly(source = "pitch_metrics")
    
    if (nrow(mv) > 0) {
      p <- p %>%
        add_trace(
          data = mv,
          x = ~HorzBreak, y = ~InducedVertBreak,
          type = "scatter", mode = "markers",
          color = ~PitchType_plot, colors = pitch_colors,
          marker = list(size = 8, opacity = 0.3),
          text  = ~HoverMovement,
          key = ~row_id_chr,
          customdata = ~row_id_chr,
          ids = ~row_id_chr,
          meta = "movement_base",
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
    }
    
    if (nrow(mv_avg) > 0) {
      for (i in seq_len(nrow(mv_avg))) {
        row <- mv_avg[i, ]
        p <- p %>%
          add_trace(
            data = row,
            x = ~HB, y = ~IVB,
            type = "scatter", mode = "markers",
            color = ~PitchType_plot, colors = pitch_colors,
            marker = list(size = 16, opacity = 1, line = list(color = "black", width = 0.6)),
            text = ~label,
            hoverinfo = "text",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }
    
    # Movement axes shapes
    shp <- list(
      list(type = "line", x0 = -mv_lim, x1 = mv_lim, y0 = 0,  y1 = 0,  xref = "x", yref = "y",
           line = list(dash = "dot", width = 1, color = "black")),
      list(type = "line", x0 = 0,  x1 = 0,  y0 = -mv_lim, y1 = mv_lim, xref = "x", yref = "y",
           line = list(dash = "dot", width = 1, color = "black"))
    )
    if (is.finite(x_end) && is.finite(y_end)) {
      shp <- c(shp, list(
        list(type = "line", x0 = 0, y0 = 0, x1 = x_end, y1 = y_end, xref = "x", yref = "y",
             line = list(dash = "dash", width = 2, color = "rgba(80,18,20,0.85)")) 
      ))
    }
    
    p <- p %>%
      layout(
        title = list(text = paste0(input$PitcherInput, ": Pitch Metrics"), x = 0.5),
        hovermode = "closest",
        clickmode = "event",
        dragmode = if (isTRUE(rv$retag)) "lasso" else "zoom",
        margin  = list(t = 70, r = 10, b = 40, l = 60),
        xaxis = list(
          title = "Horizontal Break (in)",
          range = c(-mv_lim, mv_lim), autorange = FALSE, dtick = 3, tickformat = ".0f",
          zeroline = FALSE, fixedrange = TRUE,
          constrain = "domain", constraintoward = "center"
        ),
        yaxis = list(
          title = "Induced Vertical Break (in)",
          range = c(-mv_lim, mv_lim), autorange = FALSE, dtick = 3, tickformat = ".0f",
          zeroline = FALSE, fixedrange = TRUE, scaleanchor = "x", scaleratio = 1,
          constrain = "domain", constraintoward = "center"
        ),
        shapes = shp
      )
    
    p <- plotly::event_register(p, "plotly_selected")
    p <- plotly::event_register(p, "plotly_click")
    p
  })
  
  output$extension_plot <- renderPlotly({
    d <- extension_plot_data()
    validate(need(nrow(d) > 0, "No extension data for current filters."))
    
    avg <- d %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(
        RelSide   = mean(RelSide, na.rm = TRUE),
        Extension = mean(Extension, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(RelSide), is.finite(Extension))
    
    p <- plotly::plot_ly()
    
    if (nrow(d) > 0) {
      p <- p %>%
        add_trace(
          data = d,
          x = ~RelSide, y = ~Extension,
          type = "scatter", mode = "markers",
          color = ~PitchType_plot, colors = pitch_colors,
          marker = list(size = 6, opacity = 0.35),
          text = ~HoverExtension,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
    }
    
    if (nrow(avg) > 0) {
      for (i in seq_len(nrow(avg))) {
        row <- avg[i, ]
        p <- p %>%
          add_trace(
            data = row,
            x = ~RelSide, y = ~Extension,
            type = "scatter", mode = "markers",
            color = ~PitchType_plot, colors = pitch_colors,
            marker = list(size = 16, opacity = 1, line = list(color = "black", width = 0.6)),
            text = ~paste0(PitchType_plot, " avg extension"),
            hoverinfo = "text",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }
    
    rubber <- list(
      list(
        type = "rect",
        x0 = -1, x1 = 1,
        y0 = 0,  y1 = 0.5,
        xref = "x", yref = "y",
        line = list(color = "black", width = 1),
        fillcolor = "rgba(0,0,0,0.15)"
      )
    )
    
    home_plate <- list(
      list(
        type = "path",
        path = "M -0.75,7.5 L 0.75,7.5 L 0.75,7.8 L 0,8 L -0.75,7.8 Z",
        xref = "x", yref = "y",
        line = list(color = "black", width = 1),
        fillcolor = "rgba(0,0,0,0.08)"
      )
    )
    
    arc_ext <- tibble::tibble(
      x = seq(-3, 3, length.out = 200)
    ) %>%
      dplyr::mutate(y = sqrt(pmax(0, 9 - x^2)))
    
    p <- p %>%
      add_trace(
        data = arc_ext,
        x = ~x, y = ~y,
        type = "scatter", mode = "lines",
        line = list(color = "#8B5A2B", width = 2),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    p %>% layout(
      title = list(text = paste0(input$PitcherInput, ": Extension (Bird's Eye)"), x = 0.5),
      margin = list(t = 60, r = 10, b = 40, l = 50),
      xaxis = list(title = "Release Side (ft)", range = c(-4, 4), fixedrange = TRUE, zeroline = FALSE),
      yaxis = list(title = "Extension (ft)", range = c(0, 8), fixedrange = TRUE, zeroline = FALSE, automargin = TRUE),
      shapes = c(rubber, home_plate)
    )
  })
  
  output$release_plot <- renderPlotly({
    d <- release_plot_data()
    validate(need(nrow(d) > 0, "No release data for current filters."))
    
    avg <- d %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(
        RelSide   = mean(RelSide, na.rm = TRUE),
        RelHeight = mean(RelHeight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight))
    
    p <- plotly::plot_ly()
    
    if (nrow(d) > 0) {
      p <- p %>%
        add_trace(
          data = d,
          x = ~RelSide, y = ~RelHeight,
          type = "scatter", mode = "markers",
          color = ~PitchType_plot, colors = pitch_colors,
          marker = list(size = 6, opacity = 0.35),
          text = ~HoverRelease,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
    }
    
    if (nrow(avg) > 0) {
      for (i in seq_len(nrow(avg))) {
        row <- avg[i, ]
        p <- p %>%
          add_trace(
            data = row,
            x = ~RelSide, y = ~RelHeight,
            type = "scatter", mode = "markers",
            color = ~PitchType_plot, colors = pitch_colors,
            marker = list(size = 16, opacity = 1, line = list(color = "black", width = 0.6)),
            text = ~paste0(PitchType_plot, " avg release"),
            hoverinfo = "text",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }
    
    mound_shapes <- list(
      list(
        type = "path",
        path = "M -1,0 A 1,1 0 0 1 1,0 L 1,0 L -1,0 Z",
        xref = "x", yref = "y",
        line = list(color = "black", width = 1),
        fillcolor = "rgba(0,0,0,0.08)"
      ),
      list(
        type = "rect",
        x0 = -0.2, x1 = 0.2,
        y0 = 0.85, y1 = 0.95,
        xref = "x", yref = "y",
        line = list(color = "black", width = 1),
        fillcolor = "rgba(0,0,0,0.15)"
      ),
      list(
        type = "path",
        path = "M -1,0 A 1,1 0 0 1 1,0",
        xref = "x", yref = "y",
        line = list(color = "#8B5A2B", width = 2),
        fillcolor = "rgba(0,0,0,0)"
      )
    )
    
    arc_rel <- tibble::tibble(
      x = seq(-1, 1, length.out = 200)
    ) %>%
      dplyr::mutate(y = sqrt(pmax(0, 1 - x^2)))
    
    p <- p %>%
      add_trace(
        data = arc_rel,
        x = ~x, y = ~y,
        type = "scatter", mode = "lines",
        line = list(color = "#8B5A2B", width = 2),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    
    p %>% layout(
      title = list(text = paste0(input$PitcherInput, ": Release (Catcher POV)"), x = 0.5),
      margin = list(t = 60, r = 10, b = 40, l = 50),
      xaxis = list(title = "Release Side (ft)", range = c(4, -4), fixedrange = TRUE, zeroline = FALSE),
      yaxis = list(title = "Release Height (ft)", range = c(0, 7), fixedrange = TRUE, zeroline = FALSE, automargin = TRUE),
      shapes = mound_shapes
    )
  })

  output$pitch_metrics_debug <- renderText({
    info <- input$pitch_metrics_click
    if (is.null(info)) {
      return("Pitch metrics debug: click a pitch to log details.")
    }
    paste0(
      "Pitch metrics debug | meta: ", info$meta,
      " | key: ", info$key,
      " | mv:", info$mv,
      " ext:", info$ext,
      " rel:", info$rel
    )
  })
  
  # --- Metrics table (pitch metrics only) ---
  output$metrics <- DT::renderDT({
    d <- dataFilter()
    
    # --- Rulebook zone height thirds (inches); plate side ignored ---
    z_min <- 18.29
    z_max <- 44.08
    z_third <- (z_max - z_min) / 3
    h_in <- suppressWarnings(readr::parse_number(as.character(d$PlateLocHeight)))
    # Convert feet -> inches when values look like feet
    h_in <- ifelse(is.finite(h_in) & h_in < 10, h_in * 12, h_in)
    d$ZoneHeight_in <- h_in
    d$IZ_T3 <- is.finite(h_in) & h_in >= (z_min + 2 * z_third) & h_in <= z_max
    d$IZ_B3 <- is.finite(h_in) & h_in >= z_min & h_in <= (z_min + z_third)
    
    base_met <- d %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Total = dplyr::n(),
        `Velo (Max)` = {
          vv <- suppressWarnings(as.numeric(RelSpeed))
          m  <- if (any(is.finite(vv))) mean(vv, na.rm = TRUE) else NA_real_
          mx <- if (any(is.finite(vv))) max(vv, na.rm = TRUE) else NA_real_
          
          dplyr::case_when(
            is.finite(m)  & is.finite(mx) ~ sprintf("%.1f (%.1f)", m, mx),
            is.finite(m)  & !is.finite(mx) ~ sprintf("%.1f", m),
            !is.finite(m) & is.finite(mx) ~ sprintf("(%.1f)", mx),
            TRUE ~ "—"
          )
        },
        
        Velo  = {
          vv <- suppressWarnings(as.numeric(RelSpeed))
          if (any(is.finite(vv))) round(mean(vv, na.rm = TRUE), 1) else NA_real_
        },
        VeloMax = {
          vv <- suppressWarnings(as.numeric(RelSpeed))
          if (any(is.finite(vv))) max(vv, na.rm = TRUE) else NA_real_
        },
        Max  = {
          vv <- suppressWarnings(as.numeric(RelSpeed))
          if (any(is.finite(vv))) round(max(vv, na.rm = TRUE), 1) else NA_real_
        },
        iVB   = round(mean(InducedVertBreak,   na.rm = TRUE), 1),
        HB    = round(mean(HorzBreak,          na.rm = TRUE), 1),
        VAA   = round(mean(VertApprAngle,      na.rm = TRUE), 1),
        `IZ T3 VAA` = {
          v <- suppressWarnings(as.numeric(VertApprAngle))
          v <- v[IZ_T3 %in% TRUE]
          if (any(is.finite(v))) round(mean(v, na.rm = TRUE), 1) else NA_real_
        },
        `IZ B3 VAA` = {
          v <- suppressWarnings(as.numeric(VertApprAngle))
          v <- v[IZ_B3 %in% TRUE]
          if (any(is.finite(v))) round(mean(v, na.rm = TRUE), 1) else NA_real_
        },
        HAA   = round(mean(HorzApprAngle,      na.rm = TRUE), 1),
        Spin  = round(mean(SpinRate,           na.rm = TRUE)),
        Gyro  = round(mean(suppressWarnings(as.numeric(SpinAxis3dLongitudinalAngle)), na.rm = TRUE), 1),
        `Height (rel)` = round(mean(RelHeight,  na.rm = TRUE), 1),
        `Side (rel)`   = round(mean(RelSide,    na.rm = TRUE), 1),
        Extension      = round(mean(Extension,  na.rm = TRUE), 1),
        swings    = sum(.is_swing_event(PitchCall),                         na.rm = TRUE),
        swings_loc= sum(.is_swing_event(PitchCall) & !is.na(inZone),         na.rm = TRUE),
        swings_in = sum(inZone == 1 & .is_swing_event(PitchCall),           na.rm = TRUE),
        whiffs    = sum(PitchCall == "StrikeSwinging",                      na.rm = TRUE),
        whiffs_in = sum(inZone == 1 & PitchCall == "StrikeSwinging",        na.rm = TRUE),
        csw_pitches = sum(PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Usage     = Total / sum(Total),
        `Usage%`  = paste0(round(Usage * 100), "%"),
        `Whiff%`  = paste0(round(ifelse(swings    == 0, NA_real_, whiffs    / swings    * 100)), "%"),
        `Chase%`  = paste0(round(ifelse(swings_loc == 0, NA_real_, (swings_loc - swings_in) / swings_loc * 100)), "%"),
        `IZWhiff%`= paste0(round(ifelse(swings_in == 0, NA_real_, whiffs_in / swings_in * 100)), "%"),
        `CSW%`    = paste0(round(ifelse(Total     == 0, NA_real_, csw_pitches / Total   * 100)), "%")
      ) %>% dplyr::arrange(dplyr::desc(Usage))
    
    has_col <- function(nm) nm %in% names(d)
    to_num  <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
    first_present <- function(...) { cands <- c(...); hit <- which(cands %in% names(d)); if (length(hit)) cands[hit[1]] else NA_character_ }
    
    # per-PA last pitch
    if (has_col("PAofInning") && has_col("Inning") && has_col("Batter")) {
      d$PA_ID <- interaction(d$Date, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
    } else if (has_col("PitchofPA")) {
      d$PA_ID <- cumsum(d$PitchofPA == 1)
    } else {
      terminal <- d$PitchCall %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled") |
        (has_col("PlayResult") & !is.na(d$PlayResult) & nzchar(d$PlayResult)) |
        (has_col("KorBB") & !is.na(d$KorBB) & nzchar(d$KorBB))
      d$PA_ID <- cumsum(c(1L, head(terminal, -1)))
    }
    pa_last <- d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    is_bip <- is_bip_from(pa_last$PitchCall, if (has_col("PlayResult")) pa_last$PlayResult else NULL)
    pr <- if (has_col("PlayResult")) as.character(pa_last$PlayResult) else rep(NA_character_, nrow(pa_last))
    kb <- if (has_col("KorBB"))      as.character(pa_last$KorBB)      else rep(NA_character_, nrow(pa_last))
    is_bb  <- grepl("(?i)\\bwalk\\b|\\bbb\\b", kb) | grepl("(?i)walk", pr)
    is_ibb <- grepl("(?i)intentional|\\bibb\\b", kb) | grepl("(?i)intentional", pr)
    is_hbp <- grepl("(?i)hbp|hit by pitch", kb) | grepl("(?i)hit by pitch|HBP", pr)
    is_hr  <- grepl("(?i)home ?run|\\bHR\\b", pr)
    is_3b  <- grepl("(?i)triple", pr)
    is_2b  <- grepl("(?i)double(?!\\s*play)", pr, perl = TRUE)
    is_1b  <- grepl("(?i)single", pr)
    d$IsBIP <- is_bip_from(d$PitchCall, if (has_col("PlayResult")) d$PlayResult else NULL)
    ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV")
    la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle")
    ev <- if (!is.na(ev_col)) to_num(pa_last[[ev_col]]) else rep(NA_real_, nrow(pa_last))
    la <- if (!is.na(la_col)) to_num(pa_last[[la_col]]) else rep(NA_real_, nrow(pa_last))
    
    ww <- woba_weights
    woba_obs <- numeric(nrow(pa_last))
    woba_obs[is_bb & !is_ibb] <- ww$BB; woba_obs[is_hbp] <- ww$HBP
    woba_obs[is_1b] <- ww$X1B; woba_obs[is_2b] <- ww$X2B; woba_obs[is_3b] <- ww$X3B; woba_obs[is_hr] <- ww$HR
    woba_denom <- as.numeric(!(is_ibb))
    
    wobacon_obs <- numeric(nrow(pa_last))
    wobacon_obs[is_1b] <- ww$X1B; wobacon_obs[is_2b] <- ww$X2B
    wobacon_obs[is_3b] <- ww$X3B; wobacon_obs[is_hr] <- ww$HR
    wobacon_denom <- as.numeric(is_bip)
    
    pa_summ <- tibble::tibble(
      PitchType = pa_last$PitchType,
      woba_num  = woba_obs,     woba_den  = woba_denom,
      wobacon_num = wobacon_obs, wobacon_den = wobacon_denom
    ) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        wOBA     = if (sum(woba_den, na.rm = TRUE) > 0)    sum(woba_num, na.rm = TRUE)    / sum(woba_den, na.rm = TRUE)    else NA_real_,
        wOBAcon  = if (sum(wobacon_den, na.rm = TRUE) > 0) sum(wobacon_num, na.rm = TRUE) / sum(wobacon_den, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
    
    tbl_metrics <- base_met %>%
      dplyr::select(
        PitchType, Total, `Usage%`,
        Velo, Max, iVB, HB, VAA, `IZ T3 VAA`, `IZ B3 VAA`, HAA,
        Spin, Gyro, `Height (rel)`, `Side (rel)`, Extension
      ) %>%
      dplyr::rename(`Gyro (°)` = Gyro)
    
    cols_to_color <- intersect(names(tbl_metrics), c(names(D1_PCT_AVG), names(ABS_RULES)))
    percent_cols  <- intersect(names(tbl_metrics), names(D1_PCT_AVG))
    lower_better  <- intersect(names(tbl_metrics), c("BB%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"))
    
    tbl_metrics_colored <- shade_columns_txst(
      tbl_metrics,
      cols_to_color = cols_to_color,
      lower_better  = lower_better,
      percent_cols  = percent_cols
    )
    
    DT::datatable(
      tbl_metrics_colored,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom='t', paging=FALSE, ordering=FALSE, autoWidth=TRUE),
      class    = "stripe"
    )
  })

  # --- Performance table (pitch outcomes only) ---
  output$metrics_perf <- DT::renderDT({
    d <- dataFilter()
    
    base_met <- d %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Total = dplyr::n(),
        swings    = sum(.is_swing_event(PitchCall),                         na.rm = TRUE),
        swings_loc= sum(.is_swing_event(PitchCall) & !is.na(inZone),         na.rm = TRUE),
        swings_in = sum(inZone == 1 & .is_swing_event(PitchCall),           na.rm = TRUE),
        whiffs    = sum(PitchCall == "StrikeSwinging",                      na.rm = TRUE),
        whiffs_in = sum(inZone == 1 & PitchCall == "StrikeSwinging",        na.rm = TRUE),
        csw_pitches = sum(PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Usage     = Total / sum(Total),
        `Usage%`  = paste0(round(Usage * 100), "%"),
        `Whiff%`  = paste0(round(ifelse(swings    == 0, NA_real_, whiffs    / swings    * 100)), "%"),
        `Chase%`  = paste0(round(ifelse(swings_loc == 0, NA_real_, (swings_loc - swings_in) / swings_loc * 100)), "%"),
        `IZWhiff%`= paste0(round(ifelse(swings_in == 0, NA_real_, whiffs_in / swings_in * 100)), "%"),
        `CSW%`    = paste0(round(ifelse(Total     == 0, NA_real_, csw_pitches / Total   * 100)), "%")
      )
    
    has_col <- function(nm) nm %in% names(d)
    to_num  <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
    first_present <- function(...) { cands <- c(...); hit <- which(cands %in% names(d)); if (length(hit)) cands[hit[1]] else NA_character_ }
    
    # per-PA last pitch
    if (has_col("PAofInning") && has_col("Inning") && has_col("Batter")) {
      d$PA_ID <- interaction(d$Date, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
    } else if (has_col("PitchofPA")) {
      d$PA_ID <- cumsum(d$PitchofPA == 1)
    } else {
      terminal <- d$PitchCall %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled") |
        (has_col("PlayResult") & !is.na(d$PlayResult) & nzchar(d$PlayResult)) |
        (has_col("KorBB") & !is.na(d$KorBB) & nzchar(d$KorBB))
      d$PA_ID <- cumsum(c(1L, head(terminal, -1)))
    }
    pa_last <- d %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    is_bip <- is_bip_from(pa_last$PitchCall, if (has_col("PlayResult")) pa_last$PlayResult else NULL)
    pr <- if (has_col("PlayResult")) as.character(pa_last$PlayResult) else rep(NA_character_, nrow(pa_last))
    kb <- if (has_col("KorBB"))      as.character(pa_last$KorBB)      else rep(NA_character_, nrow(pa_last))
    is_bb  <- grepl("(?i)\\bwalk\\b|\\bbb\\b", kb) | grepl("(?i)walk", pr)
    is_ibb <- grepl("(?i)intentional|\\bibb\\b", kb) | grepl("(?i)intentional", pr)
    is_hbp <- grepl("(?i)hbp|hit by pitch", kb) | grepl("(?i)hit by pitch|HBP", pr)
    is_hr  <- grepl("(?i)home ?run|\\bHR\\b", pr)
    is_3b  <- grepl("(?i)triple", pr)
    is_2b  <- grepl("(?i)double(?!\\s*play)", pr, perl = TRUE)
    is_1b  <- grepl("(?i)single", pr)
    d$IsBIP <- is_bip_from(d$PitchCall, if (has_col("PlayResult")) d$PlayResult else NULL)
    ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV")
    la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle")
    ev <- if (!is.na(ev_col)) to_num(pa_last[[ev_col]]) else rep(NA_real_, nrow(pa_last))
    la <- if (!is.na(la_col)) to_num(pa_last[[la_col]]) else rep(NA_real_, nrow(pa_last))
    
    ww <- woba_weights
    woba_obs <- numeric(nrow(pa_last))
    woba_obs[is_bb & !is_ibb] <- ww$BB; woba_obs[is_hbp] <- ww$HBP
    woba_obs[is_1b] <- ww$X1B; woba_obs[is_2b] <- ww$X2B; woba_obs[is_3b] <- ww$X3B; woba_obs[is_hr] <- ww$HR
    woba_denom <- as.numeric(!(is_ibb))
    
    wobacon_obs <- numeric(nrow(pa_last))
    wobacon_obs[is_1b] <- ww$X1B; wobacon_obs[is_2b] <- ww$X2B
    wobacon_obs[is_3b] <- ww$X3B; wobacon_obs[is_hr] <- ww$HR
    wobacon_denom <- as.numeric(is_bip)
    
    resolve_stat_vec <- function(df, cands, default_vec) {
      col <- cands[cands %in% names(df)][1]
      if (!is.na(col)) {
        return(list(vec = to_num(df[[col]]), present = TRUE))
      }
      list(vec = default_vec, present = FALSE)
    }
    resolve_rbi_vec <- function(df, outc) {
      rbi_cands <- c("RBI","RBI|PIT","RBI_PIT","RBI (PIT)","RBI_P","RunsBattedIn","Runs_Batted_In","RBIs")
      col <- rbi_cands[rbi_cands %in% names(df)][1]
      if (!is.na(col)) {
        return(list(vec = to_num(df[[col]]), present = TRUE))
      }
      if ("RunsScored" %in% names(df)) {
        rbi <- to_num(df[["RunsScored"]])
        rbi <- ifelse(outc$BIP_pa %in% TRUE, rbi, 0)
        return(list(vec = rbi, present = TRUE))
      }
      list(vec = rep(NA_real_, nrow(df)), present = FALSE)
    }

    pa_summ <- tibble::tibble(
      PitchType = pa_last$PitchType,
      woba_num  = woba_obs,     woba_den  = woba_denom,
      wobacon_num = wobacon_obs, wobacon_den = wobacon_denom
    ) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        wOBA     = if (sum(woba_den, na.rm = TRUE) > 0)    sum(woba_num, na.rm = TRUE)    / sum(woba_den, na.rm = TRUE)    else NA_real_,
        wOBAcon  = if (sum(wobacon_den, na.rm = TRUE) > 0) sum(wobacon_num, na.rm = TRUE) / sum(wobacon_den, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    outc <- pa_outcome_cols(pa_last)
    kb_chr <- if (has_col("KorBB")) as.character(pa_last$KorBB) else rep("", nrow(pa_last))
    pr_chr <- if (has_col("PlayResult")) as.character(pa_last$PlayResult) else rep("", nrow(pa_last))
    ibb_vec <- grepl("intentional|\\bibb\\b", kb_chr, ignore.case = TRUE) |
      grepl("intentional", pr_chr, ignore.case = TRUE)

    tb_default <- as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
    bb_default <- as.numeric(outc$BB) + as.numeric(ibb_vec)
    k_default  <- as.numeric(outc$K)
    hr_default <- as.numeric(outc$HR)

    tb_info <- resolve_stat_vec(pa_last, c("TB|PIT","TB_PIT","TB","TotalBases"), tb_default)
    bb_info <- resolve_stat_vec(pa_last, c("BB|PIT","BB_PIT","BB"), bb_default)
    k_info  <- resolve_stat_vec(pa_last, c("K|PIT","SO|PIT","K_PIT","SO","SO_PIT","SO|PIT"), k_default)
    hr_info <- resolve_stat_vec(pa_last, c("HR|PIT","HR_PIT","HR"), hr_default)
    rbi_info <- resolve_rbi_vec(pa_last, outc)

    prv_tbl <- tibble::tibble(
      PitchType = pa_last$PitchType,
      TB = tb_info$vec,
      BB = bb_info$vec,
      K  = k_info$vec,
      HR = hr_info$vec,
      RBI = rbi_info$vec
    ) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        TB  = sum(TB,  na.rm = TRUE),
        BB  = sum(BB,  na.rm = TRUE),
        K   = sum(K,   na.rm = TRUE),
        HR  = sum(HR,  na.rm = TRUE),
        RBI = if (rbi_info$present) sum(RBI, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
    
    tbl_perf <- base_met %>%
      dplyr::left_join(pa_summ, by = "PitchType") %>%
      dplyr::left_join(prv_tbl, by = "PitchType") %>%
      dplyr::mutate(across(dplyr::any_of(c("wOBA","wOBAcon")), ~ ifelse(is.na(.x), NA, sprintf("%.3f", .x)))) %>%
      dplyr::select(
        PitchType, Total, `Usage%`,
        `Whiff%`, `Chase%`, `IZWhiff%`, `CSW%`,
        wOBA, wOBAcon,
        TB, BB, K, HR, RBI
      )

    prv_vals <- ifelse(
      is.finite(tbl_perf$RBI),
      sdiv((((tbl_perf$TB + tbl_perf$BB - tbl_perf$K) / 4) + tbl_perf$RBI + tbl_perf$HR), tbl_perf$Total) * 100,
      NA_real_
    )
    tbl_perf$pRV <- ifelse(is.finite(prv_vals), sprintf("%.2f", prv_vals), "NA")
    tbl_perf <- tbl_perf %>%
      dplyr::select(
        PitchType, Total, `Usage%`, pRV,
        `Whiff%`, `Chase%`, `IZWhiff%`, `CSW%`,
        wOBA, wOBAcon
      )
    
    cols_to_color <- intersect(names(tbl_perf), c(names(D1_PCT_AVG), names(ABS_RULES)))
    percent_cols  <- intersect(names(tbl_perf), names(D1_PCT_AVG))
    lower_better  <- intersect(names(tbl_perf), c("BB%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"))
    
    tbl_perf_colored <- shade_columns_txst(
      tbl_perf,
      cols_to_color = cols_to_color,
      lower_better  = lower_better,
      percent_cols  = percent_cols
    )
    
    DT::datatable(
      tbl_perf_colored,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom='t', paging=FALSE, ordering=FALSE, autoWidth=TRUE),
      class    = "stripe"
    )
  })

  # --- Self Scouting outputs ---
  output$self_staff_table <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_staff_table(d)
    raw_df <- out_df
    cols_to_color <- intersect(names(out_df), c(names(D1_PCT_AVG), names(ABS_RULES)))
    percent_cols  <- intersect(names(out_df), names(D1_PCT_AVG))
    lower_better  <- intersect(names(out_df), c("BB%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"))
    out_df <- shade_columns_txst(
      out_df,
      cols_to_color = cols_to_color,
      lower_better  = lower_better,
      percent_cols  = percent_cols
    )
    out_df <- force_midpoint_shading(out_df, raw_df, cols = c("wOBA","wOBAcon","SLG","OPS"))
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  output$self_usage_total <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_usage_table(d, hand = NULL)
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  output$self_usage_lhh <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_usage_table(d, hand = "L")
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  output$self_usage_rhh <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_usage_table(d, hand = "R")
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  get_self_loc_data <- function(group_label, two_strike = FALSE) {
    d <- staff_data()
    if (is.null(d) || !nrow(d)) return(d[0, , drop = FALSE])
    p <- prepare_aar_flags(d)
    p$PitchTypeStd <- vapply(p$PitchType, self_scout_pitch_type, character(1))
    p$LocGroup <- vapply(p$PitchTypeStd, self_scout_loc_group, character(1))
    p <- p %>% dplyr::filter(!is.na(.data$LocGroup), .data$LocGroup == group_label)
    if (two_strike) {
      p <- p %>% dplyr::filter(StrikesPre >= 2)
    } else {
      p <- p %>% dplyr::filter(StrikesPre < 2)
    }
    p %>%
      dplyr::filter(
        !is.na(PlateLocSide), !is.na(PlateLocHeight),
        PlateLocSide >= -3, PlateLocSide <= 3,
        PlateLocHeight >= 0, PlateLocHeight <= 5
      )
  }

  output$self_loc_fastball_non2k <- renderPlot({
    d <- get_self_loc_data("Fastball", two_strike = FALSE)
    self_loc_plot(d, "Fastball (Non-2K)")
  })
  output$self_loc_fastball_2k <- renderPlot({
    d <- get_self_loc_data("Fastball", two_strike = TRUE)
    self_loc_plot(d, "Fastball (2K)")
  })
  output$self_loc_sinker_non2k <- renderPlot({
    d <- get_self_loc_data("Sinker", two_strike = FALSE)
    self_loc_plot(d, "Sinker (Non-2K)")
  })
  output$self_loc_sinker_2k <- renderPlot({
    d <- get_self_loc_data("Sinker", two_strike = TRUE)
    self_loc_plot(d, "Sinker (2K)")
  })
  output$self_loc_cutter_slider_non2k <- renderPlot({
    d <- get_self_loc_data("Cutter & Slider", two_strike = FALSE)
    self_loc_plot(d, "Cutter & Slider (Non-2K)")
  })
  output$self_loc_cutter_slider_2k <- renderPlot({
    d <- get_self_loc_data("Cutter & Slider", two_strike = TRUE)
    self_loc_plot(d, "Cutter & Slider (2K)")
  })
  output$self_loc_curve_sweeper_non2k <- renderPlot({
    d <- get_self_loc_data("Curveball & Sweeper", two_strike = FALSE)
    self_loc_plot(d, "Curveball & Sweeper (Non-2K)")
  })
  output$self_loc_curve_sweeper_2k <- renderPlot({
    d <- get_self_loc_data("Curveball & Sweeper", two_strike = TRUE)
    self_loc_plot(d, "Curveball & Sweeper (2K)")
  })
  output$self_loc_change_split_non2k <- renderPlot({
    d <- get_self_loc_data("Changeup & Splitter", two_strike = FALSE)
    self_loc_plot(d, "Changeup & Splitter (Non-2K)")
  })
  output$self_loc_change_split_2k <- renderPlot({
    d <- get_self_loc_data("Changeup & Splitter", two_strike = TRUE)
    self_loc_plot(d, "Changeup & Splitter (2K)")
  })

  output$self_lineup_usage <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_lineup_usage_table(d)
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  output$self_lineup_results <- DT::renderDT({
    d <- staff_data()
    out_df <- build_self_lineup_results_table(d)
    cols_to_color <- intersect(names(out_df), c(names(D1_PCT_AVG), names(ABS_RULES)))
    percent_cols  <- intersect(names(out_df), names(D1_PCT_AVG))
    lower_better  <- intersect(names(out_df), c("BB%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"))
    out_df <- shade_columns_txst(
      out_df,
      cols_to_color = cols_to_color,
      lower_better  = lower_better,
      percent_cols  = percent_cols
    )
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe self-scout-table"
    )
  })

  output$self_inning_usage_plot <- renderPlot({
    d <- staff_data()
    df <- build_self_inning_series_df(d)
    validate(need(!is.null(df) && nrow(df) > 0, "No inning usage data."))

    df <- df %>% dplyr::filter(is.finite(value))
    df$InningStd <- factor(df$InningStd, levels = c(as.character(1:9), "10+"))
    inning_levels <- levels(df$InningStd)
    metrics <- c("Hard","Breaking","Soft","Barrel%","K%")
    colors <- c("Hard" = "red", "Breaking" = "blue", "Soft" = "green", "Barrel%" = "orange", "K%" = "black")

    y_range <- range(df$value, na.rm = TRUE)
    y_range <- c(0, max(y_range, na.rm = TRUE))
    y_ticks <- pretty(y_range)

    op <- par(mar = c(4, 4, 2, 6), xpd = NA, col.axis = "black", col.lab = "black", fg = "black")
    on.exit(par(op), add = TRUE)

    plot(
      NA, xlim = c(1, length(inning_levels)), ylim = y_range,
      xaxt = "n", yaxt = "n", xlab = "Inning", ylab = "Percent"
    )
    axis(1, at = seq_along(inning_levels), labels = inning_levels)
    axis(2, at = y_ticks, labels = scales::percent(y_ticks, accuracy = 1))
    abline(h = y_ticks, col = "#eeeeee", lwd = 1)

    for (m in metrics) {
      sub <- df[df$Metric == m, ]
      if (!nrow(sub)) next
      x <- match(as.character(sub$InningStd), inning_levels)
      lines(x, sub$value, col = colors[[m]], lwd = 2)
      points(x, sub$value, col = colors[[m]], pch = 16)
      text(
        x, sub$value,
        labels = scales::percent(sub$value, accuracy = 1),
        col = colors[[m]], pos = 3, cex = 0.7
      )
    }

    legend(
      "topright",
      legend = c("Hard", "Break", "Soft", "Barrel%", "K%"),
      col = colors[metrics],
      lty = 1, pch = 16,
      bty = "n",
      text.col = colors[metrics],
      inset = c(0.02, 0.02)
    )
  })

  # --- Pitch Sequencing tables ---
  output$pitch_sequence_tables <- renderUI({
    ids <- pitch_seq_ids()
    if (!nrow(ids)) {
      return(div("No pitch sequencing data for current filters."))
    }
    tagList(lapply(seq_len(nrow(ids)), function(i) {
      pt <- ids$pt[i]
      id <- ids$id[i]
      tagList(
        div(class = "table-title mb-1", pt),
        withSpinner(DTOutput(id), type = 4, color = "#501214"),
        div(class = "mb-3")
      )
    }))
  })

  observe({
    ids <- pitch_seq_ids()
    if (!nrow(ids)) return()
    for (i in seq_len(nrow(ids))) {
      local({
        pt_i <- ids$pt[i]
        id_i <- ids$id[i]
        output[[id_i]] <- DT::renderDT({
          base <- pitch_seq_base()
          out_df <- build_pitch_seq_table(pt_i, base$data, base$pa_last, base$pitch_types)
          display_cols <- c("Context","Whiff%","CSW%","Swing%","SLG","wOBA","Barrel%","Chase%","GB%")
          sort_cols_names <- paste0("..sort_", display_cols[display_cols != "Context"])
          display_idx0 <- seq_along(display_cols) - 1
          sort_idx0 <- match(sort_cols_names, names(out_df)) - 1
          sort_idx0 <- sort_idx0[is.finite(sort_idx0)]
          order_defs <- lapply(seq_along(display_cols), function(i) {
            nm <- display_cols[[i]]
            if (nm == "Context") {
              list(targets = display_idx0[[i]], orderDataType = "string")
            } else {
              sname <- paste0("..sort_", nm)
              sidx <- match(sname, names(out_df)) - 1
              if (!is.finite(sidx)) {
                list(targets = display_idx0[[i]], orderDataType = "string")
              } else {
                list(targets = display_idx0[[i]], orderData = sidx)
              }
            }
          })
          DT::datatable(
            out_df,
            rownames = FALSE,
            escape   = FALSE,
            options  = list(
              dom = "t",
              paging = FALSE,
              ordering = TRUE,
              autoWidth = TRUE,
              columnDefs = c(
                if (length(sort_idx0)) list(list(targets = sort_idx0, visible = FALSE)) else list(),
                order_defs
              )
            ),
            class    = "stripe pitch-seq"
          )
        })
      })
    }
  })

  # --- Usage Recs tables ---
  output$usage_recs_tables <- renderUI({
    base <- pitch_seq_base_allhands()
    if (is.null(base$data) || !nrow(base$data) || !length(base$pitch_types)) {
      return(div("No usage recommendations data for current filters."))
    }
    tagList(
      div(class = "table-title mb-1", "Total"),
      withSpinner(DTOutput("usage_recs_total"), type = 4, color = "#501214"),
      div(class = "mb-3"),
      div(class = "table-title mb-1", "vLHH"),
      withSpinner(DTOutput("usage_recs_lhh"), type = 4, color = "#501214"),
      div(class = "mb-3"),
      div(class = "table-title mb-1", "vRHH"),
      withSpinner(DTOutput("usage_recs_rhh"), type = 4, color = "#501214")
    )
  })

  output$usage_recs_total <- DT::renderDT({
    base <- pitch_seq_base_allhands()
    out_df <- build_usage_recs_table(base, hand = NULL)
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe pitch-seq"
    )
  })

  output$usage_recs_lhh <- DT::renderDT({
    base <- pitch_seq_base_allhands()
    out_df <- build_usage_recs_table(base, hand = "L")
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe pitch-seq"
    )
  })

  output$usage_recs_rhh <- DT::renderDT({
    base <- pitch_seq_base_allhands()
    out_df <- build_usage_recs_table(base, hand = "R")
    DT::datatable(
      out_df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class    = "stripe pitch-seq"
    )
  })

  build_pitch_decay_base <- function(d) {
    if (is.null(d) || !nrow(d)) {
      return(list(pitches = tibble::tibble(), pa_last = tibble::tibble()))
    }
    d <- prepare_aar_flags(d)
    d <- ensure_counts(d)
    if (!"CustomGameID" %in% names(d)) d$CustomGameID <- "Game"
    for (nm in c("Inning","PAofInning","PitchNo","PitchNum")) {
      if (!nm %in% names(d)) d[[nm]] <- NA
    }
    d <- d %>%
      dplyr::mutate(
        .inning_num = suppressWarnings(readr::parse_number(as.character(Inning))),
        .paofinning_num = suppressWarnings(readr::parse_number(as.character(PAofInning))),
        .pitchno_num = suppressWarnings(readr::parse_number(as.character(PitchNo))),
        .row_order = dplyr::row_number()
      ) %>%
      dplyr::arrange(CustomGameID, .inning_num, .paofinning_num, .pitchno_num, PitchNum, .row_order) %>%
      dplyr::group_by(CustomGameID) %>%
      dplyr::mutate(
        .game_pitch_order = dplyr::row_number(),
        OutingInning = dplyr::dense_rank(.inning_num)
      ) %>%
      dplyr::ungroup()

    pa_index <- d %>%
      dplyr::group_by(CustomGameID, PA_ID) %>%
      dplyr::summarise(.pa_order = min(.game_pitch_order, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(CustomGameID, .pa_order) %>%
      dplyr::group_by(CustomGameID) %>%
      dplyr::mutate(
        PAOuting = dplyr::row_number(),
        LineupSpot = ((PAOuting - 1L) %% 9L) + 1L,
        TimeThrough = dplyr::case_when(
          PAOuting <= 9L  ~ "First",
          PAOuting <= 18L ~ "Second",
          PAOuting <= 27L ~ "Third",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(TimeThrough))

    d <- d %>% dplyr::left_join(pa_index, by = c("CustomGameID","PA_ID"))
    d <- d %>% dplyr::filter(!is.na(TimeThrough))
    pa_last <- d %>% dplyr::group_by(CustomGameID, PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    list(pitches = d, pa_last = pa_last)
  }

  pitch_decay_base <- reactive({
    build_pitch_decay_base(dataFilter_allhands())
  })

  render_pitch_decay_velocity <- function(base_reactive) {
    plotly::renderPlotly({
    base <- base_reactive()
    d <- base$pitches
    validate(need(!is.null(d) && nrow(d) > 0, "No data for current filters."))
    d <- d %>%
      dplyr::mutate(
        RelSpeedNum = suppressWarnings(as.numeric(RelSpeed)),
        IsHeater = tolower(trimws(as.character(PitchType))) %in% c(
          "fastball","four-seam","four seam","fourseam","4-seam","4 seam",
          "sinker","two-seam","two seam","twoseam","2-seam","2 seam"
        )
      ) %>%
      dplyr::filter(IsHeater, is.finite(OutingInning), is.finite(RelSpeedNum)) %>%
      dplyr::group_by(OutingInning) %>%
      dplyr::summarise(Velo = mean(RelSpeedNum, na.rm = TRUE), Pitches = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(OutingInning)
    validate(need(nrow(d) > 0, "No fastball/sinker velocity data for current filters."))
    plotly::plot_ly(
      d,
      x = ~OutingInning,
      y = ~Velo,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#501214"),
      marker = list(color = "#B4975A"),
      text = ~paste0("Heater<br>Outing Inning: ", OutingInning, "<br>Avg Velo: ", sprintf("%.1f", Velo), "<br>Pitches: ", Pitches),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      plotly::layout(
        title = list(text = "Heater Velocity", x = 0.5),
        xaxis = list(title = "Outing Inning", dtick = 1, tickformat = ".0f"),
        yaxis = list(title = "Average Velocity"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18)
      )
    })
  }

  render_pitch_decay_table <- function(base_reactive) {
    DT::renderDT({
    base <- base_reactive()
    d <- base$pitches
    pa_last <- base$pa_last
    if (is.null(d) || !nrow(d) || is.null(pa_last) || !nrow(pa_last)) {
      return(DT::datatable(data.frame(Status = "No pitch decay data for current filters."), rownames = FALSE, options = list(dom = "t", paging = FALSE)))
    }

    calc_decay_stats <- function(d_sub, pa_sub) {
      if ((is.null(d_sub) || !nrow(d_sub)) && (is.null(pa_sub) || !nrow(pa_sub))) {
        return(c(wOBA = NA_real_, BAA = NA_real_, SLG = NA_real_, Whiff = NA_real_, Chase = NA_real_))
      }
      whiff <- NA_real_
      chase <- NA_real_
      if (!is.null(d_sub) && nrow(d_sub)) {
        swing <- if ("IsSwing" %in% names(d_sub)) as.logical(d_sub$IsSwing) else .is_swing_event(d_sub$PitchCall)
        inz <- if ("InZone" %in% names(d_sub)) as.logical(d_sub$InZone) else if ("inZone" %in% names(d_sub)) d_sub$inZone == 1L else derive_zone_inches(d_sub)
        whiff <- sdiv(sum(as.character(d_sub$PitchCall) == "StrikeSwinging", na.rm = TRUE), sum(swing, na.rm = TRUE))
        chase <- sdiv(sum(!inz & swing, na.rm = TRUE), sum(!inz, na.rm = TRUE))
      }
      woba <- if (!is.null(pa_sub) && nrow(pa_sub)) {
        compute_woba_grouped(pa_sub, rep("x", nrow(pa_sub)))$wOBA[1]
      } else NA_real_
      baa <- NA_real_
      slg <- NA_real_
      if (!is.null(pa_sub) && nrow(pa_sub)) {
        outc <- pa_outcome_cols(pa_sub)
        sf <- grepl("(?i)sacrifice fly|\\bsf\\b", outc$PR_txt)
        h <- sum(as.numeric(outc$X1B) + as.numeric(outc$X2B) + as.numeric(outc$X3B) + as.numeric(outc$HR), na.rm = TRUE)
        tb <- sum(as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR), na.rm = TRUE)
        ab <- pmax(nrow(pa_sub) - sum(as.numeric(outc$BB), na.rm = TRUE) - sum(as.numeric(outc$HBP), na.rm = TRUE) - sum(as.numeric(sf), na.rm = TRUE), 0)
        baa <- sdiv(h, ab)
        slg <- sdiv(tb, ab)
      }
      c(wOBA = woba, BAA = baa, SLG = slg, Whiff = whiff, Chase = chase)
    }

    cell_stats <- function(spot, time) {
      if (identical(spot, "Total")) {
        d_sub <- d %>% dplyr::filter(TimeThrough == time)
        pa_sub <- pa_last %>% dplyr::filter(TimeThrough == time)
      } else {
        d_sub <- d %>% dplyr::filter(LineupSpot == spot, TimeThrough == time)
        pa_sub <- pa_last %>% dplyr::filter(LineupSpot == spot, TimeThrough == time)
      }
      calc_decay_stats(d_sub, pa_sub)
    }

    fmt_dec <- function(x) {
      if (!is.finite(x)) return("NA")
      sub("^0\\.", ".", sprintf("%.3f", x))
    }
    fmt_dec_delta <- function(x) {
      if (!is.finite(x)) return("")
      sub("([+-])0\\.", "\\1.", sprintf("%+.3f", x))
    }
    fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100*x), "NA")
    fmt_pct_delta <- function(x) ifelse(is.finite(x), sprintf("%+.1f%%", 100*x), "")
    delta_piece <- function(txt, good) {
      if (!nzchar(txt)) return("")
      col <- if (isTRUE(good)) "#008000" else "#C00000"
      paste0(" (<span style='color:", col, "; font-weight:700;'>", txt, "</span>)")
    }
    fmt_cell <- function(vals, prev = NULL) {
      if (is.null(prev)) {
        return(paste0(
          fmt_dec(vals[["wOBA"]]), " wOBA<br>",
          fmt_dec(vals[["BAA"]]), " BAA<br>",
          fmt_dec(vals[["SLG"]]), " SLG<br>",
          fmt_pct(vals[["Whiff"]]), " Whiff<br>",
          fmt_pct(vals[["Chase"]]), " Chase"
        ))
      }
      dw <- vals[["wOBA"]] - prev[["wOBA"]]
      dbaa <- vals[["BAA"]] - prev[["BAA"]]
      dslg <- vals[["SLG"]] - prev[["SLG"]]
      dwh <- vals[["Whiff"]] - prev[["Whiff"]]
      dch <- vals[["Chase"]] - prev[["Chase"]]
      paste0(
        fmt_dec(vals[["wOBA"]]), " wOBA", delta_piece(fmt_dec_delta(dw), dw < 0), "<br>",
        fmt_dec(vals[["BAA"]]), " BAA", delta_piece(fmt_dec_delta(dbaa), dbaa < 0), "<br>",
        fmt_dec(vals[["SLG"]]), " SLG", delta_piece(fmt_dec_delta(dslg), dslg < 0), "<br>",
        fmt_pct(vals[["Whiff"]]), " Whiff", delta_piece(fmt_pct_delta(dwh), dwh > 0), "<br>",
        fmt_pct(vals[["Chase"]]), " Chase", delta_piece(fmt_pct_delta(dch), dch > 0)
      )
    }

    spots <- c(as.list(1:9), "Total")
    rows <- lapply(spots, function(spot) {
      s1 <- cell_stats(spot, "First")
      s2 <- cell_stats(spot, "Second")
      s3 <- cell_stats(spot, "Third")
      tibble::tibble(
        `PA Spot` = as.character(spot),
        `First Time Through` = fmt_cell(s1),
        `Second Time Through` = fmt_cell(s2, s1),
        `Third Time Through` = fmt_cell(s3, s2)
      )
    }) %>% dplyr::bind_rows()

    DT::datatable(
      rows,
      rownames = FALSE,
      escape = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
      class = "stripe pitch-decay"
    )
    })
  }

  pitch_decay_game_table <- reactive({
    d <- pitch_decay_base()$pitches
    if (is.null(d) || !nrow(d) || !"CustomGameID" %in% names(d)) {
      return(tibble::tibble(GameID = character(), GameDate = as.Date(character()), Label = character()))
    }
    game_date <- if ("GameDate" %in% names(d)) parse_date_any(d$GameDate) else parse_gameid_date(d$CustomGameID)
    d %>%
      dplyr::mutate(.GameDate = game_date) %>%
      dplyr::filter(!is.na(CustomGameID), nzchar(as.character(CustomGameID))) %>%
      dplyr::group_by(CustomGameID) %>%
      dplyr::summarise(
        GameDate = {
          vals <- .GameDate[!is.na(.GameDate)]
          if (length(vals)) min(vals) else as.Date(NA)
        },
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(GameDate), dplyr::desc(CustomGameID)) %>%
      dplyr::mutate(
        GameID = as.character(CustomGameID),
        Label = ifelse(!is.na(GameDate), paste0(format(GameDate, "%m/%d/%y"), " - ", GameID), GameID)
      ) %>%
      dplyr::select(GameID, GameDate, Label)
  })

  output$pitch_decay_game_picker <- renderUI({
    games <- pitch_decay_game_table()
    if (is.null(games) || !nrow(games)) {
      return(div(class = "text-muted", "No games available for current filters."))
    }
    current <- isolate(input$pitch_decay_game)
    has_current <- !is.null(current) && length(current) > 0 && current[[1]] %in% games$GameID
    selected <- if (has_current) current[[1]] else games$GameID[[1]]
    selectInput(
      "pitch_decay_game",
      "Game:",
      choices = stats::setNames(games$GameID, games$Label),
      selected = selected
    )
  })

  pitch_decay_single_base <- reactive({
    base <- pitch_decay_base()
    games <- pitch_decay_game_table()
    game_id <- input$pitch_decay_game
    missing_game <- is.null(game_id) || length(game_id) == 0 || !nzchar(game_id[[1]])
    if (missing_game && !is.null(games) && nrow(games)) game_id <- games$GameID[[1]]
    missing_game <- is.null(game_id) || length(game_id) == 0 || !nzchar(game_id[[1]])
    if (missing_game) {
      return(list(pitches = tibble::tibble(), pa_last = tibble::tibble()))
    }
    list(
      pitches = base$pitches %>% dplyr::filter(as.character(CustomGameID) == as.character(game_id)),
      pa_last = base$pa_last %>% dplyr::filter(as.character(CustomGameID) == as.character(game_id))
    )
  })

  output$pitch_decay_velocity <- render_pitch_decay_velocity(pitch_decay_base)
  output$pitch_decay_table <- render_pitch_decay_table(pitch_decay_base)
  output$pitch_decay_single_velocity <- render_pitch_decay_velocity(pitch_decay_single_base)
  output$pitch_decay_single_table <- render_pitch_decay_table(pitch_decay_single_base)
  
  # --- Locations heatmap ---
  wblyrm_palette <- c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff")
  
  output$locations <- renderPlot(
    {
      d <- dataFilter() %>%
        dplyr::filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
                      PlateLocSide >= -3, PlateLocSide <= 3,
                      PlateLocHeight >= 0, PlateLocHeight <= 5)
      validate(need(nrow(d) > 2, "Not enough pitches with locations."))
      
      ggplot(d, aes(PlateLocSide, PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(ndensity)),
          geom = "raster",
          contour = FALSE,
          n = 200,
          na.rm = TRUE
        ) +
        scale_fill_gradientn(colors = c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff"), guide = "none") +
        facet_wrap(~PitchType, ncol = 2) +  # <-- always 2 columns (half-screen each)
        labs(
          title = paste0(input$PitcherInput, ": Pitch Location (pitcher's view)"),
          y = "Vertical Location", x = "Horizontal Location"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
          strip.text = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
          panel.background = element_rect(fill = "transparent", color = NA),
          legend.position = "none",
          plot.margin = margin(10, 10, 10, 10)
        ) +
        geom_rect(
          data = strike_zone,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.2
        ) +
        geom_segment(
          data = home_plate_segments,
          aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, colour = "black", linewidth = 0.9
        ) +
        coord_fixed(xlim = c(-3,3), ylim = c(0,5), expand = FALSE)
    },
    height = function() {
      # 2 columns; each row gets ~450px. Don't condense—just grow.
      d <- dataFilter()
      n_types <- length(unique(na.omit(as.character(d$PitchType))))
      rows <- ceiling(max(1, n_types) / 2)
      as.integer(450 * rows + 100)  # +100 for titles/margins
    }
  )
  
  # --- Outcome location tabs (per pitch type) ---
  make_safe_id <- function(x) {
    out <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
    out <- gsub("^_|_$", "", out)
    if (!nzchar(out)) out <- "pitch"
    out
  }
  
  pitch_type_tabs <- reactive({
    d <- location_outcome_base()
    types <- sort(unique(na.omit(as.character(d$PitchType_plot %||% d$PitchType))))
    types <- types[nzchar(types)]
    if (!length(types)) {
      return(tibble::tibble(PitchType = character(), id = character()))
    }
    
    known <- intersect(names(pitch_colors), types)
    other <- setdiff(types, names(pitch_colors))
    types <- c(known, sort(other))
    
    ids <- vapply(types, make_safe_id, character(1))
    if (any(duplicated(ids))) ids <- make.unique(ids, sep = "_")
    
    tibble::tibble(PitchType = types, id = ids)
  })
  
  output$outcome_location_tabs <- renderUI({
    pts <- pitch_type_tabs()
    if (!nrow(pts)) {
      return(div("No pitch types available for current filters."))
    }
    
    tabs <- lapply(seq_len(nrow(pts)), function(i) {
      pt <- pts$PitchType[i]
      id <- pts$id[i]
      
      nav_panel(
        title = pt,
        fluidRow(
          column(6, withSpinner(plotlyOutput(paste0("loc_whiff_", id),  height = "450px", width = "100%"), type = 4, color = "#501214")),
          column(6, withSpinner(plotlyOutput(paste0("loc_chase_", id),  height = "450px", width = "100%"), type = 4, color = "#501214"))
        ),
        fluidRow(
          column(6, withSpinner(plotlyOutput(paste0("loc_called_", id), height = "450px", width = "100%"), type = 4, color = "#501214")),
          column(6, withSpinner(plotlyOutput(paste0("loc_barrel_", id), height = "450px", width = "100%"), type = 4, color = "#501214"))
        )
      )
    })
    
    do.call(navset_tab, c(list(id = "outcome_pitch_tabs"), tabs))
  })
  
  make_location_plotly <- function(d, title) {
    avg <- d %>%
      dplyr::group_by(PitchType_plot) %>%
      dplyr::summarise(
        PlateLocSide   = mean(PlateLocSide, na.rm = TRUE),
        PlateLocHeight = mean(PlateLocHeight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
    
    p <- plotly::plot_ly()
    
    if (nrow(d) > 0) {
      p <- p %>%
        add_trace(
          data = d,
          x = ~PlateLocSide, y = ~PlateLocHeight,
          type = "scatter", mode = "markers",
          color = ~PitchType_plot, colors = pitch_colors,
          marker = list(size = 6, opacity = 0.35),
          text = ~HoverText,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
    }
    
    if (nrow(avg) > 0) {
      for (i in seq_len(nrow(avg))) {
        row <- avg[i, ]
        p <- p %>%
          add_trace(
            data = row,
            x = ~PlateLocSide, y = ~PlateLocHeight,
            type = "scatter", mode = "markers",
            color = ~PitchType_plot, colors = pitch_colors,
            marker = list(size = 16, opacity = 1, line = list(color = "black", width = 0.6)),
            text = ~paste0(PitchType_plot, " avg location"),
            hoverinfo = "text",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }
    
    zone_shapes <- list(
      list(type = "rect",
           x0 = strike_zone$xmin, x1 = strike_zone$xmax,
           y0 = strike_zone$ymin, y1 = strike_zone$ymax,
           xref = "x", yref = "y",
           line = list(color = "black", width = 1),
           fillcolor = "rgba(0,0,0,0)")
    )
    for (i in seq_len(nrow(home_plate_segments))) {
      zone_shapes <- c(zone_shapes, list(
        list(type = "line",
             x0 = home_plate_segments$x[i], y0 = home_plate_segments$y[i],
             x1 = home_plate_segments$xend[i], y1 = home_plate_segments$yend[i],
             xref = "x", yref = "y",
             line = list(color = "black", width = 1))
      ))
    }
    
    loc_xlim <- c(-1.07, 1.07)
    loc_ylim <- c(0.9, 4.1)
    
    p <- p %>%
      layout(
        title = list(text = title, x = 0.5),
        margin = list(t = 50, r = 10, b = 20, l = 10),
        xaxis = list(
          range = loc_xlim, fixedrange = TRUE,
          showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = list(text = "")
        ),
        yaxis = list(
          range = loc_ylim, fixedrange = TRUE,
          showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = list(text = ""),
          scaleanchor = "x", scaleratio = 1
        ),
        shapes = zone_shapes
      )
    
    p
  }
  
  build_outcome_plot <- function(outcome, pitch_type) {
    d <- location_outcome_data()
    if (!nrow(d)) {
      validate(need(FALSE, "No pitch data for current filters."))
    }
    
    pt_key <- tolower(trimws(as.character(pitch_type %||% "")))
    if ("PitchType" %in% names(d)) {
      d$PitchType_norm <- tolower(trimws(as.character(d$PitchType)))
    }
    if ("PitchType_plot" %in% names(d)) {
      d$PitchType_plot_norm <- tolower(trimws(as.character(d$PitchType_plot)))
    }
    if ("PitchType_norm" %in% names(d)) {
      d <- d %>% dplyr::filter(PitchType_norm == pt_key)
    } else if ("PitchType_plot_norm" %in% names(d)) {
      d <- d %>% dplyr::filter(PitchType_plot_norm == pt_key)
    }
    if (!nrow(d) && "PitchType_plot_norm" %in% names(d)) {
      d <- d %>% dplyr::filter(PitchType_plot_norm == pt_key)
    }
    if (!nrow(d)) {
      validate(need(FALSE, "No pitch data for current filters."))
    }
    
    # Recompute outcome flags locally for robustness
    pc_raw <- tolower(trimws(as.character(d$PitchCall %||% "")))
    is_whiff <- pc_raw %in% c("strikeswinging","swingingstrike","swingingstrikeblocked","swinging strike","swinging strike blocked") |
      (grepl("swing", pc_raw) & grepl("strike", pc_raw))
    is_called <- pc_raw %in% c("strikecalled","calledstrike","called strike")
    
    if ("Chase" %in% names(d)) {
      ch_raw <- d$Chase
      is_chase <- if (is.logical(ch_raw)) ch_raw else {
        ch_chr <- tolower(trimws(as.character(ch_raw %||% "")))
        ch_chr %in% c("1","true","t","yes","y","chase")
      }
    } else {
      inz <- if ("InZone" %in% names(d)) as.logical(d$InZone) else if ("inZone" %in% names(d)) d$inZone == 1 else NA
      swing <- if ("IsSwing" %in% names(d)) as.logical(d$IsSwing) else .is_swing_event(pc_raw)
      is_chase <- !is.na(inz) & inz == FALSE & swing == TRUE
    }
    
    is_barrel <- if ("IsBarrel" %in% names(d)) {
      d$IsBarrel %in% TRUE
    } else {
      evla <- resolve_ev_la(d)
      pr <- if ("PlayResult" %in% names(d)) d$PlayResult else NULL
      is_barrel_from_row(d$PitchCall, pr, evla$ev, evla$la)
    }
    
    empty_msg <- NULL
    if (outcome == "Whiff") {
      d <- d[is_whiff %in% TRUE, , drop = FALSE]
      empty_msg <- "No whiffs for current filters."
    } else if (outcome == "Chase") {
      d <- d[is_chase %in% TRUE, , drop = FALSE]
      empty_msg <- "No chases for current filters."
    } else if (outcome == "Called Strike") {
      d <- d[is_called %in% TRUE, , drop = FALSE]
      empty_msg <- "No called strikes for current filters."
    } else if (outcome == "Barrel") {
      d <- d[is_barrel %in% TRUE, , drop = FALSE]
      empty_msg <- "No barrels for current filters."
    }
    
    validate(need(nrow(d) > 0, empty_msg))
    
    make_location_plotly(
      d,
      paste0(input$PitcherInput, ": ", outcome, " (pitcher's view)")
    )
  }
  
  observe({
    pts <- pitch_type_tabs()
    if (!nrow(pts)) return()
    
    for (i in seq_len(nrow(pts))) {
      local({
        pt <- pts$PitchType[i]
        id <- pts$id[i]
        
        output[[paste0("loc_whiff_", id)]] <- renderPlotly({
          build_outcome_plot("Whiff", pt)
        })
        output[[paste0("loc_chase_", id)]] <- renderPlotly({
          build_outcome_plot("Chase", pt)
        })
        output[[paste0("loc_called_", id)]] <- renderPlotly({
          build_outcome_plot("Called Strike", pt)
        })
        output[[paste0("loc_barrel_", id)]] <- renderPlotly({
          build_outcome_plot("Barrel", pt)
        })
      })
    }
  })
  pct_safe <- function(num, den) ifelse(den > 0, 100 * num / den, NA_real_)

  # ======== Self Scouting helpers ========
  fmt_pct0 <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
  fmt_num2 <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "NA")
  fmt_num3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
  fmt_count <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 0, big.mark = ","), "NA")
  fmt_pct_count <- function(pct, n) {
    ifelse(is.finite(pct), sprintf("%.0f%% (%s)", 100 * pct, fmt_count(n)), "NA")
  }

  force_midpoint_shading <- function(shaded_df, raw_df, cols) {
    if (is.null(shaded_df) || !nrow(shaded_df)) return(shaded_df)
    for (nm in cols) {
      if (!nm %in% names(shaded_df) || !nm %in% names(raw_df)) next
      rule <- ABS_RULES[[nm]]
      if (is.null(rule) || is.null(rule$green_max) || is.null(rule$red_min)) next
      avg <- (rule$green_max + rule$red_min) / 2
      txt <- as.character(raw_df[[nm]])
      v <- suppressWarnings(readr::parse_number(txt))
      fills <- ifelse(is.finite(v), ifelse(v <= avg, CF_GREEN, CF_RED), NA_character_)
      order_attr <- ifelse(is.finite(v), as.character(v), "")
      shaded_df[[nm]] <- ifelse(
        is.na(fills),
        sprintf("<span class='cf-cell' data-order='%s'>%s</span>", order_attr, txt),
        sprintf("<span class='cf-cell' data-order='%s' style='background-color:%s'>%s</span>", order_attr, fills, txt)
      )
    }
    shaded_df
  }

  normalize_batter_side <- function(x) {
    dplyr::case_when(
      x %in% c("L","Left","LHH","LH") ~ "L",
      x %in% c("R","Right","RHH","RH") ~ "R",
      TRUE ~ as.character(x)
    )
  }

  summarize_batting_simple <- function(pa_last_df, grp_vec) {
    if (is.null(pa_last_df) || !nrow(pa_last_df)) {
      return(tibble::tibble())
    }
    outc <- pa_outcome_cols(pa_last_df)
    pc_vec <- if ("PitchCall" %in% names(pa_last_df)) as.character(pa_last_df$PitchCall) else rep("", nrow(pa_last_df))
    hbp_vec <- grepl("(?i)hit\\s*by\\s*pitch|\\bhbp\\b|hitbypitch", pc_vec)
    sf_vec <- grepl("(?i)sacrifice fly|\\bsf\\b", outc$PR_txt)
    h_vec  <- outc$X1B | outc$X2B | outc$X3B | outc$HR
    tb_vec <- as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
    
    tibble::tibble(
      grp = grp_vec,
      K   = as.numeric(outc$K),
      BB  = as.numeric(outc$BB),
      HBP = as.numeric(hbp_vec),
      HR  = as.numeric(outc$HR),
      H   = as.numeric(h_vec),
      TB  = tb_vec,
      SF  = as.numeric(sf_vec)
    ) %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        K   = sum(K,  na.rm = TRUE),
        BB  = sum(BB, na.rm = TRUE),
        HBP = sum(HBP,na.rm = TRUE),
        HR  = sum(HR, na.rm = TRUE),
        H   = sum(H,  na.rm = TRUE),
        TB  = sum(TB, na.rm = TRUE),
        SF  = sum(SF, na.rm = TRUE),
        AB  = pmax(PA - BB - HBP - SF, 0),
        OBP = sdiv(H + BB + HBP, AB + BB + HBP + SF),
        SLG = sdiv(TB, AB),
        OPS = ifelse(is.finite(OBP) & is.finite(SLG), OBP + SLG, NA_real_),
        .groups = "drop"
      )
  }

  build_self_staff_block <- function(p) {
    if (is.null(p) || !nrow(p)) return(tibble::tibble())
    
    p <- prepare_aar_flags(p)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    
    first_present <- function(df, ...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }
    get_ev_la_local <- function(df) {
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present(df, "ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present(df, "Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }
    
    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la
    
    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    
    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la  = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,
        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        ))
      )
    
    pitch_rates <- p_flags %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),
        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),
        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),
        Strike_pct = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct   = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct   = sdiv(whiffs, swings),
        CSW_pct     = sdiv(csw_num, Pitches),
        IZWhiff_pct = sdiv(iz_whiffs, iz_swings),
        Chase_pct   = sdiv(chases, ooz)
      )
    
    pa_first <- p %>% dplyr::group_by(PA_ID) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    pa_group <- pa_first %>% dplyr::transmute(PA_ID, grp = .data$grp)
    
    fps_pa <- p %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::left_join(pa_group, by = "PA_ID")
    
    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID") %>%
      dplyr::left_join(pa_group, by = "PA_ID")
    
    pa_last <- pa_last_from(p)
    pa_grp_vec <- pa_group$grp[match(pa_last$PA_ID, pa_group$PA_ID)]
    
    pa_out <- pa_last %>%
      dplyr::mutate(grp = pa_grp_vec) %>%
      {
        outc <- pa_outcome_cols(.)
        pr <- outc$PR_txt
        kb <- as.character(.$KorBB %||% "")
        any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
        outs_play <- if ("OutsOnPlay" %in% names(.)) {
          v <- suppressWarnings(as.integer(.$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
        } else rep(0L, nrow(.))
        outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
        outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
        outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
        outs_total <- outs_play + as.integer(outc$K)
        
        tibble::tibble(
          grp  = .$grp,
          K    = outc$K,
          BB   = (any_walk),
          outs = outs_total
        )
      }
    
    pa_rates <- pa_out %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        PA   = dplyr::n(),
        Kp   = mean(K,  na.rm = TRUE),
        BBp  = mean(BB, na.rm = TRUE),
        Outs = sum(outs, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        fps_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()), .groups = "drop"),
        by = "grp"
      ) %>%
      dplyr::left_join(
        ea_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(
          EA = calc_ea_rate_from_pa(tibble::tibble(
            n_pitches = n_pitches,
            strikes_first3 = strikes_first3,
            early_bip = early_bip,
            any_hbp = any_hbp,
            any_barrel = any_barrel
          )),
          .groups = "drop"
        ),
        by = "grp"
      )
    
    pa_bat <- summarize_batting_simple(pa_last, pa_grp_vec)
    
    fip <- compute_fip_xfip_grouped(pa_last, grp = pa_grp_vec) %>%
      dplyr::select(.grp, FIP) %>% dplyr::rename(grp = .grp)
    wob <- compute_woba_grouped(pa_last, grp = pa_grp_vec) %>%
      dplyr::select(.grp, wOBA, wOBAcon) %>% dplyr::rename(grp = .grp)
    
    pitch_rates %>%
      dplyr::left_join(pa_rates, by = "grp") %>%
      dplyr::left_join(pa_bat,  by = "grp") %>%
      dplyr::left_join(fip,     by = "grp") %>%
      dplyr::left_join(wob,     by = "grp") %>%
      dplyr::mutate(
        Split = grp,
        IP    = sprintf("%d.%d", Outs %/% 3, Outs %% 3),
        `K%`      = fmt_pct_count(Kp,  K),
        `BB%`     = fmt_pct_count(BBp, BB),
        `Barrel%` = fmt_pct_count(Barrel_pct, Barrels),
        `GB%`     = fmt_pct0(GB_pct),
        `Strike%` = fmt_pct0(Strike_pct),
        `FPS%`    = fmt_pct0(FPS),
        `E&A%`    = fmt_pct0(EA),
        `Zone%`   = fmt_pct0(Zone_pct),
        `Whiff%`  = fmt_pct0(Whiff_pct),
        `Chase%`  = fmt_pct0(Chase_pct),
        `CSW%`    = fmt_pct0(CSW_pct),
        `IZWhiff%`= fmt_pct0(IZWhiff_pct),
        wOBA    = fmt_num3(wOBA),
        wOBAcon = fmt_num3(wOBAcon),
        SLG     = fmt_num3(SLG),
        OPS     = fmt_num3(OPS),
        FIP     = fmt_num2(FIP)
      ) %>%
      dplyr::select(
        Split, IP,
        `K%`, `BB%`, Hits = H, `Barrel%`, `HBP's` = HBP, `HR's` = HR,
        wOBA, wOBAcon, SLG, OPS, `GB%`, FIP,
        `Strike%`, `FPS%`, `E&A%`, `Zone%`,
        `Whiff%`, `Chase%`, `CSW%`, `IZWhiff%`
      )
  }

  build_self_staff_table <- function(d) {
    if (is.null(d) || !nrow(d)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    p <- prepare_aar_flags(d)
    p$BatterSideStd <- normalize_batter_side(p$BatterSide)
    p$grp <- dplyr::case_when(
      p$BatterSideStd == "L" ~ "vLHH",
      p$BatterSideStd == "R" ~ "vRHH",
      TRUE ~ NA_character_
    )
    p <- p %>% dplyr::filter(!is.na(.data$grp))
    if (!nrow(p)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    
    tab_lr <- build_self_staff_block(p)
    p_total <- p; p_total$grp <- "Total"
    tab_total <- build_self_staff_block(p_total)
    
    out <- dplyr::bind_rows(tab_lr, tab_total) %>%
      dplyr::mutate(Split = factor(Split, levels = c("vLHH","vRHH","Total"))) %>%
      dplyr::arrange(Split)
    
    as.data.frame(out)
  }

  build_self_usage_table <- function(d, hand = NULL) {
    if (is.null(d) || !nrow(d)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    p <- prepare_aar_flags(d)
    p$BatterSideStd <- normalize_batter_side(p$BatterSide)
    if (!is.null(hand)) {
      p <- p %>% dplyr::filter(.data$BatterSideStd == hand)
    }
    p$PitchTypeStd <- vapply(p$PitchType, self_scout_pitch_type, character(1))
    p <- p %>% dplyr::filter(!is.na(.data$PitchTypeStd))
    if (!nrow(p)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    p$PitchGroup <- vapply(p$PitchTypeStd, self_scout_usage_group, character(1))
    p <- ensure_counts(p)
    
    p <- p %>%
      dplyr::mutate(
        Count = paste0(BallsPre, "-", StrikesPre),
        PitcherAhead  = Count %in% c("0-1","0-2","1-2"),
        PitcherBehind = Count %in% c("1-0","2-0","2-1","3-0","3-1"),
        Even          = Count %in% c("1-1","2-2","3-2"),
        TwoStrike     = StrikesPre == 2
      )
    
    usage_vec <- function(df, col, levels) {
      tot <- nrow(df)
      out <- stats::setNames(rep(NA_real_, length(levels)), levels)
      if (tot == 0) return(out)
      cnt <- df %>% dplyr::count(.data[[col]])
      for (i in seq_len(nrow(cnt))) {
        nm <- as.character(cnt[[col]][i])
        if (nm %in% levels) out[nm] <- cnt$n[i] / tot
      }
      out
    }
    
    double_triple_rates <- function(df, col, levels) {
      if (!nrow(df)) {
        return(tibble::tibble(level = levels, double = NA_real_, triple = NA_real_))
      }
      seq_df <- df %>%
        dplyr::arrange(PA_ID, PitchNum) %>%
        dplyr::group_by(PA_ID) %>%
        dplyr::mutate(
          prev1 = dplyr::lag(.data[[col]]),
          next1 = dplyr::lead(.data[[col]]),
          next2 = dplyr::lead(.data[[col]], 2L)
        ) %>%
        dplyr::ungroup()
      
      out <- tibble::tibble(level = levels) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          double_opp = sum(seq_df$prev1 == level, na.rm = TRUE),
          double_hit = sum(seq_df$prev1 == level & seq_df[[col]] == level, na.rm = TRUE),
          triple_opp = sum(seq_df[[col]] == level & !is.na(seq_df$next2), na.rm = TRUE),
          triple_hit = sum(seq_df[[col]] == level & seq_df$next1 == level & seq_df$next2 == level, na.rm = TRUE),
          double = sdiv(double_hit, double_opp),
          triple = sdiv(triple_hit, triple_opp)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(level, double, triple)
      
      out
    }
    
    pt_levels <- SELF_SCOUT_PITCH_LEVELS
    grp_levels <- c("Hard","Breaking","Soft")
    
    pt_total <- usage_vec(p, "PitchTypeStd", pt_levels)
    pt_fp    <- usage_vec(p %>% dplyr::filter(FirstPitch %in% TRUE), "PitchTypeStd", pt_levels)
    pt_ahead <- usage_vec(p %>% dplyr::filter(PitcherAhead), "PitchTypeStd", pt_levels)
    pt_behind<- usage_vec(p %>% dplyr::filter(PitcherBehind), "PitchTypeStd", pt_levels)
    pt_even  <- usage_vec(p %>% dplyr::filter(Even), "PitchTypeStd", pt_levels)
    pt_2k    <- usage_vec(p %>% dplyr::filter(TwoStrike), "PitchTypeStd", pt_levels)
    pt_dt    <- double_triple_rates(p, "PitchTypeStd", pt_levels)
    
    grp_total <- usage_vec(p, "PitchGroup", grp_levels)
    grp_fp    <- usage_vec(p %>% dplyr::filter(FirstPitch %in% TRUE), "PitchGroup", grp_levels)
    grp_ahead <- usage_vec(p %>% dplyr::filter(PitcherAhead), "PitchGroup", grp_levels)
    grp_behind<- usage_vec(p %>% dplyr::filter(PitcherBehind), "PitchGroup", grp_levels)
    grp_even  <- usage_vec(p %>% dplyr::filter(Even), "PitchGroup", grp_levels)
    grp_2k    <- usage_vec(p %>% dplyr::filter(TwoStrike), "PitchGroup", grp_levels)
    grp_dt    <- double_triple_rates(p, "PitchGroup", grp_levels)
    
    make_tbl <- function(levels, total, fp, ahead, behind, even, twoK, dt_tbl) {
      tibble::tibble(
        `Pitch Type` = levels,
        `Total` = total[levels],
        `First Pitch` = fp[levels],
        `Pitcher Ahead (0-1, 0-2, 1-2)` = ahead[levels],
        `Pitcher Behind (1-0, 2-0, 2-1, 3-0, 3-1)` = behind[levels],
        `Even (1-1, 2-2, 3-2)` = even[levels],
        `2 Strike` = twoK[levels],
        `Double Up%` = dt_tbl$double[match(levels, dt_tbl$level)],
        `Triple Up%` = dt_tbl$triple[match(levels, dt_tbl$level)]
      )
    }
    
    tbl_pt  <- make_tbl(pt_levels, pt_total, pt_fp, pt_ahead, pt_behind, pt_even, pt_2k, pt_dt)
    tbl_grp <- make_tbl(grp_levels, grp_total, grp_fp, grp_ahead, grp_behind, grp_even, grp_2k, grp_dt)
    out <- dplyr::bind_rows(tbl_pt, tbl_grp) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~ ifelse(is.finite(.x), sprintf("%.0f%%", 100 * .x), "NA")
        )
      )
    
    as.data.frame(out)
  }

  build_self_lineup_map <- function(p) {
    if (is.null(p) || !nrow(p)) return(list(p = p, pa_last = p, map = NULL))
    p <- prepare_aar_flags(p)
    p <- ensure_counts(p)
    p$row_index <- seq_len(nrow(p))

    first_present <- function(df, ...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }

    game_col <- first_present(p, "CustomGameID", "CustomGameID_BP", "GameID", "Game")
    batter_col <- first_present(p, "Batter", "BatterName", "BatterID", "BatterId", "Hitter", "BatterID")
    if (is.na(game_col) || is.na(batter_col)) {
      p$LineupSpot <- NA_character_
      return(list(p = p, pa_last = pa_last_from(p), map = NULL))
    }

    p$GameID <- as.character(p[[game_col]])
    p$BatterID <- as.character(p[[batter_col]])

    pa_first <- p %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        Inn = if ("Inning" %in% names(.)) suppressWarnings(as.integer(readr::parse_number(as.character(Inning)))) else NA_integer_,
        PA_inn = if ("PAofInning" %in% names(.)) suppressWarnings(as.integer(readr::parse_number(as.character(PAofInning)))) else NA_integer_
      ) %>%
      dplyr::group_by(GameID) %>%
      dplyr::arrange(dplyr::coalesce(Inn, 99L), dplyr::coalesce(PA_inn, 99L), row_index, .by_group = TRUE) %>%
      dplyr::mutate(PA_seq = dplyr::row_number()) %>%
      dplyr::ungroup()

    lineup_map <- pa_first %>%
      dplyr::filter(PA_seq <= 9, !is.na(BatterID) & nzchar(BatterID)) %>%
      dplyr::group_by(GameID, BatterID) %>%
      dplyr::summarise(lineup_spot = min(PA_seq, na.rm = TRUE), .groups = "drop")

    p <- p %>%
      dplyr::left_join(lineup_map, by = c("GameID", "BatterID")) %>%
      dplyr::mutate(LineupSpot = ifelse(is.na(lineup_spot), "PH", as.character(lineup_spot)))

    list(p = p, pa_last = pa_last_from(p), map = lineup_map)
  }

  build_self_lineup_usage_table <- function(d) {
    if (is.null(d) || !nrow(d)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    lm <- build_self_lineup_map(d)
    p <- lm$p
    if (is.null(p) || !nrow(p) || !"LineupSpot" %in% names(p)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }

    p <- p %>% dplyr::mutate(TwoStrike = StrikesPre == 2)
    p$PitchTypeStd <- vapply(p$PitchType, self_scout_pitch_type, character(1))
    p$PitchGroup <- vapply(p$PitchTypeStd, self_scout_usage_group, character(1))
    p <- p %>% dplyr::filter(!is.na(PitchGroup), !is.na(LineupSpot))

    lineup_levels <- c(as.character(1:9), "PH")

    overall <- p %>%
      dplyr::group_by(LineupSpot) %>%
      dplyr::summarise(
        Hard = sdiv(sum(PitchGroup == "Hard", na.rm = TRUE), dplyr::n()),
        Breaking = sdiv(sum(PitchGroup == "Breaking", na.rm = TRUE), dplyr::n()),
        Soft = sdiv(sum(PitchGroup == "Soft", na.rm = TRUE), dplyr::n()),
        .groups = "drop"
      )

    two_k <- p %>%
      dplyr::filter(TwoStrike %in% TRUE) %>%
      dplyr::group_by(LineupSpot) %>%
      dplyr::summarise(
        Hard_2k = sdiv(sum(PitchGroup == "Hard", na.rm = TRUE), dplyr::n()),
        Breaking_2k = sdiv(sum(PitchGroup == "Breaking", na.rm = TRUE), dplyr::n()),
        Soft_2k = sdiv(sum(PitchGroup == "Soft", na.rm = TRUE), dplyr::n()),
        .groups = "drop"
      )

    seq_df <- p %>%
      dplyr::arrange(GameID, PA_ID, PitchNum) %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::mutate(prev_grp = dplyr::lag(PitchGroup)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(prev_grp))

    double_tbl <- seq_df %>%
      dplyr::group_by(LineupSpot) %>%
      dplyr::summarise(
        Hard_du = sdiv(sum(prev_grp == "Hard" & PitchGroup == "Hard", na.rm = TRUE),
                       sum(prev_grp == "Hard", na.rm = TRUE)),
        Breaking_du = sdiv(sum(prev_grp == "Breaking" & PitchGroup == "Breaking", na.rm = TRUE),
                           sum(prev_grp == "Breaking", na.rm = TRUE)),
        Soft_du = sdiv(sum(prev_grp == "Soft" & PitchGroup == "Soft", na.rm = TRUE),
                       sum(prev_grp == "Soft", na.rm = TRUE)),
        .groups = "drop"
      )

    tbl <- overall %>%
      dplyr::left_join(two_k, by = "LineupSpot") %>%
      dplyr::left_join(double_tbl, by = "LineupSpot") %>%
      dplyr::rename(`Lineup Spot` = LineupSpot) %>%
      tidyr::complete(`Lineup Spot` = lineup_levels) %>%
      dplyr::mutate(
        `Hard%` = Hard,
        `Break%` = Breaking,
        `Soft%` = Soft,
        `2k Hard%` = Hard_2k,
        `2k Break%` = Breaking_2k,
        `2k Soft%` = Soft_2k,
        `Hard Double Up%` = Hard_du,
        `Break Double Up%` = Breaking_du,
        `Soft Double Up%` = Soft_du
      ) %>%
      dplyr::select(
        `Lineup Spot`,
        `Hard%`, `Break%`, `Soft%`,
        `2k Hard%`, `2k Break%`, `2k Soft%`,
        `Hard Double Up%`, `Break Double Up%`, `Soft Double Up%`
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.finite(.x), sprintf("%.0f%%", 100 * .x), "NA")))

    total_row <- tibble::tibble(
      `Lineup Spot` = "Total",
      `Hard%` = sdiv(sum(p$PitchGroup == "Hard", na.rm = TRUE), nrow(p)),
      `Break%` = sdiv(sum(p$PitchGroup == "Breaking", na.rm = TRUE), nrow(p)),
      `Soft%` = sdiv(sum(p$PitchGroup == "Soft", na.rm = TRUE), nrow(p)),
      `2k Hard%` = sdiv(sum(p$PitchGroup == "Hard" & p$TwoStrike %in% TRUE, na.rm = TRUE), sum(p$TwoStrike %in% TRUE, na.rm = TRUE)),
      `2k Break%` = sdiv(sum(p$PitchGroup == "Breaking" & p$TwoStrike %in% TRUE, na.rm = TRUE), sum(p$TwoStrike %in% TRUE, na.rm = TRUE)),
      `2k Soft%` = sdiv(sum(p$PitchGroup == "Soft" & p$TwoStrike %in% TRUE, na.rm = TRUE), sum(p$TwoStrike %in% TRUE, na.rm = TRUE)),
      `Hard Double Up%` = sdiv(sum(seq_df$prev_grp == "Hard" & seq_df$PitchGroup == "Hard", na.rm = TRUE),
                               sum(seq_df$prev_grp == "Hard", na.rm = TRUE)),
      `Break Double Up%` = sdiv(sum(seq_df$prev_grp == "Breaking" & seq_df$PitchGroup == "Breaking", na.rm = TRUE),
                                sum(seq_df$prev_grp == "Breaking", na.rm = TRUE)),
      `Soft Double Up%` = sdiv(sum(seq_df$prev_grp == "Soft" & seq_df$PitchGroup == "Soft", na.rm = TRUE),
                               sum(seq_df$prev_grp == "Soft", na.rm = TRUE))
    ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.finite(.x), sprintf("%.0f%%", 100 * .x), "NA")))

    out <- dplyr::bind_rows(tbl, total_row) %>%
      dplyr::mutate(`Lineup Spot` = factor(`Lineup Spot`, levels = c(lineup_levels, "Total"))) %>%
      dplyr::arrange(`Lineup Spot`)

    as.data.frame(out)
  }

  build_self_lineup_results_table <- function(d) {
    if (is.null(d) || !nrow(d)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }
    lm <- build_self_lineup_map(d)
    p <- lm$p
    pa_last <- lm$pa_last
    if (is.null(p) || !nrow(p) || !"LineupSpot" %in% names(p)) {
      return(as.data.frame(tibble::tibble(Status = "No data")))
    }

    first_present <- function(df, ...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }
    evla <- resolve_ev_la_strict(p)
    evn <- suppressWarnings(as.numeric(evla$ev))
    lan <- suppressWarnings(as.numeric(evla$la))
    if (all(!is.finite(evn))) {
      ev_col <- first_present(p, "ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
      if (!is.na(ev_col)) evn <- suppressWarnings(readr::parse_number(as.character(p[[ev_col]])))
    }
    if (all(!is.finite(lan))) {
      la_col <- first_present(p, "Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
      if (!is.na(la_col)) lan <- suppressWarnings(readr::parse_number(as.character(p[[la_col]])))
    }

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc)
      )

    pitch_rates <- p_flags %>%
      dplyr::group_by(LineupSpot) %>%
      dplyr::summarise(
        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        Whiff_pct = sdiv(whiffs, swings),
        Barrels   = sum(.barrel & .barrel_ok, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        .groups = "drop"
      )

    outc <- pa_outcome_cols(pa_last)
    pa_tmp <- pa_last %>%
      dplyr::mutate(
        K = outc$K,
        BB = outc$BB
      )

    pa_rates <- pa_tmp %>%
      dplyr::group_by(LineupSpot) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        Kp  = mean(K,  na.rm = TRUE),
        BBp = mean(BB, na.rm = TRUE),
        .groups = "drop"
      )

    pa_bat <- summarize_batting_simple(pa_last, pa_last$LineupSpot) %>%
      dplyr::select(grp, SLG) %>%
      dplyr::rename(LineupSpot = grp)

    wob <- compute_woba_grouped(pa_last, grp = pa_last$LineupSpot) %>%
      dplyr::select(.grp, wOBA) %>% dplyr::rename(LineupSpot = .grp)

    fip <- compute_fip_xfip_grouped(pa_last, grp = pa_last$LineupSpot) %>%
      dplyr::select(.grp, FIP) %>% dplyr::rename(LineupSpot = .grp)

    out <- pitch_rates %>%
      dplyr::left_join(pa_rates, by = "LineupSpot") %>%
      dplyr::left_join(pa_bat,  by = "LineupSpot") %>%
      dplyr::left_join(wob,     by = "LineupSpot") %>%
      dplyr::left_join(fip,     by = "LineupSpot") %>%
      dplyr::mutate(
        `K%` = fmt_pct0(Kp),
        `BB%` = fmt_pct0(BBp),
        `Barrel%` = fmt_pct0(Barrel_pct),
        `Whiff%` = fmt_pct0(Whiff_pct),
        `SLG` = fmt_num3(SLG),
        `wOBA` = fmt_num3(wOBA),
        `FIP` = fmt_num2(FIP)
      ) %>%
      dplyr::select(LineupSpot, `K%`, `BB%`, `Barrel%`, SLG, `Whiff%`, wOBA, FIP)

    # Total row
    pa_total <- pa_outcome_cols(pa_last)
    total_k <- mean(pa_total$K, na.rm = TRUE)
    total_bb <- mean(pa_total$BB, na.rm = TRUE)
    total_whiff <- sdiv(sum(p_flags$.pc == "StrikeSwinging", na.rm = TRUE), sum(p_flags$.swing, na.rm = TRUE))
    total_barrel <- sdiv(sum(p_flags$.barrel & p_flags$.barrel_ok, na.rm = TRUE), sum(p_flags$.bip_evla & p_flags$.barrel_ok, na.rm = TRUE))
    total_slg <- summarize_batting_simple(pa_last, rep("Total", nrow(pa_last)))$SLG[1]
    total_woba <- compute_woba_grouped(pa_last, grp = rep("Total", nrow(pa_last)))$wOBA[1]
    total_fip <- compute_fip_xfip_grouped(pa_last, grp = rep("Total", nrow(pa_last)))$FIP[1]

    total_row <- tibble::tibble(
      LineupSpot = "Total",
      `K%` = fmt_pct0(total_k),
      `BB%` = fmt_pct0(total_bb),
      `Barrel%` = fmt_pct0(total_barrel),
      SLG = fmt_num3(total_slg),
      `Whiff%` = fmt_pct0(total_whiff),
      wOBA = fmt_num3(total_woba),
      FIP = fmt_num2(total_fip)
    )

    lineup_levels <- c(as.character(1:9), "PH", "Total")
    out <- dplyr::bind_rows(out, total_row) %>%
      tidyr::complete(
        LineupSpot = lineup_levels,
        fill = list(
          `K%` = "NA",
          `BB%` = "NA",
          `Barrel%` = "NA",
          SLG = "NA",
          `Whiff%` = "NA",
          wOBA = "NA",
          FIP = "NA"
        )
      ) %>%
      dplyr::mutate(LineupSpot = factor(LineupSpot, levels = lineup_levels)) %>%
      dplyr::arrange(LineupSpot)

    as.data.frame(out)
  }

  self_loc_plot <- function(d, title) {
    if (is.null(d) || !nrow(d)) {
      return(ggplot() + theme_void() + labs(title = paste0(title, " (no data)")))
    }
    ggplot(d, aes(PlateLocSide, PlateLocHeight)) +
      stat_density_2d(
        aes(fill = after_stat(ndensity)),
        geom = "raster",
        contour = FALSE,
        n = 150,
        na.rm = TRUE
      ) +
      scale_fill_gradientn(colors = c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff"), guide = "none") +
      labs(title = title, x = NULL, y = NULL) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(6, 6, 6, 6)
      ) +
      geom_rect(
        data = strike_zone,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.8
      ) +
      geom_segment(
        data = home_plate_segments,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, colour = "black", linewidth = 0.6
      ) +
      coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE)
  }

  build_self_inning_series_df <- function(d) {
    if (is.null(d) || !nrow(d)) return(NULL)
    p <- prepare_aar_flags(d)
    if (!"Inning" %in% names(p)) return(NULL)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    p$PitchTypeStd <- vapply(p$PitchType, self_scout_pitch_type, character(1))
    p$PitchGroup <- vapply(p$PitchTypeStd, self_scout_usage_group, character(1))
    inn <- suppressWarnings(as.integer(readr::parse_number(as.character(p$Inning))))
    p$InningStd <- ifelse(is.finite(inn) & inn >= 10, "10+", as.character(inn))

    p_use <- p %>%
      dplyr::filter(
        !is.na(PitchGroup),
        InningStd %in% c(as.character(1:9), "10+")
      )
    if (!nrow(p_use)) return(NULL)

    counts <- p_use %>%
      dplyr::group_by(InningStd, PitchGroup) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    totals <- counts %>%
      dplyr::group_by(InningStd) %>%
      dplyr::summarise(total = sum(n), .groups = "drop")

    usage_df <- counts %>%
      dplyr::left_join(totals, by = "InningStd") %>%
      dplyr::mutate(value = sdiv(n, total), Metric = PitchGroup) %>%
      dplyr::select(InningStd, Metric, value)

    # Barrel% by inning
    evla <- resolve_ev_la_strict(p_use)
    evn <- suppressWarnings(as.numeric(evla$ev))
    lan <- suppressWarnings(as.numeric(evla$la))
    if (all(!is.finite(evn))) {
      ev_col <- c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
      ev_col <- ev_col[ev_col %in% names(p_use)][1]
      if (!is.na(ev_col)) evn <- suppressWarnings(readr::parse_number(as.character(p_use[[ev_col]])))
    }
    if (all(!is.finite(lan))) {
      la_col <- c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
      la_col <- la_col[la_col %in% names(p_use)][1]
      if (!is.na(la_col)) lan <- suppressWarnings(readr::parse_number(as.character(p_use[[la_col]])))
    }
    pc_vec  <- as.character(p_use$PitchCall %||% "")
    bip_vec <- safe_is_bip(pc_vec, p_use$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX

    p_flags <- p_use %>%
      dplyr::mutate(
        .bip_evla = bip_vec & is.finite(evn) & is.finite(lan),
        .barrel_ok = barrel_date_ok(p_use),
        .barrel = barrel_vec
      )

    barrel_df <- p_flags %>%
      dplyr::group_by(InningStd) %>%
      dplyr::summarise(
        bar_num = sum(.barrel & .barrel_ok, na.rm = TRUE),
        bar_den = sum(.bip_evla & .barrel_ok, na.rm = TRUE),
        value = sdiv(bar_num, bar_den),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Metric = "Barrel%") %>%
      dplyr::select(InningStd, Metric, value)

    # K% by inning (PA-level)
    pa_last <- pa_last_from(p_use)
    outc <- pa_outcome_cols(pa_last)
    pa_last$K <- outc$K
    inn_pa <- suppressWarnings(as.integer(readr::parse_number(as.character(pa_last$Inning))))
    pa_last$InningStd <- ifelse(is.finite(inn_pa) & inn_pa >= 10, "10+", as.character(inn_pa))
    k_df <- pa_last %>%
      dplyr::filter(InningStd %in% c(as.character(1:9), "10+")) %>%
      dplyr::group_by(InningStd) %>%
      dplyr::summarise(value = mean(K, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Metric = "K%") %>%
      dplyr::select(InningStd, Metric, value)

    df <- dplyr::bind_rows(usage_df, barrel_df, k_df)
    df$InningStd <- factor(df$InningStd, levels = c(as.character(1:9), "10+"))
    df$Metric <- factor(df$Metric, levels = c("Hard","Breaking","Soft","Barrel%","K%"))
    df
  }

  # ======== Performance Time Series ========
  roll_mean <- function(x, n) {
    x_num <- suppressWarnings(as.numeric(x))
    out <- rep(NA_real_, length(x_num))
    if (!length(x_num) || !is.finite(n) || n <= 1) return(x_num)
    for (i in seq_along(x_num)) {
      if (i < n) next
      window <- x_num[(i - n + 1):i]
      if (all(!is.finite(window))) {
        out[i] <- NA_real_
      } else {
        out[i] <- mean(window, na.rm = TRUE)
      }
    }
    out
  }

  perf_ts_data <- reactive({
    d <- dataFilter()
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    p <- prepare_aar_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    # GameID (fallbacks)
    if ("CustomGameID" %in% names(p)) {
      p$GameID <- as.character(p$CustomGameID)
    } else if ("GameID" %in% names(p)) {
      p$GameID <- as.character(p$GameID)
    } else if ("Game" %in% names(p)) {
      p$GameID <- as.character(p$Game)
    } else {
      p$GameID <- "Game"
    }
    p$GameID <- ifelse(is.na(p$GameID) | !nzchar(p$GameID), "Game", p$GameID)

    # GameDate (fallbacks)
    gd <- if ("GameDate" %in% names(p)) {
      parse_date_any(p$GameDate)
    } else if ("Date" %in% names(p)) {
      parse_date_any(p$Date)
    } else if ("PitchDate" %in% names(p)) {
      parse_date_any(p$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(p)) {
      gd <- parse_gameid_date(p$CustomGameID)
    }
    p$GameDate <- gd

    # --- helper: EV/LA detection (matches leaderboard/performance fallback logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,

        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),

        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )

    safe_min_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) min(x) else as.Date(NA)
    }

    game_info <- p_flags %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        GameDate = safe_min_date(GameDate),
        .order   = min(.row, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::if_else(is.na(GameDate), 1L, 0L), GameDate, .order) %>%
      dplyr::mutate(
        GameIndex = dplyr::row_number(),
        GameLabel = ifelse(!is.na(GameDate), format(GameDate, "%m/%d/%y"), as.character(GameID))
      )

    pitch_rates <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),

        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),

        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),

        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),

        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },

        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),

        oneone_den = sum(.at_11, na.rm = TRUE),
        oneone_win = sum(.at_11 & .win_11, na.rm = TRUE),
        putaway_den = sum(.twok, na.rm = TRUE),
        putaway_num = sum(.twok & .is_k_pitch, na.rm = TRUE),

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz),
        Win11_pct    = sdiv(oneone_win, oneone_den),
        PutAway_pct  = sdiv(putaway_num, putaway_den)
      )

    fps_pa <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop")

    fps_rates <- fps_pa %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                       .groups = "drop")

    ea_pa <- calc_ea_pa_summary(p_flags, c("GameID", "PA_ID"))

    ea_rates <- ea_pa %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        EA = calc_ea_rate_from_pa(tibble::tibble(
          n_pitches = n_pitches,
          strikes_first3 = strikes_first3,
          early_bip = early_bip,
          any_hbp = any_hbp,
          any_barrel = any_barrel
        )),
        .groups = "drop"
      )

    pa_last <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()

    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)

    pa_last$K_flag  <- outc$K
    pa_last$BB_flag <- any_walk

    pa_rates <- pa_last %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        Kp  = mean(K_flag,  na.rm = TRUE),
        BBp = mean(BB_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(fps_rates, by = "GameID") %>%
      dplyr::left_join(ea_rates,  by = "GameID")

    woba_rates <- compute_woba_grouped(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = .grp)
    fip_rates <- compute_fip_xfip_grouped(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = .grp) %>%
      dplyr::select(GameID, FIP)
    batting_rates <- summarize_batting_simple(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = grp) %>%
      dplyr::select(GameID, SLG, OPS)

    max_velo <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(MaxVelocity = suppressWarnings(max(as.numeric(RelSpeed), na.rm = TRUE)),
                       .groups = "drop")
    max_velo$MaxVelocity[!is.finite(max_velo$MaxVelocity)] <- NA_real_

    pitch_metrics_all <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        IVB = {
          v <- suppressWarnings(as.numeric(InducedVertBreak))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        HB = {
          v <- suppressWarnings(as.numeric(HorzBreak))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        Extension = {
          v <- suppressWarnings(as.numeric(Extension))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        VAA = {
          v <- suppressWarnings(as.numeric(VertApprAngle))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        Usage_pct = 1,
        .groups = "drop"
      )

    pitch_grp <- function(pt) {
      x <- tolower(trimws(as.character(pt)))
      fb <- x %in% c("fastball","four-seam","four seam","4-seam","4 seam","four-seam fastball","4-seam fastball")
      si <- x %in% c("sinker","two-seam","two seam","2-seam","2 seam","two-seam fastball","2-seam fastball")
      ifelse(fb, "FB", ifelse(si, "SI", NA_character_))
    }

    fbsi <- p_flags %>%
      dplyr::mutate(
        .fs_grp = pitch_grp(PitchType),
        .velo = suppressWarnings(as.numeric(RelSpeed))
      ) %>%
      dplyr::filter(!is.na(.fs_grp), is.finite(.velo)) %>%
      dplyr::group_by(GameID, .fs_grp) %>%
      dplyr::summarise(
        n = dplyr::n(),
        AvgVelo = mean(.velo, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(GameID, dplyr::desc(n), dplyr::desc(AvgVelo)) %>%
      dplyr::group_by(GameID) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(GameID, AvgVelo)

    game_info %>%
      dplyr::left_join(pitch_rates, by = "GameID") %>%
      dplyr::left_join(pa_rates,   by = "GameID") %>%
      dplyr::left_join(woba_rates, by = "GameID") %>%
      dplyr::left_join(batting_rates, by = "GameID") %>%
      dplyr::left_join(fip_rates, by = "GameID") %>%
      dplyr::left_join(fbsi,       by = "GameID") %>%
      dplyr::left_join(pitch_metrics_all, by = "GameID") %>%
      dplyr::left_join(max_velo,   by = "GameID")
  })

  perf_ts_data_ptype <- reactive({
    d <- dataFilter()
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    p <- prepare_aar_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    # GameID (fallbacks)
    if ("CustomGameID" %in% names(p)) {
      p$GameID <- as.character(p$CustomGameID)
    } else if ("GameID" %in% names(p)) {
      p$GameID <- as.character(p$GameID)
    } else if ("Game" %in% names(p)) {
      p$GameID <- as.character(p$Game)
    } else {
      p$GameID <- "Game"
    }
    p$GameID <- ifelse(is.na(p$GameID) | !nzchar(p$GameID), "Game", p$GameID)

    # GameDate (fallbacks)
    gd <- if ("GameDate" %in% names(p)) {
      parse_date_any(p$GameDate)
    } else if ("Date" %in% names(p)) {
      parse_date_any(p$Date)
    } else if ("PitchDate" %in% names(p)) {
      parse_date_any(p$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(p)) {
      gd <- parse_gameid_date(p$CustomGameID)
    }
    p$GameDate <- gd

    # --- helper: EV/LA detection (matches leaderboard/performance fallback logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,

        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),

        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )

    safe_min_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) min(x) else as.Date(NA)
    }

    game_info <- p_flags %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        GameDate = safe_min_date(GameDate),
        .order   = min(.row, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::if_else(is.na(GameDate), 1L, 0L), GameDate, .order) %>%
      dplyr::mutate(
        GameIndex = dplyr::row_number(),
        GameLabel = ifelse(!is.na(GameDate), format(GameDate, "%m/%d/%y"), as.character(GameID))
      )

    p_flags <- p_flags %>%
      dplyr::mutate(PitchTypeStd = as.character(PitchType))
    p_flags$PitchTypeStd[is.na(p_flags$PitchTypeStd) | !nzchar(p_flags$PitchTypeStd)] <- "Undefined"
    p_flags <- p_flags %>% dplyr::filter(.data$PitchTypeStd != "Bad data")

    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    max_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_
    }

    pitch_rates_pt <- p_flags %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),

        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),

        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),

        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),

        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },

        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),

        oneone_den = sum(.at_11, na.rm = TRUE),
        oneone_win = sum(.at_11 & .win_11, na.rm = TRUE),
        putaway_den = sum(.twok, na.rm = TRUE),
        putaway_num = sum(.twok & .is_k_pitch, na.rm = TRUE),

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz),
        Win11_pct    = sdiv(oneone_win, oneone_den),
        PutAway_pct  = sdiv(putaway_num, putaway_den)
      )

    totals <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(TotalPitches = dplyr::n(), .groups = "drop")

    pitch_rates_pt <- pitch_rates_pt %>%
      dplyr::left_join(totals, by = "GameID") %>%
      dplyr::mutate(Usage_pct = sdiv(Pitches, TotalPitches))

    pitch_metrics_pt <- p_flags %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        IVB        = mean_or_na(InducedVertBreak),
        HB         = mean_or_na(HorzBreak),
        AvgVelo    = mean_or_na(RelSpeed),
        Extension  = mean_or_na(Extension),
        VAA        = mean_or_na(VertApprAngle),
        MaxVelocity = max_or_na(RelSpeed),
        .groups = "drop"
      )

    fps_pa <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop")

    pa_last <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    pa_last$PitchTypeStd <- as.character(pa_last$PitchType)
    pa_last$PitchTypeStd[is.na(pa_last$PitchTypeStd) | !nzchar(pa_last$PitchTypeStd)] <- "Undefined"

    pa_group <- pa_last %>% dplyr::select(GameID, PA_ID, PitchTypeStd)

    fps_rates_pt <- fps_pa %>%
      dplyr::left_join(pa_group, by = c("GameID","PA_ID")) %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                       .groups = "drop")

    ea_pa <- calc_ea_pa_summary(p_flags, c("GameID", "PA_ID")) %>%
      dplyr::left_join(pa_group, by = c("GameID","PA_ID"))

    ea_rates_pt <- ea_pa %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        EA = calc_ea_rate_from_pa(tibble::tibble(
          n_pitches = n_pitches,
          strikes_first3 = strikes_first3,
          early_bip = early_bip,
          any_hbp = any_hbp,
          any_barrel = any_barrel
        )),
        .groups = "drop"
      )

    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)

    pa_last$K_flag  <- outc$K
    pa_last$BB_flag <- any_walk

    pa_rates_pt <- pa_last %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        Kp  = mean(K_flag,  na.rm = TRUE),
        BBp = mean(BB_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(fps_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(ea_rates_pt,  by = c("GameID","PitchTypeStd"))

    pa_last <- pa_last %>%
      dplyr::mutate(.ts_grp = paste(GameID, PitchTypeStd, sep = "||"))
    pa_ts_groups <- pa_last %>%
      dplyr::distinct(.ts_grp, GameID, PitchTypeStd)

    woba_rates_pt <- compute_woba_grouped(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = .grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, wOBA, wOBAcon)
    fip_rates_pt <- compute_fip_xfip_grouped(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = .grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, FIP)
    batting_rates_pt <- summarize_batting_simple(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, SLG, OPS)

    pitch_rates_pt %>%
      dplyr::left_join(pa_rates_pt,   by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(woba_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(batting_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(fip_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(pitch_metrics_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(game_info,     by = "GameID")
  })

  team_trends_data <- reactive({
    d <- rv$df
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))

    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }

    d <- prefer_team_or_portal(d, "PitcherTeam")

    season_col <- {
      nms <- names(d)
      cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
      idx <- which(tolower(nms) %in% tolower(cands))
      if (length(idx)) nms[idx[1]] else NULL
    }
    sel_seasons <- input$team_trends_seasons %||% character(0)
    if (!is.null(season_col) && length(sel_seasons)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sel_seasons)
    }

    date_vec <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("PitchDate" %in% names(d)) {
      parse_date_any(d$PitchDate)
    } else if ("CustomGameID" %in% names(d)) {
      parse_gameid_date(d$CustomGameID)
    } else {
      as.Date(NA)
    }
    if (!is.null(input$team_trends_dates) && length(input$team_trends_dates) == 2) {
      d_start <- as.Date(input$team_trends_dates[[1]])
      d_end   <- as.Date(input$team_trends_dates[[2]])
      if (is.finite(d_start) || is.finite(d_end)) {
        keep <- rep(TRUE, nrow(d))
        if (is.finite(d_start)) keep <- keep & (date_vec >= d_start)
        if (is.finite(d_end))   keep <- keep & (date_vec <= d_end)
        d <- d[keep, , drop = FALSE]
      }
    }

    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)

    d
  })

  team_trends_data_season <- reactive({
    d <- rv$df
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))

    if ("PitchType" %in% names(d)) {
      d <- d %>% dplyr::filter(!(as.character(PitchType) %in% "Bad data"))
    }

    d <- prefer_team_or_portal(d, "PitcherTeam")

    season_col <- {
      nms <- names(d)
      cands <- c("SeasonTag","SeasonGroup","Season_Group","Season","Season_Code","SeasonCode")
      idx <- which(tolower(nms) %in% tolower(cands))
      if (length(idx)) nms[idx[1]] else NULL
    }
    sel_seasons <- input$team_trends_seasons %||% character(0)
    if (!is.null(season_col) && length(sel_seasons)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sel_seasons)
    }

    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)

    d
  })

  team_ts_data <- reactive({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    p <- prepare_aar_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    # GameID (fallbacks)
    if ("CustomGameID" %in% names(p)) {
      p$GameID <- as.character(p$CustomGameID)
    } else if ("GameID" %in% names(p)) {
      p$GameID <- as.character(p$GameID)
    } else if ("Game" %in% names(p)) {
      p$GameID <- as.character(p$Game)
    } else {
      p$GameID <- "Game"
    }
    p$GameID <- ifelse(is.na(p$GameID) | !nzchar(p$GameID), "Game", p$GameID)

    # GameDate (fallbacks)
    gd <- if ("GameDate" %in% names(p)) {
      parse_date_any(p$GameDate)
    } else if ("Date" %in% names(p)) {
      parse_date_any(p$Date)
    } else if ("PitchDate" %in% names(p)) {
      parse_date_any(p$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(p)) {
      gd <- parse_gameid_date(p$CustomGameID)
    }
    p$GameDate <- gd

    # --- helper: EV/LA detection (matches leaderboard/performance fallback logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,

        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),

        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )

    safe_min_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) min(x) else as.Date(NA)
    }

    game_info <- p_flags %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        GameDate = safe_min_date(GameDate),
        .order   = min(.row, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::if_else(is.na(GameDate), 1L, 0L), GameDate, .order) %>%
      dplyr::mutate(
        GameIndex = dplyr::row_number(),
        GameLabel = ifelse(!is.na(GameDate), format(GameDate, "%m/%d/%y"), as.character(GameID))
      )

    pitch_rates <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),

        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),

        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),

        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),

        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },

        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),

        oneone_den = sum(.at_11, na.rm = TRUE),
        oneone_win = sum(.at_11 & .win_11, na.rm = TRUE),
        putaway_den = sum(.twok, na.rm = TRUE),
        putaway_num = sum(.twok & .is_k_pitch, na.rm = TRUE),

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz),
        Win11_pct    = sdiv(oneone_win, oneone_den),
        PutAway_pct  = sdiv(putaway_num, putaway_den)
      )

    fps_pa <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop")

    fps_rates <- fps_pa %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                       .groups = "drop")

    ea_pa <- calc_ea_pa_summary(p_flags, c("GameID", "PA_ID"))

    ea_rates <- ea_pa %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        EA = calc_ea_rate_from_pa(tibble::tibble(
          n_pitches = n_pitches,
          strikes_first3 = strikes_first3,
          early_bip = early_bip,
          any_hbp = any_hbp,
          any_barrel = any_barrel
        )),
        .groups = "drop"
      )

    pa_last <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()

    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)

    pa_last$K_flag  <- outc$K
    pa_last$BB_flag <- any_walk

    pa_rates <- pa_last %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        Kp  = mean(K_flag,  na.rm = TRUE),
        BBp = mean(BB_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(fps_rates, by = "GameID") %>%
      dplyr::left_join(ea_rates,  by = "GameID")

    woba_rates <- compute_woba_grouped(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = .grp)
    fip_rates <- compute_fip_xfip_grouped(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = .grp) %>%
      dplyr::select(GameID, FIP)
    batting_rates <- summarize_batting_simple(pa_last, pa_last$GameID) %>%
      dplyr::rename(GameID = grp) %>%
      dplyr::select(GameID, SLG, OPS)

    max_velo <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(MaxVelocity = suppressWarnings(max(as.numeric(RelSpeed), na.rm = TRUE)),
                       .groups = "drop")
    max_velo$MaxVelocity[!is.finite(max_velo$MaxVelocity)] <- NA_real_

    pitch_metrics_all <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        IVB = {
          v <- suppressWarnings(as.numeric(InducedVertBreak))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        HB = {
          v <- suppressWarnings(as.numeric(HorzBreak))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        Extension = {
          v <- suppressWarnings(as.numeric(Extension))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        VAA = {
          v <- suppressWarnings(as.numeric(VertApprAngle))
          if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_
        },
        Usage_pct = 1,
        .groups = "drop"
      )

    pitch_grp <- function(pt) {
      x <- tolower(trimws(as.character(pt)))
      fb <- x %in% c("fastball","four-seam","four seam","4-seam","4 seam","four-seam fastball","4-seam fastball")
      si <- x %in% c("sinker","two-seam","two seam","2-seam","2 seam","two-seam fastball","2-seam fastball")
      ifelse(fb, "FB", ifelse(si, "SI", NA_character_))
    }

    fbsi <- p_flags %>%
      dplyr::mutate(
        .fs_grp = pitch_grp(PitchType),
        .velo = suppressWarnings(as.numeric(RelSpeed))
      ) %>%
      dplyr::filter(!is.na(.fs_grp), is.finite(.velo)) %>%
      dplyr::group_by(GameID, .fs_grp) %>%
      dplyr::summarise(
        n = dplyr::n(),
        AvgVelo = mean(.velo, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(GameID, dplyr::desc(n), dplyr::desc(AvgVelo)) %>%
      dplyr::group_by(GameID) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(GameID, AvgVelo)

    game_info %>%
      dplyr::left_join(pitch_rates, by = "GameID") %>%
      dplyr::left_join(pa_rates,   by = "GameID") %>%
      dplyr::left_join(woba_rates, by = "GameID") %>%
      dplyr::left_join(batting_rates, by = "GameID") %>%
      dplyr::left_join(fip_rates, by = "GameID") %>%
      dplyr::left_join(fbsi,       by = "GameID") %>%
      dplyr::left_join(pitch_metrics_all, by = "GameID") %>%
      dplyr::left_join(max_velo,   by = "GameID")
  })

  team_ts_data_ptype <- reactive({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    p <- prepare_aar_flags(d)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)

    # GameID (fallbacks)
    if ("CustomGameID" %in% names(p)) {
      p$GameID <- as.character(p$CustomGameID)
    } else if ("GameID" %in% names(p)) {
      p$GameID <- as.character(p$GameID)
    } else if ("Game" %in% names(p)) {
      p$GameID <- as.character(p$Game)
    } else {
      p$GameID <- "Game"
    }
    p$GameID <- ifelse(is.na(p$GameID) | !nzchar(p$GameID), "Game", p$GameID)

    # GameDate (fallbacks)
    gd <- if ("GameDate" %in% names(p)) {
      parse_date_any(p$GameDate)
    } else if ("Date" %in% names(p)) {
      parse_date_any(p$Date)
    } else if ("PitchDate" %in% names(p)) {
      parse_date_any(p$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(p)) {
      gd <- parse_gameid_date(p$CustomGameID)
    }
    p$GameDate <- gd

    # --- helper: EV/LA detection (matches leaderboard/performance fallback logic) ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,

        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),

        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )

    safe_min_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) min(x) else as.Date(NA)
    }

    game_info <- p_flags %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        GameDate = safe_min_date(GameDate),
        .order   = min(.row, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::if_else(is.na(GameDate), 1L, 0L), GameDate, .order) %>%
      dplyr::mutate(
        GameIndex = dplyr::row_number(),
        GameLabel = ifelse(!is.na(GameDate), format(GameDate, "%m/%d/%y"), as.character(GameID))
      )

    p_flags <- p_flags %>%
      dplyr::mutate(PitchTypeStd = as.character(PitchType))
    p_flags$PitchTypeStd[is.na(p_flags$PitchTypeStd) | !nzchar(p_flags$PitchTypeStd)] <- "Undefined"
    p_flags <- p_flags %>% dplyr::filter(.data$PitchTypeStd != "Bad data")

    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    max_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_
    }

    pitch_rates_pt <- p_flags %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),

        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),

        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),

        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),

        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },

        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),

        oneone_den = sum(.at_11, na.rm = TRUE),
        oneone_win = sum(.at_11 & .win_11, na.rm = TRUE),
        putaway_den = sum(.twok, na.rm = TRUE),
        putaway_num = sum(.twok & .is_k_pitch, na.rm = TRUE),

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz),
        Win11_pct    = sdiv(oneone_win, oneone_den),
        PutAway_pct  = sdiv(putaway_num, putaway_den)
      )

    totals <- p_flags %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(TotalPitches = dplyr::n(), .groups = "drop")

    pitch_rates_pt <- pitch_rates_pt %>%
      dplyr::left_join(totals, by = "GameID") %>%
      dplyr::mutate(Usage_pct = sdiv(Pitches, TotalPitches))

    pitch_metrics_pt <- p_flags %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        IVB        = mean_or_na(InducedVertBreak),
        HB         = mean_or_na(HorzBreak),
        AvgVelo    = mean_or_na(RelSpeed),
        Extension  = mean_or_na(Extension),
        VAA        = mean_or_na(VertApprAngle),
        MaxVelocity = max_or_na(RelSpeed),
        .groups = "drop"
      )

    fps_pa <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop")

    pa_last <- p_flags %>%
      dplyr::group_by(GameID, PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    pa_last$PitchTypeStd <- as.character(pa_last$PitchType)
    pa_last$PitchTypeStd[is.na(pa_last$PitchTypeStd) | !nzchar(pa_last$PitchTypeStd)] <- "Undefined"

    pa_group <- pa_last %>% dplyr::select(GameID, PA_ID, PitchTypeStd)

    fps_rates_pt <- fps_pa %>%
      dplyr::left_join(pa_group, by = c("GameID","PA_ID")) %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                       .groups = "drop")

    ea_pa <- calc_ea_pa_summary(p_flags, c("GameID", "PA_ID")) %>%
      dplyr::left_join(pa_group, by = c("GameID","PA_ID"))

    ea_rates_pt <- ea_pa %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        EA = calc_ea_rate_from_pa(tibble::tibble(
          n_pitches = n_pitches,
          strikes_first3 = strikes_first3,
          early_bip = early_bip,
          any_hbp = any_hbp,
          any_barrel = any_barrel
        )),
        .groups = "drop"
      )

    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)

    pa_last$K_flag  <- outc$K
    pa_last$BB_flag <- any_walk

    pa_rates_pt <- pa_last %>%
      dplyr::group_by(GameID, PitchTypeStd) %>%
      dplyr::summarise(
        PA  = dplyr::n(),
        Kp  = mean(K_flag,  na.rm = TRUE),
        BBp = mean(BB_flag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(fps_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(ea_rates_pt,  by = c("GameID","PitchTypeStd"))

    pa_last <- pa_last %>%
      dplyr::mutate(.ts_grp = paste(GameID, PitchTypeStd, sep = "||"))
    pa_ts_groups <- pa_last %>%
      dplyr::distinct(.ts_grp, GameID, PitchTypeStd)

    woba_rates_pt <- compute_woba_grouped(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = .grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, wOBA, wOBAcon)
    fip_rates_pt <- compute_fip_xfip_grouped(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = .grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, FIP)
    batting_rates_pt <- summarize_batting_simple(pa_last, pa_last$.ts_grp) %>%
      dplyr::rename(.ts_grp = grp) %>%
      dplyr::left_join(pa_ts_groups, by = ".ts_grp") %>%
      dplyr::select(GameID, PitchTypeStd, SLG, OPS)

    pitch_rates_pt %>%
      dplyr::left_join(pa_rates_pt,   by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(woba_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(batting_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(fip_rates_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(pitch_metrics_pt, by = c("GameID","PitchTypeStd")) %>%
      dplyr::left_join(game_info,     by = "GameID")
  })

  perf_ts_pitch_types <- reactive({
    df <- perf_ts_data_ptype()
    if (is.null(df) || !nrow(df)) return(character(0))
    df %>%
      dplyr::filter(valid_ts_pitch_type(.data$PitchTypeStd)) %>%
      dplyr::group_by(PitchTypeStd) %>%
      dplyr::summarise(Pitches = sum(Pitches, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Pitches), PitchTypeStd) %>%
      dplyr::pull(PitchTypeStd)
  })

  ptype_input_id <- function(pt) {
    paste0("perf_ts_ptype_", gsub("[^A-Za-z0-9]", "_", tolower(pt)))
  }

  perf_ts_ptype_selections <- reactive({
    pts <- perf_ts_pitch_types()
    out <- list()
    for (pt in pts) {
      id <- ptype_input_id(pt)
      sel <- input[[id]]
      if (!is.null(sel) && length(sel)) out[[pt]] <- sel
    }
    out
  })

  output$perf_ts_ptype_perf_boxes <- renderUI({
    pts <- perf_ts_pitch_types()
    if (!length(pts)) return(div("No pitch types available."))
    ptype_choices <- PERF_TS_PTYPE_CHOICES
    div(class = "base-pitch-metric-grid", lapply(pts, function(pt) {
      id <- ptype_input_id(pt)
      div(
        class = "base-pitch-metric-filter",
        span(class = "base-pitch-metric-label", pt),
        selectizeInput(
          id, NULL,
          choices = ptype_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Add metrics"),
          width = "100%"
        )
      )
    }))
  })

  team_ts_pitch_types <- reactive({
    df <- team_ts_data_ptype()
    if (is.null(df) || !nrow(df)) return(character(0))
    df %>%
      dplyr::filter(valid_ts_pitch_type(.data$PitchTypeStd)) %>%
      dplyr::group_by(PitchTypeStd) %>%
      dplyr::summarise(Pitches = sum(Pitches, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Pitches), PitchTypeStd) %>%
      dplyr::pull(PitchTypeStd)
  })

  team_ptype_input_id <- function(pt) {
    paste0("team_ts_ptype_", gsub("[^A-Za-z0-9]", "_", tolower(pt)))
  }

  team_ts_ptype_selections <- reactive({
    pts <- team_ts_pitch_types()
    out <- list()
    for (pt in pts) {
      id <- team_ptype_input_id(pt)
      sel <- input[[id]]
      if (!is.null(sel) && length(sel)) out[[pt]] <- sel
    }
    out
  })

  output$team_ts_ptype_perf_boxes <- renderUI({
    pts <- team_ts_pitch_types()
    if (!length(pts)) return(div("No pitch types available."))
    ptype_choices <- PERF_TS_PTYPE_CHOICES
    div(class = "base-pitch-metric-grid", lapply(pts, function(pt) {
      id <- team_ptype_input_id(pt)
      div(
        class = "base-pitch-metric-filter",
        span(class = "base-pitch-metric-label", pt),
        selectizeInput(
          id, NULL,
          choices = ptype_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Add metrics"),
          width = "100%"
        )
      )
    }))
  })

  perf_ts_meta <- function(stat) {
    if (stat %in% names(PERF_TS_PERF_STATS)) return(PERF_TS_PERF_STATS[[stat]])
    if (stat %in% names(PERF_TS_PITCH_METRICS)) return(PERF_TS_PITCH_METRICS[[stat]])
    NULL
  }

  perf_ts_d1_ref <- function(stat, pitch_type = NULL) {
    stat <- as.character(stat %||% "")
    if (stat %in% c("Whiff%", "Chase%") && !is.null(pitch_type)) {
      ref <- suppressWarnings(as.numeric(d1_pct_avg_for_metric(stat, pitch_type)))
      if (is.finite(ref)) return(ref)
    }
    if (stat %in% names(D1_PCT_AVG)) {
      ref <- suppressWarnings(as.numeric(D1_PCT_AVG[[stat]]))
      if (is.finite(ref)) return(ref)
    }
    idx <- match(tolower(stat), tolower(names(D1_PCT_AVG)))
    if (!is.na(idx)) {
      ref <- suppressWarnings(as.numeric(D1_PCT_AVG[[idx]]))
      if (is.finite(ref)) return(ref)
    }
    if (stat %in% names(ABS_RULES)) {
      rule <- ABS_RULES[[stat]]
      if (!is.null(rule$green_max) && !is.null(rule$red_min)) {
        ref <- mean(c(rule$green_max, rule$red_min))
        if (is.finite(ref)) return(ref)
      }
      if (!is.null(rule$green_min) && !is.null(rule$red_max)) {
        ref <- mean(c(rule$green_min, rule$red_max))
        if (is.finite(ref)) return(ref)
      }
    }
    NA_real_
  }

  build_perf_ts_plot_gg <- function(df_all, df_pt, stats_all, ptype_sel, roll = FALSE) {
    rows <- list()
    refs <- list()

    add_rows <- function(df, stat, chart_label, series_label, pitch_type = NULL) {
      meta <- perf_ts_meta(stat)
      if (is.null(meta) || is.null(df) || !nrow(df) || !(meta$key %in% names(df))) return(NULL)
      dd <- df %>% dplyr::arrange(.data$GameIndex)
      y_raw <- suppressWarnings(as.numeric(dd[[meta$key]]))
      if (isTRUE(roll)) y_raw <- roll_mean(y_raw, PERF_TS_ROLL_WINDOW)
      y_plot <- if (identical(meta$type, "pct")) y_raw * 100 else y_raw
      keep <- is.finite(y_plot)
      if (!any(keep)) return(NULL)

      rows[[length(rows) + 1L]] <<- tibble::tibble(
        Chart = chart_label,
        Stat = stat,
        Series = series_label,
        GameIndex = dd$GameIndex[keep],
        GameLabel = as.character(dd$GameLabel[keep]),
        Value = y_plot[keep],
        Type = meta$type
      )

      ref <- perf_ts_d1_ref(stat, pitch_type = pitch_type)
      if (is.finite(ref)) {
        refs[[length(refs) + 1L]] <<- tibble::tibble(
          Chart = chart_label,
          LeagueAvg = if (identical(meta$type, "pct")) ref * 100 else ref
        )
      }
      NULL
    }

    stats_all <- intersect(stats_all %||% character(0), PERF_TS_ALL_STATS)
    for (stat in stats_all) {
      add_rows(df_all, stat, stat, stat)
    }

    if (length(ptype_sel)) {
      for (pt in names(ptype_sel)) {
        df_pt_sub <- df_pt %>% dplyr::filter(.data$PitchTypeStd == pt)
        stats_vec <- intersect(ptype_sel[[pt]] %||% character(0), PERF_TS_ALL_STATS)
        for (stat in stats_vec) {
          add_rows(df_pt_sub, stat, paste(pt, stat), pt, pitch_type = pt)
        }
      }
    }

    long <- dplyr::bind_rows(rows)
    validate(need(nrow(long) > 0, "No finite values for selected stats."))

    ref <- if (length(refs)) {
      dplyr::bind_rows(refs) %>%
        dplyr::group_by(.data$Chart) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
    } else {
      tibble::tibble(Chart = character(), LeagueAvg = numeric())
    }

    label_frames <- list()
    if (!is.null(df_all) && nrow(df_all) && all(c("GameIndex", "GameLabel") %in% names(df_all))) {
      label_frames[[length(label_frames) + 1L]] <- df_all %>% dplyr::select(GameIndex, GameLabel)
    }
    if (!is.null(df_pt) && nrow(df_pt) && all(c("GameIndex", "GameLabel") %in% names(df_pt))) {
      label_frames[[length(label_frames) + 1L]] <- df_pt %>% dplyr::select(GameIndex, GameLabel)
    }
    all_labels <- dplyr::bind_rows(label_frames) %>%
      dplyr::filter(is.finite(.data$GameIndex)) %>%
      dplyr::distinct(.data$GameIndex, .data$GameLabel) %>%
      dplyr::arrange(.data$GameIndex)

    limits <- long %>%
      dplyr::left_join(ref, by = "Chart") %>%
      dplyr::group_by(.data$Chart) %>%
      dplyr::summarise(
        mid = {
          avg_vals <- .data$LeagueAvg[is.finite(.data$LeagueAvg)]
          if (length(avg_vals)) avg_vals[[1]] else mean(.data$Value, na.rm = TRUE)
        },
        span = max(abs(.data$Value - mid), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        span = ifelse(is.finite(.data$span) & .data$span > 0, .data$span * 1.25, 0.05),
        ymin = .data$mid - .data$span,
        ymax = .data$mid + .data$span,
        GameIndex = min(long$GameIndex, na.rm = TRUE)
      )

    ggplot(long, aes(GameIndex, Value, group = Series)) +
      geom_blank(data = limits, aes(x = GameIndex, y = ymin), inherit.aes = FALSE) +
      geom_blank(data = limits, aes(x = GameIndex, y = ymax), inherit.aes = FALSE) +
      geom_hline(data = ref, aes(yintercept = LeagueAvg), linetype = "dashed", color = "grey45", linewidth = 0.7) +
      geom_text(
        data = ref,
        aes(x = Inf, y = LeagueAvg, label = "D1 Avg"),
        inherit.aes = FALSE,
        hjust = 1.05, vjust = -0.35, color = "grey35", size = 3
      ) +
      geom_line(color = "#501214", linewidth = 1.1, na.rm = TRUE) +
      geom_point(color = "#B4975A", fill = "#B4975A", size = 2.5, na.rm = TRUE) +
      facet_wrap(~Chart, scales = "free_y", ncol = 2) +
      scale_x_continuous(
        breaks = all_labels$GameIndex,
        labels = all_labels$GameLabel,
        expand = expansion(mult = c(0.02, 0.12))
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(8, 12, 8, 8)
      )
  }

  build_perf_ts_plot <- function(df_all, df_pt, stats_all, ptype_sel, roll = FALSE) {
    df_ref <- if (!is.null(df_all) && nrow(df_all)) df_all else df_pt
    if (is.null(df_ref) || !nrow(df_ref)) return(plotly::plot_ly())

    use_dates <- any(!is.na(df_ref$GameDate))
    xaxis_cfg <- if (use_dates) {
      list(title = "Game Date")
    } else {
      list(title = "Game", tickmode = "array", tickvals = df_ref$GameIndex, ticktext = df_ref$GameLabel)
    }

    get_pt_color <- function(pt) {
      col <- pitch_colors[[pt]]
      if (is.null(col) || is.na(col)) "#808080" else col
    }

    pct_only <- TRUE
    p <- plotly::plot_ly()

    add_series <- function(df, stat_label, meta, color) {
      if (is.null(df) || !nrow(df)) return()
      key <- meta$key
      if (!key %in% names(df)) return()
      df <- df %>% dplyr::arrange(GameIndex)
      x_vals <- if (use_dates) df$GameDate else df$GameIndex
      y_raw <- df[[key]]
      if (isTRUE(roll)) y_raw <- roll_mean(y_raw, PERF_TS_ROLL_WINDOW)
      y_plot <- if (meta$type == "pct") y_raw * 100 else y_raw

      val_fmt <- if (meta$type == "pct") {
        ifelse(is.finite(y_plot), sprintf("%.1f%%", y_plot), "NA")
      } else {
        ifelse(is.finite(y_plot), sprintf("%.1f", y_plot), "NA")
      }
      hover_txt <- paste0("Game: ", df$GameLabel, "<br>", stat_label, ": ", val_fmt)

      p <<- p %>%
        plotly::add_trace(
          type = "scatter",
          mode = "lines+markers",
          x = x_vals,
          y = y_plot,
          name = stat_label,
          line   = list(color = color),
          marker = list(color = color),
          text = hover_txt,
          hoverinfo = "text"
        )
      pct_only <<- pct_only && (meta$type == "pct")
    }

    stats_all <- intersect(stats_all %||% character(0), PERF_TS_ALL_STATS)
    for (stat in stats_all) {
      meta <- perf_ts_meta(stat)
      if (!is.null(meta)) add_series(df_all, stat, meta, PERF_TS_STAT_COLORS[[stat]] %||% "#501214")
    }

    if (length(ptype_sel)) {
      for (pt in names(ptype_sel)) {
        df_pt_sub <- df_pt %>% dplyr::filter(PitchTypeStd == pt)
        stats_vec <- ptype_sel[[pt]] %||% character(0)
        for (stat in stats_vec) {
          meta <- perf_ts_meta(stat)
          if (is.null(meta)) next
          add_series(df_pt_sub, paste(pt, stat), meta, get_pt_color(pt))
        }
      }
    }

    p %>%
      plotly::layout(
        xaxis = xaxis_cfg,
        yaxis = list(
          title = if (pct_only) "Percent" else "Value",
          ticksuffix = if (pct_only) "%" else ""
        ),
        legend = list(orientation = "h", x = 0, y = 1.1),
        margin = list(t = 30),
        hovermode = "x unified"
      )
  }

  output$perf_ts_plot <- renderPlot({
    df_all <- perf_ts_data()
    df_pt  <- perf_ts_data_ptype()
    stats_all <- input$perf_ts_stats %||% character(0)
    ptype_sel <- perf_ts_ptype_selections()
    n_sel <- length(stats_all) + sum(lengths(ptype_sel))
    mode <- input$perf_ts_mode %||% "roll"
    validate(need((nrow(df_all) > 0 || nrow(df_pt) > 0), "No time series data for this selection."))
    validate(need(n_sel > 0, "Select at least one stat to plot."))
    build_perf_ts_plot_gg(df_all, df_pt, stats_all, ptype_sel, roll = identical(mode, "roll"))
  })

  output$team_ts_roll <- plotly::renderPlotly({
    df_all <- team_ts_data()
    df_pt  <- team_ts_data_ptype()
    stats_all <- input$team_ts_stats %||% character(0)
    ptype_sel <- team_ts_ptype_selections()
    n_sel <- length(stats_all) + sum(lengths(ptype_sel))
    validate(need((nrow(df_all) > 0 || nrow(df_pt) > 0), "No time series data for this selection."))
    validate(need(n_sel > 0, "Select at least one stat to plot."))
    build_perf_ts_plot(df_all, df_pt, stats_all, ptype_sel, roll = TRUE)
  })

  output$team_ts_game <- plotly::renderPlotly({
    df_all <- team_ts_data()
    df_pt  <- team_ts_data_ptype()
    stats_all <- input$team_ts_stats %||% character(0)
    ptype_sel <- team_ts_ptype_selections()
    n_sel <- length(stats_all) + sum(lengths(ptype_sel))
    validate(need((nrow(df_all) > 0 || nrow(df_pt) > 0), "No time series data for this selection."))
    validate(need(n_sel > 0, "Select at least one stat to plot."))
    build_perf_ts_plot(df_all, df_pt, stats_all, ptype_sel, roll = FALSE)
  })

  # ======== Team Trends: Release Points ========
  summarize_release_points <- function(d) {
    if (is.null(d) || !nrow(d) || !"Pitcher" %in% names(d)) return(tibble::tibble())

    d <- d %>%
      dplyr::mutate(
        RelHeight = suppressWarnings(as.numeric(RelHeight)),
        RelSide   = suppressWarnings(as.numeric(RelSide)),
        Extension = suppressWarnings(as.numeric(Extension))
      )
    height_present <- "PitcherHeight" %in% names(d)
    if (height_present) d$PitcherHeight <- suppressWarnings(as.numeric(d$PitcherHeight))

    d <- d %>% dplyr::filter(!is.na(Pitcher) & nzchar(Pitcher))

    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    first_height <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)][1]
      if (is.finite(x)) x else NA_real_
    }

    out <- d %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::summarise(
        RelHeight = mean_or_na(RelHeight),
        RelSide   = mean_or_na(RelSide),
        Extension = mean_or_na(Extension),
        PitcherHeight_ft = if (height_present) first_height(PitcherHeight) else NA_real_,
        .groups = "drop"
      )

    out$Height_ft <- mapply(resolve_pitcher_height_ft, out$Pitcher, out$PitcherHeight_ft)
    out$ArmAngle  <- mapply(arm_angle_deg_from_rel, out$RelHeight, out$RelSide, out$Height_ft)
    out
  }

  team_release_points <- reactive({
    summarize_release_points(team_trends_data())
  })

  output$team_release_points_plot <- plotly::renderPlotly({
    pts <- team_release_points()
    validate(need(!is.null(pts) && nrow(pts) > 0, "No release-point data for this selection."))

    pts <- pts %>%
      dplyr::mutate(
        PitcherDisplay = name_display(Pitcher),
        Class = pitcher_class_2026(Pitcher),
        Class = factor(Class, levels = c(names(TEAM_CLASS_COLORS), "Unknown"))
      ) %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight))
    validate(need(nrow(pts) > 0, "No finite release-point data for this selection."))

    class_colors <- c(TEAM_CLASS_COLORS, Unknown = "#7A7A7A")
    plotly::plot_ly(
      data = pts,
      x = ~RelSide,
      y = ~RelHeight,
      type = "scatter",
      mode = "markers+text",
      color = ~Class,
      colors = unname(class_colors),
      text = ~PitcherDisplay,
      textposition = "top center",
      textfont = list(size = 10, color = "#2A2421"),
      marker = list(size = 13, opacity = 0.9, line = list(color = "#FFFFFF", width = 1.5)),
      hovertext = ~paste0(
        PitcherDisplay,
        "<br>Class: ", Class,
        "<br>Release Side: ", sprintf("%.1f ft", RelSide),
        "<br>Release Height: ", sprintf("%.1f ft", RelHeight)
      ),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Release Side (ft)", zeroline = TRUE, zerolinecolor = "#B8B1AA"),
        yaxis = list(title = "Release Height (ft)"),
        legend = list(title = list(text = "Class"), orientation = "h", x = 0, y = 1.12),
        margin = list(t = 72, r = 30, b = 55, l = 65),
        hovermode = "closest"
      )
  })

  team_release_season_points <- reactive({
    summarize_release_points(team_trends_data_season())
  })

  team_release_date_vec <- reactive({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d)) return(as.Date(character(0)))

    gd <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("PitchDate" %in% names(d)) {
      parse_date_any(d$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(d)) {
      gd <- parse_gameid_date(d$CustomGameID)
    }
    gd
  })

  output$team_release_game_date_ui <- renderUI({
    gd <- team_release_date_vec()
    gd <- sort(unique(gd[!is.na(gd)]), decreasing = TRUE)
    if (!length(gd)) return(div("No game dates available."))

    choices <- setNames(as.character(gd), format(gd, "%m/%d/%y"))
    selected <- input$team_release_game_date
    if (is.null(selected) || !length(selected) || !(selected[[1]] %in% choices)) selected <- choices[[1]]

    selectInput(
      "team_release_game_date",
      "Game Date:",
      choices  = choices,
      selected = selected,
      multiple = FALSE
    )
  })

  team_release_game_data <- reactive({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    gd <- team_release_date_vec()
    selected_date <- input$team_release_game_date
    if (is.null(selected_date) || !length(selected_date) || !nzchar(as.character(selected_date[[1]]))) {
      return(tibble::tibble())
    }
    target <- suppressWarnings(as.Date(selected_date[[1]]))
    if (length(target) != 1L || is.na(target)) return(tibble::tibble())
    d <- d[!is.na(gd) & gd == target, , drop = FALSE]
    d
  })

  output$team_release_player_ui <- renderUI({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d) || !"Pitcher" %in% names(d)) return(div("No pitchers available."))
    pits <- sort(unique(na.omit(as.character(d$Pitcher))))
    pits <- setdiff(pits, EXCLUDE_PLAYERS)
    if (!length(pits)) return(div("No pitchers available."))
    selected <- input$team_release_player
    if (is.null(selected) || !selected %in% pits) selected <- pits[[1]]
    selectInput(
      "team_release_player",
      "Pitcher:",
      choices  = pits,
      selected = selected,
      multiple = FALSE
    )
  })

  team_release_player_games <- reactive({
    d <- team_trends_data()
    if (is.null(d) || !nrow(d) || !"Pitcher" %in% names(d)) return(tibble::tibble())
    req(input$team_release_player)
    d <- d %>% dplyr::filter(Pitcher == input$team_release_player)
    if (!nrow(d)) return(tibble::tibble())

    # GameID + GameDate fallbacks
    game_id <- if ("CustomGameID" %in% names(d)) {
      as.character(d$CustomGameID)
    } else if ("GameID" %in% names(d)) {
      as.character(d$GameID)
    } else if ("Game" %in% names(d)) {
      as.character(d$Game)
    } else {
      rep("Game", nrow(d))
    }
    d$GameID <- ifelse(is.na(game_id) | !nzchar(game_id), "Game", game_id)

    gd <- if ("GameDate" %in% names(d)) {
      parse_date_any(d$GameDate)
    } else if ("Date" %in% names(d)) {
      parse_date_any(d$Date)
    } else if ("PitchDate" %in% names(d)) {
      parse_date_any(d$PitchDate)
    } else {
      as.Date(NA)
    }
    if (all(is.na(gd)) && "CustomGameID" %in% names(d)) {
      gd <- parse_gameid_date(d$CustomGameID)
    }
    d$GameDate <- gd

    d
  })

  summarize_release_points_by_game <- function(d) {
    if (is.null(d) || !nrow(d) || !"Pitcher" %in% names(d)) return(tibble::tibble())

    d <- d %>%
      dplyr::mutate(
        RelHeight = suppressWarnings(as.numeric(RelHeight)),
        RelSide   = suppressWarnings(as.numeric(RelSide)),
        Extension = suppressWarnings(as.numeric(Extension))
      )
    height_present <- "PitcherHeight" %in% names(d)
    if (height_present) d$PitcherHeight <- suppressWarnings(as.numeric(d$PitcherHeight))

    mean_or_na <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
    }
    first_height <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)][1]
      if (is.finite(x)) x else NA_real_
    }

    out <- d %>%
      dplyr::group_by(Pitcher, GameID, GameDate) %>%
      dplyr::summarise(
        RelHeight = mean_or_na(RelHeight),
        RelSide   = mean_or_na(RelSide),
        Extension = mean_or_na(Extension),
        PitcherHeight_ft = if (height_present) first_height(PitcherHeight) else NA_real_,
        .groups = "drop"
      )

    out$Height_ft <- mapply(resolve_pitcher_height_ft, out$Pitcher, out$PitcherHeight_ft)
    out$ArmAngle  <- mapply(arm_angle_deg_from_rel, out$RelHeight, out$RelSide, out$Height_ft)
    out
  }

  output$team_release_player_table <- DT::renderDT({
    d <- team_release_player_games()
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data for selected pitcher"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    game_sum <- summarize_release_points_by_game(d)
    if (is.null(game_sum) || !nrow(game_sum)) {
      return(DT::datatable(data.frame(Status = "No data for selected pitcher"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    season_sum <- team_release_season_points()

    tbl <- game_sum %>%
      dplyr::left_join(season_sum, by = "Pitcher", suffix = c("_game","_season")) %>%
      dplyr::mutate(
        relh_delta = RelHeight_game - RelHeight_season,
        rels_delta = RelSide_game   - RelSide_season,
        ext_delta  = Extension_game - Extension_season,
        arm_delta  = ArmAngle_game  - ArmAngle_season
      )

    has_id <- !is.na(tbl$GameID) & nzchar(tbl$GameID)
    game_label <- ifelse(
      !is.na(tbl$GameDate),
      format(tbl$GameDate, "%m/%d/%y"),
      ifelse(has_id, as.character(tbl$GameID), "")
    )
    game_label <- ifelse(!is.na(tbl$GameDate) & has_id, paste0(game_label, " (", tbl$GameID, ")"), game_label)
    tbl$GameLabel <- game_label

    tbl <- tbl %>% dplyr::arrange(dplyr::desc(GameDate))

    out_display <- tbl %>%
      dplyr::transmute(
        Game = GameLabel,
        `Release Height` = format_metric_with_delta_vec(RelHeight_game, relh_delta, threshold = 0.3, digits = 1),
        `Release Side`   = format_metric_with_delta_vec(RelSide_game,   rels_delta, threshold = 0.3, digits = 1),
        Extension        = format_metric_with_delta_vec(Extension_game, ext_delta,  threshold = 0.3, digits = 1, pos_green = TRUE),
        `Arm Angle (deg)` = format_metric_with_delta_vec(ArmAngle_game, arm_delta,  threshold = 5,   digits = 1)
      )

    sort_cols <- tbl %>%
      dplyr::transmute(
        ..sort_game_date     = as.numeric(GameDate),
        ..sort_release_height = RelHeight_game,
        ..sort_release_side   = RelSide_game,
        ..sort_extension      = Extension_game,
        ..sort_arm_angle      = ArmAngle_game
      )

    out <- cbind(out_display, sort_cols)

    display_cols <- names(out_display)
    sort_cols_names <- names(sort_cols)
    display_idx0 <- seq_along(display_cols) - 1
    sort_idx0 <- seq_along(sort_cols_names) - 1 + length(display_cols)
    order_defs <- list(
      list(targets = display_idx0[[1]], orderData = sort_idx0[[1]]),
      list(targets = display_idx0[[2]], orderData = sort_idx0[[2]]),
      list(targets = display_idx0[[3]], orderData = sort_idx0[[3]]),
      list(targets = display_idx0[[4]], orderData = sort_idx0[[4]]),
      list(targets = display_idx0[[5]], orderData = sort_idx0[[5]])
    )

    DT::datatable(
      out,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom        = 't',
        paging     = FALSE,
        ordering   = TRUE,
        scrollX    = TRUE,
        columnDefs = c(list(list(targets = sort_idx0, visible = FALSE)), order_defs)
      ),
      class = "stripe"
    )
  })

  format_metric_with_delta <- function(val, delta, threshold, digits = 1, pos_green = FALSE) {
    if (!is.finite(val)) return("—")
    val_str <- sprintf(paste0("%.", digits, "f"), val)
    if (!is.finite(delta)) return(val_str)

    delta_str <- sprintf(paste0("%+.", digits, "f"), delta)
    color <- NA_character_
    if (abs(delta) >= threshold) {
      if (isTRUE(pos_green) && delta > 0) {
        color <- "#1B5E20"
      } else {
        color <- "#B00020"
      }
    }

    delta_html <- if (is.na(color)) {
      sprintf("<span style=\"color:#666;\">[%s]</span>", delta_str)
    } else {
      sprintf("<span style=\"color:%s; font-weight:600;\">[%s]</span>", color, delta_str)
    }

    sprintf("<span data-order=\"%s\">%s %s</span>", sprintf("%.3f", val), val_str, delta_html)
  }

  format_metric_with_delta_vec <- function(vals, deltas, threshold, digits = 1, pos_green = FALSE) {
    mapply(
      function(v, d) format_metric_with_delta(v, d, threshold, digits, pos_green),
      vals, deltas,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
  }

  output$team_release_points_table <- DT::renderDT({
    base <- team_release_points()
    if (is.null(base) || !nrow(base)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    out <- base %>%
      dplyr::transmute(
        Pitcher = Pitcher,
        `Release Height` = RelHeight,
        `Release Side`   = RelSide,
        Extension        = Extension,
        `Arm Angle (deg)` = ArmAngle
      ) %>%
      dplyr::arrange(Pitcher)

    dt <- DT::datatable(
      out,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = 't',
        paging   = FALSE,
        ordering = TRUE,
        scrollX  = TRUE
      ),
      class = "stripe"
    )
    DT::formatRound(dt, columns = 2:5, digits = 1)
  })

  output$team_release_game_table <- DT::renderDT({
    game <- team_release_game_data()
    if (is.null(game) || !nrow(game)) {
      return(DT::datatable(data.frame(Status = "No data for selected date"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    game_sum <- summarize_release_points(game)
    if (is.null(game_sum) || !nrow(game_sum)) {
      return(DT::datatable(data.frame(Status = "No data for selected date"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    season_sum <- team_release_season_points()

    tbl <- game_sum %>%
      dplyr::left_join(season_sum, by = "Pitcher", suffix = c("_game","_season")) %>%
      dplyr::mutate(
        relh_delta = RelHeight_game - RelHeight_season,
        rels_delta = RelSide_game   - RelSide_season,
        ext_delta  = Extension_game - Extension_season,
        arm_delta  = ArmAngle_game  - ArmAngle_season
      )

    tbl <- tbl %>% dplyr::arrange(Pitcher)

    out_display <- tbl %>%
      dplyr::transmute(
        Pitcher = Pitcher,
        `Release Height` = format_metric_with_delta_vec(RelHeight_game, relh_delta, threshold = 0.3, digits = 1),
        `Release Side`   = format_metric_with_delta_vec(RelSide_game,   rels_delta, threshold = 0.3, digits = 1),
        Extension        = format_metric_with_delta_vec(Extension_game, ext_delta,  threshold = 0.3, digits = 1, pos_green = TRUE),
        `Arm Angle (deg)` = format_metric_with_delta_vec(ArmAngle_game, arm_delta,  threshold = 5,   digits = 1)
      )

    sort_cols <- tbl %>%
      dplyr::transmute(
        ..sort_release_height = RelHeight_game,
        ..sort_release_side   = RelSide_game,
        ..sort_extension      = Extension_game,
        ..sort_arm_angle      = ArmAngle_game
      )

    out <- cbind(out_display, sort_cols)

    display_cols <- names(out_display)
    sort_cols_names <- names(sort_cols)
    display_idx0 <- seq_along(display_cols) - 1
    sort_idx0 <- seq_along(sort_cols_names) - 1 + length(display_cols)
    order_defs <- list(
      list(targets = display_idx0[[2]], orderData = sort_idx0[[1]]),
      list(targets = display_idx0[[3]], orderData = sort_idx0[[2]]),
      list(targets = display_idx0[[4]], orderData = sort_idx0[[3]]),
      list(targets = display_idx0[[5]], orderData = sort_idx0[[4]])
    )

    DT::datatable(
      out,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom        = 't',
        paging     = FALSE,
        ordering   = TRUE,
        scrollX    = TRUE,
        columnDefs = c(list(list(targets = sort_idx0, visible = FALSE)), order_defs)
      ),
      class = "stripe"
    )
  })

  # ---- Shut Down Inning% helper ----
  compute_shutdown_by_pitcher <- function(d_pitch, d_all = NULL, team_code = TEAM_CODE) {
    d_pitch <- tibble::as_tibble(d_pitch)
    if (is.null(d_pitch) || !nrow(d_pitch) || !"Pitcher" %in% names(d_pitch)) {
      return(tibble::tibble(Pitcher = character(), ShutDownOpp = integer(), ShutDown = integer(), ShutDown_pct = NA_real_))
    }
    
    pitchers_all <- unique(as.character(d_pitch$Pitcher))
    pitchers_all <- pitchers_all[!is.na(pitchers_all) & nzchar(pitchers_all)]
    if (!length(pitchers_all)) {
      return(tibble::tibble(Pitcher = character(), ShutDownOpp = integer(), ShutDown = integer(), ShutDown_pct = NA_real_))
    }
    if (is.null(d_all) || !nrow(d_all)) d_all <- d_pitch
    d_all <- tibble::as_tibble(d_all)
    
    topbot_cols <- c("Top/Bottom","TopBottom","InningTopBot","TopBot","Top_Bottom")
    topbot_all  <- intersect(topbot_cols, names(d_all))[1]
    topbot_pitch <- intersect(topbot_cols, names(d_pitch))[1]
    
    need_cols_all <- c("Inning","RunsScored","HomeTeam","AwayTeam")
    if (is.na(topbot_all) || !all(need_cols_all %in% names(d_all)) || is.na(topbot_pitch) || !"Inning" %in% names(d_pitch)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    game_cols <- c("CustomGameID","GameUID","GameID","GameId","Game","Game_Id")
    game_col <- intersect(game_cols, names(d_all))
    game_col <- intersect(game_col, names(d_pitch))[1]
    
    get_game_key <- function(df, gcol) {
      if (!is.na(gcol)) {
        return(as.character(df[[gcol]]))
      }
      if (!all(c("HomeTeam","AwayTeam") %in% names(df))) return(rep(NA_character_, nrow(df)))
      date_vec <- if ("GameDate" %in% names(df)) parse_date_any(df$GameDate) else if ("Date" %in% names(df)) parse_date_any(df$Date) else as.Date(NA)
      paste0(as.character(date_vec), "|", as.character(df$HomeTeam %||% ""), "|", as.character(df$AwayTeam %||% ""))
    }
    
    norm_topbot <- function(x) {
      x <- toupper(trimws(as.character(x)))
      dplyr::case_when(
        x %in% c("TOP","T") ~ "Top",
        x %in% c("BOTTOM","BOT","B") ~ "Bottom",
        TRUE ~ NA_character_
      )
    }
    norm_team <- function(x) {
      x <- toupper(trimws(as.character(x)))
      x[is.na(x) | !nzchar(x)] <- NA_character_
      x
    }
    
    game_key_all <- get_game_key(d_all, game_col)
    game_key_pitch <- get_game_key(d_pitch, game_col)
    keys <- unique(game_key_pitch[!is.na(game_key_pitch) & nzchar(game_key_pitch)])
    if (!length(keys)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    d_all2 <- tibble::tibble(
      GameKey = game_key_all,
      Inning = suppressWarnings(as.integer(readr::parse_number(as.character(d_all$Inning)))),
      TopBot = norm_topbot(d_all[[topbot_all]]),
      RunsScored = suppressWarnings(as.numeric(d_all$RunsScored)),
      HomeTeam = as.character(d_all$HomeTeam),
      AwayTeam = as.character(d_all$AwayTeam)
    ) %>%
      dplyr::filter(!is.na(GameKey), GameKey %in% keys, !is.na(Inning), !is.na(TopBot))
    d_all2$RunsScored[!is.finite(d_all2$RunsScored)] <- 0
    
    if (!nrow(d_all2)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    d_all2 <- d_all2 %>%
      dplyr::mutate(
        BatTeam   = ifelse(TopBot == "Top", AwayTeam, HomeTeam),
        FieldTeam = ifelse(TopBot == "Top", HomeTeam, AwayTeam),
        BatTeamCode = norm_team(BatTeam),
        FieldTeamCode = norm_team(FieldTeam)
      )
    
    half_runs <- d_all2 %>%
      dplyr::group_by(GameKey, Inning, TopBot) %>%
      dplyr::summarise(
        Runs = sum(RunsScored, na.rm = TRUE),
        BatTeamCode = dplyr::first(BatTeamCode),
        FieldTeamCode = dplyr::first(FieldTeamCode),
        .groups = "drop"
      )
    
    scoring_half <- half_runs %>%
      dplyr::filter(Runs > 0, !is.na(BatTeamCode), nzchar(BatTeamCode))
    if (!nrow(scoring_half)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    opp_half <- scoring_half %>%
      dplyr::mutate(
        TeamCode = BatTeamCode,
        NextTopBot = ifelse(TopBot == "Top", "Bottom", "Top"),
        NextInning = ifelse(TopBot == "Top", Inning, Inning + 1L)
      ) %>%
      dplyr::transmute(GameKey, Inning = NextInning, TopBot = NextTopBot, TeamCode) %>%
      dplyr::distinct() %>%
      dplyr::left_join(half_runs, by = c("GameKey","Inning","TopBot")) %>%
      dplyr::filter(FieldTeamCode == TeamCode)
    
    if (!nrow(opp_half)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    d_pitch2 <- d_pitch
    d_pitch2 <- prefer_team_or_portal(d_pitch2, "PitcherTeam", team_code)
    game_key_pitch2 <- get_game_key(d_pitch2, game_col)
    pitcher_team_vals <- if ("PitcherTeam" %in% names(d_pitch2)) {
      as.character(d_pitch2$PitcherTeam)
    } else {
      rep(NA_character_, nrow(d_pitch2))
    }
    pitcher_team_vals <- norm_team(pitcher_team_vals)
    if (!is.null(team_code) && nzchar(team_code)) {
      pitcher_team_vals[is.na(pitcher_team_vals) | !nzchar(pitcher_team_vals)] <- norm_team(team_code)
    }
    
    pitcher_half <- tibble::tibble(
      GameKey = game_key_pitch2,
      Inning = suppressWarnings(as.integer(readr::parse_number(as.character(d_pitch2$Inning)))),
      TopBot = norm_topbot(d_pitch2[[topbot_pitch]]),
      Pitcher = as.character(d_pitch2$Pitcher),
      TeamCode = pitcher_team_vals,
      PitcherRuns = if ("RunsScored" %in% names(d_pitch2)) suppressWarnings(as.numeric(d_pitch2$RunsScored)) else 0
    ) %>%
      dplyr::mutate(PitcherRuns = dplyr::coalesce(PitcherRuns, 0)) %>%
      dplyr::filter(!is.na(GameKey), GameKey %in% keys, !is.na(Inning), !is.na(TopBot),
                    !is.na(Pitcher) & nzchar(Pitcher),
                    !is.na(TeamCode) & nzchar(TeamCode)) %>%
      dplyr::group_by(GameKey, Inning, TopBot, Pitcher, TeamCode) %>%
      dplyr::summarise(PitcherRuns = sum(PitcherRuns, na.rm = TRUE), .groups = "drop")
    
    if (!nrow(pitcher_half)) {
      return(tibble::tibble(Pitcher = pitchers_all, ShutDownOpp = 0L, ShutDown = 0L, ShutDown_pct = NA_real_))
    }
    
    shut <- pitcher_half %>%
      dplyr::inner_join(opp_half, by = c("GameKey","Inning","TopBot","TeamCode")) %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::summarise(
        ShutDownOpp = dplyr::n(),
        ShutDown    = sum(PitcherRuns == 0, na.rm = TRUE),
        ShutDown_pct = sdiv(ShutDown, ShutDownOpp),
        .groups = "drop"
      )
    
    tibble::tibble(Pitcher = pitchers_all) %>%
      dplyr::left_join(shut, by = "Pitcher") %>%
      dplyr::mutate(
        ShutDownOpp = dplyr::coalesce(ShutDownOpp, 0L),
        ShutDown    = dplyr::coalesce(ShutDown, 0L),
        ShutDown_pct = ifelse(ShutDownOpp > 0, sdiv(ShutDown, ShutDownOpp), NA_real_)
      )
  }

  # ======== Performance Tables ========
  performance_table_data <- reactive({
    req(input$PitcherInput, input$perf_split)
    d <- dataFilter()   
    nr <- NROW(d)
    
    
    p <- d
    
    
    nr <- NROW(d)
    if (nr == 0) {
      validate(shiny::need(FALSE, paste0("No performance rows after filters. Rows=", nr)))
    }
    
    
    
   
    # game filter
    if (!is.null(input$GameInput) && length(input$GameInput) &&
        !any(input$GameInput %in% c("All","ALL","All Games","AllGames"))) {
      p <- p %>% dplyr::filter(CustomGameID %in% input$GameInput)
    }
    
    # batter hand filter (use normalized hand so L/R vs Left/Right never mis-match)
    if (!is.null(input$BatterHand) && length(input$BatterHand) &&
        !any(input$BatterHand %in% c("All","Both"))) {
      p <- p %>%
        dplyr::mutate(
          BatterSideStd = dplyr::case_when(
            BatterSide %in% c("L","Left","LHH","LH") ~ "L",
            BatterSide %in% c("R","Right","RHH","RH") ~ "R",
            TRUE ~ as.character(BatterSide)
          )
        ) %>%
        dplyr::filter(.data$BatterSideStd %in% input$BatterHand)
    }
    
    p0 <- p
    p <- prepare_aar_flags(p)
    
    
    if (!nrow(p)) {
      return(data.frame(Status = "No data"))
    }
    
    # GROUPING
    split <- match.arg(input$perf_split, c("hand","ptype"))
    
    if (split == "hand") {
      # normalize BatterSide once for grouping consistency
      p$BatterSideStd <- dplyr::case_when(
        p$BatterSide %in% c("L","Left","LHH","LH") ~ "L",
        p$BatterSide %in% c("R","Right","RHH","RH") ~ "R",
        TRUE ~ as.character(p$BatterSide)
      )
      p$grp <- dplyr::case_when(
        p$BatterSideStd == "L" ~ "vLHH",
        p$BatterSideStd == "R" ~ "vRHH",
        TRUE ~ NA_character_
      )
      group_label <- "Batter"
    } else {
      pt <- as.character(p$PitchType)
      pt[!nzchar(pt)] <- "Undefined"
      p$grp <- pt
      group_label <- "Pitch type"
    }
    p <- p %>% dplyr::filter(!is.na(.data$grp))
    
    # =========================
    # PITCH-LEVEL FLAGS & RATES
    # =========================
    # Robust EV/LA detection (fallback if strict resolver yields all NA)
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    to_num_local <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))

    resolve_rbi_vec <- function(df, outc) {
      rbi_cands <- c("RBI","RBI|PIT","RBI_PIT","RBI (PIT)","RBI_P","RunsBattedIn","Runs_Batted_In","RBIs")
      col <- rbi_cands[rbi_cands %in% names(df)][1]
      if (!is.na(col)) {
        return(list(vec = to_num_local(df[[col]]), present = TRUE))
      }
      if ("RunsScored" %in% names(df)) {
        rbi <- to_num_local(df[["RunsScored"]])
        rbi <- ifelse(outc$BIP_pa %in% TRUE, rbi, 0)
        return(list(vec = rbi, present = TRUE))
      }
      list(vec = rep(NA_real_, nrow(df)), present = FALSE)
    }

    summarize_batting <- function(pa_last_df, grp_vec) {
      if (is.null(pa_last_df) || !nrow(pa_last_df)) {
        return(tibble::tibble(
          grp = character(),
          BB = double(), HBP = double(), K = double(), HR = double(),
          H = double(), TB = double(), SF = double(), RBI = double(),
          PA = integer(), AB = double(), OBP = double(), SLG = double(), OPS = double()
        ))
      }

      outc <- pa_outcome_cols(pa_last_df)
      rbi_info <- resolve_rbi_vec(pa_last_df, outc)
      rbi_vec <- rbi_info$vec
      rbi_present <- rbi_info$present

      resolve_stat_vec <- function(df, cands, default_vec) {
        col <- cands[cands %in% names(df)][1]
        if (!is.na(col)) {
          return(list(vec = to_num_local(df[[col]]), present = TRUE))
        }
        list(vec = default_vec, present = FALSE)
      }

      sf_vec <- grepl("(?i)sacrifice fly|\\bsf\\b", outc$PR_txt)
      tb_default <- as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
      h_vec  <- outc$X1B | outc$X2B | outc$X3B | outc$HR

      kb_chr <- if ("KorBB" %in% names(pa_last_df)) as.character(pa_last_df$KorBB) else rep("", nrow(pa_last_df))
      pr_chr <- if ("PlayResult" %in% names(pa_last_df)) as.character(pa_last_df$PlayResult) else rep("", nrow(pa_last_df))
      ibb_vec <- grepl("intentional|\\bibb\\b", kb_chr, ignore.case = TRUE) |
        grepl("intentional", pr_chr, ignore.case = TRUE)

      bb_default <- as.numeric(outc$BB) + as.numeric(ibb_vec)
      k_default  <- as.numeric(outc$K)
      hr_default <- as.numeric(outc$HR)

      tb_info <- resolve_stat_vec(pa_last_df, c("TB|PIT","TB_PIT","TB","TotalBases"), tb_default)
      bb_info <- resolve_stat_vec(pa_last_df, c("BB|PIT","BB_PIT","BB"), bb_default)
      k_info  <- resolve_stat_vec(pa_last_df, c("K|PIT","SO|PIT","K_PIT","SO","SO_PIT","SO|PIT"), k_default)
      hr_info <- resolve_stat_vec(pa_last_df, c("HR|PIT","HR_PIT","HR"), hr_default)

      tibble::tibble(
        grp = grp_vec,
        BB  = bb_info$vec,
        HBP = as.numeric(outc$HBP),
        K   = k_info$vec,
        HR  = hr_info$vec,
        H   = as.numeric(h_vec),
        TB  = tb_info$vec,
        SF  = as.numeric(sf_vec),
        RBI = rbi_vec
      ) %>%
        dplyr::group_by(grp) %>%
        dplyr::summarise(
          PA  = dplyr::n(),
          BB  = sum(BB,  na.rm = TRUE),
          HBP = sum(HBP, na.rm = TRUE),
          K   = sum(K,   na.rm = TRUE),
          HR  = sum(HR,  na.rm = TRUE),
          H   = sum(H,   na.rm = TRUE),
          TB  = sum(TB,  na.rm = TRUE),
          SF  = sum(SF,  na.rm = TRUE),
          RBI = if (rbi_present) sum(RBI, na.rm = TRUE) else NA_real_,
          AB  = pmax(PA - BB - HBP - SF, 0),
          OBP = sdiv(H + BB + HBP, AB + BB + HBP + SF),
          SLG = sdiv(TB, AB),
          OPS = ifelse(is.finite(OBP) & is.finite(SLG), OBP + SLG, NA_real_),
          .groups = "drop"
        )
    }

    calc_prv <- function(tb, bb, k, rbi, hr, p) {
      ifelse(is.finite(rbi), sdiv((((tb + bb - k) / 4) + rbi + hr), p) * 100, NA_real_)
    }
    
    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la
    # pitch-level rollups (shared by both tables)
    # ---- 1) build ONE flagged dataset used for ALL summaries ----
    # keep evn/lan as vectors you already computed earlier (same length as p)
    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    # Performance-table barrel definition (match global barrel rules)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    
    # PA-ending K pitch flag (vector, length = nrow(p))
    is_k_pitch_vec <- calc_is_k_pitch(p)
    
    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la  = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,
        
        # robust in-zone + swing + strike flags for downstream rates
        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),
        
        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .twok_no32 = (!is.na(StrikesPre) & StrikesPre == 2) & (!is.na(BallsPre) & BallsPre != 3),
        
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )
    
    
    # ---- 2) summarise ONLY the flagged dataset ----
    pitch_rates <- p_flags %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),
        
        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),
        
        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),
        
        ooz       = sum(!.inz, na.rm = TRUE),                 # out-of-zone opportunities
        chases    = sum(!.inz & .swing, na.rm = TRUE),        # swings out of zone
        
        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        Pre2kZone_pct   = {
          den <- sum(.pre2k & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.pre2k & .inz, na.rm = TRUE), den) else NA_real_
        },
        TwoKZone_pct = {
          den <- sum(.twok_no32 & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok_no32 & .inz, na.rm = TRUE), den) else NA_real_
        },
        PutAway_pct = {
          den <- sum(.twok, na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok & .is_k_pitch, na.rm = TRUE), den) else NA_real_
        },
        
        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),
        
        oneone_den = sum(.at_11, na.rm = TRUE),
        oneone_win = sum(.at_11 & .win_11, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz),
        Win11_pct    = sdiv(oneone_win, oneone_den)
      )
    
    
    # =========================
    # PA-LEVEL RATES
    # =========================
    # Map each PA to a grouping key:
    # - hand table: group by the *first pitch's* BatterSide group in that PA
    # - pitch-type table: group by the *last pitch's* PitchType (terminal pitch type)
    pa_first <- p %>% dplyr::group_by(PA_ID) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    pa_last  <- p %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    
    if (split == "hand") {
      pa_group <- pa_first %>% dplyr::transmute(PA_ID, grp = .data$grp)
    } else {
      # terminal pitch type drives PA-level grouping for FIP/wOBA in pitch-type table
      pt_last <- as.character(pa_last$PitchType); pt_last[!nzchar(pt_last)] <- "Undefined"
      pa_group <- pa_last %>% dplyr::transmute(PA_ID, grp = pt_last)
    }
    
    # FPS% (first-pitch strike) per PA group
    fps_pa <- p %>%
      dplyr::group_by(PA_ID) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::left_join(pa_group, by = "PA_ID")
    
    # E&A%: every PA is in the denominator; numerator is Ahead or eligible Early action
    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID") %>%
      dplyr::left_join(pa_group, by = "PA_ID")
    
    
    # last-pitch outcomes for K/BB/HBP & outs (PA-level)
    pa_out <- pa_last %>%
      dplyr::mutate(grp = pa_group$grp[match(PA_ID, pa_group$PA_ID)]) %>%
      { 
        outc <- pa_outcome_cols(.)
        pr <- outc$PR_txt
        kb <- as.character(.$KorBB %||% "")
        any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
        outs_play <- if ("OutsOnPlay" %in% names(.)) {
          v <- suppressWarnings(as.integer(.$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
        } else rep(0L, nrow(.))
        outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
        outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
        outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
        outs_total <- outs_play + as.integer(outc$K)
        
        tibble::tibble(
          PA_ID = .$PA_ID,
          grp   = .$grp,
          K     = outc$K,
          BB    = (any_walk),
          outs  = outs_total
        )
      }
    
    # FIP & wOBA per group (PA-based, grouped by pa_group logic above)
    fip <- compute_fip_xfip_grouped(
      pa_last,
      grp = pa_group$grp[match(pa_last$PA_ID, pa_group$PA_ID)]
    ) %>%
      dplyr::select(.grp, FIP) %>% dplyr::rename(grp = .grp)
    wob <- compute_woba_grouped(
      pa_last,
      grp = pa_group$grp[match(pa_last$PA_ID, pa_group$PA_ID)]
    ) %>%
      dplyr::select(.grp, wOBA, wOBAcon) %>% dplyr::rename(grp = .grp)
    
    pa_rates <- pa_out %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        PA   = dplyr::n(),
        Kp   = mean(K,     na.rm = TRUE),                # K% per PA
        BBp  = mean(BB,    na.rm = TRUE),                # BB% = walk per PA
        Outs = sum(outs,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        fps_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                                                             .groups = "drop"),
        by = "grp"
      ) %>%
      dplyr::left_join(
        ea_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(
          EA = calc_ea_rate_from_pa(tibble::tibble(
            n_pitches = n_pitches,
            strikes_first3 = strikes_first3,
            early_bip = early_bip,
            any_hbp = any_hbp,
            any_barrel = any_barrel
          )),
          .groups = "drop"
        ),
        by = "grp"
      )

    # SLG / OPS / pRV (PA-level outcomes; pRV uses total pitches as denominator)
    pa_grp_vec <- pa_group$grp[match(pa_last$PA_ID, pa_group$PA_ID)]
    pa_bat <- summarize_batting(pa_last, pa_grp_vec)
    pa_bat_small <- pa_bat %>%
      dplyr::select(grp, SLG, OPS, TB, BB, HBP, K, RBI, HR, H, AB)
    
    # Shut Down Inning% (pitcher-level, applied to all rows)
    sd_tbl <- compute_shutdown_by_pitcher(p, d_all = df_nonbp)
    sd_val <- sd_tbl$ShutDown_pct[match(input$PitcherInput, sd_tbl$Pitcher)]
    if (length(sd_val) == 0L || !is.finite(sd_val)) sd_val <- NA_real_
    sd_fmt <- ifelse(is.finite(sd_val), sprintf("%.0f%%", 100*sd_val), "NA")
    
    # =========================
    # BUILD TABLES
    # =========================
    if (split == "hand") {
      # Handedness table (Batter, includes IP & PA)
      # Handedness table (Batter, includes IP & PA)
      
      # --- precompute 2k OS% by grp safely (NO vector recycling) ---
      p2k_map <- {
        if (all(c("2k OS","2k Opp") %in% names(p_flags))) {
          tmp <- p_flags %>%
            dplyr::group_by(grp) %>%
            dplyr::summarise(
              `2k OS%` = sdiv(
                sum(.data$`2k OS`,  na.rm = TRUE),
                sum(.data$`2k Opp`, na.rm = TRUE)
              ),
              .groups = "drop"
            )
          rlang::set_names(tmp$`2k OS%`, tmp$grp)
        } else {
          # if those columns don't exist, keep it as NA (but never crash)
          grps <- unique(p_flags$grp)
          stats::setNames(rep(NA_real_, length(grps)), grps)
        }
      }
      
      tab <- pitch_rates %>%
        dplyr::left_join(pa_rates, by = "grp") %>%
        dplyr::left_join(pa_bat_small, by = "grp") %>%
        dplyr::left_join(fip,      by = "grp") %>%
        dplyr::left_join(wob,      by = "grp") %>%
        dplyr::mutate(
          Batter = grp,
          IP     = sprintf("%d.%d", Outs %/% 3, Outs %% 3),
          BAA    = ifelse(is.finite(sdiv(H, AB)), sprintf("%.3f", sdiv(H, AB)), "NA"),
          WHIP   = ifelse(is.finite(sdiv(BB + H, Outs / 3)), sprintf("%.2f", sdiv(BB + H, Outs / 3)), "NA"),
          `K/9`  = ifelse(is.finite(sdiv(K * 9, Outs / 3)), sprintf("%.1f", sdiv(K * 9, Outs / 3)), "NA"),
          `BB/9` = ifelse(is.finite(sdiv(BB * 9, Outs / 3)), sprintf("%.1f", sdiv(BB * 9, Outs / 3)), "NA"),
          `H/9`  = ifelse(is.finite(sdiv(H * 9, Outs / 3)), sprintf("%.1f", sdiv(H * 9, Outs / 3)), "NA"),
          
          `K%`      = sprintf("%.0f%%", 100*Kp),
          `BB%`     = sprintf("%.0f%%", 100*BBp),
          `BB+HBP%` = ifelse(is.finite(sdiv(BB + HBP, PA)), sprintf("%.0f%%", 100*sdiv(BB + HBP, PA)), "NA"),
          `Barrel%` = ifelse(is.finite(Barrel_pct), sprintf("%.0f%%", 100*Barrel_pct), "NA"),
          `GB%`     = ifelse(is.finite(GB_pct),     sprintf("%.0f%%", 100*GB_pct),     "NA"),
          `FPS%`    = sprintf("%.0f%%", 100*FPS),
          `1-1 Win%`= ifelse(is.finite(Win11_pct),  sprintf("%.0f%%", 100*Win11_pct),  "NA"),
          `E&A%`    = sprintf("%.0f%%", 100*EA),
          
          `Strike%`     = sprintf("%.0f%%", 100*Strike_pct),
          `Zone%`       = sprintf("%.0f%%", 100*Zone_pct),
          `Pre2k Zone%` = ifelse(is.finite(Pre2kZone_pct), sprintf("%.0f%%", 100*Pre2kZone_pct), "NA"),
          `2k Zone%`    = ifelse(is.finite(TwoKZone_pct),  sprintf("%.0f%%", 100*TwoKZone_pct),  "NA"),
          `Put Away%`   = ifelse(is.finite(PutAway_pct),   sprintf("%.0f%%", 100*PutAway_pct),   "NA"),
          
          `Whiff%`   = ifelse(is.finite(Whiff_pct),   sprintf("%.0f%%", 100*Whiff_pct),   "NA"),
          `CSW%`     = ifelse(is.finite(CSW_pct),     sprintf("%.0f%%", 100*CSW_pct),     "NA"),
          `IZWhiff%` = ifelse(is.finite(IZWhiff_pct), sprintf("%.0f%%", 100*IZWhiff_pct), "NA"),
          `Chase%`   = ifelse(is.finite(Chase_pct),   sprintf("%.0f%%", 100*Chase_pct),   "NA"),
          `Shut Down Inning%` = sd_fmt,
          
          FIP     = ifelse(is.finite(FIP),     sprintf("%.2f", FIP),     "NA"),
          wOBA    = ifelse(is.finite(wOBA),    sprintf("%.3f", wOBA),    "NA"),
          wOBAcon = ifelse(is.finite(wOBAcon), sprintf("%.3f", wOBAcon), "NA"),
          SLG     = ifelse(is.finite(SLG),     sprintf("%.3f", SLG),     "NA"),
          OPS     = ifelse(is.finite(OPS),     sprintf("%.3f", OPS),     "NA"),
          pRV     = ifelse(is.finite(calc_prv(TB, BB, K, RBI, HR, Pitches)),
                           sprintf("%.2f", calc_prv(TB, BB, K, RBI, HR, Pitches)), "NA")
        ) %>%
        dplyr::select(
          Batter, IP, PA, BAA, SLG, OPS, WHIP, `K/9`, `BB/9`, `H/9`,
          wOBA, wOBAcon, pRV, FIP, `K%`, `BB%`, `BB+HBP%`, `Barrel%`, `GB%`, `CSW%`, `Whiff%`, `IZWhiff%`, `Chase%`,
          `FPS%`,
          `1-1 Win%`, `E&A%`, `Strike%`, `Zone%`, `Pre2k Zone%`, `2k Zone%`, `Put Away%`,
          `Shut Down Inning%`
        ) %>%
        dplyr::arrange(factor(Batter, levels = c("vLHH","vRHH")), Batter)
      
      # TOTAL row for handedness
      make_total_hand <- function(p_all, p_flags_all) {
        
        # ---- pitch-level totals from p_flags_all ----
        n_all <- nrow(p_flags_all)
        
        strike  <- sdiv(sum(p_flags_all$.strike, na.rm = TRUE), n_all)
        zone    <- sdiv(sum(p_flags_all$.inz,    na.rm = TRUE), sum(!is.na(p_flags_all$.inz), na.rm = TRUE))
        
        pre2k_den <- sum(p_flags_all$.pre2k & !is.na(p_flags_all$.inz), na.rm = TRUE)
        pre2k     <- if (pre2k_den > 0) sdiv(sum(p_flags_all$.pre2k & p_flags_all$.inz, na.rm = TRUE), pre2k_den) else NA_real_
        twok_den  <- sum(p_flags_all$.twok_no32 & !is.na(p_flags_all$.inz), na.rm = TRUE)
        twok_zone <- if (twok_den > 0) sdiv(sum(p_flags_all$.twok_no32 & p_flags_all$.inz, na.rm = TRUE), twok_den) else NA_real_
        put_den   <- sum(p_flags_all$.twok, na.rm = TRUE)
        put_away  <- if (put_den > 0) sdiv(sum(p_flags_all$.twok & p_flags_all$.is_k_pitch, na.rm = TRUE), put_den) else NA_real_
        
        swings <- sum(p_flags_all$.swing, na.rm = TRUE)
        whiff  <- sdiv(sum(p_flags_all$.pc == "StrikeSwinging", na.rm = TRUE), swings)
        
        csw    <- sdiv(sum(p_flags_all$.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE), n_all)
        
        iz_sw  <- sum(p_flags_all$.inz & p_flags_all$.swing, na.rm = TRUE)
        izw    <- sdiv(sum(p_flags_all$.inz & (p_flags_all$.pc == "StrikeSwinging"), na.rm = TRUE), iz_sw)
        
        ooz    <- sum(!p_flags_all$.inz, na.rm = TRUE)
        chase  <- sdiv(sum(!p_flags_all$.inz & p_flags_all$.swing, na.rm = TRUE), ooz)
        
        at11_den <- sum(p_flags_all$.at_11, na.rm = TRUE)
        win11p   <- sdiv(sum(p_flags_all$.at_11 & p_flags_all$.win_11, na.rm = TRUE), at11_den)
        
        bipT     <- sum(p_flags_all$.bip,    na.rm = TRUE)
        barT     <- sum(p_flags_all$.barrel & p_flags_all$.barrel_ok, na.rm = TRUE)
        bipT_evla <- sum(p_flags_all$.bip_evla & p_flags_all$.barrel_ok, na.rm = TRUE)
        barrelp  <- sdiv(barT, bipT_evla)
        gbT      <- sum(p_flags_all$.gb,     na.rm = TRUE)
        bipT_la  <- sum(p_flags_all$.bip_la, na.rm = TRUE)
        gbp      <- sdiv(gbT, bipT_la)
        
        # ---- PA-level totals ----
        # Recompute PA last-pitch rows locally to avoid any scope/capture issues
        pa_lastT <- get_pa_last(p_all)
        outc     <- pa_outcome_cols(pa_lastT)
        pr       <- outc$PR_txt
        kb       <- as.character(pa_lastT$KorBB %||% "")
        
        any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
        bb_only <- (any_walk)
        
        outs_play <- if ("OutsOnPlay" %in% names(pa_lastT)) {
          v <- suppressWarnings(as.integer(pa_lastT$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
        } else rep(0L, nrow(pa_lastT))
        outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
        outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
        outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
        
        outs_total <- sum(outs_play, na.rm = TRUE) + sum(as.integer(outc$K), na.rm = TRUE)
        
        # FPS% (PA-level)
        fps_paT <- p_all %>%
          dplyr::group_by(PA_ID) %>%
          dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE), .groups = "drop")
        FPS <- mean(fps_paT$fp_strike, na.rm = TRUE)
        
        # E&A%: every PA is in the denominator; numerator is Ahead or eligible Early action
        ea_paT <- calc_ea_pa_summary(
          p_all %>% dplyr::mutate(.strike = p_flags_all$.strike, .barrel = p_flags_all$.barrel, .hbp = p_flags_all$HBP),
          "PA_ID"
        )
        EA <- calc_ea_rate_from_pa(ea_paT)
        
        # FIP + wOBA overall
        FIP <- compute_fip_xfip_grouped(pa_lastT, grp = "TOTAL")$FIP[1]
        W   <- compute_woba_grouped(pa_lastT,  grp = "TOTAL")
        bat_tot <- summarize_batting(pa_lastT, rep("TOTAL", nrow(pa_lastT)))
        prv_tot <- calc_prv(bat_tot$TB[1], bat_tot$BB[1], bat_tot$K[1], bat_tot$RBI[1], bat_tot$HR[1], nrow(p_all))


        # Totals for wOBA/wOBAcon (compute directly to avoid any drift)
        outc_tot <- pa_outcome_cols(pa_lastT)
        woba_num_tot <- with(outc_tot, woba_weights$BB  * as.numeric(BB)  +
                               woba_weights$HBP * as.numeric(HBP) +
                               woba_weights$X1B * as.numeric(X1B) +
                               woba_weights$X2B * as.numeric(X2B) +
                               woba_weights$X3B * as.numeric(X3B) +
                               woba_weights$HR  * as.numeric(HR))
        woba_den_tot <- with(outc_tot, as.numeric(BB) + as.numeric(HBP) + as.numeric(BIP_pa) + as.numeric(K))
        wobacon_num_tot <- with(outc_tot, woba_weights$X1B * as.numeric(X1B) +
                                  woba_weights$X2B * as.numeric(X2B) +
                                  woba_weights$X3B * as.numeric(X3B) +
                                  woba_weights$HR  * as.numeric(HR))
        wobacon_den_tot <- as.numeric(outc_tot$BIP_pa)
        woba_tot <- if (sum(woba_den_tot, na.rm = TRUE) > 0) sum(woba_num_tot, na.rm = TRUE) / sum(woba_den_tot, na.rm = TRUE) else NA_real_
        wobacon_tot <- if (sum(wobacon_den_tot, na.rm = TRUE) > 0) sum(wobacon_num_tot, na.rm = TRUE) / sum(wobacon_den_tot, na.rm = TRUE) else NA_real_
        
        tibble::tibble(
          Batter       = "TOTAL",
          IP           = sprintf("%d.%d", outs_total %/% 3, outs_total %% 3),
          PA           = nrow(pa_lastT),
          BAA          = ifelse(is.finite(sdiv(bat_tot$H[1], bat_tot$AB[1])), sprintf("%.3f", sdiv(bat_tot$H[1], bat_tot$AB[1])), "NA"),
          wOBA         = ifelse(is.finite(woba_tot),    sprintf("%.3f", woba_tot),    "NA"),
          wOBAcon      = ifelse(is.finite(wobacon_tot), sprintf("%.3f", wobacon_tot), "NA"),
          SLG          = ifelse(is.finite(bat_tot$SLG[1]), sprintf("%.3f", bat_tot$SLG[1]), "NA"),
          OPS          = ifelse(is.finite(bat_tot$OPS[1]), sprintf("%.3f", bat_tot$OPS[1]), "NA"),
          WHIP         = ifelse(is.finite(sdiv(bat_tot$BB[1] + bat_tot$H[1], outs_total / 3)), sprintf("%.2f", sdiv(bat_tot$BB[1] + bat_tot$H[1], outs_total / 3)), "NA"),
          `K/9`        = ifelse(is.finite(sdiv(bat_tot$K[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$K[1] * 9, outs_total / 3)), "NA"),
          `BB/9`       = ifelse(is.finite(sdiv(bat_tot$BB[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$BB[1] * 9, outs_total / 3)), "NA"),
          `H/9`        = ifelse(is.finite(sdiv(bat_tot$H[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$H[1] * 9, outs_total / 3)), "NA"),
          pRV          = ifelse(is.finite(prv_tot), sprintf("%.2f", prv_tot), "NA"),
          `K%`         = sprintf("%.0f%%", 100*mean(outc$K, na.rm = TRUE)),
          `BB%`        = sprintf("%.0f%%", 100*mean(bb_only, na.rm = TRUE)),
          `BB+HBP%`    = ifelse(is.finite(sdiv(bat_tot$BB[1] + bat_tot$HBP[1], nrow(pa_lastT))), sprintf("%.0f%%", 100*sdiv(bat_tot$BB[1] + bat_tot$HBP[1], nrow(pa_lastT))), "NA"),
          `Barrel%`    = ifelse(is.finite(barrelp), sprintf("%.0f%%", 100*barrelp), "NA"),
          `GB%`        = ifelse(is.finite(gbp),     sprintf("%.0f%%", 100*gbp),     "NA"),
          FIP          = ifelse(is.finite(FIP), sprintf("%.2f", FIP), "NA"),
          `FPS%`       = sprintf("%.0f%%", 100*FPS),
          `1-1 Win%`   = ifelse(is.finite(win11p), sprintf("%.0f%%", 100*win11p), "NA"),
          `E&A%`       = ifelse(is.finite(EA), sprintf("%.0f%%", 100*EA), "NA"),
          `Strike%`    = ifelse(is.finite(strike), sprintf("%.0f%%", 100*strike), "NA"),
          `Zone%`      = ifelse(is.finite(zone),   sprintf("%.0f%%", 100*zone),   "NA"),
          `Pre2k Zone%`= ifelse(is.finite(pre2k),  sprintf("%.0f%%", 100*pre2k),  "NA"),
          `2k Zone%`   = ifelse(is.finite(twok_zone), sprintf("%.0f%%", 100*twok_zone), "NA"),
          `Put Away%`  = ifelse(is.finite(put_away),  sprintf("%.0f%%", 100*put_away),  "NA"),
          `Whiff%`     = ifelse(is.finite(whiff),  sprintf("%.0f%%", 100*whiff),  "NA"),
          `CSW%`       = ifelse(is.finite(csw),    sprintf("%.0f%%", 100*csw),    "NA"),
          `IZWhiff%`   = ifelse(is.finite(izw),    sprintf("%.0f%%", 100*izw),    "NA"),
          `Chase%`     = ifelse(is.finite(chase),  sprintf("%.0f%%", 100*chase),  "NA"),
          `Shut Down Inning%` = sd_fmt
        )
      }
      
      total_row <- make_total_hand(p, p_flags)
      out_df <- dplyr::bind_rows(tab, total_row)
      
    } else {
      # Pitch-type table (per-pitch metrics + PA metrics grouped by terminal pitch type)
      tab <- pitch_rates %>%
        dplyr::left_join(pa_rates, by = "grp") %>%
        dplyr::left_join(pa_bat_small, by = "grp") %>%
        dplyr::left_join(fip,      by = "grp") %>%
        dplyr::left_join(wob,      by = "grp") %>%
        dplyr::mutate(
          `Pitch type`  = grp,
          `Pitches (#)` = Pitches,
          BAA            = ifelse(is.finite(sdiv(H, AB)), sprintf("%.3f", sdiv(H, AB)), "NA"),
          WHIP           = ifelse(is.finite(sdiv(BB + H, Outs / 3)), sprintf("%.2f", sdiv(BB + H, Outs / 3)), "NA"),
          `K/9`          = ifelse(is.finite(sdiv(K * 9, Outs / 3)), sprintf("%.1f", sdiv(K * 9, Outs / 3)), "NA"),
          `BB/9`         = ifelse(is.finite(sdiv(BB * 9, Outs / 3)), sprintf("%.1f", sdiv(BB * 9, Outs / 3)), "NA"),
          `H/9`          = ifelse(is.finite(sdiv(H * 9, Outs / 3)), sprintf("%.1f", sdiv(H * 9, Outs / 3)), "NA"),
          `K%`           = ifelse(is.finite(Kp), sprintf("%.0f%%", 100*Kp), "NA"),
          `BB%`          = ifelse(is.finite(BBp), sprintf("%.0f%%", 100*BBp), "NA"),
          `BB+HBP%`      = ifelse(is.finite(sdiv(BB + HBP, PA)), sprintf("%.0f%%", 100*sdiv(BB + HBP, PA)), "NA"),
          `Barrel%`     = ifelse(is.finite(Barrel_pct), sprintf("%.0f%%", 100*Barrel_pct), "NA"),
          `GB%`         = ifelse(is.finite(GB_pct),     sprintf("%.0f%%", 100*GB_pct),     "NA"),
          `FPS%`        = sprintf("%.0f%%", 100*FPS),
          `1-1 Win%`    = ifelse(is.finite(Win11_pct),  sprintf("%.0f%%", 100*Win11_pct),  "NA"),
          `Strike%`     = sprintf("%.0f%%", 100*Strike_pct),
          `Zone%`       = sprintf("%.0f%%", 100*Zone_pct),
          `Pre2k Zone%` = sprintf("%.0f%%", 100*Pre2kZone_pct),
          `2k Zone%`    = ifelse(is.finite(TwoKZone_pct), sprintf("%.0f%%", 100*TwoKZone_pct), "NA"),
          `Put Away%`   = ifelse(is.finite(PutAway_pct),  sprintf("%.0f%%", 100*PutAway_pct),  "NA"),
          `Whiff%`      = sprintf("%.0f%%", 100*Whiff_pct),
          `CSW%`        = sprintf("%.0f%%", 100*CSW_pct),
          `IZWhiff%`    = sprintf("%.0f%%", 100*IZWhiff_pct),
          `Chase%`      = sprintf("%.0f%%", 100*Chase_pct),
          `Shut Down Inning%` = sd_fmt,
          FIP           = ifelse(is.finite(FIP), sprintf("%.2f", FIP), "NA"),
          wOBA          = ifelse(is.finite(wOBA),    sprintf("%.3f", wOBA),    "NA"),
          wOBAcon       = ifelse(is.finite(wOBAcon), sprintf("%.3f", wOBAcon), "NA"),
          SLG           = ifelse(is.finite(SLG),     sprintf("%.3f", SLG),     "NA"),
          OPS           = ifelse(is.finite(OPS),     sprintf("%.3f", OPS),     "NA"),
          pRV           = ifelse(is.finite(calc_prv(TB, BB, K, RBI, HR, Pitches)),
                                 sprintf("%.2f", calc_prv(TB, BB, K, RBI, HR, Pitches)), "NA")
        ) %>%
        dplyr::select(
          `Pitch type`, `Pitches (#)`, BAA, SLG, OPS, WHIP, `K/9`, `BB/9`, `H/9`,
          wOBA, wOBAcon, pRV, FIP, `K%`, `BB%`, `BB+HBP%`, `Barrel%`, `GB%`, `CSW%`, `Whiff%`, `IZWhiff%`, `Chase%`,
          `FPS%`, `1-1 Win%`, `Strike%`, `Zone%`, `Pre2k Zone%`, `2k Zone%`, `Put Away%`,
          `Shut Down Inning%`
        ) %>%
        dplyr::arrange(dplyr::desc(`Pitches (#)`))
      
      # TOTAL row for pitch types (overall)
      make_total_pt <- function(p_all, p_flags_all) {
        # Use robust, precomputed flags for totals (consistent with row stats)
        n_all <- nrow(p_flags_all)
        
        strike  <- sdiv(sum(p_flags_all$.strike, na.rm = TRUE), n_all)
        zone    <- sdiv(sum(p_flags_all$.inz,    na.rm = TRUE), sum(!is.na(p_flags_all$.inz), na.rm = TRUE))
        
        pre2k_den <- sum(p_flags_all$.pre2k & !is.na(p_flags_all$.inz), na.rm = TRUE)
        pre2k     <- if (pre2k_den > 0) sdiv(sum(p_flags_all$.pre2k & p_flags_all$.inz, na.rm = TRUE), pre2k_den) else NA_real_
        twok_den  <- sum(p_flags_all$.twok_no32 & !is.na(p_flags_all$.inz), na.rm = TRUE)
        twok_zone <- if (twok_den > 0) sdiv(sum(p_flags_all$.twok_no32 & p_flags_all$.inz, na.rm = TRUE), twok_den) else NA_real_
        put_den   <- sum(p_flags_all$.twok, na.rm = TRUE)
        put_away  <- if (put_den > 0) sdiv(sum(p_flags_all$.twok & p_flags_all$.is_k_pitch, na.rm = TRUE), put_den) else NA_real_
        
        swings <- sum(p_flags_all$.swing, na.rm = TRUE)
        whiff  <- sdiv(sum(p_flags_all$.pc == "StrikeSwinging", na.rm = TRUE), swings)
        
        csw    <- sdiv(sum(p_flags_all$.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE), n_all)
        
        iz_sw  <- sum(p_flags_all$.inz & p_flags_all$.swing, na.rm = TRUE)
        izw    <- sdiv(sum(p_flags_all$.inz & (p_flags_all$.pc == "StrikeSwinging"), na.rm = TRUE), iz_sw)
        
        ooz    <- sum(!p_flags_all$.inz, na.rm = TRUE)
        chase  <- sdiv(sum(!p_flags_all$.inz & p_flags_all$.swing, na.rm = TRUE), ooz)
        
        at11_den <- sum(p_flags_all$.at_11, na.rm = TRUE)
        win11p   <- sdiv(sum(p_flags_all$.at_11 & p_flags_all$.win_11, na.rm = TRUE), at11_den)
        
        bipT     <- sum(p_flags_all$.bip,    na.rm = TRUE)
        barT     <- sum(p_flags_all$.barrel & p_flags_all$.barrel_ok, na.rm = TRUE)
        bipT_evla <- sum(p_flags_all$.bip_evla & p_flags_all$.barrel_ok, na.rm = TRUE)
        barrelp  <- sdiv(barT, bipT_evla)
        gbT      <- sum(p_flags_all$.gb,     na.rm = TRUE)
        bipT_la  <- sum(p_flags_all$.bip_la, na.rm = TRUE)
        gbp      <- sdiv(gbT, bipT_la)
        
        # PA totals (recompute locally)
        pa_lastT <- get_pa_last(p_all)
        outc_ip <- pa_outcome_cols(pa_lastT)
        pr_ip <- outc_ip$PR_txt
        outs_play_ip <- if ("OutsOnPlay" %in% names(pa_lastT)) {
          v <- suppressWarnings(as.integer(pa_lastT$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
        } else rep(0L, nrow(pa_lastT))
        outs_play_ip <- ifelse(grepl("(?i)triple ?play", pr_ip), 3L, outs_play_ip)
        outs_play_ip <- ifelse(grepl("(?i)double ?play", pr_ip), 2L, outs_play_ip)
        outs_play_ip <- ifelse(grepl("(?i)\\bout\\b", pr_ip) & !outc_ip$K, pmax(outs_play_ip, 1L), outs_play_ip)
        outs_total <- sum(outs_play_ip, na.rm = TRUE) + sum(as.integer(outc_ip$K), na.rm = TRUE)
        
        # FPS% (PA-level)
        fps_paT <- p_all %>%
          dplyr::group_by(PA_ID) %>%
          dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE), .groups = "drop")
        FPS <- mean(fps_paT$fp_strike, na.rm = TRUE)
        
        # PA totals for terminal pitch type = overall (same as hand total but no IP/PA needed here)
        # Attribute PA-level FIP & wOBA overall:
        FIP <- compute_fip_xfip_grouped(pa_lastT, grp = "TOTAL")$FIP[1]
        W   <- compute_woba_grouped(pa_lastT,    grp = "TOTAL")
        bat_tot <- summarize_batting(pa_lastT, rep("TOTAL", nrow(pa_lastT)))
        prv_tot <- calc_prv(bat_tot$TB[1], bat_tot$BB[1], bat_tot$K[1], bat_tot$RBI[1], bat_tot$HR[1], nrow(p_all))


        # Totals for wOBA/wOBAcon (compute directly to avoid any drift)
        outc_tot <- pa_outcome_cols(pa_lastT)
        woba_num_tot <- with(outc_tot, woba_weights$BB  * as.numeric(BB)  +
                               woba_weights$HBP * as.numeric(HBP) +
                               woba_weights$X1B * as.numeric(X1B) +
                               woba_weights$X2B * as.numeric(X2B) +
                               woba_weights$X3B * as.numeric(X3B) +
                               woba_weights$HR  * as.numeric(HR))
        woba_den_tot <- with(outc_tot, as.numeric(BB) + as.numeric(HBP) + as.numeric(BIP_pa) + as.numeric(K))
        wobacon_num_tot <- with(outc_tot, woba_weights$X1B * as.numeric(X1B) +
                                  woba_weights$X2B * as.numeric(X2B) +
                                  woba_weights$X3B * as.numeric(X3B) +
                                  woba_weights$HR  * as.numeric(HR))
        wobacon_den_tot <- as.numeric(outc_tot$BIP_pa)
        woba_tot <- if (sum(woba_den_tot, na.rm = TRUE) > 0) sum(woba_num_tot, na.rm = TRUE) / sum(woba_den_tot, na.rm = TRUE) else NA_real_
        wobacon_tot <- if (sum(wobacon_den_tot, na.rm = TRUE) > 0) sum(wobacon_num_tot, na.rm = TRUE) / sum(wobacon_den_tot, na.rm = TRUE) else NA_real_
        
        tibble::tibble(
          `Pitch type`  = "TOTAL",
          `Pitches (#)` = nrow(p_all),
          BAA            = ifelse(is.finite(sdiv(bat_tot$H[1], bat_tot$AB[1])), sprintf("%.3f", sdiv(bat_tot$H[1], bat_tot$AB[1])), "NA"),
          WHIP           = ifelse(is.finite(sdiv(bat_tot$BB[1] + bat_tot$H[1], outs_total / 3)), sprintf("%.2f", sdiv(bat_tot$BB[1] + bat_tot$H[1], outs_total / 3)), "NA"),
          `K/9`          = ifelse(is.finite(sdiv(bat_tot$K[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$K[1] * 9, outs_total / 3)), "NA"),
          `BB/9`         = ifelse(is.finite(sdiv(bat_tot$BB[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$BB[1] * 9, outs_total / 3)), "NA"),
          `H/9`          = ifelse(is.finite(sdiv(bat_tot$H[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(bat_tot$H[1] * 9, outs_total / 3)), "NA"),
          `K%`           = ifelse(is.finite(sdiv(bat_tot$K[1], nrow(pa_lastT))), sprintf("%.0f%%", 100*sdiv(bat_tot$K[1], nrow(pa_lastT))), "NA"),
          `BB%`          = ifelse(is.finite(sdiv(bat_tot$BB[1], nrow(pa_lastT))), sprintf("%.0f%%", 100*sdiv(bat_tot$BB[1], nrow(pa_lastT))), "NA"),
          `BB+HBP%`      = ifelse(is.finite(sdiv(bat_tot$BB[1] + bat_tot$HBP[1], nrow(pa_lastT))), sprintf("%.0f%%", 100*sdiv(bat_tot$BB[1] + bat_tot$HBP[1], nrow(pa_lastT))), "NA"),
          `Barrel%`     = ifelse(is.finite(barrelp), sprintf("%.0f%%", 100*barrelp), "NA"),
          `GB%`         = ifelse(is.finite(gbp),     sprintf("%.0f%%", 100*gbp),     "NA"),
          FIP           = ifelse(is.finite(FIP), sprintf("%.2f", FIP), "NA"),
          `FPS%`        = sprintf("%.0f%%", 100 * FPS),
          `1-1 Win%`    = ifelse(is.finite(win11p), sprintf("%.0f%%", 100*win11p), "NA"),
          `Strike%`     = sprintf("%.0f%%", 100*strike),
          `Zone%`       = sprintf("%.0f%%", 100*zone),
          `Pre2k Zone%` = sprintf("%.0f%%", 100*pre2k),
          `2k Zone%`    = ifelse(is.finite(twok_zone), sprintf("%.0f%%", 100*twok_zone), "NA"),
          `Put Away%`   = ifelse(is.finite(put_away),  sprintf("%.0f%%", 100*put_away),  "NA"),
          `Whiff%`      = sprintf("%.0f%%", 100*whiff),
          `CSW%`        = sprintf("%.0f%%", 100*csw),
          `IZWhiff%`    = sprintf("%.0f%%", 100*izw),
          `Chase%`      = sprintf("%.0f%%", 100*chase),
          `Shut Down Inning%` = sd_fmt,
          wOBA          = ifelse(is.finite(woba_tot),    sprintf("%.3f", woba_tot),    "NA"),
          wOBAcon       = ifelse(is.finite(wobacon_tot), sprintf("%.3f", wobacon_tot), "NA"),
          SLG           = ifelse(is.finite(bat_tot$SLG[1]), sprintf("%.3f", bat_tot$SLG[1]), "NA"),
          OPS           = ifelse(is.finite(bat_tot$OPS[1]), sprintf("%.3f", bat_tot$OPS[1]), "NA"),
          pRV           = ifelse(is.finite(prv_tot), sprintf("%.2f", prv_tot), "NA")
        )
      }
      
      total_row <- make_total_pt(p, p_flags)
      out_df <- dplyr::bind_rows(tab, total_row)
    }
    
    # =========================
    # SHADING
    # =========================
    shade_cols <- c("BAA","SLG","OPS","WHIP","K/9","BB/9","H/9",
                    "pRV","FIP","wOBA","wOBAcon","K%","BB%","BB+HBP%",
                    "Barrel%","GB%","CSW%","Whiff%","IZWhiff%","Chase%",
                    "Strike%","Zone%","FPS%","E&A%","Pre2k Zone%","2k Zone%",
                    "Put Away%","Shut Down Inning%","1-1 Win%")
    out_df_shaded <- shade_columns_txst(
      out_df,
      cols = intersect(shade_cols, names(out_df)),
      lower_better = intersect(c("BAA","SLG","OPS","WHIP","BB/9","H/9",
                                 "pRV","FIP","wOBA","wOBAcon","BB%","BB+HBP%","Barrel%"), names(out_df)),
      percent_cols = intersect(c("K%","BB%","BB+HBP%","Barrel%","GB%","1-1 Win%","Strike%","Zone%","Pre2k Zone%","2k Zone%","Put Away%",
                                 "Shut Down Inning%","Whiff%","CSW%","IZWhiff%","Chase%","FPS%","E&A%"), names(out_df))
    )
    
    
    attr(out_df_shaded, "raw_df") <- out_df
    out_df_shaded
  })

  performance_percentile_specs <- tibble::tribble(
    ~label, ~metric, ~kind, ~lower_better,
    "BAA", "performance_baa", "dec3", TRUE,
    "SLG", "performance_slg", "dec3", TRUE,
    "OPS", "performance_ops", "dec3", TRUE,
    "WHIP", "performance_whip", "num2", TRUE,
    "K/9", "performance_k9", "num1", FALSE,
    "BB/9", "performance_bb9", "num1", TRUE,
    "H/9", "performance_h9", "num1", TRUE,
    "FIP", "performance_fip", "num2", TRUE,
    "wOBA", "performance_woba", "dec3", TRUE,
    "wOBAcon", "performance_wobacon", "dec3", TRUE,
    "K%", "performance_k_pct", "pct", FALSE,
    "BB%", "performance_bb_pct", "pct", TRUE,
    "BB+HBP%", "performance_bb_hbp_pct", "pct", TRUE,
    "Barrel%", "performance_barrel_pct", "pct", TRUE,
    "GB%", "performance_gb_pct", "pct", FALSE,
    "Strike%", "performance_strike_pct", "pct", FALSE,
    "Zone%", "performance_zone_pct", "pct", FALSE,
    "FPS%", "performance_fps_pct", "pct", FALSE,
    "E&A%", "performance_ea_pct", "pct", FALSE,
    "Pre2k Zone%", "performance_pre2k_zone_pct", "pct", FALSE,
    "2k Zone%", "performance_2k_zone_pct", "pct", FALSE,
    "Put Away%", "performance_put_away_pct", "pct", FALSE,
    "Shut Down Inning%", "performance_shutdown_pct", "pct", FALSE,
    "1-1 Win%", "performance_win11_pct", "pct", FALSE,
    "CSW%", "performance_csw_pct", "pct", FALSE,
    "Whiff%", "performance_whiff_pct", "pct", FALSE,
    "IZWhiff%", "performance_izwhiff_pct", "pct", FALSE,
    "Chase%", "performance_chase_pct", "pct", FALSE
  )

  staff_percentile_specs <- tibble::tribble(
    ~label, ~metric, ~kind, ~lower_better,
    "BAA", "performance_baa", "dec3", TRUE,
    "SLG", "performance_slg", "dec3", TRUE,
    "OPS", "performance_ops", "dec3", TRUE,
    "WHIP", "performance_whip", "num2", TRUE,
    "K/9", "performance_k9", "num1", FALSE,
    "BB/9", "performance_bb9", "num1", TRUE,
    "H/9", "performance_h9", "num1", TRUE,
    "FIP", "performance_fip", "num2", TRUE,
    "wOBA", "performance_woba", "dec3", TRUE,
    "wOBAcon", "performance_wobacon", "dec3", TRUE,
    "K%", "performance_k_pct", "pct", FALSE,
    "BB%", "performance_bb_pct", "pct", TRUE,
    "BB+HBP%", "performance_bb_hbp_pct", "pct", TRUE,
    "Barrel%", "performance_barrel_pct", "pct", TRUE,
    "GB%", "performance_gb_pct", "pct", FALSE,
    "pRV", "performance_prv", "num2", TRUE,
    "CSW%", "performance_csw_pct", "pct", FALSE,
    "Whiff%", "performance_whiff_pct", "pct", FALSE,
    "IZWhiff%", "performance_izwhiff_pct", "pct", FALSE,
    "Chase%", "performance_chase_pct", "pct", FALSE,
    "Strike%", "performance_strike_pct", "pct", FALSE,
    "Zone%", "performance_zone_pct", "pct", FALSE,
    "FPS%", "performance_fps_pct", "pct", FALSE,
    "E&A%", "performance_ea_pct", "pct", FALSE,
    "Pre2k Zone%", "performance_pre2k_zone_pct", "pct", FALSE,
    "2k Zone%", "performance_2k_zone_pct", "pct", FALSE,
    "Put Away%", "performance_put_away_pct", "pct", FALSE,
    "Shut Down Inning%", "performance_shutdown_pct", "pct", FALSE,
    "1-1 Win%", "performance_win11_pct", "pct", FALSE,
    "Heater Velocity", "heater_velocity", "num1", FALSE,
    "Heater Extension", "extension", "num1", FALSE
  )

  staff_percentile_values <- function(d) {
    if (is.null(d) || !nrow(d)) return(tibble::tibble(label = character(), value = double()))
    p <- prepare_aar_flags(d)
    if ("pitch_uid" %in% names(p)) p <- p %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id" %in% names(p)) p <- p %>% dplyr::distinct(row_id, .keep_all = TRUE)
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    if (!"Pitcher" %in% names(p)) p$Pitcher <- "Staff"

    game_key <- if ("CustomGameID" %in% names(p)) {
      as.character(p$CustomGameID)
    } else if ("GameID" %in% names(p)) {
      as.character(p$GameID)
    } else if ("Game" %in% names(p)) {
      as.character(p$Game)
    } else if ("Date" %in% names(p)) {
      as.character(p$Date)
    } else {
      rep("Game", nrow(p))
    }
    game_key[is.na(game_key) | !nzchar(game_key)] <- "Game"
    p$PA_ID_STAFF <- interaction(game_key, as.character(p$Pitcher), p$PA_ID, drop = TRUE)

    first_present <- function(df, ...) {
      cands <- c(...)
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[1] else NA_character_
    }
    get_ev_la_local <- function(df) {
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present(df, "ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present(df, "Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    evla <- get_ev_la_local(p)
    evn <- evla$ev
    lan <- evla$la
    pc_vec <- as.character(p$PitchCall %||% "")
    bip_vec <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX
    is_k_pitch_vec <- calc_is_k_pitch(p, "PA_ID_STAFF", "PitchNum")

    p_flags <- p %>%
      dplyr::mutate(
        .pc = pc_vec,
        .bip = bip_vec,
        .ev_ok = is.finite(evn),
        .la_ok = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb = .bip & is.finite(lan) & lan < 5,
        .inz = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),
        .pre2k = !is.na(StrikesPre) & StrikesPre < 2,
        .twok = !is.na(StrikesPre) & StrikesPre == 2,
        .twok_no32 = (!is.na(StrikesPre) & StrikesPre == 2) & (!is.na(BallsPre) & BallsPre != 3),
        .at_11 = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )
    p_flags$PitchTypeMetric <- pitch_metric_pitch_type_vec(p_flags)

    n_all <- nrow(p_flags)
    strike <- sdiv(sum(p_flags$.strike, na.rm = TRUE), n_all)
    zone <- sdiv(sum(p_flags$.inz, na.rm = TRUE), sum(!is.na(p_flags$.inz), na.rm = TRUE))
    pre2k <- sdiv(sum(p_flags$.pre2k & p_flags$.inz, na.rm = TRUE), sum(p_flags$.pre2k & !is.na(p_flags$.inz), na.rm = TRUE))
    twok_zone <- sdiv(sum(p_flags$.twok_no32 & p_flags$.inz, na.rm = TRUE), sum(p_flags$.twok_no32 & !is.na(p_flags$.inz), na.rm = TRUE))
    put_away <- sdiv(sum(p_flags$.twok & p_flags$.is_k_pitch, na.rm = TRUE), sum(p_flags$.twok, na.rm = TRUE))
    whiff <- sdiv(sum(p_flags$.pc == "StrikeSwinging", na.rm = TRUE), sum(p_flags$.swing, na.rm = TRUE))
    csw <- sdiv(sum(p_flags$.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE), n_all)
    izwhiff <- sdiv(sum(p_flags$.inz & p_flags$.pc == "StrikeSwinging", na.rm = TRUE), sum(p_flags$.inz & p_flags$.swing, na.rm = TRUE))
    chase <- sdiv(sum(!p_flags$.inz & p_flags$.swing, na.rm = TRUE), sum(!p_flags$.inz, na.rm = TRUE))
    barrel <- sdiv(sum(p_flags$.barrel & p_flags$.barrel_ok, na.rm = TRUE), sum(p_flags$.bip_evla & p_flags$.barrel_ok, na.rm = TRUE))
    gb <- sdiv(sum(p_flags$.gb, na.rm = TRUE), sum(p_flags$.bip_la, na.rm = TRUE))
    win11 <- sdiv(sum(p_flags$.at_11 & p_flags$.win_11, na.rm = TRUE), sum(p_flags$.at_11, na.rm = TRUE))

    pa_last <- p_flags %>%
      dplyr::group_by(PA_ID_STAFF) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    outc <- pa_outcome_cols(pa_last)
    pr <- outc$PR_txt
    kb <- as.character(pa_last$KorBB %||% "")
    any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
    pa_n <- nrow(pa_last)
    k <- sum(as.numeric(outc$K), na.rm = TRUE)
    bb <- sum(as.numeric(any_walk), na.rm = TRUE)
    hbp <- sum(as.numeric(outc$HBP), na.rm = TRUE)
    hr <- sum(as.numeric(outc$HR), na.rm = TRUE)
    h <- sum(as.numeric(outc$X1B | outc$X2B | outc$X3B | outc$HR), na.rm = TRUE)
    tb <- sum(as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR), na.rm = TRUE)
    sf <- sum(as.numeric(grepl("(?i)sacrifice fly|\\bsf\\b", pr)), na.rm = TRUE)
    ab <- max(pa_n - bb - hbp - sf, 0)

    outs_play <- if ("OutsOnPlay" %in% names(pa_last)) {
      v <- suppressWarnings(as.integer(pa_last$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
    } else rep(0L, pa_n)
    outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
    outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
    outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
    outs_total <- sum(outs_play, na.rm = TRUE) + k
    ip <- outs_total / 3

    wob <- compute_woba_grouped(pa_last, rep("STAFF", pa_n))
    fip <- compute_fip_xfip_grouped(pa_last, rep("STAFF", pa_n))$FIP[1]
    woba <- wob$wOBA[1]
    wobacon <- wob$wOBAcon[1]

    fps_pa <- p_flags %>%
      dplyr::group_by(PA_ID_STAFF) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE), .groups = "drop")
    fps <- sdiv(sum(fps_pa$fp_strike, na.rm = TRUE), nrow(fps_pa))
    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID_STAFF")
    ea <- calc_ea_rate_from_pa(ea_pa)

    sd_tbl <- compute_shutdown_by_pitcher(p, d_all = df_nonbp)
    sd_opp <- sum(sd_tbl$ShutDownOpp, na.rm = TRUE)
    sd_success <- sum(sd_tbl$ShutDown, na.rm = TRUE)
    shutdown <- if (sd_opp > 0) sdiv(sd_success, sd_opp) else NA_real_

    rbi <- if ("RunsScored" %in% names(pa_last)) {
      r <- suppressWarnings(as.numeric(pa_last$RunsScored))
      sum(ifelse(outc$BIP_pa %in% TRUE, dplyr::coalesce(r, 0), 0), na.rm = TRUE)
    } else {
      NA_real_
    }
    prv <- if (is.finite(rbi)) sdiv((((tb + bb - k) / 4) + rbi + hr), n_all) * 100 else NA_real_

    heater <- p_flags %>%
      dplyr::filter(.data$PitchTypeMetric %in% c("Fastball", "Sinker"))
    heater_velocity <- mean(suppressWarnings(as.numeric(heater$RelSpeed)), na.rm = TRUE)
    heater_extension <- mean(suppressWarnings(as.numeric(heater$Extension)), na.rm = TRUE)
    if (!is.finite(heater_velocity)) heater_velocity <- NA_real_
    if (!is.finite(heater_extension)) heater_extension <- NA_real_

    tibble::tibble(
      label = c(
        "BAA","SLG","OPS","WHIP","K/9","BB/9","H/9",
        "FIP","wOBA","wOBAcon","K%","BB%","BB+HBP%","Barrel%","GB%",
        "pRV","CSW%","Whiff%","IZWhiff%","Chase%",
        "Strike%","Zone%","FPS%","E&A%","Pre2k Zone%","2k Zone%","Put Away%","Shut Down Inning%","1-1 Win%",
        "Heater Velocity","Heater Extension"
      ),
      value = c(
        sdiv(h, ab),
        sdiv(tb, ab),
        {
          obp <- sdiv(h + bb + hbp, ab + bb + hbp + sf)
          slg <- sdiv(tb, ab)
          if (is.finite(obp) & is.finite(slg)) obp + slg else NA_real_
        },
        sdiv(bb + h, ip),
        sdiv(k * 9, ip),
        sdiv(bb * 9, ip),
        sdiv(h * 9, ip),
        fip,
        woba,
        wobacon,
        sdiv(k, pa_n),
        sdiv(bb, pa_n),
        sdiv(bb + hbp, pa_n),
        barrel,
        gb,
        prv,
        csw,
        whiff,
        izwhiff,
        chase,
        strike,
        zone,
        fps,
        ea,
        pre2k,
        twok_zone,
        put_away,
        shutdown,
        win11,
        heater_velocity,
        heater_extension
      )
    )
  }

  team_staff_percentile_data <- reactive({
    vals <- staff_percentile_values(team_trends_data())
    if (is.null(vals) || !nrow(vals)) return(tibble::tibble())
    staff_percentile_specs %>%
      dplyr::left_join(vals, by = "label") %>%
      dplyr::mutate(
        percentile = mapply(
          function(value, metric, lower_better) pitch_metric_percentile(value, metric, lower_better = lower_better),
          .data$value, .data$metric, .data$lower_better
        )
      )
  })

  output$team_staff_percentiles <- renderUI({
    rows <- team_staff_percentile_data()
    if (is.null(rows) || !nrow(rows)) {
      return(div(
        class = "cr-percentile-card staff-percentile-card",
        div(class = "cr-percentile-header",
            div(class = "cr-percentile-title", "Staff Percentiles"),
            div(class = "cr-percentile-subtitle", "Team Trends")
        ),
        div(class = "cr-percentile-empty", "No percentile data")
      ))
    }
    div(
      class = "cr-percentile-card staff-percentile-card",
      div(class = "cr-percentile-header",
          div(class = "cr-percentile-title", "Staff Percentiles"),
          div(class = "cr-percentile-subtitle", "Team Trends")
      ),
      div(class = "cr-percentile-scale", span("Poor"), span("Avg"), span("Great")),
      div(
        class = "cr-percentile-rows",
        Map(
          function(label, value, percentile, kind) pitch_metric_row_ui(label, value, percentile, kind),
          rows$label, rows$value, rows$percentile, rows$kind
        )
      )
    )
  })

  performance_cell_value <- function(x, kind = "num1") {
    if (length(x) == 0L || is.null(x) || is.na(x[1])) return(NA_real_)
    txt <- trimws(gsub("<[^>]*>", "", as.character(x[1])))
    if (!nzchar(txt) || toupper(txt) %in% c("NA", "NAN", "--")) return(NA_real_)
    val <- suppressWarnings(readr::parse_number(txt))
    if (!is.finite(val)) return(NA_real_)
    if (identical(kind, "pct")) val / 100 else val
  }

  performance_percentile_data <- reactive({
    out_df <- performance_table_data()
    raw_df <- attr(out_df, "raw_df")
    if (is.null(raw_df) || !nrow(raw_df)) raw_df <- out_df
    if (is.null(raw_df) || !nrow(raw_df)) return(tibble::tibble())

    label_col <- intersect(c("Batter", "Pitch type"), names(raw_df))[1]
    total_row <- if (!is.na(label_col)) {
      raw_df[trimws(as.character(raw_df[[label_col]])) == "TOTAL", , drop = FALSE]
    } else {
      raw_df[0, , drop = FALSE]
    }
    if (!nrow(total_row)) total_row <- raw_df[nrow(raw_df), , drop = FALSE]

    specs <- performance_percentile_specs %>%
      dplyr::filter(.data$label %in% names(total_row)) %>%
      dplyr::mutate(
        value = mapply(
          function(label, kind) performance_cell_value(total_row[[label]], kind),
          .data$label, .data$kind
        ),
        percentile = mapply(
          function(value, metric, lower_better) pitch_metric_percentile(value, metric, lower_better = lower_better),
          .data$value, .data$metric, .data$lower_better
        )
      )
    specs
  })

  output$performance_percentiles <- renderUI({
    rows <- performance_percentile_data()
    if (is.null(rows) || !nrow(rows)) {
      return(div(
        class = "cr-percentile-card performance-percentile-card",
        div(class = "cr-percentile-header",
            div(class = "cr-percentile-title", "D1 Performance Percentiles"),
            div(class = "cr-percentile-subtitle", input$PitcherInput %||% "")
        ),
        div(class = "cr-percentile-empty", "No percentile data")
      ))
    }
    div(
      class = "cr-percentile-card performance-percentile-card",
      div(class = "cr-percentile-header",
          div(class = "cr-percentile-title", "D1 Performance Percentiles"),
          div(class = "cr-percentile-subtitle", input$PitcherInput %||% "")
      ),
      div(class = "cr-percentile-scale", span("Poor"), span("Avg"), span("Great")),
      div(
        class = "cr-percentile-rows",
        Map(
          function(label, value, percentile, kind) pitch_metric_row_ui(label, value, percentile, kind),
          rows$label, rows$value, rows$percentile, rows$kind
        )
      )
    )
  })

  render_performance_slice <- function(metric_cols) {
    out_df <- performance_table_data()
    if (is.null(out_df) || !nrow(out_df)) out_df <- data.frame(Status = "No data")
    id_cols <- intersect(c("Batter","IP","PA","Pitch type","Pitches (#)"), names(out_df))
    keep <- unique(c(id_cols, intersect(metric_cols, names(out_df))))
    if (!length(keep)) keep <- names(out_df)
    DT::datatable(
      out_df[, keep, drop = FALSE],
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = 't',
        paging   = FALSE,
        ordering = FALSE,
        stripe   = TRUE,
        scrollX  = TRUE,
        autoWidth = TRUE
      ),
      class = "stripe"
    )
  }

  output$performance_traditional_table <- DT::renderDT({
    render_performance_slice(c("BAA","SLG","OPS","WHIP","K/9","BB/9","H/9"))
  })

  output$performance_modern_table <- DT::renderDT({
    render_performance_slice(c("FIP","wOBA","wOBAcon","K%","BB%","BB+HBP%","Barrel%","GB%"))
  })

  output$performance_results_table <- DT::renderDT({
    render_performance_slice(c("pRV","CSW%","Whiff%","IZWhiff%","Chase%"))
  })

  output$performance_process_table <- DT::renderDT({
    render_performance_slice(c("Strike%","Zone%","FPS%","E&A%","Pre2k Zone%","2k Zone%","Put Away%","Shut Down Inning%","1-1 Win%"))
  })

  output$performance_table <- DT::renderDT({
    render_performance_slice(names(performance_table_data()))
  })

  leaderboard_totals_cache <- reactiveVal(NULL)

  # ======== Leaderboard Table ========
  output$leaderboard_table <- DT::renderDT({
    d <- leaderboard_data()
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    p <- prepare_aar_flags(d)

    if (!"Pitcher" %in% names(p)) {
      return(DT::datatable(data.frame(Status = "Pitcher column not found"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    p$grp <- as.character(p$Pitcher)
    p <- p %>% dplyr::filter(!is.na(.data$grp) & nzchar(.data$grp))
    if (!nrow(p)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }
    if (!"PA_ID" %in% names(p)) p <- ensure_pa(p)
    if (!"PA_ID_LB" %in% names(p)) p$PA_ID_LB <- interaction(p$Pitcher, p$PA_ID, drop = TRUE)

    # --- helper: EV/LA detection ---
    get_ev_la_local <- function(df) {
      first_present <- function(...) {
        cands <- c(...)
        hit <- cands[cands %in% names(df)]
        if (length(hit)) hit[1] else NA_character_
      }
      evla <- resolve_ev_la_strict(df)
      ev <- suppressWarnings(as.numeric(evla$ev))
      la <- suppressWarnings(as.numeric(evla$la))
      if (all(!is.finite(ev))) {
        ev_col <- first_present("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
        if (!is.na(ev_col)) ev <- suppressWarnings(readr::parse_number(as.character(df[[ev_col]])))
      }
      if (all(!is.finite(la))) {
        la_col <- first_present("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
        if (!is.na(la_col)) la <- suppressWarnings(readr::parse_number(as.character(df[[la_col]])))
      }
      list(ev = ev, la = la)
    }

    to_num_local <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))

    resolve_rbi_vec <- function(df, outc) {
      rbi_cands <- c("RBI","RBI|PIT","RBI_PIT","RBI (PIT)","RBI_P","RunsBattedIn","Runs_Batted_In","RBIs")
      col <- rbi_cands[rbi_cands %in% names(df)][1]
      if (!is.na(col)) {
        return(list(vec = to_num_local(df[[col]]), present = TRUE))
      }
      if ("RunsScored" %in% names(df)) {
        rbi <- to_num_local(df[["RunsScored"]])
        rbi <- ifelse(outc$BIP_pa %in% TRUE, rbi, 0)
        return(list(vec = rbi, present = TRUE))
      }
      list(vec = rep(NA_real_, nrow(df)), present = FALSE)
    }

    summarize_batting <- function(pa_last_df, grp_vec) {
      if (is.null(pa_last_df) || !nrow(pa_last_df)) {
        return(tibble::tibble(
          grp = character(),
          BB = double(), HBP = double(), K = double(), HR = double(),
          H = double(), TB = double(), SF = double(), RBI = double(),
          PA = integer(), AB = double(), OBP = double(), SLG = double(), OPS = double()
        ))
      }

      outc <- pa_outcome_cols(pa_last_df)
      rbi_info <- resolve_rbi_vec(pa_last_df, outc)
      rbi_vec <- rbi_info$vec
      rbi_present <- rbi_info$present

      resolve_stat_vec <- function(df, cands, default_vec) {
        col <- cands[cands %in% names(df)][1]
        if (!is.na(col)) {
          return(list(vec = to_num_local(df[[col]]), present = TRUE))
        }
        list(vec = default_vec, present = FALSE)
      }

      sf_vec <- grepl("(?i)sacrifice fly|\\bsf\\b", outc$PR_txt)
      tb_default <- as.numeric(outc$X1B) + 2 * as.numeric(outc$X2B) + 3 * as.numeric(outc$X3B) + 4 * as.numeric(outc$HR)
      h_vec  <- outc$X1B | outc$X2B | outc$X3B | outc$HR

      kb_chr <- if ("KorBB" %in% names(pa_last_df)) as.character(pa_last_df$KorBB) else rep("", nrow(pa_last_df))
      pr_chr <- if ("PlayResult" %in% names(pa_last_df)) as.character(pa_last_df$PlayResult) else rep("", nrow(pa_last_df))
      ibb_vec <- grepl("intentional|\\bibb\\b", kb_chr, ignore.case = TRUE) |
        grepl("intentional", pr_chr, ignore.case = TRUE)

      bb_default <- as.numeric(outc$BB) + as.numeric(ibb_vec)
      k_default  <- as.numeric(outc$K)
      hr_default <- as.numeric(outc$HR)

      tb_info <- resolve_stat_vec(pa_last_df, c("TB|PIT","TB_PIT","TB","TotalBases"), tb_default)
      bb_info <- resolve_stat_vec(pa_last_df, c("BB|PIT","BB_PIT","BB"), bb_default)
      k_info  <- resolve_stat_vec(pa_last_df, c("K|PIT","SO|PIT","K_PIT","SO","SO_PIT","SO|PIT"), k_default)
      hr_info <- resolve_stat_vec(pa_last_df, c("HR|PIT","HR_PIT","HR"), hr_default)

      tibble::tibble(
        grp = grp_vec,
        BB  = bb_info$vec,
        HBP = as.numeric(outc$HBP),
        K   = k_info$vec,
        HR  = hr_info$vec,
        H   = as.numeric(h_vec),
        TB  = tb_info$vec,
        SF  = as.numeric(sf_vec),
        RBI = rbi_vec
      ) %>%
        dplyr::group_by(grp) %>%
        dplyr::summarise(
          PA  = dplyr::n(),
          BB  = sum(BB,  na.rm = TRUE),
          HBP = sum(HBP, na.rm = TRUE),
          K   = sum(K,   na.rm = TRUE),
          HR  = sum(HR,  na.rm = TRUE),
          H   = sum(H,   na.rm = TRUE),
          TB  = sum(TB,  na.rm = TRUE),
          SF  = sum(SF,  na.rm = TRUE),
          RBI = if (rbi_present) sum(RBI, na.rm = TRUE) else NA_real_,
          AB  = pmax(PA - BB - HBP - SF, 0),
          OBP = sdiv(H + BB + HBP, AB + BB + HBP + SF),
          SLG = sdiv(TB, AB),
          OPS = ifelse(is.finite(OBP) & is.finite(SLG), OBP + SLG, NA_real_),
          .groups = "drop"
        )
    }

    calc_prv <- function(tb, bb, k, rbi, hr, p) {
      ifelse(is.finite(rbi), sdiv((((tb + bb - k) / 4) + rbi + hr), p) * 100, NA_real_)
    }

    evla <- get_ev_la_local(p)
    evn  <- evla$ev
    lan  <- evla$la

    pc_vec     <- as.character(p$PitchCall %||% "")
    bip_vec    <- safe_is_bip(pc_vec, p$PlayResult %||% NULL)
    barrel_vec <- bip_vec & is.finite(evn) & is.finite(lan) &
      evn >= BARREL_EV_MIN & lan >= BARREL_LA_MIN & lan <= BARREL_LA_MAX

    is_k_pitch_vec <- calc_is_k_pitch(p)

    p_flags <- p %>%
      dplyr::mutate(
        .pc     = pc_vec,
        .bip    = bip_vec,
        .ev_ok  = is.finite(evn),
        .la_ok  = is.finite(lan),
        .bip_evla = .bip & .ev_ok & .la_ok,
        .bip_la  = .bip & .la_ok,
        .barrel_ok = barrel_date_ok(p),
        .barrel = barrel_vec,
        .gb     = .bip & is.finite(lan) & lan < 5,

        .inz    = if ("InZone" %in% names(.)) as.logical(InZone) else (inZone == 1L),
        .swing  = if ("IsSwing" %in% names(.)) as.logical(IsSwing) else .is_swing_event(.pc),
        .strike = if ("IsStrike" %in% names(.)) as.logical(IsStrike) else (.pc %in% c(
          "StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"
        )),

        .pre2k  = !is.na(StrikesPre) & StrikesPre < 2,
        .twok   = !is.na(StrikesPre) & StrikesPre == 2,
        .twok_no32 = (!is.na(StrikesPre) & StrikesPre == 2) & (!is.na(BallsPre) & BallsPre != 3),
        .at_11  = (BallsPre == 1L & StrikesPre == 1L),
        .is_strike_pitch = .pc %in% c("StrikeCalled","StrikeSwinging",
                                      "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
        .win_11 = .is_strike_pitch,
        .is_k_pitch = is_k_pitch_vec
      )

    pitch_rates <- p_flags %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        Pitches   = dplyr::n(),

        swings    = sum(.swing, na.rm = TRUE),
        whiffs    = sum(.pc == "StrikeSwinging", na.rm = TRUE),
        csw_num   = sum(.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE),

        iz_swings = sum(.inz & .swing, na.rm = TRUE),
        iz_whiffs = sum(.inz & (.pc == "StrikeSwinging"), na.rm = TRUE),

        ooz       = sum(!.inz, na.rm = TRUE),
        chases    = sum(!.inz & .swing, na.rm = TRUE),

        Strike_pct      = sdiv(sum(.strike, na.rm = TRUE), Pitches),
        Zone_pct        = sdiv(sum(.inz,    na.rm = TRUE), sum(!is.na(.inz), na.rm = TRUE)),
        TwoKZone_pct = {
          den <- sum(.twok_no32 & !is.na(.inz), na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok_no32 & .inz, na.rm = TRUE), den) else NA_real_
        },
        PutAway_pct = {
          den <- sum(.twok, na.rm = TRUE)
          if (den > 0) sdiv(sum(.twok & .is_k_pitch, na.rm = TRUE), den) else NA_real_
        },

        BIP        = sum(.bip, na.rm = TRUE),
        Barrels    = sum(.barrel, na.rm = TRUE),
        Barrel_pct = sdiv(sum(.barrel & .barrel_ok, na.rm = TRUE), sum(.bip_evla & .barrel_ok, na.rm = TRUE)),
        GB         = sum(.gb, na.rm = TRUE),
        GB_pct     = sdiv(sum(.gb, na.rm = TRUE), sum(.bip_la, na.rm = TRUE)),

        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Whiff_pct    = sdiv(whiffs, swings),
        CSW_pct      = sdiv(csw_num, Pitches),
        IZWhiff_pct  = sdiv(iz_whiffs, iz_swings),
        Chase_pct    = sdiv(chases, ooz)
      )

    pa_first <- p %>% dplyr::group_by(PA_ID_LB) %>% dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    pa_group <- pa_first %>% dplyr::transmute(PA_ID_LB, grp = .data$Pitcher)

    fps_pa <- p %>%
      dplyr::group_by(PA_ID_LB) %>%
      dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::left_join(pa_group, by = "PA_ID_LB")

    ea_pa <- calc_ea_pa_summary(p_flags, "PA_ID_LB") %>%
      dplyr::left_join(pa_group, by = "PA_ID_LB")

    pa_last <- p %>% dplyr::group_by(PA_ID_LB) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    pa_out <- pa_last %>%
      dplyr::mutate(grp = pa_group$grp[match(PA_ID_LB, pa_group$PA_ID_LB)]) %>%
      {
        outc <- pa_outcome_cols(.)
        pr <- outc$PR_txt
        kb <- as.character(.$KorBB %||% "")
        any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
        outs_play <- if ("OutsOnPlay" %in% names(.)) {
          v <- suppressWarnings(as.integer(.$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
        } else rep(0L, nrow(.))
        outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
        outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
        outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
        outs_total <- outs_play + as.integer(outc$K)

        tibble::tibble(
          PA_ID_LB = .$PA_ID_LB,
          grp   = .$grp,
          K     = outc$K,
          BB    = (any_walk),
          outs  = outs_total
        )
      }

    fip <- compute_fip_xfip_grouped(
      pa_last,
      grp = pa_group$grp[match(pa_last$PA_ID_LB, pa_group$PA_ID_LB)]
    ) %>%
      dplyr::select(.grp, FIP) %>% dplyr::rename(grp = .grp)
    wob <- compute_woba_grouped(
      pa_last,
      grp = pa_group$grp[match(pa_last$PA_ID_LB, pa_group$PA_ID_LB)]
    ) %>%
      dplyr::select(.grp, wOBA, wOBAcon) %>% dplyr::rename(grp = .grp)

    pa_rates <- pa_out %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(
        PA   = dplyr::n(),
        Kp   = mean(K,     na.rm = TRUE),
        BBp  = mean(BB,    na.rm = TRUE),
        Outs = sum(outs,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(
        fps_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(FPS = sdiv(sum(fp_strike, na.rm = TRUE), dplyr::n()),
                                                             .groups = "drop"),
        by = "grp"
      ) %>%
      dplyr::left_join(
        ea_pa %>% dplyr::group_by(grp) %>% dplyr::summarise(
          EA = calc_ea_rate_from_pa(tibble::tibble(
            n_pitches = n_pitches,
            strikes_first3 = strikes_first3,
            early_bip = early_bip,
            any_hbp = any_hbp,
            any_barrel = any_barrel
          )),
          .groups = "drop"
        ),
        by = "grp"
      )

    pa_grp_vec <- pa_group$grp[match(pa_last$PA_ID_LB, pa_group$PA_ID_LB)]
    pa_bat <- summarize_batting(pa_last, pa_grp_vec)
    pa_bat_small <- pa_bat %>%
      dplyr::select(grp, SLG, OPS, TB, BB, HBP, K, RBI, HR, H, AB)

    max_velo <- p %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(MaxVelocity = suppressWarnings(max(as.numeric(RelSpeed), na.rm = TRUE)),
                       .groups = "drop")

    sd_tbl <- compute_shutdown_by_pitcher(p, d_all = df_nonbp) %>%
      dplyr::select(Pitcher, ShutDown_pct)

    pct_fmt <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100*x), "NA")
    num_fmt_3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
    num_fmt_2 <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "NA")
    num_fmt_1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "NA")

    tab_base <- pitch_rates %>%
      dplyr::left_join(pa_rates, by = "grp") %>%
      dplyr::left_join(pa_bat_small, by = "grp") %>%
      dplyr::left_join(fip, by = "grp") %>%
      dplyr::left_join(wob, by = "grp") %>%
      dplyr::left_join(max_velo, by = "grp") %>%
      dplyr::left_join(sd_tbl, by = c("grp" = "Pitcher")) %>%
      dplyr::mutate(
        Pitcher = grp,
        PA      = ifelse(is.finite(PA), as.integer(PA), NA_integer_),
        BAA     = sdiv(H, AB),
        WHIP    = sdiv(BB + H, Outs / 3),
        K9      = sdiv(K * 9, Outs / 3),
        BB9     = sdiv(BB * 9, Outs / 3),
        H9      = sdiv(H * 9, Outs / 3),
        BBHBPp  = sdiv(BB + HBP, PA),
        pRV     = calc_prv(TB, BB, K, RBI, HR, Pitches)
      ) %>%
      dplyr::select(
        Pitcher, PA, BAA, wOBA, wOBAcon, SLG, OPS, WHIP, K9, BB9, H9, pRV, Kp, BBp, BBHBPp, Barrel_pct, GB_pct, FIP,
        MaxVelocity, FPS, EA, Strike_pct, Zone_pct, TwoKZone_pct, PutAway_pct, ShutDown_pct,
        Whiff_pct, CSW_pct, IZWhiff_pct, Chase_pct
      ) %>%
      dplyr::arrange(Pitcher)
    
    tab <- tab_base %>%
      dplyr::mutate(
        wOBA    = num_fmt_3(wOBA),
        wOBAcon = num_fmt_3(wOBAcon),
        BAA     = num_fmt_3(BAA),
        SLG     = num_fmt_3(SLG),
        OPS     = num_fmt_3(OPS),
        WHIP    = num_fmt_2(WHIP),
        `K/9`   = num_fmt_1(K9),
        `BB/9`  = num_fmt_1(BB9),
        `H/9`   = num_fmt_1(H9),
        pRV     = num_fmt_2(pRV),
        `K%`       = pct_fmt(Kp),
        `BB%`      = pct_fmt(BBp),
        `BB+HBP%`  = pct_fmt(BBHBPp),
        `Barrel%`  = pct_fmt(Barrel_pct),
        `GB%`      = pct_fmt(GB_pct),
        `FIP`      = num_fmt_2(FIP),
        `Max Velocity` = num_fmt_1(MaxVelocity),
        `FPS%`     = pct_fmt(FPS),
        `E&A%`     = pct_fmt(EA),
        `Strike%`  = pct_fmt(Strike_pct),
        `Zone%`    = pct_fmt(Zone_pct),
        `2k Zone%` = pct_fmt(TwoKZone_pct),
        `Put Away%`= pct_fmt(PutAway_pct),
        `Whiff%`   = pct_fmt(Whiff_pct),
        `CSW%`     = pct_fmt(CSW_pct),
        `IZWhiff%` = pct_fmt(IZWhiff_pct),
        `Chase%`   = pct_fmt(Chase_pct),
        `Shut Down Inning%` = pct_fmt(ShutDown_pct)
      ) %>%
      dplyr::select(
        Pitcher, PA, BAA, wOBA, wOBAcon, SLG, OPS, WHIP, `K/9`, `BB/9`, `H/9`, pRV, `K%`, `BB%`, `BB+HBP%`, `Barrel%`, `GB%`, `FIP`,
        `Max Velocity`, `FPS%`, `E&A%`, `Strike%`, `Zone%`, `2k Zone%`, `Put Away%`,
        `Shut Down Inning%`, `Whiff%`, `CSW%`, `IZWhiff%`, `Chase%`
      )

    shade_cols <- c("BAA","wOBA","wOBAcon","SLG","OPS","WHIP","K/9","BB/9","H/9","K%","BB%","BB+HBP%","Barrel%","GB%","FIP",
                    "FPS%","E&A%","Strike%","Zone%","2k Zone%","Put Away%","Shut Down Inning%",
                    "Whiff%","CSW%","IZWhiff%","Chase%")
    tab_shaded <- shade_columns_txst(
      tab,
      cols = intersect(shade_cols, names(tab)),
      lower_better = intersect(c("BAA","WHIP","BB/9","H/9","BB%","BB+HBP%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"), names(tab)),
      palette = "player"
    )
    
    # Build hidden numeric sort columns to guarantee correct ordering
    sort_cols <- tab_base %>%
      dplyr::transmute(
        `..sort_PA` = PA,
        `..sort_BAA` = BAA,
        `..sort_wOBA` = wOBA,
        `..sort_wOBAcon` = wOBAcon,
        `..sort_SLG` = SLG,
        `..sort_OPS` = OPS,
        `..sort_WHIP` = WHIP,
        `..sort_K/9` = K9,
        `..sort_BB/9` = BB9,
        `..sort_H/9` = H9,
        `..sort_pRV` = pRV,
        `..sort_K%` = Kp,
        `..sort_BB%` = BBp,
        `..sort_BB+HBP%` = BBHBPp,
        `..sort_Barrel%` = Barrel_pct,
        `..sort_GB%` = GB_pct,
        `..sort_FIP` = FIP,
        `..sort_Max Velocity` = MaxVelocity,
        `..sort_FPS%` = FPS,
        `..sort_E&A%` = EA,
        `..sort_Strike%` = Strike_pct,
        `..sort_Zone%` = Zone_pct,
        `..sort_2k Zone%` = TwoKZone_pct,
        `..sort_Put Away%` = PutAway_pct,
        `..sort_Shut Down Inning%` = ShutDown_pct,
        `..sort_Whiff%` = Whiff_pct,
        `..sort_CSW%` = CSW_pct,
        `..sort_IZWhiff%` = IZWhiff_pct,
        `..sort_Chase%` = Chase_pct
      )
    
    # Totals row (all pitchers combined, same stat set)
    total_row <- {
      p_all <- p
      if (!"PA_ID" %in% names(p_all)) p_all <- ensure_pa(p_all)
      # Totals should reflect unique PAs across all pitchers (not summed by pitcher)
      pa_n_total <- length(unique(p_all$PA_ID))
      # pitch-level totals from p_flags
      n_all <- nrow(p_flags)
      strike  <- sdiv(sum(p_flags$.strike, na.rm = TRUE), n_all)
      zone    <- sdiv(sum(p_flags$.inz,    na.rm = TRUE), sum(!is.na(p_flags$.inz), na.rm = TRUE))
      swings  <- sum(p_flags$.swing, na.rm = TRUE)
      whiff   <- sdiv(sum(p_flags$.pc == "StrikeSwinging", na.rm = TRUE), swings)
      csw     <- sdiv(sum(p_flags$.pc %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE), n_all)
      iz_sw   <- sum(p_flags$.inz & p_flags$.swing, na.rm = TRUE)
      izw     <- sdiv(sum(p_flags$.inz & (p_flags$.pc == "StrikeSwinging"), na.rm = TRUE), iz_sw)
      ooz     <- sum(!p_flags$.inz, na.rm = TRUE)
      chase   <- sdiv(sum(!p_flags$.inz & p_flags$.swing, na.rm = TRUE), ooz)
      twok_den <- sum(p_flags$.twok_no32 & !is.na(p_flags$.inz), na.rm = TRUE)
      twok_zone <- if (twok_den > 0) sdiv(sum(p_flags$.twok_no32 & p_flags$.inz, na.rm = TRUE), twok_den) else NA_real_
      put_den <- sum(p_flags$.twok, na.rm = TRUE)
      put_away <- if (put_den > 0) sdiv(sum(p_flags$.twok & p_flags$.is_k_pitch, na.rm = TRUE), put_den) else NA_real_
      
      bipT    <- sum(p_flags$.bip, na.rm = TRUE)
      barT    <- sum(p_flags$.barrel & p_flags$.barrel_ok, na.rm = TRUE)
      barrelp <- sdiv(barT, sum(p_flags$.bip_evla & p_flags$.barrel_ok, na.rm = TRUE))
      gbT     <- sum(p_flags$.gb, na.rm = TRUE)
      gbp     <- sdiv(gbT, sum(p_flags$.bip_la, na.rm = TRUE))
      
      # PA-level totals
      # Use pitcher-unique PA IDs for totals as well
      pa_lastT <- p_all %>% dplyr::group_by(PA_ID) %>% dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
      outc     <- pa_outcome_cols(pa_lastT)
      pr       <- outc$PR_txt
      kb       <- as.character(pa_lastT$KorBB %||% "")
      any_walk <- grepl("(?i)walk|\\bBB\\b|\\bIBB\\b", pr) | grepl("(?i)\\bBB\\b|\\bIBB\\b|walk", kb)
      bb_only  <- any_walk
      outs_play <- if ("OutsOnPlay" %in% names(pa_lastT)) {
        v <- suppressWarnings(as.integer(pa_lastT$OutsOnPlay)); v[!is.finite(v)] <- 0L; v
      } else rep(0L, nrow(pa_lastT))
      outs_play <- ifelse(grepl("(?i)triple ?play", pr), 3L, outs_play)
      outs_play <- ifelse(grepl("(?i)double ?play", pr), 2L, outs_play)
      outs_play <- ifelse(grepl("(?i)\\bout\\b", pr) & !outc$K, pmax(outs_play, 1L), outs_play)
      outs_total <- sum(outs_play, na.rm = TRUE) + sum(as.integer(outc$K), na.rm = TRUE)
      
      fps_paT <- p_all %>%
        dplyr::group_by(PA_ID) %>%
        dplyr::summarise(fp_strike = any(FirstPitch %in% TRUE & IsStrike %in% TRUE, na.rm = TRUE), .groups = "drop")
      FPS <- mean(fps_paT$fp_strike, na.rm = TRUE)
      
      ea_paT <- calc_ea_pa_summary(
        p_all %>% dplyr::mutate(.strike = p_flags$.strike, .barrel = p_flags$.barrel, .hbp = p_flags$HBP),
        "PA_ID"
      )
      EA <- calc_ea_rate_from_pa(ea_paT)
      
      sd_all <- compute_shutdown_by_pitcher(p_all, d_all = df_nonbp)
      sd_opp <- sum(sd_all$ShutDownOpp, na.rm = TRUE)
      sd_succ <- sum(sd_all$ShutDown, na.rm = TRUE)
      sd_pct <- ifelse(sd_opp > 0, sdiv(sd_succ, sd_opp), NA_real_)
      
      FIP <- compute_fip_xfip_grouped(pa_lastT, grp = "TOTAL")$FIP[1]
      # wOBA / wOBAcon totals (explicit, PA-based)
      w_outc <- pa_outcome_cols(pa_lastT)
      w_num <- with(w_outc, woba_weights$BB  * as.numeric(BB)  +
                      woba_weights$HBP * as.numeric(HBP) +
                      woba_weights$X1B * as.numeric(X1B) +
                      woba_weights$X2B * as.numeric(X2B) +
                      woba_weights$X3B * as.numeric(X3B) +
                      woba_weights$HR  * as.numeric(HR))
      w_den <- with(w_outc, as.numeric(BB) + as.numeric(HBP) + as.numeric(BIP_pa) + as.numeric(K))
      w_num_con <- with(w_outc, woba_weights$X1B * as.numeric(X1B) +
                          woba_weights$X2B * as.numeric(X2B) +
                          woba_weights$X3B * as.numeric(X3B) +
                          woba_weights$HR  * as.numeric(HR))
      w_den_con <- as.numeric(w_outc$BIP_pa)
      woba_total <- sdiv(sum(w_num, na.rm = TRUE), sum(w_den, na.rm = TRUE))
      wobacon_total <- sdiv(sum(w_num_con, na.rm = TRUE), sum(w_den_con, na.rm = TRUE))
      pa_grp_vecT <- rep("TOTAL", nrow(pa_lastT))
      pa_batT <- summarize_batting(pa_lastT, pa_grp_vecT)
      prv_tot <- calc_prv(pa_batT$TB[1], pa_batT$BB[1], pa_batT$K[1], pa_batT$RBI[1], pa_batT$HR[1], nrow(p_all))
      
      max_vel <- suppressWarnings(max(as.numeric(p_all$RelSpeed), na.rm = TRUE))
      
      tibble::tibble(
        Pitcher = "TOTAL",
        PA = pa_n_total,
        BAA = ifelse(is.finite(sdiv(pa_batT$H[1], pa_batT$AB[1])), sprintf("%.3f", sdiv(pa_batT$H[1], pa_batT$AB[1])), "NA"),
        wOBA = ifelse(is.finite(woba_total), sprintf("%.3f", woba_total), "NA"),
        wOBAcon = ifelse(is.finite(wobacon_total), sprintf("%.3f", wobacon_total), "NA"),
        SLG = ifelse(is.finite(pa_batT$SLG[1]), sprintf("%.3f", pa_batT$SLG[1]), "NA"),
        OPS = ifelse(is.finite(pa_batT$OPS[1]), sprintf("%.3f", pa_batT$OPS[1]), "NA"),
        WHIP = ifelse(is.finite(sdiv(pa_batT$BB[1] + pa_batT$H[1], outs_total / 3)), sprintf("%.2f", sdiv(pa_batT$BB[1] + pa_batT$H[1], outs_total / 3)), "NA"),
        `K/9` = ifelse(is.finite(sdiv(pa_batT$K[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(pa_batT$K[1] * 9, outs_total / 3)), "NA"),
        `BB/9` = ifelse(is.finite(sdiv(pa_batT$BB[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(pa_batT$BB[1] * 9, outs_total / 3)), "NA"),
        `H/9` = ifelse(is.finite(sdiv(pa_batT$H[1] * 9, outs_total / 3)), sprintf("%.1f", sdiv(pa_batT$H[1] * 9, outs_total / 3)), "NA"),
        pRV = ifelse(is.finite(prv_tot), sprintf("%.2f", prv_tot), "NA"),
        `K%` = sprintf("%.0f%%", 100*mean(outc$K, na.rm = TRUE)),
        `BB%` = sprintf("%.0f%%", 100*mean(bb_only, na.rm = TRUE)),
        `BB+HBP%` = ifelse(is.finite(sdiv(pa_batT$BB[1] + pa_batT$HBP[1], pa_n_total)), sprintf("%.0f%%", 100*sdiv(pa_batT$BB[1] + pa_batT$HBP[1], pa_n_total)), "NA"),
        `Barrel%` = ifelse(is.finite(barrelp), sprintf("%.0f%%", 100*barrelp), "NA"),
        `GB%` = ifelse(is.finite(gbp), sprintf("%.0f%%", 100*gbp), "NA"),
        FIP = ifelse(is.finite(FIP), sprintf("%.2f", FIP), "NA"),
        `Max Velocity` = ifelse(is.finite(max_vel), sprintf("%.1f", max_vel), "NA"),
        `FPS%` = ifelse(is.finite(FPS), sprintf("%.0f%%", 100*FPS), "NA"),
        `E&A%` = ifelse(is.finite(EA), sprintf("%.0f%%", 100*EA), "NA"),
        `Strike%` = ifelse(is.finite(strike), sprintf("%.0f%%", 100*strike), "NA"),
        `Zone%` = ifelse(is.finite(zone), sprintf("%.0f%%", 100*zone), "NA"),
        `2k Zone%` = ifelse(is.finite(twok_zone), sprintf("%.0f%%", 100*twok_zone), "NA"),
        `Put Away%` = ifelse(is.finite(put_away), sprintf("%.0f%%", 100*put_away), "NA"),
        `Whiff%` = ifelse(is.finite(whiff), sprintf("%.0f%%", 100*whiff), "NA"),
        `CSW%` = ifelse(is.finite(csw), sprintf("%.0f%%", 100*csw), "NA"),
        `IZWhiff%` = ifelse(is.finite(izw), sprintf("%.0f%%", 100*izw), "NA"),
        `Chase%` = ifelse(is.finite(chase), sprintf("%.0f%%", 100*chase), "NA"),
        `Shut Down Inning%` = ifelse(is.finite(sd_pct), sprintf("%.0f%%", 100*sd_pct), "NA")
      )
    }
    
    # shade totals row consistently
    total_row_shaded <- shade_columns_txst(
      total_row,
      cols = intersect(shade_cols, names(total_row)),
      lower_better = intersect(c("BAA","WHIP","BB/9","H/9","BB%","BB+HBP%","Barrel%","wOBA","wOBAcon","FIP","SLG","OPS"), names(total_row)),
      palette = "player"
    )

    leaderboard_totals_cache(total_row_shaded)

    tab_out <- cbind(tab_shaded, sort_cols)

    display_cols <- names(tab_shaded)
    sort_cols_names <- names(sort_cols)
    display_idx0 <- seq_along(display_cols) - 1
    sort_idx0 <- seq_along(sort_cols_names) - 1 + length(display_cols)
    
    order_defs <- lapply(seq_along(display_cols), function(i) {
      nm <- display_cols[[i]]
      if (nm == "Pitcher") {
        list(targets = display_idx0[[i]], orderDataType = "string")
      } else {
        # match display column to sort column by name
        sname <- paste0("..sort_", nm)
        sidx <- sort_idx0[match(sname, sort_cols_names)]
        list(targets = display_idx0[[i]], orderData = sidx)
      }
    })

    DT::datatable(
      tab_out,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = 't',
        paging   = FALSE,
        ordering = TRUE,
        stripe   = TRUE,
        scrollX  = TRUE,
        columnDefs = c(
          list(list(targets = sort_idx0, visible = FALSE)),
          order_defs
        )
      ),
      class = "stripe"
    )
  })

  output$leaderboard_totals_table <- DT::renderDT({
    total_row_shaded <- leaderboard_totals_cache()
    if (is.null(total_row_shaded)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    DT::datatable(
      total_row_shaded,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = 't',
        paging   = FALSE,
        ordering = FALSE,
        stripe   = TRUE,
        scrollX  = TRUE
      ),
      class = "stripe"
    )
  })
  
  output$leader_results_pdf <- downloadHandler(
    filename = function() {
      paste0("Staff_Leaderboard_Results_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      d <- leaderboard_data()
      stats_df <- leaderboard_summary(d)
      render_leaderboard_pdf(file, "results", stats_df)
    }
  )
  
  output$leader_process_pdf <- downloadHandler(
    filename = function() {
      paste0("Staff_Leaderboard_Process_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      d <- leaderboard_data()
      stats_df <- leaderboard_summary(d)
      render_leaderboard_pdf(file, "process", stats_df)
    }
  )
}
