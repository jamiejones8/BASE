suppressPackageStartupMessages({
  library(magrittr)
  library(shiny)
  if (!isTRUE(get0("BASE_HITTING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
    library(tidyverse)
  }
  library(bslib)
  library(htmltools)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(readr)
  library(plotly)
  library(patchwork)
  library(shinycssloaders)
  library(gridExtra)
  library(png)
  library(jpeg)
  library(grid)      
  library(gtable) 
  library(cowplot)   
  library(grid)     
})

# ---- Static assets path (works for single-file standalone shinyApp) ----
if (!isTRUE(get0("BASE_HITTING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE)) &&
    dir.exists("www")) {
  shiny::addResourcePath("static", normalizePath("www", mustWork = TRUE))
}

# -------------------- Logos --------------------
txst_logo   <- "www/txst_logo.png"
bobcat_logo <- "www/bobcat_watermark.png"

# --- LOGO PATHS (edit these if needed) ---
TXST_LOGO_PATH <- get0(
  "BASE_HITTING_TXST_LOGO_PATH",
  inherits = FALSE,
  ifnotfound = "img/txst_logo.png"
)
BOBCAT_LOGO_PATH <- get0(
  "BASE_HITTING_BOBCAT_LOGO_PATH",
  inherits = FALSE,
  ifnotfound = "img/bobcat_logo.png"
)
bobcat_logo <- BOBCAT_LOGO_PATH

# -------------------- wOBA/xwOBA helpers --------------------
woba_weights <- list(BB=0.690, HBP=0.720, X1B=0.880, X2B=1.247, X3B=1.578, HR=2.031)
safe_ratio <- function(a,b) ifelse(b > 0, a/b, NA_real_)
to_num     <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
nz_chr     <- function(x) ifelse(is.na(x), "", as.character(x))
pick_first <- function(cands, in_df) { cands <- cands[cands %in% names(in_df)]; if (length(cands)) cands[[1]] else NA_character_ }

# Reuse the expected-contact grid and PA weights from the older CAPS workflow.
.hitting_xwoba_cache <- new.env(parent = emptyenv())
hitting_xwoba_grid <- function() {
  key <- "grid"
  if (exists(key, envir = .hitting_xwoba_cache, inherits = FALSE)) {
    return(get(key, envir = .hitting_xwoba_cache, inherits = FALSE))
  }
  configured <- get0("BASE_HITTING_XWOBA_GRID_PATH",
                     envir = environment(hitting_xwoba_grid), inherits = FALSE, ifnotfound = "")
  env_path <- Sys.getenv("BASE_XWGRID_FILE", unset = "")
  candidates <- unique(c(
    configured,
    env_path,
    file.path("models", "shared", "xwoba_grid.rds"),
    file.path("..", "..", "models", "shared", "xwoba_grid.rds")
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  value <- if (length(candidates)) {
    tryCatch(readRDS(candidates[[1]]), error = function(e) NULL)
  } else NULL
  assign(key, value, envir = .hitting_xwoba_cache)
  value
}

hitting_xwoba_lookup <- function(ev, la, grid = hitting_xwoba_grid()) {
  ev <- to_num(ev); la <- to_num(la)
  out <- rep(NA_real_, max(length(ev), length(la)))
  if (is.null(grid) || !all(c("ev_edges", "la_edges", "grid") %in% names(grid))) return(out)
  ei <- findInterval(ev, grid$ev_edges)
  li <- findInterval(la, grid$la_edges)
  ok <- is.finite(ev) & is.finite(la) &
    ei >= 1 & ei < length(grid$ev_edges) &
    li >= 1 & li < length(grid$la_edges)
  out[ok] <- grid$grid[cbind(ei[ok], li[ok])]
  out
}
name_display <- function(x){
  x <- as.character(x)
  vapply(x, function(s){
    s <- trimws(s)
    s <- gsub("\\s+", " ", s)
    if (grepl(",", s)) {
      parts <- strsplit(s, ",\\s*")[[1]]
      if (length(parts) >= 2) return(paste0(parts[2], " ", parts[1]))
    }
    s
  }, "", USE.NAMES = FALSE)
}
name_norm <- function(x){
  tolower(name_display(x))
}

# -------------------- Zone logic helpers (INCHES) --------------------
zone_h_in <- function(h){
  h <- to_num(h)
  if (!length(h)) return(h)
  q90 <- suppressWarnings(stats::quantile(h, 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 10) h <- h * 12
  h
}
zone_s_in <- function(s){
  s <- to_num(s)
  if (!length(s)) return(s)
  q90 <- suppressWarnings(stats::quantile(abs(s), 0.90, na.rm = TRUE))
  if (is.finite(q90) && q90 < 5) s <- s * 12
  s
}
in_zone_inches <- function(s_in, h_in){
  is.finite(s_in) & is.finite(h_in) &
    dplyr::between(h_in, 18.29, 44.08) &
    dplyr::between(s_in, -9.97, 9.97)
}

# Ball-in-play detector (excludes BB/HBP/K/interference)
is_bip_txst <- function(pc, pr){
  pr_chr <- nz_chr(pr)
  pc_bip <- pc %in% c("InPlay","InPlayNoOut","InPlayOut")
  
  hit <- grepl("(?i)(^|\\b)(1B|2B|3B|HR)($|\\b)", pr_chr) |
    grepl("(?i)home\\s*run|homerun|\\bHR\\b|\\bsingle\\b|\\btriple\\b|\\bdouble\\b(?!\\s*play)", pr_chr, perl=TRUE)
  
  # outs recorded on balls in play (ground/fly/line/pop/field/force outs)
  outs_in_play <- grepl("(?i)(ground|fly|line|pop|field|force)\\s*-?out", pr_chr)
  
  # other in-play results that still produced a batted ball (ROE/sac/bunt etc.)
  inplay_other <- grepl("(?i)reaches? on error|\\berror\\b|sacrifice|sac\\s*bunt|bunt", pr_chr)
  
  non_bip <- grepl("(?i)walk|intentional|\\bIBB\\b|hit\\s*by\\s*pitch|\\bHBP\\b|interference|catcher\\s*interference", pr_chr)
  
  (pc_bip | hit | outs_in_play | inplay_other) & !non_bip
}

# Barrel if (BIP) AND EV >= 95.0 AND 5 <= LA <= 35.
is_barrel_txst <- function(pc, pr, ev, la){
  bip <- is_bip_txst(pc, pr)
  ev <- to_num(ev)
  la <- to_num(la)
  bip & is.finite(ev) & is.finite(la) & ev >= 95.0 & la >= 5 & la <= 35
}


# -------------------- Pitch palette & mapping --------------------
pitch_colors <- c(
  "Fastball"  = "#FF0000", "Four-Seam" = "#FF0000", "Two-Seam" = "#FF0000",
  "Sinker"    = "#FFA500",
  "Slider"    = "#FFFF00", "Sweeper"   = "#FFD700",
  "Curveball" = "#0000FF",
  "Changeup"  = "#008000",
  "Splitter"  = "#000080",
  "Cutter"    = "#000000",
  "Untagged"  = "#808080",
  "Undefined" = "#808080"
)
facet_levels <- c("Fastball","Sinker","Changeup","Splitter","Slider","Cutter","Sweeper","Curveball")
canonical_pitch <- function(x) dplyr::case_when(
  x %in% c("Fastball","Four-Seam","Four Seam","FourSeam","Two-Seam","Two Seam","2-Seam") ~ "Fastball",
  x %in% c("ChangeUp","Changeup","CH") ~ "Changeup",
  x %in% c("Sweeper","Sweep") ~ "Sweeper",
  TRUE ~ x
)

# --- Air Pull helper (use a tolerance so tiny mis-aim near CF doesn't count as "pulled") ---
PULL_TOL_DEG <- 10  # adjust if you want stricter/looser definition

pulled_side <- function(bearing_deg, bats_chr, tol = PULL_TOL_DEG){
  bearing_deg <- to_num(bearing_deg)
  bats_chr    <- nz_chr(bats_chr)
  ifelse(
    !is.finite(bearing_deg) | !nzchar(bats_chr), NA,
    ifelse(bats_chr == "R", bearing_deg <= -tol,
           ifelse(bats_chr == "L", bearing_deg >=  tol, NA))
  )
}

# -------------------- Data load --------------------
data_dir <- Sys.getenv("DATA_DIR", unset = "")
candidates <- unique(Filter(function(p) nzchar(p) && dir.exists(p),
                            c(data_dir, "data", "www/data", "Data")))
if (exists("BASE_HITTING_DATA", inherits = FALSE) &&
    is.data.frame(BASE_HITTING_DATA)) {
  # BASE injects the normalized Texas State payload from the shared source
  # route. Standalone Wally behavior is unchanged when it is absent.
  df <- tibble::as_tibble(BASE_HITTING_DATA)
  rm(BASE_HITTING_DATA)
} else if (file.exists("data/data.rds")) {
  message("Loading RDS: data/data.rds"); df <- readRDS("data/data.rds")
} else {
  message("Data search paths: ", paste(candidates, collapse = " | "))
  csvs <- unlist(lapply(candidates, function(d) list.files(d, pattern="\\.csv$", full.names=TRUE)))
  if (length(csvs)) {
    message("Found ", length(csvs), " CSV(s): ", paste(basename(csvs), collapse=", "))
    df <- purrr::map_dfr(csvs, ~ tryCatch(
      readr::read_csv(.x, col_types = readr::cols(.default = readr::col_character()), show_col_types=FALSE) %>%
        dplyr::mutate(source_file = basename(.x), row_in_file = dplyr::row_number()),
      error=function(e){ warning("Failed to read ", .x, ": ", conditionMessage(e)); tibble::tibble() }
    )) %>% readr::type_convert()
  } else {
    warning("No CSV files found; starting with a typed empty tibble.")
    df <- tibble::tibble(
      PlateLocSide = numeric(), PlateLocHeight = numeric(),
      PitchCall = character(), PlayResult = character(), PitchType = character(),
      ExitSpeed = numeric(), Angle = numeric(),
      Batter = character(), BatterTeam = character(),
      Pitcher = character(), PitcherHand = character(),
      Date = character(), AwayTeam = character(), HomeTeam = character()
    )
    df$source_file <- character(0); df$row_in_file <- integer(0)
  }
}

# -------------------- Normalize columns --------------------
# plate  (robustly convert inches → feet) + coalesce across all present cols
px_candidates <- intersect(c("PlateLocSide","px","PlateX","Plate_X","PlateLocSideInches"), names(df))
pz_candidates <- intersect(c("PlateLocHeight","pz","PlateZ","Plate_Z","PlateLocHeightInches"), names(df))

normalize_plate_coord <- function(vec, colname){
  v <- to_num(vec); if (!length(v)) return(v)
  is_inch_name <- !is.na(colname) && grepl("(?i)inch|_in\\b", colname)
  q90 <- suppressWarnings(stats::quantile(abs(v), 0.90, na.rm = TRUE)); if (!is.finite(q90)) q90 <- 0
  looks_like_inches <- if (!is.na(colname) && grepl("(?i)z|height", colname)) q90 > 6 else q90 > 3.5
  if (is_inch_name || looks_like_inches) v <- v / 12
  v
}

coalesce_plate <- function(cands){
  if (!length(cands)) return(NA_real_)
  mats <- lapply(cands, function(nm) normalize_plate_coord(df[[nm]], nm))
  out <- mats[[1]]; if (length(mats) > 1) {
    for (k in 2:length(mats)) out <- dplyr::coalesce(out, mats[[k]])
  }
  out
}

df$plate_x <- coalesce_plate(px_candidates)
df$plate_z <- coalesce_plate(pz_candidates)

# ---- Inches versions for zone decisions (keep plate_x/plate_z as-is for plotting) ----
df$plate_z_in <- zone_h_in(df$plate_z)
df$plate_x_in <- zone_s_in(df$plate_x)

if (!("Zone" %in% names(df))) df$Zone <- NA_character_
df <- df %>%
  dplyr::mutate(
    Zone = dplyr::case_when(
      !is.na(.data$Zone) ~ as.character(.data$Zone),
      is.na(.data$plate_z_in) | is.na(.data$plate_x_in) ~ NA_character_,
      in_zone_inches(.data$plate_x_in, .data$plate_z_in) ~ "InZone",
      TRUE ~ "OutZone"
    ),
    in_zone = dplyr::case_when(
      is.na(.data$plate_z_in) | is.na(.data$plate_x_in) ~ NA,
      in_zone_inches(.data$plate_x_in, .data$plate_z_in) ~ TRUE,
      TRUE ~ FALSE
    )
  )


# DEBUG: show which sources contributed plate coords
if (interactive()) {
  px_counts <- sapply(px_candidates, function(nm) sum(is.finite(to_num(df[[nm]]))))
  pz_counts <- sapply(pz_candidates, function(nm) sum(is.finite(to_num(df[[nm]]))))
  message(sprintf("[AAR DEBUG] plate_x sources: %s | finite counts: %s",
                  paste(px_candidates, collapse=", "),
                  paste(px_counts, collapse=", ")))
  message(sprintf("[AAR DEBUG] plate_z sources: %s | finite counts: %s",
                  paste(pz_candidates, collapse=", "),
                  paste(pz_counts, collapse=", ")))
  message(sprintf("[AAR DEBUG] coalesced finite plate coords: %d",
                  sum(is.finite(df$plate_x) & is.finite(df$plate_z))))
}


# calls/outcomes
pc <- pick_first(c("PitchCall","Pitch_Call","Call","PitchResult","Pitch_Result"), df)
pr <- pick_first(c("PlayResult","Result","Event","Play_Result"), df)
df$pitch_call  <- if (!is.na(pc)) as.character(df[[pc]]) else NA_character_
df$play_result <- if (!is.na(pr)) as.character(df[[pr]]) else NA_character_

# Canonicalize pitch_call tokens (Oct-18 fix)
# Canonicalize pitch_call tokens (robust vendor variants)
canon_pitch_call <- function(x){
  x  <- trimws(as.character(x))
  x  <- gsub("_|-", " ", x)
  x  <- gsub("\\s+", " ", x)
  xl <- tolower(x)
  
  ifelse(
    grepl("strike.*(called|looking)|called.*strike|\\bcs\\b|\\bstrike look", xl, perl=TRUE),
    "StrikeCalled",
    ifelse(
      grepl("strike.*(swing|miss|whiff)|swing.*strike|\\bss\\b|\\bswstr\\b", xl, perl=TRUE),
      "StrikeSwinging",
      ifelse(
        grepl("foul\\s*tip", xl, perl=TRUE),
        "FoulTip",
        ifelse(
          grepl("foul.*(pop|fly|fieldable|line|ground)", xl, perl=TRUE),
          "FoulBallFieldable",
          ifelse(
            (grepl("\\bfoul\\b", xl) & !grepl("foul[\\s-]*tip", xl, perl=TRUE)),
            "FoulBallNotFieldable",
            ifelse(
              grepl("in\\s*play.*no\\s*out|in\\s*play.*noout|inplay.*no\\s*out", xl, perl=TRUE),
              "InPlayNoOut",
              ifelse(
                grepl("in\\s*play.*out", xl, perl=TRUE),
                "InPlayOut",
                ifelse(
                  grepl("in\\s*play|ball\\s*in\\s*play|\\bbip\\b", xl, perl=TRUE),
                  "InPlay",
                  ifelse(
                    grepl("\\bball\\b|pitchout|auto\\s*ball|intentional|\\bibb\\b", xl, perl=TRUE),
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

df$pitch_call <- canon_pitch_call(df$pitch_call)

# EV/LA
evc <- pick_first(c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV"), df)
lac <- pick_first(c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle"), df)
df$ev <- if (!is.na(evc)) to_num(df[[evc]]) else NA_real_
df$la <- if (!is.na(lac)) to_num(df[[lac]]) else NA_real_

# Spray (optional)
sx <- pick_first(c("hc_x","HC_X","bb_x","BB_X"), df)
sy <- pick_first(c("hc_y","HC_Y","bb_y","BB_Y"), df)
df$hc_x <- if (!is.na(sx)) to_num(df[[sx]]) else NA_real_
df$hc_y <- if (!is.na(sy)) to_num(df[[sy]]) else NA_real_
# TrackMan distance & bearing (optional)
bdc <- pick_first(c("Bearing","bearing","SprayAngle","spray_angle","HC_Bearing","HitDirection"), df)
dst <- pick_first(c("Distance","HitDistance","CarryDistance","Carry_Distance",
                    "ProjectedDistance","EstimatedDistance","TrackmanDistance","carry_distance"), df)
bat <- pick_first(c("BatterSide","Stand","BatterHand","HitterSide","Side"), df)

df$bearing     <- if (!is.na(bdc)) to_num(df[[bdc]]) else NA_real_
df$distance_ft <- if (!is.na(dst)) to_num(df[[dst]]) else NA_real_
df$bats <- if (!is.na(bat)) toupper(as.character(df[[bat]])) else NA_character_
df$bats <- dplyr::case_when(
  df$bats %in% c("R","RH","RHB","RIGHT") ~ "R",
  df$bats %in% c("L","LH","LHB","LEFT")  ~ "L",
  TRUE ~ NA_character_
)
# --- Standardize Pitcher handedness (derive if missing; length-safe) ---
{
  n <- nrow(df)
  ph_candidates <- c("PitcherHand","PitcherThrows","Pitcher_Hand","Throws","P_Throws","PitcherHandedness")
  ph_col <- ph_candidates[ph_candidates %in% names(df)][1]
  ph_raw <- if (!is.na(ph_col)) as.character(df[[ph_col]]) else rep(NA_character_, n)
  
  df$PitcherHand <- toupper(nz_chr(ph_raw))
  df$PitcherHand <- dplyr::case_when(
    df$PitcherHand %in% c("L","LH","LEFT","LHP") ~ "LHP",
    df$PitcherHand %in% c("R","RH","RIGHT","RHP") ~ "RHP",
    TRUE ~ df$PitcherHand
  )
}


# -------------------- Pitch type --------------------
# -------------------- Pitch type (FUZZY + HARD FALLBACK) --------------------
pitch_levels_all <- c(facet_levels, "Undefined", "Untagged")

canonical_pitch_fuzzy <- function(x){
  x0 <- trimws(as.character(x))
  x0 <- gsub("_|-", " ", x0)
  x0 <- gsub("\\s+", " ", x0)
  dplyr::case_when(
    # Fastball family
    grepl("(?i)fast ?ball|\\bff\\b|\\bfb\\b|\\bfour ?seam|\\b4 ?seam|\\b2 ?seam|\\btwo ?seam|\\bft\\b", x0) ~ "Fastball",
    # Sinker
    grepl("(?i)sink|\\bsi\\b", x0) ~ "Sinker",
    # Changeup
    grepl("(?i)change|\\bch\\b", x0) ~ "Changeup",
    # Splitter
    grepl("(?i)split|fork", x0) ~ "Splitter",
    # Slider
    grepl("(?i)slider|\\bsl\\b", x0) ~ "Slider",
    # Cutter
    grepl("(?i)cutter|\\bct\\b|\\bfc\\b", x0) ~ "Cutter",
    # Sweeper
    grepl("(?i)sweep", x0) ~ "Sweeper",
    # Curveball (incl. knuckle curve/slurve buckets)
    grepl("(?i)curve|knuckle\\s*curve|slurve|\\bcu\\b|\\bkc\\b", x0) ~ "Curveball",
    # Untagged/empty
    x0 == "" ~ "Untagged",
    TRUE ~ "Undefined"
  )
}

pt_coalesce <- function(d){
  n <- nrow(d)
  g <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n)
  raw <- dplyr::coalesce(
    g("PitchType"),
    g("TaggedPitchType"),
    g("AutoPitchType"),
    g("PitchName"),
    g("pitch_type_canon")
  )
  canonical_pitch_fuzzy(raw)
}

# Build a unified, SAFE column for the whole app
df$PitchType_UNI <- pt_coalesce(df)
# Force to the fixed levels; any unknown already mapped to Undefined/Untagged
df$PitchType_UNI <- factor(df$PitchType_UNI, levels = pitch_levels_all)



# ---- GameDate helpers (normalize date for IDs/filters) ----
parse_date_any <- function(x){
  if (inherits(x, "Date")) return(x)
  x <- trimws(as.character(x))
  out <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(e) rep(as.Date(NA), length(x))
  )
  need <- which(is.na(out) & !is.na(x) & nzchar(x))
  if (length(need)) {
    ymd <- gsub("[^0-9]", "", x[need]); ok <- nchar(ymd) == 8
    tmp <- rep(as.Date(NA), length(ymd)); tmp[ok] <- as.Date(ymd[ok], format = "%Y%m%d")
    out[need] <- tmp
  }
  # Fallbacks for common slash formats (e.g., 2/15/25 or 02/15/2025)
  need <- which(is.na(out) & !is.na(x) & nzchar(x))
  if (length(need)) {
    tmp1 <- suppressWarnings(as.Date(x[need], format = "%m/%d/%Y"))
    tmp2 <- suppressWarnings(as.Date(x[need], format = "%m/%d/%y"))
    out[need] <- dplyr::coalesce(tmp1, tmp2, out[need])
  }
  out
}
extract_date_from_filename <- function(fname){
  s <- as.character(fname)
  ymd <- stringr::str_extract(s, "(?<!\\d)(\\d{8})(?!\\d)")
  suppressWarnings(as.Date(ymd, format = "%Y%m%d"))
}
extract_date_from_game_key <- function(x){
  s <- trimws(as.character(x))
  s[!nzchar(s)] <- NA_character_
  ymd <- stringr::str_extract(s, "(?<!\\d)(\\d{8})(?!\\d)")
  suppressWarnings(as.Date(ymd, format = "%Y%m%d"))
}
coalesce_nz_chr <- function(...) {
  vals <- list(...)
  if (!length(vals)) return(character(0))
  out <- trimws(nz_chr(vals[[1]]))
  if (length(vals) > 1L) {
    for (i in 2:length(vals)) {
      cur <- trimws(nz_chr(vals[[i]]))
      use <- !nzchar(out) & nzchar(cur)
      out[use] <- cur[use]
    }
  }
  out[!nzchar(out)] <- NA_character_
  out
}
resolve_game_key <- function(d){
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(character(0))
  key_cols <- intersect(
    c("GameID", "game_id", "GameUID", "game_uid", "GameForeignID", "game_foreign_id", "Game", "game_pk", "GamePk"),
    names(d)
  )
  if (!length(key_cols)) return(rep(NA_character_, nrow(d)))
  do.call(coalesce_nz_chr, lapply(key_cols, function(nm) d[[nm]]))
}

# GameID base (prefer source game IDs, then fall back)
if (!("AwayTeam" %in% names(df))) df$AwayTeam <- NA_character_
if (!("HomeTeam" %in% names(df))) df$HomeTeam <- NA_character_
if (!("Date"     %in% names(df))) df$Date     <- NA_character_
if (!"GameDate" %in% names(df)) df$GameDate <- as.Date(NA)
df$GameDate <- dplyr::coalesce(df$GameDate, parse_date_any(df$Date), extract_date_from_filename(df$source_file))

# ---- Game splitter (fallback only when source game IDs are missing) ----
split_games_by_inning1 <- function(d){
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(d)
  if (!("Inning" %in% names(d))) return(d)
  
  src_file <- if ("source_file" %in% names(d)) d$source_file else NA_character_
  date_key <- dplyr::coalesce(
    parse_date_any(d$GameDate),
    parse_date_any(d$Date),
    extract_date_from_filename(src_file)
  )
  date_chr <- as.character(date_key)
  missing_date <- is.na(date_key) | !nzchar(date_chr)
  if (any(missing_date)) {
    date_chr[missing_date] <- nz_chr(d$Date[missing_date])
  }
  missing_date <- !nzchar(date_chr)
  if (any(missing_date)) {
    date_chr[missing_date] <- nz_chr(src_file[missing_date])
  }
  date_chr[!nzchar(date_chr)] <- "UnknownDate"
  
  ord <- if ("row_in_file" %in% names(d)) {
    suppressWarnings(as.numeric(d$row_in_file))
  } else if ("PitchNum" %in% names(d)) {
    suppressWarnings(as.numeric(d$PitchNum))
  } else if ("PitchofPA" %in% names(d)) {
    suppressWarnings(as.numeric(d$PitchofPA))
  } else {
    seq_len(nrow(d))
  }
  
  out <- d %>%
    dplyr::mutate(
      .ord__ = ord,
      .inn__ = suppressWarnings(as.numeric(.data$Inning)),
      .date_key__ = date_chr
    ) %>%
    dplyr::group_by(.data$.date_key__) %>%
    dplyr::arrange(.data$.ord__, .by_group = TRUE) %>%
    dplyr::mutate(
      .new_game__ = dplyr::if_else(
        dplyr::row_number() == 1L,
        1L,
        dplyr::if_else(
          is.finite(.data$.inn__) &
            .data$.inn__ == 1 &
            (is.na(dplyr::lag(.data$.inn__)) | dplyr::lag(.data$.inn__) != 1),
          1L, 0L, missing = 0L
        )
      ),
      .game_idx__ = cumsum(.data$.new_game__)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      CustomGameID = dplyr::if_else(
        .data$.game_idx__ > 1L,
        paste0(.data$.date_key__, " (G", .data$.game_idx__, ")"),
        .data$.date_key__
      )
    ) %>%
    dplyr::select(-dplyr::all_of(c(".ord__",".inn__",".date_key__",".new_game__", ".game_idx__")))
  
  out
}

assign_custom_game_id <- function(d){
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(d)
  game_key <- resolve_game_key(d)
  has_game_key <- !is.na(game_key) & nzchar(game_key)
  d$CustomGameID <- NA_character_
  if (any(has_game_key)) {
    d$CustomGameID[has_game_key] <- game_key[has_game_key]
  }
  if (any(!has_game_key)) {
    fallback <- split_games_by_inning1(d[!has_game_key, , drop = FALSE])
    d$CustomGameID[!has_game_key] <- fallback$CustomGameID
  }
  d
}

df <- assign_custom_game_id(df)

SEASON_CHOICES <- c(
  "2025 Season" = "S25",
  "2025 Fall"   = "F25",
  "2026 Squads" = "SQ26",
  "2026 Season" = "S26"
)

# ---- Season grouping (season CSV format) ----
# Prefer an explicit column if your new season CSVs have one; otherwise infer from source_file.
infer_season_group_from_file <- function(f) {
  f <- tolower(as.character(ifelse(is.na(f), "", f)))
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

# -------------------- hit_Bullpens / BP flag --------------------
normalize_person_name <- function(x){
  x <- as.character(x); x <- stringr::str_replace_all(x, "(\\w+),\\s*(\\w+)", "\\2 \\1")
  x <- gsub("[^A-Za-z\\s]", " ", x)  # drop punctuation like hyphens
  tolower(gsub("\\s+", " ", trimws(x)))
}
has_bp_suffix <- function(x){
  x <- ifelse(is.na(x), "", basename(x))
  stem <- sub("\\.csv$", "", x, ignore.case = TRUE)
  grepl("(?i)(?:-|_)bp$|(?i)-bullpens$", stem)
}
# optional batter name column
batter_name_col <- {
  nm <- names(df)
  hit <- which(grepl("(?i)^(batter|hitter)$", nm) | grepl("(?i)(batter|hitter).*name", nm))
  if (length(hit)) nm[hit[1]] else NA_character_
}
flag_by_name <- if (!is.na(batter_name_col) && "Pitcher" %in% names(df)) {
  pnorm <- normalize_person_name(df$Pitcher); bnorm <- normalize_person_name(df[[batter_name_col]])
  nzchar(pnorm) & nzchar(bnorm) & (pnorm == bnorm)
} else rep(FALSE, nrow(df))
flag_by_file <- has_bp_suffix(df$source_file)
df$is_bullpen <- flag_by_file | flag_by_name

bullpens_df <- df[df$is_bullpen %in% TRUE, , drop = FALSE]
if (nrow(bullpens_df)) {
  bp_label <- if ("Date" %in% names(bullpens_df)) as.character(bullpens_df$Date) else rep(NA_character_, nrow(bullpens_df))
  idx <- which(is.na(bp_label) | bp_label == "")
  if (length(idx)) bp_label[idx] <- bullpens_df$source_file[idx]
  idx <- which(is.na(bp_label) | bp_label == "")
  if (length(idx)) bp_label[idx] <- "(undated)"
  bullpens_df$CustomGameID_BP <- paste0(bp_label, ": BP - ", bullpens_df$Pitcher)
} else {
  bullpens_df$CustomGameID_BP <- character(0)
}
bp_game_ids <- sort(unique(stats::na.omit(bullpens_df$CustomGameID_BP)))

# -------------------- Team filter (TXST) & choices --------------------
TEAM_CODE <- get0(
  "BASE_HITTING_TEAM_CODE",
  inherits = FALSE,
  ifnotfound = "TEX_BOB"
)
df$BatterTeam <- gsub("\\s+"," ", trimws(dplyr::coalesce(df$BatterTeam, NA_character_)))

team_mask <- function(d, team_col, team_code = TEAM_CODE) {
  if (is.null(d) || !nrow(d)) return(logical(0))
  if (!(team_col %in% names(d)) || is.null(team_code) || !nzchar(team_code)) {
    return(rep(TRUE, nrow(d)))
  }
  team_vals <- toupper(trimws(as.character(d[[team_col]])))
  !is.na(team_vals) & team_vals == toupper(trimws(team_code))
}

filter_team <- function(d, team_col, team_code = TEAM_CODE) {
  if (is.null(d) || !nrow(d)) return(d)
  d[team_mask(d, team_col, team_code), , drop = FALSE]
}

txst_df <- df %>%
  dplyr::filter(!(is_bullpen %in% TRUE)) %>%
  filter_team("BatterTeam")

nz_choices <- function(x) { x <- sort(unique(stats::na.omit(x))); if (length(x)) x else NULL }
hitters_txst <- nz_choices(txst_df$Batter)
if (is.null(hitters_txst)) hitters_txst <- nz_choices(df$Batter)
if (is.null(hitters_txst)) hitters_txst <- "(no hitters found)"

# Remove departed players from selector
# Remove departed players from selector (robust to factor + "Last, First")
DROP_HITTERS <- c(
  "Caden Baker","Travis Bragg","Ian Collier","Austin Eaton","Ryne Farber",
  "Rocco Garza-Gongora","Rocco Garza Gongora","Theodore Kummer","Trent Rucker","Alan Shibley",
  "Cole Tabor","Cameron Thompson"
)

hitters_txst <- as.character(hitters_txst)

drop_norm <- normalize_person_name(DROP_HITTERS)
hitters_norm <- normalize_person_name(hitters_txst)
keep_idx  <- !(hitters_norm %in% drop_norm)
# extra guard for any variants slipping through
keep_idx  <- keep_idx & !(grepl("\\brocco\\b", hitters_norm) &
                          grepl("\\bgarza\\b", hitters_norm) &
                          grepl("\\bgongora\\b", hitters_norm))

hitters_txst <- hitters_txst[keep_idx]

# -------------------- AAR game choices (authoritative) --------------------
make_aar_games <- function(d){
  if (is.null(d) || !nrow(d)) return(character(0))
  
  out <- d %>%
    dplyr::filter(!is.na(CustomGameID), nzchar(CustomGameID)) %>%
    dplyr::distinct(CustomGameID, GameDate) %>%
    dplyr::arrange(dplyr::desc(GameDate))
  
  stats::setNames(out$CustomGameID, out$CustomGameID)
}


# -------------------- Geometry --------------------
home_plate_segments <- data.frame(
  x=c(0,0.71,0.71,0,-0.71,-0.71), y=c(0.15,0.15,0.3,0.5,0.3,0.15),
  xend=c(0.71,0.71,0,-0.71,-0.71,0), yend=c(0.15,0.3,0.5,0.3,0.15,0.15)
)
# Flip vertically so the apex points down (catcher POV)
{
  yy  <- c(home_plate_segments$y, home_plate_segments$yend)
  mid <- (min(yy, na.rm = TRUE) + max(yy, na.rm = TRUE))
  home_plate_segments$y    <- mid - home_plate_segments$y
  home_plate_segments$yend <- mid - home_plate_segments$yend
}
strike_zone <- data.frame(xmin=-0.71, xmax=0.71, ymin=1.60, ymax=3.40)

# -------------------- Simple 9-bucket coloring (local rank) --------------------
parse_num <- function(v) { if (is.numeric(v)) v else suppressWarnings(readr::parse_number(as.character(v))) }
percent_rank_dir <- function(x, higher_is_better = TRUE) {
  v <- parse_num(x); ok <- is.finite(v); p <- rep(NA_real_, length(v)); n <- sum(ok)
  if (n >= 1) { r <- rank(v[ok], ties.method="average"); p[ok] <- (r - 0.5) / n
  if (!higher_is_better) p[ok] <- 1 - p[ok]; p[ok] <- pmin(pmax(p[ok],0),1) }
  p
}
bucket_color <- local({
  rgba <- function(hex, alpha){ rgb <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha) }
  function(p){
    if (is.na(p)) return(NA_character_)
    p <- pmin(pmax(p, 0), 1)
    severity <- abs((p - 0.5) * 2)
    if (severity < 0.08) return(NA_character_)
    alpha <- 0.16 + 0.72 * severity^0.80
    rgba(if (p >= 0.5) "#E33434" else "#5D7EBC", alpha)
  }
})
STAT_GOOD_FILL <- "rgba(46,125,50,0.30)"
STAT_BAD_FILL <- "rgba(214,40,40,0.30)"
STAT_GOOD_SOLID <- "#CFE8D2"
STAT_BAD_SOLID <- "#F4CCCC"

player_severity_hex <- function(score) {
  if (!is.finite(score)) return("")
  score <- pmin(pmax(score, -1), 1)
  severity <- abs(score)
  if (severity < 0.08) return("")
  base <- grDevices::col2rgb(if (score > 0) "#2E7D32" else "#D62828")
  alpha <- 0.16 + 0.72 * severity^0.80
  mixed <- round(255 * (1 - alpha) + base[, 1] * alpha)
  grDevices::rgb(mixed[1], mixed[2], mixed[3], maxColorValue = 255)
}
shade_columns9 <- function(df_in, cols, lower_better = character(0), percent_cols = character(0)) {
  out <- df_in; cols <- intersect(cols, names(out))
  for (nm in cols) {
    hib <- !(nm %in% lower_better)
    if (nm %in% percent_cols) {
      v <- parse_num(out[[nm]]) / 100; v <- pmin(pmax(v, 0), 1)
      p <- if (hib) v else (1 - v)
    } else {
      p <- percent_rank_dir(out[[nm]], higher_is_better = hib)
    }
    bg  <- vapply(p, bucket_color, FUN.VALUE = character(1))
    txt <- as.character(out[[nm]])
    out[[nm]] <- ifelse(is.na(p), txt,
                        sprintf("<span class='cf-cell' style='background-color:%s'>%s</span>", bg, txt))
  }
  out
}

# -------------------- Theme & CSS --------------------
txst_theme <- bs_theme(
  version   = 5, primary="#501214", secondary="#B4975A", bootswatch="flatly"
)
app_title_link <- tags$a(href="https://BobcatsHittingReports.com", target = "_blank",
                         class="app-title-link", "Bobcats Hitting Reports")

head_css <- htmltools::tags$head(
  htmltools::tags$style(HTML("
      :root { --txst-maroon:#501214; --txst-gold:#B4975A; }

      /* NAV + TITLE (keep maroon) */
      .navbar,.bslib-navbar,.bslib-page-header,.page-sidebar .navbar,.page-sidebar .navbar .container-fluid{
        background-color:var(--txst-maroon)!important;border:0!important}
      .navbar .navbar-brand,.navbar .nav-link,.bslib-navbar .navbar-brand,.bslib-page-header .navbar-brand,.app-title-link{
        color:var(--txst-gold)!important}
      .navbar,.bslib-navbar{box-shadow:none!important}
      .app-title-link{color:var(--txst-gold);font-weight:700;text-decoration:none}
      .app-title-link:hover{text-decoration:underline}

      /* WATERMARKS (background layers so all content sits ABOVE them) */
      body::after{
        content:'';position:fixed;inset:0;
        background-image:url('Bobcatlogo.png');             /* same name/path as pitching app */
        background-repeat:no-repeat;background-position:center;background-size:85vmin;
        opacity:0.7;pointer-events:none;z-index:0;
      }
      body::before{
        content:'';position:fixed;
        right:max(16px,env(safe-area-inset-right));bottom:max(16px,env(safe-area-inset-bottom));
        width:clamp(120px,22vmin,360px);height:clamp(120px,22vmin,360px);
        background-image:url('txstlogo.jpeg');              /* same name/path as pitching app */
        background-repeat:no-repeat;background-position:right bottom;background-size:contain;
        opacity:0.7;pointer-events:none;z-index:0;
      }

      /* Make all main containers sit above the background watermarks */
      .container-fluid,.bslib-grid,.page-sidebar,.nav-tabs,.tab-content,.card,.table{
        position:relative;z-index:1;
      }

      /* TABS */
      .tab-content{padding-bottom:clamp(120px,22vmin,360px)} /* leave room so bottom-right logo isn't covered */
      .nav-tabs{
        background-color:var(--txst-maroon)!important;border-bottom:2px solid var(--txst-maroon)!important;
        padding:.25rem .5rem;border-radius:.5rem;margin-bottom:.25rem}
      .nav-tabs .nav-link{color:var(--txst-gold)!important;background-color:transparent!important;border:0!important}
      .nav-tabs .nav-link:hover,.nav-tabs .nav-link:focus,.nav-tabs .nav-link.active{
        color:var(--txst-gold)!important;background-color:transparent!important;box-shadow: inset 0 -3px 0 0 var(--txst-gold)}

      /* BUTTONS (match pitching app) */
      .btn-primary{background-color:var(--txst-maroon)!important;border-color:var(--txst-maroon)!important}
      .btn-primary:hover{filter:brightness(0.9)}

      /* TABLES/CARDS: white (90% opaque) so they stay readable over watermark */
      table th,.table th{background-color:var(--txst-maroon)!important;color:var(--txst-gold)!important}
      .table tbody tr:hover{background-color:rgba(80,18,20,0.08)!important}
      
      .dt-row-odd  { background-color: #f9f9f9 !important; }
      .dt-row-even { background-color: #ffffff !important; }

      .game-picker-wrap .bootstrap-select .dropdown-toggle{
        height:auto; min-height:38px; white-space:normal;
      }
      .game-picker-wrap .bootstrap-select .filter-option-inner-inner{
        white-space:normal; overflow:visible; text-overflow:clip; line-height:1.2;
      }
      .game-picker-wrap .bootstrap-select .dropdown-menu{
        min-width:min(720px, calc(100vw - 40px))!important;
      }
      .game-picker-wrap .bootstrap-select .dropdown-menu li a span.text{
        white-space:normal; overflow:visible; text-overflow:clip;
      }

      .cf-cell{
        display:block;width:100%;min-height:32px;padding:6px 8px;margin:0;
        border:2px solid #fff;border-radius:4px;background-clip:padding-box;
        box-sizing:border-box;min-width:3ch;text-align:center;
      }
      /* Leaderboard: keep each conditional fill inset from its neighbors. */
      #hit_leaderboard_table td > .cf-cell{
        display:block;width:100%;min-height:32px;
        padding:6px 8px;margin:0;box-sizing:border-box;
      }

      /* Keep card/table containers white, but allow DT cells to override bg */
.table, .bslib-card, .card{
  background-color:rgba(255,255,255,0.9)!important;
  backdrop-filter:saturate(1) blur(0px);
  border-radius:8px;
}

/* Default body tables can stay white */
.table > :not(caption) > * > *{
  background-color:rgba(255,255,255,0.9);
}

/* For the Swing Decisions DT only, let cells set their own background */
.aar-table table.dataTable thead th,
.aar-table table.dataTable tbody td{
  background-color: inherit !important;
}
/* === FIX: keep Swing Decisions DT inside the card width === */
.aar-table .dataTables_wrapper{
  width:100% !important;
  max-width:100% !important;
}

.aar-table table.dataTable{
  width:100% !important;
  max-width:100% !important;
}

.aar-table .dataTables_scroll,
.aar-table .dataTables_scrollHead,
.aar-table .dataTables_scrollBody{
  width:100% !important;
  max-width:100% !important;
}

.aar-table .dataTables_scrollBody{
  overflow-x:auto !important;
}


      /* Keep plots tight like before */
      .tight-plot{padding:0;margin:0}
      
      /* DT group-header support (moved from prependContent to global head) */
table.dataTable thead tr.group-header th.group-label{
  text-align:center; font-weight:800; color:var(--txst-maroon);
  border-bottom:2px solid var(--txst-maroon) !important;
  background:rgba(255,255,255,0.92);
}
table.dataTable thead th.grp-start,
table.dataTable tbody td.grp-start{
  border-left:2px solid var(--txst-maroon) !important;
}
table.dataTable thead th, table.dataTable tbody td{
  border-color:transparent;
}
table.dataTable tbody td:first-child, table.dataTable thead th:first-child{
  text-align:left;
}

/* === AAR styling === */
.aar-title { font-weight:800; font-size:1.6rem; color:#501214; margin:0; }
.aar-subtitle { font-size:0.95rem; color:#501214; opacity:0.9; margin:0 0 .25rem 0; }
.aar-card { background:rgba(255,255,255,0.92)!important; border:0; border-radius:10px; padding:8px 10px; }
.aar-table th { background:#501214!important; color:#B4975A!important; font-weight:800!important; }
.aar-good { background:rgba(0,128,0,0.12); }

/* === Lineup Builder === */
.lineup-table td { vertical-align: middle; }
.lineup-table select { min-width: 120px; }
.lineup-total td { font-weight: 800; }
.lineup-bats-l { color:#1e90ff; }
.aar-bad  { background:rgba(255,0,0,0.12); }
.aar-kpi  { width:100%; border-collapse:separate; border-spacing:0; }
.aar-kpi th, .aar-kpi td { padding:8px 10px; border-bottom:1px solid rgba(0,0,0,0.06); }
.aar-kpi thead th { background:#501214; color:#B4975A; font-weight:800; }
.legend-box { font-size:0.85rem; }
/* HSH title bar */
.aar-kpi-title{
  background: var(--txst-maroon);
  color: var(--txst-gold);
  font-weight: 800;
  text-align: center;
  padding: 6px 8px;
  border-radius: 6px;
  margin: 6px 0 10px 0;
  font-size: 1rem;
}

/* Zebra rows for the HSH table (UI) */
.aar-kpi tbody tr:nth-child(odd){ background-color: #ffffff; }
.aar-kpi tbody tr:nth-child(even){ background-color: #f7f7f7; }


  "))
)
# --- Build game dropdown choices (safe) ---
make_games_txst <- function(d){
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(character(0))
  
  # Pick best available columns
  date_col <- intersect(c("game_date","GameDate","Date","date"), names(d))[1]
  opp_col  <- intersect(c("opponent","Opponent","Opp","AwayTeam","HomeTeam"), names(d))[1]
  game_col <- intersect(c("game_id","GameID","Game","game_pk","GamePk"), names(d))[1]
  
  dd <- d
  
  # Normalize date if present
  if (!is.na(date_col)) {
    dd[[date_col]] <- parse_date_any(dd[[date_col]])
  }
  
  key <- if (!is.na(game_col)) {
    as.character(dd[[game_col]])
  } else {
    paste0(
      if (!is.na(date_col)) as.character(dd[[date_col]]) else "UnknownDate",
      " | ",
      if (!is.na(opp_col))  as.character(dd[[opp_col]])  else "UnknownOpp"
    )
  }
  
  label <- if (!is.na(date_col) || !is.na(opp_col)) {
    paste(
      if (!is.na(date_col)) format(dd[[date_col]], "%Y-%m-%d") else "UnknownDate",
      if (!is.na(opp_col))  as.character(dd[[opp_col]])        else "UnknownOpp",
      sep = "  "
    )
  } else {
    key
  }
  
  out <- tapply(label, key, function(x) x[1])
  out <- out[order(names(out), decreasing = TRUE)]
  out
}

# Try to find your “main” data object to build games from (won't crash if not found)
dat_for_games <- NULL
for (nm in c("dat_all","dat_raw","dat","all_pitches","pitches","trackman","tm")) {
  if (exists(nm, inherits = TRUE)) { dat_for_games <- get(nm, inherits = TRUE); break }
}
if (is.null(dat_for_games)) dat_for_games <- data.frame()

games_txst <- make_games_txst(dat_for_games)

# --- Team Report (Hitters) game choices ---
team_report_games <- make_aar_games(txst_df)
if (!length(team_report_games)) team_report_games <- make_aar_games(df)

# ---- Leaderboard date defaults (safe) ----
leader_date_vec <- if ("GameDate" %in% names(txst_df)) {
  parse_date_any(txst_df$GameDate)
} else if ("Date" %in% names(txst_df)) {
  parse_date_any(txst_df$Date)
} else {
  parse_date_any(NA)
}
leader_date_vec <- leader_date_vec[!is.na(leader_date_vec)]
leader_date_min <- if (length(leader_date_vec)) min(leader_date_vec) else NULL
leader_date_max <- if (length(leader_date_vec)) max(leader_date_vec) else NULL

# =========================
# Performance table helpers (stable + self-contained)
# =========================

.pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

.get_chr <- function(df, candidates) {
  n <- nrow(df)
  nm <- .pick_col(df, candidates)
  if (is.na(nm)) return(rep(NA_character_, n))
  as.character(df[[nm]])
}

.get_num <- function(df, candidates) {
  n <- nrow(df)
  nm <- .pick_col(df, candidates)
  if (is.na(nm)) return(rep(NA_real_, n))
  to_num(df[[nm]])
}

.safe_mean <- function(x) if (length(x) && any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_

# --- robust pitch-level swing/whiff flags ---
.is_swing_flag <- function(pitch_call) {
  pc <- nz_chr(pitch_call)
  pc %in% c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut","FoulTip","FoulBallFieldable","FoulBallNotFieldable") |
    grepl("Swing", pc, ignore.case = TRUE) |
    grepl("^Foul", pc, ignore.case = TRUE) |
    grepl("InPlay", pc, ignore.case = TRUE)
}
.is_whiff_flag <- function(pitch_call) {
  pc <- nz_chr(pitch_call)
  pc %in% c("StrikeSwinging","StrikeSwingingBlocked") | grepl("StrikeSwinging", pc, ignore.case = TRUE)
}

# --- compute in_zone (if not already present) ---
.calc_in_zone_logical <- function(d){
  n <- nrow(d)
  if ("in_zone" %in% names(d) && is.logical(d$in_zone)) return(d$in_zone)
  
  sx_in <- if ("plate_x_in" %in% names(d)) to_num(d$plate_x_in) else rep(NA_real_, n)
  hz_in <- if ("plate_z_in" %in% names(d)) to_num(d$plate_z_in) else rep(NA_real_, n)
  
  if (any(!is.finite(sx_in))) {
    px <- if ("plate_x" %in% names(d)) d$plate_x else .get_num(d, c("PlateLocSide","px","PlateX","Plate_X","PlateLocSideInches"))
    sx_fill <- zone_s_in(px)
    sx_in[!is.finite(sx_in)] <- sx_fill[!is.finite(sx_in)]
  }
  if (any(!is.finite(hz_in))) {
    pz <- if ("plate_z" %in% names(d)) d$plate_z else .get_num(d, c("PlateLocHeight","pz","PlateZ","Plate_Z","PlateLocHeightInches"))
    hz_fill <- zone_h_in(pz)
    hz_in[!is.finite(hz_in)] <- hz_fill[!is.finite(hz_in)]
  }
  
  out <- rep(NA, n)
  ok <- is.finite(sx_in) & is.finite(hz_in)
  out[ok] <- in_zone_inches(sx_in[ok], hz_in[ok])
  out
}

# --- parse PA outcome for wOBA (PA-level) ---
.pa_event <- function(pr, kb, pc = NULL){
  prl <- tolower(trimws(nz_chr(pr)))
  kbl <- tolower(trimws(nz_chr(kb)))
  
  is_hbp <- grepl("hbp|hit\\s*by\\s*pitch", kbl) | grepl("hit\\s*by\\s*pitch|\\bhbp\\b", prl)
  # TrackMan commonly stores HBP only in PitchCall, with both result fields
  # left as Undefined. Count those PAs in actual and expected wOBA alike.
  if (!is.null(pc)) {
    is_hbp <- is_hbp | grepl("hit\\s*by\\s*pitch|\\bhbp\\b", tolower(nz_chr(pc)))
  }
  is_ibb <- grepl("\\bibb\\b|intentional", kbl) | grepl("intentional|\\bibb\\b", prl)
  is_bb  <- (!is_ibb) & (grepl("^bb$|walk", kbl) | grepl("\\bwalk\\b", prl))
  is_k   <- grepl("^k$|\\bso\\b|strikeout", kbl) | grepl("strikeout|\\bso\\b|\\bk\\b", prl)
  
  is_hr  <- grepl("home\\s*run|homerun|\\bhr\\b", prl)
  is_3b  <- grepl("\\b3b\\b|triple", prl)
  is_2b  <- grepl("\\b2b\\b|double", prl)
  is_1b  <- grepl("\\b1b\\b|single", prl)
  
  dplyr::case_when(
    is_hbp ~ "HBP",
    is_ibb ~ "IBB",
    is_bb  ~ "BB",
    is_hr  ~ "HR",
    is_3b  ~ "3B",
    is_2b  ~ "2B",
    is_1b  ~ "1B",
    is_k   ~ "K",
    TRUE   ~ "OUT"
  )
}

.woba_weight <- function(ev){
  dplyr::case_when(
    ev == "BB"  ~ woba_weights$BB,
    ev == "HBP" ~ woba_weights$HBP,
    ev == "1B"  ~ woba_weights$X1B,
    ev == "2B"  ~ woba_weights$X2B,
    ev == "3B"  ~ woba_weights$X3B,
    ev == "HR"  ~ woba_weights$HR,
    TRUE        ~ 0
  )
}

summarize_overall <- function(d){
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    return(tibble::tibble(
      PA = 0,
      wOBA = NA_real_, wOBAcon = NA_real_, xwOBA = NA_real_, xwOBAcon = NA_real_,
      OBP = NA_real_, SLG = NA_real_, OPS = NA_real_,
      hRV = NA_real_,
      `K%` = NA_real_, `BB%` = NA_real_, `Barrel%` = NA_real_,
      `Contact%` = NA_real_, `Z-Contact%` = NA_real_, `Whiff%` = NA_real_, `IZ-Whiff%` = NA_real_,
      `Swing%` = NA_real_, `IZ-Swing%` = NA_real_,
      `Chase%` = NA_real_, `Pre2K Chase%` = NA_real_, `2K Chase%` = NA_real_,
      MaxEV = NA_real_, `90th EV` = NA_real_, `EV>95%` = NA_real_, `10-35*%` = NA_real_,
      `GB%` = NA_real_, `LD%` = NA_real_, `FB%` = NA_real_, `PU%` = NA_real_,
      `Foul Ball%` = NA_real_, `LD+FB%` = NA_real_, `Airpull%` = NA_real_
    ))
  }
  
  n <- nrow(d)
  
  pitch_call <- dplyr::coalesce(
    .get_chr(d, c("pitch_call","PitchCall","Pitch_Call","PitchResult","Pitch_Result","Call")),
    rep(NA_character_, n)
  )
  play_result <- dplyr::coalesce(
    .get_chr(d, c("play_result","PlayResult","Play_Result","Result","Event")),
    rep(NA_character_, n)
  )
  
  # pitch-level flags
  is_swing <- if ("is_swing" %in% names(d)) as.logical(d$is_swing) else .is_swing_flag(pitch_call)
  is_whiff <- if ("is_whiff" %in% names(d)) as.logical(d$is_whiff) else .is_whiff_flag(pitch_call)
  in_zone  <- .calc_in_zone_logical(d)
  # Normalize in_zone to TRUE/FALSE/NA to avoid mixed types
  in_zone  <- ifelse(is.na(in_zone), NA, in_zone %in% TRUE)
  
  swings  <- sum(is_swing, na.rm = TRUE)
  whiffs  <- sum(is_whiff, na.rm = TRUE)
  contact <- sum(is_swing & !is_whiff, na.rm = TRUE)
  
  zone_swings <- is_swing & (in_zone %in% TRUE)
  foul_contact <- grepl("foul", nz_chr(pitch_call), ignore.case = TRUE)
  bip_contact <- grepl("in\\s*play|inplay|ball\\s*in\\s*play", nz_chr(pitch_call), ignore.case = TRUE)
  
  contact_pct   <- safe_ratio(contact, swings)
  whiff_pct     <- safe_ratio(whiffs, swings)
  swing_pct     <- safe_ratio(swings, n)
  izswing_pct   <- safe_ratio(sum(zone_swings, na.rm = TRUE), sum(in_zone %in% TRUE, na.rm = TRUE))
  zcontact_pct  <- safe_ratio(sum((is_swing & !is_whiff) & (in_zone %in% TRUE), na.rm = TRUE),
                              sum(zone_swings, na.rm = TRUE))
  izwhiff_pct   <- safe_ratio(sum(is_whiff & (in_zone %in% TRUE), na.rm = TRUE),
                              sum(zone_swings, na.rm = TRUE))
  foul_ball_pct <- safe_ratio(sum(foul_contact, na.rm = TRUE),
                              sum(foul_contact | bip_contact, na.rm = TRUE))
  
  # Chase% (O-Swing%): swings at out-of-zone / out-of-zone pitches
  out_zone <- in_zone %in% FALSE
  chase_pct <- safe_ratio(sum(is_swing & out_zone, na.rm = TRUE), sum(out_zone, na.rm = TRUE))
  
  # Pre2K / 2K chase using strikes-before if available
  strikes_b4 <- .get_num(d, c("StrikesBeforePitch","Strikes","StrikeCount","StrikesCount","PitcherStrikes"))
  pre2k <- is.finite(strikes_b4) & strikes_b4 < 2
  two_k <- is.finite(strikes_b4) & strikes_b4 == 2
  
  pre2k_den <- out_zone & pre2k
  two_k_den <- out_zone & two_k
  
  pre2k_chase <- safe_ratio(sum(is_swing & pre2k_den, na.rm = TRUE), sum(pre2k_den, na.rm = TRUE))
  two_k_chase <- safe_ratio(sum(is_swing & two_k_den, na.rm = TRUE), sum(two_k_den, na.rm = TRUE))
  
  # ---------- PA-level table ----------
  d2 <- d
  if (!("PA_ID" %in% names(d2))) d2$PA_ID <- make_pa_id(d2)
  
  pa_last <- d2 %>%
    dplyr::group_by(.data$PA_ID) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  
  PA <- nrow(pa_last)
  
  pr_last <- dplyr::coalesce(
    if ("play_result" %in% names(pa_last)) as.character(pa_last$play_result) else NA_character_,
    if ("PlayResult" %in% names(pa_last))  as.character(pa_last$PlayResult)  else NA_character_
  )
  kb_last <- if ("KorBB" %in% names(pa_last)) as.character(pa_last$KorBB) else NA_character_
  
  ev_pa <- if ("ev" %in% names(pa_last)) to_num(pa_last$ev) else .get_num(pa_last, c("EV","ExitSpeed","ExitVelocity","ExitVel","HitSpeed"))
  la_pa <- if ("la" %in% names(pa_last)) to_num(pa_last$la) else .get_num(pa_last, c("LA","Angle","LaunchAngle","launch_angle"))
  
  pc_last <- dplyr::coalesce(
    if ("pitch_call" %in% names(pa_last)) as.character(pa_last$pitch_call) else NA_character_,
    if ("PitchCall" %in% names(pa_last))  as.character(pa_last$PitchCall)  else NA_character_
  )
  
  ev_type <- .pa_event(pr_last, kb_last, pc_last)
  w       <- .woba_weight(ev_type)
  
  ibb_n <- sum(ev_type == "IBB", na.rm = TRUE)
  woba_den <- PA - ibb_n
  wOBA <- safe_ratio(sum(w, na.rm = TRUE), woba_den)
  
  # BIP (PA-level) for wOBAcon + barrel/EV/LA metrics
  bip_pa <- if (exists("is_bip_txst", inherits = TRUE)) {
    is_bip_txst(pc_last, pr_last)
  } else {
    grepl("(?i)in\\s*play|\\bbip\\b", nz_chr(pc_last)) |
      grepl("(?i)(1b|2b|3b|hr|single|double|triple|home\\s*run|out|error)", nz_chr(pr_last))
  }
  
  bip_den <- sum(bip_pa %in% TRUE, na.rm = TRUE)
  
  # wOBAcon: only BIP PA; BB/HBP/K excluded naturally by bip flag
  w_con <- ifelse(bip_pa %in% TRUE, w, 0)
  wOBAcon <- safe_ratio(sum(w_con, na.rm = TRUE), bip_den)

  # CAPS expected-wOBA logic: grid value on contact, fixed BB/HBP weights,
  # zero for strikeouts, and intentional walks excluded from the PA mean.
  contact_xw <- .get_num(pa_last, c("xwOBA", "xwoba", "ExpectedWOBA", "ExpectedwOBA"))
  grid_xw <- hitting_xwoba_lookup(ev_pa, la_pa)
  contact_xw[!is.finite(contact_xw)] <- grid_xw[!is.finite(contact_xw)]
  xwOBAcon <- .safe_mean(contact_xw[(bip_pa %in% TRUE) & is.finite(contact_xw)])
  pa_xw <- dplyr::case_when(
    ev_type == "IBB" ~ NA_real_,
    ev_type == "K" ~ 0,
    ev_type == "BB" ~ woba_weights$BB,
    ev_type == "HBP" ~ woba_weights$HBP,
    bip_pa %in% TRUE ~ contact_xw,
    TRUE ~ NA_real_
  )
  xwOBA <- .safe_mean(pa_xw)
  
  barrel_flag <- if (exists("is_barrel_txst", inherits = TRUE)) {
    is_barrel_txst(pc_last, pr_last, ev_pa, la_pa)
  } else {
    (bip_pa %in% TRUE) & is.finite(ev_pa) & is.finite(la_pa) & ev_pa >= 95.0 & la_pa >= 5 & la_pa <= 35
  }
  barrel_pct <- safe_ratio(sum(barrel_flag %in% TRUE, na.rm = TRUE), bip_den)
  
  ev95_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(ev_pa) & ev_pa >= 95, na.rm = TRUE), bip_den)

  max_ev <- if (bip_den > 0 && any(is.finite(ev_pa[bip_pa %in% TRUE]))) {
    max(ev_pa[bip_pa %in% TRUE], na.rm = TRUE)
  } else NA_real_
  
  ev90 <- if (bip_den > 0 && any(is.finite(ev_pa[bip_pa %in% TRUE]))) {
    suppressWarnings(as.numeric(stats::quantile(ev_pa[bip_pa %in% TRUE], 0.90, na.rm = TRUE)))
  } else NA_real_
  
  # Launch-angle rates use only classifiable BIP. A BIP with no measured LA
  # cannot be assigned to a bucket and therefore must not dilute all four.
  bip_la_den <- sum((bip_pa %in% TRUE) & is.finite(la_pa), na.rm = TRUE)
  ldfb_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa >= 10, na.rm = TRUE), bip_la_den)
  sweet_spot_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa >= 10 & la_pa <= 35, na.rm = TRUE), bip_la_den)
  gb_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa < 5, na.rm = TRUE), bip_la_den)
  ld_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa >= 5 & la_pa < 25, na.rm = TRUE), bip_la_den)
  fb_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa >= 25 & la_pa <= 50, na.rm = TRUE), bip_la_den)
  pu_pct <- safe_ratio(sum((bip_pa %in% TRUE) & is.finite(la_pa) & la_pa > 50, na.rm = TRUE), bip_la_den)
  
  # Airpull% (BIP + air (LA>=10) + pulled)
  bats_pa <- if ("bats" %in% names(pa_last)) nz_chr(pa_last$bats) else NA_character_
  bear_tm <- if ("bearing" %in% names(pa_last)) to_num(pa_last$bearing) else rep(NA_real_, PA)
  hx <- if ("hc_x" %in% names(pa_last)) to_num(pa_last$hc_x) else rep(NA_real_, PA)
  hy <- if ("hc_y" %in% names(pa_last)) to_num(pa_last$hc_y) else rep(NA_real_, PA)
  bear_xy <- ifelse(is.finite(hx) & is.finite(hy), atan2(hx, hy) * 180/pi, NA_real_)
  bearing_use <- dplyr::coalesce(bear_tm, bear_xy)
  
  pulled <- pulled_side(bearing_use, bats_pa)
  airpull_flag <- (bip_pa %in% TRUE) & (pulled %in% TRUE) & is.finite(la_pa) & la_pa >= 10
  airpull_pct <- safe_ratio(sum(airpull_flag, na.rm = TRUE), bip_den)
  
  # K% / BB% from PA-level outcomes
  k_pct  <- safe_ratio(sum(ev_type == "K", na.rm = TRUE), PA)
  bb_pct <- safe_ratio(sum(ev_type == "BB", na.rm = TRUE), PA)
  
  # OBP / SLG / OPS (PA-level)
  bb_n  <- sum(ev_type %in% c("BB","IBB"), na.rm = TRUE)
  hbp_n <- sum(ev_type == "HBP", na.rm = TRUE)
  sf_n  <- sum(grepl("(?i)sacrifice fly|\\bsf\\b", nz_chr(pr_last)), na.rm = TRUE)
  h_n   <- sum(ev_type %in% c("1B","2B","3B","HR"), na.rm = TRUE)
  
  ab_n <- PA - bb_n - hbp_n - sf_n
  if (!is.finite(ab_n) || ab_n < 0) ab_n <- 0
  
  tb_n <- sum(ev_type == "1B", na.rm = TRUE) +
    2 * sum(ev_type == "2B", na.rm = TRUE) +
    3 * sum(ev_type == "3B", na.rm = TRUE) +
    4 * sum(ev_type == "HR", na.rm = TRUE)
  
  obp <- safe_ratio(h_n + bb_n + hbp_n, ab_n + bb_n + hbp_n + sf_n)
  slg <- safe_ratio(tb_n, ab_n)
  ops <- ifelse(is.finite(obp) & is.finite(slg), obp + slg, NA_real_)
  
  # ---- Hitting RV (hRV) ----
  calc_hrv <- function(d_sub, p_den){
    if (is.null(d_sub) || !nrow(d_sub) || !is.finite(p_den) || p_den <= 0) return(NA_real_)
    if (!("PA_ID" %in% names(d_sub))) d_sub$PA_ID <- make_pa_id(d_sub)
    
    pa_last_sub <- d_sub %>%
      dplyr::group_by(.data$PA_ID) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    
    pr_last_sub <- dplyr::coalesce(
      if ("play_result" %in% names(pa_last_sub)) as.character(pa_last_sub$play_result) else NA_character_,
      if ("PlayResult" %in% names(pa_last_sub))  as.character(pa_last_sub$PlayResult)  else NA_character_
    )
    kb_last_sub <- if ("KorBB" %in% names(pa_last_sub)) as.character(pa_last_sub$KorBB) else NA_character_
    pc_last_sub <- dplyr::coalesce(
      if ("pitch_call" %in% names(pa_last_sub)) as.character(pa_last_sub$pitch_call) else NA_character_,
      if ("PitchCall" %in% names(pa_last_sub))  as.character(pa_last_sub$PitchCall)  else NA_character_
    )
    
    ev_sub <- .pa_event(pr_last_sub, kb_last_sub, pc_last_sub)
    tb <- sum(ev_sub == "1B", na.rm = TRUE) +
      2 * sum(ev_sub == "2B", na.rm = TRUE) +
      3 * sum(ev_sub == "3B", na.rm = TRUE) +
      4 * sum(ev_sub == "HR", na.rm = TRUE)
    bb <- sum(ev_sub %in% c("BB","IBB"), na.rm = TRUE)
    k  <- sum(ev_sub == "K", na.rm = TRUE)
    hr <- sum(ev_sub == "HR", na.rm = TRUE)
    
    # RBI = RunsScored on BallInPlay only
    rbi_vals <- .get_num(pa_last_sub, c("RunsScored","RunsScoredOnPlay","RunsScoredOnBallInPlay",
                                        "RunsScoredBIP","RunsScored_BIP","RBI","RBIs","RunsBattedIn","RBIResult"))
    bip_pa <- if (exists("is_bip_txst", inherits = TRUE)) {
      is_bip_txst(pc_last_sub, pr_last_sub)
    } else {
      grepl("(?i)in\\s*play|\\bbip\\b", nz_chr(pc_last_sub)) |
        grepl("(?i)(1b|2b|3b|hr|single|double|triple|home\\s*run|out|error)", nz_chr(pr_last_sub))
    }
    rbi <- if (all(is.na(rbi_vals))) NA_real_ else sum(rbi_vals[bip_pa %in% TRUE], na.rm = TRUE)
    if (!is.finite(rbi)) return(NA_real_)
    
    (((tb + bb - k) / 4) + rbi + hr) / p_den * 100
  }
  
  total_pitches <- nrow(d)
  hRV  <- calc_hrv(d, total_pitches)
  
  tibble::tibble(
    PA = PA,
    wOBA = wOBA,
    wOBAcon = wOBAcon,
    xwOBA = xwOBA,
    xwOBAcon = xwOBAcon,
    OBP = obp,
    SLG = slg,
    OPS = ops,
    hRV = hRV,
    `K%` = k_pct,
    `BB%` = bb_pct,
    `Barrel%` = barrel_pct,
    `Contact%` = contact_pct,
    `Z-Contact%` = zcontact_pct,
    `Whiff%` = whiff_pct,
    `IZ-Whiff%` = izwhiff_pct,
    `Swing%` = swing_pct,
    `IZ-Swing%` = izswing_pct,
    `Chase%` = chase_pct,
    `Pre2K Chase%` = pre2k_chase,
    `2K Chase%` = two_k_chase,
    MaxEV = max_ev,
    `90th EV` = ev90,
    `EV>95%` = ev95_pct,
    `10-35*%` = sweet_spot_pct,
    `GB%` = gb_pct,
    `LD%` = ld_pct,
    `FB%` = fb_pct,
    `PU%` = pu_pct,
    `Foul Ball%` = foul_ball_pct,
    `LD+FB%` = ldfb_pct,
    `Airpull%` = airpull_pct
  )
}

# Output used by output$perf_tbl when split_mode == "ptype"
summarize_by_pitchtype <- function(d){
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    return(tibble::tibble(
      PitchType = character(),
      PA = integer(),
      wOBA = double(), wOBAcon = double(), xwOBA = double(), xwOBAcon = double(),
      OBP = double(), SLG = double(), OPS = double(), hRV = double(),
      `K%` = double(), `BB%` = double(), `Barrel%` = double(),
      `Contact%` = double(), `Z-Contact%` = double(), `Whiff%` = double(), `IZ-Whiff%` = double(),
      `Swing%` = double(), `IZ-Swing%` = double(),
      `Chase%` = double(), `Pre2K Chase%` = double(), `2K Chase%` = double(),
      MaxEV = double(), `90th EV` = double(), `EV>95%` = double(), `10-35*%` = double(),
      `GB%` = double(), `LD%` = double(), `FB%` = double(), `PU%` = double(),
      `Foul Ball%` = double(), `LD+FB%` = double(), `Airpull%` = double()
    ))
  }
  
  n <- nrow(d)
  pt_raw <- dplyr::coalesce(
    if ("PitchType" %in% names(d)) as.character(d$PitchType) else rep(NA_character_, n),
    if ("PitchType_UNI" %in% names(d)) as.character(d$PitchType_UNI) else rep(NA_character_, n),
    if ("pitch_type_canon" %in% names(d)) as.character(d$pitch_type_canon) else rep(NA_character_, n)
  )
  pt_chr <- canonical_pitch_fuzzy(pt_raw)
  pt_chr[is.na(pt_chr) | !(pt_chr %in% pitch_levels_all)] <- "Undefined"
  
  d$.__pt <- pt_chr
  pts <- intersect(pitch_levels_all, unique(pt_chr))
  
  out <- dplyr::bind_rows(lapply(pts, function(pt){
    dd <- d[d$.__pt == pt, , drop = FALSE]
    summarize_overall(dd) %>% dplyr::mutate(PitchType = pt, .before = 1)
  }))
  
  totals <- summarize_overall(d) %>% dplyr::mutate(PitchType = "Totals", .before = 1)
  
dplyr::bind_rows(out, totals)
}

performance_timeseries_choices <- c(
  "wOBA", "wOBAcon", "xwOBA", "xwOBAcon", "OBP", "SLG", "OPS",
  "K%", "BB%", "Barrel%",
  "Swing%", "IZ-Swing%", "Whiff%", "IZ-Whiff%",
  "Chase%", "Pre2K Chase%", "2K Chase%",
  "MaxEV", "90th EV", "EV>95%", "10-35*%", "GB%", "LD%", "FB%", "PU%", "Foul Ball%", "Airpull%",
  "Heater Usage", "Breaker Usage", "Soft Usage"
)

performance_table_metrics <- c(
  "PA", "wOBA", "wOBAcon", "xwOBA", "xwOBAcon", "OBP", "SLG", "OPS", "hRV",
  "K%", "BB%", "Barrel%",
  "Swing%", "IZ-Swing%", "Whiff%", "IZ-Whiff%", "Chase%", "Pre2K Chase%", "2K Chase%",
  "MaxEV", "90th EV", "EV>95%", "10-35*%", "GB%", "LD%", "FB%", "PU%", "Foul Ball%", "Airpull%"
)

contact_type_levels <- c("Pop Up", "Fly Ball", "Line Drive", "Ground Ball")
result_type_levels <- c("Out/Error/FC", "1B", "2B", "3B", "HR")
contact_shape_values <- c("Pop Up"=21, "Fly Ball"=22, "Line Drive"=24, "Ground Ball"=23)
contact_legend_shape_values <- c("Pop Up"=1, "Fly Ball"=0, "Line Drive"=2, "Ground Ball"=5)
result_fill_values <- c("Out/Error/FC"="#1E88E5", "1B"="#2E7D32", "2B"="#FDD835", "3B"="#FB8C00", "HR"="#E53935")
result_type_labels <- c("Out/Error/FC", "Single", "Double", "Triple", "Home Run")
spray_xlim <- c(-285, 285)
spray_ylim <- c(0, 500)

spray_contact_type <- function(la){
  la <- to_num(la)
  dplyr::case_when(
    is.finite(la) & la >= 50 ~ "Pop Up",
    is.finite(la) & la >= 25 ~ "Fly Ball",
    is.finite(la) & la >= 10 ~ "Line Drive",
    is.finite(la)            ~ "Ground Ball",
    TRUE                     ~ NA_character_
  )
}

spray_result_bucket <- function(pr){
  pr <- nz_chr(pr)
  dplyr::case_when(
    grepl("(?i)home\\s*run|homerun|\\bHR\\b", pr) ~ "HR",
    grepl("(?i)\\b3B\\b|triple", pr)              ~ "3B",
    grepl("(?i)\\b2B\\b|double", pr)              ~ "2B",
    grepl("(?i)\\b1B\\b|single", pr)              ~ "1B",
    TRUE                                           ~ "Out/Error/FC"
  )
}

pitch_usage_group <- function(pt){
  pt <- canonical_pitch_fuzzy(pt)
  dplyr::case_when(
    pt %in% c("Fastball", "Sinker") ~ "Heater Usage",
    pt %in% c("Cutter", "Slider", "Sweeper", "Curveball") ~ "Breaker Usage",
    pt %in% c("Changeup", "Splitter") ~ "Soft Usage",
    TRUE ~ NA_character_
  )
}

summarize_pitch_usage <- function(d){
  if (is.null(d) || !nrow(d)) {
    return(tibble::tibble(`Heater Usage` = NA_real_, `Breaker Usage` = NA_real_, `Soft Usage` = NA_real_))
  }
  n <- nrow(d)
  pt_raw <- dplyr::coalesce(
    if ("PitchType" %in% names(d)) as.character(d$PitchType) else rep(NA_character_, n),
    if ("PitchType_UNI" %in% names(d)) as.character(d$PitchType_UNI) else rep(NA_character_, n),
    if ("pitch_type_canon" %in% names(d)) as.character(d$pitch_type_canon) else rep(NA_character_, n)
  )
  grp <- pitch_usage_group(pt_raw)
  den <- sum(!is.na(grp))
  tibble::tibble(
    `Heater Usage` = safe_ratio(sum(grp == "Heater Usage", na.rm = TRUE), den),
    `Breaker Usage` = safe_ratio(sum(grp == "Breaker Usage", na.rm = TRUE), den),
    `Soft Usage` = safe_ratio(sum(grp == "Soft Usage", na.rm = TRUE), den)
  )
}


# BASE supplies a scoped theme/head when this file is hosted as a workspace.
# The standalone app retains its original title, theme, and page structure.
if (isTRUE(get0("BASE_HITTING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
  txst_theme <- get0(
    "BASE_HITTING_THEME",
    inherits = FALSE,
    ifnotfound = txst_theme
  )
  head_css <- get0(
    "BASE_HITTING_HEAD",
    inherits = FALSE,
    ifnotfound = head_css
  )
  app_title_link <- NULL
  base_hitting_page <- function(..., title = NULL, sidebar = NULL, theme = NULL) {
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
        class = "base-hitting-embedded-layout",
        htmltools::tags$aside(
          class = "base-hitting-sidebar",
          sidebar_children
        ),
        htmltools::tags$main(
          class = "base-hitting-main",
          dots[!is_head_item]
        )
      )
    )
  }
} else {
  base_hitting_page <- page_sidebar
}

base_hitting_postgame_ui <- tagList(
  if (isTRUE(get0("BASE_HITTING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
    div(
      class = "aar-card",
      style = "display:flex; gap:12px; align-items:end; flex-wrap:wrap;",
      selectInput(
        "aar_hitter", "Hitter", choices = hitters_txst,
        selected = if (length(hitters_txst)) hitters_txst[[1]] else NULL,
        width = "280px"
      ),
      checkboxGroupInput(
        "hit_aar_season_groups", "Quick-select seasons",
        choices = SEASON_CHOICES, selected = "S26", inline = TRUE
      )
    )
  },
  div(
    class="mb-2 d-flex align-items-center justify-content-between",
    div(
      style="display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
      selectInput(
        "AARGame",
        "Game (most recent first)",
        choices = character(0),
        selected = NULL,
        width = "420px"
      ),
      downloadButton("hit_aar_pdf", "Download AAR (PDF)", class = "btn btn-primary")
    )
  ),
  uiOutput("aar_header"),
  fluidRow(
    column(
      width = 6,
      div(
        class="aar-card aar-table",
        style="width:100%; overflow-x:auto;",
        DTOutput("aar_table")
      )
    ),
    column(
      width = 6,
      div(class="aar-card", plotOutput("aar_strike", height = "620px"))
    )
  ),
  fluidRow(
    column(
      width = 4,
      div(class="aar-card", plotOutput("aar_spray", height = "520px"))
    ),
    column(
      width = 4,
      div(class="aar-card", plotOutput("aar_contact", height = "520px"))
    ),
    column(
      width = 4,
      div(class="aar-card", uiOutput("aar_kpi"))
    )
  )
)

# -------------------- UI --------------------
ui <- base_hitting_page(
  theme = txst_theme,
  title = app_title_link,
  head_css,
  tags$script(HTML("
    function baseKeepHittingTabVisible(link){
      var $link=$(link), nav=$link.closest('.nav-tabs')[0];
      if(!nav) return;
      var left=$link.position().left+nav.scrollLeft;
      var target=Math.max(0,left-(nav.clientWidth-$link.outerWidth())/2);
      nav.scrollTo({left:target,behavior:'smooth'});
    }
    $(document).on('shown.bs.tab','a[data-bs-toggle=\"tab\"],a[data-toggle=\"tab\"]',function(e){
      if($(e.target).closest('.base-hitting-main').length) baseKeepHittingTabVisible(e.target);
    });
    $(function(){ window.setTimeout(function(){
      $('.base-hitting-main .nav-tabs .nav-link.active').each(function(){baseKeepHittingTabVisible(this);});
    },150); });
  ")),
  sidebar = sidebar(
    title = "Select Hitter / Game / Splits",
    selectInput("Hitter", "Hitter", choices = hitters_txst, selected = hitters_txst[[1]]),
    checkboxGroupInput(
      "hit_season_groups", "Quick-select seasons",
      choices  = SEASON_CHOICES,
      selected = "S26",
      inline   = TRUE
    ),
    div(
      class = "game-picker-wrap",
      pickerInput("Game", HTML("Select Game<br>(selects all by default)"),
                  choices = character(0), selected = character(0),
                  width = "100%",
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count",
                    `count-selected-text` = "{0} games selected",
                    title = "Select games",
                    size = 12
                  ),
                  multiple = TRUE)
    ),
    selectInput("PitcherHand", "Pitcher Handedness", choices = c("All","LHP","RHP"), selected = "All")
  ),
  navset_tab(
    nav_panel(
      title = "Performance",
      div(
        class = "base-hitting-performance-controls",
        radioGroupButtons(
          inputId  = "hit_perf_split",
          label    = "Split performance by",
          choices  = c("Pitcher Handedness" = "hand", "Pitch Type" = "ptype"),
          selected = "hand",
          justified = TRUE, size = "sm"
        )
      ),
      div(
        class = "base-hitting-performance-table",
        div(
          class = "base-hitting-performance-heading",
          div(
            span("Season snapshot"),
            strong("Hitter performance")
          ),
          tags$small("Results, swing decisions, and batted-ball quality in one view")
        ),
        withSpinner(DTOutput("perf_tbl"), type = 4, color = "#501214")
      ),
      div(
        class = "mt-3",
        selectizeInput(
          "hit_perf_ts_stats", "Time Series Stats",
          choices = performance_timeseries_choices,
          selected = c("wOBA", "Barrel%", "Whiff%", "Chase%"),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Choose time-series metrics"),
          width = "100%"
        ),
        withSpinner(plotOutput("perf_time_series", height = "520px"), type = 4, color = "#501214")
      )
    ),
    nav_panel(
      title = "Lineup Builder",
      div(
        class = "aar-card",
        div(
          class = "mb-2",
          actionButton("lineup_clear_all", "Clear All", class = "btn btn-outline-secondary btn-sm")
        ),
        div(
          class = "mb-2",
          "Build a 9-slot lineup from the selected season. Positions and players are unique; totals are PA-weighted."
        ),
        uiOutput("lineup_builder_ui")
      )
    ),
    nav_panel(
      title = "Damage Heat Map",
      tabsetPanel(
        tabPanel(
          "Total Heat Map",
          div(class="tight-plot p-0 m-0",
              withSpinner(plotOutput("damage_heat_total", height="700px"), type=4, color="#501214"))
        ),
        tabPanel(
          "Pitch Type Filter",
          selectizeInput(
            "damage_ptypes", "Pitch Types",
            choices = facet_levels,
            selected = facet_levels,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Filter pitch types"),
            width = "100%"
          ),
          div(class="tight-plot p-0 m-0",
              withSpinner(plotOutput("damage_heat_filter", height="700px"), type=4, color="#501214"))
        )
      )
    ),
    nav_panel(
      title = "Whiff Zones",
      tabsetPanel(
        tabPanel(
          "Total Heat Map",
          div(class="tight-plot p-0 m-0",
              withSpinner(plotOutput("whiff_heat_total", height="700px"), type=4, color="#501214"))
        ),
        tabPanel(
          "Pitch Type Filter",
          selectizeInput(
            "whiff_ptypes", "Pitch Types",
            choices = facet_levels,
            selected = facet_levels,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Filter pitch types"),
            width = "100%"
          ),
          div(class="tight-plot p-0 m-0",
              withSpinner(plotOutput("whiff_heat_filter", height="700px"), type=4, color="#501214"))
        )
      )
    ),
    
    nav_panel(
      title = "Team Report",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "team_report_game",
            "Game:",
            choices = team_report_games,
            selected = if (length(team_report_games)) team_report_games[[1]] else NULL,
            multiple = FALSE,
            selectize = FALSE
          ),
          downloadButton("hit_team_report_dl", "Download Team Report PDF")
        ),
        mainPanel(
          imageOutput("hit_team_report_preview", height = "1000px")
        )
      )
    ),
    if (!isTRUE(get0("BASE_HITTING_EMBEDDED", inherits = FALSE, ifnotfound = FALSE))) {
      nav_panel(title = "Game Reports (AAR)", base_hitting_postgame_ui)
    },
    
    nav_panel(
      title = "Leaderboard",
      div(
        class = "mb-2",
        fluidRow(
          column(
            6,
            checkboxGroupInput(
              "hit_leader_seasons", "Seasons",
              choices  = SEASON_CHOICES,
              selected = "S26",
              inline   = TRUE
            )
          ),
          column(
            3,
            checkboxGroupInput(
              "hit_leader_hand", "Pitcher Hand",
              choices  = c("v LHP" = "LHP", "v RHP" = "RHP"),
              selected = c("LHP","RHP"),
              inline   = TRUE
            )
          ),
          column(
            3,
            dateRangeInput(
              "hit_leader_dates", "Date range",
              start = leader_date_min,
              end   = leader_date_max,
              min   = leader_date_min,
              max   = leader_date_max
            )
          )
        )
      ),
      div(
        class = "mb-2 d-flex gap-2",
        downloadButton("leaderboard_pdf", "Download Leaderboard PDF", class = "btn btn-primary")
      ),
      withSpinner(DTOutput("hit_leaderboard_table"), type = 4, color = "#501214"),
      withSpinner(DTOutput("leaderboard_team_table"), type = 4, color = "#501214")
    ),
    
    nav_panel(
      title = "Swing Decisions",
      selectizeInput(
        "sd_ptypes", "Pitch Types",
        choices = pitch_levels_all,
        selected = pitch_levels_all,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Filter pitch types"),
        width = "100%"
      ),
      tabsetPanel(
        tabPanel(
          "All Counts",
          fluidRow(
            column(6,
                   withSpinner(plotOutput("sd_strikes_taken", height="520px"), type=4, color="#501214")
            ),
            column(6,
                   withSpinner(plotOutput("sd_balls_chased", height="520px"), type=4, color="#501214")
            )
          )
        ),
        tabPanel(
          "Count Filters",
          selectizeInput(
            "sd_count_groups", "Counts",
            choices = c(
              "First pitch (0-0)" = "first",
              "Evens (1-1, 2-2)" = "evens",
              "Ahead (1-0, 2-0, 3-0, 2-1, 3-1)" = "ahead",
              "Behind (0-1, 0-2, 1-2)" = "behind",
              "2K (0-2, 1-2, 2-2, 3-2)" = "twok"
            ),
            selected = c("first", "evens", "ahead", "behind", "twok"),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Filter count groups"),
            width = "100%"
          ),
          fluidRow(
            column(6,
                   withSpinner(plotOutput("sd_count_strikes_taken", height="520px"), type=4, color="#501214")
            ),
            column(6,
                   withSpinner(plotOutput("sd_count_balls_chased", height="520px"), type=4, color="#501214")
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Ball Flight",
      div(
        class = "mb-2",
        radioGroupButtons(
          inputId  = "spray_split",
          label    = "View",
          choices  = c("Total" = "total", "By Pitch Type" = "ptype"),
          selected = "ptype",
          justified = TRUE, size = "sm"
        )
      ),
      selectizeInput(
        "ev_bins", "Exit Velocity",
        choices = c("<80"="<80","80-90"="80-90","90-95"="90-95","95-100"="95-100","100+"="100+"),
        selected = c("<80","80-90","90-95","95-100","100+"),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Filter exit velocity"),
        width = "100%"
      ),
      selectizeInput(
        "spray_contact_types", "Contact Type",
        choices = contact_type_levels,
        selected = contact_type_levels,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Filter contact types"),
        width = "100%"
      ),
      selectizeInput(
        "spray_result_types", "Result Type",
        choices = result_type_levels,
        selected = result_type_levels,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Filter results"),
        width = "100%"
      ),
      
      conditionalPanel(
        condition = "input.spray_split == 'total'",
        div(style="width:100%;",
            withSpinner(plotOutput("spray_chart", height="750px", width="100%"), type=4, color="#501214")
        )
      ),
      
      conditionalPanel(
        condition = "input.spray_split == 'ptype'",
        uiOutput("spray_grid")
      )
    ),
    
    nav_panel(
      title = "Contact Point",
      tabsetPanel(
        tabPanel(
          "Total",
          div(style="width:100%;",
              withSpinner(plotlyOutput("contact_point_total", height="750px"), type=4, color="#501214")),
          div(class="mt-2",
              withSpinner(DTOutput("contact_point_total_tbl"), type=4, color="#501214"))
        ),
        tabPanel(
          "By Pitch Type",
          uiOutput("contact_point_grid")
        )
      )
    )
  )
)

# -------------------- Helpers for summaries --------------------
make_pa_id <- function(d){
  if (all(c("Inning","PAofInning","Batter") %in% names(d))) {
    game_key <- coalesce_nz_chr(
      resolve_game_key(d),
      if ("CustomGameID" %in% names(d)) d$CustomGameID else NULL,
      if ("Date" %in% names(d)) d$Date else NULL
    )
    half_col <- intersect(c("Top/Bottom", "TopBottom", "HalfInning"), names(d))[1]
    half_key <- if (!is.na(half_col)) d[[half_col]] else rep(NA_character_, nrow(d))
    interaction(game_key, half_key, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
  } else if ("PitchofPA" %in% names(d)) {
    cumsum(d$PitchofPA == 1)
  } else {
    terminal <- d$pitch_call %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled") |
      (!is.na(d$play_result) & nzchar(d$play_result))
    cumsum(c(1L, head(terminal, -1)))
  }
}
abbr_name <- function(x){
  x <- trimws(x)
  ifelse(grepl(",", x),
         {parts <- strsplit(x, ",\\s*"); vapply(parts, function(p){
           if (length(p) >= 2) paste0(substr(p[2],1,1), ". ", p[1]) else x
         }, "", USE.NAMES = FALSE)},
         {parts <- strsplit(x, "\\s+"); vapply(parts, function(p){
           if (length(p) >= 2) paste0(substr(p[1],1,1), ". ", p[length(p)]) else x
         }, "", USE.NAMES = FALSE)}
  )
}

pa_last_table <- function(d, balls_col, strikes_col){
  
  n <- nrow(d)
  
  get_num_safe <- function(nm){
    if (!is.na(nm) && nm %in% names(d)) to_num(d[[nm]]) else rep(NA_real_, n)
  }
  
  d2 <- d %>%
    dplyr::mutate(
      balls_b4_raw   = get_num_safe(balls_col),
      strikes_b4_raw = get_num_safe(strikes_col),
      
      ev_pa = to_num(.data$ev),
      la_pa = to_num(.data$la),
      
      bearing_tm = to_num(.data$bearing),
      bearing_xy = {
        hx <- to_num(.data$hc_x); hy <- to_num(.data$hc_y)
        ifelse(is.finite(hx) & is.finite(hy), atan2(hx, hy) * 180/pi, NA_real_)
      },
      bearing_pa = dplyr::coalesce(bearing_tm, bearing_xy),
      dist_pa    = to_num(.data$distance_ft),
      bats_pa    = if ("bats" %in% names(.)) nz_chr(.data$bats) else NA_character_,
      
      plot_x_pa = {
        use_tm <- is.finite(dist_pa) & is.finite(bearing_pa)
        ifelse(use_tm, dist_pa * sin(bearing_pa * pi/180), to_num(.data$hc_x))
      },
      plot_y_pa = {
        use_tm <- is.finite(dist_pa) & is.finite(bearing_pa)
        ifelse(use_tm, dist_pa * cos(bearing_pa * pi/180), to_num(.data$hc_y))
      }
    )
  
  d2$PA_ID <- make_pa_id(d2)
  
  out <- d2 %>%
    dplyr::group_by(.data$PA_ID) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pr = nz_chr(.data$play_result),
      kb = if ("KorBB" %in% names(.)) nz_chr(.data$KorBB) else "",
      pc = nz_chr(.data$pitch_call),
      
      balls_b4   = .data$balls_b4_raw,
      strikes_b4 = .data$strikes_b4_raw,
      
      .s_in = if ("plate_x_in" %in% names(.)) to_num(.data$plate_x_in) else zone_s_in(.data$plate_x),
      .h_in = if ("plate_z_in" %in% names(.)) to_num(.data$plate_z_in) else zone_h_in(.data$plate_z),
      
      in_zone = dplyr::case_when(
        is.finite(.data$.s_in) & is.finite(.data$.h_in) ~ in_zone_inches(.data$.s_in, .data$.h_in),
        TRUE ~ NA
      )
    ) %>%
    dplyr::select(-dplyr::all_of(c(".s_in", ".h_in")))
  
  out
}

# =========================
# Pitch-level swing table used by AAR KPI + AAR swing mini table
# =========================
swing_table <- function(d, balls_col = NA_character_, strikes_col = NA_character_) {
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    return(tibble::tibble(
      is_swing   = logical(0),
      is_whiff   = logical(0),
      is_contact = logical(0),
      in_zone    = logical(0),
      pre2k      = logical(0),
      two_k      = logical(0),
      balls_b4   = numeric(0),
      strikes_b4 = numeric(0)
    ))
  }
  
  n <- nrow(d)
  
  # Pitch call (prefer canonical pitch_call)
  pitch_call <- dplyr::coalesce(
    if ("pitch_call" %in% names(d)) as.character(d$pitch_call) else rep(NA_character_, n),
    if ("PitchCall"  %in% names(d)) as.character(d$PitchCall)  else rep(NA_character_, n)
  )
  pitch_call <- nz_chr(pitch_call)
  
  # Zone (inches) with safe fallbacks
  sx_in <- if ("plate_x_in" %in% names(d)) {
    to_num(d$plate_x_in)
  } else if ("plate_x" %in% names(d)) {
    zone_s_in(to_num(d$plate_x))
  } else {
    rep(NA_real_, n)
  }
  
  hz_in <- if ("plate_z_in" %in% names(d)) {
    to_num(d$plate_z_in)
  } else if ("plate_z" %in% names(d)) {
    zone_h_in(to_num(d$plate_z))
  } else {
    rep(NA_real_, n)
  }
  
  in_zone <- rep(NA, n)
  ok <- is.finite(sx_in) & is.finite(hz_in)
  in_zone[ok] <- in_zone_inches(sx_in[ok], hz_in[ok])
  
  # Fallback to precomputed in_zone if plate coords missing
  if ("in_zone" %in% names(d)) {
    in_zone <- ifelse(is.na(in_zone), as.logical(d$in_zone), in_zone)
  }
  
  # Swing / whiff / contact flags (derive if missing)
  is_swing <- if ("is_swing" %in% names(d)) as.logical(d$is_swing) else .is_swing_flag(pitch_call)
  is_whiff <- if ("is_whiff" %in% names(d)) as.logical(d$is_whiff) else .is_whiff_flag(pitch_call)
  is_contact <- if ("is_contact" %in% names(d)) as.logical(d$is_contact) else (is_swing & !is_whiff)
  
  # Balls/strikes before pitch (use provided column names when present)
  balls_b4 <- rep(NA_real_, n)
  if (!is.na(balls_col) && balls_col %in% names(d)) balls_b4 <- to_num(d[[balls_col]])
  
  strikes_b4 <- rep(NA_real_, n)
  if (!is.na(strikes_col) && strikes_col %in% names(d)) strikes_b4 <- to_num(d[[strikes_col]])
  
  # If we still don't have strikes-before, rebuild within PA
  if (!any(is.finite(strikes_b4))) {
    d2 <- d
    if (!("PA_ID" %in% names(d2))) d2$PA_ID <- make_pa_id(d2)
    
    adds_strike <- pitch_call %in% c(
      "StrikeSwinging", "StrikeCalled", "FoulTip",
      "FoulBallFieldable", "FoulBallNotFieldable"
    )
    
    strikes_b4 <- d2 %>%
      dplyr::mutate(.adds_strike = adds_strike) %>%
      dplyr::group_by(.data$PA_ID) %>%
      dplyr::mutate(.sb = dplyr::lag(cumsum(.adds_strike), default = 0)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(.sb)
  }
  
  pre2k <- is.finite(strikes_b4) & strikes_b4 < 2
  two_k <- is.finite(strikes_b4) & strikes_b4 == 2
  
  tibble::tibble(
    is_swing   = is_swing,
    is_whiff   = is_whiff,
    is_contact = is_contact,
    in_zone    = in_zone,
    pre2k      = pre2k,
    two_k      = two_k,
    balls_b4   = balls_b4,
    strikes_b4 = strikes_b4
  )
}

# -------------------- Team Report (Hitters) helpers --------------------
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
title_strip_plot <- .title_strip_plot

txst_table_header_style <- function(tbl, n_cols) {
  tbl <- .tbl_bg(tbl,   row = 1, column = 1:n_cols, fill  = "#501214")
  tbl <- .tbl_font(tbl, row = 1, column = 1:n_cols, face = "bold", color = "#B4975A", size = 10.5)
  for (j in seq_len(n_cols)) {
    tbl <- .tbl_bg(tbl, row = 1, column = j, fill = "#501214", color = "#C8C8C8", linewidth = 0.7)
  }
  tbl
}

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

as_txst_table <- function(df) {
  tbl <- ggpubr::ggtexttable(df, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df))
  tbl <- txst_table_polish(tbl, df)
  tbl
}

txst_process_table <- function(df, season_col_label = "Season") {
  if (is.null(df) || !inherits(df, c("data.frame","tbl_df","tbl")) || !nrow(df)) {
    df <- data.frame(Metric = "Process metrics unavailable", Game = NA, Season = NA, D1 = NA, stringsAsFactors = FALSE)
  } else {
    if (!inherits(df, "data.frame")) df <- as.data.frame(df)
    nms <- names(df)
    nms[tolower(nms) == "metric"] <- "Metric"
    nms[tolower(nms) == "game"]   <- "Game"
    nms[tolower(nms) == "season"] <- "Season"
    nms[tolower(nms) == "d1"]     <- "D1"
    if (!("Season" %in% nms) && nzchar(season_col_label) && season_col_label %in% nms) {
      nms[nms == season_col_label] <- "Season"
    }
    if (!("Season" %in% nms)) {
      season_idx <- which(!(nms %in% c("Metric","Game","D1")))[1]
      if (is.finite(season_idx)) nms[season_idx] <- "Season"
    }
    names(df) <- nms
  }
  
  df_render <- df
  if ("Season" %in% names(df_render) && nzchar(season_col_label)) {
    names(df_render)[names(df_render) == "Season"] <- season_col_label
  }
  
  tbl <- ggpubr::ggtexttable(df_render, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df_render))
  tbl <- txst_table_polish(tbl, df_render)
  
  to_frac <- function(x) {
    v <- suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (!length(v)) return(v)
    if (all(is.na(v))) return(v)
    if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v <- v / 100
    v
  }
  
  col_game <- which(names(df_render) == "Game")[1]
  col_seas <- which(names(df_render) == season_col_label)[1]
  col_d1   <- which(names(df_render) == "D1")[1]
  # Force fixed positions for Process Metrics (Metric | Game | Season | D1)
  if (ncol(df_render) >= 4) {
    col_game <- 2L
    col_seas <- 3L
    col_d1   <- 4L
  } else {
    if (!is.finite(col_game) && ncol(df_render) >= 2) col_game <- 2L
    if (!is.finite(col_seas) && ncol(df_render) >= 3) col_seas <- 3L
    if (!is.finite(col_d1)   && ncol(df_render) >= 4) col_d1   <- 4L
  }
  if (!is.finite(col_game) || !is.finite(col_seas) || !is.finite(col_d1)) return(tbl)
  
  v_game <- to_frac(df$Game)
  v_seas <- to_frac(df$Season)
  v_d1   <- to_frac(df$D1)
  
  for (i in seq_len(nrow(df))) {
    metric <- as.character(df$Metric[i])
    lower_better <- metric %in% c("Chase%","Whiff%")
    fill_g <- d1_shade_fill(v_game[i], v_d1[i], lower_better)
    fill_s <- d1_shade_fill(v_seas[i], v_d1[i], lower_better)
    if (nzchar(fill_g)) tbl <- .tbl_bg(tbl, row = i + 1, column = col_game, fill = fill_g, color = "#BEBEBE", linewidth = 0.6)
    if (nzchar(fill_s)) tbl <- .tbl_bg(tbl, row = i + 1, column = col_seas, fill = fill_s, color = "#BEBEBE", linewidth = 0.6)
    
    # Explicitly shade Season column in case of header/column mismatch
    if (nzchar(fill_s) && ncol(df_render) >= 3) {
      tbl <- .tbl_bg(tbl, row = i + 1, column = 3L, fill = fill_s, color = "#BEBEBE", linewidth = 0.6)
    }
  }
  
  tbl
}

txst_count_breakdown_table <- function(df) {
  if (is.null(df) || !inherits(df, c("data.frame","tbl_df","tbl")) || !nrow(df)) {
    return(as_txst_table(data.frame(Status = "No data")))
  }
  df <- as.data.frame(df)
  
  tbl <- ggpubr::ggtexttable(df, rows = NULL, theme = ggpubr::ttheme("blank"))
  tbl <- txst_table_header_style(tbl, ncol(df))
  tbl <- txst_table_polish(tbl, df)
  
  metric_labels <- as.character(df[[1]])
  
  d1_refs <- list(
    "Whiff%"  = D1_PCT[["Whiff%"]],
    "Chase%"  = D1_PCT[["Chase%"]],
    "Barrel%" = D1_PCT[["Barrel%"]]
  )
  
  metric_key <- function(lbl) {
    if (is.null(lbl)) return(NA_character_)
    lbl <- as.character(lbl)
    if (length(lbl) == 0 || is.na(lbl) || !nzchar(lbl)) return(NA_character_)
    if (grepl("Whiff%", lbl, fixed = TRUE)) return("Whiff%")
    if (grepl("Chase%", lbl, fixed = TRUE)) return("Chase%")
    if (grepl("Barrel%", lbl, fixed = TRUE)) return("Barrel%")
    NA_character_
  }
  
  to_frac <- function(x) {
    v <- suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (!length(v)) return(v)
    if (all(is.na(v))) return(v)
    if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v <- v / 100
    v
  }
  
  for (i in seq_len(nrow(df))) {
    key <- metric_key(metric_labels[i] %||% "")
    ref <- d1_refs[[key]]
    if (!is.finite(ref)) next
    lower_better <- key %in% c("Whiff%","Chase%")
    for (j in 2:ncol(df)) {
      v <- to_frac(df[i, j][[1]])
      fill <- d1_shade_fill(v, ref, lower_better)
      if (nzchar(fill)) {
        tbl <- .tbl_bg(tbl, row = i + 1, column = j, fill = fill, color = "#BEBEBE", linewidth = 0.6)
      }
    }
  }
  
  tbl
}

txst_ptperf_table <- function(df) {
  if (is.null(df) || !nrow(df)) return(as_txst_table(data.frame(Status = "No data")))
  if ("Status" %in% names(df)) return(as_txst_table(df))
  if (!("Pitch Type" %in% names(df))) return(as_txst_table(df))
  
  metric_cols <- c("Rate%","IZ Swing%","Chase%","Whiff%","Barrel%")
  if (!all(metric_cols %in% names(df))) return(as_txst_table(df))
  
  to_frac <- function(x) {
    v <- suppressWarnings(as.numeric(readr::parse_number(as.character(x))))
    if (!length(v)) return(v)
    if (all(is.na(v))) return(v)
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
  
  d1_vals <- c(
    `Rate%`     = NA_real_,
    `IZ Swing%` = if ("Z-Swing%" %in% names(D1_PCT)) D1_PCT[["Z-Swing%"]] else NA_real_,
    `Chase%`    = D1_PCT[["Chase%"]]  %||% NA_real_,
    `Whiff%`    = D1_PCT[["Whiff%"]]  %||% NA_real_,
    `Barrel%`   = D1_PCT[["Barrel%"]] %||% NA_real_
  )

  metric_vals <- lapply(metric_cols, function(mc) parse_dual(df[[mc]]))
  names(metric_vals) <- metric_cols
  
  rows <- as.character(df$`Pitch Type`)
  n <- length(rows)
  y_vals <- n - seq_len(n) + 1
  zebra_fill <- ifelse(seq_len(n) %% 2 == 0, "#F8F3EA", "#FFFFFF")

  ptype_key <- function(lbl){
    x <- toupper(trimws(nz_chr(lbl)))
    dplyr::case_when(
      x %in% c("HARD","FASTBALL","FASTBALLS","FB") ~ "FASTBALL",
      x %in% c("BREAK","SPIN","BREAKING","SLIDER","CURVEBALL","CUTTER","SWEEPER") ~ "SPIN",
      x %in% c("SOFT","CH","CHANGEUP","SPLITTER") ~ "SOFT",
      TRUE ~ NA_character_
    )
  }
  pt_keys <- vapply(rows, ptype_key, character(1))
  pt_ref_whiff <- vapply(pt_keys, function(k){
    if (is.na(k) || is.null(D1_PCT_PTYPE[[k]])) return(NA_real_)
    D1_PCT_PTYPE[[k]]$whiff
  }, numeric(1))
  pt_ref_chase <- vapply(pt_keys, function(k){
    if (is.na(k) || is.null(D1_PCT_PTYPE[[k]])) return(NA_real_)
    D1_PCT_PTYPE[[k]]$chase
  }, numeric(1))
  
  col_labels <- c("Pitch Type", metric_cols)
  x_vals <- seq_along(col_labels)
  
  note_cells <- data.frame(xmin = 0.5, xmax = length(col_labels) + 0.5,
                           ymin = n + 2 - 0.5, ymax = n + 2 + 0.5)
  note_text <- data.frame(x = (length(col_labels) + 1) / 2, y = n + 2, label = "Game % | Season %")
  header_cells <- data.frame(xmin = x_vals - 0.5, xmax = x_vals + 0.5,
                             ymin = n + 1 - 0.5, ymax = n + 1 + 0.5)
  header_text <- data.frame(x = x_vals, y = n + 1, label = col_labels)
  
  pt_cells <- data.frame(xmin = 0.5, xmax = 1.5,
                         ymin = y_vals - 0.5, ymax = y_vals + 0.5,
                         fill = zebra_fill)
  pt_text <- data.frame(x = 1, y = y_vals, label = rows)
  
  half_cells <- list()
  half_text  <- list()
  
  for (j in seq_along(metric_cols)) {
    mc <- metric_cols[j]
    lower_better <- mc %in% c("Chase%","Whiff%")
    vals <- metric_vals[[mc]]
    
    g_vals <- vals$game
    s_vals <- vals$season
    
    if (mc == "Whiff%") {
      fill_g <- mapply(function(v, r) d1_shade_fill(v, r, lower_better), g_vals, pt_ref_whiff, USE.NAMES = FALSE)
      fill_s <- mapply(function(v, r) d1_shade_fill(v, r, lower_better), s_vals, pt_ref_whiff, USE.NAMES = FALSE)
    } else if (mc == "Chase%") {
      fill_g <- mapply(function(v, r) d1_shade_fill(v, r, lower_better), g_vals, pt_ref_chase, USE.NAMES = FALSE)
      fill_s <- mapply(function(v, r) d1_shade_fill(v, r, lower_better), s_vals, pt_ref_chase, USE.NAMES = FALSE)
    } else {
      ref <- d1_vals[[mc]]
      fill_g <- vapply(g_vals, function(v) d1_shade_fill(v, ref, lower_better), character(1))
      fill_s <- vapply(s_vals, function(v) d1_shade_fill(v, ref, lower_better), character(1))
    }
    fill_g <- ifelse(nzchar(fill_g), fill_g, zebra_fill)
    fill_s <- ifelse(nzchar(fill_s), fill_s, zebra_fill)
    
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
    geom_rect(data = note_cells, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "white", color = "#C8C8C8", linewidth = 0.7) +
    geom_text(data = note_text, aes(x = x, y = y, label = label),
              color = "black", fontface = "bold", size = 3.2) +
    geom_rect(data = header_cells, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "white", color = "#C8C8C8", linewidth = 0.7) +
    geom_text(data = header_text, aes(x = x, y = y, label = label),
              color = "black", fontface = "bold", size = 3.4) +
    geom_rect(data = pt_cells, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              color = "#D0D0D0", linewidth = 0.6) +
    geom_rect(data = half_cells_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              color = "#D0D0D0", linewidth = 0.6, show.legend = FALSE) +
    scale_fill_identity() +
    geom_text(data = pt_text, aes(x = x, y = y, label = label),
              color = "#1a1a1a", size = 3.2) +
    geom_text(data = half_text_df, aes(x = x, y = y, label = label),
              color = "#1a1a1a", size = 3.0) +
    coord_cartesian(xlim = c(0.5, length(col_labels) + 0.5),
                    ylim = c(0.5, n + 2.5), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

safe_tbl_plot <- function(obj, title = "Table", season_col_label = "Season") {
  build_body <- function(o) {
    if (inherits(o, c("data.frame","tbl_df"))) {
      fills_attr <- attr(o, "cell_fills")
      df <- as.data.frame(o)
      if (!is.null(fills_attr)) attr(df, "cell_fills") <- fills_attr
      tbl <-
        if (identical(title, "Process Metrics")) {
          txst_process_table(df, season_col_label = season_col_label)
        } else if (identical(title, "Count Breakdown")) {
          txst_count_breakdown_table(df)
        } else if (identical(title, "Pitch Type Performance")) {
          txst_ptperf_table(df)
        } else {
          as_txst_table(df)
        }
      
      if (inherits(tbl, "ggplot")) return(tbl + theme(plot.margin = margin(0,0,0,0)))
      g <- try(ggplotify::as.ggplot(tbl), silent = TRUE)
      if (inherits(g, "try-error")) {
        g <- cowplot::ggdraw() + cowplot::draw_grob(cowplot::as_grob(tbl))
      }
      return(g + theme(plot.margin = margin(0,0,0,0)))
    }
    if (inherits(o, "ggplot")) return(o + theme(plot.margin = margin(0,0,0,0)))
    if (inherits(o, c("grob","gTree","gTable","gtable"))) {
      return(ggplotify::as.ggplot(o) + theme(plot.margin = margin(0,0,0,0)))
    }
    ggplot() + theme_void() + labs(title = paste(title, "(unavailable)"))
  }
  
  body_plot  <- build_body(obj)
  title_plot <- .title_strip_plot(title)
  if (identical(title, "Count Breakdown")) {
    return((title_plot / patchwork::plot_spacer() / body_plot) +
             patchwork::plot_layout(heights = c(0.09, 0.00, 0.91)))
  }
  (title_plot / body_plot) + patchwork::plot_layout(heights = c(0.09, 0.91))
}

wrap_plot_with_title <- function(title, p, title_h = 0.09) {
  (title_strip_plot(title) / p) + patchwork::plot_layout(heights = c(title_h, 1 - title_h))
}

calc_hitter_metrics <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) return(list(iz_swing = NA_real_, chase = NA_real_, whiff = NA_real_, barrel = NA_real_))
  
  # Match hitting AAR/leaderboard logic: swings in zone / pitches in zone
  pitch_call <- dplyr::coalesce(
    .get_chr(d, c("pitch_call","PitchCall","Pitch_Call","PitchResult","Pitch_Result","Call")),
    rep(NA_character_, nrow(d))
  )
  is_swing <- if ("is_swing" %in% names(d)) as.logical(d$is_swing) else .is_swing_flag(pitch_call)
  in_zone  <- .calc_in_zone_logical(d)
  in_zone  <- ifelse(is.na(in_zone), NA, in_zone %in% TRUE)
  
  z_seen   <- sum(in_zone %in% TRUE, na.rm = TRUE)
  z_swings <- sum(is_swing %in% TRUE & (in_zone %in% TRUE), na.rm = TRUE)
  iz_swing <- safe_ratio(z_swings, z_seen)
  
  # Align remaining rates with performance table (AAR)
  base <- summarize_overall(d)
  chase <- base$`Chase%`
  whiff <- base$`Whiff%`
  barrel <- base$`Barrel%`
  
  list(iz_swing = iz_swing, chase = chase, whiff = whiff, barrel = barrel)
}

team_hit_statline_bits <- function(d) {
  d <- tibble::as_tibble(d)
  if (!nrow(d)) {
    return(list(PA = 0L, K = 0L, BB = 0L, H = 0L, Barrel = 0L, Chases = 0L, Whiffs = 0L))
  }
  
  pitch_call <- dplyr::coalesce(
    .get_chr(d, c("pitch_call","PitchCall","Pitch_Call","PitchResult","Pitch_Result","Call")),
    rep(NA_character_, nrow(d))
  )
  is_swing <- if ("is_swing" %in% names(d)) as.logical(d$is_swing) else .is_swing_flag(pitch_call)
  is_whiff <- if ("is_whiff" %in% names(d)) as.logical(d$is_whiff) else .is_whiff_flag(pitch_call)
  in_zone  <- .calc_in_zone_logical(d)
  in_zone  <- ifelse(is.na(in_zone), NA, in_zone %in% TRUE)
  
  whiffs <- sum(is_whiff %in% TRUE, na.rm = TRUE)
  chases <- sum(is_swing %in% TRUE & (in_zone %in% FALSE), na.rm = TRUE)
  
  balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  
  pa <- pa_last_table(d, balls_col, strikes_col)
  ev_type <- .pa_event(pa$pr, pa$kb)
  
  PA <- nrow(pa)
  K  <- sum(ev_type == "K", na.rm = TRUE)
  BB <- sum(ev_type %in% c("BB","IBB"), na.rm = TRUE)
  H  <- sum(ev_type %in% c("1B","2B","3B","HR"), na.rm = TRUE)
  
  barrel_flag <- is_barrel_txst(pa$pc, pa$pr, pa$ev_pa, pa$la_pa)
  barrel_n <- sum(barrel_flag %in% TRUE, na.rm = TRUE)
  
  list(
    PA = as.integer(PA),
    K = as.integer(K),
    BB = as.integer(BB),
    H = as.integer(H),
    Barrel = as.integer(barrel_n),
    Chases = as.integer(chases),
    Whiffs = as.integer(whiffs)
  )
}

d1_shade_fill <- function(val, d1, lower_better = FALSE) {
  if (!is.finite(val) || !is.finite(d1) || d1 == 0) return("")
  span <- max(abs(d1) * 0.25, 0.05)
  score <- (val - d1) / span
  if (isTRUE(lower_better)) score <- -score
  player_severity_hex(score)
}

build_hitter_process_table <- function(game_p, season_p, season_header = "Season") {
  G <- calc_hitter_metrics(game_p)
  S <- calc_hitter_metrics(season_p)
  
  d1 <- list(
    iz_swing = if ("Z-Swing%" %in% names(D1_PCT)) D1_PCT[["Z-Swing%"]] else NA_real_,
    chase    = D1_PCT[["Chase%"]]  %||% NA_real_,
    whiff    = D1_PCT[["Whiff%"]]  %||% NA_real_,
    barrel   = D1_PCT[["Barrel%"]] %||% NA_real_
  )
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "—")
  
  raw <- tibble::tibble(
    Metric = c("IZ Swing%","Chase%","Whiff%","Barrel%"),
    Game   = c(G$iz_swing, G$chase, G$whiff, G$barrel),
    Season = c(S$iz_swing, S$chase, S$whiff, S$barrel),
    D1     = c(d1$iz_swing, d1$chase, d1$whiff, d1$barrel)
  )
  
  out <- raw %>%
    dplyr::mutate(
      Game   = fmt_pct(.data$Game),
      Season = fmt_pct(.data$Season),
      D1     = fmt_pct(.data$D1)
    )
  
  season_label <- season_header %||% "Season"
  names(out)[names(out) == "Season"] <- season_label
  
  # Cell shading vs D1 (Game + Season columns)
  fills <- matrix("", nrow = nrow(out), ncol = ncol(out))
  col_game <- which(names(out) == "Game")
  col_season <- which(names(out) == season_label)
  for (i in seq_len(nrow(out))) {
    metric <- raw$Metric[i]
    d1_val <- raw$D1[i]
    lower_better <- metric %in% c("Chase%","Whiff%")
    if (length(col_game)) {
      fills[i, col_game] <- d1_shade_fill(raw$Game[i], d1_val, lower_better)
    }
    if (length(col_season)) {
      fills[i, col_season] <- d1_shade_fill(raw$Season[i], d1_val, lower_better)
    }
  }
  attr(out, "cell_fills") <- fills
  
  structure(as.data.frame(out), cell_fills = fills)
}

build_hitter_pitchtype_table <- function(game_p, season_p = NULL) {
  g <- tibble::as_tibble(game_p)
  if (!nrow(g)) return(as.data.frame(tibble::tibble(Status = "No data")))
  s <- if (!is.null(season_p) && nrow(season_p)) tibble::as_tibble(season_p) else g[0, , drop = FALSE]
  
  norm_pitch_type <- function(d) {
    n <- nrow(d)
    pt <- dplyr::coalesce(
      if ("PitchType_UNI" %in% names(d)) as.character(d$PitchType_UNI) else rep(NA_character_, n),
      if ("pitch_type_canon" %in% names(d)) as.character(d$pitch_type_canon) else rep(NA_character_, n),
      if ("PitchType" %in% names(d)) as.character(d$PitchType) else rep(NA_character_, n),
      if ("PitchName" %in% names(d)) as.character(d$PitchName) else rep(NA_character_, n),
      if ("TaggedPitchType" %in% names(d)) as.character(d$TaggedPitchType) else rep(NA_character_, n),
      if ("AutoPitchType" %in% names(d)) as.character(d$AutoPitchType) else rep(NA_character_, n)
    )
    pt <- canonical_pitch_fuzzy(pt)
    pt[is.na(pt) | !(pt %in% pitch_levels_all)] <- "Undefined"
    d$PitchType <- pt
    d
  }
  
  g <- norm_pitch_type(g)
  s <- norm_pitch_type(s)
  
  ptype_group_map <- function(pt){
    ifelse(pt %in% c("Fastball","Sinker"), "HARD",
           ifelse(pt %in% c("Cutter","Slider","Curveball","Sweeper"), "BREAK",
                  ifelse(pt %in% c("Changeup","Splitter"), "SOFT", NA_character_)))
  }
  
  g$PitchGroup <- ptype_group_map(g$PitchType)
  s$PitchGroup <- ptype_group_map(s$PitchType)
  g <- g %>% dplyr::filter(!is.na(.data$PitchGroup))
  s <- s %>% dplyr::filter(!is.na(.data$PitchGroup))
  
  groups <- c("HARD","BREAK","SOFT")
  groups <- groups[groups %in% unique(c(g$PitchGroup, s$PitchGroup))]
  if (!length(groups)) return(as.data.frame(tibble::tibble(Status = "No data")))
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "—")
  fmt_dual <- function(gv, sv) paste0(fmt_pct(gv), " | ", fmt_pct(sv))
  
  g_total <- nrow(g)
  s_total <- nrow(s)
  
  rows <- lapply(groups, function(grp) {
    g_sub <- g[g$PitchGroup == grp, , drop = FALSE]
    s_sub <- s[s$PitchGroup == grp, , drop = FALSE]
    g_m <- calc_hitter_metrics(g_sub)
    s_m <- calc_hitter_metrics(s_sub)
    g_rate <- safe_ratio(nrow(g_sub), g_total)
    s_rate <- safe_ratio(nrow(s_sub), s_total)
    list(
      row = tibble::tibble(
      `Pitch Type` = grp,
      `Rate%`      = fmt_dual(g_rate, s_rate),
      `IZ Swing%`  = fmt_dual(g_m$iz_swing, s_m$iz_swing),
      `Chase%`     = fmt_dual(g_m$chase,    s_m$chase),
      `Whiff%`     = fmt_dual(g_m$whiff,    s_m$whiff),
      `Barrel%`    = fmt_dual(g_m$barrel,   s_m$barrel)
      ),
      season_raw = c(
        `Rate%`     = s_rate,
        `IZ Swing%` = s_m$iz_swing,
        `Chase%`    = s_m$chase,
        `Whiff%`    = s_m$whiff,
        `Barrel%`   = s_m$barrel
      )
    )
  })
  
  out <- dplyr::bind_rows(lapply(rows, function(x) x$row))
  season_raw <- do.call(rbind, lapply(rows, function(x) x$season_raw))
  
  # Cell shading vs D1 (use Season values for combined cells)
  d1_vals <- c(
    `Rate%`     = NA_real_,
    `IZ Swing%` = if ("Z-Swing%" %in% names(D1_PCT)) D1_PCT[["Z-Swing%"]] else NA_real_,
    `Chase%`    = D1_PCT[["Chase%"]]  %||% NA_real_,
    `Whiff%`    = D1_PCT[["Whiff%"]]  %||% NA_real_,
    `Barrel%`   = D1_PCT[["Barrel%"]] %||% NA_real_
  )

  ptype_key <- function(lbl){
    x <- toupper(trimws(nz_chr(lbl)))
    dplyr::case_when(
      x %in% c("HARD","FASTBALL","FASTBALLS","FB") ~ "FASTBALL",
      x %in% c("BREAK","SPIN","BREAKING","SLIDER","CURVEBALL","CUTTER","SWEEPER") ~ "SPIN",
      x %in% c("SOFT","CH","CHANGEUP","SPLITTER") ~ "SOFT",
      TRUE ~ NA_character_
    )
  }
  pt_keys <- vapply(out$`Pitch Type`, ptype_key, character(1))
  pt_ref_whiff <- vapply(pt_keys, function(k){
    if (is.na(k) || is.null(D1_PCT_PTYPE[[k]])) return(NA_real_)
    D1_PCT_PTYPE[[k]]$whiff
  }, numeric(1))
  pt_ref_chase <- vapply(pt_keys, function(k){
    if (is.na(k) || is.null(D1_PCT_PTYPE[[k]])) return(NA_real_)
    D1_PCT_PTYPE[[k]]$chase
  }, numeric(1))
  
  fills <- matrix("", nrow = nrow(out), ncol = ncol(out))
  for (i in seq_len(nrow(out))) {
    for (nm in names(d1_vals)) {
      j <- which(names(out) == nm)
      if (!length(j)) next
      lower_better <- nm %in% c("Chase%","Whiff%")
      if (nm == "Whiff%") {
        if (is.finite(pt_ref_whiff[i])) {
          fills[i, j] <- d1_shade_fill(season_raw[i, nm], pt_ref_whiff[i], lower_better)
        }
      } else if (nm == "Chase%") {
        if (is.finite(pt_ref_chase[i])) {
          fills[i, j] <- d1_shade_fill(season_raw[i, nm], pt_ref_chase[i], lower_better)
        }
      } else if (is.finite(d1_vals[[nm]])) {
        fills[i, j] <- d1_shade_fill(season_raw[i, nm], d1_vals[[nm]], lower_better)
      }
    }
  }
  attr(out, "cell_fills") <- fills
  
  as.data.frame(out)
}

build_hitter_count_breakdown_table <- function(game_p, season_p, season_header = "Season") {
  counts <- c("0-0","0-1","1-0","1-1","0-2","2-0","2-1","1-2","2-2","3-0","3-1","3-2")
  
  calc_metrics <- function(d) {
    d <- tibble::as_tibble(d)
    empty <- setNames(rep(NA_real_, length(counts)), counts)
    if (!nrow(d)) return(list(whiff = empty, chase = empty, barrel = empty))
    
    balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
    strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
    
    sw <- swing_table(d, balls_col, strikes_col)
    cnt <- paste0(sw$balls_b4, "-", sw$strikes_b4)
    
    pc <- if ("pitch_call" %in% names(d)) as.character(d$pitch_call) else
      dplyr::coalesce(.get_chr(d, c("PitchCall","Pitch_Call","Call","PitchResult","Pitch_Result")),
                      rep(NA_character_, nrow(d)))
    pr <- if ("play_result" %in% names(d)) as.character(d$play_result) else
      dplyr::coalesce(.get_chr(d, c("PlayResult","Play_Result","Result","Event")),
                      rep(NA_character_, nrow(d)))
    
    ev <- if ("ev" %in% names(d)) to_num(d$ev) else .get_num(d, c("EV","ExitSpeed","ExitVelocity","ExitVel","HitSpeed"))
    la <- if ("la" %in% names(d)) to_num(d$la) else .get_num(d, c("LA","Angle","LaunchAngle","launch_angle"))
    
    bip <- is_bip_txst(pc, pr)
    barrel_flag <- is_barrel_txst(pc, pr, ev, la)
    
    out_whiff  <- empty
    out_chase  <- empty
    out_barrel <- empty
    
    for (c in counts) {
      idx <- which(cnt == c)
      if (!length(idx)) next
      
      sw_n <- sum(sw$is_swing[idx], na.rm = TRUE)
      if (sw_n > 0) out_whiff[c] <- sum(sw$is_whiff[idx], na.rm = TRUE) / sw_n
      
      oz_n <- sum(sw$in_zone[idx] %in% FALSE, na.rm = TRUE)
      if (oz_n > 0) out_chase[c] <- sum(sw$is_swing[idx] & (sw$in_zone[idx] %in% FALSE), na.rm = TRUE) / oz_n
      
      bip_n <- sum(bip[idx], na.rm = TRUE)
      if (bip_n > 0) out_barrel[c] <- sum(barrel_flag[idx], na.rm = TRUE) / bip_n
    }
    
    list(whiff = out_whiff, chase = out_chase, barrel = out_barrel)
  }
  
  g <- calc_metrics(game_p)
  s <- calc_metrics(season_p)
  
  fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "—")
  season_label <- season_header %||% "Season"
  
  out <- data.frame(
    Metric = c(
      "Game Whiff%",
      paste0(season_label, " Whiff%"),
      "Game Chase%",
      paste0(season_label, " Chase%"),
      "Game Barrel%",
      paste0(season_label, " Barrel%")
    ),
    rbind(
      fmt_pct(g$whiff),
      fmt_pct(s$whiff),
      fmt_pct(g$chase),
      fmt_pct(s$chase),
      fmt_pct(g$barrel),
      fmt_pct(s$barrel)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out)[1] <- ""
  
  # Cell shading vs D1 (all count columns)
  d1_whiff  <- D1_PCT[["Whiff%"]]  %||% NA_real_
  d1_chase  <- D1_PCT[["Chase%"]]  %||% NA_real_
  d1_barrel <- D1_PCT[["Barrel%"]] %||% NA_real_
  
  raw_rows <- list(
    g$whiff,
    s$whiff,
    g$chase,
    s$chase,
    g$barrel,
    s$barrel
  )
  
  fills <- matrix("", nrow = nrow(out), ncol = ncol(out))
  for (i in seq_len(nrow(out))) {
    metric_type <- if (i %in% c(1, 2)) "Whiff" else if (i %in% c(3, 4)) "Chase" else "Barrel"
    d1_val <- if (metric_type == "Whiff") d1_whiff else if (metric_type == "Chase") d1_chase else d1_barrel
    lower_better <- metric_type %in% c("Whiff","Chase")
    
    vals <- raw_rows[[i]]
    for (j in seq_along(counts)) {
      v <- vals[[counts[j]]]
      fills[i, j + 1] <- d1_shade_fill(v, d1_val, lower_better)
    }
  }
  attr(out, "cell_fills") <- fills
  
  out
}

build_inning_breakdown_plot <- function(game_p) {
  d <- tibble::as_tibble(game_p)
  inn_col <- pick_first(c("Inning","InningNo","Inning_Num","InningNumber","Inn"), d)
  
  if (is.na(inn_col)) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No inning data"))
  }
  
  d$Inning__ <- to_num(d[[inn_col]])
  d <- d %>% dplyr::filter(is.finite(.data$Inning__))
  if (!nrow(d)) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No inning data"))
  }
  
  innings <- sort(unique(d$Inning__))
  inn_tbl <- dplyr::bind_rows(lapply(innings, function(i) {
    di <- d[d$Inning__ == i, , drop = FALSE]
    m <- calc_hitter_metrics(di)
    tibble::tibble(
      Inning = i,
      `Chase%`    = m$chase,
      `Whiff%`    = m$whiff,
      `IZ Swing%` = m$iz_swing,
      `Barrel%`   = m$barrel
    )
  }))
  
  long <- tidyr::pivot_longer(
    inn_tbl,
    cols = c("Chase%","Whiff%","IZ Swing%","Barrel%"),
    names_to = "Metric",
    values_to = "Value"
  )
  
  long$Metric <- factor(long$Metric, levels = c("Chase%","Whiff%","IZ Swing%","Barrel%"))
  
  ggplot(long, aes(x = Inning, y = Value, color = Metric, group = Metric)) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    scale_color_manual(values = c(
      "Chase%"    = "#501214",
      "Whiff%"    = "#FF0000",
      "IZ Swing%" = "#B4975A",
      "Barrel%"   = "#008000"
    )) +
    scale_x_continuous(breaks = innings) +
    scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, 1)) +
    labs(x = "Inning", y = "%") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(6, 6, 6, 6)
    )
}

compose_team_hit_report_plot <- function(game_p, season_p, game_id = NULL, season_col_label = "Season") {
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
    date_val <- dplyr::coalesce(
      parse_date_any(game_id),
      extract_date_from_game_key(game_id)
    )[1]
  }
  
  date_str <- if (!is.na(date_val)) {
    lt <- as.POSIXlt(date_val)
    paste0(month.name[lt$mon + 1L], " ", lt$mday, ", ", lt$year + 1900L)
  } else {
    as.character(game_id %||% "")
  }
  
  # --- Statline ---
  bits <- team_hit_statline_bits(game_raw)
  statline_text <- sprintf(
    "PA: %s   K: %s   BB: %s   H: %s   Barrel: %s   Chases: %s   Whiffs: %s",
    bits$PA, bits$K, bits$BB, bits$H, bits$Barrel, bits$Chases, bits$Whiffs
  )
  
  header <- ggplot() +
    annotate("text", x = 0, y = 0.94, label = "Bobcats Hitters", hjust = 0, vjust = 1, size = 5.2, fontface = "bold") +
    annotate("text", x = 0, y = 0.64, label = date_str,         hjust = 0, vjust = 1, size = 3.8) +
    annotate("text", x = 0, y = 0.34, label = statline_text,     hjust = 0, vjust = 1, size = 3.4) +
    xlim(0, 1) + ylim(0, 1) + theme_void() + theme(plot.margin = margin(0, 2, 0, 2))
  
  proc_df <- tryCatch(
    build_hitter_process_table(game_raw, season_raw, season_col_label),
    error = function(e) data.frame(Status = paste("Unavailable:", conditionMessage(e)))
  )
  
  pt_df <- tryCatch(
    build_hitter_pitchtype_table(game_raw, season_raw),
    error = function(e) data.frame(Status = paste("Unavailable:", conditionMessage(e)))
  )
  
  count_df <- tryCatch(
    build_hitter_count_breakdown_table(game_raw, season_raw, season_col_label),
    error = function(e) data.frame(Status = paste("Unavailable:", conditionMessage(e)))
  )
  
  row2_left  <- safe_tbl_plot(proc_df, "Process Metrics")
  row2_right <- safe_tbl_plot(pt_df, "Pitch Type Performance")
  row2 <- (row2_left | row2_right) + patchwork::plot_layout(widths = c(0.95, 1.05))
  
  row_counts <- safe_tbl_plot(count_df, "Count Breakdown")
  
  inn_plot <- build_inning_breakdown_plot(game_raw)
  row_inn  <- wrap_plot_with_title("Inning Breakdown", inn_plot, title_h = 0.06)
  
  final <- header / row2 / row_counts / row_inn +
    patchwork::plot_layout(heights = c(0.09, 0.25, 0.28, 0.38))
  
  # Watermark (bobcat)
  wm_path <- if (exists("bobcat_logo", inherits = TRUE) && nzchar(bobcat_logo) && file.exists(bobcat_logo)) {
    bobcat_logo
  } else {
    NULL
  }
  wm_grob <- NULL
  if (!is.null(wm_path)) {
    ext <- tolower(tools::file_ext(wm_path))
    img <- if (ext == "png") {
      png::readPNG(wm_path)
    } else if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
      jpeg::readJPEG(wm_path)
    } else {
      NULL
    }
    if (!is.null(img)) {
      wm_grob <- grid::rasterGrob(img, x = 0.5, y = 0.5, width = 0.80, height = 0.80,
                                  just = "center", interpolate = TRUE, gp = grid::gpar(alpha = 0.60))
    }
  }
  
  if (!is.null(wm_grob)) {
    cowplot::ggdraw() +
      cowplot::draw_grob(wm_grob, x = 0.5, y = 0.5, width = 1, height = 1) +
      cowplot::draw_plot(final, x = 0, y = 0, width = 1, height = 1)
  } else {
    final
  }
}
# NOTE: Strike zone here is FEET for visualization only.
# All zone logic (stats, Good/Bad, chase, whiff) is inches-based upstream.
# ---- AAR strike-zone (robust) ----
aar_zone_df <- function(d){
  if (!nrow(d)) return(d[0, , drop = FALSE])
  
  # Safe PitchNum for ANY caller (AAR or non-AAR)
  if (!("PitchNum" %in% names(d))) d$PitchNum <- seq_len(nrow(d))
  
  # Pick a pitch-type source safely
  pt_src <- if ("type" %in% names(d)) d$type else if ("PitchType" %in% names(d)) d$PitchType
  else if ("PitchType_UNI" %in% names(d)) d$PitchType_UNI
  else if ("pitch_type_canon" %in% names(d)) d$pitch_type_canon
  else rep(NA_character_, nrow(d))
  
  pt_fac <- factor(canonical_pitch_fuzzy(pt_src), levels = pitch_levels_all)
  
  res_col <- if ("res" %in% names(d)) d$res else dplyr::case_when(
    d$pitch_call == "StrikeCalled" ~ "Take",
    d$pitch_call %in% c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut",
                        "FoulBallFieldable","FoulBallNotFieldable","FoulTip") ~ "Swing",
    TRUE ~ NA_character_
  )
  
  d %>%
    dplyr::mutate(
      PitchTypePlot = pt_fac,
      swing_take    = res_col
    ) %>%
    dplyr::filter(
      is.finite(plate_x), is.finite(plate_z), !is.na(swing_take)
    )
}

aar_strike_zone_plot <- function(d){
  dd <- aar_zone_df(d)
  
  base_zone <- list(
    geom_rect(data = strike_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.1),
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, colour = "black", linewidth = 0.9)
  )
  
  pitch_shapes <- c(
    "Fastball"=21, "Sinker"=24, "Changeup"=23, "Splitter"=25,
    "Slider"=22, "Cutter"=4,  "Sweeper"=1, "Curveball"=17,
    "Undefined"=16, "Untagged"=16
  )
  
  if (!nrow(dd)) {
    return(
      ggplot() + base_zone +
        coord_fixed(xlim = c(-3,3), ylim = c(0,5), expand = FALSE) +
        scale_x_reverse() +
        labs(title = "Strike Zone", x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
              plot.margin = margin(0,0,0,0),
              legend.position = "right")
    )
  }
  
  ggplot(dd, aes(plate_x, plate_z)) +
    base_zone +
    geom_point(
      aes(fill = swing_take, shape = PitchTypePlot),
      colour = "black", size = 3, alpha = 0.95, stroke = 0.9, na.rm = TRUE
    ) +
    geom_text(aes(label = PitchNum), size = 3, fontface = "bold", vjust = 0.5, hjust = 0.5, na.rm = TRUE) +
    scale_fill_manual(
      name = "Result",
      values = c("Take"="#501214", "Swing"="#B4975A"),
      breaks = c("Take","Swing"), drop = FALSE
    ) +
    scale_shape_manual(
      name = "Pitch Type",
      values = pitch_shapes, drop = FALSE
    ) +
    guides(
      fill  = guide_legend(order = 1, override.aes = list(shape = 21, size = 4, alpha = 1)),
      shape = guide_legend(order = 2, override.aes = list(size = 4, alpha = 1))
    ) +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
    labs(title = "Strike Zone", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid  = element_blank(),
      plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "right",
      legend.box = "vertical"
    )
}

is_called_strike <- function(pc, decision = NULL){
  pc <- nz_chr(pc)
  pc %in% "StrikeCalled"
}

is_called_ball <- function(pc, decision = NULL){
  pc <- nz_chr(pc)
  pc %in% "BallCalled"
}

framing_zone_type <- function(s_in, h_in){
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  
  in_heart <- is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 6.7) & dplyr::between(h_in, 22, 38)
  in_zone <- is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 10.0) & dplyr::between(h_in, 18, 42) & !in_heart
  
  in_shadow <- is.finite(abs_s) & is.finite(h_in) & (
    # side bands
    (abs_s > 10.0 & abs_s <= 13.3 & dplyr::between(h_in, 18, 42)) |
    # top band
    (abs_s <= 13.3 & dplyr::between(h_in, 42, 46)) |
    # bottom band
    (abs_s <= 13.3 & dplyr::between(h_in, 14, 18))
  )
  
  dplyr::case_when(
    in_heart ~ "Heart",
    in_zone  ~ "Zone",
    in_shadow ~ "Shadow",
    TRUE     ~ "Chase"
  )
}

framing_in_zone <- function(s_in, h_in){
  s_in <- to_num(s_in)
  h_in <- to_num(h_in)
  abs_s <- abs(s_in)
  is.finite(abs_s) & is.finite(h_in) &
    (abs_s <= 10.0) & dplyr::between(h_in, 18, 42)
}

framing_zone_stats <- function(d, mode = c("ball_to_strike","strike_to_ball")){
  mode <- match.arg(mode)
  zones <- c("Heart","Zone","Shadow","Chase")
  
  if (!nrow(d)) {
    out <- tibble::tibble(
      Zone = zones,
      Chances = 0L,
      Strikes = 0L,
      Pct = NA_real_
    )
    tot <- tibble::tibble(Zone = "Total", Chances = 0L, Strikes = 0L, Pct = NA_real_)
    return(dplyr::bind_rows(out, tot))
  }
  
  sx_in <- zone_s_in(d$plate_x)
  hz_in <- zone_h_in(d$plate_z)
  zone_type <- framing_zone_type(sx_in, hz_in)
  
  pc <- canon_pitch_call(d$pitch_call)
  called <- pc %in% c("BallCalled","StrikeCalled")
  in_zone_std <- framing_in_zone(sx_in, hz_in)
  
  if (mode == "ball_to_strike") {
    idx <- called & !in_zone_std
    strikes <- pc %in% "StrikeCalled" & idx
  } else {
    idx <- called & in_zone_std
    strikes <- pc %in% "BallCalled" & idx
  }
  
  df <- tibble::tibble(
    Zone = zone_type,
    Chance = idx,
    Strike = strikes
  ) %>%
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

catcher_framing_subset <- function(d, type = c("ball_to_strike","strike_to_ball")){
  type <- match.arg(type)
  if (!nrow(d)) return(d[0, , drop = FALSE])
  
  # Recompute inches-based zone (match framing plot zone box)
  px <- to_num(d$plate_x)
  pz <- to_num(d$plate_z)
  sx_in <- zone_s_in(px)
  hz_in <- zone_h_in(pz)
  in_zone_calc <- framing_in_zone(sx_in, hz_in)
  keep <- is.finite(px) & is.finite(pz)
  
  d <- d[keep, , drop = FALSE]
  in_zone_calc <- in_zone_calc[keep]
  pc <- canon_pitch_call(d$pitch_call)
  
  # Precompute plotting coords in feet for consistent plot/table counts
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
  
  d <- d %>%
    dplyr::arrange(PitchNum) %>%
    dplyr::mutate(PitchNumSub = dplyr::row_number())
  
  d
}

catcher_framing_zone_plot <- function(d, title = NULL){
  xlim <- c(-2, 2)
  ylim <- c(-0.5, 4.5)
  
  # Framing regions (inches -> feet)
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
  
  # Use precomputed plot_x/plot_z when provided (ensures plot/table parity)
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
  
  # Expand view to include all points while keeping the desired zoom minimum
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
    geom_point(
      aes(fill = PitchType),
      shape = 21, size = 7.4, colour = "black", stroke = 0.9, alpha = 0.95, show.legend = TRUE
    ) +
    geom_text(aes(label = PitchNumSub), size = 4.4, fontface = "bold", color = "white") +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_reverse() +
    scale_fill_manual(
      values = pitch_colors,
      breaks = facet_levels,
      limits = facet_levels,
      drop = FALSE,
      name = "Pitch Type"
    ) +
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

catcher_framing_legend_plot <- function(){
  labs_df <- data.frame(
    PitchType = facet_levels,
    y = seq_along(facet_levels)
  )
  
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

catcher_framing_table <- function(d){
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
  last_name <- function(x){
    x <- nz_chr(x)
    ifelse(grepl(",", x),
           {parts <- strsplit(x, ",\\s*"); vapply(parts, function(p){
             if (length(p) >= 1) p[1] else x
           }, "", USE.NAMES = FALSE)},
           {parts <- strsplit(x, "\\s+"); vapply(parts, function(p){
             if (length(p) >= 1) p[length(p)] else x
           }, "", USE.NAMES = FALSE)})
  }
  
  d %>%
    dplyr::mutate(
      P = PitchNumSub,
      PA = PAofInning,
      `PA P#` = PitchofPA,
      Inn = Inning,
      `Pitch type` = as.character(PitchType),
      Pitcher = last_name(get_chr("Pitcher")),
      Hitter = last_name(get_chr("Batter")),
      Count = get_chr("CountStr"),
      Result = get_chr("pitch_call")
    ) %>%
    dplyr::select(
      P,
      Inn,
      PA,
      `PA P#`,
      Pitcher,
      `Pitch type`,
      Hitter,
      Count,
      Result
    )
}

format_perf_table <- function(tbl){
  if ("IZWhiff%" %in% names(tbl) && !("IZ-Whiff%" %in% names(tbl))) {
    tbl <- dplyr::rename(tbl, `IZ-Whiff%` = `IZWhiff%`)
  }
  for (nm in c("xwOBA", "xwOBAcon", "Swing%", "IZ-Swing%", "MaxEV",
               "10-35*%", "GB%", "LD%", "FB%", "PU%", "Foul Ball%")) {
    if (!(nm %in% names(tbl))) tbl[[nm]] <- NA_real_
  }
  if (!("Z-Swing%" %in% names(tbl))) {
    tbl$`Z-Swing%` <- NA_real_
  }

  # Allocate rounding remainders so the four displayed BIP buckets always
  # total exactly 100.0%, rather than occasionally showing 99.9/100.1.
  partition_cols <- c("GB%", "LD%", "FB%", "PU%")
  partition_fmt <- matrix(NA_character_, nrow = nrow(tbl), ncol = length(partition_cols),
                          dimnames = list(NULL, partition_cols))
  if (nrow(tbl)) {
    for (i in seq_len(nrow(tbl))) {
      vals <- as.numeric(unlist(tbl[i, partition_cols, drop = FALSE], use.names = FALSE))
      if (!all(is.finite(vals)) || sum(vals) <= 0) next
      raw_tenths <- vals / sum(vals) * 1000
      shown_tenths <- floor(raw_tenths)
      remainder <- as.integer(1000 - sum(shown_tenths))
      if (remainder > 0) {
        add_to <- order(raw_tenths - shown_tenths, decreasing = TRUE)[seq_len(remainder)]
        shown_tenths[add_to] <- shown_tenths[add_to] + 1
      }
      partition_fmt[i, ] <- sprintf("%.1f%%", shown_tenths / 10)
    }
  }

  out <- tbl %>%
    dplyr::mutate(
      wOBA      = ifelse(is.finite(wOBA), sprintf("%.3f", wOBA), NA),
      wOBAcon   = ifelse(is.finite(wOBAcon), sprintf("%.3f", wOBAcon), NA),
      xwOBA     = ifelse(is.finite(xwOBA), sprintf("%.3f", xwOBA), NA),
      xwOBAcon  = ifelse(is.finite(xwOBAcon), sprintf("%.3f", xwOBAcon), NA),
      OBP       = ifelse(is.finite(OBP), sprintf("%.3f", OBP), NA),
      SLG       = ifelse(is.finite(SLG), sprintf("%.3f", SLG), NA),
      OPS       = ifelse(is.finite(OPS), sprintf("%.3f", OPS), NA),
      hRV      = ifelse(is.finite(hRV), sprintf("%.2f", hRV), NA),
      `K%`      = sprintf("%.1f%%", 100 * `K%`),
      `BB%`     = sprintf("%.1f%%", 100 * `BB%`),
      `Barrel%` = sprintf("%.1f%%", 100 * `Barrel%`),
      `Contact%`   = ifelse(is.na(`Contact%`), NA, sprintf("%.1f%%", 100 * `Contact%`)),
      `Z-Contact%` = ifelse(is.na(`Z-Contact%`), NA, sprintf("%.1f%%", 100 * `Z-Contact%`)),
      `Whiff%`     = ifelse(is.na(`Whiff%`), NA, sprintf("%.1f%%", 100 * `Whiff%`)),
      `IZ-Whiff%` = ifelse(is.finite(`IZ-Whiff%`), sprintf("%.1f%%", 100*`IZ-Whiff%`), NA),
      `Swing%`     = ifelse(is.finite(`Swing%`), sprintf("%.1f%%", 100 * `Swing%`), NA),
      `IZ-Swing%`  = ifelse(is.finite(`IZ-Swing%`), sprintf("%.1f%%", 100 * `IZ-Swing%`), NA),
      `Z-Swing%`   = ifelse(is.finite(`Z-Swing%`), sprintf("%.1f%%", 100 * `Z-Swing%`), NA),
      `Chase%`     = ifelse(is.na(`Chase%`), NA, sprintf("%.1f%%", 100 * `Chase%`)),
      `Pre2K Chase%` = ifelse(is.na(`Pre2K Chase%`), NA, sprintf("%.1f%%", 100 * `Pre2K Chase%`)),
      `2K Chase%`    = ifelse(is.na(`2K Chase%`),   NA, sprintf("%.1f%%", 100 * `2K Chase%`)),
      MaxEV       = ifelse(is.finite(MaxEV), sprintf("%.1f", MaxEV), NA),
      `90th EV`   = ifelse(is.finite(`90th EV`), sprintf("%.1f", `90th EV`), NA),
      `EV>95%`    = sprintf("%.1f%%", 100 * `EV>95%`),
      `10-35*%`   = ifelse(is.finite(`10-35*%`), sprintf("%.1f%%", 100 * `10-35*%`), NA),
      `GB%`       = ifelse(is.finite(`GB%`), sprintf("%.1f%%", 100 * `GB%`), NA),
      `LD%`       = ifelse(is.finite(`LD%`), sprintf("%.1f%%", 100 * `LD%`), NA),
      `FB%`       = ifelse(is.finite(`FB%`), sprintf("%.1f%%", 100 * `FB%`), NA),
      `PU%`       = ifelse(is.finite(`PU%`), sprintf("%.1f%%", 100 * `PU%`), NA),
      `Foul Ball%` = ifelse(is.finite(`Foul Ball%`), sprintf("%.1f%%", 100 * `Foul Ball%`), NA),
      `LD+FB%`    = sprintf("%.1f%%", 100 * `LD+FB%`),
      `Airpull%`  = ifelse(is.na(`Airpull%`), NA, sprintf("%.1f%%", 100 * `Airpull%`))
    )
  out[partition_cols] <- as.data.frame(partition_fmt, stringsAsFactors = FALSE)
  out
}

# -------------------- D1 shading helpers (shared) --------------------
D1_PCT <- c(
  `K%`=0.192, `BB%`=0.114, `Barrel%`=0.174,
  `Contact%`=0.771, `Z-Contact%`=0.843,
  `Whiff%`=0.229, `IZ-Whiff%`=0.157,
  `Chase%`=0.242, `Pre2K Chase%`=0.192, `2K Chase%`=0.363,
  `Z-Swing%`=0.680, `IZ-Swing%`=0.680,
  `LD+FB%`=0.477
)
D1_PCT_PTYPE <- list(
  FASTBALL = list(whiff = 0.18, chase = 0.19),
  SPIN     = list(whiff = 0.32, chase = 0.25),
  SOFT     = list(whiff = 0.32, chase = 0.27)
)
D1_NON <- c(
  wOBA=0.363, wOBAcon=0.395, xwOBA=0.363, xwOBAcon=0.395,
  OBP=0.384, SLG=0.441, OPS=0.825, `90th EV`=103.1
)
lower_better <- c("K%","Whiff%","IZ-Whiff%","Chase%","Pre2K Chase%","2K Chase%")

shade_cell <- function(txt, bg){
  if (is.na(bg) || bg == "") return(txt)
  alpha <- suppressWarnings(as.numeric(sub(".*,(0?\\.[0-9]+)\\)$", "\\1", bg)))
  fg <- if (is.finite(alpha) && alpha >= 0.58) "#FFFFFF" else "#391315"
  sprintf("<span class='cf-cell' style='background-color:%s;color:%s;font-weight:700'>%s</span>", bg, fg, txt)
}

severity_rgba_fill <- function(score, palette = c("coach", "player")) {
  palette <- match.arg(palette)
  if (!is.finite(score)) return("")
  score <- pmin(pmax(score, -1), 1)
  severity <- abs(score)
  if (severity < 0.08) return("")
  colors <- if (identical(palette, "player")) c(good = "#2E7D32", bad = "#D62828") else c(good = "#E33434", bad = "#5D7EBC")
  rgb <- grDevices::col2rgb(if (score > 0) colors[["good"]] else colors[["bad"]])
  alpha <- 0.16 + 0.72 * severity^0.80
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

stat_severity_fill <- function(score) severity_rgba_fill(score, "coach")
player_severity_fill <- function(score) severity_rgba_fill(score, "player")

# Supplied reference averages and standard deviations. Swing rates are stored
# as fractions, so a 6.50 percentage-point standard deviation is 0.0650.
HITTING_PERCENTILE_REFERENCE <- list(
  `Swing%` = c(mean = 0.4216, sd = 0.0650),
  MaxEV = c(mean = 106.28, sd = 5.62)
)

apply_hitting_percentile_shading <- function(tbl_num, tbl_fmt, palette = "coach",
                                            reference = HITTING_PERCENTILE_REFERENCE) {
  out <- tbl_fmt
  for (nm in intersect(c("Swing%", "MaxEV"), intersect(names(tbl_num), names(out)))) {
    distribution <- reference[[nm]]
    values <- tbl_num[[nm]]
    out[[nm]] <- vapply(seq_along(values), function(i) {
      value <- values[i]
      if (!is.finite(value)) return(as.character(out[[nm]][i]))
      # Estimate percentiles from a normal distribution around the reference.
      pct <- 100 * stats::pnorm(value, mean = distribution[["mean"]], sd = distribution[["sd"]])
      bg <- severity_rgba_fill((pct - 50) / 50, palette)
      cell <- shade_cell(as.character(out[[nm]][i]), bg)
      attributes <- sprintf("title='Normal estimate: %.0f percentile' data-order='%s'", pct, value)
      # Keep .cf-cell directly inside the table cell so the shared full-cell
      # padding, sizing, and border-radius styles apply to these columns too.
      if (nzchar(bg)) {
        sub("<span ", paste0("<span ", attributes, " "), cell, fixed = TRUE)
      } else {
        sprintf("<span %s>%s</span>", attributes, cell)
      }
    }, character(1))
  }
  out
}

apply_d1_shading <- function(tbl_num, tbl_fmt, palette = c("coach", "player")){
  palette <- match.arg(palette)
  fill_fun <- if (identical(palette, "player")) player_severity_fill else stat_severity_fill
  out <- tbl_fmt
  
  # Continuous severity: benchmark is neutral; +/- 5 points reaches full intensity.
  for (nm in intersect(names(D1_PCT), names(tbl_num))) {
    v <- tbl_num[[nm]]
    avg <- D1_PCT[[nm]]
    low_better <- nm %in% lower_better
    score <- (v - avg) / 0.05
    if (low_better) score <- -score
    bg <- vapply(score, fill_fun, FUN.VALUE = character(1))
    out[[nm]] <- mapply(shade_cell, out[[nm]], bg, USE.NAMES = FALSE)
  }
  
  # non-% stats: wOBA/wOBAcon +/- .025; 90th EV +/- 2.5 (per CELL)
  if ("wOBA" %in% names(tbl_num) && "wOBA" %in% names(out)) {
    v <- tbl_num$wOBA; avg <- D1_NON[["wOBA"]]
    bg <- vapply((v - avg) / 0.025, fill_fun, FUN.VALUE = character(1))
    out$wOBA <- mapply(shade_cell, out$wOBA, bg, USE.NAMES = FALSE)
  }
  
  if ("wOBAcon" %in% names(tbl_num) && "wOBAcon" %in% names(out)) {
    v <- tbl_num$wOBAcon; avg <- D1_NON[["wOBAcon"]]
    bg <- vapply((v - avg) / 0.025, fill_fun, FUN.VALUE = character(1))
    out$wOBAcon <- mapply(shade_cell, out$wOBAcon, bg, USE.NAMES = FALSE)
  }

  if ("xwOBA" %in% names(tbl_num) && "xwOBA" %in% names(out)) {
    v <- tbl_num$xwOBA; avg <- D1_NON[["xwOBA"]]
    bg <- vapply((v - avg) / 0.025, fill_fun, FUN.VALUE = character(1))
    out$xwOBA <- mapply(shade_cell, out$xwOBA, bg, USE.NAMES = FALSE)
  }

  if ("xwOBAcon" %in% names(tbl_num) && "xwOBAcon" %in% names(out)) {
    v <- tbl_num$xwOBAcon; avg <- D1_NON[["xwOBAcon"]]
    bg <- vapply((v - avg) / 0.025, fill_fun, FUN.VALUE = character(1))
    out$xwOBAcon <- mapply(shade_cell, out$xwOBAcon, bg, USE.NAMES = FALSE)
  }
  
  # OBP / SLG / OPS: +/- .100 (same green/red rule as wOBA)
  if ("OBP" %in% names(tbl_num) && "OBP" %in% names(out)) {
    v <- tbl_num$OBP; avg <- D1_NON[["OBP"]]
    bg <- vapply((v - avg) / 0.100, fill_fun, FUN.VALUE = character(1))
    out$OBP <- mapply(shade_cell, out$OBP, bg, USE.NAMES = FALSE)
  }
  
  if ("SLG" %in% names(tbl_num) && "SLG" %in% names(out)) {
    v <- tbl_num$SLG; avg <- D1_NON[["SLG"]]
    bg <- vapply((v - avg) / 0.100, fill_fun, FUN.VALUE = character(1))
    out$SLG <- mapply(shade_cell, out$SLG, bg, USE.NAMES = FALSE)
  }
  
  if ("OPS" %in% names(tbl_num) && "OPS" %in% names(out)) {
    v <- tbl_num$OPS; avg <- D1_NON[["OPS"]]
    bg <- vapply((v - avg) / 0.100, fill_fun, FUN.VALUE = character(1))
    out$OPS <- mapply(shade_cell, out$OPS, bg, USE.NAMES = FALSE)
  }
  
  if ("90th EV" %in% names(tbl_num) && "90th EV" %in% names(out)) {
    v <- tbl_num[["90th EV"]]; avg <- D1_NON[["90th EV"]]
    bg <- vapply((v - avg) / 2.5, fill_fun, FUN.VALUE = character(1))
    out[["90th EV"]] <- mapply(shade_cell, out[["90th EV"]], bg, USE.NAMES = FALSE)
  }
  
  apply_hitting_percentile_shading(tbl_num, out, palette)
}

build_swing_decisions_tbl <- function(d){
  d <- dplyr::distinct(d)
  # De-dupe by pitch index when available
  if ("PitchNum" %in% names(d)) {
    d <- d %>% dplyr::arrange(PitchNum) %>%
      dplyr::distinct(PitchNum, .keep_all = TRUE)
  }
  # Also de-dupe by file row when available
  if (all(c("source_file","row_in_file") %in% names(d))) {
    d <- d %>% dplyr::distinct(source_file, row_in_file, .keep_all = TRUE)
  }
  
  n <- nrow(d)
  if (!n) {
    return(tibble::tibble(
      `#` = integer(), `PA` = integer(), Pitcher = abbr_name(get_chr("Pitcher")),
       Count = character(),
      `Pitch Res` = character(), `AB Res` = character(), `Pitch Type` = character(),
      Velo = character(), EV = character(), LA = character(), Dist = character(),
      Decision = character(), `Good?` = character()
    ))
  }
  
  # Ensure PA_ID exists (defensive – also added inside aar_data())
  if (!("PA_ID" %in% names(d))) d$PA_ID <- make_pa_id(d)
  
  
  # Helpers
  get_chr <- function(nm) if (nm %in% names(d)) nz_chr(d[[nm]]) else rep("", n)
  get_num <- function(nm) if (nm %in% names(d)) to_num(d[[nm]]) else rep(NA_real_, n)
  
  # PA numbering in appearance order
  pa_seq <- as.integer(factor(d$PA_ID, levels = unique(d$PA_ID)))
  
  # Counts (balls/strikes before pitch) with fallback build
  balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  balls_b4_raw   <- if (!is.na(balls_col))   get_num(balls_col)   else rep(NA_real_, n)
  strikes_b4_raw <- if (!is.na(strikes_col)) get_num(strikes_col) else rep(NA_real_, n)
  
  if (!any(is.finite(strikes_b4_raw))) {
    # rebuild strikes-before using in-PA cumulative logic
    d$PA_ID_tmp <- make_pa_id(d)
    adds_strike <- get_chr("pitch_call") %in% c("StrikeSwinging","StrikeCalled","FoulTip",
                                                "FoulBallFieldable","FoulBallNotFieldable")
    strikes_b4_raw <- d %>%
      dplyr::mutate(adds_strike = adds_strike) %>%
      dplyr::group_by(PA_ID_tmp) %>%
      dplyr::mutate(strikes_b4_raw = dplyr::lag(cumsum(adds_strike), default = 0)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(strikes_b4_raw)
  }
  Count <- sprintf("%d-%d",
                   ifelse(is.finite(balls_b4_raw),   balls_b4_raw,   0),
                   ifelse(is.finite(strikes_b4_raw), strikes_b4_raw, 0))
  
  # Velocity (coalesce common vendor names)
  velo_num <- dplyr::coalesce(
    get_num("RelSpeed"), get_num("Velocity"), get_num("ReleaseSpeed"), get_num("Velo")
  )
  velo_chr <- ifelse(is.finite(velo_num), sprintf("%.1f", velo_num), "")
  
  # Pitch type (length-safe canonicalization → factor → char)
  n_now <- n
  pt_chr <- local({
    getc <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n_now)
    tmp <- dplyr::coalesce(
      getc("PitchType_UNI"),
      getc("pitch_type_canon"),
      getc("PitchType"),
      getc("PitchName"),
      getc("TaggedPitchType"),
      getc("AutoPitchType")
    )
    tmp <- canonical_pitch_fuzzy(tmp)
    tmp[is.na(tmp) | !(tmp %in% c(facet_levels,"Undefined","Untagged"))] <- "Undefined"
    as.character(factor(tmp, levels = c(facet_levels,"Undefined","Untagged")))
  })
  
  # AB result (use existing if present; otherwise fall back to play_result)
  # AB result (prefer KorBB for K/BB/HBP; otherwise abbreviate play_result)
  abbr_abres <- function(x){
    x0 <- trimws(as.character(x))
    x0 <- gsub("_|-", " ", x0)
    x0 <- gsub("\\s+", " ", x0)
    xl <- tolower(x0)
    dplyr::case_when(
      xl == ""                                ~ "",
      grepl("home ?run|\\bhr\\b", xl)         ~ "HR",
      grepl("\\btriple\\b|\\b3b\\b", xl)      ~ "3B",
      grepl("\\bdouble\\b|\\b2b\\b", xl)      ~ "2B",
      grepl("\\bsingle\\b|\\b1b\\b", xl)      ~ "1B",
      grepl("intentional|\\bibb\\b", xl)      ~ "IBB",
      grepl("\\bwalk\\b|\\bbb\\b", xl)        ~ "BB",
      grepl("hit by pitch|\\bhbp\\b", xl)     ~ "HBP",
      grepl("strikeout|\\bso\\b|\\bk\\b", xl) ~ "K",
      grepl("reache?d? on error|\\broe\\b", xl) ~ "E",
      grepl("fielder.?s choice|\\bfc\\b", xl) ~ "FC",
      grepl("sacrifice fly|\\bsf\\b", xl)     ~ "SF",
      grepl("sac|bunt", xl)                   ~ "SAC",
      grepl("undefined", xl)                  ~ "",
      TRUE ~ toupper(x0)
    )
  }
  # Base AB result from columns (ABResult/ABRes/play_result)
  ab_base <- {
    a <- get_chr("ABResult")
    if (!any(nzchar(a))) a <- get_chr("ABRes")
    if (!any(nzchar(a))) a <- get_chr("play_result")
    abbr_abres(a)
  }
  # KorBB override for K/BB/IBB/HBP when the base isn't a hit (HR/3B/2B/1B)
  korbb <- get_chr("KorBB")
  kb_abbrev <- abbr_abres(korbb)
  is_hit <- ab_base %in% c("HR","3B","2B","1B")
  abres <- ifelse(
    !is_hit & kb_abbrev %in% c("K","BB","IBB","HBP"),
    kb_abbrev,
    ab_base
  )
  abres <- ifelse(grepl("(?i)^undefined$", abres), "", abres)
  abres
  
  # Decision is DISPLAY ONLY — derive locally from pitch_call
  Decision <- ifelse(
    nz_chr(d$pitch_call) %in% SWING_LIKE,
    "SWING",
    "TAKE"
  )
  
  
  
  # In-zone and Good/Bad (derive if missing)
  in_zone <- if ("in_zone" %in% names(d)) {
    as.logical(d$in_zone)
  } 
  stopifnot("in_zone" %in% names(d))
  in_zone <- as.logical(d$in_zone)
  
  # Other display cols
  pitchnum <- if ("PitchNum" %in% names(d)) to_num(d$PitchNum) else seq_len(n)
  ev_chr   <- { ev <- get_num("ev");   ifelse(is.finite(ev), sprintf("%.1f", ev), "") }
  la_chr   <- { la <- get_num("la");   ifelse(is.finite(la), sprintf("%.0f", la), "") }
  dist_chr <- { ds <- get_num("distance_ft"); ifelse(is.finite(ds), sprintf("%.0f", ds), "") }
  
  good_calc <- dplyr::case_when(
    in_zone %in% TRUE  & Decision == "SWING" ~ "YES",
    in_zone %in% FALSE & Decision == "TAKE"  ~ "YES",
    in_zone %in% TRUE  & Decision == "TAKE"  ~ "NO",
    in_zone %in% FALSE & Decision == "SWING" ~ "NO",
    TRUE                                     ~ ""
  )
  
  tbl <- tibble::tibble(
    `#`         = pitchnum,
    `PA`        = pa_seq,
    Pitcher     = abbr_name(get_chr("Pitcher")),
    Count       = Count,
    `Pitch Res.`= get_chr("pitch_call"),
    `Res.`      = abres,
    `Type`      = pt_chr,
    Velo        = velo_chr,
    EV          = ev_chr,
    LA          = la_chr,
    Dist        = dist_chr,
    Decision    = Decision,
    `Good?`     = good_calc
    
  )
  
  
  # Final safety: remove exact duplicate rows (common if joins/merges repeated)
  tbl <- dplyr::distinct(tbl)
  
  return(tbl)
}

# Auto column widths from max string width (header + body), with padding
compute_table_widths <- function(df, header_cex = 0.9, body_cex = 0.8, pad_mm = 3) {
  hw <- lapply(names(df), function(s) grid::grobWidth(grid::textGrob(s, gp = grid::gpar(cex = header_cex))))
  cw <- lapply(seq_along(df), function(i) {
    vals <- as.character(df[[i]])
    if (!length(vals)) return(grid::unit(0, "mm"))
    ws <- lapply(vals, function(x) grid::grobWidth(grid::textGrob(x, gp = grid::gpar(cex = body_cex))))
    Reduce(grid::unit.pmax, ws)
  })
  combined <- Map(grid::unit.pmax, hw, cw)
  base::do.call(grid::unit.c, lapply(combined, function(u) u + grid::unit(pad_mm, "mm")))
}
# --- NEW: shrink a tableGrob so total column width <= max_in (inches) ---
shrink_table_to_width <- function(tg, max_in){
  tot_in <- sum(vapply(seq_along(tg$widths),
                       function(i) grid::convertWidth(tg$widths[i], "inches", valueOnly = TRUE),
                       numeric(1)))
  if (is.finite(tot_in) && tot_in > max_in) {
    sf <- max_in / tot_in
    tg$widths <- do.call(grid::unit.c, lapply(seq_along(tg$widths), function(i){
      grid::unit(grid::convertWidth(tg$widths[i], "inches", valueOnly = TRUE) * sf, "inches")
    }))
  }
  tg
}
# --- Clamp helpers (prevent "could not find function clamp_grob_height") ---
clamp_grob_height <- function(g, max_in){
  gh <- tryCatch(grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE),
                 error = function(e) NA_real_)
  if (is.finite(gh) && gh > max_in) {
    sf <- max_in / gh
    if (!is.null(g$heights)) {
      g$heights <- do.call(grid::unit.c, lapply(seq_along(g$heights), function(i){
        grid::unit(grid::convertHeight(g$heights[i], "inches", valueOnly = TRUE) * sf, "inches")
      }))
    }
  }
  g
}

clamp_grob_width <- function(g, max_in){
  gw <- tryCatch(grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE),
                 error = function(e) NA_real_)
  if (is.finite(gw) && gw > max_in) {
    sf <- max_in / gw
    if (!is.null(g$widths)) {
      g$widths <- do.call(grid::unit.c, lapply(seq_along(g$widths), function(i){
        grid::unit(grid::convertWidth(g$widths[i], "inches", valueOnly = TRUE) * sf, "inches")
      }))
    }
  }
  g
}

# --- Draw any grob inside a hard-clipped box in NPC units ---
draw_in_box <- function(g, left = 0.05, right = 0.60, bottom = 0.05, top = 0.95){
  w <- right - left
  h <- top - bottom
  grid::pushViewport(grid::viewport(
    x = left, y = top,
    width = w, height = h,
    just = c("left","top"),
    clip = "on"
  ))
  grid::grid.draw(g)
  grid::popViewport()
}

# --- NEW: clamp a grob's TOTAL HEIGHT to a max (inches) ---
clamp_grob_height <- function(g, max_in){
  if (is.null(g) || !inherits(g, "grob")) return(g)
  
  h_in <- sum(grid::convertUnit(g$heights, "in", valueOnly = TRUE))
  
  if (is.finite(h_in) && h_in > max_in) {
    scale <- max_in / h_in
    g$heights <- g$heights * scale
  }
  
  g
}


`%||%` <- function(x, y) if (is.null(x)) y else x
hitting_team_display_text <- function(x) {
  if (exists("base_replace_team_codes", mode = "function", inherits = TRUE)) {
    return(get("base_replace_team_codes", mode = "function", inherits = TRUE)(x))
  }
  as.character(x)
}

# safer alpha helper for PDF fills (tableGrob won't understand "rgba(...)")
alpha_hex <- function(hex, a = 0.12) grDevices::adjustcolor(hex, alpha.f = a)

# ---- Legend ghosts (used both on-screen & PDF) ----
ghost_strike_legends <- list(
  geom_point(
    data = data.frame(
      x = -10, y = -10,
      TypeGroup = factor(c("HARD","BREAK","SOFT"), levels = c("HARD","BREAK","SOFT"))
    ),
    aes(x, y, shape = TypeGroup),
    inherit.aes = FALSE, alpha = 0,
    show.legend = c(shape = TRUE, fill = FALSE)
  ),
  geom_point(
    data = data.frame(
      x = -10, y = -10,
      Decision = factor(c("SWING","TAKE"), levels = c("SWING","TAKE"))
    ),
    aes(x, y, fill = Decision),
    inherit.aes = FALSE, shape = 21, alpha = 0,
    show.legend = c(shape = FALSE, fill = TRUE)
  )
)

# --- decoupled legend layers (off-plot; alpha=0) ---
ghost_spray_legends <- {
  df_ct <- data.frame(BBType = c("Pop Up","Fly Ball","Line Drive","Ground Ball"))
  df_rs <- data.frame(ResultBucket = result_type_levels)
  
  list(
    # Contact type legend (shapes only)
    ggplot2::geom_point(
      data = df_ct,
      mapping = ggplot2::aes(x = -999, y = -999, shape = BBType),
      size = 5.5, alpha = 0,
      inherit.aes = FALSE, show.legend = TRUE
    ),
    # Result legend (fills only) — uses ResultBucket
    ggplot2::geom_point(
      data = df_rs,
      mapping = ggplot2::aes(x = -999, y = -999, fill = ResultBucket),
      shape = 21, size = 5.5, alpha = 0,
      inherit.aes = FALSE, show.legend = TRUE
    )
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

make_pdf_header <- function(player_name, gdate){
  # Fallback-safe logo loader (returns a blank grob if file missing or png not available)
  load_logo <- function(path, w_in = 0.95, h_in = 0.55){
    if (!is.character(path) || !file.exists(path)) {
      return(grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)))
    }
    ok_png <- requireNamespace("png", quietly = TRUE)
    if (!ok_png) return(grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)))
    img <- tryCatch(png::readPNG(path), error = function(e) NULL)
    if (is.null(img)) return(grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)))
    grid::rasterGrob(img, width = grid::unit(w_in, "in"), height = grid::unit(h_in, "in"))
  }
  
  left_logo  <- load_logo(TXST_LOGO_PATH)
  right_logo <- load_logo(BOBCAT_LOGO_PATH)
  
  title_txt <- grid::textGrob(
    sprintf("%s - %s - Post Game Report (AAR)", player_name, as.character(gdate)),
    gp = grid::gpar(fontface = 2, cex = 1.10)
  )
  
  # 3-column header: [logo] [title expands] [logo]
  header_gt <- gtable::gtable(
    widths  = grid::unit.c(grid::unit(1.10, "in"), grid::unit(1, "null"), grid::unit(1.10, "in")),
    heights = grid::unit(0.80, "in")
  )
  
  # add grobs into single row (row = 1)
  header_gt <- gtable::gtable_add_grob(header_gt, left_logo,  t = 1, l = 1, name = "hdr_left_logo")
  header_gt <- gtable::gtable_add_grob(header_gt, title_txt,  t = 1, l = 2, name = "hdr_title")
  header_gt <- gtable::gtable_add_grob(header_gt, right_logo, t = 1, l = 3, name = "hdr_right_logo")
  
  header_gt
}


make_field_layers <- function(track_width_ft = 15) {
  # Bobcat Ballpark wall dimensions from the shared Texas State field diagram.
  foul_point <- function(bearing_deg, dist) {
    c(x = dist * sin(bearing_deg * pi/180), y = dist * cos(bearing_deg * pi/180))
  }
  point_on_segment_at_radius <- function(p0, p1, radius) {
    dx <- p1[["x"]] - p0[["x"]]
    dy <- p1[["y"]] - p0[["y"]]
    a <- dx^2 + dy^2
    b <- 2 * (p0[["x"]] * dx + p0[["y"]] * dy)
    c0 <- p0[["x"]]^2 + p0[["y"]]^2 - radius^2
    disc <- b^2 - 4 * a * c0
    if (!is.finite(disc) || disc < 0) return(c(x = NA_real_, y = NA_real_))
    roots <- c((-b - sqrt(disc)) / (2 * a), (-b + sqrt(disc)) / (2 * a))
    t <- roots[is.finite(roots) & roots >= 0 & roots <= 1]
    if (!length(t)) return(c(x = NA_real_, y = NA_real_))
    t <- t[[1]]
    c(x = p0[["x"]] + t * dx, y = p0[["y"]] + t * dy)
  }
  
  lf <- foul_point(-45, 330)
  rf <- foul_point(45, 331)
  lcf_wall <- c(x = -60, y = 399)
  rcf_wall <- c(x = 60, y = 399)
  
  l342 <- point_on_segment_at_radius(lf, lcf_wall, 342)
  l381 <- point_on_segment_at_radius(lf, lcf_wall, 381)
  r385 <- point_on_segment_at_radius(rcf_wall, rf, 385)
  r344 <- point_on_segment_at_radius(rcf_wall, rf, 344)
  
  wall <- tibble::tibble(
    label = c("LF 330", "LC 342", "LC 381", "LCF 399", "RCF 399", "RC 385", "RC 344", "RF 331"),
    x = c(lf[["x"]], l342[["x"]], l381[["x"]], lcf_wall[["x"]], rcf_wall[["x"]], r385[["x"]], r344[["x"]], rf[["x"]]),
    y = c(lf[["y"]], l342[["y"]], l381[["y"]], lcf_wall[["y"]], rcf_wall[["y"]], r385[["y"]], r344[["y"]], rf[["y"]])
  ) %>%
    dplyr::mutate(
      r = sqrt(.data$x^2 + .data$y^2),
      ang = atan2(.data$x, .data$y) * 180/pi
    )
  
  wall_inner <- wall %>% dplyr::mutate(
    r_inner = pmax(.data$r - track_width_ft, 0),
    x = .data$x * .data$r_inner / .data$r,
    y = .data$y * .data$r_inner / .data$r
  )
  
  track_poly <- dplyr::bind_rows(
    wall %>% dplyr::select(x, y),
    wall_inner %>% dplyr::select(x, y) %>% dplyr::arrange(dplyr::desc(dplyr::row_number()))
  )
  
  foul_tip <- function(bearing_deg, dist) {
    tibble::tibble(x = 0, y = 0,
                   xend = dist * sin(bearing_deg * pi/180),
                   yend = dist * cos(bearing_deg * pi/180))
  }
  foul_lines <- dplyr::bind_rows(foul_tip(-45, 330), foul_tip(+45, 331))
  
  # Base diamond (90 ft)
  half <- 90 / sqrt(2); sec <- 90 * sqrt(2)
  bases_diamond <- tibble::tibble(
    x = c(0,  half,   0, -half,   0),
    y = c(0,  half,  sec,  half,   0)
  )
  
  # Distance rings with center-line labels.
  ang <- seq(-45, 45, by = 0.5)
  make_ring <- function(r) {
    tibble::tibble(
      r = r,
      ang = ang,
      x = r * sin(ang * pi/180),
      y = r * cos(ang * pi/180)
    ) %>%
      dplyr::filter(abs(.data$ang) >= 4.5)
  }
  ring_distances <- c(seq(200, 450, by = 50), 475)
  rings <- dplyr::bind_rows(lapply(ring_distances, make_ring))
  ring_labels <- tibble::tibble(
    x = 0,
    y = ring_distances,
    label = paste0(ring_distances, " ft")
  )
  
  list(
    wall = wall, wall_inner = wall_inner, track_poly = track_poly,
    foul_lines = foul_lines, bases_diamond = bases_diamond, rings = rings,
    ring_labels = ring_labels
  )
}
# ===== Top-level spray grob builder (row-safe; no recycling) =====
# NOTE: Ball Flight tab only — NO pitch-number labels inside icons.
build_spray_grob <- function(d, show_numbers = FALSE){
  
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    fld <- make_field_layers()
    p0 <- ggplot() +
      geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
      geom_path(   data=fld$wall,        aes(x=x,y=y), color="#501214", linewidth=1.1) +
      geom_segment(data=fld$foul_lines,  aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
      geom_path(   data=fld$rings,       aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
      geom_text(   data=fld$ring_labels, aes(x=x,y=y,label=label), color="grey35", size=3.2, vjust=0.5) +
      geom_path(   data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
      coord_fixed(xlim=spray_xlim, ylim=spray_ylim, expand=FALSE) +
      theme_minimal(base_size=11) +
      theme(panel.grid=element_blank(), panel.border=element_rect(color="black", fill=NA))
    return(ggplotGrob(p0))
  }
  
  n0 <- nrow(d)
  
  # Ensure PitchNum exists ONLY as a stable unique key (we do NOT plot labels)
  if (!("PitchNum" %in% names(d))) d$PitchNum <- seq_len(n0)
  
  # Safe numeric sources
  dist <- if ("distance_ft" %in% names(d)) to_num(d$distance_ft) else rep(NA_real_, n0)
  bear <- if ("bearing" %in% names(d)) to_num(d$bearing) else rep(NA_real_, n0)
  hx   <- if ("hc_x" %in% names(d)) to_num(d$hc_x) else rep(NA_real_, n0)
  hy   <- if ("hc_y" %in% names(d)) to_num(d$hc_y) else rep(NA_real_, n0)
  
  use_tm <- is.finite(dist) & is.finite(bear)
  
  spray <- d %>%
    dplyr::mutate(
      plot_x = dplyr::if_else(use_tm, dist * sin(bear * pi/180), hx),
      plot_y = dplyr::if_else(use_tm, dist * cos(bear * pi/180), hy)
    )
  
  # BIP flag (safe)
  pc_all <- dplyr::coalesce(
    if ("PitchCall"  %in% names(spray)) as.character(spray$PitchCall)  else NA_character_,
    if ("pitch_call" %in% names(spray)) as.character(spray$pitch_call) else NA_character_
  )
  pr_all <- dplyr::coalesce(
    if ("PlayResult"  %in% names(spray)) as.character(spray$PlayResult)  else NA_character_,
    if ("play_result" %in% names(spray)) as.character(spray$play_result) else NA_character_
  )
  
  spray$bip_flag <- if (exists("is_bip_txst", inherits=TRUE)) {
    is_bip_txst(pc_all, pr_all)
  } else {
    grepl("(?i)in\\s*play|\\bbip\\b", nz_chr(pc_all)) |
      grepl("(?i)single|double|triple|home\\s*run|error|out|\\b1b\\b|\\b2b\\b|\\b3b\\b|\\bhr\\b", nz_chr(pr_all))
  }
  
  spray <- spray %>%
    dplyr::filter(bip_flag %in% TRUE, is.finite(plot_x), is.finite(plot_y))
  
  # Launch angle aligned to spray rows
  m <- nrow(spray)
  la_vec <- if ("LA" %in% names(spray)) to_num(spray$LA) else
    if ("la" %in% names(spray)) to_num(spray$la) else
      if ("LaunchAngle" %in% names(spray)) to_num(spray$LaunchAngle) else
        rep(NA_real_, m)
  
  spray$BBType <- spray_contact_type(la_vec)
  spray$BBType <- factor(spray$BBType, levels=contact_type_levels)
  spray <- spray %>% dplyr::filter(!is.na(BBType)) %>% dplyr::distinct(PitchNum, .keep_all = TRUE)
  
  # Ground balls: draw dashed line out to 120 ft along bearing and place icon at 120 ft
  if ("bearing" %in% names(spray)) {
    spray$gb_x <- ifelse(spray$BBType == "Ground Ball" & is.finite(spray$bearing),
                         120 * sin(spray$bearing * pi/180), NA_real_)
    spray$gb_y <- ifelse(spray$BBType == "Ground Ball" & is.finite(spray$bearing),
                         120 * cos(spray$bearing * pi/180), NA_real_)
    spray$plot_x <- ifelse(!is.na(spray$gb_x), spray$gb_x, spray$plot_x)
    spray$plot_y <- ifelse(!is.na(spray$gb_y), spray$gb_y, spray$plot_y)
  }
  
  pr_vec <- dplyr::coalesce(
    if ("PlayResult"  %in% names(spray)) as.character(spray$PlayResult)  else NA_character_,
    if ("play_result" %in% names(spray)) as.character(spray$play_result) else NA_character_
  )
  
  spray$ResultBucket <- spray_result_bucket(pr_vec)
  spray$ResultBucket <- factor(spray$ResultBucket, levels=result_type_levels)
  
  fld <- make_field_layers()
  
  base_field <- ggplot() +
    geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
    geom_path(   data=fld$wall,        aes(x=x,y=y), color="#501214", linewidth=1.1) +
    geom_segment(data=fld$foul_lines,  aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
    geom_path(   data=fld$rings,       aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
    geom_text(   data=fld$ring_labels, aes(x=x,y=y,label=label), color="grey35", size=3.2, vjust=0.5) +
    geom_path(   data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
    coord_fixed(xlim=spray_xlim, ylim=spray_ylim, expand=FALSE) +
    theme_minimal(base_size=11) +
    theme(
      panel.grid=element_blank(),
      panel.border=element_rect(color="black", fill=NA),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Legend dummies (always-on)
  contact_legend <- data.frame(
    x=-10, y=-10,
    BBType=factor(contact_type_levels, levels=contact_type_levels)
  )
  result_legend <- data.frame(
    x=-10, y=-10,
    ResultBucket=factor(result_type_levels, levels=result_type_levels)
  )
  
  p <- base_field +
    geom_point(data=contact_legend, aes(x=x,y=y, shape=BBType),
               inherit.aes=FALSE, alpha=0, show.legend=TRUE) +
    geom_point(data=result_legend, aes(x=x,y=y, fill=ResultBucket),
               inherit.aes=FALSE, shape=21, alpha=0, show.legend=TRUE)
  
  if (nrow(spray)) {
    p <- p +
      geom_segment(
        data = spray %>% dplyr::filter(BBType == "Ground Ball", is.finite(gb_x), is.finite(gb_y)),
        aes(x = 0, y = 0, xend = gb_x, yend = gb_y),
        linetype = "dashed", color = "black", linewidth = 0.6, alpha = 0.7,
        inherit.aes = FALSE
      ) +
      geom_point(
        data=spray,
        aes(x=plot_x, y=plot_y, shape=BBType, fill=ResultBucket),
        size=6.0, color="black", stroke=0.6, alpha=0.95,
        show.legend = FALSE
      )
    if (isTRUE(show_numbers)) {
      p <- p + geom_text(
        data = spray,
        aes(x = plot_x, y = plot_y, label = PitchNum),
        color = "white", size = 3.8, fontface = "bold",
        show.legend = FALSE
      )
    }
  }
  
  p <- p +
    scale_shape_manual(
      name="Contact type",
      values=contact_shape_values,
      breaks=contact_type_levels,
      drop=FALSE
    ) +
    scale_fill_manual(
      name="Result",
      values=result_fill_values,
      breaks=result_type_levels,
      labels=result_type_labels,
      drop=FALSE
    ) +
    guides(
      shape = guide_legend(
        title="Contact type", order=1,
        override.aes=list(
          shape = unname(contact_legend_shape_values),
          fill  = NA, size=5.5, color="black", stroke=0.8, alpha=1
        )
      ),
      fill  = guide_legend(
        title="Result", order=2,
        override.aes=list(shape=21, size=5.5, color="black", stroke=0.6, alpha=1)
      )
    )
  
  ggplotGrob(p)
}


# --- AAR constants reused across UI + downloads ---
SWING_LIKE <- c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut",
                "FoulBallFieldable","FoulBallNotFieldable","FoulTip")
TAKE_LIKE  <- c("BallCalled","StrikeCalled")
# --- Back-compat aliases so old code keeps working ---
swing_like <- SWING_LIKE
take_like  <- TAKE_LIKE

# -------- Safe helper: aar_data_all() --------
aar_data_all <- function() {
  # Prefer non-reactive full dataset if present
  if (exists("df", inherits = TRUE)) return(get("df"))
  # Fall back to current AAR subset if nothing else
  if (exists("aar_data", inherits = TRUE)) {
    out <- tryCatch(aar_data(), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  # Empty tibble as last resort
  return(dplyr::tibble())
}

# -------------------- TruMedia-style zone heatmaps (MATCH pitching-app Locations look) --------------------
zone_limits <- list(x = c(-3.0, 3.0), z = c(0.0, 5.0))  # match pitching app coord_fixed

empty_zone_plot <- function(msg = "No data"){
  ggplot() +
    coord_fixed(xlim = zone_limits$x, ylim = zone_limits$z, expand = FALSE) +
    annotate("text", x = 0, y = mean(zone_limits$z), label = msg, fontface = 2, size = 6) +
    theme_void() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
      plot.margin  = margin(10, 10, 10, 10)
    )
}

# Build a KDE grid and return a raster-ready df (0..1 normalized)
# - value_col: optional event filter (logical TRUE / numeric >0 / non-empty char)
# - weight_col: optional resampling weights (e.g., EV, or whiff 1/0)
bin_zone_stat <- function(d, value_col = NULL, weight_col = NULL,
                          n = 200, bw = c(0.28, 0.28), sample_n = 4500){
  
  d <- d %>%
    dplyr::filter(
      is.finite(plate_x), is.finite(plate_z),
      plate_x >= zone_limits$x[1], plate_x <= zone_limits$x[2],
      plate_z >= zone_limits$z[1], plate_z <= zone_limits$z[2]
    )
  
  # Optional: if value_col provided, keep only "event" rows (same behavior you had)
  if (!is.null(value_col) && value_col %in% names(d)) {
    v <- d[[value_col]]
    keep <- if (is.logical(v)) (v %in% TRUE) else if (is.numeric(v)) is.finite(v) & v > 0 else nzchar(as.character(v))
    d <- d[keep, , drop = FALSE]
  }
  
  if (nrow(d) < 3) return(tibble::tibble())
  
  x <- to_num(d$plate_x)
  z <- to_num(d$plate_z)
  ok <- is.finite(x) & is.finite(z)
  x <- x[ok]; z <- z[ok]
  if (length(x) < 3) return(tibble::tibble())
  
  # Optional weighting via resampling (MASS::kde2d has no native weights)
  if (!is.null(weight_col) && weight_col %in% names(d)) {
    w_full <- to_num(d[[weight_col]])[ok]
    w_full[!is.finite(w_full)] <- 0
    w_full <- pmax(w_full, 0)
    
    if (sum(w_full) <= 0) return(tibble::tibble())
    
    nn <- length(x)
    m  <- min(sample_n, max(nn, 2L))
    idx <- sample.int(nn, size = m, replace = TRUE, prob = w_full)
    x <- x[idx]; z <- z[idx]
  }
  
  h <- pmax(to_num(bw), 1e-6)
  kd <- MASS::kde2d(x, z, n = n, lims = c(zone_limits$x, zone_limits$z), h = h)
  
  grid <- expand.grid(x = kd$x, z = kd$y)
  grid$fill <- as.vector(kd$z)
  
  # Normalize to 0..1 like after_stat(ndensity)
  mx <- suppressWarnings(max(grid$fill, na.rm = TRUE))
  if (is.finite(mx) && mx > 0) {
    grid$fill <- grid$fill / mx
    grid$fill <- pmin(pmax(grid$fill, 0), 1)
  } else {
    grid$fill <- 0
  }
  
  tibble::as_tibble(grid)
}

# Match pitching-app Locations heatmap styling (no legend, bold title, black border, overlays)
plot_trumedia_heat <- function(tile_df, title = NULL){
  if (!nrow(tile_df)) return(empty_zone_plot("Not enough pitches with locations."))
  
  ggplot(tile_df, aes(x, z)) +
    geom_raster(aes(fill = fill), interpolate = FALSE) +
    scale_fill_gradientn(colors = wblyrm_palette, guide = "none", limits = c(0,1), na.value = "#ffffff") +
    labs(
      title = title,
      y = "Vertical Location", x = "Horizontal Location"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
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
    coord_fixed(xlim = zone_limits$x, ylim = zone_limits$z, expand = FALSE)
}


plot_zone_points <- function(d, title = NULL){
  tile <- bin_zone_stat(d, value_col = NULL)
  plot_trumedia_heat(tile, title = title %||% "Zone Density")
}

plot_sd_zone_points <- function(d, title = NULL){
  d <- d %>% dplyr::filter(is.finite(plate_x), is.finite(plate_z))
  if (!nrow(d)) return(empty_zone_plot("No pitches"))
  
  # Force identical legend content/order on both plots
  d$PitchType <- factor(as.character(d$PitchType), levels = pitch_levels_all)
  
  ggplot(d, aes(x = -plate_x, y = plate_z, color = PitchType)) +
    geom_rect(data = strike_zone,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.1) +
    geom_segment(data = home_plate_segments,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
    geom_point(size = 3.2, alpha = 0.85) +
    coord_fixed(xlim = zone_limits$x, ylim = zone_limits$z, expand = FALSE) +
    scale_x_reverse() +
    scale_color_manual(
      values = pitch_colors,
      breaks = c(facet_levels, "Undefined", "Untagged"),
      limits = pitch_levels_all,
      drop = FALSE,
      name = "Pitch Type"
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid   = element_blank(),
      plot.title   = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      legend.text  = element_text(color = "black"),
      legend.title = element_text(color = "black", face = "bold")
    )
}


# -------------------- Server --------------------
server <- function(input, output, session){
  
  observe({
    d <- dat_filt()
    updateSelectInput(session, "game", choices = make_games_txst(d))
  })
  # Filtered dataset
  dat_filt <- reactive({
    req(input$Hitter)
    
    d <- if (isTRUE(input$hit_Bullpens)) {
      bullpens_df %>%
        dplyr::filter((Batter == input$Hitter) | (Pitcher == input$Hitter))
    } else {
      dd <- df %>%
        dplyr::filter(
          .data$Batter == input$Hitter,
          .data$CustomGameID %in% input$Game,
          !(is_bullpen %in% TRUE)
        )
      if (!is.null(input$PitcherHand) && input$PitcherHand != "All") {
        dd <- dd %>% dplyr::filter(.data$PitcherHand == input$PitcherHand)
      }
      if (nrow(dd) == 0 && nzchar(TEAM_CODE)) {
        dd <- df %>%
          dplyr::filter(
            .data$Batter == input$Hitter,
            .data$CustomGameID %in% input$Game,
            !(is_bullpen %in% TRUE)
          ) %>%
          filter_team("BatterTeam")
        if (!is.null(input$PitcherHand) && input$PitcherHand != "All") {
          dd <- dd %>% dplyr::filter(.data$PitcherHand == input$PitcherHand)
        }
      }
      dd
    }
    d$is_swing <- d$pitch_call %in% SWING_LIKE
    if (!("is_whiff" %in% names(d))) {
      d$is_whiff <- d$pitch_call %in% c("StrikeSwinging")
    }
    if (!("is_contact" %in% names(d))) {
      d$is_contact <- d$is_swing & !d$is_whiff
    }
    
    
    # -------- SAFE PITCH TYPE COALESCE (no length-0 vectors) --------
    n <- nrow(d)
    get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n)
    
    # Prefer the unified column you already built globally; fall back if needed
    pt_raw <- dplyr::coalesce(
      get_chr("PitchType_UNI"),
      get_chr("pitch_type_canon"),
      get_chr("PitchType")
    )
    
    pt_map <- canonical_pitch_fuzzy(pt_raw)
    pt_map[!pt_map %in% pitch_levels_all] <- "Undefined"
    
    # --- Authoritative zone (INCHES ONLY) ---
    d$plate_x_in <- zone_s_in(d$plate_x)
    d$plate_z_in <- zone_h_in(d$plate_z)
    
    d$plate_x_in <- to_num(d$plate_x_in)
    d$plate_z_in <- to_num(d$plate_z_in)
    
    d$in_zone <- ifelse(
      is.finite(d$plate_x_in) & is.finite(d$plate_z_in),
      in_zone_inches(d$plate_x_in, d$plate_z_in),
      NA
    )
    
    d %>%
      dplyr::mutate(
        PitchType = factor(pt_map, levels = pitch_levels_all),
        # AAR helpers expected by aar_zone_df()
        type = as.character(PitchType),
        res  = dplyr::case_when(
          pitch_call == "StrikeCalled" ~ "Take",
          pitch_call %in% c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut",
                            "FoulBallFieldable","FoulBallNotFieldable","FoulTip") ~ "Swing",
          TRUE ~ NA_character_
        ),
        damage_event = is_bip_txst(pitch_call, play_result) & is.finite(ev) & ev >= 95.0,
        whiff_event  = (pitch_call == "StrikeSwinging")
      )
  }) |> bindCache(input$Hitter, input$hit_Bullpens, input$Game, input$PitcherHand)

  # ---- Leaderboard (all TXST hitters; independent of sidebar) ----
  leaderboard_data <- reactive({
    d <- txst_df
    if (is.null(d) || !nrow(d)) return(d)

    # Season filter
    season_col <- if ("SeasonTag" %in% names(d)) "SeasonTag" else if ("SeasonGroup" %in% names(d)) "SeasonGroup" else NULL
    sel_seasons <- input$hit_leader_seasons
    if (is.null(sel_seasons)) sel_seasons <- character(0)
    if (!is.null(season_col) && length(sel_seasons)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sel_seasons)
    }

    # Pitcher hand filter (LHP / RHP)
    sel_hands <- input$hit_leader_hand
    if (is.null(sel_hands)) sel_hands <- character(0)
    if (length(sel_hands) && "PitcherHand" %in% names(d)) {
      d <- d %>% dplyr::filter(.data$PitcherHand %in% sel_hands)
    }

    # Date range filter (GameDate -> Date -> filename)
    date_vec <- dplyr::coalesce(d$GameDate, parse_date_any(d$Date), extract_date_from_filename(d$source_file))
    if (!is.null(input$hit_leader_dates) && length(input$hit_leader_dates) == 2) {
      d_start <- as.Date(input$hit_leader_dates[[1]])
      d_end   <- as.Date(input$hit_leader_dates[[2]])
      if (!is.na(d_start) || !is.na(d_end)) {
        keep <- rep(TRUE, nrow(d))
        if (!is.na(d_start)) keep <- keep & (date_vec >= d_start)
        if (!is.na(d_end))   keep <- keep & (date_vec <= d_end)
        d <- d[keep, , drop = FALSE]
      }
    }

    # Deduplicate pitches to avoid inflated counts
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    if (!("pitch_uid" %in% names(d)) && !("row_id" %in% names(d)) &&
        all(c("Date","Inning","PAofInning","PitchofPA") %in% names(d))) {
      d <- d %>% dplyr::distinct(Date, Inning, PAofInning, PitchofPA, .keep_all = TRUE)
    }
    d
  })

  leaderboard_summary <- function(d){
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    if (!"Batter" %in% names(d)) return(tibble::tibble())

    d <- d %>% dplyr::filter(!is.na(.data$Batter), nzchar(trimws(as.character(.data$Batter))))
    if (!nrow(d)) return(tibble::tibble())

    # ---- Align ALL prep with performance table (dat_filt) ----
    n <- nrow(d)
    if (!"pitch_call" %in% names(d)) {
      d$pitch_call <- dplyr::coalesce(
        .get_chr(d, c("PitchCall","Pitch_Call","PitchResult","Pitch_Result","Call")),
        rep(NA_character_, n)
      )
    }
    if (!"play_result" %in% names(d)) {
      d$play_result <- dplyr::coalesce(
        .get_chr(d, c("PlayResult","Play_Result","Result","Event")),
        rep(NA_character_, n)
      )
    }
    # Match dat_filt swing/whiff/contact logic
    d$is_swing   <- d$pitch_call %in% SWING_LIKE
    d$is_whiff   <- d$pitch_call %in% c("StrikeSwinging")
    d$is_contact <- d$is_swing & !d$is_whiff
    # Match dat_filt zone prep (inches)
    d$plate_x_in <- zone_s_in(d$plate_x)
    d$plate_z_in <- zone_h_in(d$plate_z)
    d$plate_x_in <- to_num(d$plate_x_in)
    d$plate_z_in <- to_num(d$plate_z_in)
    d$in_zone <- ifelse(
      is.finite(d$plate_x_in) & is.finite(d$plate_z_in),
      in_zone_inches(d$plate_x_in, d$plate_z_in),
      NA
    )

    hitters <- unique(nz_chr(as.character(d$Batter)))
    hitters <- hitters[nzchar(hitters)]
    if (!length(hitters)) return(tibble::tibble())

    calc_row <- function(h){
      dd <- d[d$Batter == h, , drop = FALSE]
      if (!nrow(dd)) return(NULL)

      # Base stats = exact performance-table definitions
      s <- summarize_overall(dd)

      # Z-Swing% for PDF grid
      balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), dd)
      strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), dd)
      sw <- swing_table(dd, balls_col, strikes_col)
      z_seen   <- sum(sw$in_zone, na.rm = TRUE)
      z_swings <- sum(sw$is_swing & sw$in_zone, na.rm = TRUE)
      z_swing  <- safe_ratio(z_swings, z_seen)

      # Max EV on BIP (PA-level)
      pa <- pa_last_table(dd, balls_col, strikes_col)
      bip <- is_bip_txst(pa$pc, pa$pr)
      max_ev <- if (any(bip %in% TRUE & is.finite(pa$ev_pa))) {
        suppressWarnings(max(pa$ev_pa[bip %in% TRUE], na.rm = TRUE))
      } else NA_real_

      s$`Z-Swing%` <- z_swing
      s$`Max EV`   <- max_ev
      s$Hitter     <- name_display(h)
      s
    }

    out <- dplyr::bind_rows(lapply(hitters, calc_row))
    if (is.null(out) || !nrow(out)) return(tibble::tibble())

    out %>%
      dplyr::mutate(PA = ifelse(is.finite(.data$PA), as.integer(.data$PA), NA_integer_)) %>%
      dplyr::select(
        Hitter, PA, wOBA, wOBAcon, OBP, SLG, OPS, hRV,
        `K%`, `BB%`, `Barrel%`,
        `Contact%`, `Z-Contact%`, `Whiff%`, `IZ-Whiff%`,
        `Chase%`, `Pre2K Chase%`, `2K Chase%`, `Z-Swing%`,
        `EV>95%`, `90th EV`, `LD+FB%`, `Airpull%`,
        `Max EV`
      ) %>%
      dplyr::arrange(.data$Hitter)
  }

  leaderboard_team_summary <- function(d){
    if (is.null(d) || !nrow(d)) return(tibble::tibble())
    
    # ---- Align ALL prep with performance table (dat_filt) ----
    n <- nrow(d)
    if (!"pitch_call" %in% names(d)) {
      d$pitch_call <- dplyr::coalesce(
        .get_chr(d, c("PitchCall","Pitch_Call","PitchResult","Pitch_Result","Call")),
        rep(NA_character_, n)
      )
    }
    if (!"play_result" %in% names(d)) {
      d$play_result <- dplyr::coalesce(
        .get_chr(d, c("PlayResult","Play_Result","Result","Event")),
        rep(NA_character_, n)
      )
    }
    d$is_swing   <- d$pitch_call %in% SWING_LIKE
    d$is_whiff   <- d$pitch_call %in% c("StrikeSwinging")
    d$is_contact <- d$is_swing & !d$is_whiff
    d$plate_x_in <- zone_s_in(d$plate_x)
    d$plate_z_in <- zone_h_in(d$plate_z)
    d$plate_x_in <- to_num(d$plate_x_in)
    d$plate_z_in <- to_num(d$plate_z_in)
    d$in_zone <- ifelse(
      is.finite(d$plate_x_in) & is.finite(d$plate_z_in),
      in_zone_inches(d$plate_x_in, d$plate_z_in),
      NA
    )
    
    s <- summarize_overall(d)
    
    balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
    strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
    sw <- swing_table(d, balls_col, strikes_col)
    z_seen   <- sum(sw$in_zone, na.rm = TRUE)
    z_swings <- sum(sw$is_swing & sw$in_zone, na.rm = TRUE)
    z_swing  <- safe_ratio(z_swings, z_seen)
    
    pa <- pa_last_table(d, balls_col, strikes_col)
    bip <- is_bip_txst(pa$pc, pa$pr)
    max_ev <- if (any(bip %in% TRUE & is.finite(pa$ev_pa))) {
      suppressWarnings(max(pa$ev_pa[bip %in% TRUE], na.rm = TRUE))
    } else NA_real_
    
    s$`Z-Swing%` <- z_swing
    s$`Max EV`   <- max_ev
    s$Hitter     <- "Team Total"
    
    s %>%
      dplyr::mutate(PA = ifelse(is.finite(.data$PA), as.integer(.data$PA), NA_integer_)) %>%
      dplyr::select(
        Hitter, PA, wOBA, wOBAcon, OBP, SLG, OPS, hRV,
        `K%`, `BB%`, `Barrel%`,
        `Contact%`, `Z-Contact%`, `Whiff%`, `IZ-Whiff%`,
        `Chase%`, `Pre2K Chase%`, `2K Chase%`, `Z-Swing%`,
        `EV>95%`, `90th EV`, `LD+FB%`, `Airpull%`,
        `Max EV`
      )
  }

  apply_leaderboard_shading <- function(tbl_num, tbl_fmt){
    apply_d1_shading(tbl_num, tbl_fmt, palette = "player")
  }

  output$hit_leaderboard_table <- DT::renderDT({
    d <- leaderboard_data()
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    stats_num <- leaderboard_summary(d)
    if (is.null(stats_num) || !nrow(stats_num)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }

    tab_fmt <- stats_num %>% format_perf_table()
    tab_shaded <- apply_leaderboard_shading(stats_num, tab_fmt)

    leader_cols <- c(
      "Hitter","PA","wOBA","wOBAcon","OBP","SLG","OPS","hRV",
      "K%","BB%","Barrel%",
      "Contact%","Z-Contact%","Whiff%","IZ-Whiff%",
      "Chase%","Pre2K Chase%","2K Chase%","Z-Swing%",
      "EV>95%","90th EV","LD+FB%","Airpull%"
    )

    tab_shaded <- tab_shaded %>% dplyr::select(dplyr::all_of(leader_cols))
    stats_num  <- stats_num  %>% dplyr::select(dplyr::all_of(leader_cols))

    sort_cols <- stats_num %>% dplyr::select(-Hitter)
    names(sort_cols) <- paste0("..sort_", names(sort_cols))

    tab_out <- cbind(tab_shaded, sort_cols)

    display_cols <- names(tab_shaded)
    sort_cols_names <- names(sort_cols)
    display_idx0 <- seq_along(display_cols) - 1
    sort_idx0 <- seq_along(sort_cols_names) - 1 + length(display_cols)

    order_defs <- lapply(seq_along(display_cols), function(i) {
      nm <- display_cols[[i]]
      if (nm == "Hitter") {
        list(targets = display_idx0[[i]], orderDataType = "string")
      } else {
        sname <- paste0("..sort_", nm)
        sidx <- sort_idx0[match(sname, sort_cols_names)]
        list(targets = display_idx0[[i]], orderData = sidx)
      }
    })

    make_container <- function(first_col_label = "Hitter"){
      htmltools::withTags(
        table(class = "display",
              thead(
                tr(class = "group-header",
                   th(colspan = 11, ""),                                 # name + PA + rate/quality
                   th(colspan = 4, class = "group-label", "HIT"),
                   th(colspan = 4, class = "group-label", "STRIKES"),
                   th(colspan = 4, class = "group-label", "HARD")
                ),
                tr(
                  th(first_col_label), th("PA"), th("wOBA"), th("wOBAcon"), th("OBP"), th("SLG"), th("OPS"), th("hRV"),
                  th("K%"), th("BB%"), th("Barrel%"),
                  th("Contact%"), th("Z-Contact%"), th("Whiff%"), th("IZ-Whiff%"),
                  th("Chase%"), th("Pre2K Chase%"), th("2K Chase%"), th("IZ Swing%"),
                  th("EV>95%"), th("90th EV"), th("LD+FB%"), th("Airpull%")
                )
              )
        )
      )
    }

    DT::datatable(
      tab_out,
      container = make_container("Hitter"),
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
          list(list(className = "grp-start", targets = c(11,15,19))),
          order_defs
        )
      ),
      class = "stripe"
    )
  })

  output$leaderboard_team_table <- DT::renderDT({
    make_container <- function(first_col_label = "Team"){
      htmltools::withTags(
        table(class = "display",
              thead(
                tr(class = "group-header",
                   th(colspan = 11, ""),                                 # name + PA + rate/quality
                   th(colspan = 4, class = "group-label", "HIT"),
                   th(colspan = 4, class = "group-label", "STRIKES"),
                   th(colspan = 4, class = "group-label", "HARD")
                ),
                tr(
                  th(first_col_label), th("PA"), th("wOBA"), th("wOBAcon"), th("OBP"), th("SLG"), th("OPS"), th("hRV"),
                  th("K%"), th("BB%"), th("Barrel%"),
                  th("Contact%"), th("Z-Contact%"), th("Whiff%"), th("IZ-Whiff%"),
                  th("Chase%"), th("Pre2K Chase%"), th("2K Chase%"), th("IZ Swing%"),
                  th("EV>95%"), th("90th EV"), th("LD+FB%"), th("Airpull%")
                )
              )
        )
      )
    }
    d <- leaderboard_data()
    if (is.null(d) || !nrow(d)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }
    
    stats_num <- leaderboard_team_summary(d)
    if (is.null(stats_num) || !nrow(stats_num)) {
      return(DT::datatable(data.frame(Status = "No data"),
                           rownames = FALSE, options = list(dom='t', paging=FALSE)))
    }
    
    tab_fmt <- stats_num %>% format_perf_table()
    tab_shaded <- apply_leaderboard_shading(stats_num, tab_fmt)
    
    leader_cols <- c(
      "Hitter","PA","wOBA","wOBAcon","OBP","SLG","OPS","hRV",
      "K%","BB%","Barrel%",
      "Contact%","Z-Contact%","Whiff%","IZ-Whiff%",
      "Chase%","Pre2K Chase%","2K Chase%","Z-Swing%",
      "EV>95%","90th EV","LD+FB%","Airpull%"
    )
    
    tab_shaded <- tab_shaded %>% dplyr::select(dplyr::all_of(leader_cols))
    stats_num  <- stats_num  %>% dplyr::select(dplyr::all_of(leader_cols))
    
    DT::datatable(
      tab_shaded,
      container = make_container("Team"),
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = 't',
        paging   = FALSE,
        ordering = FALSE,
        stripe   = TRUE,
        scrollX  = TRUE,
        columnDefs = c(
          list(list(className = "grp-start", targets = c(11,15,19)))
        )
      ),
      class = "stripe"
    )
  })

  render_leaderboard_pdf <- function(outfile, data_df) {
    maroon <- "#501214"
    gold   <- "#B4975A"
    date_str <- format(Sys.Date(), "%m-%d-%Y")

    logo_path <- get0(
      "BASE_HITTING_REPORT_LOGO_PATH",
      inherits = FALSE,
      ifnotfound = "www/baseballTS logo gold.png"
    )
    logo_img <- NULL
    if (file.exists(logo_path)) {
      logo_img <- tryCatch(png::readPNG(logo_path), error = function(e) NULL)
    }

    d1_pct <- c(
      `Z-Contact%` = D1_PCT[["Z-Contact%"]],
      `Chase%`     = D1_PCT[["Chase%"]],
      `Barrel%`    = D1_PCT[["Barrel%"]],
      `Z-Swing%`   = D1_PCT[["Z-Swing%"]]
    )
    d1_non <- c(
      `90th EV` = D1_NON[["90th EV"]]
    )

    top10_tbl <- function(df, col, label, higher = TRUE, fmt = function(x) x) {
      if (is.null(df) || !nrow(df) || !"Hitter" %in% names(df) || !(col %in% names(df))) {
        blank <- data.frame(Hitter = rep(" ", 10), Value = rep(" ", 10), stringsAsFactors = FALSE)
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
      if (!nrow(df)) return(data.frame(Hitter = character(0), Value = character(0)))
      ord <- order(if (higher) -v_num else v_num, df$Hitter, na.last = TRUE)
      df <- df[ord, , drop = FALSE]
      df <- head(df, 10)
      value_vec <- fmt(df[[col]])
      out <- data.frame(
        Hitter = df$Hitter,
        Value = value_vec,
        stringsAsFactors = FALSE
      )
      if (nrow(out) < 10) {
        pad <- data.frame(
          Hitter = rep(" ", 10 - nrow(out)),
          Value = rep(" ", 10 - nrow(out)),
          stringsAsFactors = FALSE
        )
        out <- rbind(out, pad)
      }
      list(display = out, values = df[[col]])
    }

    fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.0f%%", 100 * x), "NA")
    fmt_num1 <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "NA")

    defs <- list(
      list(name = "Z-Contact%", col = "Z-Contact%", higher = TRUE,  fmt = fmt_pct),
      list(name = "Chase%",     col = "Chase%",     higher = FALSE, fmt = fmt_pct),
      list(name = "Z-Swing%",   col = "Z-Swing%",   higher = TRUE,  fmt = fmt_pct),
      list(name = "Max EV",     col = "Max EV",     higher = TRUE,  fmt = fmt_num1),
      list(name = "90th EV",    col = "90th EV",    higher = TRUE,  fmt = fmt_num1),
      list(name = "Barrel%",    col = "Barrel%",    higher = TRUE,  fmt = fmt_pct)
    )

    make_stat_block <- function(title, df_info, higher = TRUE) {
      df <- df_info$display

      # zebra fills (Hitter col only) + conditional green for Value col
      n <- nrow(df)
      zebra1 <- "#FFFFFF"
      zebra2 <- "#F5F5F5"
      fills <- matrix(rep(c(zebra1, zebra2), length.out = n), nrow = n, ncol = 2, byrow = FALSE)

      val_num <- df_info$values
      if (length(val_num) < n) val_num <- c(val_num, rep(NA_real_, n - length(val_num)))

      if (title %in% names(d1_pct)) {
        avg <- d1_pct[[title]]
        if (is.finite(avg)) {
          if (title == "Z-Swing%") {
            good <- val_num > 0.68
          } else {
            tol <- 0.05
            good <- if (higher) (val_num > avg * (1 + tol)) else (val_num < avg * (1 - tol))
          }
          fills[good, 2] <- STAT_GOOD_SOLID
        }
      }
      if (title %in% names(d1_non)) {
        avg <- d1_non[[title]]
        if (is.finite(avg)) {
          good <- val_num > (avg + 2.5)
          fills[good, 2] <- STAT_GOOD_SOLID
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
      # align Hitter column center, Value column right
      core_idx <- which(tbl_g$layout$name == "core-fg")
      if (length(core_idx)) {
        l_vals <- sort(unique(tbl_g$layout$l[core_idx]))
        if (length(l_vals) >= 2) {
          hit_col_l <- l_vals[1]
          val_col_l <- l_vals[2]
          idx_hit <- which(tbl_g$layout$name == "core-fg" & tbl_g$layout$l == hit_col_l)
          for (i in idx_hit) {
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
      # two columns: Hitter 2/3 + Value 1/3
      tbl_g$widths <- grid::unit(c(0.67, 0.33), "npc")

      # bold leader names/values (including ties)
      core_idx <- which(tbl_g$layout$name == "core-fg")
      if (length(core_idx)) {
        l_vals <- sort(unique(tbl_g$layout$l[core_idx]))
        t_vals <- sort(unique(tbl_g$layout$t[core_idx]))
        if (length(l_vals) >= 2 && length(t_vals) >= 1) {
          hit_col_l <- l_vals[1]
          val_col_l <- l_vals[2]
          if (length(val_num)) {
            top_val <- if (higher) suppressWarnings(max(val_num, na.rm = TRUE)) else suppressWarnings(min(val_num, na.rm = TRUE))
            top_rows <- which(is.finite(val_num) & val_num == top_val)
            top_t <- t_vals[top_rows]
            if (length(top_t)) {
              idx_bold <- which(tbl_g$layout$name == "core-fg" &
                                  tbl_g$layout$t %in% top_t &
                                  tbl_g$layout$l %in% c(hit_col_l, val_col_l))
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
        fmt = d$fmt
      )
      make_stat_block(d$name, df_top, higher = d$higher)
    })

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

      gridExtra::arrangeGrob(grobs = as.vector(t(mat)), ncol = cols,
                             widths = widths, heights = heights)
    }

    grid_body <- grid_with_spacers(blocks)

    header_g <- grid::grobTree(
      if (!is.null(logo_img)) grid::rasterGrob(logo_img, x = 0.05, y = 0.66, width = 0.08, just = c("left","center")) else grid::nullGrob(),
      if (!is.null(logo_img)) grid::rasterGrob(logo_img, x = 0.95, y = 0.66, width = 0.08, just = c("right","center")) else grid::nullGrob(),
      grid::textGrob("Hit Strikes Hard Leaderboard", x = 0.5, y = 0.75,
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

  output$leaderboard_pdf <- downloadHandler(
    filename = function() {
      paste0("Hit_Strikes_Hard_Leaderboard_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      d <- leaderboard_data()
      stats_df <- leaderboard_summary(d)
      render_leaderboard_pdf(file, stats_df)
    }
  )

  # -------------------- Lineup Builder helpers --------------------
  lineup_positions <- c("DH","C","1B","2B","3B","SS","LF","CF","RF")
  lineup_stat_choices <- c(
    "PA","wOBA","wOBAcon","OBP","SLG","OPS","hRV",
    "K%","BB%","Barrel%","Contact%","Z-Contact%","Whiff%","IZ-Whiff%",
    "Chase%","Pre2K Chase%","2K Chase%","EV>95%","90th EV","LD+FB%","Airpull%"
  )
  lineup1_pos_ids <- paste0("lineup1_pos_", 1:9)
  lineup1_player_ids <- paste0("lineup1_player_", 1:9)
  lineup1_stat_ids <- paste0("lineup1_stat_", 1:6)
  
  lineup2_pos_ids <- paste0("lineup2_pos_", 1:9)
  lineup2_player_ids <- paste0("lineup2_player_", 1:9)
  lineup2_stat_ids <- paste0("lineup2_stat_", 1:6)

  lineup_pool_data <- reactive({
    d <- if (isTRUE(input$hit_Bullpens)) {
      bullpens_df
    } else {
      dd <- df
      if ("is_bullpen" %in% names(dd)) dd <- dd %>% dplyr::filter(!(is_bullpen %in% TRUE))
      dd
    }
    
    # Restrict to team when possible
    d <- filter_team(d, "BatterTeam")
    
    d
  }) |> bindCache(input$hit_Bullpens)
  
  lineup_prepped_data <- reactive({
    d <- lineup_pool_data()
    if (!nrow(d)) return(d)
    
    d$is_swing <- d$pitch_call %in% SWING_LIKE
    if (!("is_whiff" %in% names(d))) {
      d$is_whiff <- d$pitch_call %in% c("StrikeSwinging")
    }
    if (!("is_contact" %in% names(d))) {
      d$is_contact <- d$is_swing & !d$is_whiff
    }
    
    # Authoritative zone (inches)
    d$plate_x_in <- zone_s_in(d$plate_x)
    d$plate_z_in <- zone_h_in(d$plate_z)
    d$plate_x_in <- to_num(d$plate_x_in)
    d$plate_z_in <- to_num(d$plate_z_in)
    
    d$in_zone <- ifelse(
      is.finite(d$plate_x_in) & is.finite(d$plate_z_in),
      in_zone_inches(d$plate_x_in, d$plate_z_in),
      NA
    )
    
    d
  }) |> bindCache(input$hit_Bullpens)
  
  lineup_data_season <- reactive({
    d <- lineup_prepped_data()
    if (isTRUE(input$hit_Bullpens)) return(d)
    season_col <- if ("SeasonTag" %in% names(d)) "SeasonTag" else if ("SeasonGroup" %in% names(d)) "SeasonGroup" else NULL
    sg <- input$hit_season_groups
    if (!is.null(season_col) && !is.null(sg) && length(sg)) {
      d <- d %>% dplyr::filter(.data[[season_col]] %in% sg)
    }
    d
  }) |> bindCache(input$hit_Bullpens, input$hit_season_groups)
  
  lineup_season_game_ids <- function(player, sg){
    if (isTRUE(input$hit_Bullpens)) {
      dd <- bullpens_df %>%
        dplyr::filter((.data$Batter == player) | (.data$Pitcher == player))
      gid <- if ("CustomGameID_BP" %in% names(dd)) dd$CustomGameID_BP else dd$CustomGameID
      gids <- nz_chr(gid)
      return(unique(gids[nzchar(gids)]))
    }
    
    dd <- df %>%
      dplyr::filter(.data$Batter == player, !(is_bullpen %in% TRUE))
    
    season_col <- if ("SeasonTag" %in% names(dd)) "SeasonTag" else if ("SeasonGroup" %in% names(dd)) "SeasonGroup" else NULL
    gid <- if ("CustomGameID" %in% names(dd)) dd$CustomGameID else character(0)
    gids_all <- unique(nz_chr(gid))
    gids_all <- gids_all[nzchar(gids_all)]
    
    if (is.null(sg) || !length(sg) || is.null(season_col)) return(gids_all)
    gids <- nz_chr(gid[dd[[season_col]] %in% sg])
    gids <- gids[nzchar(gids)]
    if (!length(gids)) gids_all else unique(gids)
  }

  lineup_perf_data_for_player <- function(player, sg, vs_hand = "All"){
    if (isTRUE(input$hit_Bullpens)) {
      dd <- bullpens_df %>% dplyr::filter((.data$Batter == player) | (.data$Pitcher == player))
      return(dd)
    }
    
    dd <- df %>%
      dplyr::filter(.data$Batter == player, !(is_bullpen %in% TRUE))
    
    season_col <- if ("SeasonTag" %in% names(dd)) "SeasonTag" else if ("SeasonGroup" %in% names(dd)) "SeasonGroup" else NULL
    if (!is.null(season_col) && !is.null(sg) && length(sg)) {
      dd <- dd %>% dplyr::filter(.data[[season_col]] %in% sg)
    }
    
    if (!is.null(vs_hand) && vs_hand != "All" && "PitcherHand" %in% names(dd)) {
      dd <- dd %>% dplyr::filter(.data$PitcherHand == vs_hand)
    }
    
    # match dat_filt preprocessing
    dd$is_swing <- dd$pitch_call %in% SWING_LIKE
    if (!("is_whiff" %in% names(dd))) {
      dd$is_whiff <- dd$pitch_call %in% c("StrikeSwinging")
    }
    if (!("is_contact" %in% names(dd))) {
      dd$is_contact <- dd$is_swing & !dd$is_whiff
    }
    
    dd$plate_x_in <- zone_s_in(dd$plate_x)
    dd$plate_z_in <- zone_h_in(dd$plate_z)
    dd$plate_x_in <- to_num(dd$plate_x_in)
    dd$plate_z_in <- to_num(dd$plate_z_in)
    
    dd$in_zone <- ifelse(
      is.finite(dd$plate_x_in) & is.finite(dd$plate_z_in),
      in_zone_inches(dd$plate_x_in, dd$plate_z_in),
      NA
    )
    
    dd
  }

  lineup_player_choices <- reactive({
    d <- lineup_data_season()
    if (!("Batter" %in% names(d))) return(character(0))
    players <- unique(nz_chr(d$Batter))
    players <- players[nzchar(players)]
    if (!length(players)) return(character(0))
    disp <- name_display(players)
    ord <- order(disp)
    stats::setNames(players[ord], disp[ord])
  })

  lineup_player_meta <- reactive({
    d <- lineup_data_season()
    if (!("Batter" %in% names(d))) {
      return(tibble::tibble(value = character(0), label = character(0), bats = character(0)))
    }
    
    most_common <- function(x){
      x <- nz_chr(x)
      x <- x[nzchar(x)]
      if (!length(x)) return(NA_character_)
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    bats_map <- if ("bats" %in% names(d)) {
      d %>%
        dplyr::mutate(Batter = as.character(.data$Batter)) %>%
        dplyr::filter(nzchar(.data$Batter)) %>%
        dplyr::group_by(.data$Batter) %>%
        dplyr::summarise(bats = most_common(.data$bats), .groups = "drop")
    } else {
      tibble::tibble(Batter = character(0), bats = character(0))
    }
    
    players <- unique(nz_chr(as.character(d$Batter)))
    players <- players[nzchar(players)]
    disp <- name_display(players)
    
    meta <- tibble::tibble(value = as.character(players), label = disp) %>%
      dplyr::left_join(bats_map, by = c("value" = "Batter"))
    
    meta <- meta %>% dplyr::arrange(.data$label)
    meta
  })

  lineup_selected_players <- function(ids){
    vals <- sapply(ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    vals[nzchar(vals)]
  }
  
  lineup_player_stats <- function(player_ids, vs_input_id){
    reactive({
      players <- lineup_selected_players(player_ids)
      if (!length(players)) {
        return(list(num = tibble::tibble(), fmt = tibble::tibble()))
      }
      sg <- input$hit_season_groups
      vs_hand <- input[[vs_input_id]]
      if (is.null(vs_hand) || !nzchar(vs_hand)) vs_hand <- "All"
      
      out_num <- dplyr::bind_rows(lapply(players, function(p){
        dd <- lineup_perf_data_for_player(p, sg, vs_hand)
        summarize_overall(dd) %>% dplyr::mutate(Player = p, .before = 1)
      }))
      out_fmt <- format_perf_table(out_num)
      out_fmt <- apply_d1_shading(out_num, out_fmt)
      list(num = out_num, fmt = out_fmt)
    })
  }
  
  lineup_total_stats <- function(player_ids, vs_input_id){
    reactive({
      players <- lineup_selected_players(player_ids)
      if (!length(players)) {
        num <- summarize_overall(df[0, , drop = FALSE])
        fmt <- format_perf_table(num)
        fmt <- apply_d1_shading(num, fmt)
        return(list(num = num, fmt = fmt))
      }
      sg <- input$hit_season_groups
      vs_hand <- input[[vs_input_id]]
      if (is.null(vs_hand) || !nzchar(vs_hand)) vs_hand <- "All"
      
      dd_list <- lapply(players, function(p){
        lineup_perf_data_for_player(p, sg, vs_hand)
      })
      dd <- dplyr::bind_rows(dd_list)
      num <- summarize_overall(dd)
      fmt <- format_perf_table(num)
      fmt <- apply_d1_shading(num, fmt)
      list(num = num, fmt = fmt)
    })
  }

  lineup1_player_stats <- lineup_player_stats(lineup1_player_ids, "lineup1_vs")
  lineup1_total_stats  <- lineup_total_stats(lineup1_player_ids, "lineup1_vs")
  lineup2_player_stats <- lineup_player_stats(lineup2_player_ids, "lineup2_vs")
  lineup2_total_stats  <- lineup_total_stats(lineup2_player_ids, "lineup2_vs")

  observe_lineup_positions <- function(pos_ids){
    observe({
      selected <- sapply(pos_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else as.character(v)
      })
      names(selected) <- pos_ids
      
      # Keep first occurrence, clear duplicates
      dup <- duplicated(selected) & nzchar(selected)
      if (any(dup)) selected[dup] <- ""
      
      for (id in pos_ids) {
        cur <- selected[[id]]
        used <- selected[setdiff(names(selected), id)]
        used <- used[nzchar(used)]
        choices <- setdiff(lineup_positions, used)
        choices_named <- c("—" = "", stats::setNames(choices, choices))
        sel <- if (nzchar(cur) && cur %in% lineup_positions) cur else ""
        updateSelectInput(session, id, choices = choices_named, selected = sel)
      }
    })
  }
  
  observe_lineup_players <- function(player_ids){
    observe({
      meta_all <- lineup_player_meta()
      selected <- sapply(player_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else nz_chr(v)
      })
      names(selected) <- player_ids
      
      # Keep first occurrence, clear duplicates
      dup <- duplicated(selected) & nzchar(selected)
      if (any(dup)) selected[dup] <- ""
      
      for (id in player_ids) {
        cur <- selected[[id]]
        used <- selected[setdiff(names(selected), id)]
        used <- used[nzchar(used)]
        
        avail_meta <- meta_all %>%
          dplyr::filter(!(.data$value %in% used))
        
        choices <- c("—" = "", stats::setNames(avail_meta$value, avail_meta$label))
        content <- c("", ifelse(avail_meta$bats == "L",
                                paste0("<span class='lineup-bats-l'>", avail_meta$label, "</span>"),
                                avail_meta$label))
        
        sel <- if (nzchar(cur) && cur %in% meta_all$value) cur else ""
        updatePickerInput(session, id, choices = choices, selected = sel,
                          choicesOpt = list(content = content))
      }
    })
  }
  
  observe_lineup_stats <- function(stat_ids){
    observe({
      selected <- sapply(stat_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else as.character(v)
      })
      names(selected) <- stat_ids
      
      # Keep first occurrence, clear duplicates
      dup <- duplicated(selected) & nzchar(selected)
      if (any(dup)) selected[dup] <- ""
      
      for (id in stat_ids) {
        cur <- selected[[id]]
        used <- selected[setdiff(names(selected), id)]
        used <- used[nzchar(used)]
        choices <- setdiff(lineup_stat_choices, used)
        choices_named <- c("—" = "", stats::setNames(choices, choices))
        sel <- if (nzchar(cur) && cur %in% lineup_stat_choices) cur else ""
        updateSelectInput(session, id, choices = choices_named, selected = sel)
      }
    })
  }
  
  # Clear duplicates within each lineup (positions, players, stats)
  observe({
    selected <- sapply(lineup1_pos_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup1_pos_ids[dup]) updateSelectInput(session, id, selected = "")
    }
  })
  observe({
    selected <- sapply(lineup2_pos_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup2_pos_ids[dup]) updateSelectInput(session, id, selected = "")
    }
  })
  observe({
    selected <- sapply(lineup1_player_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup1_player_ids[dup]) updatePickerInput(session, id, selected = "")
    }
  })
  observe({
    selected <- sapply(lineup2_player_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup2_player_ids[dup]) updatePickerInput(session, id, selected = "")
    }
  })
  observe({
    selected <- sapply(lineup1_stat_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup1_stat_ids[dup]) updateSelectInput(session, id, selected = "")
    }
  })
  observe({
    selected <- sapply(lineup2_stat_ids, function(id){
      v <- input[[id]]
      if (is.null(v)) "" else as.character(v)
    })
    dup <- duplicated(selected) & nzchar(selected)
    if (any(dup)) {
      for (id in lineup2_stat_ids[dup]) updateSelectInput(session, id, selected = "")
    }
  })
  
  observeEvent(input$lineup_clear_all, {
    for (id in c(lineup1_pos_ids, lineup2_pos_ids)) {
      updateSelectInput(session, id, selected = "")
    }
    for (id in c(lineup1_player_ids, lineup2_player_ids)) {
      updatePickerInput(session, id, selected = "")
    }
  }, ignoreInit = TRUE)

  output$lineup_builder_ui <- renderUI({
    meta_all <- lineup_player_meta()
    meta_choices <- stats::setNames(meta_all$value, meta_all$label)
    meta_content <- if (nrow(meta_all)) {
      ifelse(meta_all$bats == "L",
             paste0("<span class='lineup-bats-l'>", meta_all$label, "</span>"),
             meta_all$label)
    } else character(0)
    
    as_cell <- function(val){
      if (is.null(val) || is.na(val)) return("")
      val_chr <- as.character(val)
      if (grepl("<[^>]+>", val_chr)) htmltools::HTML(val_chr) else val_chr
    }
    
    build_lineup_table <- function(label, vs_id, pos_ids, player_ids, stat_ids, stats_data, total_data){
      stat_vals <- sapply(stat_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else as.character(v)
      })
      vs_val <- input[[vs_id]]
      if (is.null(vs_val) || !nzchar(as.character(vs_val))) vs_val <- "All"
      
      pos_selected <- sapply(pos_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else as.character(v)
      })
      
      player_selected <- sapply(player_ids, function(id){
        v <- input[[id]]
        if (is.null(v)) "" else as.character(v)
      })
      
      get_player_value <- function(stat, player){
        if (!nzchar(stat) || !nzchar(player)) return("")
        num <- stats_data$num
        fmt <- stats_data$fmt
        if (!nrow(num)) return("")
        idx <- which(num$Player == player)
        if (!length(idx)) return("")
        if (stat == "PA") return(as.character(num$PA[idx][1]))
        val <- fmt[[stat]][idx][1]
        if (is.null(val) || is.na(val)) "" else as.character(val)
      }
      
      get_total_value <- function(stat){
        if (!nzchar(stat)) return("")
        num <- total_data$num
        fmt <- total_data$fmt
        if (!nrow(num)) return("")
        if (stat == "PA") return(as.character(num$PA[1]))
        val <- fmt[[stat]][1]
        if (is.null(val) || is.na(val)) "" else as.character(val)
      }
      
      tags$div(
        class = "mb-3",
        tags$div(
          class = "mb-2",
          tags$div(
            style = "display:flex; align-items:center; gap:12px;",
            tags$strong(label),
            selectInput(
              inputId = vs_id,
              label = NULL,
              choices = c("All","LHP","RHP"),
              selected = vs_val,
              width = "160px"
            )
          )
        ),
        tags$table(
          class = "table table-sm lineup-table",
          tags$thead(
            tags$tr(
              tags$th("#"),
              tags$th("Pos"),
              tags$th("Player"),
              lapply(seq_along(stat_ids), function(i){
                cur_stat <- stat_vals[[i]]
                used_stats <- stat_vals[-i]
                used_stats <- used_stats[nzchar(used_stats)]
                choices_stats <- setdiff(lineup_stat_choices, used_stats)
                if (nzchar(cur_stat) && !(cur_stat %in% choices_stats)) {
                  choices_stats <- c(cur_stat, choices_stats)
                }
                tags$th(
                  selectInput(
                    inputId = stat_ids[[i]],
                    label = NULL,
                    choices = c("—" = "", stats::setNames(choices_stats, choices_stats)),
                    selected = cur_stat,
                    width = "140px"
                  )
                )
              })
            )
          ),
          tags$tbody(
            lapply(1:9, function(i){
              pos_id <- pos_ids[[i]]
              ply_id <- player_ids[[i]]
              player_val <- input[[ply_id]]
              if (is.null(player_val)) player_val <- ""
              
              cur_pos <- pos_selected[[i]]
              used_pos <- pos_selected[-i]
              used_pos <- used_pos[nzchar(used_pos)]
              choices_pos <- setdiff(lineup_positions, used_pos)
              if (nzchar(cur_pos) && !(cur_pos %in% choices_pos)) {
                choices_pos <- c(cur_pos, choices_pos)
              }
              
              cur_player <- player_selected[[i]]
              used_players <- player_selected[-i]
              used_players <- used_players[nzchar(used_players)]
              avail_meta <- meta_all %>% dplyr::filter(!(.data$value %in% used_players))
              if (nzchar(cur_player) && !(cur_player %in% avail_meta$value)) {
                cur_row <- meta_all %>% dplyr::filter(.data$value == cur_player)
                avail_meta <- dplyr::bind_rows(cur_row, avail_meta)
              }
              choices_players <- c("—" = "", stats::setNames(avail_meta$value, avail_meta$label))
              content_players <- c("", ifelse(avail_meta$bats == "L",
                                              paste0("<span class='lineup-bats-l'>", avail_meta$label, "</span>"),
                                              avail_meta$label))
              
              tags$tr(
                tags$td(i),
                tags$td(selectInput(pos_id, label = NULL, choices = c("—" = "", stats::setNames(choices_pos, choices_pos)), selected = cur_pos, width = "100%")),
                tags$td(
                  pickerInput(
                    inputId = ply_id,
                    label = NULL,
                    choices = choices_players,
                    selected = cur_player,
                    choicesOpt = list(content = content_players),
                    options = list(`live-search` = TRUE, `none-selected-text` = "—", size = 10),
                    width = "100%"
                  )
                ),
                lapply(stat_vals, function(stat){
                  tags$td(as_cell(get_player_value(stat, as.character(player_val))))
                })
              )
            }),
            tags$tr(
              class = "lineup-total",
              tags$td("Total"),
              tags$td(""),
              tags$td("Total"),
              lapply(stat_vals, function(stat){
                tags$td(as_cell(get_total_value(stat)))
              })
            )
          )
        )
      )
    }
    
    tags$div(
      build_lineup_table("Lineup 1", "lineup1_vs", lineup1_pos_ids, lineup1_player_ids, lineup1_stat_ids, lineup1_player_stats(), lineup1_total_stats()),
      build_lineup_table("Lineup 2", "lineup2_vs", lineup2_pos_ids, lineup2_player_ids, lineup2_stat_ids, lineup2_player_stats(), lineup2_total_stats())
    )
  })
  
  
  # -------------------- Populate AAR Game dropdown --------------------
  
  filter_ev_bins <- function(d, bins){
    if (is.null(bins) || !length(bins)) return(d[0, , drop = FALSE])
    
    ev <- to_num(d$ev)
    keep <- rep(FALSE, nrow(d))
    
    for (b in bins) {
      if (b == "<80")      keep <- keep | (is.finite(ev) & ev < 80)
      if (b == "80-90")    keep <- keep | (is.finite(ev) & ev >= 80 & ev < 90)
      if (b == "90-95")    keep <- keep | (is.finite(ev) & ev >= 90 & ev < 95)
      if (b == "95-100")   keep <- keep | (is.finite(ev) & ev >= 95 & ev < 100)
      if (b == "100+")     keep <- keep | (is.finite(ev) & ev >= 100)
    }
    
    d[keep, , drop = FALSE]
  }
  
  spray_dat <- reactive({
    d <- dat_filt()
    d <- filter_ev_bins(d, input$ev_bins)
    filter_spray_buckets(d)
  })
  
  # -------------------- Damage / Whiff heat maps (Pitching-app look) --------------------
  
  # --- Helper: safely grab the first non-NULL input value from a list of IDs ---
  get_first_input_value <- function(input, ids, default = "All") {
    for (id in ids) {
      val <- input[[id]]
      if (!is.null(val) && !is.na(val) && nzchar(as.character(val))) {
        return(as.character(val))
      }
    }
    default
  }
  
  # -------------------- TruMedia-style MOVEMENT density helpers --------------------
  
  wblyrm_palette <- c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff")
  
  pick_col <- function(df, candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(NULL)
    hit[[1]]
  }
  
  calc_limits <- function(x, default = c(-25, 25), pad = 2, clamp = c(-35, 35)) {
    x <- x[is.finite(x)]
    if (length(x) < 5) return(default)
    lo <- max(clamp[1], quantile(x, 0.01, na.rm = TRUE) - pad)
    hi <- min(clamp[2], quantile(x, 0.99, na.rm = TRUE) + pad)
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi) return(default)
    c(as.numeric(lo), as.numeric(hi))
  }
  
  # Finds the pitch-type dropdown value on a page without needing the exact inputId
  get_pitchtype_selection <- function(input, context = c("damage", "whiff")) {
    context <- match.arg(context)
    in_list <- shiny::reactiveValuesToList(input)
    ids <- names(in_list)
    
    # context-specific IDs first (contains context + pitch + type)
    cand <- ids[
      grepl(context, ids, ignore.case = TRUE) &
        grepl("pitch", ids, ignore.case = TRUE) &
        grepl("type",  ids, ignore.case = TRUE)
    ]
    
    # fallback: any PitchType-like input
    if (length(cand) == 0) cand <- ids[grepl("PitchType", ids, ignore.case = TRUE)]
    
    if (length(cand) == 0) return("All")
    
    val <- in_list[[cand[[1]]]]
    if (is.null(val) || is.na(val)) return("All")
    trimws(as.character(val))
  }
  
  # --- Helper: player label for titles (works whether your app uses PitcherInput or BatterInput/HitterInput) ---
  get_player_label <- function(input) {
    get_first_input_value(input, c("PitcherInput", "BatterInput", "HitterInput", "PlayerInput"), default = "")
  }
  
  # =========================
  # 1) TOTAL DAMAGE heatmap
  # =========================
  output$damage_locations_total <- renderPlot(
    {
      d <- dat_filt() %>%
        dplyr::filter(damage_event %in% TRUE)
      
      
      hb_col  <- pick_col(d, c("HorzBreak", "HorizontalBreak", "HB", "pfx_x", "HorzBrk", "HBreak"))
      ivb_col <- pick_col(d, c("InducedVertBreak", "IVB", "pfx_z", "IndVertBreak", "VertBreak", "VBreak"))
      
      validate(need(!is.null(hb_col) && !is.null(ivb_col),
                    "Missing movement columns (need HorzBreak/HB and InducedVertBreak/IVB or equivalents)."))
      
      d <- d %>%
        dplyr::mutate(
          HB  = as.numeric(.data[[hb_col]]),
          IVB = as.numeric(.data[[ivb_col]])
        ) %>%
        dplyr::filter(is.finite(HB), is.finite(IVB), !is.na(PitchType))
      
      validate(need(nrow(d) > 10, "Not enough pitches to plot movement density."))
      
      xlim <- calc_limits(d$HB)
      ylim <- calc_limits(d$IVB)
      
      ggplot(d, aes(HB, IVB)) +
        stat_density_2d(
          aes(fill = after_stat(ndensity)),
          geom = "raster",
          contour = FALSE,
          n = 250,
          na.rm = TRUE
        ) +
        scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
        facet_wrap(~PitchType, ncol = 2) +
        geom_hline(yintercept = 0, linewidth = 0.7) +
        geom_vline(xintercept = 0, linewidth = 0.7) +
        labs(
          title = paste0(get_player_label(input), ": DAMAGE (TruMedia movement density)"),
          x = "Horizontal Break (in.)",
          y = "Induced Vertical Break (in.)"
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
        coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
    },
    height = function() {
      d <- dat_filt() %>%
        dplyr::filter(damage_event %in% TRUE, !is.na(PitchType))
      n_types <- length(unique(as.character(d$PitchType)))
      rows <- ceiling(max(1, n_types) / 2)
      as.integer(450 * rows + 100)
    }
  )
  
  # ============================================
  # 2) DAMAGE heatmap (PITCH TYPE FILTER page)
  # ============================================
  output$damage_locations_pitchtype <- renderPlot(
    {
      d <- dat_filt() %>%
        dplyr::filter(damage_event %in% TRUE)
      
      pt_sel <- get_pitchtype_selection(input, "damage")
      pt_sel_low <- tolower(pt_sel)
      
      if (pt_sel_low %in% c("all", "all pitches", "all pitch types")) pt_sel <- "All"
      if (!is.null(pt_sel) && !is.na(pt_sel) && pt_sel != "All") {
        d <- d %>% dplyr::filter(as.character(PitchType) == pt_sel)
      }
      
      hb_col  <- pick_col(d, c("HorzBreak", "HorizontalBreak", "HB", "pfx_x", "HorzBrk", "HBreak"))
      ivb_col <- pick_col(d, c("InducedVertBreak", "IVB", "pfx_z", "IndVertBreak", "VertBreak", "VBreak"))
      
      validate(need(!is.null(hb_col) && !is.null(ivb_col),
                    "Missing movement columns (need HorzBreak/HB and InducedVertBreak/IVB or equivalents)."))
      
      d <- d %>%
        dplyr::mutate(
          HB  = as.numeric(.data[[hb_col]]),
          IVB = as.numeric(.data[[ivb_col]])
        ) %>%
        dplyr::filter(is.finite(HB), is.finite(IVB), !is.na(PitchType))
      
      validate(need(nrow(d) > 10, "Not enough pitches to plot movement density for that pitch type."))
      
      xlim <- calc_limits(d$HB)
      ylim <- calc_limits(d$IVB)
      
      ggplot(d, aes(HB, IVB)) +
        stat_density_2d(
          aes(fill = after_stat(ndensity)),
          geom = "raster",
          contour = FALSE,
          n = 250,
          na.rm = TRUE
        ) +
        scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
        facet_wrap(~PitchType, ncol = 2) +
        geom_hline(yintercept = 0, linewidth = 0.7) +
        geom_vline(xintercept = 0, linewidth = 0.7) +
        labs(
          title = paste0(get_player_label(input), ": DAMAGE (TruMedia density • ", pt_sel, ")"),
          x = "Horizontal Break (in.)",
          y = "Induced Vertical Break (in.)"
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
        coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
    },
    height = function() {
      # Usually 1 facet after filtering; keep your scaling consistent anyway
      d <- dat_filt() %>%
        dplyr::filter(damage_event %in% TRUE)
      pt_sel <- get_pitchtype_selection(input, "damage")
      if (!is.null(pt_sel) && !is.na(pt_sel) && pt_sel != "All") {
        d <- d %>% dplyr::filter(as.character(PitchType) == pt_sel)
      }
      d <- d %>% dplyr::filter(!is.na(PitchType))
      n_types <- length(unique(as.character(d$PitchType)))
      rows <- ceiling(max(1, n_types) / 2)
      as.integer(450 * rows + 100)
    }
  )
  
  # =======================
  # 3) TOTAL WHIFF heatmap
  # =======================
  output$whiff_locations_total <- renderPlot(
    {
      d <- dat_filt() %>%
        dplyr::filter(whiff_event %in% TRUE)
      
      hb_col  <- pick_col(d, c("HorzBreak", "HorizontalBreak", "HB", "pfx_x", "HorzBrk", "HBreak"))
      ivb_col <- pick_col(d, c("InducedVertBreak", "IVB", "pfx_z", "IndVertBreak", "VertBreak", "VBreak"))
      
      validate(need(!is.null(hb_col) && !is.null(ivb_col),
                    "Missing movement columns (need HorzBreak/HB and InducedVertBreak/IVB or equivalents)."))
      
      d <- d %>%
        dplyr::mutate(
          HB  = as.numeric(.data[[hb_col]]),
          IVB = as.numeric(.data[[ivb_col]])
        ) %>%
        dplyr::filter(is.finite(HB), is.finite(IVB), !is.na(PitchType))
      
      validate(need(nrow(d) > 10, "Not enough whiffs to plot movement density."))
      
      xlim <- calc_limits(d$HB)
      ylim <- calc_limits(d$IVB)
      
      ggplot(d, aes(HB, IVB)) +
        stat_density_2d(
          aes(fill = after_stat(ndensity)),
          geom = "raster",
          contour = FALSE,
          n = 250,
          na.rm = TRUE
        ) +
        scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
        facet_wrap(~PitchType, ncol = 2) +
        geom_hline(yintercept = 0, linewidth = 0.7) +
        geom_vline(xintercept = 0, linewidth = 0.7) +
        labs(
          title = paste0(get_player_label(input), ": WHIFF (TruMedia movement density)"),
          x = "Horizontal Break (in.)",
          y = "Induced Vertical Break (in.)"
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
        coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
    },
    height = function() {
      d <- dat_filt() %>%
        dplyr::filter(whiff_event %in% TRUE, !is.na(PitchType))
      n_types <- length(unique(as.character(d$PitchType)))
      rows <- ceiling(max(1, n_types) / 2)
      as.integer(450 * rows + 100)
    }
  )
  
  # ==========================================
  # 4) WHIFF heatmap (PITCH TYPE FILTER page)
  # ==========================================
  output$whiff_locations_pitchtype <- renderPlot(
    {
      d <- dat_filt() %>%
        dplyr::filter(whiff_event %in% TRUE)
      
      pt_sel <- get_pitchtype_selection(input, "whiff")
      pt_sel_low <- tolower(pt_sel)
      
      if (pt_sel_low %in% c("all", "all pitches", "all pitch types")) pt_sel <- "All"
      if (!is.null(pt_sel) && !is.na(pt_sel) && pt_sel != "All") {
        d <- d %>% dplyr::filter(as.character(PitchType) == pt_sel)
      }
      
      hb_col  <- pick_col(d, c("HorzBreak", "HorizontalBreak", "HB", "pfx_x", "HorzBrk", "HBreak"))
      ivb_col <- pick_col(d, c("InducedVertBreak", "IVB", "pfx_z", "IndVertBreak", "VertBreak", "VBreak"))
      
      validate(need(!is.null(hb_col) && !is.null(ivb_col),
                    "Missing movement columns (need HorzBreak/HB and InducedVertBreak/IVB or equivalents)."))
      
      d <- d %>%
        dplyr::mutate(
          HB  = as.numeric(.data[[hb_col]]),
          IVB = as.numeric(.data[[ivb_col]])
        ) %>%
        dplyr::filter(is.finite(HB), is.finite(IVB), !is.na(PitchType))
      
      validate(need(nrow(d) > 10, "Not enough whiffs to plot movement density for that pitch type."))
      
      xlim <- calc_limits(d$HB)
      ylim <- calc_limits(d$IVB)
      
      ggplot(d, aes(HB, IVB)) +
        stat_density_2d(
          aes(fill = after_stat(ndensity)),
          geom = "raster",
          contour = FALSE,
          n = 250,
          na.rm = TRUE
        ) +
        scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
        facet_wrap(~PitchType, ncol = 2) +
        geom_hline(yintercept = 0, linewidth = 0.7) +
        geom_vline(xintercept = 0, linewidth = 0.7) +
        labs(
          title = paste0(get_player_label(input), ": WHIFF (TruMedia density • ", pt_sel, ")"),
          x = "Horizontal Break (in.)",
          y = "Induced Vertical Break (in.)"
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
        coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
    },
    height = function() {
      d <- dat_filt() %>%
        dplyr::filter(whiff_event %in% TRUE)
      pt_sel <- get_pitchtype_selection(input, "whiff")
      if (!is.null(pt_sel) && !is.na(pt_sel) && pt_sel != "All") {
        d <- d %>% dplyr::filter(as.character(PitchType) == pt_sel)
      }
      d <- d %>% dplyr::filter(!is.na(PitchType))
      n_types <- length(unique(as.character(d$PitchType)))
      rows <- ceiling(max(1, n_types) / 2)
      as.integer(450 * rows + 100)
    }
  )
  
  
  # ---- AAR helpers (constants) ----
  D1_BENCH <- list(whiff = 0.23, chase = 0.24, barrel = 0.17)  # D1 averages matching the PDF look
  ptype_group_map <- function(pt){
    ifelse(pt %in% c("Fastball","Sinker"), "HARD",
           ifelse(pt %in% c("Cutter","Slider","Curveball","Sweeper"), "BREAK",
                  ifelse(pt %in% c("Changeup","Splitter"), "SOFT", "SOFT")))
  }
  ptype_shape_vals <- c("HARD"=21, "BREAK"=24, "SOFT"=22)  
  decision_fill <- c("SWING"="#B4975A", "TAKE"="#501214")  

  aar_hitter_value <- reactive({
    embedded_choice <- input$aar_hitter
    if (!is.null(embedded_choice) && length(embedded_choice) && nzchar(embedded_choice[[1]])) {
      return(as.character(embedded_choice[[1]]))
    }
    as.character(input$Hitter %||% "")
  })

  aar_season_group_values <- reactive({
    embedded_choice <- input$hit_aar_season_groups
    if (!is.null(embedded_choice)) return(as.character(embedded_choice))
    as.character(input$hit_season_groups %||% character(0))
  })
  
  # AAR data for a single selected game
  aar_game_data_for <- function(sel_game, use_full_game = FALSE){
    aar_hitter <- aar_hitter_value()
    req(nzchar(aar_hitter))
    
    # AAR controls are independent from the Hitting workspace sidebar because
    # the report now lives in the unified Postgame Reports workspace.
    d <- df
    d$is_swing <- d$pitch_call %in% SWING_LIKE
    d$is_swing[is.na(d$is_swing)] <- FALSE
    
    if (!("is_swing" %in% names(d))) {
      d$is_swing <- d$pitch_call %in% SWING_LIKE
    }
    
    if (!("in_zone" %in% names(d))) {
      d$in_zone <- .calc_in_zone_logical(d)
    }
    
    d$GoodFlag <- dplyr::case_when(
      is.na(d$is_swing) | is.na(d$in_zone) ~ NA,
      d$in_zone & d$is_swing               ~ TRUE,
      !d$in_zone & !d$is_swing             ~ TRUE,
      TRUE                                 ~ FALSE
    )
    
    # Zone (logical, inches-based)
    if (!("in_zone" %in% names(d))) {
      sx_in <- if ("plate_x_in" %in% names(d)) to_num(d$plate_x_in) else zone_s_in(d$plate_x)
      hz_in <- if ("plate_z_in" %in% names(d)) to_num(d$plate_z_in) else zone_h_in(d$plate_z)
      
      d$in_zone <- is.finite(sx_in) & is.finite(hz_in) &
        in_zone_inches(sx_in, hz_in)
    }
    
    d$in_zone <- as.logical(d$in_zone)
    
    # -------------------- Swing Decision Logic (AUTHORITATIVE) --------------------
    d <- d %>%
      dplyr::mutate(
        is_swing = pitch_call %in% SWING_LIKE,
        is_take  = pitch_call %in% TAKE_LIKE,
        
        GoodFlag = dplyr::case_when(
          in_zone %in% TRUE  & is_swing           ~ TRUE,
          in_zone %in% FALSE & is_take            ~ TRUE,
          in_zone %in% TRUE  & is_take            ~ FALSE,
          in_zone %in% FALSE & is_swing           ~ FALSE,
          TRUE                                     ~ NA
        )
      )
    
    # Keep only this hitter’s **real game** rows (or BP if toggled)
    d <- d %>% dplyr::filter(.data$Batter == aar_hitter, !(is_bullpen %in% TRUE))
    
    # Build the hitter's game list (newest → oldest) and pick the current selection
    gf <- games_for_aar_hitter()
    gids <- gf
    if (is.data.frame(gf)) gids <- gf$gid
    gids <- as.character(gids)
    
    # If dropdown empty at first render, choose newest
    sel <- sel_game
    if (is.null(sel) || !nzchar(sel) || !(sel %in% gids)) {
      if (length(gids)) sel <- gids[[1]]
    }
    validate(need(length(gids) > 0 && !is.null(sel) && nzchar(sel), "No games for this hitter."))
    
    # Filter to that single game id (BP vs Games use different id columns)
    d <- d %>% dplyr::filter(.data$CustomGameID == sel)
    validate(need(nrow(d) > 0, "No rows in selected game."))
    
    # robust game-order sorting with fallbacks
    n_now <- nrow(d)
    num_col <- function(nm) {
      if (nm %in% names(d)) {
        out <- suppressWarnings(readr::parse_number(as.character(d[[nm]])))
        if (length(out) != n_now) out <- rep(NA_real_, n_now)
        out
      } else {
        rep(NA_real_, n_now)
      }
    }
    
    k_inn  <- num_col("Inning")
    half_col <- intersect(c("Top/Bottom", "TopBottom", "HalfInning"), names(d))[1]
    k_half <- if (!is.na(half_col)) {
      half_chr <- tolower(trimws(as.character(d[[half_col]])))
      dplyr::case_when(
        half_chr %in% c("top", "t") ~ 1,
        half_chr %in% c("bottom", "bot", "b") ~ 2,
        TRUE ~ NA_real_
      )
    } else {
      rep(NA_real_, n_now)
    }
    k_pa   <- num_col("PAofInning")
    k_pop  <- num_col("PitchofPA")
    k_pno  <- num_col("PitchNo")
    k_pnum <- num_col("PitchNum")
    k_row  <- if ("row_in_file" %in% names(d)) num_col("row_in_file") else seq_len(n_now)
    
    d <- d %>%
      dplyr::mutate(
        .k_inn = k_inn, .k_half = k_half, .k_pa = k_pa,
        .k_pop = k_pop, .k_pno = k_pno, .k_pnum = k_pnum, .k_row = k_row
      ) %>%
      dplyr::arrange(
        .k_inn, .k_half, .k_pa, .k_pop, .k_pno, .k_pnum, .k_row
      ) %>%
      dplyr::select(-.k_inn, -.k_half, -.k_pa, -.k_pop, -.k_pno, -.k_pnum, -.k_row)
    
    # --- HARD DE-DUPE: identical pitch rows sneaking in from file merges ---
    if (all(c("source_file","row_in_file") %in% names(d))) {
      d <- d %>% dplyr::distinct(source_file, row_in_file, .keep_all = TRUE)
    } else if (all(c("Date","Inning","PAofInning","PitchofPA") %in% names(d))) {
      d <- d %>% dplyr::distinct(Date, Inning, PAofInning, PitchofPA, .keep_all = TRUE)
    } else {
      d <- d %>% dplyr::distinct(dplyr::across(dplyr::everything()), .keep_all = TRUE)
    }
    
    # Pitch numbering 1..N for this game
    d$PitchNum <- seq_len(nrow(d))
    # Ensure PA_ID exists for tables that number plate appearances
    d$PA_ID <- make_pa_id(d)
    
    # Normalize pitch type (length-safe, works when columns are missing)
    n_now <- nrow(d)
    get_chr <- function(nm) if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, n_now)
    
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
    d$PitchType <- factor(pt_chr, levels = c(facet_levels, "Undefined", "Untagged"))
    
    # Decision / GoodBad / helpers
    swing_set <- c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
    d$Decision <- ifelse(d$pitch_call %in% swing_set, "SWING", "TAKE")
    if (!("plate_x_in" %in% names(d))) d$plate_x_in <- zone_s_in(d$plate_x)
    if (!("plate_z_in" %in% names(d))) d$plate_z_in <- zone_h_in(d$plate_z)
    
    d$plate_x_in <- to_num(d$plate_x_in)
    d$plate_z_in <- to_num(d$plate_z_in)
    
    d$in_zone <- ifelse(
      is.finite(d$plate_x_in) & is.finite(d$plate_z_in),
      in_zone_inches(d$plate_x_in, d$plate_z_in),
      NA
    )
    
    
    pr_chr <- nz_chr(d$play_result)
    is_ball <- grepl("(?i)\\bball\\b", nz_chr(d$pitch_call)) | grepl("(?i)\\bwalk|intentional|\\bIBB\\b", pr_chr)
    
    # Count string for the table
    balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
    strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
    d$CountStr <- sprintf("%s-%s",
                          if (!is.na(balls_col)) to_num(d[[balls_col]]) else NA_integer_,
                          if (!is.na(strikes_col)) to_num(d[[strikes_col]]) else NA_integer_)
    
    # Type group & BIP flag (for plots)
    ptype_group_map <- function(pt){
      ifelse(pt %in% c("Fastball","Sinker"), "HARD",
             ifelse(pt %in% c("Cutter","Slider","Curveball","Sweeper"), "BREAK",
                    ifelse(pt %in% c("Changeup","Splitter"), "SOFT", "SOFT")))
    }
    d$TypeGroup <- ptype_group_map(as.character(d$PitchType))
    d$bip_flag  <- is_bip_txst(d$pitch_call, d$play_result)
    
    d
  }
  
  # AAR data for the single selected game
  aar_data <- reactive({
    aar_game_data_for(input$AARGame)
  })
  
  output$aar_player_name <- renderText({
    d <- aar_data(); if (!nrow(d)) return("")
    unique(d$Batter)[1]
  })
  output$aar_game_date <- renderText({
    d <- aar_data(); if (!nrow(d)) return("")
    gd <- dplyr::coalesce(d$GameDate, parse_date_any(d$Date), extract_date_from_filename(d$source_file))
    gd <- gd[!is.na(gd)]
    if (length(gd)) format(gd[1], "%B %d, %Y") else ""
  })

  output$aar_header <- renderUI({
    d <- aar_data()
    if (is.null(d) || !nrow(d)) return(NULL)

    player_name <- name_display(unique(d$Batter)[1] %||% "Hitter")
    gd <- dplyr::coalesce(d$GameDate, parse_date_any(d$Date), extract_date_from_filename(d$source_file))
    gd <- gd[!is.na(gd)]
    date_label <- if (length(gd)) format(gd[1], "%B %d, %Y") else as.character(input$AARGame %||% "")

    home <- if ("HomeTeam" %in% names(d)) nz_chr(unique(d$HomeTeam)[1]) else ""
    away <- if ("AwayTeam" %in% names(d)) nz_chr(unique(d$AwayTeam)[1]) else ""
    team <- if ("BatterTeam" %in% names(d)) nz_chr(unique(d$BatterTeam)[1]) else TEAM_CODE
    opponent <- if (nzchar(home) && nzchar(away)) {
      if (toupper(team) == toupper(home)) paste("vs", away)
      else if (toupper(team) == toupper(away)) paste("at", home)
      else paste(away, "at", home)
    } else ""
    opponent <- hitting_team_display_text(opponent)

    asset_prefix <- get0("BASE_HITTING_ASSET_PREFIX", inherits = TRUE, ifnotfound = "static")
    div(
      class = "aar-report-header",
      tags$img(src = paste0(asset_prefix, "/txstlogo.jpeg"), alt = "Texas State logo"),
      div(
        div(class = "aar-report-title", paste(player_name, "After Action Report")),
        div(class = "aar-report-meta", paste(c(date_label, opponent)[nzchar(c(date_label, opponent))], collapse = "  •  "))
      ),
      tags$img(src = paste0(asset_prefix, "/Bobcatlogo.png"), alt = "Texas State Bobcat logo")
    )
  })
  
  output$aar_table <- DT::renderDT({
    d <- aar_data()
    validate(need(nrow(d) > 0, "No data for AAR."))
    
    tbl <- build_swing_decisions_tbl(d) %>%
      tibble::as_tibble() %>%
      dplyr::distinct(.keep_all = TRUE)
    
    # --- Good? (YES/NO) must align by pitch number ---
    if ("GoodFlag" %in% names(d)) {
      good_map <- d %>%
        dplyr::transmute(`#` = as.integer(PitchNum), GoodFlag = GoodFlag)
      
      if ("#" %in% names(tbl)) {
        tbl <- tbl %>%
          dplyr::mutate(`#` = suppressWarnings(as.integer(`#`))) %>%
          dplyr::left_join(good_map, by = "#") %>%
          dplyr::mutate(
            `Good?` = dplyr::case_when(
              GoodFlag %in% TRUE  ~ "YES",
              GoodFlag %in% FALSE ~ "NO",
              TRUE                ~ ""
            )
          ) %>%
          dplyr::select(-GoodFlag)
      }
    }
    
    # Fallback: if Decision is blank/NA, infer from Pitch Res.
    col_pres <- intersect(names(tbl), c("Pitch Res.","Pitch Res"))
    col_pres <- if (length(col_pres)) col_pres[1] else NA_character_
    
    if (!is.na(col_pres)) {
      blank_dec <- is.na(tbl$Decision) | tbl$Decision == ""
      if (any(blank_dec)) {
        tbl$Decision[blank_dec] <- ifelse(tbl[[col_pres]][blank_dec] %in% SWING_LIKE, "SWING",
                                          ifelse(tbl[[col_pres]][blank_dec] %in% TAKE_LIKE,  "TAKE",  tbl$Decision[blank_dec]))
      }
    }
    
    col_good <- intersect(names(tbl), c("Good?","Good/Bad"))
    col_good <- if (length(col_good) == 0) NULL else col_good[1]
    col_pres <- intersect(names(tbl), c("Pitch Res.","Pitch Res"))
    col_pres <- if (length(col_pres) == 0) NULL else col_pres[1]
    
    decision_col <- match("Decision", names(tbl)) - 1L
    row_callback <- DT::JS(sprintf(
      "function(row, data) {
         var decision = data[%d];
         if (decision === 'SWING') {
           $(row).addClass('aar-swing-row');
           $('td', row).css({'background-color':'#B4975A','color':'#501214','font-weight':'650'});
         } else if (decision === 'TAKE') {
           $(row).addClass('aar-take-row');
           $('td', row).css({'background-color':'#501214','color':'#FFFFFF','font-weight':'650'});
         }
       }",
      decision_col
    ))

    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      class = "compact hover stripe aar-table",    # ← add 'stripe'
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        autoWidth = FALSE,
        stripeClasses = c("dt-row-odd","dt-row-even"),
        rowCallback = row_callback
      )
    )
    
    dt <- dt %>%
      DT::formatStyle(
        "Decision",
        backgroundColor = DT::styleEqual(c("SWING","TAKE"), c("#B4975A", "#501214")),
        color           = DT::styleEqual(c("SWING","TAKE"), c("#501214", "#FFFFFF")),
        fontWeight      = "bold"
      )
    
    if (!is.null(col_good)) {
      dt <- dt %>%
        DT::formatStyle(
          col_good,
          backgroundColor = DT::styleEqual(
            c("YES","NO","GOOD","BAD"),
            c("rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)", "rgba(0,128,0,0.20)", "rgba(255,0,0,0.20)")
          ),
          color = "black",
          fontWeight = "bold"
        )
    }
    
    if (!is.null(col_pres)) {
      dt <- dt %>%
        DT::formatStyle(
          col_pres,
          backgroundColor = DT::styleEqual(
            c(SWING_LIKE, TAKE_LIKE),
            c(rep("#B4975A", length(SWING_LIKE)), rep("#501214", length(TAKE_LIKE)))
          ),
          color = DT::styleEqual(
            c(SWING_LIKE, TAKE_LIKE),
            c(rep("#501214", length(SWING_LIKE)), rep("#FFFFFF", length(TAKE_LIKE)))
          ),
          fontWeight = "bold"
        )
    }
    
    dt
  })
  
  output$download_aar_table <- downloadHandler(
    filename = function() paste0("AAR_SwingDecisions_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
    content = function(file) {
      d <- aar_data()
      if (is.null(d) || NROW(d) == 0) {
        writeLines("No data for AAR.", con = file)
        return(invisible())
      }
      
      tbl <- build_swing_decisions_tbl(d) %>%
        tibble::as_tibble() %>%
        dplyr::distinct(.keep_all = TRUE)
      
      col_pres <- intersect(names(tbl), c("Pitch Res.","Pitch Res"))
      col_pres <- if (length(col_pres) == 0) NA_character_ else col_pres[1]
      
      clamp_grob_height <- function(g, max_in){
        h_in <- sum(grid::convertUnit(g$heights, "in", valueOnly = TRUE))
        if (is.finite(h_in) && h_in > max_in) {
          scale <- max_in / h_in
          g$heights <- g$heights * scale
        }
        g
      }
      
      if (!is.na(col_pres)) {
        tbl$DecisionClass <- dplyr::case_when(
          tbl[[col_pres]] %in% SWING_LIKE ~ "swing-like",
          tbl[[col_pres]] %in% TAKE_LIKE  ~ "take-like",
          TRUE ~ NA_character_
        )
      }
      
      utils::write.csv(tbl, file, row.names = FALSE, na = "")
    }
  )
  
  # For the selected hitter, list only games they appeared in, newest -> oldest
  games_for_hitter <- reactive({
    h <- input$Hitter
    if (is.null(h) || !nzchar(h)) return(tibble::tibble(gid = character(0), gdate = as.Date(character(0))))
    
    if (isTRUE(input$hit_Bullpens)) {
      dd <- bullpens_df %>%
        dplyr::filter((.data$Batter == h) | (.data$Pitcher == h))
      gd <- dplyr::coalesce(parse_date_any(dd$Date), extract_date_from_filename(dd$source_file))
      tibble::tibble(gid = dd$CustomGameID_BP, gdate = gd, season = NA_character_) %>%
        dplyr::distinct(gid, gdate) %>%
        dplyr::arrange(dplyr::desc(gdate), dplyr::desc(gid))
    } else {
      dd <- df %>%
        dplyr::filter(.data$Batter == h, !(is_bullpen %in% TRUE))
      gd <- dplyr::coalesce(dd$GameDate, parse_date_any(dd$Date), extract_date_from_filename(dd$source_file))
      season_col <- if ("SeasonTag" %in% names(dd)) "SeasonTag" else if ("SeasonGroup" %in% names(dd)) "SeasonGroup" else NULL
      sg <- if (!is.null(season_col)) as.character(dd[[season_col]]) else NA_character_
      tibble::tibble(gid = dd$CustomGameID, gdate = gd, season = sg) %>%
        dplyr::distinct(gid, gdate, season) %>%
        dplyr::arrange(dplyr::desc(gdate), dplyr::desc(gid))
    }
  })

  games_for_aar_hitter <- reactive({
    h <- aar_hitter_value()
    if (!nzchar(h)) {
      return(tibble::tibble(
        gid = character(0), gdate = as.Date(character(0)), season = character(0)
      ))
    }
    dd <- df %>% dplyr::filter(.data$Batter == h, !(is_bullpen %in% TRUE))
    gd <- dplyr::coalesce(
      dd$GameDate,
      parse_date_any(dd$Date),
      extract_date_from_filename(dd$source_file)
    )
    season_col <- if ("SeasonTag" %in% names(dd)) {
      "SeasonTag"
    } else if ("SeasonGroup" %in% names(dd)) {
      "SeasonGroup"
    } else {
      NULL
    }
    sg <- if (!is.null(season_col)) as.character(dd[[season_col]]) else NA_character_
    tibble::tibble(gid = dd$CustomGameID, gdate = gd, season = sg) %>%
      dplyr::filter(!is.na(.data$gid), nzchar(.data$gid)) %>%
      dplyr::distinct(.data$gid, .data$gdate, .data$season) %>%
      dplyr::arrange(dplyr::desc(.data$gdate), dplyr::desc(.data$gid))
  })

  aar_game_choices <- reactive({
    gf <- games_for_aar_hitter()
    if (!nrow(gf)) return(character(0))
    sg <- aar_season_group_values()
    if (length(sg) && "season" %in% names(gf)) {
      filtered <- gf$gid[gf$season %in% sg]
      filtered <- filtered[!is.na(filtered) & nzchar(filtered)]
      if (length(filtered)) return(unique(as.character(filtered)))
    }
    unique(as.character(gf$gid))
  })
  
  all_game_choices_for_hitter <- reactive({
    gf <- games_for_hitter()
    if (nrow(gf) == 0) return(character(0))
    as.character(gf$gid)
  })

  season_selected_game_ids <- reactive({
    # seasons only apply to non-bullpen games
    if (isTRUE(input$hit_Bullpens)) return(all_game_choices_for_hitter())
    
    ch <- all_game_choices_for_hitter()
    sg <- input$hit_season_groups
    if (is.null(sg)) sg <- character(0)
    
    # If no seasons checked -> select NONE
    if (!length(sg)) return(character(0))
    
    gf <- games_for_hitter()
    if (!("season" %in% names(gf))) return(character(0))
    
    sel <- gf$gid[gf$season %in% sg]
    sel <- sel[!is.na(sel) & nzchar(trimws(sel))]
    sel <- intersect(ch, unique(sel))
    
    # If a season selection yields nothing, keep NONE
    if (!length(sel)) return(character(0))
    sel
  })

  # -------------------- Sidebar Game picker (authoritative) --------------------
  observeEvent(
    list(input$Hitter, input$hit_Bullpens),
    {
      ch <- all_game_choices_for_hitter()
      if (!length(ch)) {
        updatePickerInput(session, "Game", choices = character(0), selected = character(0))
        return()
      }
      sel <- season_selected_game_ids()
      
      updatePickerInput(
        session,
        "Game",
        choices  = ch,
        selected = sel
      )
    },
    ignoreInit = FALSE
  )
  
  # (B) When season checkboxes change:
  # DO NOT change choices, only change selected.
  observeEvent(input$hit_season_groups, {
    ch  <- all_game_choices_for_hitter()
    sel <- season_selected_game_ids()
    
    updatePickerInput(
      session,
      "Game",
      choices  = ch,
      selected = sel
    )
    
  }, ignoreInit = TRUE)
  
  # -------------------- AAR Game selector (single source of truth) --------------------
  observeEvent(
    list(aar_hitter_value(), aar_season_group_values()),
    {
      ch <- aar_game_choices()
      if (!length(ch)) {
        updateSelectInput(session, "AARGame", choices = character(0), selected = NULL)
        return()
      }
      updateSelectInput(
        session,
        "AARGame",
        choices  = ch,
        selected = ch[[1]]
      )
    },
    ignoreInit = FALSE
  )
  
  # -------------------- Performance Table --------------------
  output$perf_tbl <- DT::renderDT({
    d <- dat_filt()
    validate(need(nrow(d) > 0, "No rows in current filter."))
    
    split_mode <- if (is.null(input$hit_perf_split)) "hand" else input$hit_perf_split
    
    # --- 2-row header container ---
    make_container <- function(first_col_label = "PitchType"){
      htmltools::withTags(
        table(class = "display",
              thead(
                tr(class = "group-header",
                   th(colspan = 1, class = "group-label", "SPLIT"),
                   th(colspan = 12, class = "group-label", "RESULTS"),
                   th(colspan = 7, class = "group-label", "SWING"),
                   th(colspan = 10, class = "group-label", "BATTED BALL")
                ),
                tr(
                  th(first_col_label), th("PA"), th("wOBA"), th("wOBAcon"), th("xwOBA"), th("xwOBAcon"),
                  th("OBP"), th("SLG"), th("OPS"), th("hRV"),
                  th("K%"), th("BB%"), th("Barrel%"),
                  th("Swing%"), th("IZ-Swing%"), th("Whiff%"), th("IZ-Whiff%"),
                  th("Chase%"), th("Pre2K Chase%"), th("2K Chase%"),
                  th("MaxEV"), th("EV90"), th("EV>95%"), th("10-35*%"),
                  th("GB%"), th("LD%"), th("FB%"), th("PU%"), th("Foul Ball%"), th("Airpull%")
                )
              )
        )
      )
    }
    
    make_row <- function(dd, label){
      summarize_overall(dd) %>% dplyr::mutate(Name = label, .before = 1)
    }
    
    if (split_mode == "hand") {
      is_L <- !is.na(d$PitcherHand) & d$PitcherHand == "LHP"
      is_R <- !is.na(d$PitcherHand) & d$PitcherHand == "RHP"
      
      tbl_num <- dplyr::bind_rows(
        make_row(d, "Totals"),
        make_row(d[is_L, , drop=FALSE], "v LHP"),
        make_row(d[is_R, , drop=FALSE], "v RHP")
      )
      
      tbl_fmt <- tbl_num %>% format_perf_table()
      
      tbl_fmt <- apply_d1_shading(tbl_num, tbl_fmt) %>%
        dplyr::select(dplyr::all_of(c("Name", performance_table_metrics)))
      
      DT::datatable(
        tbl_fmt,
        container = make_container("Name"),
        rownames  = FALSE,
        escape    = FALSE,
        selection = "none",
        options   = list(
          dom='t', paging=FALSE, ordering=FALSE, stripe=TRUE,
          scrollX=TRUE, autoWidth=TRUE,
          rowCallback = DT::JS(
            "function(row, data) {",
            "  $(row).toggleClass('base-total-row', data[0] === 'Totals');",
            "}"
          ),
          columnDefs = list(list(className = "grp-start", targets = c(0,1,13,20)))
        ),
        class = "stripe"
      )
      
    } else {
      tbl_num <- summarize_by_pitchtype(d)
      
      tbl_fmt <- tbl_num %>% format_perf_table()
      tbl_fmt <- apply_d1_shading(tbl_num, tbl_fmt) %>%
        dplyr::select(dplyr::all_of(c("PitchType", performance_table_metrics)))
      
      DT::datatable(
        tbl_fmt,
        container = make_container("PitchType"),
        rownames  = FALSE,
        escape    = FALSE,
        selection = "none",
        options   = list(
          dom='t', paging=FALSE, ordering=FALSE, stripe=TRUE,
          scrollX=TRUE, autoWidth=TRUE,
          rowCallback = DT::JS(
            "function(row, data) {",
            "  $(row).toggleClass('base-total-row', data[0] === 'Totals');",
            "}"
          ),
          columnDefs = list(list(className = "grp-start", targets = c(0,1,13,20)))
        ),
        class = "stripe"
      )
    }
  })

  output$perf_time_series <- renderPlot({
    d <- dat_filt()
    validate(need(nrow(d) > 0, "No rows in current filter."))
    
    stats_sel <- input$hit_perf_ts_stats
    if (is.null(stats_sel) || !length(stats_sel)) {
      stats_sel <- c("wOBA", "Barrel%", "Whiff%", "Chase%")
    }
    stats_sel <- intersect(stats_sel, performance_timeseries_choices)
    validate(need(length(stats_sel) > 0, "Select at least one stat."))
    
    row_id <- if ("CustomGameID" %in% names(d)) nz_chr(d$CustomGameID) else rep("", nrow(d))
    row_id[!nzchar(row_id)] <- "Selected Data"
    row_date <- dplyr::coalesce(
      if ("GameDate" %in% names(d)) d$GameDate else as.Date(NA),
      if ("Date" %in% names(d)) parse_date_any(d$Date) else as.Date(NA),
      if ("source_file" %in% names(d)) extract_date_from_filename(d$source_file) else as.Date(NA)
    )
    date_key <- ifelse(!is.na(row_date), format(row_date, "%Y-%m-%d"), row_id)
    
    dates <- tibble::tibble(DateKey = date_key, GameDate = row_date, .row = seq_len(nrow(d))) %>%
      dplyr::group_by(.data$DateKey) %>%
      dplyr::summarise(
        GameDate = suppressWarnings(min(.data$GameDate, na.rm = TRUE)),
        FirstRow = min(.data$.row),
        .groups = "drop"
      ) %>%
      dplyr::mutate(GameDate = dplyr::if_else(is.finite(as.numeric(.data$GameDate)), .data$GameDate, as.Date(NA))) %>%
      dplyr::arrange(dplyr::coalesce(.data$GameDate, as.Date("1900-01-01")), .data$FirstRow) %>%
      dplyr::mutate(
        DateIndex = dplyr::row_number(),
        DateLabel = ifelse(!is.na(.data$GameDate), format(.data$GameDate, "%m/%d"), .data$DateKey)
      )
    
    rows <- lapply(seq_len(nrow(dates)), function(i){
      dd <- d[date_key %in% dates$DateKey[seq_len(i)], , drop = FALSE]
      dplyr::bind_cols(summarize_overall(dd), summarize_pitch_usage(dd)) %>%
        dplyr::mutate(
          DateKey = dates$DateKey[[i]],
          GameDate = dates$GameDate[[i]],
          DateIndex = dates$DateIndex[[i]],
          DateLabel = dates$DateLabel[[i]],
          .before = 1
        )
    })
    
    ts_wide <- dplyr::bind_rows(rows)
    validate(need(nrow(ts_wide) > 0, "No date-level rolling stats available."))
    
    long <- ts_wide %>%
      dplyr::select("DateIndex", "DateLabel", dplyr::all_of(stats_sel)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(stats_sel), names_to = "Metric", values_to = "Value") %>%
      dplyr::filter(is.finite(.data$Value))
    
    validate(need(nrow(long) > 0, "No finite values for selected stats."))
    
    avg_lookup <- c(D1_NON, D1_PCT)
    ref <- tibble::tibble(
      Metric = stats_sel,
      LeagueAvg = unname(avg_lookup[stats_sel])
    ) %>%
      dplyr::mutate(LeagueAvg = suppressWarnings(as.numeric(.data$LeagueAvg)))
    
    limits <- long %>%
      dplyr::left_join(ref, by = "Metric") %>%
      dplyr::group_by(.data$Metric) %>%
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
        DateIndex = min(long$DateIndex, na.rm = TRUE)
      )
    
    ref <- ref %>% dplyr::filter(is.finite(.data$LeagueAvg))
    
    ggplot(long, aes(DateIndex, Value, group = Metric)) +
      geom_blank(data = limits, aes(x = DateIndex, y = ymin), inherit.aes = FALSE) +
      geom_blank(data = limits, aes(x = DateIndex, y = ymax), inherit.aes = FALSE) +
      geom_hline(data = ref, aes(yintercept = LeagueAvg), linetype = "dashed", color = "grey45", linewidth = 0.7) +
      geom_text(
        data = ref,
        aes(x = Inf, y = LeagueAvg, label = "League Avg"),
        inherit.aes = FALSE,
        hjust = 1.05, vjust = -0.35, color = "grey35", size = 3
      ) +
      geom_line(color = "#501214", linewidth = 1.1, na.rm = TRUE) +
      geom_point(color = "#B4975A", fill = "#B4975A", size = 2.5, na.rm = TRUE) +
      facet_wrap(~Metric, scales = "free_y", ncol = 2) +
      scale_x_continuous(
        breaks = dates$DateIndex,
        labels = dates$DateLabel,
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
  })
  
  # ----- AAR strike zone -----
  output$aar_strike <- renderPlot({
    d <- aar_data() %>% dplyr::filter(is.finite(plate_x), is.finite(plate_z))
    validate(need(nrow(d) > 0, "No zoned pitches."))
    
    d$TypeGroup <- factor(d$TypeGroup, levels = c("HARD","BREAK","SOFT"))
    d$Decision  <- factor(d$Decision,  levels = c("SWING","TAKE"))
    
    # --- dashed thirds (catcher POV, zone = [-0.71,0.71] x [1.60,3.40]) ---
    x_breaks <- c(-0.71 + (1.42/3), -0.71 + (2*1.42/3))
    y_breaks <- c(1.60 + 0.60,       1.60 + 1.20)
    
    thirds_v <- data.frame(
      x    = x_breaks,
      xend = x_breaks,
      y    = 1.60,
      yend = 3.40
    )
    thirds_h <- data.frame(
      x    = -0.71,
      xend =  0.71,
      y    = y_breaks,
      yend = y_breaks
    )
    
    # ghost legend rows (off-canvas but valid)
    legend_type <- data.frame(
      x = -10, y = -10,
      TypeGroup = factor(c("HARD","BREAK","SOFT"), levels=c("HARD","BREAK","SOFT"))
    )
    legend_dec  <- data.frame(
      x = -10, y = -10,
      Decision = factor(c("SWING","TAKE"), levels=c("SWING","TAKE"))
    )
    
    p <- ggplot(d, aes(x = -plate_x, y = plate_z)) +
      # zone + home plate
      geom_rect(data = strike_zone,
                mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.1) +
      geom_segment(data = home_plate_segments,
                   mapping = aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
      # dashed thirds (all inside ggplot chain)
      geom_segment(data = thirds_v,
                   mapping = aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, linetype = "dashed", alpha = 0.35) +
      geom_segment(data = thirds_h,
                   mapping = aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, linetype = "dashed", alpha = 0.35) +
      # legend ghosts
      geom_point(data = legend_type, mapping = aes(x = x, y = y, shape = TypeGroup),
                 inherit.aes = FALSE, alpha = 0, show.legend = c(shape = TRUE, fill = FALSE)) +
      geom_point(data = legend_dec,  mapping = aes(x = x, y = y, fill = Decision),
                 inherit.aes = FALSE, shape = 21, alpha = 0, show.legend = c(shape = FALSE, fill = TRUE)) +
      # points + numbers
      geom_point(aes(shape = TypeGroup, fill = Decision),
                 size = 8, color = "black", stroke = 0.6, alpha = 0.95, show.legend = FALSE) +
      geom_text(aes(label = PitchNum), size = 4.8, fontface = 700, color = "white") +
      scale_shape_manual(
        name   = "Pitch type",
        values = c("HARD" = 21, "BREAK" = 24, "SOFT" = 22),
        breaks = c("HARD","BREAK","SOFT"),
        labels = c("HARD","BREAK","SOFT"),
        limits = c("HARD","BREAK","SOFT"),
        drop   = FALSE
      ) +
      scale_fill_manual(
        name   = "Decision",
        values = c("SWING" = "#B4975A", "TAKE" = "#501214"),
        breaks = c("SWING","TAKE"),
        labels = c("Swing","Take"),
        limits = c("SWING","TAKE"),
        drop   = FALSE
      ) +
      guides(
        shape = guide_legend(order = 1,
                             override.aes = list(fill = NA, size = 5, color = "black", stroke = 0.8, alpha = 1)),
        fill  = guide_legend(order = 2,
                             override.aes = list(shape = 21, size = 5, color = "black", stroke = 0.6, alpha = 1))
      ) +
      coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
      scale_x_reverse() +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid   = element_blank(),
        panel.border = element_blank(),
        legend.position     = "right",
        legend.box          = "vertical",
        legend.key.size     = grid::unit(10, "pt"),
        legend.text         = element_text(size = 9),
        legend.title        = element_text(size = 9),
        legend.margin       = margin(t = 0, r = 0, b = 0, l = 8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank()
      )
    
    print(p)
  })
  
  bip_type_from_LA <- function(la){
    la <- to_num(la)
    dplyr::case_when(
      is.na(la)              ~ NA_character_,
      la >= 50               ~ "Pop Up",
      la >= 20 & la < 50     ~ "Fly Ball",
      la >= 10 & la < 20     ~ "Line Drive",
      TRUE                   ~ "Ground Ball"
    )
  }
  
  # ----- AAR spray chart (ORIGINAL, authoritative) -----
  output$aar_spray <- renderPlot({
    d <- aar_data()
    validate(need(nrow(d) > 0, "No spray data"))
    
    grid::grid.newpage()
    grid::grid.draw(build_spray_grob(d, show_numbers = TRUE))
  })
  
  # ----- AAR contact point (overhead) -----
  output$aar_contact <- renderPlot({
    d <- aar_data()
    validate(need(nrow(d) > 0, "No contact data"))
    
    # Use same contact columns as app (Z for side, Y for depth)
    cx <- if ("ContactPositionZ" %in% names(d)) to_num(d$ContactPositionZ) else
      if ("ContactX" %in% names(d)) to_num(d$ContactX) else NA_real_
    cy <- if ("ContactPositionY" %in% names(d)) to_num(d$ContactPositionY) else
      if ("ContactY" %in% names(d)) to_num(d$ContactY) else NA_real_
    
    ok <- is.finite(cx) & is.finite(cy)
    validate(need(any(ok), "No contact points for this game."))
    
    x <- cx[ok]
    y <- cy[ok]
    
    # normalize units (feet -> inches) and orientation
    if (is.finite(suppressWarnings(max(abs(x), na.rm = TRUE))) && suppressWarnings(max(abs(x), na.rm = TRUE)) < 5) {
      x <- x * 12
    }
    if (is.finite(suppressWarnings(max(abs(y), na.rm = TRUE))) && suppressWarnings(max(abs(y), na.rm = TRUE)) < 10) {
      y <- y * 12
    }
    y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
    if (is.finite(y_med) && y_med < 0) y <- -y
    y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
    if (is.finite(y_med) && y_med < 5) y <- y + 17
    
    dd <- d[ok, , drop = FALSE]
    dd$ContactX_plot <- x
    dd$ContactY_plot <- y
    
    # pitch numbers + class
    if (!("PitchNum" %in% names(dd))) dd$PitchNum <- seq_len(nrow(dd))
    dd$TypeGroup <- factor(dd$TypeGroup, levels = c("HARD","BREAK","SOFT"))
    dd$Decision  <- factor(dd$Decision,  levels = c("SWING","TAKE"))
    
    plate <- data.frame(
      x = c(-8.5,  8.5,  8.5,   0, -8.5, -8.5),
      y = c(17.0, 17.0,  8.5,  0.0,  8.5, 17.0)
    )
    
    ggplot(dd, aes(ContactX_plot, ContactY_plot)) +
      geom_polygon(data = plate, aes(x = x, y = y),
                   inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.8) +
      geom_hline(yintercept = seq(10, 50, by = 10), linetype = "dashed",
                 color = "grey70", linewidth = 0.4) +
      geom_point(aes(shape = TypeGroup, fill = Decision),
                 size = 5.2, color = "black", stroke = 0.6, alpha = 0.95, show.legend = FALSE) +
      geom_text(aes(label = PitchNum), size = 3.2, fontface = 700, color = "white", show.legend = FALSE) +
      scale_shape_manual(values = c("HARD"=21, "BREAK"=24, "SOFT"=22), drop = FALSE) +
      scale_fill_manual(values = c("SWING"="#B4975A","TAKE"="#501214"), drop = FALSE) +
      coord_fixed(xlim = c(-20, 20), ylim = c(0, 50), expand = FALSE) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid   = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_blank(),
        axis.text    = element_blank(),
        axis.ticks   = element_blank(),
        plot.margin  = margin(0,0,0,0)
      )
  })
  
  
  # ----- AAR Swing Decisions table (Z-Swing%, Chase%, Pre2K/2K) -----
  output$aar_swing_tbl <- DT::renderDT({
    d <- aar_data()
    validate(need(nrow(d) > 0, "No rows for selected game."))
    
    balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
    strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
    
    d_sw <- swing_table(d, balls_col, strikes_col)
    
    z_seen     <- sum(d_sw$in_zone, na.rm = TRUE)
    z_swings   <- sum(d_sw$is_swing & d_sw$in_zone, na.rm = TRUE)
    chase_seen <- sum(!d_sw$in_zone, na.rm = TRUE)
    chase_sw   <- sum(d_sw$is_swing & !d_sw$in_zone, na.rm = TRUE)
    
    pre2k_den  <- sum(!d_sw$in_zone & d_sw$pre2k, na.rm = TRUE)
    pre2k_num  <- sum(d_sw$is_swing & !d_sw$in_zone & d_sw$pre2k, na.rm = TRUE)
    two_k_den  <- sum(!d_sw$in_zone & d_sw$two_k,  na.rm = TRUE)
    two_k_num  <- sum(d_sw$is_swing & !d_sw$in_zone & d_sw$two_k,  na.rm = TRUE)
    
    tbl <- tibble::tibble(
      `Z-Swing%`     = safe_ratio(z_swings, z_seen),
      `Chase%`       = safe_ratio(chase_sw, chase_seen),
      `Pre2K Chase%` = safe_ratio(pre2k_num, pre2k_den),
      `2K Chase%`    = safe_ratio(two_k_num, two_k_den)
    ) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), NA, sprintf("%.1f%%", 100*.))))
    
    DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
  })
  

  # ================== AAR PDF DOWNLOAD ==================
  output$hit_aar_pdf <- downloadHandler(
    filename = function() {
      d <- aar_data()
      plyr <- if (!is.null(d) && nrow(d) && "Batter" %in% names(d)) gsub("[^A-Za-z0-9]+","_", unique(d$Batter)[1]) else "Player"
      gdt  <- if (!is.null(d) && nrow(d) && "GameDate" %in% names(d)) as.character(sort(unique(d$GameDate))[1]) else as.character(Sys.Date())
      sprintf("AAR_%s_%s.pdf", plyr, gdt)
    },
    content = function(file){
      
      # ---------- local helpers (self-contained so this block works anywhere) ----------
      nz_chr <- function(x) ifelse(is.na(x), "", as.character(x))
      abbr_name <- function(x){
        x <- nz_chr(x)
        ifelse(grepl(",", x),
               {parts <- strsplit(x, ",\\s*"); vapply(parts, function(p){
                 if (length(p) >= 2) paste0(substr(p[2],1,1), ". ", p[1]) else x
               }, "", USE.NAMES = FALSE)},
               {parts <- strsplit(x, "\\s+"); vapply(parts, function(p){
                 if (length(p) >= 2) paste0(substr(p[1],1,1), ". ", p[length(p)]) else x
               }, "", USE.NAMES = FALSE)}
        )
      }
      # Shade one whole column in a tableGrob (vectorized fills + text colors)
      shade_table_col <- function(tg, col_idx, fills, text_col = NULL){
        lay <- tg$layout
        core_bg <- which(lay$name == "core-bg" & lay$l == col_idx)
        if (length(core_bg)) {
          first_row <- min(lay$t[core_bg])
          for (i in seq_along(core_bg)) {
            k <- core_bg[i]
            f <- fills[min(i, length(fills))]
            tg$grobs[[k]]$gp <- grid::gpar(fill = if (is.na(f) || f == "") "transparent" else f, col = NA)
          }
        }
        if (!is.null(text_col)) {
          core_fg <- which(lay$name == "core-fg" & lay$l == col_idx)
          for (i in seq_along(core_fg)) {
            k <- core_fg[i]
            c0 <- text_col[min(i, length(text_col))]
            gp <- tg$grobs[[k]]$gp %||% grid::gpar()
            gp$col <- c0
            tg$grobs[[k]]$gp <- gp
          }
        }
        tg
      }
      `%||%` <- function(a,b) if (is.null(a)) b else a
      
      # Fallback constants (if not defined elsewhere)
      if (!exists("SWING_LIKE", inherits = TRUE)) {
        SWING_LIKE <<- c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut",
                         "FoulBallFieldable","FoulBallNotFieldable","FoulTip")
      }
      if (!exists("TAKE_LIKE", inherits = TRUE)) {
        TAKE_LIKE  <<- c("BallCalled","StrikeCalled")
      }
      
      # ---------- get data ----------
      d <- aar_data()
      validate(need(nrow(d) > 0, "No data for AAR."))
      
      # ---------- PAGE & COLUMN LAYOUT (Legal 14x8.5, 60/40 split) ----------
      PAGE_W_IN  <- 14.0
      PAGE_H_IN  <-  8.5
      LEFT_FRAC  <-  0.60
      GUTTER_IN  <-  0.25
      LEFT_W_IN  <- PAGE_W_IN * LEFT_FRAC
      RIGHT_W_IN <- PAGE_W_IN - LEFT_W_IN - GUTTER_IN
      
      # open a PDF device; prefer cairo_pdf, but fall back to pdf() if XQuartz/cairo not available
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
      
      
      # ---------- HEADER (Player, date, opponent; logos if available) ----------
      player_name <- unique(d$Batter)[1] %||% ""
      gdates <- dplyr::coalesce(d$GameDate, parse_date_any(d$Date), extract_date_from_filename(d$source_file))
      gdate  <- gdates[!is.na(gdates)][1]
      
      get_opponent <- function(df){
        home <- if ("HomeTeam" %in% names(df)) nz_chr(unique(df$HomeTeam)[1]) else ""
        away <- if ("AwayTeam" %in% names(df)) nz_chr(unique(df$AwayTeam)[1]) else ""
        batt <- if ("BatterTeam" %in% names(df)) nz_chr(unique(df$BatterTeam)[1]) else ""
        if (!nzchar(batt) && exists("TEAM_CODE", inherits = TRUE)) batt <- nz_chr(get("TEAM_CODE", inherits = TRUE))
        
        if (nzchar(home) && nzchar(away)) {
          if (nzchar(batt)) {
            if (toupper(batt) == toupper(home)) return(paste("vs", away))
            if (toupper(batt) == toupper(away)) return(paste("at", home))
          }
          return(paste(away, "at", home))
        }
        ""
      }
      
      date_str <- if (!is.na(gdate)) format(parse_date_any(gdate), "%B %d, %Y") else ""
      opp_str  <- hitting_team_display_text(get_opponent(d))
      hdr_line <- paste(c(player_name, date_str, opp_str)[nzchar(c(player_name, date_str, opp_str))], collapse = "  •  ")
      
      hdr_bg  <- grid::rectGrob(gp = grid::gpar(fill = "#501214", col = NA))
      hdr_txt <- grid::textGrob(
        hdr_line,
        gp = grid::gpar(fontface = 2, cex = 1.15, col = "#B4975A")
      )
      
      
      
      load_header_logo <- function(path) {
        if (!is.character(path) || length(path) != 1L || !file.exists(path)) return(grid::nullGrob())
        ext <- tolower(tools::file_ext(path))
        img <- tryCatch(
          if (ext == "png") png::readPNG(path)
          else if (ext %in% c("jpg", "jpeg")) jpeg::readJPEG(path)
          else NULL,
          error = function(e) NULL
        )
        if (is.null(img)) grid::nullGrob() else grid::rasterGrob(img, interpolate = TRUE)
      }
      left_logo <- load_header_logo(TXST_LOGO_PATH)
      right_logo <- load_header_logo(BOBCAT_LOGO_PATH)
      
      # Nudge/align logos inside their cells (no vp= on gtable_add_grob)
      if (!inherits(left_logo, "null")) {
        left_logo <- grid::editGrob(left_logo, x = grid::unit(0, "npc"), y = grid::unit(0.5, "npc"),
                                    just = c("left", "center"))
      }
      if (!inherits(right_logo, "null")) {
        right_logo <- grid::editGrob(right_logo, x = grid::unit(1, "npc"), y = grid::unit(0.5, "npc"),
                                     just = c("right", "center"))
      }
      
      # Build a 3-column header: [logo] [title] [logo]
      header <- gtable::gtable(
        widths  = grid::unit.c(grid::unit(0.9, "in"), grid::unit(1, "null"), grid::unit(0.9, "in")),
        heights = grid::unit.c(grid::unit(0.65, "in"))
      )
      
      # Background spans all columns
      header <- gtable::gtable_add_grob(header, list(hdr_bg),  t = 1, l = 1, r = 3, name = "hdr_bg",  z = 1)
      # Title centered in middle column
      header <- gtable::gtable_add_grob(header, list(hdr_txt), t = 1, l = 2, r = 2, name = "hdr_txt", z = 2)
      
      # Logos on the sides (only if provided)
      if (!inherits(left_logo, "null")) {
        header <- gtable::gtable_add_grob(header, list(left_logo),  t = 1, l = 1, r = 1, name = "hdr_logo_l", z = 3)
      }
      if (!inherits(right_logo, "null")) {
        header <- gtable::gtable_add_grob(header, list(right_logo), t = 1, l = 3, r = 3, name = "hdr_logo_r", z = 3)
      }
      # ---------- end HEADER ----------
      
      
      
      # ---------- SWING DECISIONS TABLE (build, widths, zebra, shading) ----------
      tbl_pdf <- build_swing_decisions_tbl(d) %>%
        tibble::as_tibble() %>%
        dplyr::distinct(.keep_all = TRUE)
      # --- Good? (YES/NO) must align by pitch number (#) ---
      # Compute from in_zone + Decision to guarantee correct labeling.
      good_map <- d %>%
        dplyr::mutate(
          Decision = ifelse(nz_chr(pitch_call) %in% SWING_LIKE, "SWING", "TAKE"),
          GoodFlag = dplyr::case_when(
            in_zone %in% TRUE  & Decision == "SWING" ~ "YES",
            in_zone %in% FALSE & Decision == "TAKE"  ~ "YES",
            in_zone %in% TRUE  & Decision == "TAKE"  ~ "NO",
            in_zone %in% FALSE & Decision == "SWING" ~ "NO",
            TRUE                                     ~ ""
          )
        ) %>%
        dplyr::transmute(`#` = as.integer(PitchNum), GoodFlag = GoodFlag)
      
      if ("#" %in% names(tbl_pdf)) {
        tbl_pdf <- tbl_pdf %>%
          dplyr::mutate(`#` = suppressWarnings(as.integer(`#`))) %>%
          dplyr::left_join(good_map, by = "#") %>%
          dplyr::mutate(
            `Good?` = dplyr::coalesce(.data$GoodFlag, "")
          ) %>%
          dplyr::select(-GoodFlag)
      }
      
      
      # abbreviate pitcher names
      if ("Pitcher" %in% names(tbl_pdf)) tbl_pdf$Pitcher <- abbr_name(tbl_pdf$Pitcher)
      
      # normalize column names
      if ("Good/Bad" %in% names(tbl_pdf) && !("Good?" %in% names(tbl_pdf)))
        names(tbl_pdf)[names(tbl_pdf) == "Good/Bad"] <- "Good?"
      if ("AB Res." %in% names(tbl_pdf) && !("Res." %in% names(tbl_pdf)))
        names(tbl_pdf)[names(tbl_pdf) == "AB Res."]  <- "Res."
      if ("AB Res" %in% names(tbl_pdf) && !("Res." %in% names(tbl_pdf)))
        names(tbl_pdf)[names(tbl_pdf) == "AB Res"]   <- "Res."
      
      # fill Decision from Pitch Res. if blank (prevents last-row missing shades)
      col_pres <- intersect(names(tbl_pdf), c("Pitch Res.","Pitch Res"))
      col_pres <- if (length(col_pres)) col_pres[1] else NA_character_
      if (!is.na(col_pres) && "Decision" %in% names(tbl_pdf)) {
        blank_dec <- is.na(tbl_pdf$Decision) | tbl_pdf$Decision == ""
        if (any(blank_dec)) {
          tbl_pdf$Decision[blank_dec] <- ifelse(tbl_pdf[[col_pres]][blank_dec] %in% SWING_LIKE, "SWING",
                                                ifelse(tbl_pdf[[col_pres]][blank_dec] %in% TAKE_LIKE,  "TAKE",
                                                       tbl_pdf$Decision[blank_dec]))
        }
      }
      
      # force exact column order
      want_cols <- c("#","PA","Pitcher","Count","Pitch Res.","Res.","Type","Velo","EV","LA","Dist","Decision","Good?")
      have <- intersect(want_cols, names(tbl_pdf))
      tbl_pdf <- dplyr::select(tbl_pdf, dplyr::all_of(have))
      missing <- setdiff(want_cols, names(tbl_pdf))
      if (length(missing)) {
        for (nm in missing) tbl_pdf[[nm]] <- ""
        tbl_pdf <- dplyr::select(tbl_pdf, dplyr::all_of(want_cols))
      }
      
      # theme: maroon header + gold text
      ttheme_tbl <- gridExtra::ttheme_minimal(
        core = list(fg_params = list(cex = 0.90, lineheight = 1.45),
                    padding   = grid::unit(c(3, 4), "pt")),
        colhead = list(
          fg_params = list(cex = 0.86, fontface = 2, col = "#B4975A", hjust = 0, x = 0.04),
          bg_params = list(fill = "#501214", col = NA)
        )
      )
      g_swing_tbl <- gridExtra::tableGrob(
        tbl_pdf,
        rows = NULL,
        theme = ttheme_tbl
      )
      # ---- HARD WIDTH CLAMP (prevents table from spilling off page) ----
      tbl_max_in <- LEFT_W_IN - 0.05  # small safety margin
      tbl_w_in <- sum(grid::convertUnit(g_swing_tbl$widths, "in", valueOnly = TRUE))
      
      if (is.finite(tbl_w_in) && tbl_w_in > tbl_max_in) {
        scale <- tbl_max_in / tbl_w_in
        g_swing_tbl$widths <- g_swing_tbl$widths * scale
      }
      
      
      # ---- HARD LOCK: widths MUST fit inside left quadrant (no drift) ----
      # Keep a small inside margin so borders never touch the gutter.
      tbl_max_in <- LEFT_W_IN - 0.10   # <-- adjust margin if you want, but keep something >0
      
      # Weights for the 13 columns (must sum to 1.00)
      # (#, PA, Pitcher, Count, Pitch Res., Res., Type, Velo, EV, LA, Dist, Decision, Good?)
      w <- c(
        0.04,  # #
        0.05,  # PA
        0.16,  # Pitcher (slightly narrower to free space for Good?)
        0.07,  # Count
        0.18,  # Pitch Res.
        0.06,  # Res.
        0.08,  # Type
        0.06,  # Velo
        0.06,  # EV
        0.05,  # LA
        0.05,  # Dist
        0.08,  # Decision
        0.06   # Good?
      )
      
      # If for any reason the table doesn't have 13 cols (defensive), fall back evenly
      if (length(g_swing_tbl$widths) != length(w)) {
        w <- rep(1/length(g_swing_tbl$widths), length(g_swing_tbl$widths))
      }
      
      g_swing_tbl$widths <- grid::unit(tbl_max_in * w, "in")
      
      
      
      # Match the strike-zone decision colors across each full table row.
      g_swing_tbl <- {
        g <- g_swing_tbl
        lay <- g$layout
        core_bg <- which(lay$name == "core-bg")
        decision_rows <- toupper(trimws(nz_chr(tbl_pdf$Decision)))
        if (length(core_bg)) {
          first_row <- min(lay$t[core_bg])
          for (k in core_bg) {
            r <- lay$t[k] - first_row + 1
            decision <- decision_rows[min(r, length(decision_rows))]
            g$grobs[[k]]$gp <- grid::gpar(
              fill = if (decision == "SWING") "#B4975A" else if (decision == "TAKE") "#501214" else if (r %% 2) "#FFFFFF" else "#F7F7F7",
              col  = NA
            )
          }
        }
        core_fg <- which(lay$name == "core-fg")
        if (length(core_fg)) {
          first_row <- min(lay$t[core_fg])
          for (k in core_fg) {
            r <- lay$t[k] - first_row + 1
            decision <- decision_rows[min(r, length(decision_rows))]
            gp <- g$grobs[[k]]$gp %||% grid::gpar()
            gp$col <- if (decision == "SWING") "#501214" else if (decision == "TAKE") "#FFFFFF" else "#222222"
            gp$fontface <- if (decision %in% c("SWING", "TAKE")) 2 else 1
            g$grobs[[k]]$gp <- gp
          }
        }
        g
      }
      
      # full-length shading vectors (Decision / Good? / Pitch Res.)
      nr <- nrow(tbl_pdf)
      # Decision
      if ("Decision" %in% names(tbl_pdf)) {
        dec_bg <- ifelse(tbl_pdf$Decision == "SWING", "#B4975A",
                         ifelse(tbl_pdf$Decision == "TAKE",  "#501214", ""))
        dec_fg <- ifelse(tbl_pdf$Decision == "SWING", "#501214",
                         ifelse(tbl_pdf$Decision == "TAKE",  "#FFFFFF", "black"))
        g_swing_tbl <- shade_table_col(
          g_swing_tbl,
          which(colnames(tbl_pdf) == "Decision"),
          fills = dec_bg,
          text_col = dec_fg
        )
      }
      
      # Good? (normalize + shade)
      if (!("Good?" %in% names(tbl_pdf))) tbl_pdf$`Good?` <- ""
      good_vals <- toupper(trimws(nz_chr(tbl_pdf$`Good?`)))
      if (all(good_vals == "") && "GoodFlag" %in% names(d) && nrow(tbl_pdf) == nrow(d)) {
        good_vals <- ifelse(is.na(d$GoodFlag), "", ifelse(d$GoodFlag, "YES", "NO"))
      }
      tbl_pdf$`Good?` <- ifelse(good_vals %in% c("YES","Y","GOOD"), "YES",
                                ifelse(good_vals %in% c("NO","N","BAD"), "NO", ""))
      good_idx <- which(colnames(tbl_pdf) == "Good?")
      if (length(good_idx) == 1) {
        good_bg <- ifelse(
          tbl_pdf$`Good?` == "YES",
          grDevices::adjustcolor("forestgreen", alpha.f = 0.20),
          ifelse(
            tbl_pdf$`Good?` == "NO",
            grDevices::adjustcolor("red3", alpha.f = 0.20),
            "transparent"
          )
        )
        g_swing_tbl <- shade_table_col(
          g_swing_tbl,
          good_idx,
          fills = good_bg,
          text_col = rep("black", length(good_bg))
        )
      }
      
      # Pitch Res. (match Decision)
      col_pres <- intersect(colnames(tbl_pdf), c("Pitch Res.","Pitch Res"))
      if (length(col_pres)) {
        col_pres <- col_pres[1]
        pres_vals <- tbl_pdf[[col_pres]]
        pres_bg <- ifelse(pres_vals %in% SWING_LIKE, "#B4975A",
                          ifelse(pres_vals %in% TAKE_LIKE,  "#501214", ""))
        pres_fg <- ifelse(pres_vals %in% SWING_LIKE, "#501214",
                          ifelse(pres_vals %in% TAKE_LIKE,  "#FFFFFF", "black"))
        g_swing_tbl <- shade_table_col(
          g_swing_tbl,
          which(colnames(tbl_pdf) == col_pres),
          fills = pres_bg,
          text_col = pres_fg
        )
      }
      
      # ---------- STRIKE ZONE PLOT (bigger marks ~ +40%) ----------
      strike_zone <- data.frame(xmin = -0.71, xmax = 0.71, ymin = 1.60, ymax = 3.40)
      if (!exists("home_plate_segments", inherits = TRUE)) {
        home_plate_segments <- data.frame(
          x    = c(-0.85,-0.50, 0.50, 0.85, 0.50),
          y    = c( 0.05, 0.45, 0.45, 0.05,-0.35),
          xend = c(-0.50, 0.50, 0.85, 0.50,-0.50),
          yend = c( 0.45, 0.45, 0.05,-0.35, 0.45)
        )
      }
      d_strike <- d %>% dplyr::filter(is.finite(plate_x), is.finite(plate_z))
      d_strike$TypeGroup <- factor(d_strike$TypeGroup, levels = c("HARD","BREAK","SOFT"))
      d_strike$Decision  <- factor(d_strike$Decision,  levels = c("SWING","TAKE"))
      
      x_breaks <- c(-0.71 + (1.42/3), -0.71 + (2*1.42/3))
      y_breaks <- c(1.60 + 0.60,       1.60 + 1.20)
      thirds_v <- data.frame(x=x_breaks, xend=x_breaks, y=1.60, yend=3.40)
      thirds_h <- data.frame(x=-0.71, xend=0.71, y=y_breaks, yend=y_breaks)
      
      p_strike <- ggplot(d_strike, aes(x = -plate_x, y = plate_z)) +
        geom_rect(data = strike_zone, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.1) +
        geom_segment(data = home_plate_segments, aes(x=x, y=y, xend=xend, yend=yend),
                     inherit.aes = FALSE, colour = "black", linewidth = 0.9) +
        geom_segment(data = thirds_v, aes(x=x, y=y, xend=xend, yend=yend),
                     inherit.aes = FALSE, linetype="dashed", alpha=0.35) +
        geom_segment(data = thirds_h, aes(x=x, y=y, xend=xend, yend=yend),
                     inherit.aes = FALSE, linetype="dashed", alpha=0.35) +
        geom_point(aes(shape = TypeGroup, fill = Decision),
                   size = 7.8, color = "black", stroke = 0.7, alpha = 0.95, show.legend = TRUE) +
        geom_text(aes(label = PitchNum), size = 5.3, fontface = 700, color = "white") +
        scale_shape_manual(
          name   = "Pitch type",
          values = c("HARD"=21,"BREAK"=24,"SOFT"=22),
          breaks = c("HARD","BREAK","SOFT"),
          labels = c("HARD","BREAK","SOFT"),
          drop   = FALSE
        ) +
        scale_fill_manual(
          name   = "Decision",
          values = c("SWING"="#B4975A","TAKE"="#501214"),
          breaks = c("SWING","TAKE"),
          labels = c("Swing","Take"),
          drop   = FALSE
        ) +
        guides(
          shape = guide_legend(order=1,
                               override.aes = list(fill = NA, size = 5, color = "black", stroke = 0.8, alpha = 1)),
          fill  = guide_legend(order=2,
                               override.aes = list(shape = 21, size = 5, color = "black", stroke = 0.6, alpha = 1))
        ) +
        coord_fixed(xlim = c(-2,2), ylim = c(-1,4.75), expand = FALSE) +
        scale_x_reverse() +
        labs(x=NULL,y=NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid=element_blank(), panel.border=element_blank(),
              legend.position="right", legend.key.size = grid::unit(10, "pt"),
              legend.text = element_text(size = 9), legend.title = element_text(size = 9),
              legend.margin = margin(l = 6),
              axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
      
      g_strike <- ggplotGrob(p_strike)
      # Nudge strike zone up ~10% within its cell
      g_strike <- grid::grobTree(
        g_strike,
        vp = grid::viewport(y = grid::unit(0.6, "npc"), height = grid::unit(1, "npc"))
      )
      # ---- Spray grob for PDF (NO grid.newpage side-effects) ----
      spray_grob_pdf <- function(df_game){
        
        # Use your existing field geometry if available
        if (!exists("make_field_layers", inherits = TRUE)) {
          return(grid::textGrob("Field layers not available", gp = grid::gpar(fontface = 2)))
        }
        return(build_spray_grob(df_game, show_numbers = TRUE))
        fld <- make_field_layers()
        
        # Build plotting coordinates (TM polar preferred, else hc_x/hc_y)
        use_tm <- is.finite(df_game$distance_ft) & is.finite(df_game$bearing)
        dd <- df_game %>%
          dplyr::mutate(
            plot_x = dplyr::if_else(use_tm, distance_ft * sin(bearing * pi/180), hc_x),
            plot_y = dplyr::if_else(use_tm, distance_ft * cos(bearing * pi/180), hc_y)
          ) %>%
          dplyr::filter(is.finite(plot_x), is.finite(plot_y))
        
        if (!("PitchNum" %in% names(dd))) dd$PitchNum <- seq_len(nrow(dd))
        
        # Contact type from LA (your same buckets)
        la_val <- to_num(dd$la)
        dd <- dd %>%
          dplyr::mutate(
            BBType = spray_contact_type(la_val),
            ResultBucket = spray_result_bucket(play_result)
          ) %>%
          dplyr::filter(!is.na(BBType))
        
        dd$BBType <- factor(dd$BBType, levels=contact_type_levels)
        dd$ResultBucket <- factor(dd$ResultBucket, levels=result_type_levels)
        
        # Ground balls: dashed line to 120 ft along bearing; icon at 120 ft
        if ("bearing" %in% names(dd)) {
          dd$gb_x <- ifelse(dd$BBType == "Ground Ball" & is.finite(dd$bearing),
                            120 * sin(dd$bearing * pi/180), NA_real_)
          dd$gb_y <- ifelse(dd$BBType == "Ground Ball" & is.finite(dd$bearing),
                            120 * cos(dd$bearing * pi/180), NA_real_)
          dd$plot_x <- ifelse(!is.na(dd$gb_x), dd$gb_x, dd$plot_x)
          dd$plot_y <- ifelse(!is.na(dd$gb_y), dd$gb_y, dd$plot_y)
        }
        
        # Legend dummies (ensure legends show in PDF)
        legend_ct <- data.frame(BBType = contact_type_levels)
        legend_rs <- data.frame(ResultBucket = result_type_levels)
        
        p <- ggplot() +
          geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
          geom_path(   data=fld$wall,        aes(x=x,y=y), color="#501214", linewidth=1.1) +
          geom_segment(data=fld$foul_lines,  aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
          geom_path(   data=fld$rings,       aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
          geom_text(   data=fld$ring_labels, aes(x=x,y=y,label=label), color="grey35", size=3.2, vjust=0.5) +
          geom_path(   data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
          geom_point(
            data = legend_ct,
            mapping = aes(x = -999, y = -999, shape = BBType),
            size = 5.2, alpha = 0,
            inherit.aes = FALSE,
            show.legend = c(shape = TRUE, fill = FALSE)
          ) +
          geom_point(
            data = legend_rs,
            mapping = aes(x = -999, y = -999, fill = ResultBucket),
            shape = 21, size = 5.2, alpha = 0,
            inherit.aes = FALSE,
            show.legend = c(shape = FALSE, fill = TRUE)
          ) +
          geom_segment(
            data = dd %>% dplyr::filter(BBType == "Ground Ball", is.finite(gb_x), is.finite(gb_y)),
            aes(x = 0, y = 0, xend = gb_x, yend = gb_y),
            linetype = "dashed", color = "black", linewidth = 0.6, alpha = 0.7,
            inherit.aes = FALSE
          ) +
          geom_point(
            data = dd,
            aes(x=plot_x, y=plot_y, shape=BBType, fill=ResultBucket),
            size=5.2, color="black", stroke=0.6, alpha=0.95,
            show.legend = FALSE
          ) +
          geom_text(
            data = dd,
            aes(x = plot_x, y = plot_y, label = PitchNum),
            color = "white", size = 3.6, fontface = "bold",
            show.legend = FALSE
          ) +
          scale_shape_manual(
            name   = "Contact type",
            values = contact_shape_values,
            breaks = contact_type_levels,
            drop = FALSE
          ) +
          scale_fill_manual(
            name="Result",
            values=result_fill_values,
            breaks=result_type_levels,
            labels=result_type_labels,
            drop=FALSE
          ) +
          guides(
            shape = guide_legend(
              order=1,
              override.aes=list(
                shape = unname(contact_legend_shape_values),
                fill  = NA, size=5.2, color="black", alpha=1
              )
            ),
            fill  = guide_legend(order=2, override.aes=list(shape=21, size=5.2, color="black", alpha=1))
          ) +
          coord_fixed(xlim=spray_xlim, ylim=spray_ylim, expand=FALSE) +
          theme_minimal(base_size=11) +
          theme(
            panel.grid   = element_blank(),
            panel.border = element_rect(color="black", fill=NA),
            axis.title   = element_blank(),
            axis.text    = element_blank(),
            axis.ticks   = element_blank(),
            plot.margin  = margin(0,0,0,0)
          )
        
        ggplotGrob(p)
      }
      
      scale_grob <- function(g, sx = 1, sy = 1){
        if (is.null(g)) return(g)
        if (!is.null(g$widths)) g$widths <- g$widths * sx
        if (!is.null(g$heights)) g$heights <- g$heights * sy
        g
      }
      
      g_spray <- scale_grob(spray_grob_pdf(d), sx = 0.75, sy = 0.75)
      
      # ---- Contact Point grob for PDF (middle bottom third) ----
      contact_point_grob_pdf <- function(df_game){
        # pull contact coords (use same columns as app)
        cx <- if ("ContactPositionZ" %in% names(df_game)) to_num(df_game$ContactPositionZ) else
          if ("ContactX" %in% names(df_game)) to_num(df_game$ContactX) else NA_real_
        cy <- if ("ContactPositionY" %in% names(df_game)) to_num(df_game$ContactPositionY) else
          if ("ContactY" %in% names(df_game)) to_num(df_game$ContactY) else NA_real_
        
        ok <- is.finite(cx) & is.finite(cy)
        if (!any(ok)) {
          return(grid::textGrob("No contact points", gp = grid::gpar(fontface = 2)))
        }
        
        x <- cx[ok]
        y <- cy[ok]
        
        # normalize units (feet -> inches) and orientation
        if (is.finite(suppressWarnings(max(abs(x), na.rm = TRUE))) && suppressWarnings(max(abs(x), na.rm = TRUE)) < 5) {
          x <- x * 12
        }
        if (is.finite(suppressWarnings(max(abs(y), na.rm = TRUE))) && suppressWarnings(max(abs(y), na.rm = TRUE)) < 10) {
          y <- y * 12
        }
        y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
        if (is.finite(y_med) && y_med < 0) y <- -y
        y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
        if (is.finite(y_med) && y_med < 5) y <- y + 17
        
        dd <- df_game[ok, , drop = FALSE]
        dd$ContactX_plot <- x
        dd$ContactY_plot <- y
        
        # ensure pitch numbers
        if (!("PitchNum" %in% names(dd))) dd$PitchNum <- seq_len(nrow(dd))
        
        # pitch class shapes like strike zone
        dd$TypeGroup <- factor(dd$TypeGroup, levels = c("HARD","BREAK","SOFT"))
        dd$Decision  <- factor(dd$Decision,  levels = c("SWING","TAKE"))
        
        # home plate outline (overhead)
        plate <- data.frame(
          x = c(-8.5,  8.5,  8.5,   0, -8.5, -8.5),
          y = c(17.0, 17.0,  8.5,  0.0,  8.5, 17.0)
        )
        
        p <- ggplot(dd, aes(ContactX_plot, ContactY_plot)) +
          geom_polygon(data = plate, aes(x = x, y = y),
                       inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.8) +
          geom_hline(yintercept = seq(10, 50, by = 10), linetype = "dashed",
                     color = "grey70", linewidth = 0.4) +
          geom_point(aes(shape = TypeGroup, fill = Decision),
                     size = 5.2, color = "black", stroke = 0.6, alpha = 0.95, show.legend = FALSE) +
          geom_text(aes(label = PitchNum), size = 3.2, fontface = 700, color = "white", show.legend = FALSE) +
          scale_shape_manual(
            values = c("HARD"=21, "BREAK"=24, "SOFT"=22),
            breaks = c("HARD","BREAK","SOFT"),
            drop = FALSE
          ) +
          scale_fill_manual(
            values = c("SWING"="#B4975A","TAKE"="#501214"),
            breaks = c("SWING","TAKE"),
            drop = FALSE
          ) +
          coord_fixed(xlim = c(-20, 20), ylim = c(0, 50), expand = FALSE) +
          theme_minimal(base_size = 11) +
          theme(
            panel.grid   = element_blank(),
            panel.border = element_blank(),
            axis.title   = element_blank(),
            axis.text    = element_blank(),
            axis.ticks   = element_blank(),
            plot.margin  = margin(0,0,0,0)
          )
        
        ggplotGrob(p)
      }
      
      g_contact <- scale_grob(contact_point_grob_pdf(d), sx = 0.75, sy = 0.75)
  
      
      # ---- Robust count-column detection for PDF scope ----
      detect_count_cols <- function(df) {
        # try common names first
        bc <- intersect(names(df), c("Balls","balls","BALLS","balls_ct","BallsCount","BallCt"))[1]
        sc <- intersect(names(df), c("Strikes","strikes","STRIKES","strikes_ct","StrikesCount","StrikeCt"))[1]
        
        # if missing, parse from "Count" like "1-2"
        if (!length(bc) || !length(sc) || is.na(bc) || is.na(sc)) {
          if ("Count" %in% names(df)) {
            cnt <- as.character(df$Count)
            # parse "B-S" or "B:S"
            bs <- suppressWarnings(as.integer(sub("^\\s*(\\d+)[-:](\\d+)\\s*$", "\\1", cnt)))
            ss <- suppressWarnings(as.integer(sub("^\\s*(\\d+)[-:](\\d+)\\s*$", "\\2", cnt)))
            df$Balls__tmp   <- bs
            df$Strikes__tmp <- ss
            bc <- "Balls__tmp"
            sc <- "Strikes__tmp"
          } else {
            bc <- NA_character_
            sc <- NA_character_
          }
        }
        list(bc = bc, sc = sc, df = df)
      }
      
      # Prime count columns for the current game data 'd' (defined above in your handler)
      cs <- detect_count_cols(d); d <- cs$df
      balls_col   <- cs$bc
      strikes_col <- cs$sc
      
      # ---- Safe wrappers that never rely on missing globals ----
      get_sw <- function(df){
        cs <- detect_count_cols(df); df <- cs$df; bc <- cs$bc; sc <- cs$sc
        if (exists("swing_table", inherits = TRUE) && !is.na(bc) && !is.na(sc)) {
          swing_table(df, bc, sc)
        } else {
          # Fallback heuristics if swing_table not available or counts unavailable
          df %>%
            dplyr::mutate(
              is_swing = grepl("(?i)swing|foul|in\\s*play", nz_chr(PitchCall)),
              in_zone  = !grepl("(?i)ball", nz_chr(PitchCall))  # coarse fallback
            )
        }
      }
      
      get_pa <- function(df){
        cs <- detect_count_cols(df); df <- cs$df; bc <- cs$bc; sc <- cs$sc
        if (exists("pa_last_table", inherits = TRUE) && !is.na(bc) && !is.na(sc)) {
          pa_last_table(df, bc, sc)
        } else {
          # Fallback shape if pa_last_table not available
          df %>% dplyr::mutate(
            pc    = nz_chr(PitchCall),
            pr    = nz_chr(PlayResult),
            ev_pa = EV,
            la_pa = LA
          )
        }
      }
      
      
      is_barrel_tx <- function(pc, pr, ev, la){
        if (exists("is_barrel_txst", inherits=TRUE))
          is_barrel_txst(pc, pr, ev, la)
        else ifelse(is.finite(ev) & is.finite(la) & ev >= 95 & la >= 10 & la <= 30, TRUE, FALSE)
      }
      safe_ratio <- function(num, den) ifelse(den > 0, num/den, NA_real_)
      
      
      
      sw_g <- get_sw(d)
      pa_g <- get_pa(d)
      whiff_cnt  <- sum(sw_g$is_whiff, na.rm=TRUE)
      chase_cnt  <- sum(sw_g$is_swing & !sw_g$in_zone, na.rm=TRUE)
      barrel_cnt <- sum(is_barrel_tx(pa_g$pc, pa_g$pr, pa_g$ev_pa, pa_g$la_pa), na.rm=TRUE)
      
      # ---------- MASTER GTABLE(S) ----------
      col_w_in <- (PAGE_W_IN - (2 * GUTTER_IN)) / 3
      cols <- grid::unit.c(
        grid::unit(col_w_in, "inches"),
        grid::unit(GUTTER_IN, "inches"),
        grid::unit(col_w_in, "inches"),
        grid::unit(GUTTER_IN, "inches"),
        grid::unit(col_w_in, "inches")
      )
      
      # ---- HSH table for PDF (bottom right) ----
      hsh_tbl_pdf <- {
        d_game <- d
        sg <- season_group_for_game(d_game)
        info <- season_info_from_group(sg)
        
        d_season <- dat_filt()
        if (!is.na(info$group) && "SeasonGroup" %in% names(d_season)) {
          d_season <- d_season %>%
            dplyr::filter(toupper(trimws(.data$SeasonGroup)) == info$group)
        }
        
        g <- hsh_rates(d_game)
        s <- hsh_rates(d_season)
        s_perf <- summarize_overall(d_season)
        d1 <- c(whiff = 0.23, chase = 0.24, barrel = 0.17)
        
        t0 <- tibble::tibble(
          Metric = c("Whiffs", "Chases", "Barrels"),
          `This Game` = c(g$whiff_n, g$chase_n, g$barrel_n),
          Season = c(s_perf$`Whiff%`, s_perf$`Chase%`, s_perf$`Barrel%`),
          `D1 Avg` = c(d1[["whiff"]], d1[["chase"]], d1[["barrel"]])
        )
        
        fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.1f%%", 100*x), "—")
        fmt_cnt <- function(x) ifelse(is.finite(x), as.character(as.integer(round(x))), "—")
        t0$`This Game` <- fmt_cnt(t0$`This Game`)
        t0$Season <- fmt_pct(t0$Season)
        t0$`D1 Avg` <- fmt_pct(t0$`D1 Avg`)
        
        # apply season label to header
        season_label <- info$label %||% "Season"
        names(t0)[names(t0) == "Season"] <- season_label
        
        tg <- gridExtra::tableGrob(
          t0,
          rows = NULL,
          theme = gridExtra::ttheme_minimal(
            core = list(fg_params = list(cex = 1.00, lineheight = 1.35),
                        padding   = grid::unit(c(4, 5), "pt")),
            colhead = list(
              fg_params = list(cex = 0.86, fontface = 2, col = "#B4975A", hjust = 0, x = 0.04),
              bg_params = list(fill = "#501214", col = NA)
            )
          )
        )
        
        # title bar (match app) - attach directly to table so it never floats
        title_g <- grid::textGrob("Hit Strikes Hard",
                                  x = 0.5, y = 0.5,
                                  gp = grid::gpar(fontface = 2, col = "#B4975A", cex = 0.9))
        title_bg <- grid::rectGrob(gp = grid::gpar(fill = "#501214", col = NA))
        
        # Season shading: continuous green/red severity vs the D1 mean.
        season_col <- which(names(t0) == season_label)
        if (length(season_col) == 1) {
          raw_season <- c(s_perf$`Whiff%`, s_perf$`Chase%`, s_perf$`Barrel%`)
          raw_d1 <- c(d1[["whiff"]], d1[["chase"]], d1[["barrel"]])
          metrics <- c("Whiffs","Chases","Barrels")
          shade <- vapply(seq_along(metrics), function(i){
            m <- metrics[i]
            v <- raw_season[i]
            b <- raw_d1[i]
            lower_better <- m %in% c("Whiffs","Chases")
            fill <- d1_shade_fill(v, b, lower_better)
            if (nzchar(fill)) fill else "transparent"
          }, character(1))
          tg <- shade_table_col(tg, season_col, fills = shade, text_col = rep("black", length(shade)))
        }
        
        # attach title row to table gtable
        hsh_stack <- gtable::gtable_add_rows(tg, heights = grid::unit(0.20, "in"), pos = 0)
        hsh_stack <- gtable::gtable_add_grob(
          hsh_stack, title_bg,
          t = 1, l = 1, r = length(hsh_stack$widths),
          name = "hsh_title_bg"
        )
        hsh_stack <- gtable::gtable_add_grob(
          hsh_stack, title_g,
          t = 1, l = 1, r = length(hsh_stack$widths),
          name = "hsh_title_txt"
        )
        
        # scale up to better fill the right-bottom cell
        scale_grob <- function(g, sx = 1, sy = 1){
          if (is.null(g)) return(g)
          if (!is.null(g$widths)) g$widths <- g$widths * sx
          if (!is.null(g$heights)) g$heights <- g$heights * sy
          g
        }
        # Keep vertical scale at 1 so the title bar stays attached to the table
        scale_grob(hsh_stack, sx = 1.12, sy = 1.00)
      }
      
      # --- Rule: if 26+ pitches, split into two pages (no layout overflow) ---
      multi_page <- nrow(d) >= 26
      
      if (isTRUE(multi_page)) {
        # Page 1: Swing Decisions table + Strike Zone plot
        rows_p1 <- grid::unit.c(
          grid::unit(0.70, "inches"),  # header
          grid::unit(0.10, "inches"),  # gap
          grid::unit(1.00, "null")     # content fills page
        )
        gt1 <- gtable::gtable(widths = cols, heights = rows_p1)
        gt1 <- gtable::gtable_add_grob(gt1, header,      t = 1, l = 1, r = 5, name = "hdr", clip = "off")
        gt1 <- gtable::gtable_add_grob(gt1, g_swing_tbl, t = 3, l = 1, r = 3, name = "tbl_top_left", clip = "on")
        gt1 <- gtable::gtable_add_grob(gt1, g_strike,    t = 3, l = 5, r = 5, name = "plot_top_right", clip = "on")
        grid::grid.draw(gt1)
        
        # Page 2: Spray chart + Contact point + HSH table
        grid::grid.newpage()
        rows_p2 <- grid::unit.c(
          grid::unit(0.70, "inches"),  # header
          grid::unit(0.10, "inches"),  # gap
          grid::unit(1.00, "null")     # content fills page
        )
        gt2 <- gtable::gtable(widths = cols, heights = rows_p2)
        gt2 <- gtable::gtable_add_grob(gt2, header,    t = 1, l = 1, r = 5, name = "hdr", clip = "off")
        gt2 <- gtable::gtable_add_grob(gt2, g_spray,   t = 3, l = 1, r = 1, name = "plot_bot_left",  clip = "on")
        gt2 <- gtable::gtable_add_grob(gt2, g_contact, t = 3, l = 3, r = 3, name = "plot_bot_mid",   clip = "on")
        gt2 <- gtable::gtable_add_grob(gt2, hsh_tbl_pdf, t = 3, l = 5, r = 5, name = "plot_bot_right", clip = "on")
        grid::grid.draw(gt2)
      } else {
        # Single page (original layout)
        rows <- grid::unit.c(
          grid::unit(0.70, "inches"),  # header
          grid::unit(0.10, "inches"),  # gap
          grid::unit(0.60, "null"),    # top row (60%)
          grid::unit(0.10, "inches"),  # gap
          grid::unit(0.40, "null")     # bottom row (40%)
        )
        gt <- gtable::gtable(widths = cols, heights = rows)
        
        # add header (spans all columns)
        gt <- gtable::gtable_add_grob(gt, header, t = 1, l = 1, r = 5, name = "hdr", clip = "off")
        
        # add content
        gt <- gtable::gtable_add_grob(gt, g_swing_tbl, t = 3, l = 1, r = 3, name = "tbl_top_left", clip = "on")
        gt <- gtable::gtable_add_grob(gt, g_strike,    t = 3, l = 5, r = 5, name = "plot_top_right", clip = "on")
        gt <- gtable::gtable_add_grob(gt, g_spray,     t = 5, l = 1, r = 1, name = "plot_bot_left",  clip = "on")
        gt <- gtable::gtable_add_grob(gt, g_contact,   t = 5, l = 3, r = 3, name = "plot_bot_mid",   clip = "on")
        gt <- gtable::gtable_add_grob(gt, hsh_tbl_pdf, t = 5, l = 5, r = 5, name = "plot_bot_right", clip = "on")
        
        grid::grid.draw(gt)
      }
      grDevices::dev.off()
    }
  )
  
  # -------------------- Damage Heat Map --------------------
  plot_damage_heat <- function(d){
    validate(need(nrow(d) > 0, "No rows in current filter."))
    
    # feet for plotting
    x <- to_num(d$plate_x); if (suppressWarnings(max(abs(x), na.rm=TRUE)) > 5) x <- x/12
    z <- to_num(d$plate_z); if (suppressWarnings(max(z, na.rm=TRUE)) > 10) z <- z/12
    
    pr <- tolower(as.character(d$play_result))
    wts <- if (exists("woba_weights")) woba_weights else list(X1B=.90, X2B=1.25, X3B=1.56, HR=1.95)
    
    dmg <- dplyr::case_when(
      grepl("home\\s*run|\\bhr\\b", pr) ~ wts$HR,
      grepl("\\b3b\\b|triple", pr)     ~ wts$X3B,
      grepl("\\b2b\\b|double", pr)     ~ wts$X2B,
      grepl("\\b1b\\b|single", pr)     ~ wts$X1B,
      TRUE ~ 0
    )
    
    dd <- d %>%
      dplyr::mutate(x_ft = x, z_ft = z, dmg = dmg) %>%
      dplyr::filter(is.finite(.data$x_ft), is.finite(.data$z_ft))
    
    ggplot(dd, aes(x_ft, z_ft)) +
      stat_summary_2d(aes(z = dmg, fill = after_stat(value)), fun = mean, bins = 35) +
      coord_fixed(xlim = c(-2,2), ylim = c(1,4), expand = FALSE) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank())
  }
  
  # -------------------- AAR Strike Zone --------------------
  output$strike_zone <- renderPlot({
    aar_strike_zone_plot(dat_filt())
  },
  height = function(){
    w <- session$clientData$output_strike_zone_width
    if (is.null(w)) w <- 600L
    as.integer((5/6) * w)
  })
  # ===== TruMedia-style ZONE KDE heatmap (RASTER only; no points; no contours) =====
  # Defensive fallbacks if not already defined
  if (!exists("strike_zone", inherits = TRUE)) {
    strike_zone <- data.frame(xmin = -0.71, xmax = 0.71, ymin = 1.60, ymax = 3.40)
  }
  if (!exists("home_plate_segments", inherits = TRUE)) {
    home_plate_segments <- data.frame(
      x    = c(-0.85,-0.50, 0.50, 0.85, 0.50),
      y    = c( 0.05, 0.45, 0.45, 0.05,-0.35),
      xend = c(-0.50, 0.50, 0.85, 0.50,-0.50),
      yend = c( 0.45, 0.45, 0.05,-0.35, 0.45)
    )
  }
  
  to_num_local <- function(x){
    if (exists("to_num", inherits = TRUE)) return(to_num(x))
    suppressWarnings(as.numeric(x))
  }
  
  # convert inches -> feet if needed (your app sometimes mixes)
  as_ft_x <- function(x){
    x <- to_num_local(x)
    if (suppressWarnings(max(abs(x), na.rm = TRUE)) > 5) x <- x / 12
    x
  }
  as_ft_z <- function(z){
    z <- to_num_local(z)
    if (suppressWarnings(max(z, na.rm = TRUE)) > 10) z <- z / 12
    z
  }
  
  # fixed strike-zone bounds (feet)
  .zone_lims <- c(strike_zone$xmin[1], strike_zone$xmax[1], strike_zone$ymin[1], strike_zone$ymax[1])
  
  kde_zone_grid <- function(x, z, n = 180, h = c(0.18, 0.22), lims = .zone_lims){
    # Always return a full zone grid (so the entire zone is colored)
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Package MASS is required (it ships with base R installs).")
    }
    
    ok <- is.finite(x) & is.finite(z)
    x <- x[ok]; z <- z[ok]
    
    # If too few points, return a white zone (nd=0 everywhere)
    if (length(x) < 2) {
      gx <- seq(lims[1], lims[2], length.out = n)
      gz <- seq(lims[3], lims[4], length.out = n)
      g  <- expand.grid(plate_x = gx, plate_z = gz)
      g$nd <- 0
      return(g)
    }
    
    kd <- MASS::kde2d(x, z, n = n, h = h, lims = lims)
    g  <- expand.grid(plate_x = kd$x, plate_z = kd$y)
    
    v <- as.vector(kd$z)
    vmax <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(vmax) || vmax <= 0) {
      v <- rep(0, length(v))
    } else {
      v <- v / vmax   # normalize 0..1 so palette is consistent
      v[!is.finite(v)] <- 0
    }
    g$nd <- v
    g
  }
  
  build_zone_kde_by_pitchtype <- function(d, n = 180, h = c(0.18, 0.22)){
    d <- d %>%
      dplyr::mutate(
        .x = as_ft_x(plate_x),
        .z = as_ft_z(plate_z)
      ) %>%
      dplyr::filter(is.finite(.x), is.finite(.z), !is.na(PitchType))
    
    pts <- unique(as.character(d$PitchType))
    pts <- pts[!is.na(pts) & nzchar(pts)]
    
    out <- lapply(pts, function(pt){
      dd <- d %>% dplyr::filter(as.character(PitchType) == pt)
      g  <- kde_zone_grid(dd$.x, dd$.z, n = n, h = h)
      g$PitchType <- pt
      g
    })
    
    dplyr::bind_rows(out) %>%
      dplyr::mutate(PitchType = factor(PitchType, levels = pitch_levels_all))
  }
  
  plot_trumedia_zone_raster <- function(g, title = ""){
    ggplot(g, aes(plate_x, plate_z, fill = nd)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_gradientn(colors = wblyrm_palette, limits = c(0, 1), guide = "none") +
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
      coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
      facet_wrap(~PitchType, ncol = 2) +
      labs(title = title, x = NULL, y = NULL) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)
      )
  }
  # ===== end TruMedia zone KDE heatmap =====
  # -------------------- TruMedia zone density (NO dots, NO contours, NO facets) --------------------
  plot_zone_density_trumedia <- function(d, title = "",
                                         xlim = c(-2, 2), ylim = c(0, 5),
                                         bw = c(0.55, 0.65), ngrid = 320) {
    
    validate(need(nrow(d) > 0, "No rows in current filter."))
    
    # Robust feet conversion (handles inches inputs)
    x <- to_num(d$plate_x); if (suppressWarnings(max(abs(x), na.rm = TRUE)) > 5)  x <- x/12
    z <- to_num(d$plate_z); if (suppressWarnings(max(z,     na.rm = TRUE)) > 10) z <- z/12
    x <- -x
    
    dd <- dplyr::tibble(px = x, pz = z) %>%
      dplyr::filter(is.finite(px), is.finite(pz),
                    px >= xlim[1], px <= xlim[2],
                    pz >= ylim[1], pz <= ylim[2])
    
    validate(need(nrow(dd) >= 8, "Not enough pitches with locations for a density heatmap."))
    
    # clamp helper (avoids needing scales::squish)
    squish01 <- function(v) pmin(pmax(v, 0), 1)
    
    ggplot(dd, aes(px, pz)) +
      stat_density_2d(
        aes(fill = squish01(after_stat(ndensity))),
        geom = "raster",
        contour = FALSE,          # <- KEY: eliminates the “rings”
        n = ngrid,
        h = bw,
        na.rm = TRUE
      ) +
      scale_fill_gradientn(colors = wblyrm_palette, guide = "none", na.value = "#FFFFFF") +
      # Zone + plate on top
      geom_rect(
        data = strike_zone,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.6
      ) +
      geom_segment(
        data = home_plate_segments,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, colour = "black", linewidth = 1.1
      ) +
      coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
      labs(title = title, x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(6, 6, 6, 6)
      )
  }
  
  # -------------------- Damage Heat Maps --------------------
  output$damage_heat_total <- renderPlot({
    d <- dat_filt()
    
    # Ensure damage_event exists (defensive)
    if (!("damage_event" %in% names(d))) {
      d$damage_event <- is_bip_txst(d$pitch_call, d$play_result) & is.finite(d$ev) & d$ev >= 95.0
    }
    
    dd <- d %>% dplyr::filter(damage_event %in% TRUE)
    
    plot_zone_density_trumedia(
      dd,
      title = "Damage (Hard-hit frequency heatmap)"
    )
  },
  height = function(){
    w <- session$clientData$output_damage_heat_total_width
    if (is.null(w)) 650L else as.integer(0.90 * w)
  })
  
  output$damage_heat_filter <- renderPlot({
    d <- dat_filt()
    
    if (!("damage_event" %in% names(d))) {
      d$damage_event <- is_bip_txst(d$pitch_call, d$play_result) & is.finite(d$ev) & d$ev >= 95.0
    }
    
    sel <- input$damage_ptypes
    if (!is.null(sel) && length(sel)) {
      d <- d %>% dplyr::filter(as.character(PitchType) %in% sel)
    }
    
    dd <- d %>% dplyr::filter(damage_event %in% TRUE)
    
    pt_lab <- if (!is.null(sel) && length(sel)) paste(sel, collapse = " • ") else "All pitch types"
    
    plot_zone_density_trumedia(
      dd,
      title = paste0("Damage (", pt_lab, ")")
    )
  },
  height = function(){
    w <- session$clientData$output_damage_heat_filter_width
    if (is.null(w)) 650L else as.integer(0.90 * w)
  })
  
  # -------------------- Whiff Heat Maps --------------------
  output$whiff_heat_total <- renderPlot({
    d <- dat_filt()
    
    if (!("whiff_event" %in% names(d))) {
      d$whiff_event <- d$pitch_call %in% c("StrikeSwinging")
    }
    
    dd <- d %>% dplyr::filter(whiff_event %in% TRUE)
    
    plot_zone_density_trumedia(
      dd,
      title = "Whiff (Swinging-strike frequency heatmap)"
    )
  },
  height = function(){
    w <- session$clientData$output_whiff_heat_total_width
    if (is.null(w)) 650L else as.integer(0.90 * w)
  })
  
  
  output$whiff_heat_filter <- renderPlot({
    d <- dat_filt()
    
    if (!("whiff_event" %in% names(d))) {
      d$whiff_event <- d$pitch_call %in% c("StrikeSwinging")
    }
    
    sel <- input$whiff_ptypes
    if (is.null(sel) || !length(sel)) {
      return(empty_zone_plot("No pitch types selected"))
    }
    
    d <- d %>% dplyr::filter(as.character(PitchType) %in% sel)
    dd <- d %>% dplyr::filter(whiff_event %in% TRUE)
    
    pt_lab <- paste(sel, collapse = " • ")
    
    plot_zone_density_trumedia(
      dd,
      title = paste0("Whiff (", pt_lab, ")")
    )
  },
  height = function(){
    w <- session$clientData$output_whiff_heat_filter_width
    if (is.null(w)) 650L else as.integer(0.90 * w)
  })
  
  # -------------------- Swing Decisions --------------------
  output$swing_decisions <- renderPlot({
    d <- dat_filt()
    d$PitchType <- factor(as.character(d$PitchType),
                          levels = c(facet_levels, "Undefined", "Untagged"))
    
    strikes_taken <- d %>% dplyr::filter(
      is.finite(plate_x), is.finite(plate_z),
      pitch_call == "StrikeCalled", in_zone
    ) %>% dplyr::mutate(panel = factor("Strikes Taken", levels = c("Strikes Taken","Balls Chased")))
    
    balls_chased  <- d %>% dplyr::filter(
      is.finite(plate_x), is.finite(plate_z),
      in_zone == FALSE,
      pitch_call %in% c("StrikeSwinging","InPlay","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
    ) %>% dplyr::mutate(panel = factor("Balls Chased", levels = c("Strikes Taken","Balls Chased")))
    
    dd <- dplyr::bind_rows(strikes_taken, balls_chased)
    
    base_zone <- list(
      geom_rect(data = strike_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1.0),
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                   inherit.aes = FALSE, colour = "black", linewidth = 0.8)
    )
    
    if (nrow(dd) == 0) {
      return(
        ggplot() + base_zone +
          coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
          labs(x = NULL, y = NULL, title = "Swing Decisions by Pitch Type") +
          theme_minimal(base_size = 12) +
          theme(
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 1.2),
            strip.text = element_text(size = 12),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
            plot.margin = margin(0, 0, 0, 0)
          )
      )
    }
    
    ggplot(dd, aes(plate_x, plate_z)) +
      base_zone +
      geom_point(
        aes(fill = panel),
        shape = 21, size = 2.6, alpha = 0.9,
        color = "black", stroke = 0.3, show.legend = FALSE
      ) +
      scale_fill_manual(values = c("Strikes Taken" = "#501214", "Balls Chased" = "#B4975A"), drop = FALSE) +
      coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
      facet_wrap(~ PitchType + panel, ncol = 2) +
      labs(x = NULL, y = NULL, title = "Swing Decisions by Pitch Type (Strikes Taken | Balls Chased)") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        strip.background = element_rect(fill = "white", color = "black", linewidth = 1.2),
        strip.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
        plot.margin = margin(0, 0, 0, 0)
      )
  },
  height = function(){
    d <- dat_filt()
    present <- as.character(d$PitchType)
    present <- intersect(c(facet_levels, "Undefined", "Untagged"),
                         unique(stats::na.omit(present)))
    rows <- ceiling(max(1, length(present)) / 1)  # each type -> 2 panels (columns), 1 row step
    as.integer(420 * rows)
  })
  plot_sd <- function(d, title){
    x <- to_num(d$plate_x); if (suppressWarnings(max(abs(x), na.rm=TRUE)) > 5) x <- x/12
    z <- to_num(d$plate_z); if (suppressWarnings(max(z, na.rm=TRUE)) > 10) z <- z/12
    
    ggplot(dplyr::mutate(d, x_ft=x, z_ft=z), aes(x_ft, z_ft)) +
      stat_bin2d(bins = 35) +
      coord_fixed(xlim = c(-2,2), ylim = c(1,4), expand = FALSE) +
      ggtitle(title) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), plot.title = element_text(face="bold"))
  }
  
  # -------------------- Swing Decisions (INCHES zone) --------------------
  sd_filtered_data <- function(count_groups = NULL){
    d <- dat_filt()
    
    sel_pt <- input$sd_ptypes
    if (is.null(sel_pt) || !length(sel_pt)) return(d[0, , drop = FALSE])
    d <- d %>% dplyr::filter(as.character(.data$PitchType) %in% sel_pt)
    
    if (!is.null(count_groups)) {
      if (!length(count_groups)) return(d[0, , drop = FALSE])
      
      count_chr <- if ("CountStr" %in% names(d)) {
        nz_chr(d$CountStr)
      } else if ("Count" %in% names(d)) {
        nz_chr(d$Count)
      } else {
        balls <- .get_num(d, c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"))
        strikes <- .get_num(d, c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"))
        ifelse(is.finite(balls) & is.finite(strikes), paste0(balls, "-", strikes), "")
      }
      count_chr <- gsub("\\s+", "", count_chr)
      
      count_sets <- list(
        first  = "0-0",
        evens  = c("1-1", "2-2"),
        ahead  = c("1-0", "2-0", "3-0", "2-1", "3-1"),
        behind = c("0-1", "0-2", "1-2"),
        twok   = c("0-2", "1-2", "2-2", "3-2")
      )
      keep_counts <- unique(unlist(count_sets[count_groups], use.names = FALSE))
      d <- d[count_chr %in% keep_counts, , drop = FALSE]
    }
    
    d
  }
  
  output$sd_strikes_taken <- renderPlot({
    d <- sd_filtered_data()
    
    validate(need("in_zone" %in% names(d), "Zone not computed upstream"))
    
    # (Common interpretation) called strikes taken in-zone
    d <- d %>%
      dplyr::filter(
        pitch_call == "StrikeCalled",
        in_zone %in% TRUE,
        is.finite(plate_x), is.finite(plate_z)
      )
    
    validate(need(nrow(d) > 0, "No taken strikes in-zone in this filter."))
    
    plot_sd_zone_points(d, title = "Strikes Taken (Called Strikes in Zone)")
  })
  
  
  output$sd_balls_chased <- renderPlot({
    d <- sd_filtered_data()
    
    validate(need("in_zone" %in% names(d), "Zone not computed upstream"))
   
    d <- d %>%
      dplyr::filter(
        pitch_call %in% SWING_LIKE,
        in_zone %in% FALSE,
        is.finite(plate_x), is.finite(plate_z)
      )
    
    validate(need(nrow(d) > 0, "No chases (swings outside the zone) in this filter."))
    
    plot_sd_zone_points(d, title = "Balls Chased (Swings Outside Zone Only)")
  })
  
  output$sd_count_strikes_taken <- renderPlot({
    d <- sd_filtered_data(input$sd_count_groups)
    
    validate(need("in_zone" %in% names(d), "Zone not computed upstream"))
    
    d <- d %>%
      dplyr::filter(
        pitch_call == "StrikeCalled",
        in_zone %in% TRUE,
        is.finite(plate_x), is.finite(plate_z)
      )
    
    validate(need(nrow(d) > 0, "No taken strikes in-zone in this filter."))
    
    plot_sd_zone_points(d, title = "Strikes Taken (Count Filter)")
  })
  
  output$sd_count_balls_chased <- renderPlot({
    d <- sd_filtered_data(input$sd_count_groups)
    
    validate(need("in_zone" %in% names(d), "Zone not computed upstream"))
    
    d <- d %>%
      dplyr::filter(
        pitch_call %in% SWING_LIKE,
        in_zone %in% FALSE,
        is.finite(plate_x), is.finite(plate_z)
      )
    
    validate(need(nrow(d) > 0, "No chases (swings outside the zone) in this filter."))
    
    plot_sd_zone_points(d, title = "Balls Chased (Count Filter)")
  })
  
  # -------------------- Ball Flight (Total + 2-col PitchType grid) --------------------
  ev_bin_label <- function(ev){
    ev <- to_num(ev)
    dplyr::case_when(
      !is.finite(ev) ~ NA_character_,
      ev < 80        ~ "<80",
      ev < 90        ~ "80-90",
      ev < 95        ~ "90-95",
      ev < 100       ~ "95-100",
      TRUE           ~ "100+"
    )
  }
  
  add_spray_buckets <- function(d){
    la_val <- if ("la" %in% names(d)) to_num(d$la) else if ("LA" %in% names(d)) to_num(d$LA) else if ("LaunchAngle" %in% names(d)) to_num(d$LaunchAngle) else rep(NA_real_, nrow(d))
    pr_val <- if ("play_result" %in% names(d)) d$play_result else if ("PlayResult" %in% names(d)) d$PlayResult else rep(NA_character_, nrow(d))
    d %>%
      dplyr::mutate(
        BBType = factor(spray_contact_type(la_val), levels = contact_type_levels),
        ResultBucket = factor(spray_result_bucket(pr_val), levels = result_type_levels)
      )
  }
  
  filter_spray_buckets <- function(d){
    d <- add_spray_buckets(d)
    
    sel_ct <- input$spray_contact_types
    if (is.null(sel_ct) || !length(sel_ct)) return(d[0, , drop = FALSE])
    d <- d %>% dplyr::filter(as.character(.data$BBType) %in% sel_ct)
    
    sel_rs <- input$spray_result_types
    if (is.null(sel_rs) || !length(sel_rs)) return(d[0, , drop = FALSE])
    d <- d %>% dplyr::filter(as.character(.data$ResultBucket) %in% sel_rs)
    
    d
  }
  
  spray_data <- reactive({
    d <- dat_filt()
    
    # Balls in play only
    bip <- is_bip_txst(d$pitch_call, d$play_result)
    d <- d[bip %in% TRUE, , drop = FALSE]
    
    # EV bins filter
    d <- d %>%
      dplyr::mutate(
        ev_bin = ev_bin_label(ev)
      )
    
    sel_bins <- input$ev_bins
    if (!is.null(sel_bins) && length(sel_bins)) {
      d <- d %>% dplyr::filter(ev_bin %in% sel_bins)
    }
    d <- filter_spray_buckets(d)
    
    # Polar -> Cartesian (prefer TrackMan, else hc_x/hc_y)
    use_tm <- is.finite(d$distance_ft) & is.finite(d$bearing)
    d <- d %>%
      dplyr::mutate(
        plot_x = dplyr::if_else(use_tm, distance_ft * sin(bearing * pi/180), hc_x),
        plot_y = dplyr::if_else(use_tm, distance_ft * cos(bearing * pi/180), hc_y)
      ) %>%
      dplyr::filter(is.finite(plot_x), is.finite(plot_y))
    
    d
  })
  
  make_ballflight_plot <- function(dd, title = NULL){
    fld <- make_field_layers()
    
    # Make sure fld exists in THIS scope
    if (!exists("fld", inherits = FALSE) || is.null(fld)) {
      fld <- make_field_layers()
    }
    
    # rings may not have column "r" depending on your field builder
    rings_df <- fld$rings
    if (!is.null(rings_df) && nrow(rings_df)) {
      ring_group_col <- intersect(names(rings_df), c("r","ring","radius","dist","d"))[1]
      if (length(ring_group_col) == 0 || is.na(ring_group_col)) {
        rings_df$.ring_group <- 1L
        ring_group_col <- ".ring_group"
      }
    }
    
    base_field <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = fld$track_poly, ggplot2::aes(x = x, y = y),
                            fill = "white", color = NA) +
      ggplot2::geom_path(data = fld$wall, ggplot2::aes(x = x, y = y),
                         color = "#501214", linewidth = 1.1) +
      ggplot2::geom_segment(data = fld$foul_lines, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = "#501214") +
      { if (!is.null(rings_df) && nrow(rings_df)) {
        ggplot2::geom_path(
          data = rings_df,
          ggplot2::aes(x = x, y = y, group = .data[[ring_group_col]]),
          linetype = "dashed", color = "grey40", linewidth = 0.5
        )
      } else NULL } +
      ggplot2::geom_text(data = fld$ring_labels, ggplot2::aes(x = x, y = y, label = label),
                         color = "grey35", size = 3.2, vjust = 0.5) +
      ggplot2::geom_path(data = fld$bases_diamond, ggplot2::aes(x = x, y = y),
                         color = "black", linewidth = 0.9) +
      ggplot2::coord_fixed(xlim = spray_xlim, ylim = spray_ylim, expand = FALSE) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "black", fill = NA),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        legend.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(order = 1, override.aes = list(shape = unname(contact_legend_shape_values), fill = NA, size = 5.5, color = "black", alpha = 1)),
        fill  = ggplot2::guide_legend(order = 2, override.aes = list(shape = 21, size = 5.5, color = "black", alpha = 1))
      )
    
    
    if (!nrow(dd)) {
      return(base_field + labs(title = title %||% "Spray Chart", x=NULL, y=NULL))
    }
    
    dd$ev_bin <- factor(dd$ev_bin, levels = c("<80","80-90","90-95","95-100","100+"))
    
    base_field +
      geom_point(data=dd, aes(x=plot_x, y=plot_y, fill=ev_bin),
                 shape=21, size=4.8, color="black", stroke=0.5, alpha=0.95) +
      scale_fill_manual(
        name = "EV",
        values = c("<80"="#bdbdbd","80-90"="#9ecae1","90-95"="#a1d99b","95-100"="#fdd835","100+"="#fb8c00"),
        drop = FALSE
      ) +
      labs(title = title %||% "Spray Chart", x=NULL, y=NULL)
  }
  
  # Pre-build plot outputs for every pitch type (UI will show only those present)
  spray_pt_ids <- setNames(
    paste0("spray_pt_", gsub("[^A-Za-z0-9]", "", pitch_levels_all)),
    pitch_levels_all
  )
  
  lapply(pitch_levels_all, function(pt){
    oid <- spray_pt_ids[[pt]]
    output[[oid]] <- renderPlot({
      d <- spray_data()
      d <- d %>% dplyr::filter(as.character(PitchType) == pt)
      make_ballflight_plot(d, title = pt)
    })
  })
  
  observeEvent(spray_dat(), {
    d <- spray_dat()
    
    pts <- sort(unique(as.character(d$PitchType)))
    pts <- pts[pts %in% pitch_levels_all]
    
    for (pt in pts) {
      id <- paste0("spray_pt_", gsub("[^A-Za-z0-9]+", "_", pt))
      
      local({
        pt_local <- pt
        id_local <- id
        
        output[[id_local]] <- renderPlot({
          dd <- spray_dat() %>% dplyr::filter(as.character(PitchType) == pt_local)
          grid::grid.newpage()
          grid::grid.draw(build_spray_grob(dd))
        })
      })
    }
  }, ignoreInit = TRUE)
  
  
  # -------------------- Ball Flight --------------------
  ev_bucket_txst <- function(ev){
    cut(
      to_num(ev),
      breaks = c(-Inf, 80, 90, 95, 100, Inf),
      labels = c("<80","80-90","90-95","95-100","100+"),
      right = FALSE
    )
  }
  
  prep_ball_flight <- function(d){
    # EV bin filter
    bins <- input$ev_bins
    if (is.null(bins) || !length(bins)) bins <- c("<80","80-90","90-95","95-100","100+")
    
    d <- d %>% dplyr::mutate(ev_bin = ev_bucket_txst(ev)) %>%
      dplyr::filter(ev_bin %in% bins)
    
    # Must have polar or xy to plot
    use_tm <- is.finite(d$distance_ft) & is.finite(d$bearing)
    d <- d %>% dplyr::mutate(
      plot_x = dplyr::if_else(use_tm, distance_ft * sin(bearing * pi/180), hc_x),
      plot_y = dplyr::if_else(use_tm, distance_ft * cos(bearing * pi/180), hc_y)
    )
    
    # BIP only
    d$bip_flag <- is_bip_txst(d$pitch_call, d$play_result)
    d <- d %>% dplyr::filter(bip_flag, is.finite(plot_x), is.finite(plot_y))
    d <- filter_spray_buckets(d)
    
    # Ensure PitchNum exists for build_spray_grob()
    d$PitchNum <- seq_len(nrow(d))
    
    d
  }
  
  # --- TOTAL spray chart (this was blank) ---
  output$spray_chart <- renderPlot({
    d <- prep_ball_flight(dat_filt())
    validate(need(nrow(d) > 0, "No batted balls in this filter (after EV bins)."))
    
    grid::grid.newpage()
    grid::grid.draw(build_spray_grob(d))
  })
  
  # --- BY PITCH TYPE: one spray chart per pitch type (NOT one combined) ---
  output$spray_grid <- renderUI({
    d <- prep_ball_flight(dat_filt())
    if (!nrow(d)) return(tags$div("No batted balls in this filter (after EV bins)."))
    
    pts <- unique(as.character(d$PitchType))
    pts <- pts[pts %in% pitch_levels_all]
    pts <- pts[pts %in% facet_levels]  # keep it clean like your other pages
    if (!length(pts)) return(tags$div("No pitch types with batted balls in this filter."))
    
    tagList(lapply(pts, function(pt){
      pid <- paste0("spray_pt_", gsub("[^A-Za-z0-9]+", "", pt))
      
      local({
        pt_local <- pt
        pid_local <- pid
        output[[pid_local]] <- renderPlot({
          dd <- prep_ball_flight(dat_filt()) %>%
            dplyr::filter(as.character(PitchType) == pt_local)
          validate(need(nrow(dd) > 0, ""))
          
          grid::grid.newpage()
          grid::grid.draw(build_spray_grob(dd))
        })
      })
      
      tags$div(
        tags$h4(pt, style="margin:10px 0 6px 0; font-weight:800; color:#501214;"),
        plotOutput(pid, height = "520px")
      )
    }))
  })
  
  output$spray_chart_ptype <- renderPlot({
    d <- dat_filt()
    make_ballflight_spray_plot(d, ev_bins = input$ev_bins, facet_by_pt = TRUE)
  })
  
  # -------------------- AAR HSH table helpers --------------------
  season_info_from_group <- function(sg){
    sg <- toupper(trimws(nz_chr(sg)))
    if (!length(sg) || !nzchar(sg)) return(list(label = "Season", group = NA_character_))
    label <- dplyr::case_when(
      sg %in% c("S25","2025 SEASON","2025_SEASON") ~ "2025 Season",
      sg %in% c("F25","2025 FALL","2025_FALL")     ~ "2025 Fall",
      sg %in% c("SQ26","2026 SQUADS","2026_SQUADS")~ "2026 Squads",
      sg %in% c("S26","2026 SEASON","2026_SEASON") ~ "2026 Season",
      sg %in% c("PORT","PORTAL","PORTAL SEASON","PORTAL_SEASON") ~ "Portal",
      TRUE ~ sg
    )
    list(label = label, group = dplyr::case_when(
      sg %in% c("S25","2025 SEASON","2025_SEASON") ~ "S25",
      sg %in% c("F25","2025 FALL","2025_FALL")     ~ "F25",
      sg %in% c("SQ26","2026 SQUADS","2026_SQUADS")~ "SQ26",
      sg %in% c("S26","2026 SEASON","2026_SEASON") ~ "S26",
      sg %in% c("PORT","PORTAL","PORTAL SEASON","PORTAL_SEASON") ~ "PORT",
      TRUE ~ sg
    ))
  }
  
  season_group_for_game <- function(d_game){
    if (is.null(d_game) || !nrow(d_game)) return(NA_character_)
    if ("SeasonGroup" %in% names(d_game)) {
      sg <- nz_chr(d_game$SeasonGroup)
    } else if ("SeasonTag" %in% names(d_game)) {
      sg <- nz_chr(d_game$SeasonTag)
    } else {
      sg <- character(0)
    }
    sg <- sg[nzchar(sg)]
    if (!length(sg)) return(NA_character_)
    toupper(trimws(sg[1]))
  }
  
  hsh_rates <- function(d){
    if (is.null(d) || !nrow(d)) {
      return(list(
        whiff_rate = NA_real_, chase_rate = NA_real_, barrel_rate = NA_real_,
        whiff_n = NA_real_, chase_n = NA_real_, barrel_n = NA_real_
      ))
    }
    
    # Ensure zone logic uses inches (consistent with rest of app)
    if (!("plate_x_in" %in% names(d))) d$plate_x_in <- zone_s_in(d$plate_x)
    if (!("plate_z_in" %in% names(d))) d$plate_z_in <- zone_h_in(d$plate_z)
    
    balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
    strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
    
    sw <- swing_table(d, balls_col, strikes_col)
    swings <- sum(sw$is_swing, na.rm = TRUE)
    whiffs <- sum(sw$is_whiff, na.rm = TRUE)
    whiff_pct <- safe_ratio(whiffs, swings)
    
    # Chase counts (game)
    in_zone  <- .calc_in_zone_logical(d)
    out_zone <- (!is.na(in_zone)) & !(in_zone %in% TRUE)
    chase_n <- sum(sw$is_swing & out_zone, na.rm = TRUE)
    chase_pct <- safe_ratio(chase_n, sum(out_zone, na.rm = TRUE))
    
    pa <- pa_last_table(d, balls_col, strikes_col)
    bip <- is_bip_txst(pa$pc, pa$pr)
    bip_den <- sum(bip %in% TRUE, na.rm = TRUE)
    barrel_flag <- is_barrel_txst(pa$pc, pa$pr, pa$ev_pa, pa$la_pa)
    barrel_n <- sum(barrel_flag %in% TRUE, na.rm = TRUE)
    barrel_pct <- safe_ratio(barrel_n, bip_den)
    
    list(
      whiff_rate = whiff_pct, chase_rate = chase_pct, barrel_rate = barrel_pct,
      whiff_n = whiffs, chase_n = chase_n, barrel_n = barrel_n
    )
  }
  
  hsh_season_tbl <- function(){
    d_game <- aar_data()
    validate(need(nrow(d_game) > 0, "No data for AAR."))
    
    sg <- season_group_for_game(d_game)
    info <- season_info_from_group(sg)
    
    # Season comparison follows the report's hitter and selected game's season,
    # independently of the Hitting workspace sidebar.
    d_season <- df %>%
      dplyr::filter(.data$Batter == aar_hitter_value(), !(is_bullpen %in% TRUE))
    if (!is.na(info$group) && "SeasonGroup" %in% names(d_season)) {
      d_season <- d_season %>%
        dplyr::filter(toupper(trimws(.data$SeasonGroup)) == info$group)
    }
    
    g <- hsh_rates(d_game)
    s <- hsh_rates(d_season)
    # Season rates should match performance table logic
    s_perf <- summarize_overall(d_season)
    
    d1 <- c(whiff = 0.23, chase = 0.24, barrel = 0.17)
    
    tibble::tibble(
      Metric = c("Whiffs", "Chases", "Barrels"),
      Game = c(g$whiff_n, g$chase_n, g$barrel_n),
      Season = c(s_perf$`Whiff%`, s_perf$`Chase%`, s_perf$`Barrel%`),
      D1 = c(d1[["whiff"]], d1[["chase"]], d1[["barrel"]]),
      SeasonLabel = info$label
    )
  }
  
  output$aar_kpi <- renderUI({
    tbl <- hsh_season_tbl()
    
    # Shade continuously by distance from the D1 mean.
    shade_for <- function(metric, season, d1){
      lower_better <- metric %in% c("Whiffs","Chases")
      d1_shade_fill(season, d1, lower_better)
    }
    
    fmt_pct <- function(x) ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "—")
    fmt_cnt <- function(x) ifelse(is.finite(x), as.character(as.integer(round(x))), "—")
    
    season_label <- tbl$SeasonLabel[1] %||% "Season"
    
    rows <- lapply(seq_len(nrow(tbl)), function(i){
      bg <- shade_for(tbl$Metric[i], tbl$Season[i], tbl$D1[i])
      tags$tr(
        tags$td(tbl$Metric[i]),
        tags$td(fmt_cnt(tbl$Game[i])),
        tags$td(style = if (nzchar(bg)) paste0("background-color:", bg, ";") else NULL,
                fmt_pct(tbl$Season[i])),
        tags$td(fmt_pct(tbl$D1[i]))
      )
    })
    
    tagList(
      div(class = "aar-kpi-title", "Hit Strikes Hard"),
      tags$table(
        class = "aar-kpi",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("This Game"),
            tags$th(season_label),
            tags$th("D1 Avg")
          )
        ),
        tags$tbody(rows)
      )
    )
  })

  # -------------------- Team Report (Hitters) --------------------
  team_report_season_info <- reactive({
    req(input$team_report_game)
    d_game <- txst_df %>%
      dplyr::filter(CustomGameID == input$team_report_game, !(is_bullpen %in% TRUE))
    
    if (!nrow(d_game)) return(list(group = NA_character_, label = "Season"))
    
    sg <- season_group_for_game(d_game)
    info <- season_info_from_group(sg)
    list(group = info$group, label = info$label)
  })
  
  team_report_game_data <- reactive({
    req(input$team_report_game)
    d <- if (nrow(txst_df)) txst_df else df
    if ("CustomGameID" %in% names(d)) d$CustomGameID <- as.character(d$CustomGameID)
    d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))
    d <- d %>% dplyr::filter(.data$CustomGameID == input$team_report_game)
    
    d <- filter_team(d, "BatterTeam")
    
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    if (!("pitch_uid" %in% names(d)) && !("row_id" %in% names(d)) &&
        all(c("Date","Inning","PAofInning","PitchofPA") %in% names(d))) {
      d <- d %>% dplyr::distinct(Date, Inning, PAofInning, PitchofPA, .keep_all = TRUE)
    }
    
    validate(need(nrow(d) > 0, "No data for that game."))
    d
  })
  
  team_report_season_data <- reactive({
    req(input$team_report_game)
    d <- if (nrow(txst_df)) txst_df else df
    if ("CustomGameID" %in% names(d)) d$CustomGameID <- as.character(d$CustomGameID)
    d <- d %>% dplyr::filter(!(is_bullpen %in% TRUE))
    
    d <- filter_team(d, "BatterTeam")
    
    info <- team_report_season_info()
    season_col <- if ("SeasonGroup" %in% names(d)) "SeasonGroup" else if ("SeasonTag" %in% names(d)) "SeasonTag" else NULL
    if (!is.null(season_col) && !is.na(info$group)) {
      d <- d %>% dplyr::filter(toupper(trimws(.data[[season_col]])) == toupper(trimws(info$group)))
    }
    
    if ("pitch_uid" %in% names(d)) d <- d %>% dplyr::distinct(pitch_uid, .keep_all = TRUE)
    if ("row_id"   %in% names(d)) d <- d %>% dplyr::distinct(row_id,   .keep_all = TRUE)
    
    d
  })
  
  output$hit_team_report_preview <- renderImage({
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
      gp <- team_report_game_data()
      sp <- team_report_season_data()
      info <- team_report_season_info()
      
      p <- compose_team_hit_report_plot(
        game_p           = gp,
        season_p         = sp,
        game_id          = input$team_report_game,
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
  
  output$hit_team_report_dl <- downloadHandler(
    filename = function() {
      gp <- team_report_game_data()
      date_val <- as.Date(NA)
      if ("GameDate" %in% names(gp)) {
        gd <- parse_date_any(gp$GameDate)
        gd <- gd[!is.na(gd)]
        if (length(gd)) date_val <- gd[1]
      }
      if (is.na(date_val) && !is.null(input$team_report_game)) {
        date_val <- dplyr::coalesce(
          parse_date_any(input$team_report_game),
          extract_date_from_game_key(input$team_report_game)
        )[1]
      }
      date_str <- if (!is.na(date_val)) format(date_val, "%Y-%m-%d") else "UnknownDate"
      paste0("Bobcats_Hitters_", date_str, "_TeamReport.pdf")
    },
    content = function(file) {
      tryCatch({
        gp <- team_report_game_data()
        sp <- team_report_season_data()
        info <- team_report_season_info()
        
        p <- compose_team_hit_report_plot(
          game_p           = gp,
          season_p         = sp,
          game_id          = input$team_report_game,
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

  # -------------------- Contact Point --------------------
  contact_event_weight <- function(pr){
    prl <- tolower(nz_chr(pr))
    dplyr::case_when(
      grepl("home\\s*run|\\bhr\\b", prl) ~ woba_weights$HR,
      grepl("\\b3b\\b|triple", prl)     ~ woba_weights$X3B,
      grepl("\\b2b\\b|double", prl)     ~ woba_weights$X2B,
      grepl("\\b1b\\b|single", prl)     ~ woba_weights$X1B,
      TRUE                              ~ 0
    )
  }
  
  contact_point_data <- reactive({
    d <- dat_filt()
    
    # Balls in play only
    bip <- is_bip_txst(d$pitch_call, d$play_result)
    if (all(is.na(bip)) || !any(bip %in% TRUE)) {
      # fallback using play_result only (exclude BB/HBP/K)
      prl <- tolower(nz_chr(d$play_result))
      bip <- grepl("(?i)in\\s*play|\\b(1b|2b|3b|hr)\\b|single|double|triple|home\\s*run|out|error", prl) &
        !grepl("(?i)walk|intentional|\\bibb\\b|hbp|hit\\s*by\\s*pitch|strikeout|\\bk\\b", prl)
    }
    d <- d[bip %in% TRUE, , drop = FALSE]
    
    # Contact coordinates
    pick_col <- function(df, cands){
      hit <- cands[cands %in% names(df)]
      if (length(hit)) hit[[1]] else NA_character_
    }
    cx_col <- pick_col(d, c("ContactPositionZ","ContactX","contactX","Contact_X","ContactX_in","ContactXIn","contact_x","contact_x_in",
                            "ContactSide","ContactSideInches"))
    cy_col <- pick_col(d, c("ContactPositionY","ContactY","contactY","Contact_Y","ContactY_in","ContactYIn","contact_y","contact_y_in",
                            "ContactDepth","ContactDepthInches"))
    d$ContactX <- if (!is.na(cx_col)) to_num(d[[cx_col]]) else NA_real_  # side (x-axis)
    d$ContactY <- if (!is.na(cy_col)) to_num(d[[cy_col]]) else NA_real_  # depth (y-axis)
    
    d <- d %>%
      dplyr::filter(is.finite(ContactX), is.finite(ContactY))
    
    # ---- Normalize units/axis for overhead view (heuristics) ----
    x <- d$ContactX
    y <- d$ContactY
    
    # If values look like feet, convert to inches
    if (is.finite(suppressWarnings(max(abs(x), na.rm = TRUE))) && suppressWarnings(max(abs(x), na.rm = TRUE)) < 5) {
      x <- x * 12
    }
    if (is.finite(suppressWarnings(max(abs(y), na.rm = TRUE))) && suppressWarnings(max(abs(y), na.rm = TRUE)) < 10) {
      y <- y * 12
    }
    
    # If depth is negative (toward pitcher), flip
    y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
    if (is.finite(y_med) && y_med < 0) y <- -y
    
    # If still too close to 0, shift by plate length (17")
    y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
    if (is.finite(y_med) && y_med < 5) y <- y + 17
    
    d$ContactX_plot <- x
    d$ContactY_plot <- y
    
    # Batted-ball type by LA
    la_val <- if ("la" %in% names(d)) to_num(d$la) else if ("LA" %in% names(d)) to_num(d$LA) else to_num(d$LaunchAngle)
    d$BBType <- dplyr::case_when(
      is.finite(la_val) & la_val >= 25 ~ "Fly Ball",
      is.finite(la_val) & la_val >= 10 ~ "Line Drive",
      is.finite(la_val)               ~ "Ground Ball",
      TRUE                            ~ NA_character_
    )
    d <- d %>% dplyr::filter(!is.na(BBType))
    
    d$BBType <- factor(d$BBType, levels = c("Ground Ball","Line Drive","Fly Ball"))
    d$RowID <- seq_len(nrow(d))
    d
  })
  
  contact_depth_table <- function(dd){
    validate(need(nrow(dd) > 0, "No contact points in this filter."))
    
    ev_val <- if ("ev" %in% names(dd)) to_num(dd$ev) else to_num(dd$EV)
    w_con  <- contact_event_weight(dd$play_result)
    
    depth_in <- pmax(dd$ContactY_plot, 0)
    bucket_start <- floor(depth_in / 10) * 10
    bucket_label <- paste0(bucket_start, "-", bucket_start + 10)
    
    tbl <- dplyr::tibble(
      Bucket = bucket_label,
      EV = ev_val,
      w_con = w_con
    ) %>%
      dplyr::group_by(Bucket) %>%
      dplyr::summarise(
        `BIP` = dplyr::n(),
        `wOBAcon` = safe_ratio(sum(w_con, na.rm = TRUE), dplyr::n()),
        `Avg EV` = ifelse(any(is.finite(EV)), mean(EV, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::arrange(as.numeric(sub("^([0-9]+).*", "\\1", Bucket)))
    
    tbl <- tbl %>%
      dplyr::mutate(
        wOBAcon = ifelse(is.finite(wOBAcon), sprintf("%.3f", wOBAcon), NA),
        `Avg EV` = ifelse(is.finite(`Avg EV`), sprintf("%.1f", `Avg EV`), NA)
      )
    tbl
  }
  
  contact_lasso_table <- function(d, selected_ids){
    selected_ids <- suppressWarnings(as.integer(selected_ids))
    selected_ids <- selected_ids[is.finite(selected_ids)]
    d <- d %>% dplyr::mutate(.group = ifelse(.data$RowID %in% selected_ids, "Circled Points", "Rest"))
    
    ev_val <- if ("ev" %in% names(d)) to_num(d$ev) else to_num(d$EV)
    w_con  <- contact_event_weight(d$play_result)
    
    tbl <- d %>%
      dplyr::mutate(EV = ev_val, w_con = w_con) %>%
      dplyr::group_by(.data$.group) %>%
      dplyr::summarise(
        BIP = dplyr::n(),
        wOBAcon = safe_ratio(sum(.data$w_con, na.rm = TRUE), dplyr::n()),
        `Avg EV` = ifelse(any(is.finite(.data$EV)), mean(.data$EV, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::right_join(tibble::tibble(.group = c("Circled Points", "Rest")), by = ".group") %>%
      dplyr::mutate(
        BIP = dplyr::coalesce(.data$BIP, 0L),
        wOBAcon = ifelse(is.finite(.data$wOBAcon), sprintf("%.3f", .data$wOBAcon), NA),
        `Avg EV` = ifelse(is.finite(.data$`Avg EV`), sprintf("%.1f", .data$`Avg EV`), NA)
      ) %>%
      dplyr::rename(Group = .group) %>%
      dplyr::select(Group, BIP, wOBAcon, `Avg EV`)
    
    tbl
  }
  
  contact_point_plot <- function(d, title = NULL, source = NULL){
    if (is.null(d) || !nrow(d)) {
      return(plotly::ggplotly(ggplot() + theme_void() + labs(title = "No contact points"), source = source))
    }
    
    # Home plate (top-down, inches). Back tip at (0,0), plate depth = 17".
    plate <- data.frame(
      x = c(-8.5,  8.5,  8.5,   0, -8.5, -8.5),
      y = c(17.0, 17.0,  8.5,  0.0,  8.5, 17.0)
    )
    
    ev_val <- if ("ev" %in% names(d)) to_num(d$ev) else to_num(d$EV)
    la_val <- if ("la" %in% names(d)) to_num(d$la) else if ("LA" %in% names(d)) to_num(d$LA) else to_num(d$LaunchAngle)
    dist_val <- if ("distance_ft" %in% names(d)) to_num(d$distance_ft) else to_num(d$Distance)
    
    d$hover_txt <- sprintf(
      "EV: %s<br>LA: %s<br>Distance: %s",
      ifelse(is.finite(ev_val), sprintf("%.1f", ev_val), "NA"),
      ifelse(is.finite(la_val), sprintf("%.0f", la_val), "NA"),
      ifelse(is.finite(dist_val), sprintf("%.0f", dist_val), "NA")
    )
    
    cols <- c(
      "Ground Ball" = "#2E7D32",
      "Line Drive"  = "#FB8C00",
      "Fly Ball"    = "#E53935"
    )
    
    p <- ggplot(d, aes(ContactX_plot, ContactY_plot, color = BBType, text = hover_txt, key = RowID)) +
      geom_polygon(data = plate, aes(x = x, y = y),
                   inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.8) +
      geom_hline(
        yintercept = seq(10, 50, by = 10),
        linetype = "dashed", color = "grey70", linewidth = 0.4
      ) +
      geom_point(size = 3.2, alpha = 0.9) +
      scale_color_manual(values = cols, name = NULL) +
      coord_fixed(xlim = c(-20, 20), ylim = c(0, 50), expand = FALSE) +
      labs(title = title %||% "Contact Point", x = "Point of Contact Side (in)", y = "Point of Contact Depth (in)") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    plotly::ggplotly(p, tooltip = "text", source = source) %>%
      plotly::layout(dragmode = "lasso") %>%
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtons = list(list("lasso2d")),
        displaylogo = FALSE
      )
  }
  
  output$contact_point_total <- renderPlotly({
    d <- contact_point_data()
    contact_point_plot(d, title = "Contact Point (Total)", source = "contact_total")
  })
  
  output$contact_point_total_tbl <- DT::renderDT({
    d <- contact_point_data()
    validate(need(nrow(d) > 0, "No contact points in this filter."))
    
    sel <- plotly::event_data("plotly_selected", source = "contact_total")
    if (!is.null(sel) && nrow(sel) > 0 && "key" %in% names(sel)) {
      keep <- suppressWarnings(as.integer(sel$key))
      keep <- keep[is.finite(keep)]
      if (length(keep)) {
        tbl <- contact_lasso_table(d, keep)
        return(DT::datatable(
          tbl, rownames = FALSE, escape = FALSE, selection = "none",
          options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
        ))
      }
    }
    
    tbl <- contact_depth_table(d)
    
    DT::datatable(
      tbl, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
    )
  })
  
  output$contact_point_grid <- renderUI({
    d <- contact_point_data()
    if (!nrow(d)) return(tags$div("No contact points in this filter."))
    
    pts <- unique(as.character(d$PitchType))
    pts <- pts[pts %in% pitch_levels_all]
    if (!length(pts)) return(tags$div("No pitch types with contact points in this filter."))
    
    tagList(lapply(pts, function(pt){
      pid <- paste0("contact_pt_", gsub("[^A-Za-z0-9]+", "", pt))
      tid <- paste0("contact_tbl_", gsub("[^A-Za-z0-9]+", "", pt))
      
      local({
        pt_local <- pt
        pid_local <- pid
        tid_local <- tid
        output[[pid_local]] <- renderPlotly({
          dd <- contact_point_data() %>% dplyr::filter(as.character(PitchType) == pt_local)
          contact_point_plot(dd, title = pt_local, source = pid_local)
        })
        output[[tid_local]] <- DT::renderDT({
          dd <- contact_point_data() %>% dplyr::filter(as.character(PitchType) == pt_local)
          
          sel <- plotly::event_data("plotly_selected", source = pid_local)
          if (!is.null(sel) && nrow(sel) > 0 && "key" %in% names(sel)) {
            keep <- suppressWarnings(as.integer(sel$key))
            keep <- keep[is.finite(keep)]
            if (length(keep)) {
              tbl <- contact_lasso_table(dd, keep)
              return(DT::datatable(
                tbl, rownames = FALSE, escape = FALSE, selection = "none",
                options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
              ))
            }
          }
          
          tbl <- contact_depth_table(dd)
          
          DT::datatable(
            tbl, rownames = FALSE, escape = FALSE, selection = "none",
            options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
          )
        })
      })
      
      tags$div(
        tags$h4(pt, style="margin:10px 0 6px 0; font-weight:800; color:#501214;"),
        fluidRow(
          column(6, plotlyOutput(pid, height = "520px")),
          column(6, DTOutput(tid))
        )
      )
    }))
  })
  
  ev_keep <- function(ev, bins){
    ev <- to_num(ev)
    if (is.null(bins) || !length(bins)) return(rep(TRUE, length(ev)))
    keep <- rep(FALSE, length(ev))
    if ("<80"   %in% bins) keep <- keep | (is.finite(ev) & ev < 80)
    if ("80-90" %in% bins) keep <- keep | (is.finite(ev) & ev >= 80  & ev < 90)
    if ("90-95" %in% bins) keep <- keep | (is.finite(ev) & ev >= 90  & ev < 95)
    if ("95-100"%in% bins) keep <- keep | (is.finite(ev) & ev >= 95  & ev < 100)
    if ("100+"  %in% bins) keep <- keep | (is.finite(ev) & ev >= 100)
    keep
  }
  spray_base <- reactive({
    d <- dat_filt()
    bip <- is_bip_txst(d$pitch_call, d$play_result)
    d <- d[bip, , drop = FALSE]
    d <- d[ev_keep(d$ev, input$ev_bins), , drop = FALSE]
    d
  })
  
  observe({
    d0 <- spray_base()
    pts <- sort(unique(as.character(d0$PitchType)))
    pts <- pts[pts %in% facet_levels]
    
    lapply(pts, function(pt){
      local({
        pt_local <- pt
        plot_id <- paste0("spray_pt_", gsub("[^A-Za-z0-9]+","_", pt_local))
        
        output[[plot_id]] <- renderPlot({
          d <- spray_base() %>% dplyr::filter(as.character(PitchType) == pt_local)
          # reuse your existing AAR-style field plot logic (no numbering)
          use_tm <- is.finite(d$distance_ft) & is.finite(d$bearing)
          d <- d %>% dplyr::mutate(
            plot_x = dplyr::if_else(use_tm, distance_ft * sin(bearing * pi/180), hc_x),
            plot_y = dplyr::if_else(use_tm, distance_ft * cos(bearing * pi/180), hc_y)
          )
          la_val <- if ("la" %in% names(d)) to_num(d$la) else if ("LaunchAngle" %in% names(d)) to_num(d$LaunchAngle) else rep(NA_real_, nrow(d))
          spr_dist <- sqrt(d$plot_x^2 + d$plot_y^2)
          
          short_gb <- is.finite(la_val) & la_val <= 10 & spr_dist < 90
          d_line <- d[short_gb, , drop = FALSE]
          d_pt   <- d[!short_gb, , drop = FALSE]
          
          fld <- make_field_layers()
          ggplot() +
            geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
            geom_path(data=fld$wall, aes(x=x,y=y), color="#501214", linewidth=1.1) +
            geom_segment(data=fld$foul_lines, aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
            geom_path(data=fld$rings, aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
            geom_text(data=fld$ring_labels, aes(x=x,y=y,label=label), color="grey35", size=3.2, vjust=0.5) +
            geom_path(data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
            geom_point(
              data = d %>% dplyr::filter(is.finite(plot_x), is.finite(plot_y)),
              aes(x = plot_x, y = plot_y),
              size = 3.8, alpha = 0.9
            ) +
            coord_fixed(xlim = spray_xlim, ylim = spray_ylim, expand = FALSE) +
            theme_minimal(base_size = 11) +
            theme(panel.grid = element_blank(),
                  panel.border = element_rect(color="black", fill=NA))
        })
      })
    })
  })
  make_ballflight_spray_plot <- function(d, ev_bins, facet_by_pt = FALSE){
    fld <- make_field_layers()
    
    base_field <- ggplot() +
      geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
      geom_path(data=fld$wall, aes(x=x,y=y), color="#501214", linewidth=1.1) +
      geom_segment(data=fld$foul_lines, aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
      geom_path(data=fld$rings, aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
      geom_text(data=fld$ring_labels, aes(x=x,y=y,label=label), color="grey35", size=3.2, vjust=0.5) +
      geom_path(data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
      coord_fixed(xlim=spray_xlim, ylim=spray_ylim, expand=FALSE) +
      theme_minimal(base_size=11) +
      theme(
        panel.grid=element_blank(),
        panel.border=element_rect(color="black", fill=NA),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # coords (TM polar preferred)
    use_tm <- is.finite(d$distance_ft) & is.finite(d$bearing)
    dd <- d %>%
      dplyr::mutate(
        plot_x = dplyr::if_else(use_tm, distance_ft * sin(bearing * pi/180), hc_x),
        plot_y = dplyr::if_else(use_tm, distance_ft * cos(bearing * pi/180), hc_y)
      )
    
    dd <- dd %>%
      dplyr::mutate(bip_flag = is_bip_txst(pitch_call, play_result)) %>%
      dplyr::filter(bip_flag, is.finite(plot_x), is.finite(plot_y))
    
    # EV bins
    dd <- dd %>%
      dplyr::mutate(
        ev_bin = dplyr::case_when(
          is.finite(ev) & ev < 80 ~ "<80",
          is.finite(ev) & ev < 90 ~ "80-90",
          is.finite(ev) & ev < 95 ~ "90-95",
          is.finite(ev) & ev < 100 ~ "95-100",
          is.finite(ev) ~ "100+",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(ev_bin), ev_bin %in% ev_bins)
    
    dd <- filter_spray_buckets(dd) %>%
      dplyr::filter(!is.na(BBType), !is.na(ResultBucket))

    # Ground balls: dashed line to 120 ft along bearing; icon at 120 ft
    if ("bearing" %in% names(dd)) {
      dd$gb_x <- ifelse(dd$BBType == "Ground Ball" & is.finite(dd$bearing),
                        120 * sin(dd$bearing * pi/180), NA_real_)
      dd$gb_y <- ifelse(dd$BBType == "Ground Ball" & is.finite(dd$bearing),
                        120 * cos(dd$bearing * pi/180), NA_real_)
      dd$plot_x <- ifelse(!is.na(dd$gb_x), dd$gb_x, dd$plot_x)
      dd$plot_y <- ifelse(!is.na(dd$gb_y), dd$gb_y, dd$plot_y)
    }
    
    if (!nrow(dd)) {
      return(base_field + annotate("text", x=0, y=200, label="No batted-ball data in this filter", fontface=2))
    }
    
    # Legend dummies for clean legend keys
    legend_ct <- data.frame(BBType = contact_type_levels)
    legend_rs <- data.frame(ResultBucket = result_type_levels)
    
    p <- base_field +
      geom_point(
        data = legend_ct,
        mapping = aes(x = -999, y = -999, shape = BBType),
        size = 5.5, alpha = 0,
        inherit.aes = FALSE,
        show.legend = c(shape = TRUE, fill = FALSE)
      ) +
      geom_point(
        data = legend_rs,
        mapping = aes(x = -999, y = -999, fill = ResultBucket),
        shape = 21, size = 5.5, alpha = 0,
        inherit.aes = FALSE,
        show.legend = c(shape = FALSE, fill = TRUE)
      ) +
      geom_segment(
        data = dd %>% dplyr::filter(BBType == "Ground Ball", is.finite(gb_x), is.finite(gb_y)),
        aes(x = 0, y = 0, xend = gb_x, yend = gb_y),
        linetype = "dashed", color = "black", linewidth = 0.6, alpha = 0.7,
        inherit.aes = FALSE
      ) +
      geom_point(
        data = dd,
        aes(x=plot_x, y=plot_y, shape=BBType, fill=ResultBucket),
        size=5.5, color="black", stroke=0.6, alpha=0.95,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      scale_shape_manual(
        name   = "Contact type",
        values = contact_shape_values,
        breaks = contact_type_levels,
        drop = FALSE
      ) +
      scale_fill_manual(
        name="Result",
        values=result_fill_values,
        breaks=result_type_levels,
        labels=result_type_labels,
        drop=FALSE
      ) +
      guides(
        shape = guide_legend(
          order=1,
          override.aes=list(
            shape = unname(contact_legend_shape_values),
            fill  = NA, size=5.5, color="black", alpha=1
          )
        ),
        fill  = guide_legend(order=2, override.aes=list(shape=21, size=5.5, color="black", alpha=1))
      )
    
    
    if (facet_by_pt) {
      dd$PitchType <- factor(as.character(dd$PitchType), levels = pitch_levels_all)
      p <- p + facet_wrap(~PitchType)
    }
    
    p
  }
}
