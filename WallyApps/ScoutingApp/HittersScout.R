suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
  library(tidyverse)
  library(readr)
  library(patchwork)
  library(grid)
  library(gridExtra)
  library(DT)
  library(ggplot2)
})

options(shiny.maxRequestSize = 1000 * 1024^2)
options(shiny.sanitize.errors = FALSE)

# ---- Paths ----
APP_ROOT <- normalizePath(".", winslash = "/", mustWork = FALSE)
DATA_DIR <- file.path(APP_ROOT, "data")
env_path <- file.path(APP_ROOT, ".Renviron")
if (file.exists(env_path)) readRenviron(env_path)

# ---- FTP config ----
FTP_BASE_PATH <- Sys.getenv("TRACKMAN_FTP_BASE", unset = "v3")
FTP_DEBUG     <- nzchar(Sys.getenv("TRACKMAN_FTP_DEBUG", unset = "")) &&
  Sys.getenv("TRACKMAN_FTP_DEBUG") != "0"
CURRENT_YEAR  <- as.integer(format(Sys.Date(), "%Y"))
PREV_YEAR     <- CURRENT_YEAR - 1

# -------------------- Page sizing --------------------
LETTER_W <- 11
LETTER_H <- 8.5
PORTRAIT_W <- 8.5
PORTRAIT_H <- 11
preview_dpi <- 130
px_from_in <- function(x) as.integer(x * preview_dpi)
`%||%` <- function(x,y) if (!is.null(x)) x else y

# -------------------- Colors --------------------
TXST_MAROON <- "#501214"
TXST_GOLD   <- "#B4975A"
SHADE_GREEN <- "#A1D99B"  # good
SHADE_RED   <- "#F4A6A6"  # bad

# -------------------- Small utils --------------------
safe_ratio <- function(a,b) ifelse(b > 0, a/b, NA_real_)
to_num     <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
nz_chr     <- function(x) ifelse(is.na(x), "", as.character(x))
pick_first <- function(cands, in_df) { cands <- cands[cands %in% names(in_df)]; if (length(cands)) cands[[1]] else NA_character_ }
read_csv_files <- function(paths){
  if (!length(paths)) return(tibble::tibble())
  dfs <- lapply(paths, function(fp){
    tryCatch(
      readr::read_csv(
        fp,
        col_types = readr::cols(.default = readr::col_character()),
        guess_max = 200000, progress = FALSE, show_col_types = FALSE
      ),
      error = function(e){
        message("[READ_CSV ERROR] ", basename(fp), " :: ", conditionMessage(e))
        tibble::tibble()
      }
    )
  })
  dplyr::bind_rows(dfs)
}

# -------------------- FTP helpers --------------------
need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for FTP access. Please install it (install.packages('%s')).", pkg, pkg))
  }
}

normalize_team_code <- function(x) {
  toupper(trimws(as.character(x)))
}

sanitize_ftp_host <- function(host) {
  h <- trimws(as.character(host))
  h <- gsub("\\s+", "", h)
  h <- sub("^['\"]", "", h)
  h <- sub("['\"]$", "", h)
  h
}

encode_ftp_path <- function(path) {
  if (!requireNamespace("curl", quietly = TRUE)) return(path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste(vapply(parts, curl::curl_escape, character(1)), collapse = "/")
}

make_ftp_url <- function(host, path, is_dir = FALSE) {
  host <- sanitize_ftp_host(host)
  scheme <- if (grepl("^s?ftps?://", host, ignore.case = TRUE)) "" else "ftp://"
  url <- paste0(scheme, host, "/", encode_ftp_path(path))
  if (is_dir && !grepl("/$", url)) url <- paste0(url, "/")
  url
}

parse_ftp_listing <- function(txt) {
  lines <- unlist(strsplit(txt, "\r?\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(character(0))
  
  out <- character(0)
  for (ln in lines) {
    if (grepl("type=", ln, ignore.case = TRUE) && grepl(";", ln)) {
      nm <- sub(".*;\\s*", "", ln)
      out <- c(out, nm)
      next
    }
    m <- regexec("^([dl-][rwx-]{9})\\s+\\d+\\s+\\S+\\s+\\S+\\s+\\d+\\s+\\w+\\s+\\d+\\s+\\S+\\s+(.*)$", ln)
    mm <- regmatches(ln, m)
    if (length(mm[[1]]) >= 3) {
      out <- c(out, mm[[1]][3])
      next
    }
    m <- regexec("^(\\d{2}-\\d{2}-\\d{2})\\s+\\d{2}:\\d{2}[AP]M\\s+(<DIR>|\\d+)\\s+(.*)$", ln)
    mm <- regmatches(ln, m)
    if (length(mm[[1]]) >= 4) {
      out <- c(out, mm[[1]][4])
      next
    }
    parts <- strsplit(ln, "\\s+")[[1]]
    if (length(parts)) out <- c(out, parts[[length(parts)]])
  }
  out <- out[nzchar(out)]
  out
}

ftp_handle <- function(user, pass, dirlistonly = NULL) {
  epsv <- Sys.getenv("TRACKMAN_FTP_EPSV", unset = "1") != "0"
  eprt <- Sys.getenv("TRACKMAN_FTP_EPRT", unset = "1") != "0"
  connect_timeout <- suppressWarnings(as.numeric(Sys.getenv("TRACKMAN_FTP_CONNECT_TIMEOUT", unset = "20")))
  timeout <- suppressWarnings(as.numeric(Sys.getenv("TRACKMAN_FTP_TIMEOUT", unset = "120")))
  verify <- Sys.getenv("TRACKMAN_FTP_SSL_VERIFY", unset = "1") != "0"
  
  opts <- list(
    userpwd = paste0(user, ":", pass),
    ftp_use_epsv = epsv,
    ftp_use_eprt = eprt
  )
  if (!is.null(dirlistonly)) opts$dirlistonly <- dirlistonly
  if (is.finite(connect_timeout)) opts$connecttimeout <- connect_timeout
  if (is.finite(timeout)) opts$timeout <- timeout
  if (!verify) {
    opts$ssl_verifypeer <- FALSE
    opts$ssl_verifyhost <- 0
  }
  
  do.call(curl::new_handle, opts)
}

ftp_retries <- function() {
  r <- suppressWarnings(as.integer(Sys.getenv("TRACKMAN_FTP_RETRIES", unset = "2")))
  if (!is.finite(r) || r < 1) 1L else r
}

ftp_list_dir <- function(host, user, pass, path, quiet = TRUE) {
  need_pkg("curl")
  url <- make_ftp_url(host, path, is_dir = TRUE)
  res <- NULL
  last_err <- NULL
  for (i in seq_len(ftp_retries())) {
    h <- ftp_handle(user, pass, dirlistonly = TRUE)
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (!inherits(res, "error")) break
    last_err <- res
    Sys.sleep(min(2, 0.5 * i))
  }
  if (inherits(res, "error") && !is.null(last_err)) res <- last_err
  if (inherits(res, "error")) {
    if (quiet) return(structure(character(0), error = conditionMessage(res)))
    stop(conditionMessage(res))
  }
  txt <- rawToChar(res$content)
  lines <- unlist(strsplit(txt, "\r?\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines)) {
    looks_like_listing <- any(
      grepl("type=", lines, ignore.case = TRUE) |
        grepl("^([dl-][rwx-]{9})", lines) |
        grepl("^\\d{2}-\\d{2}-\\d{2}", lines)
    )
    if (looks_like_listing) return(parse_ftp_listing(paste(lines, collapse = "\n")))
    return(lines)
  }
  
  res2 <- NULL
  last_err <- NULL
  for (i in seq_len(ftp_retries())) {
    h2 <- ftp_handle(user, pass, dirlistonly = FALSE)
    res2 <- tryCatch(curl::curl_fetch_memory(url, handle = h2), error = function(e) e)
    if (!inherits(res2, "error")) break
    last_err <- res2
    Sys.sleep(min(2, 0.5 * i))
  }
  if (inherits(res2, "error") && !is.null(last_err)) res2 <- last_err
  if (inherits(res2, "error")) {
    if (quiet) return(structure(character(0), error = conditionMessage(res2)))
    stop(conditionMessage(res2))
  }
  txt2 <- rawToChar(res2$content)
  parse_ftp_listing(txt2)
}

ftp_download <- function(host, user, pass, path, dest) {
  need_pkg("curl")
  url <- make_ftp_url(host, path, is_dir = FALSE)
  last_err <- NULL
  for (i in seq_len(ftp_retries())) {
    h <- ftp_handle(user, pass, dirlistonly = NULL)
    ok <- tryCatch({
      curl::curl_download(url, destfile = dest, handle = h, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) { last_err <<- e; FALSE })
    if (ok) return(invisible(TRUE))
    if (file.exists(dest)) unlink(dest)
    Sys.sleep(min(2, 0.5 * i))
  }
  if (!is.null(last_err)) stop(conditionMessage(last_err))
  invisible(FALSE)
}

find_col_norm <- function(df, target) {
  if (is.null(names(df))) return(NULL)
  nms <- names(df)
  norm <- toupper(gsub("[^A-Z0-9]", "", nms))
  tgt  <- toupper(gsub("[^A-Z0-9]", "", target))
  idx <- match(tgt, norm)
  if (!is.na(idx)) df[[idx]] else NULL
}

extract_team_cols <- function(df) {
  pt <- find_col_norm(df, "PitcherTeam")
  bt <- find_col_norm(df, "BatterTeam")
  if (is.null(pt) && is.null(bt) && ncol(df) >= 13) {
    pt <- df[[9]]
    bt <- df[[13]]
  }
  list(pt = pt, bt = bt)
}

filter_team_rows <- function(df, team_code) {
  cols <- extract_team_cols(df)
  if (is.null(cols$pt) && is.null(cols$bt)) {
    return(df[0, , drop = FALSE])
  }
  pt <- normalize_team_code(cols$pt)
  bt <- normalize_team_code(cols$bt)
  keep <- (!is.na(pt) & pt == team_code) | (!is.na(bt) & bt == team_code)
  df[keep, , drop = FALSE]
}

list_year_paths <- function(host, user, pass, year) {
  errs <- character(0)
  year_path <- file.path(FTP_BASE_PATH, year)
  months <- ftp_list_dir(host, user, pass, year_path, quiet = TRUE)
  err <- attr(months, "error", exact = TRUE)
  if (!is.null(err) && nzchar(err)) errs <- c(errs, sprintf("list %s: %s", year_path, err))
  months <- months[grepl("^(0?[1-9]|1[0-2])$", months)]
  if (length(months) == 0) return(structure(character(0), errors = errs))
  
  paths <- character(0)
  for (m in months) {
    month_path <- file.path(year_path, m)
    days <- ftp_list_dir(host, user, pass, month_path, quiet = TRUE)
    err <- attr(days, "error", exact = TRUE)
    if (!is.null(err) && nzchar(err)) errs <- c(errs, sprintf("list %s: %s", month_path, err))
    days <- days[grepl("^(0?[1-9]|[12][0-9]|3[01])$", days)]
    if (length(days) == 0) next
    for (d in days) {
      day_path <- file.path(month_path, d)
      files <- ftp_list_dir(host, user, pass, day_path, quiet = TRUE)
      err <- attr(files, "error", exact = TRUE)
      if (!is.null(err) && nzchar(err)) errs <- c(errs, sprintf("list %s: %s", day_path, err))
      files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
      if (length(files) == 0) next
      paths <- c(paths, file.path(day_path, files))
    }
  }
  structure(paths, errors = errs)
}

coalesce_cols_safe <- function(d, names_vec, as = c("character","numeric")){
  as <- match.arg(as); n <- nrow(d)
  vecs <- lapply(names_vec, function(nm){
    if (nm %in% names(d)) {
      v <- d[[nm]]
      if (as == "character") as.character(v) else suppressWarnings(readr::parse_number(as.character(v)))
    } else {
      if (as == "character") rep(NA_character_, n) else rep(NA_real_, n)
    }
  })
  Reduce(dplyr::coalesce, vecs)
}

# -------------------- Geometry + palette --------------------
home_plate_segments <- data.frame(
  x=c(0,0.71,0.71,0,-0.71,-0.71), y=c(0.15,0.15,0.3,0.5,0.3,0.15),
  xend=c(0.71,0.71,0,-0.71,-0.71,0), yend=c(0.15,0.3,0.5,0.3,0.15,0.15)
)
strike_zone <- data.frame(xmin=-0.71, xmax=0.71, ymin=1.60, ymax=3.40)
wblyrm_palette <- c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff")

zone_layers <- list(
  geom_rect(data = strike_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.6),
  geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE, colour = "black", linewidth = 0.45)
)

# -------------------- Pitch type mapping --------------------
facet_levels <- c("Fastball","Sinker","Changeup","Splitter","Slider","Cutter","Sweeper","Curveball")
pitch_levels_all <- c(facet_levels, "Undefined", "Untagged")

canonical_pitch_fuzzy <- function(x){
  x0 <- trimws(as.character(x))
  x0 <- gsub("_|-", " ", x0)
  x0 <- gsub("\\s+", " ", x0)
  dplyr::case_when(
    grepl("(?i)fast ?ball|\\bff\\b|\\bfb\\b|\\bfour ?seam|\\b4 ?seam|\\b2 ?seam|\\btwo ?seam|\\bft\\b", x0, perl=TRUE) ~ "Fastball",
    grepl("(?i)sink|\\bsi\\b", x0, perl=TRUE) ~ "Sinker",
    grepl("(?i)change|\\bch\\b", x0, perl=TRUE) ~ "Changeup",
    grepl("(?i)split|fork", x0, perl=TRUE) ~ "Splitter",
    grepl("(?i)slider|\\bsl\\b", x0, perl=TRUE) ~ "Slider",
    grepl("(?i)cutter|\\bct\\b|\\bfc\\b", x0, perl=TRUE) ~ "Cutter",
    grepl("(?i)sweep", x0, perl=TRUE) ~ "Sweeper",
    grepl("(?i)curve|knuckle\\s*curve|slurve|\\bcu\\b|\\bkc\\b", x0, perl=TRUE) ~ "Curveball",
    x0 == "" ~ "Untagged",
    TRUE ~ "Undefined"
  )
}

# ---- EXACTLY SIX groups (no "Other") ----
pitch_group_levels <- c("Fastball","Sinker","Cutter","Change/Split","Slider/Sweeper","Curveball")
pitch_group_of <- function(pt_chr){
  case_when(
    pt_chr %in% c("Fastball") ~ "Fastball",
    pt_chr %in% c("Sinker") ~ "Sinker",
    pt_chr %in% c("Cutter") ~ "Cutter",
    pt_chr %in% c("Changeup","Splitter") ~ "Change/Split",
    pt_chr %in% c("Slider","Sweeper") ~ "Slider/Sweeper",
    pt_chr %in% c("Curveball") ~ "Curveball",
    TRUE ~ NA_character_
  )
}

hitter_report_pitch_types <- c("Fastball","Sinker","Cutter","Changeup","Splitter","Slider","Sweeper","Curveball")

filter_hitter_report_pitches <- function(d){
  if (is.null(d) || !is.data.frame(d) || !nrow(d) || !("PitchType" %in% names(d))) return(d)
  d %>%
    dplyr::filter(!is.na(PitchType), as.character(PitchType) %in% hitter_report_pitch_types)
}

# -------------------- Canonical pitch_call (safe, perl=TRUE) --------------------
canon_pitch_call <- function(x){
  x <- trimws(as.character(x))
  x <- gsub("_|-", " ", x)
  x <- gsub("\\s+", " ", x)
  xl <- tolower(x)
  out <- ifelse(
    grepl("strike.*(called|looking)|called.*stride?ke|\\bcs\\b|\\bstrike look", xl, perl = TRUE),
    "StrikeCalled",
    ifelse(
      grepl("strike.*(swing|miss|whiff)|swing.*strike|\\bss\\b|\\bswstr\\b", xl, perl = TRUE),
      "StrikeSwinging",
      ifelse(
        grepl("foul\\s*tip", xl, perl = TRUE), "FoulTip",
        ifelse(
          grepl("foul.*(pop|fly|fieldable|line|ground)", xl, perl = TRUE), "FoulBallFieldable",
          ifelse(
            grepl("\\bfoul\\b", xl, perl = TRUE), "FoulBallNotFieldable",
            ifelse(
              grepl("in\\s*play.*no\\s*out|in\\s*play.*noout|inplay.*no\\s*out", xl, perl = TRUE), "InPlayNoOut",
              ifelse(
                grepl("in\\s*play.*out", xl, perl = TRUE), "InPlayOut",
                ifelse(
                  grepl("in\\s*play|ball\\s*in\\s*play|\\bbip\\b", xl, perl = TRUE), "InPlay",
                  ifelse(
                    grepl("\\bball\\b|pitchout|auto\\s*ball|intentional|\\bibb\\b", xl, perl = TRUE), "BallCalled",
                    x
                  )))))))
    )
  )
  out
}

# -------------------- BIP semantics (no lookaheads) --------------------
is_bip_txst <- function(pc, pr){
  pr_chr <- nz_chr(pr)
  pc_bip <- pc %in% c("InPlay","InPlayNoOut","InPlayOut")
  hits_any <- grepl("(?i)home\\s*run|homerun|\\bHR\\b|\\bsingle\\b|\\btriple\\b|\\bdouble\\b", pr_chr, perl=TRUE)
  hits_any <- hits_any & !grepl("(?i)double\\s*play", pr_chr, perl=TRUE)
  outs_in_play <- grepl("(?i)(ground|fly|line|pop|field|force)\\s*-?out", pr_chr, perl=TRUE)
  inplay_other <- grepl("(?i)reaches? on error|\\berror\\b|sacrifice|sac\\s*bunt|bunt", pr_chr, perl=TRUE)
  non_bip <- grepl("(?i)walk|intentional|\\bIBB\\b|hit\\s*by\\s*pitch|\\bHBP\\b|interference|catcher\\s*interference", pr_chr, perl=TRUE)
  (pc_bip | hits_any | outs_in_play | inplay_other) & !non_bip
}

# ---- PA outcome helpers (match PitchingApp) ----
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

pa_outcome_cols <- function(pa) {
  n <- nrow(pa)
  pr <- if ("PlayResult" %in% names(pa)) as.character(pa$PlayResult) else rep("", n)
  kb <- if ("KorBB" %in% names(pa)) as.character(pa$KorBB) else rep("", n)
  pr2 <- trimws(pr)
  kb2 <- trimws(kb)
  is_k   <- grepl("strike.?out|\\bK\\b", pr2, ignore.case = TRUE) |
    grepl("\\bK\\b|strikeout",   kb2, ignore.case = TRUE)
  is_bb  <- grepl("\\bwalk\\b|\\bbb\\b", kb2, ignore.case = TRUE) |
    grepl("\\bwalk\\b",          pr2, ignore.case = TRUE)
  is_ibb <- grepl("intentional|\\bibb\\b", kb2, ignore.case = TRUE) |
    grepl("intentional",           pr2, ignore.case = TRUE)
  tibble::tibble(
    K  = is_k,
    BB = is_bb & !is_ibb,
    BIP_pa = safe_is_bip(pa$PitchCall, pr)
  )
}

# ---- Barrel (match PitchingApp: strict EV/LA on BIP only) ----
is_bip_for_barrel <- function(pc) {
  x <- trimws(as.character(pc))
  !is.na(x) & x %in% c("InPlay","InPlayNoOut","InPlayOut")
}
is_barrel_strict <- function(pc, ev, la) {
  evn <- to_num(ev); lan <- to_num(la)
  bip <- is_bip_for_barrel(pc)
  out <- bip & is.finite(evn) & is.finite(lan) & evn >= 95 & lan >= 5 & lan <= 40
  out[is.na(out)] <- FALSE
  out
}

# -------------------- Plate coalesce --------------------
normalize_plate_coord <- function(vec, colname){
  v <- to_num(vec); if (!length(v)) return(v)
  is_inch_name <- !is.na(colname) && grepl("(?i)inch|_in\\b", colname)
  q90 <- suppressWarnings(stats::quantile(abs(v), 0.90, na.rm = TRUE)); if (!is.finite(q90)) q90 <- 0
  looks_like_inches <- if (!is.na(colname) && grepl("(?i)z|height", colname)) q90 > 6 else q90 > 3.5
  if (is_inch_name || looks_like_inches) v <- v / 12
  v
}
derive_zone_inches <- function(d){
  h <- if ("PlateLocHeight" %in% names(d)) to_num(d$PlateLocHeight) else d$plate_z
  s <- if ("PlateLocSide"   %in% names(d)) to_num(d$PlateLocSide)   else d$plate_x
  h <- ifelse(is.finite(h) & h < 10, h * 12, h)
  s <- ifelse(is.finite(s) & abs(s) < 5, s * 12, s)
  dplyr::case_when(
    !is.finite(h) | !is.finite(s) ~ NA,
    h >= 18.29 & h <= 44.08 & s >= -9.97 & s <= 9.97 ~ TRUE,
    TRUE ~ FALSE
  )
}
coalesce_plate <- function(d, cands){
  if (!length(cands)) return(rep(NA_real_, nrow(d)))
  mats <- lapply(cands, function(nm) normalize_plate_coord(d[[nm]], nm))
  out <- mats[[1]]; if (length(mats) > 1) for (k in 2:length(mats)) out <- dplyr::coalesce(out, mats[[k]])
  out
}

# -------------------- PA & swing tables --------------------
make_pa_id <- function(d){
  if (all(c("Date","Inning","PAofInning","Batter") %in% names(d))) {
    interaction(d$Date, d$Inning, d$PAofInning, d$Batter, drop = TRUE)
  } else if ("PitchofPA" %in% names(d)) {
    cumsum(d$PitchofPA == 1)
  } else {
    terminal <- d$pitch_call %in% c("InPlay","InPlayOut","InPlayNoOut","StrikeSwinging","StrikeCalled") |
      (!is.na(d$play_result) & nzchar(d$play_result))
    cumsum(c(1L, head(terminal, -1)))
  }
}
pa_last_table <- function(d, balls_col, strikes_col){
  d <- d %>%
    mutate(
      balls_b4_raw   = if (!is.na(balls_col))   to_num(.data[[balls_col]])   else NA_real_,
      strikes_b4_raw = if (!is.na(strikes_col)) to_num(.data[[strikes_col]]) else NA_real_,
      ev_pa = to_num(ev),
      la_pa = to_num(la)
    )
  d$PA_ID <- make_pa_id(d)
  d %>% group_by(PA_ID) %>% slice_tail(n = 1) %>% ungroup() %>%
    mutate(pr = nz_chr(play_result), kb = if ("KorBB" %in% names(.)) nz_chr(KorBB) else "",
           pc = nz_chr(pitch_call), balls_b4 = balls_b4_raw, strikes_b4 = strikes_b4_raw)
}
swing_table <- function(d, balls_col, strikes_col){
  d <- d %>%
    mutate(
      balls_b4_raw   = if (!is.na(balls_col))   to_num(.data[[balls_col]])   else NA_real_,
      strikes_b4_raw = if (!is.na(strikes_col)) to_num(.data[[strikes_col]]) else NA_real_
    )
  need_fallback <- !any(is.finite(d$strikes_b4_raw))
  if (need_fallback) {
    d$PA_ID <- make_pa_id(d)
    adds_strike <- d$pitch_call %in% c("StrikeSwinging","StrikeCalled","FoulTip","FoulBallFieldable","FoulBallNotFieldable")
    d <- d %>% group_by(PA_ID) %>% mutate(strikes_b4_raw = lag(cumsum(adds_strike), default = 0)) %>% ungroup()
  }
  d %>%
    mutate(
      is_swing   = pitch_call %in% c("StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip","InPlay","InPlayOut","InPlayNoOut"),
      is_contact = pitch_call %in% c("InPlay","FoulBallFieldable","FoulBallNotFieldable","FoulTip"),
      is_whiff   = pitch_call == "StrikeSwinging",
      strikes_b4 = strikes_b4_raw,
      pre2k      = is.finite(strikes_b4) & strikes_b4 < 2,
      two_k      = is.finite(strikes_b4) & strikes_b4 >= 2,
      in_zone    = if ("in_zone" %in% names(.)) as.logical(.data$in_zone)
      else (dplyr::between(plate_z, 1.60, 3.40) & dplyr::between(plate_x, -0.71, 0.71))
    )
}

# -------------------- Summary (TOP TABLE) --------------------
summarize_overall <- function(d){
  balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  d$PA_ID <- make_pa_id(d)
  pa_last <- pa_last_table(d, balls_col, strikes_col)
  d_sw    <- swing_table(d, balls_col, strikes_col)
  
  outc <- pa_outcome_cols(pa_last)
  pa_sum <- pa_outcome_summary(pa_last)
  hit_type <- coalesce_cols_safe(pa_last, c("TaggedHitType","AutoHitType","HitType","BattedBallType"), as = "character")
  launch_angle <- dplyr::coalesce(to_num(pa_last$la_pa), coalesce_cols_safe(pa_last, c("Angle","LaunchAngle","Launch_Angle"), as = "numeric"))
  bip <- safe_is_bip(pa_last$pc, pa_last$pr) | is.finite(launch_angle) | nzchar(trimws(hit_type))
  is_gb <- grepl("(?i)ground", hit_type, perl = TRUE) | (is.finite(launch_angle) & launch_angle < 10)
  hard_hit_flag <- bip & is.finite(pa_last$ev_pa) & pa_last$ev_pa >= 95
  
  swings      <- sum(d_sw$is_swing,   na.rm=TRUE)
  whiffs      <- sum(d_sw$is_whiff,   na.rm=TRUE)
  o_zone_seen <- sum(d_sw$in_zone %in% FALSE, na.rm = TRUE)
  chases      <- sum(d_sw$is_swing & (d_sw$in_zone %in% FALSE), na.rm = TRUE)
  
  tibble::tibble(
    `K%`    = safe_ratio(sum(outc$K, na.rm = TRUE), nrow(pa_last)),
    `BB%`   = safe_ratio(sum(outc$BB, na.rm = TRUE), nrow(pa_last)),
    `SLUG`  = safe_ratio(sum(pa_sum$TB, na.rm = TRUE), sum(pa_sum$AB, na.rm = TRUE)),
    `GB%`   = safe_ratio(sum(is_gb & bip, na.rm = TRUE), sum(bip, na.rm = TRUE)),
    `HH%`   = safe_ratio(sum(hard_hit_flag, na.rm = TRUE), sum(bip, na.rm = TRUE)),
    `Whiff%` = safe_ratio(whiffs, swings),
    `Chase%` = safe_ratio(chases, o_zone_seen)
  )
}
# ---- First-pitch swing % helpers (robust) ----
# ---- First-pitch detection + FPS math (exact by-PA definitions) ----
HD_SET <- c("Fastball","Sinker")
OS_SET <- c("Cutter","Changeup","Splitter","Slider","Sweeper","Curveball")

compute_is_first_pitch <- function(d){
  # Priority 1: explicit pitch index in PA
  pofpa <- pick_first(c("PitchofPA","PitchOfPA","PitchInPA","PitchNo","Pitch_Number"), d)
  if (!is.na(pofpa) && any(is.finite(to_num(d[[pofpa]])))) {
    return(to_num(d[[pofpa]]) == 1)
  }
  # Priority 2: count-based (0-0 before pitch)
  balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  if (!is.na(balls_col) || !is.na(strikes_col)) {
    b <- if (!is.na(balls_col))   to_num(d[[balls_col]])   else NA_real_
    s <- if (!is.na(strikes_col)) to_num(d[[strikes_col]]) else NA_real_
    return(dplyr::coalesce(b, 0) == 0 & dplyr::coalesce(s, 0) == 0)
  }
  # Priority 3: PA boundary fallback (order-dependent)
  d$PA_ID <- make_pa_id(d)
  out <- d %>% dplyr::group_by(PA_ID) %>% dplyr::mutate(.fp = dplyr::row_number() == 1) %>% dplyr::ungroup()
  out$.fp %in% TRUE
}

first_pitch_rows <- function(d){
  if (!nrow(d)) return(tibble::tibble())
  d2 <- d
  d2$PA_ID <- make_pa_id(d2)
  is_fp <- compute_is_first_pitch(d2)
  fp <- d2[is_fp %in% TRUE, , drop = FALSE]
  if (!nrow(fp)) return(tibble::tibble())
  tibble::tibble(
    PA_ID = fp$PA_ID,
    hand  = dplyr::if_else(fp$PitcherHand %in% c("LHP","RHP"), fp$PitcherHand, NA_character_),
    pt    = as.character(fp$PitchType),
    is_hd = !is.na(fp$PitchType) & as.character(fp$PitchType) %in% HD_SET,
    is_os = !is.na(fp$PitchType) & as.character(fp$PitchType) %in% OS_SET,
    is_strike = fp$pitch_call %in% c("StrikeCalled","StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip","InPlay","InPlayOut","InPlayNoOut")
  )
}

fps_metrics_by_hand <- function(d){
  fp <- first_pitch_rows(d)
  if (!nrow(fp)) {
    mk <- function(lbl) tibble::tibble(Split = lbl, `FPS%` = NA_real_, `FPS% HD` = NA_real_, `FPS% OS` = NA_real_)
    return(dplyr::bind_rows(mk("v LHP"), mk("v RHP"), mk("Total")))
  }
  calc_row <- function(sub, lbl){
    # Denominators follow your spec:
    #  - FPS%      : first-pitch STRIKE rate (PitchingApp logic)
    #  - FPS% HD   : first-pitch strike rate on HD-set
    #  - FPS% OS   : first-pitch strike rate on OS-set
    fps_all <- safe_ratio(sum(sub$is_strike, na.rm = TRUE),               nrow(sub))
    fps_hd  <- safe_ratio(sum(sub$is_strike & sub$is_hd, na.rm = TRUE),    sum(sub$is_hd, na.rm = TRUE))
    fps_os  <- safe_ratio(sum(sub$is_strike & sub$is_os, na.rm = TRUE),    sum(sub$is_os, na.rm = TRUE))
    tibble::tibble(Split = lbl, `FPS%` = fps_all, `FPS% HD` = fps_hd, `FPS% OS` = fps_os)
  }
  vL <- fp %>% dplyr::filter(hand == "LHP")
  vR <- fp %>% dplyr::filter(hand == "RHP")
  dplyr::bind_rows(
    calc_row(vL, "v LHP"),
    calc_row(vR, "v RHP"),
    calc_row(fp, "Total")
  )
}


fps_rates <- function(d){
  if (!nrow(d)) return(list(all = NA_real_, hd = NA_real_, os = NA_real_))
  # Identify first-pitch rows only
  is_fp <- compute_is_first_pitch(d)
  fp <- d[is_fp %in% TRUE, , drop = FALSE]
  if (!nrow(fp)) return(list(all = NA_real_, hd = NA_real_, os = NA_real_))
  
  # Swing on the first pitch?
  is_swing <- fp$pitch_call %in% c("StrikeSwinging","InPlay","FoulBallFieldable","FoulBallNotFieldable","FoulTip")
  
  # Canonical pitch type on first pitch
  pt <- as.character(fp$PitchType)
  # FPS% (all first pitches)
  fps_all <- safe_ratio(sum(is_swing, na.rm = TRUE), length(is_swing))
  
  # HD = Fastball or Sinker
  hd_mask <- !is.na(pt) & pt %in% HD_SET
  fps_hd  <- safe_ratio(sum(is_swing & hd_mask, na.rm = TRUE), sum(hd_mask, na.rm = TRUE))
  
  # OS = all defined pitch types EXCEPT Fastball/Sinker
  os_mask <- !is.na(pt) & pt %in% OS_SET
  fps_os  <- safe_ratio(sum(is_swing & os_mask, na.rm = TRUE), sum(os_mask, na.rm = TRUE))
  
  list(all = fps_all, hd = fps_hd, os = fps_os)
}


# -------------------- Column standardizer --------------------
std_cols <- function(d){
  n <- nrow(d)
  px_candidates <- intersect(c("PlateLocSide","px","PlateX","Plate_X","PlateLocSideInches"), names(d))
  pz_candidates <- intersect(c("PlateLocHeight","pz","PlateZ","Plate_Z","PlateLocHeightInches"), names(d))
  pc   <- pick_first(c("PitchCall","Pitch_Call","Call","PitchResult","Pitch_Result"), d)
  pr   <- pick_first(c("PlayResult","Result","Event","Play_Result"), d)
  evc  <- pick_first(c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV"), d)
  lac  <- pick_first(c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle"), d)
  sx   <- pick_first(c("hc_x","HC_X","bb_x","BB_X"), d)
  sy   <- pick_first(c("hc_y","HC_Y","bb_y","BB_Y"), d)
  bdc  <- pick_first(c("Bearing","bearing","SprayAngle","spray_angle","HC_Bearing","HitDirection"), d)
  dst  <- pick_first(c("Distance","HitDistance","CarryDistance","Carry_Distance","ProjectedDistance",
                       "EstimatedDistance","TrackmanDistance","carry_distance"), d)
  
  d$plate_x <- coalesce_plate(d, px_candidates)
  d$plate_z <- coalesce_plate(d, pz_candidates)
  d$in_zone <- derive_zone_inches(d)
  d$pitch_call  <- if (!is.na(pc))  canon_pitch_call(d[[pc]]) else rep(NA_character_, n)
  d$play_result <- if (!is.na(pr))  as.character(d[[pr]])     else rep(NA_character_, n)
  d$ev <- if (!is.na(evc)) to_num(d[[evc]]) else rep(NA_real_, n)
  d$la <- if (!is.na(lac)) to_num(d[[lac]]) else rep(NA_real_, n)
  d$hc_x <- if (!is.na(sx)) to_num(d[[sx]]) else rep(NA_real_, n)
  d$hc_y <- if (!is.na(sy)) to_num(d[[sy]]) else rep(NA_real_, n)
  d$bearing     <- if (!is.na(bdc)) to_num(d[[bdc]]) else rep(NA_real_, n)
  d$distance_ft <- if (!is.na(dst)) to_num(d[[dst]]) else rep(NA_real_, n)
  
  raw_pt_chr <- coalesce_cols_safe(d, c("PitchType","pitch_type_canon","TaggedPitchType","PitchName","AutoPitchType"), as = "character")
  d$PitchType <- factor(canonical_pitch_fuzzy(raw_pt_chr), levels = c(pitch_levels_all))
  
  side_raw <- coalesce_cols_safe(d, c("BatterSide","BatterStand","Stand","HitterSide","Batter_Side"), as = "character")
  side_raw <- toupper(trimws(side_raw))
  d$BatterSide <- dplyr::case_when(
    side_raw %in% c("L","LEFT","LHH") ~ "L",
    side_raw %in% c("R","RIGHT","RHH") ~ "R",
    side_raw %in% c("S","SW","SWITCH","SH") ~ "S",
    TRUE ~ NA_character_
  )
  
  if (!("PitcherHand" %in% names(d))) {
    pt <- pick_first(c("p_throws","PThrows","PitcherThrows","throws","PitcherHandedness","P_Hand"), d)
    if (!is.na(pt)) {
      ph <- toupper(as.character(d[[pt]]))
      d$PitcherHand <- dplyr::case_when(
        ph %in% c("L","LH","LHP","LEFT")  ~ "LHP",
        ph %in% c("R","RH","RHP","RIGHT") ~ "RHP",
        TRUE ~ NA_character_
      )
    } else {
      d$PitcherHand <- rep(NA_character_, n)
    }
  } else {
    ph <- toupper(as.character(d$PitcherHand))
    d$PitcherHand <- dplyr::case_when(
      ph %in% c("L","LH","LHP","LEFT")  ~ "LHP",
      ph %in% c("R","RH","RHP","RIGHT") ~ "RHP",
      TRUE ~ NA_character_
    )
  }
  d
}

batter_side_suffix <- function(d){
  if (!("BatterSide" %in% names(d))) return("")
  v <- toupper(na.omit(d$BatterSide))
  if (!length(v)) return("")
  side <- names(sort(table(v), decreasing = TRUE))[1]
  if (side == "L") return(" - LHH")
  if (side == "R") return(" - RHH")
  if (side == "S") return(" - SH")
  ""
}

# -------------------- Row 1 stats + table (with shading) --------------------
row1_table_numeric <- function(d){
  make_stat_row <- function(dd, split_label){
    if (!nrow(dd)) {
      tibble::tibble(
        Split = split_label,
        `K%` = NA_real_, `BB%` = NA_real_, `SLUG` = NA_real_,
        `GB%` = NA_real_, `HH%` = NA_real_,
        `Whiff%` = NA_real_, `Chase%` = NA_real_
      )
    } else {
      s <- summarize_overall(dd)
      tibble::tibble(
        Split = split_label,
        `K%`     = s$`K%`[[1]],
        `BB%`    = s$`BB%`[[1]],
        `SLUG`   = s$`SLUG`[[1]],
        `GB%`    = s$`GB%`[[1]],
        `HH%`    = s$`HH%`[[1]],
        `Whiff%` = s$`Whiff%`[[1]],
        `Chase%` = s$`Chase%`[[1]]
      )
    }
  }
  
  is_L <- !is.na(d$PitcherHand) & d$PitcherHand == "LHP"
  is_R <- !is.na(d$PitcherHand) & d$PitcherHand == "RHP"
  
  core <- dplyr::bind_rows(
    make_stat_row(d[is_L, , drop = FALSE], "v LHP"),
    make_stat_row(d[is_R, , drop = FALSE], "v RHP"),
    make_stat_row(d,                        "Total")
  )
  
  core %>%
    dplyr::select(Split, `K%`, `BB%`, `SLUG`, `GB%`, `HH%`, `Whiff%`, `Chase%`)
}



# -------------------- D1 shading --------------------
D1_PCT <- c(
  `K%`=0.192, `BB%`=0.114, `SLUG`=0.400, `GB%`=0.420, `HH%`=0.174, `Barrel%`=0.174,
  `Contact%`=0.771, `Z-Contact%`=0.843,
  `Whiff%`=0.229, `IZ-Whiff%`=0.157,
  `Chase%`=0.242, `Pre2K Chase%`=0.192, `2K Chase%`=0.363,
  `LD+FB%`=0.477
)
lower_better <- c("BB%","SLUG","HH%","Barrel%","Contact%","Z-Contact%","LD+FB%")
metric_map <- c("Pre2kC%" = "Pre2K Chase%", "2kC%" = "2K Chase%")

format_percent_display <- function(df, exclude_cols = character(0), decimal_cols = character(0)){
  disp <- df
  for (nm in setdiff(names(df), exclude_cols)) {
    if (!is.numeric(df[[nm]])) next
    if (nm %in% decimal_cols) {
      disp[[nm]] <- scales::number(df[[nm]], accuracy = 0.001)
    } else {
      disp[[nm]] <- scales::percent(df[[nm]], accuracy = 0.1)
    }
  }
  disp
}

# Build tableGrob with pitch-table styling + per-cell shading
make_shaded_table_grob <- function(disp_df, num_df, header_title = NULL,
                                   widen_first = 2.2, other_w = 1.25, padding_pt = 2){
  if (is.null(disp_df) || nrow(disp_df) == 0 || ncol(disp_df) == 0) {
    return(grid::grobTree(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
      grid::textGrob("No data", gp = grid::gpar(col = "#666666", cex = 0.9))
    ))
  }
  stopifnot(identical(names(disp_df), names(num_df)))
  th <- gridExtra::ttheme_minimal(
    core    = list(
      fg_params = list(fontsize = 8.4, fontface = "bold"),
      bg_params = list(fill = "#FFFFFF", col = "#D7D0C6", lwd = 0.4),
      padding  = unit(c(padding_pt, max(padding_pt * 0.35, 0.35)), "pt")
    ),
    colhead = list(
      fg_params = list(fontsize = 8.1, fontface = "bold", col = TXST_GOLD),
      bg_params = list(fill = TXST_MAROON, col = TXST_MAROON, lwd = 0.45),
      padding = unit(c(padding_pt, max(padding_pt * 0.35, 0.35)), "pt")
    )
  )
  
  nr <- nrow(disp_df); nc <- ncol(disp_df)
  fills <- matrix(NA_character_, nrow = nr, ncol = nc)
  col_idx <- function(nm) match(nm, names(disp_df))
  
  # D1-based shading (±0.05 vs D1)
  for (nm in names(disp_df)) {
    ref_key <- metric_map[[nm]] %||% nm
    if (!(ref_key %in% names(D1_PCT))) next
    j <- col_idx(nm); if (is.na(j)) next
    v <- suppressWarnings(as.numeric(num_df[[nm]]))
    avg <- D1_PCT[[ref_key]]
    low_better <- ref_key %in% lower_better
    good <- if (low_better) (!is.na(v) & v <= (avg - 0.05)) else (!is.na(v) & v >= (avg + 0.05))
    bad  <- if (low_better) (!is.na(v) & v >= (avg + 0.05)) else (!is.na(v) & v <= (avg - 0.05))
    fills[good, j] <- SHADE_GREEN
    fills[bad,  j] <- SHADE_RED
  }
  
  tg <- tryCatch(
    gridExtra::tableGrob(disp_df, rows = NULL, theme = th),
    error = function(e){
      message("[make_shaded_table_grob] tableGrob failed: ", conditionMessage(e))
      gridExtra::tableGrob(disp_df, rows = NULL, theme = gridExtra::ttheme_minimal())
    }
  )

  # Apply D1 shading on top of the base table styling (per-cell)
  tryCatch({
    sh <- as.matrix(fills)
    if (all(dim(sh) == dim(as.matrix(disp_df)))) {
      idx_bg <- which(tg$layout$name == "core-bg")
      if (length(idx_bg) == nrow(disp_df) * ncol(disp_df)) {
        lay <- tg$layout[idx_bg, c("t","l","b","r")]
        core_rows <- sort(unique(lay$t))
        core_cols <- sort(unique(lay$l))
        row_idx <- match(lay$t, core_rows)
        col_idx <- match(lay$l, core_cols)
        for (k in seq_along(idx_bg)) {
          col <- sh[row_idx[k], col_idx[k]]
          if (!is.na(col) && nzchar(col)) {
            if (is.null(tg$grobs[[idx_bg[k]]]$gp)) tg$grobs[[idx_bg[k]]]$gp <- grid::gpar()
            tg$grobs[[idx_bg[k]]]$gp$fill <- col
          }
        }
      }
    }
  }, error = function(e){
    message("[make_shaded_table_grob] shade apply failed: ", conditionMessage(e))
  })
  
  # Wider columns everywhere
  w <- c(widen_first, rep(other_w, nc - 1))
  tryCatch({
    if (length(tg$widths) == length(w)) tg$widths <- unit(w, "null")
    tg$heights <- unit(rep(1, length(tg$heights)), "null")
  }, error = function(e){
    message("[make_shaded_table_grob] widths set failed: ", conditionMessage(e))
  })
  
  body <- if (!is.null(header_title) && nzchar(header_title)) {
    title_g <- grid::grobTree(
      grid::rectGrob(gp = gpar(fill = TXST_MAROON, col = TXST_MAROON)),
      grid::textGrob(header_title, gp = gpar(col = TXST_GOLD, cex = 0.95, fontface = "bold"))
    )
    tryCatch(
      gridExtra::arrangeGrob(title_g, tg, ncol = 1, heights = unit.c(unit(14, "pt"), unit(1, "null"))),
      error = function(e){
        message("[make_shaded_table_grob] header add failed: ", conditionMessage(e))
        tg
      }
    )
  } else {
    tg
  }
  
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = "#C9C1B2", lwd = 0.8)),
    body
  )
}

row1_block <- function(player_name, d){
  num <- row1_table_numeric(d)
  if (is.null(num) || nrow(num) == 0 || ncol(num) == 0) {
    empty <- tibble::tibble(
      Split = c("v LHP","v RHP","Total"),
      `K%` = NA_real_, `BB%` = NA_real_, `SLUG` = NA_real_,
      `GB%` = NA_real_, `HH%` = NA_real_,
      `Whiff%` = NA_real_, `Chase%` = NA_real_
    )
    num <- empty
  }
  disp <- format_percent_display(num, exclude_cols = c("Split"), decimal_cols = c("SLUG"))
  tg <- tryCatch(
    make_shaded_table_grob(
      disp, num,
      header_title = "HITTER SPLITS (vLHP / vRHP / TOTAL)",
      widen_first = 2.6, other_w = 1.35, padding_pt = 4
    ),
    error = function(e){
      message("[row1_block] make_shaded_table_grob failed: ", conditionMessage(e))
      gridExtra::tableGrob(disp, rows = NULL, theme = gridExtra::ttheme_minimal())
    }
  )
  # Player name in the TOP-LEFT corner (half size now)
  name_g <- ggplot() + theme_void() +
    annotate("text", x=0, y=1, hjust=0, vjust=1, label=player_name,
             size=6, fontface="bold", colour=TXST_MAROON) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1), expand=FALSE)
  
  # Build as a gtable to avoid patchwork subscript errors
  name_grob <- ggplotGrob(name_g)
  tg_comb <- tryCatch(
    {
      gt <- gtable::gtable(
        widths  = grid::unit.c(grid::unit(0.22, "npc"), grid::unit(0.78, "npc")),
        heights = grid::unit(1, "npc")
      )
      gt <- gtable::gtable_add_grob(gt, name_grob, t = 1, l = 1, r = 1)
      gtable::gtable_add_grob(gt, tg,       t = 1, l = 2, r = 2)
    },
    error = function(e){
      message("[row1_block] gtable layout failed: ", conditionMessage(e))
      name_grob
    }
  )
  ggplotify::as.ggplot(function() { grid::grid.draw(tg_comb) })
}

# -------------------- Row 2: density grids (2x6) --------------------
damage_by_group_plot <- function(d){
  dd <- filter_hitter_report_pitches(d) %>%
    filter(is.finite(plate_x), is.finite(plate_z),
           between(plate_x, -3, 3), between(plate_z, 0, 5),
           is.finite(ev)) %>%
    mutate(PitchGroup = factor(pitch_group_of(as.character(PitchType)), levels = pitch_group_levels)) %>%
    filter(!is.na(PitchGroup))
  if (!nrow(dd)) return(
    ggplot() + zone_layers + coord_fixed(xlim=c(-3,3), ylim=c(0,5), expand=FALSE) +
      labs(title="Damage Heat Map") + theme_void()
  )
  if (sd(dd$plate_x, na.rm=TRUE)==0) dd$plate_x <- dd$plate_x + rnorm(nrow(dd), 0, 1e-6)
  if (sd(dd$plate_z, na.rm=TRUE)==0) dd$plate_z <- dd$plate_z + rnorm(nrow(dd), 0, 1e-6)
  
  ggplot(dd, aes(plate_x, plate_z)) +
    stat_density_2d(aes(fill = after_stat(ndensity)), geom = "raster",
                    contour = FALSE, n = 160, na.rm = TRUE) +
    scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
    facet_wrap(~ PitchGroup, ncol = 6, drop = FALSE) +
    zone_layers +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
    labs(title = "Damage Heat Map", x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 4)),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 1.4),
      strip.text = element_text(size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.6),
      panel.spacing = grid::unit(4, "pt"),
      plot.margin = margin(0, 0, 0, 0)
    )
}
whiff_by_group_plot <- function(d){
  dd <- filter_hitter_report_pitches(d) %>%
    filter(pitch_call == "StrikeSwinging",
           is.finite(plate_x), is.finite(plate_z),
           between(plate_x, -3, 3), between(plate_z, 0, 5)) %>%
    mutate(PitchGroup = factor(pitch_group_of(as.character(PitchType)), levels = pitch_group_levels)) %>%
    filter(!is.na(PitchGroup))
  if (!nrow(dd)) return(
    ggplot() + zone_layers + coord_fixed(xlim=c(-3,3), ylim=c(0,5), expand=FALSE) +
      labs(title="Whiff Heat Map") + theme_void()
  )
  if (sd(dd$plate_x, na.rm=TRUE)==0) dd$plate_x <- dd$plate_x + rnorm(nrow(dd), 0, 1e-6)
  if (sd(dd$plate_z, na.rm=TRUE)==0) dd$plate_z <- dd$plate_z + rnorm(nrow(dd), 0, 1e-6)
  
  ggplot(dd, aes(plate_x, plate_z)) +
    stat_density_2d(aes(fill = after_stat(ndensity)), geom = "raster",
                    contour = FALSE, n = 160, na.rm = TRUE) +
    scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
    facet_wrap(~ PitchGroup, ncol = 6, drop = FALSE) +
    zone_layers +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE) +
    labs(title = "Whiff Heat Map", x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 4)),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 1.4),
      strip.text = element_text(size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.6),
      panel.spacing = grid::unit(4, "pt"),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# -------------------- Row 4: tables + blank zone + spray --------------------
blank_zone_plot <- function(){
  ggplot() + zone_layers +
    coord_fixed(xlim=c(-3,3), ylim=c(0,5), expand=FALSE) +
    theme_void() + labs(title="") +
    theme(plot.margin = margin(0,0,0,0))
}

# ---- Spray chart (match HittingApp AAR exactly) ----
make_field_layers <- function(track_width_ft = 15) {
  anchor_ang <- c(-45,  -22.5,   0,   22.5,   45)
  anchor_r   <- c(330,   365,   400,   365,   330)
  ang <- seq(-45, 45, by = 0.5)
  
  wall_r <- approx(x = anchor_ang, y = anchor_r, xout = ang, rule = 2)$y
  wall <- tibble::tibble(ang = ang, r = wall_r,
                         x = r * sin(ang * pi/180), y = r * cos(ang * pi/180))
  
  wall_inner <- wall %>% dplyr::mutate(
    r = pmax(r - track_width_ft, 0),
    x = r * sin(ang * pi/180), y = r * cos(ang * pi/180)
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
  foul_lines <- dplyr::bind_rows(foul_tip(-45, 330), foul_tip(+45, 330))
  
  half <- 90 / sqrt(2); sec <- 90 * sqrt(2)
  bases_diamond <- tibble::tibble(
    x = c(0,  half,   0, -half,   0),
    y = c(0,  half,  sec,  half,   0)
  )
  
  make_ring <- function(r) {
    tibble::tibble(
      r = r,
      ang = ang,
      x = r * sin(ang * pi/180),
      y = r * cos(ang * pi/180)
    )
  }
  rings <- dplyr::bind_rows(lapply(seq(200, 450, by = 50), make_ring))
  
  list(
    wall = wall, wall_inner = wall_inner, track_poly = track_poly,
    foul_lines = foul_lines, bases_diamond = bases_diamond, rings = rings
  )
}

build_spray_grob <- function(d, show_numbers = FALSE){
  d <- filter_hitter_report_pitches(d)
  
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    fld <- make_field_layers()
    p0 <- ggplot() +
      geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
      geom_path(   data=fld$wall,        aes(x=x,y=y), color="#501214", linewidth=1.1) +
      geom_segment(data=fld$foul_lines,  aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
      geom_path(   data=fld$rings,       aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
      geom_path(   data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
      coord_fixed(xlim=c(-230,230), ylim=c(0,420), expand=FALSE) +
      theme_minimal(base_size=11) +
      theme(panel.grid=element_blank(), panel.border=element_rect(color="black", fill=NA))
    return(ggplotGrob(p0))
  }
  
  n0 <- nrow(d)
  if (!("PitchNum" %in% names(d))) d$PitchNum <- seq_len(n0)
  
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
    dplyr::mutate(hard_hit_flag = bip_flag %in% TRUE & is.finite(ev) & ev >= 95) %>%
    dplyr::filter(hard_hit_flag %in% TRUE, is.finite(plot_x), is.finite(plot_y))
  
  m <- nrow(spray)
  la_vec <- if ("LA" %in% names(spray)) to_num(spray$LA) else
    if ("la" %in% names(spray)) to_num(spray$la) else
      if ("LaunchAngle" %in% names(spray)) to_num(spray$LaunchAngle) else
        rep(NA_real_, m)
  
  spray$BBType <- dplyr::case_when(
    is.finite(la_vec) & la_vec >= 50 ~ "Pop Up",
    is.finite(la_vec) & la_vec >= 25 ~ "Fly Ball",
    is.finite(la_vec) & la_vec >= 10 ~ "Line Drive",
    is.finite(la_vec)                ~ "Ground Ball",
    TRUE                             ~ NA_character_
  )
  spray$BBType <- factor(spray$BBType, levels=c("Pop Up","Fly Ball","Line Drive","Ground Ball"))
  spray <- spray %>% dplyr::filter(!is.na(BBType)) %>% dplyr::distinct(PitchNum, .keep_all = TRUE)
  
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
  
  spray$ResultBucket <- dplyr::case_when(
    grepl("(?i)home\\s*run|\\bHR\\b", nz_chr(pr_vec)) ~ "HR",
    grepl("(?i)\\b3B\\b|triple",       nz_chr(pr_vec)) ~ "3B",
    grepl("(?i)\\b2B\\b|double",       nz_chr(pr_vec)) ~ "2B",
    grepl("(?i)\\b1B\\b|single",       nz_chr(pr_vec)) ~ "1B",
    TRUE                                              ~ "Out/Error"
  )
  spray$ResultBucket <- factor(spray$ResultBucket, levels=c("Out/Error","1B","2B","3B","HR"))
  
  fld <- make_field_layers()
  
  base_field <- ggplot() +
    geom_polygon(data=fld$track_poly, aes(x=x,y=y), fill="white", color=NA) +
    geom_path(   data=fld$wall,        aes(x=x,y=y), color="#501214", linewidth=1.1) +
    geom_segment(data=fld$foul_lines,  aes(x=x,y=y,xend=xend,yend=yend), color="#501214") +
    geom_path(   data=fld$rings,       aes(x=x,y=y, group=r), linetype="dashed", color="grey40", linewidth=.5) +
    geom_path(   data=fld$bases_diamond, aes(x=x,y=y), color="black", linewidth=.9) +
    coord_fixed(xlim=c(-230,230), ylim=c(0,420), expand=FALSE) +
    theme_minimal(base_size=11) +
    theme(
      panel.grid=element_blank(),
      panel.border=element_rect(color="black", fill=NA),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  contact_legend <- data.frame(
    x=-999, y=-999,
    BBType=factor(c("Pop Up","Fly Ball","Line Drive","Ground Ball"),
                  levels=c("Pop Up","Fly Ball","Line Drive","Ground Ball"))
  )
  result_legend <- data.frame(
    x=-999, y=-999,
    ResultBucket=factor(c("Out/Error","1B","2B","3B","HR"),
                        levels=c("Out/Error","1B","2B","3B","HR"))
  )
  
  p <- base_field +
    geom_point(
      data = contact_legend,
      aes(x = x, y = y, shape = BBType),
      inherit.aes = FALSE, fill = "white", color = "black",
      stroke = 0.8, size = 5.5, show.legend = TRUE
    ) +
    geom_point(
      data = result_legend,
      aes(x = x, y = y, fill = ResultBucket),
      inherit.aes = FALSE, shape = 21, color = "black",
      stroke = 0.6, size = 5.5, show.legend = TRUE
    )
  
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
      values=c("Pop Up"=22,"Fly Ball"=21,"Line Drive"=24,"Ground Ball"=23),
      breaks=c("Pop Up","Fly Ball","Line Drive","Ground Ball"),
      drop=FALSE
    ) +
    scale_fill_manual(
      name="Result",
      values=c("Out/Error"="#1E88E5","1B"="#2E7D32","2B"="#FDD835","3B"="#FB8C00","HR"="#E53935"),
      breaks=c("Out/Error","1B","2B","3B","HR"),
      labels=c("Out/Error","Single","Double","Triple","Home Run"),
      drop=FALSE
    ) +
    guides(
      shape = guide_legend(
        title="Contact type", order=1,
        override.aes=list(fill = NA, color = "black", stroke = 0.8, alpha = 1, shape = c(22, 21, 24, 23))
      ),
      fill  = guide_legend(
        title="Result", order=2,
        override.aes=list(shape = 21, size = 5.5, color = "black", stroke = 0.6, alpha = 1)
      )
    ) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.key = element_blank()
    )
  
  ggplotGrob(p)
}

plot_spray <- function(d){
  patchwork::wrap_elements(full = build_spray_grob(d, show_numbers = FALSE))
}

row4_vspt_table_numeric <- function(d){
  d <- filter_hitter_report_pitches(d)
  if (!nrow(d)) {
    return(tibble::tibble(
      `Pitch Group` = character(),
      `Whiff%` = numeric(),
      `Chase%` = numeric(),
      `Pre2kC%` = numeric(),
      `2kC%` = numeric(),
      `Barrel%` = numeric()
    ))
  }
  balls_col   <- pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d)
  strikes_col <- pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  
  d$PitchGroup <- factor(pitch_group_of(as.character(d$PitchType)), levels = pitch_group_levels)
  d$PA_ID <- make_pa_id(d)
  pa_last <- pa_last_table(d, balls_col, strikes_col) %>%
    mutate(
      PitchGroup = factor(pitch_group_of(as.character(PitchType)), levels = pitch_group_levels),
      bip_flag = is_bip_for_barrel(pc),
      barrel_flag = is_barrel_strict(pc, ev_pa, la_pa)
    )
  
  d_sw <- swing_table(d, balls_col, strikes_col) %>%
    mutate(PitchGroup = factor(PitchGroup, levels = pitch_group_levels))
  
  swings_by_pg <- d_sw %>%
    filter(!is.na(PitchGroup)) %>%
    group_by(PitchGroup) %>%
    summarise(
      swings      = sum(is_swing,   na.rm = TRUE),
      whiffs      = sum(is_whiff,   na.rm = TRUE),
      o_zone_seen = sum(in_zone %in% FALSE, na.rm = TRUE),
      chases      = sum(is_swing & (in_zone %in% FALSE), na.rm = TRUE),
      pre2k_den   = sum((in_zone %in% FALSE) & pre2k, na.rm = TRUE),
      pre2k_num   = sum(is_swing & (in_zone %in% FALSE) & pre2k, na.rm = TRUE),
      two_k_den   = sum((in_zone %in% FALSE) & two_k,  na.rm = TRUE),
      two_k_num   = sum(is_swing & (in_zone %in% FALSE) & two_k,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      `Pitch Group`   = PitchGroup,
      `Whiff%`        = safe_ratio(whiffs, swings),
      `Chase%`        = safe_ratio(chases, o_zone_seen),
      `Pre2kC%`       = safe_ratio(pre2k_num, pre2k_den),
      `2kC%`          = safe_ratio(two_k_num, two_k_den)
    )
  
  hard_by_pg <- pa_last %>%
    filter(!is.na(PitchGroup)) %>%
    group_by(PitchGroup) %>%
    summarise(`Barrel%` = safe_ratio(sum(barrel_flag, na.rm = TRUE), sum(bip_flag, na.rm = TRUE)), .groups="drop") %>%
    rename(`Pitch Group` = PitchGroup)
  
  vpt <- dplyr::left_join(swings_by_pg, hard_by_pg, by = "Pitch Group") %>%
    dplyr::arrange(factor(`Pitch Group`, levels = pitch_group_levels)) %>%
    dplyr::select(`Pitch Group`, `Whiff%`, `Chase%`, `Pre2kC%`, `2kC%`, `Barrel%`)
  vpt
}

row4_vspt_table_grob <- function(d){
  num <- row4_vspt_table_numeric(d)
  disp <- num %>% mutate(across(-`Pitch Group`, ~ scales::percent(.x, accuracy = 0.1)))
  make_shaded_table_grob(
    disp, num,
    header_title = "VS PITCH TYPE",
    widen_first = 2.6, other_w = 1.35, padding_pt = 2
  )
}

# -------------------- Assemble full card --------------------
build_card <- function(d_player, player_name){
  d_player <- filter_hitter_report_pitches(d_player)
  safe_block <- function(label, expr){
    tryCatch(expr, error = function(e){
      message("[build_card] ", label, " failed: ", conditionMessage(e))
      stop(e)
    })
  }
  r1  <- safe_block("row1_block", row1_block(player_name, d_player))
  dmg <- safe_block("damage_by_group_plot", damage_by_group_plot(d_player))
  whf <- safe_block("whiff_by_group_plot", whiff_by_group_plot(d_player))
  top_block <- r1
  mid_block <- dmg / whf + plot_layout(heights = c(1,1))
  
  # Left table: more space (≈ 2.5 strike-zone panels)
  left  <- safe_block("row4_vspt_table_grob", ggplot() +
    xlim(0,1) + ylim(0,1) +
    annotation_custom(grob = row4_vspt_table_grob(d_player),
                      xmin = 0.03, xmax = 0.97, ymin = 0.03, ymax = 0.97) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.margin = margin(6, 10, 6, 12))
  )
  
  mid   <- safe_block("blank_zone_plot", blank_zone_plot())
  right <- safe_block("plot_spray", plot_spray(d_player))
  
  bottom_block <- left | mid | right
  bottom_block <- bottom_block + plot_layout(widths = c(1.75, 1, 1.25))
  
  top_block / mid_block / bottom_block + plot_layout(heights = c(0.50, 1.60, 1.15)) &
    theme(plot.margin=margin(5,5,5,5))
}

# -------------------- All-hitter pitch type tables --------------------
scout_pitch_rows <- tibble::tibble(
  PitchBucket = factor(c("FB","SNK","CT","SL","SW","CB","CH/SP"),
                       levels = c("FB","SNK","CT","SL","SW","CB","CH/SP")),
  PitchTypes = list(
    "Fastball",
    "Sinker",
    "Cutter",
    "Slider",
    "Sweeper",
    "Curveball",
    c("Changeup","Splitter")
  )
)

scout_pitch_bucket <- function(pt_chr){
  dplyr::case_when(
    pt_chr %in% c("Fastball") ~ "FB",
    pt_chr %in% c("Sinker") ~ "SNK",
    pt_chr %in% c("Cutter") ~ "CT",
    pt_chr %in% c("Slider") ~ "SL",
    pt_chr %in% c("Sweeper") ~ "SW",
    pt_chr %in% c("Curveball") ~ "CB",
    pt_chr %in% c("Changeup","Splitter") ~ "CH/SP",
    TRUE ~ NA_character_
  )
}

pitch_type_display_levels <- function(combine_sl_sw = FALSE){
  lvls <- levels(scout_pitch_rows$PitchBucket)
  if (isTRUE(combine_sl_sw)) setdiff(lvls, "SW") else lvls
}

normalize_pitch_type_bucket <- function(bucket, combine_sl_sw = FALSE){
  bucket_chr <- as.character(bucket)
  if (isTRUE(combine_sl_sw)) bucket_chr[bucket_chr == "SW"] <- "SL"
  factor(bucket_chr, levels = pitch_type_display_levels(combine_sl_sw))
}

scout_heat_group <- function(pt_chr){
  dplyr::case_when(
    pt_chr %in% c("Fastball") ~ "Fastball",
    pt_chr %in% c("Sinker") ~ "Sinker",
    pt_chr %in% c("Cutter","Slider","Sweeper","Curveball") ~ "Spin",
    pt_chr %in% c("Changeup","Splitter") ~ "Soft",
    TRUE ~ NA_character_
  )
}

pitch_color_values <- c(
  "Fastball" = "#FF0000",
  "Sinker" = "#FFA500",
  "Cutter" = "#000000",
  "Slider" = "#FFFF00",
  "Sweeper" = "#FFD700",
  "Curveball" = "#0000FF",
  "Changeup" = "#008000",
  "Splitter" = "#000080",
  "Undefined" = "#808080",
  "Untagged" = "#808080"
)

filter_by_pitcher_hand <- function(d, hand_filter){
  hand_filter <- hand_filter %||% "Total"
  if (identical(hand_filter, "LHP")) return(d %>% dplyr::filter(PitcherHand == "LHP"))
  if (identical(hand_filter, "RHP")) return(d %>% dplyr::filter(PitcherHand == "RHP"))
  d
}

hand_label <- function(hand_filter){
  hand_filter <- hand_filter %||% "Total"
  if (hand_filter == "LHP") "v LHP" else if (hand_filter == "RHP") "v RHP" else "Total"
}

count_cols <- function(d) {
  list(
    balls = pick_first(c("Balls","BallsBeforePitch","BallCount","BallsCount","PitcherBalls"), d),
    strikes = pick_first(c("Strikes","StrikesBeforePitch","StrikeCount","StrikesCount","PitcherStrikes"), d)
  )
}

pa_rbi_values <- function(pa, allow_runs_scored_as_rbi = FALSE){
  n <- nrow(pa)
  rbi_col <- pick_first(c("RBI","RBIs","RunsBattedIn","Runs_Batted_In","RBIResult",
                          "RBI_Result","RBI|PIT","RBI_PIT","RBI (PIT)"), pa)
  if (!is.na(rbi_col)) {
    rbi <- to_num(pa[[rbi_col]])
  } else if (isTRUE(allow_runs_scored_as_rbi) && "RunsScored" %in% names(pa)) {
    # TrackMan RunsScored is team runs on the play, not always batter RBI; keep this opt-in.
    rbi <- to_num(pa$RunsScored)
  } else {
    rbi <- rep(0, n)
  }
  rbi[!is.finite(rbi)] <- 0
  rbi
}

pa_outcome_summary <- function(pa, allow_runs_scored_as_rbi = FALSE){
  n <- nrow(pa)
  if (!n) {
    return(tibble::tibble(AB = integer(), H = integer(), TB = integer(), BB = integer(),
                          HBP = integer(), SF = integer(), K = integer(), RBI = numeric(), HR = integer()))
  }
  play <- tolower(nz_chr(pa$play_result))
  korbb <- tolower(nz_chr(if ("KorBB" %in% names(pa)) pa$KorBB else ""))
  pc <- nz_chr(if ("pitch_call" %in% names(pa)) pa$pitch_call else "")
  one_b <- grepl("\\bsingle\\b|\\b1b\\b", play, perl = TRUE)
  two_b <- grepl("\\bdouble\\b|\\b2b\\b", play, perl = TRUE) & !grepl("double\\s*play", play, perl = TRUE)
  three_b <- grepl("\\btriple\\b|\\b3b\\b", play, perl = TRUE)
  hr <- grepl("home\\s*run|homerun|\\bhr\\b", play, perl = TRUE)
  h <- one_b | two_b | three_b | hr
  tb <- ifelse(one_b, 1, 0) + ifelse(two_b, 2, 0) + ifelse(three_b, 3, 0) + ifelse(hr, 4, 0)
  bb <- (grepl("\\bwalk\\b|\\bbb\\b", korbb, perl = TRUE) | grepl("\\bwalk\\b", play, perl = TRUE)) &
    !grepl("intentional|\\bibb\\b", korbb, perl = TRUE) &
    !grepl("intentional|\\bibb\\b", play, perl = TRUE)
  hbp <- grepl("hit\\s*by\\s*pitch|\\bhbp\\b", play, perl = TRUE)
  k <- grepl("strike.?out|\\bk\\b", korbb, perl = TRUE) | grepl("strike.?out", play, perl = TRUE)
  sf <- grepl("sacrifice\\s*fly|sac\\s*fly|\\bsf\\b", play, perl = TRUE)
  sac <- sf | grepl("sacrifice|sac\\s*bunt", play, perl = TRUE)
  ci <- grepl("interference", play, perl = TRUE)
  ball_in_play_out <- pc %in% c("InPlayOut") |
    grepl("out|error|fielder|fielders\\s*choice|fielderschoice|force", play, perl = TRUE)
  has_result <- h | bb | hbp | k | sac | ball_in_play_out
  ab <- has_result & !bb & !hbp & !sac & !ci
  rbi <- pa_rbi_values(pa, allow_runs_scored_as_rbi)
  tibble::tibble(
    AB = as.integer(ab),
    H = as.integer(h),
    TB = as.integer(tb),
    BB = as.integer(bb),
    HBP = as.integer(hbp),
    SF = as.integer(sf),
    K = as.integer(k),
    RBI = rbi,
    HR = as.integer(hr)
  )
}

summarise_pa_results <- function(pa, group_vars){
  if (!nrow(pa)) {
    return(tibble::tibble())
  }
  out <- pa_outcome_summary(pa)
  dplyr::bind_cols(pa, out) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(c(AB,H,TB,BB,HBP,SF,K,RBI,HR), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

summarise_selected_pitch_results <- function(d, group_vars, balls_col, strikes_col, two_k_only = FALSE){
  if (!nrow(d)) return(tibble::tibble())
  d2 <- d %>%
    dplyr::mutate(
      balls_b4 = if (!is.na(balls_col)) to_num(.data[[balls_col]]) else NA_real_,
      strikes_b4 = if (!is.na(strikes_col)) to_num(.data[[strikes_col]]) else NA_real_
    )
  d2$PA_ID <- make_pa_id(d2)
  if (isTRUE(two_k_only)) {
    d2 <- d2 %>% dplyr::filter(is.finite(strikes_b4), strikes_b4 >= 2)
  }
  if (!nrow(d2)) return(tibble::tibble())
  selected_last <- d2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "PA_ID")))) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
  summarise_pa_results(selected_last, group_vars)
}

ensure_pa_summary_cols <- function(tbl, group_vars){
  need <- c(group_vars, "AB","H","TB","BB","HBP","SF","K","RBI","HR")
  for (nm in need) {
    if (!(nm %in% names(tbl))) {
      tbl[[nm]] <- if (nm %in% group_vars) character() else numeric()
    }
  }
  tbl %>% dplyr::select(dplyr::all_of(need))
}

rv_formula <- function(tb, bb, k, rbi, hr, pitches){
  ifelse(pitches > 0, (((tb + bb - k) / 4) + rbi + hr) / pitches * 100, NA_real_)
}

hitter_order_from_data <- function(d, mode = c("game", "pa")){
  mode <- match.arg(mode)
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) return(character(0))
  if (mode == "game") {
    return(unique(na.omit(as.character(d[[bat_col]]))))
  }
  hitter_report_meta(d, "Total")$Hitter
}

apply_player_order <- function(players, player_order = NULL){
  players <- unique(na.omit(as.character(players)))
  if (is.null(player_order)) return(players)
  if (!length(player_order)) return(character(0))
  player_order <- unique(na.omit(as.character(player_order)))
  intersect(player_order, players)
}

hitter_report_meta <- function(d, hand_filter = "Total", player_order = NULL){
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) return(tibble::tibble(Hitter = character(), PA = integer(), Side = character()))
  all_players <- sort(unique(na.omit(as.character(d[[bat_col]]))))
  players <- if (is.null(player_order)) all_players else apply_player_order(all_players, player_order)
  side_tbl <- d %>%
    dplyr::mutate(Hitter = as.character(.data[[bat_col]]), Side = as.character(BatterSide)) %>%
    dplyr::filter(!is.na(Hitter), !is.na(Side), Side %in% c("L","R")) %>%
    dplyr::count(Hitter, Side, name = "n") %>%
    dplyr::arrange(Hitter, dplyr::desc(n)) %>%
    dplyr::group_by(Hitter) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Hitter, Side)
  
  ds <- filter_by_pitcher_hand(d, hand_filter)
  cc <- count_cols(ds)
  pa_tbl <- if (nrow(ds)) {
    pa_last_table(ds, cc$balls, cc$strikes) %>%
      dplyr::mutate(Hitter = as.character(.data[[bat_col]])) %>%
      dplyr::filter(!is.na(Hitter)) %>%
      dplyr::count(Hitter, name = "PA")
  } else {
    tibble::tibble(Hitter = character(), PA = integer())
  }
  
  tibble::tibble(Hitter = players) %>%
    dplyr::left_join(pa_tbl, by = "Hitter") %>%
    dplyr::left_join(side_tbl, by = "Hitter") %>%
    dplyr::mutate(PA = tidyr::replace_na(PA, 0L), Side = tidyr::replace_na(Side, "R")) %>%
    {
      if (is.null(player_order)) {
        dplyr::arrange(., dplyr::desc(PA), Hitter)
      } else {
        ord <- players
        dplyr::arrange(dplyr::mutate(., .order = match(Hitter, ord)), .order) %>%
          dplyr::select(-.order)
      }
    }
}

player_name_color <- function(side){
  side <- toupper(as.character(side %||% "R"))
  ifelse(side == "L", "#C00000", "#111111")
}

build_pitch_type_table_numeric <- function(d, hand_filter = "Total", player_order = NULL, combine_sl_sw = FALSE){
  d <- filter_hitter_report_pitches(d)
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) return(tibble::tibble())
  meta <- hitter_report_meta(d, hand_filter, player_order)
  players <- meta$Hitter
  base <- tidyr::expand_grid(
    Hitter = players,
    PitchBucket = pitch_type_display_levels(combine_sl_sw)
  ) %>%
    dplyr::mutate(PitchBucket = factor(PitchBucket, levels = pitch_type_display_levels(combine_sl_sw)))
  if (!length(players)) return(base)
  
  ds <- filter_by_pitcher_hand(d, hand_filter) %>%
    dplyr::mutate(
      Hitter = as.character(.data[[bat_col]]),
      PitchBucket = normalize_pitch_type_bucket(scout_pitch_bucket(as.character(PitchType)), combine_sl_sw)
    ) %>%
    dplyr::filter(!is.na(Hitter), !is.na(PitchBucket))
  
  if (!nrow(ds)) {
    return(base %>% dplyr::mutate(P = 0L, `FPS%` = NA_real_, RV = NA_real_,
                                  Avg = NA_real_, SLG = NA_real_, `Whiff%` = NA_real_, `Chase%` = NA_real_,
                                  `2k RV` = NA_real_, `2k Whiff` = NA_real_, `2k Chase` = NA_real_,
                                  FP_swing = 0, FP_total = 0,
                                  swings = 0, whiffs = 0, chases = 0, o_zone_seen = 0,
                                  swings_2k = 0, whiffs_2k = 0,
                                  AB = 0, H = 0, TB = 0, BB = 0, HBP = 0, SF = 0, K = 0, RBI = 0, HR = 0,
                                  P2K = 0, AB_2k = 0, H_2k = 0, TB_2k = 0, BB_2k = 0, HBP_2k = 0,
                                  SF_2k = 0, K_2k = 0, RBI_2k = 0, HR_2k = 0,
                                  chase_2k_num = 0, chase_2k_den = 0) %>%
             dplyr::left_join(meta, by = "Hitter"))
  }
  
  cc <- count_cols(ds)
  p_counts <- ds %>%
    dplyr::count(Hitter, PitchBucket, name = "P")
  
  fp <- ds[compute_is_first_pitch(ds) %in% TRUE, , drop = FALSE]
  fps <- fp %>%
    dplyr::mutate(is_fp_swing = pitch_call %in% c("StrikeSwinging","InPlay","InPlayOut","InPlayNoOut",
                                                  "FoulBall","FoulBallFieldable","FoulBallNotFieldable","FoulTip")) %>%
    dplyr::group_by(Hitter, PitchBucket) %>%
    dplyr::summarise(
      FP_swing = sum(is_fp_swing, na.rm = TRUE),
      FP_total = dplyr::n(),
      `FPS%` = safe_ratio(FP_swing, FP_total),
      .groups = "drop"
    )
  
  ds_sw <- swing_table(ds, cc$balls, cc$strikes)
  whiff_by_pitch <- ds_sw %>%
    dplyr::group_by(Hitter, PitchBucket) %>%
    dplyr::summarise(
      swings = sum(is_swing, na.rm = TRUE),
      whiffs = sum(is_whiff, na.rm = TRUE),
      chases = sum(is_swing & (in_zone %in% FALSE), na.rm = TRUE),
      o_zone_seen = sum(in_zone %in% FALSE, na.rm = TRUE),
      `Whiff%` = safe_ratio(whiffs, swings),
      `Chase%` = safe_ratio(chases, o_zone_seen),
      .groups = "drop"
    )
  two_k_p <- ds_sw %>%
    dplyr::filter(two_k %in% TRUE) %>%
    dplyr::count(Hitter, PitchBucket, name = "P2K")
  two_k_whiff <- ds_sw %>%
    dplyr::filter(two_k %in% TRUE) %>%
    dplyr::group_by(Hitter, PitchBucket) %>%
    dplyr::summarise(
      swings_2k = sum(is_swing, na.rm = TRUE),
      whiffs_2k = sum(is_whiff, na.rm = TRUE),
      `2k Whiff` = safe_ratio(whiffs_2k, swings_2k),
      .groups = "drop"
    )
  two_k_chase <- ds_sw %>%
    dplyr::filter(two_k %in% TRUE) %>%
    dplyr::group_by(Hitter, PitchBucket) %>%
    dplyr::summarise(
      chase_2k_num = sum(is_swing & (in_zone %in% FALSE), na.rm = TRUE),
      chase_2k_den = sum(in_zone %in% FALSE, na.rm = TRUE),
      `2k Chase` = safe_ratio(chase_2k_num, chase_2k_den),
      .groups = "drop"
    )
  
  pa_sum <- summarise_selected_pitch_results(ds, c("Hitter","PitchBucket"), cc$balls, cc$strikes) %>%
    ensure_pa_summary_cols(c("Hitter","PitchBucket"))
  pa_2k_sum <- summarise_selected_pitch_results(ds, c("Hitter","PitchBucket"), cc$balls, cc$strikes, two_k_only = TRUE) %>%
    ensure_pa_summary_cols(c("Hitter","PitchBucket"))
  
  base %>%
    dplyr::left_join(p_counts, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(fps, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(whiff_by_pitch, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(pa_sum, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(two_k_p, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(two_k_whiff, by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(pa_2k_sum %>% dplyr::rename_with(~paste0(.x, "_2k"), c(AB,H,TB,BB,HBP,SF,K,RBI,HR)),
                     by = c("Hitter","PitchBucket")) %>%
    dplyr::left_join(two_k_chase, by = c("Hitter","PitchBucket")) %>%
    dplyr::mutate(
      P = tidyr::replace_na(P, 0L),
      dplyr::across(c(FP_swing, FP_total, swings, whiffs, chases, o_zone_seen, swings_2k, whiffs_2k, AB,H,TB,BB,HBP,SF,K,RBI,HR,P2K,
                      AB_2k,H_2k,TB_2k,BB_2k,HBP_2k,SF_2k,K_2k,RBI_2k,HR_2k),
                    ~tidyr::replace_na(.x, 0)),
      chase_2k_num = tidyr::replace_na(chase_2k_num, 0),
      chase_2k_den = tidyr::replace_na(chase_2k_den, 0),
      RV = rv_formula(TB, BB, K, RBI, HR, P),
      Avg = safe_ratio(H, AB),
      SLG = safe_ratio(TB, AB),
      `Whiff%` = safe_ratio(whiffs, swings),
      `Chase%` = safe_ratio(chases, o_zone_seen),
      `2k RV` = rv_formula(TB_2k, BB_2k, K_2k, RBI_2k, HR_2k, P2K),
      `2k Whiff` = safe_ratio(whiffs_2k, swings_2k)
    ) %>%
    dplyr::left_join(meta, by = "Hitter") %>%
    dplyr::mutate(.order = match(Hitter, meta$Hitter)) %>%
    dplyr::arrange(.order, PitchBucket) %>%
    dplyr::select(Hitter, Side, PA, PitchBucket, P, `FPS%`, RV, Avg, SLG, `Whiff%`, `Chase%`, `2k RV`, `2k Whiff`, `2k Chase`,
                  FP_swing, FP_total, swings, whiffs, chases, o_zone_seen, swings_2k, whiffs_2k, AB, H, TB, BB, HBP, SF, K, RBI, HR, P2K,
                  AB_2k, H_2k, TB_2k, BB_2k, HBP_2k, SF_2k, K_2k, RBI_2k, HR_2k,
                  chase_2k_num, chase_2k_den)
}

metric_badness <- function(v, lower_is_pitcher_good = TRUE){
  ok <- is.finite(v)
  out <- rep(NA_real_, length(v))
  if (sum(ok) < 2 || length(unique(v[ok])) < 2) return(out)
  rng <- range(v[ok], na.rm = TRUE)
  scaled <- (v - rng[1]) / diff(rng)
  if (isTRUE(lower_is_pitcher_good)) scaled else 1 - scaled
}

metric_fill_colors <- function(v, lower_is_pitcher_good = TRUE, neutral = FALSE){
  if (isTRUE(neutral)) return(rep("#F6F3EC", length(v)))
  bad <- metric_badness(v, lower_is_pitcher_good)
  pal <- grDevices::colorRamp(c(SHADE_GREEN, "#FFFFFF", SHADE_RED), space = "rgb")
  out <- rep("#F6F3EC", length(v))
  ok <- is.finite(bad)
  out[ok] <- grDevices::rgb(pal(pmin(pmax(bad[ok], 0), 1)) / 255)
  out
}

PITCH_TABLE_D1 <- tibble::tribble(
  ~Metric, ~Ref,  ~Scale, ~LowerGood,
  "FPS%",     0.31, 0.18, TRUE,
  "RV",       0.00, 18.0, TRUE,
  "Avg",      0.260, 0.120, TRUE,
  "SLG",      0.400, 0.220, TRUE,
  "Whiff%",   0.229, 0.120, FALSE,
  "Chase%",   0.242, 0.120, FALSE,
  "2k RV",    0.00, 18.0, TRUE,
  "2k Whiff", 0.229, 0.120, FALSE,
  "2k Chase", 0.363, 0.200, FALSE
)

pitch_metric_fill_colors <- function(v, metric){
  out <- rep("#FFFFFF", length(v))
  if (metric == "P") return(rep("#F6F3EC", length(v)))
  ref_row <- PITCH_TABLE_D1 %>% dplyr::filter(.data$Metric == metric)
  if (!nrow(ref_row)) return(out)
  ref <- ref_row$Ref[[1]]
  scale <- ref_row$Scale[[1]]
  lower_good <- ref_row$LowerGood[[1]]
  diff <- (v - ref) / scale
  badness <- if (isTRUE(lower_good)) diff else -diff
  badness <- pmin(pmax(badness, -1), 1)
  pal <- grDevices::colorRamp(c("#3FA34D", "#FFFFFF", "#E15759"), space = "rgb")
  ok <- is.finite(badness)
  out[ok] <- grDevices::rgb(pal((badness[ok] + 1) / 2) / 255)
  out
}

format_pitch_table_value <- function(metric, value){
  if (!is.finite(value)) return("")
  if (metric == "P") return(scales::comma(value, accuracy = 1))
  if (metric %in% c("FPS%","Whiff%","Chase%","2k Whiff","2k Chase")) return(scales::percent(value, accuracy = 1))
  if (metric %in% c("RV","2k RV")) return(scales::number(value, accuracy = 0.1))
  scales::number(value, accuracy = 0.001)
}

single_pitch_type_table_grob <- function(player_name, player_num, all_num, combine_sl_sw = FALSE){
  metric_levels <- c("P","FPS%","RV","Avg","SLG","Whiff%","Chase%","2k RV","2k Whiff","2k Chase")
  total_row <- player_num %>%
    dplyr::summarise(
      Hitter = dplyr::first(Hitter),
      Side = dplyr::first(Side),
      PA = dplyr::first(PA),
      PitchBucket = factor("Total", levels = c(pitch_type_display_levels(combine_sl_sw), "Total")),
      P = sum(P, na.rm = TRUE),
      `FPS%` = safe_ratio(sum(FP_swing, na.rm = TRUE), sum(FP_total, na.rm = TRUE)),
      RV = rv_formula(sum(TB, na.rm = TRUE), sum(BB, na.rm = TRUE), sum(K, na.rm = TRUE),
                      sum(RBI, na.rm = TRUE), sum(HR, na.rm = TRUE), sum(P, na.rm = TRUE)),
      Avg = safe_ratio(sum(H, na.rm = TRUE), sum(AB, na.rm = TRUE)),
      SLG = safe_ratio(sum(TB, na.rm = TRUE), sum(AB, na.rm = TRUE)),
      `Whiff%` = safe_ratio(sum(whiffs, na.rm = TRUE), sum(swings, na.rm = TRUE)),
      `Chase%` = safe_ratio(sum(chases, na.rm = TRUE), sum(o_zone_seen, na.rm = TRUE)),
      `2k RV` = rv_formula(sum(TB_2k, na.rm = TRUE), sum(BB_2k, na.rm = TRUE), sum(K_2k, na.rm = TRUE),
                           sum(RBI_2k, na.rm = TRUE), sum(HR_2k, na.rm = TRUE), sum(P2K, na.rm = TRUE)),
      `2k Whiff` = safe_ratio(sum(whiffs_2k, na.rm = TRUE), sum(swings_2k, na.rm = TRUE)),
      `2k Chase` = safe_ratio(sum(chase_2k_num, na.rm = TRUE), sum(chase_2k_den, na.rm = TRUE)),
      FP_swing = sum(FP_swing, na.rm = TRUE),
      FP_total = sum(FP_total, na.rm = TRUE),
      swings = sum(swings, na.rm = TRUE),
      whiffs = sum(whiffs, na.rm = TRUE),
      chases = sum(chases, na.rm = TRUE),
      o_zone_seen = sum(o_zone_seen, na.rm = TRUE),
      swings_2k = sum(swings_2k, na.rm = TRUE),
      whiffs_2k = sum(whiffs_2k, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      TB = sum(TB, na.rm = TRUE),
      BB = sum(BB, na.rm = TRUE),
      HBP = sum(HBP, na.rm = TRUE),
      SF = sum(SF, na.rm = TRUE),
      K = sum(K, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      P2K = sum(P2K, na.rm = TRUE),
      AB_2k = sum(AB_2k, na.rm = TRUE),
      H_2k = sum(H_2k, na.rm = TRUE),
      TB_2k = sum(TB_2k, na.rm = TRUE),
      BB_2k = sum(BB_2k, na.rm = TRUE),
      HBP_2k = sum(HBP_2k, na.rm = TRUE),
      SF_2k = sum(SF_2k, na.rm = TRUE),
      K_2k = sum(K_2k, na.rm = TRUE),
      RBI_2k = sum(RBI_2k, na.rm = TRUE),
      HR_2k = sum(HR_2k, na.rm = TRUE),
      chase_2k_num = sum(chase_2k_num, na.rm = TRUE),
      chase_2k_den = sum(chase_2k_den, na.rm = TRUE),
      .groups = "drop"
    )
  player_num <- dplyr::bind_rows(player_num, total_row)
  disp <- player_num %>%
    dplyr::mutate(Pitch = as.character(PitchBucket)) %>%
    dplyr::select(Pitch, dplyr::all_of(metric_levels))
  for (nm in metric_levels) {
    disp[[nm]] <- vapply(disp[[nm]], function(x) format_pitch_table_value(nm, x), character(1))
  }
  disp <- as.data.frame(disp, stringsAsFactors = FALSE)
  
  fills <- matrix("#FFFFFF", nrow = nrow(disp), ncol = ncol(disp), dimnames = list(NULL, names(disp)))
  fills[, "Pitch"] <- "#F6F3EC"
  for (nm in metric_levels) {
    player_vals <- player_num[[nm]]
    fills[, nm] <- pitch_metric_fill_colors(player_vals, nm)
    fills[!is.finite(player_vals), nm] <- "#FFFFFF"
  }
  
  th <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 3.35, fontface = "bold"),
      bg_params = list(fill = "#FFFFFF", col = "#D7D0C6", lwd = 0.35),
      padding = grid::unit(c(1.8, 0.55), "pt")
    ),
    colhead = list(
      fg_params = list(fontsize = 3.45, fontface = "bold", col = TXST_GOLD),
      bg_params = list(fill = TXST_MAROON, col = TXST_MAROON, lwd = 0.45),
      padding = grid::unit(c(1.8, 0.55), "pt")
    )
  )
  tg <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  tryCatch({
    idx_bg <- which(tg$layout$name == "core-bg")
    lay <- tg$layout[idx_bg, c("t","l")]
    core_rows <- sort(unique(lay$t))
    core_cols <- sort(unique(lay$l))
    row_idx <- match(lay$t, core_rows)
    col_idx <- match(lay$l, core_cols)
    for (k in seq_along(idx_bg)) {
      fill <- fills[row_idx[k], col_idx[k]]
      tg$grobs[[idx_bg[k]]]$gp$fill <- fill
    }
    tg$widths <- grid::unit(c(0.78, rep(0.82, length(metric_levels))), "null")
    tg$heights <- grid::unit(rep(1, length(tg$heights)), "null")
  }, error = function(e) NULL)
  
  side_vals <- player_num$Side[!is.na(player_num$Side)]
  side <- if (length(side_vals)) side_vals[[1]] else "R"
  title_g <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = NA)),
    grid::textGrob(player_name, x = 0.02, hjust = 0,
                   gp = grid::gpar(col = player_name_color(side), fontface = "bold", cex = 0.40))
  )
  framed <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = "#C9C1B2", lwd = 0.8)),
    gridExtra::arrangeGrob(title_g, tg, ncol = 1, heights = grid::unit(c(0.9, 9.1), "null"))
  )
  framed
}

pitch_type_page_count <- function(d, hand_filter = "Total", player_order = NULL, per_page = 18L, combine_sl_sw = FALSE){
  num <- build_pitch_type_table_numeric(d, hand_filter, player_order, combine_sl_sw = combine_sl_sw)
  if (!nrow(num)) return(1L)
  max(1L, ceiling(dplyr::n_distinct(num$Hitter) / per_page))
}

pitch_type_tables_page <- function(d, hand_filter = "Total", page_num = 1, player_order = NULL, per_page = 18L, combine_sl_sw = FALSE){
  num <- build_pitch_type_table_numeric(d, hand_filter, player_order, combine_sl_sw = combine_sl_sw)
  if (!nrow(num)) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0.5, y = 0.5, label = "No hitter data found", size = 6))
  }
  players <- unique(num$Hitter)
  n_hitters <- length(players)
  total_pages <- max(1L, ceiling(n_hitters / per_page))
  page_num <- suppressWarnings(as.integer(page_num))
  if (!is.finite(page_num) || page_num < 1) page_num <- 1L
  page_num <- min(page_num, total_pages)
  idx <- ((page_num - 1L) * per_page + 1L):min(n_hitters, page_num * per_page)
  page_players <- players[idx]
  table_cols <- 3L
  table_rows <- 6L
  table_grobs <- lapply(page_players, function(player) {
    player_num <- num %>%
      dplyr::filter(Hitter == player) %>%
      dplyr::arrange(PitchBucket)
    single_pitch_type_table_grob(player, player_num, num, combine_sl_sw = combine_sl_sw)
  })
  rows_needed <- table_rows
  if (length(table_grobs) %% table_cols != 0) {
    blanks <- table_cols - (length(table_grobs) %% table_cols)
    table_grobs <- c(table_grobs, replicate(blanks, grid::nullGrob(), simplify = FALSE))
  }
  if (length(table_grobs) < table_cols * table_rows) {
    table_grobs <- c(table_grobs, replicate(table_cols * table_rows - length(table_grobs), grid::nullGrob(), simplify = FALSE))
  }
  body <- gridExtra::arrangeGrob(
    grobs = table_grobs,
    ncol = table_cols,
    padding = grid::unit(3.2, "pt"),
    heights = grid::unit(rep(1, rows_needed), "null")
  )
  title_g <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FBFAF7", col = NA)),
    grid::textGrob(sprintf("%s | Page %d of %d", hand_label(hand_filter), page_num, total_pages), x = 0.5, y = 0.5,
                   gp = grid::gpar(col = "#333333", fontface = "bold", cex = 0.72))
  )
  page <- gridExtra::arrangeGrob(title_g, body, ncol = 1, heights = grid::unit(c(0.28, 10.72), "null"))
  ggplotify::as.ggplot(function() grid::grid.draw(page))
}

# -------------------- All-hitter heat maps and sprays --------------------
prepare_heatmap_pa <- function(d){
  d <- filter_hitter_report_pitches(d)
  if (!nrow(d)) return(tibble::tibble())
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) return(tibble::tibble())
  d %>%
    dplyr::mutate(
      Hitter = as.character(.data[[bat_col]]),
      HeatGroup = factor(scout_heat_group(as.character(PitchType)),
                         levels = c("Fastball","Sinker","Spin","Soft")),
      PlateLocSide = plate_x,
      PlateLocHeight = plate_z
    ) %>%
    dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight),
                  dplyr::between(plate_x, -3, 3), dplyr::between(plate_z, 0, 5),
                  !is.na(HeatGroup))
}

heatmap_cell_plot <- function(dd){
  base <- ggplot() +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.55),
      panel.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    geom_rect(
      data = strike_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.45
    ) +
    geom_segment(
      data = home_plate_segments,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = 0.35
    ) +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE)
  if (!nrow(dd) || nrow(dd) <= 2) return(base)
  ggplot(dd, aes(PlateLocSide, PlateLocHeight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      n = 200,
      na.rm = TRUE
    ) +
    scale_fill_gradientn(colors = wblyrm_palette, guide = "none") +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.55),
      panel.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    geom_rect(
      data = strike_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.45
    ) +
    geom_segment(
      data = home_plate_segments,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = 0.35
    ) +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE)
}

spray_result_bucket <- function(pr_vec){
  dplyr::case_when(
    grepl("(?i)home\\s*run|homerun|\\bHR\\b", nz_chr(pr_vec), perl = TRUE) ~ "hit",
    grepl("(?i)\\bsingle\\b|\\b1B\\b|\\bdouble\\b|\\b2B\\b|\\btriple\\b|\\b3B\\b", nz_chr(pr_vec), perl = TRUE) ~ "hit",
    grepl("(?i)out|fielder|fielders\\s*choice|fielderschoice|force|sacrifice", nz_chr(pr_vec), perl = TRUE) ~ "out",
    TRUE ~ "other"
  )
}

compact_spray_data <- function(d, bucket = c("hit","out")){
  bucket <- match.arg(bucket)
  d <- filter_hitter_report_pitches(d)
  if (!nrow(d)) return(tibble::tibble())
  n0 <- nrow(d)
  dist <- if ("distance_ft" %in% names(d)) to_num(d$distance_ft) else rep(NA_real_, n0)
  bear <- if ("bearing" %in% names(d)) to_num(d$bearing) else rep(NA_real_, n0)
  hx <- if ("hc_x" %in% names(d)) to_num(d$hc_x) else rep(NA_real_, n0)
  hy <- if ("hc_y" %in% names(d)) to_num(d$hc_y) else rep(NA_real_, n0)
  use_tm <- is.finite(dist) & is.finite(bear)
  pr <- if ("play_result" %in% names(d)) d$play_result else rep("", n0)
  pc <- if ("pitch_call" %in% names(d)) d$pitch_call else rep("", n0)
  la_vec <- if ("la" %in% names(d)) to_num(d$la) else rep(NA_real_, n0)
  hit_type <- coalesce_cols_safe(d, c("TaggedHitType","AutoHitType","HitType","BattedBallType"), as = "character")
  
  d %>%
    dplyr::mutate(
      plot_x = dplyr::if_else(use_tm, dist * sin(bear * pi / 180), hx),
      plot_y = dplyr::if_else(use_tm, dist * cos(bear * pi / 180), hy),
      result_bucket = spray_result_bucket(pr),
      bip_flag = is_bip_txst(pc, pr),
      BBType = dplyr::case_when(
        is.finite(la_vec) & la_vec < 10 ~ "Ground Ball",
        grepl("(?i)ground", hit_type, perl = TRUE) ~ "Ground Ball",
        TRUE ~ "Air"
      ),
      gb_x = dplyr::if_else(BBType == "Ground Ball" & is.finite(bear), 120 * sin(bear * pi / 180), NA_real_),
      gb_y = dplyr::if_else(BBType == "Ground Ball" & is.finite(bear), 120 * cos(bear * pi / 180), NA_real_),
      plot_x = dplyr::if_else(is.finite(gb_x), gb_x, plot_x),
      plot_y = dplyr::if_else(is.finite(gb_y), gb_y, plot_y),
      PitchType = factor(as.character(PitchType), levels = names(pitch_color_values))
    ) %>%
    dplyr::filter(bip_flag %in% TRUE, result_bucket == bucket, is.finite(plot_x), is.finite(plot_y))
}

spray_cell_plot <- function(d, bucket = c("hit","out")){
  bucket <- match.arg(bucket)
  spray <- compact_spray_data(d, bucket)
  fld <- make_field_layers()
  p <- ggplot() +
    geom_polygon(data = fld$track_poly, aes(x = x, y = y), fill = "white", color = NA) +
    geom_path(data = fld$wall, aes(x = x, y = y), color = TXST_MAROON, linewidth = 0.35) +
    geom_segment(data = fld$foul_lines, aes(x = x, y = y, xend = xend, yend = yend),
                 color = TXST_MAROON, linewidth = 0.25) +
    geom_path(data = fld$rings, aes(x = x, y = y, group = r),
              linetype = "dashed", color = "grey55", linewidth = 0.18) +
    geom_path(data = fld$bases_diamond, aes(x = x, y = y), color = "black", linewidth = 0.25) +
    coord_fixed(xlim = c(-230, 230), ylim = c(0, 420), expand = FALSE) +
    theme_void(base_size = 6) +
    theme(
      panel.border = element_rect(color = "#1D1D1D", fill = NA, linewidth = 0.35),
      plot.margin = margin(0, 0, 0, 0)
    )
  if (!nrow(spray)) return(p)
  p +
    geom_segment(
      data = spray %>% dplyr::filter(BBType == "Ground Ball", is.finite(gb_x), is.finite(gb_y)),
      aes(x = 0, y = 0, xend = gb_x, yend = gb_y, color = PitchType),
      linetype = "dashed", linewidth = 0.35, alpha = 0.85,
      inherit.aes = FALSE
    ) +
    geom_point(data = spray, aes(x = plot_x, y = plot_y, color = PitchType),
               size = 1.15, alpha = 0.95, show.legend = FALSE) +
    scale_color_manual(values = pitch_color_values, drop = FALSE, guide = "none")
}

text_cell_grob <- function(label, face = "bold", fill = "#FFFFFF", col = "#222222", cex = 0.7){
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = fill, col = "#D9D2C2", lwd = 0.5)),
    grid::textGrob(label, gp = grid::gpar(col = col, fontface = face, cex = cex))
  )
}

heat_map_players <- function(d, player_order = NULL){
  d <- filter_hitter_report_pitches(d)
  hitter_report_meta(d, "Total", player_order)$Hitter
}

heat_map_page_count <- function(d, player_order = NULL){
  max(1L, ceiling(length(heat_map_players(d, player_order)) / 9))
}

blank_heat_grid_grob <- function(){
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = "#D9D2C2", lwd = 0.5))
  )
}

heat_maps_page <- function(d, hand_filter = "Total", page_num = 1, player_order = NULL){
  d <- filter_hitter_report_pitches(d)
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0.5, y = 0.5, label = "No hitter data found", size = 6))
  }
  players <- heat_map_players(d, player_order)
  if (!length(players)) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0.5, y = 0.5, label = "No hitters found", size = 6))
  }
  total_pages <- heat_map_page_count(d, player_order)
  page_num <- suppressWarnings(as.integer(page_num))
  if (!is.finite(page_num) || page_num < 1) page_num <- 1L
  page_num <- min(page_num, total_pages)
  idx <- ((page_num - 1L) * 9L + 1L):min(length(players), page_num * 9L)
  page_players <- players[idx]
  row_players <- c(page_players, rep(NA_character_, 9L - length(page_players)))
  ds <- filter_by_pitcher_hand(d, hand_filter)
  heat_pa <- prepare_heatmap_pa(ds)
  meta <- hitter_report_meta(d, "Total", player_order)
  side_lookup <- stats::setNames(meta$Side, meta$Hitter)
  headers <- c("", "Fastball SLG", "Sinker SLG", "Spin SLG", "Soft SLG", "Hits", "Outs")
  name_cex <- 0.58
  header_cex <- 0.58
  
  grobs <- lapply(headers, text_cell_grob, fill = TXST_MAROON, col = TXST_GOLD, cex = header_cex)
  for (player in row_players) {
    if (is.na(player)) {
      grobs <- c(grobs, replicate(7, blank_heat_grid_grob(), simplify = FALSE))
    } else {
      d_player <- ds %>% dplyr::filter(as.character(.data[[bat_col]]) == player)
      hp_player <- heat_pa %>% dplyr::filter(Hitter == player)
      grobs <- c(
        grobs,
        list(text_cell_grob(player, fill = "#FBFAF7", col = player_name_color(side_lookup[[player]] %||% "R"), cex = name_cex)),
        lapply(c("Fastball","Sinker","Spin","Soft"), function(pg) {
          ggplotGrob(heatmap_cell_plot(hp_player %>% dplyr::filter(HeatGroup == pg)))
        }),
        list(
          ggplotGrob(spray_cell_plot(d_player, "hit")),
          ggplotGrob(spray_cell_plot(d_player, "out"))
        )
      )
    }
  }
  
  grid_body <- gridExtra::arrangeGrob(
    grobs = grobs,
    ncol = 7,
    widths = grid::unit(c(0.78, rep(1, 6)), "null"),
    heights = grid::unit(c(0.20, rep(1, 9)), "null"),
    padding = grid::unit(0.8, "pt")
  )
  title_g <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FBFAF7", col = NA)),
    grid::textGrob("Heat Maps", x = 0.5, y = 0.68,
                   gp = grid::gpar(col = TXST_MAROON, fontface = "bold", cex = 1.35)),
    grid::textGrob(sprintf("%s | Page %d of %d", hand_label(hand_filter), page_num, total_pages), x = 0.5, y = 0.25,
                   gp = grid::gpar(col = "#333333", fontface = "bold", cex = 0.72))
  )
  page <- gridExtra::arrangeGrob(
    title_g,
    grid_body,
    ncol = 1,
    heights = grid::unit(c(0.42, 7.9), "null")
  )
  ggplotify::as.ggplot(function() grid::grid.draw(page))
}
# --- Robust PDF helpers with detailed error messages ---
write_error_pdf <- function(file, msg, width_in = LETTER_W, height_in = LETTER_H){
  grDevices::pdf(file, width = width_in, height = height_in, onefile = FALSE, useDingbats = FALSE)
  grid::grid.newpage()
  grid::grid.text(
    label = paste0("Export error:\n\n", msg),
    x = 0.5, y = 0.6, gp = grid::gpar(cex = 0.95, col = "#501214", fontface = "bold")
  )
  grid::grid.text(
    label = "(PDF was generated to avoid a browser error. See the message above.)",
    x = 0.5, y = 0.10, gp = grid::gpar(cex = 0.7, col = "#444444")
  )
  grDevices::dev.off()
}

save_card_pdf <- function(file, plot, width_in, height_in){
  errs <- character(0)
  ok <- FALSE
  
  # Try cairo_pdf via ggsave
  if (isTRUE(capabilities("cairo"))) {
    tryCatch({
      ggplot2::ggsave(
        filename = file, plot = plot,
        device   = grDevices::cairo_pdf,
        width    = width_in, height = height_in, units = "in", dpi = 300,
        dev.args = list(useDingbats = FALSE)
      )
      ok <- file.exists(file)
    }, error = function(e){ errs <<- c(errs, paste("cairo_pdf:", conditionMessage(e))) })
  }
  
  # Fallback: base pdf via ggsave
  if (!ok) {
    tryCatch({
      ggplot2::ggsave(
        filename = file, plot = plot,
        device   = grDevices::pdf,
        width    = width_in, height = height_in, units = "in", dpi = 300
      )
      ok <- file.exists(file)
    }, error = function(e){ errs <<- c(errs, paste("pdf(ggsave):", conditionMessage(e))) })
  }
  
  # Final fallback: manual device open/close
  if (!ok) {
    tryCatch({
      grDevices::pdf(file, width = width_in, height = height_in, onefile = FALSE, useDingbats = FALSE)
      print(plot)
      grDevices::dev.off()
      ok <- file.exists(file)
    }, error = function(e){ errs <<- c(errs, paste("pdf(manual):", conditionMessage(e))) })
  }
  
  list(ok = ok, err = if (length(errs)) paste(errs, collapse = "\n") else NULL)
}

save_multi_page_pdf <- function(file, plots, width_in, height_in){
  errs <- character(0)
  ok <- FALSE
  tryCatch({
    grDevices::pdf(file, width = width_in, height = height_in, onefile = TRUE, useDingbats = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (p in plots) print(p)
    ok <- file.exists(file)
  }, error = function(e){
    errs <<- c(errs, conditionMessage(e))
  })
  list(ok = ok, err = if (length(errs)) paste(errs, collapse = "\n") else NULL)
}




# -------------------- UI --------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, primary = TXST_MAROON, secondary = TXST_GOLD),
  tags$head(
    tags$style(HTML("
      body { background:#f5f1e8; }
      .report-shell { background:#fbfaf7; border:1px solid #d8cfbd; border-radius:8px; padding:10px; }
      .report-toolbar { display:flex; gap:12px; align-items:end; flex-wrap:wrap; margin-bottom:8px; }
      .report-toolbar .form-group { margin-bottom:0; min-width:180px; }
      .nav-tabs .nav-link { font-weight:700; color:#501214; }
      .nav-tabs .nav-link.active { color:#501214; border-top:4px solid #B4975A; }
    "))
  ),
  titlePanel("TXST Hitting Scouting"),
  sidebarLayout(
  sidebarPanel(width = 3,
               tags$h4("Report File"),
               uiOutput("csv_files_ui"),
               hr(),
               uiOutput("hitter_order_ui")
  ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel(
                  "Hitter Card",
                  uiOutput("guard_msg"),
                  div(class = "report-shell",
                      div(class = "report-toolbar",
                          uiOutput("player_ui"),
                          actionButton("preview", "Generate Preview", class = "btn btn-primary"),
                          downloadButton("dl_pdf","Download PDF", class = "btn btn-success")
                      ),
                      plotOutput("card", height = px_from_in(LETTER_H), width = px_from_in(LETTER_W)))
                ),
                tabPanel(
                  "Pitch Type Tables",
                  div(class = "report-shell",
                      div(class = "report-toolbar",
                          selectInput("pitch_type_hand", "Pitcher hand", choices = c("Total","LHP","RHP"), selected = "Total"),
                          checkboxInput("pitch_type_combine_sl_sw", "Combine SL + SW", value = FALSE, width = "180px"),
                          uiOutput("pitch_type_page_ui"),
                          downloadButton("dl_pitch_type_pdf", "Export PDF", class = "btn btn-success")
                      ),
                      plotOutput("pitch_type_tables", height = px_from_in(PORTRAIT_H), width = px_from_in(PORTRAIT_W))
                  )
                ),
                tabPanel(
                  "Heat Maps",
                  div(class = "report-shell",
                      div(class = "report-toolbar",
                          selectInput("heat_maps_hand", "Pitcher hand", choices = c("Total","LHP","RHP"), selected = "Total"),
                          uiOutput("heat_maps_page_ui"),
                          downloadButton("dl_heat_maps_pdf", "Export PDF", class = "btn btn-success")
                      ),
                      plotOutput("heat_maps", height = px_from_in(PORTRAIT_H), width = px_from_in(PORTRAIT_W))
                  )
                )
              )
    )
  )
)

# -------------------- Server --------------------
server <- function(input, output, session){
  
  files_refresh <- reactiveVal(0)
  ftp_log <- reactiveVal("")
  ftp_busy <- reactiveVal(FALSE)
  
  data_files <- reactive({
    files_refresh()
    if (!dir.exists(DATA_DIR)) return(character(0))
    list.files(DATA_DIR, pattern = "\\.(csv|CSV)$", full.names = FALSE)
  })
  
  output$csv_files_ui <- renderUI({
    files <- data_files()
    if (length(files) == 0) {
      return(helpText("No CSV files found in data/. Add a local CSV and refresh the app."))
    }
    selectizeInput("csv_files", "Choose game file(s) (data/):",
                   choices = files, multiple = TRUE,
                   options = list(placeholder = "Select one or more CSVs"))
  })
  
  output$ftp_log <- renderText({
    ftp_log()
  })
  
  output$ftp_spinner <- renderUI({
    if (!isTRUE(ftp_busy())) return(NULL)
    tags$div(style = "margin-top:6px;",
             tags$div(class = "spinner-border text-info", role = "status"),
             tags$span(style = "margin-left:8px;", "Searching FTP..."))
  })
  
  get_ftp_creds <- function() {
    host <- Sys.getenv("TRACKMAN_FTP_HOST", unset = "")
    user <- Sys.getenv("TRACKMAN_FTP_USER", unset = "")
    pass <- Sys.getenv("TRACKMAN_FTP_PASS", unset = "")
    list(host = host, user = user, pass = pass)
  }
  
  run_ftp_import <- function(years, label) {
    team <- normalize_team_code(input$team_code)
    if (!nzchar(team)) {
      ftp_log("Enter a team code first (e.g., TEX_BOB).")
      return(invisible(NULL))
    }
    
    creds <- get_ftp_creds()
    if (!nzchar(creds$host)) {
      ftp_log("Missing env var TRACKMAN_FTP_HOST.")
      return(invisible(NULL))
    }
    if (!nzchar(creds$user)) {
      ftp_log("Missing env var TRACKMAN_FTP_USER.")
      return(invisible(NULL))
    }
    if (!nzchar(creds$pass)) {
      ftp_log("Missing env var TRACKMAN_FTP_PASS.")
      return(invisible(NULL))
    }
    
    ftp_busy(TRUE)
    on.exit(ftp_busy(FALSE), add = TRUE)
    ftp_log(sprintf("Starting FTP import for %s (%s)...", team, label))
    
    errs <- character(0)
    paths <- unlist(lapply(years, function(y) {
      res <- list_year_paths(creds$host, creds$user, creds$pass, y)
      e <- attr(res, "errors", exact = TRUE)
      if (!is.null(e) && length(e)) errs <<- c(errs, e)
      res
    }), use.names = FALSE)
    
    if (length(paths) == 0) {
      if (length(errs)) {
        ftp_log(paste("No CSV files found. FTP errors:", paste(unique(errs), collapse = " | ")))
      } else if (FTP_DEBUG) {
        ftp_log(paste0(
          "No CSV files found. Checked base path '", FTP_BASE_PATH,
          "'. Try setting TRACKMAN_FTP_BASE if the FTP path differs."
        ))
      } else {
        ftp_log("No CSV files found for the selected year(s).")
      }
      return(invisible(NULL))
    }
    
    results <- vector("list", length = 0)
    used_files <- 0L
    rows_total <- 0L
    total_files <- length(paths)
    
    withProgress(message = "Downloading and filtering CSVs...", value = 0, {
      for (i in seq_along(paths)) {
        incProgress(1 / length(paths), detail = basename(paths[[i]]))
        tmp <- tempfile(fileext = ".csv")
        ok <- TRUE
        tryCatch({
          ftp_download(creds$host, creds$user, creds$pass, paths[[i]], tmp)
        }, error = function(e) {
          ok <<- FALSE
        })
        if (!ok) {
          unlink(tmp)
          next
        }
        
        df <- tryCatch(
          readr::read_csv(
            tmp,
            col_types = readr::cols(.default = readr::col_character()),
            guess_max = 200000, progress = FALSE, show_col_types = FALSE
          ),
          error = function(e) NULL
        )
        unlink(tmp)
        if (is.null(df)) next
        
        filtered <- filter_team_rows(df, team)
        if (nrow(filtered) == 0) next
        
        used_files <- used_files + 1L
        rows_total <- rows_total + nrow(filtered)
        results[[length(results) + 1L]] <- filtered
      }
    })
    
    if (length(results) == 0) {
      ftp_log(sprintf("Finished. No rows matched team code %s in %s.", team, label))
      return(invisible(NULL))
    }
    
    combined <- dplyr::bind_rows(results)
    years_label <- if (length(years) == 1) as.character(years[[1]]) else paste(range(years), collapse = "-")
    out_name <- sprintf("combined_%s_%s.csv", team, years_label)
    out_path <- file.path(DATA_DIR, out_name)
    readr::write_csv(combined, out_path)
    
    ftp_log(sprintf(
      "Finished. %d/%d file(s) matched, %d total rows. Saved to data/%s",
      used_files, total_files, rows_total, out_name
    ))
    files_refresh(files_refresh() + 1)
  }
  
  observeEvent(input$fetch_2026, {
    run_ftp_import(CURRENT_YEAR, paste0("Year ", CURRENT_YEAR))
  }, ignoreInit = TRUE)
  
  observeEvent(input$fetch_2025, {
    run_ftp_import(PREV_YEAR, paste0("Year ", PREV_YEAR))
  }, ignoreInit = TRUE)
  
  observeEvent(input$fetch_all, {
    run_ftp_import(c(PREV_YEAR, CURRENT_YEAR), paste0(PREV_YEAR, " + ", CURRENT_YEAR))
  }, ignoreInit = TRUE)
  
  observeEvent(input$ftp_test, {
    creds <- get_ftp_creds()
    if (!nzchar(creds$host) || !nzchar(creds$user) || !nzchar(creds$pass)) {
      ftp_log("Missing FTP creds. Set TRACKMAN_FTP_HOST, TRACKMAN_FTP_USER, TRACKMAN_FTP_PASS.")
      return(invisible(NULL))
    }
    ftp_busy(TRUE)
    on.exit(ftp_busy(FALSE), add = TRUE)
    
    fmt_line <- function(nm, p, lst) {
      err <- attr(lst, "error", exact = TRUE)
      if (!is.null(err) && nzchar(err)) {
        return(sprintf("[%s] %s :: ERROR: %s", nm, p, err))
      }
      sample <- if (length(lst)) paste(utils::head(lst, 8), collapse = ", ") else "<empty>"
      sprintf("[%s] %s :: %s", nm, p, sample)
    }
    
    out_lines <- character(0)
    root_path <- ""
    root_list <- ftp_list_dir(creds$host, creds$user, creds$pass, root_path, quiet = TRUE)
    out_lines <- c(out_lines, fmt_line("root", root_path, root_list))
    
    base_path <- FTP_BASE_PATH
    base_list <- ftp_list_dir(creds$host, creds$user, creds$pass, base_path, quiet = TRUE)
    out_lines <- c(out_lines, fmt_line("base", base_path, base_list))
    
    year_path <- file.path(FTP_BASE_PATH, CURRENT_YEAR)
    year_list <- ftp_list_dir(creds$host, creds$user, creds$pass, year_path, quiet = TRUE)
    out_lines <- c(out_lines, fmt_line("year", year_path, year_list))
    
    month_pick <- if (length(year_list)) year_list[[1]] else "01"
    month_path <- file.path(year_path, month_pick)
    month_list <- ftp_list_dir(creds$host, creds$user, creds$pass, month_path, quiet = TRUE)
    out_lines <- c(out_lines, fmt_line("month", month_path, month_list))
    
    day_pick <- if (length(month_list)) month_list[[1]] else "01"
    day_path <- file.path(month_path, day_pick)
    day_list <- ftp_list_dir(creds$host, creds$user, creds$pass, day_path, quiet = TRUE)
    out_lines <- c(out_lines, fmt_line("day", day_path, day_list))
    
    ftp_log(paste(out_lines, collapse = "\n"))
  }, ignoreInit = TRUE)
  
  df_all <- reactive({
    req(input$csv_files)
    files <- input$csv_files
    paths <- file.path(DATA_DIR, files)
    validate(need(all(file.exists(paths)), "One or more selected files are missing from data/."))
    out <- read_csv_files(paths)
    validate(need(is.data.frame(out), "Loaded file is not a data frame / tibble."))
    out
  })
  
  std_all <- reactive({
    d_raw <- df_all()
    tryCatch(std_cols(d_raw), error = function(e){
      message("[std_cols ERROR] ", conditionMessage(e))
      validate(need(FALSE, paste("Column standardization error:", conditionMessage(e))))
    })
  })
  
  game_hitter_order <- reactive({
    req(std_all())
    hitter_order_from_data(std_all(), "game")
  })
  
  pa_hitter_order <- reactive({
    req(std_all())
    hitter_order_from_data(std_all(), "pa")
  })
  
  output$hitter_order_ui <- renderUI({
    req(std_all())
    hitters <- game_hitter_order()
    validate(need(length(hitters) > 0, "No hitters found for ordering."))
    tagList(
      tags$h4("Hitter Order"),
      selectizeInput(
        "hitter_order",
        "Type a hitter name, select, then drag to order:",
        choices = hitters,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("drag_drop", "remove_button"),
          persist = TRUE,
          placeholder = "Start typing a hitter name"
        )
      ),
      fluidRow(
        column(6, actionButton("reset_game_order", "Game Order", class = "btn btn-outline-secondary btn-sm w-100")),
        column(6, actionButton("reset_pa_order", "PA Order", class = "btn btn-outline-secondary btn-sm w-100"))
      ),
      helpText("Reports include only hitters you add here. Use the buttons to auto-fill an order.")
    )
  })
  
  report_hitter_order <- reactive({
    req(std_all())
    hitters <- game_hitter_order()
    selected <- input$hitter_order
    if (is.null(selected)) selected <- character(0)
    apply_player_order(hitters, selected)
  })
  
  observeEvent(input$reset_game_order, {
    req(game_hitter_order())
    updateSelectizeInput(session, "hitter_order", choices = game_hitter_order(), selected = game_hitter_order(), server = FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_pa_order, {
    req(game_hitter_order(), pa_hitter_order())
    updateSelectizeInput(session, "hitter_order", choices = game_hitter_order(), selected = pa_hitter_order(), server = FALSE)
  }, ignoreInit = TRUE)
  
  output$guard_msg <- renderUI({
    if (is.null(input$csv_files) || length(input$csv_files) == 0) {
      div(style="margin:8px 0; padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
          HTML("<b>Select one or more CSVs</b> from data/ to begin."))
    }
  })
  
  output$player_ui <- renderUI({
    req(std_all())
    d <- std_all()
    bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
    validate(need(!is.na(bat_col), "No batter/hitter name column found."))
    choices <- report_hitter_order()
    if (!length(choices)) return(helpText("Add hitters in the sidebar to enable the player card."))
    selectInput("player","Player", choices = choices)
  })
  
  df_player <- reactive({
    req(std_all(), input$player)
    d <- std_all()
    bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
    validate(need(!is.na(bat_col), "Missing batter name column after load."))
    d %>% filter(.data[[bat_col]] == input$player)
  })
  
  card_plot <- eventReactive(input$preview, {
    d <- df_player()
    validate(need(nrow(d) > 0, "No rows for selected player."))
    player_disp <- paste0(input$player, batter_side_suffix(d))
    tryCatch(build_card(d, player_disp),
             error = function(e){
               message("[build_card ERROR] ", conditionMessage(e))
               validate(need(FALSE, paste("Card build error:", conditionMessage(e))))
             })
  }, ignoreInit = TRUE)
  
  output$card <- renderPlot({
    req(card_plot())
    card_plot()
  }, res = preview_dpi)
  
  pitch_type_page_plot <- reactive({
    req(std_all(), input$pitch_type_hand, report_hitter_order())
    page_num <- input$pitch_type_page_num %||% 1
    pitch_type_tables_page(
      std_all(), input$pitch_type_hand, page_num, report_hitter_order(),
      combine_sl_sw = isTRUE(input$pitch_type_combine_sl_sw)
    )
  })
  
  output$pitch_type_tables <- renderPlot({
    req(pitch_type_page_plot())
    pitch_type_page_plot()
  }, res = preview_dpi)
  
  output$pitch_type_page_ui <- renderUI({
    req(std_all(), input$pitch_type_hand, report_hitter_order())
    n_pages <- pitch_type_page_count(
      std_all(), input$pitch_type_hand, report_hitter_order(),
      combine_sl_sw = isTRUE(input$pitch_type_combine_sl_sw)
    )
    if (n_pages <= 1) return(NULL)
    selectInput("pitch_type_page_num", "Page", choices = seq_len(n_pages), selected = 1)
  })
  
  output$heat_maps_page_ui <- renderUI({
    req(std_all(), report_hitter_order())
    n_pages <- heat_map_page_count(std_all(), report_hitter_order())
    if (n_pages <= 1) return(NULL)
    selectInput("heat_maps_page_num", "Page", choices = seq_len(n_pages), selected = 1)
  })
  
  heat_maps_page_plot <- reactive({
    req(std_all(), input$heat_maps_hand, report_hitter_order())
    page_num <- input$heat_maps_page_num %||% 1
    heat_maps_page(std_all(), input$heat_maps_hand, page_num, report_hitter_order())
  })
  
  output$heat_maps <- renderPlot({
    req(heat_maps_page_plot())
    heat_maps_page_plot()
  }, res = preview_dpi)
  
  selected_files_slug <- function(default = "scouting"){
    files <- isolate(input$csv_files)
    if (is.null(files) || !length(files)) return(default)
    base <- tools::file_path_sans_ext(basename(files))
    gsub("[^A-Za-z0-9]+", "_", paste(base, collapse = "_"))
  }
  
  output$dl_pdf <- downloadHandler(
    filename = function(){
      d <- tryCatch(df_player(), error = function(e) NULL)
      nm <- (input$player %||% "card")
      if (!is.null(d)) nm <- paste0(nm, batter_side_suffix(d))
      paste0(gsub("\\s+","_", nm), "_hitting_card.pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      # Build data WITHOUT calling df_player() (avoids validate/need inside reactive)
      err_msg <- NULL
      player_name <- isolate(input$player %||% "")
      raw <- tryCatch(df_all(), error = function(e){ err_msg <<- paste("df_all():", conditionMessage(e)); NULL })
      if (is.null(raw)) {
        write_error_pdf(file, err_msg %||% "Failed to load data.")
        return(invisible(NULL))
      }
      
      std <- tryCatch(std_cols(raw), error = function(e){ err_msg <<- paste("std_cols():", conditionMessage(e)); NULL })
      if (is.null(std)) {
        write_error_pdf(file, err_msg %||% "Failed while standardizing columns.")
        return(invisible(NULL))
      }
      
      bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), std)
      d <- tryCatch({
        if (is.na(bat_col) || !nzchar(player_name)) std[0, ] else dplyr::filter(std, .data[[bat_col]] == player_name)
      }, error = function(e){ err_msg <<- paste("filter(player):", conditionMessage(e)); NULL })
      
      if (is.null(d) || !nrow(d)) {
        write_error_pdf(file, err_msg %||% "No rows for selected player.")
        return(invisible(NULL))
      }
      
      player_disp <- paste0(player_name, batter_side_suffix(d))
      card <- tryCatch(
        build_card(d, player_disp),
        error = function(e){ err_msg <<- paste("build_card():", conditionMessage(e)); NULL }
      )
      
      if (is.null(card)) {
        write_error_pdf(file, err_msg %||% "Could not assemble the plotting layout.")
        return(invisible(NULL))
      }
      
      res <- save_card_pdf(file, card, width_in = LETTER_W, height_in = LETTER_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.")
      }
    }
  )
  
  output$dl_pitch_type_pdf <- downloadHandler(
    filename = function(){
      paste0(selected_files_slug(), "_pitch_type_tables_", isolate(input$pitch_type_hand %||% "Total"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      err_msg <- NULL
      raw <- tryCatch(df_all(), error = function(e){ err_msg <<- paste("df_all():", conditionMessage(e)); NULL })
      if (is.null(raw)) {
        write_error_pdf(file, err_msg %||% "Failed to load data.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      std <- tryCatch(std_cols(raw), error = function(e){ err_msg <<- paste("std_cols():", conditionMessage(e)); NULL })
      if (is.null(std)) {
        write_error_pdf(file, err_msg %||% "Failed while standardizing columns.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      page <- tryCatch(
        {
          hand <- isolate(input$pitch_type_hand %||% "Total")
          combine_sl_sw <- isTRUE(isolate(input$pitch_type_combine_sl_sw))
          selected <- isolate(input$hitter_order)
          if (is.null(selected)) selected <- character(0)
          order <- apply_player_order(hitter_order_from_data(std, "game"), selected)
          n_pages <- pitch_type_page_count(std, hand, order, combine_sl_sw = combine_sl_sw)
          lapply(seq_len(n_pages), function(pg) {
            pitch_type_tables_page(std, hand, pg, order, combine_sl_sw = combine_sl_sw)
          })
        },
        error = function(e){ err_msg <<- paste("pitch_type_tables_page():", conditionMessage(e)); NULL }
      )
      if (is.null(page)) {
        write_error_pdf(file, err_msg %||% "Could not assemble pitch type tables.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      res <- save_multi_page_pdf(file, page, width_in = PORTRAIT_W, height_in = PORTRAIT_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.", PORTRAIT_W, PORTRAIT_H)
      }
    }
  )
  
  output$dl_heat_maps_pdf <- downloadHandler(
    filename = function(){
      paste0(selected_files_slug(), "_heat_maps_", isolate(input$heat_maps_hand %||% "Total"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      err_msg <- NULL
      raw <- tryCatch(df_all(), error = function(e){ err_msg <<- paste("df_all():", conditionMessage(e)); NULL })
      if (is.null(raw)) {
        write_error_pdf(file, err_msg %||% "Failed to load data.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      std <- tryCatch(std_cols(raw), error = function(e){ err_msg <<- paste("std_cols():", conditionMessage(e)); NULL })
      if (is.null(std)) {
        write_error_pdf(file, err_msg %||% "Failed while standardizing columns.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      page <- tryCatch(
        {
          selected <- isolate(input$hitter_order)
          if (is.null(selected)) selected <- character(0)
          order <- apply_player_order(hitter_order_from_data(std, "game"), selected)
          n_pages <- heat_map_page_count(std, order)
          lapply(seq_len(n_pages), function(pg) {
            heat_maps_page(std, isolate(input$heat_maps_hand %||% "Total"), pg, order)
          })
        },
        error = function(e){ err_msg <<- paste("heat_maps_page():", conditionMessage(e)); NULL }
      )
      if (is.null(page)) {
        write_error_pdf(file, err_msg %||% "Could not assemble heat maps.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      res <- save_multi_page_pdf(file, page, width_in = PORTRAIT_W, height_in = PORTRAIT_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.", PORTRAIT_W, PORTRAIT_H)
      }
    }
  )
  
  
}

shinyApp(ui, server)
