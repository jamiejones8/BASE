SCOUTING_EMBEDDED_MODE <- isTRUE(get0(
  "BASE_SCOUTING_EMBEDDED",
  envir = environment(),
  inherits = FALSE,
  ifnotfound = FALSE
))

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
  if (!SCOUTING_EMBEDDED_MODE) {
    library(tidyverse)
  } else {
    library(dplyr)
    library(purrr)
    library(stringr)
    library(tidyr)
  }
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
APP_ROOT <- get0(
  "BASE_SCOUTING_APP_ROOT",
  inherits = FALSE,
  ifnotfound = normalizePath(".", winslash = "/", mustWork = FALSE)
)
DATA_DIR <- get0(
  "BASE_SCOUTING_DATA_DIR",
  inherits = FALSE,
  ifnotfound = file.path(APP_ROOT, "data")
)
SCOUTING_SEASON_SOURCE <- get0("BASE_SCOUTING_SEASON_SOURCE", inherits = FALSE)
env_path <- file.path(APP_ROOT, ".Renviron")
if (!SCOUTING_EMBEDDED_MODE &&
    file.exists(env_path)) readRenviron(env_path)

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
PDF_PAGE_MARGIN_IN <- 1
PLAYER_TABLE_SPACER_IN <- 0.25
RV_PER_PITCH_SCALE <- 100
preview_dpi <- 130
px_from_in <- function(x) as.integer(x * preview_dpi)
`%||%` <- function(x,y) if (!is.null(x)) x else y
scouting_team_display_name <- function(x) {
  if (exists("base_team_display_name", mode = "function", inherits = TRUE)) {
    return(get("base_team_display_name", mode = "function", inherits = TRUE)(x))
  }
  as.character(x)
}

pdf_inner_fraction <- function(page_size_in, margin_in = PDF_PAGE_MARGIN_IN){
  if (!is.finite(page_size_in) || page_size_in <= 0) return(1)
  usable <- page_size_in - (2 * margin_in)
  if (!is.finite(usable) || usable <= 0) return(1)
  usable / page_size_in
}

as_inset_page_plot <- function(page_grob,
                               page_width_in = PORTRAIT_W,
                               page_height_in = PORTRAIT_H,
                               margin_in = PDF_PAGE_MARGIN_IN,
                               bg = "#FFFFFF"){
  ggplotify::as.ggplot(function(){
    grid::grid.rect(gp = grid::gpar(fill = bg, col = NA))
    grid::pushViewport(grid::viewport(
      width = pdf_inner_fraction(page_width_in, margin_in),
      height = pdf_inner_fraction(page_height_in, margin_in)
    ))
    grid::grid.draw(page_grob)
    grid::upViewport()
  })
}

# -------------------- Colors --------------------
TXST_MAROON <- "#501214"
TXST_GOLD   <- "#B4975A"
SHADE_GREEN <- "#A1D99B"  # good
SHADE_RED   <- "#F4A6A6"  # bad

# -------------------- Small utils --------------------
safe_ratio <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  len <- max(length(a), length(b))
  if (!is.finite(len) || len < 1) return(numeric(0))
  a <- rep_len(a, len)
  b <- rep_len(b, len)
  out <- rep(NA_real_, len)
  ok <- is.finite(b) & b > 0
  out[ok] <- a[ok] / b[ok]
  out
}
to_num     <- function(x) if (is.numeric(x)) x else suppressWarnings(readr::parse_number(as.character(x)))
nz_chr     <- function(x) ifelse(is.na(x), "", as.character(x))
pick_first <- function(cands, in_df) { cands <- cands[cands %in% names(in_df)]; if (length(cands)) cands[[1]] else NA_character_ }
source_file_label <- function(x){
  tools::file_path_sans_ext(basename(as.character(x)))
}
read_csv_files <- function(paths, source_labels = NULL){
  if (!length(paths)) return(tibble::tibble())
  if (is.null(source_labels)) source_labels <- basename(paths)
  source_labels <- rep_len(as.character(source_labels), length(paths))
  dfs <- lapply(paths, function(fp){
    idx <- match(fp, paths)
    tryCatch(
      readr::read_csv(
        fp,
        col_types = readr::cols(.default = readr::col_character()),
        guess_max = 200000, progress = FALSE, show_col_types = FALSE
      ) %>%
        dplyr::mutate(.source_file = source_labels[[idx]]),
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
HEATMAP_ZONE_SCALE <- 1.75
HEATMAP_HEART_INSET <- 0.25
scale_heatmap_x <- function(x) x * HEATMAP_ZONE_SCALE
scale_heatmap_z <- function(z) 2.5 + (z - 2.5) * HEATMAP_ZONE_SCALE
heatmap_strike_zone <- data.frame(
  xmin = scale_heatmap_x(strike_zone$xmin),
  xmax = scale_heatmap_x(strike_zone$xmax),
  ymin = scale_heatmap_z(strike_zone$ymin),
  ymax = scale_heatmap_z(strike_zone$ymax)
)
heatmap_heart_zone <- data.frame(
  xmin = heatmap_strike_zone$xmin + (heatmap_strike_zone$xmax - heatmap_strike_zone$xmin) * HEATMAP_HEART_INSET,
  xmax = heatmap_strike_zone$xmax - (heatmap_strike_zone$xmax - heatmap_strike_zone$xmin) * HEATMAP_HEART_INSET,
  ymin = heatmap_strike_zone$ymin + (heatmap_strike_zone$ymax - heatmap_strike_zone$ymin) * HEATMAP_HEART_INSET,
  ymax = heatmap_strike_zone$ymax - (heatmap_strike_zone$ymax - heatmap_strike_zone$ymin) * HEATMAP_HEART_INSET
)
heatmap_home_plate_segments <- data.frame(
  x = scale_heatmap_x(home_plate_segments$x),
  y = home_plate_segments$y,
  xend = scale_heatmap_x(home_plate_segments$xend),
  yend = home_plate_segments$yend
)
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

pitcher_bust_barrel_flag <- function(pc, ev, la) {
  evn <- to_num(ev)
  lan <- to_num(la)
  bip <- is_bip_for_barrel(pc)
  out <- bip & is.finite(evn) & is.finite(lan) & evn >= 95 & lan >= 5 & lan <= 35
  out[is.na(out)] <- FALSE
  out
}

pitcher_bust_gb_flag <- function(pc, ev, la) {
  evn <- to_num(ev)
  lan <- to_num(la)
  bip <- is_bip_for_barrel(pc)
  out <- bip & is.finite(evn) & is.finite(lan) & (
    (evn > 95 & lan <= 5) |
      (evn < 85 & lan <= 10) |
      (evn >= 85 & evn <= 95 & lan < 7.5)
  )
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
    return(dplyr::bind_rows(mk("Total"), mk("v LHP"), mk("v RHP")))
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
    calc_row(fp, "Total"),
    calc_row(vL, "v LHP"),
    calc_row(vR, "v RHP")
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
    make_stat_row(d,                        "Total"),
    make_stat_row(d[is_L, , drop = FALSE], "v LHP"),
    make_stat_row(d[is_R, , drop = FALSE], "v RHP")
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
    ref_key <- if (nm %in% names(metric_map)) metric_map[[nm]] else nm
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
      Split = c("Total","v LHP","v RHP"),
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
      header_title = "HITTER SPLITS (TOTAL / vLHP / vRHP)",
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

apply_matchup_order <- function(players, selected = NULL, max_items = 15L){
  players <- unique(stats::na.omit(as.character(players)))
  if (!length(players)) return(character(0))
  if (is.null(selected)) return(utils::head(players, max_items))
  if (!length(selected)) return(character(0))
  selected <- unique(stats::na.omit(as.character(selected)))
  utils::head(intersect(selected, players), max_items)
}

apply_pitcher_bust_order <- function(players, selected = NULL){
  players <- unique(stats::na.omit(as.character(players)))
  if (!length(players) || is.null(selected) || !length(selected)) return(character(0))
  selected <- unique(stats::na.omit(as.character(selected)))
  intersect(selected, players)
}

pitcher_bust_source_input_id <- function(pitcher){
  key <- paste(utf8ToInt(as.character(pitcher)), collapse = "_")
  paste0("pitcher_bust_source__", gsub("[^A-Za-z0-9]+", "_", as.character(pitcher)), "_", key)
}

pitcher_bust_source_choices <- function(source_files){
  source_files <- unique(stats::na.omit(as.character(source_files)))
  c("All selected sources" = "__ALL__", stats::setNames(source_files, source_file_label(source_files)))
}

selected_pitch_pa_rows <- function(d, group_vars, balls_col, strikes_col, two_k_only = FALSE){
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
  d2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "PA_ID")))) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}

matchup_metric_specs <- function(metric = "rv"){
  metric <- tolower(as.character(metric %||% "rv"))
  switch(
    metric,
    "whiff" = list(label = "Whiff%", hitter_col = "HitterWhiff", pitcher_col = "PitcherWhiff", ref = 0.229, scale = 0.120, lower_good = FALSE),
    "gb" = list(label = "GB%", hitter_col = "HitterGB", pitcher_col = "PitcherGB", ref = 0.420, scale = 0.120, lower_good = FALSE),
    "slg" = list(label = "SLG", hitter_col = "HitterSLG", pitcher_col = "PitcherSLG", ref = 0.400, scale = 0.220, lower_good = TRUE),
    list(label = "Run Value", hitter_col = "HitterRV", pitcher_col = "PitcherRV", ref = 0, scale = 18, lower_good = TRUE)
  )
}

matchup_edge_from_value <- function(value, metric = "rv"){
  spec <- matchup_metric_specs(metric)
  ifelse(
    is.finite(value),
    if (isTRUE(spec$lower_good)) {
      -((value - spec$ref) / spec$scale)
    } else {
      (value - spec$ref) / spec$scale
    },
    NA_real_
  )
}

matchup_score_from_edge <- function(edge){
  ifelse(
    is.finite(edge),
    pmin(pmax(50 + 50 * edge, 0), 100),
    NA_real_
  )
}

matchup_score_from_value <- function(value, metric = "rv"){
  matchup_score_from_edge(matchup_edge_from_value(value, metric))
}

matchup_pitch_family_levels <- c("Hard", "Breaking", "Soft")

matchup_pitch_family <- function(pt_chr){
  pt <- canonical_pitch_fuzzy(pt_chr)
  dplyr::case_when(
    pt %in% c("Fastball", "Sinker") ~ "Hard",
    pt %in% c("Cutter", "Slider", "Sweeper", "Curveball") ~ "Breaking",
    pt %in% c("Changeup", "Splitter") ~ "Soft",
    TRUE ~ NA_character_
  )
}

matchup_hitter_label <- function(name, side, html = FALSE){
  side <- toupper(as.character(side %||% ""))
  side_lbl <- dplyr::case_when(
    side == "L" ~ "LHH",
    side == "R" ~ "RHH",
    side == "S" ~ "SH",
    TRUE ~ ""
  )
  out <- ifelse(nzchar(side_lbl), paste0(name, " | ", side_lbl), as.character(name))
  if (!isTRUE(html)) return(out)
  name_html <- htmltools::htmlEscape(out)
  ifelse(side == "L", paste0("<span style=\"color:#C00000;\">", name_html, "</span>"), name_html)
}

matchup_pitcher_label <- function(name, throws, html = FALSE){
  throws <- toupper(as.character(throws %||% ""))
  out <- ifelse(nzchar(throws), paste0(name, " | ", throws), as.character(name))
  if (!isTRUE(html)) return(out)
  name_html <- htmltools::htmlEscape(out)
  ifelse(throws == "LHP", paste0("<span style=\"color:#C00000;\">", name_html, "</span>"), name_html)
}

build_matchup_hitter_summary <- function(d){
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col) || !nrow(d)) {
    return(tibble::tibble(
      Hitter = character(),
      HitterSide = character(),
      PitcherThrows = character(),
      PitchFamily = character(),
      HitterPA = integer(),
      HitterP = integer(),
      HitterRV = numeric(),
      HitterWhiff = numeric(),
      HitterGB = numeric(),
      HitterSLG = numeric()
    ))
  }
  meta <- hitter_report_meta(d, "Total") %>%
    dplyr::rename(HitterSide = Side, TotalPA = PA) %>%
    dplyr::select(Hitter, HitterSide, TotalPA)
  cc <- count_cols(d)
  splits <- c("LHP", "RHP")
  pieces <- lapply(splits, function(hand) {
    ds <- filter_by_pitcher_hand(d, hand) %>%
      dplyr::mutate(
        Hitter = as.character(.data[[bat_col]]),
        PitchFamily = matchup_pitch_family(as.character(PitchType)),
        hit_type = coalesce_cols_safe(., c("TaggedHitType","AutoHitType","HitType","BattedBallType"), as = "character"),
        launch_angle = coalesce_cols_safe(., c("Angle", "LaunchAngle", "Launch_Angle"), as = "numeric")
      ) %>%
      dplyr::filter(!is.na(Hitter), nzchar(trimws(Hitter)), !is.na(PitchFamily))
    if (!nrow(ds)) return(NULL)
    ds_sw <- swing_table(ds, cc$balls, cc$strikes)
    whiff_tbl <- ds_sw %>%
      dplyr::group_by(Hitter, PitchFamily) %>%
      dplyr::summarise(
        hitter_swings = sum(is_swing, na.rm = TRUE),
        hitter_whiffs = sum(is_whiff, na.rm = TRUE),
        HitterWhiff = safe_ratio(hitter_whiffs, hitter_swings),
        .groups = "drop"
      )
    selected_last <- selected_pitch_pa_rows(ds, c("Hitter", "PitchFamily"), cc$balls, cc$strikes)
    gb_tbl <- selected_last %>%
      dplyr::mutate(
        is_bip = safe_is_bip(pitch_call, play_result) |
          is.finite(launch_angle) |
          nzchar(trimws(hit_type)),
        is_gb = grepl("(?i)ground", hit_type, perl = TRUE) |
          (is.finite(launch_angle) & launch_angle < 10)
      ) %>%
      dplyr::group_by(Hitter, PitchFamily) %>%
      dplyr::summarise(
        gb_num = sum(is_gb & is_bip, na.rm = TRUE),
        gb_den = sum(is_bip, na.rm = TRUE),
        HitterGB = safe_ratio(gb_num, gb_den),
        .groups = "drop"
      )
    pa_tbl <- ds %>%
      dplyr::mutate(PA_ID = make_pa_id(ds)) %>%
      dplyr::distinct(Hitter, PitchFamily, PA_ID) %>%
      dplyr::count(Hitter, PitchFamily, name = "HitterPA")
    pitch_tbl <- ds %>%
      dplyr::count(Hitter, PitchFamily, name = "HitterP")
    pa_sum <- summarise_selected_pitch_results(ds, c("Hitter", "PitchFamily"), cc$balls, cc$strikes) %>%
      ensure_pa_summary_cols(c("Hitter", "PitchFamily"))
    pitch_tbl %>%
      dplyr::left_join(pa_tbl, by = c("Hitter", "PitchFamily")) %>%
      dplyr::left_join(pa_sum, by = c("Hitter", "PitchFamily")) %>%
      dplyr::left_join(whiff_tbl, by = c("Hitter", "PitchFamily")) %>%
      dplyr::left_join(gb_tbl, by = c("Hitter", "PitchFamily")) %>%
      dplyr::mutate(
        PitcherThrows = hand,
        HitterPA = tidyr::replace_na(HitterPA, 0L),
        HitterP = tidyr::replace_na(HitterP, 0L),
        dplyr::across(c(AB, H, TB, BB, HBP, SF, K, RBI, HR), ~tidyr::replace_na(.x, 0)),
        HitterRV = rv_formula(TB, BB, K, RBI, HR, HitterP),
        HitterSLG = safe_ratio(TB, AB)
      ) %>%
      dplyr::select(Hitter, PitcherThrows, PitchFamily, HitterPA, HitterP, HitterRV, HitterWhiff, HitterGB, HitterSLG)
  })
  out <- dplyr::bind_rows(pieces)
  if (!nrow(out)) return(tibble::tibble(
    Hitter = character(),
    HitterSide = character(),
    PitcherThrows = character(),
    PitchFamily = character(),
    HitterPA = integer(),
    HitterP = integer(),
    HitterRV = numeric(),
    HitterWhiff = numeric(),
    HitterGB = numeric(),
    HitterSLG = numeric()
  ))
  out %>%
    dplyr::left_join(meta, by = "Hitter") %>%
    dplyr::mutate(
      HitterSide = tidyr::replace_na(HitterSide, "R"),
      HitterPA = as.integer(HitterPA),
      HitterP = as.integer(HitterP)
    ) %>%
    dplyr::select(Hitter, HitterSide, PitcherThrows, PitchFamily, HitterPA, HitterP, HitterRV, HitterWhiff, HitterGB, HitterSLG, TotalPA)
}

build_matchup_pitcher_summary <- function(d){
  prepped <- pitcher_bust_prepare(d, "Total") %>%
    dplyr::mutate(
      PitchFamily = matchup_pitch_family(as.character(PitchType))
    ) %>%
    dplyr::filter(!is.na(PitchFamily))
  if (!nrow(prepped)) {
    return(tibble::tibble(
      PitcherName = character(),
      Throws = character(),
      BatterSide = character(),
      PitchFamily = character(),
      PitcherPA = integer(),
      PitcherP = integer(),
      PitcherRV = numeric(),
      PitcherWhiff = numeric(),
      PitcherGB = numeric(),
      PitcherSLG = numeric()
    ))
  }
  pa_tbl <- prepped %>%
    dplyr::distinct(PitcherName, BatterSide, PitchFamily, PA_ID) %>%
    dplyr::count(PitcherName, BatterSide, PitchFamily, name = "PitcherPA")
  pitch_tbl <- prepped %>%
    dplyr::count(PitcherName, BatterSide, PitchFamily, name = "PitcherP")
  whiff_tbl <- prepped %>%
    dplyr::group_by(PitcherName, BatterSide, PitchFamily) %>%
    dplyr::summarise(
      PitcherWhiff = safe_ratio(sum(is_whiff, na.rm = TRUE), sum(is_swing, na.rm = TRUE)),
      .groups = "drop"
    )
  throws_tbl <- prepped %>%
    dplyr::group_by(PitcherName) %>%
    dplyr::summarise(Throws = pitcher_bust_first(PitcherThrows), .groups = "drop")
  selected_last <- selected_pitch_pa_rows(
    prepped,
    c("PitcherName", "BatterSide", "PitchFamily"),
    "balls_b4",
    "strikes_b4"
  )
  gb_tbl <- selected_last %>%
    dplyr::group_by(PitcherName, BatterSide, PitchFamily) %>%
    dplyr::summarise(
      PitcherGB = safe_ratio(sum(is_gb & is_bip, na.rm = TRUE), sum(is_bip, na.rm = TRUE)),
      .groups = "drop"
    )
  pa_sum <- summarise_pa_results(selected_last, c("PitcherName", "BatterSide", "PitchFamily")) %>%
    ensure_pa_summary_cols(c("PitcherName", "BatterSide", "PitchFamily"))
  pitch_tbl %>%
    dplyr::left_join(pa_tbl, by = c("PitcherName", "BatterSide", "PitchFamily")) %>%
    dplyr::left_join(pa_sum, by = c("PitcherName", "BatterSide", "PitchFamily")) %>%
    dplyr::left_join(whiff_tbl, by = c("PitcherName", "BatterSide", "PitchFamily")) %>%
    dplyr::left_join(gb_tbl, by = c("PitcherName", "BatterSide", "PitchFamily")) %>%
    dplyr::left_join(throws_tbl, by = "PitcherName") %>%
    dplyr::mutate(
      PitcherPA = tidyr::replace_na(PitcherPA, 0L),
      PitcherP = tidyr::replace_na(PitcherP, 0L),
      dplyr::across(c(AB, H, TB, BB, HBP, SF, K, RBI, HR), ~tidyr::replace_na(.x, 0)),
      PitcherRV = rv_formula(TB, BB, K, RBI, HR, PitcherP),
      PitcherSLG = safe_ratio(TB, AB)
    ) %>%
    dplyr::select(PitcherName, Throws, BatterSide, PitchFamily, PitcherPA, PitcherP, PitcherRV, PitcherWhiff, PitcherGB, PitcherSLG)
}

build_matchup_grid <- function(hitter_summary, pitcher_summary, hitter_order, pitcher_order, metric = "rv"){
  spec <- matchup_metric_specs(metric)
  display_pitch_families <- c("Total", matchup_pitch_family_levels)
  hitters <- apply_matchup_order(hitter_order, hitter_order)
  pitchers <- apply_matchup_order(pitcher_order, pitcher_order)
  if (!length(hitters) || !length(pitchers)) {
    return(list(
      wide = tibble::tibble(Hitter = character()),
      long = tibble::tibble()
    ))
  }
  hitter_lookup <- hitter_summary %>%
    dplyr::distinct(Hitter, HitterSide, TotalPA)
  pitcher_lookup <- pitcher_summary %>%
    dplyr::distinct(PitcherName, Throws)
  grid_components <- tidyr::expand_grid(
    Hitter = hitters,
    PitcherName = pitchers,
    PitchFamily = matchup_pitch_family_levels
  ) %>%
    dplyr::left_join(hitter_lookup, by = "Hitter") %>%
    dplyr::left_join(pitcher_lookup, by = "PitcherName") %>%
    dplyr::left_join(
      hitter_summary %>%
        dplyr::select(Hitter, PitcherThrows, PitchFamily, HitterPA, HitterP, HitterRV, HitterWhiff, HitterGB, HitterSLG),
      by = dplyr::join_by(Hitter, PitchFamily, Throws == PitcherThrows)
    ) %>%
    dplyr::left_join(
      pitcher_summary %>%
        dplyr::select(PitcherName, BatterSide, PitchFamily, PitcherPA, PitcherP, PitcherRV, PitcherWhiff, PitcherGB, PitcherSLG),
      by = dplyr::join_by(PitcherName, PitchFamily, HitterSide == BatterSide)
    ) %>%
    dplyr::mutate(
      HitterEdge = matchup_edge_from_value(.data[[spec$hitter_col]], metric),
      PitcherEdge = matchup_edge_from_value(.data[[spec$pitcher_col]], metric),
      MatchupEdge = dplyr::if_else(
        is.finite(HitterEdge) | is.finite(PitcherEdge),
        rowSums(cbind(HitterEdge, PitcherEdge), na.rm = TRUE),
        NA_real_
      ),
      MatchupScore = matchup_score_from_edge(MatchupEdge),
      PitcherUsage = safe_ratio(PitcherP, sum(PitcherP, na.rm = TRUE)),
      HitterLabel = matchup_hitter_label(Hitter, HitterSide, html = TRUE),
      PitcherLabel = matchup_pitcher_label(PitcherName, Throws, html = TRUE),
      ColumnKey = paste(PitcherName, PitchFamily, sep = "|||"),
      ColumnLabel = paste0(PitcherLabel, "<br>", PitchFamily)
    ) %>%
    dplyr::group_by(Hitter, PitcherName) %>%
    dplyr::mutate(PitcherUsage = safe_ratio(PitcherP, sum(PitcherP, na.rm = TRUE))) %>%
    dplyr::ungroup()

  total_rows <- grid_components %>%
    dplyr::group_by(Hitter, PitcherName, HitterSide, Throws, HitterLabel, PitcherLabel) %>%
    dplyr::summarise(
      PitchFamily = "Total",
      HitterPA = dplyr::first(HitterPA),
      HitterP = sum(HitterP, na.rm = TRUE),
      PitcherPA = dplyr::first(PitcherPA),
      PitcherP = sum(PitcherP, na.rm = TRUE),
      MatchupEdge = {
        ok <- is.finite(MatchupEdge) & is.finite(PitcherUsage) & PitcherUsage > 0
        if (any(ok)) stats::weighted.mean(MatchupEdge[ok], w = PitcherUsage[ok]) else NA_real_
      },
      PitcherUsage = 1,
      ColumnKey = paste(dplyr::first(PitcherName), "Total", sep = "|||"),
      ColumnLabel = paste0(dplyr::first(PitcherLabel), "<br>Total"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      MatchupScore = matchup_score_from_edge(MatchupEdge)
    )

  grid <- dplyr::bind_rows(total_rows, grid_components)

  column_order <- as.vector(unlist(lapply(pitchers, function(pitcher) {
    paste(pitcher, display_pitch_families, sep = "|||")
  })))
  column_labels <- grid %>%
    dplyr::distinct(ColumnKey, ColumnLabel) %>%
    tibble::deframe()
  sub_labels <- rep(display_pitch_families, times = length(pitchers))
  pitcher_labels <- pitcher_lookup %>%
    dplyr::filter(PitcherName %in% pitchers) %>%
    dplyr::mutate(PitcherLabel = matchup_pitcher_label(PitcherName, Throws, html = TRUE)) %>%
    dplyr::select(PitcherName, PitcherLabel) %>%
    tibble::deframe()
  hitter_labels <- grid %>%
    dplyr::distinct(Hitter, HitterLabel)
  wide <- grid %>%
    dplyr::mutate(MatchupScore = round(MatchupScore)) %>%
    dplyr::select(Hitter, HitterLabel, ColumnKey, MatchupScore) %>%
    tidyr::pivot_wider(names_from = ColumnKey, values_from = MatchupScore) %>%
    dplyr::left_join(hitter_labels, by = c("Hitter", "HitterLabel")) %>%
    dplyr::mutate(.order = match(Hitter, hitters)) %>%
    dplyr::arrange(.order) %>%
    dplyr::select(-.order, -Hitter) %>%
    dplyr::rename(Hitter = HitterLabel)
  missing_cols <- setdiff(column_order, names(wide))
  for (nm in missing_cols) wide[[nm]] <- NA_real_
  wide <- wide %>%
    dplyr::select(Hitter, dplyr::all_of(column_order))
  total_cols <- column_order[seq(1, length(column_order), by = 4)]
  list(
    wide = wide,
    long = grid,
    score_cols = column_order,
    colnames = c("Hitter", sub_labels),
    total_wide = wide %>% dplyr::select(Hitter, dplyr::all_of(total_cols)),
    total_cols = total_cols,
    total_colnames = c("Hitter", unname(pitcher_labels[pitchers])),
    pitcher_spanners = unname(pitcher_labels[pitchers]),
    pitch_family_labels = sub_labels,
    pitcher_group_end_cols = column_order[seq(4, length(column_order), by = 4)],
    metric_label = spec$label
  )
}

filename_part <- function(x, default = "scouting"){
  x <- trimws(as.character(x %||% default))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nzchar(x)) x else default
}

team_code_from_files <- function(files, default = "TeamCode"){
  if (is.null(files) || !length(files)) return(default)
  bases <- tools::file_path_sans_ext(basename(files))
  codes <- vapply(bases, function(base) {
    code <- base
    code <- sub("(?i)^combined[-_ ]+", "", code, perl = TRUE)
    code <- sub("(?i)[-_ ]*(hitters?|pitchers?)[-_ ]*cleaned$", "", code, perl = TRUE)
    code <- sub("(?i)[-_ ]*(hitters?|pitchers?)$", "", code, perl = TRUE)
    code <- sub("(?i)[-_ ]*cleaned$", "", code, perl = TRUE)
    code <- sub("(?i)[-_ ]*(20[0-9]{2})([-_ ]*20[0-9]{2})*$", "", code, perl = TRUE)
    filename_part(code, default)
  }, character(1))
  codes <- unique(codes[nzchar(codes)])
  if (!length(codes)) default else if (length(codes) == 1L) codes[[1]] else filename_part(paste(codes, collapse = "_"), default)
}

player_first_last_slug <- function(name, default = "Pitcher"){
  name <- trimws(as.character(name %||% default))
  if (grepl(",", name, fixed = TRUE)) {
    parts <- strsplit(name, ",", fixed = TRUE)[[1]]
    last <- trimws(parts[[1]])
    first <- trimws(paste(parts[-1], collapse = " "))
    name <- trimws(paste(first, last))
  }
  filename_part(name, default)
}

pitcher_last_slug <- function(name, default = "Pitcher"){
  name <- trimws(as.character(name %||% default))
  if (grepl(",", name, fixed = TRUE)) {
    last <- trimws(strsplit(name, ",", fixed = TRUE)[[1]][[1]])
    return(filename_part(last, default))
  }
  parts <- strsplit(name, "\\s+")[[1]]
  filename_part(utils::tail(parts, 1), default)
}

pitcher_card_split_tag <- function(side){
  side <- as.character(side %||% "Total")
  if (identical(side, "L")) return("vLHH")
  if (identical(side, "R")) return("vRHH")
  "vTotal"
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

rv_formula <- function(tb, bb, k, rbi, hr, pitches, scale = RV_PER_PITCH_SCALE){
  ifelse(pitches > 0, (((tb + bb - k) / 4) + rbi + hr) / pitches * scale, NA_real_)
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
                                  Avg = NA_real_, SLG = NA_real_, `Chase%` = NA_real_, `Whiff%` = NA_real_,
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
      dplyr::across(c(FP_swing, FP_total, swings, whiffs, swings_2k, whiffs_2k, AB,H,TB,BB,HBP,SF,K,RBI,HR,P2K,
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

pitch_table_col_widths <- function(disp_df,
                                   min_stat_chars = 5L){
  col_names <- names(disp_df)
  char_counts <- vapply(col_names, function(col_nm){
    if (identical(col_nm, "Pitch")) {
      max(min_stat_chars, nchar(c(col_nm, as.character(disp_df[[col_nm]])), type = "width"), na.rm = TRUE)
    } else {
      max(min_stat_chars, nchar(col_nm, type = "width"))
    }
  }, integer(1))
  grid::unit(char_counts, "null")
}

summarise_pitch_type_group <- function(player_num, label, members, display_levels){
  rows <- player_num %>% dplyr::filter(as.character(PitchBucket) %in% members)
  if (!nrow(rows)) return(NULL)
  rows %>%
    dplyr::summarise(
      Hitter = dplyr::first(Hitter),
      Side = dplyr::first(Side),
      PA = dplyr::first(PA),
      PitchBucket = factor(label, levels = display_levels),
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
}

single_pitch_type_table_grob <- function(player_name, player_num, all_num, combine_sl_sw = FALSE){
  metric_levels <- c("P","FPS%","RV","Avg","SLG","Whiff%","Chase%","2k RV","2k Whiff","2k Chase")
  display_levels <- c(pitch_type_display_levels(combine_sl_sw), "HARD", "SPIN", "Total")
  hard_row <- summarise_pitch_type_group(player_num, "HARD", c("FB", "SNK"), display_levels)
  spin_row <- summarise_pitch_type_group(player_num, "SPIN", c("CT", "SL", "SW", "CB"), display_levels)
  total_row <- player_num %>%
    dplyr::summarise(
      Hitter = dplyr::first(Hitter),
      Side = dplyr::first(Side),
      PA = dplyr::first(PA),
      PitchBucket = factor("Total", levels = display_levels),
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
  player_num <- dplyr::bind_rows(player_num, hard_row, spin_row, total_row) %>%
    dplyr::mutate(PitchBucket = factor(as.character(PitchBucket), levels = display_levels)) %>%
    dplyr::arrange(PitchBucket)
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
      fg_params = list(fontsize = 4.55, fontface = "bold"),
      bg_params = list(fill = "#FFFFFF", col = "#D7D0C6", lwd = 0.35),
      padding = grid::unit(c(0.35, 0.10), "pt")
    ),
    colhead = list(
      fg_params = list(fontsize = 3.95, fontface = "bold", col = TXST_GOLD),
      bg_params = list(fill = TXST_MAROON, col = TXST_MAROON, lwd = 0.45),
      padding = grid::unit(c(0.35, 0.10), "pt")
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
    tg$widths <- pitch_table_col_widths(disp)
    tg$heights <- grid::unit(rep(1, length(tg$heights)), "null")
  }, error = function(e) NULL)
  
  side_vals <- player_num$Side[!is.na(player_num$Side)]
  side <- if (length(side_vals)) side_vals[[1]] else "R"
  title_g <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = NA)),
    grid::textGrob(player_name, x = 0.02, hjust = 0,
                   gp = grid::gpar(col = player_name_color(side), fontface = "bold", cex = 0.58))
  )
  framed <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = "#C9C1B2", lwd = 0.8)),
    gridExtra::arrangeGrob(title_g, tg, ncol = 1, heights = grid::unit(c(0.78, 9.22), "null"))
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
    widths = grid::unit(rep(1, table_cols), "null"),
    padding = grid::unit(6, "pt"),
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
prepare_heatmap_events <- function(d){
  d <- filter_hitter_report_pitches(d)
  if (!nrow(d)) return(tibble::tibble())
  bat_col <- pick_first(c("Batter","Hitter","batter_name","batter"), d)
  if (is.na(bat_col)) return(tibble::tibble())
  swing_calls <- c(
    "StrikeSwinging","FoulBall","FoulBallFieldable","FoulBallNotFieldable",
    "FoulTip","InPlay","InPlayOut","InPlayNoOut"
  )
  d %>%
    dplyr::mutate(
      Hitter = as.character(.data[[bat_col]]),
      PlateLocSide = plate_x,
      PlateLocHeight = plate_z,
      HeatLocSide = scale_heatmap_x(PlateLocSide),
      HeatLocHeight = scale_heatmap_z(PlateLocHeight),
      hard_hit_bip = is_bip_txst(pitch_call, play_result) & is.finite(ev) & ev > 95,
      whiff_flag = pitch_call == "StrikeSwinging",
      chase_flag = (pitch_call %in% swing_calls) & (in_zone %in% FALSE)
    ) %>%
    dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight),
                  dplyr::between(plate_x, -3, 3), dplyr::between(plate_z, 0, 5))
}

heatmap_metric_filter <- function(dd, metric){
  if (!nrow(dd)) return(dd)
  keep <- switch(
    metric,
    hard_hit = dd$hard_hit_bip %in% TRUE,
    whiff = dd$whiff_flag %in% TRUE,
    chase = dd$chase_flag %in% TRUE,
    rep(FALSE, nrow(dd))
  )
  dd[keep, , drop = FALSE]
}

heatmap_metric_specs <- function(){
  list(
    list(key = "hard_hit", label = "Hard Hit Freq"),
    list(key = "whiff", label = "Whiff Freq"),
    list(key = "chase", label = "Chase Freq")
  )
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
      data = heatmap_strike_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.45
    ) +
    geom_rect(
      data = heatmap_heart_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "#2FA84F", linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_segment(
      data = heatmap_home_plate_segments,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = 0.35
    ) +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE)
  dens_dd <- prepare_density_heatmap_points(dd)
  if (!nrow(dens_dd)) return(base)
  ggplot(dens_dd, aes(HeatLocSide, HeatLocHeight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      n = 220,
      h = c(1.28, 1.18),
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
      data = heatmap_strike_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 0.45
    ) +
    geom_rect(
      data = heatmap_heart_zone,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, colour = "#2FA84F", linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_segment(
      data = heatmap_home_plate_segments,
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = 0.35
    ) +
    coord_fixed(xlim = c(-3, 3), ylim = c(0, 5), expand = FALSE)
}

spread_duplicate_values <- function(v, group_key, amount){
  if (!length(v) || !is.finite(amount) || amount <= 0) return(v)
  grp <- ave(v, group_key, FUN = seq_along)
  grp_n <- ave(v, group_key, FUN = length)
  v + ((grp - (grp_n + 1) / 2) / pmax(grp_n, 1)) * amount
}

prepare_density_heatmap_points <- function(dd, duplicate_spread = c(0.008, 0.01)){
  if (!nrow(dd)) return(tibble::tibble())
  dd <- dd %>%
    dplyr::filter(is.finite(HeatLocSide), is.finite(HeatLocHeight))
  if (nrow(dd) < 2L) return(tibble::tibble())
  dup_key <- paste(round(dd$HeatLocSide, 4), round(dd$HeatLocHeight, 4))
  dd %>%
    dplyr::mutate(
      HeatLocSide = spread_duplicate_values(HeatLocSide, dup_key, duplicate_spread[[1]]),
      HeatLocHeight = spread_duplicate_values(HeatLocHeight, dup_key, duplicate_spread[[2]])
    ) %>%
    dplyr::filter(
      dplyr::between(HeatLocSide, -3, 3),
      dplyr::between(HeatLocHeight, 0, 5)
    ) %>%
    {
      if (nrow(.) < 2L ||
          dplyr::n_distinct(round(.$HeatLocSide, 4)) < 2L ||
          dplyr::n_distinct(round(.$HeatLocHeight, 4)) < 2L) {
        tibble::tibble()
      } else {
        .
      }
    }
}

spray_result_bucket <- function(pr_vec){
  dplyr::case_when(
    grepl("(?i)home\\s*run|homerun|\\bHR\\b", nz_chr(pr_vec), perl = TRUE) ~ "hit",
    grepl("(?i)\\bsingle\\b|\\b1B\\b|\\bdouble\\b|\\b2B\\b|\\btriple\\b|\\b3B\\b", nz_chr(pr_vec), perl = TRUE) ~ "hit",
    grepl("(?i)out|fielder|fielders\\s*choice|fielderschoice|force|sacrifice", nz_chr(pr_vec), perl = TRUE) ~ "out",
    TRUE ~ "other"
  )
}

compact_spray_data <- function(d, bucket = c("ev95_plus","under85")){
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
  ev_vec <- if ("ev" %in% names(d)) to_num(d$ev) else rep(NA_real_, n0)
  la_vec <- if ("la" %in% names(d)) to_num(d$la) else rep(NA_real_, n0)
  hit_type <- coalesce_cols_safe(d, c("TaggedHitType","AutoHitType","HitType","BattedBallType"), as = "character")
  
  spray <- d %>%
    dplyr::mutate(
      plot_x = dplyr::if_else(use_tm, dist * sin(bear * pi / 180), hx),
      plot_y = dplyr::if_else(use_tm, dist * cos(bear * pi / 180), hy),
      bip_flag = is_bip_txst(pc, pr),
      ev_bucket = dplyr::case_when(
        bip_flag %in% TRUE & is.finite(ev_vec) & ev_vec >= 95 ~ "ev95_plus",
        bip_flag %in% TRUE & is.finite(ev_vec) & ev_vec < 85 ~ "under85",
        TRUE ~ NA_character_
      ),
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
    dplyr::filter(bip_flag %in% TRUE, ev_bucket == bucket, is.finite(plot_x), is.finite(plot_y))
  spray
}

spray_zone_polygons <- function(infield_r = 150){
  wall_radius <- function(ang){
    anchor_ang <- c(-45, -22.5, 0, 22.5, 45)
    anchor_r <- c(330, 365, 400, 365, 330)
    approx(anchor_ang, anchor_r, xout = ang, rule = 2)$y
  }
  wedge <- function(zone, area, a0, a1, r0, r1_fun){
    ang <- seq(a0, a1, length.out = 40)
    outer <- r1_fun(ang)
    inner <- rep(r0, length(ang))
    tibble::tibble(
      Zone = zone,
      Area = area,
      ang = c(ang, rev(ang)),
      r = c(outer, rev(inner))
    ) %>%
      dplyr::mutate(
        x = r * sin(ang * pi / 180),
        y = r * cos(ang * pi / 180)
      )
  }
  dplyr::bind_rows(
    wedge("IF1", "Infield", -45.0, -22.5, 0, function(ang) rep(infield_r, length(ang))),
    wedge("IF2", "Infield", -22.5, 0.0, 0, function(ang) rep(infield_r, length(ang))),
    wedge("IF3", "Infield", 0.0, 22.5, 0, function(ang) rep(infield_r, length(ang))),
    wedge("IF4", "Infield", 22.5, 45.0, 0, function(ang) rep(infield_r, length(ang))),
    wedge("OF1", "Outfield", -45.0, -15.0, infield_r, wall_radius),
    wedge("OF2", "Outfield", -15.0, 15.0, infield_r, wall_radius),
    wedge("OF3", "Outfield", 15.0, 45.0, infield_r, wall_radius)
  )
}

spray_zone_counts <- function(spray, infield_r = 150){
  zones <- c("IF1", "IF2", "IF3", "IF4", "OF1", "OF2", "OF3")
  if (!nrow(spray)) {
    return(tibble::tibble(Zone = zones, n = 0L))
  }
  dist <- sqrt(spray$plot_x^2 + spray$plot_y^2)
  ang <- atan2(spray$plot_x, spray$plot_y) * 180 / pi
  ang <- pmin(pmax(ang, -45), 45)
  if_zone <- cut(ang, breaks = c(-45, -22.5, 0, 22.5, 45),
                 labels = c("IF1", "IF2", "IF3", "IF4"), include.lowest = TRUE)
  of_zone <- cut(ang, breaks = c(-45, -15, 15, 45),
                 labels = c("OF1", "OF2", "OF3"), include.lowest = TRUE)
  zone <- as.character(ifelse(dist <= infield_r, as.character(if_zone), as.character(of_zone)))
  tibble::tibble(Zone = zones) %>%
    dplyr::left_join(tibble::tibble(Zone = zone) %>% dplyr::count(Zone, name = "n"), by = "Zone") %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0L))
}

spray_zone_shading <- function(spray){
  zones <- spray_zone_polygons()
  counts <- spray_zone_counts(spray)
  max_n <- max(counts$n, na.rm = TRUE)
  min_n <- min(counts$n, na.rm = TRUE)
  pal <- grDevices::colorRamp(c("#E15759", "#FFFFFF", "#2FA84F"), space = "rgb")
  counts <- counts %>%
    dplyr::mutate(
      scaled = dplyr::case_when(
        max_n <= 0 ~ NA_real_,
        max_n == min_n ~ 1,
        TRUE ~ (n - min_n) / (max_n - min_n)
      )
    )
  counts$fill <- "#FFFFFF"
  ok <- is.finite(counts$scaled)
  if (any(ok)) {
    counts$fill[ok] <- grDevices::rgb(pal(pmin(pmax(counts$scaled[ok], 0), 1)) / 255)
  }
  zones %>% dplyr::left_join(counts, by = "Zone")
}

spray_infield_divider <- function(r = 150){
  ang <- seq(-45, 45, length.out = 100)
  tibble::tibble(
    x = r * sin(ang * pi / 180),
    y = r * cos(ang * pi / 180)
  )
}

spray_cell_plot <- function(d, bucket = c("ev95_plus","under85")){
  bucket <- match.arg(bucket)
  spray <- compact_spray_data(d, bucket)
  fld <- make_field_layers()
  zone_shade <- spray_zone_shading(spray)
  p <- ggplot() +
    geom_polygon(data = fld$track_poly, aes(x = x, y = y), fill = "white", color = NA) +
    geom_polygon(data = zone_shade, aes(x = x, y = y, group = Zone, fill = fill),
                 color = NA, alpha = 0.45) +
    geom_path(data = fld$wall, aes(x = x, y = y), color = TXST_MAROON, linewidth = 0.35) +
    geom_segment(data = fld$foul_lines, aes(x = x, y = y, xend = xend, yend = yend),
                 color = TXST_MAROON, linewidth = 0.25) +
    geom_path(data = spray_infield_divider(), aes(x = x, y = y),
              color = "black", linewidth = 0.35) +
    geom_path(data = fld$rings, aes(x = x, y = y, group = r),
              linetype = "dashed", color = "grey55", linewidth = 0.18) +
    geom_path(data = fld$bases_diamond, aes(x = x, y = y), color = "black", linewidth = 0.25) +
    coord_fixed(xlim = c(-230, 230), ylim = c(0, 420), expand = FALSE) +
    theme_void(base_size = 6) +
    theme(
      panel.border = element_rect(color = "#1D1D1D", fill = NA, linewidth = 0.35),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_identity()
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
  heat_events <- prepare_heatmap_events(ds)
  meta <- hitter_report_meta(d, "Total", player_order)
  side_lookup <- stats::setNames(meta$Side, meta$Hitter)
  metric_specs <- heatmap_metric_specs()
  headers <- c("", vapply(metric_specs, `[[`, character(1), "label"))
  name_cex <- 0.62
  header_cex <- 0.62
  
  grobs <- lapply(headers, text_cell_grob, fill = TXST_MAROON, col = TXST_GOLD, cex = header_cex)
  for (player in row_players) {
    if (is.na(player)) {
      grobs <- c(grobs, replicate(length(headers), blank_heat_grid_grob(), simplify = FALSE))
    } else {
      hp_player <- heat_events %>% dplyr::filter(Hitter == player)
      grobs <- c(
        grobs,
        list(text_cell_grob(player, fill = "#FBFAF7", col = player_name_color(side_lookup[[player]] %||% "R"), cex = name_cex)),
        lapply(metric_specs, function(spec) {
          ggplotGrob(heatmap_cell_plot(heatmap_metric_filter(hp_player, spec$key)))
        })
      )
    }
  }
  
  grid_body <- gridExtra::arrangeGrob(
    grobs = grobs,
    ncol = length(headers),
    widths = grid::unit(c(0.92, rep(1, length(headers) - 1L)), "null"),
    heights = grid::unit(c(0.20, rep(1, 9)), "null"),
    padding = grid::unit(1.5, "pt")
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
  as_inset_page_plot(page)
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





# -------------------- Pitcher card helpers (from app.R) --------------------
pitcher_env <- new.env(parent = globalenv())
pitcher_env$BASE_SCOUTING_EMBEDDED <- SCOUTING_EMBEDDED_MODE
pitcher_env$BASE_SCOUTING_APP_ROOT <- APP_ROOT
pitcher_env$BASE_SCOUTING_DATA_DIR <- DATA_DIR
pitcher_env$BASE_SCOUTING_OUTPUT_DIR <- get0(
  "BASE_SCOUTING_OUTPUT_DIR",
  envir = environment(),
  inherits = FALSE,
  ifnotfound = file.path(APP_ROOT, "outputs")
)
local({
  #Pitcher single page reports, for scouting or portal eval
  suppressPackageStartupMessages({
    library(shiny)
    if (!isTRUE(get0("BASE_SCOUTING_EMBEDDED", inherits = TRUE, ifnotfound = FALSE))) {
      library(tidyverse)
      library(readxl)
    }
    library(ggplot2)
    library(patchwork)
    library(gridExtra)
    library(ggplotify)
    library(grid)
    library(scales)
    library(png)
    library(cowplot)  # for watermark composition
  })
  
  # ---- Paths ----
  APP_ROOT <- get0(
    "BASE_SCOUTING_APP_ROOT",
    inherits = TRUE,
    ifnotfound = normalizePath(".", winslash = "/", mustWork = FALSE)
  )
  DATA_DIR <- get0(
    "BASE_SCOUTING_DATA_DIR",
    inherits = TRUE,
    ifnotfound = file.path(APP_ROOT, "data")
  )
  OUTPUT_DIR <- get0(
    "BASE_SCOUTING_OUTPUT_DIR",
    inherits = TRUE,
    ifnotfound = file.path(APP_ROOT, "outputs")
  )
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # ---- FTP config ----
  FTP_BASE_PATH <- Sys.getenv("TRACKMAN_FTP_BASE", unset = "v3")
  FTP_DEBUG     <- nzchar(Sys.getenv("TRACKMAN_FTP_DEBUG", unset = "")) &&
    Sys.getenv("TRACKMAN_FTP_DEBUG") != "0"
  CURRENT_YEAR  <- as.integer(format(Sys.Date(), "%Y"))
  PREV_YEAR     <- CURRENT_YEAR - 1
  
  # ---- Theme/colors ----
  MAROON <- "#501214"
  GOLD1  <- "#F2E8C9"; GOLD2 <- "#B4975A"
  pitch_heat_palette <- c("#ffffff", "#1e90ff", "#90ee90", "#ffff00", "#ff0000", "#ff00ff")
  
  # -------------------- Pitch colors (single source of truth) --------------------
  pitch_colors <- c(
    "Fastball"  = "#FF0000",
    "Four-Seam" = "#FF0000",
    "Two-Seam"  = "#FF0000",
    "Sinker"    = "#FFA500",
    "Slider"    = "#FFFF00",
    "Sweeper"   = "#FFD700",
    "Curveball" = "#0000FF",
    "Changeup"  = "#008000",
    "Splitter"  = "#000080",
    "Cutter"    = "#000000",
    "Untagged"  = "#808080",
    "Undefined" = "#808080"
  )
  
  # ---- Helpers ----
  nz_or <- function(x, alt) {
    if (is.null(x) || length(x) == 0) return(alt)
    if (is.character(x) && (is.na(x[1]) || trimws(x[1]) == "")) return(alt)
    if (is.na(x[1])) return(alt)
    x
  }
  is_bad_pitch_type <- function(x) {
    if (length(x) == 0) return(logical(0))
    t <- toupper(trimws(as.character(x)))
    is.na(x) | t %in% c("", "NA", "N/A", "UNDEFINED", "UNTAGGED", "UNKNOWN", "OTHER")
  }
  coalesce_col <- function(df, candidates, .default = NA) {
    for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
    rep(.default, nrow(df))
  }
  
  # Map an observed tag to a palette key WITHOUT changing the tag itself
  palette_key_from_tag <- function(tag) {
    if (is.na(tag) || !nzchar(trimws(tag))) return("Untagged")
    t <- trimws(tag)
    
    # 1) exact literal match to palette names
    if (t %in% names(pitch_colors)) return(t)
    
    # 2) normalized literal match (ignore spaces/dashes/case)
    norm <- function(s) toupper(gsub("[^A-Z0-9]", "", s))
    keys_norm <- norm(names(pitch_colors))
    t_norm    <- norm(t)
    if (t_norm %in% keys_norm) return(names(pitch_colors)[match(t_norm, keys_norm)])
    
    # 3) common abbreviations -> palette keys
    dict <- c(
      "FF"="Four-Seam","FA"="Four-Seam","4S"="Four-Seam","4SEAM"="Four-Seam","4FB"="Four-Seam",
      "FOURSEAM"="Four-Seam","FOURSEAMFASTBALL"="Four-Seam","FB"="Fastball","FASTBALL"="Fastball",
      "FS"="Four-Seam",
      "2S"="Two-Seam","2SEAM"="Two-Seam","TWOOSEAM"="Two-Seam","FT"="Two-Seam",
      "SI"="Sinker","SNK"="Sinker","SINKER"="Sinker",
      "FC"="Cutter","CUT"="Cutter","CT"="Cutter",
      "SL"="Slider","SLDR"="Slider","SLIDER"="Slider",
      "SWE"="Sweeper","SWP"="Sweeper","SWEEPER"="Sweeper",
      "CU"="Curveball","CB"="Curveball","CURVE"="Curveball","CURVEBALL"="Curveball","KC"="Curveball","KNUCKLECURVE"="Curveball",
      "CH"="Changeup","CHG"="Changeup","CHANGE"="Changeup","CHANGEUP"="Changeup",
      "SPL"="Splitter","SPLIT"="Splitter","SPLITTER"="Splitter","SFF"="Splitter","SF"="Splitter",
      "UNK"="Untagged","UNDEFINED"="Untagged","UNTAGGED"="Untagged","UNKNOWN"="Untagged"
    )
    if (t_norm %in% names(dict)) return(dict[[t_norm]])
    
    "Untagged"
  }
  
  # Given the unique tags present, build a named color vector (names = those tags)
  match_pitch_colors <- function(type_levels) {
    if (is.null(type_levels)) return(NULL)
    cols <- setNames(rep("#808080", length(type_levels)), type_levels)
    for (i in seq_along(type_levels)) {
      key <- palette_key_from_tag(type_levels[i]) # palette key (e.g., "Changeup")
      if (key %in% names(pitch_colors)) cols[i] <- pitch_colors[[key]]
    }
    cols
  }
  
  # ---- Robust file reader ----
  smart_read <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("xlsx","xls")) return(readxl::read_xlsx(path) |> tibble::as_tibble())
    .try <- function(expr) tryCatch(force(expr), error = identity)
    out <- .try(readr::read_csv(path, guess_max = 100000, show_col_types = FALSE))
    if (!inherits(out, "error") && ncol(out) > 1) return(out)
    for (enc in c("UTF-16LE","UTF-16BE","windows-1252","latin1","UTF-8")) {
      out <- .try(readr::read_csv(path, locale = readr::locale(encoding = enc),
                                  guess_max = 100000, show_col_types = FALSE))
      if (!inherits(out, "error") && ncol(out) > 1) return(out)
    }
    for (enc in c("UTF-16LE","UTF-8","latin1","windows-1252")) {
      out <- .try(readr::read_delim(path, delim = "\t",
                                    locale = readr::locale(encoding = enc),
                                    guess_max = 100000, show_col_types = FALSE))
      if (!inherits(out, "error") && ncol(out) > 1) return(out)
    }
    raw_sz <- file.info(path)$size
    if (is.finite(raw_sz) && !is.na(raw_sz) && raw_sz > 0) {
      raw <- readBin(path, what = "raw", n = raw_sz)
      raw <- raw[raw != as.raw(0x00)]
      tmp <- tempfile(fileext = ".csv")
      writeBin(raw, tmp)
      out <- .try(readr::read_csv(tmp, guess_max = 100000, show_col_types = FALSE))
      if (!inherits(out, "error") && ncol(out) > 1) return(out)
      out <- .try(readr::read_delim(tmp, delim = "\t", guess_max = 100000, show_col_types = FALSE))
      if (!inherits(out, "error") && ncol(out) > 1) return(out)
    }
    stop("Failed to read file. Please re-export as CSV (UTF-8) or XLSX.")
  }
  
  # ---- FTP helpers ----
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
  
  # ---- Standardize columns (KEEP TAGS AS-IS) ----
  standardize_tm <- function(df_raw) {
    df <- df_raw
    df$PitcherName   <- coalesce_col(df, c("Pitcher","PitcherName","PitcherFullName","Pitcher Last First"))
    df$PitcherThrows <- coalesce_col(df, c("PitcherThrows","Throws","PitcherSide","PitcherHand"))
    
    raw_pt <- coalesce_col(df, c(
      "TaggedPitchType","PitchType","Pitch_Type","Pitch Type","tagged_pitch_type",
      "AutoPitchType","AutoPitch"
    ))
    pt <- as.character(raw_pt)
    pt <- ifelse(is.na(pt) | trimws(pt) == "", "Untagged", trimws(pt))
    df$PitchType <- pt  # display exactly what was tagged (no relabeling)
    
    df$RelSpeed  <- suppressWarnings(as.numeric(coalesce_col(df, c("RelSpeed","Velocity","PitchVelocity","Velo","ReleaseVelo","RelSpeedMph"))))
    df$SpinRate  <- suppressWarnings(as.numeric(coalesce_col(df, c("SpinRate","Spin_Rate","Spin","SpinRate(RPM)","RawSpinRate"))))
    df$IVB <- suppressWarnings(as.numeric(coalesce_col(
      df, c("IVB","InducedVertBreak","InducedVerticalBreak","IVB_Inches","VBreak","VerticalBreak","IVB (in)"))))
    df$HB  <- suppressWarnings(as.numeric(coalesce_col(
      df, c("HB","HorizontalBreak","HorzBreak","HorizBreak","Horizontal Break","HBreak","HMov",
            "HorizontalMovement","Horz Movement","HorzBreak_Inches","HB_Inches"))))
    df$VAA <- suppressWarnings(as.numeric(coalesce_col(
      df, c("VertApprAngle","VerticalApproachAngle","VAA","Vert_Appr_Angle","Vertical Approach Angle"))))
    
    df$RelHeight <- suppressWarnings(as.numeric(coalesce_col(df, c("RelHeight","ReleaseHeight","Release_Height"))))
    df$RelSide   <- suppressWarnings(as.numeric(coalesce_col(df, c("RelSide","ReleaseSide","Release_Side"))))
    df$Extension <- suppressWarnings(as.numeric(coalesce_col(df, c("Extension","Ext","ReleaseExtension","Release_Extension"))))
    df$PlateX    <- suppressWarnings(as.numeric(coalesce_col(df, c("PlateLocSide","plate_x","PlateX","px","PlateSide"))))
    df$PlateZ    <- suppressWarnings(as.numeric(coalesce_col(df, c("PlateLocHeight","plate_z","PlateZ","pz","PlateHeight"))))
    
    df$BatterSide <- toupper(coalesce_col(df, c("BatterSide","HitterSide","Stand","BatterStand")))
    df$BatterSide[df$BatterSide %in% c("L","LEFT")]  <- "L"
    df$BatterSide[df$BatterSide %in% c("R","RIGHT")] <- "R"
    df$BatterSide[!df$BatterSide %in% c("L","R")] <- NA
    
    Balls    <- coalesce_col(df, c("Balls","CountBalls"))
    Strikes  <- coalesce_col(df, c("Strikes","CountStrikes"))
    CountStr <- coalesce_col(df, c("Count","CountString","PitchCount"))
    if (all(is.na(Balls)) || all(is.na(Strikes))) {
      if (!all(is.na(CountStr))) {
        parsed <- stringr::str_match(CountStr, "^(\\d)\\s*[-:]\\s*(\\d)$")
        df$BallsBefore   <- suppressWarnings(as.numeric(parsed[,2]))
        df$StrikesBefore <- suppressWarnings(as.numeric(parsed[,3]))
      } else {
        df$BallsBefore <- NA_real_; df$StrikesBefore <- NA_real_
      }
    } else {
      df$BallsBefore   <- suppressWarnings(as.numeric(Balls))
      df$StrikesBefore <- suppressWarnings(as.numeric(Strikes))
    }
    
    df$PitcherThrows <- toupper(as.character(df$PitcherThrows))
    df$PitcherThrows[df$PitcherThrows %in% c("L", "LEFT", "LH", "LHP", "LEFTY")] <- "LHP"
    df$PitcherThrows[df$PitcherThrows %in% c("R", "RIGHT", "RH", "RHP", "RIGHTY")] <- "RHP"
    df$PitcherThrows[!df$PitcherThrows %in% c("LHP","RHP")] <- NA
    df$RelSide <- ifelse(df$PitcherThrows == "RHP" & is.finite(df$RelSide) & df$RelSide < 0, 0, df$RelSide)
    
    df
  }
  
  # ---- Tables ----
  summarize_movement <- function(dfp) {
    dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType)) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        `Avg Velo` = mean(RelSpeed, na.rm = TRUE),
        `Max Velo` = suppressWarnings(max(RelSpeed, na.rm = TRUE)),
        `Spin`     = mean(SpinRate, na.rm = TRUE),
        `IVB`      = mean(IVB, na.rm = TRUE),
        `HB`       = mean(HB, na.rm = TRUE),
        `Rel Ht`   = mean(RelHeight, na.rm = TRUE),
        `Ext`      = mean(Extension, na.rm = TRUE),
        `IZ-VAA`   = ifelse(
          sum(
            is.finite(PlateX) & is.finite(PlateZ) &
              dplyr::between(PlateX, -0.708, 0.708) &
              dplyr::between(PlateZ, 1.5, 3.5) &
              is.finite(VAA),
            na.rm = TRUE
          ) > 0,
          mean(
            VAA[
              is.finite(PlateX) & is.finite(PlateZ) &
                dplyr::between(PlateX, -0.708, 0.708) &
                dplyr::between(PlateZ, 1.5, 3.5)
            ],
            na.rm = TRUE
          ),
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))
  }
  valid_pitch_types_for_tables <- function(dfp) {
    dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType)) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        has_defined_metrics = any(
          is.finite(RelSpeed) |
            is.finite(SpinRate) |
            is.finite(IVB) |
            is.finite(HB) |
            is.finite(RelHeight) |
            is.finite(Extension) |
            is.finite(VAA) |
            (is.finite(PlateX) & is.finite(PlateZ)),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      dplyr::filter(has_defined_metrics) %>%
      dplyr::pull(PitchType) %>%
      as.character()
  }
  format_movetable <- function(tbl) {
    tbl %>%
      dplyr::mutate(Velo = ifelse(is.na(`Avg Velo`), NA_character_,
                                  sprintf("%.1f (%.1f)", `Avg Velo`, `Max Velo`))) %>%
      dplyr::select(Pitch = PitchType, Velo, Spin = `Spin`, IVB = `IVB`, HB = `HB`, `Rel Ht`, Ext, `IZ-VAA`) %>%
      dplyr::mutate(
        dplyr::across(c(Spin), ~scales::comma(.x, accuracy = 1)),
        dplyr::across(c(IVB, HB, `Rel Ht`, Ext, `IZ-VAA`), ~scales::number(.x, accuracy = 0.1))
      )
  }
  pitcher_pitch_call <- function(dfp) {
    pc <- coalesce_col(dfp, c("pitch_call", "PitchCall", "Pitch_Call", "Call", "PitchResult", "Pitch_Result"))
    if (exists("canon_pitch_call", mode = "function")) return(canon_pitch_call(pc))
    as.character(pc)
  }
  pitcher_zone_flag <- function(dfp) {
    is.finite(dfp$PlateX) & is.finite(dfp$PlateZ) &
      dplyr::between(dfp$PlateX, -0.708, 0.708) &
      dplyr::between(dfp$PlateZ, 1.5, 3.5)
  }
  pitch_results_table_raw <- function(dfp) {
    dfp <- dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType)) %>%
      dplyr::mutate(
        PitchCallStd = pitcher_pitch_call(.),
        in_zone = pitcher_zone_flag(.),
        has_zone = is.finite(PlateX) & is.finite(PlateZ),
        two_k = !is.na(StrikesBefore) & StrikesBefore >= 2,
        is_strike = PitchCallStd %in% c("StrikeCalled", "StrikeSwinging", "FoulBall",
                                        "FoulBallFieldable", "FoulBallNotFieldable",
                                        "FoulTip", "InPlay", "InPlayOut", "InPlayNoOut"),
        is_swing = PitchCallStd %in% c("StrikeSwinging", "FoulBall", "FoulBallFieldable",
                                       "FoulBallNotFieldable", "FoulTip", "InPlay",
                                       "InPlayOut", "InPlayNoOut"),
        is_whiff = PitchCallStd == "StrikeSwinging"
      )
    total_pitches <- nrow(dfp)
    dfp %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        `Usage%` = ifelse(total_pitches > 0, dplyr::n() / total_pitches, NA_real_),
        `Strike%` = ifelse(dplyr::n() > 0, sum(is_strike, na.rm = TRUE) / dplyr::n(), NA_real_),
        `Zone%` = ifelse(sum(has_zone, na.rm = TRUE) > 0,
                         sum(in_zone, na.rm = TRUE) / sum(has_zone, na.rm = TRUE), NA_real_),
        `2k Zone%` = ifelse(sum(two_k & has_zone, na.rm = TRUE) > 0,
                            sum(two_k & in_zone, na.rm = TRUE) / sum(two_k & has_zone, na.rm = TRUE), NA_real_),
        `Chase%` = ifelse(sum(has_zone & !in_zone, na.rm = TRUE) > 0,
                          sum(is_swing & has_zone & !in_zone, na.rm = TRUE) /
                            sum(has_zone & !in_zone, na.rm = TRUE), NA_real_),
        `Whiff%` = ifelse(sum(is_swing, na.rm = TRUE) > 0,
                          sum(is_whiff, na.rm = TRUE) / sum(is_swing, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(`Usage%`))
  }
  format_pitch_results_table <- function(tbl) {
    tbl %>%
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) scales::percent(x, accuracy = 0.1))) %>%
      dplyr::rename(Pitch = PitchType)
  }
  
  usage_table_raw <- function(dfp) {
    dfp <- dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType)) %>%
      dplyr::mutate(
      is_L = BatterSide == "L",
      is_R = BatterSide == "R",
      is_FP = BallsBefore == 0 & StrikesBefore == 0,
      is_2K = !is.na(StrikesBefore) & StrikesBefore >= 2,
      CountStr = paste0(BallsBefore,"-",StrikesBefore),
      is_hitter_ahead = !is.na(BallsBefore) & !is.na(StrikesBefore) &
        CountStr %in% c("1-0","2-0","3-0","2-1","3-1"),
      is_pitcher_ahead = !is.na(BallsBefore) & !is.na(StrikesBefore) &
        CountStr %in% c("0-1","0-2","1-2")
    )
    tot_all <- nrow(dfp)
    tot_L   <- sum(dfp$is_L, na.rm = TRUE)
    tot_R   <- sum(dfp$is_R, na.rm = TRUE)
    tot_FP  <- sum(dfp$is_FP, na.rm = TRUE)
    tot_2K  <- sum(dfp$is_2K, na.rm = TRUE)
    tot_HA  <- sum(dfp$is_hitter_ahead, na.rm = TRUE)
    tot_PA  <- sum(dfp$is_pitcher_ahead, na.rm = TRUE)
    
    dfp %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        Overall     = ifelse(tot_all > 0, n()/tot_all, NA_real_),
        `v LHH`     = ifelse(tot_L   > 0, sum(is_L, na.rm = TRUE)/tot_L, NA_real_),
        `v RHH`     = ifelse(tot_R   > 0, sum(is_R, na.rm = TRUE)/tot_R, NA_real_),
        `1st Pitch` = ifelse(tot_FP  > 0, sum(is_FP, na.rm = TRUE)/tot_FP, NA_real_),
        `2 Strikes` = ifelse(tot_2K  > 0, sum(is_2K, na.rm = TRUE)/tot_2K, NA_real_),
        `Hitter Ahead` = ifelse(tot_HA > 0, sum(is_hitter_ahead, na.rm = TRUE)/tot_HA, NA_real_),
        `Pitcher Ahead` = ifelse(tot_PA > 0, sum(is_pitcher_ahead, na.rm = TRUE)/tot_PA, NA_real_),
        .groups = "drop"
      )
  }
  format_usage_table <- function(ut_raw) {
    ut_raw %>%
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) scales::percent(x, accuracy = 0.1))) %>%
      dplyr::arrange(dplyr::desc(Overall)) %>%
      dplyr::rename(Pitch = PitchType)
  }
  format_usage_table_noreorder <- function(ut_raw) {
    ut_raw %>%
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) scales::percent(x, accuracy = 0.1))) %>%
      dplyr::rename(Pitch = PitchType)
  }
  
  # One green + one red per numeric column (skip ties / all-equal)
  make_shade_matrix <- function(df_numeric, exclude_cols = "PitchType") {
    nm <- setdiff(names(df_numeric), exclude_cols)
    sh <- matrix("", nrow(df_numeric), ncol(df_numeric),
                 dimnames = list(NULL, names(df_numeric)))
    for (v in nm) {
      col <- df_numeric[[v]]
      if (!is.numeric(col)) next
      ok <- which(is.finite(col))
      if (length(ok) < 2) next
      if (length(unique(col[ok])) == 1) next
      imax <- ok[which.max(col[ok])]
      imin <- ok[which.min(col[ok])]
      if (!is.na(imax) && !is.na(imin) && imax != imin) {
        sh[imax, v] <- "max"
        sh[imin, v] <- "min"
      }
    }
    sh
  }
  
  # ---------- Logos / watermark ----------
  find_logo <- function(base) {
    if (grepl("^Gozone", base, ignore.case = FALSE)) {
      cands <- c(paste0(base, ".jpg"), paste0(base, ".jpeg"),
                 paste0(base, ".png"), paste0(tolower(base), ".png"),
                 paste0(base, ".PNG"))
    } else {
      cands <- c(paste0(base, ".png"), paste0(tolower(base), ".png"),
                 paste0(base, ".PNG"), paste0(base, ".jpg"), paste0(base, ".jpeg"))
    }
    for (fn in cands) {
      fp <- file.path(APP_ROOT, "www", fn)
      if (file.exists(fp)) return(fp)
    }
    NULL
  }
  load_logo <- function(base) {
    fp <- find_logo(base)
    if (is.null(fp)) return(NULL)
    ext <- tolower(tools::file_ext(fp))
    if (ext == "png") return(png::readPNG(fp))
    if (ext %in% c("jpg","jpeg") && requireNamespace("jpeg", quietly = TRUE))
      return(jpeg::readJPEG(fp))
    NULL
  }
  
  # 90% transparent watermark BEHIND the full page (no layout changes)
  add_watermark <- function(p) {
    img <- load_logo("bobcatlogo")
    if (is.null(img)) return(p)
    
    # Ensure RGBA and set alpha to 0.1 (90% transparent)
    if (is.array(img)) {
      d  <- dim(img); h <- d[1]; w <- d[2]; ch <- if (length(d) >= 3) d[3] else 1
      if (ch == 4) {
        img[,,4] <- img[,,4] * 0.1
      } else if (ch == 3) {
        alpha <- matrix(0.1, nrow = h, ncol = w)
        img   <- array(c(img[,,1], img[,,2], img[,,3], alpha), dim = c(h, w, 4))
      } else { # grayscale
        g     <- img[,,1]
        alpha <- matrix(0.1, nrow = h, ncol = w)
        img   <- array(c(g, g, g, alpha), dim = c(h, w, 4))
      }
    }
    
    # Full-page raster grob
    bg_grob <- grid::rasterGrob(
      img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
      interpolate = TRUE
    )
    
    # Compose: watermark first (background), then full page on top
    cowplot::ggdraw() +
      cowplot::draw_grob(bg_grob, x = 0.5, y = 0.5, width = 1, height = 1,
                         hjust = 0.5, vjust = 0.5) +
      cowplot::draw_plot(p, x = 0, y = 0, width = 1, height = 1)
  }
  image_panel_plot <- function(base, title = NULL) {
    img <- load_logo(base)
    if (is.null(img)) return(NULL)
    p <- ggplotify::as.ggplot(function() {
      grid::grid.rect(gp = grid::gpar(col = NA, fill = NA))
      grid::grid.raster(
        img,
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        width = unit(1, "npc"),
        height = unit(1, "npc"),
        interpolate = TRUE
      )
    }) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
    if (!is.null(title)) {
      p <- p + ggtitle(title) +
        theme(
          plot.title = element_text(face = "bold", color = MAROON, hjust = 0, size = 10.5),
          plot.margin = margin(0, 0, 0, 0)
        )
    }
    p
  }
  shrink_plot <- function(p, scale = 1, hjust = 0.5, vjust = 0.5) {
    scale <- max(min(scale, 1), 0.01)
    x <- max(min((1 - scale) * hjust, 1 - scale), 0)
    y <- max(min((1 - scale) * vjust, 1 - scale), 0)
    cowplot::ggdraw() +
      cowplot::draw_plot(p, x = x, y = y, width = scale, height = scale)
  }
  split_side_from_label <- function(split_label) {
    if (identical(split_label, "vLHH")) return("L")
    if (identical(split_label, "vRHH")) return("R")
    "Total"
  }
  gozone_plot <- function(side) {
    if (identical(side, "L")) return(image_panel_plot("GozoneLHH") + theme(plot.margin = margin(0, 0, 0, 0)))
    if (identical(side, "R")) return(image_panel_plot("GozoneRHH") + theme(plot.margin = margin(0, 0, 0, 0)))
    NULL
  }
  section_title_plot <- function(title_text, size = 10.5, margins = margin(0, 6, 0, 6)) {
    ggplotify::as.ggplot(function() {
      grid::grid.rect(gp = grid::gpar(col = NA, fill = NA))
      grid::grid.text(
        title_text,
        x = unit(0, "npc"),
        y = unit(0.5, "npc"),
        just = c("left", "center"),
        gp = grid::gpar(fontface = "bold", col = MAROON, cex = size / 11)
      )
    }) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margins
      )
  }
  stack_with_title <- function(body_plot, title_text, title_size = 10.5,
                               title_margins = margin(0, 6, 2, 6),
                               title_height = 0.08) {
    section_title_plot(title_text, size = title_size, margins = title_margins) /
      body_plot +
      patchwork::plot_layout(heights = c(title_height, 1 - title_height))
  }
  pitch_usage_order <- function(dfp) {
    dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType)) %>%
      dplyr::count(PitchType, sort = TRUE, name = "pitch_n") %>%
      dplyr::pull(PitchType) %>%
      as.character()
  }
  order_pitch_rows <- function(tbl, pitch_col = "Pitch", pitch_order = NULL) {
    if (is.null(pitch_order) || !length(pitch_order) || !(pitch_col %in% names(tbl))) return(tbl)
    keep_order <- unique(c(pitch_order, setdiff(as.character(tbl[[pitch_col]]), pitch_order)))
    tbl %>%
      dplyr::mutate(`__pitch_order` = match(as.character(.data[[pitch_col]]), keep_order)) %>%
      dplyr::arrange(`__pitch_order`) %>%
      dplyr::select(-`__pitch_order`)
  }
  pitch_type_text_matrix <- function(tbl, pitch_col = "Pitch") {
    out <- matrix("#000000", nrow = nrow(tbl), ncol = ncol(tbl),
                  dimnames = list(NULL, names(tbl)))
    if (!nrow(tbl) || !(pitch_col %in% names(tbl))) return(out)
    row_cols <- unname(match_pitch_colors(as.character(tbl[[pitch_col]])))
    row_cols[is.na(row_cols) | !nzchar(row_cols)] <- "#000000"
    for (i in seq_len(nrow(out))) out[i, ] <- row_cols[i]
    out
  }
  pitch_type_highlight_matrix <- function(tbl, pitch_col = "Pitch") {
    out <- matrix(FALSE, nrow = nrow(tbl), ncol = ncol(tbl),
                  dimnames = list(NULL, names(tbl)))
    if (!nrow(tbl) || !(pitch_col %in% names(tbl))) return(out)
    pitch_keys <- vapply(as.character(tbl[[pitch_col]]), palette_key_from_tag, character(1))
    slider_rows <- which(pitch_keys == "Slider")
    if (length(slider_rows)) out[slider_rows, ] <- TRUE
    out
  }
  
  # ---- Plots ----
  inferred_shoulder_height <- function(extension = NA_real_){
    5.0
  }
  arm_slot_segment <- function(dfp) {
    throws <- dfp$PitcherThrows %>%
      as.character() %>%
      stats::na.omit() %>%
      trimws()
    throws <- throws[nzchar(throws)]
    throws <- if (length(throws)) names(sort(table(throws), decreasing = TRUE))[1] else ""
    rel_ht_mean <- suppressWarnings(mean(dfp$RelHeight, na.rm = TRUE))
    rel_side_mean <- suppressWarnings(mean(abs(dfp$RelSide), na.rm = TRUE))
    shoulder_ht <- inferred_shoulder_height()
    if (!is.finite(rel_ht_mean) || !is.finite(rel_side_mean) || !is.finite(shoulder_ht) || !nzchar(throws)) return(NULL)
    vertical_gap <- abs(shoulder_ht - rel_ht_mean)
    angle <- atan2(vertical_gap, rel_side_mean)
    if (!is.finite(angle)) return(NULL)
    ray_len <- 25
    x_sign <- if (throws == "LHP") -1 else 1
    xend <- x_sign * ray_len * cos(angle)
    yend <- ray_len * sin(angle)
    data.frame(x = 0, y = 0, xend = xend, yend = yend)
  }
  release_point_plot <- function(dfp) {
    all_levels <- sort(unique(stats::na.omit(dfp$PitchType)))
    d_pts <- dfp %>%
      dplyr::filter(!is_bad_pitch_type(PitchType), is.finite(RelSide), is.finite(RelHeight))
    if (length(all_levels)) {
      d_pts$PitchType <- factor(d_pts$PitchType, levels = all_levels)
    }
    cols <- match_pitch_colors(all_levels)
    avg_pts <- d_pts %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        RelSide = mean(RelSide, na.rm = TRUE),
        RelHeight = mean(RelHeight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight))
    rubber_half_width_ft <- 8.5 / 12
    rubber_center_height_ft <- 10 / 12
    rubber_half_thickness_ft <- 0.05
    mound_half_width_ft <- max(1, rubber_half_width_ft + 0.12)
    mound_peak_height_ft <- 9.25 / 12
    mound_side_span_ft <- mound_half_width_ft - rubber_half_width_ft
    theta_left <- seq(pi, pi / 2, length.out = 100)
    theta_right <- seq(pi / 2, 0, length.out = 100)
    mound_left_arc <- tibble::tibble(
      x = -rubber_half_width_ft + mound_side_span_ft * cos(theta_left),
      y = mound_peak_height_ft * sin(theta_left)
    )
    mound_top <- tibble::tibble(
      x = c(-rubber_half_width_ft, rubber_half_width_ft),
      y = c(mound_peak_height_ft, mound_peak_height_ft)
    )
    mound_right_arc <- tibble::tibble(
      x = rubber_half_width_ft + mound_side_span_ft * cos(theta_right),
      y = mound_peak_height_ft * sin(theta_right)
    )
    mound_outline <- dplyr::bind_rows(mound_left_arc, mound_top, mound_right_arc)
    mound_fill <- dplyr::bind_rows(
      mound_outline,
      tibble::tibble(x = c(mound_half_width_ft, -mound_half_width_ft), y = c(0, 0))
    )
    rubber <- tibble::tibble(
      xmin = -rubber_half_width_ft,
      xmax = rubber_half_width_ft,
      ymin = rubber_center_height_ft - rubber_half_thickness_ft,
      ymax = rubber_center_height_ft + rubber_half_thickness_ft
    )
    p <- ggplot() +
      geom_polygon(
        data = mound_fill,
        aes(x = x, y = y),
        inherit.aes = FALSE,
        fill = scales::alpha("#8B5A2B", 0.18),
        color = NA
      ) +
      geom_rect(
        data = rubber,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "#F2F2F2",
        color = "black",
        linewidth = 0.4
      ) +
      geom_path(
        data = mound_outline,
        aes(x = x, y = y),
        inherit.aes = FALSE,
        color = "#8B5A2B",
        linewidth = 1
      )
    if (nrow(d_pts)) {
      p <- p +
        geom_point(
          data = d_pts,
          aes(x = RelSide, y = RelHeight, fill = PitchType),
          shape = 21, size = 2.1, color = "black", stroke = 0.2, alpha = 0.35, na.rm = TRUE
        )
    }
    if (nrow(avg_pts)) {
      p <- p +
        geom_point(
          data = avg_pts,
          aes(x = RelSide, y = RelHeight, fill = PitchType),
          shape = 21, size = 4.8, color = "black", stroke = 0.65, alpha = 1, na.rm = TRUE
        )
    }
    if (!nrow(d_pts)) {
      p <- p + annotate("text", x = 0, y = 3.5, label = "No release data", size = 3.5)
    }
    p +
      annotate("text", x = 1, y = -0.28, label = "3B Side <--", size = 2.8, color = "#333333") +
      annotate("text", x = -1, y = -0.28, label = "--> 1B Side", size = 2.8, color = "#333333") +
      coord_fixed(xlim = c(4, -4), ylim = c(0, 7), expand = FALSE, clip = "off") +
      scale_x_continuous(
        breaks = NULL
      ) +
      scale_y_continuous(
        breaks = c(3, 4, 5, 6, 7),
        labels = c("3.0", "4.0", "5.0", "6.0", "7.0")
      ) +
      scale_fill_manual(values = cols, drop = FALSE, na.value = "#808080", guide = "none") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
        axis.title = element_blank(),
        axis.text = element_text(size = 8.2, color = "#333333"),
        axis.ticks = element_line(color = "#666666", linewidth = 0.25),
        plot.title = element_text(face = "bold", color = MAROON),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(4, 6, 16, 6)
      ) +
      ggtitle("Release (Hitter POV)")
  }
  movement_plot <- function(dfp, show_legend = TRUE) {
    all_levels <- sort(unique(stats::na.omit(dfp$PitchType)))
    dfp$PitchType <- factor(dfp$PitchType, levels = all_levels)
    cols <- match_pitch_colors(all_levels)
    pitch_counts <- dfp %>%
      dplyr::count(PitchType, name = "pitch_n") %>%
      dplyr::mutate(show_avg = pitch_n > 50)
    dfp <- dfp %>%
      dplyr::left_join(pitch_counts, by = "PitchType") %>%
      dplyr::mutate(
        point_alpha = ifelse(show_avg, 0.20, 0.65),
        point_size = ifelse(show_avg, 1.8, 2.4)
      )
    avg_pts <- dfp %>%
      dplyr::filter(show_avg, is.finite(HB), is.finite(IVB)) %>%
      dplyr::group_by(PitchType) %>%
      dplyr::summarise(
        HB = mean(HB, na.rm = TRUE),
        IVB = mean(IVB, na.rm = TRUE),
        .groups = "drop"
      )
    arm_seg <- arm_slot_segment(dfp)
    
    ggplot(dfp, aes(x = HB, y = IVB, fill = PitchType)) +
      geom_point(aes(alpha = point_alpha, size = point_size), shape = 21, color = "black", stroke = 0.2, na.rm = TRUE) +
      geom_hline(yintercept = 0, linewidth = 0.7, color = "black") +
      geom_vline(xintercept = 0, linewidth = 0.7, color = "black") +
      (if (!is.null(arm_seg)) geom_segment(
        data = arm_seg,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, linetype = "dashed", linewidth = 0.7, alpha = 0.9
      ) else NULL) +
      geom_point(
        data = avg_pts,
        aes(x = HB, y = IVB, fill = PitchType),
        shape = 21, size = 4.8, color = "black", stroke = 0.65, alpha = 1, na.rm = TRUE
      ) +
      coord_equal(xlim = c(-25, 25), ylim = c(-25, 25), expand = FALSE) +
      scale_x_continuous(breaks = seq(-20, 20, by = 10)) +
      scale_y_continuous(breaks = seq(-20, 20, by = 10)) +
      scale_fill_manual(values = cols, drop = FALSE, na.value = "#808080", name = "Pitch") +
      scale_alpha_identity() +
      scale_size_identity() +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
        axis.title  = element_text(size = 9, color = "#333333"),
        axis.text   = element_text(size = 8.2, color = "#333333"),
        axis.ticks  = element_line(color = "#666666", linewidth = 0.25),
        plot.title  = element_text(face = "bold", color = MAROON),
        legend.position = if (isTRUE(show_legend)) "left" else "none",
        legend.box.margin = margin(0, 4, 0, 0),
        legend.key.size = unit(10, "pt"),
        legend.text = element_text(size = 9),
        plot.background  = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(4, 6, 4, 6)
      ) +
      labs(x = "HB", y = "iVB") +
      ggtitle("Movement")
  }
  
  # Locations = scatter (catcher POV), color-coded circles per pitch type
  # Home plate (same shape as before, flipped tip DOWN but kept within zone area)
  home_plate_segments <- data.frame(
    x=c(0,0.71,0.71,0,-0.71,-0.71), y=c(0.50,0.50,0.35,0.15,0.35,0.50),
    xend=c(0.71,0.71,0,-0.71,-0.71,0), yend=c(0.50,0.35,0.15,0.35,0.50,0.50)
  )
  locations_scatter <- function(dfp, compact = FALSE, pitch_order = NULL) {
    loc_title <- if (isTRUE(compact)) NULL else "Locations / Frequency (Catcher POV)"
    if (isTRUE(compact)) {
      d_pts <- dfp %>%
        dplyr::filter(
          !is_bad_pitch_type(PitchType),
          is.finite(PlateX),
          is.finite(PlateZ)
        )
      all_levels <- unique(stats::na.omit(as.character(d_pts$PitchType)))
      if (!is.null(pitch_order) && length(pitch_order)) {
        all_levels <- c(intersect(pitch_order, all_levels), setdiff(all_levels, pitch_order))
      }
      if (!length(all_levels)) {
        p_empty <- ggplot() +
          theme_void() +
          theme(
            plot.background = element_rect(fill = NA, color = NA),
            plot.margin = margin(2, 4, 2, 4)
          ) +
          annotate("text", x = 0.5, y = 0.5, label = "No location data", size = 4)
        if (!is.null(loc_title)) {
          p_empty <- p_empty + ggtitle(loc_title) +
            theme(plot.title = element_text(face = "bold", color = MAROON, size = 10, hjust = 0))
        }
        return(p_empty)
      }
    } else {
      all_levels <- sort(unique(stats::na.omit(dfp$PitchType)))
      d_pts <- dplyr::filter(dfp, !is.na(PlateX), !is.na(PlateZ))
    }
    d_pts$PitchType <- factor(d_pts$PitchType, levels = all_levels)
    cols <- match_pitch_colors(all_levels)
    pitch_counts <- d_pts %>%
      dplyr::count(PitchType, name = "pitch_n") %>%
      dplyr::mutate(use_heat = pitch_n > 50)
    d_pts <- d_pts %>%
      dplyr::left_join(pitch_counts, by = "PitchType")
    d_heat <- d_pts %>%
      dplyr::filter(use_heat, dplyr::between(PlateX, -3, 3), dplyr::between(PlateZ, 0, 5))
    d_scatter <- d_pts %>% dplyr::filter(!use_heat)
    
    # Dummy to keep empty facets visible in the full card only
    dummy <- if (isTRUE(compact)) {
      NULL
    } else {
      tibble(PitchType = factor(all_levels, levels = all_levels), PlateX = NA_real_, PlateZ = NA_real_)
    }
    
    # Catcher's POV strike zone + plate (HittingApp style)
    zone <- data.frame(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5)
    
    nrows_facets <- if (isTRUE(compact)) 1 else if (length(all_levels) <= 5) 1 else 2
    facet_ncol <- if (isTRUE(compact)) length(all_levels) else NULL
    point_size <- if (isTRUE(compact)) 1.35 else 1.8
    base_size <- if (isTRUE(compact)) 10 else 12
    strip_size <- if (isTRUE(compact)) 8.2 else 10.5
    title_size <- if (isTRUE(compact)) 10 else 12
    
    p <- ggplot() +
      (if (!is.null(dummy)) geom_blank(data = dummy, aes(x = PlateX, y = PlateZ)) else NULL) +
      stat_density_2d(
        data = d_heat,
        aes(x = PlateX, y = PlateZ, fill = after_stat(ndensity)),
        geom = "raster",
        contour = FALSE,
        n = 160,
        na.rm = TRUE
      ) +
      geom_point(data = d_scatter,
                 aes(x = PlateX, y = PlateZ, color = PitchType),
                 size = point_size, alpha = 0.75, na.rm = TRUE) +
      facet_wrap(~ PitchType, nrow = nrows_facets,
                 ncol = facet_ncol, scales = "fixed", drop = FALSE) +
      scale_x_reverse(limits = c(1.8, -1.8), expand = c(0, 0)) +  # catcher POV
      scale_y_continuous(limits = c(0.0, 4.0),  expand = c(0, 0)) +
      scale_fill_gradientn(colors = pitch_heat_palette, guide = "none") +
      scale_color_manual(values = cols, drop = FALSE, guide = "none") +
      geom_rect(data = zone, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = "black", fill = NA, linewidth = 0.5) +
      geom_segment(data = home_plate_segments, inherit.aes = FALSE,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", linewidth = 0.9) +
      coord_fixed() +
      theme_minimal(base_size = base_size) +
      theme(
        strip.text = element_text(face = "bold", size = strip_size),
        panel.grid = element_blank(),
        axis.title  = element_blank(),
        axis.text   = element_blank(),
        axis.ticks  = element_blank(),
        legend.position = "none",
        plot.background  = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        plot.margin = if (isTRUE(compact)) margin(2, 4, 2, 4) else margin(4, 6, 4, 6)
      )
    if (!is.null(loc_title)) {
      p <- p + ggtitle(loc_title) +
        theme(plot.title = element_text(face = "bold", color = MAROON, size = title_size))
    }
    p
  }
  
  # ----- Tables (fit + shading) -----
  table_to_plot <- function(tbl, title = NULL, shade_matrix = NULL,
                            core_cex = 0.85, head_cex = 1.0,
                            title_size = 12, title_margin = 9,
                            col_widths = NULL,
                            text_color_matrix = NULL,
                            text_highlight_matrix = NULL) {
    tg <- gridExtra::tableGrob(
      tbl, rows = NULL,
      theme = gridExtra::ttheme_minimal(
        core = list(
          fg_params = list(cex = core_cex),
          bg_params = list(fill = rep(c("#FFFFFF","#F7F7F7"), length.out = nrow(tbl)))
        ),
        colhead = list(
          fg_params = list(fontface = 2, cex = head_cex, col = "white"),
          bg_params = list(fill = MAROON)
        )
      )
    )
    if (!is.null(col_widths) && length(col_widths) == ncol(tbl)) {
      tg$widths <- grid::unit(col_widths, "null")
    } else {
      tg$widths <- rep(unit(1, "null"), ncol(tg))
    }
    
    if (!is.null(shade_matrix)) {
      sh <- as.matrix(shade_matrix)
      if (all(dim(sh) == dim(as.matrix(tbl)))) {
        idx_bg <- which(tg$layout$name == "core-bg")
        if (length(idx_bg) == nrow(tbl) * ncol(tbl)) {
          lay <- tg$layout[idx_bg, c("t","l","b","r")]
          core_rows <- sort(unique(lay$t))
          core_cols <- sort(unique(lay$l))
          row_idx <- match(lay$t, core_rows)
          col_idx <- match(lay$l, core_cols)
          fill_for <- function(tag) dplyr::case_when(
            tag == "max" ~ "#BFEAD0",  # green
            tag == "min" ~ "#F4BFBF",  # red
            TRUE ~ NA_character_
          )
          for (k in seq_along(idx_bg)) {
            tag <- sh[row_idx[k], col_idx[k]]
            col <- fill_for(tag)
            if (!is.na(col)) {
              if (is.null(tg$grobs[[idx_bg[k]]]$gp)) tg$grobs[[idx_bg[k]]]$gp <- grid::gpar()
              tg$grobs[[idx_bg[k]]]$gp$fill <- col
            }
          }
        }
      }
    }
    if (!is.null(text_color_matrix)) {
      txt <- as.matrix(text_color_matrix)
      if (all(dim(txt) == dim(as.matrix(tbl)))) {
        idx_fg <- which(tg$layout$name == "core-fg")
        if (length(idx_fg) == nrow(tbl) * ncol(tbl)) {
          lay <- tg$layout[idx_fg, c("t", "l")]
          core_rows <- sort(unique(lay$t))
          core_cols <- sort(unique(lay$l))
          row_idx <- match(lay$t, core_rows)
          col_idx <- match(lay$l, core_cols)
          for (k in seq_along(idx_fg)) {
            col <- txt[row_idx[k], col_idx[k]]
            if (!is.na(col) && nzchar(col)) {
              if (is.null(tg$grobs[[idx_fg[k]]]$gp)) tg$grobs[[idx_fg[k]]]$gp <- grid::gpar()
              tg$grobs[[idx_fg[k]]]$gp$col <- col
            }
          }
        }
      }
    }
    if (!is.null(text_highlight_matrix)) {
      hi <- as.matrix(text_highlight_matrix)
      if (all(dim(hi) == dim(as.matrix(tbl)))) {
        idx_fg <- which(tg$layout$name == "core-fg")
        if (length(idx_fg) == nrow(tbl) * ncol(tbl)) {
          lay <- tg$layout[idx_fg, c("t", "l")]
          core_rows <- sort(unique(lay$t))
          core_cols <- sort(unique(lay$l))
          row_idx <- match(lay$t, core_rows)
          col_idx <- match(lay$l, core_cols)
          for (k in seq_along(idx_fg)) {
            if (isTRUE(hi[row_idx[k], col_idx[k]])) {
              main_grob <- tg$grobs[[idx_fg[k]]]
              highlight_grob <- grid::roundrectGrob(
                x = 0.5, y = 0.5,
                width = unit(0.90, "npc"),
                height = unit(0.68, "npc"),
                r = unit(0.08, "snpc"),
                gp = grid::gpar(fill = "#000000", col = NA)
              )
              tg$grobs[[idx_fg[k]]] <- grid::grobTree(highlight_grob, main_grob)
            }
          }
        }
      }
    }
    
    p <- ggplotify::as.ggplot(function() {
      pushViewport(viewport(width = unit(0.985, "npc"), height = unit(0.985, "npc")))
      grid::grid.draw(tg)
      popViewport()
    })
    p <- p + theme(
      plot.margin = margin(8, 6, 6, 6),
      plot.background = element_rect(fill = NA, color = NA)
    )
    p
  }
  
  # Title (no corner logos, just text; bumped down ~1/2")
  title_plot <- function(title_text, subtitle_text = NULL) {
    ggplotify::as.ggplot(function() {
      grid::grid.rect(gp = grid::gpar(col = NA, fill = NA))
      grid::grid.text(nz_or(title_text, "Unknown Pitcher"),
                      x = unit(0.04,"npc"), y = unit(0.62,"npc"),
                      just = c("left","center"),
                      gp = grid::gpar(fontface = "bold", col = MAROON, cex = 1.25))
      if (!is.null(subtitle_text) && nzchar(subtitle_text)) {
        grid::grid.text(subtitle_text,
                        x = unit(0.04,"npc"), y = unit(0.36,"npc"),
                        just = c("left","center"),
                        gp = grid::gpar(cex = 0.85))
      }
    })
  }
  
  compose_report <- function(dfp, split_label = "vTotal") {
    pitcher  <- dplyr::first(na.omit(dfp$PitcherName))
    throws   <- dplyr::first(na.omit(dfp$PitcherThrows))
    title_txt <- paste(nz_or(pitcher, "Unknown Pitcher"), "-", nz_or(throws, ""), "-", nz_or(split_label, "vTotal"))
    side_filter <- split_side_from_label(split_label)
    valid_pitch_types <- valid_pitch_types_for_tables(dfp)
    dfp_tables <- if (length(valid_pitch_types)) {
      dplyr::filter(dfp, PitchType %in% valid_pitch_types)
    } else {
      dplyr::filter(dfp, !is_bad_pitch_type(PitchType))
    }
    pitch_order <- pitch_usage_order(dfp_tables)
    split_mode <- !identical(side_filter, "Total")
    
    mov_tbl   <- summarize_movement(dfp_tables) %>% format_movetable() %>% order_pitch_rows(pitch_order = pitch_order)
    mov_text_cols <- pitch_type_text_matrix(mov_tbl)
    mov_highlight <- pitch_type_highlight_matrix(mov_tbl)
    p_mov     <- movement_plot(dfp, show_legend = identical(side_filter, "Total"))
    p_release <- release_point_plot(dfp_tables)
    p_mov_tbl_body <- table_to_plot(
      mov_tbl,
      title = NULL,
      core_cex = if (split_mode) 0.62 else 0.68,
      head_cex = if (split_mode) 0.74 else 0.78,
      title_size = 10.5,
      title_margin = 7,
      col_widths = c(1.18, 1.26, 0.94, 0.76, 0.76, 0.86, 0.76, 0.92),
      text_color_matrix = if (identical(side_filter, "Total")) NULL else mov_text_cols,
      text_highlight_matrix = if (identical(side_filter, "Total")) NULL else mov_highlight
    )
    p_mov_tbl <- stack_with_title(
      p_mov_tbl_body,
      "Movement Metrics",
      title_size = 10.5,
      title_margins = margin(0, 6, 2, 6),
      title_height = if (split_mode) 0.12 else 0.10
    )
    
    results_tbl <- pitch_results_table_raw(dfp_tables) %>% format_pitch_results_table() %>% order_pitch_rows(pitch_order = pitch_order)
    results_text_cols <- pitch_type_text_matrix(results_tbl)
    results_highlight <- pitch_type_highlight_matrix(results_tbl)
    p_results_tbl_body <- table_to_plot(
      results_tbl,
      title = NULL,
      core_cex = if (split_mode) 0.60 else 0.66,
      head_cex = if (split_mode) 0.67 else 0.70,
      title_size = 10.5,
      title_margin = 8,
      col_widths = c(1.45, 1.05, 1.05, 0.98, 1.25, 1.05, 1.05),
      text_color_matrix = if (identical(side_filter, "Total")) NULL else results_text_cols,
      text_highlight_matrix = if (identical(side_filter, "Total")) NULL else results_highlight
    )
    p_results_tbl <- stack_with_title(
      p_results_tbl_body,
      "Pitch Results",
      title_size = 10.5,
      title_margins = margin(0, 6, 2, 6),
      title_height = if (split_mode) 0.12 else 0.10
    )
    
    p_loc     <- locations_scatter(dfp_tables, compact = !identical(side_filter, "Total"), pitch_order = pitch_order)
    
    # Usage table – generous height; build shade on numeric (prettify later)
    ut_raw    <- usage_table_raw(dfp_tables)
    ut_sorted <- ut_raw %>% dplyr::arrange(dplyr::desc(Overall))
    shade     <- make_shade_matrix(ut_sorted, exclude_cols = "PitchType")
    usage_tbl <- format_usage_table_noreorder(ut_sorted)
    p_usage   <- table_to_plot(usage_tbl, title = "Usage % (Overall, Splits, Ahead)", shade_matrix = shade)
    
    top  <- title_plot(title_txt)
    if (identical(side_filter, "Total")) {
      info_stack <- top / p_mov_tbl / p_results_tbl +
        patchwork::plot_layout(heights = c(0.16, 0.41, 0.43))
      top_block <- p_mov + info_stack + patchwork::plot_layout(widths = c(0.56, 0.44))
      
      page <- (top_block / p_loc / p_usage +
                 patchwork::plot_layout(heights = c(0.56, 0.23, 0.21))) &
        theme(plot.margin = margin(6, 6, 6, 6),
              plot.background = element_rect(fill = NA, color = NA))
    } else {
      n_pitch_rows <- max(nrow(mov_tbl), nrow(results_tbl), 1L)
      usage_height <- 0.21
      upper_height <- 1 - usage_height
      loc_pitch_levels <- dfp %>%
        dplyr::filter(
          !is_bad_pitch_type(PitchType),
          is.finite(PlateX),
          is.finite(PlateZ)
        ) %>%
        dplyr::distinct(PitchType) %>%
        dplyr::pull(PitchType) %>%
        as.character()
      if (length(pitch_order)) {
        loc_pitch_levels <- c(intersect(pitch_order, loc_pitch_levels), setdiff(loc_pitch_levels, pitch_order))
      }
      n_loc_pitches <- max(length(loc_pitch_levels), 1L)
      locations_title_height <- 0.035
      heatmaps_height <- dplyr::case_when(
        n_loc_pitches >= 5L ~ 0.145,
        n_loc_pitches == 4L ~ 0.16,
        TRUE ~ 0.18
      )
      right_tables_height <- max(
        0.56 + 0.04 * max(0, n_pitch_rows - 4L),
        upper_height - locations_title_height - heatmaps_height
      )
      right_tables_height <- min(right_tables_height, upper_height - locations_title_height - heatmaps_height)
      info_stack <- top / p_mov_tbl / p_results_tbl +
        patchwork::plot_layout(heights = c(0.09, 0.455, 0.455))
      p_mov <- p_mov + theme(plot.margin = margin(0, 2, 0, 0))
      p_release <- p_release + theme(plot.margin = margin(0, 0, 0, 2))
      p_gozone <- gozone_plot(side_filter)
      top_left_row <- patchwork::wrap_plots(
        list(
          shrink_plot(p_mov, scale = 1, hjust = 0, vjust = 1),
          shrink_plot(p_release, scale = 1, hjust = 0, vjust = 1)
        ),
        ncol = 2,
        widths = c(1, 1)
      )
      left_column <- patchwork::wrap_plots(
        list(
          top_left_row,
          p_gozone %||% patchwork::plot_spacer()
        ),
        ncol = 1,
        heights = c(0.36, 0.64)
      )
      p_loc_body <- p_loc + theme(plot.margin = margin(0, 4, 0, 4))
      p_loc_title <- section_title_plot(
        "Locations / Frequency (Catcher POV)",
        size = 10,
        margins = margin(0, 4, 0, 4)
      )
      right_column <- patchwork::wrap_plots(
        list(info_stack, p_loc_title, p_loc_body),
        ncol = 1,
        heights = c(right_tables_height, locations_title_height, heatmaps_height)
      )
      upper_block <- patchwork::wrap_plots(
        list(left_column, right_column),
        ncol = 2,
        widths = c(0.56, 0.44)
      )
      
      page <- (upper_block / p_usage +
                 patchwork::plot_layout(heights = c(upper_height, usage_height))) &
        theme(plot.margin = margin(6, 6, 6, 6),
              plot.background = element_rect(fill = NA, color = NA))
    }
    
    # Watermark behind entire page
    add_watermark(page)
  }
  
}, envir = pitcher_env)

# -------------------- Pitcher bust table --------------------
pitcher_bust_first <- function(x, default = ""){
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) x[[1]] else default
}

pitcher_bust_filter_source <- function(d, source_map = NULL){
  if (!nrow(d) || is.null(source_map) || !length(source_map) || !(".source_file" %in% names(d))) return(d)
  keep <- rep(TRUE, nrow(d))
  pitch_names <- as.character(d$PitcherName)
  src_vals <- as.character(d$.source_file)
  for (pitcher in names(source_map)) {
    selected_source <- source_map[[pitcher]]
    if (!nzchar(selected_source) || identical(selected_source, "__ALL__")) next
    idx <- !is.na(pitch_names) & pitch_names == pitcher
    keep[idx] <- src_vals[idx] == selected_source
  }
  d[keep, , drop = FALSE]
}

pitcher_bust_filter_by_hand <- function(d, hand_filter = "Total"){
  if (!("PitcherThrows" %in% names(d))) return(d)
  if (identical(hand_filter, "LHP")) return(d %>% dplyr::filter(PitcherThrows == "LHP"))
  if (identical(hand_filter, "RHP")) return(d %>% dplyr::filter(PitcherThrows == "RHP"))
  d
}

pitcher_bust_zone_flag <- function(d){
  if (exists("pitcher_zone_flag", envir = pitcher_env, inherits = FALSE)) {
    return(pitcher_env$pitcher_zone_flag(d))
  }
  is.finite(d$PlateX) & is.finite(d$PlateZ) &
    dplyr::between(d$PlateX, -0.708, 0.708) &
    dplyr::between(d$PlateZ, 1.5, 3.5)
}

pitcher_bust_in_zone <- function(d){
  if ("InZone" %in% names(d)) {
    return(pitcher_bust_flag(d$InZone))
  }
  if ("inZone" %in% names(d)) {
    return(pitcher_bust_flag(d$inZone))
  }
  pitcher_bust_zone_flag(d)
}

pitcher_bust_flag <- function(x){
  if (is.logical(x)) return(x %in% TRUE)
  xn <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.finite(xn), xn == 1, NA)
  xc <- trimws(tolower(as.character(x)))
  out[is.na(out) & xc %in% c("true", "t", "yes", "y")] <- TRUE
  out[is.na(out) & xc %in% c("false", "f", "no", "n")] <- FALSE
  out
}

pitcher_bust_is_swing <- function(d, pitch_call){
  if ("IsSwing" %in% names(d)) {
    raw_flag <- pitcher_bust_flag(d$IsSwing)
    return((raw_flag %in% TRUE) | pitch_call %in% c(
      "StrikeSwinging", "FoulBall", "FoulBallFieldable",
      "FoulBallNotFieldable", "FoulTip", "InPlay",
      "InPlayOut", "InPlayNoOut"
    ))
  }
  pitch_call %in% c("StrikeSwinging", "FoulBall", "FoulBallFieldable",
                    "FoulBallNotFieldable", "FoulTip", "InPlay",
                    "InPlayOut", "InPlayNoOut")
}

pitcher_bust_is_strike <- function(d, pitch_call){
  strike_events <- pitch_call %in% c(
    "StrikeCalled", "StrikeSwinging",
    "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip",
    "InPlay", "InPlayOut", "InPlayNoOut"
  )
  if ("IsStrike" %in% names(d)) {
    raw_flag <- pitcher_bust_flag(d$IsStrike)
    return((raw_flag %in% TRUE) | strike_events)
  }
  strike_events
}

pitcher_bust_col <- function(d, cands, as = c("character", "numeric")){
  as <- match.arg(as)
  nm <- pick_first(cands, d)
  if (is.na(nm)) {
    return(if (as == "numeric") rep(NA_real_, nrow(d)) else rep(NA_character_, nrow(d)))
  }
  if (as == "numeric") return(to_num(d[[nm]]))
  as.character(d[[nm]])
}

pitcher_bust_resolve_ev_la <- function(d) {
  ev_candidates <- c("ExitSpeed","ExitVelocity","ExitVel","HitSpeed","BallExitSpeed","EV","EV_mph","EV (mph)")
  la_candidates <- c("Angle","LaunchAngle","LA","Launch_Angle","Launch.Angle","Launch Angle","LAdeg","LA (deg)")
  ev_col <- ev_candidates[ev_candidates %in% names(d)][1]
  ev <- if (!is.na(ev_col)) to_num(d[[ev_col]]) else rep(NA_real_, nrow(d))

  la_cols <- la_candidates[la_candidates %in% names(d)]
  if (!length(la_cols)) return(list(ev = ev, la = rep(NA_real_, nrow(d))))
  la_list <- lapply(la_cols, function(nm) to_num(d[[nm]]))
  score <- function(v) mean(is.finite(v) & v >= -10 & v <= 60, na.rm = TRUE)
  idx <- if ("Angle" %in% la_cols && score(la_list[[which(la_cols == "Angle")]]) >= 0.25) {
    which(la_cols == "Angle")
  } else {
    which.max(vapply(la_list, score, numeric(1)))
  }
  list(ev = ev, la = la_list[[idx]])
}

pitcher_bust_barrel_flag <- function(pc, pr, ev, la) {
  evn <- to_num(ev)
  lan <- to_num(la)
  bip <- safe_is_bip(pc, pr)
  out <- bip & is.finite(evn) & is.finite(lan) & evn >= 95 & lan >= 5 & lan <= 40
  out[is.na(out)] <- FALSE
  out
}

pitcher_bust_gb_flag <- function(pc, pr, la) {
  lan <- to_num(la)
  bip <- safe_is_bip(pc, pr)
  out <- bip & is.finite(lan) & lan < 5
  out[is.na(out)] <- FALSE
  out
}

pitcher_bust_prepare <- function(d, hand_filter = "Total", source_map = NULL){
  d <- pitcher_bust_filter_source(d, source_map)
  d <- pitcher_bust_filter_by_hand(d, hand_filter)
  if (!nrow(d) || !all(c("PitcherName", "PitchType") %in% names(d))) return(tibble::tibble())
  pc <- pitcher_bust_col(d, c("pitch_call", "PitchCall", "Pitch_Call", "Call", "PitchResult", "Pitch_Result"))
  play <- pitcher_bust_col(d, c("play_result", "PlayResult", "Result", "PAResult", "Outcome"))
  hit_type <- pitcher_bust_col(d, c("TaggedHitType", "AutoHitType", "HitType", "BattedBallType"))
  evla <- pitcher_bust_resolve_ev_la(d)
  exit_velo <- evla$ev
  launch_angle <- evla$la
  in_zone_raw <- pitcher_bust_in_zone(d)
  out <- d %>%
    dplyr::mutate(
      PitcherName = as.character(PitcherName),
      PitchType = as.character(PitchType),
      BatterSide = as.character(BatterSide),
      pitch_call = canon_pitch_call(pc),
      play_result = play,
      balls_b4 = to_num(BallsBefore),
      strikes_b4 = to_num(StrikesBefore),
      in_zone = in_zone_raw,
      has_zone = !is.na(in_zone),
      is_strike = pitcher_bust_is_strike(d, pitch_call),
      is_swing = pitcher_bust_is_swing(d, pitch_call),
      is_whiff = pitch_call == "StrikeSwinging",
      is_bip = safe_is_bip(pitch_call, play_result),
      is_bip_la = is_bip & is.finite(launch_angle),
      is_bip_evla = is_bip & is.finite(exit_velo) & is.finite(launch_angle),
      is_gb = pitcher_bust_gb_flag(pitch_call, play_result, launch_angle),
      barrel_flag = pitcher_bust_barrel_flag(pitch_call, play_result, exit_velo, launch_angle)
    ) %>%
    dplyr::filter(
      !is.na(PitcherName),
      nzchar(trimws(PitcherName)),
      !pitcher_env$is_bad_pitch_type(PitchType),
      BatterSide %in% c("L", "R")
    )
  if (!nrow(out)) return(out)
  out$PA_ID <- make_pa_id(out)
  out
}

pitcher_bust_table_numeric <- function(d, hand_filter = "Total", source_map = NULL){
  d <- pitcher_bust_prepare(d, hand_filter, source_map = source_map)
  if (!nrow(d)) {
    return(tibble::tibble(
      PitcherName = character(), Throws = character(), PA = integer(),
      BatterSide = character(), PitchType = character(), P = integer(), pRV = numeric(),
      `Strike%` = numeric(), `Zone%` = numeric(), `Whiff%` = numeric(),
      `IZWhiff%` = numeric(), `Chase%` = numeric(), BA = numeric(), SLG = numeric(),
      `GB%` = numeric(), `Barrel%` = numeric()
    ))
  }
  group_vars <- c("PitcherName", "BatterSide", "PitchType")
  summary_group_vars <- list(
    by_pitch = c("PitcherName", "BatterSide", "PitchType"),
    total = c("PitcherName", "BatterSide")
  )
  build_pitcher_bust_summary <- function(df, group_cols, pitch_type_value = NULL){
    base_pa_group_cols <- setdiff(group_cols, "PitchType")
    p_counts <- df %>%
      dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = "P")
    pitch_stats <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        `Strike%` = safe_ratio(sum(is_strike, na.rm = TRUE), dplyr::n()),
        `Zone%` = safe_ratio(sum(has_zone & in_zone, na.rm = TRUE), sum(has_zone, na.rm = TRUE)),
        `IZWhiff%` = safe_ratio(sum(is_whiff & has_zone & in_zone, na.rm = TRUE),
                                sum(is_swing & has_zone & in_zone, na.rm = TRUE)),
        `Chase%` = safe_ratio(sum(is_swing & has_zone & !in_zone, na.rm = TRUE),
                              sum(has_zone & !in_zone, na.rm = TRUE)),
        `Whiff%` = safe_ratio(sum(is_whiff, na.rm = TRUE), sum(is_swing, na.rm = TRUE)),
        `GB%` = safe_ratio(sum(is_gb, na.rm = TRUE), sum(is_bip_la, na.rm = TRUE)),
        `Barrel%` = safe_ratio(sum(barrel_flag, na.rm = TRUE), sum(is_bip_evla, na.rm = TRUE)),
        .groups = "drop"
      )
    pa_sum <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(base_pa_group_cols, "PA_ID")))) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup() %>%
      summarise_pa_results(group_cols) %>%
      ensure_pa_summary_cols(group_cols)
    out <- p_counts %>%
      dplyr::left_join(pa_sum, by = group_cols) %>%
      dplyr::left_join(pitch_stats, by = group_cols) %>%
      dplyr::mutate(
        dplyr::across(c(AB, H, TB, BB, HBP, SF, K, RBI, HR), ~tidyr::replace_na(.x, 0)),
        pRV = rv_formula(TB, BB, K, RBI, HR, P),
        BA = safe_ratio(H, AB),
        SLG = safe_ratio(TB, AB)
      )
    if (!is.null(pitch_type_value)) {
      out <- out %>% dplyr::mutate(PitchType = pitch_type_value, .after = BatterSide)
    }
    out
  }
  throw_lookup <- d %>%
    dplyr::group_by(PitcherName) %>%
    dplyr::summarise(Throws = pitcher_bust_first(PitcherThrows), .groups = "drop")
  pa_lookup <- d %>%
    dplyr::group_by(PitcherName) %>%
    dplyr::summarise(PA = dplyr::n_distinct(PA_ID), .groups = "drop")
  dplyr::bind_rows(
    build_pitcher_bust_summary(d, summary_group_vars$by_pitch),
    build_pitcher_bust_summary(d, summary_group_vars$total, pitch_type_value = "Total")
  ) %>%
    dplyr::left_join(throw_lookup, by = "PitcherName") %>%
    dplyr::left_join(pa_lookup, by = "PitcherName") %>%
    dplyr::arrange(dplyr::desc(PA), PitcherName, BatterSide, dplyr::desc(P), PitchType) %>%
    dplyr::select(PitcherName, Throws, PA, BatterSide, PitchType, P, pRV,
                  `Strike%`, `Zone%`, `Whiff%`, `IZWhiff%`, `Chase%`,
                  BA, SLG, `GB%`, `Barrel%`)
}

PITCHER_BUST_D1 <- tibble::tribble(
  ~Metric, ~Ref, ~Scale, ~LowerGood,
  "pRV",      0.000, 18.000, TRUE,
  "Strike%",  0.605,  0.120, FALSE,
  "Zone%",    0.456,  0.120, FALSE,
  "Whiff%",   0.240,  0.120, FALSE,
  "IZWhiff%", 0.157,  0.100, FALSE,
  "Chase%",   0.210,  0.120, FALSE,
  "BA",       0.260,  0.120, TRUE,
  "SLG",      0.400,  0.220, TRUE,
  "GB%",      0.420,  0.120, FALSE,
  "Barrel%",  0.174,  0.100, TRUE
)

d1_pitch_bucket <- function(pitch_type) {
  p <- trimws(as.character(pitch_type %||% ""))
  if (!nzchar(p)) return(NA_character_)
  if (p == "Fastball") return("Fastball")
  if (p == "Sinker") return("Sinker")
  if (p %in% c("Slider", "Sweeper")) return("Slider/Sweeper")
  if (p == "Curveball") return("Curveball")
  if (p %in% c("Changeup", "Splitter")) return("Changeup/Splitter")
  if (p == "Cutter") return("Cutter")
  NA_character_
}

PITCHER_BUST_D1_BY_PITCH <- list(
  `Whiff%` = c(
    "Fastball" = 0.18,
    "Sinker" = 0.14,
    "Slider/Sweeper" = 0.32,
    "Curveball" = 0.29,
    "Changeup/Splitter" = 0.32,
    "Cutter" = 0.27
  ),
  `Chase%` = c(
    "Fastball" = 0.18,
    "Sinker" = 0.20,
    "Slider/Sweeper" = 0.24,
    "Curveball" = 0.19,
    "Changeup/Splitter" = 0.27,
    "Cutter" = 0.26
  )
)

pitcher_bust_metric_ref <- function(metric, pitch_type = NA_character_) {
  metric <- as.character(metric %||% "")
  if (metric %in% c("Whiff%", "Chase%")) {
    bucket <- d1_pitch_bucket(pitch_type)
    ref_map <- PITCHER_BUST_D1_BY_PITCH[[metric]]
    if (!is.null(ref_map) && length(ref_map) && is.character(bucket) && nzchar(bucket) && bucket %in% names(ref_map)) {
      ref <- ref_map[[bucket]]
      if (is.finite(ref)) return(ref)
    }
  }
  ref_row <- PITCHER_BUST_D1 %>% dplyr::filter(.data$Metric == metric)
  if (!nrow(ref_row)) return(NA_real_)
  ref_row$Ref[[1]]
}

pitcher_bust_metric_fill_colors <- function(v, metric, pitch_type = NA_character_){
  out <- rep("#FFFFFF", length(v))
  ref_row <- PITCHER_BUST_D1 %>% dplyr::filter(.data$Metric == metric)
  if (!nrow(ref_row)) return(out)
  scale <- ref_row$Scale[[1]]
  lower_good <- ref_row$LowerGood[[1]]
  refs <- vapply(pitch_type, function(pt) pitcher_bust_metric_ref(metric, pt), numeric(1))
  diff <- (v - refs) / scale
  badness <- if (isTRUE(lower_good)) diff else -diff
  badness <- pmin(pmax(badness, -1), 1)
  pal <- grDevices::colorRamp(c("#3FA34D", "#FFFFFF", "#E15759"), space = "rgb")
  ok <- is.finite(badness)
  out[ok] <- grDevices::rgb(pal((badness[ok] + 1) / 2) / 255)
  out
}

pitcher_bust_format_value <- function(metric, value){
  if (!is.finite(value)) return("")
  if (metric == "P") return(scales::comma(value, accuracy = 1))
  if (metric %in% c("Strike%", "Zone%", "Whiff%", "IZWhiff%", "Chase%", "GB%", "Barrel%")) {
    return(scales::percent(value, accuracy = 1))
  }
  if (metric %in% c("BA", "SLG")) return(sub("^0", "", scales::number(value, accuracy = 0.001)))
  scales::number(value, accuracy = 0.1)
}

single_pitcher_bust_side_grob <- function(pitcher_num, title_label, title_col = "#000000"){
  metric_levels <- c("P", "pRV", "Strike%", "Zone%", "Whiff%", "IZWhiff%", "Chase%", "BA", "SLG", "GB%", "Barrel%")
  if (!nrow(pitcher_num)) {
    pitcher_num <- tibble::tibble(
      PitchType = character(), P = integer(), pRV = numeric(),
      `Strike%` = numeric(), `Zone%` = numeric(), `Whiff%` = numeric(),
      `IZWhiff%` = numeric(), `Chase%` = numeric(), BA = numeric(), SLG = numeric(),
      `GB%` = numeric(), `Barrel%` = numeric()
    )
  }
  pitcher_num <- pitcher_num %>%
    dplyr::mutate(is_total_row = PitchType == "Total") %>%
    dplyr::arrange(is_total_row, dplyr::desc(P), PitchType) %>%
    dplyr::select(-is_total_row)
  disp <- pitcher_num %>%
    dplyr::mutate(Pitch = PitchType) %>%
    dplyr::select(Pitch, dplyr::all_of(metric_levels))
  for (nm in metric_levels) {
    disp[[nm]] <- vapply(disp[[nm]], function(x) pitcher_bust_format_value(nm, x), character(1))
  }
  if (!nrow(disp)) {
    disp <- data.frame(
      Pitch = "No pitches", P = "", pRV = "", `Strike%` = "", `Zone%` = "",
      `Whiff%` = "", `IZWhiff%` = "", `Chase%` = "", BA = "", SLG = "",
      `GB%` = "", `Barrel%` = "", check.names = FALSE
    )
  }
  disp <- as.data.frame(disp, stringsAsFactors = FALSE)
  
  fills <- matrix("#FFFFFF", nrow = nrow(disp), ncol = ncol(disp), dimnames = list(NULL, names(disp)))
  for (nm in metric_levels) {
    vals <- suppressWarnings(as.numeric(pitcher_num[[nm]]))
    if (length(vals) == nrow(fills)) {
      fills[, nm] <- pitcher_bust_metric_fill_colors(vals, nm, pitcher_num$PitchType)
      fills[!is.finite(vals), nm] <- "#FFFFFF"
    }
  }
  
  th <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 5.7, fontface = "bold", col = "#000000"),
      bg_params = list(fill = "#FFFFFF", col = "#000000", lwd = 0.45),
      padding = grid::unit(c(2.5, 0.6), "pt")
    ),
    colhead = list(
      fg_params = list(fontsize = 5.5, fontface = "bold", col = "#000000"),
      bg_params = list(fill = "#FFFFFF", col = "#000000", lwd = 0.65),
      padding = grid::unit(c(2.5, 0.6), "pt")
    )
  )
  tg <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  tryCatch({
    idx_bg <- which(tg$layout$name == "core-bg")
    lay <- tg$layout[idx_bg, c("t", "l")]
    core_rows <- sort(unique(lay$t))
    core_cols <- sort(unique(lay$l))
    row_idx <- match(lay$t, core_rows)
    col_idx <- match(lay$l, core_cols)
    for (k in seq_along(idx_bg)) {
      tg$grobs[[idx_bg[k]]]$gp$fill <- fills[row_idx[k], col_idx[k]]
    }
    idx_fg <- which(tg$layout$name == "core-fg")
    if (length(idx_fg)) {
      fg_lay <- tg$layout[idx_fg, c("t", "l")]
      fg_cols <- sort(unique(fg_lay$l))
      fg_col_idx <- match(fg_lay$l, fg_cols)
      for (k in seq_along(idx_fg)) {
        if (fg_col_idx[k] == 1L) tg$grobs[[idx_fg[k]]]$gp$col <- "#000000"
      }
    }
    column_widths <- do.call(
      grid::unit.c,
      lapply(seq_along(colnames(disp)), function(j) {
        lbl <- colnames(disp)[[j]]
        vals <- as.character(disp[[j]])
        head_w <- grid::grobWidth(
          grid::textGrob(lbl, gp = grid::gpar(fontsize = 5.5, fontface = "bold"))
        )
        val_widths <- lapply(vals, function(val) {
          grid::grobWidth(
            grid::textGrob(val, gp = grid::gpar(fontsize = 5.7, fontface = "bold"))
          )
        })
        do.call(grid::unit.pmax, c(list(head_w), val_widths)) +
          grid::grobWidth(grid::textGrob("00", gp = grid::gpar(fontsize = 5.7, fontface = "bold")))
      })
    )
    tg$widths <- column_widths
    tg$heights <- tg$heights * 2
  }, error = function(e) NULL)
  title_g <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = NA)),
    grid::textGrob(title_label, x = 0.015, hjust = 0,
                   gp = grid::gpar(col = title_col, fontface = "bold", cex = 0.80))
  )
  gridExtra::arrangeGrob(title_g, tg, ncol = 1, heights = grid::unit(c(0.55, 3.45), "null"))
}

single_pitcher_bust_row_grob <- function(pitcher_name, pitcher_num){
  left <- single_pitcher_bust_side_grob(
    dplyr::filter(pitcher_num, BatterSide == "L"),
    paste0(pitcher_name, " - LHH"),
    "#000000"
  )
  right <- single_pitcher_bust_side_grob(
    dplyr::filter(pitcher_num, BatterSide == "R"),
    paste0(pitcher_name, " - RHH"),
    "#000000"
  )
  gridExtra::arrangeGrob(
    grid::nullGrob(), left, grid::nullGrob(), right, grid::nullGrob(),
    ncol = 5,
    widths = grid::unit(c(0.15, 3.45, 0.80, 3.45, 0.15), "in")
  )
}

pitcher_bust_selected_players <- function(num, pitcher_order = NULL){
  all_pitchers <- num %>%
    dplyr::distinct(PitcherName, PA) %>%
    dplyr::arrange(dplyr::desc(PA), PitcherName) %>%
    dplyr::pull(PitcherName)
  if (is.null(pitcher_order)) return(all_pitchers)
  apply_pitcher_bust_order(all_pitchers, pitcher_order)
}

pitcher_bust_page_count <- function(d, hand_filter = "Total", pitcher_order = NULL, source_map = NULL, per_page = 9L){
  num <- pitcher_bust_table_numeric(d, hand_filter, source_map = source_map)
  pitchers <- pitcher_bust_selected_players(num, pitcher_order)
  if (!nrow(num) || !length(pitchers)) return(1L)
  max(1L, ceiling(length(pitchers) / per_page))
}

pitcher_bust_tables_page <- function(d, hand_filter = "Total", page_num = 1, pitcher_order = NULL, source_map = NULL, per_page = 9L){
  num <- pitcher_bust_table_numeric(d, hand_filter, source_map = source_map)
  if (!nrow(num)) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0.5, y = 0.5, label = "No pitcher data found", size = 6))
  }
  
  pitchers <- pitcher_bust_selected_players(num, pitcher_order)
  if (!length(pitchers)) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0.5, y = 0.5, label = "Select one or more pitchers to build the bust table", size = 6))
  }
  total_pages <- max(1L, ceiling(length(pitchers) / per_page))
  page_num <- suppressWarnings(as.integer(page_num))
  if (!is.finite(page_num) || page_num < 1) page_num <- 1L
  page_num <- min(page_num, total_pages)
  
  idx <- ((page_num - 1L) * per_page + 1L):min(length(pitchers), page_num * per_page)
  page_pitchers <- pitchers[idx]
  grobs <- lapply(page_pitchers, function(pitcher) {
    single_pitcher_bust_row_grob(pitcher, dplyr::filter(num, PitcherName == pitcher))
  })
  
  title <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "#FFFFFF", col = NA)),
    grid::textGrob(
      sprintf("Stuff Sheet - %s - Page %d of %d", hand_label(hand_filter), page_num, total_pages),
      x = 0.5,
      gp = grid::gpar(col = "#000000", fontface = "bold", cex = 2.10)
    )
  )
  
  page_grob <- gridExtra::arrangeGrob(
    grobs = c(list(title), grobs),
    ncol = 1,
    heights = grid::unit(c(0.42, rep(1, length(grobs))), "null")
  )
  
  ggplotify::as.ggplot(page_grob) +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(8, 8, 8, 8))
}

# -------------------- UI --------------------
scouting_theme <- bs_theme(version = 5, primary = TXST_MAROON, secondary = TXST_GOLD)
scouting_head <- tags$head(
    tags$style(HTML("
      body { background:#f5f1e8; }
      .report-shell { background:#fbfaf7; border:1px solid #d8cfbd; border-radius:8px; padding:10px; }
      .report-toolbar { display:flex; gap:12px; align-items:end; flex-wrap:wrap; margin-bottom:8px; }
      .report-toolbar .form-group { margin-bottom:0; min-width:180px; }
      .nav-tabs .nav-link { font-weight:700; color:#501214; }
      .nav-tabs .nav-link.active { color:#501214; border-top:4px solid #B4975A; }
      .matchup-grid-shell .dataTables_wrapper { width:100%; overflow-x:auto; }
      .matchup-grid-shell table.dataTable thead th { white-space:normal; text-align:center; font-size:12px; }
      .matchup-grid-shell table.dataTable tbody td { text-align:center; font-weight:700; }
      .matchup-grid-shell table.dataTable tbody td:first-child { text-align:left; font-weight:700; white-space:nowrap; }
    "))
  )
scouting_title <- titlePanel("TXST Scouting App")
base_scouting_page <- fluidPage
base_scouting_layout <- function(sidebar, main) sidebarLayout(sidebar, main)

if (SCOUTING_EMBEDDED_MODE) {
  scouting_theme <- get0("BASE_SCOUTING_THEME", inherits = FALSE, ifnotfound = scouting_theme)
  scouting_head <- get0("BASE_SCOUTING_HEAD", inherits = FALSE, ifnotfound = scouting_head)
  scouting_title <- NULL
  base_scouting_page <- function(..., theme = NULL) htmltools::tagList(...)
  base_scouting_layout <- function(sidebar, main) {
    htmltools::tags$div(
      class = "base-scouting-embedded-layout",
      htmltools::tags$aside(class = "base-scouting-sidebar", sidebar$children),
      htmltools::tags$main(class = "base-scouting-main", main$children)
    )
  }
}

ui <- base_scouting_page(
  theme = scouting_theme,
  scouting_head,
  scouting_title,
  base_scouting_layout(
  sidebarPanel(width = 3,
               if (!is.null(SCOUTING_SEASON_SOURCE)) tagList(
                 radioButtons("scout_data_source", "Scouting data",
                              choices = c("College season" = "season", "Game CSVs" = "csv"),
                              selected = "season"),
                 conditionalPanel(
                   "input.scout_data_source === 'season'",
                   uiOutput("scout_season_status"),
                   if (!SCOUTING_EMBEDDED_MODE) {
                     selectizeInput("scout_team", "Opponent team", choices = NULL)
                   },
                   selectizeInput("scout_season_hitters", "Hitters to load", choices = NULL,
                                  multiple = TRUE, options = list(plugins = list("remove_button"))),
                   selectizeInput("scout_season_pitchers", "Pitchers to load", choices = NULL,
                                  multiple = TRUE, options = list(plugins = list("remove_button"))),
                   helpText("Choose an opponent first. Only that team's player directory and selected players' pitches are loaded."),
                   hr()
                 )
               ),
               conditionalPanel(
                 "input.main_tab !== 'matchup_grid'",
                 conditionalPanel(
                 "input.scout_data_source !== 'season'",
                 tags$h4("Report File"),
                 uiOutput("csv_files_ui"),
                 hr()),
                 conditionalPanel(
                   "input.main_tab !== 'Pitcher Card' && input.main_tab !== 'Stuff Sheet'",
                   uiOutput("hitter_order_ui")
                 )
               ),
               conditionalPanel(
                 "input.main_tab === 'matchup_grid'",
                 conditionalPanel(
                 "input.scout_data_source !== 'season'",
                 tags$h4("Matchup Files"),
                 fileInput("matchup_pitchers_file", "Pitchers CSV", accept = c(".csv", "text/csv")),
                 fileInput("matchup_hitters_file", "Hitters CSV", accept = c(".csv", "text/csv")),
                 hr()),
                 uiOutput("matchup_pitcher_order_ui"),
                 hr(),
                 uiOutput("matchup_hitter_order_ui")
               )
  ),
    mainPanel(width = 9,
              tabsetPanel(id = "main_tab",
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
                ),
                tabPanel(
                  "Pitcher Card",
                  uiOutput("guard_msg"),
	                  div(class = "report-shell",
	                      div(class = "report-toolbar",
	                          uiOutput("pitcher_card_pitcher_ui"),
	                          selectInput(
	                            "pitcher_card_batter_side",
	                            "Batter side",
	                            choices = c("Total" = "Total", "vs LHH" = "L", "vs RHH" = "R"),
	                            selected = "Total"
	                          ),
	                          uiOutput("pitcher_card_page_ui"),
	                          actionButton("pitcher_preview", "Generate Preview", class = "btn btn-primary"),
	                          downloadButton("dl_pitcher_card_pdf", "Download PDF", class = "btn btn-success")
	                      ),
                      plotOutput("pitcher_card", height = px_from_in(LETTER_H), width = px_from_in(LETTER_W))
                  )
                ),
                tabPanel(
                  "Stuff Sheet",
                  div(class = "report-shell",
                      div(class = "report-toolbar",
                          selectInput("pitcher_bust_hand", "Pitcher hand", choices = c("Total","LHP","RHP"), selected = "Total"),
                          uiOutput("pitcher_bust_order_ui"),
                          actionButton("pitcher_bust_source_toggle", "Pitcher Source Filters", class = "btn btn-outline-secondary"),
                          uiOutput("pitcher_bust_page_ui"),
                          downloadButton("dl_pitcher_bust_pdf", "Export PDF", class = "btn btn-success")
                      ),
                      uiOutput("pitcher_bust_source_ui"),
                      plotOutput("pitcher_bust_table", height = px_from_in(PORTRAIT_H), width = px_from_in(PORTRAIT_W))
                  )
                ),
                tabPanel(
                  "Matchup Grid",
                  value = "matchup_grid",
                  div(
                    class = "report-shell matchup-grid-shell",
                    div(
                      class = "report-toolbar",
                      radioButtons(
                        "matchup_view",
                        "View",
                        choices = c("Total" = "total", "Pitch Types" = "pitch_types"),
                        selected = "pitch_types",
                        inline = TRUE
                      ),
                      radioButtons(
                        "matchup_metric",
                        "Matchup logic",
                        choices = c("Run Value" = "rv", "Whiff%" = "whiff", "SLG" = "slg"),
                        selected = "rv",
                        inline = TRUE
                      )
                    ),
                    uiOutput("matchup_guard_msg"),
                    DTOutput("matchup_grid"),
                    uiOutput("matchup_score_note")
                  )
                )
              )
    )
  )
)

# -------------------- Server --------------------
server <- function(input, output, session){

  season_mode <- reactive({
    !is.null(SCOUTING_SEASON_SOURCE) &&
      identical(input$scout_data_source %||% "season", "season")
  })
  selected_season_team <- reactive({
    if (SCOUTING_EMBEDDED_MODE) {
      input$base_scouting_team %||% ""
    } else {
      input$scout_team %||% ""
    }
  })
  season_teams <- reactive({
    req(season_mode())
    tryCatch(
      withProgress(message = "Loading college team directory", value = 0.5, {
        SCOUTING_SEASON_SOURCE$teams("pitcher")
      }),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })
  output$scout_season_status <- renderUI({
    teams <- season_teams()
    helpText(sprintf("College season ready: %s teams. Select one to begin.",
                     format(length(teams), big.mark = ",")))
  })
  observeEvent(season_teams(), {
    if (SCOUTING_EMBEDDED_MODE) return()
    teams <- season_teams()
    selected <- input$scout_team %||% ""
    if (!selected %in% teams) selected <- ""
    team_choices <- stats::setNames(teams, scouting_team_display_name(teams))
    updateSelectizeInput(session, "scout_team",
                         choices = c("Choose a team" = "", team_choices),
                         selected = selected, server = TRUE)
  })
  for (role_value in c("hitter", "pitcher")) local({
    role <- role_value
    player_id <- paste0("scout_season_", role, "s")
    observeEvent(list(selected_season_team(), season_teams()), {
      team <- selected_season_team()
      catalog <- SCOUTING_SEASON_SOURCE$catalog(role, team)
      choices <- catalog$Player[catalog$Team == team]
      updateSelectizeInput(session, player_id, choices = choices,
                           selected = intersect(input[[player_id]], choices), server = TRUE)
    }, ignoreInit = FALSE)
  })
  season_rows <- function(role) {
    req(season_mode())
    team <- selected_season_team()
    players <- input[[paste0("scout_season_", role, "s")]]
    catalog <- SCOUTING_SEASON_SOURCE$catalog(role, team)
    players <- intersect(players, catalog$Player[catalog$Team == team])
    validate(need(nzchar(team) && length(players) > 0,
                  paste("Choose a team and", paste0(role, "s"), "in the sidebar.")))
    rows <- tryCatch(
      withProgress(message = paste("Loading selected", paste0(role, "s")), value = 0.5, {
        SCOUTING_SEASON_SOURCE$load_players(role, team, players)
      }),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
    validate(need(nrow(rows) > 0, "No season pitches found for the selected players."))
    rows
  }
  season_hitter_rows <- reactive(season_rows("hitter"))
  season_pitcher_rows <- reactive(season_rows("pitcher"))
  
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
    out <- read_csv_files(paths, source_labels = files)
    validate(need(is.data.frame(out), "Loaded file is not a data frame / tibble."))
    out
  })
  
  std_all <- reactive({
    d_raw <- if (season_mode()) season_hitter_rows() else df_all()
    tryCatch(std_cols(d_raw), error = function(e){
      message("[std_cols ERROR] ", conditionMessage(e))
      validate(need(FALSE, paste("Column standardization error:", conditionMessage(e))))
    })
  })
  
  pitcher_std_all <- reactive({
    d_raw <- if (season_mode()) season_pitcher_rows() else df_all()
    tryCatch(
      pitcher_env$standardize_tm(d_raw) %>%
        dplyr::filter(!pitcher_env$is_bad_pitch_type(PitchType)),
      error = function(e){
        message("[pitcher standardize_tm ERROR] ", conditionMessage(e))
        validate(need(FALSE, paste("Pitcher column standardization error:", conditionMessage(e))))
      }
    )
  })

  matchup_pitchers_raw <- reactive({
    if (season_mode()) return(season_pitcher_rows())
    req(input$matchup_pitchers_file)
    out <- read_csv_files(input$matchup_pitchers_file$datapath)
    validate(need(is.data.frame(out) && nrow(out) > 0, "Pitcher CSV did not load any rows."))
    out
  })

  matchup_hitters_raw <- reactive({
    if (season_mode()) return(season_hitter_rows())
    req(input$matchup_hitters_file)
    out <- read_csv_files(input$matchup_hitters_file$datapath)
    validate(need(is.data.frame(out) && nrow(out) > 0, "Hitter CSV did not load any rows."))
    out
  })

  matchup_pitchers_std <- reactive({
    d_raw <- matchup_pitchers_raw()
    tryCatch(
      pitcher_env$standardize_tm(d_raw) %>%
        dplyr::filter(!pitcher_env$is_bad_pitch_type(PitchType)),
      error = function(e){
        message("[matchup pitcher standardize_tm ERROR] ", conditionMessage(e))
        validate(need(FALSE, paste("Pitcher matchup file error:", conditionMessage(e))))
      }
    )
  })

  matchup_hitters_std <- reactive({
    d_raw <- matchup_hitters_raw()
    tryCatch(
      std_cols(d_raw),
      error = function(e){
        message("[matchup std_cols ERROR] ", conditionMessage(e))
        validate(need(FALSE, paste("Hitter matchup file error:", conditionMessage(e))))
      }
    )
  })

  matchup_pitcher_meta <- reactive({
    req(matchup_pitchers_std())
    d <- matchup_pitchers_std()
    validate(need(nrow(d) > 0, "No pitcher rows found in the selected data."))
    d %>%
      dplyr::mutate(PA_ID = make_pa_id(d)) %>%
      dplyr::filter(!is.na(PitcherName), nzchar(trimws(PitcherName))) %>%
      dplyr::group_by(PitcherName) %>%
      dplyr::summarise(
        Throws = pitcher_bust_first(PitcherThrows),
        PA = dplyr::n_distinct(PA_ID),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(PA), PitcherName)
  })

  matchup_hitter_meta <- reactive({
    req(matchup_hitters_std())
    hitter_report_meta(matchup_hitters_std(), "Total")
  })

  matchup_pitcher_choices <- reactive({
    req(matchup_pitcher_meta())
    matchup_pitcher_meta()$PitcherName
  })

  matchup_hitter_choices <- reactive({
    req(matchup_hitter_meta())
    matchup_hitter_meta()$Hitter
  })

  observeEvent(matchup_pitcher_choices(), {
    choices <- matchup_pitcher_choices()
    req(length(choices) > 0)
    selected <- apply_matchup_order(choices, input$matchup_pitcher_order)
    updateSelectizeInput(
      session,
      "matchup_pitcher_order",
      choices = choices,
      selected = selected,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  observeEvent(matchup_hitter_choices(), {
    choices <- matchup_hitter_choices()
    req(length(choices) > 0)
    selected <- apply_matchup_order(choices, input$matchup_hitter_order)
    updateSelectizeInput(
      session,
      "matchup_hitter_order",
      choices = choices,
      selected = selected,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  output$matchup_pitcher_order_ui <- renderUI({
    req(matchup_pitcher_choices())
    choices <- matchup_pitcher_choices()
    validate(need(length(choices) > 0, "No pitchers found in the selected data."))
    tagList(
      selectizeInput(
        "matchup_pitcher_order",
        "Pitchers (sorted by PA, left to right, max 15):",
        choices = choices,
        selected = apply_matchup_order(choices, NULL),
        multiple = TRUE,
        options = list(
          plugins = list("drag_drop", "remove_button"),
          persist = FALSE,
          maxItems = 15,
          placeholder = "Select up to 15 pitchers"
        )
      ),
      helpText("The list auto-sorts by PA when the data loads. Drag names to change the column order.")
    )
  })

  output$matchup_hitter_order_ui <- renderUI({
    req(matchup_hitter_choices())
    choices <- matchup_hitter_choices()
    validate(need(length(choices) > 0, "No hitters found in the selected data."))
    tagList(
      selectizeInput(
        "matchup_hitter_order",
        "Hitters (sorted by PA, top to bottom, max 15):",
        choices = choices,
        selected = apply_matchup_order(choices, NULL),
        multiple = TRUE,
        options = list(
          plugins = list("drag_drop", "remove_button"),
          persist = FALSE,
          maxItems = 15,
          placeholder = "Select up to 15 hitters"
        )
      ),
      helpText("The list auto-sorts by PA when the data loads. Drag names to change the row order.")
    )
  })

  matchup_hitter_summary <- reactive({
    req(matchup_hitters_std())
    build_matchup_hitter_summary(matchup_hitters_std())
  })

  matchup_pitcher_summary <- reactive({
    req(matchup_pitchers_std())
    build_matchup_pitcher_summary(matchup_pitchers_std())
  })

  matchup_grid_bundle <- reactive({
    req(matchup_hitter_summary(), matchup_pitcher_summary())
    hitter_order <- apply_matchup_order(matchup_hitter_choices(), input$matchup_hitter_order)
    pitcher_order <- apply_matchup_order(matchup_pitcher_choices(), input$matchup_pitcher_order)
    build_matchup_grid(
      matchup_hitter_summary(),
      matchup_pitcher_summary(),
      hitter_order,
      pitcher_order,
      metric = input$matchup_metric %||% "rv"
    )
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
        selected = if (season_mode()) hitters else character(0),
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
    if (season_mode()) {
      role <- if (identical(input$main_tab, "Pitcher Card")) "pitchers" else "hitters"
      if (!length(input[[paste0("scout_season_", role)]])) {
        return(helpText(paste("Choose a team and", role, "in the sidebar to begin.")))
      }
      return(NULL)
    }
    if (is.null(input$csv_files) || length(input$csv_files) == 0) {
      div(style="margin:8px 0; padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
          HTML("<b>Select one or more CSVs</b> from data/ to begin."))
    }
  })

  output$matchup_guard_msg <- renderUI({
    if (season_mode() && (!length(input$scout_season_hitters) || !length(input$scout_season_pitchers))) {
      return(helpText("Choose hitters and pitchers in the sidebar to build the matchup grid."))
    }
    if (!season_mode() && (is.null(input$matchup_pitchers_file) || is.null(input$matchup_hitters_file))) {
      return(
        div(
          style = "margin:0 0 10px; padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
          HTML("<b>Upload both matchup CSVs</b> in the left sidebar to build the grid.")
        )
      )
    }
    hitter_order <- apply_matchup_order(matchup_hitter_choices(), input$matchup_hitter_order)
    pitcher_order <- apply_matchup_order(matchup_pitcher_choices(), input$matchup_pitcher_order)
    if (!length(hitter_order) || !length(pitcher_order)) {
      return(
        div(
          style = "margin:0 0 10px; padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
          HTML("<b>Select at least one hitter and one pitcher</b> to populate the matchup grid.")
        )
      )
    }
    NULL
  })

  output$matchup_grid <- DT::renderDT({
    bundle <- matchup_grid_bundle()
    view_mode <- input$matchup_view %||% "pitch_types"
    is_total_view <- identical(view_mode, "total")
    tbl <- if (is_total_view) bundle$total_wide else bundle$wide
    validate(need(ncol(tbl) > 1, "Select at least one hitter and one pitcher to build the grid."))
    score_cols <- if (is_total_view) bundle$total_cols else bundle$score_cols
    pal <- grDevices::colorRampPalette(c("#B2182B", "#F7F7F7", "#1A9850"))(10)
    header_container <- if (is_total_view) {
      htmltools::withTags(
        table(
          class = "display",
          thead(
            tr(
              th("Hitter"),
              lapply(bundle$total_colnames[-1], function(lbl) th(HTML(lbl)))
            )
          )
        )
      )
    } else {
      htmltools::withTags(
        table(
          class = "display",
          thead(
            tr(
              th(rowspan = 2, "Hitter"),
              lapply(bundle$pitcher_spanners, function(lbl) {
                th(colspan = 4, style = "border-right:3px solid #000000;", HTML(lbl))
              })
            ),
            tr(
              lapply(seq_along(bundle$pitch_family_labels), function(i) {
                style <- if (i %% 4 == 0) "border-right:3px solid #000000;" else NULL
                th(style = style, bundle$pitch_family_labels[[i]])
              })
            )
          )
        )
      )
    }
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      class = "compact stripe",
      escape = FALSE,
      colnames = if (is_total_view) bundle$total_colnames else bundle$colnames,
      container = header_container,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
    if (length(score_cols)) {
      dt <- DT::formatRound(dt, columns = score_cols, digits = 0)
      dt <- DT::formatStyle(
        dt,
        columns = score_cols,
        backgroundColor = DT::styleInterval(seq(10, 90, by = 10), pal),
        color = "#111111"
      )
      if (!is_total_view) {
        dt <- DT::formatStyle(
          dt,
          columns = bundle$pitcher_group_end_cols,
          borderRight = "3px solid #000000"
        )
      }
    }
    DT::formatStyle(
      dt,
      columns = "Hitter",
      backgroundColor = "#FBFAF7",
      fontWeight = "700"
    )
  })

  output$matchup_score_note <- renderUI({
    bundle <- matchup_grid_bundle()
    long <- bundle$long
    if (!nrow(long)) return(NULL)
    view_mode <- input$matchup_view %||% "pitch_types"
    scored_n <- sum(is.finite(long$MatchupScore))
    total_n <- nrow(long)
    view_text <- if (identical(view_mode, "total")) {
      "The Total view shows only the weighted overall score for each pitcher."
    } else {
      "The Pitch Types view shows the weighted Total plus Hard, Breaking, and Soft split scores for each pitcher."
    }
    tags$p(
      style = "margin:10px 0 0; color:#4b4030;",
      sprintf(
        "%s %s mode converts the hitter split and pitcher split into directional matchup edges, then adds those edges so double-favorable splits push high, double-unfavorable splits push low, and offsetting splits land near 50. The Total score weights Hard, Breaking, and Soft by that pitcher's usage%% against the hitter's side. Run Value uses per-100-pitch RV, while Whiff%% and SLG use their split rates. %d of %d cells had enough split data to score.",
        view_text, bundle$metric_label, scored_n, total_n
      )
    )
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
  
  output$pitcher_card_pitcher_ui <- renderUI({
    req(pitcher_std_all())
    d <- pitcher_std_all()
    pitchers <- d$PitcherName %>%
      as.character() %>%
      stats::na.omit() %>%
      unique() %>%
      sort()
    validate(need(length(pitchers) > 0, "No pitcher names found in the selected file(s)."))
    selectInput("pitcher_card_pitcher", "Pitcher", choices = pitchers, selected = pitchers[[1]])
  })

  pitcher_bust_meta <- reactive({
    req(pitcher_std_all(), input$pitcher_bust_hand)
    pitcher_bust_table_numeric(pitcher_std_all(), input$pitcher_bust_hand %||% "Total") %>%
      dplyr::distinct(PitcherName, Throws, PA) %>%
      dplyr::arrange(dplyr::desc(PA), PitcherName)
  })

  pitcher_bust_choices <- reactive({
    req(pitcher_bust_meta())
    pitcher_bust_meta()$PitcherName
  })

  observeEvent(pitcher_bust_choices(), {
    choices <- pitcher_bust_choices()
    selected <- apply_pitcher_bust_order(choices, input$pitcher_bust_order)
    updateSelectizeInput(
      session,
      "pitcher_bust_order",
      choices = choices,
      selected = selected,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  output$pitcher_bust_order_ui <- renderUI({
    req(pitcher_bust_meta())
    choices <- pitcher_bust_choices()
    validate(need(length(choices) > 0, "No pitchers found in the selected data."))
    tagList(
      selectizeInput(
        "pitcher_bust_order",
        "Pitchers (drag to set order):",
        choices = choices,
        selected = if (season_mode()) choices else character(0),
        multiple = TRUE,
        options = list(
          plugins = list("drag_drop", "remove_button"),
          persist = FALSE,
          placeholder = "Start typing a pitcher name"
        )
      ),
      helpText("Choose pitchers, then drag to set the report order.")
    )
  })

  pitcher_bust_order <- reactive({
    apply_pitcher_bust_order(pitcher_bust_choices(), input$pitcher_bust_order)
  })

  pitcher_bust_source_meta <- reactive({
    req(pitcher_std_all())
    d <- pitcher_std_all()
    validate(need(".source_file" %in% names(d), "Source file tracking is unavailable for the selected pitcher data."))
    d %>%
      dplyr::filter(!is.na(PitcherName), nzchar(trimws(PitcherName)), !is.na(.source_file), nzchar(trimws(.source_file))) %>%
      dplyr::distinct(PitcherName, .source_file) %>%
      dplyr::arrange(PitcherName, .source_file)
  })

  pitcher_bust_source_panel_open <- reactiveVal(FALSE)

  observeEvent(input$pitcher_bust_source_toggle, {
    pitcher_bust_source_panel_open(!isTRUE(pitcher_bust_source_panel_open()))
  }, ignoreInit = TRUE)

  output$pitcher_bust_source_ui <- renderUI({
    req(pitcher_bust_source_meta())
    if (!isTRUE(pitcher_bust_source_panel_open())) return(NULL)
    pitchers <- pitcher_bust_order()
    if (!length(pitchers)) {
      return(helpText("Add pitchers first, then choose all selected sources or one source per pitcher."))
    }
    meta <- pitcher_bust_source_meta()
    selectors <- lapply(pitchers, function(pitcher) {
      source_files <- meta %>%
        dplyr::filter(PitcherName == pitcher) %>%
        dplyr::pull(.source_file)
      selectInput(
        pitcher_bust_source_input_id(pitcher),
        pitcher,
        choices = pitcher_bust_source_choices(source_files),
        selected = "__ALL__"
      )
    })
    tags$div(
      class = "report-toolbar",
      style = "margin-bottom:8px;",
      selectors
    )
  })

  pitcher_bust_source_map <- reactive({
    pitchers <- pitcher_bust_order()
    if (!length(pitchers)) return(setNames(character(0), character(0)))
    vals <- vapply(pitchers, function(pitcher) {
      input[[pitcher_bust_source_input_id(pitcher)]] %||% "__ALL__"
    }, character(1))
    stats::setNames(vals, pitchers)
  })
  
	  pitcher_card_page_specs <- reactive({
	    req(input$pitcher_card_batter_side)
	    side <- input$pitcher_card_batter_side %||% "Total"
	    if (identical(side, "Total")) {
	      return(tibble::tibble(
	        batter_side = c("R", "L"),
	        split_label = c("vRHH", "vLHH"),
	        display_label = c("vs RHH", "vs LHH")
	      ))
	    }
	    tibble::tibble(
	      batter_side = side,
	      split_label = pitcher_card_split_tag(side),
	      display_label = if (identical(side, "R")) "vs RHH" else "vs LHH"
	    )
	  })
	  
	  output$pitcher_card_page_ui <- renderUI({
	    specs <- pitcher_card_page_specs()
	    if (nrow(specs) <= 1) return(NULL)
	    selectInput(
	      "pitcher_card_page_num",
	      "Page",
	      choices = stats::setNames(seq_len(nrow(specs)), specs$display_label),
	      selected = 1
	    )
	  })
	  
	  df_pitcher_card <- reactive({
	    req(pitcher_std_all(), input$pitcher_card_pitcher)
	    d <- pitcher_std_all() %>%
	      dplyr::filter(PitcherName == input$pitcher_card_pitcher)
	    validate(need(nrow(d) > 0, "No rows for this pitcher in the selected file(s)."))
	    d
	  })
	  
	  build_pitcher_card_pages <- function(d_base, side_choice){
	    specs <- if (identical(side_choice, "Total")) {
	      tibble::tibble(
	        batter_side = c("R", "L"),
	        split_label = c("vRHH", "vLHH"),
	        display_label = c("vs RHH", "vs LHH")
	      )
	    } else {
	      tibble::tibble(
	        batter_side = side_choice,
	        split_label = pitcher_card_split_tag(side_choice),
	        display_label = if (identical(side_choice, "R")) "vs RHH" else "vs LHH"
	      )
	    }
	    pitcher_name <- dplyr::first(stats::na.omit(d_base$PitcherName)) %||% (input$pitcher_card_pitcher %||% "Pitcher")
	    throws <- dplyr::first(stats::na.omit(d_base$PitcherThrows)) %||% ""
	    lapply(seq_len(nrow(specs)), function(i) {
	      spec <- specs[i, ]
	      d_page <- d_base %>% dplyr::filter(BatterSide == spec$batter_side[[1]])
	      if (!nrow(d_page)) {
	        ggplot() +
	          theme_void() +
	          ggtitle(paste(pitcher_name, "-", throws, "-", spec$split_label[[1]])) +
	          theme(plot.title = element_text(face = "bold", color = TXST_MAROON, hjust = 0.5)) +
	          annotate("text", x = 0.5, y = 0.5, label = paste("No rows for", spec$display_label[[1]]), size = 6)
	      } else {
	        pitcher_env$compose_report(d_page, spec$split_label[[1]])
	      }
	    })
	  }
	  
	  pitcher_card_pages <- eventReactive(input$pitcher_preview, {
	    d_base <- df_pitcher_card()
	    side_choice <- input$pitcher_card_batter_side %||% "Total"
	    build_pitcher_card_pages(d_base, side_choice)
	  }, ignoreInit = TRUE)
  
  output$pitcher_card <- renderPlot({
    pages <- pitcher_card_pages()
    req(length(pages) > 0)
    page_num <- suppressWarnings(as.integer(input$pitcher_card_page_num %||% 1))
    if (!is.finite(page_num) || page_num < 1) page_num <- 1L
    page_num <- min(page_num, length(pages))
    pages[[page_num]]
  }, res = preview_dpi)
  
  output$pitcher_bust_page_ui <- renderUI({
    req(pitcher_std_all(), input$pitcher_bust_hand)
    if (!length(pitcher_bust_order())) return(NULL)
    n_pages <- pitcher_bust_page_count(
      pitcher_std_all(),
      input$pitcher_bust_hand,
      pitcher_bust_order(),
      source_map = pitcher_bust_source_map()
    )
    if (n_pages <= 1) return(NULL)
    selectInput("pitcher_bust_page_num", "Page", choices = seq_len(n_pages), selected = 1)
  })
  
  pitcher_bust_page_plot <- reactive({
    req(pitcher_std_all(), input$pitcher_bust_hand)
    page_num <- input$pitcher_bust_page_num %||% 1
    pitcher_bust_tables_page(
      pitcher_std_all(),
      input$pitcher_bust_hand,
      page_num,
      pitcher_bust_order(),
      source_map = pitcher_bust_source_map()
    )
  })
  
  output$pitcher_bust_table <- renderPlot({
    req(pitcher_bust_page_plot())
    pitcher_bust_page_plot()
  }, res = preview_dpi)
  
  selected_files_slug <- function(default = "scouting"){
    files <- isolate(input$csv_files)
    if (is.null(files) || !length(files)) return(default)
    base <- tools::file_path_sans_ext(basename(files))
    gsub("[^A-Za-z0-9]+", "_", paste(base, collapse = "_"))
  }
  selected_team_code <- function(default = "TeamCode"){
    team_code_from_files(isolate(input$csv_files), default)
  }
  
  output$dl_pitcher_card_pdf <- downloadHandler(
    filename = function(){
      pitcher <- isolate(input$pitcher_card_pitcher %||% "Pitcher")
      paste0(pitcher_last_slug(pitcher), "_", pitcher_card_split_tag(isolate(input$pitcher_card_batter_side %||% "Total")), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      err_msg <- NULL
      d_base <- tryCatch(df_pitcher_card(), error = function(e){ err_msg <<- conditionMessage(e); NULL })
      if (is.null(d_base) || !nrow(d_base)) {
        write_error_pdf(file, err_msg %||% "No rows for selected pitcher.")
        return(invisible(NULL))
      }
      pages <- tryCatch(
        build_pitcher_card_pages(d_base, isolate(input$pitcher_card_batter_side %||% "Total")),
        error = function(e){ err_msg <<- paste("compose_report():", conditionMessage(e)); NULL }
      )
      if (is.null(pages) || !length(pages)) {
        write_error_pdf(file, err_msg %||% "Could not assemble pitcher card.")
        return(invisible(NULL))
      }
      res <- save_multi_page_pdf(file, pages, width_in = LETTER_W, height_in = LETTER_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.")
      }
    }
  )
  
  output$dl_pitcher_bust_pdf <- downloadHandler(
    filename = function(){
      paste0(selected_team_code(), " Pitch Type Splits.pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      err_msg <- NULL
      std <- tryCatch(pitcher_std_all(), error = function(e){ err_msg <<- conditionMessage(e); NULL })
      if (is.null(std)) {
        write_error_pdf(file, err_msg %||% "Failed while standardizing pitcher columns.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      pages <- tryCatch(
        {
          hand <- isolate(input$pitcher_bust_hand %||% "Total")
          order <- isolate(pitcher_bust_order())
          source_map <- isolate(pitcher_bust_source_map())
          if (!length(order)) {
            write_error_pdf(file, "Select at least one pitcher to export the bust table.", PORTRAIT_W, PORTRAIT_H)
            return(invisible(NULL))
          }
          n_pages <- pitcher_bust_page_count(std, hand, order, source_map = source_map)
          lapply(seq_len(n_pages), function(pg) {
            pitcher_bust_tables_page(std, hand, pg, order, source_map = source_map)
          })
        },
        error = function(e){ err_msg <<- paste("pitcher_bust_tables_page():", conditionMessage(e)); NULL }
      )
      if (is.null(pages)) {
        write_error_pdf(file, err_msg %||% "Could not assemble pitcher bust table.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      res <- save_multi_page_pdf(file, pages, width_in = PORTRAIT_W, height_in = PORTRAIT_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.", PORTRAIT_W, PORTRAIT_H)
      }
    }
  )
  
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
      paste0(selected_team_code(), "_Pitch_Types.pdf")
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
      pages <- tryCatch(
        {
          selected <- isolate(input$hitter_order)
          if (is.null(selected)) selected <- character(0)
          order <- apply_player_order(hitter_order_from_data(std, "game"), selected)
          export_hands <- c("RHP", "LHP")
          combine_sl_sw <- isTRUE(isolate(input$pitch_type_combine_sl_sw))
          lapply(export_hands, function(hand) {
            pitch_type_tables_page(std, hand, page_num = 1, player_order = order, combine_sl_sw = combine_sl_sw)
          })
        },
        error = function(e){ err_msg <<- paste("pitch_type_tables_page():", conditionMessage(e)); NULL }
      )
      if (is.null(pages)) {
        write_error_pdf(file, err_msg %||% "Could not assemble pitch type tables.", PORTRAIT_W, PORTRAIT_H)
        return(invisible(NULL))
      }
      res <- save_multi_page_pdf(file, pages, width_in = PORTRAIT_W, height_in = PORTRAIT_H)
      if (!isTRUE(res$ok)) {
        write_error_pdf(file, res$err %||% "Unknown error while generating the report.", PORTRAIT_W, PORTRAIT_H)
      }
    }
  )
  
  output$dl_heat_maps_pdf <- downloadHandler(
    filename = function(){
      paste0(selected_team_code(), "_HeatMaps.pdf")
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
