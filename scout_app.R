# ============================================================================
#  OPPONENT SCOUTING + ACQUISITIONS  —  VT / Brewster
#  Load a TrackMan CSV *or* a SQLite .db. Pitching + Hitting reports,
#  plus an Acquisitions tab that ranks prospects and cross-references an
#  (optional) reference roster to flag who's already signed elsewhere.
#  Standalone: no source() files.  Open and Run App.
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(DBI)
library(RSQLite)
suppressWarnings(suppressMessages(library(xgboost)))

options(shiny.maxRequestSize = 500 * 1024^2)

# ---- Pull models from a private HF dataset (token via Space secret HF_TOKEN) -
library(httr)
SCOUT_DATASET <- Sys.getenv("SCOUT_DATASET", "BrewsterWhitecapsMAC/REPLACE-ME")
.tok <- Sys.getenv("HF_TOKEN")
message(sprintf(">>> HF_TOKEN check — present:%s  length:%d  prefix:'%s'  (want length ~37-40, prefix 'hf_')",
                nzchar(.tok), nchar(.tok), substr(.tok, 1, 3)))
hf_get <- function(file, dest = file) {
  if (file.exists(dest)) return(dest)
  url <- paste0("https://huggingface.co/datasets/", SCOUT_DATASET, "/resolve/main/", file)
  resp <- tryCatch(
    httr::GET(url, httr::add_headers(Authorization = paste("Bearer", Sys.getenv("HF_TOKEN")))),
    error = function(e) { message("HF connection error (", file, "): ", conditionMessage(e)); NULL })
  if (is.null(resp)) return(dest)
  st <- httr::status_code(resp)
  if (st != 200) {
    message(sprintf(">>> HF download FAILED for '%s' — HTTP %s from dataset '%s'. ",
                    file, st, SCOUT_DATASET),
            "Check that HF_TOKEN can read this (org-)private dataset and SCOUT_DATASET is exact.")
    return(dest)
  }
  writeBin(httr::content(resp, "raw"), dest)
  message(sprintf(">>> HF download OK: %s (%s bytes) from %s",
                  file, format(file.info(dest)$size, big.mark = ","), SCOUT_DATASET))
  dest
}
hf_get("pitch_models.rds")
hf_get("xwoba_grid.rds")

# Trained models for live, in-app scoring of raw uploads (blank if not present).
MODELS <- tryCatch(if (file.exists("pitch_models.rds")) readRDS("pitch_models.rds") else NULL,
                   error = function(e) { message(">>> readRDS pitch_models.rds failed: ",
                                                  conditionMessage(e)); NULL })
XWGRID <- tryCatch(if (file.exists("xwoba_grid.rds")) readRDS("xwoba_grid.rds") else NULL,
                   error = function(e) { message(">>> readRDS xwoba_grid.rds failed: ",
                                                  conditionMessage(e)); NULL })
if (is.null(MODELS)) message(">>> MODELS is NULL — Stuff+/Location+/Pitching+ and all grades will be blank.")
if (is.null(XWGRID)) message(">>> XWGRID is NULL — xwOBA columns will be blank.")
# Linear-weights for full xwOBA (≈ recent MLB run-value scale)
WOBA_BB <- 0.69; WOBA_HBP <- 0.72

# ---- NA-safe helpers --------------------------------------------------------
qn   <- function(x, p) { x <- x[!is.na(x)]; if (!length(x)) return(NA_real_); as.numeric(stats::quantile(x, p)) }
mxn  <- function(x)    { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else max(x) }
mnn  <- function(x)    { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else mean(x) }
rate <- function(num, den) if (den > 0) num / den else NA_real_
pctl <- function(x) { n <- sum(!is.na(x)); if (n == 0) return(rep(NA_real_, length(x)))
                      rank(x, na.last = "keep", ties.method = "average") / n }

# Estimated arm angle from the release point (no biometrics, so it's an estimate):
# vertical rise above an assumed shoulder vs horizontal distance from the body.
# ~90 = over the top, ~0 = sidearm, negative = submarine. Vectorized.
arm_angle <- function(rh, rs) {
  horiz <- pmax(abs(rs) - 1.0, 0.05)
  atan2(rh - 5.0, horiz) * 180 / pi
}
arm_slot_label <- function(deg) dplyr::case_when(
  is.na(deg)  ~ NA_character_,
  deg >= 75   ~ "Over the top",
  deg >= 60   ~ "High 3/4",
  deg >= 40   ~ "3/4",
  deg >= 22   ~ "Low 3/4",
  deg >= 5    ~ "Sidearm",
  TRUE        ~ "Submarine")

# normalize a name to a join key: handles "Last, First" and "First Last"
norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[.'\"-]", "", x)
  x <- gsub(",", " ", x)
  vapply(strsplit(x, "\\s+"),
         function(t) { t <- t[t != ""]; paste(sort(t), collapse = " ") },
         character(1))
}

# ============================================================================
#  LIVE SCORING — compute Stuff+/Loc+/Pitch+, xRV, xwOBA from raw TrackMan
#  columns using the trained models. Mirrors score_and_store.R exactly.
# ============================================================================
score_pitches <- function(df) {
  if (is.null(MODELS)) return(df)
  PTL <- MODELS$pt_levels; HSIGN <- -1
  numv <- function(x) suppressWarnings(as.numeric(x))
  src <- c("TaggedPitchType","AutoPitchType","PitcherThrows","RelSpeed","SpinRate",
           "Extension","RelSide","RelHeight","HorzBreak","InducedVertBreak","SpinAxis",
           "PlateLocSide","PlateLocHeight","Balls","Strikes","BatterSide","Pitcher",
           "ExitSpeed","Angle")
  for (cc in src) if (!cc %in% names(df)) df[[cc]] <- NA
  g <- df %>% mutate(
    PT_raw = dplyr::coalesce(as.character(TaggedPitchType), as.character(AutoPitchType)),
    pt = dplyr::case_when(
      PT_raw %in% c("Fastball","FourSeamFastBall","Four-Seam","FF") ~ "FF",
      PT_raw %in% c("Sinker","TwoSeamFastBall","Two-Seam","SI","FT") ~ "SI",
      PT_raw %in% c("Cutter","FC") ~ "FC",
      PT_raw %in% c("Slider","Sweeper","Slurve","SL","ST") ~ "SL",
      PT_raw %in% c("Curveball","Knuckle Curve","KnuckleCurve","CU","KC") ~ "CU",
      PT_raw %in% c("ChangeUp","Changeup","CH") ~ "CH",
      PT_raw %in% c("Splitter","FS") ~ "FS", TRUE ~ NA_character_),
    arm = ifelse(PitcherThrows %in% c("Left","L"), -1, 1),
    release_speed = numv(RelSpeed), release_spin_rate = numv(SpinRate),
    release_extension = numv(Extension),
    release_pos_x = numv(RelSide) * arm * HSIGN, release_pos_z = numv(RelHeight),
    pfx_x = (numv(HorzBreak) / 12) * arm * HSIGN, pfx_z = numv(InducedVertBreak) / 12,
    spin_axis = ifelse(arm == 1, numv(SpinAxis), (360 - numv(SpinAxis)) %% 360),
    plate_x = numv(PlateLocSide) * arm * HSIGN, plate_z = numv(PlateLocHeight),
    balls = numv(Balls), strikes = numv(Strikes),
    out_of_zone = ifelse(!(abs(numv(PlateLocSide)) <= 0.83 &
                           dplyr::between(numv(PlateLocHeight), 1.5, 3.5)), 1L, 0L),
    stand_same = ifelse((arm == 1 & BatterSide %in% c("Right","R")) |
                        (arm == -1 & BatterSide %in% c("Left","L")), 1L, 0L))
  fb <- g %>% filter(pt %in% c("FF","FC","SI")) %>% group_by(Pitcher) %>%
    summarise(fb_velo = mean(release_speed, na.rm = TRUE), fb_ivb = quantile(pfx_z, .8, na.rm = TRUE),
              fb_xmax = quantile(pfx_x, .8, na.rm = TRUE), fb_xmin = quantile(pfx_x, .2, na.rm = TRUE),
              fb_axis = mean(spin_axis, na.rm = TRUE), .groups = "drop")
  g <- g %>% left_join(fb, by = "Pitcher") %>%
    mutate(velo_dif = release_speed - fb_velo, ivb_dif = fb_ivb - pfx_z,
           break_dif = (fb_xmax*.5 + fb_xmin*.5) - pfx_x, spin_dif = spin_axis - fb_axis)
  for (lv in PTL) g[[paste0("pt_", lv)]] <- as.integer(g$pt == lv)
  allf <- unique(c(MODELS$stuff$feats, MODELS$loc$feats, MODELS$pitch$feats))
  for (f in allf) if (!f %in% names(g)) g[[f]] <- NA_real_
  s1 <- function(m) { mdl <- xgboost::xgb.load.raw(m$model_raw)
    round(100 - 10 * ((predict(mdl, as.matrix(g[, m$feats])) - m$mean) / m$sd)) }
  df$StuffPlus    <- s1(MODELS$stuff)
  df$LocationPlus <- s1(MODELS$loc)
  df$PitchingPlus <- s1(MODELS$pitch)
  prp <- predict(xgboost::xgb.load.raw(MODELS$pitch$model_raw), as.matrix(g[, MODELS$pitch$feats]))
  df$xRV <- round(-(prp - MODELS$pitch$mean), 4)
  if (!is.null(XWGRID)) {
    lk <- function(ev, la) { out <- rep(NA_real_, length(ev)); ok <- !is.na(ev) & !is.na(la)
      ei <- findInterval(ev, XWGRID$ev_edges); li <- findInterval(la, XWGRID$la_edges)
      v <- ok & ei >= 1 & ei < length(XWGRID$ev_edges) & li >= 1 & li < length(XWGRID$la_edges)
      out[v] <- XWGRID$grid[cbind(ei[v], li[v])]; out }
    df$xwOBA <- round(lk(numv(df$ExitSpeed), numv(df$Angle)), 3)
  }
  df
}

# Debug: show exactly what the Stuff+ model ingests per pitch + its raw output.
debug_inputs <- function(d) {
  if (is.null(MODELS)) return(data.frame(Note = "pitch_models.rds not loaded — no debug."))
  numv <- function(x) suppressWarnings(as.numeric(x)); HSIGN <- -1; PTL <- MODELS$pt_levels
  for (cc in c("TaggedPitchType","AutoPitchType","PitcherThrows","RelSpeed","SpinRate",
               "Extension","RelSide","RelHeight","HorzBreak","InducedVertBreak","SpinAxis",
               "PlateLocSide","PlateLocHeight","Balls","Strikes","BatterSide","Pitcher"))
    if (!cc %in% names(d)) d[[cc]] <- NA
  g <- d %>% mutate(
    PT_raw = dplyr::coalesce(as.character(TaggedPitchType), as.character(AutoPitchType)),
    pt = dplyr::case_when(
      PT_raw %in% c("Fastball","FourSeamFastBall","Four-Seam","FF") ~ "FF",
      PT_raw %in% c("Sinker","TwoSeamFastBall","Two-Seam","SI","FT") ~ "SI",
      PT_raw %in% c("Cutter","FC") ~ "FC",
      PT_raw %in% c("Slider","Sweeper","Slurve","SL","ST") ~ "SL",
      PT_raw %in% c("Curveball","Knuckle Curve","KnuckleCurve","CU","KC") ~ "CU",
      PT_raw %in% c("ChangeUp","Changeup","CH") ~ "CH",
      PT_raw %in% c("Splitter","FS") ~ "FS", TRUE ~ NA_character_),
    arm = ifelse(PitcherThrows %in% c("Left","L"), -1, 1),
    release_speed = numv(RelSpeed), release_spin_rate = numv(SpinRate),
    release_extension = numv(Extension),
    release_pos_x = numv(RelSide) * arm * HSIGN, release_pos_z = numv(RelHeight),
    pfx_x = (numv(HorzBreak) / 12) * arm * HSIGN, pfx_z = numv(InducedVertBreak) / 12,
    spin_axis = ifelse(arm == 1, numv(SpinAxis), (360 - numv(SpinAxis)) %% 360),
    plate_x = numv(PlateLocSide) * arm * HSIGN, plate_z = numv(PlateLocHeight),
    balls = numv(Balls), strikes = numv(Strikes),
    out_of_zone = ifelse(!(abs(numv(PlateLocSide)) <= 0.83 &
                           dplyr::between(numv(PlateLocHeight), 1.5, 3.5)), 1L, 0L),
    stand_same = ifelse((arm == 1 & BatterSide %in% c("Right","R")) |
                        (arm == -1 & BatterSide %in% c("Left","L")), 1L, 0L))
  fb <- g %>% filter(pt %in% c("FF","FC","SI")) %>% group_by(Pitcher) %>%
    summarise(fb_velo = mean(release_speed, na.rm = TRUE), fb_ivb = quantile(pfx_z, .8, na.rm = TRUE),
              fb_xmax = quantile(pfx_x, .8, na.rm = TRUE), fb_xmin = quantile(pfx_x, .2, na.rm = TRUE),
              fb_axis = mean(spin_axis, na.rm = TRUE), .groups = "drop")
  g <- g %>% left_join(fb, by = "Pitcher") %>%
    mutate(velo_dif = release_speed - fb_velo, ivb_dif = fb_ivb - pfx_z,
           break_dif = (fb_xmax*.5 + fb_xmin*.5) - pfx_x, spin_dif = spin_axis - fb_axis)
  for (lv in PTL) g[[paste0("pt_", lv)]] <- as.integer(g$pt == lv)
  for (f in MODELS$stuff$feats) if (!f %in% names(g)) g[[f]] <- NA_real_
  mdl <- xgboost::xgb.load.raw(MODELS$stuff$model_raw)
  g$rawRV <- predict(mdl, as.matrix(g[, MODELS$stuff$feats]))
  g$St <- round(100 - 10 * ((g$rawRV - MODELS$stuff$mean) / MODELS$stuff$sd))
  g %>% filter(!is.na(PitchType)) %>% group_by(Pitch = PitchType) %>%
    summarise(N = dplyr::n(), Velo = round(mean(release_speed, na.rm = TRUE), 1),
      `pfx_z ft` = round(mean(pfx_z, na.rm = TRUE), 2),
      `pfx_x ft` = round(mean(pfx_x, na.rm = TRUE), 2),
      Spin = round(mean(release_spin_rate, na.rm = TRUE)),
      Ext = round(mean(release_extension, na.rm = TRUE), 1),
      relZ = round(mean(release_pos_z, na.rm = TRUE), 2),
      veloDif = round(mean(velo_dif, na.rm = TRUE), 1),
      ivbDif = round(mean(ivb_dif, na.rm = TRUE), 2),
      RawRV = round(mean(rawRV, na.rm = TRUE), 4),
      `Stuff+` = round(mean(St, na.rm = TRUE)), .groups = "drop") %>%
    arrange(desc(N))
}

# ============================================================================
#  ROW-LEVEL PREP
# ============================================================================
prep_pitches <- function(df) {
  need_chr <- c("Pitcher","Batter","PitcherThrows","BatterSide","PitcherTeam",
                "BatterTeam","TaggedPitchType","AutoPitchType","PitchCall",
                "KorBB","PlayResult")
  need_num <- c("RelSpeed","SpinRate","InducedVertBreak","HorzBreak","RelHeight",
                "RelSide","Extension","PlateLocSide","PlateLocHeight","ExitSpeed",
                "Angle","Balls","Strikes","Inning","PAofInning","PitchofPA",
                "StuffPlus","LocationPlus","PitchingPlus","Bearing","Distance","xRV","xwOBA")
  for (c in need_chr) if (!c %in% names(df)) df[[c]] <- NA_character_
  for (c in need_num) if (!c %in% names(df)) df[[c]] <- NA_real_
  for (c in need_num) df[[c]] <- suppressWarnings(as.numeric(df[[c]]))
  # If the plus columns aren't already present (i.e. a raw TrackMan upload),
  # compute them live from the trained models.
  if (!is.null(MODELS) && all(is.na(df$StuffPlus)))
    df <- tryCatch(score_pitches(df),
                   error = function(e) { message(">>> SCORING ERROR: ", conditionMessage(e)); df })
  if (!"Date"   %in% names(df)) df$Date   <- NA
  if (!"GameID" %in% names(df)) df$GameID <- paste(df$Date, df$Inning, sep = "_")

  df <- df %>%
    mutate(
      PitchType = dplyr::coalesce(TaggedPitchType, AutoPitchType),
      PitchType = ifelse(is.na(PitchType) | PitchType %in% c("", "Undefined"),
                         NA_character_, PitchType),
      PitchGroup = case_when(
        PitchType %in% c("Fastball","FourSeamFastBall","TwoSeamFastBall","Sinker","Cutter") ~ "Fastball",
        PitchType %in% c("ChangeUp","Splitter")                                             ~ "Offspeed",
        PitchType %in% c("Slider","Curveball","Sweeper","Slurve","Knuckle Curve")           ~ "Breaking",
        TRUE ~ "Other"),
      BatSide = case_when(BatterSide %in% c("Left","L")  ~ "L",
                          BatterSide %in% c("Right","R") ~ "R", TRUE ~ NA_character_),
      PThrows = case_when(PitcherThrows %in% c("Left","L")  ~ "L",
                          PitcherThrows %in% c("Right","R") ~ "R", TRUE ~ NA_character_))

  df <- df %>%
    arrange(GameID, Inning, PAofInning, PitchofPA) %>%
    group_by(GameID, Inning, PAofInning) %>%
    mutate(lastPitch = dplyr::row_number() == dplyr::n()) %>%
    ungroup()

  df %>%
    mutate(
      PACheck = lastPitch,
      BB  = PACheck & KorBB %in% c("Walk","IntentionalWalk"),
      K   = PACheck & KorBB == "Strikeout",
      HBP = PACheck & (PlayResult == "HitByPitch" | PitchCall == "HitByPitch"),
      SF  = PACheck & PlayResult %in% c("Sacrifice","SacFly"),
      X1B = PACheck & PlayResult == "Single",
      X2B = PACheck & PlayResult == "Double",
      X3B = PACheck & PlayResult == "Triple",
      HR  = PACheck & PlayResult == "HomeRun",
      Hit = X1B | X2B | X3B | HR,
      AB  = PACheck & !BB & !HBP & !SF,
      Swing = PitchCall %in% c("InPlay","StrikeSwinging","FoulBall",
                               "FoulBallNotFieldable","FoulBallFieldable","FoulTip"),
      WhiffP  = Swing & PitchCall == "StrikeSwinging",
      Contact = Swing & PitchCall %in% c("InPlay","FoulBall","FoulBallNotFieldable",
                                         "FoulBallFieldable","FoulTip"),
      InZone  = dplyr::between(PlateLocSide, -0.83, 0.83) &
                dplyr::between(PlateLocHeight, 1.5, 3.5),
      OutZone = !InZone,
      Chase   = OutZone & Swing,
      BBE     = PitchCall == "InPlay" & !is.na(ExitSpeed),
      FirstPitch = Balls == 0 & Strikes == 0,
      TwoStrike  = Strikes == 2) %>%
    group_by(GameID, Inning, PAofInning) %>%
    mutate(paval_tmp = dplyr::case_when(
             KorBB == "Strikeout"        ~ 0,
             KorBB == "Walk"             ~ WOBA_BB,
             KorBB == "IntentionalWalk"  ~ NA_real_,
             HBP                          ~ WOBA_HBP,
             PitchCall == "InPlay"        ~ xwOBA,
             TRUE                         ~ NA_real_),
           paWOBA = { v <- paval_tmp[!is.na(paval_tmp)]; if (length(v)) v[[1]] else NA_real_ }) %>%
    ungroup() %>% dplyr::select(-paval_tmp)
}

# ============================================================================
#  STAT BLOCKS
# ============================================================================
hitter_line <- function(d) {
  pitches <- nrow(d)
  pa  <- sum(d$PACheck, na.rm = TRUE); ab <- sum(d$AB, na.rm = TRUE)
  h   <- sum(d$Hit, na.rm = TRUE); bb <- sum(d$BB, na.rm = TRUE)
  k   <- sum(d$K, na.rm = TRUE); hbp <- sum(d$HBP, na.rm = TRUE); sf <- sum(d$SF, na.rm = TRUE)
  tb  <- sum(d$X1B, na.rm = TRUE) + 2*sum(d$X2B, na.rm = TRUE) +
         3*sum(d$X3B, na.rm = TRUE) + 4*sum(d$HR, na.rm = TRUE)
  swings <- sum(d$Swing, na.rm = TRUE); whiffs <- sum(d$WhiffP, na.rm = TRUE)
  fp <- sum(d$FirstPitch, na.rm = TRUE); fpsw <- sum(d$FirstPitch & d$Swing, na.rm = TRUE)
  ooz <- sum(d$OutZone, na.rm = TRUE); chase <- sum(d$Chase, na.rm = TRUE)
  obp <- rate(h + bb + hbp, ab + bb + hbp + sf); slg <- rate(tb, ab)
  tibble(
    P = pitches, `1stP Sw%` = rate(fpsw, fp), `Sw%` = rate(swings, pitches),
    `Miss%` = rate(whiffs, swings), `SwStr%` = rate(whiffs, pitches),
    `Chase%` = rate(chase, ooz), `Contact%` = rate(sum(d$Contact, na.rm = TRUE), swings),
    EV = round(mnn(d$ExitSpeed[d$BBE]), 1), LA = round(mnn(d$Angle[d$BBE]), 1),
    AVG = round(rate(h, ab), 3), OBP = round(obp, 3), SLG = round(slg, 3),
    OPS = round(ifelse(is.na(obp) | is.na(slg), NA_real_, obp + slg), 3),
    `K%` = rate(k, pa), `BB%` = rate(bb, pa))
}

# Estimated arm slot (degrees) from release point. 90 = straight over the top,
# ~45 = high 3/4, ~20 = low 3/4, 0 or below = sidearm/submarine. Shoulder pivot
# is approximated at 5.0 ft since TrackMan has no pose data — so it's an estimate.
arm_slot <- function(relH, relS) {
  h <- mnn(relH); s <- mnn(relS)
  if (is.na(h) || is.na(s)) return(NA_real_)
  round(atan2(h - 5.0, abs(s)) * 180 / pi)
}

pitcher_arsenal <- function(d) {
  tot <- nrow(d)
  d %>% filter(!is.na(PitchType)) %>% group_by(PitchType) %>%
    summarise(N = dplyr::n(), Usage = dplyr::n()/tot,
              VLo = round(qn(RelSpeed, .15), 1), VHi = round(qn(RelSpeed, .85), 1),
              VTop = round(mxn(RelSpeed), 1), IVB = round(mnn(InducedVertBreak),1),
              HB = round(mnn(HorzBreak),1), Spin = round(mnn(SpinRate)),
              Ext = round(mnn(Extension),1), Slot = arm_slot(RelHeight, RelSide),
              Zone = mnn(InZone),
              Whiff = rate(sum(WhiffP,na.rm=TRUE), sum(Swing,na.rm=TRUE)),
              Chase = rate(sum(Chase,na.rm=TRUE), sum(OutZone,na.rm=TRUE)),
              St = round(mnn(StuffPlus)), Lo = round(mnn(LocationPlus)),
              Pi = round(mnn(PitchingPlus)), XW = round(mnn(xwOBA), 3),
              .groups = "drop") %>%
    arrange(desc(Usage)) %>%
    mutate(Velo = ifelse(is.na(VLo), "—", paste0(VLo,"-",VHi," (T",VTop,")"))) %>%
    transmute(Pitch = PitchType, `Usage%` = Usage, `#` = N, Velo, IVB, HB,
              Spin, Ext, `Slot°` = Slot, xwOBAcon = XW)
}

pitcher_split <- function(d) {
  d %>% filter(!is.na(BatSide)) %>% group_by(BatSide) %>% group_split() %>%
    map_dfr(function(g) { hl <- hitter_line(g)
      tibble(Split = ifelse(g$BatSide[1]=="L","vs LHH","vs RHH"),
             `Stuff+` = round(mnn(g$StuffPlus)), `Pitch+` = round(mnn(g$PitchingPlus)),
             PA = sum(g$PACheck, na.rm=TRUE), AVG = hl$AVG, OPS = hl$OPS,
             OBP = hl$OBP, SLG = hl$SLG, `Whiff%` = hl$`Miss%`,
             `Chase%` = hl$`Chase%`, `K%` = hl$`K%`, `BB%` = hl$`BB%`) }) %>%
    arrange(Split)
}

# Stuff+/Location+/Pitching+ per pitch type, split vs LHH / vs RHH
pplus_split <- function(d) {
  d %>% filter(!is.na(PitchType), !is.na(BatSide)) %>%
    group_by(Pitch = PitchType, Side = ifelse(BatSide == "L", "vs LHH", "vs RHH")) %>%
    summarise(`#` = dplyr::n(), `Stuff+` = round(mnn(StuffPlus)),
              `Loc+` = round(mnn(LocationPlus)), `Pitch+` = round(mnn(PitchingPlus)),
              .groups = "drop") %>%
    arrange(Pitch, Side)
}

# Per-pitch grades (+) and outcomes for one batter side ("L" or "R")
pitcher_perf_side <- function(d, side) {
  d %>% filter(!is.na(PitchType), BatSide == side) %>% group_by(PitchType) %>%
    summarise(N = dplyr::n(), St = round(mnn(StuffPlus)), Lo = round(mnn(LocationPlus)),
              Pi = round(mnn(PitchingPlus)), RV = sum(xRV, na.rm = TRUE), XW = mnn(xwOBA),
              Zone = mnn(InZone),
              Whiff = rate(sum(WhiffP, na.rm = TRUE), sum(Swing, na.rm = TRUE)),
              Chase = rate(sum(Chase, na.rm = TRUE), sum(OutZone, na.rm = TRUE)),
              .groups = "drop") %>%
    arrange(desc(N)) %>%
    transmute(Pitch = PitchType, `#` = N, `Stuff+` = St, `Loc+` = Lo, `Pitch+` = Pi,
              RV = round(RV, 1), xwOBAcon = round(XW, 3),
              `Zone%` = Zone, `Whiff%` = Whiff, `Chase%` = Chase)
}

pmix_wide <- function(d) {
  m <- d %>% filter(!is.na(PitchType), !is.na(BatSide)) %>%
    group_by(Side = ifelse(BatSide=="L","vs LHH","vs RHH"), PitchType) %>%
    summarise(n = dplyr::n(), .groups = "drop_last") %>%
    mutate(Usage = n/sum(n)) %>% ungroup() %>%
    select(Side, PitchType, Usage) %>%
    pivot_wider(names_from = Side, values_from = Usage, values_fill = 0)
  if (!"vs LHH" %in% names(m)) m$`vs LHH` <- 0
  if (!"vs RHH" %in% names(m)) m$`vs RHH` <- 0
  m %>% arrange(desc(`vs LHH` + `vs RHH`)) %>%
    transmute(Pitch = PitchType, `vs LHH`, `vs RHH`)
}

pusage <- function(d) {
  mk <- function(g, lbl) { if (nrow(g)==0) return(NULL)
    g %>% group_by(PitchType) %>%
      summarise(Usage = dplyr::n()/nrow(g),
                Whiff = rate(sum(WhiffP,na.rm=TRUE), sum(Swing,na.rm=TRUE)),
                .groups = "drop") %>% arrange(desc(Usage)) %>%
      transmute(Situation = lbl, Pitch = PitchType, `Usage%` = Usage, `Whiff%` = Whiff) }
  bind_rows(mk(d %>% filter(FirstPitch, !is.na(PitchType)), "First Pitch"),
            mk(d %>% filter(TwoStrike,  !is.na(PitchType)), "Two Strikes"))
}

lineup_table <- function(d, split, minp = 1) {
  if (split == "LHP") d <- d %>% filter(PThrows == "L")
  if (split == "RHP") d <- d %>% filter(PThrows == "R")
  d <- d %>% filter(!is.na(Batter))
  if (nrow(d) == 0) return(d[0, , drop = FALSE])
  d %>% group_by(Batter) %>%
    summarise(
      P = dplyr::n(), pa = sum(PACheck, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE), h = sum(Hit, na.rm = TRUE), bb = sum(BB, na.rm = TRUE),
      k = sum(K, na.rm = TRUE), hbp = sum(HBP, na.rm = TRUE), sf = sum(SF, na.rm = TRUE),
      tb = sum(X1B, na.rm = TRUE) + 2*sum(X2B, na.rm = TRUE) +
           3*sum(X3B, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE),
      swings = sum(Swing, na.rm = TRUE), whiffs = sum(WhiffP, na.rm = TRUE),
      contact = sum(Contact, na.rm = TRUE),
      fp = sum(FirstPitch, na.rm = TRUE), fpsw = sum(FirstPitch & Swing, na.rm = TRUE),
      ooz = sum(OutZone, na.rm = TRUE), chase = sum(Chase, na.rm = TRUE),
      EV = round(mnn(ExitSpeed[BBE]), 1), LA = round(mnn(Angle[BBE]), 1),
      xw = round(mnn(xwOBA), 3), xwf = round(mnn(paWOBA[PACheck]), 3),
      .groups = "drop") %>%
    filter(P >= minp) %>%
    mutate(
      `1stP Sw%` = ifelse(fp > 0, fpsw / fp, NA_real_),
      `Sw%`      = ifelse(P > 0, swings / P, NA_real_),
      `Miss%`    = ifelse(swings > 0, whiffs / swings, NA_real_),
      `SwStr%`   = ifelse(P > 0, whiffs / P, NA_real_),
      `Chase%`   = ifelse(ooz > 0, chase / ooz, NA_real_),
      `Contact%` = ifelse(swings > 0, contact / swings, NA_real_),
      AVG = round(ifelse(ab > 0, h / ab, NA_real_), 3),
      OBP = round(ifelse((ab+bb+hbp+sf) > 0, (h+bb+hbp)/(ab+bb+hbp+sf), NA_real_), 3),
      SLG = round(ifelse(ab > 0, tb / ab, NA_real_), 3),
      OPS = round(ifelse(is.na(OBP) | is.na(SLG), NA_real_, OBP + SLG), 3),
      `K%`  = ifelse(pa > 0, k / pa, NA_real_),
      `BB%` = ifelse(pa > 0, bb / pa, NA_real_)) %>%
    transmute(NAME = Batter, P, `1stP Sw%`, `Sw%`, `Miss%`, `SwStr%`, `Chase%`,
              `Contact%`, EV, LA, AVG, OBP, SLG, OPS, xwOBA = xwf, xwOBAcon = xw, `K%`, `BB%`) %>%
    arrange(desc(P))
}

btype_table <- function(d, split) {
  if (split == "LHP") d <- d %>% filter(PThrows == "L")
  if (split == "RHP") d <- d %>% filter(PThrows == "R")
  d %>% filter(PitchGroup %in% c("Fastball","Breaking","Offspeed")) %>%
    group_by(PitchGroup) %>% group_split() %>%
    map_dfr(function(g) { hl <- hitter_line(g)
      tibble(`Pitch Group` = g$PitchGroup[1], P = hl$P, `Sw%` = hl$`Sw%`,
             `Miss%` = hl$`Miss%`, `Chase%` = hl$`Chase%`,
             `Contact%` = hl$`Contact%`, EV = hl$EV, xwOBAcon = round(mnn(g$xwOBA), 3),
             OPS = hl$OPS) }) %>%
    arrange(match(`Pitch Group`, c("Fastball","Breaking","Offspeed")))
}

# ---- Acquisition grades -----------------------------------------------------
acq_pitchers <- function(d, minp) {
  out <- d %>% filter(!is.na(Pitcher)) %>%
    group_by(Pitcher) %>%
    summarise(
      Hand    = dplyr::first(PThrows),
      SeenAt  = dplyr::first(PitcherTeam),
      Pitches = dplyr::n(),
      FBVelo  = round(mnn(RelSpeed[PitchGroup == "Fastball"]), 1),
      TopVelo = round(mxn(RelSpeed)),
      swings = sum(Swing, na.rm = TRUE), whiffs = sum(WhiffP, na.rm = TRUE),
      ooz = sum(OutZone, na.rm = TRUE), chase = sum(Chase, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE), h = sum(Hit, na.rm = TRUE), bb = sum(BB, na.rm = TRUE),
      hbp = sum(HBP, na.rm = TRUE), sf = sum(SF, na.rm = TRUE),
      tb = sum(X1B, na.rm = TRUE) + 2*sum(X2B, na.rm = TRUE) +
           3*sum(X3B, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE),
      .groups = "drop") %>%
    filter(Pitches >= minp) %>%
    mutate(
      `Whiff%` = ifelse(swings > 0, whiffs / swings, NA_real_),
      `Chase%` = ifelse(ooz > 0, chase / ooz, NA_real_),
      obp = ifelse((ab + bb + hbp + sf) > 0, (h + bb + hbp) / (ab + bb + hbp + sf), NA_real_),
      slg = ifelse(ab > 0, tb / ab, NA_real_),
      `OPS Agst` = round(ifelse(is.na(obp) | is.na(slg), NA_real_, obp + slg), 3)) %>%
    transmute(Player = Pitcher, Hand, SeenAt, Pitches, FBVelo, TopVelo,
              `Whiff%`, `Chase%`, `OPS Agst`)
  if (nrow(out) == 0) return(out)
  out %>% mutate(Grade = round(100 * rowMeans(
    cbind(pctl(FBVelo), pctl(`Whiff%`), pctl(`Chase%`), pctl(-`OPS Agst`)),
    na.rm = TRUE))) %>% arrange(desc(Grade))
}

acq_hitters <- function(d, minp) {
  out <- d %>% filter(!is.na(Batter)) %>%
    group_by(Batter) %>%
    summarise(
      Side    = dplyr::first(BatSide),
      SeenAt  = dplyr::first(BatterTeam),
      Pitches = dplyr::n(),
      EV = round(mnn(ExitSpeed[BBE]), 1),
      swings = sum(Swing, na.rm = TRUE), whiffs = sum(WhiffP, na.rm = TRUE),
      contact = sum(Contact, na.rm = TRUE),
      ooz = sum(OutZone, na.rm = TRUE), chase = sum(Chase, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE), h = sum(Hit, na.rm = TRUE), bb = sum(BB, na.rm = TRUE),
      hbp = sum(HBP, na.rm = TRUE), sf = sum(SF, na.rm = TRUE),
      tb = sum(X1B, na.rm = TRUE) + 2*sum(X2B, na.rm = TRUE) +
           3*sum(X3B, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE),
      .groups = "drop") %>%
    filter(Pitches >= minp) %>%
    mutate(
      obp = ifelse((ab + bb + hbp + sf) > 0, (h + bb + hbp) / (ab + bb + hbp + sf), NA_real_),
      slg = ifelse(ab > 0, tb / ab, NA_real_),
      OPS = round(ifelse(is.na(obp) | is.na(slg), NA_real_, obp + slg), 3),
      `Contact%` = ifelse(swings > 0, contact / swings, NA_real_),
      `Chase%` = ifelse(ooz > 0, chase / ooz, NA_real_),
      `Whiff%` = ifelse(swings > 0, whiffs / swings, NA_real_)) %>%
    transmute(Player = Batter, Side, SeenAt, Pitches, EV, OPS,
              `Contact%`, `Chase%`, `Whiff%`)
  if (nrow(out) == 0) return(out)
  out %>% mutate(Grade = round(100 * rowMeans(
    cbind(pctl(EV), pctl(OPS), pctl(`Contact%`), pctl(-`Whiff%`)),
    na.rm = TRUE))) %>% arrange(desc(Grade))
}

# join reference roster -> College + current Status
attach_status <- function(tbl, ref) {
  if (is.null(tbl) || nrow(tbl) == 0) return(tbl)
  tbl$.key <- norm_name(tbl$Player)
  if (is.null(ref)) { tbl$College <- NA_character_; tbl$Status <- "no roster loaded"
                      return(tbl %>% select(-.key)) }
  out <- tbl %>% left_join(ref, by = ".key")
  out %>% mutate(
      Status = ifelse(is.na(RefTeam), "AVAILABLE?",
                paste0(RefTeam, ifelse(is.na(RefLeague), "", paste0(" (", RefLeague, ")")))),
      College = College) %>%
    select(-.key, -RefTeam, -RefLeague)
}

# ============================================================================
#  TABLE RENDERER
# ============================================================================
zone_heat <- function(d, mode = c("freq", "whiff", "damage", "xwoba", "xwobafull")) {
  mode <- match.arg(mode)
  d <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
  iz <- d %>% filter(InZone)
  if (nrow(iz) == 0) return(div("No in-zone pitches for this selection.",
                                style = "color:#888;padding:10px;"))
  col_idx <- cut(iz$PlateLocSide,   c(-0.83, -0.277, 0.277, 0.83),
                 labels = FALSE, include.lowest = TRUE)
  row_idx <- cut(iz$PlateLocHeight, c(1.5, 2.17, 2.83, 3.5),
                 labels = FALSE, include.lowest = TRUE)
  val <- matrix(NA_real_, 3, 3)
  for (r in 1:3) for (cc in 1:3) {
    sel <- which(row_idx == r & col_idx == cc)
    if (mode == "freq") val[r, cc] <- length(sel) / nrow(iz)
    else if (mode == "whiff") { sw <- sum(iz$Swing[sel], na.rm = TRUE); wh <- sum(iz$WhiffP[sel], na.rm = TRUE)
           val[r, cc] <- if (sw > 0) wh / sw else NA_real_ }
    else if (mode == "xwoba") { xw <- iz$xwOBA[sel][iz$BBE[sel] %in% TRUE]
           val[r, cc] <- if (sum(!is.na(xw)) > 0) mean(xw, na.rm = TRUE) else NA_real_ }
    else if (mode == "xwobafull") { pw <- iz$paWOBA[sel]
           val[r, cc] <- if (sum(!is.na(pw)) > 0) mean(pw, na.rm = TRUE) else NA_real_ }
    else { evv <- iz$ExitSpeed[sel][iz$BBE[sel] %in% TRUE]
           val[r, cc] <- if (sum(!is.na(evv)) > 0) mean(evv, na.rm = TRUE) else NA_real_ }
  }
  if (mode == "damage") { norm <- function(v) pmin(pmax((v - 75) / 25, 0), 1)
  } else if (mode == "xwoba") { norm <- function(v) pmin(pmax((v - .250) / .300, 0), 1)
  } else if (mode == "xwobafull") { norm <- function(v) pmin(pmax((v - .200) / .250, 0), 1)
  } else { mx <- suppressWarnings(max(val, na.rm = TRUE)); if (!is.finite(mx) || mx == 0) mx <- 1
           norm <- function(v) v / mx }
  cellcol <- function(v) { if (is.na(v)) return("#eef0f3"); t <- norm(v)
    if (mode == "freq")       sprintf("rgb(%d,%d,%d)", round(235 - 225*t), round(238 - 203*t), round(242 - 176*t))
    else if (mode == "whiff") sprintf("rgb(%d,%d,%d)", round(235 - 209*t), round(238 - 75*t), round(242 - 61*t))
    else                      sprintf("rgb(%d,%d,%d)", 235, round(238 - 188*t), round(242 - 202*t)) }
  txtcol <- function(v) { if (is.na(v)) return("#10233a"); if (norm(v) > 0.5) "#ffffff" else "#10233a" }
  fmt <- function(v) if (is.na(v)) "" else if (mode == "damage") as.character(round(v)) else if (mode %in% c("xwoba","xwobafull")) sub("^0", "", sprintf("%.3f", v)) else paste0(round(100*v), "%")
  rows <- lapply(3:1, function(r)
    tags$tr(lapply(1:3, function(cc)
      tags$td(fmt(val[r, cc]),
        style = paste0("background:", cellcol(val[r, cc]),
          ";color:", txtcol(val[r, cc]),
          ";text-align:center;padding:18px 0;font-weight:700;",
          "border:1px solid #d7dee7;width:33%;")))))
  div(style = "text-align:center;",
      tags$table(style = "width:260px;border-collapse:collapse;margin:6px auto;", rows),
      tags$div("Catcher's view — top = up in zone",
               style = "color:#888;font-size:11px;margin-top:4px;"))
}

# Launch-angle distribution of batted balls (drawn inside renderPlot)
draw_la <- function(d) {
  a <- d$Angle[d$BBE %in% TRUE]; a <- a[!is.na(a) & a >= -90 & a <= 90]
  op <- par(mar = c(4, 4, 1, 1), bg = "white"); on.exit(par(op))
  if (!length(a)) { plot.new(); text(0.5, 0.5, "No batted balls", col = "#888"); return(invisible()) }
  h <- hist(a, breaks = seq(-90, 90, 5), plot = FALSE)
  plot(h, col = "#1aa3b5", border = "white", main = "", las = 1,
       xlab = "Launch angle (deg)", ylab = "Batted balls", xlim = c(-60, 80))
  abline(v = c(10, 25, 50), col = "#bbbbbb", lty = 2)
  mtext(c("GB", "LD", "FB", "PU"), side = 3, line = -1,
        at = c(0, 17, 37, 65), col = "#888", cex = 0.85)
}

# Spray chart of batted balls (drawn inside renderPlot). Bearing: 0 = CF,
# negative = LF (3B side), positive = RF (1B side). Flip the sign if pull/oppo look reversed.
draw_spray <- function(d) {
  b <- d[d$BBE %in% TRUE & !is.na(d$Bearing) & !is.na(d$Distance), ]
  op <- par(mar = c(0, 0, 0, 0), bg = "white"); on.exit(par(op))
  plot(NA, xlim = c(-260, 260), ylim = c(-15, 420), asp = 1, axes = FALSE, xlab = "", ylab = "")
  fl <- 340
  segments(0, 0,  fl * sin(45 * pi/180), fl * cos(45 * pi/180), col = "#cccccc")
  segments(0, 0, -fl * sin(45 * pi/180), fl * cos(45 * pi/180), col = "#cccccc")
  th <- seq(-45, 45, 1) * pi/180
  lines(400 * sin(th), 400 * cos(th), col = "#cccccc")
  bs <- 90 / sqrt(2)
  lines(c(0, bs, 0, -bs, 0), c(0, bs, 2*bs, bs, 0), col = "#dddddd")
  if (nrow(b)) {
    x <- b$Distance * sin(b$Bearing * pi/180)
    y <- b$Distance * cos(b$Bearing * pi/180)
    hit <- b$PlayResult %in% c("Single", "Double", "Triple", "HomeRun")
    points(x, y, pch = 19, cex = 0.7, col = ifelse(hit, "#a83b3b", "#0a2342"))
    legend("topright", c("Hit", "Out/other"), pch = 19,
           col = c("#a83b3b", "#0a2342"), bty = "n", cex = 0.9)
  } else text(0, 200, "No batted-ball locations", col = "#888")
}

make_table <- function(df, pct = character(), d3 = character(),
                        d1 = character(), int = character()) {
  if (is.null(df) || nrow(df) == 0)
    return(reactable(data.frame(Note = "No data for this selection.")))
  defs <- lapply(seq_along(names(df)), function(i) {
    nm  <- names(df)[i]
    stk <- if (i == 1) "left" else NULL
    sty <- if (i == 1) list(background = "#ffffff", borderRight = "1px solid #dbe3ec",
                            fontWeight = 600) else NULL
    if      (nm %in% pct) colDef(format = colFormat(percent = TRUE, digits = 0), align = "center", sticky = stk, style = sty)
    else if (nm %in% d3)  colDef(format = colFormat(digits = 3), align = "center", sticky = stk, style = sty)
    else if (nm %in% d1)  colDef(format = colFormat(digits = 1), align = "center", sticky = stk, style = sty)
    else if (nm %in% int) colDef(format = colFormat(digits = 0), align = "center", sticky = stk, style = sty)
    else                  colDef(align = "left", sticky = stk, style = sty) })
  names(defs) <- names(df)
  reactable(df, columns = defs, pagination = FALSE, compact = TRUE,
            borderless = TRUE, defaultPageSize = 9999, highlight = TRUE,
            searchable = nrow(df) > 12,
            theme = reactableTheme(
              backgroundColor = "#ffffff", color = "#10233a",
              borderColor = "#dbe3ec", highlightColor = "#eef4fb",
              cellStyle = list(fontSize = "14px", padding = "8px 10px"),
              headerStyle = list(background = "#0a2342", color = "#ffffff",
                                 fontWeight = "bold", borderBottom = "3px solid #1aa3b5",
                                 textTransform = "uppercase", fontSize = "12px",
                                 letterSpacing = "0.4px")))
}
PCT_HIT <- c("1stP Sw%","Sw%","Miss%","SwStr%","Chase%","Contact%","K%","BB%")
D3_HIT  <- c("AVG","OBP","SLG","OPS")

# ============================================================================
#  20-80 GRADE ENGINE  — data-driven FIRST PASS. TUNE THESE ANCHORS TO YOUR EYE.
#  Stepwise: value >= brk[i]  ->  grade ladder G[i].  Two scales: college / MLB.
#  Current = avg velo, Future = peak (T) velo / max EV (the ceiling).
# ============================================================================
G <- c(80, 70, 65, 60, 55, 50, 45, 40, 35)
grade_step <- function(x, brks) {
  if (is.na(x)) return(NA_real_)
  for (i in seq_along(brks)) if (x >= brks[i]) return(G[i])
  G[length(G)]
}
FBV_COL <- c(96,95,94,93,92,91,90,88,0);          FBV_MLB <- c(100,99,98,97,95,94,92,90,0)
IVB_COL <- c(21,19,18,17,15,13,11,9,-99);         IVB_MLB <- c(22,20,19,18,16,14,12,10,-99)
BRK_COL <- c(20,17,15,13,11,9,7,5,-99);           BRK_MLB <- c(22,19,17,15,13,11,9,7,-99)
CHV_COL <- c(12,11,10,9,8,7,6,5,-99);             CHV_MLB <- c(13,12,11,10,9,8,7,6,-99)
ZON_COL <- c(.58,.55,.53,.51,.49,.47,.45,.42,0);  ZON_MLB <- c(.60,.57,.55,.53,.51,.49,.46,.43,0)
HIT_COL <- c(.88,.85,.83,.80,.78,.75,.72,.68,0);  HIT_MLB <- c(.90,.87,.85,.82,.80,.77,.74,.70,0)
PWR_COL <- c(95,93,91,90,88,86,84,82,0);          PWR_MLB <- c(98,96,94,92,90,88,86,84,0)
AVG_COL <- c(.360,.335,.320,.300,.285,.270,.250,.225,0)
AVG_MLB <- c(.380,.355,.340,.320,.305,.290,.270,.245,0)
XWOBA_COL <- c(.430,.410,.395,.380,.365,.350,.330,.305,0)
XWOBA_MLB <- c(.450,.430,.415,.400,.385,.370,.350,.320,0)
blend_g <- function(a, b, wa = 0.5) {
  if (is.na(a) && is.na(b)) return(NA_real_)
  if (is.na(a)) return(b); if (is.na(b)) return(a)
  round(wa * a + (1 - wa) * b)
}
blend3 <- function(a, b, cc, wa, wb, wc) {
  v <- c(a, b, cc); w <- c(wa, wb, wc); ok <- !is.na(v)
  if (!any(ok)) return(NA_real_)
  round(sum(v[ok] * w[ok]) / sum(w[ok]))
}

pfam <- function(pt) dplyr::case_when(
  pt %in% c("Fastball","FourSeamFastBall")                          ~ "FB",
  pt %in% c("Sinker","TwoSeamFastBall")                             ~ "SI",
  pt == "Cutter"                                                    ~ "CT",
  pt %in% c("Slider","Sweeper","Curveball","Slurve","Knuckle Curve")~ "BRK",
  pt %in% c("ChangeUp","Splitter")                                  ~ "CH",
  TRUE ~ "OTH")

# Map a "+" metric to the 20-80 scale. MLB: 100 = 50, each point = 1 grade point.
# College: re-center vs the loaded population (mean m, sd s) so 50 = avg arm here.
g2080_mlb <- function(p) if (is.na(p)) NA_real_ else max(20, min(80, round(50 + (p - 100))))
g2080_col <- function(p, m, s) {
  if (is.na(p)) return(NA_real_)
  if (is.null(m) || is.na(m) || is.na(s) || s == 0) return(g2080_mlb(p))
  max(20, min(80, round(50 + 10 * (p - m) / s)))
}

pitcher_grade_detail <- function(d, minpitch = 15, pop = NULL) {
  ar <- d %>% filter(!is.na(PitchType)) %>% group_by(PitchType) %>%
    summarise(N = dplyr::n(), st_avg = mnn(StuffPlus), st_top = qn(StuffPlus, .80),
              .groups = "drop") %>%
    filter(N >= minpitch) %>% arrange(desc(N))
  if (nrow(ar) == 0) return(NULL)
  sm <- if (is.null(pop)) NA_real_ else pop$st_m; ss <- if (is.null(pop)) NA_real_ else pop$st_s
  lm <- if (is.null(pop)) NA_real_ else pop$lo_m; ls <- if (is.null(pop)) NA_real_ else pop$lo_s
  pm <- if (is.null(pop)) NA_real_ else pop$pi_m; ps <- if (is.null(pop)) NA_real_ else pop$pi_s
  rows <- ar %>% transmute(Category = PitchType,                     # each pitch <- Stuff+
    `Coll Cur` = vapply(st_avg, g2080_col, numeric(1), m = sm, s = ss),
    `Coll Fut` = vapply(st_top, g2080_col, numeric(1), m = sm, s = ss),
    `MLB Cur`  = vapply(st_avg, g2080_mlb, numeric(1)),
    `MLB Fut`  = vapply(st_top, g2080_mlb, numeric(1)))
  lo <- mnn(d$LocationPlus)                                          # Control <- Location+
  ctl <- tibble(Category = "Control",
    `Coll Cur` = g2080_col(lo, lm, ls), `Coll Fut` = g2080_col(lo, lm, ls),
    `MLB Cur`  = g2080_mlb(lo), `MLB Fut` = g2080_mlb(lo))
  pavg <- mnn(d$PitchingPlus); ptop <- qn(d$PitchingPlus, .80)       # Overall <- Pitching+
  overall <- tibble(Category = "OVERALL",
    `Coll Cur` = g2080_col(pavg, pm, ps), `Coll Fut` = g2080_col(ptop, pm, ps),
    `MLB Cur`  = g2080_mlb(pavg), `MLB Fut` = g2080_mlb(ptop))
  bind_rows(rows, ctl, overall)
}

hitter_grade_detail <- function(d, minpa = 10) {
  if (sum(d$PACheck, na.rm = TRUE) < minpa) return(NULL)
  hl <- hitter_line(d)
  contact <- hl$`Contact%`; avg <- hl$AVG; ev <- hl$EV; evmax <- mxn(d$ExitSpeed[d$BBE])
  xw <- round(mnn(d$xwOBA), 3)
  mk <- function(cat, cc, cf, mc, mf) tibble(Category = cat,
    `Coll Cur` = cc, `Coll Fut` = cf, `MLB Cur` = mc, `MLB Fut` = mf)
  hit_col <- blend3(grade_step(contact, HIT_COL), grade_step(avg, AVG_COL),
                    grade_step(xw, XWOBA_COL), 0.35, 0.30, 0.35)
  hit_mlb <- blend3(grade_step(contact, HIT_MLB), grade_step(avg, AVG_MLB),
                    grade_step(xw, XWOBA_MLB), 0.35, 0.30, 0.35)
  hit <- mk("Hit", hit_col, hit_col, hit_mlb, hit_mlb)
  pwr <- mk("Power", grade_step(ev, PWR_COL), grade_step(evmax, PWR_COL),
                     grade_step(ev, PWR_MLB), grade_step(evmax, PWR_MLB))
  na3 <- list(mk("Run", NA, NA, NA, NA), mk("Field", NA, NA, NA, NA), mk("Arm", NA, NA, NA, NA))
  ovf <- function(h, p) if (is.na(h) && is.na(p)) NA_real_ else
    round(0.6 * ifelse(is.na(h), 50, h) + 0.4 * ifelse(is.na(p), 50, p))
  ov <- mk("OVERALL",
    ovf(hit$`Coll Cur`, pwr$`Coll Cur`), ovf(hit$`Coll Fut`, pwr$`Coll Fut`),
    ovf(hit$`MLB Cur`,  pwr$`MLB Cur`),  ovf(hit$`MLB Fut`,  pwr$`MLB Fut`))
  bind_rows(hit, pwr, na3[[1]], na3[[2]], na3[[3]], ov)
}

grade_board <- function(d, type, minp, pop = NULL) {
  res <- if (type == "P") {
    d %>% filter(!is.na(Pitcher)) %>% group_by(Pitcher) %>% group_split() %>%
      map_dfr(function(g) { if (nrow(g) < minp) return(NULL)
        det <- pitcher_grade_detail(g, pop = pop); if (is.null(det)) return(NULL)
        o <- det %>% filter(Category == "OVERALL")
        tibble(Player = g$Pitcher[1], Hand = g$PThrows[1],
               `Coll Cur` = o$`Coll Cur`, `Coll Fut` = o$`Coll Fut`,
               `MLB Cur` = o$`MLB Cur`, `MLB Fut` = o$`MLB Fut`) })
  } else {
    d %>% filter(!is.na(Batter)) %>% group_by(Batter) %>% group_split() %>%
      map_dfr(function(g) { det <- hitter_grade_detail(g); if (is.null(det)) return(NULL)
        o <- det %>% filter(Category == "OVERALL")
        tibble(Player = g$Batter[1], Side = g$BatSide[1],
               `Coll Cur` = o$`Coll Cur`, `Coll Fut` = o$`Coll Fut`,
               `MLB Cur` = o$`MLB Cur`, `MLB Fut` = o$`MLB Fut`) })
  }
  if (nrow(res) > 0 && "Coll Cur" %in% names(res)) res <- arrange(res, desc(`Coll Cur`))
  res
}
GCOLS <- c("Coll Cur","Coll Fut","MLB Cur","MLB Fut")

grade_color <- function(v) {
  if (is.na(v)) return("#c2cbd6")
  if (v >= 70) "#1a7a3a" else if (v >= 60) "#3f9e4f" else if (v >= 55) "#5fae57"
  else if (v >= 50) "#3a7ebf" else if (v >= 45) "#c98a3d" else if (v >= 40) "#c9553d" else "#a82a2a"
}

grade_react <- function(df, gcols) {
  if (is.null(df) || nrow(df) == 0)
    return(reactable(data.frame(Note = "Not enough data to grade this player.")))
  defs <- lapply(names(df), function(nm) {
    if (nm %in% gcols)
      colDef(align = "center", minWidth = 95,
             style = function(value) list(background = grade_color(value), color = "#ffffff",
               fontWeight = "700", textAlign = "center", borderRadius = "4px"))
    else colDef(align = "left", minWidth = 130, style = list(fontWeight = "600")) })
  names(defs) <- names(df)
  reactable(df, columns = defs, pagination = FALSE, compact = TRUE, borderless = TRUE,
            defaultPageSize = 9999,
            theme = reactableTheme(backgroundColor = "#ffffff", color = "#10233a",
              borderColor = "#dbe3ec", cellStyle = list(fontSize = "15px", padding = "8px 10px"),
              headerStyle = list(background = "#0a2342", color = "#ffffff", fontWeight = "bold",
                borderBottom = "3px solid #1aa3b5", textTransform = "uppercase", fontSize = "12px")))
}

# ============================================================================
#  UI
# ============================================================================
# Theme: navy + seafoam green on white, to match the Brewster Whitecaps.
theme_bw <- bs_theme(version = 5, bg = "#ffffff", fg = "#10233a",
                     primary = "#0a2342", secondary = "#1aa3b5",
                     "navbar-bg" = "#0a2342",
                     base_font = font_google("Inter"))

# Banner = the navy navbar. Drop the official team logo at  www/logo.png  next to
# this script and it shows automatically; if it's missing, the text still appears.
# ============================================================================
#  xwOBA / xwOBAcon TABLES  (live, computed from the uploaded CSV via the models)
# ============================================================================
pitcher_xw_table <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(data.frame(Note = "No data loaded / no pitcher selected."))
  out <- d %>% filter(!is.na(PitchType)) %>% group_by(Pitch = PitchType) %>%
    summarise(`#`      = dplyr::n(),
              BBE      = sum(BBE, na.rm = TRUE),
              xwOBAcon = round(mnn(xwOBA), 3),
              xwOBA    = round(mnn(paWOBA), 3),
              .groups  = "drop") %>%
    arrange(desc(`#`))
  if (nrow(out) == 0) return(data.frame(Note = "No pitches."))
  out
}
hitter_xw_table <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(data.frame(Note = "No data loaded / no hitter selected."))
  mk <- function(sub, lab) {
    if (nrow(sub) == 0) return(NULL)
    data.frame(Split = lab, `#` = nrow(sub), BBE = sum(sub$BBE, na.rm = TRUE),
               xwOBAcon = round(mnn(sub$xwOBA), 3),
               xwOBA    = round(mnn(sub$paWOBA), 3), check.names = FALSE)
  }
  out <- dplyr::bind_rows(
    mk(d, "All"),
    mk(d[d$PThrows %in% "L", , drop = FALSE], "vs LHP"),
    mk(d[d$PThrows %in% "R", , drop = FALSE], "vs RHP"))
  if (is.null(out) || nrow(out) == 0) return(data.frame(Note = "No batted balls."))
  out
}

# ============================================================================
#  SCOUTING PAGES  (CAPS-styled; each opens on its own; one shared CSV upload)
# ============================================================================
scout_databar <- function() {
  tags$div(
    style = "display:grid;grid-template-columns:2fr 1fr 1fr;gap:16px;margin-bottom:8px;",
    fileInput("csv", "TrackMan CSV (live-scored)", accept = ".csv",
              buttonLabel = "Browse", placeholder = "No file selected"),
    selectInput("team", "Team", choices = NULL),
    numericInput("minp", "Min pitches (player lists)", 20, min = 1, step = 5))
}
scout_shell <- function(title, ...) {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(style = "margin-bottom:20px;",
        tags$button("\u2190 Back to Hub",
          onclick = "Shiny.setInputValue('nav_to','hub',{priority:'event'})",
          class = "btn btn-outline-secondary btn-sm")),
      tags$h2(title, style = "font-family:var(--font-head);color:var(--navy);margin-bottom:18px;"),
      scout_databar(),
      textOutput("status"),
      tags$hr(style = "border-color:#dbe3ec;"),
      ...),
    tags$div(class = "hub-footer",
             paste0("Brewster Whitecaps Analytics \u00b7 ", format(Sys.Date(), "%Y"))))
}

scout_pitching_ui <- function() scout_shell("Pitcher Scouting",
  selectInput("pitcher", "Pitcher", choices = NULL, width = "100%"),
  textOutput("pitcher_hdr"),
  card(card_header("Arsenal \u2014 velo / movement / shape"),
       card_body(reactableOutput("arsenal"))),
  layout_columns(col_widths = c(6, 6),
    card(card_header("vs LHH \u2014 grades & results by pitch"),
         card_body(reactableOutput("perf_lhh"))),
    card(card_header("vs RHH \u2014 grades & results by pitch"),
         card_body(reactableOutput("perf_rhh")))),
  layout_columns(col_widths = c(7, 5),
    card(card_header("Results vs LHH / vs RHH"), card_body(reactableOutput("psplit"))),
    card(card_header("Pitch Mix by Side"), card_body(reactableOutput("pmix")))),
  card(card_header("Get-Ahead & Put-Away"), card_body(reactableOutput("pusage"))),
  card(card_header("xwOBA / xwOBAcon \u2014 by pitch (live from CSV)"),
       card_body(reactableOutput("pitch_xw_table"))),
  layout_columns(col_widths = c(4, 4, 4),
    card(card_header("Location \u2014 where he lives"), card_body(uiOutput("pitch_zone"))),
    card(card_header("Whiff Zone \u2014 where he misses bats"), card_body(uiOutput("pitch_whiff_zone"))),
    card(card_header("Damage Zone \u2014 exit velo allowed"), card_body(uiOutput("pitch_dmg_zone")))),
  layout_columns(col_widths = c(6, 6),
    card(card_header("xwOBAcon Zone \u2014 contact quality allowed"),
         card_body(uiOutput("pitch_xwc_zone"))),
    card(card_header("xwOBA Zone \u2014 expected outcome allowed by location"),
         card_body(uiOutput("pitch_xwf_zone")))),
  card(card_header("Launch Angle \u2014 contact against him"),
       card_body(plotOutput("p_la", height = "300px"))),
  card(card_header("Release Point & Percentiles (catcher's view)"),
       card_body(plotOutput("release_plot", height = "340px"), uiOutput("release_pct"))),
  card(card_header("DEBUG \u2014 what the Stuff+ model actually sees (per pitch)"),
       card_body(reactableOutput("debug_stuff"))))

scout_hitting_ui <- function() scout_shell("Hitter Scouting",
  radioButtons("hsplit", "Pitcher hand split",
               c("vs LHP" = "LHP", "vs RHP" = "RHP", "All" = "ALL"),
               selected = "RHP", inline = TRUE),
  card(card_header("Full Lineup \u2014 one row per hitter"),
       card_body(reactableOutput("lineup"))),
  tags$hr(style = "border-color:#dbe3ec;"),
  selectInput("batter", "Drill into a hitter", choices = NULL, width = "100%"),
  card(card_header("By Pitch Group \u2014 best pitches against"),
       card_body(reactableOutput("btype"))),
  card(card_header("xwOBA / xwOBAcon \u2014 by split (live from CSV)"),
       card_body(reactableOutput("hit_xw_table"))),
  layout_columns(col_widths = c(4, 4, 4),
    card(card_header("Whiff Zone \u2014 where he swings & misses"),
         card_body(uiOutput("hit_zone"))),
    card(card_header("Damage Zone \u2014 his exit velo by location"),
         card_body(uiOutput("hit_dmg_zone"))),
    card(card_header("xwOBAcon Zone \u2014 quality of contact by location"),
         card_body(uiOutput("hit_xw_zone")))),
  layout_columns(col_widths = c(6, 6),
    card(card_header("xwOBA Zone \u2014 expected outcome by location"),
         card_body(uiOutput("hit_xwf_zone"))),
    card(card_header("Launch Angle"),
         card_body(plotOutput("h_la", height = "300px")))),
  card(card_header("Spray Chart"),
       card_body(plotOutput("h_spray", height = "320px"))))

scout_acq_ui <- function() scout_shell("Acquisitions",
  fileInput("roster_csv", "Reference roster CSV (optional)", accept = ".csv",
            buttonLabel = "Browse", placeholder = "No file selected"),
  layout_columns(col_widths = c(4, 4, 4),
    radioButtons("acq_type", "Target", c("Pitchers" = "P", "Hitters" = "H"),
                 selected = "P", inline = TRUE),
    checkboxInput("acq_available", "Only show AVAILABLE (not on a loaded roster)", FALSE),
    textInput("acq_college", "College contains", "")),
  tags$p(style = "color:#888;font-size:12px;",
         "Grade = composite percentile within the loaded data (higher = better target). ",
         "Load a reference roster to fill College + signed status."),
  card(card_header("Acquisition Board"),
       card_body(reactableOutput("acq_board"))))

scout_grades_ui <- function() scout_shell("Player Grades",
  layout_columns(col_widths = c(4, 8),
    radioButtons("grade_type", "Player type", c("Pitchers" = "P", "Hitters" = "H"),
                 selected = "P", inline = TRUE),
    selectInput("grade_player", "Player", choices = NULL, width = "100%")),
  tags$p(style = "color:#888;font-size:12px;",
         "Data-driven 20-80 grades \u2014 College and MLB scale side by side, ",
         "Current (avg) and Future (peak)."),
  card(card_header("Grade Card \u2014 College vs MLB, Current & Future"),
       card_body(reactableOutput("grade_card"))),
  card(card_header("Rankings \u2014 Overall by player"),
       card_body(reactableOutput("grade_rank"))))

# ============================================================================
#  SERVER
# ============================================================================
scout_server <- function(input, output, session) {
  raw <- reactiveVal(NULL)
  ref <- reactiveVal(NULL)
  db_path_ok <- reactiveVal(NULL)

  # --- CSV ---
  observeEvent(input$csv, {
    req(input$csv)
    df <- tryCatch(utils::read.csv(input$csv$datapath, stringsAsFactors = FALSE,
                                   check.names = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { showNotification("Could not read CSV.", type = "error"); return() }
    raw(prep_pitches(df)); showNotification(paste0("Loaded ", nrow(df), " pitches."), type = "message")
  })

  # --- DB connect / load ---
  observeEvent(input$connect, {
    p <- gsub('^["\']|["\']$', '', trimws(input$db_path))
    if (p == "" || !file.exists(p)) { showNotification("DB path not found.", type = "error"); return() }
    con <- tryCatch(dbConnect(SQLite(), p), error = function(e) NULL)
    if (is.null(con)) { showNotification("Could not open DB.", type = "error"); return() }
    on.exit(dbDisconnect(con), add = TRUE)
    tbls <- dbListTables(con)
    db_path_ok(p)
    sel <- if ("masterTableAllGames" %in% tbls) "masterTableAllGames" else tbls[1]
    updateSelectInput(session, "db_table", choices = tbls, selected = sel)
    showNotification(paste0("Connected. ", length(tbls), " tables."), type = "message")
  })
  observeEvent(input$load_db, {
    p <- db_path_ok(); req(p, input$db_table)
    showNotification(paste0("Loading '", input$db_table, "'..."), type = "message", duration = 2)
    con <- dbConnect(SQLite(), p); on.exit(dbDisconnect(con), add = TRUE)
    # Read ONLY the columns prep needs. A TrackMan table has ~150 columns;
    # this app uses ~29. Reading the rest is what made the load hang.
    need <- c("Pitcher","Batter","PitcherThrows","BatterSide","PitcherTeam","BatterTeam",
              "TaggedPitchType","AutoPitchType","PitchCall","KorBB","PlayResult",
              "RelSpeed","SpinRate","InducedVertBreak","HorzBreak","RelHeight","RelSide",
              "Extension","PlateLocSide","PlateLocHeight","ExitSpeed","Angle","Balls",
              "Strikes","Inning","PAofInning","PitchofPA","Date","GameID",
              "StuffPlus","LocationPlus","PitchingPlus","Bearing","Distance","xRV","xwOBA")
    have <- tryCatch(dbListFields(con, input$db_table), error = function(e) character())
    keep <- intersect(need, have)
    qtbl <- DBI::dbQuoteIdentifier(con, input$db_table)
    q <- if (length(keep))
      paste0("SELECT ", paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", "), " FROM ", qtbl)
    else paste0("SELECT * FROM ", qtbl)
    df <- tryCatch(dbGetQuery(con, q), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { showNotification("Table empty / unreadable.", type = "error"); return() }
    raw(prep_pitches(df))
    showNotification(paste0("Loaded ", nrow(df), " pitches (", length(keep),
                            " of ", length(have), " columns)."), type = "message")
  })

  # --- reference roster ---
  observeEvent(input$roster_csv, {
    req(input$roster_csv)
    r <- tryCatch(utils::read.csv(input$roster_csv$datapath, stringsAsFactors = FALSE,
                                  check.names = FALSE), error = function(e) NULL)
    if (is.null(r)) { showNotification("Could not read roster CSV.", type = "error"); return() }
    pick <- function(cands) { hit <- intersect(tolower(cands), tolower(names(r)))
      if (!length(hit)) return(NULL); names(r)[tolower(names(r)) == hit[1]][1] }
    nm <- pick(c("Player","Name")); if (is.null(nm)) { showNotification("Roster needs a Player/Name column.", type = "error"); return() }
    col <- pick(c("College","School")); lvl <- pick(c("Level","Division"))
    tm  <- pick(c("Team","Club")); lg <- pick(c("League"))
    ref(tibble(.key = norm_name(r[[nm]]),
               College  = if (!is.null(col)) r[[col]] else NA_character_,
               Level    = if (!is.null(lvl)) r[[lvl]] else NA_character_,
               RefTeam  = if (!is.null(tm))  r[[tm]]  else NA_character_,
               RefLeague= if (!is.null(lg))  r[[lg]]  else NA_character_) %>%
          distinct(.key, .keep_all = TRUE))
    showNotification(paste0("Roster loaded: ", nrow(r), " players."), type = "message")
  })

  output$status <- renderText({ d <- raw()
    if (is.null(d)) "No data loaded." else paste0(nrow(d), " pitches loaded.") })

  observe({ d <- raw(); req(d)
    teams <- sort(unique(stats::na.omit(c(d$PitcherTeam, d$BatterTeam))))
    updateSelectInput(session, "team", choices = c("All teams" = "__all__", setNames(teams, teams))) })

  team_filter <- function(d, col) {
    if (!is.null(input$team) && input$team != "__all__") d <- d[d[[col]] %in% input$team, , drop = FALSE]
    d }

  observe({ d <- raw(); req(d, input$minp)
    dd <- team_filter(d, "PitcherTeam")
    pl <- dd %>% filter(!is.na(Pitcher)) %>% count(Pitcher, PThrows) %>%
      filter(n >= input$minp) %>% arrange(Pitcher)
    ch <- pl$Pitcher
    names(ch) <- ifelse(is.na(pl$PThrows), pl$Pitcher, paste0(pl$Pitcher, " (", pl$PThrows, "HP)"))
    updateSelectInput(session, "pitcher", choices = ch) })

  observe({ d <- raw(); req(d, input$minp)
    dd <- team_filter(d, "BatterTeam")
    bl <- dd %>% filter(!is.na(Batter)) %>% count(Batter) %>% filter(n >= input$minp) %>% arrange(Batter)
    updateSelectInput(session, "batter", choices = setNames(bl$Batter, bl$Batter)) })

  pdata <- reactive({ d <- raw(); req(d, input$pitcher); d %>% filter(Pitcher == input$pitcher) })
  hteam <- reactive({ d <- raw(); req(d); team_filter(d, "BatterTeam") })
  bdata <- reactive({ req(input$batter); hteam() %>% filter(Batter == input$batter) })

  arm_pop <- reactive({ d <- raw(); req(d)
    d %>% filter(!is.na(Pitcher), !is.na(RelHeight), !is.na(RelSide)) %>%
      group_by(Pitcher) %>%
      summarise(rh = mnn(RelHeight), rs = mnn(RelSide), .groups = "drop") %>%
      mutate(ang = arm_angle(rh, rs), pct = pctl(ang)) })

  rel_pop <- reactive({ d <- raw(); req(d)
    d %>% filter(!is.na(Pitcher)) %>% group_by(Pitcher) %>%
      summarise(relH = mnn(RelHeight), relS = mnn(RelSide), ext = mnn(Extension),
                .groups = "drop") %>%
      mutate(pH = pctl(relH), pS = pctl(relS), pE = pctl(ext)) })

  output$pitcher_hdr <- renderText({ d <- pdata(); req(nrow(d) > 0)
    hand <- d$PThrows[!is.na(d$PThrows)][1]
    ap <- arm_pop(); row <- ap[ap$Pitcher == input$pitcher, ]
    slot <- if (nrow(row) == 1 && !is.na(row$ang))
      paste0(" | Arm slot: ", arm_slot_label(row$ang), " (~", round(row$ang),
             "\u00b0, ", round(100 * row$pct), "th pctile)") else ""
    sp <- round(mnn(d$StuffPlus)); pp <- round(mnn(d$PitchingPlus)); lp <- round(mnn(d$LocationPlus))
    plus <- if (!is.na(pp)) paste0(" | Stuff+ ", sp, " / Location+ ", lp, " / Pitching+ ", pp) else ""
    tr <- if (any(!is.na(d$xRV))) paste0(" | Run value: ", sprintf("%+.1f", sum(d$xRV, na.rm = TRUE))) else ""
    xwf <- mnn(d$paWOBA[d$PACheck])
    xw  <- if (!is.na(xwf)) paste0(" | xwOBA: ", sub("^0", "", sprintf("%.3f", xwf))) else ""
    paste0(input$pitcher, " — ", ifelse(is.na(hand), "?", hand), "HP | ",
           nrow(d), " pitches | ", sum(d$PACheck, na.rm = TRUE), " PA faced", slot, plus, tr, xw) })
  output$arsenal <- renderReactable(make_table(pitcher_arsenal(pdata()),
    pct = "Usage%", d1 = c("IVB","HB","Ext"), d3 = "xwOBAcon", int = c("#","Spin")))
  output$psplit  <- renderReactable(make_table(pitcher_split(pdata()),
    pct = c("Whiff%","Chase%","K%","BB%"), d3 = c("AVG","OPS","OBP","SLG"), int = c("PA","Stuff+","Pitch+")))
  output$pmix    <- renderReactable(make_table(pmix_wide(pdata()), pct = c("vs LHH","vs RHH")))
  output$pusage  <- renderReactable(make_table(pusage(pdata()), pct = c("Usage%","Whiff%")))
  output$pitch_zone <- renderUI({ d <- pdata(); req(nrow(d) > 0); zone_heat(d, "freq") })
  output$pitch_whiff_zone <- renderUI({ d <- pdata(); req(nrow(d) > 0); zone_heat(d, "whiff") })
  output$pitch_dmg_zone   <- renderUI({ d <- pdata(); req(nrow(d) > 0); zone_heat(d, "damage") })
  output$pitch_xwc_zone   <- renderUI({ d <- pdata(); req(nrow(d) > 0); zone_heat(d, "xwoba") })
  output$pitch_xwf_zone   <- renderUI({ d <- pdata(); req(nrow(d) > 0); zone_heat(d, "xwobafull") })
  output$p_la <- renderPlot({ d <- pdata(); req(nrow(d) > 0); draw_la(d) })
  output$debug_stuff <- renderReactable(make_table(debug_inputs(pdata()),
    int = c("N","Spin","Stuff+")))
  output$pitch_xw_table <- renderReactable(make_table(pitcher_xw_table(pdata()),
    d3 = c("xwOBAcon","xwOBA"), int = c("#","BBE")))
  output$perf_lhh <- renderReactable(make_table(pitcher_perf_side(pdata(), "L"),
    pct = c("Zone%","Whiff%","Chase%"), d3 = "xwOBAcon", d1 = "RV", int = c("#","Stuff+","Loc+","Pitch+")))
  output$perf_rhh <- renderReactable(make_table(pitcher_perf_side(pdata(), "R"),
    pct = c("Zone%","Whiff%","Chase%"), d3 = "xwOBAcon", d1 = "RV", int = c("#","Stuff+","Loc+","Pitch+")))
  output$release_plot <- renderPlot({ d <- pdata(); req(nrow(d) > 0)
    d <- d %>% filter(!is.na(RelSide), !is.na(RelHeight)); req(nrow(d) > 0)
    ptf  <- factor(d$PitchType)
    cols <- c("#0a2342","#1aa3b5","#e07b39","#5fae57","#a83b3b","#7b5ea7","#c9a13b","#3a7ebf")
    pal  <- cols[(as.integer(ptf) - 1) %% length(cols) + 1]
    op <- par(mar = c(4, 4, 1, 1), bg = "white"); on.exit(par(op))
    plot(d$RelSide, d$RelHeight, col = pal, pch = 19, cex = 0.5, las = 1,
         xlab = "Release side (ft)", ylab = "Release height (ft)",
         xlim = c(-4.5, 4.5), ylim = c(0, 7))
    abline(h = 0, col = "#d7dee7"); abline(v = 0, col = "#d7dee7")
    legend("topright", legend = levels(ptf),
           col = cols[seq_along(levels(ptf))], pch = 19, bty = "n", cex = 0.9) })
  output$release_pct <- renderUI({ d <- pdata(); req(nrow(d) > 0)
    rp <- rel_pop(); row <- rp[rp$Pitcher == input$pitcher, ]
    if (nrow(row) != 1) return(NULL)
    f <- function(lbl, v, p) paste0("<b>", lbl, ":</b> ", round(v, 1), " ft (",
                                    round(100 * p), "th pctile)")
    HTML(paste0("<div style='font-size:13px;color:#10233a;margin-top:6px;'>",
      f("Release height", row$relH, row$pH), " &nbsp;|&nbsp; ",
      f("Release side", row$relS, row$pS), " &nbsp;|&nbsp; ",
      f("Extension", row$ext, row$pE), "</div>")) })

  output$lineup <- renderReactable(make_table(lineup_table(hteam(), input$hsplit, input$minp),
    pct = PCT_HIT, d3 = c(D3_HIT, "xwOBA", "xwOBAcon"), d1 = c("EV","LA"), int = "P"))
  output$btype  <- renderReactable(make_table(btype_table(bdata(), input$hsplit),
    pct = c("Sw%","Miss%","Chase%","Contact%"), d3 = c("OPS","xwOBAcon"), d1 = "EV", int = "P"))
  output$hit_zone <- renderUI({ d <- bdata(); req(nrow(d) > 0); zone_heat(d, "whiff") })
  output$hit_dmg_zone <- renderUI({ d <- bdata(); req(nrow(d) > 0); zone_heat(d, "damage") })
  output$hit_xw_zone  <- renderUI({ d <- bdata(); req(nrow(d) > 0); zone_heat(d, "xwoba") })
  output$hit_xwf_zone <- renderUI({ d <- bdata(); req(nrow(d) > 0); zone_heat(d, "xwobafull") })
  output$hit_xw_table <- renderReactable(make_table(hitter_xw_table(bdata()),
    d3 = c("xwOBAcon","xwOBA"), int = c("#","BBE")))
  output$h_la    <- renderPlot({ d <- bdata(); req(nrow(d) > 0); draw_la(d) })
  output$h_spray <- renderPlot({ d <- bdata(); req(nrow(d) > 0); draw_spray(d) })

  # --- Acquisitions ---
  output$acq_board <- renderReactable({
    d <- raw(); req(d)
    pool <- team_filter(d, if (input$acq_type == "P") "PitcherTeam" else "BatterTeam")
    tbl <- if (input$acq_type == "P") acq_pitchers(pool, input$minp) else acq_hitters(pool, input$minp)
    tbl <- attach_status(tbl, ref())
    if (!is.null(tbl) && nrow(tbl) > 0) {
      if (isTRUE(input$acq_available)) tbl <- tbl %>% filter(Status == "AVAILABLE?")
      if (nzchar(input$acq_college) && "College" %in% names(tbl))
        tbl <- tbl %>% filter(grepl(input$acq_college, College, ignore.case = TRUE))
      front <- intersect(c("Player","College","Hand","Side","Grade"), names(tbl))
      tbl <- tbl %>% select(all_of(front), everything())
    }
    pcts <- intersect(c("Whiff%","Chase%","Contact%"), names(tbl))
    make_table(tbl, pct = pcts, d3 = intersect(c("OPS","OPS Agst"), names(tbl)),
               d1 = intersect(c("FBVelo","EV"), names(tbl)),
               int = intersect(c("Pitches","TopVelo","Grade"), names(tbl)))
  })

  # --- Player Grades ---
  observe({ d <- raw(); req(d, input$grade_type, input$minp)
    if (input$grade_type == "P") {
      dd <- team_filter(d, "PitcherTeam")
      pl <- dd %>% filter(!is.na(Pitcher)) %>% count(Pitcher, PThrows) %>%
        filter(n >= input$minp) %>% arrange(Pitcher)
      ch <- pl$Pitcher
      names(ch) <- ifelse(is.na(pl$PThrows), pl$Pitcher, paste0(pl$Pitcher, " (", pl$PThrows, "HP)"))
    } else {
      dd <- team_filter(d, "BatterTeam")
      pl <- dd %>% filter(!is.na(Batter)) %>% count(Batter) %>% filter(n >= input$minp) %>% arrange(Batter)
      ch <- setNames(pl$Batter, pl$Batter)
    }
    updateSelectInput(session, "grade_player", choices = ch)
  })

  plus_pop <- reactive({ d <- raw(); req(d)
    pp <- d %>% filter(!is.na(Pitcher)) %>% group_by(Pitcher) %>%
      summarise(st = mnn(StuffPlus), lo = mnn(LocationPlus), pi = mnn(PitchingPlus),
                .groups = "drop")
    list(st_m = mean(pp$st, na.rm = TRUE), st_s = stats::sd(pp$st, na.rm = TRUE),
         lo_m = mean(pp$lo, na.rm = TRUE), lo_s = stats::sd(pp$lo, na.rm = TRUE),
         pi_m = mean(pp$pi, na.rm = TRUE), pi_s = stats::sd(pp$pi, na.rm = TRUE)) })

  output$grade_card <- renderReactable({
    d <- raw(); req(d, input$grade_player)
    det <- if (input$grade_type == "P")
      pitcher_grade_detail(d %>% filter(Pitcher == input$grade_player), pop = plus_pop())
    else hitter_grade_detail(d %>% filter(Batter == input$grade_player))
    grade_react(det, GCOLS)
  })

  output$grade_rank <- renderReactable({
    d <- raw(); req(d)
    pool <- team_filter(d, if (input$grade_type == "P") "PitcherTeam" else "BatterTeam")
    grade_react(grade_board(pool, input$grade_type, input$minp, pop = plus_pop()), GCOLS)
  })
}

