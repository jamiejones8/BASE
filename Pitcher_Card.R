library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

pcard_format_pitcher_name <- function(nm) {
  vapply(as.character(nm), function(n) {
    if (length(n) == 0 || is.na(n) || !grepl(",", n, fixed = TRUE)) return(as.character(n))
    parts <- strsplit(n, ",\\s*")[[1]]
    if (length(parts) >= 2) paste(trimws(parts[2]), trimws(parts[1])) else as.character(n)
  }, character(1), USE.NAMES = FALSE)
}

# ── pitch type canonicalization + palette ──
pcard_canonicalize_pitch <- function(x) {
  dplyr::case_when(
    x %in% c("Fastball", "FourSeamFastBall", "FF", "FastBall") ~ "Four-Seam",
    x %in% c("TwoSeamFastBall", "OneSeamFastBall", "Sinker", "SI") ~ "Sinker",
    x %in% c("ChangeUp", "CH", "Changeup") ~ "Changeup",
    x %in% c("KnuckleCurve", "KC") ~ "Curveball",
    x %in% c("CutFastBall", "FC") ~ "Cutter",
    x %in% c("SL") ~ "Slider",
    x %in% c("CU") ~ "Curveball",
    x %in% c("FS") ~ "Splitter",
    TRUE ~ x
  )
}

pcard_pitch_colors <- c(
  "Four-Seam" = "#D94A3F", "Two-Seam Fastball" = "#B83A95", "Sinker" = "#D17A2A",
  "Cutter" = "#C9A82E", "Splitter" = "#7FD9A2", "Changeup" = "#3DA84B",
  "Slider" = "#2C7AB8", "Sweeper" = "#B566B5", "Curveball" = "#8E2EA0",
  "Knuckle Curve" = "#7A2A52", "Slurve" = "#3F8A66", "Knuckle Ball" = "#3FA3A3",
  "Eephus" = "#9A9AA0", "Fastball" = "#5FA8C9", "Slow Curve" = "#B8B8BE",
  "Screwball" = "#C96A95"
)

# ── stat formatting helpers ──
pcard_format_ip <- function(outs) {
  outs <- suppressWarnings(as.integer(round(outs)))
  if (length(outs) == 0 || is.na(outs)) return("--")
  paste0(outs %/% 3, ".", outs %% 3)
}
pcard_format_num <- function(x, digits = 1) {
  ifelse(is.na(x) | !is.finite(x), "--", formatC(x, format = "f", digits = digits))
}
pcard_format_rate <- function(x, digits = 3) {
  ifelse(is.na(x) | !is.finite(x), "--",
         sub("^0", "", formatC(x, format = "f", digits = digits)))
}

pcard_pitcher_boxscore <- function(df, pitcher_raw) {
  d <- df %>% filter(Pitcher == pitcher_raw)
  d <- d %>%
    mutate(
      IsHit      = PlayResult %in% c("Single","Double","Triple","HomeRun"),
      IsWalk     = KorBB == "Walk",
      IsK        = KorBB == "Strikeout",
      IsHBP      = PitchCall == "HitByPitch",
      IsHR       = PlayResult == "HomeRun",
      OutsOnPlay = suppressWarnings(as.numeric(OutsOnPlay))
    )

  outs <- sum(d$OutsOnPlay, na.rm = TRUE) + sum(d$IsK, na.rm = TRUE)
  ip_dec <- outs / 3
  hits <- sum(d$IsHit, na.rm = TRUE)
  bb   <- sum(d$IsWalk, na.rm = TRUE)
  k    <- sum(d$IsK, na.rm = TRUE)
  hbp  <- sum(d$IsHBP, na.rm = TRUE)
  hr   <- sum(d$IsHR, na.rm = TRUE)

  whip <- if (ip_dec > 0) (hits + bb) / ip_dec else NA_real_
  fip  <- if (ip_dec > 0) ((13*hr + 3*(bb+hbp) - 2*k) / ip_dec) + 3.87 else NA_real_

  list(
    IP = pcard_format_ip(outs), H = as.character(hits), BB = as.character(bb),
    K = as.character(k), HR = as.character(hr),
    WHIP = pcard_format_num(whip, 2), FIP = pcard_format_num(fip, 2)
  )
}

# ── usage table data ──
pcard_usage_table <- function(pitcher_data, side = c("All", "Left", "Right")) {
  side <- match.arg(side)
  d <- pitcher_data
  
  if (side != "All") {
    d <- d %>% filter(BatterSide == side)
  }
  
  d <- d %>% mutate(TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType))
  total_n <- nrow(d)
  
  if (total_n == 0) {
    return(tibble::tibble(Pitch = character(), `#` = character(),
                          `Usage%` = character(), Velo = character()))
  }
  
  d %>%
    filter(!is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined") %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      `#`       = as.character(n()),
      `Usage%`  = paste0(round(n() / total_n * 100, 1), "%"),
      Velo      = pcard_format_num(mean(RelSpeed, na.rm = TRUE), 1),
      .groups   = "drop"
    ) %>%
    arrange(desc(as.integer(`#`))) %>%
    rename(Pitch = TaggedPitchType_clean)
}

pcard_pitch_metrics_table <- function(pitcher_data) {
  d <- pitcher_data %>%
    mutate(TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType)) %>%
    filter(!is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined")

  d %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      `#`        = n(),
      Velo       = pcard_format_num(mean(RelSpeed, na.rm = TRUE), 1),
      `Max Velo` = pcard_format_num(suppressWarnings(max(RelSpeed, na.rm = TRUE)), 1),
      Spin       = pcard_format_num(mean(SpinRate, na.rm = TRUE), 0),
      iVB        = pcard_format_num(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB         = pcard_format_num(mean(HorzBreak, na.rm = TRUE), 1),
      RelH       = pcard_format_num(mean(RelHeight, na.rm = TRUE), 1),
      RelS       = pcard_format_num(mean(RelSide, na.rm = TRUE), 1),
      Ext        = pcard_format_num(mean(Extension, na.rm = TRUE), 1),
      VAA        = pcard_format_num(mean(VertApprAngle, na.rm = TRUE), 1),
      HAA        = pcard_format_num(mean(HorzApprAngle, na.rm = TRUE), 1),
      .groups    = "drop"
    ) %>%
    arrange(desc(`#`)) %>%
    rename(Pitch = TaggedPitchType_clean)
}

pcard_pitch_hit_metrics <- function(pitcher_data) {
  d <- pitcher_data %>%
    mutate(TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType)) %>%
    filter(!is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined")

  d <- d %>%
    mutate(
      IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable",
                                   "FoulBallFieldable","FoulTip","InPlay"),
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsBBE     = PitchCall == "InPlay",
      IsHardHit = IsBBE & !is.na(ExitSpeed) & ExitSpeed >= 95,
      IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulBallFieldable","FoulTip","InPlay"),
      InZone    = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
        abs(PlateLocSide) <= 0.83 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5
    )

  swing_summary <- d %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      N           = n(),
      Whiff_pct   = ifelse(sum(IsSwing) > 0, sum(IsWhiff) / sum(IsSwing), NA_real_),
      BBE         = sum(IsBBE),
      HardHit_pct = ifelse(BBE > 0, sum(IsHardHit) / BBE, NA_real_),
      AvgEV       = ifelse(BBE > 0, mean(ExitSpeed[IsBBE], na.rm = TRUE), NA_real_),
      AvgLA       = ifelse(BBE > 0, mean(Angle[IsBBE], na.rm = TRUE), NA_real_),
      Zone_pct    = mean(InZone, na.rm = TRUE),
      Strike_pct  = mean(IsStrike, na.rm = TRUE),
      .groups     = "drop"
    )

  pa_end <- d %>%
    filter(
      PlayResult %in% c("Single","Double","Triple","HomeRun","Out",
                        "FieldersChoice","Error","Sacrifice", "Sacrificie") |
        KorBB %in% c("Strikeout","Walk") |
        PitchCall == "HitByPitch"
    ) %>%
    mutate(
      IsHit  = PlayResult %in% c("Single","Double","Triple","HomeRun"),
      TB     = case_when(
        PlayResult == "Single"   ~ 1,
        PlayResult == "Double"   ~ 2,
        PlayResult == "Triple"   ~ 3,
        PlayResult == "HomeRun"  ~ 4,
        TRUE ~ 0
      ),
      IsBB   = KorBB == "Walk",
      IsHBP  = PitchCall == "HitByPitch",
      IsSac  = PlayResult == "Sacrifice",
      IsAB   = !IsBB & !IsHBP & !IsSac
    )

  outcome_summary <- pa_end %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      PA          = n(),
      AB          = sum(IsAB),
      Hits        = sum(IsHit),
      Walks       = sum(IsBB),
      HBP         = sum(IsHBP),
      TotalBases  = sum(TB),
      AVG         = ifelse(AB > 0, Hits / AB, NA_real_),
      OBP         = ifelse((AB+Walks+HBP) > 0, (Hits+Walks+HBP) / (AB+Walks+HBP), NA_real_),
      SLG         = ifelse(AB > 0, TotalBases / AB, NA_real_),
      OPS         = ifelse(is.na(OBP) | is.na(SLG), NA_real_, OBP + SLG),
      .groups     = "drop"
    )

  full_join(swing_summary, outcome_summary, by = "TaggedPitchType_clean") %>%
    mutate(
      `Whiff%`   = ifelse(is.na(Whiff_pct), "--", paste0(round(Whiff_pct * 100, 1), "%")),
      `HardHit%` = ifelse(is.na(HardHit_pct), "--", paste0(round(HardHit_pct * 100, 1), "%")),
      `Zone%`    = ifelse(is.na(Zone_pct), "--", paste0(round(Zone_pct * 100, 1), "%")),
      `Strike%`  = ifelse(is.na(Strike_pct), "--", paste0(round(Strike_pct * 100, 1), "%")),
      `Avg EV`   = pcard_format_num(AvgEV, 1),
      `Avg LA`   = pcard_format_num(AvgLA, 1),
      AVG        = pcard_format_rate(AVG, 3),
      SLG        = pcard_format_rate(SLG, 3),
      OPS        = pcard_format_rate(OPS, 3),
      BBE        = ifelse(is.na(BBE), 0L, BBE)
    ) %>%
    select(Pitch = TaggedPitchType_clean, `#` = N, BBE, AVG, SLG, OPS,
           `Whiff%`, `HardHit%`, `Zone%`, `Strike%`, `Avg EV`, `Avg LA`) %>%
    arrange(desc(`#`))
}


pcard_count_bucket <- function(balls, strikes) {
  count <- paste0(balls, "-", strikes)
  case_when(
    count == "3-2"                          ~ "3-2",
    strikes == 2                            ~ "Kill/Putaway",
    count == "0-0"                          ~ "Early",
    count %in% c("0-1","0-2","1-2")         ~ "Ahead",
    count %in% c("1-1","2-2")               ~ "Even",
    TRUE                                    ~ "Other"
  )
}

pcard_usage_by_count <- function(pitcher_data) {
  d <- pitcher_data %>%
    mutate(
      TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType),
      Bucket = pcard_count_bucket(Balls, Strikes)
    ) %>%
    filter(!is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined",
           Bucket != "Other")

  bucket_totals <- d %>% count(Bucket, name = "bucket_n")

  d %>%
    count(Bucket, TaggedPitchType_clean, name = "n") %>%
    left_join(bucket_totals, by = "Bucket") %>%
    mutate(pct = round(n / bucket_n * 100, 1)) %>%
    select(Bucket, Pitch = TaggedPitchType_clean, `#` = n, `Usage%` = pct) %>%
    mutate(`Usage%` = paste0(`Usage%`, "%")) %>%
    arrange(factor(Bucket, levels = c("Early","Ahead","Even","3-2","Kill/Putaway")), desc(`#`))
}

pcard_strike_zone_box <- data.frame(
  x = c(-0.83, -0.83, 0.83, 0.83, -0.83),
  y = c(1.5, 3.5, 3.5, 1.5, 1.5)
)

pcard_density_heatmap <- function(pitcher_data, pitch_type = "All", side = "All") {
  d <- pitcher_data %>%
    mutate(TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType)) %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))

  if (pitch_type != "All") d <- d %>% filter(TaggedPitchType_clean == pitch_type)
  if (side != "All")       d <- d %>% filter(BatterSide == side)

  title_txt <- paste0(
    if (pitch_type == "All") "All Pitches" else pitch_type,
    " vs ",
    if (side == "All") "All Hitters" else paste0(toupper(substr(side,1,1)), "HH")
  )

  if (nrow(d) < 5) {
    return(ggplot() + theme_void() + labs(title = paste0(title_txt, " — insufficient data")))
  }

  ggplot(d, aes(x = -PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster",
                    contour = FALSE, interpolate = TRUE) +
    scale_fill_gradient(low = "white", high = "#C8102E", guide = "none") +
    geom_path(data = pcard_strike_zone_box, aes(x = x, y = y),
              color = "#0C2340", linewidth = 1, inherit.aes = FALSE) +
    geom_polygon(data = pcard_home_plate_shape, aes(x = x, y = y),
                fill = "white", color = "#0C2340", linewidth = 0.8, inherit.aes = FALSE) +
    coord_fixed() + xlim(-2.5, 2.5) + ylim(0, 5) +
    labs(title = title_txt, x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold", colour = "#0C2340"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}


# ── pitch location data + plot ──
pcard_home_plate_shape <- data.frame(
  x = c(0.6, -0.6, -0.7083, 0, 0.7083),
  y = c(0.5, 0.5, 0.25, 0, 0.25)
)

pcard_location_plot <- function(pitcher_data, side = c("Left", "Right")) {
  side <- match.arg(side)
  side_label <- if (side == "Left") "LHH" else "RHH"
  
  d <- pitcher_data %>%
    filter(BatterSide == side) %>%
    mutate(
      TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType),
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsBBE     = PitchCall == "InPlay",
      IsHardHit = IsBBE & !is.na(ExitSpeed) & ExitSpeed >= 95,
      outcome   = factor(
        case_when(
          IsHardHit ~ "Hard Hit",
          IsWhiff   ~ "Whiff",
          TRUE      ~ "Other"
        ),
        levels = c("Other", "Whiff", "Hard Hit")
      )
    ) %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
           !is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined")
  
  if (nrow(d) == 0) {
    return(
      ggplot() + theme_void() +
        labs(title = paste("No pitches vs", side_label))
    )
  }
  
  ggplot(d, aes(x = -PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType_clean)) +
    geom_point(data = ~ dplyr::filter(.x, outcome == "Other"),
               aes(shape = outcome, size = outcome, stroke = outcome),
               colour = "black", alpha = 0.85) +
    geom_polygon(data = pcard_home_plate_shape, aes(x = x, y = y),
                 fill = NA, colour = "#5F5F6B", linewidth = 0.7, inherit.aes = FALSE) +
    annotate("rect", xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.6,
             fill = NA, colour = "#36363F", linewidth = 0.8) +
    geom_point(data = ~ dplyr::filter(.x, outcome %in% c("Whiff", "Hard Hit")),
               aes(shape = outcome, size = outcome, stroke = outcome),
               colour = "black", alpha = 0.9) +
    scale_fill_manual(values = pcard_pitch_colors, na.value = "#888888") +
    scale_shape_manual(values = c("Other" = 21, "Whiff" = 23, "Hard Hit" = 22),
                       limits = c("Other", "Whiff", "Hard Hit"),
                       breaks = c("Hard Hit", "Whiff"), name = NULL, drop = FALSE) +
    scale_size_manual(values = c("Other" = 2.6, "Whiff" = 4.6, "Hard Hit" = 4.6),
                      limits = c("Other", "Whiff", "Hard Hit"), guide = "none", drop = FALSE) +
    scale_discrete_manual(aesthetics = "stroke",
                          values = c("Other" = 0.4, "Whiff" = 0.8, "Hard Hit" = 0.8),
                          limits = c("Other", "Whiff", "Hard Hit"), guide = "none", drop = FALSE) +
    guides(fill = "none") +
    coord_fixed() +
    xlim(-2.5, 2.5) + ylim(0, 5) +
    labs(title = paste("Location vs", side_label), subtitle = "Catcher's View",
         x = NULL, y = NULL) +
    theme_void(base_family = "sans") +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold", colour = "#0C2340"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, colour = "#8B8B96", face = "italic")
    )
}

# ── movement plot ──
pcard_movement_plot <- function(pitcher_data) {
  pitcher_data <- pitcher_data %>%
    mutate(TaggedPitchType_clean = pcard_canonicalize_pitch(TaggedPitchType)) %>%
    filter(!is.na(TaggedPitchType_clean), TaggedPitchType_clean != "Undefined")
  
  movement_avg <- pitcher_data %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      HB   = round(mean(HorzBreak, na.rm = TRUE), 1),
      iVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      Velo = round(mean(RelSpeed, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  ggplot() +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(data = pitcher_data,
               aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType_clean),
               size = 4, alpha = 0.8, shape = 21, color = "black", stroke = 0.5) +
    geom_point(data = movement_avg,
               aes(x = HB, y = iVB, color = TaggedPitchType_clean),
               size = 10, alpha = 0.9) +
    geom_text(data = movement_avg, aes(x = HB, y = iVB, label = Velo),
              color = "white", size = 3.5, fontface = "bold") +
    scale_color_manual(values = pcard_pitch_colors, na.value = "#888888") +
    scale_fill_manual(values = pcard_pitch_colors, na.value = "#888888") +
    labs(title = "Pitch Movement", x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    scale_x_continuous(limits = c(-25, 25)) +
    scale_y_continuous(limits = c(-25, 25)) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
}

# ── header + boxscore + table draw functions ──
pcard_draw_header <- function(name) {
  grid.rect(gp = gpar(fill = "#0C2340", col = NA))
  grid.text(name, x = 0.05, y = 0.5, just = "left",
            gp = gpar(fontsize = 32, fontface = "bold", fontfamily = "Impact", col = "white"))
}

pcard_draw_boxscore <- function(stats) {
  cols <- names(stats); vals <- unlist(stats); n <- length(cols); cell_w <- 1 / n
  grid.rect(gp = gpar(fill = "#FAFAFB", col = "#EAEAEE"))
  for (i in seq_len(n)) {
    cx <- (i - 0.5) * cell_w
    if (i > 1) {
      grid.lines(x = unit(c((i-1)*cell_w, (i-1)*cell_w), "npc"), y = unit(c(0.15, 0.85), "npc"),
                 gp = gpar(col = "#EAEAEE", lwd = 1))
    }
    grid.text(toupper(cols[i]), x = unit(cx, "npc"), y = unit(0.70, "npc"),
              gp = gpar(col = "#5F5F6B", fontsize = 11, fontfamily = "sans"))
    grid.text(vals[i], x = unit(cx, "npc"), y = unit(0.32, "npc"),
              gp = gpar(col = "#16161B", fontsize = 24, fontface = "bold", fontfamily = "sans"))
  }
}

pcard_draw_table <- function(tbl, title) {
  if (nrow(tbl) == 0) {
    grid.text(paste0(title, ": no pitches"),
              gp = gpar(fontsize = 11, col = "#5F5F6B", fontfamily = "sans"))
    return(invisible(NULL))
  }

  n_rows <- nrow(tbl)
  n_cols <- ncol(tbl)

  fg_mat <- matrix("#16161B", n_rows, n_cols)
  bg_mat <- matrix("#FAFAFB", n_rows, n_cols)
  for (i in seq_len(n_rows)) {
    pitch_name <- tbl$Pitch[i]
    if (pitch_name %in% names(pcard_pitch_colors)) {
      fg_mat[i, 1] <- unname(pcard_pitch_colors[pitch_name])
    }
  }

  tbl_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = bg_mat, col = "#EAEAEE"),
      fg_params = list(col = fg_mat, fontface = "bold", fontsize = 15, fontfamily = "sans")   # bumped from 11
    ),
    colhead = list(
      bg_params = list(fill = "#00827F", col = "#EAEAEE"),
      fg_params = list(col = "white", fontface = "bold", fontsize = 13, fontfamily = "sans")  # bumped from 10
    )
  )

  tg <- tableGrob(tbl, rows = NULL, theme = tbl_theme)

  # Make the table fill whatever canvas it's drawn on, instead of drawing at
  # its natural (small) size — same trick as build_arsenal_grob() in app.R.
  tg$widths  <- unit(rep(1, ncol(tg)), "null")
  tg$heights <- unit(rep(1, nrow(tg)), "null")

  title_grob <- textGrob(title, gp = gpar(fontsize = 14, fontface = "bold",     # bumped from 12
                                          col = "#0C2340", fontfamily = "sans"))

  tg <- gtable_add_rows(tg, heights = grobHeight(title_grob) + unit(10, "pt"), pos = 0)
  tg <- gtable_add_grob(tg, title_grob, t = 1, l = 1, r = ncol(tg), name = "title")

  # Draw the whole thing (title + table) filling the current viewport.
  pushViewport(viewport(width = unit(1, "npc") - unit(4, "pt"),
                        height = unit(1, "npc") - unit(4, "pt")))
  grid.draw(tg)
  popViewport()
}

# ── standalone page wrappers — each takes explicit args, no globals ──
pcard_draw_header_page <- function(pitcher_raw) {
  grid.newpage()
  pcard_draw_header(pcard_format_pitcher_name(pitcher_raw))
}

pcard_draw_boxscore_page <- function(box_stats) {
  grid.newpage()
  pcard_draw_boxscore(box_stats)
}

pcard_draw_movement_page <- function(p_movement) {
  grid.newpage()
  print(p_movement, newpage = FALSE)
}

pcard_draw_pitch_metrics_page <- function(pitch_metrics_tbl) {
  grid.newpage()
  pcard_draw_table(pitch_metrics_tbl, "Pitch Metrics")
}

pcard_draw_count_usage_page <- function(count_usage_tbl) {
  grid.newpage()
  pcard_draw_table(count_usage_tbl, "Usage by Count Situation")
}

pcard_draw_location_page <- function(p_location_lhh, p_location_rhh) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  
  pushViewport(viewport(layout.pos.col = 1))
  print(p_location_lhh, newpage = FALSE)
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 2))
  print(p_location_rhh, newpage = FALSE)
  popViewport()
  
  popViewport()
}

# ── single-panel page wrappers (finer-grained than the combined ones) ──
pcard_draw_location_lhh_page <- function(p_location_lhh) {
  grid.newpage()
  print(p_location_lhh, newpage = FALSE)
}

pcard_draw_location_rhh_page <- function(p_location_rhh) {
  grid.newpage()
  print(p_location_rhh, newpage = FALSE)
}

pcard_draw_single_table_page <- function(tbl, title) {
  grid.newpage()
  pcard_draw_table(tbl, title)
}

pcard_draw_usage_page <- function(usage_total, usage_rhh, usage_lhh) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 3)))
  
  pushViewport(viewport(layout.pos.col = 1))
  pcard_draw_table(usage_total, "Overall")
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 2))
  pcard_draw_table(usage_rhh, "RHH")
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 3))
  pcard_draw_table(usage_lhh, "LHH")
  popViewport()
  
  popViewport()
}

pcard_draw_hit_metrics_page <- function(hit_metrics_tbl) {
  grid.newpage()
  pcard_draw_table(hit_metrics_tbl, "Hit Metrics by Pitch")
}

# ── convenience: build every data object for one pitcher in one call ──
pcard_build_all <- function(df, pitcher_raw) {
  pitcher_data <- df %>% filter(Pitcher == pitcher_raw)

  pitch_types <- sort(unique(pcard_canonicalize_pitch(pitcher_data$TaggedPitchType)))
  pitch_types <- pitch_types[!pitch_types %in% c("Undefined", NA)]

  list(
    pitcher_raw       = pitcher_raw,
    pitcher_data      = pitcher_data,
    box_stats         = pcard_pitcher_boxscore(df, pitcher_raw),
    p_movement        = pcard_movement_plot(pitcher_data),
    pitch_metrics_tbl = pcard_pitch_metrics_table(pitcher_data),
    p_location_lhh    = pcard_location_plot(pitcher_data, "Left"),
    p_location_rhh    = pcard_location_plot(pitcher_data, "Right"),
    usage_total       = pcard_usage_table(pitcher_data, "All"),
    usage_rhh         = pcard_usage_table(pitcher_data, "Right"),
    usage_lhh         = pcard_usage_table(pitcher_data, "Left"),
    hit_metrics_tbl   = pcard_pitch_hit_metrics(pitcher_data),
    count_usage_tbl   = pcard_usage_by_count(pitcher_data),   
    pitch_types       = pitch_types                            
  )
}