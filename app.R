library(shiny)
library(htmltools)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(grid)
library(magick)
library(readr)
library(tidymodels)

# ==========================================
# STANDINGS
# ==========================================
fetch_standings <- function() {
  url  <- "https://baseball.pointstreak.com/standings.html?leagueid=166&seasonid=33239"
  page <- read_html(url)
  tables <- xml_find_all(page, ".//table")
  lapply(tables, function(t) {
    rows <- xml_find_all(t, ".//tr")
    data <- lapply(rows, function(r) {
      xml_text(xml_find_all(r, ".//td|.//th"))
    })
    data <- data[sapply(data, length) > 0]
    df <- as.data.frame(do.call(rbind, data[-1]), stringsAsFactors = FALSE)
    names(df) <- data[[1]]
    df
  })
}

standings <- tryCatch(fetch_standings(), error = function(e) NULL)

# ==========================================
# CATCHER — HELPER FUNCTIONS
# ==========================================
format_name <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) paste(trimws(parts[2]), trimws(parts[1])) else name
}

draw_grid_table <- function(df,
                            title        = NULL,
                            y_top        = 0.95,
                            x_center     = 0.5,
                            row_h        = 0.028,
                            table_width  = 0.85,
                            header_bg    = "#0C2340",
                            zebra_bg     = "#f0f4f8",
                            title_cex    = 0.75,
                            header_cex   = 0.60,
                            cell_cex     = 0.58,
                            color_matrix = NULL) {
  df[] <- lapply(df, function(x) ifelse(is.na(x), "-", as.character(x)))
  headers    <- names(df)
  n_cols     <- length(headers)
  col_widths <- rep(table_width / n_cols, n_cols)
  x_start    <- x_center - sum(col_widths) / 2
  x_pos      <- c(x_start, x_start + cumsum(col_widths[-n_cols]))
  y_cursor   <- y_top

  if (!is.null(title)) {
    grid.text(title, x = x_center, y = y_cursor,
              gp = gpar(fontface = "bold", cex = title_cex, col = "#0C2340"))
    y_cursor <- y_cursor - row_h * 0.9
  }

  for (i in seq_along(headers)) {
    grid.rect(x = x_pos[i], y = y_cursor,
              width = col_widths[i] * 0.98, height = row_h,
              just = c("left","top"),
              gp = gpar(fill = header_bg, col = "black", lwd = 0.5))
    grid.text(headers[i],
              x = x_pos[i] + col_widths[i] * 0.49,
              y = y_cursor - row_h * 0.5,
              gp = gpar(col = "white", cex = header_cex, fontface = "bold"))
  }
  y_cursor <- y_cursor - row_h

  for (r in seq_len(nrow(df))) {
    for (i in seq_along(headers)) {
      bg <- if (!is.null(color_matrix)) color_matrix[r, i] else if (r %% 2 == 0) zebra_bg else "white"
      grid.rect(x = x_pos[i], y = y_cursor,
                width = col_widths[i] * 0.98, height = row_h,
                just = c("left","top"),
                gp = gpar(fill = bg, col = "grey80", lwd = 0.3))
      grid.text(as.character(df[[i]][r]),
                x = x_pos[i] + col_widths[i] * 0.49,
                y = y_cursor - row_h * 0.5,
                gp = gpar(cex = cell_cex, fontface = "bold"))
    }
    y_cursor <- y_cursor - row_h
  }
  invisible(y_cursor)
}

# ==========================================
# CATCHER — DATA PREP
# ==========================================
prep_catcher_data <- function(raw, team) {
  raw <- raw %>%
    filter(CatcherTeam == team) %>%
    mutate(
      TaggedPitchType = case_when(
        TaggedPitchType %in% c("Fastball","FourSeamFastBall","Four-Seam","OneSeamFastball",
                                "FourSeamFastball","Sinker","TwoSeamFastball","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","CurveBall","Slider","Sweeper","Slurve")     ~ "Breaking Ball",
        TaggedPitchType %in% c("ChangeUp","Changeup","Splitter")                        ~ "Offspeed",
        TRUE ~ TaggedPitchType
      )
    )

  dfFraming <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, BatterSide,
           PitchCall, PlateLocHeight, PlateLocSide, Balls, Strikes, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PlateLocHeight), !is.na(PlateLocSide),
           PitchCall %in% c("BallCalled","StrikeCalled"))

  dfThrowing <- raw %>%
    select(Date, CatcherTeam, Catcher, CatcherId, TaggedPitchType, KorBB, PitchCall,
           Strikes, OutsOnPlay, PopTime, ExchangeTime, TimeToBase, ThrowSpeed,
           BasePositionX, BasePositionY, BasePositionZ, Inning, Outs, Pitcher, Batter) %>%
    filter(!is.na(PopTime), PitchCall %in% c("BallCalled","StrikeCalled","StrikeSwinging"))

  list(framing = dfFraming, throwing = dfThrowing)
}

# ==========================================
# CATCHER — STAT FUNCTIONS
# ==========================================
overallStealInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    summarise(
      `Caught Stealing` = sum(OutsOnPlay == 1, na.rm = TRUE),
      `Stolen Bases`    = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`             = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`        = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time`   = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`        = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`     = round(mean(ThrowSpeed,   na.rm = TRUE), 1)
    )
}

overallStealByPitch <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    group_by(`Pitch Type` = TaggedPitchType) %>%
    summarise(
      `CS`            = sum(OutsOnPlay == 1, na.rm = TRUE),
      `SB`            = sum(OutsOnPlay == 0, na.rm = TRUE),
      `CS%`           = paste0(round(sum(OutsOnPlay == 1, na.rm = TRUE) / n() * 100, 1), "%"),
      `Pop Time`      = round(mean(PopTime,      na.rm = TRUE), 2),
      `Exchange Time` = round(mean(ExchangeTime, na.rm = TRUE), 2),
      `Air Time`      = round(mean(TimeToBase,   na.rm = TRUE), 2),
      `Throw Speed`   = round(mean(ThrowSpeed,   na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(CS))
}

overallFramingInfo <- function(catcher, df) {
  df %>% filter(Catcher == catcher) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0)
    ) %>%
    summarise(
      `Strikes Won`  = sum(PhysicalZone == 0 & PitchCall == "StrikeCalled", na.rm = TRUE),
      `Strikes Lost` = sum(PhysicalZone == 1 & PitchCall == "BallCalled",   na.rm = TRUE),
      `Ratio`        = round(`Strikes Won` / `Strikes Lost`, 2)
    )
}

gameStealInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date) %>%
    mutate(
      `Caught Stealing` = if_else(OutsOnPlay == 1, "Yes", "No"),
      `Pop Time`        = round(PopTime,      2),
      `Exchange Time`   = round(ExchangeTime, 2),
      `Air Time`        = round(TimeToBase,   2),
      `Throw Speed`     = round(ThrowSpeed,   1)
    ) %>%
    rename(Pitch = TaggedPitchType) %>%
    select(Inning, Batter, Pitch, `Caught Stealing`, `Pop Time`, `Exchange Time`, `Air Time`, `Throw Speed`)
}

gameFramingInfo <- function(catcher, df, game_date) {
  df %>%
    filter(Catcher == catcher, as.Date(Date) == game_date,
           PitchCall %in% c("StrikeCalled","BallCalled")) %>%
    mutate(
      PhysicalZone = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      Count          = paste0(Balls, "-", Strikes),
      `Plate Height` = round(PlateLocHeight, 2),
      `Plate Side`   = round(PlateLocSide,   2)
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    rename(Pitch = TaggedPitchType) %>%
    mutate(`#` = row_number()) %>%
    select(`#`, Inning, Batter, Pitch, `Strike Outcome`, Count, `Plate Height`, `Plate Side`)
}

framingPlotData <- function(catcher, df, game_date = NULL) {
  out <- df %>% filter(Catcher == catcher, PitchCall %in% c("StrikeCalled","BallCalled"))
  if (!is.null(game_date)) out <- out %>% filter(as.Date(Date) == game_date)
  out <- out %>%
    mutate(
      PhysicalZone     = if_else(
        between(PlateLocHeight, 1.5, 3.3775) & between(PlateLocSide, -0.83083, 0.83083), 1, 0),
      `Strike Outcome` = case_when(
        PhysicalZone == 0 & PitchCall == "StrikeCalled" ~ "Won",
        PhysicalZone == 1 & PitchCall == "BallCalled"   ~ "Lost"
      ),
      `Plate Height` = PlateLocHeight,
      `Plate Side`   = PlateLocSide
    ) %>%
    filter(`Strike Outcome` %in% c("Won","Lost")) %>%
    select(-Catcher)

  if (!is.null(game_date)) out <- out %>% mutate(`#` = row_number())
  out
}

plot_framing <- function(plot_df, outcome_filter, plot_title) {
  df_filtered <- plot_df %>% filter(`Strike Outcome` == outcome_filter)
  pt_color    <- ifelse(outcome_filter == "Won", "#00840D", "#E1463E")
  has_numbers <- "#" %in% names(df_filtered)

  p <- ggplot() +
    geom_polygon(data = data.frame(x = c(-0.708, 0.708, 0.708, 0, -0.708),
                                   y = c(0, 0, 0.25, 0.5, 0.25)),
                 aes(x = x, y = y), fill = "grey90", color = "black") +
    annotate("rect", xmin = -0.83083, xmax = 0.83083, ymin = 1.5, ymax = 3.3775,
             fill = NA, color = "black", linewidth = 1) +
    geom_point(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`),
               color = pt_color, size = ifelse(has_numbers, 6, 3), alpha = 0.9)

  if (has_numbers) {
    p <- p + geom_text(data = df_filtered, aes(x = `Plate Side`, y = `Plate Height`, label = `#`),
                       color = "white", size = 2.5, fontface = "bold")
  }

  p +
    xlim(-1.5, 1.5) + ylim(0, 3.75) +
    coord_fixed() +
    theme_minimal() +
    labs(title = plot_title, x = "Horizontal (ft)", y = "Height (ft)") +
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ==========================================
# CATCHER — PDF GENERATION
# ==========================================
generate_catcher_pdf <- function(game_framing, game_throwing, season_framing, season_throwing, catcher, game_date, output_file, logo_path = NULL) {

  catcher_name <- format_name(catcher)

  g_steal        <- gameStealInfo(catcher, game_throwing, game_date)
g_framing      <- gameFramingInfo(catcher, game_framing, game_date)
g_frame_coords <- framingPlotData(catcher, game_framing, game_date)
  g_won_p        <- plot_framing(g_frame_coords, "Won",  paste0(game_date, " — Strikes Won"))
  g_lost_p       <- plot_framing(g_frame_coords, "Lost", paste0(game_date, " — Strikes Lost"))

  s_steal        <- overallStealInfo(catcher, season_throwing)
s_steal_pitch  <- overallStealByPitch(catcher, season_throwing)
s_framing      <- overallFramingInfo(catcher, season_framing)
s_frame_coords <- framingPlotData(catcher, season_framing)
  s_won_p        <- plot_framing(s_frame_coords, "Won",  "Season — Strikes Won")
  s_lost_p       <- plot_framing(s_frame_coords, "Lost", "Season — Strikes Lost")

  logo_grob <- tryCatch({
    img <- magick::image_read(logo_path)
    img <- magick::image_resize(img, "x100")
    rasterGrob(as.raster(img), interpolate = TRUE)
  }, error = function(e) nullGrob())

  pdf(output_file, width = 11, height = 15)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  tryCatch({

    # PAGE 1 — GAME
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "— Postgame Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
    grid.text(as.character(game_date), x = 0.5, y = 0.05,
              gp = gpar(cex = 0.85, col = "#0C2340"))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    draw_grid_table(g_steal,
                    title = "Game Throwing — Steal Attempts",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.88, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(g_framing,
                    title = "Game Framing — Strike Log",
                    y_top = 0.77, x_center = 0.5, row_h = 0.020,
                    table_width = 0.75, header_cex = 0.68, cell_cex = 0.68, title_cex = 0.90)

    grid.text("Game Framing — Strike Locations", x = 0.5, y = 0.46,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
    print(g_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.44, width = 0.42, height = 0.22, just = c("center","top")))
    print(g_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

    # PAGE 2 — SEASON
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center","top")))
    grid.text(paste(catcher_name, "— Season Catching Report"), x = 0.5, y = 0.5,
              gp = gpar(fontface = "bold", cex = 1.6, col = "#0C2340"))
    pushViewport(viewport(x = 0.92, y = 0.5, width = 0.10, height = 0.90))
    grid.draw(logo_grob)
    popViewport()
    popViewport()

    draw_grid_table(s_steal,
                    title = "Season Throwing — Overall",
                    y_top = 0.88, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_steal_pitch,
                    title = "Season Throwing — By Pitch Type",
                    y_top = 0.78, x_center = 0.5, row_h = 0.022,
                    table_width = 0.85, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    draw_grid_table(s_framing,
                    title = "Season Framing — Strike Summary",
                    y_top = 0.63, x_center = 0.5, row_h = 0.022,
                    table_width = 0.40, header_cex = 0.70, cell_cex = 0.72, title_cex = 0.90)

    grid.text("Season Framing — Strike Locations", x = 0.5, y = 0.54,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))

    pushViewport(viewport(x = 0.27, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
    print(s_won_p, newpage = FALSE)
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.52, width = 0.42, height = 0.22, just = c("center","top")))
    print(s_lost_p, newpage = FALSE)
    popViewport()

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "grey40", fontface = "italic"))

  }, error = function(e) message("PDF generation error: ", conditionMessage(e)))
}

# ==========================================
# PITCHER — MODELS
# ==========================================
pitcher_model        <- readRDS("PitcherModels/Stuff+2.rds")
league_stats         <- readRDS("PitcherModels/NEW_LeagueStats2.rds")
xgb_fit              <- readRDS("PitcherModels/location_plus_model.rds")
league_stats_pitcher <- readRDS("PitcherModels/location_plus_league_stats_pitcher.rds")

# ==========================================
# PITCHER — CONSTANTS
# ==========================================
lg_zone_pct   <- "47.2%"
lg_strike_pct <- "62.1%"
lg_whiff_pct  <- "25.4%"
lg_hh_pct     <- "35%"

pitcher_pitch_colors <- c(
  "Fastball"  = "red",       "Sinker"    = "orange",
  "Slider"    = "gold",      "Sweeper"   = "pink",
  "Curveball" = "blue",      "Changeup"  = "green3",
  "Cutter"    = "#8B4513",   "Splitter"  = "mediumpurple3"
)

pitcher_strike_zone <- tibble(
  PlateLocSide   = c(-0.8303, -0.8303, 0.8303, 0.8303, -0.8303),
  PlateLocHeight = c(1.5, 3.3775, 3.3775, 1.5, 1.5)
)

pitcher_home_plate <- data.frame(
  x = c(-0.708, 0.708, 0.708, 0, -0.708),
  y = c(0, 0, 0.25, 0.5, 0.25)
)

# ==========================================
# PITCHER — HELPER FUNCTIONS
# ==========================================
map_pitch_type <- function(pt) {
  case_when(
    pt %in% c("Fastball","FourSeamFastBall","Four-Seam","FourSeam") ~ "Fastball",
    pt %in% c("Sinker","TwoSeamFastBall","OneSeamFastball")         ~ "Sinker",
    pt == "Cutter"                                                   ~ "Cutter",
    pt %in% c("Curveball","CurveBall")                              ~ "Curveball",
    pt == "Slider"                                                   ~ "Slider",
    pt == "Sweeper"                                                  ~ "Sweeper",
    pt %in% c("ChangeUp","Changeup")                                ~ "Changeup",
    pt == "Splitter"                                                 ~ "Splitter",
    TRUE                                                             ~ NA_character_
  )
}

clean_pitcher_pitch_type <- function(pt) {
  case_when(
    pt %in% c("Fastball","FourSeamFastBall","Four-Seam","FourSeam") ~ "Fastball",
    pt == "Sinker"                                                   ~ "Sinker",
    pt == "Slider"                                                   ~ "Slider",
    pt == "Sweeper"                                                  ~ "Sweeper",
    pt == "Curveball"                                                ~ "Curveball",
    pt %in% c("Changeup","ChangeUp")                                 ~ "Changeup",
    pt == "Cutter"                                                   ~ "Cutter",
    pt == "Splitter"                                                 ~ "Splitter",
    TRUE ~ NA_character_
  )
}

build_stuff_plus <- function(data) {
  fb_avgs <- data %>%
    filter(TaggedPitchType %in% c("Fastball","FourSeamFastBall","Four-Seam","FourSeam",
                                  "Sinker","TwoSeamFastBall","OneSeamFastball","Cutter")) %>%
    mutate(
      PitcherId = as.character(PitcherId),
      priority  = case_when(
        TaggedPitchType %in% c("Fastball","FourSeamFastBall","Four-Seam","FourSeam") ~ 1,
        TaggedPitchType %in% c("Sinker","TwoSeamFastBall","OneSeamFastball")         ~ 2,
        TaggedPitchType == "Cutter"                                                  ~ 3
      )
    ) %>%
    group_by(PitcherId, priority) %>%
    summarize(
      count   = n(),
      fb_velo = mean(RelSpeed, na.rm = TRUE),
      fb_ivb  = mean(InducedVertBreak, na.rm = TRUE),
      fb_hb   = mean(HorzBreak, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(PitcherId, priority, desc(count)) %>%
    slice_head(by = PitcherId, n = 1) %>%
    select(PitcherId, PrimaryFB = priority, fb_velo, fb_ivb, fb_hb) %>%
    mutate(PrimaryFB = case_when(
      PrimaryFB == 1 ~ "Fastball",
      PrimaryFB == 2 ~ "Sinker",
      PrimaryFB == 3 ~ "Cutter"
    ))

  data %>%
    mutate(
      PitcherId = as.character(PitcherId),
      PitchType = map_pitch_type(TaggedPitchType)
    ) %>%
    filter(!is.na(PitchType)) %>%
    left_join(fb_avgs, by = "PitcherId") %>%
    mutate(
      FF_diff  = ifelse(PitchType == PrimaryFB, 0, RelSpeed - fb_velo),
      IVB_diff = ifelse(PitchType == PrimaryFB, 0, InducedVertBreak - fb_ivb),
      HB_diff  = ifelse(PitchType == PrimaryFB, 0, HorzBreak - fb_hb),
      SameSide = as.factor(ifelse(BatterSide == PitcherThrows, 1, 0)),
      PitchType = as.factor(PitchType)
    ) %>%
    filter(
      !is.na(InducedVertBreak), !is.na(RelSide), !is.na(HorzBreak),
      !is.na(SpinRate), !is.na(Extension), !is.na(RelHeight),
      !is.na(RelSpeed), !is.na(SameSide)
    ) %>%
    bind_cols(suppressWarnings(predict(pitcher_model, ., type = "prob"))) %>%
    rename(expected_whiff = .pred_1) %>%
    group_by(PitchType) %>%
    summarise(expected_whiff_rate = mean(expected_whiff), .groups = "drop") %>%
    left_join(league_stats, by = "PitchType") %>%
    mutate(
      `Stuff+` = round(((expected_whiff_rate - league_avg) / league_sd) * 10 + 100, 0),
      Type     = as.character(PitchType)
    ) %>%
    select(Type, `Stuff+`)
}

build_location_plus <- function(data) {
  loc_data <- data %>%
    mutate(
      Balls       = as.integer(Balls),
      Strikes     = as.integer(Strikes),
      count_state = paste0(Balls, "-", Strikes),
      count_state = ifelse(!count_state %in% c("0-0","0-1","0-2","1-0","1-1","1-2",
                                               "2-0","2-1","2-2","3-0","3-1","3-2"),
                           NA, count_state),
      count_state = as.factor(count_state),
      PitchType   = map_pitch_type(TaggedPitchType)
    ) %>%
    filter(!is.na(PitchType), !is.na(count_state))

  predictions <- predict(xgb_fit, new_data = loc_data) %>%
    rename(xRV288 = .pred)

  bind_cols(predictions, loc_data) %>%
    group_by(PitchType) %>%
    summarise(avg_xRV288 = mean(xRV288, na.rm = TRUE), n = n(), .groups = "drop") %>%
    left_join(league_stats_pitcher, by = "PitchType") %>%
    mutate(
      `Location+` = round((-(avg_xRV288 - league_avg) / league_sd) * 10 + 100, 0),
      Type        = as.character(PitchType)
    ) %>%
    select(Type, `Location+`)
}

build_color_matrix_pitcher <- function(df, benchmarks, lower_is_better = c()) {
  get_color <- function(value, bottom, top, flip = FALSE) {
    value <- suppressWarnings(as.numeric(gsub("%", "", value)))
    if (is.na(value)) return("white")
    normalized <- pmax(0, pmin(1, (value - bottom) / (top - bottom)))
    if (flip) normalized <- 1 - normalized
    colorRamp(c("#E1463E", "#CDCD00", "#00840D"))(normalized) %>%
      { rgb(.[1], .[2], .[3], maxColorValue = 255) }
  }
  color_matrix <- matrix("white", nrow = nrow(df), ncol = ncol(df))
  for (i in seq_along(names(df))) {
    col_name <- names(df)[i]
    if (col_name %in% names(benchmarks)) {
      bounds <- benchmarks[[col_name]]
      flip   <- col_name %in% lower_is_better
      for (r in seq_len(nrow(df))) {
        color_matrix[r, i] <- get_color(df[[i]][r], bounds[1], bounds[2], flip)
      }
    }
  }
  color_matrix
}

build_split_color_matrix_pitcher <- function(df, bench_map, lower_is_better = c()) {
  color_matrix <- matrix("white", nrow = nrow(df), ncol = ncol(df))
  type_col <- if ("Pitch Type" %in% names(df)) "Pitch Type" else "Type"
  for (r in seq_len(nrow(df))) {
    pitch_type <- df[[type_col]][r]
    benchmarks <- bench_map[[pitch_type]]
    if (!is.null(benchmarks)) {
      row_colors <- build_color_matrix_pitcher(df[r,,drop=FALSE], benchmarks, lower_is_better)
      color_matrix[r, ] <- row_colors[1, ]
    }
  }
  color_matrix
}

prep_pitcher_data <- function(raw) {
  raw %>%
    mutate(TaggedPitchType_clean = clean_pitcher_pitch_type(TaggedPitchType)) %>%
    filter(!is.na(TaggedPitchType_clean))
}

make_zone_plot_pitcher <- function(data, side, title) {
  zone_data <- data %>%
    filter(BatterSide == side, !is.na(PlateLocSide), !is.na(PlateLocHeight),
           !PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulBallFieldable","FoulTip")) %>%
    mutate(HardHit = factor(case_when(
      !is.na(ExitSpeed) & ExitSpeed >= 95 ~ "Hard Hit",
      TRUE ~ "Normal"
    ), levels = c("Normal","Hard Hit")))

  ggplot() +
    geom_polygon(data = pitcher_home_plate, aes(x = x, y = y),
                 fill = "white", color = "black", linewidth = 0.8) +
    geom_path(data = pitcher_strike_zone, aes(x = PlateLocSide, y = PlateLocHeight),
              color = "black", linewidth = 1) +
    geom_point(data = zone_data %>% filter(HardHit == "Normal"),
               aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType_clean),
               size = 4.5, alpha = 0.90, shape = 21, color = "black", stroke = 1.2) +
    geom_point(data = zone_data %>% filter(HardHit == "Hard Hit"),
               aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType_clean),
               size = 6, alpha = 1, shape = 23, color = "black", stroke = 2) +
    scale_fill_manual(values = pitcher_pitch_colors, drop = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    xlim(-2.5, 2.5) + ylim(0, 5) +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title  = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text   = element_blank(), axis.ticks = element_blank(),
          panel.grid  = element_blank())
}

make_density_plots_pitcher <- function(data, batter_side) {
  side_data <- data %>% filter(BatterSide == batter_side)
  pitch_types <- side_data %>%
    count(TaggedPitchType_clean) %>%
    filter(n >= 3) %>%
    pull(TaggedPitchType_clean)

  lapply(pitch_types, function(pt) {
    pt_data      <- side_data %>% filter(TaggedPitchType_clean == pt)
    whiff_data   <- pt_data %>% filter(PitchCall == "StrikeSwinging")
    hardHit_data <- pt_data %>% filter(!is.na(ExitSpeed) & ExitSpeed >= 95)

    p <- ggplot(pt_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = after_stat(density)),
                      geom = "raster", contour = FALSE, interpolate = TRUE) +
      scale_fill_gradient(low = "lightblue", high = "red") +
      geom_path(data = pitcher_strike_zone, aes(x = PlateLocSide, y = PlateLocHeight),
                color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_polygon(data = pitcher_home_plate, aes(x = x, y = y),
                   fill = "white", color = "black", linewidth = 0.8, inherit.aes = FALSE)

    if (nrow(whiff_data) > 0)
      p <- p + geom_point(data = whiff_data, aes(x = PlateLocSide, y = PlateLocHeight),
                          shape = 4, size = 2, color = "black", stroke = 1.5, inherit.aes = FALSE)
    if (nrow(hardHit_data) > 0)
      p <- p + geom_point(data = hardHit_data, aes(x = PlateLocSide, y = PlateLocHeight),
                          shape = 23, size = 2, color = "white", fill = NA,
                          stroke = 1.5, inherit.aes = FALSE)

    p + xlim(-2.5, 2.5) + ylim(0, 5) + coord_fixed() + labs(title = pt) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title  = element_text(hjust = 0.5, size = 11, face = "bold"),
            axis.text   = element_blank(), axis.ticks = element_blank(),
            axis.title  = element_blank(), panel.grid = element_blank())
  })
}

place_density_row_pitcher <- function(plots, y_top, height = 0.18) {
  n <- length(plots)
  if (n == 0) return(invisible(NULL))
  while (length(plots) < 4) plots <- c(list(ggplot() + theme_void()), plots)
  plots <- plots[1:4]
  xs <- c(0.13, 0.38, 0.63, 0.88)
  for (i in seq_along(xs)) {
    pushViewport(viewport(x = xs[i], y = y_top, width = 0.24, height = height,
                          just = c("center","top")))
    suppressWarnings(print(plots[[i]], newpage = FALSE))
    popViewport()
  }
}

generate_pitcher_pdf <- function(pitcher_data, pitcher_data_season, selected_pitcher,
                                 manual_pitches, manual_ks, manual_bbs, manual_hits, manual_runs,
                                 output_file, logo_path = NULL) {

  message("INSIDE generate_pitcher_pdf")

  # ── Force manual overrides to proper NA ──
  manual_pitches <- if (is.null(manual_pitches) || length(manual_pitches) == 0 || is.na(manual_pitches)) NA_integer_ else as.integer(manual_pitches)
  manual_ks      <- if (is.null(manual_ks)      || length(manual_ks) == 0      || is.na(manual_ks))      NA_integer_ else as.integer(manual_ks)
  manual_bbs     <- if (is.null(manual_bbs)     || length(manual_bbs) == 0     || is.na(manual_bbs))     NA_integer_ else as.integer(manual_bbs)
  manual_hits    <- if (is.null(manual_hits)    || length(manual_hits) == 0    || is.na(manual_hits))    NA_integer_ else as.integer(manual_hits)
  manual_runs    <- if (is.null(manual_runs)    || length(manual_runs) == 0    || is.na(manual_runs))    NA_integer_ else as.integer(manual_runs)
  message("manual overrides done")

  pitcher_name <- format_name(selected_pitcher)
  message("pitcher_name: ", pitcher_name)

  games_played <- pitcher_data_season %>%
    summarise(games = n_distinct(as.Date(as.character(Date)))) %>% pull(games)
  message("games_played: ", games_played)

  # ── Force numeric columns ──
  coerce_numeric <- function(df) {
    df %>% mutate(
      RunsScored       = as.numeric(RunsScored),
      ExitSpeed        = as.numeric(ExitSpeed),
      PlateLocSide     = as.numeric(PlateLocSide),
      PlateLocHeight   = as.numeric(PlateLocHeight),
      RelSpeed         = as.numeric(RelSpeed),
      InducedVertBreak = as.numeric(InducedVertBreak),
      HorzBreak        = as.numeric(HorzBreak),
      RelHeight        = as.numeric(RelHeight),
      RelSide          = as.numeric(RelSide),
      Extension        = as.numeric(Extension),
      SpinRate         = as.numeric(SpinRate),
      VertApprAngle    = as.numeric(VertApprAngle),
      HorzApprAngle    = as.numeric(HorzApprAngle),
      Balls            = as.integer(Balls),
      Strikes          = as.integer(Strikes),
      Inning           = as.integer(Inning),
      PAofInning       = as.integer(PAofInning),
      PitchofPA        = as.integer(PitchofPA)
    )
  }

  pitcher_data        <- coerce_numeric(pitcher_data)
  pitcher_data_season <- coerce_numeric(pitcher_data_season)
  message("coercion done")

  # ── Counting stats ──
  counting_stats <- pitcher_data %>%
    mutate(
      IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsZone    = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
                  abs(PlateLocSide) <= 0.8303 &
                  PlateLocHeight >= 1.5 & PlateLocHeight <= 3.3775,
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsHardHit = !is.na(ExitSpeed) & ExitSpeed >= 95 &
                  !PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulBallFieldable","FoulTip")
    ) %>%
    summarise(
      Pitches     = n(),
      `K's`       = sum(KorBB == "Strikeout", na.rm = TRUE),
      `BB's`      = sum(KorBB == "Walk",      na.rm = TRUE),
      Hits        = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE),
      ER          = sum(RunsScored, na.rm = TRUE),
      `Zone%`     = paste0(round(mean(IsZone,   na.rm = TRUE) * 100, 1), "%"),
      `Strike%`   = paste0(round(mean(IsStrike, na.rm = TRUE) * 100, 1), "%"),
      `Whiff%`    = paste0(round(sum(IsWhiff)   / sum(IsSwing) * 100, 1), "%"),
      `Hard Hit%` = paste0(round(sum(IsHardHit) / sum(!is.na(ExitSpeed)) * 100, 1), "%")
    ) %>%
    mutate(
      Pitches = if (!is.na(manual_pitches)) as.integer(manual_pitches) else Pitches,
      `K's`   = if (!is.na(manual_ks))     as.integer(manual_ks)      else `K's`,
      `BB's`  = if (!is.na(manual_bbs))    as.integer(manual_bbs)     else `BB's`,
      Hits    = if (!is.na(manual_hits))   as.integer(manual_hits)    else Hits,
      ER      = if (!is.na(manual_runs))   as.integer(manual_runs)    else ER
    )
  message("step 1: counting_stats done")

  lg_avg_row <- data.frame(
    Pitches = "-", `K's` = "-", `BB's` = "-", Hits = "-", ER = "-",
    `Zone%` = lg_zone_pct, `Strike%` = lg_strike_pct,
    `Whiff%` = lg_whiff_pct, `Hard Hit%` = lg_hh_pct,
    check.names = FALSE
  )

  counting_stats_display <- bind_rows(
    counting_stats %>% mutate(across(everything(), as.character)),
    lg_avg_row
  )
  message("step 2: counting_stats_display done")

  # ── Pitch specs ──
  stuff_game    <- tryCatch(build_stuff_plus(pitcher_data),    error = function(e) { message("stuff_game error: ", e$message); NULL })
  location_game <- tryCatch(build_location_plus(pitcher_data), error = function(e) { message("location_game error: ", e$message); NULL })
  message("step 3: stuff/location done")

  total_pitches_game <- nrow(pitcher_data)

  pitch_specs <- pitcher_data %>%
    mutate(
      PitchType = map_pitch_type(TaggedPitchType),
      IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsZone    = abs(PlateLocSide) <= 0.8303 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.3775,
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsHardHit = !is.na(ExitSpeed) & ExitSpeed >= 95 &
                  !PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulBallFieldable","FoulTip")
    ) %>%
    filter(!is.na(PitchType)) %>%
    group_by(PitchType) %>%
    summarise(
      usage_n     = n(),
      `Usage%`    = paste0(round(n() / total_pitches_game * 100, 1), "%"),
      Velo        = round(mean(RelSpeed,           na.rm = TRUE), 1),
      iVB         = round(mean(InducedVertBreak,   na.rm = TRUE), 1),
      HB          = round(mean(HorzBreak,          na.rm = TRUE), 1),
      RelH        = round(mean(RelHeight,          na.rm = TRUE), 2),
      RelS        = round(mean(RelSide,            na.rm = TRUE), 2),
      Ext         = round(mean(Extension,          na.rm = TRUE), 2),
      `Whiff%`    = round(ifelse(sum(IsSwing) == 0, NA, sum(IsWhiff) / sum(IsSwing) * 100), 1),
      `Zone%`     = round(sum(IsZone, na.rm = TRUE) / n() * 100, 1),
      `Strike%`   = round(sum(IsStrike, na.rm = TRUE) / n() * 100, 1),
      `Hard Hit%` = round(ifelse(sum(!is.na(ExitSpeed)) == 0, NA,
                                 sum(IsHardHit) / sum(!is.na(ExitSpeed)) * 100), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(usage_n)) %>%
    select(-usage_n) %>%
    rename(Type = PitchType) %>%
    relocate(`Usage%`, .after = Type) %>%
    { if (!is.null(stuff_game))    left_join(., stuff_game,    by = "Type") else . } %>%
    { if (!is.null(location_game)) left_join(., location_game, by = "Type") else . } %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
  message("step 4: pitch_specs done - rows: ", nrow(pitch_specs))

  # ── RISP ──
  risp_data <- tryCatch({
    pitcher_data %>%
      arrange(Inning, PAofInning, PitchofPA) %>%
      group_by(Inning) %>%
      filter(n() > 0) %>%
      mutate(
        RunnerOnBase = PAofInning > min(PAofInning, na.rm = TRUE) &
          cumsum(lag(PlayResult %in% c("Single","Double","Triple","Error","FieldersChoice") |
                       KorBB %in% c("Walk","HitByPitch"), default = FALSE)) > 0,
        RISP = RunnerOnBase & (
          cumsum(lag(PlayResult %in% c("Double","Triple"), default = FALSE)) > 0 |
            cumsum(lag(PlayResult == "StolenBase", default = FALSE)) > 0 |
            cumsum(lag(PlayResult %in% c("Single","Double","Triple","Error","FieldersChoice") |
                         KorBB %in% c("Walk","HitByPitch"), default = FALSE)) >= 2
        )
      ) %>%
      ungroup() %>%
      filter(RISP == TRUE)
  }, error = function(e) { message("risp_data error: ", e$message); data.frame() })

  risp_stats <- tryCatch({
    risp_data %>%
      mutate(
        PitchType = map_pitch_type(TaggedPitchType),
        IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                     "FoulBallNotFieldable","FoulTip","InPlay"),
        IsZone    = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
                    abs(PlateLocSide) <= 0.8303 &
                    PlateLocHeight >= 1.5 & PlateLocHeight <= 3.3775,
        IsWhiff   = PitchCall == "StrikeSwinging",
        IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall",
                                     "FoulBallNotFieldable","FoulTip","InPlay"),
        IsHardHit = !is.na(ExitSpeed) & ExitSpeed >= 95
      ) %>%
      filter(!is.na(PitchType)) %>%
      group_by(Type = PitchType) %>%
      summarise(
        Pitches     = n(),
        Velo        = round(mean(RelSpeed,         na.rm = TRUE), 1),
        iVB         = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HB          = round(mean(HorzBreak,        na.rm = TRUE), 1),
        `Whiff%`    = round(ifelse(sum(IsSwing) == 0, NA, sum(IsWhiff) / sum(IsSwing) * 100), 1),
        `Zone%`     = round(mean(IsZone,   na.rm = TRUE) * 100, 1),
        `Strike%`   = round(mean(IsStrike, na.rm = TRUE) * 100, 1),
        `Hard Hit%` = round(ifelse(sum(!is.na(ExitSpeed)) == 0, NA,
                                   sum(IsHardHit) / sum(!is.na(ExitSpeed)) * 100), 1),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
  }, error = function(e) { message("risp_stats error: ", e$message); data.frame() })
  message("step 5: risp_stats done - rows: ", nrow(risp_stats))

  # ── Game plots ──
  message("step 5b: building movement_data")

  movement_data <- pitcher_data %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(HB   = round(mean(HorzBreak,        na.rm = TRUE), 1),
              iVB  = round(mean(InducedVertBreak,  na.rm = TRUE), 1),
              Velo = round(mean(RelSpeed,          na.rm = TRUE), 1),
              .groups = "drop")
  message("step 5c: p_movement done")

  p_movement <- ggplot() +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(data = pitcher_data,
               aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType_clean),
               size = 4, alpha = 0.8, shape = 21, color = "black", stroke = 0.5) +
    geom_point(data = movement_data,
               aes(x = HB, y = iVB, color = TaggedPitchType_clean),
               size = 10, alpha = 0.9) +
    geom_text(data = movement_data, aes(x = HB, y = iVB, label = Velo),
              color = "white", size = 3.5, fontface = "bold") +
    scale_color_manual(values = pitcher_pitch_colors, drop = TRUE) +
    scale_fill_manual(values  = pitcher_pitch_colors, drop = TRUE) +
    labs(title = "Pitch Movement",
         x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    xlim(-25, 25) + ylim(-25, 25) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  message("step 5d: p_velo done")

  p_velo <- ggplot(pitcher_data,
                   aes(x = RelSpeed, y = TaggedPitchType_clean,
                       fill = TaggedPitchType_clean, color = TaggedPitchType_clean)) +
    ggridges::geom_density_ridges(alpha = 0.6, scale = 0.9, rel_min_height = 0.01,
                                  quantile_lines = TRUE, quantiles = 2) +
    scale_fill_manual(values  = pitcher_pitch_colors, drop = TRUE) +
    scale_color_manual(values = pitcher_pitch_colors, drop = TRUE) +
    labs(title = "Velocity Distribution", x = "Velocity (mph)", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title  = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"))
  message("step 5e: p_left done")

  p_left  <- make_zone_plot_pitcher(pitcher_data, "Left",  "Strike Zone vs. LHH")
  message("step 5f: p_right done")

  p_right <- make_zone_plot_pitcher(pitcher_data, "Right", "Strike Zone vs. RHH")
  message("step 5g: combined_plot done")

  combined_plot <- (p_movement | p_velo) / (p_left | p_right) +
    patchwork::plot_annotation(
      caption = "Diamond = Hard Hit (95+ mph EV)",
      theme   = theme(plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"))
    )
  message("step 6: game plots done")

  # ── Season stats ──
  season_totals <- pitcher_data_season %>%
    mutate(
      IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsZone    = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
                  abs(PlateLocSide) <= 0.8303 &
                  PlateLocHeight >= 1.5 & PlateLocHeight <= 3.3775,
      IsWhiff   = PitchCall == "StrikeSwinging",
      IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall",
                                   "FoulBallNotFieldable","FoulTip","InPlay"),
      IsHardHit = !is.na(ExitSpeed) & ExitSpeed >= 95,
      IsGB      = TaggedHitType == "GroundBall",
      IsBIP     = !is.na(TaggedHitType) & TaggedHitType != "",
      IsK       = KorBB == "Strikeout",
      IsBB      = KorBB == "Walk",
      IsPA      = KorBB %in% c("Strikeout","Walk") | PitchCall == "InPlay"
    ) %>%
    summarise(
      Games       = games_played,
      PA          = sum(IsPA,  na.rm = TRUE),
      `R/App`     = round(sum(RunsScored, na.rm = TRUE) / games_played, 2),
      `GB%`       = paste0(round(sum(IsGB,  na.rm = TRUE) / sum(IsBIP, na.rm = TRUE) * 100, 1), "%"),
      `K%`        = paste0(round(sum(IsK,   na.rm = TRUE) / sum(IsPA,  na.rm = TRUE) * 100, 1), "%"),
      `BB%`       = paste0(round(sum(IsBB,  na.rm = TRUE) / sum(IsPA,  na.rm = TRUE) * 100, 1), "%"),
      `Hard Hit%` = paste0(round(sum(IsHardHit, na.rm = TRUE) / sum(!is.na(ExitSpeed)) * 100, 1), "%"),
      `Zone%`     = paste0(round(mean(IsZone,   na.rm = TRUE) * 100, 1), "%"),
      `Strike%`   = paste0(round(mean(IsStrike, na.rm = TRUE) * 100, 1), "%"),
      `Whiff%`    = paste0(round(sum(IsWhiff)   / sum(IsSwing)  * 100, 1), "%")
    )
  message("step 7: season_totals done")

  # ── Season movement plot ──
  movement_data_season <- pitcher_data_season %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(HB   = round(mean(HorzBreak,       na.rm = TRUE), 1),
              iVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
              Velo = round(mean(RelSpeed,         na.rm = TRUE), 1),
              .groups = "drop")

  p_movement_season <- ggplot() +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(data = pitcher_data_season,
               aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType_clean),
               size = 4, alpha = 0.8, shape = 21, color = "black", stroke = 0.5) +
    geom_point(data = movement_data_season,
               aes(x = HB, y = iVB, color = TaggedPitchType_clean),
               size = 10, alpha = 0.9) +
    geom_text(data = movement_data_season, aes(x = HB, y = iVB, label = Velo),
              color = "white", size = 3.5, fontface = "bold") +
    scale_color_manual(values = pitcher_pitch_colors, drop = TRUE) +
    scale_fill_manual(values  = pitcher_pitch_colors, drop = TRUE) +
    labs(title = "Season Pitch Movement",
         x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    xlim(-25, 25) + ylim(-30, 30) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  message("step 8: season movement done")

  # ── Season pitch specs ──
  total_pitches_season <- nrow(pitcher_data_season)

  season_pitch_specs <- pitcher_data_season %>%
    mutate(PitchType = map_pitch_type(TaggedPitchType)) %>%
    filter(!is.na(PitchType)) %>%
    group_by(Type = PitchType) %>%
    summarise(
      usage_n    = n(),
      `Usage%`   = paste0(round(n() / total_pitches_season * 100, 1), "%"),
      iVB        = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB         = round(mean(HorzBreak,        na.rm = TRUE), 1),
      Velo       = round(mean(RelSpeed,         na.rm = TRUE), 1),
      `Max Velo` = round(max(RelSpeed,          na.rm = TRUE), 1),
      Ext        = round(mean(Extension,        na.rm = TRUE), 2),
      Spin       = round(mean(SpinRate,         na.rm = TRUE), 0),
      RelH       = round(mean(RelHeight,        na.rm = TRUE), 2),
      RelS       = round(mean(RelSide,          na.rm = TRUE), 2),
      VAA        = round(mean(VertApprAngle,    na.rm = TRUE), 1),
      HAA        = round(mean(HorzApprAngle,    na.rm = TRUE), 1),
      .groups    = "drop"
    ) %>%
    arrange(desc(usage_n)) %>%
    select(-usage_n)
  message("step 9: season_pitch_specs done - rows: ", nrow(season_pitch_specs))

  # ── Split tables ──
  make_split_table_pitcher <- function(data, batter_side) {
    side_data <- data %>%
      filter(BatterSide == batter_side) %>%
      mutate(
        IsStrike  = PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                     "FoulBallNotFieldable","FoulTip","InPlay"),
        IsZone    = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
                    abs(PlateLocSide) <= 0.8303 &
                    PlateLocHeight >= 1.5 & PlateLocHeight <= 3.3775,
        IsWhiff   = PitchCall == "StrikeSwinging",
        IsSwing   = PitchCall %in% c("StrikeSwinging","FoulBall",
                                     "FoulBallNotFieldable","FoulTip","InPlay"),
        IsHardHit = !is.na(ExitSpeed) & ExitSpeed >= 95,
        IsChase   = !IsZone & IsSwing,
        IsContact = PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulTip","InPlay")
      )

    total_n       <- nrow(side_data)
    stuff_side    <- tryCatch(build_stuff_plus(data %>% filter(BatterSide == batter_side)),
                              error = function(e) { message("stuff_side error: ", e$message); NULL })
    location_side <- tryCatch(build_location_plus(data %>% filter(BatterSide == batter_side)),
                              error = function(e) { message("location_side error: ", e$message); NULL })

    result <- side_data %>%
      group_by(TaggedPitchType_clean) %>%
      summarise(
        usage_n    = n(),
        `Zone%`    = paste0(round(mean(IsZone,   na.rm = TRUE) * 100, 1), "%"),
        `Strike%`  = paste0(round(mean(IsStrike, na.rm = TRUE) * 100, 1), "%"),
        `Whiff%`   = ifelse(sum(IsSwing) == 0, "-",
                            paste0(round(sum(IsWhiff)   / sum(IsSwing)  * 100, 1), "%")),
        `Chase%`   = ifelse(sum(!IsZone) == 0, "-",
                            paste0(round(sum(IsChase)   / sum(!IsZone)  * 100, 1), "%")),
        `Contact%` = ifelse(sum(IsSwing) == 0, "-",
                            paste0(round(sum(IsContact) / sum(IsSwing)  * 100, 1), "%")),
        `Hard Hit%` = ifelse(sum(!is.na(ExitSpeed)) == 0, "-",
                             paste0(round(sum(IsHardHit) / sum(!is.na(ExitSpeed)) * 100, 1), "%")),
        .groups = "drop"
      ) %>%
      arrange(desc(usage_n)) %>%
      mutate(`Usage%` = paste0(round(usage_n / total_n * 100, 1), "%")) %>%
      select(-usage_n) %>%
      relocate(`Usage%`, .after = TaggedPitchType_clean) %>%
      rename(`Pitch Type` = TaggedPitchType_clean) %>%
      slice_head(n = 4)

    if (!is.null(stuff_side))    result <- left_join(result, stuff_side,    by = c("Pitch Type" = "Type"))
    if (!is.null(location_side)) result <- left_join(result, location_side, by = c("Pitch Type" = "Type"))
    result
  }

  lhh_table <- tryCatch(make_split_table_pitcher(pitcher_data_season, "Left"),
                        error = function(e) { message("lhh_table error: ", e$message); data.frame() })
  rhh_table <- tryCatch(make_split_table_pitcher(pitcher_data_season, "Right"),
                        error = function(e) { message("rhh_table error: ", e$message); data.frame() })
  message("step 10: split tables done - lhh: ", nrow(lhh_table), " rhh: ", nrow(rhh_table))

  density_lhh <- tryCatch(make_density_plots_pitcher(pitcher_data_season, "Left"),
                          error = function(e) { message("density_lhh error: ", e$message); list() })
  density_rhh <- tryCatch(make_density_plots_pitcher(pitcher_data_season, "Right"),
                          error = function(e) { message("density_rhh error: ", e$message); list() })
  message("step 11: density plots done")

  # ── Color matrices ──
  game_stats_benchmarks <- list(
    `Zone%` = c(42,53), `Strike%` = c(58,67), `Whiff%` = c(15,30), `Hard Hit%` = c(27,44)
  )
  game_stats_color_matrix <- tryCatch(
    rbind(
      build_color_matrix_pitcher(counting_stats_display[1,,drop=FALSE],
                                 game_stats_benchmarks, lower_is_better = "Hard Hit%"),
      matrix("white", nrow = 1, ncol = ncol(counting_stats_display))
    ), error = function(e) { message("game_stats_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(counting_stats_display), ncol = ncol(counting_stats_display)) }
  )

  fastball_pitch_benchmarks <- list(
    `Whiff%` = c(10,25), `Zone%` = c(44,57), `Strike%` = c(58,70),
    `Hard Hit%` = c(29,50), `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  breaker_pitch_benchmarks <- list(
    `Whiff%` = c(18,41), `Zone%` = c(33,50), `Strike%` = c(51,70),
    `Hard Hit%` = c(15,42), `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  offspeed_pitch_benchmarks <- list(
    `Whiff%` = c(18,46), `Zone%` = c(24,48), `Strike%` = c(44,68),
    `Hard Hit%` = c(9,50), `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  pitch_bench_map <- list(
    "Fastball" = fastball_pitch_benchmarks, "Sinker"    = fastball_pitch_benchmarks,
    "Cutter"   = fastball_pitch_benchmarks, "Slider"    = breaker_pitch_benchmarks,
    "Sweeper"  = breaker_pitch_benchmarks,  "Curveball" = breaker_pitch_benchmarks,
    "Changeup" = offspeed_pitch_benchmarks, "Splitter"  = offspeed_pitch_benchmarks
  )

  pitch_specs_color_matrix <- tryCatch(
    build_split_color_matrix_pitcher(pitch_specs, pitch_bench_map, "Hard Hit%"),
    error = function(e) { message("pitch_specs_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(pitch_specs), ncol = ncol(pitch_specs)) }
  )

  risp_color_matrix <- if (nrow(risp_stats) > 0) tryCatch(
    build_split_color_matrix_pitcher(risp_stats, pitch_bench_map, "Hard Hit%"),
    error = function(e) { message("risp_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(risp_stats), ncol = ncol(risp_stats)) }
  ) else NULL

  season_totals_color_matrix <- tryCatch(
    build_color_matrix_pitcher(
      season_totals,
      list(`GB%` = c(35,53), `K%` = c(14,27), `BB%` = c(5,12),
           `Hard Hit%` = c(27,43), `Zone%` = c(44,55), `Strike%` = c(59,67), `Whiff%` = c(16,30)),
      lower_is_better = c("BB%","Hard Hit%")
    ), error = function(e) { message("season_totals_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(season_totals), ncol = ncol(season_totals)) }
  )

  fastball_split_benchmarks <- list(
    `Zone%` = c(33,50), `Strike%` = c(58,72), `Whiff%` = c(10,25),
    `Chase%` = c(17,30), `Contact%` = c(75,90), `Hard Hit%` = c(29,50),
    `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  breaker_split_benchmarks <- list(
    `Zone%` = c(33,50), `Strike%` = c(51,70), `Whiff%` = c(18,41),
    `Chase%` = c(17,33), `Contact%` = c(59,82), `Hard Hit%` = c(15,42),
    `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  offspeed_split_benchmarks <- list(
    `Zone%` = c(24,48), `Strike%` = c(44,68), `Whiff%` = c(18,46),
    `Chase%` = c(15,40), `Contact%` = c(53,83), `Hard Hit%` = c(9,50),
    `Stuff+` = c(80,120), `Location+` = c(80,120)
  )
  lhh_rhh_bench_map <- list(
    "Fastball" = fastball_split_benchmarks, "Sinker"    = fastball_split_benchmarks,
    "Cutter"   = fastball_split_benchmarks, "Slider"    = breaker_split_benchmarks,
    "Sweeper"  = breaker_split_benchmarks,  "Curveball" = breaker_split_benchmarks,
    "Changeup" = offspeed_split_benchmarks, "Splitter"  = offspeed_split_benchmarks
  )

  lhh_color_matrix <- tryCatch(
    build_split_color_matrix_pitcher(lhh_table, lhh_rhh_bench_map, c("Hard Hit%","Contact%")),
    error = function(e) { message("lhh_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(lhh_table), ncol = ncol(lhh_table)) }
  )

  rhh_color_matrix <- tryCatch(
    build_split_color_matrix_pitcher(rhh_table, lhh_rhh_bench_map, c("Hard Hit%","Contact%")),
    error = function(e) { message("rhh_color_matrix failed: ", e$message)
      matrix("white", nrow = nrow(rhh_table), ncol = ncol(rhh_table)) }
  )

  arsenal_color_matrix <- tryCatch({
    m <- matrix("white", nrow = nrow(season_pitch_specs), ncol = ncol(season_pitch_specs))
    for (r in seq_len(nrow(season_pitch_specs))) {
      pt <- season_pitch_specs$Type[r]
      m[r, 1] <- ifelse(!is.na(pitcher_pitch_colors[pt]), pitcher_pitch_colors[pt], "white")
    }
    m
  }, error = function(e) { message("arsenal_color_matrix failed: ", e$message)
    matrix("white", nrow = nrow(season_pitch_specs), ncol = ncol(season_pitch_specs)) }
  )
  message("step 12: all color matrices done")

  # ── Logo ──
  logo_grob <- tryCatch({
    img <- magick::image_read(logo_path)
    img <- magick::image_resize(img, "x100")
    rasterGrob(as.raster(img), interpolate = TRUE)
  }, error = function(e) nullGrob())

  pdf(output_file, width = 11, height = 15)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  tryCatch({

    # PAGE 1 — GAME
    grid.newpage()
    grid.rect(x = 0, y = 1, width = 1, height = 0.06,
              just = c("left","top"), gp = gpar(fill = "#0C2340", col = NA))
    grid.text(pitcher_name, x = 0.03, y = 0.978, just = "left",
              gp = gpar(col = "white", fontface = "bold", cex = 1.2))
    grid.text("Postgame Pitcher Report", x = 0.03, y = 0.948, just = "left",
              gp = gpar(col = "white", cex = 1))
    pushViewport(viewport(x = 0.97, y = 0.970, width = 0.04, height = 0.055,
                          just = c("center","center")))
    grid.draw(logo_grob)
    popViewport()

    grid.text("Game Stats", x = 0.5, y = 0.92,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
    draw_grid_table(counting_stats_display,
                    y_top = 0.90, x_center = 0.5, row_h = 0.020,
                    table_width = 0.70, header_cex = 0.80, cell_cex = 0.80,
                    alt_row_bg = "grey80", color_matrix = game_stats_color_matrix)

    pushViewport(viewport(x = 0.5, y = 0.82, width = 0.98, height = 0.46, just = c("center","top")))
    suppressWarnings(print(combined_plot, newpage = FALSE))
    popViewport()

    grid.text("Pitch Specifications", x = 0.5, y = 0.355,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
    draw_grid_table(pitch_specs,
                    y_top = 0.34, x_center = 0.5, row_h = 0.024,
                    table_width = 0.94, header_cex = 0.80, cell_cex = 0.80,
                    color_matrix = pitch_specs_color_matrix)

    if (nrow(risp_stats) > 0) {
      grid.text("Pitch Specs - RISP", x = 0.5, y = 0.185,
                gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
      draw_grid_table(risp_stats,
                      y_top = 0.175, x_center = 0.5, row_h = 0.024,
                      table_width = 0.80, header_cex = 0.80, cell_cex = 0.80,
                      color_matrix = risp_color_matrix)
    }

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.02, gp = gpar(cex = 0.55, col = "#0C2340", fontface = "italic"))

    # PAGE 2 — SEASON
    grid.newpage()
    grid.rect(x = 0, y = 1, width = 1, height = 0.09,
              just = c("left","top"), gp = gpar(fill = "#0C2340", col = NA))
    grid.text(pitcher_name, x = 0.03, y = 0.965, just = "left",
              gp = gpar(col = "white", fontface = "bold", cex = 1.3))
    grid.text("2025 Season Report", x = 0.03, y = 0.927, just = "left",
              gp = gpar(col = "white", fontface = "bold", cex = 1))
    pushViewport(viewport(x = 0.96, y = 0.955, width = 0.07, height = 0.08,
                          just = c("center","center")))
    grid.draw(logo_grob)
    popViewport()

    grid.text("Season Totals", x = 0.5, y = 0.885,
              gp = gpar(fontface = "bold", cex = 1, col = "#0C2340"))
    draw_grid_table(season_totals,
                    y_top = 0.875, x_center = 0.5, row_h = 0.025,
                    table_width = 0.86, header_cex = 0.80, cell_cex = 0.80,
                    color_matrix = season_totals_color_matrix)

    draw_grid_table(season_pitch_specs,
                    title = "Season Pitch Specs",
                    y_top = 0.75, x_center = 0.28, row_h = 0.024,
                    table_width = 0.56, header_cex = 0.62, cell_cex = 0.55,
                    color_matrix = arsenal_color_matrix)

    pushViewport(viewport(x = 0.78, y = 0.820, width = 0.40, height = 0.26, just = c("center","top")))
    suppressWarnings(print(p_movement_season, newpage = FALSE))
    popViewport()

    grid.text("vs. LHH (Season)", x = 0.52, y = 0.555,
              gp = gpar(fontface = "bold", cex = 0.82, col = "#0C2340"))
    draw_grid_table(lhh_table,
                    y_top = 0.545, x_center = 0.5, row_h = 0.018,
                    table_width = 0.92, header_cex = 0.80, cell_cex = 0.80,
                    color_matrix = lhh_color_matrix)

    grid.text("Location Density vs. LHH", x = 0.52, y = 0.440,
              gp = gpar(fontface = "bold", cex = 0.82, col = "#0C2340"))
    grid.text("X = Whiff  |  Diamond = Hard Hit (95+ mph EV)", x = 0.52, y = 0.430,
              gp = gpar(cex = 0.55, col = "#0C2340", fontface = "italic"))
    place_density_row_pitcher(density_lhh, y_top = 0.42, height = 0.145)

    grid.text("vs. RHH (Season)", x = 0.52, y = 0.27,
              gp = gpar(fontface = "bold", cex = 0.82, col = "#0C2340"))
    draw_grid_table(rhh_table,
                    y_top = 0.26, x_center = 0.5, row_h = 0.018,
                    table_width = 0.92, header_cex = 0.80, cell_cex = 0.80,
                    color_matrix = rhh_color_matrix)

    grid.text("Location Density vs. RHH", x = 0.52, y = 0.16,
              gp = gpar(fontface = "bold", cex = 0.82, col = "#0C2340"))
    grid.text("X = Whiff  |  Diamond = Hard Hit (95+ mph EV)", x = 0.52, y = 0.152,
              gp = gpar(cex = 0.50, col = "#0C2340", fontface = "italic"))
    place_density_row_pitcher(density_rhh, y_top = 0.147, height = 0.150)

    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x = 0.5, y = 0.005, gp = gpar(cex = 0.45, col = "#0C2340", fontface = "italic"))

  }, error = function(e) message("PDF generation error: ", conditionMessage(e)))
}


           
# ==========================================
# HUB UI
# ==========================================
apps <- list(
  list(id = "catcher", title = "Catcher Reports",        page = "catcher", status = "live"),
  list(id = "hitter",  title = "Postgame Hitter Reports", page = NULL,      status = "live"),
  list(id = "pitcher", title = "Postgame Pitcher Reports",page = "pitcher",      status = "live"),
  list(id = "umpire",  title = "Umpire Reports",          page = NULL,      status = "live")
)

make_card <- function(app) {
  is_coming_soon <- app$status == "coming_soon"
  card_class  <- paste("app-card", if (is_coming_soon) "coming-soon" else "")
  badge_class <- paste("status-badge", if (is_coming_soon) "coming-soon" else "live")
  badge_label <- if (is_coming_soon) "Coming Soon" else "Live"

  if (!is.null(app$page) && app$status == "live") {
    onclick_js <- paste0("Shiny.setInputValue('nav_to', '", app$page, "', {priority: 'event'})")
    tags$div(
      onclick = onclick_js,
      class   = card_class,
      style   = "cursor: pointer;",
      tags$img(src = paste0(app$id, ".png"), class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", "→")
        )
      )
    )
  } else {
    tags$a(
      href   = if (!is.null(app$url)) app$url else "#",
      target = "_blank",
      class  = card_class,
      tags$img(src = paste0(app$id, ".png"), class = "card-img"),
      tags$div(
        class = "card-body",
        tags$div(class = "card-title", app$title),
        tags$div(
          class = "card-footer",
          tags$span(class = badge_class, badge_label),
          tags$span(class = "card-arrow", "→")
        )
      )
    )
  }
}

hub_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",

      tags$div(class = "section-label", "Applications"),
      tags$div(class = "app-grid", lapply(apps, make_card)),

      tags$div(class = "section-label", style = "margin-top: 40px;",
               "2025 Cape Cod League Standings"),

      tags$div(
        class = "standings-wrapper",
        if (!is.null(standings)) {
          tagList(
            tags$div(class = "standings-division-label", "East Division"),
            tableOutput("east_standings"),
            tags$div(class = "standings-division-label", "West Division"),
            tableOutput("west_standings")
          )
        } else {
          tags$p("Standings unavailable.", style = "color: var(--text-muted);")
        }
      )
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

catcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button(
          "← Back to Hub",
          onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
          class = "btn btn-outline-secondary btn-sm"
        )
      ),
      tags$h2("Catcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),

      # ── Controls row ──
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",

        tags$div(
          tags$h4("Postgame Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("team_select", "Select Team:", choices = NULL),
          selectInput("game_date_select", "Select Game Date:", choices = NULL),
          selectInput("catcher_name", "Select Catcher:", choices = NULL),
          actionButton("generate_catcher", "Generate Report",
                       class = "btn btn-primary w-100")
        ),

        tags$div(
          tags$h4("Season Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("season_csvs", "Upload Season CSVs:", accept = ".csv", multiple = TRUE,
                    buttonLabel = "Browse", placeholder = "No files selected"),
          selectInput("season_team_select", "Select Team:", choices = NULL)
        )
      ),

      uiOutput("catcher_status"),
      br(),
      uiOutput("catcher_download_ui")
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

pitcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button(
          "← Back to Hub",
          onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
          class = "btn btn-outline-secondary btn-sm"
        )
      ),
      tags$h2("Pitcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),

      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",

        tags$div(
          tags$h4("Game CSV", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("pitcher_game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("pitcher_select", "Select Pitcher:", choices = NULL),
          tags$h4("Manual Overrides", style = "color: var(--navy); margin-top: 16px; margin-bottom: 8px;"),
          tags$p("Leave blank to use Trackman values.",
                 style = "font-size: 0.82rem; color: var(--text-muted); margin-bottom: 10px;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px;",
            numericInput("manual_pitches", "Pitches", value = NA, min = 0),
            numericInput("manual_ks",      "K's",     value = NA, min = 0),
            numericInput("manual_bbs",     "BB's",    value = NA, min = 0)
          ),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 8px;",
            numericInput("manual_hits", "Hits", value = NA, min = 0),
            numericInput("manual_runs", "ER",   value = NA, min = 0)
          )
        ),

        tags$div(
          tags$h4("Season CSVs", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("pitcher_season_csvs", "Upload Season CSVs:", accept = ".csv", multiple = TRUE,
                    buttonLabel = "Browse", placeholder = "No files selected")
        )
      ),

      actionButton("generate_pitcher", "Generate Report",
                   class = "btn btn-primary", style = "width: 200px;"),
      br(), br(),
      uiOutput("pitcher_status"),
      br(),
      uiOutput("pitcher_download_ui")
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}
                       
# ==========================================
# UI
# ==========================================
ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=4")
  ),

  tags$div(
    class = "hub-header",
    tags$div(
      class = "header-text",
      tags$h1("Brewster Whitecaps"),
      tags$p("C.A.P.S. - Centralized Application Platform for Staff")
    ),
    tags$img(src = "logo.png", class = "team-logo")
  ),

  uiOutput("page_content")
)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  current_page <- reactiveVal("hub")

  observeEvent(input$nav_to, {
    current_page(input$nav_to)
  })

  output$page_content <- renderUI({
  if      (current_page() == "hub")     hub_ui()
  else if (current_page() == "catcher") catcher_ui()
  else if (current_page() == "pitcher") pitcher_ui()
})

  # ── Standings ──
  output$east_standings <- renderTable({ standings[[1]] }, striped = TRUE, hover = TRUE)
  output$west_standings <- renderTable({ standings[[2]] }, striped = TRUE, hover = TRUE)

  # ── Game CSV ──
  raw_game <- reactive({
    req(input$game_csv)
    read_csv(input$game_csv$datapath, show_col_types = FALSE)
  })

  observe({
    req(raw_game())
    teams <- sort(unique(raw_game()$CatcherTeam))
    updateSelectInput(session, "team_select", choices = teams)
  })

  game_data <- reactive({
    req(raw_game(), input$team_select)
    prep_catcher_data(raw_game(), input$team_select)
  })

  observe({
    req(game_data())
    dates <- sort(unique(as.Date(game_data()$framing$Date)), decreasing = TRUE)
    updateSelectInput(session, "game_date_select", choices = as.character(dates))
  })

  observe({
    req(game_data())
    catchers <- sort(unique(game_data()$framing$Catcher))
    updateSelectInput(session, "catcher_name", choices = catchers)
  })

  # ── Season CSVs ──
  raw_season <- reactive({
    req(input$season_csvs)
    bind_rows(lapply(input$season_csvs$datapath, function(f) {
      read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
    })) %>% type.convert(as.is = TRUE)
  })

  observe({
    req(raw_season())
    teams <- sort(unique(raw_season()$CatcherTeam))
    updateSelectInput(session, "season_team_select", choices = teams)
  })

  season_data <- reactive({
    req(raw_season(), input$season_team_select)
    prep_catcher_data(raw_season(), input$season_team_select)
  })

  # ── Generate ──
  catcher_pdf_path <- reactiveVal(NULL)

observeEvent(input$generate_catcher, {
  req(input$catcher_name, game_data(), season_data(), input$game_date_select)
  output$catcher_status <- renderUI({
    div(style = "color: orange; font-weight: bold;", "Generating report...")
  })
  tryCatch({
    tmp_pdf   <- tempfile(fileext = ".pdf")
    game_date <- as.Date(input$game_date_select)

    generate_catcher_pdf(
      game_framing    = game_data()$framing  %>% mutate(Date = as.Date(as.character(Date))),
      game_throwing   = game_data()$throwing %>% mutate(Date = as.Date(as.character(Date))),
      season_framing  = season_data()$framing  %>% mutate(Date = as.Date(as.character(Date))),
      season_throwing = season_data()$throwing %>% mutate(Date = as.Date(as.character(Date))),
      catcher     = input$catcher_name,
      game_date   = game_date,
      output_file = tmp_pdf,
      logo_path   = "www/logo.png"
    )

    catcher_pdf_path(tmp_pdf)
    output$catcher_status <- renderUI({
      div(style = "color: green; font-weight: bold;", "\u2713 Report ready!")
    })
  }, error = function(e) {
    output$catcher_status <- renderUI({
      div(style = "color: red;", paste("Error:", e$message))
    })
  })
})

  # ── Download ──
  output$catcher_download_ui <- renderUI({
    req(catcher_pdf_path())
    downloadButton("download_catcher_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_catcher_pdf <- downloadHandler(
    filename = function() {
      paste0(gsub(", ", "_", input$catcher_name), "_CatcherReport.pdf")
    },
    content = function(file) {
      req(catcher_pdf_path())
      file.copy(catcher_pdf_path(), file, overwrite = TRUE)
    }
  )

  # ── Pitcher game CSV ──
raw_pitcher_game <- reactive({
  req(input$pitcher_game_csv)
  read_csv(input$pitcher_game_csv$datapath, show_col_types = FALSE)
})

observe({
  req(raw_pitcher_game())
  pitchers <- sort(unique(raw_pitcher_game()$Pitcher))
  updateSelectInput(session, "pitcher_select", choices = pitchers)
})

# ── Pitcher season CSVs ──
raw_pitcher_season <- reactive({
  req(input$pitcher_season_csvs)
  bind_rows(lapply(input$pitcher_season_csvs$datapath, function(f) {
    read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
      select(-any_of("GameForeignID"))
  })) %>% type.convert(as.is = TRUE)
})

# ── Generate pitcher PDF ──
pitcher_pdf_path <- reactiveVal(NULL)

observeEvent(input$generate_pitcher, {
  req(input$pitcher_select, raw_pitcher_game(), raw_pitcher_season())

  output$pitcher_status <- renderUI({
    div(style = "color: orange; font-weight: bold;", "Generating report...")
  })

  tryCatch({
    tmp_pdf <- tempfile(fileext = ".pdf")

    message("pitcher step A: filtering game data")
    pitcher_data <- raw_pitcher_game() %>%
      filter(Pitcher == input$pitcher_select)
    message("pitcher step B: game data rows: ", nrow(pitcher_data))
    
    pitcher_data <- pitcher_data %>% prep_pitcher_data()
    message("pitcher step C: game data prepped rows: ", nrow(pitcher_data))

    message("pitcher step D: filtering season data")
    pitcher_data_season <- raw_pitcher_season() %>%
      filter(Pitcher == input$pitcher_select)
    message("pitcher step E: season data rows: ", nrow(pitcher_data_season))
    
    pitcher_data_season <- pitcher_data_season %>% prep_pitcher_data()
    message("pitcher step F: season data prepped rows: ", nrow(pitcher_data_season))

    generate_pitcher_pdf(
      pitcher_data        = pitcher_data,
      pitcher_data_season = pitcher_data_season,
      selected_pitcher    = input$pitcher_select,
      manual_pitches      = input$manual_pitches,
      manual_ks           = input$manual_ks,
      manual_bbs          = input$manual_bbs,
      manual_hits         = input$manual_hits,
      manual_runs         = input$manual_runs,
      output_file         = tmp_pdf,
      logo_path           = "www/logo.png"
    )

    pitcher_pdf_path(tmp_pdf)
    output$pitcher_status <- renderUI({
      div(style = "color: green; font-weight: bold;", "\u2713 Report ready!")
    })

  }, error = function(e) {
    message("pitcher ERROR: ", e$message)
    output$pitcher_status <- renderUI({
      div(style = "color: red;", paste("Error:", e$message))
    })
  })
})

# ── Pitcher download ──
output$pitcher_download_ui <- renderUI({
  req(pitcher_pdf_path())
  downloadButton("download_pitcher_pdf", "Download Report",
                 class = "btn btn-success", style = "width: 200px;")
})

output$download_pitcher_pdf <- downloadHandler(
  filename = function() {
    paste0(gsub(", ", "_", input$pitcher_select), "_PitcherReport.pdf")
  },
  content = function(file) {
    req(pitcher_pdf_path())
    file.copy(pitcher_pdf_path(), file, overwrite = TRUE)
  }
)
}

shinyApp(ui = ui, server = server)