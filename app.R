library(shiny)
library(htmltools)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(grid)
library(magick)
library(readr)
library(workflows)
library(parsnip)
library(recipes)
library(tune)
library(xgboost)
library(base64enc)
library(ggridges)

source("scout_app.R")

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
# SHARED HELPERS
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
                            alt_row_bg   = NULL,
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
      bg <- if (!is.null(color_matrix)) {
        color_matrix[r, i]
      } else if (!is.null(alt_row_bg) && r == 2) {
        alt_row_bg
      } else {
        if (r %% 2 == 0) zebra_bg else "white"
      }
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
# CATCHER — HELPER FUNCTIONS
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

generate_catcher_pdf <- function(game_framing, game_throwing, season_framing, season_throwing,
                                  catcher, game_date, output_file, logo_path = NULL) {

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
      count_state = factor(count_state, levels = c("0-0","0-1","0-2","1-0","1-1","1-2",
                                                    "2-0","2-1","2-2","3-0","3-1","3-2")),
      PitchType   = map_pitch_type(TaggedPitchType)
    ) %>%
    filter(
      !is.na(PitchType),
      !is.na(count_state),
      !is.na(PlateLocSide),
      !is.na(PlateLocHeight)
    ) %>%
    mutate(count_state = droplevels(count_state))

  if (nrow(loc_data) == 0) return(NULL)

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
  pitcher_data        <- pitcher_data        %>% mutate(TaggedPitchType_clean = as.character(TaggedPitchType_clean))
  pitcher_data_season <- pitcher_data_season %>% mutate(TaggedPitchType_clean = as.character(TaggedPitchType_clean))
  pitcher_data        <- coerce_numeric(pitcher_data)
  pitcher_data_season <- coerce_numeric(pitcher_data_season)
  message("coercion done")

  counting_stats <- tryCatch({
    pitcher_data %>%
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
  }, error = function(e) {
    message("counting_stats error: ", e$message)
    data.frame(Pitches=0, `K's`=0, `BB's`=0, Hits=0, ER=0,
               `Zone%`="-", `Strike%`="-", `Whiff%`="-", `Hard Hit%`="-", check.names=FALSE)
  })
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

  stuff_game    <- tryCatch(build_stuff_plus(pitcher_data),    error = function(e) { message("stuff_game error: ", e$message); NULL })
  location_game <- tryCatch(build_location_plus(pitcher_data), error = function(e) { message("location_game error: ", e$message); NULL })
  message("step 3: stuff/location done")

  total_pitches_game <- nrow(pitcher_data)
  pitch_specs <- tryCatch({
    pitcher_data %>%
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
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .))) %>%
      { cols <- c("Type", "Usage%",
                  intersect(c("Stuff+", "Location+"), names(.)),
                  setdiff(names(.), c("Type", "Usage%", "Stuff+", "Location+")))
        select(., all_of(cols)) }
  }, error = function(e) { message("pitch_specs error: ", e$message); data.frame() })
  message("step 4: pitch_specs done - rows: ", nrow(pitch_specs))

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

  movement_data <- tryCatch({
    pitcher_data %>%
      group_by(TaggedPitchType_clean) %>%
      summarise(HB   = round(mean(HorzBreak,        na.rm = TRUE), 1),
                iVB  = round(mean(InducedVertBreak,  na.rm = TRUE), 1),
                Velo = round(mean(RelSpeed,          na.rm = TRUE), 1),
                .groups = "drop")
  }, error = function(e) { message("movement_data error: ", e$message); data.frame() })

  p_movement <- tryCatch({
    ggplot() +
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
  }, error = function(e) { message("p_movement error: ", e$message); ggplot() + theme_void() })

  p_velo <- tryCatch({
    ggplot(pitcher_data,
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
  }, error = function(e) { message("p_velo error: ", e$message); ggplot() + theme_void() })

  p_left  <- tryCatch(make_zone_plot_pitcher(pitcher_data, "Left",  "Strike Zone vs. LHH"),
                      error = function(e) { message("p_left error: ", e$message); ggplot() + theme_void() })
  p_right <- tryCatch(make_zone_plot_pitcher(pitcher_data, "Right", "Strike Zone vs. RHH"),
                      error = function(e) { message("p_right error: ", e$message); ggplot() + theme_void() })

  season_totals <- tryCatch({
    pitcher_data_season %>%
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
  }, error = function(e) { message("season_totals error: ", e$message); data.frame() })
  message("step 7: season_totals done")

  movement_data_season <- tryCatch({
    pitcher_data_season %>%
      group_by(TaggedPitchType_clean) %>%
      summarise(HB   = round(mean(HorzBreak,       na.rm = TRUE), 1),
                iVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
                Velo = round(mean(RelSpeed,         na.rm = TRUE), 1),
                .groups = "drop")
  }, error = function(e) { message("movement_data_season error: ", e$message); data.frame() })

  p_movement_season <- tryCatch({
    ggplot() +
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
  }, error = function(e) { message("p_movement_season error: ", e$message); ggplot() + theme_void() })
  message("step 8: season movement done")

  total_pitches_season <- nrow(pitcher_data_season)
  season_pitch_specs <- tryCatch({
    pitcher_data_season %>%
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
  }, error = function(e) { message("season_pitch_specs error: ", e$message); data.frame() })
  message("step 9: season_pitch_specs done - rows: ", nrow(season_pitch_specs))

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

  game_stats_benchmarks <- list(
    `Zone%` = c(42,53), `Strike%` = c(58,67), `Whiff%` = c(15,30), `Hard Hit%` = c(27,44)
  )
  game_stats_color_matrix <- tryCatch({
    row1 <- build_color_matrix_pitcher(
      counting_stats_display[1,,drop=FALSE],
      game_stats_benchmarks,
      lower_is_better = "Hard Hit%"
    )
    row2 <- matrix("white", nrow = 1, ncol = ncol(counting_stats_display))
    rbind(row1, row2)
  }, error = function(e) {
    message("game_stats_color_matrix failed: ", e$message)
    matrix("white", nrow = 2, ncol = ncol(counting_stats_display))
  })

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
    pushViewport(viewport(x = 0.96, y = 0.965, width = 0.07, height = 0.08,
                          just = c("center","center")))
    grid.draw(logo_grob)
    popViewport()

    grid.text("Game Stats", x = 0.5, y = 0.92,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
    draw_grid_table(counting_stats_display,
                    y_top = 0.90, x_center = 0.5, row_h = 0.020,
                    table_width = 0.70, header_cex = 0.80, cell_cex = 0.80,
                    alt_row_bg = "grey80", color_matrix = game_stats_color_matrix)

    pushViewport(viewport(x = 0.27, y = 0.82, width = 0.46, height = 0.23, just = c("center","top")))
    suppressWarnings(print(p_movement, newpage = FALSE))
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.82, width = 0.46, height = 0.23, just = c("center","top")))
    suppressWarnings(print(p_velo, newpage = FALSE))
    popViewport()

    pushViewport(viewport(x = 0.27, y = 0.59, width = 0.46, height = 0.23, just = c("center","top")))
    suppressWarnings(print(p_left, newpage = FALSE))
    popViewport()

    pushViewport(viewport(x = 0.73, y = 0.59, width = 0.46, height = 0.23, just = c("center","top")))
    suppressWarnings(print(p_right, newpage = FALSE))
    popViewport()

    grid.text("Diamond = Hard Hit (95+ mph EV)", x = 0.5, y = 0.355,
              gp = gpar(cex = 0.65, col = "grey40", fontface = "italic"))

    grid.text("Pitch Specifications", x = 0.5, y = 0.33,
              gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
    draw_grid_table(pitch_specs,
                    y_top = 0.31, x_center = 0.5, row_h = 0.024,
                    table_width = 0.94, header_cex = 0.80, cell_cex = 0.80,
                    color_matrix = pitch_specs_color_matrix)

    if (nrow(risp_stats) > 0) {
      grid.text("Pitch Specs - RISP", x = 0.5, y = 0.165,
                gp = gpar(fontface = "bold", cex = 0.90, col = "#0C2340"))
      draw_grid_table(risp_stats,
                      y_top = 0.155, x_center = 0.5, row_h = 0.024,
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
# HITTER — CONSTANTS & HELPERS
# ==========================================
hitter_pitch_colors <- c(
  "Four Seam" = "red",    "Sinker"    = "orange",
  "Slider"    = "gold",   "Curveball" = "blue",
  "Changeup"  = "green3", "Cutter"    = "#8B4513",
  "Splitter"  = "mediumpurple3"
)

hitter_strike_zone <- tibble(
  PlateLocSide   = c(-0.8303, -0.8303, 0.8303, 0.8303, -0.8303),
  PlateLocHeight = c(1.5, 3.3775, 3.3775, 1.5, 1.5)
)

hitter_home_plate <- data.frame(
  x = c(-0.708, 0.708, 0.708, 0, -0.708),
  y = c(0, 0, 0.25, 0.5, 0.25)
)

clean_hitter_pitch_type <- function(pt) {
  case_when(
    pt %in% c("Fastball", "FourSeamFastBall", "Four-Seam") ~ "Four Seam",
    pt %in% c("Sinker", "TwoSeamFastBall")                 ~ "Sinker",
    pt == "Slider"    ~ "Slider",
    pt == "Curveball" ~ "Curveball",
    pt %in% c("ChangeUp", "Changeup") ~ "Changeup",
    pt == "Cutter"    ~ "Cutter",
    pt == "Splitter"  ~ "Splitter",
    TRUE ~ NA_character_
  )
}

build_color_matrix_hitter <- function(df, benchmarks, lower_is_better = c()) {
  get_color <- function(value, bottom, top, flip = FALSE) {
    value <- suppressWarnings(as.numeric(gsub("%", "", value)))
    if (is.na(value)) return("white")
    normalized <- (value - bottom) / (top - bottom)
    normalized <- pmax(0, pmin(1, normalized))
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
      for (r in seq_len(nrow(df)))
        color_matrix[r, i] <- get_color(df[[i]][r], bounds[1], bounds[2], flip)
    }
  }
  color_matrix
}

hitter_season_benchmarks <- list(
  AVG        = c(0.200, 0.360), OBP  = c(0.290, 0.450), SLG  = c(0.339, 0.667),
  `K%`       = c(9.8,  26.5),  `BB%`= c(6.3,  18.8),
  `Whiff%`   = c(12.8, 30.7),  `HardHit%` = c(20, 55)
)
hitter_lower_is_better <- c("K%", "Whiff%")

hitter_fastball_split_benchmarks <- list(
  `Swing%` = c(37.2,52.2), `Contact%` = c(72,90), `Chase%` = c(14,29),
  `Barrel%`= c(6,30),      `IZ Whiff%`= c(6,23),  `HardHit%`= c(18,56), `Avg EV`= c(80,92))
hitter_breaker_split_benchmarks <- list(
  `Swing%` = c(30,48),     `Contact%` = c(56,82), `Chase%` = c(15,35),
  `Barrel%`= c(0,30),      `IZ Whiff%`= c(8,29),  `HardHit%`= c(9,50),  `Avg EV`= c(77,90))
hitter_offspeed_split_benchmarks <- list(
  `Swing%` = c(36,54),     `Contact%` = c(56,81), `Chase%` = c(20,39),
  `Barrel%`= c(4,30),      `IZ Whiff%`= c(10,34), `HardHit%`= c(14,54), `Avg EV`= c(79,91))
hitter_split_lower_is_better <- c("Chase%", "IZ Whiff%")
hitter_split_bench_map <- list(
  "Fastball" = hitter_fastball_split_benchmarks,
  "Breaker"  = hitter_breaker_split_benchmarks,
  "Offspeed" = hitter_offspeed_split_benchmarks)

build_split_color_matrix_hitter <- function(df, bench_map, lower_is_better = c()) {
  color_matrix <- matrix("white", nrow = nrow(df), ncol = ncol(df))
  for (r in seq_len(nrow(df))) {
    pitch_type <- df$`Pitch Type`[r]
    benchmarks <- bench_map[[pitch_type]]
    if (!is.null(benchmarks)) {
      row_colors <- build_color_matrix_hitter(df[r,,drop=FALSE], benchmarks, lower_is_better)
      color_matrix[r, ] <- row_colors[1, ]
    }
  }
  color_matrix
}

# ==========================================
# HITTER — SWING DECISION MODELS
# ==========================================
download_from_hf_dataset <- function(repo_id, filename, token) {
  url  <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filename)
  tmp  <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  resp <- httr::GET(url,
                    httr::add_headers(Authorization = paste("Bearer", token)),
                    httr::write_disk(tmp, overwrite = TRUE),
                    httr::timeout(120))
  if (httr::http_error(resp)) stop("Failed to download ", filename, ": ", httr::status_code(resp))
  tmp
}

message("HITTER_TOKEN present: ", nchar(Sys.getenv("HITTER_TOKEN")) > 0)
sd_models <- tryCatch({
  token <- Sys.getenv("HITTER_TOKEN")
  repo  <- "BrewsterWhitecapsMAC/swing-decision-models"
  message("Downloading swing decision models from HF dataset: ", repo)
  list(
    model_take  = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Take.ubj",  token)),
    model_swing = xgb.load(download_from_hf_dataset(repo, "HitterXRV_Swing.ubj", token)),
    encodings   = readRDS(download_from_hf_dataset(repo,  "encodings.rds",        token))
  )
}, error = function(e) { message("Swing decision models not loaded: ", e$message); NULL })
message("sd_models loaded: ", !is.null(sd_models))

sd_features <- c("PlateLocHeight", "PlateLocSide", "count_state_enc", "pitch_type_enc")

brewster_roster <- c(
  "French, Anderson", "Jenkins, Owen", "Lee, Jacob",
  "Wentz, Dalton", "Lawson, Brendan", "Daniel, Pete", "Partida, Nicholas",
  "Moore, Will", "Craska, Petey", "Laskofski, Jamie", "Penfield, Landon",
  "Rhine, Will", "Lambdin, Jake",
  "Magpoc, Adam", "DeLamielleure, Brody", "Torres, Michael", "Carney, Frankie",
  "Daniel, Conlan", "Kiel II, Terrence", "Abernathy, Jay", "Brown, Blaine",
  "Strayer, Cash"
)

recode_pitch_type_model <- function(x) {
  case_when(
    x %in% c("Fastball","Four-Seam","FourSeamFastBall","FourSeamFastball") ~ "FF",
    x %in% c("Sinker","TwoSeamFastBall","TwoSeamFastball")                 ~ "SI",
    x == "Cutter"                     ~ "FC",
    x %in% c("Curveball","CurveBall") ~ "CU",
    x == "Slider"                     ~ "SL",
    x == "Sweeper"                    ~ "SW",
    x %in% c("ChangeUp","Changeup")   ~ "CH",
    x == "Splitter"                   ~ "FS",
    TRUE ~ "Other"
  )
}

score_pitches_xrv <- function(df, models = sd_models) {
  if (is.null(models)) return(df %>% mutate(xRV_swing=NA_real_, xRV_take=NA_real_, xRV_diff=NA_real_))
  enc <- models$encodings
  scored <- df %>%
    mutate(
      Balls       = as.integer(Balls),
      Strikes     = as.integer(Strikes),
      count_state = paste0(Balls, "-", Strikes),
      count_state = ifelse(!count_state %in% c("0-0","0-1","0-2","1-0","1-1","1-2",
                                               "2-0","2-1","2-2","3-0","3-1","3-2"),
                           NA, count_state),
      pitch_type_model = recode_pitch_type_model(TaggedPitchType),
      count_state_enc  = match(count_state, enc$count_state),
      pitch_type_enc   = match(pitch_type_model, enc$pitch_type)
    )
  valid <- !is.na(scored$PlateLocHeight) & !is.na(scored$PlateLocSide) &
    !is.na(scored$count_state_enc) & !is.na(scored$pitch_type_enc) &
    scored$pitch_type_model != "Other"
  scored$xRV_swing <- NA_real_; scored$xRV_take <- NA_real_
  if (any(valid)) {
    dmat <- xgb.DMatrix(as.matrix(scored[valid, sd_features]))
    scored$xRV_swing[valid] <- predict(models$model_swing, dmat)
    scored$xRV_take[valid]  <- predict(models$model_take,  dmat)
  }
  scored %>% mutate(xRV_diff = xRV_swing - xRV_take) %>%
    select(-pitch_type_model, -count_state_enc, -pitch_type_enc)
}

plot_xrv_diff_heatmap <- function(count_label=NULL, pitch_label="FF", title_prefix="", models=sd_models) {
  if (is.null(models)) return(ggplot() + theme_void())
  enc <- models$encodings
  grid_df <- expand.grid(PlateLocHeight=seq(0.75,4.25,by=0.20), PlateLocSide=seq(-2.0,2.0,by=0.20))
  grid_df$count_state_enc <- if (!is.null(count_label)) match(count_label, enc$count_state) else 1L
  grid_df$pitch_type_enc  <- match(pitch_label, enc$pitch_type)
  if (any(is.na(grid_df$count_state_enc)) || any(is.na(grid_df$pitch_type_enc)))
    return(ggplot() + theme_void())
  dmat <- xgb.DMatrix(as.matrix(grid_df[, sd_features]))
  grid_df$xRV_swing <- predict(models$model_swing, dmat)
  grid_df$xRV_take  <- predict(models$model_take,  dmat)
  grid_df$diff      <- grid_df$xRV_swing - grid_df$xRV_take
  subtitle <- if (!is.null(count_label)) paste0(title_prefix,"Count: ",count_label," | Pitch: ",pitch_label) else paste0(title_prefix,"Pitch: ",pitch_label)
  ggplot(grid_df, aes(x=PlateLocSide, y=PlateLocHeight, fill=diff)) +
    geom_tile() +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, name="Swing\u2212Take\nxRV") +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide, y=PlateLocHeight), inherit.aes=FALSE, color="black", linewidth=1) +
    annotate("rect", xmin=-0.7083, xmax=0.7083, ymin=0.0, ymax=0.15, fill="grey70", color="black") +
    coord_fixed() + xlim(-2.5,2.5) + ylim(0,5) +
    labs(subtitle=subtitle, x=NULL, y=NULL) +
    theme_minimal(base_size=9) +
    theme(plot.subtitle=element_text(hjust=0.5,size=7.5), panel.grid=element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank(),
          legend.key.size=unit(0.35,"cm"), legend.text=element_text(size=6),
          legend.title=element_text(size=6.5))
}

summarise_xrv_swdec <- function(scored_df) {
  scored_df %>% filter(!is.na(xRV_diff)) %>%
    mutate(ModelShouldSwing = xRV_diff > 0,
           DidSwing = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
           GoodxDec = (ModelShouldSwing & DidSwing) | (!ModelShouldSwing & !DidSwing)) %>%
    summarise(Total=n(), GoodDec=sum(GoodxDec,na.rm=TRUE),
              SwingPitches=sum(ModelShouldSwing,na.rm=TRUE),
              ActualSwings=sum(DidSwing,na.rm=TRUE), .groups="drop") %>%
    mutate(`xSwDec%`    = paste0(round(GoodDec      /pmax(Total,1)*100,1),"%"),
           `Mdl Swing%` = paste0(round(SwingPitches /pmax(Total,1)*100,1),"%"),
           `Act Swing%` = paste0(round(ActualSwings /pmax(Total,1)*100,1),"%")) %>%
    select(`xSwDec%`, `Mdl Swing%`, `Act Swing%`)
}

make_swdec_plot <- function(df, plot_title) {
  dec_colors <- c("Good Swing"="#00840D","Good Take"="#5BBF6A","Bad Swing"="#E1463E","Bad Take"="#F4A49E")
  dec_shapes <- c("Good Swing"=17,"Good Take"=21,"Bad Swing"=25,"Bad Take"=21)
  ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="grey85", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1.2) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,shape=DecLabel),
               size=6.5, color="black", alpha=0.3) +
    geom_point(data=df, aes(x=PlateLocSide,y=PlateLocHeight,color=DecLabel,shape=DecLabel),
               size=5.5, alpha=0.92) +
    scale_color_manual(values=dec_colors, name=NULL,
                       guide=guide_legend(override.aes=list(size=3.5))) +
    scale_shape_manual(values=dec_shapes, name=NULL,
                       guide=guide_legend(override.aes=list(size=3.5))) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title,
         subtitle="\u25b2 Swing  \u25cf Take  |  Green = Good  Red = Bad",
         x=NULL, y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color="#0C2340"),
          plot.subtitle=element_text(hjust=0.5,size=7,color="grey50"),
          axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(),
          legend.position="bottom", legend.text=element_text(size=7.5),
          legend.key.size=unit(0.35,"cm"), legend.spacing.x=unit(0.2,"cm"))
}

make_swdec_heatmap <- function(df, plot_title) {
  bin_w <- 0.30
  binned <- df %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    mutate(
      bx = round(PlateLocSide   / bin_w) * bin_w,
      by = round(PlateLocHeight / bin_w) * bin_w
    ) %>%
    group_by(bx, by) %>%
    summarise(GoodPct = mean(SwDec == 1, na.rm=TRUE), N = n(), .groups="drop") %>%
    filter(N >= 2)

  ggplot() +
    geom_tile(data=binned, aes(x=bx, y=by, fill=GoodPct),
              width=bin_w*0.97, height=bin_w*0.97) +
    scale_fill_gradient2(low="blue", mid="white", high="red",
                         midpoint=0.70, limits=c(0,1), guide="none") +
    geom_polygon(data=hitter_home_plate, aes(x=x, y=y),
                 fill="grey85", color="black", linewidth=0.8, inherit.aes=FALSE) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide, y=PlateLocHeight),
              color="black", linewidth=1.2, inherit.aes=FALSE) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() +
    labs(title=plot_title,
         subtitle="Red = Good Decisions  |  Blue = Bad Decisions",
         x=NULL, y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(hjust=0.5,face="bold",size=11,color="#0C2340"),
          plot.subtitle=element_text(hjust=0.5,size=7,color="grey50"),
          axis.text=element_blank(), axis.ticks=element_blank(),
          panel.grid=element_blank(), legend.position="none")
}

# ==========================================
# HITTER — PDF GENERATION
# ==========================================
generate_hitter_pdf <- function(game_data, season_data, selected_hitter, output_file,
                                 active_models = sd_models) {

  hitter_name <- format_name(selected_hitter)

  dedup <- function(df) {
    key_cols <- intersect(c("GameID","Batter","Inning","Balls","Strikes","Outs",
                            "PitchCall","TaggedPitchType","PlateLocHeight","PlateLocSide"), names(df))
    df %>% distinct(across(all_of(key_cols)), .keep_all = TRUE)
  }

  game_hitter   <- game_data   %>% filter(Batter == selected_hitter) %>% dedup() %>% score_pitches_xrv(models=active_models)
  season_hitter <- season_data %>% filter(Batter == selected_hitter) %>% dedup() %>% score_pitches_xrv(models=active_models)

  logo_grob <- tryCatch({
    img <- magick::image_read("www/logo1.png")
    img <- magick::image_resize(img, "x100")
    grid::rasterGrob(as.raster(img), interpolate=TRUE)
  }, error=function(e) grid::nullGrob())

  # ── Game Stats ───────────────────────────────────────────────────────────────
  counting_stats <- game_hitter %>%
    mutate(
      IsWhiff  = PitchCall == "StrikeSwinging",
      IsSwing  = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsChase  = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip") &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsBall   = PitchCall == "BallCalled" &
        (abs(PlateLocSide) > 0.8303 | PlateLocHeight < 1.5 | PlateLocHeight > 3.3775),
      IsLast   = if ("is_last_pitch_of_PA" %in% names(.)) is_last_pitch_of_PA == TRUE else
        PitchCall %in% c("InPlay","HitByPitch")
    ) %>%
    summarise(
      PA       = sum(IsLast, na.rm=TRUE),
      Hits     = sum(IsLast & PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm=TRUE),
      `K's`    = sum(IsLast & PitchCall == "StrikeSwinging" & Strikes == 2, na.rm=TRUE) +
        sum(IsLast & PitchCall == "StrikeCalled"   & Strikes == 2, na.rm=TRUE),
      `BB's`   = sum(IsLast & PitchCall == "BallCalled" & Balls == 3, na.rm=TRUE),
      `2B`     = sum(IsLast & PlayResult == "Double", na.rm=TRUE),
      `3B`     = sum(IsLast & PlayResult == "Triple", na.rm=TRUE),
      HR       = sum(IsLast & PlayResult == "HomeRun", na.rm=TRUE),
      `Whiff%` = paste0(round(sum(IsWhiff,na.rm=TRUE)/pmax(sum(IsSwing,na.rm=TRUE),1)*100,1),"%"),
      `Chase%` = paste0(round(sum(IsChase,na.rm=TRUE)/pmax(sum(IsBall+IsChase,na.rm=TRUE),1)*100,1),"%")
    )

  swing_decisions <- game_hitter %>%
    mutate(
      InZone   = PlateLocHeight>=1.5 & PlateLocHeight<=3.3775 & abs(PlateLocSide)<=0.8303,
      DidSwing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","FoulTip","InPlay"),
      SwDec = case_when(
        InZone  & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 1,
        !InZone & PitchCall == "BallCalled"                                                     ~ 1,
        !InZone & PitchCall == "StrikeCalled" & Strikes < 2                                     ~ 1,
        PitchCall == "HitByPitch"                                                               ~ 1,
        InZone  & PitchCall == "StrikeCalled"                                                   ~ 0,
        !InZone & PitchCall == "StrikeCalled" & Strikes == 2                                    ~ 0,
        !InZone & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 0,
        TRUE ~ NA_real_),
      DecLabel   = case_when(
        SwDec==1 & DidSwing  ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing  ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_),
      CountLabel = paste0(Balls,"-",Strikes)
    ) %>%
    filter(!is.na(SwDec), !is.na(PlateLocHeight), !is.na(PlateLocSide))

  overall_swdec <- swing_decisions %>%
    summarise(Good=sum(SwDec==1), Total=n()) %>%
    mutate(` `="Overall", SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(` `, SwDec, `SwDec%`)

  swdec_by_pitch <- swing_decisions %>%
    mutate(PitchTypeGroup=case_when(
      TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
      TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
      TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(PitchTypeGroup)) %>%
    group_by(PitchTypeGroup) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(`Pitch Type`=PitchTypeGroup, SwDec, `SwDec%`)

  swdec_by_count <- swing_decisions %>%
    mutate(CountType=case_when(
      Balls-Strikes>=1 ~ "Hitter Ahead", Strikes-Balls>=1 ~ "Pitcher Ahead",
      Balls==Strikes   ~ "Even",         TRUE             ~ "Neutral")) %>%
    group_by(CountType) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(Count=CountType, SwDec, `SwDec%`)

  game_swdec_plot <- make_swdec_plot(swing_decisions, "Game Swing Decisions by Location")

  game_xrv_overall <- if (!is.null(active_models) && any(!is.na(game_hitter$xRV_diff))) {
    summarise_xrv_swdec(game_hitter) %>% mutate(` `="Overall") %>% select(` `, everything())
  } else NULL

  game_xrv_by_pitch <- if (!is.null(active_models) && any(!is.na(game_hitter$xRV_diff))) {
    game_hitter %>%
      mutate(PitchTypeGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchTypeGroup)) %>%
      group_by(`Pitch Type`=PitchTypeGroup) %>%
      group_modify(~summarise_xrv_swdec(.x)) %>% ungroup()
  } else NULL

  stats_by_pitch <- game_hitter %>%
    mutate(
      TaggedPitchType_clean = clean_hitter_pitch_type(TaggedPitchType),
      IsSwing     = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsWhiff     = PitchCall == "StrikeSwinging",
      InZone      = abs(PlateLocSide)<=0.8303 & PlateLocHeight>=1.5 & PlateLocHeight<=3.3775,
      IsZoneWhiff = IsWhiff & InZone, IsZoneSwing = IsSwing & InZone,
      IsChase     = IsSwing & !InZone, IsOutZone   = !InZone,
      IsHardHit   = PitchCall=="InPlay" & !is.na(ExitSpeed) & ExitSpeed>=95
    ) %>%
    filter(!is.na(TaggedPitchType_clean)) %>%
    group_by(TaggedPitchType_clean) %>%
    summarise(
      `#`         = n(),
      `Swing%`    = round(sum(IsSwing)/n()*100,1),
      `Whiff%`    = round(ifelse(sum(IsSwing)==0,NA,sum(IsWhiff)/sum(IsSwing)*100),1),
      `IZ Whiff%` = round(ifelse(sum(IsZoneSwing)==0,NA,sum(IsZoneWhiff)/sum(IsZoneSwing)*100),1),
      `Chase%`    = round(ifelse(sum(IsOutZone)==0,NA,sum(IsChase)/sum(IsOutZone)*100),1),
      `Hard Hit%` = round(ifelse(sum(!is.na(ExitSpeed))==0,NA,sum(IsHardHit)/sum(!is.na(ExitSpeed))*100),1),
      `Avg EV`    = round(mean(ExitSpeed,na.rm=TRUE),1),
      `Avg LA`    = round(mean(Angle,na.rm=TRUE),1),
      .groups="drop") %>%
    rename(Type=TaggedPitchType_clean) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.),NA,.)))

  contact_shapes <- c("Take"=21,"Whiff"=4,"In Play"=22,"Hard Hit"=23)

  zone_hitter <- game_hitter %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    mutate(TaggedPitchType_clean=clean_hitter_pitch_type(TaggedPitchType),
           ContactType=case_when(
             ExitSpeed>=95                                ~ "Hard Hit",
             PitchCall=="StrikeSwinging"                 ~ "Whiff",
             PitchCall %in% c("StrikeCalled","BallCalled") ~ "Take",
             PitchCall=="InPlay"                         ~ "In Play",
             TRUE ~ "Take")) %>%
    filter(!is.na(TaggedPitchType_clean))

  zone_plot <- ggplot() +
    geom_polygon(data=hitter_home_plate, aes(x=x,y=y), fill="white", color="black", linewidth=0.8) +
    geom_path(data=hitter_strike_zone, aes(x=PlateLocSide,y=PlateLocHeight), color="black", linewidth=1) +
    geom_point(data=zone_hitter,
               aes(x=PlateLocSide,y=PlateLocHeight,fill=TaggedPitchType_clean,shape=ContactType),
               size=3, alpha=0.90, color="black", stroke=0.8) +
    scale_fill_manual(values=hitter_pitch_colors, drop=TRUE) +
    scale_shape_manual(values=contact_shapes, drop=FALSE) +
    facet_wrap(~TaggedPitchType_clean, nrow=1) +
    labs(title="Strike Zone by Pitch Type", x=NULL, y=NULL) +
    xlim(-2.5,2.5) + ylim(0,5) + coord_fixed() + theme_minimal() +
    theme(plot.title=element_text(hjust=0.5,size=10,face="bold"),
          strip.text=element_text(size=8,face="bold"),
          axis.text=element_blank(), axis.ticks=element_blank(),
          panel.grid=element_blank(), legend.position="bottom",
          legend.title=element_text(size=8,face="bold"),
          legend.text=element_text(size=8)) +
    guides(fill="none", shape=guide_legend(title="Contact Type"))

  # ── Season Stats ─────────────────────────────────────────────────────────────
  season_stats <- season_hitter %>%
    mutate(
      IsSwing    = PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
      IsWhiff    = PitchCall=="StrikeSwinging",
      IsHardHit  = PitchCall=="InPlay" & !is.na(ExitSpeed) & ExitSpeed>=95,
      IsBIP      = PitchCall=="InPlay" & !is.na(ExitSpeed),
      IsHit      = PlayResult %in% c("Single","Double","Triple","HomeRun"),
      Is2B=PlayResult=="Double", Is3B=PlayResult=="Triple", IsHR=PlayResult=="HomeRun",
      IsK=KorBB=="Strikeout", IsBB=KorBB=="Walk", IsHBP=PitchCall=="HitByPitch",
      IsLastPitch = if ("is_last_pitch_of_PA" %in% names(.)) is_last_pitch_of_PA==TRUE else
        (KorBB %in% c("Strikeout","Walk") | PitchCall %in% c("InPlay","HitByPitch")),
      IsPA = IsLastPitch & (KorBB %in% c("Strikeout","Walk") | PitchCall %in% c("InPlay","HitByPitch")),
      IsAB = IsPA & !IsBB & !IsHBP & !PlayResult %in% c("Sacrifice","SacrificeFly")
    ) %>%
    summarise(
      PA=sum(IsPA,na.rm=TRUE), AB=sum(IsAB,na.rm=TRUE),
      H=sum(IsHit & IsLastPitch,na.rm=TRUE), `2B`=sum(Is2B & IsLastPitch,na.rm=TRUE),
      `3B`=sum(Is3B & IsLastPitch,na.rm=TRUE), HR=sum(IsHR & IsLastPitch,na.rm=TRUE),
      BB=sum(IsBB & IsLastPitch,na.rm=TRUE), K=sum(IsK & IsLastPitch,na.rm=TRUE),
      HBP=sum(IsHBP,na.rm=TRUE), Swings=sum(IsSwing,na.rm=TRUE),
      Whiffs=sum(IsWhiff,na.rm=TRUE), BIP=sum(IsBIP,na.rm=TRUE), HH=sum(IsHardHit,na.rm=TRUE)
    ) %>%
    mutate(
      AVG=sprintf("%.3f",round(H/pmax(AB,1),3)),
      OBP=sprintf("%.3f",round((H+BB+HBP)/pmax(AB+BB+HBP,1),3)),
      SLG=sprintf("%.3f",round((H+`2B`+2*`3B`+3*HR)/pmax(AB,1),3)),
      `K%`=paste0(round(K/pmax(PA,1)*100,1),"%"),
      `BB%`=paste0(round(BB/pmax(PA,1)*100,1),"%"),
      `Whiff%`=paste0(round(Whiffs/pmax(Swings,1)*100,1),"%"),
      `HardHit%`=paste0(round(HH/pmax(BIP,1)*100,1),"%")
    ) %>%
    select(AVG,OBP,SLG,`K%`,`BB%`,`Whiff%`,`HardHit%`)

  season_color_matrix <- build_color_matrix_hitter(season_stats, hitter_season_benchmarks, hitter_lower_is_better)

  # ── Season Swing Decisions ────────────────────────────────────────────────────
  season_swing_decisions <- season_hitter %>%
    mutate(
      InZone   = PlateLocHeight>=1.5 & PlateLocHeight<=3.3775 & abs(PlateLocSide)<=0.8303,
      DidSwing = PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","FoulTip","InPlay"),
      SwDec=case_when(
        InZone  & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 1,
        !InZone & PitchCall=="BallCalled"                                                       ~ 1,
        !InZone & PitchCall=="StrikeCalled" & Strikes<2                                         ~ 1,
        PitchCall=="HitByPitch"                                                                 ~ 1,
        InZone  & PitchCall=="StrikeCalled"                                                     ~ 0,
        !InZone & PitchCall=="StrikeCalled" & Strikes==2                                        ~ 0,
        !InZone & PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay") ~ 0,
        TRUE ~ NA_real_),
      DecLabel=case_when(
        SwDec==1 & DidSwing  ~ "Good Swing", SwDec==1 & !DidSwing ~ "Good Take",
        SwDec==0 & DidSwing  ~ "Bad Swing",  SwDec==0 & !DidSwing ~ "Bad Take",
        TRUE ~ NA_character_),
      CountLabel=paste0(Balls,"-",Strikes)
    ) %>%
    filter(!is.na(SwDec), !is.na(PlateLocHeight), !is.na(PlateLocSide))

  season_overall_swdec <- season_swing_decisions %>%
    summarise(Good=sum(SwDec==1), Total=n()) %>%
    mutate(` `="Overall", SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(` `, SwDec, `SwDec%`) %>%
    bind_rows(data.frame(` `="League Avg", SwDec="-", `SwDec%`=73.0, check.names=FALSE))

  season_swdec_by_pitch <- season_swing_decisions %>%
    mutate(PitchTypeGroup=case_when(
      TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
      TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
      TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(PitchTypeGroup)) %>%
    group_by(PitchTypeGroup) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(`Pitch Type`=PitchTypeGroup, SwDec, `SwDec%`)

  season_swdec_by_count <- season_swing_decisions %>%
    mutate(CountType=case_when(
      Balls-Strikes>=1 ~ "Hitter Ahead", Strikes-Balls>=1 ~ "Pitcher Ahead",
      Balls==Strikes ~ "Even", TRUE ~ "Neutral")) %>%
    group_by(CountType) %>%
    summarise(Good=sum(SwDec), Total=n(), .groups="drop") %>%
    mutate(SwDec=paste0(Good," / ",Total), `SwDec%`=round(Good/Total*100,1)) %>%
    select(Count=CountType, SwDec, `SwDec%`)

  season_swdec_plot <- make_swdec_heatmap(season_swing_decisions, "Season Swing Decisions by Location")

  season_xrv_overall <- if (!is.null(active_models) && any(!is.na(season_hitter$xRV_diff))) {
    summarise_xrv_swdec(season_hitter) %>% mutate(` `="Overall") %>% select(` `, everything())
  } else NULL

  season_xrv_by_pitch <- if (!is.null(active_models) && any(!is.na(season_hitter$xRV_diff))) {
    season_hitter %>%
      mutate(PitchTypeGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaking Ball",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchTypeGroup)) %>%
      group_by(`Pitch Type`=PitchTypeGroup) %>%
      group_modify(~summarise_xrv_swdec(.x)) %>% ungroup()
  } else NULL

  # ── RHP/LHP Stats ────────────────────────────────────────────────────────────
  make_split_stats_hitter <- function(data, hand) {
    data %>%
      filter(Batter==selected_hitter, PitcherThrows==hand) %>%
      mutate(
        PitchGroup=case_when(
          TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
          TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaker",
          TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
          TRUE ~ NA_character_),
        IsSwing=PitchCall %in% c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
        IsContact=PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulTip","InPlay"),
        IsOutZone=!is.na(PlateLocSide)&!is.na(PlateLocHeight)&
          (abs(PlateLocSide)>0.8303|PlateLocHeight<1.5|PlateLocHeight>3.3775),
        IsChase=IsSwing&IsOutZone,
        InZone=!is.na(PlateLocSide)&!is.na(PlateLocHeight)&
          abs(PlateLocSide)<=0.8303&PlateLocHeight>=1.5&PlateLocHeight<=3.3775,
        IsWhiff=PitchCall=="StrikeSwinging",
        IsZoneSwing=IsSwing&InZone, IsZoneWhiff=IsWhiff&InZone,
        IsHardHit=PitchCall=="InPlay"&!is.na(ExitSpeed)&ExitSpeed>=95,
        IsBarrel=PitchCall=="InPlay"&!is.na(ExitSpeed)&!is.na(Angle)&ExitSpeed>=95&Angle>=10&Angle<=35,
        IsBIP=PitchCall=="InPlay"&!is.na(ExitSpeed)
      ) %>%
      filter(!is.na(PitchGroup)) %>%
      group_by(`Pitch Type`=PitchGroup) %>%
      summarise(
        `Swing%`   =paste0(round(sum(IsSwing,na.rm=TRUE)/n()*100,1),"%"),
        `Contact%` =paste0(round(ifelse(sum(IsSwing,na.rm=TRUE)==0,NA,sum(IsContact,na.rm=TRUE)/sum(IsSwing,na.rm=TRUE)*100),1),"%"),
        `Chase%`   =paste0(round(ifelse(sum(IsOutZone,na.rm=TRUE)==0,0,sum(IsChase,na.rm=TRUE)/sum(IsOutZone,na.rm=TRUE)*100),1),"%"),
        `Barrel%`  =paste0(round(ifelse(sum(IsBIP,na.rm=TRUE)==0,NA,sum(IsBarrel,na.rm=TRUE)/sum(IsBIP,na.rm=TRUE)*100),1),"%"),
        `IZ Whiff%`=paste0(round(ifelse(sum(IsZoneSwing,na.rm=TRUE)==0,NA,sum(IsZoneWhiff,na.rm=TRUE)/sum(IsZoneSwing,na.rm=TRUE)*100),1),"%"),
        `HardHit%` =paste0(round(ifelse(sum(IsBIP,na.rm=TRUE)==0,NA,sum(IsHardHit,na.rm=TRUE)/sum(IsBIP,na.rm=TRUE)*100),1),"%"),
        `Avg EV`   =round(mean(ExitSpeed[IsBIP],na.rm=TRUE),1),
        `Avg LA`   =round(mean(Angle[IsBIP],na.rm=TRUE),1),
        .groups="drop") %>%
      arrange(factor(`Pitch Type`,levels=c("Fastball","Breaker","Offspeed"))) %>%
      mutate(across(everything(),~ifelse(is.na(.)|.=="NA%","-",as.character(.))))
  }

  rhp_stats <- make_split_stats_hitter(season_data, "Right")
  lhp_stats <- make_split_stats_hitter(season_data, "Left")
  rhp_color_matrix <- build_split_color_matrix_hitter(rhp_stats, hitter_split_bench_map, hitter_split_lower_is_better)
  lhp_color_matrix <- build_split_color_matrix_hitter(lhp_stats, hitter_split_bench_map, hitter_split_lower_is_better)

  # ── Density Plots ─────────────────────────────────────────────────────────────
  make_density_plots_hitter <- function(data, hand) {
    split_data <- data %>%
      filter(Batter==selected_hitter, PitcherThrows==hand) %>%
      mutate(PitchGroup=case_when(
        TaggedPitchType %in% c("FourSeamFastBall","Fastball","Four-Seam","Sinker","TwoSeamFastBall","Cutter") ~ "Fastball",
        TaggedPitchType %in% c("Curveball","Slider","Sweeper","Slurve")                                      ~ "Breaker",
        TaggedPitchType %in% c("Changeup","Splitter","ChangeUp")                                             ~ "Offspeed",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(PitchGroup), !is.na(PlateLocSide), !is.na(PlateLocHeight))
    group_levels <- c("Fastball","Breaker","Offspeed")
    plots <- lapply(group_levels, function(grp) {
      grp_data   <- split_data %>% filter(PitchGroup==grp)
      whiff_data <- grp_data  %>% filter(PitchCall=="StrikeSwinging")
      hh_data    <- grp_data  %>% filter(PitchCall=="InPlay",!is.na(ExitSpeed),ExitSpeed>=95)
      if (nrow(grp_data)<1)
        return(ggplot()+theme_void()+labs(title=grp)+
                 theme(plot.title=element_text(hjust=0.5,size=13,face="bold")))
      p <- ggplot(grp_data, aes(x=PlateLocSide,y=PlateLocHeight)) +
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE,interpolate=TRUE) +
        scale_fill_gradient(low="lightblue",high="red") +
        geom_path(data=hitter_strike_zone,aes(x=PlateLocSide,y=PlateLocHeight),color="black",linewidth=1,inherit.aes=FALSE) +
        geom_polygon(data=hitter_home_plate,aes(x=x,y=y),fill="white",color="black",linewidth=0.8,inherit.aes=FALSE)
      if (nrow(whiff_data)>0)
        p <- p+geom_point(data=whiff_data,aes(x=PlateLocSide,y=PlateLocHeight),shape=4,size=2.5,color="black",stroke=1,inherit.aes=FALSE)
      if (nrow(hh_data)>0)
        p <- p+geom_point(data=hh_data,aes(x=PlateLocSide,y=PlateLocHeight),shape=23,size=2.5,color="white",fill=NA,stroke=1,inherit.aes=FALSE)
      p+xlim(-2.5,2.5)+ylim(0,5)+coord_fixed()+labs(title=grp)+theme_minimal()+
        theme(legend.position="none",plot.title=element_text(hjust=0.5,size=13,face="bold"),
              axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid=element_blank())
    })
    setNames(plots, group_levels)
  }

  density_rhp <- make_density_plots_hitter(season_data, "Right")
  density_lhp <- make_density_plots_hitter(season_data, "Left")

  # ── Draw PDF ──────────────────────────────────────────────────────────────────
  pdf(output_file, width=11, height=15)
  on.exit(try(dev.off(), silent=TRUE), add=TRUE)

  page_header_hitter <- function(name, subtitle) {
    grid.rect(x=0,y=1,width=1,height=0.06,just=c("left","top"),gp=gpar(fill="#0C2340",col=NA))
    grid.text(name,     x=0.03,y=0.978,just="left",gp=gpar(col="white",   fontface="bold",cex=1.2))
    grid.text(subtitle, x=0.03,y=0.948,just="left",gp=gpar(col="#9DC2EA", cex=1.0))
    pushViewport(viewport(x=0.96,y=0.965,width=0.07,height=0.08,just=c("center","center")))
    grid.draw(logo_grob); popViewport()
  }

  page_footer_hitter <- function() {
    grid.text("Data: TrackMan | Brewster Whitecaps Analytics",
              x=0.5,y=0.02,gp=gpar(cex=0.55,col="#0C2340",fontface="italic"))
  }

  tryCatch({

    # PAGE 1 — GAME
    grid.newpage()
    page_header_hitter(hitter_name, "Postgame Hitter Report")

    grid.text("Game Stats", x=0.5,y=0.89,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(counting_stats, y_top=0.865, x_center=0.5, row_h=0.018, cell_cex=0.90)

    pushViewport(viewport(x=0.5,y=0.82,width=0.96,height=0.30,just=c("center","top")))
    print(zone_plot, newpage=FALSE); popViewport()

    draw_grid_table(stats_by_pitch, title="Stats by Pitch Type",
                    y_top=0.490, x_center=0.5, row_h=0.026, title_cex=0.90, header_cex=0.80, cell_cex=0.80)

    grid.lines(x=c(0.03,0.97), y=c(0.300,0.300), gp=gpar(col="#9DC2EA", lwd=1))
    grid.text("Swing Decisions", x=0.5, y=0.290,
              gp=gpar(fontface="bold", cex=0.90, col="#0C2340"))

    grid.text("Game", x=0.25, y=0.290,
              gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
    pushViewport(viewport(x=0.12, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(game_swdec_plot +
            theme(plot.title=element_blank(), plot.subtitle=element_blank(),
                  legend.position="none"),
          newpage=FALSE); popViewport()

    draw_grid_table(overall_swdec,  title="Overall",
                    y_top=0.260, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    draw_grid_table(swdec_by_pitch, title="By Pitch",
                    y_top=0.180, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    if (!is.null(active_models) && !is.null(game_xrv_overall))
      draw_grid_table(game_xrv_overall, title="xRV",
                      y_top=0.075, x_center=0.34, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)

    grid.text("Season", x=0.75, y=0.290,
              gp=gpar(fontface="bold", cex=0.75, col="#0C2340"))
    pushViewport(viewport(x=0.62, y=0.312, width=0.22, height=0.250, just=c("center","top")))
    print(season_swdec_plot +
            theme(plot.title=element_blank(), plot.subtitle=element_blank()),
          newpage=FALSE); popViewport()

    draw_grid_table(season_overall_swdec, title="Overall",
                    y_top=0.260, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60,
                    title_cex=0.82, alt_row_bg="grey80")
    draw_grid_table(season_swdec_by_pitch, title="By Pitch",
                    y_top=0.180, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)
    if (!is.null(active_models) && !is.null(season_xrv_overall))
      draw_grid_table(season_xrv_overall, title="xRV",
                      y_top=0.075, x_center=0.84, row_h=0.016, table_width=0.20, cell_cex=0.60, title_cex=0.82)

    page_footer_hitter()

    # PAGE 2 — SEASON
    grid.newpage()
    page_header_hitter(hitter_name, "2026 Season Report")

    draw_grid_table(season_stats, title="Season Stats",
                    y_top=0.900, x_center=0.5, row_h=0.020, table_width=0.65,
                    header_cex=0.80, cell_cex=0.80, title_cex=0.90, color_matrix=season_color_matrix)

    grid.text("Location Density vs. RHP", x=0.5,y=0.83,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    grid.text("X = Whiff  |  Diamond = Hard Hit (95+ EV)", x=0.5,y=0.695,gp=gpar(cex=0.58,col="grey40",fontface="italic"))
    pushViewport(viewport(x=0.17,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.81,width=0.30,height=0.20,just=c("center","top")))
    print(density_rhp[["Offspeed"]],newpage=FALSE); popViewport()

    grid.text("Stats vs. RHP by Pitch Type", x=0.5,y=0.60,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(rhp_stats, y_top=0.59, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=rhp_color_matrix)

    grid.text("Location Density vs. LHP", x=0.5,y=0.50,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    pushViewport(viewport(x=0.17,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Fastball"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.50,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Breaker"]],newpage=FALSE); popViewport()
    pushViewport(viewport(x=0.83,y=0.49,width=0.28,height=0.20,just=c("center","top")))
    print(density_lhp[["Offspeed"]],newpage=FALSE); popViewport()

    grid.text("Stats vs. LHP by Pitch Type", x=0.5,y=0.28,gp=gpar(fontface="bold",cex=1,col="#0C2340"))
    draw_grid_table(lhp_stats, y_top=0.27, x_center=0.5, row_h=0.015,
                    table_width=0.90, header_cex=0.62, cell_cex=0.75, color_matrix=lhp_color_matrix)

    page_footer_hitter()

  }, error=function(e) message("PDF error: ", e$message))
}

# ==========================================
# HUB UI
# ==========================================
apps <- list(
  list(id = "catcher",          title = "Catcher Reports",          page = "catcher",        status = "live"),
  list(id = "hitter",           title = "Postgame Hitter Reports",  page = "hitter",         status = "live"),
  list(id = "pitcher",          title = "Postgame Pitcher Reports", page = "pitcher",        status = "live"),
  list(id = "pitcher_scouting", title = "Pitcher Scouting",         page = "scout_pitching", status = "live"),
  list(id = "hitter_scouting",  title = "Hitter Scouting",          page = "scout_hitting",  status = "live"),
  list(id = "acquisitions",     title = "Acquisitions",             page = "scout_acq",      status = "live"),
  list(id = "player_grades",    title = "Player Grades",            page = "scout_grades",   status = "live"),
  list(id = "umpire",           title = "Umpire Reports",           page = NULL,             status = "live")
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
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
      ),
      tags$h2("Catcher Report Generator",
              style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          tags$h4("Postgame Report", style = "color: var(--navy); margin-bottom: 12px;"),
          fileInput("game_csv", "Upload Game CSV:", accept = ".csv",
                    buttonLabel = "Browse", placeholder = "No file selected"),
          selectInput("team_select", "Select Team:", choices = NULL),
          selectInput("game_date_select", "Select Game Date:", choices = NULL),
          selectInput("catcher_name", "Select Catcher:", choices = NULL),
          actionButton("generate_catcher", "Generate Report", class = "btn btn-primary w-100")
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
    tags$div(class = "hub-footer",
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

hitter_ui <- function() {
  tagList(
    tags$div(class="hub-main",
      tags$div(style="margin-bottom: 24px;",
        tags$button("← Back to Hub", onclick="Shiny.setInputValue('nav_to','hub',{priority:'event'})", class="btn btn-outline-secondary btn-sm")),
      tags$h2("Hitter Report Generator", style="font-family: var(--font-head); color: var(--navy); margin-bottom: 24px;"),
      tags$div(style="display: grid; grid-template-columns: 1fr 1fr; gap: 32px; margin-bottom: 32px;",
        tags$div(
          tags$h4("Game CSV", style="color: var(--navy); margin-bottom: 12px;"),
          fileInput("hitter_game_csv", "Upload Game CSV:", accept=".csv", buttonLabel="Browse", placeholder="No file selected"),
          tags$h4("Season CSVs", style="color: var(--navy); margin-bottom: 12px;"),
          fileInput("hitter_season_csvs", "Upload Season CSVs:", accept=".csv", multiple=TRUE, buttonLabel="Browse", placeholder="No files selected")),
        tags$div(
          tags$h4("Select Player", style="color: var(--navy); margin-bottom: 12px;"),
          uiOutput("hitter_team_select_ui"),
          uiOutput("hitter_select_ui"))
      ),
      actionButton("generate_hitter", "Generate Report", class="btn btn-primary", style="width: 200px;"),
      br(), br(), uiOutput("hitter_status"), br(), uiOutput("hitter_download_ui")),
    tags$div(class="hub-footer", paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
  )
}

pitcher_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$div(
        style = "margin-bottom: 24px;",
        tags$button("← Back to Hub",
                    onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
                    class = "btn btn-outline-secondary btn-sm")
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
    tags$div(class = "hub-footer",
             paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y")))
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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=7"),
    tags$style(HTML("
      #caps-splash {
        position: fixed;
        inset: 0;
        background: #0C2340;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        transition: opacity 0.6s ease;
      }
      #caps-splash.fade-out {
        opacity: 0;
        pointer-events: none;
      }
      #splash-logo {
        width: 100px;
        height: 100px;
        border-radius: 50%;
        background: white;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: scaleIn 1.2s ease forwards;
        opacity: 0;
        overflow: hidden;
      }
      #splash-logo img {
        width: 90px;
        height: 90px;
        object-fit: contain;
      }
      #splash-title {
        color: white;
        font-family: 'Oswald', sans-serif;
        font-size: 28px;
        letter-spacing: 6px;
        margin-top: 20px;
        animation: fadeUp 1s ease 0.8s forwards;
        opacity: 0;
        text-transform: uppercase;
      }
      #splash-sub {
        color: #9DC2EA;
        font-size: 12px;
        letter-spacing: 2px;
        margin-top: 8px;
        animation: fadeUp 1s ease 1.2s forwards;
        opacity: 0;
        text-align: center;
      }
      @keyframes scaleIn {
        0%   { transform: scale(0.3); opacity: 0; }
        70%  { transform: scale(1.1); opacity: 1; }
        100% { transform: scale(1);   opacity: 1; }
      }
      @keyframes fadeUp {
        from { transform: translateY(12px); opacity: 0; }
        to   { transform: translateY(0);    opacity: 1; }
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        setTimeout(function() {
          $('#caps-splash').addClass('fade-out');
          setTimeout(function() {
            $('#caps-splash').remove();
          }, 700);
        }, 2800);
      });
    "))
  ),

  tags$div(
    id = "caps-splash",
    tags$div(
      id = "splash-logo",
      tags$img(src = "logo1.png")
    ),
    tags$div(id = "splash-title", "C.A.P.S."),
    tags$div(id = "splash-sub", "Centralized Application Platform for Staff")
  ),

  tags$div(
    class = "hub-header",
    tags$div(
      class = "header-text",
      tags$h1("Brewster Whitecaps"),
      tags$p("C.A.P.S. - Centralized Application Platform for Staff")
    ),
    tags$img(src = "logo1.png", class = "team-logo")
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

  scout_server(input, output, session)

  output$page_content <- renderUI({
    if      (current_page() == "hub")            hub_ui()
    else if (current_page() == "catcher")        catcher_ui()
    else if (current_page() == "hitter")         hitter_ui()
    else if (current_page() == "pitcher")        pitcher_ui()
    else if (current_page() == "scout_pitching") scout_pitching_ui()
    else if (current_page() == "scout_hitting")  scout_hitting_ui()
    else if (current_page() == "scout_acq")      scout_acq_ui()
    else if (current_page() == "scout_grades")   scout_grades_ui()
  })

  # ── Standings ──────────────────────────────────────────────────────────────
  output$east_standings <- renderTable({ standings[[1]] }, striped = TRUE, hover = TRUE)
  output$west_standings <- renderTable({ standings[[2]] }, striped = TRUE, hover = TRUE)

  # ==========================================
  # CATCHER SERVER LOGIC
  # ==========================================
  raw_game <- reactive({
    req(input$game_csv)
    read_csv(input$game_csv$datapath, show_col_types = FALSE)
  })

  observe({
    req(raw_game())
    teams <- sort(unique(raw_game()$CatcherTeam))
    updateSelectInput(session, "team_select", choices = teams)
  })

  game_data_catcher <- reactive({
    req(raw_game(), input$team_select)
    prep_catcher_data(raw_game(), input$team_select)
  })

  observe({
    req(game_data_catcher())
    dates <- sort(unique(as.Date(game_data_catcher()$framing$Date)), decreasing = TRUE)
    updateSelectInput(session, "game_date_select", choices = as.character(dates))
  })

  observe({
    req(game_data_catcher())
    catchers <- sort(unique(game_data_catcher()$framing$Catcher))
    updateSelectInput(session, "catcher_name", choices = catchers)
  })

  raw_season_catcher <- reactive({
    req(input$season_csvs)
    bind_rows(lapply(input$season_csvs$datapath, function(f) {
      read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
    })) %>% type.convert(as.is = TRUE)
  })

  observe({
    req(raw_season_catcher())
    teams <- sort(unique(raw_season_catcher()$CatcherTeam))
    updateSelectInput(session, "season_team_select", choices = teams)
  })

  season_data_catcher <- reactive({
    req(raw_season_catcher(), input$season_team_select)
    prep_catcher_data(raw_season_catcher(), input$season_team_select)
  })

  catcher_pdf_path <- reactiveVal(NULL)

  observeEvent(input$generate_catcher, {
    req(input$catcher_name, game_data_catcher(), season_data_catcher(), input$game_date_select)
    output$catcher_status <- renderUI({
      div(style = "color: orange; font-weight: bold;", "Generating report...")
    })
    tryCatch({
      tmp_pdf   <- tempfile(fileext = ".pdf")
      game_date <- as.Date(input$game_date_select)
      generate_catcher_pdf(
        game_framing    = game_data_catcher()$framing  %>% mutate(Date = as.Date(as.character(Date))),
        game_throwing   = game_data_catcher()$throwing %>% mutate(Date = as.Date(as.character(Date))),
        season_framing  = season_data_catcher()$framing  %>% mutate(Date = as.Date(as.character(Date))),
        season_throwing = season_data_catcher()$throwing %>% mutate(Date = as.Date(as.character(Date))),
        catcher     = input$catcher_name,
        game_date   = game_date,
        output_file = tmp_pdf,
        logo_path   = "www/logo1.png"
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

  output$catcher_download_ui <- renderUI({
    req(catcher_pdf_path())
    downloadButton("download_catcher_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_catcher_pdf <- downloadHandler(
    filename = function() paste0(gsub(", ", "_", input$catcher_name), "_CatcherReport.pdf"),
    content  = function(file) { req(catcher_pdf_path()); file.copy(catcher_pdf_path(), file, overwrite = TRUE) }
  )

  # ==========================================
  # HITTER SERVER LOGIC
  # ==========================================

# ==========================================
# HITTER SERVER LOGIC
# ==========================================
# ==========================================
# HITTER SERVER LOGIC
# ==========================================
raw_hitter_game <- reactive({
  req(input$hitter_game_csv)
  read.csv(input$hitter_game_csv$datapath, stringsAsFactors = FALSE)
})

raw_hitter_season <- reactive({
  req(input$hitter_season_csvs)
  lapply(input$hitter_season_csvs$datapath, function(f) {
    read.csv(f, stringsAsFactors = FALSE, colClasses = "character") %>%
      select(-any_of("GameForeignID"))
  }) %>% bind_rows() %>% type.convert(as.is = TRUE)
})

output$hitter_team_select_ui <- renderUI({
  req(input$hitter_game_csv, input$hitter_season_csvs)
  teams <- tryCatch(
    sort(unique(c(raw_hitter_game()$BatterTeam, raw_hitter_season()$BatterTeam))),
    error = function(e) character(0)
  )
  req(length(teams) > 0)
  selectInput("hitter_team_select", "Select Team:", choices = teams)
})

output$hitter_select_ui <- renderUI({
  req(input$hitter_game_csv, input$hitter_season_csvs, input$hitter_team_select)
  game_h <- tryCatch(
    raw_hitter_game() %>%
      filter(BatterTeam == input$hitter_team_select) %>%
      pull(Batter) %>% unique(),
    error = function(e) character(0)
  )
  season_h <- tryCatch(
    raw_hitter_season() %>%
      filter(BatterTeam == input$hitter_team_select) %>%
      pull(Batter) %>% unique(),
    error = function(e) character(0)
  )
  hitters <- sort(unique(c(game_h, season_h)))
  req(length(hitters) > 0)
  selectInput("selected_hitter", "Select Hitter:", choices = hitters)
})

hitter_pdf_path <- reactiveVal(NULL)

observeEvent(input$generate_hitter, {
  req(input$selected_hitter, input$hitter_team_select, raw_hitter_game(), raw_hitter_season())
  output$hitter_status <- renderUI({
    div(style = "color: orange; font-weight: bold;", "Generating report...")
  })
  tryCatch({
    tmp_pdf <- tempfile(fileext = ".pdf")
    generate_hitter_pdf(
      game_data       = raw_hitter_game() %>% filter(BatterTeam == input$hitter_team_select),
      season_data     = raw_hitter_season() %>% filter(BatterTeam == input$hitter_team_select),
      selected_hitter = input$selected_hitter,
      output_file     = tmp_pdf,
      active_models   = sd_models
    )
    hitter_pdf_path(tmp_pdf)
    output$hitter_status <- renderUI({
      div(style = "color: green; font-weight: bold;", "\u2713 Report ready!")
    })
  }, error = function(e) {
    message("hitter ERROR: ", e$message)
    output$hitter_status <- renderUI({
      div(style = "color: red;", paste("Error:", e$message))
    })
  })
})

output$hitter_download_ui <- renderUI({
  req(hitter_pdf_path())
  downloadButton("download_hitter_pdf", "Download Report",
                 class = "btn btn-success", style = "width: 200px;")
})

output$download_hitter_pdf <- downloadHandler(
  filename = function() paste0(gsub(", ", "_", input$selected_hitter), "_HitterReport.pdf"),
  content  = function(file) { req(hitter_pdf_path()); file.copy(hitter_pdf_path(), file, overwrite = TRUE) }
)


  # ==========================================
  # PITCHER SERVER LOGIC
  # ==========================================
  raw_pitcher_game <- reactive({
    req(input$pitcher_game_csv)
    read_csv(input$pitcher_game_csv$datapath, show_col_types = FALSE)
  })

  observe({
    req(raw_pitcher_game())
    pitchers <- sort(unique(raw_pitcher_game()$Pitcher))
    updateSelectInput(session, "pitcher_select", choices = pitchers)
  })

  raw_pitcher_season <- reactive({
    req(input$pitcher_season_csvs)
    bind_rows(lapply(input$pitcher_season_csvs$datapath, function(f) {
      read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
        select(-any_of("GameForeignID"))
    })) %>% type.convert(as.is = TRUE)
  })

  pitcher_pdf_path <- reactiveVal(NULL)

  observeEvent(input$generate_pitcher, {
    req(input$pitcher_select, raw_pitcher_game(), raw_pitcher_season())
    output$pitcher_status <- renderUI({
      div(style = "color: orange; font-weight: bold;", "Generating report...")
    })
    tryCatch({
      tmp_pdf <- tempfile(fileext = ".pdf")
      message("pitcher step A: filtering game data")
      pitcher_data <- raw_pitcher_game() %>% filter(Pitcher == input$pitcher_select)
      message("pitcher step B: game data rows: ", nrow(pitcher_data))
      pitcher_data <- pitcher_data %>% prep_pitcher_data()
      message("pitcher step C: game data prepped rows: ", nrow(pitcher_data))
      message("pitcher step D: filtering season data")
      pitcher_data_season <- raw_pitcher_season() %>% filter(Pitcher == input$pitcher_select)
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
        logo_path           = "www/logo1.png"
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

  output$pitcher_download_ui <- renderUI({
    req(pitcher_pdf_path())
    downloadButton("download_pitcher_pdf", "Download Report",
                   class = "btn btn-success", style = "width: 200px;")
  })

  output$download_pitcher_pdf <- downloadHandler(
    filename = function() paste0(gsub(", ", "_", input$pitcher_select), "_PitcherReport.pdf"),
    content  = function(file) { req(pitcher_pdf_path()); file.copy(pitcher_pdf_path(), file, overwrite = TRUE) }
  )
}

shinyApp(ui = ui, server = server)