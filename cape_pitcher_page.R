cape_pitcher_tag_choices <- c(
  "Four-Seam", "Sinker", "Cutter", "Slider", "Sweeper",
  "Curveball", "Knuckle Curve", "Slurve", "Changeup", "Splitter",
  "Fastball", "Slow Curve", "Knuckle Ball", "Eephus",
  "Screwball", "Other", "Undefined"
)

cape_pitcher_count_levels <- c(
  "0-0", "0-1", "0-2",
  "1-0", "1-1", "1-2",
  "2-0", "2-1", "2-2",
  "3-0", "3-1", "3-2"
)

cape_pitcher_team_levels <- c(
  "BRE_WHI", "BOU_BRA", "CHA_ANG", "COT_KET", "FAL_COM",
  "HAR_MAR", "HYA_HAR", "ORL_FIR", "WAR_GAT", "YAR_RED"
)

cape_pitcher_pitch_pal <- c(
  "Four-Seam" = "#D94A3F",
  "Two-Seam Fastball" = "#B83A95",
  "Sinker" = "#D17A2A",
  "Cutter" = "#C9A82E",
  "Splitter" = "#7FD9A2",
  "Changeup" = "#3DA84B",
  "Slider" = "#2C7AB8",
  "Sweeper" = "#B566B5",
  "Curveball" = "#8E2EA0",
  "Knuckle Curve" = "#7A2A52",
  "Slurve" = "#3F8A66",
  "Knuckle Ball" = "#3FA3A3",
  "Eephus" = "#9A9AA0",
  "Fastball" = "#5FA8C9",
  "Slow Curve" = "#B8B8BE",
  "Screwball" = "#C96A95"
)

cape_pitcher_format_name <- function(name) {
  name <- as.character(name)
  if (length(name) == 0 || is.na(name) || !grepl(",", name, fixed = TRUE)) {
    return(name)
  }
  parts <- strsplit(name, ",\\s*")[[1]]
  if (length(parts) == 2) paste(trimws(parts[2]), trimws(parts[1])) else name
}

cape_pitcher_format_pitcher_name <- function(nm) {
  vapply(as.character(nm), cape_pitcher_format_name, character(1), USE.NAMES = FALSE)
}

cape_pitcher_canonicalize_pitch <- function(x) {
  dplyr::case_when(
    x %in% c("Fastball", "FourSeamFastBall", "FF", "FastBall") ~ "Four-Seam",
    x %in% c("TwoSeamFastBall", "OneSeamFastBall", "Sinker", "SI") ~ "Sinker",
    x %in% c("ChangeUp", "CH") ~ "Changeup",
    x %in% c("KnuckleCurve", "KC") ~ "Curveball",
    x %in% c("CutFastBall", "FC") ~ "Cutter",
    x %in% c("SL") ~ "Slider",
    x %in% c("CU") ~ "Curveball",
    x %in% c("FS") ~ "Splitter",
    TRUE ~ x
  )
}

cape_pitcher_pal_for <- function(types, palette = cape_pitcher_pitch_pal, default = "#888888") {
  types <- unique(as.character(types))
  types <- types[!is.na(types)]
  unknown <- setdiff(types, names(palette))
  if (length(unknown)) {
    palette <- c(palette, stats::setNames(rep(default, length(unknown)), unknown))
  }
  palette
}

cape_pitcher_ccbl_name <- function(team_code) {
  map <- c(
    BRE_WHI = "Brewster Whitecaps",
    BOU_BRA = "Bourne Braves",
    CHA_ANG = "Chatham Anglers",
    COT_KET = "Cotuit Kettleers",
    FAL_COM = "Falmouth Commodores",
    HAR_MAR = "Harwich Mariners",
    HYA_HAR = "Hyannis Harbor Hawks",
    ORL_FIR = "Orleans Firebirds",
    WAR_GAT = "Wareham Gatemen",
    YAR_RED = "Yarmouth-Dennis Red Sox"
  )
  code_chr <- as.character(team_code)
  out <- unname(map[code_chr])
  ifelse(is.na(out), code_chr, out)
}

cape_pitcher_safe_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

cape_pitcher_rate <- function(num, den) {
  if (!is.finite(den) || den <= 0) return(NA_real_)
  num / den
}

cape_pitcher_rate_vec <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

cape_pitcher_format_ip <- function(outs) {
  outs <- suppressWarnings(as.integer(round(outs)))
  if (length(outs) == 0 || is.na(outs)) return("--")
  paste0(outs %/% 3, ".", outs %% 3)
}

cape_pitcher_format_num <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("--")
  formatC(x, format = "f", digits = digits)
}

cape_pitcher_format_rate <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("--")
  sub("^0", "", formatC(x, format = "f", digits = digits))
}

cape_pitcher_format_pct <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("--")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

cape_pitcher_quick_btn <- function(id, label) {
  actionButton(
    id,
    label,
    class = "btn btn-default btn-sm",
    style = paste(
      "margin:0 8px 8px 0;",
      "border-color:#cfd8e3;",
      "color:#0C2340;",
      "font-weight:600;"
    )
  )
}

cape_pitcher_read_parquet <- function(path = "CapeCod26.parquet") {
  arrow::read_parquet(path) %>%
    tibble::as_tibble()
}

cape_pitcher_init_data <- function(path = "CapeCod26.parquet") {
  cape_pitcher_read_parquet(path) %>%
    mutate(
      caps_row_id = dplyr::row_number(),
      TaggedPitchType = as.character(TaggedPitchType)
    )
}

cape_pitcher_prepare_view <- function(raw_df) {
  prep_pitches(raw_df) %>%
    mutate(
      TaggedPitchType = cape_pitcher_canonicalize_pitch(as.character(TaggedPitchType)),
      PitchType = cape_pitcher_canonicalize_pitch(as.character(PitchType)),
      Count = dplyr::if_else(
        !is.na(Balls) & !is.na(Strikes),
        paste0(as.integer(Balls), "-", as.integer(Strikes)),
        NA_character_
      ),
      PitcherDisplay = cape_pitcher_format_pitcher_name(Pitcher),
      BatterDisplay = vapply(as.character(Batter), cape_pitcher_format_name, character(1)),
      TeamDisplay = cape_pitcher_ccbl_name(PitcherTeam),
      CSW = PitchCall %in% c("StrikeCalled", "StrikeSwinging")
    )
}

cape_pitcher_statline <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(list())
  }

  pa_rows <- d %>% filter(PACheck)
  outs_on_play <- cape_pitcher_safe_num(pa_rows$OutsOnPlay)
  outs_on_play[is.na(outs_on_play)] <- 0
  outs_recorded <- sum(ifelse(pa_rows$K %in% TRUE, 1, outs_on_play), na.rm = TRUE)
  innings_pitched <- outs_recorded / 3

  games <- if ("GameUID" %in% names(d)) {
    length(unique(d$GameUID[!is.na(d$GameUID)]))
  } else {
    length(unique(d$Date[!is.na(d$Date)]))
  }

  bf <- sum(pa_rows$PACheck, na.rm = TRUE)
  hits <- sum(pa_rows$Hit, na.rm = TRUE)
  runs <- sum(cape_pitcher_safe_num(d$RunsScored), na.rm = TRUE)
  walks <- sum(pa_rows$BB, na.rm = TRUE)
  strikeouts <- sum(pa_rows$K, na.rm = TRUE)
  homers <- sum(pa_rows$HR, na.rm = TRUE)
  hbp <- sum(pa_rows$HBP, na.rm = TRUE)
  sf <- sum(pa_rows$SF, na.rm = TRUE)
  ab <- sum(pa_rows$AB, na.rm = TRUE)
  total_bases <- sum(pa_rows$X1B, na.rm = TRUE) +
    2 * sum(pa_rows$X2B, na.rm = TRUE) +
    3 * sum(pa_rows$X3B, na.rm = TRUE) +
    4 * sum(pa_rows$HR, na.rm = TRUE)

  avg_against <- cape_pitcher_rate(hits, ab)
  obp_against <- cape_pitcher_rate(hits + walks + hbp, ab + walks + hbp + sf)
  slg_against <- cape_pitcher_rate(total_bases, ab)
  ops_against <- if (is.na(obp_against) || is.na(slg_against)) NA_real_ else obp_against + slg_against
  whip <- cape_pitcher_rate(hits + walks, innings_pitched)
  csw_rate <- cape_pitcher_rate(sum(d$CSW, na.rm = TRUE), nrow(d))
  whiff_rate <- cape_pitcher_rate(sum(d$WhiffP, na.rm = TRUE), sum(d$Swing, na.rm = TRUE))
  k_rate <- cape_pitcher_rate(strikeouts, bf)
  bb_rate <- cape_pitcher_rate(walks, bf)
  k_minus_bb <- cape_pitcher_rate(strikeouts - walks, bf)
  xwoba <- mean(pa_rows$paWOBA, na.rm = TRUE)
  if (!is.finite(xwoba)) xwoba <- NA_real_

  list(
    Games = as.character(games),
    IP = cape_pitcher_format_ip(outs_recorded),
    BF = as.character(bf),
    H = as.character(hits),
    R = as.character(runs),
    BB = as.character(walks),
    K = as.character(strikeouts),
    HR = as.character(homers),
    WHIP = cape_pitcher_format_num(whip, 2),
    AVG = cape_pitcher_format_rate(avg_against, 3),
    OBP = cape_pitcher_format_rate(obp_against, 3),
    SLG = cape_pitcher_format_rate(slg_against, 3),
    OPS = cape_pitcher_format_rate(ops_against, 3),
    `CSW%` = cape_pitcher_format_pct(csw_rate, 1),
    `Whiff%` = cape_pitcher_format_pct(whiff_rate, 1),
    `K-BB%` = cape_pitcher_format_pct(k_minus_bb, 1),
    xwOBA = cape_pitcher_format_rate(xwoba, 3)
  )
}

cape_pitcher_statline_ui <- function(statline) {
  if (!length(statline)) return(NULL)

  tiles <- lapply(names(statline), function(label) {
    tags$div(
      class = "cpp-stat-tile",
      tags$div(class = "cpp-stat-label", label),
      tags$div(class = "cpp-stat-value", statline[[label]])
    )
  })

  tags$div(class = "cpp-stat-grid", tiles)
}

cape_pitcher_hover_text <- function(d) {
  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
  }

  paste0(
    "Pitch: ", ifelse(is.na(d$PitchType), "--", d$PitchType),
    "<br>Tagged: ", ifelse(is.na(d$TaggedPitchType), "--", d$TaggedPitchType),
    "<br>Auto: ", ifelse(is.na(d$AutoPitchType), "--", as.character(d$AutoPitchType)),
    "<br>Date: ", ifelse(is.na(d$Date), "--", as.character(d$Date)),
    "<br>Batter: ", ifelse(is.na(d$BatterDisplay), "--", d$BatterDisplay),
    "<br>Count: ", ifelse(is.na(d$Count), "--", d$Count),
    "<br>Pitch Call: ", ifelse(is.na(d$PitchCall), "--", d$PitchCall),
    "<br>Play Result: ", ifelse(is.na(d$PlayResult), "--", d$PlayResult),
    "<br>Velo: ", fmt_num(d$RelSpeed, 1), " mph",
    "<br>Spin: ", ifelse(is.na(d$SpinRate), "--", format(round(d$SpinRate), big.mark = ",")), " rpm",
    "<br>IVB: ", fmt_num(d$InducedVertBreak, 1),
    "<br>HB: ", fmt_num(d$HorzBreak, 1),
    "<br>Rel Height: ", fmt_num(d$RelHeight, 2),
    "<br>Rel Side: ", fmt_num(d$RelSide, 2),
    "<br>Extension: ", fmt_num(d$Extension, 2),
    "<br>Plate X: ", fmt_num(d$PlateLocSide, 2),
    "<br>Plate Z: ", fmt_num(d$PlateLocHeight, 2),
    "<br>Exit Velo: ", fmt_num(d$ExitSpeed, 1),
    "<br>Launch Angle: ", fmt_num(d$Angle, 1),
    "<br>PitchUID: ", ifelse(is.na(d$PitchUID), "--", as.character(d$PitchUID))
  )
}

cape_pitcher_movement_plot <- function(d, batter_side = c("Left", "Right"), source_id) {
  batter_side <- match.arg(batter_side)
  side_short <- if (batter_side == "Left") "LHH" else "RHH"
  side_data <- d %>% filter(BatterSide == batter_side)

  if (nrow(side_data) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          title = paste("No pitches vs", side_short, "for this filter.")
        )
    )
  }

  hover_text <- cape_pitcher_hover_text(side_data)

  plotly::plot_ly(
    data = side_data,
    x = ~HorzBreak,
    y = ~InducedVertBreak,
    color = ~PitchType,
    colors = cape_pitcher_pal_for(side_data$PitchType),
    type = "scatter",
    mode = "markers",
    text = hover_text,
    hoverinfo = "text",
    customdata = ~caps_row_id,
    source = source_id,
    marker = list(
      size = 10,
      opacity = 0.82,
      line = list(color = "rgba(12,35,64,0.30)", width = 1)
    )
  ) %>%
    plotly::layout(
      title = paste("Movement vs", side_short),
      dragmode = "lasso",
      xaxis = list(
        title = "Horizontal Break (in)",
        zerolinecolor = "rgba(12,35,64,0.25)"
      ),
      yaxis = list(
        title = "Induced Vertical Break (in)",
        zerolinecolor = "rgba(12,35,64,0.25)",
        scaleanchor = "x",
        scaleratio = 1
      ),
      legend = list(orientation = "h", x = 0, y = -0.14),
      margin = list(l = 60, r = 20, b = 70, t = 60)
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "toggleSpikelines"
      )
    ) %>%
    plotly::event_register("plotly_selected")
}

cape_pitcher_player_page_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main",
      tags$head(
        tags$style(HTML("
          #cpp-page .cpp-stat-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
            gap: 12px;
          }
          #cpp-page .cpp-control-stack .form-group {
            margin-bottom: 12px;
          }
          #cpp-page,
          #cpp-page .card,
          #cpp-page .card-body,
          #cpp-page .bslib-grid,
          #cpp-page .bslib-grid-item {
            overflow: visible !important;
          }
          #cpp-page .selectize-control,
          #cpp-page .selectize-input {
            position: relative;
            z-index: 2500;
          }
          #cpp-page .selectize-dropdown,
          body > .selectize-dropdown {
            z-index: 5000 !important;
          }
          #cpp-page .cpp-helper {
            color: #5F6B7A;
            font-size: 12px;
            line-height: 1.45;
            margin: 4px 0 0 0;
          }
          #cpp-page .cpp-meta-card {
            background: linear-gradient(180deg, #F7FAFC 0%, #EEF3F8 100%);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 14px;
            padding: 16px 18px;
            min-height: 132px;
          }
          #cpp-page .cpp-stat-tile {
            background: linear-gradient(180deg, #0C2340 0%, #14355F 100%);
            border-radius: 14px;
            padding: 14px 12px;
            box-shadow: 0 10px 22px rgba(12, 35, 64, 0.08);
          }
          #cpp-page .cpp-stat-label {
            color: rgba(255, 255, 255, 0.70);
            font-size: 11px;
            font-weight: 700;
            letter-spacing: 1px;
            margin-bottom: 8px;
            text-transform: uppercase;
          }
          #cpp-page .cpp-stat-value {
            color: #FFFFFF;
            font-size: 28px;
            font-weight: 700;
            letter-spacing: -0.5px;
            line-height: 1;
          }
          #cpp-page .cpp-meta-name {
            color: #0C2340;
            font-size: 20px;
            font-weight: 700;
            margin-bottom: 4px;
          }
          #cpp-page .cpp-meta-line {
            color: #5F6B7A;
            font-size: 13px;
            line-height: 1.45;
          }
          #cpp-page .cpp-section-label {
            color: #0C2340;
            font-size: 18px;
            font-weight: 700;
            margin: 22px 0 10px 0;
          }
          #cpp-page .cpp-section-copy {
            color: #5F6B7A;
            font-size: 12px;
            line-height: 1.45;
            margin: -2px 0 12px 0;
          }
          #cpp-page .cpp-quick-row {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
            margin-top: 6px;
          }
          #cpp-page .cpp-quick-row .btn {
            margin: 0;
          }
          #cpp-page .cpp-retag-card .form-group {
            margin-bottom: 10px;
          }
          #cpp-page .cpp-retag-card .btn {
            font-weight: 600;
          }
          #cpp-page .cpp-retag-note {
            color: #5F6B7A;
            font-size: 11px;
            line-height: 1.4;
            margin: 6px 0 8px 0;
          }
          #cpp-page .cpp-retag-meta {
            color: #344054;
            font-size: 12px;
            line-height: 1.4;
            margin-bottom: 8px;
          }
          #cpp-page .cpp-retag-card .cpp-status {
            margin-top: 6px;
          }
          #cpp-page .cpp-heatmap-controls {
            background: linear-gradient(180deg, #F7FAFC 0%, #EEF3F8 100%);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 12px;
            padding: 12px 14px;
            margin-bottom: 14px;
          }
          #cpp-page .cpp-heatmap-controls .form-group {
            margin-bottom: 8px;
          }
          #cpp-page .cpp-status {
            border-radius: 12px;
            padding: 10px 12px;
            font-size: 13px;
            font-weight: 600;
          }
          #cpp-page .cpp-status.clean {
            background: #ECF7F3;
            color: #16684F;
          }
          #cpp-page .cpp-status.dirty {
            background: #FFF6E5;
            color: #8A5A00;
          }
          #cpp-page .cpp-status.error {
            background: #FDECEC;
            color: #A12626;
          }
        "))
      ),
      tags$div(
        style = "margin-bottom: 16px;",
        tags$button(
          "← Back to Hub",
          onclick = "Shiny.setInputValue('nav_to', 'hub', {priority: 'event'})",
          class = "btn btn-outline-secondary btn-sm"
        )
      ),
      tags$h2(
        "Cape Cod Pitcher Player Page",
        style = "font-family: var(--font-head); color: var(--navy); margin-bottom: 10px;"
      ),
      tags$p(
        "Choose a Cape team, load a pitcher, and work through movement, tables, and heatmaps in one place.",
        style = "color:#5F6B7A; font-size:14px; margin-bottom:20px;"
      ),
      tags$div(
        id = "cpp-page",
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Select Team & Pitcher"),
            card_body(
              tags$div(
                class = "cpp-control-stack",
                selectInput(
                  "cpp_team",
                  "Cape Cod League team",
                  choices = NULL,
                  width = "100%",
                  selectize = FALSE
                ),
                selectInput(
                  "cpp_pitcher",
                  "Pitcher",
                  choices = NULL,
                  width = "100%",
                  selectize = FALSE
                ),
                tags$p(
                  "Pick a team first, then choose from the pitchers currently available for that club.",
                  class = "cpp-helper"
                )
              )
            )
          ),
          card(
            card_header("Pitcher Snapshot"),
            card_body(
              tags$div(
                class = "cpp-meta-card",
                uiOutput("cpp_pitcher_meta")
              )
            )
          )
        ),
        card(
          card_header("Season Statline"),
          card_body(uiOutput("cpp_statline_tiles"))
        ),
        tags$div(class = "cpp-section-label", "Visual Analysis"),
        tags$p(
          "Shared filters, movement plots, and session retagging live together here so the scouting workflow stays in one place.",
          class = "cpp-section-copy"
        ),
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header("Visual Filters"),
            card_body(
              selectizeInput(
                "cpp_pitch_filter",
                "Pitch type",
                choices = NULL,
                multiple = TRUE,
                width = "100%",
                options = list(
                  plugins = list("remove_button"),
                  dropdownParent = "body",
                  placeholder = "All pitch types"
                )
              ),
              selectizeInput(
                "cpp_count_filter",
                "Count",
                choices = NULL,
                multiple = TRUE,
                width = "100%",
                options = list(
                  plugins = list("remove_button"),
                  dropdownParent = "body",
                  placeholder = "All counts"
                )
              ),
              tags$div(
                class = "cpp-quick-row",
                cape_pitcher_quick_btn("cpp_counts_all", "All Counts"),
                cape_pitcher_quick_btn("cpp_counts_two_strike", "All 2-Strike"),
                cape_pitcher_quick_btn("cpp_counts_02", "0-2"),
                cape_pitcher_quick_btn("cpp_counts_12", "1-2"),
                cape_pitcher_quick_btn("cpp_counts_22", "2-2"),
                cape_pitcher_quick_btn("cpp_counts_32", "3-2")
              )
            )
          ),
          card(
            card_header("Retag Selection"),
            card_body(
              class = "cpp-retag-card",
              selectInput(
                "cpp_new_pitch_type",
                "Retag selected pitches as",
                choices = cape_pitcher_tag_choices,
                selected = "Slider",
                selectize = FALSE
              ),
              actionButton(
                "cpp_apply_retag",
                "Apply Retag",
                class = "btn btn-primary btn-sm btn-block"
              ),
              actionButton(
                "cpp_clear_selection",
                "Clear Plot Selection",
                class = "btn btn-default btn-sm btn-block"
              ),
              tags$p(
                "Session only. Retags stay on this page until refresh.",
                class = "cpp-retag-note"
              ),
              tags$div(
                class = "cpp-retag-meta",
                textOutput("cpp_selection_info", inline = TRUE)
              ),
              uiOutput("cpp_save_status")
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Movement vs LHH"),
            card_body(plotlyOutput("cpp_mov_lhh", height = "560px"))
          ),
          card(
            card_header("Movement vs RHH"),
            card_body(plotlyOutput("cpp_mov_rhh", height = "560px"))
          )
        ),
        card(
          card_header("Heatmaps"),
          card_body(
            tags$div(
              class = "cpp-heatmap-controls",
              radioButtons(
                "cpp_heat_side",
                "Heatmap batter side",
                choices = c("All" = "ALL", "vs LHH" = "L", "vs RHH" = "R"),
                selected = "ALL",
                inline = TRUE
              ),
              tags$p(
                "Pitch type and count filters from the visual analysis section also apply here.",
                class = "cpp-section-copy",
                style = "margin: 0;"
              )
            ),
            tags$p(
              "Use the filters above to narrow by pitch type and count. Quick buttons make two-strike heatmap views one click away.",
              style = "color:#5F6B7A; font-size:12px; margin-bottom:12px;"
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              card(
                card_header("Location - where he lives"),
                card_body(uiOutput("cpp_pitch_zone"))
              ),
              card(
                card_header("Whiff Zone - where he misses bats"),
                card_body(uiOutput("cpp_pitch_whiff_zone"))
              ),
              card(
                card_header("Damage Zone - exit velo allowed"),
                card_body(uiOutput("cpp_pitch_dmg_zone"))
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("xwOBAcon Zone - contact quality allowed"),
                card_body(uiOutput("cpp_pitch_xwc_zone"))
              ),
              card(
                card_header("xwOBA Zone - expected outcome allowed"),
                card_body(uiOutput("cpp_pitch_xwf_zone"))
              )
            )
          )
        ),
        tags$div(class = "cpp-section-label", "Performance Tables"),
        tags$p(
          "Pitch shape, split performance, usage, and expected outcomes are grouped below for report-building.",
          class = "cpp-section-copy"
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Arsenal - velo / movement / shape"),
            card_body(reactableOutput("cpp_arsenal"))
          ),
          card(
            card_header("Results vs LHH / vs RHH"),
            card_body(reactableOutput("cpp_psplit"))
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("vs LHH - grades & results by pitch"),
            card_body(reactableOutput("cpp_perf_lhh"))
          ),
          card(
            card_header("vs RHH - grades & results by pitch"),
            card_body(reactableOutput("cpp_perf_rhh"))
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Pitch Mix by Side"),
            card_body(reactableOutput("cpp_pmix"))
          ),
          card(
            card_header("Get-Ahead & Put-Away"),
            card_body(reactableOutput("cpp_pusage"))
          )
        ),
        card(
          card_header("xwOBA / xwOBAcon - by pitch"),
          card_body(reactableOutput("cpp_pitch_xw_table"))
        )
      )
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

cape_pitcher_player_page_server <- function(input, output, session,
                                            data_path = "CapeCod26.parquet",
                                            source_data = NULL) {
  raw_data <- reactiveVal(NULL)
  loaded_snapshot <- reactiveVal(NULL)
  data_loaded <- reactiveVal(FALSE)
  data_loading <- reactiveVal(FALSE)
  status_message <- reactiveVal("Pitcher page data will load when this tab is opened.")
  status_class <- reactiveVal("clean")
  selected_rows <- reactiveVal(integer(0))

  observeEvent(input$caps_nav, {
    if (!identical(input$caps_nav, "tab_pitcher_player") || isTRUE(data_loaded()) || isTRUE(data_loading())) {
      return()
    }

    data_loading(TRUE)
    status_class("clean")
    status_message("Loading pitcher page data...")

    tryCatch({
      initial_data <- cape_pitcher_init_data(path = data_path)
      raw_data(initial_data)
      loaded_snapshot(initial_data)
      data_loaded(TRUE)
      selected_rows(integer(0))
    }, error = function(e) {
      status_class("error")
      status_message(paste("Pitcher page load failed:", e$message))
      showNotification(paste("Pitcher page load failed:", e$message), type = "error")
    })

    data_loading(FALSE)
  }, ignoreInit = FALSE)

  page_data <- reactive({
    req(isTRUE(data_loaded()))
    req(!is.null(raw_data()))
    cape_pitcher_prepare_view(raw_data())
  })

  pitcher_catalog <- reactive({
    d <- page_data()
    req(nrow(d) > 0)

    d %>%
      filter(
        !is.na(PitcherTeam), nzchar(PitcherTeam),
        !is.na(Pitcher), nzchar(Pitcher)
      ) %>%
      transmute(
        PitcherTeam = as.character(PitcherTeam),
        Pitcher = as.character(Pitcher)
      ) %>%
      distinct() %>%
      mutate(
        TeamDisplay = cape_pitcher_ccbl_name(PitcherTeam),
        PitcherDisplay = cape_pitcher_format_pitcher_name(Pitcher),
        TeamOrder = match(PitcherTeam, cape_pitcher_team_levels),
        TeamOrder = ifelse(is.na(TeamOrder), 999, TeamOrder)
      ) %>%
      arrange(TeamOrder, TeamDisplay, PitcherDisplay, Pitcher)
  })

  observe({
    tc <- pitcher_catalog() %>%
      distinct(PitcherTeam, TeamDisplay, TeamOrder) %>%
      arrange(TeamOrder, TeamDisplay)

    current_team <- isolate(input$cpp_team)
    default_team <- if ("BRE_WHI" %in% tc$PitcherTeam) "BRE_WHI" else tc$PitcherTeam[1]
    selected_team <- if (!is.null(current_team) && current_team %in% tc$PitcherTeam) {
      current_team
    } else {
      default_team
    }

    updateSelectInput(
      session,
      "cpp_team",
      choices = stats::setNames(tc$PitcherTeam, tc$TeamDisplay),
      selected = selected_team
    )
  })

  team_pitchers <- reactive({
    req(input$cpp_team)
    pitcher_catalog() %>%
      filter(PitcherTeam == input$cpp_team)
  })

  observe({
    ch <- team_pitchers()
    req(nrow(ch) > 0)

    current_pitcher <- isolate(input$cpp_pitcher)
    selected_pitcher <- if (!is.null(current_pitcher) && current_pitcher %in% ch$Pitcher) {
      current_pitcher
    } else {
      ch$Pitcher[1]
    }

    updateSelectInput(
      session,
      "cpp_pitcher",
      choices = stats::setNames(ch$Pitcher, ch$PitcherDisplay),
      selected = selected_pitcher
    )
  })

  pitcher_full <- reactive({
    req(input$cpp_team, input$cpp_pitcher)
    page_data() %>% filter(PitcherTeam == input$cpp_team, Pitcher == input$cpp_pitcher)
  })

  current_pending_changes <- reactive({
    current <- raw_data()
    snapshot <- loaded_snapshot()
    req(nrow(current) == nrow(snapshot))

    current_tags <- as.character(current$TaggedPitchType)
    snapshot_tags <- as.character(snapshot$TaggedPitchType)
    current_tags[is.na(current_tags)] <- "__NA__"
    snapshot_tags[is.na(snapshot_tags)] <- "__NA__"
    sum(current_tags != snapshot_tags)
  })

  observe({
    pending <- current_pending_changes()
    if (pending > 0) {
      status_class("dirty")
      status_message(paste0(pending, " pitch(es) retagged in this session."))
    } else {
      status_class("clean")
      status_message("Retags are temporary and apply only to this session.")
    }
  })

  output$cpp_pitcher_meta <- renderUI({
    d <- pitcher_full()
    req(nrow(d) > 0)

    team_code <- d$PitcherTeam[!is.na(d$PitcherTeam)][1]
    throw_hand <- d$PitcherThrows[!is.na(d$PitcherThrows)][1]
    game_count <- if ("GameUID" %in% names(d)) {
      length(unique(d$GameUID[!is.na(d$GameUID)]))
    } else {
      length(unique(d$Date[!is.na(d$Date)]))
    }
    date_vals <- suppressWarnings(as.Date(as.character(d$Date)))
    date_vals <- date_vals[!is.na(date_vals)]
    date_line <- if (length(date_vals)) {
      paste0(min(date_vals), " to ", max(date_vals))
    } else {
      "Dates unavailable"
    }

    tags$div(
      tags$div(class = "cpp-meta-name", cape_pitcher_format_pitcher_name(input$cpp_pitcher)),
      tags$div(
        class = "cpp-meta-line",
        paste(
          c(
            cape_pitcher_ccbl_name(team_code),
            if (!is.na(throw_hand) && nzchar(throw_hand)) paste0(substr(throw_hand, 1, 1), "HP") else NULL,
            paste0(game_count, " game(s)"),
            paste0(nrow(d), " pitches")
          ),
          collapse = " · "
        )
      ),
      tags$div(class = "cpp-meta-line", paste("Season window:", date_line))
    )
  })

  output$cpp_statline_tiles <- renderUI({
    cape_pitcher_statline_ui(cape_pitcher_statline(pitcher_full()))
  })

  observe({
    d <- pitcher_full()
    req(nrow(d) > 0)

    pitch_choices <- sort(unique(d$PitchType[!is.na(d$PitchType)]))
    count_choices <- intersect(cape_pitcher_count_levels, unique(d$Count[!is.na(d$Count)]))

    current_pitch <- isolate(input$cpp_pitch_filter)
    current_count <- isolate(input$cpp_count_filter)

    updateSelectizeInput(
      session,
      "cpp_pitch_filter",
      choices = pitch_choices,
      selected = current_pitch[current_pitch %in% pitch_choices],
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "cpp_count_filter",
      choices = count_choices,
      selected = current_count[current_count %in% count_choices],
      server = TRUE
    )
  })

  count_choices_current <- reactive({
    d <- pitcher_full()
    req(nrow(d) > 0)
    intersect(cape_pitcher_count_levels, unique(d$Count[!is.na(d$Count)]))
  })

  observeEvent(input$cpp_counts_all, {
    updateSelectizeInput(session, "cpp_count_filter", selected = character(0))
  })
  observeEvent(input$cpp_counts_two_strike, {
    updateSelectizeInput(
      session,
      "cpp_count_filter",
      selected = intersect(c("0-2", "1-2", "2-2", "3-2"), count_choices_current())
    )
  })
  observeEvent(input$cpp_counts_02, {
    updateSelectizeInput(session, "cpp_count_filter", selected = intersect("0-2", count_choices_current()))
  })
  observeEvent(input$cpp_counts_12, {
    updateSelectizeInput(session, "cpp_count_filter", selected = intersect("1-2", count_choices_current()))
  })
  observeEvent(input$cpp_counts_22, {
    updateSelectizeInput(session, "cpp_count_filter", selected = intersect("2-2", count_choices_current()))
  })
  observeEvent(input$cpp_counts_32, {
    updateSelectizeInput(session, "cpp_count_filter", selected = intersect("3-2", count_choices_current()))
  })

  pitcher_visual <- reactive({
    d <- pitcher_full()
    req(nrow(d) > 0)

    if (length(input$cpp_pitch_filter) > 0) {
      d <- d %>% filter(PitchType %in% input$cpp_pitch_filter)
    }
    if (length(input$cpp_count_filter) > 0) {
      d <- d %>% filter(Count %in% input$cpp_count_filter)
    }
    d
  })

  observeEvent(
    list(input$cpp_team, input$cpp_pitcher, input$cpp_pitch_filter, input$cpp_count_filter),
    {
      selected_rows(integer(0))
    },
    ignoreInit = TRUE
  )

  heatmap_data <- reactive({
    d <- pitcher_visual()
    req(nrow(d) > 0)

    if (identical(input$cpp_heat_side, "L")) {
      d <- d %>% filter(BatterSide == "Left")
    } else if (identical(input$cpp_heat_side, "R")) {
      d <- d %>% filter(BatterSide == "Right")
    }
    d
  })

  output$cpp_mov_lhh <- plotly::renderPlotly({
    cape_pitcher_movement_plot(pitcher_visual(), "Left", "cpp_mov_lhh_src")
  })

  output$cpp_mov_rhh <- plotly::renderPlotly({
    cape_pitcher_movement_plot(pitcher_visual(), "Right", "cpp_mov_rhh_src")
  })

  observeEvent(
    suppressWarnings(
      plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event")
    ),
    {
      ed <- suppressWarnings(
        plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event")
      )
      ids <- suppressWarnings(as.integer(ed$customdata))
      ids <- ids[!is.na(ids)]
      selected_rows(ids)
    },
    ignoreNULL = TRUE
  )

  observeEvent(
    suppressWarnings(
      plotly::event_data("plotly_selected", source = "cpp_mov_rhh_src", priority = "event")
    ),
    {
      ed <- suppressWarnings(
        plotly::event_data("plotly_selected", source = "cpp_mov_rhh_src", priority = "event")
      )
      ids <- suppressWarnings(as.integer(ed$customdata))
      ids <- ids[!is.na(ids)]
      selected_rows(ids)
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$cpp_clear_selection, {
    selected_rows(integer(0))
  })

  output$cpp_selection_info <- renderText({
    sel_n <- length(selected_rows())
    pending <- current_pending_changes()
    if (sel_n == 0 && pending == 0) {
      "No pitches selected."
    } else if (sel_n == 0) {
      paste0("No pitches selected. ", pending, " session retag(s).")
    } else if (pending == 0) {
      paste0(sel_n, " pitch(es) selected.")
    } else {
      paste0(sel_n, " pitch(es) selected. ", pending, " session retag(s).")
    }
  })

  output$cpp_save_status <- renderUI({
    tags$div(
      class = paste("cpp-status", status_class()),
      status_message()
    )
  })

  observeEvent(input$cpp_apply_retag, {
    ids <- selected_rows()
    if (!length(ids)) {
      showNotification("Select pitches on a movement plot first.", type = "warning")
      return()
    }

    updated <- raw_data()
    rows <- which(updated$caps_row_id %in% ids)
    if (!length(rows)) {
      showNotification("Selected pitches are no longer available. Reload the page data and try again.", type = "error")
      return()
    }

    updated$TaggedPitchType[rows] <- input$cpp_new_pitch_type
    raw_data(updated)
    status_class("dirty")
    status_message(paste0("Retagged ", length(rows), " pitch(es) for this session."))
    showNotification(
      paste0("Retagged ", length(rows), " pitch(es) as ", input$cpp_new_pitch_type, "."),
      type = "message"
    )
  })

  output$cpp_arsenal <- renderReactable({
    make_table(
      pitcher_arsenal(pitcher_full()),
      pct = "Usage%",
      d1 = c("IVB", "HB", "Ext"),
      d3 = "xwOBAcon",
      int = c("#", "Spin")
    )
  })

  output$cpp_psplit <- renderReactable({
    make_table(
      pitcher_split(pitcher_full()),
      pct = c("Whiff%", "Chase%", "K%", "BB%"),
      d3 = c("AVG", "OPS", "OBP", "SLG"),
      int = c("PA", "Stuff+", "Pitch+")
    )
  })

  output$cpp_perf_lhh <- renderReactable({
    make_table(
      pitcher_perf_side(pitcher_full(), "L"),
      pct = c("Zone%", "Whiff%", "Chase%"),
      d3 = "xwOBAcon",
      d1 = "RV",
      int = c("#", "Stuff+", "Loc+", "Pitch+")
    )
  })

  output$cpp_perf_rhh <- renderReactable({
    make_table(
      pitcher_perf_side(pitcher_full(), "R"),
      pct = c("Zone%", "Whiff%", "Chase%"),
      d3 = "xwOBAcon",
      d1 = "RV",
      int = c("#", "Stuff+", "Loc+", "Pitch+")
    )
  })

  output$cpp_pmix <- renderReactable({
    make_table(pmix_wide(pitcher_full()), pct = c("vs LHH", "vs RHH"))
  })

  output$cpp_pusage <- renderReactable({
    make_table(pusage(pitcher_full()), pct = c("Usage%", "Whiff%"))
  })

  output$cpp_pitch_xw_table <- renderReactable({
    make_table(
      pitcher_xw_table(pitcher_full()),
      d3 = c("xwOBAcon", "xwOBA"),
      int = c("#", "BBE")
    )
  })

  output$cpp_pitch_zone <- renderUI({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(div("No pitches match the current heatmap filter.", style = "color:#888;padding:10px;"))
    }
    zone_heat(d, "freq")
  })

  output$cpp_pitch_whiff_zone <- renderUI({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(div("No pitches match the current heatmap filter.", style = "color:#888;padding:10px;"))
    }
    zone_heat(d, "whiff")
  })

  output$cpp_pitch_dmg_zone <- renderUI({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(div("No pitches match the current heatmap filter.", style = "color:#888;padding:10px;"))
    }
    zone_heat(d, "damage")
  })

  output$cpp_pitch_xwc_zone <- renderUI({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(div("No pitches match the current heatmap filter.", style = "color:#888;padding:10px;"))
    }
    zone_heat(d, "xwoba")
  })

  output$cpp_pitch_xwf_zone <- renderUI({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(div("No pitches match the current heatmap filter.", style = "color:#888;padding:10px;"))
    }
    zone_heat(d, "xwobafull")
  })
}
