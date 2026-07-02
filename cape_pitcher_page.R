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
    tibble::as_tibble() %>%
    mutate(
      caps_row_id = dplyr::row_number(),
      TaggedPitchType = as.character(TaggedPitchType)
    )
}

cape_pitcher_prepare_view <- function(raw_df) {
  prep_pitches(raw_df) %>%
    mutate(
      TaggedPitchType = canonicalize_pitch(as.character(TaggedPitchType)),
      PitchType = canonicalize_pitch(as.character(PitchType)),
      Count = dplyr::if_else(
        !is.na(Balls) & !is.na(Strikes),
        paste0(as.integer(Balls), "-", as.integer(Strikes)),
        NA_character_
      ),
      PitcherDisplay = format_pitcher_name(Pitcher),
      BatterDisplay = vapply(as.character(Batter), format_name, character(1)),
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
    colors = pal_for(side_data$PitchType),
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
        "Search any pitcher, inspect movement by handedness, and retag pitches for the current session.",
        style = "color:#5F6B7A; font-size:14px; margin-bottom:20px;"
      ),
      tags$div(
        id = "cpp-page",
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header("Pitcher Lookup"),
            card_body(
              selectizeInput(
                "cpp_pitcher",
                "Search pitcher",
                choices = NULL,
                width = "100%",
                options = list(placeholder = "Start typing a pitcher name")
              ),
              uiOutput("cpp_pitcher_meta")
            )
          ),
          card(
            card_header("Retag (Session Only)"),
            card_body(
              selectInput(
                "cpp_new_pitch_type",
                "Retag selected pitches as",
                choices = cape_pitcher_tag_choices,
                selected = "Slider"
              ),
              actionButton(
                "cpp_apply_retag",
                "Retag Selected Pitches",
                class = "btn btn-primary btn-block"
              ),
              br(),
              tags$p(
                "Retags are temporary and will last until this app session ends or the page is refreshed.",
                style = "color:#5F6B7A; font-size:12px; margin-bottom:10px;"
              ),
              textOutput("cpp_selection_info"),
              uiOutput("cpp_save_status")
            )
          )
        ),
        card(
          card_header("Season Statline"),
          card_body(uiOutput("cpp_statline_tiles"))
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
                  placeholder = "All counts"
                )
              ),
              tags$div(
                style = "margin-top: 6px;",
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
            card_header("Heatmap Side"),
            card_body(
              radioButtons(
                "cpp_heat_side",
                "Batter side",
                choices = c("All" = "ALL", "vs LHH" = "L", "vs RHH" = "R"),
                selected = "ALL",
                inline = TRUE
              ),
              tags$p(
                "The pitch-type and count filters above drive both the movement plots and the heatmaps. Statline and summary tables stay season-level.",
                style = "color:#5F6B7A; font-size:12px; margin:8px 0 0 0;"
              ),
              actionButton(
                "cpp_clear_selection",
                "Clear Plot Selection",
                class = "btn btn-default btn-sm"
              )
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
        ),
        card(
          card_header("Heatmaps"),
          card_body(
            tags$p(
              "Use the visual filters above to narrow by pitch type and count. Quick buttons make two-strike views one click away.",
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
        )
      )
    ),
    tags$div(
      class = "hub-footer",
      paste0("Brewster Whitecaps Analytics · ", format(Sys.Date(), "%Y"))
    )
  )
}

cape_pitcher_player_page_server <- function(input, output, session) {
  raw_data <- reactiveVal(cape_pitcher_read_parquet())
  loaded_snapshot <- reactiveVal(raw_data())
  status_message <- reactiveVal("Retags are temporary and apply only to this session.")
  status_class <- reactiveVal("clean")
  selected_rows <- reactiveVal(integer(0))

  page_data <- reactive({
    cape_pitcher_prepare_view(raw_data())
  })

  pitcher_choices <- reactive({
    d <- page_data()
    req(nrow(d) > 0)

    d %>%
      filter(!is.na(Pitcher), nzchar(Pitcher)) %>%
      group_by(Pitcher) %>%
      summarise(
        PitcherTeam = {
          vals <- PitcherTeam[!is.na(PitcherTeam)]
          if (length(vals)) vals[1] else PitcherTeam[1]
        },
        .groups = "drop"
      ) %>%
      arrange(Pitcher)
  })

  observe({
    ch <- pitcher_choices()
    current <- isolate(input$cpp_pitcher)
    default_pitcher <- ch$Pitcher[grepl("^BRE_", ch$PitcherTeam)][1]
    if (is.na(default_pitcher) || !nzchar(default_pitcher)) {
      default_pitcher <- ch$Pitcher[1]
    }
    selected <- if (!is.null(current) && current %in% ch$Pitcher) current else default_pitcher
    labels <- paste0(
      format_pitcher_name(ch$Pitcher),
      " - ",
      cape_pitcher_ccbl_name(ch$PitcherTeam)
    )
    updateSelectizeInput(
      session,
      "cpp_pitcher",
      choices = stats::setNames(ch$Pitcher, labels),
      selected = selected,
      server = TRUE
    )
  })

  pitcher_full <- reactive({
    req(input$cpp_pitcher)
    page_data() %>% filter(Pitcher == input$cpp_pitcher)
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
      tags$div(class = "cpp-meta-name", format_pitcher_name(input$cpp_pitcher)),
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
    list(input$cpp_pitcher, input$cpp_pitch_filter, input$cpp_count_filter),
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
    plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event"),
    {
      ed <- plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event")
      ids <- suppressWarnings(as.integer(ed$customdata))
      ids <- ids[!is.na(ids)]
      selected_rows(ids)
    },
    ignoreNULL = TRUE
  )

  observeEvent(
    plotly::event_data("plotly_selected", source = "cpp_mov_rhh_src", priority = "event"),
    {
      ed <- plotly::event_data("plotly_selected", source = "cpp_mov_rhh_src", priority = "event")
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
