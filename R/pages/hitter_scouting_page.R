# Opponent hitter scouting workspace for BASE.

hsp_rate <- function(num, den) {
  if (length(den) != 1L || is.na(den) || den <= 0) NA_real_ else num / den
}

hsp_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

hsp_quantile <- function(x, p) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE)) else NA_real_
}

hsp_fmt_num <- function(x, digits = 1, suffix = "") {
  if (length(x) != 1L || !is.finite(x)) return("--")
  paste0(formatC(x, format = "f", digits = digits), suffix)
}

hsp_fmt_rate <- function(x, digits = 3) {
  if (length(x) != 1L || !is.finite(x)) return("--")
  sub("^0", "", formatC(x, format = "f", digits = digits))
}

hsp_fmt_pct <- function(x, digits = 1) {
  if (length(x) != 1L || !is.finite(x)) return("--")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

hsp_hand_label <- function(value) {
  dplyr::case_when(
    value %in% c("L", "Left") ~ "vs LHP",
    value %in% c("R", "Right") ~ "vs RHP",
    TRUE ~ "All Pitchers"
  )
}

hsp_prepare_data <- function(df) {
  if (is.null(df)) return(tibble::tibble())
  df <- tibble::as_tibble(df)
  if (!nrow(df)) return(df)

  if (!"Bearing" %in% names(df)) df$Bearing <- NA_real_
  if ("Direction" %in% names(df)) {
    bearing <- suppressWarnings(as.numeric(df$Bearing))
    direction <- suppressWarnings(as.numeric(df$Direction))
    df$Bearing <- dplyr::coalesce(bearing, direction)
  }

  prepared <- prep_pitches(base_apply_pitch_retags(df))
  prepared %>%
    mutate(
      PitchGroupDisplay = case_when(
        PitchGroup == "Fastball" ~ "Fastball",
        PitchGroup == "Breaking" ~ "Breaking Ball",
        PitchGroup == "Offspeed" ~ "Offspeed",
        TRUE ~ "Other"
      ),
      Count = paste0(as.integer(Balls), "-", as.integer(Strikes)),
      ZoneSwing = InZone & Swing,
      ZoneContact = InZone & Contact,
      OutContact = OutZone & Contact,
      HardHit = BBE & !is.na(ExitSpeed) & ExitSpeed >= 95,
      SweetSpot = BBE & !is.na(Angle) & dplyr::between(Angle, 8, 32),
      PullSide = case_when(
        BatSide == "R" & !is.na(Bearing) & Bearing < -8 ~ "Pull",
        BatSide == "L" & !is.na(Bearing) & Bearing > 8 ~ "Pull",
        !is.na(Bearing) & abs(Bearing) <= 8 ~ "Center",
        !is.na(Bearing) ~ "Opposite",
        TRUE ~ NA_character_
      )
    )
}

hsp_filter_hand <- function(d, hand = "ALL") {
  if (identical(hand, "L")) return(d %>% filter(PThrows == "L"))
  if (identical(hand, "R")) return(d %>% filter(PThrows == "R"))
  d
}

hsp_add_supplement <- function(primary, supplemental, player) {
  if (is.null(primary) || !nrow(primary) || is.null(supplemental) || !nrow(supplemental)) {
    return(primary)
  }
  extra <- base_player_supplement_rows(primary, supplemental, player, role = "Batter")
  if (!nrow(extra)) return(primary)
  extra$Batter <- as.character(player)[[1]]
  tryCatch(
    dplyr::bind_rows(primary, extra),
    error = function(e) {
      message("Cape hitter supplement bind failed for ", player, ": ", conditionMessage(e))
      primary
    }
  )
}

hsp_summary <- function(d) {
  empty <- list(
    Games = 0L, PA = 0L, Pitches = nrow(d), AVG = NA_real_, OBP = NA_real_,
    SLG = NA_real_, OPS = NA_real_, KRate = NA_real_, BBRate = NA_real_,
    SwingRate = NA_real_, FirstPitchSwing = NA_real_, ChaseRate = NA_real_,
    ContactRate = NA_real_, ZoneSwingRate = NA_real_, ZoneContactRate = NA_real_,
    TwoStrikeContact = NA_real_, AvgEV = NA_real_, MaxEV = NA_real_, P90EV = NA_real_,
    HardHitRate = NA_real_, SweetSpotRate = NA_real_, xwOBA = NA_real_,
    xwOBAcon = NA_real_, PullRate = NA_real_
  )
  if (is.null(d) || !nrow(d)) return(empty)

  line <- hitter_line(d)
  bbe <- d$BBE %in% TRUE
  pa_rows <- d$PACheck %in% TRUE
  games <- if ("GameID" %in% names(d)) {
    length(unique(d$GameID[!is.na(d$GameID) & nzchar(as.character(d$GameID))]))
  } else {
    length(unique(d$Date[!is.na(d$Date)]))
  }
  two_strike_swings <- sum(d$TwoStrike & d$Swing, na.rm = TRUE)

  list(
    Games = games,
    PA = sum(pa_rows, na.rm = TRUE),
    Pitches = nrow(d),
    AVG = line$AVG[[1]], OBP = line$OBP[[1]], SLG = line$SLG[[1]], OPS = line$OPS[[1]],
    KRate = line$`K%`[[1]], BBRate = line$`BB%`[[1]],
    SwingRate = line$`Sw%`[[1]], FirstPitchSwing = line$`1stP Sw%`[[1]],
    ChaseRate = line$`Chase%`[[1]], ContactRate = line$`Contact%`[[1]],
    ZoneSwingRate = hsp_rate(sum(d$ZoneSwing, na.rm = TRUE), sum(d$InZone, na.rm = TRUE)),
    ZoneContactRate = hsp_rate(sum(d$ZoneContact, na.rm = TRUE), sum(d$InZone & d$Swing, na.rm = TRUE)),
    TwoStrikeContact = hsp_rate(sum(d$TwoStrike & d$Contact, na.rm = TRUE), two_strike_swings),
    AvgEV = hsp_mean(d$ExitSpeed[bbe]),
    MaxEV = if (any(is.finite(d$ExitSpeed[bbe]))) max(d$ExitSpeed[bbe], na.rm = TRUE) else NA_real_,
    P90EV = hsp_quantile(d$ExitSpeed[bbe], 0.90),
    HardHitRate = hsp_rate(sum(d$HardHit, na.rm = TRUE), sum(bbe, na.rm = TRUE)),
    SweetSpotRate = hsp_rate(sum(d$SweetSpot, na.rm = TRUE), sum(bbe, na.rm = TRUE)),
    xwOBA = hsp_mean(d$paWOBA[pa_rows]),
    xwOBAcon = hsp_mean(d$xwOBA[bbe]),
    PullRate = hsp_rate(sum(d$PullSide == "Pull", na.rm = TRUE), sum(!is.na(d$PullSide)))
  )
}

hsp_stat_groups <- function(d) {
  s <- hsp_summary(d)
  list(
    `Season Line` = list(
      G = as.character(s$Games), PA = as.character(s$PA),
      AVG = hsp_fmt_rate(s$AVG), OBP = hsp_fmt_rate(s$OBP),
      SLG = hsp_fmt_rate(s$SLG), OPS = hsp_fmt_rate(s$OPS)
    ),
    `Plate Discipline` = list(
      `K%` = hsp_fmt_pct(s$KRate), `BB%` = hsp_fmt_pct(s$BBRate),
      `Swing%` = hsp_fmt_pct(s$SwingRate), `Chase%` = hsp_fmt_pct(s$ChaseRate),
      `Contact%` = hsp_fmt_pct(s$ContactRate), `Zone Contact%` = hsp_fmt_pct(s$ZoneContactRate)
    ),
    `Contact Quality` = list(
      `Avg EV` = hsp_fmt_num(s$AvgEV, 1), `Max EV` = hsp_fmt_num(s$MaxEV, 1),
      `P90 EV` = hsp_fmt_num(s$P90EV, 1), `Hard Hit%` = hsp_fmt_pct(s$HardHitRate),
      `Sweet Spot%` = hsp_fmt_pct(s$SweetSpotRate), xwOBAcon = hsp_fmt_rate(s$xwOBAcon)
    ),
    Approach = list(
      `1st Pitch Swing%` = hsp_fmt_pct(s$FirstPitchSwing),
      `Zone Swing%` = hsp_fmt_pct(s$ZoneSwingRate),
      `2-Strike Contact%` = hsp_fmt_pct(s$TwoStrikeContact),
      `Pull%` = hsp_fmt_pct(s$PullRate),
      xwOBA = hsp_fmt_rate(s$xwOBA),
      Pitches = format(s$Pitches, big.mark = ",")
    )
  )
}

hsp_statline_ui <- function(d) {
  groups <- hsp_stat_groups(d)
  tags$div(
    class = "hsp-stat-groups",
    lapply(names(groups), function(group_name) {
      metrics <- groups[[group_name]]
      tags$section(
        class = "hsp-stat-group",
        tags$div(class = "hsp-stat-group-title", group_name),
        tags$div(
          class = "hsp-stat-grid",
          lapply(names(metrics), function(label) {
            tags$div(
              class = "hsp-stat-tile",
              tags$div(class = "hsp-stat-label", label),
              tags$div(class = "hsp-stat-value", metrics[[label]])
            )
          })
        )
      )
    })
  )
}

hsp_split_row <- function(d, label) {
  s <- hsp_summary(d)
  tibble::tibble(
    Split = label, PA = s$PA, Pitches = s$Pitches,
    AVG = s$AVG, OBP = s$OBP, SLG = s$SLG, OPS = s$OPS,
    `K%` = s$KRate, `BB%` = s$BBRate, `Chase%` = s$ChaseRate,
    `Contact%` = s$ContactRate, `Hard Hit%` = s$HardHitRate,
    `Avg EV` = s$AvgEV, xwOBA = s$xwOBA, xwOBAcon = s$xwOBAcon
  )
}

hsp_hand_split_table <- function(d) {
  bind_rows(
    hsp_split_row(d, "All Pitchers"),
    hsp_split_row(d %>% filter(PThrows == "R"), "vs RHP"),
    hsp_split_row(d %>% filter(PThrows == "L"), "vs LHP")
  )
}

hsp_pitch_group_table <- function(d) {
  if (is.null(d) || !nrow(d)) return(tibble::tibble())
  total <- nrow(d)
  grouped <- d %>%
    filter(PitchGroupDisplay %in% c("Fastball", "Breaking Ball", "Offspeed"))
  if (!nrow(grouped)) return(tibble::tibble())
  grouped %>%
    group_by(`Pitch Family` = PitchGroupDisplay) %>%
    group_split() %>%
    purrr::map_dfr(function(g) {
      s <- hsp_summary(g)
      tibble::tibble(
        `Pitch Family` = g$PitchGroupDisplay[[1]],
        `#` = nrow(g), `Usage%` = nrow(g) / total,
        `Avg Velo` = hsp_mean(g$RelSpeed), `Swing%` = s$SwingRate,
        `Whiff%` = hsp_rate(sum(g$WhiffP, na.rm = TRUE), sum(g$Swing, na.rm = TRUE)),
        `Chase%` = s$ChaseRate, `Contact%` = s$ContactRate,
        `Hard Hit%` = s$HardHitRate, `Avg EV` = s$AvgEV,
        OPS = s$OPS, xwOBAcon = s$xwOBAcon
      )
    }) %>%
    arrange(match(`Pitch Family`, c("Fastball", "Breaking Ball", "Offspeed")))
}

hsp_pitch_type_table <- function(d) {
  if (is.null(d) || !nrow(d)) return(tibble::tibble())
  total <- nrow(d)
  grouped <- d %>%
    filter(!is.na(PitchType), nzchar(PitchType))
  if (!nrow(grouped)) return(tibble::tibble())
  grouped %>%
    group_by(Pitch = PitchType) %>%
    group_split() %>%
    purrr::map_dfr(function(g) {
      s <- hsp_summary(g)
      tibble::tibble(
        Pitch = g$PitchType[[1]], `#` = nrow(g), `Usage%` = nrow(g) / total,
        `Avg Velo` = hsp_mean(g$RelSpeed), `Zone%` = hsp_mean(g$InZone),
        `Swing%` = s$SwingRate,
        `Whiff%` = hsp_rate(sum(g$WhiffP, na.rm = TRUE), sum(g$Swing, na.rm = TRUE)),
        `Chase%` = s$ChaseRate, `Contact%` = s$ContactRate,
        `Hard Hit%` = s$HardHitRate, `Avg EV` = s$AvgEV,
        OPS = s$OPS, xwOBAcon = s$xwOBAcon
      )
    }) %>%
    arrange(desc(`#`))
}

hsp_count_table <- function(d) {
  if (is.null(d) || !nrow(d)) return(tibble::tibble())
  situations <- list(
    `First Pitch` = d %>% filter(FirstPitch),
    `Hitter Ahead` = d %>% filter(Balls > Strikes, !TwoStrike, !FirstPitch),
    Even = d %>% filter(Balls == Strikes, !FirstPitch),
    `Pitcher Ahead` = d %>% filter(Strikes > Balls, !TwoStrike),
    `Two Strikes` = d %>% filter(TwoStrike)
  )
  purrr::imap_dfr(situations, function(g, label) {
    s <- hsp_summary(g)
    tibble::tibble(
      Situation = label, `#` = nrow(g), `Swing%` = s$SwingRate,
      `Whiff%` = hsp_rate(sum(g$WhiffP, na.rm = TRUE), sum(g$Swing, na.rm = TRUE)),
      `Chase%` = s$ChaseRate, `Contact%` = s$ContactRate,
      `Hard Hit%` = s$HardHitRate, `Avg EV` = s$AvgEV, xwOBAcon = s$xwOBAcon
    )
  })
}

hsp_launch_plot <- function(d) {
  bbe <- d %>% filter(BBE, is.finite(Angle), Angle >= -60, Angle <= 80)
  if (!nrow(bbe)) return(cape_pitcher_empty_heatmap_plot("No launch-angle data for this view."))
  ggplot2::ggplot(bbe, ggplot2::aes(x = Angle)) +
    ggplot2::geom_histogram(binwidth = 5, boundary = 0, fill = TEAM_CONFIG$colors$primary,
                            colour = "#FFFFFF", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = c(8, 32), colour = TEAM_CONFIG$colors$accent,
                        linewidth = 0.8, linetype = "dashed") +
    ggplot2::annotate("text", x = 20, y = Inf, label = "Sweet spot", vjust = 1.6,
                      colour = TEAM_CONFIG$colors$primary, fontface = "bold", size = 3.4) +
    ggplot2::labs(x = "Launch angle (degrees)", y = "Batted balls") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "#5F5152", face = "bold"),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA)
    )
}

hsp_spray_plot <- function(d) {
  bbe <- d %>%
    filter(BBE, is.finite(Bearing), is.finite(Distance)) %>%
    mutate(
      spray_x = Distance * sin(Bearing * pi / 180),
      spray_y = Distance * cos(Bearing * pi / 180),
      Result = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), "Hit", "Out / other")
    )
  if (!nrow(bbe)) return(cape_pitcher_empty_heatmap_plot("No batted-ball coordinates for this view."))

  theta <- seq(-45, 45, length.out = 181) * pi / 180
  wall <- tibble::tibble(x = 400 * sin(theta), y = 400 * cos(theta))
  ggplot2::ggplot() +
    ggplot2::geom_path(data = wall, ggplot2::aes(x, y), colour = "#D8CFCA", linewidth = 0.8) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = -283, yend = 283, colour = "#D8CFCA") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = 283, yend = 283, colour = "#D8CFCA") +
    ggplot2::geom_point(
      data = bbe,
      ggplot2::aes(spray_x, spray_y, colour = Result, size = ExitSpeed),
      alpha = 0.72
    ) +
    ggplot2::scale_colour_manual(values = c("Hit" = TEAM_CONFIG$colors$primary, "Out / other" = TEAM_CONFIG$colors$accent)) +
    ggplot2::scale_size_continuous(range = c(2.5, 6), guide = "none") +
    ggplot2::coord_fixed(xlim = c(-310, 310), ylim = c(-10, 420), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(colour = "#5F5152", size = 10),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA)
    )
}

hsp_table <- function(df, pct = character(), d3 = character(), d1 = character(),
                      int = character(), bar_pct = character()) {
  cape_pitcher_make_table(
    df, pct = pct, d3 = d3, d1 = d1, int = int,
    bar_pct = bar_pct, grade_cols = character()
  )
}

hsp_table_card <- function(title, subtitle, output_id, wide = TRUE) {
  card(
    class = paste("hsp-table-card", if (wide) "hsp-table-card-wide" else ""),
    cape_pitcher_table_header(title, subtitle),
    card_body(class = "hsp-table-body", reactableOutput(output_id))
  )
}

hitter_scouting_page_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-scout-page",
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "hitter-scouting.css?v=2")),
      tags$div(
        class = "hsp-page-intro",
        tags$div(class = "hsp-kicker", paste(TEAM_CONFIG$name, "Opponent Preparation")),
        tags$h2("Hitter Scouting", class = "hsp-page-title"),
        tags$p(
          "Build an evidence-based plan for opposing hitters from 2026 college performance, swing decisions, contact quality, pitch-family results, and location tendencies.",
          class = "hsp-page-copy"
        )
      ),
      tags$div(
        id = "hsp-page",
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Select Team & Hitter"),
            card_body(
              class = "hsp-control-stack",
              selectInput("hsp_team", "College team", choices = NULL, width = "100%", selectize = TRUE),
              selectInput("hsp_hitter", "Hitter", choices = NULL, width = "100%", selectize = TRUE),
              checkboxInput("hsp_include_cape", "Include matched 2026 Cape Cod League pitches", value = TRUE),
              uiOutput("hsp_source_note"),
              tags$div(class = "hsp-status-wrap", uiOutput("hsp_status"))
            )
          ),
          card(
            card_header("Hitter Snapshot"),
            card_body(tags$div(class = "hsp-meta-card", uiOutput("hsp_meta")))
          )
        ),
        card(
          class = "hsp-view-card",
          card_body(
            tags$div(
              class = "hsp-view-row",
              tags$div(
                tags$div(class = "hsp-view-label", "Scouting view"),
                tags$p("This split drives the statline, visuals, and detailed tables.")
              ),
              radioButtons(
                "hsp_hand", NULL,
                choices = c("All Pitchers" = "ALL", "vs RHP" = "R", "vs LHP" = "L"),
                selected = "ALL", inline = TRUE
              )
            )
          )
        ),
        card(card_header("Season Profile"), card_body(uiOutput("hsp_statline"))),
        tags$div(class = "hsp-section-label", "Location & Contact Profile"),
        tags$p(
          "Filter the location surfaces independently while retaining the pitcher-hand view selected above.",
          class = "hsp-section-copy"
        ),
        card(
          class = "hsp-filter-card",
          card_header("Visual Filters"),
          card_body(
            layout_columns(
              col_widths = c(7, 5),
              selectizeInput(
                "hsp_pitch_groups", "Pitch families", choices = NULL, multiple = TRUE,
                options = list(plugins = list("remove_button"), placeholder = "All pitch families")
              ),
              selectizeInput(
                "hsp_counts", "Counts", choices = NULL, multiple = TRUE,
                options = list(plugins = list("remove_button"), placeholder = "All counts")
              )
            ),
            uiOutput("hsp_filter_summary")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "hsp-visual-card",
            card_header("Pitch Location Density"),
            card_body(tags$p("Where pitchers have attacked this hitter.", class = "hsp-card-copy"),
                      plotOutput("hsp_location_plot", height = "350px"))
          ),
          card(
            class = "hsp-visual-card",
            card_header("Whiff Rate Surface"),
            card_body(tags$p("Where his swings have been most vulnerable.", class = "hsp-card-copy"),
                      plotOutput("hsp_whiff_plot", height = "350px"))
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            class = "hsp-visual-card",
            card_header("Exit Velocity Surface"),
            card_body(tags$p("Contact damage by pitch location.", class = "hsp-card-copy"),
                      plotOutput("hsp_damage_plot", height = "320px"))
          ),
          card(
            class = "hsp-visual-card",
            card_header("xwOBAcon Surface"),
            card_body(tags$p("Expected quality on balls in play.", class = "hsp-card-copy"),
                      plotOutput("hsp_xwobacon_plot", height = "320px"))
          ),
          card(
            class = "hsp-visual-card",
            card_header("Expected Outcome Surface"),
            card_body(tags$p("Expected plate-ending value by location.", class = "hsp-card-copy"),
                      plotOutput("hsp_xwoba_plot", height = "320px"))
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "hsp-visual-card",
            card_header("Spray Profile"),
            card_body(plotOutput("hsp_spray_plot", height = "390px"))
          ),
          card(
            class = "hsp-visual-card",
            card_header("Launch-Angle Profile"),
            card_body(plotOutput("hsp_launch_plot", height = "390px"))
          )
        ),
        tags$div(class = "hsp-section-label", "Scouting Tables"),
        tags$p(
          "Handedness, pitch family, pitch type, and count behavior are separated for quick game-plan review.",
          class = "hsp-section-copy"
        ),
        hsp_table_card(
          "Platoon Results", "Full-season production and approach against right- and left-handed pitching.",
          "hsp_hand_table"
        ),
        hsp_table_card(
          "Results by Pitch Family", "Fastball, breaking-ball, and offspeed performance in the active scouting view.",
          "hsp_family_table"
        ),
        hsp_table_card(
          "Results by Pitch Type", "Detailed outcomes against each individual pitch type in the active scouting view.",
          "hsp_pitch_table"
        ),
        hsp_table_card(
          "Count & Approach Tendencies", "Swing decisions and contact quality as the count changes.",
          "hsp_count_table"
        )
      )
    ),
    tags$div(class = "hub-footer", base_brand_footer())
  )
}

hitter_scouting_page_server <- function(input, output, session,
                                        catalog_loader = base_get_hitter_catalog,
                                        player_loader = base_load_hitter_rows,
                                        supplement_data = NULL) {
  catalog <- reactiveVal(NULL)
  raw_player <- reactiveVal(NULL)
  supplement_snapshot <- reactiveVal(NULL)
  loaded_key <- reactiveVal(NULL)
  initialized <- reactiveVal(FALSE)
  loading <- reactiveVal(FALSE)
  status_message <- reactiveVal("Hitter scouting data will load when this page is opened.")
  status_class <- reactiveVal("clean")

  observeEvent(input$base_nav, {
    if (!identical(input$base_nav, "tab_hitter_scouting") || initialized() || loading()) return()
    loading(TRUE)
    status_message("Preparing the 2026 hitter catalog...")
    tryCatch({
      catalog_value <- catalog_loader()
      if (is.null(catalog_value) || !nrow(catalog_value)) stop("No hitter catalog is available.")
      catalog(base_validate_hitter_catalog(catalog_value))
      supplement_value <- if (shiny::is.reactive(supplement_data)) {
        supplement_data()
      } else if (is.function(supplement_data)) {
        supplement_data()
      } else {
        supplement_data
      }
      supplement_snapshot(supplement_value)
      initialized(TRUE)
      status_message("Select an opposing hitter to load his 2026 pitch history.")
    }, error = function(e) {
      status_class("error")
      status_message(paste("Hitter scouting initialization failed:", conditionMessage(e)))
      showNotification(status_message(), type = "error")
    })
    loading(FALSE)
  }, ignoreInit = FALSE)

  hitter_catalog <- reactive({
    req(initialized())
    d <- catalog()
    req(!is.null(d), nrow(d) > 0)
    d %>%
      mutate(
        TeamDisplay = cape_pitcher_team_name(BatterTeam),
        HitterDisplay = cape_pitcher_format_pitcher_name(Batter)
      ) %>%
      arrange(TeamDisplay, HitterDisplay, Batter)
  })

  observe({
    teams <- hitter_catalog() %>%
      distinct(BatterTeam, TeamDisplay) %>%
      arrange(TeamDisplay)
    current <- isolate(input$hsp_team)
    selected <- if (!is.null(current) && current %in% teams$BatterTeam) current else base_team_default(teams$BatterTeam)
    if (!length(selected)) selected <- teams$BatterTeam[[1]]
    updateSelectizeInput(
      session, "hsp_team",
      choices = stats::setNames(teams$BatterTeam, teams$TeamDisplay),
      selected = selected, server = TRUE
    )
  })

  team_hitters <- reactive({
    req(input$hsp_team)
    hitter_catalog() %>% filter(BatterTeam == input$hsp_team)
  })

  observe({
    choices <- team_hitters()
    req(nrow(choices) > 0)
    current <- isolate(input$hsp_hitter)
    selected <- if (!is.null(current) && current %in% choices$Batter) current else choices$Batter[[1]]
    labels <- paste0(
      choices$HitterDisplay,
      ifelse(is.na(choices$BatterSide) | !nzchar(choices$BatterSide), "", paste0(" (", substr(choices$BatterSide, 1, 1), "HH)"))
    )
    updateSelectizeInput(
      session, "hsp_hitter",
      choices = stats::setNames(choices$Batter, labels),
      selected = selected, server = TRUE
    )
  })

  selected_key <- reactive({
    req(input$hsp_team, input$hsp_hitter)
    available <- team_hitters()
    req(nrow(available) > 0, input$hsp_hitter %in% available$Batter)
    paste(input$hsp_team, input$hsp_hitter, sep = "::")
  })

  observeEvent(selected_key(), {
    key <- selected_key()
    team <- input$hsp_team
    hitter <- input$hsp_hitter
    loaded_key(NULL)
    raw_player(NULL)
    status_class("clean")
    status_message("Loading the selected hitter's 2026 pitch history...")
    tryCatch({
      rows <- player_loader(team, hitter)
      if (is.null(rows) || !nrow(rows)) stop("No 2026 college pitches were found for this hitter.")
      if (!all(c("BatterTeam", "Batter") %in% names(rows))) {
        stop("Loaded hitter data is missing its team or player identifier.")
      }
      raw_player(tibble::as_tibble(rows))
      loaded_key(key)
      status_message("Scouting view ready.")
    }, error = function(e) {
      raw_player(NULL)
      loaded_key(NULL)
      status_class("error")
      status_message(paste("Hitter load failed:", conditionMessage(e)))
      showNotification(status_message(), type = "error")
    })
  }, ignoreInit = FALSE)

  college_rows <- reactive({
    key <- selected_key()
    req(identical(loaded_key(), key))
    d <- raw_player()
    req(!is.null(d), all(c("BatterTeam", "Batter") %in% names(d)))
    d %>% filter(BatterTeam == input$hsp_team, Batter == input$hsp_hitter)
  })

  player_rows <- reactive({
    primary <- college_rows()
    supplement <- supplement_snapshot()
    combined <- if (isTRUE(input$hsp_include_cape) && !is.null(supplement) && nrow(supplement)) {
      hsp_add_supplement(primary, supplement, input$hsp_hitter)
    } else {
      primary
    }
    hsp_prepare_data(combined)
  })

  view_data <- reactive({
    d <- player_rows()
    req(nrow(d) > 0)
    hsp_filter_hand(d, input$hsp_hand %||% "ALL")
  })

  observe({
    d <- view_data()
    groups <- c("Fastball", "Breaking Ball", "Offspeed")
    groups <- groups[groups %in% unique(d$PitchGroupDisplay)]
    counts <- cape_pitcher_count_levels[cape_pitcher_count_levels %in% unique(d$Count)]
    current_groups <- isolate(input$hsp_pitch_groups)
    current_counts <- isolate(input$hsp_counts)
    updateSelectizeInput(session, "hsp_pitch_groups", choices = groups,
                         selected = current_groups[current_groups %in% groups], server = TRUE)
    updateSelectizeInput(session, "hsp_counts", choices = counts,
                         selected = current_counts[current_counts %in% counts], server = TRUE)
  })

  heat_data <- reactive({
    d <- view_data()
    if (length(input$hsp_pitch_groups)) d <- d %>% filter(PitchGroupDisplay %in% input$hsp_pitch_groups)
    if (length(input$hsp_counts)) d <- d %>% filter(Count %in% input$hsp_counts)
    d
  })

  output$hsp_status <- renderUI({
    tags$div(class = paste("hsp-status", status_class()), status_message())
  })

  output$hsp_source_note <- renderUI({
    primary <- college_rows()
    supplement <- supplement_snapshot()
    cape_n <- if (is.null(supplement) || !nrow(supplement)) 0L else nrow(
      base_player_supplement_rows(primary, supplement, input$hsp_hitter, role = "Batter")
    )
    tags$p(
      paste0(
        format(nrow(primary), big.mark = ","), " college pitches",
        if (cape_n > 0) paste0(" + ", format(cape_n, big.mark = ","), " matched Cape pitches") else "; no matched Cape pitches"
      ),
      class = "hsp-helper"
    )
  })

  output$hsp_meta <- renderUI({
    d <- player_rows()
    req(nrow(d) > 0)
    s <- hsp_summary(d)
    side <- unique(d$BatSide[!is.na(d$BatSide)])
    side_label <- if (length(side)) paste0(side[[1]], "HH") else "Hand unavailable"
    dates <- suppressWarnings(as.Date(as.character(d$Date)))
    dates <- dates[!is.na(dates)]
    tags$div(
      tags$div(class = "hsp-meta-name", cape_pitcher_format_pitcher_name(input$hsp_hitter)),
      tags$div(
        class = "hsp-meta-line",
        paste(c(cape_pitcher_team_name(input$hsp_team), side_label,
                paste0(s$Games, " game(s)"), paste0(s$PA, " PA")), collapse = " | ")
      ),
      tags$div(
        class = "hsp-meta-detail",
        if (length(dates)) paste("Data through", format(max(dates), "%B %d, %Y")) else TEAM_CONFIG$season_label
      ),
      tags$div(class = "hsp-meta-badge", paste(hsp_hand_label(input$hsp_hand %||% "ALL"), "active"))
    )
  })

  output$hsp_statline <- renderUI({ hsp_statline_ui(view_data()) })

  output$hsp_filter_summary <- renderUI({
    d <- heat_data()
    tags$div(
      class = "hsp-filter-summary",
      tags$strong(format(nrow(d), big.mark = ",")),
      " pitches in the active visual filter",
      tags$span(" | "),
      tags$span(hsp_hand_label(input$hsp_hand %||% "ALL"))
    )
  })

  hitter_outline_side <- reactive({
    d <- player_rows()
    side <- unique(d$BatSide[!is.na(d$BatSide)])
    if (length(side) && side[[1]] %in% c("L", "R")) side[[1]] else "ALL"
  })

  output$hsp_location_plot <- renderPlot({
    cape_pitcher_zone_heat_plot(heat_data(), "freq", hitter_outline_side())
  })
  output$hsp_whiff_plot <- renderPlot({
    cape_pitcher_zone_heat_plot(heat_data(), "whiff", hitter_outline_side())
  })
  output$hsp_damage_plot <- renderPlot({
    cape_pitcher_zone_heat_plot(heat_data(), "damage", hitter_outline_side())
  })
  output$hsp_xwobacon_plot <- renderPlot({
    cape_pitcher_zone_heat_plot(heat_data(), "xwoba", hitter_outline_side())
  })
  output$hsp_xwoba_plot <- renderPlot({
    cape_pitcher_zone_heat_plot(heat_data(), "xwobafull", hitter_outline_side())
  })
  output$hsp_spray_plot <- renderPlot({ hsp_spray_plot(view_data()) })
  output$hsp_launch_plot <- renderPlot({ hsp_launch_plot(view_data()) })

  output$hsp_hand_table <- renderReactable({
    hsp_table(
      hsp_hand_split_table(player_rows()),
      pct = c("K%", "BB%", "Chase%", "Contact%", "Hard Hit%"),
      d3 = c("AVG", "OBP", "SLG", "OPS", "xwOBA", "xwOBAcon"),
      d1 = "Avg EV", int = c("PA", "Pitches")
    )
  })
  output$hsp_family_table <- renderReactable({
    hsp_table(
      hsp_pitch_group_table(view_data()),
      pct = c("Usage%", "Swing%", "Whiff%", "Chase%", "Contact%", "Hard Hit%"),
      d3 = c("OPS", "xwOBAcon"), d1 = c("Avg Velo", "Avg EV"), int = "#",
      bar_pct = "Usage%"
    )
  })
  output$hsp_pitch_table <- renderReactable({
    hsp_table(
      hsp_pitch_type_table(view_data()),
      pct = c("Usage%", "Zone%", "Swing%", "Whiff%", "Chase%", "Contact%", "Hard Hit%"),
      d3 = c("OPS", "xwOBAcon"), d1 = c("Avg Velo", "Avg EV"), int = "#",
      bar_pct = "Usage%"
    )
  })
  output$hsp_count_table <- renderReactable({
    hsp_table(
      hsp_count_table(view_data()),
      pct = c("Swing%", "Whiff%", "Chase%", "Contact%", "Hard Hit%"),
      d3 = "xwOBAcon", d1 = "Avg EV", int = "#"
    )
  })
}
