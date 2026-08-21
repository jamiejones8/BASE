# Defensive Analytics page. Positioning comes from the defensive tracking
# master; pitch, batter, contact, and result context comes from College26.

dap_positions <- c("1B", "2B", "3B", "SS", "LF", "CF", "RF")

dap_field_layers <- function(max_depth = 390) {
  foul <- data.frame(
    x = c(-max_depth, 0, 0, max_depth),
    y = c(max_depth, 0, 0, max_depth),
    group = c(1, 1, 2, 2)
  )
  diamond <- data.frame(
    x = c(0, 63.64, 0, -63.64, 0),
    y = c(0, 63.64, 127.28, 63.64, 0)
  )
  list(
    geom_path(data = foul, aes(x, y, group = group),
              inherit.aes = FALSE, colour = "#D7BD8A", linewidth = 0.75),
    geom_path(data = diamond, aes(x, y), inherit.aes = FALSE,
              colour = "#9A7B4F", linewidth = 0.75),
    annotate("path",
      x = 380 * sin(seq(-pi / 4, pi / 4, length.out = 120)),
      y = 380 * cos(seq(-pi / 4, pi / 4, length.out = 120)),
      colour = "#D8CFC2", linewidth = 0.65
    )
  )
}

dap_field_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(colour = "#E9E1D7", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "#FCFAF6", colour = NA),
      axis.title = element_text(colour = "#6C5550", face = "bold"),
      axis.text = element_text(colour = "#725E58"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

dap_empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, colour = "#806D67", size = 4.2) +
    xlim(-1, 1) + ylim(-1, 1) + theme_void()
}

dap_metric_card <- function(label, output_id, detail = NULL) {
  tags$div(
    class = "dap-metric-card",
    tags$div(class = "dap-metric-label", label),
    tags$div(class = "dap-metric-value", textOutput(output_id, container = tags$span)),
    if (!is.null(detail)) tags$div(class = "dap-metric-detail", detail)
  )
}

dap_panel <- function(title, description, ...) {
  tags$section(
    class = "dap-panel",
    tags$div(
      class = "dap-panel-head",
      tags$div(tags$h3(title), tags$p(description))
    ),
    tags$div(class = "dap-panel-body", ...)
  )
}

defense_page_ui <- function() {
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "defense-analytics.css")),
    tags$main(
      class = "base-page dap-page",
      tags$section(
        class = "dap-hero",
        tags$div(
          tags$div(class = "base-eyebrow", "2026 positioning + pitch context"),
          tags$h1("Defensive Analytics"),
          tags$p("Explore starting alignment, shifts, batted-ball direction, and opponent-specific positioning from one joined play record.")
        ),
        uiOutput("dap_runtime_status")
      ),
      tags$section(
        class = "dap-filter-panel",
        tags$div(class = "dap-filter-heading",
          tags$div(tags$span(class = "dap-filter-kicker", "VIEW CONTROLS"), tags$h2("Define the defensive sample")),
          tags$p("Every chart and table below follows these filters.")),
        tags$div(
          class = "dap-filter-grid",
          selectInput("dap_team", "Fielding team", choices = NULL),
          dateRangeInput("dap_dates", "Game dates", start = Sys.Date() - 180, end = Sys.Date()),
          selectInput("dap_position", "Position", choices = c("All positions", dap_positions)),
          selectInput("dap_player", "Defender", choices = "All defenders"),
          selectInput("dap_batter", "Opponent hitter", choices = "All hitters"),
          selectInput("dap_shift", "Alignment", choices = c("All alignments", "No shift", "Shift"))
        )
      ),
      uiOutput("dap_data_notice"),
      tags$section(
        class = "dap-metric-grid",
        dap_metric_card("Pitches", "dap_pitches"),
        dap_metric_card("Games", "dap_games"),
        dap_metric_card("Tracking complete", "dap_tracking"),
        dap_metric_card("Pitch match rate", "dap_match_rate"),
        dap_metric_card("Balls in play", "dap_bip"),
        dap_metric_card("Shift rate", "dap_shift_rate")
      ),
      tags$div(
        class = "dap-two-column dap-primary-grid",
        dap_panel(
          "Starting Alignment",
          "Each point is a defender's location at pitch release; labels mark the filtered average.",
          plotOutput("dap_alignment_plot", height = "520px")
        ),
        dap_panel(
          "Batted-Ball Context",
          "Contact locations and team outcomes from matched pitch-by-pitch records.",
          plotOutput("dap_contact_plot", height = "520px")
        )
      ),
      tags$section(
        class = "dap-attribution-intro",
        tags$div(
          tags$div(class = "base-eyebrow", "Phase 1 · Candidate engine"),
          tags$h2("Who most likely fielded the ball?"),
          tags$p("This first pass ranks geometric candidates without treating them as official assignments. The runner-up and distance margin remain visible so close calls are easy to identify.")
        ),
        tags$div(
          class = "dap-attribution-methods",
          tags$div(tags$strong("GROUND BALLS"), tags$span("Distance from each infielder to the projected spray-angle path")),
          tags$div(tags$strong("AIR BALLS"), tags$span("Distance from all seven defenders to the projected landing area")),
          tags$div(tags$strong("CONFIDENCE"), tags$span("Candidate separation, absolute distance, and required travel speed"))
        )
      ),
      tags$div(
        class = "dap-attribution-metrics",
        dap_metric_card("Attributed BIP", "dap_attr_coverage"),
        dap_metric_card("High confidence", "dap_attr_high"),
        dap_metric_card("Needs review", "dap_attr_review")
      ),
      tags$div(
        class = "dap-two-column",
        dap_panel(
          "Candidate Summary",
          "Assignment volume by geometric method and confidence tier.",
          reactableOutput("dap_attribution_summary")
        ),
        dap_panel(
          "What the distances mean",
          "A smaller first-to-second margin means a more ambiguous play.",
          tags$div(class = "dap-attribution-key",
            tags$p(tags$strong("Ground distance:"), " feet from the defender's starting point to the projected batted-ball path."),
            tags$p(tags$strong("Air distance:"), " feet from the defender's starting point to the projected landing location."),
            tags$p(tags$strong("Required speed:"), " air-ball travel distance divided by recorded hang time; this is a demand estimate, not measured player speed."),
            tags$p(tags$strong("Current limitation:"), " bunts and missing trajectories remain unassigned. Pitcher and catcher starting coordinates are not available."))
        )
      ),
      dap_panel(
        "Play-by-Play Candidate Review",
        "The closest and second-closest candidates are shown together. No selections are saved or used for player value yet.",
        reactableOutput("dap_attribution_table")
      ),
      dap_panel(
        "Positioning Summary",
        "Typical depth and lateral location, spread, sample size, and shift usage by position.",
        reactableOutput("dap_position_table")
      ),
      tags$div(
        class = "dap-two-column",
        dap_panel(
          "Alignment Usage",
          "How often each recorded alignment appears in the filtered sample.",
          reactableOutput("dap_shift_table")
        ),
        dap_panel(
          "Outcome Context",
          "Team results on balls in play. These are descriptive and are not credited to an individual defender.",
          reactableOutput("dap_outcome_table")
        )
      ),
      dap_panel(
        "Opponent-Specific Alignment",
        "Compare positioning against the selected hitter with the team's overall filtered baseline.",
        reactableOutput("dap_batter_table")
      ),
      tags$aside(
        class = "dap-method-note",
        tags$strong("How to read this page"),
        tags$p("Starting coordinates come from the defensive tracking file. Batter, count, pitch, contact, and result fields come from the 2026 pitch-by-pitch file. Records are matched by PitchUID first, with GameUID + pitch number + date as a controlled fallback. Date and time from both sources remain in the runtime for audit checks. Outcome tables describe the full defense behind a ball in play; they are not OAA, route efficiency, or individual fielding credit.")
      )
    )
  )
}

defense_page_server <- function(input, output, session) {
  team_choices <- sort(unique(base_defense_team_catalog$FieldingTeam))
  observe({
    default_team <- if (length(team_choices)) team_choices[[1]] else ""
    if (TEAM_CONFIG$data_code %in% team_choices) default_team <- TEAM_CONFIG$data_code
    updateSelectInput(session, "dap_team", choices = team_choices,
      selected = default_team)
  })

  output$dap_runtime_status <- renderUI({
    if (base_defense_available()) {
      tags$div(class = "dap-status dap-status-ready", tags$span(), "Joined runtime ready")
    } else {
      tags$div(class = "dap-status dap-status-waiting", tags$span(), "Defensive runtime not mounted")
    }
  })

  team_rows <- reactive({
    req(input$dap_team)
    base_load_defense_team(input$dap_team)
  })

  observeEvent(team_rows(), {
    rows <- team_rows()
    req(nrow(rows))
    dates <- range(rows$Date, na.rm = TRUE)
    if (all(is.finite(dates))) {
      updateDateRangeInput(session, "dap_dates", start = dates[[1]], end = dates[[2]], min = dates[[1]], max = dates[[2]])
    }
  }, ignoreInit = TRUE)

  observe({
    req(input$dap_team)
    players <- base_defense_player_catalog %>% filter(FieldingTeam == input$dap_team)
    if (!is.null(input$dap_position) && input$dap_position != "All positions") {
      players <- players %>% filter(Position == input$dap_position)
    }
    choices <- c("All defenders", sort(unique(players$Player[!is.na(players$Player)])))
    current <- isolate(input$dap_player)
    updateSelectInput(session, "dap_player", choices = choices,
      selected = if (!is.null(current) && current %in% choices) current else "All defenders")
  })

  observe({
    req(input$dap_team)
    batters <- base_defense_batter_catalog %>% filter(FieldingTeam == input$dap_team)
    choices <- c("All hitters", sort(unique(batters$Batter[!is.na(batters$Batter)])))
    current <- isolate(input$dap_batter)
    updateSelectInput(session, "dap_batter", choices = choices,
      selected = if (!is.null(current) && current %in% choices) current else "All hitters")
  })

  filtered_rows <- reactive({
    rows <- team_rows()
    if (!nrow(rows)) return(rows)
    if (!is.null(input$dap_dates) && length(input$dap_dates) == 2) {
      rows <- rows %>% filter(Date >= as.Date(input$dap_dates[[1]]), Date <= as.Date(input$dap_dates[[2]]))
    }
    if (!is.null(input$dap_batter) && input$dap_batter != "All hitters") rows <- rows %>% filter(Batter == input$dap_batter)
    if (!is.null(input$dap_shift) && input$dap_shift == "No shift") rows <- rows %>% filter(!IsShift)
    if (!is.null(input$dap_shift) && input$dap_shift == "Shift") rows <- rows %>% filter(IsShift)
    rows
  })

  positioning_long <- reactive({
    rows <- filtered_rows()
    if (!nrow(rows)) return(tibble::tibble())
    selected_positions <- if (!is.null(input$dap_position) && input$dap_position != "All positions") input$dap_position else dap_positions
    pieces <- lapply(selected_positions, function(pos) {
      tibble::tibble(
        DefenseRowId = rows$DefenseRowId,
        GameUID = rows$GameUID,
        Date = rows$Date,
        Position = pos,
        Player = as.character(rows[[paste0(pos, "_Name")]]),
        PlayerId = as.character(rows[[paste0(pos, "_Id")]]),
        Depth = as.numeric(rows[[paste0(pos, "_Depth")]]),
        Lateral = as.numeric(rows[[paste0(pos, "_Lateral")]]),
        IsShift = rows$IsShift
      )
    })
    long <- bind_rows(pieces) %>% filter(is.finite(Depth), is.finite(Lateral))
    if (!is.null(input$dap_player) && input$dap_player != "All defenders") long <- long %>% filter(Player == input$dap_player)
    long
  })

  attribution_rows <- reactive({
    base_geometric_attribution(filtered_rows())
  })

  output$dap_data_notice <- renderUI({
    rows <- filtered_rows()
    if (nrow(rows)) return(NULL)
    tags$div(class = "dap-empty-notice", "No records match the current filters.")
  })

  output$dap_pitches <- renderText(format(nrow(filtered_rows()), big.mark = ","))
  output$dap_games <- renderText(format(dplyr::n_distinct(filtered_rows()$GameUID, na.rm = TRUE), big.mark = ","))
  output$dap_tracking <- renderText({
    rows <- filtered_rows(); if (!nrow(rows)) return("—")
    paste0(round(mean(rows$FieldTrackingComplete %in% TRUE, na.rm = TRUE) * 100, 1), "%")
  })
  output$dap_match_rate <- renderText({
    rows <- filtered_rows(); if (!nrow(rows)) return("—")
    paste0(round(mean(rows$JoinMethod != "unmatched", na.rm = TRUE) * 100, 1), "%")
  })
  output$dap_bip <- renderText(format(sum(filtered_rows()$IsBIP %in% TRUE, na.rm = TRUE), big.mark = ","))
  output$dap_shift_rate <- renderText({
    rows <- filtered_rows(); if (!nrow(rows)) return("—")
    paste0(round(mean(rows$IsShift %in% TRUE, na.rm = TRUE) * 100, 1), "%")
  })

  output$dap_attr_coverage <- renderText({
    values <- attribution_rows(); if (!nrow(values)) return("—")
    paste0(round(mean(values$ConfidenceTier != "Unassigned") * 100, 1), "%")
  })
  output$dap_attr_high <- renderText({
    values <- attribution_rows(); assigned <- values %>% filter(ConfidenceTier != "Unassigned")
    if (!nrow(assigned)) return("—")
    paste0(round(mean(assigned$ConfidenceTier == "High") * 100, 1), "%")
  })
  output$dap_attr_review <- renderText({
    values <- attribution_rows(); if (!nrow(values)) return("—")
    format(sum(values$ConfidenceTier %in% c("Low", "Unassigned")), big.mark = ",")
  })

  output$dap_alignment_plot <- renderPlot({
    long <- positioning_long()
    if (!nrow(long)) return(dap_empty_plot("No tracked positioning in this sample"))
    centers <- long %>% group_by(Position) %>% summarise(Lateral = median(Lateral), Depth = median(Depth), .groups = "drop")
    ggplot(long, aes(Lateral, Depth, colour = Position)) +
      dap_field_layers(390) +
      geom_point(alpha = 0.16, size = 1.45) +
      geom_point(data = centers, size = 5, shape = 21, fill = "white", stroke = 1.1) +
      geom_text(data = centers, aes(label = Position), colour = "#501214", fontface = "bold", size = 3.2) +
      coord_fixed(xlim = c(-250, 250), ylim = c(-10, 400), clip = "off") +
      labs(x = "Lateral position (ft)", y = "Depth from home plate (ft)") +
      scale_colour_brewer(palette = "Dark2") + dap_field_theme()
  })

  output$dap_contact_plot <- renderPlot({
    rows <- filtered_rows() %>% filter(HasContactLocation)
    if (!nrow(rows)) return(dap_empty_plot("No matched batted-ball locations in this sample"))
    rows <- rows %>% mutate(Outcome = if_else(IsOut, "Out", "Reached / other"))
    ggplot(rows, aes(LandingLateral, LandingDepth, colour = Outcome)) +
      dap_field_layers(430) +
      geom_point(alpha = 0.55, size = 2) +
      coord_fixed(xlim = c(-300, 300), ylim = c(-10, 430), clip = "off") +
      labs(x = "Lateral landing location (ft)", y = "Distance from home plate (ft)") +
      scale_colour_manual(values = c("Out" = "#501214", "Reached / other" = "#C8943D")) + dap_field_theme()
  })

  output$dap_position_table <- renderReactable({
    long <- positioning_long()
    summary <- long %>% group_by(Position) %>% summarise(
      Pitches = n(), Games = n_distinct(GameUID),
      `Median lateral` = round(median(Lateral, na.rm = TRUE), 1),
      `Median depth` = round(median(Depth, na.rm = TRUE), 1),
      `Lateral spread` = round(stats::sd(Lateral, na.rm = TRUE), 1),
      `Depth spread` = round(stats::sd(Depth, na.rm = TRUE), 1),
      `Shift rate` = paste0(round(mean(IsShift %in% TRUE, na.rm = TRUE) * 100, 1), "%"), .groups = "drop")
    reactable(summary, striped = TRUE, highlight = TRUE, compact = TRUE, defaultPageSize = 10,
      columns = list(Position = colDef(name = "POS", sticky = "left"), Pitches = colDef(format = colFormat(separators = TRUE))))
  })

  output$dap_attribution_summary <- renderReactable({
    values <- attribution_rows()
    summary <- values %>% count(GeometryMode, ConfidenceTier, name = "Plays") %>% arrange(GeometryMode, desc(Plays))
    reactable(summary, striped = TRUE, compact = TRUE, pagination = FALSE,
      columns = list(GeometryMode = colDef(name = "METHOD"), ConfidenceTier = colDef(name = "CONFIDENCE"),
        Plays = colDef(format = colFormat(separators = TRUE))))
  })

  output$dap_attribution_table <- renderReactable({
    values <- attribution_rows() %>%
      mutate(
        Date = as.character(Date),
        `Primary candidate` = if_else(is.na(PrimaryPosition), "Unassigned", paste0(PrimaryPosition, " · ", coalesce(PrimaryPlayer, "Unknown"))),
        `Second candidate` = if_else(is.na(SecondPosition), "—", paste0(SecondPosition, " · ", coalesce(SecondPlayer, "Unknown"))),
        `1st distance` = round(PrimaryDistance, 1), `2nd distance` = round(SecondDistance, 1),
        `Margin` = round(CandidateMargin, 1), `Required ft/s` = round(RequiredSpeed, 1)
      ) %>%
      select(Date, Batter, HitType, PlayResult, GeometryMode, `Primary candidate`, `1st distance`,
        `Second candidate`, `2nd distance`, Margin, `Required ft/s`, ConfidenceTier, AttributionNote)
    reactable(values, striped = TRUE, highlight = TRUE, searchable = TRUE, defaultPageSize = 12,
      defaultSorted = "Date", defaultSortOrder = "desc",
      columns = list(
        Date = colDef(minWidth = 92), Batter = colDef(minWidth = 145), GeometryMode = colDef(name = "METHOD", minWidth = 125),
        `Primary candidate` = colDef(minWidth = 175), `Second candidate` = colDef(minWidth = 175),
        `1st distance` = colDef(name = "1ST DIST (FT)"), `2nd distance` = colDef(name = "2ND DIST (FT)"),
        `Required ft/s` = colDef(name = "REQ FT/S"), ConfidenceTier = colDef(name = "CONFIDENCE"),
        AttributionNote = colDef(name = "NOTE", minWidth = 250)
      ))
  })

  output$dap_shift_table <- renderReactable({
    rows <- filtered_rows() %>% mutate(Alignment = coalesce(DetectedShift, "Unclassified"))
    summary <- rows %>% count(Alignment, name = "Pitches", sort = TRUE) %>% mutate(`Share` = Pitches / sum(Pitches))
    reactable(summary, striped = TRUE, compact = TRUE, pagination = FALSE,
      columns = list(Pitches = colDef(format = colFormat(separators = TRUE)), Share = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })

  output$dap_outcome_table <- renderReactable({
    rows <- filtered_rows() %>% filter(IsBIP) %>% mutate(Result = coalesce(PlayResult, "Unclassified"))
    summary <- rows %>% group_by(Result) %>% summarise(
      `Balls in play` = n(), `Share` = n() / nrow(rows),
      `Avg exit velo` = round(mean(ExitSpeed, na.rm = TRUE), 1), .groups = "drop") %>% arrange(desc(`Balls in play`))
    reactable(summary, striped = TRUE, compact = TRUE, pagination = FALSE,
      columns = list(`Balls in play` = colDef(format = colFormat(separators = TRUE)), Share = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })

  output$dap_batter_table <- renderReactable({
    rows <- filtered_rows()
    long <- positioning_long()
    if (is.null(input$dap_batter) || input$dap_batter == "All hitters") {
      return(reactable(data.frame(Status = "Select an opponent hitter to compare their alignments with the team baseline."), pagination = FALSE))
    }
    baseline_rows <- team_rows()
    if (!is.null(input$dap_dates) && length(input$dap_dates) == 2) baseline_rows <- baseline_rows %>%
      filter(Date >= as.Date(input$dap_dates[[1]]), Date <= as.Date(input$dap_dates[[2]]))
    baseline_long <- bind_rows(lapply(dap_positions, function(pos) tibble::tibble(
      Position = pos,
      Lateral = as.numeric(baseline_rows[[paste0(pos, "_Lateral")]]),
      Depth = as.numeric(baseline_rows[[paste0(pos, "_Depth")]])
    ))) %>% filter(is.finite(Lateral), is.finite(Depth))
    hitter <- long %>% group_by(Position) %>% summarise(
      `Hitter pitches` = n(), `Hitter lateral` = median(Lateral), `Hitter depth` = median(Depth), .groups = "drop")
    baseline <- baseline_long %>% group_by(Position) %>% summarise(
      `Baseline lateral` = median(Lateral), `Baseline depth` = median(Depth), .groups = "drop")
    comparison <- left_join(hitter, baseline, by = "Position") %>% mutate(
      `Lateral difference` = round(`Hitter lateral` - `Baseline lateral`, 1),
      `Depth difference` = round(`Hitter depth` - `Baseline depth`, 1)
    ) %>% select(Position, `Hitter pitches`, `Lateral difference`, `Depth difference`)
    reactable(comparison, striped = TRUE, compact = TRUE, pagination = FALSE,
      columns = list(`Hitter pitches` = colDef(format = colFormat(separators = TRUE))))
  })
}
