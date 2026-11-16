# The same editor, persistence and tracking server powers both applications.
development_server <- function(id, rows, player_column, seasons, summarize, percentile, mode,
                               average = function(metric) NA_real_,
                               lower_better = function(metric) FALSE, store_path = NULL,
                               player_choices = function(players, season) players) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(store_path)) store_path <- development_store_path()
    team <- development_team_key()
    two <- shiny::reactiveVal(FALSE)
    expanded <- shiny::reactiveVal(NULL)
    expected_revision <- shiny::reactiveVal(0L)
    loaded_record <- shiny::reactiveVal(NULL)
    local_refresh <- shiny::reactiveVal(0L)
    poll <- shiny::reactivePoll(2000, session,
      checkFunc = function() development_store_stamp(store_path, mode, team),
      valueFunc = function() development_store_stamp(store_path, mode, team))
    records <- shiny::reactive({
      poll()
      local_refresh()
      tryCatch(list(plans = development_store_read(store_path, mode, team), error = NULL),
        error = function(e) list(plans = list(), error = conditionMessage(e)))
    })
    summary_cache <- new.env(parent = emptyenv())
    cached_summary <- function(d) {
      key <- digest::digest(d)
      if (exists(key, summary_cache, inherits = FALSE)) return(get(key, summary_cache))
      value <- summarize(d)
      keys <- get0("keys", summary_cache, inherits = FALSE, ifnotfound = character())
      if (length(keys) >= 128) {
        rm(list = keys[1], envir = summary_cache)
        keys <- keys[-1]
      }
      assign(key, value, summary_cache)
      summary_cache$keys <- c(keys, key)
      value
    }
    season_rows <- shiny::reactive({
      shiny::req(input$season)
      development_rows_for_season(rows(), input$season)
    })
    player_key <- shiny::reactive({
      shiny::req(input$player)
      development_name_key(development_name(input$player))
    })
    player_rows <- shiny::reactive({
      d <- development_rows_for_season(rows(), input$season, player_column, player_key())
      shiny::req(nrow(d))
      d
    })
    stats <- shiny::reactive(cached_summary(player_rows()))
    game_series <- shiny::reactive(development_game_values(player_rows(), summarize, mode))
    shiny::observeEvent(input$add, two(TRUE))
    shiny::observeEvent(input$remove, two(FALSE))
    output$two <- shiny::renderText(if (two()) "true" else "false")
    shiny::outputOptions(output, "two", suspendWhenHidden = FALSE)

    shiny::observe({
      d <- season_rows()
      players <- sort(unique(as.character(d[[player_column]])))
      players <- players[!is.na(players) & nzchar(players)]
      saved <- development_effective_plans(records()$plans, input$season)
      saved_names <- vapply(saved, function(p) p$player, character(1))
      keys <- vapply(players, function(p) development_name_key(development_name(p)), character(1))
      players <- c(players, saved_names[!vapply(saved_names, development_name_key, character(1)) %in% keys])
      players <- player_choices(players, input$season)
      current <- shiny::isolate(input$player)
      selected <- if (length(current) == 1 && current %in% players) current else head(players, 1)
      shiny::updateSelectInput(session, "player",
        choices = stats::setNames(players, vapply(players, development_name, character(1))), selected = selected)
    })
    tracking_choices <- shiny::reactive({
      codes <- unname(development_baseline_seasons(seasons))
      column <- intersect(c("SeasonTag", "SeasonGroup"), names(rows()))[1]
      present <- if (!is.na(column)) unique(as.character(rows()[[column]])) else character()
      codes <- unique(c(codes, present, vapply(records()$plans, function(p) p$baseline_season, character(1))))
      codes <- codes[is.finite(development_season_rank(codes)) & development_season_rank(codes) >= development_season_rank("S26")]
      options <- unique(c("F26", vapply(codes, development_next_season, character(1))))
      options <- options[order(development_season_rank(options))]
      available <- intersect(options, present)
      default <- if (length(available)) tail(available[order(development_season_rank(available))], 1) else "F26"
      list(options = stats::setNames(options, development_season_label(options)), default = default)
    })
    tracking_initialized <- FALSE
    shiny::observe({
      choices <- tracking_choices()
      current <- shiny::isolate(input$tracking_season)
      selected <- if (tracking_initialized && length(current) == 1 && current %in% choices$options) current else choices$default
      shiny::updateSelectInput(session, "tracking_season", choices = choices$options, selected = selected)
      tracking_initialized <<- TRUE
    })
    baseline_initialized <- FALSE
    shiny::observe({
      choices <- tracking_choices()
      if (!baseline_initialized) {
        baseline <- development_previous_season(choices$default)
        if (baseline %in% unname(development_baseline_seasons(seasons))) {
          shiny::updateSelectInput(session, "season", selected = baseline)
        }
        baseline_initialized <<- TRUE
      }
    })

    load_editor <- function() {
      shiny::req(input$player, input$season)
      state <- records()
      if (!is.null(state$error)) return(invisible(NULL))
      eligible <- development_effective_plans(state$plans, input$season)
      matching <- Filter(function(p) identical(p$player_key, player_key()), eligible)
      saved <- if (length(matching)) matching[[1]] else NULL
      exact <- !is.null(saved) && identical(saved$baseline_season, input$season)
      expected_revision(if (exact) saved$revision else 0L)
      loaded_record(saved)
      two(!is.null(saved) && length(saved$objectives) == 2L)
      d <- development_rows_for_season(rows(), input$season, player_column, player_key())
      choices <- if (nrow(d)) names(cached_summary(d)) else unique(c(unname(development_specs(mode)),
        unlist(lapply(saved$objectives, function(o) o$metric))))
      for (i in 1:2) {
        o <- if (!is.null(saved) && length(saved$objectives) >= i) saved$objectives[[i]] else NULL
        metric <- if (!is.null(o)) o$metric else if ("K%" %in% choices) "K%" else head(choices, 1)
        shiny::updateSelectInput(session, paste0("stat", i), choices = choices, selected = metric)
        shiny::updateTextInput(session, paste0("title", i), value = if (!is.null(o)) o$title else "")
        shiny::updateTextInput(session, paste0("goal", i), value = if (!is.null(o)) o$goal else "")
        override <- if (!is.null(o) && !is.null(o$target_input)) o$target_input else NA_real_
        shiny::updateNumericInput(session, paste0("target", i), value = override)
        for (j in 1:3) shiny::updateTextAreaInput(session, paste0("support", i, "_", j),
          value = if (!is.null(o) && length(o$supports) >= j) o$supports[j] else "")
        for (j in 1:5) shiny::updateTextAreaInput(session, paste0("drill", i, "_", j),
          value = if (!is.null(o) && length(o$drills) >= j) o$drills[j] else "")
      }
      if(identical(mode,"hitting")) {
        defense <- saved$defense
        shiny::updateTextAreaInput(session,"defense_goal",value=if(is.null(defense)) "" else defense$goal)
        for(j in 1:3) shiny::updateTextAreaInput(session,paste0("defense_support",j),
          value=if(is.null(defense) || length(defense$supports)<j) "" else defense$supports[j])
        for(j in 1:5) shiny::updateTextAreaInput(session,paste0("defense_drill",j),
          value=if(is.null(defense) || length(defense$drills)<j) "" else defense$drills[j])
      }
      invisible(saved)
    }
    shiny::observeEvent(list(input$player, input$season), load_editor(), ignoreInit = FALSE, priority = 50)
    shiny::observeEvent(input$reload_saved, {
      local_refresh(shiny::isolate(local_refresh()) + 1L)
      load_editor()
    })
    for (i in 1:2) local({
      j <- i
      output[[paste0("current", j)]] <- shiny::renderUI({
        metric <- input[[paste0("stat", j)]]
        shiny::req(metric)
        shiny::div(class = "development-current", paste("Season value:", development_format(stats()[[metric]], metric)))
      })
    })
    plan <- shiny::reactive({
      values <- stats()
      specs <- development_specs(mode)
      rankings <- data.frame(label = names(specs), metric = unname(specs), stringsAsFactors = FALSE)
      rankings$value <- vapply(rankings$metric, function(m) if (m %in% names(values)) as.numeric(values[[m]]) else NA_real_, numeric(1))
      rankings$percentile <- mapply(percentile, rankings$value, rankings$metric)
      objectives <- lapply(seq_len(if (two()) 2 else 1), function(i) {
        metric <- input[[paste0("stat", i)]] %||% "K%"
        series <- game_series()
        series <- if (metric %in% names(series)) data.frame(GameIndex = series$GameIndex, GameLabel = series$GameLabel,
          Value = as.numeric(series[[metric]])) else data.frame(GameIndex = integer(), GameLabel = character(), Value = numeric())
        list(series = series, average = as.numeric(average(metric)), lower_better = isTRUE(lower_better(metric)),
          baseline_value = as.numeric(values[[metric]] %||% NA_real_),
          target_input = input[[paste0("target", i)]] %||% NA_real_,
          target = development_goal_target(input[[paste0("goal", i)]] %||% "", metric, input[[paste0("target", i)]] %||% NA_real_),
          title = input[[paste0("title", i)]] %||% "", metric = metric, current = development_format(values[[metric]], metric),
          goal = input[[paste0("goal", i)]] %||% "",
          supports = development_support_actions(vapply(1:3, function(j) input[[paste0("support", i, "_", j)]] %||% "", character(1))),
          drills = Filter(nzchar, vapply(1:5, function(j) trimws(input[[paste0("drill", i, "_", j)]] %||% ""), character(1))))
      })
      defense <- NULL
      if(identical(mode,"hitting")) {
        goal <- trimws(input$defense_goal %||% "")
        supports <- development_support_actions(vapply(1:3,function(j) input[[paste0("defense_support",j)]] %||% "",character(1)))
        drills <- Filter(nzchar,vapply(1:5,function(j) trimws(input[[paste0("defense_drill",j)]] %||% ""),character(1)))
        if(any(nzchar(c(goal,supports,drills)))) defense <- list(title="Defensive Goals",goal=goal,supports=supports,drills=drills)
      }
      list(player = development_name(input$player), player_key = player_key(), baseline_season = input$season,
        season = development_season_label(input$season), mode = mode, portrait = development_portrait(input$player),
        rankings = rankings, objectives = objectives, defense = defense)
    })
    errors <- shiny::reactive({
      p <- plan()
      problems <- development_payload_errors(p)
      if (any(!vapply(p$objectives, function(o) is.finite(o$baseline_value), logical(1)))) {
        problems <- c(problems, "Choose goal stats with a valid baseline-season value.")
      }
      problems
    })
    output$saved_status <- shiny::renderUI({
      shiny::req(input$season)
      state <- records()
      if (!is.null(state$error)) return(shiny::div(class = "development-saved-status", paste("Could not load saved goals:", state$error)))
      saved <- loaded_record()
      label <- paste("Goals for", development_season_label(development_next_season(input$season)))
      detail <- if (is.null(saved)) "No saved plan yet." else if (identical(saved$baseline_season, input$season)) {
        paste("Saved", saved$saved_at)
      } else paste("Carried forward from", development_season_label(saved$baseline_season), "\u2014 save to keep this season's plan.")
      shiny::div(class = "development-saved-status", paste(label, "\u00b7", detail))
    })
    output$status <- shiny::renderUI({
      if (!nrow(season_rows())) return(shiny::tags$p("No baseline-season data is available for this selection."))
      p <- plan()
      shiny::tagList(
        if (length(errors())) shiny::tags$p(class = "text-muted", paste(errors(), collapse = " ")),
        if (any(!is.finite(p$rankings$percentile))) shiny::tags$p(class = "text-muted", "N/A means the season value or matching D1 percentile reference is unavailable."),
        if (!nzchar(p$portrait)) shiny::tags$p(class = "text-muted", "No official roster headshot was matched; the report uses initials."))
    })
    output$export_control <- shiny::renderUI({
      if (!nrow(season_rows())) return(shiny::tags$button(class = "btn", disabled = "disabled", "Select a baseline season with data"))
      if (length(errors())) shiny::tags$button(class = "btn", disabled = "disabled", "Complete goals to save and generate PDF")
      else shiny::downloadButton(session$ns("pdf"), "Save and Generate PDF")
    })
    output$preview <- shiny::renderImage({
      p <- plan()
      file <- tempfile(fileext = ".png")
      grDevices::png(file, width = 1650, height = 1050, res = 108)
      tryCatch(development_draw(p), finally = grDevices::dev.off())
      list(src = file, contentType = "image/png", alt = paste(p$player, "Development Goals PDF preview"))
    }, deleteFile = TRUE)
    save_pdf <- function(file) {
      shiny::validate(shiny::need(!length(errors()), paste(errors(), collapse = " ")))
      p <- plan()
      development_pdf(file, p)
      record <- list(player = p$player, player_key = p$player_key, mode = mode, baseline_season = p$baseline_season,
        defense = p$defense,
        objectives = lapply(p$objectives, function(o) o[c("title", "metric", "goal", "target", "target_input", "lower_better", "baseline_value", "supports", "drills")]))
      saved <- development_store_save(store_path, record, expected_revision(), team)
      expected_revision(saved$revision)
      loaded_record(saved)
      local_refresh(shiny::isolate(local_refresh()) + 1L)
      shiny::showNotification(paste("Saved goals for", p$player), type = "message")
      invisible(saved)
    }
    output$pdf <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", plan()$player), "_Development_Goals.pdf"),
      content = function(file) {
        tryCatch(save_pdf(file), error = function(e) {
          shiny::showNotification(paste("Goals were not saved:", conditionMessage(e)), type = "error", duration = NULL)
          stop(conditionMessage(e), call. = FALSE)
        })
      })

    tracking <- shiny::reactive({
      shiny::req(input$tracking_season)
      development_tracking_models(records()$plans, input$tracking_season, rows(), player_column, cached_summary)
    })
    shiny::observeEvent(input$tracking_season, expanded(NULL))
    shiny::observeEvent(input$track_player, {
      expanded(if (identical(expanded(), input$track_player)) NULL else input$track_player)
    })
    output$tracking_note <- shiny::renderUI({
      shiny::req(input$tracking_season)
      shiny::div(class = "development-tracking-note",
        paste(development_season_label(development_previous_season(input$tracking_season)), "\u2192",
              development_season_label(input$tracking_season),
              "\u00b7 Green: goal hit. Red: farther from the goal. Black: improving or unchanged. Click a player to see trends."),
        if(identical(mode,"hitting")) shiny::tags$p("Goal-hit progress measures hitting stats. Defensive Goals are saved written coaching plans."))
    })
    output$tracking_table <- shiny::renderUI({
      state <- records()
      if (!is.null(state$error)) return(shiny::tags$p(paste("Could not load saved goals:", state$error)))
      models <- tracking()
      totals <- development_tracking_totals(models)
      defense_count <- sum(vapply(models,function(m) !is.null(m$plan$defense),logical(1)))
      total_row <- shiny::tags$tr(class = "development-team-total",
        shiny::tags$td("Team Total", shiny::div(paste(totals$players, if (totals$players == 1) "player" else "players"))),
        shiny::tags$td(paste(totals$hit, "of", totals$goals, "goals hit"),
          development_progress_bar(totals$fraction, "All team goals hit")),
        shiny::tags$td(if(mode=="hitting") "All stat goals; one-goal plans count once." else "All assigned goals; one-goal plans count once."),
        if(identical(mode,"hitting")) shiny::tags$td(paste(defense_count,if(defense_count==1L) "defensive plan saved" else "defensive plans saved")),
        shiny::tags$td(sprintf("%.1f%%", 100 * totals$fraction)))
      player_rows <- lapply(models, function(m) {
        open <- identical(expanded(), m$player_key)
        onclick <- paste0("Shiny.setInputValue(", jsonlite::toJSON(session$ns("track_player"), auto_unbox = TRUE), ",",
          jsonlite::toJSON(m$player_key, auto_unbox = TRUE), ",{priority:'event'})")
        hit_list <- shiny::div(class = "development-hit-list", lapply(seq_along(m$objectives), function(i) {
          p <- m$objectives[[i]]$progress
          shiny::tags$span(title = paste("Goal", i, p$state),
            shiny::tags$span(class = "development-hit-number", paste0(i, ": ")), if (p$hit) "\u2705" else "\u274c")
        }))
        row <- shiny::tags$tr(
          shiny::tags$td(shiny::tags$button(type = "button", class = "development-player-toggle",
            "aria-expanded" = if (open) "true" else "false", onclick = onclick, m$player)),
          development_tracking_goal_cell(m$objectives[[1]]),
          development_tracking_goal_cell(if (length(m$objectives) > 1) m$objectives[[2]] else NULL),
          if(identical(mode,"hitting")) {
            defense <- m$plan$defense
            if(is.null(defense)) shiny::tags$td(class="development-no-goal","\u2014") else
              shiny::tags$td(class="development-tracked-defense",shiny::tags$strong("Defensive Goals"),
                shiny::div(defense$goal),shiny::div(paste(length(defense$supports),if(length(defense$supports)==1L) "action ·" else "actions ·",length(defense$drills),"drills")))
          },
          shiny::tags$td(hit_list))
        defense <- if(identical(mode,"hitting")) m$plan$defense else NULL
        detail <- if (open) shiny::tags$tr(class = "development-tracking-detail", shiny::tags$td(colspan = if(mode=="hitting") 5 else 4,
          shiny::tags$strong(paste(m$player, "\u00b7", development_season_label(input$tracking_season))),
          shiny::div(class = "development-tracking-charts", lapply(seq_along(m$objectives),
            function(i) shiny::plotOutput(session$ns(paste0("tracking_plot", i)), height = "310px"))),
          if(!is.null(defense)) shiny::div(class="development-defense-details",
            shiny::tags$h4("Defensive Goals"),shiny::tags$p(defense$goal),
            shiny::tags$strong("Supporting actions"),shiny::tags$ul(lapply(defense$supports,shiny::tags$li)),
            if(length(defense$drills)) shiny::tagList(shiny::tags$strong("Drills"),shiny::tags$ul(lapply(defense$drills,shiny::tags$li))))))
        shiny::tagList(row, detail)
      })
      shiny::tagList(
        if (!length(models)) shiny::tags$p("No saved goals yet for this season. Save a plan in Build Goals to start tracking."),
        shiny::div(class = "development-tracking-scroll", shiny::tags$table(class = paste("development-tracking-table",if(mode=="hitting") "development-tracking-hitting"),
          shiny::tags$thead(shiny::tags$tr(lapply(c("Player", "Goal 1", "Goal 2", if(mode=="hitting") "Defensive Goals", "Goal hit"), shiny::tags$th))),
          shiny::tags$tbody(total_row, player_rows))))
    })
    for (i in 1:2) local({
      j <- i
      output[[paste0("tracking_plot", j)]] <- shiny::renderPlot({
        selected <- Filter(function(m) identical(m$player_key, expanded()), tracking())
        shiny::req(length(selected), length(selected[[1]]$objectives) >= j)
        m <- selected[[1]]
        o <- m$objectives[[j]]
        d <- development_rows_for_season(rows(), input$tracking_season, player_column, m$player_key)
        values <- development_game_values(d, summarize, mode)
        o$series <- if (o$metric %in% names(values)) data.frame(GameIndex = values$GameIndex,
          GameLabel = values$GameLabel, Value = values[[o$metric]]) else data.frame(GameIndex = integer(), GameLabel = character(), Value = numeric())
        o$average <- as.numeric(average(o$metric))
        development_chart(o, j) + ggplot2::theme(plot.title = ggplot2::element_text(size = 13),
          plot.subtitle = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10))
      }, res = 96)
    })
    list(plan = plan, stats = stats, two = two, game_series = game_series, records = records,
         loaded_record = loaded_record, tracking = tracking, save_pdf = save_pdf)
  })
}
