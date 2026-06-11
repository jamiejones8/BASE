# =========================================================
#  BREWSTER WHITECAPS ANALYTICS
#  Bundled single-file data loader for leaderboard pages
# =========================================================

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(dplyr)

WHITE_CAPS_APP_DIR <- normalizePath(".", winslash = "/", mustWork = TRUE)
WHITE_CAPS_DATA_DIR <- file.path(WHITE_CAPS_APP_DIR, "data")
WHITE_CAPS_CACHE_DIR <- file.path(WHITE_CAPS_APP_DIR, "cache")
WHITE_CAPS_WWW_DIR <- file.path(WHITE_CAPS_APP_DIR, "www")
WHITE_CAPS_RESOURCE_PREFIX <- "whitecaps-assets"

if (!(WHITE_CAPS_RESOURCE_PREFIX %in% names(shiny::resourcePaths()))) {
  shiny::addResourcePath(WHITE_CAPS_RESOURCE_PREFIX, WHITE_CAPS_WWW_DIR)
}

whitecaps_asset_path <- function(path = "") {
  paste0(WHITE_CAPS_RESOURCE_PREFIX, "/", path)
}

# ---- BRAND CONFIG ----
source("helpers/brand_config.R", local = TRUE)

# ---- PAGE MODULES ----
source("pages/home_page.R", local = TRUE)
source("pages/pitching_page.R", local = TRUE)
source("pages/hitting_page.R", local = TRUE)
source("pages/process_page.R", local = TRUE)
source("pages/upload_page.R", local = TRUE)
source("pages/pitching_totals_page.R", local = TRUE)
source("pages/hitting_totals_page.R", local = TRUE)
source("pages/pitch_type_breakdown_page.R", local = TRUE)
source("pages/strength_of_competition_page.R", local = TRUE)

# ---- HELPERS ----
source("helpers/process_data.R", local = TRUE)
source("helpers/pitcher_times_through_order.R", local = TRUE)
source("helpers/calculate_pitcher_stats.R", local = TRUE)
source("helpers/calculate_hitter_stats.R", local = TRUE)
source("helpers/process_calculations.R", local = TRUE)
source("helpers/checkbox_loader.R", local = TRUE)
source("helpers/home_counts.R", local = TRUE)
source("helpers/pitch_type_summary.R", local = TRUE)
source("helpers/cache_loader.R", local = TRUE)
source("helpers/print_layouts.R", local = TRUE)

# ---- THEME ----
whitecaps_theme <- bs_theme(
  bg = "#f4f7f8",
  fg = "#172033",
  primary = "#06768A",
  secondary = "#21283D",
  base_font = font_google("Inter")
)

# =========================================================
#  UI
# =========================================================
ui <- fluidPage(
  theme = whitecaps_theme,
  useShinyjs(),
    tags$head(
      tags$title(APP_TITLE),
      tags$link(rel = "icon", type = "image/png", href = BRAND_LOGO_FILE),
      tags$link(
        id = "whitecaps-stylesheet",
        rel = "stylesheet",
        type = "text/css",
        href = whitecaps_asset_path("styles.css")
      ),
      tags$link(
        id = "whitecaps-font-bebas",
        href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&display=swap",
      rel = "stylesheet"
    ),
    tags$script(HTML("
      $(document).on('click', '.nav-item', function(e) {
        e.preventDefault();
        var id = $(this).attr('id');
        Shiny.setInputValue(id, Math.random());
      });
      $(document).on('click', '#nav_home_logo', function(e) {
        e.preventDefault();
        Shiny.setInputValue('nav_home', Math.random());
      });
      $(document).on('click', '#save_pdf', function(e) {
        e.preventDefault();
        window.print();
      });
    "))
  ),
  
  # ---- NAV BAR ----
  div(
    class = "top-nav",
    div(
      class = "logo-wrapper",
      a(
        href = "#",
        id = "nav_home_logo",
        class = "nav-logo-link",
        tags$img(src = BRAND_LOGO_FILE, class = "nav-logo")
      )
    ),
    div(
      class = "nav-links",
      a("CAPS Hub", href = "#", class = "nav-item", id = "nav_caps_hub"),
      a("Home", href = "#", class = "nav-item active", id = "nav_home"),
      a("Hitting", href = "#", class = "nav-item", id = "nav_hitting"),
      a("Pitching", href = "#", class = "nav-item", id = "nav_pitching"),
      a("Process", href = "#", class = "nav-item", id = "nav_process"),
      a("Pitch Type Breakdown", href = "#", class = "nav-item", id = "nav_pitchtype"),
      a("Strength of Comp", href = "#", class = "nav-item", id = "nav_soc"),  # ✅ NEW
      a("Hitting Totals", href = "#", class = "nav-item", id = "nav_hitter_rl"),
      a("Pitching Totals", href = "#", class = "nav-item", id = "nav_pitcher_rl"),
      a("Data Files", href = "#", class = "nav-item", id = "nav_upload")
    )
  ),

  uiOutput("pitcher_tto_filter_bar"),

  div(
    class = "page-actions-bar",
    div(
      class = "page-actions-inner",
      actionButton(
        inputId = "save_pdf",
        label = "Save Current Page as PDF",
        class = "txst-btn print-page-btn"
      )
    )
  ),
  
  # ---- PAGE BODY ----
  div(
    id = "print-target",
    div(class = "screen-page-body", uiOutput("page_body")),
    div(class = "print-page-body", uiOutput("print_page_body"))
  ),
  
  # ---- FOOTER ----
  div(class = "site-footer", p(APP_FOOTER_TEXT))
)

# =========================================================
#  SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ---- REACTIVES ----
  current_page          <- reactiveVal("home")
  combined_data         <- reactiveVal(NULL)   # full processed pitch-by-pitch from one bundled file
  pitching_data         <- reactiveVal(NULL)   # leaderboard output
  hitting_data          <- reactiveVal(NULL)   # leaderboard output
  process_data_reactive <- reactiveVal(NULL)   # process stats output
  precomputed_bundle    <- reactiveVal(NULL)   # optional precomputed leaderboard cache
  refresh_trigger       <- reactiveVal(runif(1))
  pitcher_tto_pages <- c("pitching", "process", "pitchtype", "pitcher_totals")
  pitcher_tto_choices <- c("All Times Through Order", PITCHER_TTO_LEVELS())
  
  # =========================================================
  #  LOAD BUNDLED DATA (ONLY on init + manual refresh)
  # =========================================================
  load_local_data <- function() {
    tryCatch({
      message("📥 Loading bundled TrackMan data from /data...")

      active_file <- pick_active_bundled_file()
      cache_result <- load_valid_leaderboards_cache(active_file)

      if (isTRUE(cache_result$valid)) {
        bundle <- cache_result$bundle
        combined_data(bundle$processed_raw)
        precomputed_bundle(bundle)
        message("⚡ Loaded precomputed leaderboards cache for ", active_file$label)
        return(invisible(NULL))
      }

      precomputed_bundle(NULL)
      if (!is.null(cache_result$reason) && nzchar(cache_result$reason)) {
        message("ℹ️ Precomputed cache unavailable: ", cache_result$reason)
      }

      data_list <- load_bundled_data_file(
        file_path = active_file$path,
        compute_summaries = FALSE
      )

      combined_data(data_list$raw)

      message("🏁 Bundled data loaded successfully using live calculations.")
    }, error = function(e) {
      message("❌ Error loading bundled data: ", e$message)
      stop(e)
    })
  }
  
  # ---- INITIAL LOAD + MANUAL REFRESH (Data Files page) ----
  observeEvent(refresh_trigger(), {
    load_local_data()
  }, ignoreInit = FALSE)

  # =========================================================
  #  DATA PIPELINE (drives ALL pages)
  # =========================================================
  filtered_combined <- reactive({
    combined_data()
  })
  
  filtered_team_pitching <- reactive({
    df <- filtered_combined()
    if (is.null(df) || nrow(df) == 0) return(df)
    get_team_pitching(df)
  })
  
  filtered_team_hitting <- reactive({
    df <- filtered_combined()
    if (is.null(df) || nrow(df) == 0) return(df)
    get_team_hitting(df)
  })

  selected_pitcher_tto <- reactive({
    choice <- input$pitcher_tto
    if (is.null(choice) || !nzchar(choice) || !choice %in% pitcher_tto_choices) {
      return(pitcher_tto_choices[[1]])
    }
    choice
  })

  get_cached_tto_table <- function(bundle, field, choice) {
    if (is.null(bundle) || !field %in% names(bundle)) {
      return(NULL)
    }

    table_list <- bundle[[field]]
    if (is.null(table_list) || !is.list(table_list) || !choice %in% names(table_list)) {
      return(NULL)
    }

    table_list[[choice]]
  }

  filtered_pitching_for_pages <- reactive({
    df <- filtered_team_pitching()
    if (is.null(df) || nrow(df) == 0) return(df)
    filter_pitcher_times_through_order(df, selected_pitcher_tto())
  })

  current_pitch_type_breakdown <- reactive({
    bundle <- precomputed_bundle()
    choice <- selected_pitcher_tto()
    cached <- get_cached_tto_table(bundle, "pitch_type_breakdown_by_tto", choice)

    if (!is.null(cached)) {
      return(cached)
    }

    filtered_pitching_for_pages()
  })
  
  # Recompute leaderboards/process whenever filtered data changes
  observeEvent(list(filtered_pitching_for_pages(), filtered_team_hitting(), selected_pitcher_tto(), precomputed_bundle()), {
    bp <- filtered_pitching_for_pages()
    bh <- filtered_team_hitting()
    bundle <- precomputed_bundle()
    choice <- selected_pitcher_tto()
    cached_pitching <- get_cached_tto_table(bundle, "pitching_stats_by_tto", choice)
    cached_process <- get_cached_tto_table(bundle, "process_stats_by_tto", choice)
    cached_hitting <- if (!is.null(bundle)) bundle$hitting_stats else NULL

    if (!is.null(cached_pitching) && !is.null(cached_process)) {
      pitching_data(
        cached_pitching %>%
          dplyr::left_join(cached_process, by = c("Pitcher", "Pitches"))
      )
      process_data_reactive(cached_process)
    } else if (is.null(bp) || nrow(bp) == 0) {
      pitching_data(NULL)
      process_data_reactive(NULL)
    } else {
      pitching_stats   <- calculate_pitching_stats(bp)
      pitching_process <- calculate_pitching_process_stats(bp)
      
      pitching_data(
        pitching_stats %>%
          dplyr::left_join(pitching_process, by = c("Pitcher", "Pitches"))
      )
      process_data_reactive(pitching_process)
    }

    if (!is.null(cached_hitting)) {
      hitting_data(cached_hitting)
    } else if (is.null(bh) || nrow(bh) == 0) {
      hitting_data(NULL)
    } else {
      hitting_data(calculate_hitter_stats(bh))
    }
  }, ignoreInit = FALSE)
  
  # =========================================================
  #  PAGE RENDERING
  # =========================================================
  output$pitcher_tto_filter_bar <- renderUI({
    if (!current_page() %in% pitcher_tto_pages) return(NULL)

    selected_choice <- isolate(input$pitcher_tto)
    if (is.null(selected_choice) || !selected_choice %in% pitcher_tto_choices) {
      selected_choice <- pitcher_tto_choices[[1]]
    }

    div(
      class = "season-filter-bar pitcher-tto-filter-bar",
      style = "padding: 0 24px 12px 24px; display:flex; justify-content:center;",
      div(
        class = "pitcher-tto-filter-inner",
        div(class = "pitcher-tto-filter-label", "Pitching Split"),
        div(
          class = "pitcher-tto-filter-select",
          selectInput(
            inputId = "pitcher_tto",
            label = NULL,
            choices = pitcher_tto_choices,
            selected = selected_choice,
            width = "320px",
            selectize = FALSE
          )
        )
      )
    )
  })

  output$page_body <- renderUI({
    switch(
      current_page(),
      "home"           = home_page_ui(),
      "hitting"        = hitting_page_ui("hit"),
      "pitching"       = pitching_page_ui("pitch"),
      "process"        = process_page_ui("process"),
      "pitchtype"      = pitch_type_breakdown_page_ui("pitchtype_page"),
      "soc"            = strength_of_competition_page_ui("soc_page"),  # ✅ NEW
      "hitting_totals" = hitting_totals_page_ui("hit_totals"),
      "pitcher_totals" = pitching_totals_page_ui("pitcher_totals"),
      "upload"         = upload_page_ui("upload_page")
    )
  })

  # =========================================================
  #  HOME PAGE COUNTING LEADERBOARDS
  # =========================================================
  pitcher_era_pa_min <- 3L
  pitcher_walks_pa_min <- 3L
  hitter_ops_pa_min <- 10L

  make_home_dt <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      df <- data.frame(Message = "No data available", stringsAsFactors = FALSE)
    }
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      class = "compact stripe hover",
      style = "bootstrap5"
    )
  }

  home_hitter_counts <- reactive({
    bundle <- precomputed_bundle()
    if (!is.null(bundle$home_counts$hitter_counts)) {
      return(bundle$home_counts$hitter_counts)
    }

    df <- filtered_team_hitting()
    build_home_hitter_counts(df)
  })

  home_pitcher_counts <- reactive({
    bundle <- precomputed_bundle()
    if (!is.null(bundle$home_counts$pitcher_counts)) {
      return(bundle$home_counts$pitcher_counts)
    }

    df <- filtered_team_pitching()
    build_home_pitcher_counts(df)
  })

  home_pitcher_walks_counts <- reactive({
    bundle <- precomputed_bundle()
    if (!is.null(bundle$home_counts$pitcher_walks_counts)) {
      return(bundle$home_counts$pitcher_walks_counts)
    }

    df <- filtered_team_pitching()
    build_home_pitcher_walks_counts(df)
  })

  output$home_hit_hits <- DT::renderDataTable({
    df <- home_hitter_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        arrange(desc(Hits), desc(PA), Batter) %>%
        slice_head(n = 10) %>%
        select(Batter, Hits, PA)
    )
  })

  output$home_hit_ops <- DT::renderDataTable({
    df <- home_hitter_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    out <- df %>%
      filter(PA >= hitter_ops_pa_min, !is.na(OPS)) %>%
      arrange(desc(OPS), desc(PA), Batter) %>%
      slice_head(n = 10) %>%
      transmute(Batter, OPS = sprintf("%.3f", OPS), PA)
    make_home_dt(out)
  })

  output$home_hit_walks <- DT::renderDataTable({
    df <- home_hitter_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        arrange(desc(Walks), desc(PA), Batter) %>%
        slice_head(n = 10) %>%
        select(Batter, Walks, PA)
    )
  })

  output$home_hit_hr <- DT::renderDataTable({
    df <- home_hitter_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        arrange(desc(HR), desc(PA), Batter) %>%
        slice_head(n = 10) %>%
        select(Batter, HR, PA)
    )
  })

  output$home_pitch_k <- DT::renderDataTable({
    df <- home_pitcher_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        arrange(desc(Strikeouts), desc(PA), Pitcher) %>%
        slice_head(n = 10) %>%
        select(Pitcher, Strikeouts, PA)
    )
  })

  output$home_pitch_era <- DT::renderDataTable({
    df <- home_pitcher_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    out <- df %>%
      filter(PA >= pitcher_era_pa_min, !is.na(ERA), Outs > 0) %>%
      arrange(ERA, desc(PA), Pitcher) %>%
      slice_head(n = 10) %>%
      transmute(Pitcher, ERA = sprintf("%.2f", ERA), IP, PA)
    make_home_dt(out)
  })

  output$home_pitch_bb_low <- DT::renderDataTable({
    df <- home_pitcher_walks_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        filter(PA >= pitcher_walks_pa_min) %>%
        arrange(Walks, desc(PA), Pitcher) %>%
        slice_head(n = 10) %>%
        select(Pitcher, Walks, PA)
    )
  })

  output$home_pitch_ip <- DT::renderDataTable({
    df <- home_pitcher_counts()
    if (is.null(df)) return(make_home_dt(NULL))
    make_home_dt(
      df %>%
        arrange(desc(Outs), desc(PA), Pitcher) %>%
        slice_head(n = 10) %>%
        select(Pitcher, IP, PA)
    )
  })

  # =========================================================
  #  PRINT-ONLY PDF LAYOUTS
  # =========================================================
  `%||%` <- function(x, y) if (is.null(x)) y else x

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  fmt_int <- function(x) {
    v <- safe_num(x)
    ifelse(
      is.na(v),
      "\u2014",
      format(round(v, 0), trim = TRUE, scientific = FALSE, big.mark = ",")
    )
  }

  fmt_plain <- function(x, digits = 1) {
    v <- safe_num(x)
    ifelse(is.na(v), "\u2014", sprintf(paste0("%.", digits, "f"), v))
  }

  fmt_pct <- function(x, digits = 1) {
    v <- safe_num(x)
    ifelse(is.na(v), "\u2014", sprintf(paste0("%.", digits, "f%%"), v))
  }

  fmt_dec <- function(x, digits = 3) {
    v <- safe_num(x)
    ifelse(is.na(v), "\u2014", sprintf(paste0("%.", digits, "f"), v))
  }

  fmt_mph <- function(x, digits = 1) {
    v <- safe_num(x)
    ifelse(is.na(v), "\u2014", sprintf(paste0("%.", digits, "f mph"), v))
  }

  fmt_text <- function(x) {
    value <- as.character(x)
    ifelse(is.na(value) | !nzchar(trimws(value)), "\u2014", value)
  }

  format_active_data_file <- function() {
    df <- combined_data()

    if (!is.null(df) && nrow(df) > 0 && "SourceFile" %in% names(df)) {
      labels <- unique(trimws(as.character(df$SourceFile)))
      labels <- labels[nzchar(labels)]
      if (length(labels) > 0) {
        return(labels[[1]])
      }
    }

    active_file <- pick_active_bundled_file()
    if (!is.null(active_file$label) && nzchar(active_file$label)) {
      return(active_file$label)
    }

    "No file loaded"
  }

  print_meta_items <- function(include_pitching_split = FALSE, extra_items = NULL) {
    items <- list(
      list(label = "Generated", value = format(Sys.time(), "%b %d, %Y %I:%M %p")),
      list(label = "Data File", value = format_active_data_file())
    )

    if (isTRUE(include_pitching_split)) {
      items <- c(items, list(list(label = "Pitching Split", value = selected_pitcher_tto())))
    }

    if (!is.null(extra_items) && length(extra_items)) {
      items <- c(items, extra_items)
    }

    items
  }

  print_leaderboard_card <- function(title, df, entity_col, value_col, formatter,
                                     descending = TRUE, sort_col = value_col,
                                     filter_fn = NULL, note = NULL) {
    print_table_card(
      title = title,
      content = build_print_table(
        build_ranked_print_table(
          df = df,
          entity_col = entity_col,
          value_col = value_col,
          formatter = formatter,
          descending = descending,
          n = 10,
          sort_col = sort_col,
          value_label = "Val",
          entity_label = "Player",
          filter_fn = filter_fn
        )
      ),
      note = note,
      extra_class = "print-card-compact"
    )
  }

  calculate_hitter_ppi_print <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    df %>%
      mutate(
        across(c(`K%`, `BB%`, `Barrel%`), safe_num),
        Z_K = (`K%` - mean(`K%`, na.rm = TRUE)) / sd(`K%`, na.rm = TRUE),
        Z_BB = (`BB%` - mean(`BB%`, na.rm = TRUE)) / sd(`BB%`, na.rm = TRUE),
        Z_Barrel = (`Barrel%` - mean(`Barrel%`, na.rm = TRUE)) / sd(`Barrel%`, na.rm = TRUE),
        RawPPI = (-0.5 * Z_K) + (0.5 * Z_BB) + (0.5 * Z_Barrel),
        HitterPPI = (1 / (1 + exp(-RawPPI))) * 0.9,
        HitterPPI = HitterPPI - 0.15,
        HitterPPI = pmin(pmax(HitterPPI, 0), 1.25),
        HitterPPI = round(HitterPPI, 3)
      ) %>%
      select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
  }

  calculate_pitcher_ppi_print <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    df %>%
      mutate(
        across(c(`K%`, `BB%`, `Barrel%`), safe_num),
        Z_K = (`K%` - mean(`K%`, na.rm = TRUE)) / sd(`K%`, na.rm = TRUE),
        Z_BB = (`BB%` - mean(`BB%`, na.rm = TRUE)) / sd(`BB%`, na.rm = TRUE),
        Z_Barrel = (`Barrel%` - mean(`Barrel%`, na.rm = TRUE)) / sd(`Barrel%`, na.rm = TRUE),
        RawPPI = (1.2 * Z_K) - (0.9 * Z_BB) - (0.9 * Z_Barrel),
        `PPI (ERA)` = round(4.50 - (0.5 * RawPPI), 2)
      ) %>%
      select(-Z_K, -Z_BB, -Z_Barrel, -RawPPI)
  }

  metric_leaders_table <- function(df, entity_col, metric_cfg) {
    rows <- lapply(metric_cfg, function(cfg) {
      work <- df
      if (is.null(work) || nrow(work) == 0) {
        return(NULL)
      }

      if (!is.null(cfg$filter_fn)) {
        work <- cfg$filter_fn(work)
      }

      sort_col <- cfg$sort_column %||% cfg$column
      value_col <- cfg$value_column %||% sort_col

      if (
        is.null(work) || nrow(work) == 0 ||
          !entity_col %in% names(work) ||
          !sort_col %in% names(work) ||
          !value_col %in% names(work)
      ) {
        return(NULL)
      }

      keep <- !is.na(safe_num(work[[sort_col]])) &
        !is.na(work[[entity_col]]) &
        nzchar(trimws(as.character(work[[entity_col]])))

      work <- work[keep, , drop = FALSE]
      if (!nrow(work)) return(NULL)

      values <- safe_num(work[[sort_col]])
      ord <- if (isTRUE(cfg$descending %||% TRUE)) {
        order(-values, as.character(work[[entity_col]]))
      } else {
        order(values, as.character(work[[entity_col]]))
      }

      work <- work[ord, , drop = FALSE]
      next_best <- "\u2014"
      if (nrow(work) >= 2) {
        next_best <- paste0(
          work[[entity_col]][2],
          " (",
          cfg$formatter(work[[value_col]][2]),
          ")"
        )
      }

      data.frame(
        Metric = cfg$label,
        Leader = as.character(work[[entity_col]][1]),
        Value = cfg$formatter(work[[value_col]][1]),
        `Next Best` = next_best,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    dplyr::bind_rows(rows)
  }

  build_snapshot_table <- function(df, entity_col, entity_label, column_cfg, sort_by,
                                   descending = TRUE, n = 10, sort_filter = NULL) {
    work <- df
    if (!is.null(sort_filter)) {
      work <- sort_filter(work)
    }

    if (
      is.null(work) || nrow(work) == 0 ||
        !entity_col %in% names(work) ||
        !sort_by %in% names(work)
    ) {
      return(data.frame())
    }

    sort_values <- safe_num(work[[sort_by]])
    keep <- !is.na(sort_values) &
      !is.na(work[[entity_col]]) &
      nzchar(trimws(as.character(work[[entity_col]])))

    work <- work[keep, , drop = FALSE]
    if (!nrow(work)) return(data.frame())

    sort_values <- safe_num(work[[sort_by]])
    ord <- if (isTRUE(descending)) {
      order(-sort_values, as.character(work[[entity_col]]))
    } else {
      order(sort_values, as.character(work[[entity_col]]))
    }

    work <- utils::head(work[ord, , drop = FALSE], n)
    out <- data.frame(
      `#` = seq_len(nrow(work)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out[[entity_label]] <- as.character(work[[entity_col]])

    for (cfg in column_cfg) {
      col_name <- cfg$column
      out[[cfg$label]] <- if (col_name %in% names(work)) {
        cfg$formatter(work[[col_name]])
      } else {
        rep("\u2014", nrow(work))
      }
    }

    out
  }

  summarize_pitch_types_for_print <- function(df) {
    prepare_pitch_type_breakdown_data(df)
  }

  build_home_print_ui <- function() {
    hitters <- home_hitter_counts()
    pitchers <- home_pitcher_counts()
    pitcher_walks <- home_pitcher_walks_counts()

    print_report_page(
      title = "Home Snapshot",
      subtitle = "Print view preserving the top-10 lists for each home-page leaderboard.",
      meta_items = print_meta_items(),
      body = print_card_grid(
        list(
          print_leaderboard_card(
            "OPS",
            hitters,
            entity_col = "Batter",
            value_col = "OPS",
            formatter = function(x) fmt_dec(x, 3),
            filter_fn = function(df) df %>% filter(PA >= hitter_ops_pa_min, !is.na(OPS))
          ),
          print_leaderboard_card("Hits", hitters, "Batter", "Hits", fmt_int),
          print_leaderboard_card("Walks", hitters, "Batter", "Walks", fmt_int),
          print_leaderboard_card("Home Runs", hitters, "Batter", "HR", fmt_int),
          print_leaderboard_card(
            "ERA",
            pitchers,
            entity_col = "Pitcher",
            value_col = "ERA",
            formatter = function(x) fmt_dec(x, 2),
            descending = FALSE,
            filter_fn = function(df) df %>% filter(PA >= pitcher_era_pa_min, !is.na(ERA), Outs > 0)
          ),
          print_leaderboard_card("Strikeouts", pitchers, "Pitcher", "Strikeouts", fmt_int),
          print_leaderboard_card(
            "Walks Allowed",
            pitcher_walks,
            entity_col = "Pitcher",
            value_col = "Walks",
            formatter = fmt_int,
            descending = FALSE,
            filter_fn = function(df) df %>% filter(PA >= pitcher_walks_pa_min)
          ),
          print_leaderboard_card(
            "Innings Pitched",
            pitchers,
            entity_col = "Pitcher",
            value_col = "IP",
            formatter = fmt_text,
            sort_col = "Outs"
          )
        ),
        columns = 4,
        extra_class = "print-grid-home"
      )
    )
  }

  build_hitting_print_ui <- function() {
    df <- calculate_hitter_ppi_print(hitting_data())

    print_report_page(
      title = "Hitting Leaderboards",
      subtitle = "Print view preserving the full top-10 list for each hitting leaderboard.",
      meta_items = print_meta_items(),
      body = print_card_grid(
        list(
          print_leaderboard_card("In-Zone Contact%", df, "Batter", "Z-Contact%", fmt_pct),
          print_leaderboard_card("Chase%", df, "Batter", "Chase%", fmt_pct, descending = FALSE),
          print_leaderboard_card("In-Zone Swing%", df, "Batter", "Z-Swing%", fmt_pct),
          print_leaderboard_card("Max Exit Velocity", df, "Batter", "MaxEV", fmt_mph),
          print_leaderboard_card("90th Percentile EV", df, "Batter", "P90EV", fmt_mph),
          print_leaderboard_card("Barrel%", df, "Batter", "Barrel%", fmt_pct),
          print_leaderboard_card("Hitter PPI", df, "Batter", "HitterPPI", function(x) fmt_dec(x, 3))
        ),
        columns = 3,
        extra_class = "print-grid-leaderboards"
      )
    )
  }

  build_pitching_print_ui <- function() {
    df <- calculate_pitcher_ppi_print(pitching_data())

    print_report_page(
      title = "Pitching Leaderboards",
      subtitle = "Print view preserving the full top-10 list for each pitching leaderboard.",
      meta_items = print_meta_items(include_pitching_split = TRUE),
      body = print_card_grid(
        list(
          print_leaderboard_card("Strikeout Rate (K%)", df, "Pitcher", "K%", fmt_pct),
          print_leaderboard_card("Walk Rate (BB%)", df, "Pitcher", "BB%", fmt_pct, descending = FALSE),
          print_leaderboard_card("Barrel Rate Allowed", df, "Pitcher", "Barrel%", fmt_pct, descending = FALSE),
          print_leaderboard_card("Max Velocity", df, "Pitcher", "MaxVelo", fmt_mph),
          print_leaderboard_card("Whiff Rate", df, "Pitcher", "Whiff%", fmt_pct),
          print_leaderboard_card("CSW%", df, "Pitcher", "CSW%", fmt_pct),
          print_leaderboard_card("Pitching Performance Index", df, "Pitcher", "PPI (ERA)", function(x) fmt_dec(x, 2), descending = FALSE)
        ),
        columns = 3,
        extra_class = "print-grid-leaderboards"
      )
    )
  }

  build_process_print_ui <- function() {
    df <- process_data_reactive()

    print_report_page(
      title = "Pitching Process Leaderboards",
      subtitle = "Print view preserving the full top-10 list for each pitching process leaderboard.",
      meta_items = print_meta_items(include_pitching_split = TRUE),
      body = print_card_grid(
        list(
          print_leaderboard_card("Strike%", df, "Pitcher", "Strike%", fmt_pct),
          print_leaderboard_card("Zone%", df, "Pitcher", "Zone%", fmt_pct),
          print_leaderboard_card("First Pitch Strike%", df, "Pitcher", "FirstPitchStrike%", fmt_pct),
          print_leaderboard_card("Early & Ahead%", df, "Pitcher", "EarlyAhead%", fmt_pct)
        ),
        columns = 4,
        extra_class = "print-grid-process"
      )
    )
  }

  build_hitting_totals_print_ui <- function() {
    df <- calculate_hitter_ppi_print(hitting_data())

    snapshot <- build_snapshot_table(
      df,
      entity_col = "Batter",
      entity_label = "Batter",
      sort_by = "HitterPPI",
      descending = TRUE,
      n = 10,
      column_cfg = list(
        list(column = "PA", label = "PA", formatter = fmt_int),
        list(column = "HitterPPI", label = "PPI", formatter = function(x) fmt_dec(x, 3)),
        list(column = "wOBA", label = "wOBA", formatter = function(x) fmt_dec(x, 3)),
        list(column = "K%", label = "K", formatter = fmt_pct),
        list(column = "BB%", label = "BB", formatter = fmt_pct),
        list(column = "Barrel%", label = "Barrel", formatter = fmt_pct),
        list(column = "Contact%", label = "Contact", formatter = fmt_pct),
        list(column = "P90EV", label = "P90EV", formatter = fmt_mph)
      )
    )

    stat_grid <- print_stat_grid(list(
      list(label = "Team wOBA", value = fmt_dec(mean(safe_num(df$wOBA), na.rm = TRUE), 3)),
      list(label = "Team PPI", value = fmt_dec(mean(safe_num(df$HitterPPI), na.rm = TRUE), 3)),
      list(label = "Team Contact", value = fmt_pct(mean(safe_num(df$`Contact%`), na.rm = TRUE))),
      list(label = "Team Z-Contact", value = fmt_pct(mean(safe_num(df$`Z-Contact%`), na.rm = TRUE))),
      list(label = "Team Barrel", value = fmt_pct(mean(safe_num(df$`Barrel%`), na.rm = TRUE))),
      list(label = "Team P90EV", value = fmt_mph(mean(safe_num(df$P90EV), na.rm = TRUE)))
    ))

    print_report_page(
      title = "Full Hitting Leaderboard",
      subtitle = "PDF summary rewritten for one page: team averages plus the top 10 hitters by Hitter PPI.",
      meta_items = print_meta_items(),
      body = tagList(
        print_table_card("Team Averages", stat_grid),
        print_table_card("Top 10 Hitters", build_print_table(snapshot))
      )
    )
  }

  build_pitching_totals_print_ui <- function() {
    df <- calculate_pitcher_ppi_print(pitching_data())

    snapshot <- build_snapshot_table(
      df,
      entity_col = "Pitcher",
      entity_label = "Pitcher",
      sort_by = "PPI (ERA)",
      descending = FALSE,
      n = 10,
      column_cfg = list(
        list(column = "PA", label = "PA", formatter = fmt_int),
        list(column = "PPI (ERA)", label = "PPI", formatter = function(x) fmt_dec(x, 2)),
        list(column = "K%", label = "K", formatter = fmt_pct),
        list(column = "BB%", label = "BB", formatter = fmt_pct),
        list(column = "Whiff%", label = "Whiff", formatter = fmt_pct),
        list(column = "CSW%", label = "CSW", formatter = fmt_pct),
        list(column = "Strike%", label = "Strike", formatter = fmt_pct),
        list(column = "Zone%", label = "Zone", formatter = fmt_pct),
        list(column = "MaxVelo", label = "MaxVelo", formatter = fmt_mph)
      )
    )

    stat_grid <- print_stat_grid(list(
      list(label = "Team PPI", value = fmt_dec(mean(safe_num(df$`PPI (ERA)`), na.rm = TRUE), 2)),
      list(label = "Team K%", value = fmt_pct(mean(safe_num(df$`K%`), na.rm = TRUE))),
      list(label = "Team BB%", value = fmt_pct(mean(safe_num(df$`BB%`), na.rm = TRUE))),
      list(label = "Team Whiff%", value = fmt_pct(mean(safe_num(df$`Whiff%`), na.rm = TRUE))),
      list(label = "Team Strike%", value = fmt_pct(mean(safe_num(df$`Strike%`), na.rm = TRUE))),
      list(label = "Team Zone%", value = fmt_pct(mean(safe_num(df$`Zone%`), na.rm = TRUE)))
    ))

    print_report_page(
      title = "Full Pitching Leaderboard",
      subtitle = "PDF summary rewritten for one page: team averages plus the top 10 pitchers by Pitching PPI.",
      meta_items = print_meta_items(include_pitching_split = TRUE),
      body = tagList(
        print_table_card("Team Averages", stat_grid),
        print_table_card("Top 10 Pitchers", build_print_table(snapshot))
      )
    )
  }

  build_pitch_type_print_ui <- function() {
    df <- summarize_pitch_types_for_print(current_pitch_type_breakdown())

    if (!is.null(df) && nrow(df) > 0) {
      top_two <- df %>%
        group_by(Pitcher) %>%
        arrange(desc(Pitches), .by_group = TRUE) %>%
        mutate(
          slot = dplyr::case_when(
            row_number() == 1 ~ "Primary",
            row_number() == 2 ~ "Secondary",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(slot)) %>%
        ungroup()

      pitcher_mix <- df %>%
        group_by(Pitcher) %>%
        summarise(
          `Total P` = sum(Pitches, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(`Total P`)) %>%
        slice_head(n = 10)

      wide_mix <- top_two %>%
        semi_join(pitcher_mix, by = "Pitcher") %>%
        select(Pitcher, slot, PitchType, Pitches, `Whiff%`, `CSW%`) %>%
        tidyr::pivot_wider(
          names_from = slot,
          values_from = c(PitchType, Pitches, `Whiff%`, `CSW%`),
          names_glue = "{slot}_{.value}"
        ) %>%
        right_join(pitcher_mix, by = "Pitcher") %>%
        arrange(desc(`Total P`)) %>%
        transmute(
          Pitcher,
          `Total P` = fmt_int(`Total P`),
          Primary = fmt_text(Primary_PitchType),
          `Pri P` = fmt_int(Primary_Pitches),
          `Pri Whiff` = fmt_pct(`Primary_Whiff%`),
          `Pri CSW` = fmt_pct(`Primary_CSW%`),
          Secondary = fmt_text(Secondary_PitchType),
          `Sec P` = fmt_int(Secondary_Pitches),
          `Sec Whiff` = fmt_pct(`Secondary_Whiff%`),
          `Sec CSW` = fmt_pct(`Secondary_CSW%`)
        )
    } else {
      wide_mix <- data.frame()
    }

    print_report_page(
      title = "Pitch Type Breakdown",
      subtitle = "PDF summary rewritten for one page using each pitcher's primary and secondary offerings.",
      meta_items = print_meta_items(include_pitching_split = TRUE),
      body = print_table_card(
        "Top 10 Pitchers By Volume",
        build_print_table(wide_mix),
        note = "Primary and secondary pitch mix shown to keep the PDF compact."
      )
    )
  }

  build_soc_print_ui <- function() {
    min_pa <- input[["soc_page-min_pa"]]
    if (is.null(min_pa)) min_pa <- 0

    pd <- filtered_combined()
    hitters_tbl <- data.frame()
    pitchers_tbl <- data.frame()

    if (!is.null(pd) && nrow(pd) > 0) {
      hitters_pd <- pd %>% filter(BatterTeam == ACTIVE_TEAM_CODE)
      pitchers_pd <- pd %>% filter(PitcherTeam == ACTIVE_TEAM_CODE)
      soc <- list(
        hitters = build_strength_of_comp_tables(hitters_pd, min_pa_qualify = min_pa)$hitters,
        pitchers = build_strength_of_comp_tables(pitchers_pd, min_pa_qualify = min_pa)$pitchers
      )

      hitters_tbl <- soc$hitters %>%
        select(Hitter, PA_Total, wOBA_Total, `wOBA vs LHP`, `wOBA vs RHP`,
               `wOBA vs Top`, `wOBA vs Middle`, `wOBA vs Bottom`) %>%
        arrange(desc(`wOBA vs Top`), desc(wOBA_Total)) %>%
        slice_head(n = 8) %>%
        mutate(
          PA_Total = fmt_int(PA_Total),
          wOBA_Total = fmt_dec(wOBA_Total, 3),
          `wOBA vs LHP` = fmt_dec(`wOBA vs LHP`, 3),
          `wOBA vs RHP` = fmt_dec(`wOBA vs RHP`, 3),
          `wOBA vs Top` = fmt_dec(`wOBA vs Top`, 3),
          `wOBA vs Middle` = fmt_dec(`wOBA vs Middle`, 3),
          `wOBA vs Bottom` = fmt_dec(`wOBA vs Bottom`, 3)
        )

      pitchers_tbl <- soc$pitchers %>%
        select(Pitcher, PA_Allowed_Total, wOBA_Allowed_Total,
               `wOBA Allowed vs Top`, `wOBA Allowed vs Middle`, `wOBA Allowed vs Bottom`) %>%
        arrange(`wOBA Allowed vs Top`, wOBA_Allowed_Total) %>%
        slice_head(n = 8) %>%
        mutate(
          PA_Allowed_Total = fmt_int(PA_Allowed_Total),
          wOBA_Allowed_Total = fmt_dec(wOBA_Allowed_Total, 3),
          `wOBA Allowed vs Top` = fmt_dec(`wOBA Allowed vs Top`, 3),
          `wOBA Allowed vs Middle` = fmt_dec(`wOBA Allowed vs Middle`, 3),
          `wOBA Allowed vs Bottom` = fmt_dec(`wOBA Allowed vs Bottom`, 3)
        )
    }

    print_report_page(
      title = "Strength of Competition",
      subtitle = "Condensed split summary focused on performance versus opponent tiers.",
      meta_items = print_meta_items(
        extra_items = list(list(label = "Min PA", value = as.character(min_pa)))
      ),
      body = print_two_column(
        print_table_card("Hitters", build_print_table(hitters_tbl)),
        print_table_card("Pitchers", build_print_table(pitchers_tbl))
      )
    )
  }

  build_upload_print_ui <- function() {
    files <- list_bundled_data_files() %>%
      mutate(Active = ifelse(row_number() == 1, "Yes", "")) %>%
      transmute(
        Active,
        File,
        Rows = fmt_int(Rows),
        `Size (MB)` = fmt_plain(SizeMB, 2),
        Modified
      )

    stat_grid <- print_stat_grid(list(
      list(label = "Active File", value = format_active_data_file()),
      list(label = "Files Found", value = fmt_int(nrow(files))),
      list(label = "Total Rows", value = fmt_int(sum(safe_num(gsub(",", "", files$Rows)), na.rm = TRUE)))
    ))

    print_report_page(
      title = "Data Files",
      subtitle = "Single-file bundled CSV inventory formatted for a one-page PDF export.",
      meta_items = print_meta_items(),
      body = tagList(
        print_table_card("Current Input", stat_grid),
        print_table_card("Bundled CSV Inventory", build_print_table(files))
      )
    )
  }

  output$print_page_body <- renderUI({
    switch(
      current_page(),
      "home" = build_home_print_ui(),
      "hitting" = build_hitting_print_ui(),
      "pitching" = build_pitching_print_ui(),
      "process" = build_process_print_ui(),
      "pitchtype" = build_pitch_type_print_ui(),
      "soc" = build_soc_print_ui(),
      "hitting_totals" = build_hitting_totals_print_ui(),
      "pitcher_totals" = build_pitching_totals_print_ui(),
      "upload" = build_upload_print_ui()
    )
  })
  outputOptions(output, "print_page_body", suspendWhenHidden = FALSE)
  
  # ---- NAVIGATION ----
  observeEvent(input$nav_home,       { current_page("home") })
  observeEvent(input$nav_hitting,    { current_page("hitting") })
  observeEvent(input$nav_pitching,   { current_page("pitching") })
  observeEvent(input$nav_process,    { current_page("process") })
  observeEvent(input$nav_pitchtype,  { current_page("pitchtype") })
  observeEvent(input$nav_soc,        { current_page("soc") })           # ✅ NEW
  observeEvent(input$nav_hitter_rl,  { current_page("hitting_totals") })
  observeEvent(input$nav_pitcher_rl, { current_page("pitcher_totals") })
  observeEvent(input$nav_upload,     { current_page("upload") })
  
  observe({
    runjs("$('.nav-item').removeClass('active');")
    
    # Map page keys to nav IDs (prevents mismatch issues)
    nav_map <- list(
      home           = "#nav_home",
      hitting        = "#nav_hitting",
      pitching       = "#nav_pitching",
      process        = "#nav_process",
      pitchtype      = "#nav_pitchtype",
      soc            = "#nav_soc",
      hitting_totals = "#nav_hitter_rl",
      pitcher_totals = "#nav_pitcher_rl",
      upload         = "#nav_upload"
    )
    
    active_id <- nav_map[[ current_page() ]]
    if (is.null(active_id)) active_id <- "#nav_home"
    
    runjs(sprintf("$(\"%s\").addClass('active');", active_id))
  })
  
  # =========================================================
  #  MODULE SERVERS
  # =========================================================
  pitching_page_server("pitch", pitching_data)
  hitting_page_server("hit", hitting_data)
  process_page_server("process", process_data_reactive)
  hitting_totals_page_server("hit_totals", hitting_data)
  pitching_totals_page_server("pitcher_totals", pitching_data)
  
  # ---- Pitch Type Breakdown ----
  pitch_type_breakdown_page_server("pitchtype_page", current_pitch_type_breakdown)
  
  # ---- Strength of Competition (uses FULL filtered combined; needs all hitters/pitchers for tiers) ----
  strength_of_competition_page_server("soc_page", pitch_data = filtered_combined)  # ✅ NEW
  
  # ---- Upload Page ----
  upload_page_server("upload_page", refresh_trigger)
}

# =========================================================
#  RUN APP
# =========================================================
shinyApp(ui, server)
