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

cape_pitcher_team_levels <- TEAM_CONFIG$data_code

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

cape_pitcher_team_name <- function(team_code) {
  code_chr <- as.character(team_code)
  out <- rep(NA_character_, length(code_chr))
  out[base_team_matches(code_chr)] <- TEAM_CONFIG$full_name
  if (exists("team_display_name", mode = "function", inherits = TRUE)) {
    unresolved <- is.na(out)
    out[unresolved] <- team_display_name(code_chr[unresolved])
  }
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

cape_pitcher_clean_pitch_type <- function(x) {
  x <- cape_pitcher_canonicalize_pitch(as.character(x))
  x <- trimws(x)
  x[x %in% c("", "Undefined", "NA")] <- NA_character_
  x
}

cape_pitcher_resolve_pitch_type <- function(tagged, auto, original_tagged) {
  tagged <- cape_pitcher_clean_pitch_type(tagged)
  auto <- cape_pitcher_clean_pitch_type(auto)
  original_tagged <- cape_pitcher_clean_pitch_type(original_tagged)

  # Keep the current tagged value sticky for the full session, even when a user
  # retags back to the pitch's original manual label after trying another tag.
  dplyr::coalesce(tagged, original_tagged, auto)
}

cape_pitcher_height_to_feet <- function(height_ft, height_in) {
  if (length(height_ft) == 0 || length(height_in) == 0 ||
      is.na(height_ft) || is.na(height_in)) {
    return(NA_real_)
  }
  as.numeric(height_ft) + (as.numeric(height_in) / 12)
}

cape_pitcher_arm_angle_value <- function(rel_height, rel_side, pitcher_throws,
                                         height_feet, set_position = 0) {
  if (length(rel_height) == 0 || length(rel_side) == 0 || length(pitcher_throws) == 0 ||
      is.na(rel_height) || is.na(rel_side) || is.na(height_feet) || !nzchar(pitcher_throws)) {
    return(NA_real_)
  }

  shoulder_height <- height_feet * 0.7
  adjusted_rel_side <- rel_side - set_position
  side_eff <- if (identical(pitcher_throws, "Left")) {
    -adjusted_rel_side
  } else {
    adjusted_rel_side
  }

  atan2(rel_height - shoulder_height, side_eff) * (180 / pi)
}

cape_pitcher_arm_angle_summary <- function(d, height_feet, set_position = 0) {
  if (is.null(d) || nrow(d) == 0 || is.na(height_feet)) {
    return(list(angle = NA_real_, rel_height = NA_real_, rel_side = NA_real_, throws = NA_character_))
  }

  rel_height <- mean(d$RelHeight, na.rm = TRUE)
  rel_side <- mean(d$RelSide, na.rm = TRUE)
  if (!is.finite(rel_height)) rel_height <- NA_real_
  if (!is.finite(rel_side)) rel_side <- NA_real_

  throws_vals <- as.character(d$PitcherThrows[!is.na(d$PitcherThrows) & nzchar(d$PitcherThrows)])
  throws <- if (length(throws_vals)) throws_vals[1] else NA_character_

  list(
    angle = cape_pitcher_arm_angle_value(rel_height, rel_side, throws, height_feet, set_position = set_position),
    rel_height = rel_height,
    rel_side = rel_side,
    throws = throws
  )
}

cape_pitcher_quick_btn <- function(id, label) {
  actionButton(
    id,
    label,
    class = "btn btn-default btn-sm",
    style = paste(
      "margin:0 8px 8px 0;",
      "border-color:#cfd8e3;",
      paste0("color:", TEAM_CONFIG$colors$primary, ";"),
      "font-weight:600;"
    )
  )
}

cape_pitcher_heatmap_filter_label <- function(values,
                                              fallback,
                                              singular,
                                              plural = paste0(singular, "s"),
                                              max_inline = 2) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]

  if (!length(values)) {
    return(fallback)
  }

  if (length(values) <= max_inline) {
    return(paste(values, collapse = " • "))
  }

  paste(length(values), plural)
}

cape_pitcher_read_parquet <- function(path = TEAM_CONFIG$data$season_file) {
  arrow::read_parquet(path) %>%
    tibble::as_tibble()
}

cape_pitcher_init_data <- function(path = TEAM_CONFIG$data$season_file, raw_df = NULL) {
  source_df <- if (!is.null(raw_df)) {
    tibble::as_tibble(raw_df)
  } else {
    cape_pitcher_read_parquet(path)
  }

  source_df %>%
    mutate(base_row_id = dplyr::row_number()) %>%
    base_prepare_retag_rows()
}

cape_pitcher_tag_snapshot <- function(d) {
  if (is.null(d) || !"TaggedPitchType" %in% names(d)) {
    return(character())
  }

  tags <- as.character(d$TaggedPitchType)
  tags[is.na(tags)] <- "__NA__"
  tags
}

cape_pitcher_prepare_view <- function(raw_df) {
  prep_pitches(raw_df) %>%
    {
      if (!"Top/Bottom" %in% names(.)) {
        if ("Top.Bottom" %in% names(.)) {
          .[["Top/Bottom"]] <- as.character(.[["Top.Bottom"]])
        } else {
          .[["Top/Bottom"]] <- NA_character_
        }
      }
      .
    } %>%
    mutate(
      TaggedPitchType = cape_pitcher_clean_pitch_type(TaggedPitchType),
      AutoPitchType = cape_pitcher_clean_pitch_type(AutoPitchType),
      OriginalTaggedPitchType = cape_pitcher_clean_pitch_type(OriginalTaggedPitchType),
      PitchType = cape_pitcher_resolve_pitch_type(
        TaggedPitchType,
        AutoPitchType,
        OriginalTaggedPitchType
      ),
      Count = dplyr::if_else(
        !is.na(Balls) & !is.na(Strikes),
        paste0(as.integer(Balls), "-", as.integer(Strikes)),
        NA_character_
      ),
      `Top/Bottom` = as.character(`Top/Bottom`),
      PitcherDisplay = cape_pitcher_format_pitcher_name(Pitcher),
      BatterDisplay = vapply(as.character(Batter), cape_pitcher_format_name, character(1)),
      TeamDisplay = cape_pitcher_team_name(PitcherTeam),
      CSW = PitchCall %in% c("StrikeCalled", "StrikeSwinging")
    )
}

cape_pitcher_summary <- function(d) {
  empty_summary <- list(
    Games = 0L,
    Pitches = 0L,
    PA = 0L,
    Outs = 0L,
    Hits = 0L,
    Runs = 0,
    Walks = 0L,
    Strikeouts = 0L,
    Homers = 0L,
    AVG = NA_real_,
    OBP = NA_real_,
    SLG = NA_real_,
    OPS = NA_real_,
    ERAEstimate = NA_real_,
    WHIP = NA_real_,
    CSW = NA_real_,
    Whiff = NA_real_,
    Chase = NA_real_,
    KRate = NA_real_,
    BBRate = NA_real_,
    KMinusBB = NA_real_,
    xwOBA = NA_real_
  )

  if (is.null(d) || nrow(d) == 0) {
    return(empty_summary)
  }

  pa_rows <- d %>% filter(PACheck %in% TRUE)
  if (nrow(pa_rows) == 0) {
    empty_summary$Games <- if ("GameUID" %in% names(d)) {
      length(unique(d$GameUID[!is.na(d$GameUID)]))
    } else {
      length(unique(d$Date[!is.na(d$Date)]))
    }
    empty_summary$Pitches <- nrow(d)
    empty_summary$CSW <- cape_pitcher_rate(sum(d$CSW, na.rm = TRUE), nrow(d))
    empty_summary$Whiff <- cape_pitcher_rate(sum(d$WhiffP, na.rm = TRUE), sum(d$Swing, na.rm = TRUE))
    empty_summary$Chase <- cape_pitcher_rate(sum(d$Chase, na.rm = TRUE), sum(d$OutZone, na.rm = TRUE))
    return(empty_summary)
  }

  outs_on_play <- suppressWarnings(as.integer(pa_rows$OutsOnPlay))
  outs_on_play[is.na(outs_on_play)] <- 0L

  is_strikeout <- pa_rows$K %in% TRUE
  is_walk <- pa_rows$BB %in% TRUE
  is_hbp <- pa_rows$HBP %in% TRUE
  is_single <- pa_rows$X1B %in% TRUE
  is_double <- pa_rows$X2B %in% TRUE
  is_triple <- pa_rows$X3B %in% TRUE
  is_homer <- pa_rows$HR %in% TRUE

  outs_recorded <- ifelse(is_strikeout & outs_on_play < 1L, 1L, outs_on_play)
  pa_total <- nrow(pa_rows)
  hits <- sum(is_single | is_double | is_triple | is_homer, na.rm = TRUE)
  walks <- sum(is_walk, na.rm = TRUE)
  hbp <- sum(is_hbp, na.rm = TRUE)
  strikeouts <- sum(is_strikeout, na.rm = TRUE)
  homers <- sum(is_homer, na.rm = TRUE)
  outs_total <- sum(outs_recorded, na.rm = TRUE)
  ab <- max(pa_total - walks - hbp, 0L)
  total_bases <- sum(is_single, na.rm = TRUE) +
    2L * sum(is_double, na.rm = TRUE) +
    3L * sum(is_triple, na.rm = TRUE) +
    4L * sum(is_homer, na.rm = TRUE)

  avg_against <- cape_pitcher_rate(hits, ab)
  obp_against <- cape_pitcher_rate(hits + walks + hbp, pa_total)
  slg_against <- cape_pitcher_rate(total_bases, ab)
  ops_against <- if (is.na(obp_against) || is.na(slg_against)) NA_real_ else obp_against + slg_against
  whip <- cape_pitcher_rate(hits + walks, outs_total / 3)
  runs_allowed <- sum(cape_pitcher_safe_num(d$RunsScored), na.rm = TRUE)
  era_estimate <- cape_pitcher_rate(runs_allowed * 27, outs_total)
  csw_rate <- cape_pitcher_rate(sum(d$CSW, na.rm = TRUE), nrow(d))
  whiff_rate <- cape_pitcher_rate(sum(d$WhiffP, na.rm = TRUE), sum(d$Swing, na.rm = TRUE))
  chase_rate <- cape_pitcher_rate(sum(d$Chase, na.rm = TRUE), sum(d$OutZone, na.rm = TRUE))
  k_rate <- cape_pitcher_rate(strikeouts, pa_total)
  bb_rate <- cape_pitcher_rate(walks, pa_total)
  k_minus_bb <- cape_pitcher_rate(strikeouts - walks, pa_total)
  xwoba <- if ("paWOBA" %in% names(pa_rows)) mean(pa_rows$paWOBA, na.rm = TRUE) else NA_real_
  if (!is.finite(xwoba)) xwoba <- NA_real_

  list(
    Games = if ("GameUID" %in% names(d)) {
      length(unique(d$GameUID[!is.na(d$GameUID)]))
    } else {
      length(unique(d$Date[!is.na(d$Date)]))
    },
    Pitches = nrow(d),
    PA = pa_total,
    Outs = outs_total,
    Hits = hits,
    Runs = runs_allowed,
    Walks = walks,
    Strikeouts = strikeouts,
    Homers = homers,
    AVG = avg_against,
    OBP = obp_against,
    SLG = slg_against,
    OPS = ops_against,
    ERAEstimate = era_estimate,
    WHIP = whip,
    CSW = csw_rate,
    Whiff = whiff_rate,
    Chase = chase_rate,
    KRate = k_rate,
    BBRate = bb_rate,
    KMinusBB = k_minus_bb,
    xwOBA = xwoba
  )
}

cape_pitcher_statline <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(list())
  }

  summary <- cape_pitcher_summary(d)

  list(
    Workload = list(
      Games = as.character(summary$Games),
      IP = cape_pitcher_format_ip(summary$Outs),
      BF = as.character(summary$PA)
    ),
    `Run Prevention` = list(
      `ERA*` = cape_pitcher_format_num(summary$ERAEstimate, 2),
      WHIP = cape_pitcher_format_num(summary$WHIP, 2),
      R = as.character(summary$Runs),
      H = as.character(summary$Hits),
      BB = as.character(summary$Walks),
      K = as.character(summary$Strikeouts),
      HR = as.character(summary$Homers)
    ),
    `Opponent Production` = list(
      AVG = cape_pitcher_format_rate(summary$AVG, 3),
      OBP = cape_pitcher_format_rate(summary$OBP, 3),
      SLG = cape_pitcher_format_rate(summary$SLG, 3),
      OPS = cape_pitcher_format_rate(summary$OPS, 3),
      xwOBA = cape_pitcher_format_rate(summary$xwOBA, 3)
    ),
    `Pitch Quality` = list(
      `CSW%` = cape_pitcher_format_pct(summary$CSW, 1),
      `Whiff%` = cape_pitcher_format_pct(summary$Whiff, 1),
      `K-BB%` = cape_pitcher_format_pct(summary$KMinusBB, 1)
    )
  )
}

cape_pitcher_split_table <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(tibble::tibble())
  }

  d %>%
    filter(!is.na(BatSide)) %>%
    group_by(BatSide) %>%
    group_split() %>%
    purrr::map_dfr(function(g) {
      summary <- cape_pitcher_summary(g)

      tibble::tibble(
        Split = ifelse(g$BatSide[1] == "L", "vs LHH", "vs RHH"),
        `Stuff+` = round(mnn(g$StuffPlus)),
        `Pitch+` = round(mnn(g$PitchingPlus)),
        PA = summary$PA,
        AVG = summary$AVG,
        OPS = summary$OPS,
        OBP = summary$OBP,
        SLG = summary$SLG,
        `Whiff%` = summary$Whiff,
        `Chase%` = summary$Chase,
        `K%` = summary$KRate,
        `BB%` = summary$BBRate
      )
    }) %>%
    arrange(Split)
}

cape_pitcher_statline_ui <- function(statline) {
  if (!length(statline)) return(NULL)

  groups <- lapply(names(statline), function(group_label) {
    metrics <- statline[[group_label]]
    tiles <- lapply(names(metrics), function(label) {
      tags$div(
        class = "cpp-stat-tile",
        tags$div(class = "cpp-stat-label", label),
        tags$div(class = "cpp-stat-value", metrics[[label]])
      )
    })

    tags$section(
      class = paste(
        "cpp-stat-group",
        paste0("cpp-stat-group-", gsub("[^a-z0-9]+", "-", tolower(group_label)))
      ),
      tags$div(class = "cpp-stat-group-title", group_label),
      tags$div(class = "cpp-stat-grid", tiles)
    )
  })

  tagList(
    tags$div(class = "cpp-stat-groups", groups),
    tags$p(
      class = "cpp-stat-footnote",
      "ERA* is estimated from all runs allowed because the pitch-level source does not identify earned runs."
    )
  )
}

cape_pitcher_table_header <- function(title, subtitle) {
  card_header(
    tags$div(
      class = "cpp-table-heading",
      tags$div(class = "cpp-table-title", title),
      tags$div(class = "cpp-table-subtitle", subtitle)
    )
  )
}

cape_pitcher_pitch_cell <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(as.character(value))) return("--")
  pitch <- as.character(value)
  palette <- cape_pitcher_pal_for(pitch)
  color <- unname(palette[[pitch]])
  tags$div(
    class = "cpp-table-pitch",
    tags$span(class = "cpp-table-pitch-dot", style = paste0("background:", color, ";")),
    tags$span(pitch)
  )
}

cape_pitcher_grade_cell <- function(value) {
  grade <- suppressWarnings(as.numeric(value))
  if (length(grade) != 1L || !is.finite(grade)) return(tags$span(class = "cpp-table-na", "--"))
  tone <- if (grade >= 110) {
    "elite"
  } else if (grade >= 100) {
    "plus"
  } else if (grade >= 90) {
    "average"
  } else {
    "below"
  }
  tags$span(class = paste("cpp-grade-pill", paste0("cpp-grade-", tone)), round(grade))
}

cape_pitcher_pct_bar <- function(value) {
  pct <- suppressWarnings(as.numeric(value))
  if (length(pct) != 1L || !is.finite(pct)) return(tags$span(class = "cpp-table-na", "--"))
  pct_width <- max(0, min(100, pct * 100))
  tags$div(
    class = "cpp-pct-bar",
    tags$span(class = "cpp-pct-bar-fill", style = paste0("width:", pct_width, "%;")),
    tags$span(class = "cpp-pct-bar-label", paste0(round(pct * 100), "%"))
  )
}

cape_pitcher_make_table <- function(df, pct = character(), d3 = character(),
                                    d1 = character(), int = character(),
                                    bar_pct = character(),
                                    grade_cols = c("Stuff+", "Loc+", "Pitch+")) {
  if (is.null(df) || nrow(df) == 0) {
    df <- data.frame(Note = "No data for this selection.")
  }

  column_defs <- lapply(seq_along(names(df)), function(i) {
    name <- names(df)[[i]]
    first_column <- i == 1L
    numeric_column <- is.numeric(df[[name]])
    align <- if (first_column) "left" else if (numeric_column || name %in% c(pct, d3, d1, int)) "right" else "left"
    sticky_style <- if (first_column) {
      function(value, index) {
        list(
          background = if (index %% 2L == 0L) "#FBF8F3" else "#FFFFFF",
          borderRight = "1px solid rgba(80,18,20,0.10)",
          fontWeight = 700,
          whiteSpace = "nowrap"
        )
      }
    } else {
      list(whiteSpace = "nowrap")
    }

    cell_renderer <- NULL
    formatter <- NULL
    if (name %in% c("Pitch", "PitchType")) {
      cell_renderer <- cape_pitcher_pitch_cell
    } else if (name %in% grade_cols) {
      cell_renderer <- cape_pitcher_grade_cell
    } else if (name %in% bar_pct) {
      cell_renderer <- cape_pitcher_pct_bar
    } else if (name %in% pct) {
      formatter <- reactable::colFormat(percent = TRUE, digits = 0)
    } else if (name %in% d3) {
      formatter <- reactable::colFormat(digits = 3)
    } else if (name %in% d1) {
      formatter <- reactable::colFormat(digits = 1)
    } else if (name %in% int) {
      formatter <- reactable::colFormat(digits = 0)
    }

    reactable::colDef(
      align = align,
      sticky = if (first_column) "left" else NULL,
      style = sticky_style,
      cell = cell_renderer,
      format = formatter,
      minWidth = if (first_column) 132 else if (name == "Velo") 150 else 82,
      headerStyle = list(whiteSpace = "nowrap")
    )
  })
  names(column_defs) <- names(df)

  reactable::reactable(
    df,
    class = "cpp-performance-table",
    columns = column_defs,
    pagination = FALSE,
    compact = TRUE,
    borderless = TRUE,
    wrap = FALSE,
    defaultPageSize = 9999,
    highlight = TRUE,
    searchable = nrow(df) > 12,
    resizable = TRUE,
    rowStyle = function(index) {
      list(background = if (index %% 2L == 0L) "#FBF8F3" else "#FFFFFF")
    },
    theme = reactable::reactableTheme(
      backgroundColor = "#FFFFFF",
      color = "#2A2021",
      borderColor = "rgba(80,18,20,0.10)",
      highlightColor = "#F4ECDF",
      cellStyle = list(
        fontSize = "13px",
        lineHeight = "1.35",
        padding = "10px 12px",
        fontVariantNumeric = "tabular-nums"
      ),
      headerStyle = list(
        background = TEAM_CONFIG$colors$primary,
        color = "#FFFFFF",
        fontWeight = 700,
        borderBottom = paste0("3px solid ", TEAM_CONFIG$colors$accent),
        textTransform = "uppercase",
        fontSize = "11px",
        letterSpacing = "0.055em",
        padding = "10px 12px"
      ),
      searchInputStyle = list(
        border = "1px solid rgba(80,18,20,0.14)",
        borderRadius = "10px",
        padding = "8px 10px"
      )
    )
  )
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

cape_pitcher_movement_plot <- function(d, batter_side = c("Left", "Right"), source_id,
                                       arm_angle = NA_real_,
                                       arm_throws = NA_character_) {
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

  p <- plotly::plot_ly(
    data = side_data,
    x = ~HorzBreak,
    y = ~InducedVertBreak,
    color = ~PitchType,
    colors = cape_pitcher_pal_for(side_data$PitchType),
    type = "scatter",
    mode = "markers",
    text = hover_text,
    hoverinfo = "text",
    customdata = ~base_pitch_key,
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
        range = c(-25, 25),
        zeroline = TRUE,
        zerolinewidth = 1.2,
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

  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = "Horizontal Break (in)",
        range = c(-25, 25),
        zeroline = TRUE,
        zerolinewidth = 1.2,
        zerolinecolor = "rgba(12,35,64,0.25)"
      )
    )

  if (is.finite(arm_angle)) {
    theta <- abs(arm_angle) * pi / 180
    line_extent <- 25
    x_direction <- if (identical(arm_throws, "Left")) -1 else 1
    p <- p %>%
      plotly::add_trace(
        inherit = FALSE,
        x = c(0, x_direction * line_extent * cos(theta)),
        y = c(0, line_extent * sin(theta)),
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(12,35,64,0.75)", width = 1.25, dash = "dash"),
        hovertemplate = paste0("Arm angle: ", formatC(arm_angle, format = "f", digits = 1), "°<extra></extra>"),
        showlegend = FALSE
      )
  }

  p
}

cape_pitcher_heatmap_bounds <- list(
  x = c(-2.25, 2.25),
  y = c(0.45, 4.75),
  plot_x = c(-2.65, 2.65),
  plot_y = c(0, 5)
)

cape_pitcher_zone_vertical_guides <- c(-0.277, 0.277)
cape_pitcher_zone_horizontal_guides <- c(2.17, 2.83)

cape_pitcher_strike_zone_box <- tibble::tibble(
  x = c(-0.83, -0.83, 0.83, 0.83, -0.83),
  y = c(1.5, 3.5, 3.5, 1.5, 1.5)
)

cape_pitcher_home_plate <- tibble::tibble(
  x = c(-0.708, 0.708, 0.708, 0, -0.708),
  y = c(0.5, 0.5, 0.25, 0, 0.25)
)

cape_pitcher_batter_outline <- local({
  cache <- new.env(parent = emptyenv())

  grow_mask <- function(mask) {
    mask |
      rbind(FALSE, mask[-nrow(mask), , drop = FALSE]) |
      rbind(mask[-1, , drop = FALSE], FALSE) |
      cbind(FALSE, mask[, -ncol(mask), drop = FALSE]) |
      cbind(mask[, -1, drop = FALSE], FALSE)
  }

  function(side = c("L", "R")) {
    side <- match.arg(side)
    if (exists(side, envir = cache, inherits = FALSE)) {
      return(get(side, envir = cache, inherits = FALSE))
    }

    obj_name <- if (identical(side, "L")) "left_batter" else "right_batter"
    img <- NULL

    if (exists(obj_name, inherits = TRUE)) {
      img <- get(obj_name, inherits = TRUE)
    } else {
      img_path <- if (identical(side, "L")) "left_batter.png" else "right_batter.png"
      if (file.exists(img_path)) {
        img <- png::readPNG(img_path)
      }
    }

    if (is.null(img) || length(dim(img)) < 3 || dim(img)[3] < 4) {
      return(NULL)
    }

    alpha <- img[, , 4] > 0.05
    boundary <- alpha & !(
      rbind(FALSE, alpha[-nrow(alpha), , drop = FALSE]) &
        rbind(alpha[-1, , drop = FALSE], FALSE) &
        cbind(FALSE, alpha[, -ncol(alpha), drop = FALSE]) &
        cbind(alpha[, -1, drop = FALSE], FALSE)
    )
    boundary <- grow_mask(grow_mask(boundary))

    outline <- array(0, dim = dim(img))
    outline[, , 1] <- 12 / 255
    outline[, , 2] <- 35 / 255
    outline[, , 3] <- 64 / 255
    outline[, , 4] <- ifelse(boundary, 0.22, 0)

    assign(side, outline, envir = cache)
    outline
  }
})

cape_pitcher_heatmap_mode_meta <- function(mode = c("freq", "whiff", "damage", "xwoba", "xwobafull")) {
  mode <- match.arg(mode)
  switch(
    mode,
    freq = list(
      colors = c("#FBF8F2", "#EFE2C8", "#D7BD8A", TEAM_CONFIG$colors$primary),
      limits = c(0, 1)
    ),
    whiff = list(
      colors = c("#F4FBFA", "#C4E9DE", "#34A58F", "#0D5C62"),
      limits = c(0, 1)
    ),
    damage = list(
      colors = c("#FFF8F2", "#F8D6AB", "#EB8551", "#9B271E"),
      limits = c(72, 100)
    ),
    xwoba = list(
      colors = c("#FFF8F4", "#F6CFBA", "#E87458", "#8E1E19"),
      limits = c(0.15, 0.60)
    ),
    xwobafull = list(
      colors = c("#FFF8F4", "#F6CAB2", "#E56D58", "#7F1C1A"),
      limits = c(0.15, 0.55)
    )
  )
}

cape_pitcher_heatmap_empty_message <- function(mode = c("freq", "whiff", "damage", "xwoba", "xwobafull")) {
  mode <- match.arg(mode)
  switch(
    mode,
    freq = "Not enough located pitches for this view.",
    whiff = "No swing outcomes are available for this filtered view.",
    damage = "No balls in play are available for this filtered view.",
    xwoba = "No contact-quality events are available for this filtered view.",
    xwobafull = "No plate-ending events are available for this filtered view."
  )
}

cape_pitcher_heatmap_metric_vectors <- function(d, mode = c("freq", "whiff", "damage", "xwoba", "xwobafull")) {
  mode <- match.arg(mode)

  if (mode == "freq") {
    return(list(num = rep(1, nrow(d)), den = rep(1, nrow(d))))
  }

  if (mode == "whiff") {
    return(list(
      num = as.numeric(d$WhiffP %in% TRUE),
      den = as.numeric(d$Swing %in% TRUE)
    ))
  }

  if (mode == "damage") {
    valid <- d$BBE %in% TRUE & !is.na(d$ExitSpeed)
    return(list(
      num = ifelse(valid, d$ExitSpeed, 0),
      den = as.numeric(valid)
    ))
  }

  if (mode == "xwoba") {
    valid <- d$BBE %in% TRUE & !is.na(d$xwOBA)
    return(list(
      num = ifelse(valid, d$xwOBA, 0),
      den = as.numeric(valid)
    ))
  }

  valid <- d$PACheck %in% TRUE & !is.na(d$paWOBA)
  list(
    num = ifelse(valid, d$paWOBA, 0),
    den = as.numeric(valid)
  )
}

cape_pitcher_heatmap_surface <- function(d,
                                         mode = c("freq", "whiff", "damage", "xwoba", "xwobafull"),
                                         nx = 88,
                                         ny = 96,
                                         bandwidth_x = 0.34,
                                         bandwidth_y = 0.40) {
  mode <- match.arg(mode)

  d_loc <- d %>%
    dplyr::filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
    dplyr::mutate(
      plot_x = -PlateLocSide,
      plot_y = PlateLocHeight
    )

  if (nrow(d_loc) < 6) {
    return(NULL)
  }

  x_seq <- seq(cape_pitcher_heatmap_bounds$x[1], cape_pitcher_heatmap_bounds$x[2], length.out = nx)
  y_seq <- seq(cape_pitcher_heatmap_bounds$y[1], cape_pitcher_heatmap_bounds$y[2], length.out = ny)

  x_kernel <- exp(-0.5 * (outer(x_seq, d_loc$plot_x, FUN = "-") / bandwidth_x)^2)
  y_kernel <- exp(-0.5 * (outer(y_seq, d_loc$plot_y, FUN = "-") / bandwidth_y)^2)

  metric <- cape_pitcher_heatmap_metric_vectors(d_loc, mode)
  den_surface <- sweep(y_kernel, 2, metric$den, `*`) %*% t(x_kernel)

  peak_support <- suppressWarnings(max(den_surface, na.rm = TRUE))
  if (!is.finite(peak_support) || peak_support <= 0) {
    return(NULL)
  }

  if (mode == "freq") {
    value_surface <- den_surface / peak_support
  } else {
    num_surface <- sweep(y_kernel, 2, metric$num, `*`) %*% t(x_kernel)
    value_surface <- num_surface / den_surface
    value_surface[den_surface <= 0] <- NA_real_
  }

  support_scaled <- den_surface / peak_support
  support_scaled[!is.finite(support_scaled)] <- 0

  mask_floor <- if (identical(mode, "freq")) 0.04 else 0.08
  alpha_span <- max(0.30, 0.62 - mask_floor)
  value_surface[support_scaled < mask_floor] <- NA_real_

  alpha_surface <- pmin(1, pmax(0, (support_scaled - mask_floor) / alpha_span))
  alpha_surface[!is.finite(alpha_surface)] <- 0
  alpha_surface[is.na(value_surface)] <- 0

  surface <- tibble::tibble(
    x = rep(x_seq, each = length(y_seq)),
    y = rep(y_seq, times = length(x_seq)),
    value = as.vector(value_surface),
    support = as.vector(den_surface),
    alpha = as.vector(alpha_surface)
  ) %>%
    dplyr::filter(is.finite(value), alpha > 0)

  if (nrow(surface) == 0) {
    return(NULL)
  }

  list(
    surface = surface,
    meta = cape_pitcher_heatmap_mode_meta(mode)
  )
}

cape_pitcher_empty_heatmap_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.55,
      label = message,
      colour = "#5F6B7A",
      size = 4.2,
      fontface = "bold"
    ) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FBFCFE", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBFCFE", colour = NA)
    )
}

cape_pitcher_zone_heat_plot <- function(d,
                                        mode = c("freq", "whiff", "damage", "xwoba", "xwobafull"),
                                        batter_side = c("ALL", "L", "R")) {
  mode <- match.arg(mode)
  batter_side <- match.arg(batter_side)
  surface_obj <- cape_pitcher_heatmap_surface(d, mode)

  if (is.null(surface_obj)) {
    return(cape_pitcher_empty_heatmap_plot(cape_pitcher_heatmap_empty_message(mode)))
  }

  surface <- surface_obj$surface
  meta <- surface_obj$meta
  contour_breaks <- stats::quantile(
    surface$support[surface$support > 0],
    probs = c(0.45, 0.70, 0.88),
    na.rm = TRUE
  )
  contour_breaks <- unique(as.numeric(contour_breaks[is.finite(contour_breaks)]))

  zone_verticals <- tibble::tibble(
    x = -cape_pitcher_zone_vertical_guides,
    xend = -cape_pitcher_zone_vertical_guides,
    y = 1.5,
    yend = 3.5
  )
  zone_horizontals <- tibble::tibble(
    x = -0.83,
    xend = 0.83,
    y = cape_pitcher_zone_horizontal_guides,
    yend = cape_pitcher_zone_horizontal_guides
  )

  plot_obj <- ggplot2::ggplot(surface, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(
      ggplot2::aes(fill = value, alpha = alpha),
      interpolate = TRUE
    ) +
    ggplot2::annotate(
      "rect",
      xmin = -0.83,
      xmax = 0.83,
      ymin = 1.5,
      ymax = 3.5,
      fill = "#FFFFFF",
      alpha = 0.05
    ) +
    ggplot2::geom_segment(
      data = zone_verticals,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      colour = grDevices::adjustcolor(TEAM_CONFIG$colors$primary, alpha.f = 0.14),
      linewidth = 0.35
    ) +
    ggplot2::geom_segment(
      data = zone_horizontals,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      colour = grDevices::adjustcolor(TEAM_CONFIG$colors$primary, alpha.f = 0.14),
      linewidth = 0.35
    ) +
    ggplot2::geom_path(
      data = cape_pitcher_strike_zone_box,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      colour = "#10233A",
      linewidth = 1.0
    ) +
    ggplot2::geom_polygon(
      data = cape_pitcher_home_plate,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      fill = "#FFFFFF",
      colour = "#10233A",
      linewidth = 0.9
    ) +
    ggplot2::scale_fill_gradientn(
      colours = meta$colors,
      limits = meta$limits,
      oob = scales::squish,
      na.value = "transparent",
      guide = "none"
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_fixed(
      xlim = cape_pitcher_heatmap_bounds$plot_x,
      ylim = cape_pitcher_heatmap_bounds$plot_y,
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 10, 8, 10),
      panel.background = ggplot2::element_rect(fill = "#FBFCFE", colour = NA),
      plot.background = ggplot2::element_rect(fill = "#FBFCFE", colour = NA)
    )

  if (length(contour_breaks) > 0) {
    plot_obj <- plot_obj +
      ggplot2::geom_contour(
        data = surface,
        ggplot2::aes(x = x, y = y, z = support),
        inherit.aes = FALSE,
        breaks = contour_breaks,
        colour = grDevices::adjustcolor(TEAM_CONFIG$colors$primary, alpha.f = 0.14),
        linewidth = 0.28
      )
  }

  if (!identical(batter_side, "ALL")) {
    batter_outline <- cape_pitcher_batter_outline(batter_side)
    if (!is.null(batter_outline)) {
      if (identical(batter_side, "L")) {
        plot_obj <- plot_obj +
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(batter_outline, interpolate = TRUE),
            xmin = 0.95, xmax = 2.85, ymin = 0.35, ymax = 4.95
          )
      } else {
        plot_obj <- plot_obj +
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(batter_outline, interpolate = TRUE),
            xmin = -2.85, xmax = -0.95, ymin = 0.35, ymax = 4.95
          )
      }
    }
  }

  plot_obj
}

cape_pitcher_player_page_ui <- function() {
  tagList(
    tags$div(
      class = "hub-main base-page base-scout-page",
      tags$head(
        tags$style(HTML("
          #cpp-page {
            color: var(--navy);
          }
          #cpp-page .card {
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 18px;
            box-shadow: 0 14px 30px rgba(12, 35, 64, 0.06);
            background: linear-gradient(180deg, #FFFFFF 0%, #FCFDFE 100%);
          }
          #cpp-page .card-header {
            background: transparent;
            border-bottom: 1px solid rgba(12, 35, 64, 0.08);
            color: var(--navy);
            font-family: var(--font-head);
            font-size: 20px;
            letter-spacing: 0.02em;
            padding: 16px 18px 14px 18px;
          }
          #cpp-page .card-body {
            padding: 18px;
          }
          #cpp-page .cpp-stat-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(104px, 1fr));
            gap: 10px;
          }
          #cpp-page .cpp-stat-groups {
            display: grid;
            grid-template-columns: repeat(12, minmax(0, 1fr));
            gap: 14px;
          }
          #cpp-page .cpp-stat-group {
            grid-column: span 6;
            min-width: 0;
            padding: 15px;
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 16px;
            background: #F7FAFC;
          }
          #cpp-page .cpp-stat-group-workload,
          #cpp-page .cpp-stat-group-pitch-quality { grid-column: span 4; }
          #cpp-page .cpp-stat-group-run-prevention,
          #cpp-page .cpp-stat-group-opponent-production { grid-column: span 8; }
          #cpp-page .cpp-stat-group-title {
            margin: 0 0 11px;
            color: var(--navy);
            font-family: var(--font-head);
            font-size: 14px;
            font-weight: 700;
            letter-spacing: 0.055em;
            text-transform: uppercase;
          }
          #cpp-page .cpp-stat-footnote {
            margin: 11px 2px 0;
            color: #697586;
            font-size: 11px;
            line-height: 1.45;
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
          .cpp-page-intro {
            background:
              radial-gradient(circle at top right, rgba(10, 147, 150, 0.16), transparent 38%),
              linear-gradient(135deg, rgba(12, 35, 64, 0.05) 0%, rgba(255, 255, 255, 0.96) 68%);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 24px;
            box-shadow: 0 18px 36px rgba(12, 35, 64, 0.06);
            margin-bottom: 22px;
            padding: 24px 28px;
          }
          .cpp-kicker {
            color: var(--teal);
            font-size: 11px;
            font-weight: 700;
            letter-spacing: 1.6px;
            margin-bottom: 10px;
            text-transform: uppercase;
          }
          .cpp-page-title {
            color: var(--navy);
            font-family: var(--font-head);
            font-size: 38px;
            line-height: 1;
            margin: 0 0 10px 0;
          }
          .cpp-page-copy {
            color: #5F6B7A;
            font-size: 14px;
            line-height: 1.6;
            margin: 0;
            max-width: 780px;
          }
          #cpp-page .cpp-meta-card {
            background: linear-gradient(180deg, #F7FAFC 0%, #EEF3F8 100%);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 14px;
            padding: 16px 18px;
            min-height: 132px;
          }
          #cpp-page .cpp-stat-tile {
            background: var(--navy);
            border-radius: 14px;
            min-height: 84px;
            padding: 13px 12px;
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
            font-size: 25px;
            font-weight: 700;
            letter-spacing: -0.5px;
            line-height: 1;
          }
          #cpp-page .cpp-meta-name {
            color: var(--navy);
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
            color: var(--navy);
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
          #cpp-page .cpp-armangle-card .form-group {
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
          #cpp-page .cpp-armangle-box {
            border-top: 1px solid rgba(12, 35, 64, 0.10);
            margin-top: 12px;
            padding-top: 12px;
          }
          #cpp-page .cpp-subhead {
            color: var(--navy);
            font-size: 12px;
            font-weight: 700;
            letter-spacing: 0.4px;
            margin-bottom: 8px;
            text-transform: uppercase;
          }
          #cpp-page .cpp-retag-card .cpp-status {
            margin-top: 6px;
          }
          #cpp-page .cpp-heatmap-controls {
            background: linear-gradient(180deg, #F7FAFC 0%, #EEF3F8 100%);
            border-color: rgba(12, 35, 64, 0.08);
            padding: 0;
            margin-bottom: 0;
          }
          #cpp-page .cpp-heatmap-controls .card-body {
            background: transparent;
          }
          #cpp-page .cpp-heatmap-controls .form-group {
            margin-bottom: 8px;
          }
          #cpp-page .cpp-heatmap-summary {
            background: linear-gradient(180deg, #F7FAFC 0%, #EFF4F8 100%);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 16px;
            margin-bottom: 16px;
            padding: 14px 16px;
          }
          #cpp-page .cpp-heatmap-summary-title {
            color: var(--navy);
            font-size: 16px;
            font-weight: 700;
            margin-bottom: 4px;
          }
          #cpp-page .cpp-heatmap-summary-note {
            color: #5F6B7A;
            font-size: 12px;
            line-height: 1.45;
            margin-bottom: 12px;
          }
          #cpp-page .cpp-chip-row {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
          }
          #cpp-page .cpp-chip {
            background: rgba(12, 35, 64, 0.06);
            border: 1px solid rgba(12, 35, 64, 0.08);
            border-radius: 999px;
            color: #35516E;
            font-size: 11px;
            font-weight: 700;
            letter-spacing: 0.25px;
            padding: 6px 10px;
          }
          #cpp-page .cpp-chip-strong {
            background: var(--navy);
            border-color: var(--navy);
            color: #FFFFFF;
          }
          #cpp-page .cpp-heatmap-card .card-body {
            padding-top: 14px;
          }
          #cpp-page .cpp-heatmap-card-copy {
            color: #5F6B7A;
            font-size: 12px;
            line-height: 1.45;
            margin-bottom: 10px;
            min-height: 34px;
          }
          #cpp-page .cpp-heatmap-side .radio-inline {
            color: var(--navy);
            font-weight: 700;
            margin-right: 14px;
          }
          #cpp-page .cpp-heatmap-note {
            color: #5F6B7A;
            font-size: 12px;
            line-height: 1.45;
            margin-bottom: 14px;
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

          /* BASE design-system alignment. These rules intentionally affect
             presentation only; input/output IDs and layout behavior remain intact. */
          #cpp-page { color: var(--base-ink); }
          #cpp-page .card {
            border: 1px solid var(--base-border);
            border-radius: var(--base-radius);
            background: var(--base-surface);
            box-shadow: var(--base-shadow-sm);
          }
          #cpp-page .card-header {
            border-bottom-color: var(--base-border);
            color: var(--base-ink);
            font-family: var(--base-font-display);
            font-size: 17px;
          }
          .cpp-page-intro {
            margin-bottom: 24px;
            padding: 24px 26px;
            border: 1px solid var(--base-border);
            border-radius: var(--base-radius-lg);
            background:
              radial-gradient(circle at top right, rgba(215,189,138,0.18), transparent 42%),
              var(--base-surface);
            box-shadow: var(--base-shadow-sm);
          }
          .cpp-kicker { color: var(--base-maroon); }
          .cpp-page-title { color: var(--base-ink); font-size: 42px; }
          .cpp-page-copy,
          #cpp-page .cpp-helper,
          #cpp-page .cpp-meta-line,
          #cpp-page .cpp-section-copy,
          #cpp-page .cpp-retag-note,
          #cpp-page .cpp-heatmap-summary-note,
          #cpp-page .cpp-heatmap-card-copy,
          #cpp-page .cpp-heatmap-note { color: var(--base-muted); }
          #cpp-page .cpp-meta-card,
          #cpp-page .cpp-heatmap-controls,
          #cpp-page .cpp-heatmap-summary {
            border-color: var(--base-border);
            border-radius: var(--base-radius);
            background: var(--base-surface-soft);
          }
          #cpp-page .cpp-stat-tile {
            border: 1px solid rgba(215,189,138,0.22);
            border-radius: var(--base-radius);
            background: var(--base-maroon);
            box-shadow: var(--base-shadow-sm);
          }
          #cpp-page .cpp-stat-group {
            border-color: var(--base-border);
            background: var(--base-surface-soft);
          }
          #cpp-page .cpp-stat-group-title { color: var(--base-maroon); }
          #cpp-page .cpp-stat-footnote { color: var(--base-muted); }
          #cpp-page .cpp-stat-value { font-family: var(--base-font-data); }
          #cpp-page .cpp-meta-name,
          #cpp-page .cpp-section-label,
          #cpp-page .cpp-subhead,
          #cpp-page .cpp-heatmap-summary-title { color: var(--base-ink); }
          #cpp-page .cpp-section-label {
            padding-bottom: 9px;
            border-bottom: 1px solid var(--base-border);
            font-family: var(--base-font-display);
            letter-spacing: 0.035em;
          }
          #cpp-page .cpp-chip {
            border-color: var(--base-border);
            background: var(--base-surface-muted);
            color: var(--base-maroon);
          }
          #cpp-page .cpp-chip-strong {
            border-color: var(--base-maroon);
            background: var(--base-maroon);
            color: #fff;
          }
          #cpp-page .cpp-status.clean { background: var(--base-success-soft); color: var(--base-success); }
          #cpp-page .cpp-status.dirty { background: var(--base-warning-soft); color: var(--base-warning); }
          #cpp-page .cpp-status.error { background: var(--base-danger-soft); color: var(--base-danger); }
          #cpp-page .cpp-table-card {
            margin-bottom: 14px;
            overflow: hidden !important;
            border-color: rgba(80,18,20,0.12);
          }
          #cpp-page .cpp-table-card .card-header {
            padding: 15px 18px 13px;
            background: linear-gradient(90deg, rgba(80,18,20,0.045), rgba(215,189,138,0.08));
            border-bottom: 1px solid var(--base-border);
          }
          #cpp-page .cpp-table-heading {
            display: flex;
            align-items: baseline;
            justify-content: space-between;
            gap: 18px;
            width: 100%;
          }
          #cpp-page .cpp-table-title {
            flex: 0 0 auto;
            color: var(--base-maroon);
            font-family: var(--base-font-display);
            font-size: 18px;
            font-weight: 700;
            letter-spacing: 0.025em;
          }
          #cpp-page .cpp-table-subtitle {
            color: var(--base-muted);
            font-family: var(--base-font-body);
            font-size: 11px;
            font-weight: 500;
            line-height: 1.4;
            text-align: right;
          }
          #cpp-page .cpp-table-card .cpp-table-body {
            padding: 14px 18px 17px;
            overflow-x: auto !important;
            overflow-y: hidden !important;
            scrollbar-color: rgba(80,18,20,0.28) transparent;
            scrollbar-width: thin;
          }
          #cpp-page .cpp-table-card-wide .cpp-performance-table {
            min-width: 1080px;
          }
          #cpp-page .cpp-performance-table {
            width: 100%;
            border: 1px solid rgba(80,18,20,0.09);
            border-radius: 12px;
            overflow: hidden;
            font-family: var(--base-font-body);
          }
          #cpp-page .cpp-performance-table .rt-thead {
            box-shadow: 0 3px 10px rgba(80,18,20,0.12);
          }
          #cpp-page .cpp-performance-table .rt-tr:hover .rt-td {
            background: #F4ECDF !important;
          }
          #cpp-page .cpp-table-pitch {
            display: inline-flex;
            align-items: center;
            gap: 9px;
            min-width: 0;
            color: var(--base-ink);
            font-weight: 700;
          }
          #cpp-page .cpp-table-pitch-dot {
            flex: 0 0 auto;
            width: 9px;
            height: 9px;
            border: 2px solid rgba(255,255,255,0.82);
            border-radius: 50%;
            box-shadow: 0 0 0 1px rgba(80,18,20,0.16);
          }
          #cpp-page .cpp-grade-pill {
            display: inline-flex;
            align-items: center;
            justify-content: center;
            min-width: 38px;
            padding: 3px 8px;
            border: 1px solid transparent;
            border-radius: 999px;
            font-family: var(--base-font-data);
            font-size: 12px;
            font-weight: 800;
            line-height: 1.2;
          }
          #cpp-page .cpp-grade-elite {
            border-color: #2D6E50;
            background: #2D6E50;
            color: #FFFFFF;
          }
          #cpp-page .cpp-grade-plus {
            border-color: #B7D9C5;
            background: #E8F4ED;
            color: #245B42;
          }
          #cpp-page .cpp-grade-average {
            border-color: #E0C991;
            background: #F8F0DC;
            color: #705820;
          }
          #cpp-page .cpp-grade-below {
            border-color: #E2B9B9;
            background: #F9E9E9;
            color: #8B3030;
          }
          #cpp-page .cpp-pct-bar {
            position: relative;
            min-width: 70px;
            height: 24px;
            overflow: hidden;
            border: 1px solid rgba(80,18,20,0.10);
            border-radius: 7px;
            background: rgba(80,18,20,0.04);
          }
          #cpp-page .cpp-pct-bar-fill {
            position: absolute;
            inset: 0 auto 0 0;
            background: linear-gradient(90deg, rgba(80,18,20,0.16), rgba(215,189,138,0.46));
          }
          #cpp-page .cpp-pct-bar-label {
            position: relative;
            z-index: 1;
            display: flex;
            align-items: center;
            justify-content: flex-end;
            height: 100%;
            padding: 0 7px;
            color: var(--base-ink);
            font-family: var(--base-font-data);
            font-size: 11px;
            font-weight: 800;
          }
          #cpp-page .cpp-table-na { color: var(--base-muted); }
          @media (max-width: 1050px) {
            #cpp-page .cpp-stat-group { grid-column: 1 / -1; }
          }
          @media (max-width: 760px) {
            #cpp-page .cpp-table-heading {
              display: block;
            }
            #cpp-page .cpp-table-subtitle {
              margin-top: 4px;
              text-align: left;
            }
          }
          @media (max-width: 560px) {
            #cpp-page .cpp-stat-group { padding: 12px; }
            #cpp-page .cpp-stat-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
          }
        "))
      ),
      tags$div(
        class = "cpp-page-intro",
        tags$div(class = "cpp-kicker", paste(TEAM_CONFIG$name, "Internal Scouting")),
        tags$h2("Pitcher Scouting", class = "cpp-page-title"),
        tags$p(
          "Choose a college team, load a pitcher, and move from movement shapes into full-location heatmaps, split tables, and report-ready context without leaving the page.",
          class = "cpp-page-copy"
        )
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
                  "College team",
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
                checkboxInput(
                  "cpp_include_cape",
                  "Include matched 2026 Cape Cod League pitches",
                  value = TRUE
                ),
                uiOutput("cpp_source_note"),
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
          "Movement plots, persistent pitch retagging, and arm-angle overlay tools are grouped here for pitch-shape review.",
          class = "cpp-section-copy"
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
                "Save Retag Permanently",
                class = "btn btn-primary btn-sm btn-block"
              ),
              actionButton(
                "cpp_revert_retag",
                "Revert Selected to Original",
                class = "btn btn-default btn-sm btn-block"
              ),
              actionButton(
                "cpp_clear_selection",
                "Clear Plot Selection",
                class = "btn btn-default btn-sm btn-block"
              ),
              tags$p(
                "Saved retags persist across sessions without modifying the source Parquet.",
                class = "cpp-retag-note"
              ),
              tags$div(
                class = "cpp-retag-meta",
                textOutput("cpp_selection_info", inline = TRUE)
              ),
              uiOutput("cpp_save_status")
            )
          ),
          card(
            card_header("Arm Angle Overlay"),
            card_body(
              class = "cpp-armangle-card",
              fluidRow(
                column(
                  6,
                  numericInput("cpp_height_ft", "Height (ft)", value = NA,
                               min = 4, max = 8, step = 1)
                ),
                column(
                  6,
                  numericInput("cpp_height_in", "Height (in)", value = NA,
                               min = 0, max = 11, step = 1)
                )
              ),
              actionButton(
                "cpp_save_height",
                "Save Height For Pitcher",
                class = "btn btn-default btn-sm btn-block"
              ),
              tags$p(
                "Uses the current pitcher's average release height and release side with shoulder height set to 70% of the entered height.",
                class = "cpp-retag-note"
              ),
              tags$div(
                class = "cpp-retag-meta",
                textOutput("cpp_arm_angle_info", inline = TRUE)
              )
            )
          )
        ),
        tags$div(class = "cpp-section-label", "Heatmap Analysis"),
        tags$p(
          "These filters drive full-location heatmaps that now include misses and chase lanes outside the strike zone, not just the box itself.",
          class = "cpp-section-copy"
        ),
        layout_columns(
          col_widths = c(8, 4),
          card(
            class = "cpp-heatmap-controls",
            card_header("Heatmap Filters"),
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
            class = "cpp-heatmap-controls cpp-heatmap-side",
            card_header("Heatmap Side"),
            card_body(
              radioButtons(
                "cpp_heat_side",
                "Heatmap batter side",
                choices = c("All" = "ALL", "vs LHH" = "L", "vs RHH" = "R"),
                selected = "ALL",
                inline = TRUE
              ),
              tags$p(
                "These controls affect the heatmaps below.",
                class = "cpp-section-copy",
                style = "margin: 0;"
              )
            )
          )
        ),
        card(
          card_header("Heatmaps"),
          card_body(
            uiOutput("cpp_heatmap_summary"),
            tags$p(
              "Each panel below is a smoothed location surface built from actual pitch coordinates, so the visual carries beyond the zone edges and removes the oversized percentage labels from the old view.",
              class = "cpp-heatmap-note"
            ),
            layout_columns(
              col_widths = c(6, 6),
              card(
                class = "cpp-heatmap-card",
                card_header("Pitch Location Density"),
                card_body(
                  tags$p(
                    "Overall pitch concentration across the full plate area.",
                    class = "cpp-heatmap-card-copy"
                  ),
                  plotOutput("cpp_pitch_zone", height = "350px")
                )
              ),
              card(
                class = "cpp-heatmap-card",
                card_header("Whiff Rate Surface"),
                card_body(
                  tags$p(
                    "Where swings are most likely to miss, including chase space off the plate.",
                    class = "cpp-heatmap-card-copy"
                  ),
                  plotOutput("cpp_pitch_whiff_zone", height = "350px")
                )
              )
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              card(
                class = "cpp-heatmap-card",
                card_header("Exit Velo Allowed"),
                card_body(
                  tags$p(
                    "Smoothed contact damage on balls put in play by location.",
                    class = "cpp-heatmap-card-copy"
                  ),
                  plotOutput("cpp_pitch_dmg_zone", height = "320px")
                )
              ),
              card(
                class = "cpp-heatmap-card",
                card_header("xwOBAcon Allowed"),
                card_body(
                  tags$p(
                    "Expected contact quality allowed at each location.",
                    class = "cpp-heatmap-card-copy"
                  ),
                  plotOutput("cpp_pitch_xwc_zone", height = "320px")
                )
              ),
              card(
                class = "cpp-heatmap-card",
                card_header("xwOBA Allowed"),
                card_body(
                  tags$p(
                    "Expected plate-ending outcomes mapped to the finish location.",
                    class = "cpp-heatmap-card-copy"
                  ),
                  plotOutput("cpp_pitch_xwf_zone", height = "320px")
                )
              )
            )
          )
        ),
        tags$div(class = "cpp-section-label", "Performance Tables"),
        tags$p(
          "Pitch shape, split performance, usage, and expected outcomes are grouped below for report-building.",
          class = "cpp-section-copy"
        ),
        card(
          class = "cpp-table-card cpp-table-card-wide",
          cape_pitcher_table_header(
            "Arsenal Overview",
            "Velocity bands, movement shape, release traits, and expected contact by pitch."
          ),
          card_body(class = "cpp-table-body", reactableOutput("cpp_arsenal"))
        ),
        card(
          class = "cpp-table-card cpp-table-card-wide",
          cape_pitcher_table_header(
            "Split Results",
            "Overall quality and outcomes against left- and right-handed hitters."
          ),
          card_body(class = "cpp-table-body", reactableOutput("cpp_psplit"))
        ),
        card(
          class = "cpp-table-card cpp-table-card-wide",
          cape_pitcher_table_header(
            "Results by Pitch vs LHH",
            "Pitch-level grades, run value, expected contact, and approach rates."
          ),
          card_body(class = "cpp-table-body", reactableOutput("cpp_perf_lhh"))
        ),
        card(
          class = "cpp-table-card cpp-table-card-wide",
          cape_pitcher_table_header(
            "Results by Pitch vs RHH",
            "Pitch-level grades, run value, expected contact, and approach rates."
          ),
          card_body(class = "cpp-table-body", reactableOutput("cpp_perf_rhh"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "cpp-table-card",
            cape_pitcher_table_header(
              "Pitch Mix by Side",
              "Usage distribution against each hitter side."
            ),
            card_body(class = "cpp-table-body", reactableOutput("cpp_pmix"))
          ),
          card(
            class = "cpp-table-card",
            cape_pitcher_table_header(
              "Get-Ahead & Put-Away",
              "First-pitch and two-strike usage with whiff results."
            ),
            card_body(class = "cpp-table-body", reactableOutput("cpp_pusage"))
          )
        ),
        card(
          class = "cpp-table-card cpp-table-card-wide",
          cape_pitcher_table_header(
            "Expected Outcomes by Pitch",
            "xwOBA on all completed plate appearances and xwOBA on contact."
          ),
          card_body(class = "cpp-table-body", reactableOutput("cpp_pitch_xw_table"))
        )
      )
    ),
    tags$div(
      class = "hub-footer",
      base_brand_footer()
    )
  )
}

cape_pitcher_player_page_server <- function(input, output, session,
                                            data_path = TEAM_CONFIG$data$season_file,
                                            source_data = NULL,
                                            catalog_data = NULL,
                                            player_loader = NULL,
                                            supplement_data = NULL) {
  raw_data <- reactiveVal(NULL)
  catalog_snapshot <- reactiveVal(NULL)
  supplement_snapshot <- reactiveVal(NULL)
  data_loaded <- reactiveVal(FALSE)
  data_loading <- reactiveVal(FALSE)
  manual_heights <- reactiveVal(
    tibble::tibble(
      pitcher_key = character(),
      height_ft = numeric(),
      height_in = numeric()
    )
  )
  status_message <- reactiveVal("Pitcher page data will load when this tab is opened.")
  status_class <- reactiveVal("clean")
  selected_rows <- reactiveVal(character(0))
  retag_revision <- reactiveVal(0L)
  loaded_player_key <- reactiveVal(NULL)

  observeEvent(input$base_nav, {
    if (!identical(input$base_nav, "tab_pitcher_player") || isTRUE(data_loaded()) || isTRUE(data_loading())) {
      return()
    }

    data_loading(TRUE)
    status_class("clean")
    status_message("Loading pitcher page data...")

    tryCatch({
      source_snapshot <- NULL

      if (!is.null(source_data)) {
        source_snapshot <- if (shiny::is.reactive(source_data)) {
          source_data()
        } else if (is.function(source_data)) {
          source_data()
        } else {
          source_data
        }
      }

      supplement_value <- NULL
      if (!is.null(supplement_data)) {
        supplement_value <- if (shiny::is.reactive(supplement_data)) {
          supplement_data()
        } else if (is.function(supplement_data)) {
          supplement_data()
        } else {
          supplement_data
        }
      }
      supplement_snapshot(supplement_value)

      catalog_value <- NULL
      if (!is.null(catalog_data)) {
        catalog_value <- if (shiny::is.reactive(catalog_data)) {
          catalog_data()
        } else if (is.function(catalog_data)) {
          catalog_data()
        } else {
          catalog_data
        }
      }

      if (!is.null(catalog_value) && nrow(catalog_value) > 0) {
        catalog_snapshot(catalog_value %>%
          transmute(
            PitcherTeam = as.character(PitcherTeam),
            Pitcher = as.character(Pitcher)
          ) %>%
          filter(
            !is.na(PitcherTeam), nzchar(PitcherTeam),
            !is.na(Pitcher), nzchar(Pitcher)
          ) %>%
          distinct())
        raw_data(NULL)
        status_message("Select a pitcher to load 2026 college data.")
      } else {
        initial_data <- cape_pitcher_init_data(
          path = data_path,
          raw_df = source_snapshot
        )
        raw_data(initial_data)
        if (nrow(initial_data) == 0) {
          status_class("clean")
          status_message(paste0(
            "No ", TEAM_CONFIG$season,
            " college pitch data is loaded. Populate ", TEAM_CONFIG$data$college_file,
            " to enable team and pitcher scouting."
          ))
        }
      }
      data_loaded(TRUE)
      selected_rows(character(0))
    }, error = function(e) {
      status_class("error")
      status_message(paste("Pitcher page load failed:", e$message))
      showNotification(paste("Pitcher page load failed:", e$message), type = "error")
    })

    data_loading(FALSE)
  }, ignoreInit = FALSE)

  pitcher_catalog <- reactive({
    req(isTRUE(data_loaded()))
    d <- catalog_snapshot()
    if (is.null(d)) d <- raw_data()
    req(!is.null(d))
    req(nrow(d) > 0)

    catalog_data <- d
    if ("DataSource" %in% names(catalog_data)) {
      catalog_data <- catalog_data %>%
        filter(is.na(DataSource) | DataSource != "2026 Cape Cod League")
    }

    catalog_data %>%
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
        TeamDisplay = cape_pitcher_team_name(PitcherTeam),
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
    default_team <- base_team_default(tc$PitcherTeam)
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

  current_pitcher_key <- reactive({
    req(input$cpp_team, input$cpp_pitcher)
    available_pitchers <- team_pitchers()
    req(
      nrow(available_pitchers) > 0,
      input$cpp_pitcher %in% available_pitchers$Pitcher
    )
    paste(input$cpp_team, input$cpp_pitcher, sep = "::")
  })

  observeEvent(current_pitcher_key(), {
    if (!is.function(player_loader)) return()

    player_key <- current_pitcher_key()
    selected_team <- input$cpp_team
    selected_pitcher <- input$cpp_pitcher
    selected_rows(character(0))
    loaded_player_key(NULL)
    raw_data(NULL)
    status_class("clean")
    status_message("Loading selected pitcher's 2026 data...")

    tryCatch({
      player_rows <- player_loader(selected_team, selected_pitcher)
      initial_data <- cape_pitcher_init_data(raw_df = player_rows)
      if (nrow(initial_data) > 0 &&
          !all(c("PitcherTeam", "Pitcher") %in% names(initial_data))) {
        stop("Loaded pitcher data is missing its team or player identifier.")
      }
      raw_data(initial_data)
      loaded_player_key(player_key)
      if (nrow(initial_data) > 0) {
        storage <- base_retag_storage_status()
        if (isTRUE(storage$ok)) {
          status_message("Saved retags are permanent and loaded automatically.")
        } else {
          status_class("error")
          status_message(paste("Persistent retag storage is unavailable:", storage$message))
        }
      } else {
        status_message("No 2026 college pitches were found for the selected player.")
      }
    }, error = function(e) {
      loaded_player_key(NULL)
      raw_data(NULL)
      status_class("error")
      status_message(paste("Player load failed:", e$message))
      showNotification(paste("Player load failed:", e$message), type = "error")
    })
  }, ignoreInit = FALSE)

  pitcher_raw <- reactive({
    player_key <- current_pitcher_key()
    if (is.function(player_loader)) {
      req(identical(loaded_player_key(), player_key))
    }
    retag_revision()
    d <- raw_data()
    req(!is.null(d))
    req(all(c("PitcherTeam", "Pitcher") %in% names(d)))

    college_rows <- d %>%
      filter(PitcherTeam == input$cpp_team, Pitcher == input$cpp_pitcher)

    supplement <- supplement_snapshot()
    if (!isTRUE(input$cpp_include_cape) || is.null(supplement) || !nrow(supplement)) {
      return(base_apply_pitch_retags(college_rows))
    }

    cape_rows <- base_player_supplement_rows(
      college_rows,
      supplement,
      input$cpp_pitcher,
      role = "Pitcher"
    )

    bind_rows(college_rows, cape_rows) %>%
      base_apply_pitch_retags()
  })

  output$cpp_source_note <- renderUI({
    player_key <- current_pitcher_key()
    if (is.function(player_loader)) {
      req(identical(loaded_player_key(), player_key))
    }
    d <- raw_data()
    req(!is.null(d))
    req(all(c("PitcherTeam", "Pitcher") %in% names(d)))
    primary_rows <- d %>%
      filter(PitcherTeam == input$cpp_team, Pitcher == input$cpp_pitcher)
    college_n <- sum(
      primary_rows$PitcherTeam == input$cpp_team &
        primary_rows$Pitcher == input$cpp_pitcher,
      na.rm = TRUE
    )
    supplement <- supplement_snapshot()
    cape_n <- if (is.null(supplement) || !nrow(supplement)) 0L else nrow(
      base_player_supplement_rows(
        primary_rows,
        supplement,
        input$cpp_pitcher,
        role = "Pitcher"
      )
    )
    tags$p(
      paste0(
        format(college_n, big.mark = ","), " college pitches",
        if (cape_n > 0L) paste0(" + ", format(cape_n, big.mark = ","), " matched Cape pitches")
        else "; no matched Cape pitches"
      ),
      class = "cpp-helper"
    )
  })

  observeEvent(current_pitcher_key(), {
    saved <- manual_heights() %>%
      filter(pitcher_key == current_pitcher_key())

    updateNumericInput(
      session,
      "cpp_height_ft",
      value = if (nrow(saved)) saved$height_ft[[1]] else NA
    )
    updateNumericInput(
      session,
      "cpp_height_in",
      value = if (nrow(saved)) saved$height_in[[1]] else NA
    )
  }, ignoreInit = FALSE)

  pitcher_full <- reactive({
    d <- pitcher_raw()
    req(nrow(d) > 0)
    cape_pitcher_prepare_view(d)
  })

  observeEvent(input$cpp_save_height, {
    req(input$cpp_team, input$cpp_pitcher)

    if (is.null(input$cpp_height_ft) || is.null(input$cpp_height_in) ||
        is.na(input$cpp_height_ft) || is.na(input$cpp_height_in)) {
      showNotification("Enter both height fields before saving.", type = "warning")
      return()
    }

    heights <- manual_heights()
    key <- current_pitcher_key()
    new_row <- tibble::tibble(
      pitcher_key = key,
      height_ft = as.numeric(input$cpp_height_ft),
      height_in = as.numeric(input$cpp_height_in)
    )

    heights <- heights %>%
      filter(pitcher_key != key) %>%
      bind_rows(new_row)

    manual_heights(heights)
    showNotification(
      paste0(
        "Saved manual height for ",
        cape_pitcher_format_pitcher_name(input$cpp_pitcher),
        ": ",
        input$cpp_height_ft, "'",
        input$cpp_height_in, "\"."
      ),
      type = "message"
    )
  })

  current_manual_height <- reactive({
    saved <- manual_heights() %>%
      filter(pitcher_key == current_pitcher_key())

    if (!nrow(saved)) {
      return(list(height_ft = NA_real_, height_in = NA_real_, height_feet = NA_real_))
    }

    list(
      height_ft = saved$height_ft[[1]],
      height_in = saved$height_in[[1]],
      height_feet = cape_pitcher_height_to_feet(saved$height_ft[[1]], saved$height_in[[1]])
    )
  })

  current_arm_angle <- reactive({
    h <- current_manual_height()
    summary <- cape_pitcher_arm_angle_summary(pitcher_full(), h$height_feet)
    c(summary, h)
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
            cape_pitcher_team_name(team_code),
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

  output$cpp_arm_angle_info <- renderText({
    arm <- current_arm_angle()

    if (!is.finite(arm$height_feet)) {
      return("No manual height saved for this pitcher yet. Save a height to draw the dashed arm-angle overlay.")
    }
    if (!is.finite(arm$angle)) {
      return("A height is saved, but release data is missing for the current pitcher so the overlay could not be computed.")
    }

    paste0(
      "Overlay using saved height ",
      arm$height_ft, "'", arm$height_in, "\"",
      ": ",
      formatC(arm$angle, format = "f", digits = 1),
      "° arm angle."
    )
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

  movement_data <- reactive({
    d <- pitcher_full()
    req(nrow(d) > 0)
    d
  })

  heatmap_filtered <- reactive({
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
    list(input$cpp_team, input$cpp_pitcher),
    {
      selected_rows(character(0))
    },
    ignoreInit = TRUE
  )

  heatmap_data <- reactive({
    d <- heatmap_filtered()

    if (identical(input$cpp_heat_side, "L")) {
      d <- d %>% filter(BatterSide %in% c("Left", "L"))
    } else if (identical(input$cpp_heat_side, "R")) {
      d <- d %>% filter(BatterSide %in% c("Right", "R"))
    }
    d
  })

  output$cpp_heatmap_summary <- renderUI({
    d <- heatmap_data()
    pitch_label <- cape_pitcher_heatmap_filter_label(
      input$cpp_pitch_filter,
      fallback = "All pitch types",
      singular = "pitch type"
    )
    count_label <- cape_pitcher_heatmap_filter_label(
      input$cpp_count_filter,
      fallback = "All counts",
      singular = "count"
    )
    side_label <- switch(
      input$cpp_heat_side %||% "ALL",
      L = "vs LHH",
      R = "vs RHH",
      "All hitters"
    )

    if (nrow(d) == 0) {
      return(
        tags$div(
          class = "cpp-heatmap-summary",
          tags$div(class = "cpp-heatmap-summary-title", "No pitches match the current heatmap filter."),
          tags$div(
            class = "cpp-heatmap-summary-note",
            "Broaden the pitch type, count, or hitter-side filter to rebuild the location surfaces."
          ),
          tags$div(
            class = "cpp-chip-row",
            tags$span(class = "cpp-chip cpp-chip-strong", side_label),
            tags$span(class = "cpp-chip", pitch_label),
            tags$span(class = "cpp-chip", count_label)
          )
        )
      )
    }

    located_n <- sum(!is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight))
    excluded_n <- nrow(d) - located_n
    note <- if (excluded_n > 0) {
      paste0(
        excluded_n,
        " filtered pitch(es) do not have location data and are excluded from the surfaces."
      )
    } else {
      "All filtered pitches have plate-location data, so every surface reflects the full sample."
    }

    tags$div(
      class = "cpp-heatmap-summary",
      tags$div(
        class = "cpp-heatmap-summary-title",
        paste0(format(located_n, big.mark = ","), " located pitch(es) in view")
      ),
      tags$div(class = "cpp-heatmap-summary-note", note),
      tags$div(
        class = "cpp-chip-row",
        tags$span(class = "cpp-chip cpp-chip-strong", side_label),
        tags$span(class = "cpp-chip", pitch_label),
        tags$span(class = "cpp-chip", count_label)
      )
    )
  })

  output$cpp_mov_lhh <- plotly::renderPlotly({
    arm <- current_arm_angle()
    cape_pitcher_movement_plot(
      movement_data(),
      "Left",
      "cpp_mov_lhh_src",
      arm_angle = arm$angle,
      arm_throws = arm$throws
    )
  })

  output$cpp_mov_rhh <- plotly::renderPlotly({
    arm <- current_arm_angle()
    cape_pitcher_movement_plot(
      movement_data(),
      "Right",
      "cpp_mov_rhh_src",
      arm_angle = arm$angle,
      arm_throws = arm$throws
    )
  })

  observeEvent(
    suppressWarnings(
      plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event")
    ),
    {
      ed <- suppressWarnings(
        plotly::event_data("plotly_selected", source = "cpp_mov_lhh_src", priority = "event")
      )
      ids <- as.character(ed$customdata)
      ids <- ids[!is.na(ids) & nzchar(ids)]
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
      ids <- as.character(ed$customdata)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      selected_rows(ids)
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$cpp_clear_selection, {
    selected_rows(character(0))
  })

  output$cpp_selection_info <- renderText({
    sel_n <- length(selected_rows())
    d <- pitcher_raw()
    saved_n <- if ("base_has_saved_retag" %in% names(d)) {
      sum(d$base_has_saved_retag, na.rm = TRUE)
    } else {
      0L
    }
    if (sel_n == 0 && saved_n == 0) {
      "No pitches selected."
    } else if (sel_n == 0) {
      paste0("No pitches selected. ", saved_n, " saved retag(s) loaded.")
    } else if (saved_n == 0) {
      paste0(sel_n, " pitch(es) selected.")
    } else {
      paste0(sel_n, " pitch(es) selected. ", saved_n, " saved retag(s) loaded.")
    }
  })

  output$cpp_save_status <- renderUI({
    tags$div(
      class = paste("cpp-status", status_class()),
      status_message()
    )
  })

  observeEvent(input$cpp_apply_retag, {
    keys <- selected_rows()
    if (!length(keys)) {
      showNotification("Select pitches on a movement plot first.", type = "warning")
      return()
    }

    current <- pitcher_raw()
    rows <- which(current$base_pitch_key %in% keys)
    if (!length(rows)) {
      showNotification("Selected pitches are no longer available. Reload the page data and try again.", type = "error")
      return()
    }

    tryCatch({
      saved <- base_save_pitch_retags(current[rows, , drop = FALSE], input$cpp_new_pitch_type)
      retag_revision(retag_revision() + 1L)
      selected_rows(character(0))
      status_class("clean")
      status_message(paste0("Saved ", saved, " permanent pitch retag(s)."))
      showNotification(
        paste0("Permanently retagged ", saved, " pitch(es) as ", input$cpp_new_pitch_type, "."),
        type = "message"
      )
    }, error = function(e) {
      status_class("error")
      status_message(paste("Retag save failed:", conditionMessage(e)))
      showNotification(paste("Retag save failed:", conditionMessage(e)), type = "error")
    })
  })

  observeEvent(input$cpp_revert_retag, {
    keys <- selected_rows()
    if (!length(keys)) {
      showNotification("Select saved retags on a movement plot first.", type = "warning")
      return()
    }

    current <- pitcher_raw()
    rows <- which(current$base_pitch_key %in% keys)
    if (!length(rows)) {
      showNotification("Selected pitches are no longer available.", type = "error")
      return()
    }

    tryCatch({
      reverted <- base_revert_pitch_retags(current[rows, , drop = FALSE])
      retag_revision(retag_revision() + 1L)
      selected_rows(character(0))
      status_class("clean")
      status_message(paste0("Reverted ", reverted, " saved pitch retag(s) to source values."))
      showNotification(
        if (reverted > 0) paste0("Reverted ", reverted, " pitch retag(s).")
        else "None of the selected pitches had saved retags.",
        type = if (reverted > 0) "message" else "warning"
      )
    }, error = function(e) {
      status_class("error")
      status_message(paste("Retag revert failed:", conditionMessage(e)))
      showNotification(paste("Retag revert failed:", conditionMessage(e)), type = "error")
    })
  })

  output$cpp_arsenal <- renderReactable({
    cape_pitcher_make_table(
      pitcher_arsenal(pitcher_full()),
      pct = "Usage%",
      bar_pct = "Usage%",
      d1 = c("IVB", "HB", "Ext"),
      d3 = "xwOBAcon",
      int = c("#", "Spin")
    )
  })

  output$cpp_psplit <- renderReactable({
    cape_pitcher_make_table(
      cape_pitcher_split_table(pitcher_full()),
      pct = c("Whiff%", "Chase%", "K%", "BB%"),
      d3 = c("AVG", "OPS", "OBP", "SLG"),
      int = c("PA", "Stuff+", "Pitch+")
    )
  })

  output$cpp_perf_lhh <- renderReactable({
    cape_pitcher_make_table(
      pitcher_perf_side(pitcher_full(), "L"),
      pct = c("Zone%", "Whiff%", "Chase%"),
      d3 = "xwOBAcon",
      d1 = "RV",
      int = c("#", "Stuff+", "Loc+", "Pitch+")
    )
  })

  output$cpp_perf_rhh <- renderReactable({
    cape_pitcher_make_table(
      pitcher_perf_side(pitcher_full(), "R"),
      pct = c("Zone%", "Whiff%", "Chase%"),
      d3 = "xwOBAcon",
      d1 = "RV",
      int = c("#", "Stuff+", "Loc+", "Pitch+")
    )
  })

  output$cpp_pmix <- renderReactable({
    cape_pitcher_make_table(
      pmix_wide(pitcher_full()),
      pct = c("vs LHH", "vs RHH"),
      bar_pct = c("vs LHH", "vs RHH")
    )
  })

  output$cpp_pusage <- renderReactable({
    cape_pitcher_make_table(
      pusage(pitcher_full()),
      pct = c("Usage%", "Whiff%"),
      bar_pct = "Usage%"
    )
  })

  output$cpp_pitch_xw_table <- renderReactable({
    cape_pitcher_make_table(
      pitcher_xw_table(pitcher_full()),
      d3 = c("xwOBAcon", "xwOBA"),
      int = c("#", "BBE")
    )
  })

  output$cpp_pitch_zone <- renderPlot({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(cape_pitcher_empty_heatmap_plot("No pitches match the current heatmap filter."))
    }
    cape_pitcher_zone_heat_plot(d, "freq", input$cpp_heat_side %||% "ALL")
  }, res = 96)

  output$cpp_pitch_whiff_zone <- renderPlot({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(cape_pitcher_empty_heatmap_plot("No pitches match the current heatmap filter."))
    }
    cape_pitcher_zone_heat_plot(d, "whiff", input$cpp_heat_side %||% "ALL")
  }, res = 96)

  output$cpp_pitch_dmg_zone <- renderPlot({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(cape_pitcher_empty_heatmap_plot("No pitches match the current heatmap filter."))
    }
    cape_pitcher_zone_heat_plot(d, "damage", input$cpp_heat_side %||% "ALL")
  }, res = 96)

  output$cpp_pitch_xwc_zone <- renderPlot({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(cape_pitcher_empty_heatmap_plot("No pitches match the current heatmap filter."))
    }
    cape_pitcher_zone_heat_plot(d, "xwoba", input$cpp_heat_side %||% "ALL")
  }, res = 96)

  output$cpp_pitch_xwf_zone <- renderPlot({
    d <- heatmap_data()
    if (nrow(d) == 0) {
      return(cape_pitcher_empty_heatmap_plot("No pitches match the current heatmap filter."))
    }
    cape_pitcher_zone_heat_plot(d, "xwobafull", input$cpp_heat_side %||% "ALL")
  }, res = 96)
}
