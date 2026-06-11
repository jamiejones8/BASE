print_replace_na <- function(x, replacement = "\u2014") {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- replacement
  x
}

print_safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

build_print_table <- function(df, class = "print-table") {
  if (is.null(df) || nrow(df) == 0) {
    return(div(class = "print-empty", "No data available for the current filters."))
  }

  df[] <- lapply(df, print_replace_na)

  tags$table(
    class = class,
    tags$thead(
      tags$tr(
        lapply(names(df), function(col) tags$th(col))
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          lapply(df[i, , drop = TRUE], function(value) tags$td(value))
        )
      })
    )
  )
}

print_table_card <- function(title, content, note = NULL, extra_class = NULL) {
  div(
    class = paste(c("print-card", extra_class), collapse = " "),
    if (!is.null(title) && nzchar(title)) {
      div(class = "print-card-title", title)
    },
    if (!is.null(note) && nzchar(note)) {
      div(class = "print-card-note", note)
    },
    content
  )
}

build_ranked_print_table <- function(df,
                                     entity_col,
                                     value_col,
                                     formatter,
                                     descending = TRUE,
                                     n = 10,
                                     sort_col = value_col,
                                     value_label = "Value",
                                     entity_label = entity_col,
                                     filter_fn = NULL) {
  work <- df

  if (is.null(work) || nrow(work) == 0) {
    return(data.frame())
  }

  if (!is.null(filter_fn)) {
    work <- filter_fn(work)
  }

  if (
    is.null(work) || nrow(work) == 0 ||
      !entity_col %in% names(work) ||
      !sort_col %in% names(work) ||
      !value_col %in% names(work)
  ) {
    return(data.frame())
  }

  sort_values <- print_safe_num(work[[sort_col]])
  keep <- !is.na(sort_values) &
    !is.na(work[[entity_col]]) &
    nzchar(trimws(as.character(work[[entity_col]])))

  work <- work[keep, , drop = FALSE]
  if (!nrow(work)) {
    return(data.frame())
  }

  sort_values <- print_safe_num(work[[sort_col]])
  ord <- if (isTRUE(descending)) {
    order(-sort_values, as.character(work[[entity_col]]))
  } else {
    order(sort_values, as.character(work[[entity_col]]))
  }

  work <- utils::head(work[ord, , drop = FALSE], n)

  data.frame(
    `#` = seq_len(nrow(work)),
    setNames(list(as.character(work[[entity_col]])), entity_label),
    setNames(list(formatter(work[[value_col]])), value_label),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

print_stat_grid <- function(stats) {
  if (is.null(stats) || length(stats) == 0) {
    return(NULL)
  }

  if (is.data.frame(stats)) {
    stats <- split(stats, seq_len(nrow(stats)))
    stats <- lapply(stats, function(row) {
      list(label = row$label[[1]], value = row$value[[1]])
    })
  }

  div(
    class = "print-stat-grid",
    lapply(stats, function(stat) {
      div(
        class = "print-stat-card",
        div(class = "print-stat-value", stat$value),
        div(class = "print-stat-label", stat$label)
      )
    })
  )
}

print_meta_bar <- function(items) {
  if (is.null(items) || !length(items)) {
    return(NULL)
  }

  div(
    class = "print-meta-bar",
    lapply(items, function(item) {
      div(
        class = "print-meta-pill",
        tags$span(class = "print-meta-label", item$label),
        tags$span(class = "print-meta-value", item$value)
      )
    })
  )
}

print_report_page <- function(title, subtitle = NULL, meta_items = NULL, body = NULL, note = NULL) {
  div(
    class = "print-report",
    div(
      class = "print-report-header",
      div(
        class = "print-report-header-top",
        div(
          class = "print-report-logo-wrap",
          tags$img(
            src = BRAND_LOGO_FILE,
            class = "print-report-logo",
            alt = ORGANIZATION_DISPLAY_NAME
          )
        ),
        div(
          class = "print-report-title-block",
          div(class = "print-report-brand", APP_TITLE),
          div(class = "print-report-title", title),
          if (!is.null(subtitle) && nzchar(subtitle)) {
            div(class = "print-report-subtitle", subtitle)
          }
        )
      ),
      print_meta_bar(meta_items)
    ),
    body,
    if (!is.null(note) && nzchar(note)) {
      div(class = "print-report-note", note)
    }
  )
}

print_two_column <- function(left, right) {
  div(
    class = "print-two-column",
    div(class = "print-column", left),
    div(class = "print-column", right)
  )
}

print_card_grid <- function(cards, columns = 3, extra_class = NULL) {
  cards <- Filter(Negate(is.null), cards)

  div(
    class = paste(c("print-card-grid", paste0("cols-", columns), extra_class), collapse = " "),
    cards
  )
}
