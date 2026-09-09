library(shiny)
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "r-user-cache"))
library(bslib)
library(DT)
library(dplyr)
library(readr)

JUCO_EMBEDDED_MODE <- isTRUE(get0(
  "BASE_JUCO_EMBEDDED",
  inherits = FALSE,
  ifnotfound = FALSE
))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

resolve_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit) || !nzchar(hit)) {
    return(normalizePath(paths[[1]], winslash = "/", mustWork = FALSE))
  }
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

app_data_path <- resolve_existing(c(
  file.path("data", "juco_player_stats_latest.csv"),
  file.path("JucoStatsApp", "data", "juco_player_stats_latest.csv"),
  file.path("data", "juco_player_stats_inventory_sample.csv"),
  file.path("JucoStatsApp", "data", "juco_player_stats_inventory_sample.csv")
))
profile_path <- resolve_existing(c(
  file.path("data", "player_profiles.csv"),
  file.path("JucoStatsApp", "data", "player_profiles.csv")
))
hitting_ref_paths <- c(
  file.path("data", "d1_hitting_metric_percentile_reference.csv"),
  file.path("JucoStatsApp", "data", "d1_hitting_metric_percentile_reference.csv"),
  file.path("..", "HittingApp", "data", "d1_hitting_metric_percentile_reference.csv"),
  file.path("HittingApp", "data", "d1_hitting_metric_percentile_reference.csv"),
  file.path("..", "PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv"),
  file.path("PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv")
)
pitching_ref_paths <- c(
  file.path("data", "d1_pitch_metric_percentile_reference.csv"),
  file.path("JucoStatsApp", "data", "d1_pitch_metric_percentile_reference.csv"),
  file.path("..", "PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv"),
  file.path("PitchingApp", "data", "d1_pitch_metric_percentile_reference.csv")
)
hitting_ref_paths <- vapply(hitting_ref_paths[file.exists(hitting_ref_paths)], normalizePath,
                            character(1), winslash = "/", mustWork = TRUE)
pitching_ref_paths <- vapply(pitching_ref_paths[file.exists(pitching_ref_paths)], normalizePath,
                             character(1), winslash = "/", mustWork = TRUE)
non_player_names <- c(
  "opponent", "opponents", "team", "team totals", "totals", "total",
  "overall", "conference", "conf", "non-conference", "non conference",
  "home", "away", "neutral", "exhibition", "division", "vs ranked"
)

txst_maroon <- "#501214"
txst_gold <- "#B4975A"

txst_theme <- bs_theme(
  version = 5,
  primary = txst_maroon,
  secondary = txst_gold
)

head_css <- tags$head(
  tags$style(HTML("
    :root{
      --txst-maroon:#501214;
      --txst-gold:#B4975A;
      --muted-border:#d9d5cb;
    }
    body{background:#f7f7f5;color:#1f1f1f;}
    .navbar,
    .navbar.bg-primary,
    .navbar-dark,
    .navbar.navbar-default.navbar-static-top,
    body > nav.navbar.navbar-default.navbar-static-top,
    .bslib-navbar,
    .bslib-page-header{
      background:var(--txst-maroon) !important;
      background-color:var(--txst-maroon) !important;
      background-image:none !important;
      border:0 !important;
      box-shadow:none !important;
    }
    .navbar .navbar-brand,
    .navbar .nav-link,
    .navbar-dark .navbar-brand,
    .navbar-dark .navbar-nav .nav-link{
      color:var(--txst-gold) !important;
      font-weight:700;
    }
    .navbar .nav-link.active{
      border-bottom:3px solid var(--txst-gold);
    }
    .juco-shell{
      padding:18px 18px 28px;
      max-width:1600px;
      margin:0 auto;
    }
    .common-strip{
      border-left:4px solid var(--txst-maroon);
      padding:8px 12px;
      margin-bottom:12px;
      background:#fff;
    }
    .common-strip strong{color:var(--txst-maroon);}
    .filter-row{
      display:grid;
      grid-template-columns:repeat(4, minmax(180px, 1fr));
      gap:12px;
      align-items:start;
      margin-bottom:12px;
    }
    .filter-row.pitching{
      grid-template-columns:repeat(3, minmax(180px, 1fr));
    }
    .filter-row .form-group{margin-bottom:0;}
    .filter-row label{
      font-size:.78rem;
      font-weight:800;
      color:var(--txst-maroon);
      text-transform:uppercase;
    }
    .checkbox-panel{
      background:rgba(255,255,255,.88);
      border:1px solid var(--muted-border);
      border-radius:4px;
      padding:8px 10px;
      min-height:100%;
    }
    .checkbox-panel .form-check{
      display:inline-flex;
      align-items:center;
      margin:2px 12px 2px 0;
      min-width:58px;
    }
    .checkbox-panel .form-check-input{
      border-color:var(--txst-maroon);
      box-shadow:none;
      margin-right:5px;
    }
    .checkbox-panel .form-check-input:checked{
      background-color:var(--txst-maroon);
      border-color:var(--txst-maroon);
    }
    .checkbox-panel .form-check-label{
      color:#242424;
      font-size:.82rem;
      font-weight:700;
      text-transform:none;
    }
    .table-title{
      background-color:var(--txst-maroon);
      color:var(--txst-gold);
      font-weight:800;
      padding:7px 11px;
      border-radius:4px;
      display:inline-block;
      margin-bottom:6px;
    }
    .dt-wrap{
      background:rgba(255,255,255,.93);
      border:1px solid var(--muted-border);
      padding:10px;
      position:relative;
      overflow:hidden;
    }
    .dt-wrap::before{
      content:'';
      position:absolute;
      inset:0;
      background:url('/Bobcatlogo.png') center 54% / min(42vw, 420px) auto no-repeat;
      opacity:.055;
      pointer-events:none;
      z-index:0;
    }
    .dt-wrap .dataTables_wrapper{
      position:relative;
      z-index:1;
    }
    table.dataTable,
    table.dataTable th,
    table.dataTable td{
      border:none !important;
      box-shadow:none !important;
    }
    table.dataTable{
      border-collapse:separate !important;
      border-spacing:0 !important;
      width:100% !important;
    }
    table.dataTable thead th{
      background-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
      font-weight:800;
      white-space:nowrap;
    }
    table.dataTable tbody tr.odd td{
      background-color:rgba(245,244,240,.88) !important;
    }
    table.dataTable tbody tr.even td{
      background-color:rgba(255,255,255,.88) !important;
    }
    table.dataTable tbody td{
      padding:8px 10px;
      line-height:1.15;
      white-space:nowrap;
    }
    table.dataTable tbody tr.selected td,
    table.dataTable tbody tr.selected th,
    table.dataTable tbody tr.selected a{
      color:var(--txst-maroon) !important;
    }
    table.dataTable tbody tr.selected td{
      background-color:rgba(180,151,90,.25) !important;
      box-shadow:inset 0 0 0 9999px rgba(180,151,90,.16) !important;
    }
    table.dataTable thead input[type='search'],
    table.dataTable thead input[type='text'],
    table.dataTable thead input[type='number']{
      width:100% !important;
      min-width:46px;
      box-sizing:border-box;
      border:1px solid var(--muted-border);
      border-radius:3px;
      padding:4px 6px;
      font-size:.74rem;
      color:#1f1f1f;
    }
    table.dataTable thead input[type='search']:not(:placeholder-shown),
    table.dataTable thead input[type='text']:not(:placeholder-shown),
    table.dataTable thead input[type='number']:not(:placeholder-shown){
      min-width:128px;
    }
    table.dataTable thead tr:nth-child(2) th{
      min-width:48px;
      padding:5px 6px !important;
    }
    .dataTables_scrollHeadInner,
    .dataTables_scrollHeadInner table{
      min-width:100%;
    }
    .dataTables_scrollBody thead{
      visibility:hidden;
    }
    table.dataTable td.dt-shaded{
      font-weight:700;
    }
    .cf-cell{
      display:block;
      margin:-8px -10px;
      padding:8px 10px;
      min-height:100%;
    }
    table.dataTable tbody tr.selected .cf-cell{
      color:var(--txst-maroon) !important;
    }
    .juco-head-filter{
      position:relative;
      min-width:90px;
    }
    .juco-head-filter-btn{
      width:100%;
      min-width:90px;
      border:1px solid var(--muted-border);
      border-radius:3px;
      background:#fff;
      color:#1f1f1f;
      font-size:.74rem;
      font-weight:700;
      line-height:1.1;
      padding:5px 22px 5px 7px;
      text-align:left;
      position:relative;
    }
    .juco-head-filter-btn::after{
      content:'';
      position:absolute;
      right:8px;
      top:50%;
      width:0;
      height:0;
      border-left:4px solid transparent;
      border-right:4px solid transparent;
      border-top:5px solid var(--txst-maroon);
      transform:translateY(-35%);
    }
    .juco-head-filter-btn.active{
      border-color:var(--txst-maroon);
      box-shadow:0 0 0 2px rgba(80,18,20,.13);
    }
    .juco-head-filter-menu{
      display:none;
      position:absolute;
      left:0;
      top:calc(100% + 3px);
      z-index:30;
      min-width:180px;
      max-height:260px;
      overflow:auto;
      background:#fff;
      color:#1f1f1f;
      border:1px solid var(--muted-border);
      box-shadow:0 8px 20px rgba(0,0,0,.18);
      padding:7px;
      text-align:left;
    }
    .juco-head-filter.open .juco-head-filter-menu{display:block;}
    .juco-head-filter-menu label{
      display:flex;
      align-items:center;
      gap:6px;
      color:#1f1f1f;
      font-size:.78rem;
      font-weight:700;
      line-height:1.15;
      margin:0;
      padding:4px 2px;
      text-transform:none;
      white-space:nowrap;
    }
    .juco-head-filter-menu input[type='checkbox']{
      width:13px;
      height:13px;
      min-width:13px;
      accent-color:var(--txst-maroon);
    }
    .juco-min-filter{
      display:flex;
      align-items:center;
      gap:3px;
      min-width:84px;
    }
    .juco-min-filter input[type='number']{
      min-width:58px !important;
      width:58px !important;
      padding-right:3px !important;
    }
    .juco-min-clear{
      border:1px solid var(--muted-border);
      border-radius:3px;
      background:#fff;
      color:var(--txst-maroon);
      font-weight:900;
      line-height:1;
      width:20px;
      height:24px;
      padding:0;
    }
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select{
      border:1px solid var(--muted-border);
      border-radius:4px;
      padding:4px 7px;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current{
      background:var(--txst-maroon) !important;
      border-color:var(--txst-maroon) !important;
      color:var(--txst-gold) !important;
    }
    @media (max-width: 900px){
      .filter-row,.filter-row.pitching{grid-template-columns:1fr;}
      .juco-shell{padding:12px;}
      .dt-wrap::before{background-size:320px auto;}
    }
  "))
)

read_app_stats <- function() {
  if (!file.exists(app_data_path)) return(tibble())
  read_csv(app_data_path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(player_name = as.character(player_name), player_key = tolower(trimws(player_name))) %>%
    filter(!player_key %in% non_player_names, !grepl("^[0-9]+$", player_key)) %>%
    select(-player_key)
}

read_profiles <- function() {
  if (!file.exists(profile_path)) {
    return(tibble(
      program_name = character(),
      player_name = character(),
      class = character(),
      bats = character(),
      throws = character(),
      position_raw = character(),
      position_buckets = character()
    ))
  }
  read_csv(profile_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

ip_to_outs <- function(ip) {
  ip <- as_num(ip)
  whole <- floor(ip)
  tenths <- round((ip - whole) * 10)
  ifelse(is.na(ip), NA_real_, whole * 3 + pmin(tenths, 2))
}

normalize_class <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  case_when(
    grepl("RS|RED\\s*SHIRT|REDSHIRT", raw) & grepl("FR|FRESH", raw) ~ "RS FR",
    grepl("RS|RED\\s*SHIRT|REDSHIRT", raw) & grepl("SO|SOPH", raw) ~ "RS SO",
    grepl("FR|FRESH", raw) ~ "FR",
    grepl("SO|SOPH", raw) ~ "SO",
    TRUE ~ NA_character_
  )
}

normalize_hand <- function(x, allowed = c("L", "R", "S")) {
  raw <- toupper(trimws(as.character(x)))
  ifelse(raw %in% allowed, raw, NA_character_)
}

position_match <- function(position_buckets, selected) {
  if (length(selected) == 0) return(rep(FALSE, length(position_buckets)))
  selected_known <- setdiff(selected, "Unknown")
  has_known <- vapply(
    strsplit(ifelse(is.na(position_buckets), "", position_buckets), "\\|"),
    function(tokens) any(tokens %in% selected_known),
    logical(1)
  )
  has_unknown <- is.na(position_buckets) | position_buckets == ""
  has_known | (("Unknown" %in% selected) & has_unknown)
}

metadata_match <- function(value, selected) {
  if (length(selected) == 0) return(rep(FALSE, length(value)))
  known <- !is.na(value) & value != ""
  (known & value %in% selected) | (!known & "Unknown" %in% selected)
}

load_percentile_ref <- local({
  cache <- list()
  function(paths) {
    path <- paths[file.exists(paths)][1]
    if (is.na(path) || !nzchar(path)) {
      return(tibble(scope = character(), metric = character(), value = double()))
    }
    mtime <- file.info(path)$mtime
    key <- normalizePath(path, mustWork = FALSE)
    cached <- cache[[key]]
    if (!is.null(cached) && identical(cached$mtime, mtime)) return(cached$data)
    data <- suppressMessages(read_csv(path, show_col_types = FALSE)) %>%
      mutate(
        scope = as.character(scope),
        metric = as.character(metric),
        value = suppressWarnings(as.numeric(value))
      )
    cache[[key]] <<- list(mtime = mtime, data = data)
    data
  }
})

metric_baseline <- function(metric) {
  baselines <- c(
    performance_obp = 0.384,
    performance_slg = 0.441,
    performance_ops = 0.825,
    performance_k_pct = 0.159,
    performance_bb_pct = 0.095
  )
  metric <- as.character(metric %||% "")
  if (!metric %in% names(baselines)) return(NA_real_)
  unname(baselines[[metric]])
}

metric_step <- function(metric, baseline) {
  if (metric %in% c("performance_obp", "performance_slg", "performance_ops")) return(0.100)
  if (grepl("_pct$", metric)) return(0.050)
  max(abs(baseline) * 0.05, 0.01)
}

baseline_percentile <- function(value, metric, lower_better = FALSE) {
  baseline <- metric_baseline(metric)
  if (!is.finite(value) || !is.finite(baseline) || baseline == 0) return(NA_real_)
  delta <- if (isTRUE(lower_better)) baseline - value else value - baseline
  pmin(99, pmax(1, round(50 + 10 * delta / metric_step(metric, baseline))))
}

d1_percentile <- function(value, metric, lower_better = FALSE, ref_paths = character()) {
  if (!is.finite(value)) return(NA_real_)
  ref <- load_percentile_ref(ref_paths)
  vals <- ref %>%
    filter(.data$scope == "overall", .data$metric == .env$metric, is.finite(.data$value)) %>%
    pull(.data$value)
  if (!length(vals)) return(baseline_percentile(value, metric, lower_better))
  pct <- if (isTRUE(lower_better)) mean(vals >= value, na.rm = TRUE) else mean(vals <= value, na.rm = TRUE)
  pmin(99, pmax(1, round(100 * pct)))
}

percentile_bucket <- function(pct) {
  case_when(
    !is.finite(pct) ~ NA_character_,
    pct >= 90 ~ "elite",
    pct >= 75 ~ "plus",
    pct >= 60 ~ "above",
    pct >= 40 ~ "average",
    pct >= 25 ~ "below",
    pct >= 10 ~ "poor",
    TRUE ~ "bottom"
  )
}

percentile_style <- function(pct) {
  if (!is.finite(pct) || (pct >= 40 && pct < 60)) return(list(fill = NA_character_, text = "#1a1a1a"))
  if (pct >= 90) return(list(fill = "#1F5B42", text = "#FFFFFF"))
  if (pct >= 75) return(list(fill = "#34795A", text = "#FFFFFF"))
  if (pct >= 60) return(list(fill = "#D9EADF", text = "#183A2B"))
  if (pct >= 25) return(list(fill = "#F4D6D2", text = "#652427"))
  if (pct >= 10) return(list(fill = "#C75A52", text = "#FFFFFF"))
  list(fill = "#8E2E32", text = "#FFFFFF")
}

format_metric_value <- function(value, kind = "num1") {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) return("")
  switch(
    kind,
    pct = sprintf("%.1f%%", 100 * value),
    dec3 = sub("^0\\.", ".", sprintf("%.3f", value)),
    num2 = sprintf("%.2f", value),
    num0 = sprintf("%.0f", value),
    sprintf("%.1f", value)
  )
}

shade_cell <- function(value, pct, kind = "num1") {
  order_attr <- if (is.finite(suppressWarnings(as.numeric(value)))) sprintf("%.8f", as.numeric(value)) else ""
  text <- format_metric_value(value, kind)
  style <- percentile_style(pct)
  if (is.na(style$fill)) {
    return(sprintf("<span class='cf-cell' data-order='%s'>%s</span>", order_attr, text))
  }
  sprintf(
    "<span class='cf-cell' data-order='%s' style='background-color:%s;color:%s;font-weight:700;'>%s</span>",
    order_attr,
    style$fill,
    style$text,
    text
  )
}

shade_specs_hitting <- tibble::tribble(
  ~column, ~metric, ~lower_better, ~kind,
  "K%", "performance_k_pct", TRUE, "pct",
  "BB%", "performance_bb_pct", FALSE, "pct",
  "OBP", "performance_obp", FALSE, "dec3",
  "SLG", "performance_slg", FALSE, "dec3",
  "OPS", "performance_ops", FALSE, "dec3"
)

shade_specs_pitching <- tibble::tribble(
  ~column, ~metric, ~lower_better, ~kind,
  "K/9", "performance_k9", FALSE, "num1",
  "BB/9", "performance_bb9", TRUE, "num1",
  "H/9", "performance_h9", TRUE, "num1",
  "WHIP", "performance_whip", TRUE, "num2"
)

apply_shading_html <- function(df, specs, ref_paths) {
  out <- df
  for (i in seq_len(nrow(specs))) {
    column <- specs$column[[i]]
    if (!column %in% names(out)) next
    values <- suppressWarnings(as.numeric(out[[column]]))
    pcts <- vapply(
      values,
      d1_percentile,
      metric = specs$metric[[i]],
      lower_better = isTRUE(specs$lower_better[[i]]),
      ref_paths = ref_paths,
      FUN.VALUE = numeric(1)
    )
    out[[column]] <- mapply(shade_cell, values, pcts, MoreArgs = list(kind = specs$kind[[i]]), USE.NAMES = FALSE)
  }
  out
}

leaderboard_filter_callback <- function(multi_cols, min_col = NULL, min_value = NULL) {
  multi_json <- jsonlite::toJSON(as.character(multi_cols), auto_unbox = TRUE)
  min_spec <- list(
    column = min_col %||% "",
    value = if (is.null(min_value)) NA_real_ else min_value
  )
  min_json <- jsonlite::toJSON(min_spec, auto_unbox = TRUE, na = "null")

  JS(paste0(
"
  var api = table;
  var container = $(api.table().container());
  var tableNode = api.table().node();
  var outputId = container.closest('.datatables').attr('id') || tableNode.id || ('table-' + Math.random().toString(36).slice(2));
  window.jucoHeaderFilterState = window.jucoHeaderFilterState || {};
  window.jucoHeaderFilterState[outputId] = window.jucoHeaderFilterState[outputId] || {multi: {}, min: {}};
  var persisted = window.jucoHeaderFilterState[outputId];
  if (container[0] && container[0]._jucoHeaderFiltersReady) return;
  if (container[0]) container[0]._jucoHeaderFiltersReady = true;
  var multiNames = ", multi_json, ";
  var minSpec = ", min_json, ";
  var multiState = persisted.multi || {};
  var optionState = {};
  var minState = persisted.min || {};
  var namespace = '.jucoHeadFilter' + Math.random().toString(36).slice(2);

  function stripHtml(value) {
    return $('<div>').html(value == null ? '' : String(value)).text().trim();
  }

  function escapeHtml(value) {
    return String(value == null ? '' : value).replace(/[&<>\"']/g, function(ch) {
      return ({'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'})[ch];
    });
  }

  function displayValue(value) {
    return value === '' ? 'Blank' : value;
  }

  function columnIndex(name) {
    var found = -1;
    api.columns().every(function(index) {
      var title = stripHtml($(this.header()).text());
      if (title === name) found = index;
    });
    return found;
  }

  function filterCells(index) {
    var row = container.find('.dataTables_scrollHead thead tr').eq(1);
    if (!row.length) row = container.find('thead tr').eq(1);
    return row.children('th,td').eq(index);
  }

  function uniqueValues(index) {
    var values = [];
    api.column(index).data().toArray().forEach(function(value) {
      var clean = stripHtml(value);
      if (values.indexOf(clean) === -1) values.push(clean);
    });
    return values.sort(function(a, b) {
      if (a === '') return 1;
      if (b === '') return -1;
      return a.localeCompare(b);
    });
  }

  function updateButton(index) {
    var selected = multiState[index] || [];
    var total = optionState[index] || [];
    var label = 'All';
    if (selected.length === 0) label = 'None';
    else if (selected.length === total.length) label = 'All';
    else if (selected.length === 1) label = displayValue(selected[0]);
    else if (selected.length < total.length) label = selected.length + ' selected';
    container.find('.juco-head-filter[data-col=\"' + index + '\"] .juco-head-filter-btn').text(label);
  }

  function updateMinInputs(index, value) {
    var display = Number.isFinite(value) ? String(value) : '';
    container.find('.juco-min-filter[data-col=\"' + index + '\"] input[type=\"number\"]').val(display);
  }

  function setChecks(index) {
    var selected = multiState[index] || [];
    container.find('.juco-head-filter[data-col=\"' + index + '\"] input[type=\"checkbox\"]').each(function() {
      this.checked = selected.indexOf($(this).attr('data-value')) !== -1;
    });
    updateButton(index);
  }

  function installMultiFilter(name) {
    var index = columnIndex(name);
    if (index < 0) return;
    var values = uniqueValues(index);
    optionState[index] = values;
    if (!Array.isArray(multiState[index])) multiState[index] = values.slice();
    var optionsHtml = values.map(function(value) {
      var checked = multiState[index].indexOf(value) !== -1 ? ' checked' : '';
      return '<label><input type=\"checkbox\" data-value=\"' + escapeHtml(value) + '\"' + checked + '> ' + escapeHtml(displayValue(value)) + '</label>';
    }).join('');
    var controlHtml = '<div class=\"juco-head-filter\" data-col=\"' + index + '\"><button type=\"button\" class=\"juco-head-filter-btn\">All</button><div class=\"juco-head-filter-menu\">' + optionsHtml + '</div></div>';
    filterCells(index).empty().append(controlHtml);
    updateButton(index);
  }

  function installMinFilter(name, value) {
    var index = columnIndex(name);
    if (index < 0) return;
    var numericValue = Number(value);
    if (!Object.prototype.hasOwnProperty.call(minState, String(index))) {
      minState[index] = Number.isFinite(numericValue) ? numericValue : null;
    }
    var display = minState[index] == null ? '' : String(minState[index]);
    var controlHtml = '<div class=\"juco-min-filter\" data-col=\"' + index + '\"><input type=\"number\" step=\"0.1\" placeholder=\"Min\" value=\"' + escapeHtml(display) + '\"><button type=\"button\" class=\"juco-min-clear\" aria-label=\"Clear minimum filter\">x</button></div>';
    filterCells(index).empty().append(controlHtml);
  }

  var customFilter = function(filterSettings, data) {
    if (filterSettings.nTable !== tableNode) return true;
    for (var key in multiState) {
      var index = Number(key);
      var selected = multiState[index] || [];
      var options = optionState[index] || [];
      if (selected.length === options.length) continue;
      if (selected.length === 0) return false;
      var value = stripHtml(data[index]);
      if (selected.indexOf(value) === -1) return false;
    }
    for (var minKey in minState) {
      var minIndex = Number(minKey);
      var minValue = minState[minIndex];
      if (!Number.isFinite(minValue)) continue;
      var raw = stripHtml(data[minIndex]).replace(/[^0-9.\\-]/g, '');
      var numberValue = Number(raw);
      if (!Number.isFinite(numberValue) || numberValue < minValue) return false;
    }
    return true;
  };

  $.fn.dataTable.ext.search.push(customFilter);
  $(tableNode).on('destroy.dt', function() {
    var filters = $.fn.dataTable.ext.search;
    var index = filters.indexOf(customFilter);
    if (index >= 0) filters.splice(index, 1);
    $(document).off(namespace);
  });

  multiNames.forEach(installMultiFilter);
  if (minSpec.column) installMinFilter(minSpec.column, minSpec.value);
  api.draw();

  container.on('click', '.juco-head-filter-btn', function(event) {
    event.preventDefault();
    event.stopPropagation();
    var parent = $(this).closest('.juco-head-filter');
    container.find('.juco-head-filter').not(parent).removeClass('open').find('.juco-head-filter-btn').removeClass('active');
    parent.toggleClass('open');
    $(this).toggleClass('active', parent.hasClass('open'));
  });

  container.on('change', '.juco-head-filter-menu input[type=\"checkbox\"]', function() {
    var parent = $(this).closest('.juco-head-filter');
    var index = Number(parent.attr('data-col'));
    var selected = [];
    parent.find('input[type=\"checkbox\"]:checked').each(function() {
      selected.push($(this).attr('data-value'));
    });
    multiState[index] = selected;
    persisted.multi = multiState;
    setChecks(index);
    api.draw();
  });

  container.on('input change', '.juco-min-filter input[type=\"number\"]', function() {
    var parent = $(this).closest('.juco-min-filter');
    var index = Number(parent.attr('data-col'));
    var rawValue = String($(this).val() || '').trim();
    var value = Number(rawValue);
    minState[index] = rawValue === '' ? null : (Number.isFinite(value) ? value : null);
    persisted.min = minState;
    updateMinInputs(index, minState[index]);
    api.draw();
  });

  container.on('click', '.juco-min-clear', function(event) {
    event.preventDefault();
    var parent = $(this).closest('.juco-min-filter');
    var index = Number(parent.attr('data-col'));
    minState[index] = null;
    persisted.min = minState;
    updateMinInputs(index, minState[index]);
    api.draw();
  });

  $(document).on('click' + namespace, function(event) {
    if ($(event.target).closest(container).length) return;
    container.find('.juco-head-filter').removeClass('open').find('.juco-head-filter-btn').removeClass('active');
  });
"
  ))
}

attach_profiles <- function(stats, profiles) {
  stats %>%
    mutate(
      player_name = as.character(player_name),
      class_from_stat = normalize_class(yr),
      throws_from_pos = case_when(
        grepl("\\bLHP\\b", toupper(pos %||% ""), perl = TRUE) ~ "L",
        grepl("\\bRHP\\b", toupper(pos %||% ""), perl = TRUE) ~ "R",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(profiles, by = c("program_name", "player_name"), suffix = c("", "_profile")) %>%
    mutate(
      class = coalesce(class, class_from_stat),
      bats = normalize_hand(bats, c("L", "R", "S")),
      throws = coalesce(normalize_hand(throws, c("L", "R")), throws_from_pos),
      position_raw = coalesce(position_raw, pos),
      position_buckets = coalesce(position_buckets, NA_character_)
    )
}

build_hitting <- function(stats, profiles) {
  stats %>%
    filter(stat_type == "hitting") %>%
    attach_profiles(profiles) %>%
    transmute(
      Player = player_name,
      School = program_name,
      Region = source_group,
      State = state,
      Class = class,
      Bats = bats,
      Throws = throws,
      Pos = position_raw,
      position_buckets,
      G = as_num(g),
      AB = as_num(ab),
      AVG = as_num(avg),
      `K%` = safe_div(as_num(k), as_num(ab) + as_num(bb)),
      `BB%` = safe_div(as_num(bb), as_num(ab) + as_num(bb)),
      OBP = as_num(obp),
      SLG = as_num(slg),
      OPS = coalesce(as_num(ops), as_num(obp) + as_num(slg)),
      `2B` = as_num(`2b`),
      `3B` = as_num(`3b`),
      HR = as_num(hr),
      RBI = as_num(rbi)
    ) %>%
    arrange(desc(OPS), desc(SLG), desc(AVG))
}

build_pitching <- function(stats, profiles) {
  stats %>%
    filter(stat_type == "pitching") %>%
    attach_profiles(profiles) %>%
    mutate(outs = ip_to_outs(ip)) %>%
    transmute(
      Player = player_name,
      School = program_name,
      Region = source_group,
      State = state,
      Class = class,
      Bats = bats,
      Throws = throws,
      Pos = position_raw,
      APP = as_num(app),
      IP = as_num(ip),
      `K/9` = safe_div(as_num(k) * 27, outs),
      `BB/9` = safe_div(as_num(bb) * 27, outs),
      `H/9` = safe_div(as_num(h) * 27, outs),
      WHIP = as_num(whip),
      ERA = as_num(era),
      HR = as_num(hr)
    ) %>%
    arrange(ERA, WHIP, desc(`K/9`))
}

table_options <- list(
  pageLength = 50,
  lengthMenu = c(25, 50, 100, -1),
  scrollX = TRUE,
  autoWidth = FALSE,
  searchHighlight = TRUE
)

column_width_defs <- function(df) {
  width_for <- function(name) {
    dplyr::case_when(
      name == "Player" ~ "128px",
      name == "School" ~ "175px",
      name %in% c("Region", "State", "Bats", "Throws", "Pos") ~ "98px",
      name == "Class" ~ "60px",
      name %in% c("AB", "IP") ~ "72px",
      name %in% c("G", "APP", "2B", "3B", "HR", "RBI") ~ "48px",
      TRUE ~ "68px"
    )
  }

  Map(
    function(index, name) list(width = width_for(name), targets = index - 1L),
    seq_along(names(df)),
    names(df)
  )
}

render_leaderboard <- function(
  df,
  percentage_cols = character(),
  three_dec_cols = character(),
  two_dec_cols = character(),
  one_dec_cols = character(),
  shade_specs = NULL,
  ref_paths = character(),
  header_filter = NULL
) {
  shaded_cols <- character()
  if (!is.null(shade_specs) && nrow(shade_specs)) {
    shaded_cols <- intersect(shade_specs$column, names(df))
    df <- apply_shading_html(df, shade_specs, ref_paths)
  }
  dt_options <- table_options
  dt_options$columnDefs <- column_width_defs(df)
  dt <- datatable(
    df,
    rownames = FALSE,
    filter = "top",
    class = "stripe compact cell-border",
    selection = "single",
    escape = FALSE,
    options = dt_options,
    callback = header_filter %||% JS("")
  )
  if (length(percentage_cols)) dt <- formatPercentage(dt, setdiff(percentage_cols, shaded_cols), digits = 1)
  if (length(three_dec_cols)) dt <- formatRound(dt, setdiff(three_dec_cols, shaded_cols), digits = 3)
  if (length(two_dec_cols)) dt <- formatRound(dt, setdiff(two_dec_cols, shaded_cols), digits = 2)
  if (length(one_dec_cols)) dt <- formatRound(dt, setdiff(one_dec_cols, shaded_cols), digits = 1)
  dt
}

filter_controls <- function(prefix, include_position = FALSE) {
  class_choices <- c("FR", "RS FR", "SO", "RS SO", "Unknown")
  throwing_choices <- c("R", "L", "Unknown")
  batting_choices <- c("R", "L", "S", "Unknown")
  position_choices <- c("Outfield", "Infield", "Corner Infield", "MIF", "Catcher", "Unknown")
  checkbox_panel <- function(input_id, label, choices) {
    div(
      class = "checkbox-panel",
      checkboxGroupInput(input_id, label, choices = choices, selected = choices, inline = TRUE)
    )
  }
  div(
    class = "juco-filter-card",
    div(
      class = "juco-filter-heading",
      div(
        span(class = "juco-kicker", "PLAYER FILTERS"),
        h3("Narrow the scouting pool")
      ),
      span(class = "juco-filter-hint", "Selections update the board instantly")
    ),
    div(
      class = if (include_position) "filter-row" else "filter-row pitching",
      checkbox_panel(paste0(prefix, "_class"), "Class", class_choices),
      checkbox_panel(paste0(prefix, "_throws"), "Throwing Hand", throwing_choices),
      checkbox_panel(paste0(prefix, "_bats"), "Batting Hand", batting_choices),
      if (include_position) {
        checkbox_panel(
          paste0(prefix, "_position"),
          "Position",
          position_choices
        )
      }
    )
  )
}

common_hitting <- "G, AB, AVG, K%, BB%, OBP, SLG, OPS, 2B, 3B, HR, RBI"
common_pitching <- "APP, IP, K/9, BB/9, H/9, WHIP, ERA, HR"

stat_guide <- function(role, stats) {
  stat_names <- trimws(strsplit(stats, ",", fixed = TRUE)[[1]])
  div(
    class = "common-strip",
    div(
      class = "juco-guide-copy",
      span(class = "juco-kicker", "EVALUATION SET"),
      strong(paste(role, "board"))
    ),
    div(
      class = "juco-stat-pills",
      lapply(stat_names, function(stat) span(class = "juco-stat-pill", stat))
    )
  )
}

leaderboard_panel <- function(title, role, output_id) {
  div(
    class = "juco-leaderboard-card",
    div(
      class = "juco-table-heading",
      div(
        span(class = "juco-kicker", "NATIONAL JUCO BOARD"),
        h2(title),
        p("Search, sort, and use the column controls to build a target list.")
      ),
      span(class = "juco-role-badge", role)
    ),
    div(class = "dt-wrap", DTOutput(output_id))
  )
}

juco_hitting_panel <- nav_panel(
    "Hitting",
    div(
      class = "juco-shell",
      stat_guide("Hitting", common_hitting),
      filter_controls("hit", include_position = TRUE),
      leaderboard_panel("Hitting leaderboard", "HIT", "hitting_table")
    )
  )

juco_pitching_panel <- nav_panel(
    "Pitching",
    div(
      class = "juco-shell",
      stat_guide("Pitching", common_pitching),
      filter_controls("pit", include_position = FALSE),
      leaderboard_panel("Pitching leaderboard", "PITCH", "pitching_table")
    )
  )

ui <- if (JUCO_EMBEDDED_MODE) {
  tagList(
    head_css,
    get0("BASE_JUCO_HEAD", inherits = FALSE, ifnotfound = NULL),
    div(
      class = "base-juco-embedded",
      navset_tab(juco_hitting_panel, juco_pitching_panel)
    )
  )
} else {
  page_navbar(
    title = "JUCO Scouting",
    theme = txst_theme,
    header = head_css,
    juco_hitting_panel,
    juco_pitching_panel
  )
}

server <- function(input, output, session) {
  raw_stats <- read_app_stats()
  profiles <- read_profiles()

  hitting_data <- reactive({
    build_hitting(raw_stats, profiles) %>%
      filter(
        metadata_match(Class, input$hit_class),
        metadata_match(Throws, input$hit_throws),
        metadata_match(Bats, input$hit_bats),
        position_match(position_buckets, input$hit_position)
      ) %>%
      select(-position_buckets)
  })

  pitching_data <- reactive({
    build_pitching(raw_stats, profiles) %>%
      filter(
        metadata_match(Class, input$pit_class),
        metadata_match(Throws, input$pit_throws),
        metadata_match(Bats, input$pit_bats)
      )
  })

  output$hitting_table <- renderDT({
    df <- hitting_data()
    if (!nrow(df)) return(datatable(data.frame(Status = "No hitters match the current filters."), rownames = FALSE, options = list(dom = "t")))
    render_leaderboard(
      df,
      percentage_cols = c("K%", "BB%"),
      three_dec_cols = c("AVG", "OBP", "SLG", "OPS"),
      shade_specs = shade_specs_hitting,
      ref_paths = hitting_ref_paths,
      header_filter = leaderboard_filter_callback(c("Region", "State", "Bats", "Throws", "Pos"), "AB", 100)
    )
  }, server = FALSE)

  output$pitching_table <- renderDT({
    df <- pitching_data()
    if (!nrow(df)) return(datatable(data.frame(Status = "No pitchers match the current filters."), rownames = FALSE, options = list(dom = "t")))
    render_leaderboard(
      df,
      two_dec_cols = c("WHIP", "ERA"),
      one_dec_cols = c("IP", "K/9", "BB/9", "H/9"),
      shade_specs = shade_specs_pitching,
      ref_paths = pitching_ref_paths,
      header_filter = leaderboard_filter_callback(c("Region", "State", "Bats", "Throws", "Pos"), "IP", 30)
    )
  }, server = FALSE)
}

shinyApp(ui, server)
