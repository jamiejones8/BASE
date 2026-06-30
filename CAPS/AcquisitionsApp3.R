library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(glue)
library(readr)
library(stringr)
library(tibble)
library(arrow)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════════════════════

data_dir <- "~/Desktop/acquisitions app"

pitch_level    <- read_parquet(file.path(data_dir, "pitch_level.parquet"))
pitcher_season <- read_parquet(file.path(data_dir, "pitcher_season.parquet"))
pitch_metrics  <- read_parquet(file.path(data_dir, "pitch_metrics.parquet"))
movement_avg   <- read_parquet(file.path(data_dir, "movement_avg.parquet"))
pitching_all   <- read_parquet(file.path(data_dir, "pitching_all.parquet"))
hitting_all    <- read_parquet(file.path(data_dir, "hitting_all.parquet"))
player_bios    <- read_parquet(file.path(data_dir, "player_bios.parquet"))
necbl_pitching <- read_parquet(file.path(data_dir, "necbl_pitching.parquet"))
necbl_hitting  <- read_parquet(file.path(data_dir, "necbl_hitting.parquet"))
nwl_pitching   <- read_parquet(file.path(data_dir, "nwl_pitching.parquet"))
nwl_hitting    <- read_parquet(file.path(data_dir, "nwl_hitting.parquet"))

message("Data loaded.")

# ══════════════════════════════════════════════════════════════════════════════
# 2. HARMONIZE DATA
# ══════════════════════════════════════════════════════════════════════════════

pitcher_season_tagged <- pitcher_season %>%
  mutate(
    has_pbp    = TRUE,
    source_key = as.character(pitcher_id),
    class_year = NA_character_
  )

necbl_pitchers_clean <- necbl_pitching %>%
  mutate(
    pitcher_name = player_name,
    pitch_hand   = throws,
    team_name    = team,
    IP_dec       = suppressWarnings(as.numeric(ip)),
    K9           = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (k  / IP_dec) * 9, NA), 1),
    BB9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (bb / IP_dec) * 9, NA), 1),
    KBB          = round(ifelse(!is.na(bb) & bb > 0, k / bb, NA), 2),
    HR9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (coalesce(hr,0L) / IP_dec) * 9, NA), 1),
    FIP          = round(ifelse(!is.na(IP_dec) & IP_dec > 0,
                                ((13*coalesce(hr,0L) + 3*(coalesce(bb,0L)+coalesce(hbp,0L)) -
                                    2*coalesce(k,0L)) / IP_dec) + 3.10, NA), 2),
    ERA          = suppressWarnings(as.numeric(era)),
    WHIP         = suppressWarnings(as.numeric(whip)),
    IP           = IP_dec,
    age          = NA_integer_,
    college      = school,
    class_year   = year,
    has_pbp      = FALSE,
    source_key   = paste(player_name, team, "NECBL")
  ) %>%
  select(pitcher_name, pitch_hand, age, class_year, college,
         team_name, league_name, G = app, IP, ERA, FIP, WHIP,
         K9, BB9, KBB, HR9, has_pbp, source_key)

nwl_pitchers_clean <- nwl_pitching %>%
  mutate(
    pitcher_name = paste(firstname, lastname),
    pitch_hand   = str_extract(coalesce(bats_throws, ""), "(?<=/).$"),
    team_name    = team_abv,
    IP_dec       = suppressWarnings(as.numeric(IP)),
    ERA_num      = suppressWarnings(as.numeric(ERA)),
    WHIP_num     = suppressWarnings(as.numeric(WHIP)),
    K9_num       = suppressWarnings(as.numeric(`K/9`)),
    BB9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (BB / IP_dec) * 9, NA), 1),
    KBB          = round(ifelse(!is.na(BB) & BB > 0, K / BB, NA), 2),
    HR9          = round(ifelse(!is.na(IP_dec) & IP_dec > 0, (HR / IP_dec) * 9, NA), 1),
    FIP          = round(ifelse(!is.na(IP_dec) & IP_dec > 0,
                                ((13*coalesce(HR,0L) + 3*(coalesce(BB,0L)+coalesce(HB,0L)) -
                                    2*coalesce(K,0L)) / IP_dec) + 3.10, NA), 2),
    age          = NA_integer_,
    class_year   = class,
    has_pbp      = FALSE,
    source_key   = paste(pitcher_name, team_abv, "Northwoods League")
  ) %>%
  select(pitcher_name, pitch_hand, age, class_year, college,
         team_name, league_name, G, IP = IP_dec,
         ERA = ERA_num, FIP, WHIP = WHIP_num,
         K9 = K9_num, BB9, KBB, HR9, has_pbp, source_key)

all_pitchers <- bind_rows(
  pitcher_season_tagged %>%
    select(pitcher_name, pitch_hand, age, class_year, college,
           team_name, league_name, G, IP, ERA, FIP, WHIP,
           K9, BB9, KBB, HR9, has_pbp, source_key),
  necbl_pitchers_clean,
  nwl_pitchers_clean
)

hitting_all_clean <- hitting_all %>%
  mutate(source_key = as.character(player_id), class_year = NA_character_, K = SO) %>%
  select(player_name, position, age, class_year, college,
         team_name, league_name, G, AB, H, R,
         `2B`, `3B`, HR, RBI, BB, K, SB, AVG, OBP, SLG, OPS, source_key)

necbl_hitting_clean <- necbl_hitting %>%
  mutate(
    player_name = player_name, team_name = Team, age = NA_integer_,
    class_year  = year, college = school, R = NA_integer_, SB = NA_integer_,
    OPS = round(coalesce(obp, 0) + coalesce(slg, 0), 3),
    AVG = avg, OBP = obp, SLG = slg,
    source_key  = paste(player_name, Team, "NECBL")
  ) %>%
  select(player_name, position, age, class_year, college,
         team_name, league_name, G = gp, AB = ab, H = h, R,
         `2B` = `2b`, `3B` = `3b`, HR = hr, RBI = rbi,
         BB = bb, K = k, SB, AVG, OBP, SLG, OPS, source_key)

nwl_hitting_clean <- nwl_hitting %>%
  mutate(
    player_name = paste(firstname, lastname), team_name = team_abv,
    age = NA_integer_, class_year = class,
    AVG = suppressWarnings(as.numeric(AVG)),
    OBP = suppressWarnings(as.numeric(OBP)),
    SLG = suppressWarnings(as.numeric(SLG)),
    OPS = suppressWarnings(as.numeric(OPS)),
    source_key  = paste(player_name, team_abv, "Northwoods League")
  ) %>%
  select(player_name, position, age, class_year, college,
         team_name, league_name, G, AB, H, R,
         `2B`, `3B`, HR, RBI, BB, K, SB, AVG, OBP, SLG, OPS, source_key)

all_hitters <- bind_rows(hitting_all_clean, necbl_hitting_clean, nwl_hitting_clean)

age_or_class <- function(age, class_year) {
  dplyr::case_when(
    !is.na(class_year) & class_year != "" ~ class_year,
    !is.na(age)                           ~ as.character(age),
    TRUE                                  ~ "—"
  )
}

message("Total pitchers: ", nrow(all_pitchers))
message("Total hitters:  ", nrow(all_hitters))

# ══════════════════════════════════════════════════════════════════════════════
# 3. CONSTANTS
# ══════════════════════════════════════════════════════════════════════════════

NAVY  <- "#0D2B56"
TEAL  <- "#00827F"
WHITE <- "#FFFFFF"

PITCH_COLORS <- c(
  "FF" = "#D22D49", "SI" = "#FE9D00", "FC" = "#933F2C",
  "SL" = "#EEE716", "ST" = "#DDB33A", "CU" = "#00D1ED",
  "KC" = "#3025CE", "CH" = "#1DBE3A", "FS" = "#4AFF89",
  "FA" = "#D22D49", "CS" = "#00D1ED"
)

ALL_LEAGUES <- c("All", "MLB Draft League", "Appalachian League",
                 "NECBL", "Northwoods League")

INELIG_FILE   <- file.path(data_dir, "ineligible_pitchers.csv")
INELIG_FILE_H <- file.path(data_dir, "ineligible_hitters.csv")

load_ineligible <- function() {
  if (!file.exists(INELIG_FILE)) return(character(0))
  df <- read_csv(INELIG_FILE, show_col_types = FALSE)
  if ("source_key" %in% names(df)) df$source_key
  else if ("pitcher_id" %in% names(df)) as.character(df$pitcher_id)
  else character(0)
}

save_ineligible <- function(keys) {
  tibble(source_key = as.character(keys)) %>% write_csv(INELIG_FILE)
}

load_ineligible_h <- function() {
  if (!file.exists(INELIG_FILE_H)) return(character(0))
  df <- read_csv(INELIG_FILE_H, show_col_types = FALSE)
  if ("source_key" %in% names(df)) df$source_key
  else if ("player_id" %in% names(df)) as.character(df$player_id)
  else character(0)
}

save_ineligible_h <- function(keys) {
  tibble(source_key = as.character(keys)) %>% write_csv(INELIG_FILE_H)
}

dt_header_js <- function() {
  JS(glue(
    "function(settings, json) {{
       $(this.api().table().header()).css({{
         'background-color':'{TEAL}', 'color':'{WHITE}'
       }});
     }}"
  ))
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. UI
# ══════════════════════════════════════════════════════════════════════════════

app_css <- glue("
  * {{ box-sizing: border-box; }}
  body {{ margin:0; background:{NAVY}; color:{WHITE};
          font-family:Arial,sans-serif; overflow:hidden; height:100vh; }}

  /* ── Shell ── */
  .app-shell {{ display:flex; height:100vh; }}

  /* ── Sidebar ── */
  .sidebar {{
    width:200px; min-width:200px; height:100vh;
    background:#061B38; border-right:1px solid #1A3A5C;
    display:flex; flex-direction:column; flex-shrink:0;
  }}
  .sidebar-logo {{
    padding:16px 14px 12px; border-bottom:1px solid #1A3A5C;
  }}
  .sidebar-logo-title {{ font-size:13px; font-weight:bold; color:{WHITE}; }}
  .sidebar-logo-sub {{ font-size:11px; color:#6B8CAE; margin-top:2px; }}

  .nav-section {{
    font-size:10px; font-weight:bold; letter-spacing:.07em;
    text-transform:uppercase; color:#4A6B8A;
    padding:14px 14px 4px;
  }}
  .nav-item {{
    display:flex; align-items:center; gap:9px;
    padding:8px 14px; font-size:13px; cursor:pointer;
    color:#8BAAC8; border-left:3px solid transparent;
    transition:all .15s;
  }}
  .nav-item:hover {{ color:{WHITE}; background:#0D2B56; }}
  .nav-item.active {{
    color:{TEAL}; background:#0A2240;
    border-left-color:{TEAL}; font-weight:bold;
  }}
  .nav-item i {{ font-size:16px; }}
  .inelig-badge {{
    margin-left:auto; font-size:9px; font-weight:bold;
    background:#7B2020; color:#FFB3B3;
    padding:1px 6px; border-radius:10px;
  }}

  .sidebar-bottom {{
    margin-top:auto; border-top:1px solid #1A3A5C; padding:10px 14px;
  }}
  .sidebar-action {{
    display:flex; align-items:center; gap:7px;
    font-size:12px; color:#6B8CAE; cursor:pointer;
    padding:6px 0;
  }}
  .sidebar-action:hover {{ color:{WHITE}; }}
  .sidebar-action i {{ font-size:14px; }}

  /* ── Main ── */
  .main-area {{
    flex:1; display:flex; flex-direction:column;
    height:100vh; overflow:hidden;
  }}
  .main-header {{
    padding:12px 20px; border-bottom:1px solid #1A3A5C;
    display:flex; align-items:center; justify-content:space-between;
    background:#0A2240; flex-shrink:0;
  }}
  .main-header-title {{ font-size:15px; font-weight:bold; color:{WHITE}; }}
  .header-filters {{ display:flex; gap:8px; align-items:center; }}

  .content-area {{ flex:1; overflow-y:auto; padding:16px 20px; }}

  /* ── Filter bar ── */
  .filter-bar {{
    display:flex; gap:10px; align-items:flex-end;
    margin-bottom:14px; flex-wrap:wrap;
  }}
  .filter-group {{ display:flex; flex-direction:column; gap:4px; }}
  .filter-label {{ font-size:10px; color:#6B8CAE; text-transform:uppercase;
                   letter-spacing:.05em; }}
  .filter-bar select, .filter-bar input {{
    background:#0F3366; color:{WHITE}; border:1px solid #1A3A5C;
    border-radius:4px; padding:5px 8px; font-size:12px;
  }}
  .filter-bar input[type=number] {{ width:80px; }}
  .btn-apply {{
    background:{TEAL}; color:{WHITE}; border:none;
    padding:6px 14px; border-radius:4px; font-size:12px; cursor:pointer;
    font-weight:bold; align-self:flex-end;
  }}
  .btn-apply:hover {{ opacity:.85; }}

  /* ── Action buttons ── */
  .action-bar {{ display:flex; gap:8px; margin-bottom:12px; flex-wrap:wrap; }}
  .btn-action {{
    display:flex; align-items:center; gap:5px;
    background:#0F3366; color:{WHITE}; border:1px solid #1A3A5C;
    padding:5px 12px; border-radius:4px; font-size:12px; cursor:pointer;
  }}
  .btn-action:hover {{ border-color:{TEAL}; color:{TEAL}; }}
  .btn-action.danger {{ border-color:#7B2020; color:#FFB3B3; }}
  .btn-action.danger:hover {{ background:#4A1010; }}
  .btn-action.restore {{ border-color:#1A5276; color:#7EC8E3; }}
  .btn-top10 {{
    display:flex; align-items:center; gap:5px;
    background:{TEAL}; color:{WHITE}; border:none;
    padding:5px 12px; border-radius:4px; font-size:12px; cursor:pointer;
    font-weight:bold; margin-left:auto;
  }}
  .btn-top10:hover {{ opacity:.85; }}

  /* ── Tables ── */
  .dataTables_wrapper, .dataTables_info,
  .dataTables_paginate {{ color:{WHITE} !important; }}
  .dataTables_filter label, .dataTables_length label {{ color:{WHITE}; }}
  .dataTables_filter input, .dataTables_length select {{
    background:#0F3366; color:{WHITE}; border:1px solid #1A3A5C;
  }}
  table.dataTable tbody tr {{
    background:#0F3366 !important; color:{WHITE} !important;
  }}
  table.dataTable tbody tr:hover {{
    background:#1A4A6C !important; cursor:pointer;
  }}
  table.dataTable tbody tr.selected td {{
    background:{TEAL} !important; color:{WHITE} !important;
  }}
  table.dataTable thead th {{
    background:{TEAL}; color:{WHITE};
  }}

  /* ── Modals ── */
  .modal-content {{
    background:#0A2040; color:{WHITE}; border:2px solid {TEAL};
  }}
  .modal-header {{ background:{TEAL}; border-bottom:none; }}
  .modal-title {{ color:{WHITE} !important; font-weight:bold; }}
  .close {{ color:{WHITE} !important; opacity:1 !important; }}
  .modal-footer {{ background:#0A2040; border-top:1px solid {TEAL}; }}

  /* ── Misc ── */
  .shiny-notification {{ background:{TEAL}; color:{WHITE}; border:none; }}
  .info-bar {{ color:#8BAAC8; font-size:13px; margin-bottom:12px; }}
  .no-pbp-badge {{
    display:inline-block; background:#2A3A50; color:#8BAAC8;
    font-size:11px; padding:2px 8px; border-radius:3px; margin-left:8px;
  }}
  label {{ color:{WHITE}; }}
  hr {{ border-color:{TEAL}; opacity:.4; }}
  select {{ background:#0F3366; color:{WHITE}; border:1px solid #1A3A5C; }}

  /* ── Pos buttons ── */
  .pos-btn {{
    background:#0F3366; color:{WHITE};
    border:1px solid #1A3A5C; margin:2px; padding:4px 10px;
    border-radius:4px; cursor:pointer; font-size:12px;
    display:inline-block;
  }}
  .pos-btn.active {{
    background:{TEAL}; color:{NAVY}; font-weight:bold;
    border-color:{TEAL};
  }}

  /* ── Page sections (hidden by default) ── */
  .page {{ display:none; }}
  .page.active {{ display:block; }}

  /* ── Scrollbar ── */
  ::-webkit-scrollbar {{ width:6px; height:6px; }}
  ::-webkit-scrollbar-track {{ background:#061B38; }}
  ::-webkit-scrollbar-thumb {{ background:#1A3A5C; border-radius:3px; }}
  ::-webkit-scrollbar-thumb:hover {{ background:{TEAL}; }}
  
  button.nav-item {{
    background:none; border-top:none; border-right:none;
    border-bottom:none; border-left:3px solid transparent;
    width:100%; text-align:left; border-radius:0;
    display:flex; align-items:center; gap:9px;
    padding:8px 14px; font-size:13px; cursor:pointer;
    color:#8BAAC8;
  }}
  button.nav-item:hover {{ color:{WHITE}; background:#0D2B56; }}
  button.nav-item.active {{
    color:{TEAL}; background:#0A2240;
    border-left-color:{TEAL}; font-weight:bold;
  }}
")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(app_css)),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/tabler-icons.min.css"
    )
  ),
  
  
  div(class = "app-shell",
      
      # ── Sidebar ────────────────────────────────────────────────────────────
      div(class = "sidebar",
          
          div(class = "sidebar-logo",
              div(class = "sidebar-logo-title", "Brewster Whitecaps"),
              div(class = "sidebar-logo-sub",   "Player Acquisitions")
          ),
          
          div(class = "nav-section", "Scouting"),
          
          actionButton("nav_pitchers",   tagList(tags$i(class="ti ti-ball-baseball"), " Pitchers"),
                       class = "nav-item active"),
          actionButton("nav_hitters",    tagList(tags$i(class="ti ti-run"),           " Hitters"),
                       class = "nav-item"),
          actionButton("nav_top10",      tagList(tags$i(class="ti ti-trophy"),        " Top 10"),
                       class = "nav-item"),
          actionButton("nav_ineligible", tagList(tags$i(class="ti ti-ban"),           " Ineligible"),
                       class = "nav-item"),
          
          div(class = "sidebar-bottom",
              div(id = "nav_run_updates", class = "sidebar-action",
                  tags$i(class = "ti ti-refresh"),
                  "Run updates"
              )
          )
      ),
      
      # ── Main area ───────────────────────────────────────────────────────────
      div(class = "main-area",
          
          div(class = "main-header",
              div(class = "main-header-title", textOutput("page_title", inline = TRUE)),
              div(class = "header-filters", uiOutput("header_subtitle"))
          ),
          
          div(class = "content-area",
              
              # ── Pitchers page ─────────────────────────────────────────────────
              div(id = "page_pitchers", class = "page active",
                  
                  div(class = "filter-bar",
                      div(class = "filter-group",
                          div(class = "filter-label", "League"),
                          selectInput("pbp_league", NULL,
                                      choices = ALL_LEAGUES, selected = "All",
                                      width = "150px")
                      ),
                      div(class = "filter-group",
                          div(class = "filter-label", "Hand"),
                          selectInput("pbp_hand", NULL,
                                      choices = c("All","R","L"), selected = "All",
                                      width = "80px")
                      ),
                      div(class = "filter-group",
                          div(class = "filter-label", "Min IP"),
                          numericInput("min_pitches", NULL, value = 0, min = 0, step = 1,
                                       width = "70px")
                      ),
                      div(class = "filter-group",
                          div(class = "filter-label", "Max Age"),
                          numericInput("max_age_p", NULL, value = 99, min = 18, step = 1,
                                       width = "70px")
                      ),
                      actionButton("apply_pbp_filter", "Apply", class = "btn-apply")
                  ),
                  
                  div(class = "action-bar",
                      actionButton("view_pitcher",   tagList(tags$i(class="ti ti-chart-line"), " View profile"),
                                   class = "btn-action"),
                      actionButton("remove_pitcher", tagList(tags$i(class="ti ti-ban"), " Mark ineligible"),
                                   class = "btn-action danger"),
                      actionButton("open_top10",     tagList(tags$i(class="ti ti-trophy"), " Top 10"),
                                   class = "btn-top10")
                  ),
                  
                  DTOutput("pbp_pitcher_table")
              ),
              
              # ── Hitters page ──────────────────────────────────────────────────
              div(id = "page_hitters", class = "page",
                  
                  div(class = "filter-bar",
                      div(class = "filter-group",
                          div(class = "filter-label", "League"),
                          selectInput("season_league_h", NULL,
                                      choices = ALL_LEAGUES, selected = "All",
                                      width = "150px")
                      ),
                      div(class = "filter-group",
                          div(class = "filter-label", "Min AB"),
                          numericInput("min_ab", NULL, value = 0, min = 0, step = 5,
                                       width = "70px")
                      ),
                      div(class = "filter-group",
                          div(class = "filter-label", "Max Age"),
                          numericInput("max_age_h", NULL, value = 99, min = 18, step = 1,
                                       width = "70px")
                      ),
                      actionButton("apply_season_h", "Apply", class = "btn-apply")
                  ),
                  
                  div(class = "action-bar",
                      actionButton("remove_hitter", tagList(tags$i(class="ti ti-ban"), " Mark ineligible"),
                                   class = "btn-action danger"),
                      actionButton("open_top10_h",  tagList(tags$i(class="ti ti-trophy"), " Top 10"),
                                   class = "btn-top10")
                  ),
                  
                  DTOutput("season_hitter_table")
              ),
              
              # ── Top 10 page ───────────────────────────────────────────────────
              div(id = "page_top10", class = "page",
                  
                  tabsetPanel(id = "top10_inline_tabs",
                              
                              tabPanel("Hitting",
                                       br(),
                                       fluidRow(
                                         column(3, selectInput("top10_league_h", "League:", choices = ALL_LEAGUES,
                                                               selected = "All", width = "100%")),
                                         column(3, numericInput("top10_min_ab", "Min AB:", value = 20,
                                                                min = 0, step = 5, width = "100%")),
                                         column(3, numericInput("top10_max_age_h", "Max Age:", value = 99,
                                                                min = 18, step = 1, width = "100%")),
                                         column(3, selectInput("top10_stat_h", "Sort by:",
                                                               choices = c("OPS","AVG","OBP","SLG","HR","RBI","SB"),
                                                               selected = "OPS", width = "100%"))
                                       ),
                                       div(style = "margin-bottom:10px;",
                                           tags$p("Position:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                                           actionButton("hpos_All","All",class="pos-btn active"),
                                           actionButton("hpos_C",  "C",  class="pos-btn"),
                                           actionButton("hpos_1B", "1B", class="pos-btn"),
                                           actionButton("hpos_2B", "2B", class="pos-btn"),
                                           actionButton("hpos_3B", "3B", class="pos-btn"),
                                           actionButton("hpos_SS", "SS", class="pos-btn"),
                                           actionButton("hpos_LF", "LF", class="pos-btn"),
                                           actionButton("hpos_CF", "CF", class="pos-btn"),
                                           actionButton("hpos_RF", "RF", class="pos-btn"),
                                           actionButton("hpos_DH", "DH", class="pos-btn")
                                       ),
                                       DTOutput("top10_hitting_table")
                              ),
                              
                              tabPanel("Pitching",
                                       br(),
                                       fluidRow(
                                         column(3, selectInput("top10_league_p", "League:", choices = ALL_LEAGUES,
                                                               selected = "All", width = "100%")),
                                         column(3, numericInput("top10_min_ip", "Min IP:", value = 5,
                                                                min = 0, step = 1, width = "100%")),
                                         column(3, selectInput("top10_stat_p", "Sort by:",
                                                               choices = c("ERA","FIP","WHIP","K9","BB9","KBB"),
                                                               selected = "ERA", width = "100%")),
                                         column(3, numericInput("top10_max_age_p", "Max Age:", value = 99,
                                                                min = 18, step = 1, width = "100%"))
                                       ),
                                       div(style = "margin-bottom:6px;",
                                           tags$p("Role:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                                           actionButton("prole_All","All",class="pos-btn active"),
                                           actionButton("prole_SP", "SP", class="pos-btn"),
                                           actionButton("prole_RP", "RP", class="pos-btn")
                                       ),
                                       div(style = "margin-bottom:10px;",
                                           tags$p("Hand:", style = "color:#8BAAC8; font-size:11px; margin-bottom:4px;"),
                                           actionButton("phand_All","All", class="pos-btn active"),
                                           actionButton("phand_R",  "RHP", class="pos-btn"),
                                           actionButton("phand_L",  "LHP", class="pos-btn")
                                       ),
                                       DTOutput("top10_pitching_table")
                              )
                  )
              ),
              
              # ── Ineligible page ───────────────────────────────────────────────
              div(id = "page_ineligible", class = "page",
                  
                  tabsetPanel(id = "inelig_tabs",
                              
                              tabPanel("Pitchers",
                                       br(),
                                       div(class = "action-bar",
                                           actionButton("restore_pitcher",
                                                        tagList(tags$i(class="ti ti-circle-check"), " Restore selected"),
                                                        class = "btn-action restore")
                                       ),
                                       tags$p(textOutput("inelig_subtitle"),
                                              style = "color:#8BAAC8; font-size:12px; margin-bottom:8px;"),
                                       DTOutput("inelig_table")
                              ),
                              
                              tabPanel("Hitters",
                                       br(),
                                       div(class = "action-bar",
                                           actionButton("restore_hitter",
                                                        tagList(tags$i(class="ti ti-circle-check"), " Restore selected"),
                                                        class = "btn-action restore")
                                       ),
                                       tags$p(textOutput("inelig_h_subtitle"),
                                              style = "color:#8BAAC8; font-size:12px; margin-bottom:8px;"),
                                       DTOutput("inelig_hitter_table")
                              )
                  )
              )
          )
      )
  ),
  
  # ── Pitcher profile modal ──────────────────────────────────────────────────
  bsModal(
    id = "pitcher_modal", title = "Pitcher Profile",
    trigger = NULL, size = "large",
    uiOutput("modal_info"),
    uiOutput("modal_body")
  ),
  
  # ── Top 10 modal (triggered from pitchers/hitters pages) ─────────────────
  bsModal(
    id = "top10_modal", title = "Top 10 Leaderboards",
    trigger = NULL, size = "large",
    
    tabsetPanel(id = "top10_modal_tabs",
                
                tabPanel("Hitting",
                         br(),
                         fluidRow(
                           column(3, selectInput("top10m_league_h", "League:", choices = ALL_LEAGUES,
                                                 selected = "All", width = "100%")),
                           column(3, numericInput("top10m_min_ab", "Min AB:", value = 20,
                                                  min = 0, step = 5, width = "100%")),
                           column(3, numericInput("top10m_max_age_h", "Max Age:", value = 99,
                                                  min = 18, step = 1, width = "100%")),
                           column(3, selectInput("top10m_stat_h", "Sort by:",
                                                 choices = c("OPS","AVG","OBP","SLG","HR","RBI","SB"),
                                                 selected = "OPS", width = "100%"))
                         ),
                         div(style = "margin-bottom:10px;",
                             actionButton("hpos_m_All","All",class="pos-btn active"),
                             actionButton("hpos_m_C",  "C",  class="pos-btn"),
                             actionButton("hpos_m_1B", "1B", class="pos-btn"),
                             actionButton("hpos_m_2B", "2B", class="pos-btn"),
                             actionButton("hpos_m_3B", "3B", class="pos-btn"),
                             actionButton("hpos_m_SS", "SS", class="pos-btn"),
                             actionButton("hpos_m_LF", "LF", class="pos-btn"),
                             actionButton("hpos_m_CF", "CF", class="pos-btn"),
                             actionButton("hpos_m_RF", "RF", class="pos-btn"),
                             actionButton("hpos_m_DH", "DH", class="pos-btn")
                         ),
                         DTOutput("top10m_hitting_table")
                ),
                
                tabPanel("Pitching",
                         br(),
                         fluidRow(
                           column(3, selectInput("top10m_league_p", "League:", choices = ALL_LEAGUES,
                                                 selected = "All", width = "100%")),
                           column(3, numericInput("top10m_min_ip", "Min IP:", value = 5,
                                                  min = 0, step = 1, width = "100%")),
                           column(3, selectInput("top10m_stat_p", "Sort by:",
                                                 choices = c("ERA","FIP","WHIP","K9","BB9","KBB"),
                                                 selected = "ERA", width = "100%")),
                           column(3, numericInput("top10m_max_age_p", "Max Age:", value = 99,
                                                  min = 18, step = 1, width = "100%"))
                         ),
                         div(style = "margin-bottom:6px;",
                             actionButton("prole_m_All","All",class="pos-btn active"),
                             actionButton("prole_m_SP", "SP", class="pos-btn"),
                             actionButton("prole_m_RP", "RP", class="pos-btn")
                         ),
                         div(style = "margin-bottom:10px;",
                             actionButton("phand_m_All","All", class="pos-btn active"),
                             actionButton("phand_m_R",  "RHP", class="pos-btn"),
                             actionButton("phand_m_L",  "LHP", class="pos-btn")
                         ),
                         DTOutput("top10m_pitching_table")
                )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# 5. SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  ineligible   <- reactiveVal(load_ineligible())
  ineligible_h <- reactiveVal(load_ineligible_h())
  selected_key <- reactiveVal(NULL)
  current_page <- reactiveVal("pitchers")
  
  h_pos    <- reactiveVal("All")
  p_role   <- reactiveVal("All")
  p_hand   <- reactiveVal("All")
  h_pos_m  <- reactiveVal("All")
  p_role_m <- reactiveVal("All")
  p_hand_m <- reactiveVal("All")
  
  h_positions <- c("All","C","1B","2B","3B","SS","LF","CF","RF","DH")
  p_roles     <- c("All","SP","RP")
  p_hands     <- c("All","R","L")
  
  set_active_btn <- function(group, selected, ids) {
    for (id in ids) {
      val <- sub(paste0("^", group), "", id)
      if (val == selected) {
        runjs(glue("$('#{id}').addClass('active').css({{'background-color':'{TEAL}','color':'{NAVY}','font-weight':'bold','border-color':'{TEAL}'}});"))
      } else {
        runjs(glue("$('#{id}').removeClass('active').css({{'background-color':'#0F3366','color':'{WHITE}','font-weight':'normal','border-color':'#1A3A5C'}});"))
      }
    }
  }
  
  lapply(h_positions, function(pos) {
    observeEvent(input[[paste0("hpos_", pos)]], {
      h_pos(pos); set_active_btn("hpos_", pos, paste0("hpos_", h_positions))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("hpos_m_", pos)]], {
      h_pos_m(pos); set_active_btn("hpos_m_", pos, paste0("hpos_m_", h_positions))
    }, ignoreInit = TRUE)
  })
  
  lapply(p_roles, function(role) {
    observeEvent(input[[paste0("prole_", role)]], {
      p_role(role); set_active_btn("prole_", role, paste0("prole_", p_roles))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("prole_m_", role)]], {
      p_role_m(role); set_active_btn("prole_m_", role, paste0("prole_m_", p_roles))
    }, ignoreInit = TRUE)
  })
  
  lapply(p_hands, function(hand) {
    observeEvent(input[[paste0("phand_", hand)]], {
      p_hand(hand); set_active_btn("phand_", hand, paste0("phand_", p_hands))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("phand_m_", hand)]], {
      p_hand_m(hand); set_active_btn("phand_m_", hand, paste0("phand_m_", p_hands))
    }, ignoreInit = TRUE)
  })
  
  # ── Sidebar navigation ────────────────────────────────────────────────────
  nav_pages <- c("pitchers","hitters","top10","ineligible")
  
  switch_page <- function(page) {
    current_page(page)
    for (p in nav_pages) {
      if (p == page) {
        runjs(glue("$('#page_{p}').addClass('active');"))
        runjs(glue("$('#nav_{p}').addClass('active');"))
      } else {
        runjs(glue("$('#page_{p}').removeClass('active');"))
        runjs(glue("$('#nav_{p}').removeClass('active');"))
      }
    }
  }
  
  
  observeEvent(input$nav_pitchers,   { switch_page("pitchers") },   ignoreInit = TRUE)
  observeEvent(input$nav_hitters,    { switch_page("hitters") },    ignoreInit = TRUE)
  observeEvent(input$nav_top10,      { switch_page("top10") },      ignoreInit = TRUE)
  observeEvent(input$nav_ineligible, { switch_page("ineligible") }, ignoreInit = TRUE)
  
  observeEvent(input$open_top10,   { toggleModal(session, "top10_modal", toggle = "open") })
  observeEvent(input$open_top10_h, { toggleModal(session, "top10_modal", toggle = "open") })
  
  # ── Page title + subtitle ─────────────────────────────────────────────────
  output$page_title <- renderText({
    switch(current_page(),
           pitchers   = "Pitchers",
           hitters    = "Hitters",
           top10      = "Top 10 Leaderboards",
           ineligible = "Ineligible Players"
    )
  })
  
  output$header_subtitle <- renderUI({
    switch(current_page(),
           pitchers   = tags$span(textOutput("pbp_subtitle", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           hitters    = tags$span(textOutput("season_h_subtitle", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           ineligible = tags$span(textOutput("inelig_nav_total", inline=TRUE),
                                  style="font-size:12px;color:#8BAAC8;"),
           NULL
    )
  })
  
  output$inelig_nav_badge <- renderUI({
    n <- length(ineligible()) + length(ineligible_h())
    if (n > 0) tags$span(n, class = "inelig-badge")
  })
  
  output$inelig_nav_total <- renderText({
    glue("{length(ineligible())} pitchers · {length(ineligible_h())} hitters")
  })
  
  # ── Pitchers ──────────────────────────────────────────────────────────────
  filtered_pitchers <- reactive({
    df <- all_pitchers %>% filter(!source_key %in% ineligible())
    
    if (input$pbp_league != "All")
      df <- df %>% filter(league_name == input$pbp_league)
    if (input$pbp_hand != "All")
      df <- df %>% filter(pitch_hand == input$pbp_hand)
    if (!is.na(input$min_pitches) && input$min_pitches > 0)
      df <- df %>% filter(!is.na(IP) & IP >= input$min_pitches)
    if (!is.na(input$max_age_p) && input$max_age_p < 99)
      df <- df %>% filter(is.na(age) | age <= input$max_age_p)
    
    df %>% arrange(ERA)
  }) %>% bindEvent(input$apply_pbp_filter, ineligible(), ignoreNULL = FALSE)
  
  output$pbp_subtitle <- renderText(glue("{nrow(filtered_pitchers())} pitchers"))
  
  output$pbp_pitcher_table <- renderDT({
    filtered_pitchers() %>%
      mutate(Profile = ifelse(has_pbp, "✓", "—"),
             `Age/Yr` = age_or_class(age, class_year)) %>%
      select(Name = pitcher_name, Hand = pitch_hand, `Age/Yr`,
             School = college, Team = team_name, League = league_name,
             Profile, G, IP, ERA, FIP, WHIP,
             `K/9` = K9, `BB/9` = BB9, `K/BB` = KBB, `HR/9` = HR9) %>%
      datatable(selection = "multiple", rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                               initComplete = dt_header_js()),
                class = "display compact") %>%
      formatRound(c("ERA","FIP","WHIP","K/9","BB/9","K/BB","HR/9"), 2)
  })
  
  observeEvent(input$view_pitcher, {
    row <- input$pbp_pitcher_table_rows_selected
    if (length(row) == 0) { showNotification("Select a pitcher first.", type="warning"); return() }
    selected_key(filtered_pitchers()$source_key[row[1]])
    toggleModal(session, "pitcher_modal", toggle = "open")
  })
  
  observeEvent(input$remove_pitcher, {
    rows <- input$pbp_pitcher_table_rows_selected
    if (length(rows) == 0) { showNotification("Select at least one pitcher.", type="warning"); return() }
    keys  <- filtered_pitchers()$source_key[rows]
    names <- filtered_pitchers()$pitcher_name[rows]
    new_inelig <- unique(c(ineligible(), keys))
    ineligible(new_inelig); save_ineligible(new_inelig)
    dataTableProxy("pbp_pitcher_table") %>% selectRows(NULL)
    showNotification(glue("{length(keys)} pitcher(s) marked ineligible."), type="warning", duration=5)
  })
  
  # ── Hitters ───────────────────────────────────────────────────────────────
  filtered_hitters <- reactive({
    df <- all_hitters %>% filter(!source_key %in% ineligible_h())
    
    if (input$season_league_h != "All")
      df <- df %>% filter(league_name == input$season_league_h)
    if (!is.na(input$max_age_h) && input$max_age_h < 99)
      df <- df %>% filter(is.na(age) | age <= input$max_age_h)
    
    df %>% filter(!is.na(AB), AB >= input$min_ab) %>% arrange(desc(OPS))
  }) %>% bindEvent(input$apply_season_h, ineligible_h(), ignoreNULL = FALSE)
  
  output$season_h_subtitle <- renderText(glue("{nrow(filtered_hitters())} hitters"))
  
  output$season_hitter_table <- renderDT({
    filtered_hitters() %>%
      mutate(`Age/Yr` = age_or_class(age, class_year)) %>%
      select(Name = player_name, `Age/Yr`, School = college,
             Team = team_name, League = league_name, Pos = position,
             G, AB, H, R, `2B`, `3B`, HR, RBI, BB, K, SB,
             AVG, OBP, SLG, OPS) %>%
      datatable(selection = "multiple", rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                               initComplete = dt_header_js()),
                class = "display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  })
  
  observeEvent(input$remove_hitter, {
    rows <- input$season_hitter_table_rows_selected
    if (length(rows) == 0) { showNotification("Select at least one hitter.", type="warning"); return() }
    keys  <- filtered_hitters()$source_key[rows]
    names <- filtered_hitters()$player_name[rows]
    new_inelig <- unique(c(ineligible_h(), keys))
    ineligible_h(new_inelig); save_ineligible_h(new_inelig)
    dataTableProxy("season_hitter_table") %>% selectRows(NULL)
    showNotification(glue("{length(keys)} hitter(s) marked ineligible."), type="warning", duration=5)
  })
  
  # ── Ineligible ────────────────────────────────────────────────────────────
  output$inelig_subtitle   <- renderText(glue("{length(ineligible())} pitchers marked ineligible"))
  output$inelig_h_subtitle <- renderText(glue("{length(ineligible_h())} hitters marked ineligible"))
  
  output$inelig_table <- renderDT({
    keys <- ineligible()
    if (length(keys) == 0) return(datatable(tibble(Message="No pitchers marked ineligible."),
                                            rownames=FALSE, options=list(dom="t", initComplete=dt_header_js()), class="display compact"))
    all_pitchers %>%
      filter(source_key %in% keys) %>%
      mutate(`Age/Yr` = age_or_class(age, class_year)) %>%
      select(Name=pitcher_name, Hand=pitch_hand, `Age/Yr`,
             Team=team_name, League=league_name, School=college,
             G, IP, ERA, FIP, WHIP) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="t", pageLength=50, initComplete=dt_header_js()),
                class="display compact") %>%
      formatRound(c("ERA","FIP","WHIP"), 2)
  })
  
  observeEvent(input$restore_pitcher, {
    row <- input$inelig_table_rows_selected
    if (length(row) == 0) { showNotification("Select a pitcher to restore.", type="warning"); return() }
    keys <- ineligible()
    df   <- all_pitchers %>% filter(source_key %in% keys)
    key  <- df$source_key[row]; name <- df$pitcher_name[row]
    new_inelig <- keys[keys != key]
    ineligible(new_inelig); save_ineligible(new_inelig)
    dataTableProxy("inelig_table") %>% selectRows(NULL)
    showNotification(glue("{name} restored."), type="message", duration=4)
  })
  
  output$inelig_hitter_table <- renderDT({
    keys <- ineligible_h()
    if (length(keys) == 0) return(datatable(tibble(Message="No hitters marked ineligible."),
                                            rownames=FALSE, options=list(dom="t", initComplete=dt_header_js()), class="display compact"))
    all_hitters %>%
      filter(source_key %in% keys) %>%
      mutate(`Age/Yr` = age_or_class(age, class_year)) %>%
      select(Name=player_name, Pos=position, `Age/Yr`,
             Team=team_name, League=league_name, School=college,
             G, AB, AVG, OBP, SLG, OPS) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="t", pageLength=50, initComplete=dt_header_js()),
                class="display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  })
  
  observeEvent(input$restore_hitter, {
    row <- input$inelig_hitter_table_rows_selected
    if (length(row) == 0) { showNotification("Select a hitter to restore.", type="warning"); return() }
    keys <- ineligible_h()
    df   <- all_hitters %>% filter(source_key %in% keys)
    key  <- df$source_key[row]; name <- df$player_name[row]
    new_inelig <- keys[keys != key]
    ineligible_h(new_inelig); save_ineligible_h(new_inelig)
    dataTableProxy("inelig_hitter_table") %>% selectRows(NULL)
    showNotification(glue("{name} restored."), type="message", duration=4)
  })
  
  # ── Top 10 (inline page) ──────────────────────────────────────────────────
  top10_pitcher_dt <- function(league_in, min_ip, max_age, stat, role, hand) {
    df <- all_pitchers %>%
      filter(!source_key %in% ineligible()) %>%
      filter(!is.na(ERA), !is.na(IP), IP >= min_ip)
    
    if (league_in != "All") df <- df %>% filter(league_name == league_in)
    if (!is.na(max_age) && max_age < 99)
      df <- df %>% filter(is.na(age) | age <= max_age)
    
    if (role %in% c("SP","RP")) {
      gs_join <- pitching_all %>%
        select(player_id, GS) %>% distinct(player_id, .keep_all=TRUE) %>%
        mutate(source_key = as.character(player_id))
      df <- df %>% left_join(gs_join %>% select(source_key, GS), by="source_key")
      if (role == "SP") df <- df %>% filter(!is.na(GS), GS/G >= 0.5)
      else              df <- df %>% filter(is.na(GS) | GS/G < 0.5)
    }
    
    if (hand != "All") df <- df %>% filter(pitch_hand == hand)
    
    asc_stats <- c("ERA","FIP","WHIP","BB9")
    df <- if (stat %in% asc_stats) arrange(df, .data[[stat]]) else
      arrange(df, desc(.data[[stat]]))
    
    df %>%
      slice_head(n = 10) %>%
      mutate(Rank = row_number(), `Age/Yr` = age_or_class(age, class_year)) %>%
      select(Rank, Name=pitcher_name, Hand=pitch_hand,
             Team=team_name, League=league_name, `Age/Yr`,
             School=college, G, IP, ERA, FIP, WHIP,
             `K/9`=K9, `BB/9`=BB9, `K/BB`=KBB) %>%
      datatable(rownames=FALSE,
                options=list(dom="t", pageLength=10, initComplete=dt_header_js()),
                class="display compact") %>%
      formatRound(c("ERA","FIP","WHIP","K/9","BB/9","K/BB"), 2)
  }
  
  top10_hitter_dt <- function(league_in, min_ab, max_age, stat, pos) {
    df <- all_hitters %>%
      filter(!source_key %in% ineligible_h()) %>%
      filter(position != "P", !is.na(OPS), !is.na(AB), AB >= min_ab)
    
    if (league_in != "All") df <- df %>% filter(league_name == league_in)
    if (!is.na(max_age) && max_age < 99)
      df <- df %>% filter(is.na(age) | age <= max_age)
    if (pos != "All") df <- df %>% filter(position == pos)
    
    df %>%
      arrange(desc(.data[[stat]])) %>%
      slice_head(n = 10) %>%
      mutate(Rank = row_number(), `Age/Yr` = age_or_class(age, class_year)) %>%
      select(Rank, Name=player_name, Pos=position,
             Team=team_name, League=league_name, `Age/Yr`,
             School=college, G, AB, AVG, OBP, SLG, OPS, HR, RBI, SB) %>%
      datatable(rownames=FALSE,
                options=list(dom="t", pageLength=10, initComplete=dt_header_js()),
                class="display compact") %>%
      formatRound(c("AVG","OBP","SLG","OPS"), 3)
  }
  
  output$top10_hitting_table  <- renderDT(top10_hitter_dt(
    input$top10_league_h, input$top10_min_ab, input$top10_max_age_h,
    input$top10_stat_h, h_pos()))
  output$top10_pitching_table <- renderDT(top10_pitcher_dt(
    input$top10_league_p, input$top10_min_ip, input$top10_max_age_p,
    input$top10_stat_p, p_role(), p_hand()))
  
  output$top10m_hitting_table  <- renderDT(top10_hitter_dt(
    input$top10m_league_h, input$top10m_min_ab, input$top10m_max_age_h,
    input$top10m_stat_h, h_pos_m()))
  output$top10m_pitching_table <- renderDT(top10_pitcher_dt(
    input$top10m_league_p, input$top10m_min_ip, input$top10m_max_age_p,
    input$top10m_stat_p, p_role_m(), p_hand_m()))
  
  # ── Modal ─────────────────────────────────────────────────────────────────
  output$modal_info <- renderUI({
    key <- selected_key(); req(key)
    p   <- all_pitchers %>% filter(source_key == key); req(nrow(p) > 0)
    tags$div(
      tags$h3(p$pitcher_name, style=glue("color:{TEAL}; margin:0 0 4px 0;")),
      if (!p$has_pbp) tags$span("No PBP data", class="no-pbp-badge"),
      tags$p(glue(
        "{coalesce(p$pitch_hand,'—')}HP  |  {age_or_class(p$age, p$class_year)}  |  ",
        "School: {ifelse(is.na(p$college),'—',p$college)}  |  ",
        "Team: {ifelse(is.na(p$team_name),'—',p$team_name)}  |  ",
        "League: {ifelse(is.na(p$league_name),'—',p$league_name)}"
      ), class="info-bar")
    )
  })
  
  output$modal_body <- renderUI({
    key <- selected_key(); req(key)
    p   <- all_pitchers %>% filter(source_key == key); req(nrow(p) > 0)
    if (p$has_pbp) {
      tagList(
        tags$hr(),
        plotOutput("movement_plot", height="500px"),
        tags$hr(),
        tags$h4("Pitch metrics", style=glue("color:{TEAL}; margin-top:0;")),
        DTOutput("pitch_metrics_table")
      )
    } else {
      tagList(
        tags$hr(),
        tags$h4("Season stats", style=glue("color:{TEAL}; margin-top:0;")),
        tags$table(
          class="table", style="color:white; width:auto;",
          tags$tr(tags$th("G"),tags$th("IP"),tags$th("ERA"),tags$th("FIP"),
                  tags$th("WHIP"),tags$th("K/9"),tags$th("BB/9"),tags$th("K/BB"),tags$th("HR/9")),
          tags$tr(
            tags$td(p$G), tags$td(round(p$IP,1)), tags$td(round(p$ERA,2)),
            tags$td(round(p$FIP,2)), tags$td(round(p$WHIP,2)), tags$td(round(p$K9,1)),
            tags$td(round(p$BB9,1)), tags$td(round(p$KBB,2)), tags$td(round(p$HR9,1))
          )
        )
      )
    }
  })
  
  output$movement_plot <- renderPlot({
    key <- selected_key(); req(key)
    p   <- all_pitchers %>% filter(source_key == key); req(p$has_pbp)
    pid <- as.integer(p$source_key)
    
    ind_data  <- pitch_level %>%
      filter(pitcher_id == pid, !is.na(hb_pov), !is.na(ivb),
             !is.na(pitch_type), pitch_type != "")
    mean_data <- movement_avg %>% filter(pitcher_id == pid)
    req(nrow(ind_data) > 0)
    
    lim <- max(abs(c(ind_data$hb_pov, ind_data$ivb)), na.rm=TRUE) + 4
    lim <- max(lim, 22)
    
    ggplot() +
      geom_vline(xintercept=0, color="black", linewidth=0.6) +
      geom_hline(yintercept=0, color="black", linewidth=0.6) +
      geom_point(data=ind_data,
                 aes(x=hb_pov, y=ivb, fill=pitch_type),
                 size=3, alpha=0.75, shape=21, color="black", stroke=0.4) +
      geom_point(data=mean_data,
                 aes(x=pfx_x, y=pfx_z, color=pitch_type),
                 size=10, alpha=0.95) +
      geom_text(data=mean_data,
                aes(x=pfx_x, y=pfx_z, label=velo),
                color=WHITE, size=3.5, fontface="bold") +
      scale_color_manual(values=PITCH_COLORS, na.value="#AAAAAA") +
      scale_fill_manual( values=PITCH_COLORS, na.value="#AAAAAA") +
      xlim(-lim, lim) + ylim(-lim, lim) + coord_fixed() +
      labs(title="Pitch Movement",
           x="Horizontal Break (in) — Pitcher's POV",
           y="Induced Vertical Break (in)") +
      theme_minimal(base_size=13) +
      theme(
        plot.background  = element_rect(fill=NAVY,      color=NA),
        panel.background = element_rect(fill="#0F3366", color=NA),
        panel.grid.major = element_line(color="#1A4080", linewidth=0.4),
        panel.grid.minor = element_blank(),
        text             = element_text(color=WHITE),
        axis.text        = element_text(color="#CCCCCC"),
        axis.title       = element_text(color="#AAAAAA", size=11),
        plot.title       = element_text(color=TEAL, face="bold", size=16,
                                        hjust=0.5, margin=margin(b=10)),
        legend.position  = "none"
      )
  }, bg=NAVY)
  
  output$pitch_metrics_table <- renderDT({
    key <- selected_key(); req(key)
    p   <- all_pitchers %>% filter(source_key == key); req(p$has_pbp)
    pid <- as.integer(p$source_key)
    pitch_metrics %>%
      filter(pitcher_id == pid, N >= 5) %>%
      arrange(desc(N)) %>%
      select(Pitch=pitch_type, N, Velo, iVB=iVB, HB,
             `Strike%`=Strike_pct, `FPS%`=FPS_pct, `Whiff%`=Whiff_pct,
             `Rel Ht`=Rel_Ht, `Rel Side`=Rel_Side, Ext) %>%
      datatable(rownames=FALSE,
                options=list(dom="t", pageLength=15, initComplete=dt_header_js()),
                class="display compact") %>%
      formatRound(c("Velo","iVB","HB","Strike%","FPS%","Whiff%","Rel Ht","Rel Side","Ext"), 1)
  })
}

shinyApp(ui, server)