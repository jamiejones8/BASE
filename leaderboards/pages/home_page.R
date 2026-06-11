# =========================================================
#  HOME PAGE — Counting Stat Leaderboards
# =========================================================
home_page_ui <- function() {
  div(
    class = "main-container",
    div(class = "txst-header", APP_HOME_TITLE),
    p(
      class = "page-subtitle",
      "Counting stat leaders for hitters and pitchers from the current bundled data file."
    ),
    div(
      class = "home-intro",
      h4(APP_COMMAND_CENTER),
      p(
        "This home screen highlights quick-look counting leaders from the current Whitecaps data file.",
        "Use the navigation above to jump into full hitter, pitcher, process, and totals breakdowns."
      )
    ),
    tags$hr(),
    div(
      class = "home-columns",
      div(
        class = "home-column",
        div(class = "home-column-title", "Hitting Leaders"),
        div(
          class = "leaderboard-card",
          h4("OPS"),
          DT::dataTableOutput("home_hit_ops")
        ),
        div(
          class = "leaderboard-card",
          h4("Hits"),
          DT::dataTableOutput("home_hit_hits")
        ),
        div(
          class = "leaderboard-card",
          h4("Walks"),
          DT::dataTableOutput("home_hit_walks")
        ),
        div(
          class = "leaderboard-card",
          h4("Home Runs"),
          DT::dataTableOutput("home_hit_hr")
        )
      ),
      div(
        class = "home-column",
        div(class = "home-column-title", "Pitching Leaders"),
        div(
          class = "leaderboard-card",
          h4("ERA"),
          DT::dataTableOutput("home_pitch_era")
        ),
        div(
          class = "leaderboard-card",
          h4("Strikeouts"),
          DT::dataTableOutput("home_pitch_k")
        ),
        div(
          class = "leaderboard-card",
          h4("Walks (Lowest, Min 3 PA)"),
          DT::dataTableOutput("home_pitch_bb_low")
        ),
        div(
          class = "leaderboard-card",
          h4("Innings Pitched"),
          DT::dataTableOutput("home_pitch_ip")
        )
      )
    ),
    tags$hr()
  )
}
