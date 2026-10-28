# Comparison queries retain only selected teams, and never collect the full D1 file.
base_comparison_d1 <- function(team_column, team = NULL) {
  path <- TEAM_CONFIG$data$ncaa_d1_master_file
  shiny::validate(shiny::need(file.exists(path), "The cleaned D1 dataset is not mounted."))
  ds <- arrow::open_dataset(path, format="parquet")
  shiny::validate(shiny::need(team_column %in% names(ds), "D1 team codes are unavailable."))
  if (is.null(team)) {
    return(sort(unique(dplyr::collect(dplyr::distinct(dplyr::select(ds,dplyr::all_of(team_column))))[[team_column]])))
  }
  dplyr::collect(dplyr::filter(ds, .data[[team_column]] == team))
}

# Pool the pitches in each trailing window so rates retain their correct denominators.
base_comparison_rolling_series <- function(d, metric, summarize, window=5L) {
  shiny::validate(shiny::need(nrow(d)>0,"No pitches for this selection."))
  game <- intersect(c("CustomGameID","GameUID","GameID","GameId","Date"),names(d))[1]
  shiny::validate(shiny::need(!is.na(game),"Game identifiers are unavailable."))
  keys <- unique(as.character(d[[game]])); keys <- keys[!is.na(keys)]
  if("Date" %in% names(d)) keys <- keys[order(as.character(d$Date[match(keys,d[[game]])]),keys)]
  window <- max(2L,min(30L,as.integer(window)))
  vals <- vapply(seq_along(keys),function(i) {
    selected <- keys[max(1L,i-window+1L):i]
    x <- summarize(d[as.character(d[[game]]) %in% selected,,drop=FALSE])
    if(metric %in% names(x)) as.numeric(x[[metric]]) else NA_real_
  },numeric(1))
  data.frame(game=seq_along(keys),value=vals,
    label=paste0(keys,"<br>Trailing ",pmin(seq_along(keys),window)," games"))
}

base_comparison_ui <- function(id) {
  ns <- shiny::NS(id)
  controls <- function(key, opponent=FALSE) {
    n <- function(x) ns(paste0(key,"_",x))
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6,shiny::uiOutput(n("season_left_ui")),
          shiny::uiOutput(n("player_left_ui"))),
        shiny::column(6,if(opponent) shiny::uiOutput(n("team_ui")) else shiny::tagList(
          shiny::uiOutput(n("season_right_ui")),
          shiny::uiOutput(n("player_right_ui"))))),
      shiny::fluidRow(
        shiny::column(8,shiny::uiOutput(n("metric_ui"))),
        shiny::column(4,shiny::numericInput(n("window"),"Rolling window (games)",value=5,min=2,max=30,step=1))),
      shiny::uiOutput(n("headers")),
      shiny::fluidRow(shiny::column(6,plotly::plotlyOutput(n("plot_left"),height="320px")),
        shiny::column(6,plotly::plotlyOutput(n("plot_right"),height="320px"))),
      shiny::uiOutput(n("rankings")))
  }
  shiny::tagList(shiny::tags$style(shiny::HTML("
    .base-compare-row{display:grid;grid-template-columns:minmax(0,1fr) 90px minmax(0,1fr);gap:12px;align-items:center;padding:9px 4px}
    .base-compare-percentile{display:grid;grid-template-columns:78px minmax(40px,1fr) 50px;gap:12px;align-items:center}
    .base-compare-track-wrap{position:relative;height:24px;margin:0 12px}
    .base-compare-track{position:absolute;left:0;right:0;top:9px;height:6px;background:#D8E8E9}
    .base-compare-fill{position:absolute;left:0;top:5px;height:14px}
    .base-compare-average{position:absolute;left:50%;top:5px;width:2px;height:14px;background:rgba(255,255,255,.55)}
    .base-compare-badge{position:absolute;top:0;transform:translateX(-50%);min-width:28px;height:24px;border-radius:14px;color:white;font-size:12px;font-weight:900;line-height:24px;text-align:center;padding:0 5px}
    .base-compare-heading{text-align:center;padding:12px}.base-compare-heading img{height:62px;max-width:100px;object-fit:contain}
    .base-compare-direction{text-align:center;font-weight:900;font-size:64px;line-height:1}
    .base-compare-value{font-size:12px;text-align:right}.base-compare-label{font-size:12px;text-align:right}
    .base-compare-scale{display:flex;justify-content:space-between;margin:0 74px 8px 102px;font-size:10px;font-weight:800}
    .base-compare-scale span:first-child{color:#3464B8}.base-compare-scale span:nth-child(2){color:#A4BEC1}.base-compare-scale span:last-child{color:#D9252E}
    .base-compare-rankings{background:#fff;border:1px solid rgba(0,117,138,.32);border-radius:8px;padding:14px}
    .base-compare-rankings h4{border-bottom:2px solid #078197;padding-bottom:8px}
    @media(max-width:750px){.base-compare-percentile{grid-template-columns:1fr;gap:2px}.base-compare-label,.base-compare-value{text-align:center}.base-compare-scale{margin:0 12px}.base-compare-row{grid-template-columns:minmax(0,1fr) 55px minmax(0,1fr)}.base-compare-direction{font-size:48px}}
  ")),
    bslib::navset_tab(id=ns("tab"),
      bslib::nav_panel("Players & Seasons",value="local",controls("local")),
      bslib::nav_panel("Opponents",value="opponents",controls("opponents",TRUE))))
}

base_comparison_server <- function(id, rows, seasons, player_column, summarize, percentile, metrics, lower_better,
                                   active=function() TRUE, d1_loader=base_comparison_d1) {
  shiny::moduleServer(id,function(input,output,session) {
    team_column <- if(player_column == "Pitcher") "PitcherTeam" else "BatterTeam"
    local_rows <- shiny::reactive({
      d <- rows()
      if("is_bullpen" %in% names(d)) d <- d[!d$is_bullpen %in% TRUE,,drop=FALSE]
      if(team_column %in% names(d)) d <- d[base_team_matches(d[[team_column]]) %in% TRUE,,drop=FALSE]
      d
    })
    season_rows <- function(season) {
      d <- local_rows()
      col <- intersect(c("SeasonGroup","SeasonTag"),names(d))[1]
      if(is.na(col)) return(d[FALSE,,drop=FALSE])
      d[!is.na(d[[col]]) & d[[col]] == season,,drop=FALSE]
    }
    # Only the active opponents pane can enumerate or query D1 teams.
    opponents <- shiny::reactive({
      shiny::req(active(),identical(input$tab,"opponents"))
      d1_loader(team_column)
    })
    output$opponents_team_ui <- shiny::renderUI({
      codes <- opponents(); codes <- codes[!is.na(codes) & nzchar(codes)]
      labels <- paste(base_team_display_name(codes),paste0("(",codes,")"))
      current <- shiny::isolate(input$opponents_team)
      shiny::selectInput(session$ns("opponents_team"),"D1 opponent",choices=stats::setNames(codes,labels),
        selected=if(length(current) && current %in% codes) current else head(codes,1))
    })
    for(key in c("local","opponents")) local({
      k <- key
      getinput <- function(n) input[[paste0(k,"_",n)]]
      for(side in if(k == "opponents") "left" else c("left","right")) local({
        s <- side
        output[[paste0(k,"_season_",s,"_ui")]] <- shiny::renderUI({
          d <- local_rows(); col <- intersect(c("SeasonGroup","SeasonTag"),names(d))[1]
          available <- if(is.na(col)) character() else unique(as.character(d[[col]]))
          choices <- seasons[seasons %in% available]
          current <- shiny::isolate(getinput(paste0("season_",s)))
          shiny::selectInput(session$ns(paste0(k,"_season_",s)),paste(if(s == "left") "Left" else "Right","season"),choices=choices,
            selected=if(length(current) && current %in% choices) current else tail(unname(choices),1))
        })
        output[[paste0(k,"_player_",s,"_ui")]] <- shiny::renderUI({
          shiny::req(getinput(paste0("season_",s)))
          d <- season_rows(getinput(paste0("season_",s)))
          choices <- c("Team Total",sort(unique(stats::na.omit(as.character(d[[player_column]])))))
          current <- shiny::isolate(getinput(paste0("player_",s)))
          shiny::selectInput(session$ns(paste0(k,"_player_",s)),"Player / team",choices=choices,
            selected=if(length(current) && current %in% choices) current else "Team Total")
        })
      })
      output[[paste0(k,"_metric_ui")]] <- shiny::renderUI({
        shiny::selectInput(session$ns(paste0(k,"_metric")),"Time-series statistic",choices=metrics,
          selected=shiny::isolate(getinput("metric")) %||% metrics[1])
      })
      selected_rows <- function(side) {
        shiny::req(active(),identical(input$tab,k))
        if(k == "opponents" && side == "right") {
          shiny::req(getinput("team"))
          return(d1_loader(team_column,getinput("team")))
        }
        shiny::req(getinput(paste0("season_",side)),getinput(paste0("player_",side)))
        d <- season_rows(getinput(paste0("season_",side)))
        player <- getinput(paste0("player_",side))
        if(player != "Team Total") d <- d[!is.na(d[[player_column]]) & d[[player_column]] == player,,drop=FALSE]
        d
      }
      left <- shiny::reactive(selected_rows("left"))
      right <- shiny::reactive(selected_rows("right"))
      values_left <- shiny::reactive(summarize(left()))
      values_right <- shiny::reactive(summarize(right()))
      label <- function(side) {
        if(k == "opponents" && side == "right") return(paste(base_team_display_name(getinput("team")),getinput("team")))
        season <- getinput(paste0("season_",side)); season_label <- names(seasons)[match(season,seasons)]
        paste(getinput(paste0("player_",side)),season_label,sep=" · ")
      }
      output[[paste0(k,"_headers")]] <- shiny::renderUI({
        shiny::req(active(),identical(input$tab,k))
        header <- function(side) {
          team <- if(k == "opponents" && side == "right") getinput("team") else TEAM_CONFIG$data_code
          url <- if(exists("team_palette",mode="function")) team_palette(team)$logo_url else NA_character_
          shiny::div(class="base-compare-heading",
            if(length(url) && !is.na(url) && nzchar(url)) shiny::tags$img(src=url,alt=base_team_display_name(team)),
            shiny::tags$h4(label(side)))
        }
        shiny::fluidRow(shiny::column(6,header("left")),shiny::column(6,header("right")))
      })
      rolling_window <- shiny::reactive({
        value <- getinput("window")
        if(is.null(value) || !is.finite(value)) 5L else max(2L,min(30L,as.integer(value)))
      })
      series_left <- shiny::reactive(base_comparison_rolling_series(left(),getinput("metric"),summarize,rolling_window()))
      series_right <- shiny::reactive(base_comparison_rolling_series(right(),getinput("metric"),summarize,rolling_window()))
      for(side in c("left","right")) local({
        s <- side
        output[[paste0(k,"_plot_",s)]] <- plotly::renderPlotly({
          shiny::req(getinput("metric"))
          a <- series_left(); b <- series_right(); d <- if(s == "left") a else b
          limits <- range(c(a$value,b$value),finite=TRUE)
          shiny::validate(shiny::need(all(is.finite(limits)),"No values for this statistic."))
          pad <- max(diff(limits)*.08,abs(limits[1])*.02,.001)
          plotly::plot_ly(d,x=~game,y=~value,text=~label,type="scatter",mode="lines+markers",
            line=list(color=if(s=="left") "#501214" else "#B4975A"),showlegend=FALSE) %>%
            plotly::layout(title=list(text=paste0(rolling_window(),"-game rolling average")),
              xaxis=list(title="Game in selected season"),
              yaxis=list(title=getinput("metric"),range=limits+c(-pad,pad),tickformat=if(grepl("%",getinput("metric"))) ".0%" else NULL))
        })
      })
      output[[paste0(k,"_rankings")]] <- shiny::renderUI({
        a <- values_left(); b <- values_right()
        bar <- function(value,pct,metric) {
          finite <- length(value) && is.finite(value)
          formatted <- if(!finite) "—" else if(grepl("%",metric)) sprintf("%.1f%%",100*value) else sprintf("%.3g",value)
          color <- if(!is.finite(pct)) "#BFC9CA" else if(pct>=90) "#E33434" else if(pct>=75) "#DC735F" else if(pct>=60) "#D39383" else if(pct>=40) "#AAC6CB" else if(pct>=25) "#8CAAD2" else "#5D7EBC"
          position <- if(is.finite(pct)) min(100,max(0,pct)) else 0
          shiny::div(class="base-compare-percentile",
            shiny::div(class="base-compare-label",metric),
            shiny::div(class="base-compare-track-wrap",
              shiny::div(class="base-compare-track"),
              shiny::div(class="base-compare-fill",style=sprintf("width:%.1f%%;background:%s",position,color)),
              shiny::div(class="base-compare-average"),
              shiny::div(class="base-compare-badge",title=if(is.finite(pct)) paste0(round(pct),"th percentile") else "No D1 reference",
                style=sprintf("left:%.1f%%;background:%s",position,color),if(is.finite(pct)) round(pct) else "--")),
            shiny::div(class="base-compare-value",formatted))
        }
        scale <- function() shiny::div(class="base-compare-scale",shiny::span("POOR"),shiny::span("AVERAGE"),shiny::span("GREAT"))
        shiny::div(class="base-compare-rankings",shiny::tags$h4("D1 Percentile Rankings"),
          shiny::tags$p("Arrows point to the better value. Equal values show =; unavailable values show —. Percentiles use the same D1 reference as Performance."),
          shiny::div(class="base-compare-row",scale(),shiny::div(),scale()),
          lapply(metrics,function(m) {
            av <- if(m %in% names(a)) a[[m]] else NA_real_;bv <- if(m %in% names(b)) b[[m]] else NA_real_
            delta <- if(m %in% lower_better) bv-av else av-bv
            arrow <- if(!is.finite(delta)) "—" else if(abs(delta)<1e-10) "=" else if(delta>0) "←" else "→"
            shiny::div(class="base-compare-row",bar(av,percentile(av,m),m),
              shiny::div(class="base-compare-direction",arrow),
              bar(bv,percentile(bv,m),m))
          }))
      })
    })
    invisible(list(local_rows=local_rows))
  })
}

# Client-side linking keeps hover responsive without re-rendering Shiny plots.
base_link_pitch_hover <- function(plot, group) {
  htmlwidgets::onRender(plot,
    paste(readLines(base_project_path("R/plots/linked_pitch_hover.js"),warn=FALSE),collapse="\n"),
    data=list(group=group))
}
