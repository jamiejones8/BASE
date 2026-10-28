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

base_comparison_ui <- function(id) {
  ns <- shiny::NS(id)
  controls <- function(key, opponent=FALSE) {
    n <- function(x) ns(paste0(key,"_",x))
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6,shiny::selectInput(n("season_left"),"Left season",choices=NULL),
          shiny::selectInput(n("player_left"),"Player / team",choices=NULL)),
        shiny::column(6,if(opponent) shiny::selectInput(n("team"),"D1 opponent",choices=NULL) else shiny::tagList(
          shiny::selectInput(n("season_right"),"Right season",choices=NULL),
          shiny::selectInput(n("player_right"),"Player / team",choices=NULL)))),
      shiny::selectInput(n("metric"),"Time-series statistic",choices=NULL),
      shiny::uiOutput(n("headers")),
      shiny::fluidRow(shiny::column(6,plotly::plotlyOutput(n("plot_left"),height="320px")),
        shiny::column(6,plotly::plotlyOutput(n("plot_right"),height="320px"))),
      shiny::uiOutput(n("rankings")))
  }
  shiny::tagList(shiny::tags$style(shiny::HTML(".base-compare-row{display:grid;grid-template-columns:minmax(0,1fr) 115px minmax(0,1fr);gap:16px;align-items:center;padding:9px 4px;border-bottom:1px solid #ded7cc}.base-compare-bar{position:relative;background:#e3e5e1;height:20px;border-radius:10px;margin-top:4px}.base-compare-bar span{display:block;height:100%;border-radius:10px;min-width:0}.base-compare-bar b{position:absolute;inset:0;text-align:center;font-size:12px;color:#222}.base-compare-heading{text-align:center;padding:12px}.base-compare-heading img{height:62px;max-width:100px;object-fit:contain}.base-compare-direction{text-align:center;font-weight:700}.base-compare-value{font-size:13px}.base-compare-label{text-align:center;font-size:12px}")),
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
    shiny::observeEvent(opponents(), {
      codes <- opponents(); codes <- codes[!is.na(codes) & nzchar(codes)]
      labels <- paste(base_team_display_name(codes),paste0("(",codes,")"))
      shiny::updateSelectInput(session,"opponents_team",choices=stats::setNames(codes,labels))
    })
    for(key in c("local","opponents")) local({
      k <- key
      getinput <- function(n) input[[paste0(k,"_",n)]]
      for(side in if(k == "opponents") "left" else c("left","right")) local({
        s <- side
        shiny::observe({
          d <- local_rows(); col <- intersect(c("SeasonGroup","SeasonTag"),names(d))[1]
          available <- if(is.na(col)) character() else unique(as.character(d[[col]]))
          choices <- seasons[seasons %in% available]
          current <- shiny::isolate(getinput(paste0("season_",s)))
          shiny::updateSelectInput(session,paste0(k,"_season_",s),choices=choices,
            selected=if(length(current) && current %in% choices) current else tail(unname(choices),1))
        })
        shiny::observe({
          shiny::req(getinput(paste0("season_",s)))
          d <- season_rows(getinput(paste0("season_",s)))
          choices <- c("Team Total",sort(unique(stats::na.omit(as.character(d[[player_column]])))))
          current <- shiny::isolate(getinput(paste0("player_",s)))
          shiny::updateSelectInput(session,paste0(k,"_player_",s),choices=choices,
            selected=if(length(current) && current %in% choices) current else "Team Total")
        })
      })
      shiny::updateSelectInput(session,paste0(k,"_metric"),choices=metrics,selected=metrics[1])
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
      series <- function(d,metric) {
        shiny::validate(shiny::need(nrow(d)>0,"No pitches for this selection."))
        game <- intersect(c("CustomGameID","GameUID","GameID","GameId","Date"),names(d))[1]
        shiny::validate(shiny::need(!is.na(game),"Game identifiers are unavailable."))
        keys <- unique(as.character(d[[game]]));keys <- keys[!is.na(keys)]
        if("Date" %in% names(d)) keys <- keys[order(as.character(d$Date[match(keys,d[[game]])]),keys)]
        vals <- vapply(keys,function(g) {
          x <- summarize(d[as.character(d[[game]]) == g & !is.na(d[[game]]),,drop=FALSE])
          if(metric %in% names(x)) as.numeric(x[[metric]]) else NA_real_
        },numeric(1))
        data.frame(game=seq_along(keys),value=vals,label=keys)
      }
      series_left <- shiny::reactive(series(left(),getinput("metric")))
      series_right <- shiny::reactive(series(right(),getinput("metric")))
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
            plotly::layout(xaxis=list(title="Game in selected season"),
              yaxis=list(title=getinput("metric"),range=limits+c(-pad,pad),tickformat=if(grepl("%",getinput("metric"))) ".0%" else NULL))
        })
      })
      output[[paste0(k,"_rankings")]] <- shiny::renderUI({
        a <- values_left(); b <- values_right()
        bar <- function(value,pct,metric) {
          finite <- length(value) && is.finite(value)
          formatted <- if(!finite) "—" else if(grepl("%",metric)) sprintf("%.1f%%",100*value) else sprintf("%.3g",value)
          color <- if(!is.finite(pct)) "#bbb" else if(pct>=50) "#25813E" else "#C83232"
          shiny::div(shiny::div(class="base-compare-value",formatted),
            shiny::div(class="base-compare-bar",shiny::span(style=sprintf("width:%.1f%%;background:%s",if(is.finite(pct)) pct else 0,color)),
              shiny::tags$b(if(is.finite(pct)) paste0(round(pct),"th percentile") else "No D1 reference")))
        }
        shiny::tagList(shiny::tags$h4("D1 Percentile Rankings"),
          shiny::tags$p("Arrows point to the better value. Equal values show =; unavailable values show —. Percentiles use the same D1 reference as Performance."),
          lapply(metrics,function(m) {
            av <- if(m %in% names(a)) a[[m]] else NA_real_;bv <- if(m %in% names(b)) b[[m]] else NA_real_
            delta <- if(m %in% lower_better) bv-av else av-bv
            arrow <- if(!is.finite(delta)) "—" else if(abs(delta)<1e-10) "=" else if(delta>0) "←" else "→"
            shiny::div(class="base-compare-row",bar(av,percentile(av,m),m),
              shiny::div(class="base-compare-direction",shiny::div(class="base-compare-label",m),shiny::span(style="font-size:24px",arrow)),
              bar(bv,percentile(bv,m),m))
          }))
      })
    })
    invisible(list(local_rows=local_rows))
  })
}
