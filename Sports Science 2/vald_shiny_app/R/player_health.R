# Independent source views. Vendor scores are imported, never reconstructed.
health_vald_jump <- function(v) {
  if(is.null(v)||!nrow(v)) return(NA_character_)
  mc<-get_metric_id_col(v);bc<-get_best_col(v);dc<-get_date_col(v)
  if(is.null(mc)||is.null(bc)||is.null(dc)) return(NA_character_)
  v<-v[v[[mc]]=="Jump Height (Imp-Mom)",,drop=FALSE]
  if("testType" %in% names(v)) v<-v[v$testType=="CMJ",,drop=FALSE]
  if(!nrow(v)) return(NA_character_)
  v<-v[order(as_date_safely(v[[dc]]),decreasing=TRUE),,drop=FALSE]
  as.character(round(health_num(v[[bc]][1]),2))
}
# Shiny auto-sources R/ alphabetically before app.R. Resolve this helper
# when called, after roster_trackman.R has also been loaded.
health_key <- function(x) player_name_key(x)
health_num <- function(x) suppressWarnings(as.numeric(x))
health_latest <- function(d) {
  if (!nrow(d)) return(d)
  d <- d[order(d$date, d$order, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  d[!duplicated(d$key), , drop = FALSE]
}
health_read <- function(root = Sys.getenv("PLAYER_HEALTH_DATA_DIR", file.path(dirname(getwd()), "data"))) {
  out <- list(fresh = data.frame(), post = data.frame(), exams = data.frame(), workload = data.frame(), events = data.frame(), errors = character())
  newest <- function(pattern) { p <- list.files(root, pattern, full.names = TRUE); if (!length(p)) return(NULL); p[which.max(file.info(p)$mtime)] }
  book <- newest("\\.xlsx$")
  daily_logs <- list.files(root,"^Daily Log.*\\.csv$",full.names=TRUE,ignore.case=TRUE)
  arm_paths <- if(length(daily_logs)) daily_logs[order(file.info(daily_logs)$mtime,daily_logs)] else book
  for (nm in c("fresh", "post", "exams", "workload", "events")) {
    out[[nm]] <- tryCatch({
      arm <- nm %in% c("fresh", "post", "exams")
      paths <- if (arm) arm_paths else newest(paste0("_", nm, "\\.csv$"))
      if (!length(paths)) stop(paste("No", if (arm) "ArmCare workbook or Daily Log CSV" else nm, "export found"))
      parts <- lapply(paths,function(path) tryCatch({
      d <- if (arm && grepl("\\.xlsx$", path, ignore.case=TRUE)) as.data.frame(readxl::read_excel(path, sheet = c(fresh="Fresh Exam Key Metrics",post="Post Exam Key Metrics",exams="Exam Data")[[nm]], col_types="text")) else read.csv(path, check.names=FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE,colClasses=if(arm)"character" else NA)
      if (arm && grepl("\\.csv$", path, ignore.case=TRUE) && nm != "exams") {
        type <- if(nm == "fresh") "Fresh" else "Post"
        d <- d[grepl(type, d[["Exam Type"]], ignore.case=TRUE), , drop=FALSE]
      }
      d$name <- if (arm) paste(trimws(d[["First Name"]]), trimws(d[["Last Name"]])) else paste(trimws(d$firstName),trimws(d$lastName))
      d$key <- health_key(d$name)
      d$date <- if (arm) {z<-as.Date(d[["Exam Date"]], "%m/%d/%Y"); missing<-is.na(z);z[missing]<-as.Date(d[["Exam Date"]][missing],"%Y-%m-%d");z} else if (nm == "events") as.Date(as.POSIXct(d$datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tz="America/Chicago") else as.Date(substr(d$date,1,10))
      d$order <- seq_len(nrow(d))
      if (arm) d$order <- as.numeric(as.POSIXct(paste(d$date,d$Time),format="%Y-%m-%d %H:%M:%S",tz="America/Chicago"))
      d <- d[!is.na(d$date) & nzchar(d$key), , drop=FALSE]
      d$source_file <- rep(basename(path),nrow(d))
      d
      },error=function(e){out$errors <<- c(out$errors,paste(nm,basename(path),conditionMessage(e)));data.frame()}))
      d <- dplyr::bind_rows(parts)
      if(arm && nrow(d)) {
        type <- if("Exam Type" %in% names(d))trimws(d[["Exam Type"]]) else rep(nm,nrow(d))
        time <- if("Time" %in% names(d))trimws(d$Time) else rep("",nrow(d))
        identity <- paste(d$key,d$date,time,type,sep="|")
        # Missing timestamps must not collapse distinct exams on a day.
        missing_time <- is.na(time)|!nzchar(time)
        fields <- setdiff(names(d),c("source_file","order"))
        identity[missing_time] <- apply(d[missing_time,fields,drop=FALSE],1,paste,collapse="|")
        d <- d[!duplicated(identity,fromLast=TRUE),,drop=FALSE]
      }
      attr(d,"source") <- paste(basename(paths),collapse="; ")
      d
    }, error=function(e) {out$errors <<- c(out$errors,paste(nm,conditionMessage(e))); data.frame()})
  }
  out
}
health_table <- function(d) {
  groups <- if(all(c("IR (%BW)","Grip Recovery") %in% names(d))) c(Player=2,`Arm Strength (%BW)`=5,`Shoulder Balance`=1,`Strength / Velocity`=3,Recovery=4) else NULL
  container <- if(!is.null(groups)) tags$table(tags$thead(tags$tr(lapply(seq_along(groups),function(i)tags$th(colspan=groups[i],names(groups)[i]))),tags$tr(lapply(names(d),tags$th)))) else tags$table(tags$thead(tags$tr(lapply(names(d),tags$th))))
  widget <- DT::datatable(d, container=container, rownames=FALSE, selection="single", options=list(scrollX=TRUE,pageLength=15,dom="ftp",order=list()), class="stripe hover compact")
  numeric_cols <- names(d)[vapply(d,is.numeric,logical(1))]
  if(length(numeric_cols)) widget <- DT::formatRound(widget,numeric_cols,digits=2)
  recovery_cols <- grep("Recovery$",names(d),value=TRUE)
  if(length(recovery_cols)) widget <- DT::formatStyle(widget,recovery_cols,fontWeight=DT::styleEqual(c("Watch","Warning","Medical"),c("bold","bold","bold")),color=DT::styleEqual(c("Normal","Watch","Warning","Medical"),c("#71644b","#927000","#7a3438","#501214")))
  widget
}
health_cards <- function(labels, values) tags$div(class="health-cards", lapply(seq_along(labels),function(i) tags$div(class="health-stat",tags$small(labels[i]),tags$strong(ifelse(is.na(values[i]),"—",as.character(values[i]))))))
health_header <- function(title, subtitle) tags$div(class="health-header",tags$img(src=paste0(PLAYER_HEALTH_ASSET_PREFIX,"/bobcat.png"),alt="Texas State Bobcats"),tags$div(tags$h2(title),tags$p(subtitle)))
health_ui <- function() list(
  nav_panel("ArmCare", value="armcare", tags$div(class="health-page",
    health_header("ArmCare", "Strength • Recovery • Exam history"),
    fluidRow(column(5,selectInput("hc_arm_player","Player",choices=c("Team"=""))),column(4,dateRangeInput("hc_arm_dates","Exam dates",start=as.Date("2026-08-01"),end=Sys.Date())),column(3,downloadButton("hc_arm_export","Export displayed data"))),
    uiOutput("hc_arm_hero"), uiOutput("hc_arm_cards"),
    navset_tab(id="hc_arm_view",
      nav_panel("Dashboard",value="dashboard",plotlyOutput("hc_arm_trend",height="340px"),DT::DTOutput("hc_arm_summary")),
      nav_panel("Key Metrics",value="metrics",radioButtons("hc_arm_mode",NULL,c("Strength & Recovery"="fresh","Fatigue"="post"),inline=TRUE),tags$p(class="health-note","Recovery labels are imported from ArmCare. Numerical change-from-average values are not included in this export."),DT::DTOutput("hc_arm_metrics")),
      nav_panel("Exams",value="exams",radioButtons("hc_exam_type",NULL,c("Fresh Exam"="Fresh","Post Exam"="Post","Arm Primer"="Primer"),inline=TRUE),radioButtons("hc_exam_measure",NULL,c("Strength (lbs)"="strength","Range of motion (°)"="rom"),inline=TRUE),DT::DTOutput("hc_arm_exams"))
    ),uiOutput("hc_arm_source")
  )),
  nav_panel("PULSE",value="pulse",tags$div(class="health-page",
    health_header("PULSE", "Throwing workload • Long-term trends • Individual throws"),
    fluidRow(column(5,selectInput("hc_pulse_player","Player",choices=character())),column(4,dateRangeInput("hc_pulse_dates","Workload dates",start=as.Date("2026-08-01"),end=Sys.Date())),column(3,downloadButton("hc_pulse_export","Export workload"))),
    navset_tab(id="hc_pulse_view",
      nav_panel("Diagnostic",uiOutput("hc_pulse_cards"),fluidRow(column(6,DT::DTOutput("hc_pulse_team")),column(6,plotlyOutput("hc_workload_plot",height="340px"),plotlyOutput("hc_chronic_plot",height="250px"))),tags$p(class="health-note","Arm health, priority workout, and projected workload are not included in the supplied export.")),
      nav_panel("Throwing Calendar",dateInput("hc_calendar_month","Month",value=Sys.Date(),format="MM yyyy",startview="year"),uiOutput("hc_calendar")),
      nav_panel("Individual Throws",fluidRow(column(4,uiOutput("hc_throw_date_ui")),column(4,checkboxInput("hc_high_effort","High effort only",FALSE)),column(4,downloadButton("hc_event_export","Export throws"))),
        checkboxGroupInput("hc_throw_metrics","Metrics",choices=c("Torque"="torque","Arm Speed"="armSpeed","Arm Slot"="armSlot","Shoulder Rotation"="shoulderRotation"),selected="torque",inline=TRUE),uiOutput("hc_throw_cards"),plotlyOutput("hc_throw_plot",height="430px"),tags$p(class="health-note","Each metric uses its own labeled scale. Event dates use America/Chicago time. Vendor fatigue units are not present in this export."))
    ),uiOutput("hc_pulse_source")
  )),
  nav_panel("Combined Health",value="combined",tags$div(class="health-page",
    health_header("Player Health", "Baseball performance + VALD + ArmCare + PULSE"),
    fluidRow(column(5,selectInput("hc_combined_player","Player",choices=character())),
      column(4,dateRangeInput("hc_combined_dates","Trend dates",start=as.Date("2025-01-01"),end=Sys.Date())),
      column(3,downloadButton("hc_trackman_export","Export daily pitching KPIs"))),
    navset_tab(id="hc_combined_view",
    nav_panel("Time Series",value="trends",
    tags$h3("Baseball performance KPIs"), uiOutput("hc_trackman_cards"),
    tags$p(class="health-note","Daily pitch-weighted averages. KPI #1: fastball/sinker velocity. Fastball/sinker includes those two tags; breaking ball includes cutter, slider, curveball and sweeper. Missing measurements stay missing; counts show valid pitches for each metric."),
    combined_trend_ui(),
    tags$h3("Latest health measurements"), uiOutput("hc_combined_cards"),
    tags$p(class="health-note","Latest readings retain their own dates. Names match the fall roster with explicit alias mappings. Each trend has its own scale; no missing dates are filled."),
    DT::DTOutput("hc_combined_table"),
    tags$h3("Daily pitching data"), DT::DTOutput("hc_trackman_table"),
    uiOutput("hc_trackman_source")),
    nav_panel("KPI Correlations",value="correlations",correlation_ui())),
    uiOutput("hc_load_status")
  ))
)
health_server <- function(input,output,session,vald_roster,vald_summary,fall_roster = reactive(read_fall_roster()),vald_sprint = reactive(data.frame())) {
  export_root <- function() Sys.getenv("PLAYER_HEALTH_DATA_DIR",file.path(dirname(getwd()),"data"))
  raw_data <- reactivePoll(5000,session,
    checkFunc=function() file_signature(list.files(export_root(),full.names=TRUE)), valueFunc=function() health_read(export_root()))
  data <- reactive({
    d <- raw_data(); r <- fall_roster()
    for(nm in c("fresh","post","exams","workload","events")) d[[nm]] <- roster_filter(d[[nm]],r)
    d
  })
  trackman_raw <- reactivePoll(5000,session,
    checkFunc=function() file_signature(c(trackman_files(export_root()),fall_roster_path(),roster_alias_path())),
    valueFunc=function() trackman_read(export_root(),read_fall_roster()))
  observeEvent(trackman_raw(), {
    dates <- trackman_raw()$daily$date
    if(length(dates)) updateDateRangeInput(session,"hc_combined_dates",start=min(dates),end=max(c(Sys.Date(),dates)))
  },once=TRUE)
  observeEvent(raw_data(), {
    dates <- raw_data()$exams$date
    if(length(dates)) updateDateRangeInput(session,"hc_arm_dates",start=min(dates),end=max(c(Sys.Date(),dates)))
  },once=TRUE)
  players <- reactive(sort(fall_roster()$players$name))
  observe({
    p <- players(); choices <- setNames(health_key(p),p)
    for(id in c("hc_arm_player","hc_pulse_player","hc_combined_player")) {
      opts <- if(id=="hc_arm_player") c("Team"="",choices) else choices
      current <- isolate(input[[id]])
      selected <- if(length(current) && current %in% unname(opts)) current else if(length(opts)) unname(opts[1]) else character()
      updateSelectInput(session,id,choices=opts,selected=selected)
    }
  })
  within_dates <- function(d,dates,key=NULL) {if(!nrow(d)) return(d); if(!is.null(dates)) d<-d[d$date>=as.Date(dates[1]) & d$date<=as.Date(dates[2]),,drop=FALSE]; if(!is.null(key)&&nzchar(key)) d<-d[d$key==key,,drop=FALSE]; d}
  arm <- reactive(within_dates(data()$fresh,input$hc_arm_dates,input$hc_arm_player))
  arm_rows <- reactive(if(is.null(input$hc_arm_player)||!nzchar(input$hc_arm_player)) health_latest(arm()) else arm()[order(arm()$date,decreasing=TRUE),,drop=FALSE])
  select_cols <- function(d,mapping) {if(!nrow(d)) return(data.frame(Message="No matching data in this date range.")); o<-d[,intersect(unname(mapping),names(d)),drop=FALSE]; names(o)<-names(mapping)[match(names(o),mapping)]; o}
  fresh_map<-c(Player="name",Date="date",`Arm Score`="Arm Score",`IR (%BW)`="IRTARM RS",`ER (%BW)`="ERTARM RS",`Scaption (%BW)`="STARM RS",`Grip (%BW)`="GTARM RS",`ER:IR`="Shoulder Balance",SVR="SVR",`Total Strength (lbs)`="Total Strength",`Max Velocity`="Velo",`IR Recovery`="IRTARM Recovery",`ER Recovery`="ERTARM Recovery",`Scaption Recovery`="STARM Recovery",`Grip Recovery`="GTARM Recovery")
  metrics <- reactive({if(identical(input$hc_arm_mode,"post")) {d<-within_dates(data()$post,input$hc_arm_dates,input$hc_arm_player); if(is.null(input$hc_arm_player)||!nzchar(input$hc_arm_player)) d<-health_latest(d); select_cols(d,c(Player="name",Date="date",`Post Strength (lbs)`="Total Strength Post",`Strength Loss (lbs)`="Post Strength Loss",`Fresh Strength Retained (%)`="Total %Fresh",`IR Retained (%)`="IRTARM %Fresh",`ER Retained (%)`="ERTARM %Fresh",`Scaption Retained (%)`="STARM %Fresh",`Grip Retained (%)`="GTARM %Fresh"))} else {d<-arm_rows(); for(n in c("IRTARM RS","ERTARM RS","STARM RS","GTARM RS")) if(n %in% names(d)) d[[n]]<-round(100*health_num(d[[n]]),1); select_cols(d,fresh_map)}})
  exam_rows <- reactive({d<-within_dates(data()$exams,input$hc_arm_dates,input$hc_arm_player); if(nrow(d)) d<-d[grepl(input$hc_exam_type %||% "Fresh",d[["Exam Type"]],ignore.case=TRUE),,drop=FALSE]; if(is.null(input$hc_arm_player)||!nzchar(input$hc_arm_player)) d<-health_latest(d); mapping<-if(identical(input$hc_exam_measure,"rom")) c(`IR (°)`="IRTARM ROM",`ER (°)`="ERTARM ROM",`Total Arc (°)`="TARM TARC",`Flexion (°)`="FTARM ROM") else c(`IR (lbs)`="IRTARM Max-Lbs",`ER (lbs)`="ERTARM Max-Lbs",`Scaption (lbs)`="STARM Max-Lbs",`Grip (lbs)`="GTARM Max-Lbs",`Primer (lbs)`="Total Primer Max-Lbs"); select_cols(d,c(Player="name",Date="date",Type="Exam Type",mapping))})
  output$hc_arm_metrics<-DT::renderDT(health_table(metrics()))
  output$hc_arm_exams<-DT::renderDT(health_table(exam_rows()))
  output$hc_arm_summary<-DT::renderDT(health_table(select_cols(arm_rows(),c(Player="name",Date="date",`Arm Score`="Arm Score",`Total Strength (lbs)`="Total Strength",`ER:IR`="Shoulder Balance"))))
  output$hc_arm_hero<-renderUI({d<-arm_rows(); if(!nrow(d)||is.null(input$hc_arm_player)||!nzchar(input$hc_arm_player)) return(NULL); tags$div(class="health-player",tags$h3(d$name[1]),paste(d[["Position 1"]][1],d[["Playing Level"]][1],paste("Throws",d$Throws[1]),paste(d[["Weight (lbs)"]][1],"lbs"),sep=" · "))})
  output$hc_arm_cards<-renderUI({d<-health_latest(arm()); health_cards(c("Players with exams","Exams in range","Latest exam"),c(nrow(d),nrow(arm()),if(nrow(d)) as.character(max(d$date)) else "—"))})
  line_plot <- function(d,x,y,title,unit="") {d <- d[is.finite(health_num(d[[y]])) & !is.na(d[[x]]),,drop=FALSE]; shiny::validate(shiny::need(nrow(d)>0,"No data for this selection.")); plotly::layout(plotly::plot_ly(d,x=d[[x]],y=health_num(d[[y]]),type="scatter",mode="lines+markers",name=unit,showlegend=FALSE,line=list(color="#501214"),marker=list(color="#501214")),title=title,xaxis=list(title=""),yaxis=list(title=unit))}
  output$hc_arm_trend<-plotly::renderPlotly({d<-arm(); shiny::validate(shiny::need(nrow(d)>0,"No ArmCare exams in this range.")); if(is.null(input$hc_arm_player)||!nzchar(input$hc_arm_player)) {d<-aggregate(health_num(d[["Arm Score"]]),list(date=d$date),mean,na.rm=TRUE); names(d)[2]<-"Arm Score"}; d<-d[order(d$date),]; line_plot(d,"date","Arm Score",if(nzchar(input$hc_arm_player %||% "")) "Arm Score" else "Team average Arm Score","Arm Score")})
  workload <- reactive(within_dates(data()$workload,input$hc_pulse_dates,input$hc_pulse_player))
  team <- reactive(health_latest(within_dates(data()$workload,input$hc_pulse_dates)))
  output$hc_pulse_team<-DT::renderDT(health_table(select_cols(team(),c(Name="name",Date="date",`A:C Ratio`="A:C Ratio",`Acute WL`="Acute Workload",`Chronic WL`="Chronic Workload",Throws="Total Throw Count",`1-Day WL`="One Day Workload"))))
  observeEvent(input$hc_pulse_team_rows_selected,{i<-input$hc_pulse_team_rows_selected; if(length(i)) updateSelectInput(session,"hc_pulse_player",selected=team()$key[i])})
  output$hc_pulse_cards<-renderUI({d<-health_latest(workload()); if(!nrow(d)) return(health_cards("Workload","No data")); health_cards(c("Latest date","A:C Ratio","Acute workload","Chronic workload"),c(as.character(d$date[1]),round(health_num(d[["A:C Ratio"]][1]),2),round(health_num(d[["Acute Workload"]][1]),2),round(health_num(d[["Chronic Workload"]][1]),2)))})
  output$hc_workload_plot <- plotly::renderPlotly({
    d <- workload(); shiny::validate(shiny::need(nrow(d)>0,"No workload data.")); d <- d[order(d$date),]
    valid <- is.finite(health_num(d[["One Day Workload"]]))
    p <- plotly::plot_ly(d[valid,,drop=FALSE],x=~date,y=~`One Day Workload`,type="bar",name="1-Day Workload",marker=list(color="#501214"))
    valid <- is.finite(health_num(d[["A:C Ratio"]]))
    segments <- split(d[valid,,drop=FALSE],cumsum(!valid)[valid])
    for(i in seq_along(segments)) p <- plotly::add_trace(p,data=segments[[i]],x=~date,y=~`A:C Ratio`,type="scatter",mode="lines+markers",
      name="A:C Ratio",legendgroup="ratio",showlegend=i==1,yaxis="y2",line=list(color="#b58b00"),inherit=FALSE)
    plotly::layout(p,title="A:C Ratio and Daily Workload",yaxis=list(title="1-Day workload"),yaxis2=list(title="A:C Ratio",overlaying="y",side="right"),xaxis=list(title=""),legend=list(orientation="h",y=-.3))
  })
  output$hc_chronic_plot<-plotly::renderPlotly({d<-workload(); if(nrow(d)) d<-d[order(d$date),];line_plot(d,"date","Chronic Workload","Chronic Workload","Workload")})
  output$hc_calendar<-renderUI({req(input$hc_calendar_month); start<-as.Date(format(as.Date(input$hc_calendar_month),"%Y-%m-01")); days<-seq(start,by="day",length.out=42); days<-days-as.integer(format(start,"%w"));d<-data()$workload; if(nrow(d)) d<-d[d$key==input$hc_pulse_player,,drop=FALSE]; tags$div(class="health-calendar",lapply(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),function(x)tags$b(x)),lapply(days,function(day) {day<-as.Date(day,origin="1970-01-01");v<-d[d$date==day,,drop=FALSE];tags$div(class=if(format(day,"%m")==format(start,"%m")) "health-day" else "health-day muted",tags$small(format(day,"%d")),if(nrow(v)&&!is.na(v[["Total Throw Count"]][1])) tags$strong(v[["Total Throw Count"]][1]) else tags$span("—"))}),tags$p(class="health-note","Total throws • — indicates no exported count, not zero throws."))})
  output$hc_throw_date_ui<-renderUI({d<-data()$events; dates<-if(nrow(d)) sort(unique(d$date[d$key==input$hc_pulse_player]),decreasing=TRUE) else character(); selectInput("hc_throw_date","Throw date",choices=as.character(dates))})
  events<-reactive({d<-data()$events; if(!nrow(d)) return(d); d<-d[d$key==input$hc_pulse_player & as.character(d$date)==(input$hc_throw_date %||% ""),,drop=FALSE]; if(isTRUE(input$hc_high_effort))d<-d[tolower(as.character(d$highEffort))=="true",,drop=FALSE];d<-d[order(d$datetime),];d$throw<-seq_len(nrow(d));d})
  output$hc_throw_cards<-renderUI({d<-events(); health_cards(c("Throws shown","High effort throws","Simulated throws"),c(nrow(d),sum(tolower(as.character(d$highEffort))=="true"),sum(tolower(as.character(d$simulated))=="true")))})
  output$hc_throw_plot<-plotly::renderPlotly({d<-events();shiny::validate(shiny::need(nrow(d)>0,"No throws for this selection."),shiny::need(length(input$hc_throw_metrics)>0,"Select at least one metric.")); labels<-c(torque="Torque (Nm)",armSpeed="Arm Speed (°/s)",armSlot="Arm Slot (°)",shoulderRotation="Shoulder Rotation (°)");colors<-c("#501214","#b58b00","#926b70","#71644b");plots<-lapply(seq_along(input$hc_throw_metrics),function(i){m<-input$hc_throw_metrics[i];plotly::layout(plotly::plot_ly(d,x=~throw,y=health_num(d[[m]]),type="scatter",mode="lines",name=labels[[m]],text=~datetime,line=list(color=colors[i])),yaxis=list(title=labels[[m]]),xaxis=list(title="Throw number"))});plotly::subplot(plots,nrows=length(plots),shareX=TRUE,titleY=TRUE)})
  output$hc_arm_source<-renderUI({d<-data()$exams;tags$p(class="health-note",paste("Sources:",attr(d,"source") %||% "Unavailable",if(nrow(d))paste("· Imported roster history:",min(d$date),"to",max(d$date),"·",nrow(d),"exams.") else "· No matched roster exams."))})
  output$hc_pulse_source<-renderUI(tags$p(class="health-note",paste("Source:",attr(data()$workload,"source") %||% "Unavailable","•",attr(data()$events,"source") %||% "Unavailable")))
  combined<-reactive({key<-input$hc_combined_player %||% ""; a<-data()$fresh;w<-data()$workload;a<-if(nrow(a))health_latest(a[a$key==key,,drop=FALSE]) else a;w<-if(nrow(w))health_latest(w[w$key==key,,drop=FALSE]) else w;r<-vald_roster();v<-vald_summary();ids<-if(nrow(r)&&"athleteName"%in%names(r)) r$profileId[health_key(r$athleteName)==key] else character(); if(length(ids)!=1||!"profileId"%in%names(v)) v<-data.frame() else v<-v[v$profileId==ids,,drop=FALSE];list(a=a,w=w,v=v)})
  combined_rows<-reactive({d<-combined();dc<-if(nrow(d$v))get_date_col(d$v) else NULL;vd<-if(!is.null(dc))max(as_date_safely(d$v[[dc]]),na.rm=TRUE) else as.Date(NA);data.frame(Source=c("VALD","ArmCare","PULSE"),`Latest measurement`=c(as.character(vd),if(nrow(d$a))as.character(d$a$date[1]) else NA,if(nrow(d$w))as.character(d$w$date[1]) else NA),Metric=c("Jump Height (cm)","Arm Score","A:C Ratio"),Value=c(health_vald_jump(d$v),if(nrow(d$a))d$a[["Arm Score"]][1] else NA,if(nrow(d$w))as.character(round(d$w[["A:C Ratio"]][1],2)) else NA),check.names=FALSE)})
  output$hc_combined_table<-DT::renderDT(health_table(combined_rows()))
  output$hc_combined_cards<-renderUI({d<-combined_rows();health_cards(paste(d$Source,d$Metric,sep=" · "),d$Value)})
  pitching_daily <- reactive(within_dates(trackman_raw()$daily,input$hc_combined_dates,input$hc_combined_player %||% ""))
  pitching_series <- reactive(trackman_series(pitching_daily()))
  output$hc_trackman_cards <- renderUI({
    d <- pitching_series()
    tags$div(class="health-cards",lapply(seq_along(trackman_metrics),function(i) {
      metric <- names(trackman_metrics)[i]
      v <- d[d$metric==metric & is.finite(d$value),,drop=FALSE]
      v <- v[order(v$date,decreasing=TRUE),,drop=FALSE]
      tags$div(class=paste("health-stat",if(i==1)"health-primary-kpi" else ""),
        tags$small(paste0(if(i==1)"KPI #1 · " else "",trackman_metrics[i])),
        tags$strong(if(nrow(v))format(round(v$value[1],if(grepl("spin",metric))0 else 1),trim=TRUE) else "—"),
        tags$small(if(nrow(v))paste(v$date[1],"·",v$n[1],"pitches") else "No measurements in range"))
    }))
  })
  vald_catalog <- reactive(combined_vald_catalog(vald_summary(),vald_sprint()))
  observe({
    catalog <- vald_catalog()
    choices <- split(setNames(catalog$id,catalog$label),paste(catalog$source,catalog$test,sep=" · "))
    current <- isolate(input$hc_vald_metric) %||% combined_default_vald
    # Keep the CMJ default even if this athlete has no jump history.
    if(!combined_default_vald %in% catalog$id) choices[["Default"]] <- c("CMJ · Jump Height (Imp-Mom) (Centimeter)"=combined_default_vald)
    if(!current %in% c(catalog$id,combined_default_vald)) current <- combined_default_vald
    updateSelectizeInput(session,"hc_vald_metric",choices=choices,selected=current,server=TRUE)
  })
  combined_plot_data <- reactive({
    key <- input$hc_combined_player %||% ""
    if(!nzchar(key)) return(setNames(rep(list(trend_empty()),4),c("trackman","armcare","pulse","vald")))
    dates <- input$hc_combined_dates
    tm <- input$hc_trackman_metric %||% "fb_velocity"
    p <- pitching_series(); p <- p[p$metric==tm,,drop=FALSE]
    track <- if(nrow(p)) trend_rows(p$date,p$value,unname(trackman_metrics[tm]),if(grepl("spin",tm))"rpm" else "mph",n=p$n) else trend_empty()
    a <- within_dates(data()$fresh,dates,key)
    w <- within_dates(data()$workload,dates,key)
    e <- within_dates(data()$events,dates,key)
    sp <- vald_sprint()
    if(is.null(sp)) sp <- data.frame()
    if(nrow(sp)) sp <- sp[health_key(sp$athlete_name)==key,,drop=FALSE]
    v <- combined_vald_series(combined()$v,sp,vald_catalog(),input$hc_vald_metric %||% combined_default_vald)
    list(trackman=track,armcare=combined_arm_series(a,input$hc_arm_metric %||% "score"),
      pulse=combined_pulse_series(w,e,input$hc_pulse_metric %||% "A:C Ratio"),vald=within_dates(v,dates))
  })
  output$hc_trackman_trend <- plotly::renderPlotly(combined_trend_plot(combined_plot_data()$trackman,input$hc_combined_dates,"TrackMan"))
  output$hc_armcare_trend <- plotly::renderPlotly(combined_trend_plot(combined_plot_data()$armcare,input$hc_combined_dates,"ArmCare"))
  output$hc_pulse_trend <- plotly::renderPlotly(combined_trend_plot(combined_plot_data()$pulse,input$hc_combined_dates,"PULSE"))
  output$hc_vald_trend <- plotly::renderPlotly(combined_trend_plot(combined_plot_data()$vald,input$hc_combined_dates,"VALD"))
  correlation_data <- reactive({
    key <- input$hc_combined_player %||% ""; dates <- input$hc_combined_dates
    if(!nzchar(key)) return(data.frame())
    h <- data()
    for(nm in c("fresh","post","exams","workload","events")) h[[nm]] <- within_dates(h[[nm]],dates,key)
    v <- combined()$v
    if(nrow(v)) {v$date<-as_date_safely(v[[get_date_col(v)]]);v<-within_dates(v,dates)}
    sp <- vald_sprint();if(is.null(sp))sp<-data.frame()
    if(nrow(sp)) {sp<-sp[health_key(sp$athlete_name)==key,,drop=FALSE];sp$date<-as_date_safely(sp$test_date);sp<-within_dates(sp,dates)}
    correlation_candidates(h,v,sp)
  })
  correlation_kpi <- reactive({
    d <- pitching_series()
    d[d$metric==(input$hc_corr_kpi %||% "fb_velocity") & is.finite(d$value) & d$n>=correlation_integer(input$hc_corr_pitches,5,1,100),,drop=FALSE]
  })
  correlations <- reactive(correlation_screen(correlation_kpi(),correlation_data(),
    min_pairs=correlation_integer(input$hc_corr_min,8,5,100),max_age=correlation_integer(input$hc_corr_age,3,0,3),method=input$hc_corr_method %||% "pearson"))
  correlation_top <- reactive(correlation_view(correlations()$results,input$hc_corr_source %||% "overall"))
  output$hc_corr_status <- renderUI({
    x<-correlations(); coverage<-x$coverage
    max_pairs<-if(nrow(coverage))max(coverage$Pairs) else 0
    tags$div(tags$p(paste(nrow(correlation_kpi()),"eligible pitching dates ·",nrow(coverage),"metrics examined ·",nrow(x$results),"eligible correlations · largest matched sample:",max_pairs,"dates."),
      if(!nrow(x$results)) tags$strong(" No metrics meet the paired-date and variation requirements. See data coverage below.")),
      tags$ul(lapply(c("ArmCare","PULSE","VALD"),function(source){d<-coverage[coverage$Source==source,,drop=FALSE];tags$li(paste(source,":",sum(d$Reason=="Eligible"),"eligible metrics; largest matched sample",if(nrow(d))max(d$Pairs) else 0,"dates."))})))
  })
  output$hc_corr_top <- DT::renderDT({
    d<-correlation_top()
    if(!nrow(d))return(DT::datatable(data.frame(Message="No eligible correlations for this selection."),rownames=FALSE,options=list(dom="t")))
    shown<-data.frame(Rank=seq_len(nrow(d)),Source=d$Source,Metric=d$Metric,r=round(d$Correlation,3),Strength=d$Strength,
      Direction=d$Direction,`Paired dates`=d$Pairs,`FDR q`=round(d$FDR_q,4),check.names=FALSE)
    DT::datatable(shown,rownames=FALSE,selection="single",options=list(dom=if((input$hc_corr_source %||% "overall")=="overall")"t" else "ftp",pageLength=10,scrollX=TRUE,ordering=FALSE))
  })
  # Reset row selection whenever a player, KPI, date or pairing setting changes.
  observeEvent(list(correlations(),input$hc_corr_source),{DT::selectRows(DT::dataTableProxy("hc_corr_top",session=session),NULL)},ignoreInit=TRUE)
  correlation_selected <- reactive({
    d<-correlation_top();if(!nrow(d))return(NULL)
    index<-input$hc_corr_top_rows_selected
    if(length(index)!=1||index<1||index>nrow(d))index<-1L
    d[index,,drop=FALSE]
  })
  output$hc_corr_detail <- renderUI({
    d<-correlation_selected();if(is.null(d))return(NULL)
    unit<-if(grepl("spin",input$hc_corr_kpi %||% "fb_velocity"))"rpm" else "mph"
    tags$div(tags$h4(paste(d$Source,d$Metric,sep=" · ")),
      tags$p(paste("Matched pitching dates:",d$First,"to",d$Last,"·",d$Pairs,"pairs · mean source age",round(d$Mean_age_days,1),"days.")),
      if(identical(input$hc_corr_method %||% "pearson","pearson"))tags$p(paste0("Pearson r = ",round(d$Correlation,3),"; nominal 95% interval [",round(d$CI_low,3),", ",round(d$CI_high,3),"]. Observed linear slope: ",signif(d$Slope,3)," ",unit," per 1 ",d$Unit,". This is an association, not an estimated causal impact. Intervals are not adjusted for selection or time dependence."))
      else tags$p(paste("Spearman rho =",round(d$Correlation,3),"(rank association; no linear effect size inferred).")))
  })
  output$hc_corr_scatter <- plotly::renderPlotly({
    d<-correlation_selected();shiny::validate(shiny::need(!is.null(d),"Select a player with enough paired dates to inspect a relationship."))
    p<-correlations()$pairs[[d$id]]
    p$hover<-paste0("Pitching: ",p$date,"<br>Source: ",p$source_date,"<br>Metric: ",round(p$x,3),"<br>KPI: ",round(p$y,3),"<br>Valid pitches: ",p$pitches)
    chart<-plotly::plot_ly(p,x=~x,y=~y,type="scatter",mode="markers",text=~hover,hoverinfo="text",marker=list(color="#501214"),showlegend=FALSE)
    if(identical(input$hc_corr_method %||% "pearson","pearson")) {
      fit<-stats::lm(y~x,p);grid<-data.frame(x=range(p$x));grid$y<-stats::predict(fit,grid)
      chart<-plotly::add_lines(chart,data=grid,x=~x,y=~y,inherit=FALSE,line=list(color="#a37b00"),name="Observed linear fit",showlegend=FALSE)
    }
    plotly::layout(chart,xaxis=list(title=paste(d$Metric,d$Unit)),yaxis=list(title=unname(trackman_metrics[input$hc_corr_kpi %||% "fb_velocity"])),margin=list(b=90))
  })
  output$hc_corr_coverage <- DT::renderDT({
    d<-correlation_view(correlations()$coverage,input$hc_corr_source %||% "overall",limit_overall=FALSE)
    if(nrow(d))d<-d[,c("Source","Metric","Pairs","Reason")]
    DT::datatable(d,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })
  correlation_export_data <- reactive({
    d<-correlations()$results
    if(nrow(d)){d$player<-input$hc_combined_player;d$kpi<-input$hc_corr_kpi %||% "fb_velocity";d$method<-input$hc_corr_method %||% "pearson";d$matching<-"same_or_earlier";d$max_age_days<-correlation_integer(input$hc_corr_age,3,0,3);d$minimum_pairs<-correlation_integer(input$hc_corr_min,8,5,100);d$minimum_pitches<-correlation_integer(input$hc_corr_pitches,5,1,100);d$window_start<-as.character(input$hc_combined_dates[1]);d$window_end<-as.character(input$hc_combined_dates[2])}
    d
  })
  output$hc_corr_export <- downloadHandler(filename=function()"kpi_correlations.csv",content=function(file){
    write.csv(correlation_export_data(),file,row.names=FALSE,na="")
  })
  output$hc_corr_view_export <- downloadHandler(filename=function()paste0("kpi_correlations_",tolower(input$hc_corr_source %||% "overall"),".csv"),content=function(file){
    d<-correlation_export_data()
    if(nrow(d)) d<-d[d$id %in% correlation_top()$id,,drop=FALSE]
    write.csv(d,file,row.names=FALSE,na="")
  })
  output$hc_corr_pairs_export <- downloadHandler(filename=function()"correlation_paired_dates.csv",content=function(file){
    d<-correlation_selected();p<-if(is.null(d))data.frame(date=as.Date(character()),source_date=as.Date(character()),x=double(),y=double(),age_days=integer(),pitches=integer()) else correlations()$pairs[[d$id]]
    if(nrow(p)){p$player<-input$hc_combined_player;p$metric<-d$Metric;p$unit<-d$Unit;p$kpi<-input$hc_corr_kpi %||% "fb_velocity"}
    write.csv(p,file,row.names=FALSE,na="")
  })
  output$hc_trackman_table <- DT::renderDT({
    d <- pitching_daily()
    if(!nrow(d)) return(health_table(data.frame(Message="No TrackMan pitching data in this range.")))
    table <- health_table(data.frame(Date=d$date,`Pitch group`=d$pitch_group,Pitches=d$pitch_count,
      `Velocity (mph)`=d$velocity_mph,`Velocity n`=d$velocity_n,`Spin (rpm)`=d$spin_rpm,`Spin n`=d$spin_n,check.names=FALSE))
    DT::formatRound(table,c("Pitches","Velocity n","Spin n"),digits=0)
  })
  output$hc_trackman_source <- renderUI({
    t <- trackman_raw()
    tags$div(class="health-note",tags$p(paste("TrackMan:",paste(t$files,collapse=", "),"·",t$duplicate_count,"duplicate pitch IDs removed.")),
      if(length(t$notes)) tags$p(paste(t$notes,collapse=" ")),
      if(length(t$unmatched)) tags$p(paste("Names not matched to fall roster:",paste(t$unmatched,collapse=", "))),
      if(length(t$excluded_tags)) tags$p(paste("Tags outside these KPIs:",paste(t$excluded_tags,collapse=", "))))
  })
  output$hc_load_status <- renderUI({
    errors <- c(fall_roster()$error,data()$errors,trackman_raw()$errors)
    if(length(errors)) tags$p(paste(errors,collapse=" • "))
  })
  output$hc_trackman_export <- downloadHandler(filename=function()"trackman_daily_kpis.csv",
    content=function(file)write.csv(pitching_daily(),file,row.names=FALSE,na=""))
  output$hc_arm_export<-downloadHandler(filename=function()"armcare.csv",content=function(file)write.csv(if(identical(input$hc_arm_view,"exams"))exam_rows() else metrics(),file,row.names=FALSE,na=""))
  output$hc_pulse_export<-downloadHandler(filename=function()"pulse_workload.csv",content=function(file)write.csv(workload(),file,row.names=FALSE,na=""))
  output$hc_event_export<-downloadHandler(filename=function()"pulse_throws.csv",content=function(file)write.csv(events(),file,row.names=FALSE,na=""))
  invisible(list(pitching_daily=pitching_daily, combined_plot_data=combined_plot_data,correlations=correlations,correlation_data=correlation_data))
}
