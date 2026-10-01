# Definitions only: Shiny may source this file before the other R helpers.
combined_arm_choices <- c("Arm Score"="score", "Total strength · lbs + %BW"="total",
  "Internal rotation · lbs + %BW"="ir", "External rotation · lbs + %BW"="er",
  "Scaption · lbs + %BW"="scaption", "Grip · lbs + %BW"="grip")
combined_pulse_choices <- c("ACWR"="A:C Ratio", "Acute workload"="Acute Workload",
  "Chronic workload"="Chronic Workload", "Daily workload"="One Day Workload",
  "Arm speed"="armSpeed", "Arm slot"="armSlot", "Shoulder rotation"="shoulderRotation", "Torque"="torque")
combined_default_vald <- "fd|CMJ|JUMP_HEIGHT_IMP_MOM|Centimeter"
trend_empty <- function() data.frame(date=as.Date(character()),value=double(),label=character(),unit=character(),axis=character(),n=integer())
trend_rows <- function(date,value,label,unit,axis="y",n=1L) {
  data.frame(date=as.Date(date),value=suppressWarnings(as.numeric(value)),label=rep(label,length(date)),
    unit=rep(unit,length(date)),axis=rep(axis,length(date)),n=rep_len(n,length(date)))
}
combined_arm_series <- function(d,metric="score") {
  if(!nrow(d)) return(trend_empty())
  col <- function(nm) if(nm %in% names(d)) health_num(d[[nm]]) else rep(NA_real_,nrow(d))
  if(metric=="score") return(trend_rows(d$date,col("Arm Score"),"Arm Score","score"))
  prefixes <- c(ir="IRTARM",er="ERTARM",scaption="STARM",grip="GTARM")
  if(metric=="total") {
    raw <- col("Total Strength"); weight <- col("Weight (lbs)")
    relative <- ifelse(is.finite(weight)&weight>0,100*raw/weight,NA_real_)
    label <- "Total strength"; relative_label <- "Total strength (%BW, calculated)"
  } else {
    if(!metric %in% names(prefixes)) return(trend_empty())
    raw <- col(paste(prefixes[[metric]],"Strength"))
    # Workbook key-metrics exports may omit the raw field; never infer PSI.
    relative <- 100*col(paste(prefixes[[metric]],"RS"))
    label <- c(ir="Internal rotation",er="External rotation",scaption="Scaption",grip="Grip")[[metric]]
    relative_label <- paste(label,"(%BW)")
  }
  rbind(trend_rows(d$date,raw,paste(label,"(lbs)"),"lbs"),
    trend_rows(d$date,relative,relative_label,"%BW","y2"))
}
combined_pulse_series <- function(workload,events,metric="A:C Ratio") {
  event_units <- c(armSpeed="°/s",armSlot="°",shoulderRotation="°",torque="Nm")
  label <- names(combined_pulse_choices)[match(metric,combined_pulse_choices)]
  if(is.na(label)) return(trend_empty())
  if(metric %in% names(event_units)) {
    if(!nrow(events)||!metric %in% names(events)) return(trend_empty())
    d <- data.frame(date=events$date,value=health_num(events[[metric]]))
    d <- d[!is.na(d$date),,drop=FALSE]
    groups <- split(d,d$date)
    if(!length(groups)) return(trend_empty())
    return(do.call(rbind,lapply(groups,function(x) {
      valid <- is.finite(x$value)
      trend_rows(x$date[1],if(any(valid))mean(x$value[valid]) else NA_real_,paste(label,"(daily mean)"),event_units[[metric]],n=sum(valid))
    })))
  }
  if(!nrow(workload)||!metric %in% names(workload)) return(trend_empty())
  trend_rows(workload$date,workload[[metric]],label,if(metric=="A:C Ratio")"ratio" else "workload")
}
combined_vald_catalog <- function(summary,sprint) {
  out <- data.frame(id=character(),label=character(),source=character(),test=character(),metric=character(),unit=character())
  if(is.data.frame(summary) && nrow(summary)>0 && all(c("testType","metricKey","metricName","metricUnit") %in% names(summary))) {
    d <- unique(summary[,c("testType","metricKey","metricName","metricUnit")])
    d$metricUnit[is.na(d$metricUnit)] <- "No Unit"
    d <- d[!is.na(d$metricKey)&!is.na(d$testType),]
    out <- rbind(out,data.frame(id=paste("fd",d$testType,d$metricKey,d$metricUnit,sep="|"),
      label=paste0(d$testType," · ",d$metricName," (",d$metricUnit,")"),source="ForceDecks",test=d$testType,metric=d$metricKey,unit=d$metricUnit))
  }
  if(is.data.frame(sprint) && nrow(sprint)>0 && "test_name" %in% names(sprint)) {
    # Include every numeric measurement in the imported sprint table, including
    # future added metrics, but never mix different sprint protocols in a trace.
    fields <- names(sprint)[vapply(sprint,is.numeric,logical(1))]
    fields <- setdiff(fields,c("athlete_id","vald_profile_id","raw_test_id"))
    units <- c(best_10yd="s",best_30yd="s",best_flying_10yd="s",mean_10yd="s",cv_10yd="ratio",max_velocity="m/s",total_time="s",n_reps="count",valid_reps="count")
    for(test in sort(unique(stats::na.omit(sprint$test_name)))) for(m in fields) {
      if(!any(is.finite(sprint[[m]][which(sprint$test_name==test)]))) next
      unit <- if(m %in% names(units)) units[[m]] else "source units"
      label <- paste(test,"·",gsub("_"," ",m),paste0("(",unit,")"))
      out <- rbind(out,data.frame(id=paste("sprint",test,m,sep="|"),label=label,source="SmartSpeed",test=test,metric=m,unit=unit))
    }
  }
  out <- out[!duplicated(out$id),]
  out[order(out$source,out$test,out$label),]
}
combined_vald_series <- function(summary,sprint,catalog,id=combined_default_vald) {
  choice <- catalog[catalog$id==id,,drop=FALSE]
  if(nrow(choice)!=1) return(trend_empty())
  if(choice$source=="ForceDecks") {
    if(!nrow(summary)) return(trend_empty())
    d <- summary[summary$testType==choice$test & summary$metricKey==choice$metric & summary$metricUnit==choice$unit,,drop=FALSE]
    bc <- get_best_col(d); dc <- get_date_col(d)
    if(!nrow(d)||is.null(bc)||is.null(dc)) return(trend_empty())
    return(trend_rows(as_date_safely(d[[dc]]),d[[bc]],choice$label,choice$unit,
      n=if("n_trials" %in% names(d))d$n_trials else 1L))
  }
  if(!nrow(sprint)||!choice$metric %in% names(sprint)) return(trend_empty())
  d <- sprint[sprint$test_name==choice$test,,drop=FALSE]
  trend_rows(as_date_safely(d$test_date),d[[choice$metric]],choice$label,choice$unit)
}
combined_trend_plot <- function(d,dates,title) {
  d <- d[!is.na(d$date),,drop=FALSE]
  shiny::validate(shiny::need(nrow(d)>0&&any(is.finite(d$value)),"No measurements for this player, metric and date range."))
  p <- plotly::plot_ly(); labels <- unique(d$label)
  for(i in seq_along(labels)) {
    rows <- d[d$label==labels[i],,drop=FALSE];rows <- rows[order(rows$date),]
    valid <- is.finite(rows$value); segment <- cumsum(!valid)
    parts <- split(rows[valid,,drop=FALSE],segment[valid])
    for(j in seq_along(parts)) {
      x <- parts[[j]]
      x$hover <- paste0(x$date,"<br>",x$label,": ",round(x$value,3)," ",x$unit,"<br>Measurements: ",x$n)
      p <- plotly::add_trace(p,data=x,x=~date,y=~value,type="scatter",mode="lines+markers",
        name=labels[i],legendgroup=labels[i],showlegend=length(labels)>1&&j==1,
        text=~hover,hoverinfo="text",yaxis=x$axis[1],line=list(color=c("#501214","#a37b00")[(i-1)%%2+1]),
        marker=list(color=c("#501214","#a37b00")[(i-1)%%2+1]),inherit=FALSE)
    }
  }
  left <- unique(d$unit[d$axis=="y"]);right <- unique(d$unit[d$axis=="y2"])
  args <- list(p,title=list(text=title,font=list(size=14)),xaxis=list(title="",type="date",range=if(length(dates)==2)as.character(dates) else NULL),
    yaxis=list(title=paste(left,collapse=", ")),margin=list(l=65,r=70,t=45,b=55),hovermode="x",
    legend=list(orientation="h",y=-.25),showlegend=length(labels)>1)
  if(length(right)) args$yaxis2 <- list(title=paste(right,collapse=", "),overlaying="y",side="right",showgrid=FALSE)
  do.call(plotly::layout,args)
}
combined_trend_ui <- function() {
  row <- function(number,title,control,plot,note) tags$div(class="combined-trend-row",
    tags$aside(class="combined-trend-control",tags$h4(paste(number,title,sep=" · ")),control,tags$p(class="health-note",note)),
    tags$div(class="combined-trend-chart",plotly::plotlyOutput(plot,height="320px")))
  tags$div(class="combined-trends",
    row(1,"TrackMan",selectInput("hc_trackman_metric","Pitching metric",choices=setNames(names(trackman_metrics),trackman_metrics),selected="fb_velocity"),"hc_trackman_trend","Daily averages of tagged pitches. Fastball/sinker velocity is the primary pitching KPI."),
    row(2,"VALD",selectizeInput("hc_vald_metric","Jump or sprint metric",choices=c("CMJ · Jump Height (Imp-Mom) (Centimeter)"=combined_default_vald),selected=combined_default_vald,options=list(maxOptions=1000)),"hc_vald_trend","Search all imported metrics by name. Jump metrics use the stored session best; sprint points retain individual test results. Test types and source units stay separate."),
    row(3,"ArmCare",selectInput("hc_arm_metric","ArmCare metric",choices=combined_arm_choices,selected="score"),"hc_armcare_trend","Fresh exams. Strength selections show lbs on the left axis and % body weight on the right. Total %BW is calculated; other %BW values are exported."),
    row(4,"PULSE",selectInput("hc_pulse_metric","PULSE metric",choices=combined_pulse_choices,selected="A:C Ratio"),"hc_pulse_trend","ACWR and workloads retain PULSE values. Throw metrics use daily means across exported throws, including simulated throws."))
}
