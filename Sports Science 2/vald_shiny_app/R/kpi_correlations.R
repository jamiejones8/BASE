# Within-player exploratory screening. No athlete pooling; date matching is explicit.
correlation_integer <- function(x,default,lower,upper) {
  x <- suppressWarnings(as.numeric(x))
  if(length(x)!=1L || !is.finite(x)) return(as.integer(default))
  as.integer(max(lower,min(upper,x)))
}
correlation_candidates <- function(health,summary,sprint) {
  parts <- list()
  add <- function(d,source,id) {
    if(!nrow(d)) return(invisible(NULL))
    for(label in unique(d$label)) {
      x <- d[d$label==label,,drop=FALSE]
      x$id <- paste(source,id,label,sep="|");x$source <- source
      parts[[length(parts)+1L]] <<- x
    }
  }
  for(m in unname(combined_arm_choices)) add(combined_arm_series(health$fresh,m),"ArmCare",m)
  # Additional exported exam measurements, kept separate by exam type and unit.
  for(kind in c("fresh","post","exams")) {
    d <- health[[kind]]
    if(!nrow(d)) next
    fields <- if(kind=="fresh") intersect(c("Shoulder Balance","SVR","Velo"),names(d)) else if(kind=="post")
      grep("Strength|Loss|%Fresh",names(d),value=TRUE) else grep("ROM$|TARC$|Total Primer Max-Lbs",names(d),value=TRUE)
    for(m in fields) {
      unit <- if(grepl("ROM$|TARC$",m))"degrees" else if(grepl("%Fresh",m))"%" else if(m=="Velo")"mph" else if(m %in% c("Shoulder Balance","SVR"))"ratio" else "lbs"
      types <- if("Exam Type" %in% names(d)) unique(as.character(d[["Exam Type"]])) else kind
      for(type in stats::na.omit(types)) {
        x <- if("Exam Type" %in% names(d))d[d[["Exam Type"]]==type,,drop=FALSE] else d
        add(trend_rows(x$date,x[[m]],paste(type,m),unit),"ArmCare",paste(kind,type,m))
      }
    }
  }
  for(m in unname(combined_pulse_choices)) add(combined_pulse_series(health$workload,health$events,m),"PULSE",m)
  catalog <- combined_vald_catalog(summary,sprint)
  # Counts are data coverage, not performance measurements.
  catalog <- catalog[!catalog$metric %in% c("n_reps","valid_reps"),,drop=FALSE]
  for(id in catalog$id) add(combined_vald_series(summary,sprint,catalog,id),"VALD",id)
  if(!length(parts)) return(data.frame())
  d <- dplyr::bind_rows(parts)
  d <- d[!is.na(d$date)&is.finite(d$value),,drop=FALSE]
  # One daily observation per metric; multiple exams/tests cannot multiply n.
  d |>
    dplyr::group_by(id,source,label,unit,date) |>
    dplyr::summarise(value=mean(value),measurements=dplyr::n(),.groups="drop")
}
correlation_pairs <- function(kpi,predictor,max_age=3L) {
  max_age <- correlation_integer(max_age,3,0,3)
  empty <- data.frame(date=as.Date(character()),source_date=as.Date(character()),x=double(),y=double(),age_days=integer(),pitches=integer())
  kpi <- kpi[is.finite(kpi$value)&!is.na(kpi$date),,drop=FALSE]
  predictor <- predictor[is.finite(predictor$value)&!is.na(predictor$date),,drop=FALSE]
  if(!nrow(kpi)||!nrow(predictor)) return(empty)
  kpi <- kpi[order(kpi$date),,drop=FALSE];predictor <- predictor[order(predictor$date),,drop=FALSE]
  stopifnot(!anyDuplicated(kpi$date),!anyDuplicated(predictor$date))
  edges <- expand.grid(target=seq_len(nrow(kpi)),source=seq_len(nrow(predictor)))
  edges$age <- as.integer(kpi$date[edges$target]-predictor$date[edges$source])
  edges <- edges[edges$age<=max_age & edges$age>=0,,drop=FALSE]
  if(!nrow(edges)) return(empty)
  # Greedy matching: smallest source age first, then earlier pitching date.
  edges <- edges[order(edges$age,kpi$date[edges$target]),,drop=FALSE]
  used_target <- rep(FALSE,nrow(kpi));used_source <- rep(FALSE,nrow(predictor))
  keep <- logical(nrow(edges))
  for(i in seq_len(nrow(edges))) {
    t <- edges$target[i];s <- edges$source[i]
    if(!used_target[t]&&!used_source[s]) {keep[i]<-TRUE;used_target[t]<-TRUE;used_source[s]<-TRUE}
  }
  edges <- edges[keep,,drop=FALSE]
  pairs <- data.frame(date=kpi$date[edges$target],source_date=predictor$date[edges$source],
    x=predictor$value[edges$source],y=kpi$value[edges$target],age_days=edges$age,pitches=kpi$n[edges$target])
  pairs[order(pairs$date),,drop=FALSE]
}
correlation_screen <- function(kpi,candidates,min_pairs=8L,max_age=3L,method="pearson") {
  stopifnot(method %in% c("pearson","spearman"),min_pairs>=5,max_age>=0)
  out <- list(results=data.frame(),coverage=data.frame(),pairs=list())
  if(!nrow(candidates)) return(out)
  rows <- list();coverage <- list()
  for(id in unique(candidates$id)) {
    d <- candidates[candidates$id==id,,drop=FALSE]
    p <- correlation_pairs(kpi,d,max_age)
    reason <- if(nrow(p)<min_pairs)"Too few paired dates" else if(stats::sd(p$x)==0||stats::sd(p$y)==0)"No variation" else "Eligible"
    coverage[[length(coverage)+1L]] <- data.frame(id=id,Source=d$source[1],Metric=d$label[1],Pairs=nrow(p),Reason=reason)
    if(reason!="Eligible") next
    test <- suppressWarnings(stats::cor.test(p$x,p$y,method=method,exact=FALSE))
    r <- unname(test$estimate)
    if(!is.finite(r)) next
    slope <- if(method=="pearson")unname(stats::coef(stats::lm(y~x,p))[2]) else NA_real_
    ci <- if(method=="pearson")unname(test$conf.int) else c(NA_real_,NA_real_)
    rows[[length(rows)+1L]] <- data.frame(id=id,Source=d$source[1],Metric=d$label[1],Unit=d$unit[1],Correlation=r,
      Strength=if(abs(r)<.3)"Weak" else if(abs(r)<.5)"Moderate" else if(abs(r)<.7)"Strong" else "Very strong",
      Direction=if(r>0)"Higher with higher KPI" else if(r<0)"Higher with lower KPI" else "None",
      Pairs=nrow(p),First=min(p$date),Last=max(p$date),CI_low=ci[1],CI_high=ci[2],P_value=test$p.value,
      Slope=slope,Mean_age_days=mean(p$age_days))
    out$pairs[[id]] <- p
  }
  out$coverage <- dplyr::bind_rows(coverage)
  out$results <- dplyr::bind_rows(rows)
  if(nrow(out$results)) {
    # Adjustment covers every eligible test, before selecting the top 10.
    out$results$FDR_q <- stats::p.adjust(out$results$P_value,method="BH")
    out$results <- out$results[order(-abs(out$results$Correlation),-out$results$Pairs,out$results$id),]
  }
  out
}
correlation_view <- function(results,source="overall",limit_overall=TRUE) {
  if(!nrow(results)) return(results)
  if(source %in% c("VALD","ArmCare","PULSE")) return(results[results$Source==source,,drop=FALSE])
  if(limit_overall) head(results,10) else results
}
correlation_ui <- function() {
  tags$div(
    tags$h3("KPI Correlations"),
    tags$p("Within-player associations across VALD, PULSE and ArmCare. Uses the player and dates selected above."),
    fluidRow(column(4,selectInput("hc_corr_kpi","TrackMan KPI",choices=setNames(names(trackman_metrics),trackman_metrics),selected="fb_velocity")),
      column(3,selectInput("hc_corr_method","Correlation",c("Pearson (linear)"="pearson","Spearman (rank)"="spearman"))),
      column(3,selectInput("hc_corr_age","Match source measurements",c("Same date or up to 3 days earlier"="3","Same date only"="0"),selected="3")),
      column(2,numericInput("hc_corr_min","Minimum paired dates",8,min=5,max=100,step=1))),
    fluidRow(column(3,numericInput("hc_corr_pitches","Minimum valid pitches per KPI date",5,min=1,max=100,step=1)),
      column(4,downloadButton("hc_corr_export","Export all eligible correlations"))),
    tags$p(class="health-note","Ranked by absolute correlation, so both positive and negative relationships appear. These are exploratory associations, not causal effects or training prescriptions. Many metrics are screened; extreme correlations can occur by chance. Related metrics and shared time trends can dominate the list."),
    uiOutput("hc_corr_status"),
    navset_tab(id="hc_corr_source",selected="overall",
      nav_panel("Overall",value="overall",tags$p("Top 10 eligible correlations across all sources.")),
      nav_panel("VALD",value="VALD",tags$p("All eligible VALD correlations, ranked by absolute correlation.")),
      nav_panel("ArmCare",value="ArmCare",tags$p("All eligible ArmCare correlations, ranked by absolute correlation.")),
      nav_panel("PULSE",value="PULSE",tags$p("All eligible PULSE correlations, ranked by absolute correlation."))),
    DT::DTOutput("hc_corr_top"),
    downloadButton("hc_corr_view_export","Export current tab"),
    tags$p(class="health-note","Strength labels describe |r| only: weak <0.30, moderate 0.30–0.49, strong 0.50–0.69, very strong ≥0.70. The minimum sample is a screening setting, not proof of reliability. Q-values adjust across all eligible metrics; repeated observations over time may violate the tests’ independence assumption."),
    uiOutput("hc_corr_detail"),plotly::plotlyOutput("hc_corr_scatter",height="360px"),
    downloadButton("hc_corr_pairs_export","Export selected paired dates"),
    tags$details(tags$summary("Pairing and data coverage"),
      tags$p("One value per metric per date: repeated exams or tests are averaged; PULSE throw metrics use their daily means. Pitching KPIs use daily pitch averages. No missing data is filled. Matching uses the pitching date or up to three calendar days earlier, never a later date. Closest dates take priority. Each pitching date and source date is used once per metric. Same-day records are matched by date, so they may include measurements after pitching on that same day; this is not a strictly pre-appearance analysis."),
      DT::DTOutput("hc_corr_coverage")))
}
