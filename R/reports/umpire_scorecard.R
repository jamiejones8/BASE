# BASE umpire scorecard. Public method inspiration:
# https://umpscorecards.com/page/info/explainers/{accuracy,consistency,favor}
# TrackMan lacks MLB ABS measurements and base occupancy; do not imply equivalence.
base_umpire_num <- function(d,nm) {
  if(nm %in% names(d)) suppressWarnings(as.numeric(as.character(d[[nm]]))) else rep(NA_real_,nrow(d))
}
base_umpire_chr <- function(d,nm) {
  x <- if(nm %in% names(d)) as.character(d[[nm]]) else rep("",nrow(d)); x[is.na(x)] <- ""; x
}
base_umpire_game_keys <- function(d) {
  uid <- base_umpire_chr(d,"GameUID")
  fallback <- paste(base_umpire_chr(d,"Date"),base_umpire_chr(d,"GameID"),base_umpire_chr(d,"AwayTeam"),base_umpire_chr(d,"HomeTeam"),sep="|")
  for (key in unique(fallback[!nzchar(uid)])) {
    known <- unique(uid[fallback==key & nzchar(uid)])
    if(length(known)==1) uid[fallback==key & !nzchar(uid)] <- known
  }
  ifelse(nzchar(uid),uid,fallback)
}
base_umpire_run_reference <- function() readr::read_csv(base_reference_path("umpire_count_out_run_expectancy.csv"),show_col_types=FALSE)
base_umpire_call_gap <- function(balls,strikes,outs,ref=base_umpire_run_reference()) {
  get <- function(b,s,o) {
    key <- paste(o,b,s)
    ref$expected_runs[match(key,paste(ref$outs,ref$balls,ref$strikes))]
  }
  ball <- get(ifelse(balls==3,4,balls+1),ifelse(balls==3,0,strikes),outs)
  strike <- get(ifelse(strikes==2,0,balls),strikes+1,outs)
  gap <- ball-strike
  gap[!(balls %in% 0:3 & strikes %in% 0:2 & outs %in% 0:2)] <- NA_real_
  # Reject invalid/noisy state estimates rather than reversing the meaning of a call.
  gap[!is.finite(gap) | gap<0] <- NA_real_
  gap
}
base_umpire_consistency <- function(x,y,called) {
  if(length(called)<30 || min(table(factor(called,levels=c(FALSE,TRUE))))<5) return(NA_real_)
  y <- y-2.5
  X <- cbind(1,x,y,x*x,y*y,x*y,x^4,y^4)
  objective <- function(b) {
    eta <- as.vector(X %*% b)
    sum(pmax(eta,0)+log1p(exp(-abs(eta)))-called*eta)+.5*sum(b[-1]^2)
  }
  fit <- tryCatch(stats::optim(c(3,0,0,-2,-2,0,-1,-1),objective,method="BFGS",control=list(maxit=500)),error=function(e) NULL)
  if(is.null(fit) || fit$convergence!=0) return(NA_real_)
  mean((as.vector(X %*% fit$par)>=0)==called)
}
base_umpire_scorecard_data <- function(d,ref=base_umpire_run_reference()) {
  d <- as.data.frame(d)
  uid <- base_umpire_chr(d,"PitchUID")
  d <- d[!duplicated(ifelse(nzchar(uid),uid,paste0("row:",seq_len(nrow(d))))),,drop=FALSE]
  d <- d[order(base_umpire_num(d,"PitchNo"),na.last=TRUE),,drop=FALSE]
  run_column <- names(d)[tolower(gsub("[^a-z]","",tolower(names(d)))) %in% c("runsscoredonplay","runsscored")]
  if("RunsScoredOnPlay" %in% run_column) run_column <- c("RunsScoredOnPlay",run_column)
  raw_runs <- if(length(run_column)) base_umpire_chr(d,run_column[1]) else rep(NA_character_,nrow(d))
  runs <- suppressWarnings(as.numeric(raw_runs))
  if(length(run_column)) runs[!nzchar(trimws(raw_runs))] <- 0
  half_all <- tolower(trimws(base_umpire_chr(d,"Top/Bottom")))
  score_ok <- length(run_column)>0 && all(is.finite(runs) & runs>=0) && all(half_all %in% c("top","bottom"))
  score <- c(away=if(score_ok) sum(runs[half_all=="top"]) else NA_real_,
             home=if(score_ok) sum(runs[half_all=="bottom"]) else NA_real_)
  d$.later_runs <- NA_real_
  innings <- paste(base_umpire_chr(d,"Inning"),half_all)
  for(key in unique(innings)) {
    idx <- which(innings==key)
    if(all(is.finite(runs[idx]))) d$.later_runs[idx] <- rev(cumsum(rev(runs[idx])))-runs[idx]
  }
  home_code <- base_umpire_chr(d,"HomeTeam"); away_code <- base_umpire_chr(d,"AwayTeam")
  focus_home <- !any(base_team_matches(away_code)) || any(base_team_matches(home_code))
  call <- base_umpire_chr(d,"PitchCall")
  taken <- call %in% c("BallCalled","StrikeCalled")
  x <- base_umpire_num(d,"PlateLocSide"); y <- base_umpire_num(d,"PlateLocHeight")
  loc <- is.finite(x)&is.finite(y)
  p <- d[taken&loc,,drop=FALSE]
  p$.x <- x[taken&loc]; p$.y <- y[taken&loc]
  p$.called_strike <- base_umpire_chr(p,"PitchCall")=="StrikeCalled"
  p$.true_strike <- base_pitch_in_zone(p$.x,p$.y)
  p$.correct <- p$.called_strike==p$.true_strike
  p$.gap <- base_umpire_call_gap(base_umpire_num(p,"Balls"),base_umpire_num(p,"Strikes"),base_umpire_num(p,"Outs"),ref)
  p$.offense_runs <- ifelse(p$.correct,0,ifelse(p$.called_strike,-p$.gap,p$.gap))
  half <- tolower(base_umpire_chr(p,"Top/Bottom"))
  p$.home_runs <- ifelse(half=="bottom",p$.offense_runs,ifelse(half=="top",-p$.offense_runs,NA_real_))
  home_benefit <- ifelse(half=="bottom",!p$.called_strike,ifelse(half=="top",p$.called_strike,NA))
  p$.favor_side <- ifelse(is.na(home_benefit),"Unknown side",ifelse(home_benefit==focus_home,"In favor","Against"))
  p$.miss_type <- ifelse(p$.called_strike,"Ball called strike","Strike called ball")
  misses <- p[!p$.correct,,drop=FALSE]
  ranked <- misses[order(-abs(misses$.home_runs),na.last=TRUE),,drop=FALSE]
  ranked$.rank <- seq_len(nrow(ranked))
  metric <- function(sel) if(any(sel)) mean(p$.correct[sel]) else NA_real_
  valid_favor <- !nrow(misses) || all(is.finite(misses$.home_runs))
  list(all=d,pitches=p,misses=ranked,score=score,focus_home=focus_home,total=nrow(d),taken=sum(taken),excluded=sum(taken&!loc),
    accuracy=if(nrow(p)) mean(p$.correct) else NA_real_,
    consistency=base_umpire_consistency(p$.x,p$.y,p$.called_strike),
    ball_accuracy=metric(!p$.called_strike),strike_accuracy=metric(p$.called_strike),
    favor=if(nrow(p) && valid_favor) sum(misses$.home_runs) else NA_real_,
    favor_coverage=sum(is.finite(misses$.home_runs)),correct=sum(p$.correct))
}
base_umpire_card_plot <- function(card) {
  first <- function(nm) {x<-base_umpire_chr(card$all,nm); x<-x[nzchar(x)];if(length(x)) x[1] else "Unknown"}
  home <- base_team_display_name(first("HomeTeam")); away <- base_team_display_name(first("AwayTeam"))
  score <- if(all(is.finite(card$score))) sprintf(" | %s - %s",card$score["away"],card$score["home"]) else " | Score unavailable"
  if(home==away) {home<-paste(home,"Home");away<-paste(away,"Away")}
  fmt <- function(x) if(is.finite(x)) sprintf("%.0f%%",100*x) else "N/A"
  ring <- function(value,title,subtitle) {
    a <- seq(0,2*pi,length.out=301)
    bg <- data.frame(x=sin(a),y=cos(a))
    fg <- if(is.finite(value)) head(bg,max(1,round(value*300)+1)) else bg[0,]
    ggplot2::ggplot()+ggplot2::geom_path(data=bg,ggplot2::aes(x,y),linewidth=6,color="#e6e2dc")+
      ggplot2::geom_path(data=fg,ggplot2::aes(x,y),linewidth=6,color="#501214")+
      ggplot2::annotate("text",x=0,y=0,label=fmt(value),size=8,fontface="bold")+
      ggplot2::coord_fixed(xlim=c(-1.3,1.3),ylim=c(-1.35,1.4))+ggplot2::theme_void()+
      ggplot2::labs(title=title,caption=subtitle)+ggplot2::theme(plot.title=ggplot2::element_text(hjust=.5,size=15,face="bold"),plot.caption=ggplot2::element_text(hjust=.5,size=9))
  }
  z <- base_pitch_zone(); missed <- card$misses
  focus <- if(card$focus_home) home else away
  zone_plot <- function(type,title) {
    points <- missed[missed$.miss_type==type,,drop=FALSE]
    plate <- data.frame(x=c(-8.5/12,8.5/12,8.5/12,0,-8.5/12),y=c(.8,.8,.5,.2,.5))
    ggplot2::ggplot(points,ggplot2::aes(.x,.y))+
      ggplot2::annotate("rect",xmin=z$xmin,xmax=z$xmax,ymin=z$ymin,ymax=z$ymax,fill=NA,color="#282828")+
      ggplot2::geom_polygon(data=plate,ggplot2::aes(x,y),inherit.aes=FALSE,fill="white",color="#282828")+
      ggplot2::geom_point(ggplot2::aes(color=.favor_side),size=4,alpha=.75)+
      ggplot2::scale_color_manual(values=c("In favor"="#58ad63","Against"="#c84b49","Unknown side"="#888888"),breaks=c("In favor","Against","Unknown side"),drop=FALSE)+
      ggplot2::coord_fixed(xlim=range(c(-1.4,1.4,missed$.x),na.rm=TRUE)+c(-.15,.15),ylim=range(c(.05,4,missed$.y),na.rm=TRUE)+c(-.05,.15))+
      ggplot2::theme_void()+ggplot2::labs(title=title,subtitle=paste(nrow(points),"missed calls"),color=focus)+
      ggplot2::theme(plot.title=ggplot2::element_text(size=14,face="bold",hjust=.5),plot.subtitle=ggplot2::element_text(hjust=.5,size=10),legend.position="bottom",legend.text=ggplot2::element_text(size=8),legend.title=ggplot2::element_text(size=8))
  }
  zone <- zone_plot("Ball called strike","Balls Called Strikes")
  zone2 <- zone_plot("Strike called ball","Strikes Called Balls")
  impact <- head(missed[is.finite(missed$.home_runs),,drop=FALSE],3)
  details <- if(!nrow(impact)) "No rated missed calls." else vapply(seq_len(nrow(impact)),function(i) {
    r<-impact[i,,drop=FALSE]
    name <- function(nm) base_umpire_chr(r,nm)[1]
    txt<-sprintf("%s. %s %s | Pitch %s\n%s to %s\n%s outs | %s-%s count\n%s\nEstimated impact: %.2f runs\nRuns later in half: %s",i,name("Top/Bottom"),name("Inning"),name("PitchNo"),name("Pitcher"),name("Batter"),name("Outs"),name("Balls"),name("Strikes"),r$.miss_type,abs(r$.home_runs),ifelse(is.finite(r$.later_runs),r$.later_runs,"N/A"))
    paste(vapply(strsplit(txt,"\n")[[1]],function(t) paste(strwrap(t,width=35),collapse="\n"),character(1)),collapse="\n")
  },character(1))
  impact_plot <- cowplot::ggdraw()+cowplot::draw_label("Impactful Missed Calls",x=.02,y=.97,hjust=0,vjust=1,size=13,fontface="bold")
  for(i in seq_along(details)) impact_plot <- impact_plot+cowplot::draw_label(details[i],x=.02+(i-1)*.33,y=.79,hjust=0,vjust=1,size=8)
  favor_label <- if(is.finite(card$favor)) sprintf("+%.2f",abs(card$favor)) else "N/A"
  favor_team <- if(!is.finite(card$favor)) "Incomplete call inputs" else if(abs(card$favor)<.005) "Even" else paste("runs for",if(card$favor>0) home else away)
  favors <- cowplot::ggdraw()+cowplot::draw_label("Estimated Favor",x=.5,y=.9,size=15,fontface="bold")+
    cowplot::draw_label(favor_label,x=.5,y=.55,size=30,color="#ac9155",fontface="bold")+
    cowplot::draw_label(favor_team,x=.5,y=.30,size=11,color="#501214")
  bars <- data.frame(label=c("Accuracy on called balls","Accuracy on called strikes"),value=c(card$ball_accuracy,card$strike_accuracy),color=c("#58ad63","#c84b49"),y=c(2,1))
  bp <- ggplot2::ggplot(bars,ggplot2::aes(y=y))+
    ggplot2::geom_segment(ggplot2::aes(x=0,xend=1,yend=y),linewidth=6,color="#e4e4e4")+
    ggplot2::geom_segment(ggplot2::aes(x=0,xend=value,yend=y,color=color),linewidth=6,na.rm=TRUE)+ggplot2::scale_color_identity()+
    ggplot2::geom_text(ggplot2::aes(x=0,y=y+.43,label=label),hjust=0,size=4,fontface="bold")+
    ggplot2::geom_text(ggplot2::aes(x=1.04,label=vapply(value,fmt,character(1))),hjust=0,size=5,fontface="bold")+
    ggplot2::coord_cartesian(xlim=c(0,1.22),ylim=c(.6,2.8),clip="off")+ggplot2::theme_void()
  foot <- paste0(card$total," game pitches; ",card$taken," called takes. ",card$excluded," taken pitches excluded: missing location.\n",
    "Favor: D1 count/out estimate; runner occupancy unavailable. Rated misses: ",card$favor_coverage,"/",nrow(missed),".\n",
    "Later runs are observed context, not runs attributed to the call. BASE zone and fitted consistency model.")
  cowplot::ggdraw()+cowplot::draw_label(paste(away,"vs.",home),x=.5,y=.96,size=19,fontface="bold")+
    cowplot::draw_label(paste0("Umpire Scorecard | ",first("Date"),score),x=.5,y=.92,size=15)+
    cowplot::draw_plot(ring(card$accuracy,"Overall Accuracy",sprintf("%s of %s located takes correct",card$correct,nrow(card$pitches))),x=.04,y=.66,width=.31,height=.22)+
    cowplot::draw_plot(favors,x=.35,y=.66,width=.30,height=.22)+
    cowplot::draw_plot(ring(card$consistency,"Overall Consistency","Consistency with fitted call zone"),x=.65,y=.66,width=.31,height=.22)+
    cowplot::draw_plot(zone,x=.03,y=.32,width=.47,height=.34)+cowplot::draw_plot(zone2,x=.52,y=.32,width=.46,height=.34)+
    cowplot::draw_plot(impact_plot,x=.04,y=.15,width=.92,height=.17)+
    cowplot::draw_plot(bp,x=.08,y=.06,width=.84,height=.10)+cowplot::draw_label(foot,x=.04,y=.055,hjust=0,vjust=1,size=7.5)
}
base_umpire_report_ui <- function() shiny::div(class="base-report-builder",
  shiny::div(class="base-report-controls",shiny::selectInput("umpire_game","Game",choices=NULL),shiny::downloadButton("umpire_pdf","Download Scorecard PDF")),
  shiny::plotOutput("umpire_scorecard",height="1050px"),
  shiny::h4("Every missed call"),shiny::tableOutput("umpire_misses"))
base_umpire_report_server <- function(input,output,session) {
  signature <- function() {p<-base_pitching_supplement_paths();i<-file.info(p);paste(p,i$size,as.numeric(i$mtime),collapse="|")}
  rows <- shiny::reactivePoll(2000,session,signature,function() {
    frames <- lapply(base_pitching_supplement_paths(),function(path) {
      d <- base_read_pitching_source(path,full_game=TRUE)
      d[] <- lapply(d,as.character)
      d
    })
    d <- dplyr::bind_rows(frames)
    if(nrow(d)) {
      d <- d[!(grepl("bullpen",d$source_file,ignore.case=TRUE)),,drop=FALSE]
      d$.game <- base_umpire_game_keys(d)
      team_game <- base_team_matches(base_umpire_chr(d,"HomeTeam")) | base_team_matches(base_umpire_chr(d,"AwayTeam")) |
        base_team_matches(base_umpire_chr(d,"PitcherTeam")) | base_team_matches(base_umpire_chr(d,"BatterTeam"))
      d <- d[d$.game %in% unique(d$.game[team_game]),,drop=FALSE]
    }
    d
  })
  shiny::observeEvent(rows(), {
    d<-rows();shiny::req(nrow(d))
    games<-d[!duplicated(d$.game),,drop=FALSE]
    games<-games[order(as.character(games$Date),decreasing=TRUE),,drop=FALSE]
    labels<-paste(games$Date,base_umpire_chr(games,"AwayTeam"),"at",base_umpire_chr(games,"HomeTeam"))
    selected<-shiny::isolate(input$umpire_game)
    if(is.null(selected)||!selected %in% games$.game) selected<-games$.game[1]
    shiny::updateSelectInput(session,"umpire_game",choices=stats::setNames(games$.game,labels),selected=selected)
  })
  card <- shiny::reactive({shiny::req(input$umpire_game);d<-rows();d<-d[d$.game==input$umpire_game,,drop=FALSE];shiny::req(nrow(d));base_umpire_scorecard_data(d)})
  output$umpire_misses <- shiny::renderTable({
    m <- card()$misses
    data.frame(Pitch=base_umpire_chr(m,"PitchNo"),Inning=paste(base_umpire_chr(m,"Top/Bottom"),base_umpire_chr(m,"Inning")),
      Pitcher=base_umpire_chr(m,"Pitcher"),Batter=base_umpire_chr(m,"Batter"),
      Count=paste0(base_umpire_chr(m,"Balls"),"-",base_umpire_chr(m,"Strikes")),Outs=base_umpire_chr(m,"Outs"),
      Call=m$.miss_type,Favor=m$.favor_side,`Runs later in half`=m$.later_runs,check.names=FALSE)
  })
  output$umpire_scorecard <- shiny::renderPlot(base_umpire_card_plot(card()),res=120)
  output$umpire_pdf <- shiny::downloadHandler(filename=function() "umpire-scorecard.pdf",content=function(file) {
    grDevices::pdf(file,width=8.5,height=11,useDingbats=FALSE);on.exit(grDevices::dev.off(),add=TRUE)
    print(base_umpire_card_plot(card()))
  })
}
