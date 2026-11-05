# Game-ordered season-to-date values use the owning app's stat summarizer.
development_goal_target <- function(goal,metric,override=NA_real_) {
  percent<-grepl("%",metric,fixed=TRUE)
  if(length(override)==1 && is.finite(override)) value<-if(percent) override/100 else override
  else {
    number<-regmatches(goal,gregexpr("[-+]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)",goal,perl=TRUE))[[1]]
    if(length(number)!=1) return(NA_real_)
    value<-as.numeric(number)
    if(percent && (grepl("%",goal,fixed=TRUE) || abs(value)>1)) value<-value/100
  }
  if(percent && (value<0 || value>1)) NA_real_ else value
}

development_pa_order <- function(d,completed_only=TRUE) {
  if(!nrow(d) || !all(c("Inning","PAofInning") %in% names(d))) return(d[0,,drop=FALSE])
  chr<-function(col) {
    value<-if(col %in% names(d)) trimws(as.character(d[[col]])) else rep(NA_character_,nrow(d))
    value[tolower(value) %in% c("","na","null","undefined")]<-NA_character_;value
  }
  num<-function(col) suppressWarnings(as.numeric(chr(col)))
  coalesce<-function(columns) Reduce(function(a,b) ifelse(is.na(a),b,a),lapply(columns,chr))
  game<-coalesce(c("GameID","CustomGameID","GameUID"))
  half<-tolower(coalesce(c("Top/Bottom","TopBottom","InningTopBot")))
  half<-ifelse(half %in% c("top","t","1"),1,ifelse(half %in% c("bottom","bot","b","2"),2,0))
  inning<-num("Inning");pa<-num("PAofInning");pitch<-coalesce(c("PitchofPA","PitchNum","PitchNo"));pitch<-suppressWarnings(as.numeric(pitch))
  key<-paste(game,inning,half,pa,sep="\037")
  valid<-!is.na(game) & is.finite(inning) & is.finite(pa)
  timestamp<-rep(NA_real_,nrow(d))
  for(col in intersect(c("UTCDateTime","LocalDateTime"),names(d))) {
    x<-d[[col]]
    value<-if(inherits(x,"POSIXt")) as.numeric(x) else as.numeric(suppressWarnings(lubridate::parse_date_time(as.character(x),orders=c("ymd HMSz","ymd HMS","mdy HMS"),tz="UTC",quiet=TRUE)))
    use<-!is.finite(timestamp);timestamp[use]<-value[use]
  }
  date<-coalesce(c("GameDate","Date","PitchDate"))
  missing<-is.na(date);date[missing]<-substr(game[missing],1,8)
  date<-as.numeric(suppressWarnings(lubridate::parse_date_time(date,orders=c("ymd HMS","ymd","mdy"),tz="UTC",quiet=TRUE)))
  # Entire games stay together, including doubleheaders on the same date.
  game_start<-tapply(timestamp,game,function(x) if(any(is.finite(x))) min(x,na.rm=TRUE) else Inf)
  order<-order(date,unname(game_start[game]),game,inning,half,pa,pitch,timestamp,na.last=TRUE)
  event<-chr("PitchUID");missing<-is.na(event)
  event[missing]<-paste(key[missing],pitch[missing],sep="\037")
  order<-order[valid[order] & !duplicated(event[order])]
  pc<-chr("PitchCall");kb<-tolower(chr("KorBB"))
  terminal<-pc %in% c("InPlay","InPlayOut","InPlayNoOut","Walk","HitByPitch") |
    kb %in% c("strikeout","k","walk","bb","ibb","hbp","intentionalwalk") | num("Strikes")>=3
  terminal[is.na(terminal)]<-FALSE
  recorded<-length(unique(key[order]))
  completed<-unique(key[order[terminal[order]]])
  if(completed_only) order<-order[key[order] %in% completed]
  result<-d[order,,drop=FALSE]
  result$.development_pa<-match(key[order],unique(key[order]))
  result$.development_game<-game[order]
  result$.development_game_date<-as.Date(date[order]/86400,origin="1970-01-01")
  attr(result,"recorded_pas")<-recorded
  result
}

.development_series_cache<-new.env(parent=emptyenv())
development_game_values <- function(d,summarize,mode="",cache=TRUE) {
  key<-digest::digest(list(mode=mode,period="game-season-to-date",rows=d))
  if(cache && exists(key,.development_series_cache,inherits=FALSE)) return(get(key,.development_series_cache))
  # Keep every recorded pitch so the final point uses the same inputs and
  # denominator rules as the owning app's season summary.
  p<-development_pa_order(d,completed_only=FALSE)
  if(!nrow(p)) return(data.frame(GameIndex=integer(),GameLabel=character()))
  games<-unique(p$.development_game)
  groups<-split(seq_len(nrow(p)),factor(p$.development_game,levels=games))
  dates<-p$.development_game_date[match(games,p$.development_game)]
  labels<-ifelse(is.na(dates),paste("Game",seq_along(games)),format(dates,"%m/%d"))
  doubleheader<-!is.na(dates) & (duplicated(dates) | duplicated(dates,fromLast=TRUE))
  for(day in unique(as.character(dates[doubleheader]))) {
    same<-which(!is.na(dates) & as.character(dates)==day)
    labels[same]<-paste(labels[same],paste0("G",seq_along(same)))
  }
  result<-dplyr::bind_rows(lapply(seq_along(groups),function(i) {
    selected<-unlist(groups[seq_len(i)],use.names=FALSE)
    values<-summarize(p[selected,,drop=FALSE])
    values<-lapply(values,function(x) if(is.numeric(x) && length(x)==1) as.numeric(x) else NA_real_)
    tibble::as_tibble(c(list(GameIndex=i,GameID=games[i],GameDate=dates[i],GameLabel=labels[i]),values))
  }))
  if(cache) {
    keys<-get0("keys",.development_series_cache,inherits=FALSE,ifnotfound=character())
    if(length(keys)>=4) {rm(list=keys[1],envir=.development_series_cache);keys<-keys[-1]}
    assign(key,result,.development_series_cache);.development_series_cache$keys<-c(keys,key)
  }
  result
}

development_goal_segments <- function(series,target,lower_better) {
  empty<-data.frame(x=numeric(),y=numeric(),xend=numeric(),yend=numeric(),met=logical())
  if(nrow(series)<2) return(empty)
  met<-function(y) if(!is.finite(target)) FALSE else if(lower_better) y<=target else y>=target
  result<-vector("list",nrow(series)-1L)
  for(i in seq_len(nrow(series)-1L)) {
    x<-series$GameIndex[i];xend<-series$GameIndex[i+1];y<-series$Value[i];yend<-series$Value[i+1]
    if(!all(is.finite(c(x,xend,y,yend)))) next
    if(is.finite(target) && (y-target)*(yend-target)<0) {
      cross<-x+(target-y)/(yend-y)*(xend-x)
      result[[i]]<-data.frame(x=c(x,cross),y=c(y,target),xend=c(cross,xend),yend=c(target,yend),met=c(met(y),met(yend)))
    } else result[[i]]<-data.frame(x=x,y=y,xend=xend,yend=yend,met=met((y+yend)/2))
  }
  if(!any(lengths(result))) empty else dplyr::bind_rows(result)
}

development_percentile_color <- function(pct) {
  if(!is.finite(pct)) "#BFC9CA" else if(pct>=90) "#E33434" else if(pct>=75) "#DC735F" else
    if(pct>=60) "#D39383" else if(pct>=40) "#AAC6CB" else if(pct>=25) "#8CAAD2" else "#5D7EBC"
}

development_chart <- function(objective,index,narrow=FALSE) {
  series<-objective$series
  if(is.null(series)) series<-data.frame(GameIndex=integer(),GameLabel=character(),Value=numeric())
  finite<-series[is.finite(series$Value),,drop=FALSE]
  title<-paste0("Objective ",index," · ",objective$metric," · Game-by-game")
  if(!nrow(finite)) return(ggplot2::ggplot()+ggplot2::annotate("text",x=0,y=0,label="No completed game values available",size=3,color="#501214")+
    ggplot2::labs(title=title)+ggplot2::theme_void(base_size=9)+ggplot2::theme(plot.title=ggplot2::element_text(size=9,face="bold",hjust=.5),plot.background=ggplot2::element_rect(fill="transparent",color=NA)))
  target<-objective$target;average<-objective$average
  met<-if(is.finite(target)) if(isTRUE(objective$lower_better)) finite$Value<=target else finite$Value>=target else rep(FALSE,nrow(finite))
  segments<-development_goal_segments(series,target,isTRUE(objective$lower_better))
  subtitle<-paste0(if(is.finite(average)) paste0("D1 avg ",development_format(average,objective$metric),"   ·   ") else "D1 average unavailable   ·   ",
    if(is.finite(target)) paste0("Goal ",development_format(target,objective$metric),"   ·   Green = goal met") else "Enter a numeric goal to highlight progress")
  if(narrow) subtitle<-paste0(if(is.finite(average)) paste0("D1 avg ",development_format(average,objective$metric)) else "D1 average unavailable",
    if(is.finite(target)) paste0("   ·   Goal ",development_format(target,objective$metric)) else "   ·   Enter a numeric goal",
    "\nSeason-to-date   ·   ",nrow(series)," games   ·   Green = goal met")
  else subtitle<-paste0(subtitle,"   ·   Season-to-date   ·   ",nrow(series)," games")
  plot<-ggplot2::ggplot()+ggplot2::geom_blank(data=finite,ggplot2::aes(GameIndex,Value))
  if(is.finite(average)) plot<-plot+ggplot2::geom_hline(yintercept=average,linetype="dashed",color="grey40",linewidth=.45)
  if(is.finite(target)) plot<-plot+ggplot2::geom_hline(yintercept=target,linetype="dotted",color="#B4975A",linewidth=.35)
  plot<-plot+ggplot2::geom_segment(data=segments,ggplot2::aes(x=x,y=y,xend=xend,yend=yend,color=met),linewidth=.75,lineend="round")+
    ggplot2::geom_point(data=finite,ggplot2::aes(GameIndex,Value),color="#B4975A",size=1)
  if(any(met)) plot<-plot+ggplot2::geom_point(data=finite[met,,drop=FALSE],ggplot2::aes(GameIndex,Value),color="#16864b",size=1.1)
  breaks<-unique(round(seq(min(finite$GameIndex),max(finite$GameIndex),length.out=min(5,nrow(finite)))))
  plot+ggplot2::scale_color_manual(values=c(`FALSE`="#501214",`TRUE`="#16864b"),guide="none")+
    ggplot2::scale_x_continuous(breaks=breaks,labels=series$GameLabel[match(breaks,series$GameIndex)],expand=ggplot2::expansion(mult=c(.03,.03)))+
    ggplot2::scale_y_continuous(labels=function(x) if(grepl("%",objective$metric,fixed=TRUE)) sprintf("%.0f%%",100*x) else format(round(x,3),trim=TRUE),breaks=function(x) pretty(x,n=3))+
    ggplot2::labs(x="Game date",y=NULL,title=title,subtitle=subtitle)+ggplot2::theme_minimal(base_size=8)+
    ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),panel.grid.major=ggplot2::element_line(color="#ded8c9",linewidth=.25),
      panel.background=ggplot2::element_rect(fill="transparent",color=NA),plot.background=ggplot2::element_rect(fill="transparent",color=NA),
      plot.title=ggplot2::element_text(size=9,face="bold",color="#501214",margin=ggplot2::margin(b=2)),
      plot.subtitle=ggplot2::element_text(size=6.5,color="#501214",margin=ggplot2::margin(b=3)),
      axis.text=ggplot2::element_text(size=7,color="#501214"),axis.title=ggplot2::element_text(size=7),plot.margin=ggplot2::margin(5,8,4,5))
}
