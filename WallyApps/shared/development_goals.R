# Shared Shiny editor and vector PDF renderer for Hitting/Pitching Development Goals.
development_source_paths <- unlist(lapply(sys.frames(),function(frame) if(is.character(frame$ofile)) frame$ofile else NULL),use.names=FALSE)
development_root <- normalizePath(dirname(tail(development_source_paths[basename(development_source_paths)=="development_goals.R"],1)),mustWork=TRUE)
source(file.path(development_root,"development_series.R"),local=TRUE)
source(file.path(development_root,"development_tracking.R"),local=TRUE)
development_portraits <- read.csv(file.path(development_root,"portraits.csv"),stringsAsFactors=FALSE)
development_name <- function(x) {
  if(length(x)!=1 || is.na(x)) return("")
  if(grepl(",",x,fixed=TRUE)) { p<-strsplit(x,",",fixed=TRUE)[[1]]; x<-paste(trimws(p[2]),trimws(p[1])) }
  trimws(x)
}
development_name_key <- function(x) gsub("[^a-z0-9]","",tolower(iconv(x,to="ASCII//TRANSLIT")))
development_portrait <- function(player) {
  i<-match(development_name_key(development_name(player)),development_name_key(development_portraits$Name))
  if(is.na(i) || is.na(development_portraits$File[i]) || !nzchar(development_portraits$File[i])) return("")
  file.path(development_root,"assets",development_portraits$File[i])
}
development_specs <- function(mode) {
  if(mode=="hitting") stats::setNames(c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZ-Whiff%","Chase%","EV>95%","10-35*%"),
    c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZWhiff%","Chase%","EV>95%","10-35%"))
  else stats::setNames(c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZWhiff%","GB%","Strike%","E&A%"),
    c("wOBA","xwOBA","OPS","K%","BB%","Barrel%","Whiff%","IZWhiff%","GB%","Strike%","E&A%"))
}
development_format <- function(value,metric) {
  if(length(value)!=1 || !is.finite(value)) return("N/A")
  if(grepl("%",metric,fixed=TRUE)) sprintf("%.1f%%",100*value) else if(metric %in% c("wOBA","xwOBA","OPS","BAA","SLG","OBP","AVG")) sprintf("%.3f",value) else sprintf("%.2f",value)
}
development_xwoba_values <- function(last,grid) {
  num<-function(col) if(col %in% names(last)) suppressWarnings(as.numeric(last[[col]])) else rep(NA_real_,nrow(last))
  chr<-function(col) if(col %in% names(last)) as.character(last[[col]]) else rep("",nrow(last))
  pc<-chr("PitchCall");kb<-tolower(chr("KorBB"));pr<-tolower(chr("PlayResult"))
  bip<-pc %in% c("InPlay","InPlayOut","InPlayNoOut")
  x<-num("xwOBA");ev<-num("ExitSpeed");la<-num("Angle")
  if(!is.null(grid) && all(c("ev_edges","la_edges","grid") %in% names(grid))) {
    ei<-findInterval(ev,grid$ev_edges);li<-findInterval(la,grid$la_edges)
    ok<-!is.finite(x) & is.finite(ev) & is.finite(la) & ei>=1 & li>=1 & ei<=nrow(grid$grid) & li<=ncol(grid$grid)
    x[ok]<-grid$grid[cbind(ei[ok],li[ok])]
  }
  out<-ifelse(bip,x,NA_real_)
  out[kb %in% c("strikeout","k")]<-0
  out[kb %in% c("walk","bb")]<-.690
  out[pc=="HitByPitch" | kb=="hbp"]<-.720
  out[kb %in% c("ibb","intentionalwalk") | grepl("intentional",pr)]<-NA_real_
  out
}
development_ui <- function(id,seasons,mode="pitching") {
  ns<-shiny::NS(id)
  assets <- normalizePath(file.path(development_root,"assets"),mustWork=TRUE)
  if(!identical(unname(shiny::resourcePaths()["base-development-assets"]),assets)) {
    shiny::addResourcePath("base-development-assets",assets)
  }
  background_url <- paste0("base-development-assets/development-ballpark-template.png?v=",
    as.numeric(file.info(file.path(assets,"development-ballpark-template.png"))$mtime))
  objective<-function(i) shiny::div(class="development-editor",
    shiny::div(class="development-editor-heading",shiny::tags$h3(paste("Objective",i)),
      if(i==1) shiny::conditionalPanel("output.two === 'false'",ns=ns,shiny::actionButton(ns("add"),"+ Add Objective 2"))
      else shiny::actionButton(ns("remove"),"Remove Objective 2")),
    shiny::textInput(ns(paste0("title",i)),"Objective",placeholder="Improve …",width="100%"),
    shiny::selectInput(ns(paste0("stat",i)),"Season stat",choices=character()),
    shiny::uiOutput(ns(paste0("current",i))),
    shiny::textInput(ns(paste0("goal",i)),"Goal",placeholder="Enter your target",width="100%"),
    shiny::numericInput(ns(paste0("target",i)),"Numeric chart goal (optional; percentages as 35 for 35%)",value=NA_real_,step=.1,width="100%"),
    shiny::tags$h4("Supporting actions"),
    shiny::div(class="development-supports",lapply(1:3,function(j) shiny::textAreaInput(ns(paste0("support",i,"_",j)),paste("Action",j),rows=2,width="100%"))),
    shiny::tags$details(shiny::tags$summary("Drills · optional, up to 5"),
      lapply(1:5,function(j) shiny::textAreaInput(ns(paste0("drill",i,"_",j)),paste("Drill",j),rows=2,width="100%"))))
  defense <- if(mode=="hitting") shiny::div(class="development-editor development-defense",
    shiny::tags$h3("Defensive Goals"),
    shiny::textAreaInput(ns("defense_goal"),"Defensive goal · optional",rows=2,width="100%",
      placeholder="Write a defensive development goal"),
    shiny::tags$h4("Supporting actions"),
    shiny::div(class="development-defense-actions",lapply(1:3,function(j)
      shiny::textAreaInput(ns(paste0("defense_support",j)),paste("Action",j),rows=2,width="100%"))),
    shiny::tags$details(shiny::tags$summary("Drills · optional, up to 5"),
      lapply(1:5,function(j) shiny::textAreaInput(ns(paste0("defense_drill",j)),paste("Drill",j),rows=2,width="100%"))))
  shiny::tagList(shiny::tags$style(shiny::HTML(paste0("
    .development-page{padding:18px;background:#501214 url('",background_url,"') center/cover fixed;border:1px solid #c6a66b;border-radius:12px;color:#fff6e4}
    .development-page h2,.development-page h3,.development-page h4{color:#e6cc92;font-weight:800}
    .development-page label{color:#fff6e4}
    .development-page .text-muted,.development-page .development-tracking-note,.development-page .development-saved-status{color:#fff6e4!important}
    .development-page .nav-tabs>li>a{color:#e6cc92}.development-page .nav-tabs>li.active>a,.development-page .nav-tabs .nav-link.active{color:#501214;background:#fff6e4}
    .development-page input,.development-page select,.development-page textarea,.development-page .selectize-input{background:#fff6e4;color:#501214;border-color:#c6a66b}
    .development-controls,.development-editors{display:grid;grid-template-columns:repeat(auto-fit,minmax(285px,1fr));gap:20px}
    .development-editor{padding:18px;background:rgba(80,18,20,.8);border:1px solid #c6a66b;border-radius:8px;min-width:0}
    .development-supports textarea{background:#501214!important;color:#e6cc92!important;border-color:#c6a66b!important}
    .development-page details{margin-top:15px}.development-page summary{cursor:pointer;font-weight:700;color:#e6cc92;padding:8px 0}
    .development-defense{margin-top:20px;border-color:#fff}
    .development-defense h3,.development-defense h4,.development-defense label,.development-defense summary{color:#501214;background:#c8a76c;border:1px solid #fff;border-radius:5px;padding:8px}
    .development-defense textarea{background:#c8a76c!important;color:#501214!important;border:1px solid #fff!important;font-weight:650}
    .development-defense-actions{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:16px}
    .development-defense-details{padding:16px;background:#c8a76c;color:#501214;border:1px solid #fff;border-radius:6px;margin-top:18px}
    .development-defense-details h4{color:#501214}
    .development-editor-heading{display:flex;align-items:center;justify-content:space-between;gap:12px}
    .development-current{padding:10px;background:#eee3cd;color:#501214;font-weight:700;margin-bottom:12px}
    .development-toolbar{display:flex;align-items:center;gap:16px;flex-wrap:wrap;margin:18px 0}
    .development-preview img{width:100%;height:auto;border:1px solid #c6a66b;border-radius:8px}
    .development-page .btn{background:#501214;color:#e6cc92;border-color:#c6a66b}
    .development-page .tab-content{padding-top:18px}
    .development-tracking-scroll{overflow-x:auto}
    .development-tracking-table{width:100%;border-collapse:collapse;background:#fffaf0;color:#501214;font-size:14px}
    .development-tracking-table th{padding:14px;background:#501214;color:#fff;text-align:left}
    .development-tracking-table td{padding:16px;vertical-align:top;border-bottom:1px solid #dccbaf}
    .development-tracking-table th:first-child{width:20%}
    .development-tracking-table th:nth-child(2),.development-tracking-table th:nth-child(3){width:33%}
    .development-tracking-table th:last-child{width:14%}
    .development-tracking-hitting th:first-child{width:17%}
    .development-tracking-hitting th:nth-child(2),.development-tracking-hitting th:nth-child(3),.development-tracking-hitting th:nth-child(4){width:24%}
    .development-tracking-hitting th:last-child{width:11%}
    .development-tracked-defense{background:#c8a76c;color:#501214;border:1px solid #fff}
    .development-tracking-table .development-team-total{background:#eee3cd;font-weight:700}
    .development-tracked-goal{line-height:1.45}.development-tracked-values{font-size:12px;color:#5e554d;margin:7px 0}
    .development-tracked-state{font-size:12px;margin-top:5px}
    .development-progress{height:8px;background:#e5ded1;border-radius:6px;overflow:hidden}
    .development-progress-fill{height:100%;border-radius:6px}
    .development-page .development-player-toggle{background:transparent;color:#501214;border:0;padding:0;text-align:left;font-weight:800;text-decoration:underline;cursor:pointer}
    .development-hit-list{display:flex;gap:12px;flex-wrap:wrap;font-size:18px}
    .development-hit-list span{white-space:nowrap}.development-hit-number{font-size:11px;color:#5e554d}
    .development-tracking-detail>td{background:#f6f1e7}
    .development-tracking-charts{display:grid;grid-template-columns:repeat(auto-fit,minmax(290px,1fr));gap:18px}
    .development-tracking-note{color:#5e554d;font-size:13px;margin:12px 0}
    .development-saved-status{padding:10px 0;color:#501214;font-weight:650}
    .development-no-goal{color:#777}
  "))),shiny::div(class="development-page",
    shiny::tags$h2("Development Goals"),
    shiny::div(style="display:none",shiny::textOutput(ns("two"))),
    shiny::tabsetPanel(id=ns("goals_tab"),
      shiny::tabPanel("Build Goals",value="build",
        shiny::tags$p("Choose a baseline season, then save the plan for the following season. Saved goals carry forward until replaced. Charts show season-to-date stats after each game."),
        shiny::div(class="development-controls",
          shiny::selectInput(ns("season"),"Baseline season",choices=development_baseline_seasons(seasons),selected="S26"),
          shiny::selectInput(ns("player"),"Player",choices=character())),
        shiny::uiOutput(ns("saved_status")),
        shiny::div(class="development-toolbar",shiny::uiOutput(ns("export_control")),
          shiny::actionButton(ns("reload_saved"),"Reload Saved Goals")),
        shiny::div(class="development-editors",objective(1),shiny::conditionalPanel("output.two === 'true'",ns=ns,objective(2))),
        defense,
        shiny::uiOutput(ns("status")),shiny::tags$h3("PDF preview"),
        shiny::div(class="development-preview",shiny::imageOutput(ns("preview"),height="auto"))),
      shiny::tabPanel("Track Goals",value="track",
        shiny::selectInput(ns("tracking_season"),"Tracking season",choices=c("2026 Fall"="F26"),selected="F26"),
        shiny::uiOutput(ns("tracking_note")),
        shiny::uiOutput(ns("tracking_table"))))))
}
development_payload_errors <- function(plan) {
  errors<-character()
  if(!nzchar(plan$player) || !nrow(plan$rankings)) errors<-c(errors,"Select a player with season data.")
  for(i in seq_along(plan$objectives)) {
    o<-plan$objectives[[i]]
    if(any(!nzchar(trimws(c(o$title,o$metric,o$goal,o$supports))))) errors<-c(errors,paste0("Complete Objective ",i,", its goal, and all three supporting actions."))
    if(nzchar(o$goal) && !is.null(o$target) && !is.finite(o$target)) errors<-c(errors,paste0("Objective ",i,": enter one numeric goal, or fill Numeric chart goal."))
    if(nchar(o$title)>100 || nchar(o$goal)>100 || any(nchar(o$supports)>180) || any(nchar(o$drills)>120)) errors<-c(errors,"Shorten the objective/goal to 100 characters, actions to 180, and drills to 120 so every box remains legible.")
  }
  unique(c(errors,development_defense_errors(plan$defense,plan$mode)))
}

# Written defense plans stay separate from numeric hitting objectives and their
# automatic hit-rate calculation, while saving/carrying forward with the plan.
development_defense_errors <- function(defense,mode) {
  if(is.null(defense)) return(character())
  if(!identical(mode,"hitting")) return("Defensive Goals are available for hitters only.")
  errors <- character()
  if(length(defense$goal)!=1L || length(defense$supports)!=3L || length(defense$drills)>5L ||
      any(!nzchar(trimws(c(defense$goal,defense$supports))))) {
    errors <- c(errors,"Complete Defensive Goals and all three supporting actions, or leave the section blank.")
  }
  if(any(nchar(defense$goal)>180) || any(nchar(defense$supports)>180) || any(nchar(defense$drills)>120)) {
    errors <- c(errors,"Keep the defensive goal/actions to 180 characters and drills to 120 so the text remains legible.")
  }
  errors
}

development_goal_columns <- function(plan) {
  n <- length(plan$objectives)
  defense <- identical(plan$mode,"hitting") && !is.null(plan$defense)
  if(defense && n==2L) return(data.frame(x=c(30,385,740),width=330,kind=c("stat","stat","defense"),index=c(1L,2L,NA_integer_)))
  if(defense) return(data.frame(x=c(30,740),width=c(685,330),kind=c("stat","defense"),index=c(1L,NA_integer_)))
  data.frame(x=if(n==1L) 170 else c(30,565),width=if(n==1L) 760 else 505,kind="stat",index=seq_len(n))
}
source(file.path(development_root,"development_server.R"),local=TRUE)

.development_image_cache<-new.env(parent=emptyenv())
development_image <- function(path,opacity=1) {
  if(!nzchar(path) || !file.exists(path)) return(NULL)
  key<-paste(path,opacity)
  if(exists(key,.development_image_cache,inherits=FALSE)) return(get(key,.development_image_cache))
  x<-if(grepl("[.]png$",path,ignore.case=TRUE)) png::readPNG(path,native=opacity==1) else jpeg::readJPEG(path,native=opacity==1)
  if(opacity==1 && grepl("^portrait-.*[.]jpg$",basename(path))) {
    mask_path<-sub("[.]jpg$","-mask.png",path)
    if(file.exists(mask_path)) {
      alpha<-png::readPNG(mask_path)
      stopifnot(identical(dim(alpha),dim(x)))
      rgba<-writeBin(as.integer(x),raw(),size=4L,endian="little")
      rgba[seq.int(4L,length(rgba),by=4L)]<-as.raw(round(t(alpha)*255))
      x[]<-readBin(rgba,"integer",n=length(x),size=4L,endian="little")
    }
  }
  if(opacity<1) {
    rgba<-array(1,c(dim(x)[1:2],4));rgba[,,1:3]<-x[,,1:3];rgba[,,4]<-if(dim(x)[3]==4) x[,,4]*opacity else opacity;x<-rgba
  }
  assign(key,x,.development_image_cache)
  if(grepl("portrait-",basename(path),fixed=TRUE)) {
    order<-get0("portrait_order",.development_image_cache,inherits=FALSE,ifnotfound=character())
    order<-c(setdiff(order,key),key)
    if(length(order)>6) rm(list=head(order,-6),envir=.development_image_cache)
    .development_image_cache$portrait_order<-tail(order,6)
  }
  x
}
development_pdf <- function(file,plan) {
  grDevices::pdf(file,width=1100/72,height=700/72,onefile=TRUE,useDingbats=FALSE,version="1.4")
  tryCatch(development_draw(plan),finally=grDevices::dev.off())
}
development_draw <- function(plan) {
  maroon<-"#501214";gold<-"#c8a76c";cream<-"#fff6e4"
  grid::grid.newpage();grid::pushViewport(grid::viewport(xscale=c(0,1100),yscale=c(0,700)));on.exit(grid::popViewport())
  rect<-function(x,y,w,h,fill=cream,col=gold) grid::grid.rect(x,y,w,h,default.units="native",just=c("left","bottom"),gp=grid::gpar(fill=fill,col=col,lwd=1.1))
  box<-function(x,y,w,h,fill=cream,col=gold) grid::grid.roundrect(x+w/2,y+h/2,w,h,r=grid::unit(5,"points"),default.units="native",gp=grid::gpar(fill=fill,col=col,lwd=1.1))
  text<-function(label,x,y,size=12,col=maroon,bold=FALSE,just="centre") grid::grid.text(iconv(label,to="ASCII//TRANSLIT",sub="?"),x,y,default.units="native",just=just,gp=grid::gpar(col=col,fontsize=size,fontfamily="Helvetica",fontface=if(bold) "bold" else "plain"))
  fit<-function(label,x,y,w,h,size=13,col=maroon,bold=TRUE) {
    label<-iconv(gsub("[\r\n]+"," ",label),to="ASCII//TRANSLIT",sub="?")
    wrap<-function(pt) {
      words<-strsplit(label," +")[[1]]
      words<-unlist(lapply(words,function(word) {
        width<-grid::convertWidth(grid::grobWidth(grid::textGrob(word,gp=grid::gpar(fontsize=pt,fontfamily="Helvetica",fontface=if(bold) "bold" else "plain"))),"points",valueOnly=TRUE)
        if(width<=w-14) return(word)
        limit<-max(1,floor((w-14)/(pt*.96)));start<-seq(1,nchar(word),by=limit)
        substring(word,start,pmin(start+limit-1,nchar(word)))
      }),use.names=FALSE)
      lines<-character();line<-""
      for(word in words) {
        nextline<-trimws(paste(line,word))
        width<-grid::convertWidth(grid::grobWidth(grid::textGrob(nextline,gp=grid::gpar(fontsize=pt,fontfamily="Helvetica",fontface=if(bold) "bold" else "plain"))),"points",valueOnly=TRUE)
        if(nzchar(line) && width>w-14) {lines<-c(lines,line);line<-word} else line<-nextline
      }
      c(lines,line)
    }
    for(pt in seq(size,7,by=-.5)) {lines<-wrap(pt);if(length(lines)*pt*1.2<h-8) break}
    text(paste(lines,collapse="\n"),x+w/2,y+h/2,pt,col,bold)
  }
  rect(0,0,1100,700,maroon,NA)
  background<-development_image(file.path(development_root,"assets","development-ballpark-template.png"),.9)
  if(!is.null(background)) grid::grid.raster(background,x=550,y=350,width=1100,height=1100*dim(background)[1]/dim(background)[2],default.units="native",interpolate=TRUE)
  rect(15,15,1070,670,NA,gold);rect(0,660,1100,40,maroon,NA);rect(0,0,1100,40,maroon,NA)
  text("T E X A S   S T A T E",230,680,17,gold,TRUE);text("B A S E B A L L",875,680,17,gold,TRUE)
  logo<-development_image(file.path(development_root,"assets","bobcat.png"))
  if(!is.null(logo)) grid::grid.raster(logo,x=550,y=680,width=34,height=34,default.units="native")
  fit(toupper(plan$player),30,602,1040,64,46,gold)
  text(paste(toupper(plan$mode),"DEVELOPMENT GOALS",toupper(plan$season),sep="   |   "),550,604,11,cream,TRUE)
  portrait<-development_image(plan$portrait)
  if(!is.null(portrait)) {
    ratio<-dim(portrait)[2]/dim(portrait)[1];h<-278;w<-h*ratio
    if(w>245) {w<-245;h<-w/ratio}
    grid::grid.raster(portrait,x=150,y=465,width=w,height=h,default.units="native",interpolate=TRUE)
  } else text(paste(substr(strsplit(plan$player," +")[[1]],1,1),collapse=""),150,475,60,gold,TRUE)
  # Charts occupy the center; percentile rankings remain one column at far right.
  count<-length(plan$objectives);chart_width<-(510-(count-1)*12)/count
  for(i in seq_len(count)) {
    x<-280+(i-1)*(chart_width+12)
    box(x,350,chart_width,245,grDevices::adjustcolor(cream,alpha.f=.92))
    grid::pushViewport(grid::viewport(x=x+chart_width/2,y=472.5,width=chart_width-10,height=241,default.units="native"))
    # Wrap the benchmark caption even for a single chart so long labels cannot
    # force the plot's title outside its viewport.
    grid::grid.draw(ggplot2::ggplotGrob(development_chart(plan$objectives[[i]],i,narrow=TRUE)))
    grid::popViewport()
  }
  text("D1 PERCENTILE RANKINGS",820,590,11,"#ffffff",TRUE,just="left")
  text("POOR",893,579,6.5,"#ffffff",TRUE,just="left");text("AVG",950,579,6.5,"#ffffff",TRUE);text("GREAT",1008,579,6.5,"#ffffff",TRUE,just="right")
  rankings<-plan$rankings
  for(i in seq_len(nrow(rankings))) {
    x<-820;y<-565-(i-1)*20
    pct<-rankings$percentile[i];value<-rankings$value[i]
    text(rankings$label[i],x,y,9,"#ffffff",TRUE,just="left")
    rect(x+73,y-2.5,115,5,"#D8E8E9",NA)
    grid::grid.segments(x+130.5,y-6,x+130.5,y+6,default.units="native",gp=grid::gpar(col="#ffffff",lwd=.7))
    if(is.finite(pct)) {
      position<-max(1,min(99,pct));color<-development_percentile_color(pct)
      rect(x+73,y-2.5,115*position/100,5,color,NA)
      grid::grid.circle(x+73+115*position/100,y,r=7.5,default.units="native",gp=grid::gpar(fill=color,col=NA))
      text(round(pct),x+73+115*position/100,y,6.5,"#ffffff",TRUE)
    } else text("N/A",x+130.5,y,7,"#ffffff",TRUE)
    text(development_format(value,rankings$metric[i]),1067,y,9,"#ffffff",TRUE,just="right")
  }
  columns <- development_goal_columns(plan)
  for(k in seq_len(nrow(columns))) {
    defensive <- columns$kind[k]=="defense"
    i <- columns$index[k]
    o <- if(defensive) plan$defense else plan$objectives[[i]]
    x <- columns$x[k];w <- columns$width[k]
    title_fill <- if(defensive) gold else maroon
    title_ink <- if(defensive) maroon else gold
    border <- if(defensive) "#ffffff" else gold
    box(x,302,w,36,title_fill,border)
    fit(if(defensive) "Defensive Goals" else paste0("OBJECTIVE ",i,"  ",if(nzchar(o$title)) o$title else "Your development objective"),x,302,w,36,16,title_ink)
    if(defensive) {
      box(x+10,246,w-20,54,gold,border);fit(o$goal,x+10,246,w-20,54,16,maroon)
    } else {
      cw<-(w-75)/2
      box(x+15,246,cw,54);text(paste(plan$season,o$metric),x+15+cw/2,286,10,maroon,TRUE);fit(o$current,x+15,247,cw,32,25)
      grid::grid.segments(x+w/2-12,268,x+w/2+12,268,default.units="native",arrow=grid::arrow(length=grid::unit(.10,"inches"),type="closed"),gp=grid::gpar(col=gold,fill=gold,lwd=2))
      box(x+w-cw-15,246,cw,54);text("GOAL",x+w-cw/2-15,286,10,maroon,TRUE);fit(if(nzchar(o$goal)) o$goal else "Your target",x+w-cw-15,247,cw,32,23)
    }
    sw<-(w-40)/3
    for(j in 1:3) {sx<-x+10+(j-1)*(sw+10);box(sx,166,sw,68,title_fill,border);fit(if(nzchar(o$supports[j])) o$supports[j] else paste("Supporting action",j),sx,166,sw,68,13,title_ink)}
    drills<-o$drills
    if(length(drills)) {
      cols<-if(nrow(columns)==1) length(drills) else min(3,length(drills));rows<-ceiling(length(drills)/cols)
      dw<-(w-20-(cols-1)*8)/cols;dh<-if(rows==1) 73 else 35
      for(j in seq_along(drills)) {
        count<-min(cols,length(drills)-(j-1)%/%cols*cols);offset<-(w-(count*dw+(count-1)*8))/2
        dx<-x+offset+(j-1)%%cols*(dw+8);dy<-if(rows==1) 66 else 104-(j-1)%/%cols*40
        box(dx,dy,dw,dh,if(defensive) gold else cream,border);fit(drills[j],dx,dy,dw,dh,12,maroon)
      }
    }
  }
  # Measure each word so its gold initial and white remainder form one centered line.
  words<-c("COMPETE","ATTITUDE","TOUGHNESS","SELFLESS");footer_size<-14;gap<-32
  word_width<-function(label) grid::convertWidth(grid::grobWidth(grid::textGrob(label,gp=grid::gpar(fontsize=footer_size,fontfamily="Helvetica",fontface="bold"))),"points",valueOnly=TRUE)
  widths<-vapply(words,word_width,numeric(1));cursor<-(1100-sum(widths)-gap*(length(words)-1))/2
  for(j in seq_along(words)) {
    initial<-substr(words[j],1,1)
    text(initial,cursor,20,footer_size,gold,TRUE,just="left")
    text(substring(words[j],2),cursor+word_width(initial),20,footer_size,"#ffffff",TRUE,just="left")
    cursor<-cursor+widths[j]+gap
  }
  if(any(!is.finite(rankings$percentile))) text("N/A: season value or D1 reference unavailable",1065,346,7,"#ffffff",TRUE,just="right")
}
