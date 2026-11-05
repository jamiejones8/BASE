# Shared Shiny editor and vector PDF renderer for Hitting/Pitching Development Goals.
development_source_paths <- unlist(lapply(sys.frames(),function(frame) if(is.character(frame$ofile)) frame$ofile else NULL),use.names=FALSE)
development_root <- normalizePath(dirname(tail(development_source_paths[basename(development_source_paths)=="development_goals.R"],1)),mustWork=TRUE)
source(file.path(development_root,"development_series.R"),local=TRUE)
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
development_ui <- function(id,seasons) {
  ns<-shiny::NS(id)
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
  shiny::tagList(shiny::tags$style(shiny::HTML("
    .development-page{padding:18px;background:#f6f1e7;border:1px solid #c6a66b;border-radius:12px}
    .development-page h2,.development-page h3{color:#501214;font-weight:800}
    .development-controls,.development-editors{display:grid;grid-template-columns:repeat(auto-fit,minmax(285px,1fr));gap:20px}
    .development-editor{padding:18px;background:#fffaf0;border:1px solid #c6a66b;border-radius:8px;min-width:0}
    .development-supports textarea{background:#501214!important;color:#e6cc92!important;border-color:#c6a66b!important}
    .development-page details{margin-top:15px}.development-page summary{cursor:pointer;font-weight:700;color:#501214;padding:8px 0}
    .development-editor-heading{display:flex;align-items:center;justify-content:space-between;gap:12px}
    .development-current{padding:10px;background:#eee3cd;color:#501214;font-weight:700;margin-bottom:12px}
    .development-toolbar{display:flex;align-items:center;gap:16px;flex-wrap:wrap;margin:18px 0}
    .development-preview img{width:100%;height:auto;border:1px solid #c6a66b;border-radius:8px}
    .development-page .btn{background:#501214;color:#e6cc92;border-color:#c6a66b}
  ")),shiny::div(class="development-page",
    shiny::tags$h2("Development Goals"),shiny::tags$p("Build a season-based development plan. The preview and PDF use the same layout. Charts use completed PAs; early points use the available history until the rolling window fills."),
    shiny::div(class="development-controls",shiny::selectInput(ns("season"),"Season",choices=seasons,selected=if("S26" %in% seasons) "S26" else unname(seasons[1])),
      shiny::selectInput(ns("player"),"Player",choices=character()),
      shiny::numericInput(ns("rolling_window"),"Rolling window (PAs)",value=25,min=1,max=500,step=1)),
    shiny::div(style="display:none",shiny::textOutput(ns("two"))),
    shiny::div(class="development-toolbar",
      shiny::uiOutput(ns("export_control"))),
    shiny::div(class="development-editors",objective(1),shiny::conditionalPanel("output.two === 'true'",ns=ns,objective(2))),
    shiny::uiOutput(ns("status")),shiny::tags$h3("PDF preview"),
    shiny::div(class="development-preview",shiny::imageOutput(ns("preview"),height="auto"))))
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
  unique(errors)
}
development_server <- function(id,rows,player_column,seasons,summarize,percentile,mode,average=function(metric) NA_real_,lower_better=function(metric) FALSE) shiny::moduleServer(id,function(input,output,session) {
  two<-shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$add,two(TRUE));shiny::observeEvent(input$remove,two(FALSE))
  output$two<-shiny::renderText(if(two()) "true" else "false")
  shiny::outputOptions(output,"two",suspendWhenHidden=FALSE)
  season_rows<-shiny::reactive({
    d<-rows();shiny::req(input$season)
    col<-intersect(c("SeasonTag","SeasonGroup"),names(d))[1]
    if(!is.na(col)) d<-d[!is.na(d[[col]]) & d[[col]]==input$season,,drop=FALSE]
    if("is_bullpen" %in% names(d)) d<-d[!d$is_bullpen %in% TRUE,,drop=FALSE]
    d
  })
  shiny::observe({
    d<-season_rows();players<-sort(unique(as.character(d[[player_column]])))
    players<-players[!is.na(players) & nzchar(players)]
    current<-shiny::isolate(input$player)
    shiny::updateSelectInput(session,"player",choices=stats::setNames(players,vapply(players,development_name,character(1))),selected=if(length(current)==1 && current %in% players) current else head(players,1))
  })
  player_rows<-shiny::reactive({
    d<-season_rows();shiny::req(input$player)
    d<-d[!is.na(d[[player_column]]) & d[[player_column]]==input$player,,drop=FALSE]
    shiny::req(nrow(d));d
  })
  stats<-shiny::reactive(summarize(player_rows()))
  rolling<-shiny::reactive({
    window<-as.integer(input$rolling_window %||% 25)
    shiny::validate(shiny::need(is.finite(window) && window>=1 && window<=500,"Choose a rolling window from 1 to 500 PAs."))
    development_rolling_values(player_rows(),window,summarize,mode)
  })
  shiny::observe({
    values<-stats();choices<-names(values)
    for(i in 1:2) {
      selected<-shiny::isolate(input[[paste0("stat",i)]])
      shiny::updateSelectInput(session,paste0("stat",i),choices=choices,selected=if(length(selected)==1 && selected %in% choices) selected else if("K%" %in% choices) "K%" else choices[1])
    }
  })
  # Plans belong to one player and season. Reset text when that context changes;
  # adding/removing the second objective never rebuilds the first editor.
  shiny::observeEvent(list(input$player,input$season),{
    for(i in 1:2) {
      shiny::updateNumericInput(session,paste0("target",i),value=NA_real_)
      for(field in c("title","goal")) shiny::updateTextInput(session,paste0(field,i),value="")
      for(j in 1:3) shiny::updateTextAreaInput(session,paste0("support",i,"_",j),value="")
      for(j in 1:5) shiny::updateTextAreaInput(session,paste0("drill",i,"_",j),value="")
    }
    two(FALSE)
  },ignoreInit=TRUE)
  for(i in 1:2) local({j<-i;output[[paste0("current",j)]]<-shiny::renderUI({
    metric<-input[[paste0("stat",j)]];shiny::req(metric)
    shiny::div(class="development-current",paste("Season value:",development_format(stats()[[metric]],metric)))
  })})
  plan<-shiny::reactive({
    values<-stats();specs<-development_specs(mode)
    rankings<-data.frame(label=names(specs),metric=unname(specs),stringsAsFactors=FALSE)
    rankings$value<-vapply(rankings$metric,function(m) if(m %in% names(values)) as.numeric(values[[m]]) else NA_real_,numeric(1))
    rankings$percentile<-mapply(percentile,rankings$value,rankings$metric)
    objectives<-lapply(seq_len(if(two()) 2 else 1),function(i) {
      metric<-input[[paste0("stat",i)]] %||% "K%"
      series<-rolling()
      series<-if(metric %in% names(series)) data.frame(PAIndex=series$PAIndex,Value=as.numeric(series[[metric]])) else data.frame(PAIndex=integer(),Value=numeric())
      list(series=series,average=as.numeric(average(metric)),lower_better=isTRUE(lower_better(metric)),
        target=development_goal_target(input[[paste0("goal",i)]] %||% "",metric,input[[paste0("target",i)]] %||% NA_real_),
        title=input[[paste0("title",i)]] %||% "",metric=metric,current=development_format(values[[metric]],metric),
        goal=input[[paste0("goal",i)]] %||% "",supports=vapply(1:3,function(j) input[[paste0("support",i,"_",j)]] %||% "",character(1)),
        drills=Filter(nzchar,vapply(1:5,function(j) trimws(input[[paste0("drill",i,"_",j)]] %||% ""),character(1))))
    })
    list(rolling_window=as.integer(input$rolling_window %||% 25),player=development_name(input$player),season=names(seasons)[match(input$season,seasons)],mode=mode,
      portrait=development_portrait(input$player),rankings=rankings,objectives=objectives)
  })
  output$status<-shiny::renderUI({
    if(!nrow(season_rows())) return(shiny::tags$p("No season data is available for this selection."))
    p<-plan();errors<-development_payload_errors(p)
    shiny::tagList(if(length(errors)) shiny::tags$p(class="text-muted",paste(errors,collapse=" ")),
      if(any(!is.finite(p$rankings$percentile))) shiny::tags$p(class="text-muted","N/A means the season value or a matching D1 percentile reference is unavailable; no ranking is estimated."),
      if(!nzchar(p$portrait)) shiny::tags$p(class="text-muted","The official roster has no matched headshot for this player; the report uses initials."))
  })
  output$export_control<-shiny::renderUI({
    if(!nrow(season_rows())) return(shiny::tags$button(class="btn",disabled="disabled","Select a season with data"))
    if(length(development_payload_errors(plan()))) shiny::tags$button(class="btn",disabled="disabled","Complete goals to export PDF")
    else shiny::downloadButton(session$ns("pdf"),"Export PDF")
  })
  output$preview<-shiny::renderImage({
    p<-plan();file<-tempfile(fileext=".png")
    grDevices::png(file,width=1650,height=1050,res=108)
    tryCatch(development_draw(p),finally=grDevices::dev.off())
    list(src=file,contentType="image/png",alt=paste(p$player,"Development Goals PDF preview"))
  },deleteFile=TRUE)
  output$pdf<-shiny::downloadHandler(filename=function() paste0(gsub("[^A-Za-z0-9]+","_",plan()$player),"_Development_Goals.pdf"),content=function(file) {
    p<-plan();shiny::validate(shiny::need(!length(development_payload_errors(p)),"Complete all required goal fields before exporting."))
    development_pdf(file,p)
  })
  list(plan=plan,stats=stats,two=two,rolling=rolling)
})

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
  background<-development_image(file.path(development_root,"assets","development-ballpark-maroon.png"),.9)
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
    grid::grid.draw(ggplot2::ggplotGrob(development_chart(plan$objectives[[i]],i,plan$rolling_window %||% 25,narrow=count==2)))
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
  n<-length(plan$objectives)
  for(i in seq_len(n)) {
    o<-plan$objectives[[i]];x<-if(n==1) 170 else 30+(i-1)*535;w<-if(n==1) 760 else 505
    box(x,302,w,36,maroon,gold);fit(paste0("OBJECTIVE ",i,"  ",if(nzchar(o$title)) o$title else "Your development objective"),x,302,w,36,16,gold)
    cw<-(w-75)/2
    box(x+15,246,cw,54);text(paste(plan$season,o$metric),x+15+cw/2,286,10,maroon,TRUE);fit(o$current,x+15,247,cw,32,25)
    grid::grid.segments(x+w/2-12,268,x+w/2+12,268,default.units="native",arrow=grid::arrow(length=grid::unit(.10,"inches"),type="closed"),gp=grid::gpar(col=gold,fill=gold,lwd=2))
    box(x+w-cw-15,246,cw,54);text("GOAL",x+w-cw/2-15,286,10,maroon,TRUE);fit(if(nzchar(o$goal)) o$goal else "Your target",x+w-cw-15,247,cw,32,23)
    sw<-(w-40)/3
    for(j in 1:3) {sx<-x+10+(j-1)*(sw+10);box(sx,166,sw,68,maroon,gold);fit(if(nzchar(o$supports[j])) o$supports[j] else paste("Supporting action",j),sx,166,sw,68,13,gold)}
    drills<-o$drills
    if(length(drills)) {
      cols<-if(n==1) length(drills) else min(3,length(drills));rows<-ceiling(length(drills)/cols)
      dw<-(w-20-(cols-1)*8)/cols;dh<-if(rows==1) 73 else 35
      for(j in seq_along(drills)) {
        count<-min(cols,length(drills)-(j-1)%/%cols*cols);offset<-(w-(count*dw+(count-1)*8))/2
        dx<-x+offset+(j-1)%%cols*(dw+8);dy<-if(rows==1) 66 else 104-(j-1)%/%cols*40
        box(dx,dy,dw,dh);fit(drills[j],dx,dy,dw,dh,12)
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
