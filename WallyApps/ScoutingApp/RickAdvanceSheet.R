# Rick Advance Sheet: portrait report built from the shared pitcher data.
# Reference rows and percentile bands copied from PitchingApp's AAR tables.
# Count/family zone cells use the overall D1 pool; chase uses its pitch-type pool.
rick_percentile_reference <- read.csv(file.path(APP_ROOT,'reference','rick_d1_percentiles.csv'))
rick_percentile <- function(value, metric, pitch_type='', lower=FALSE) {
  if (!is.finite(value)) return(NA_real_)
  ref <- rick_percentile_reference
  keep <- ref$metric == metric & is.finite(ref$value)
  if (startsWith(metric,'pitchtype_')) keep <- keep & ref$scope == 'pitch_type' & ref$pitch_type == pitch_type
  else keep <- keep & ref$scope == 'overall'
  values <- ref$value[keep]
  if (!length(values)) return(NA_real_)
  pct <- if (lower) mean(values >= value) else mean(values <= value)
  pmin(99,pmax(1,round(100*pct)))
}
rick_cell_style <- function(value, metric, pitch_type='', lower=FALSE) {
  neutral <- list(fill='#FFFFFF',text='#1a1a1a')
  if (is.na(metric) || !is.finite(value)) return(neutral)
  if (metric == 'usage') return(neutral)
  pct <- rick_percentile(value,metric,pitch_type,lower)
  if (!is.finite(pct) || (pct >= 40 && pct < 60)) return(neutral)
  if (pct >= 90) return(list(fill='#8B0000',text='#FFFFFF'))
  if (pct >= 75) return(list(fill='#FF0000',text='#FFFFFF'))
  if (pct >= 60) return(list(fill='#FF9999',text='#1a1a1a'))
  if (pct >= 25) return(list(fill='#A1D99B',text='#1a1a1a'))
  if (pct >= 10) return(list(fill='#008000',text='#FFFFFF'))
  list(fill='#006400',text='#FFFFFF')
}
rick_shaded_table <- function(display, values, metrics, pitch_types=rep('',nrow(display)),
                              lower=rep(FALSE,ncol(display)),size=6) {
  tbl <- tableGrob(display,rows=NULL,theme=ttheme_minimal(base_size=size,
    padding=unit(c(2,2),'mm'),
    core=list(bg_params=list(fill='#FFFFFF',col='#d8d3c9')),
    colhead=list(fg_params=list(fontface='bold'),bg_params=list(fill='#e8e2d7',col='#d8d3c9'))))
  for (i in which(tbl$layout$name == 'core-fg')) {
    row <- tbl$layout$t[i]-1; col <- tbl$layout$l[i]
    if (grepl('[0-9]', as.character(display[[col]][row]))) {
      tbl$grobs[[i]]$gp$font <- 2L
    }
  }
  for (i in which(tbl$layout$name == 'core-bg')) {
    row <- tbl$layout$t[i]-1; col <- tbl$layout$l[i]
    if (is.na(metrics[col])) next
    style <- rick_cell_style(values[[col]][row],metrics[col],pitch_types[row],lower[col])
    tbl$grobs[[i]]$gp$fill <- style$fill
    fg <- which(tbl$layout$name == 'core-fg' & tbl$layout$t == row+1 & tbl$layout$l == col)
    tbl$grobs[[fg]]$gp$col <- style$text
  }
  tbl
}
rick_metric_table <- function(values, metrics, lower=rep(FALSE,ncol(values)),size=6) {
  display <- as.data.frame(lapply(values,function(x) ifelse(is.finite(x),sprintf('%.0f%%',100*x),'-')),check.names=FALSE)
  tbl <- rick_shaded_table(display,values,metrics,lower=lower,size=size)
  tbl$widths <- unit(rep(1/ncol(values),ncol(values)),'npc')
  tbl
}

# Keep the three header slots, with plain values and a compact D1 rank below.
rick_header_stats <- function(values) {
  metrics <- c('performance_k_pct','performance_bb_pct','performance_gb_pct')
  cards <- lapply(seq_along(metrics),function(i) {
    value <- values[[i]][1]
    pct <- rick_percentile(value,metrics[i],lower=i==2)
    style <- rick_cell_style(value,metrics[i],lower=i==2)
    bar_color <- if(style$fill == '#FFFFFF') '#888888' else style$fill
    grobTree(
      textGrob(names(values)[i],x=.5,y=.90,gp=gpar(fontsize=8,fontface='bold')),
      textGrob(if(is.finite(value)) sprintf('%.0f%%',100*value) else '-',x=.5,y=.56,
        gp=gpar(fontsize=15,fontface='bold',col='#1a1a1a')),
      rectGrob(x=.10,y=.15,width=.57,height=.09,just='left',gp=gpar(fill='#E5E5E5',col=NA)),
      if(is.finite(pct)) rectGrob(x=.10,y=.15,width=.57*pct/100,height=.09,just='left',
        gp=gpar(fill=bar_color,col=NA)) else nullGrob(),
      textGrob(if(is.finite(pct)) sprintf('%d pct',pct) else '-',x=.72,y=.15,hjust=0,
        gp=gpar(fontsize=5.5,col='#555555')))
  })
  arrangeGrob(grobs=cards,ncol=3)
}

rick_pct <- function(n, d) if (d > 0) sprintf('%.0f%%', 100 * n / d) else '-'
rick_prepare <- function(d) {
  if (!nrow(d)) {
    for (nm in c('pitch_call','play_result','family','PA_ID')) d[[nm]] <- character(0)
    for (nm in c('has_zone','in_zone','is_swing','is_whiff','is_bip','pre','two')) d[[nm]] <- logical(0)
    d$ev <- d$la <- numeric(0)
    return(d)
  }
  d$pitch_call <- pitcher_env$pitcher_pitch_call(d)
  d$play_result <- pitcher_bust_col(d, c('play_result', 'PlayResult', 'Result', 'PAResult'))
  d$has_zone <- is.finite(d$PlateX) & is.finite(d$PlateZ)
  d$in_zone <- pitcher_env$pitcher_zone_flag(d)
  d$is_swing <- pitcher_bust_is_swing(d, d$pitch_call)
  d$is_whiff <- d$pitch_call %in% 'StrikeSwinging'
  d$is_bip <- safe_is_bip(d$pitch_call, d$play_result)
  evla <- pitcher_bust_resolve_ev_la(d)
  d$ev <- evla$ev
  d$la <- evla$la
  d$family <- matchup_pitch_family(d$PitchType)
  d$family[d$family %in% 'Hard'] <- 'Fast'
  d$pre <- !is.na(d$StrikesBefore) & d$StrikesBefore < 2
  d$two <- !is.na(d$StrikesBefore) & d$StrikesBefore == 2
  d$PA_ID <- make_pa_id(d)
  d
}
rick_table <- function(d, formatted = TRUE) {
  pct <- if (formatted) rick_pct else function(n, d) if (d > 0) n/d else NA_real_
  types <- names(sort(table(d$PitchType), decreasing = TRUE))
  if (!length(types)) types <- 'No pitches'
  do.call(rbind, lapply(types, function(pt) {
    x <- d[d$PitchType == pt, ]
    v <- x$RelSpeed[is.finite(x$RelSpeed)]
    data.frame(Pitch = pt, `Velocity (max)` = if (length(v)) sprintf('%.1f (%.1f)', mean(v), max(v)) else '-',
      `Pre2k Usage%` = pct(sum(x$pre), sum(d$pre)),
      `IZ%` = pct(sum(x$pre & x$in_zone), sum(x$pre & x$has_zone)),
      `2k Usage%` = pct(sum(x$two), sum(d$two)),
      `2k IZ%` = pct(sum(x$two & x$in_zone), sum(x$two & x$has_zone)),
      `IZ Whiff%` = pct(sum(x$is_whiff & x$in_zone), sum(x$is_swing & x$in_zone, na.rm = TRUE)),
      `Chase%` = pct(sum(x$is_swing & x$has_zone & !x$in_zone, na.rm = TRUE), sum(x$has_zone & !x$in_zone)),
      check.names = FALSE)
  }))
}
rick_zone <- function(approach = FALSE) {
  ggplot() +
    annotate('rect', xmin = -.708, xmax = .708, ymin = 1.5, ymax = 3.5, fill = NA, colour = '#333333', linewidth = .4) +
    geom_segment(data = pitcher_env$home_plate_segments, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, linewidth=.35) +
    scale_x_reverse() +
    coord_fixed(xlim=if(approach) c(1.2,-1.2) else c(2,-2), ylim=if(approach) c(0,4) else c(0,5), expand=FALSE) + theme_void(base_size=8) +
    theme(plot.margin=margin(2,2,2,2), plot.title=element_text(hjust=.5, face='bold', size=8))
}
rick_heat <- function(d, metric) {
  keep <- switch(metric, Location = rep(TRUE,nrow(d)), `IZ Whiff` = d$is_whiff & d$in_zone,
    Chase = d$is_swing & !d$in_zone, `Exit velocity` = d$is_bip & is.finite(d$ev) & d$ev > 0)
  x <- d[which(keep & d$has_zone), ]
  heat_coord <- coord_fixed(ratio=1,xlim=c(1.7,-1.7),ylim=c(.05,4.5),expand=FALSE)
  p <- ggplot() + rick_zone()$layers + scale_x_reverse() + heat_coord +
    theme_void() + theme(plot.margin=margin(0,0,0,0))
  if (!nrow(x)) return(p + annotate('text',x=0,y=4.3,label='No events',size=2))
  # Fixed-bandwidth weighted kernel: EV squared emphasizes harder contact.
  gx <- seq(-2,2,length.out=45); gz <- seq(0,5,length.out=55)
  w <- if (metric == 'Exit velocity') (x$ev/100)^2 else rep(1,nrow(x))
  z <- (exp(-outer(gx,x$PlateX,'-')^2/(2*.24^2)) * rep(w,each=length(gx))) %*%
    t(exp(-outer(gz,x$PlateZ,'-')^2/(2*.28^2)))
  grid <- expand.grid(x=gx,y=gz); grid$value <- as.vector(z)/max(z)
  ggplot(grid,aes(x,y,fill=value)) + geom_raster() +
    scale_fill_gradientn(colours=c('#ffffff','#bddff1','#72b7ce','#ffe082','#ed713c','#a71930'),limits=c(0,1),guide='none') +
    rick_zone()$layers + scale_x_reverse() + heat_coord + theme_void() +
    theme(plot.margin=margin(0,0,0,0))
}
rick_movement <- function(d) {
  movement_values <- c(d$HB, d$IVB)
  movement_limit <- if (any(is.finite(movement_values) & abs(movement_values) >= 24)) 30 else 24
  cols <- pitcher_env$match_pitch_colors(unique(d$PitchType))
  avg <- d %>% filter(is.finite(HB),is.finite(IVB)) %>% group_by(PitchType) %>% summarise(HB=mean(HB),IVB=mean(IVB),.groups='drop')
  p <- ggplot(d,aes(HB,IVB,fill=PitchType)) + geom_point(shape=21,alpha=.25,size=1.5,na.rm=TRUE) +
    geom_hline(yintercept=0,linetype=3) + geom_vline(xintercept=0,linetype=3) +
    geom_point(data=avg,shape=21,size=4,na.rm=TRUE) + scale_fill_manual(values=cols) +
    scale_x_reverse(limits=c(movement_limit,-movement_limit),breaks=seq(-movement_limit,movement_limit,3)) +
    scale_y_continuous(breaks=seq(-movement_limit,movement_limit,3)) +
    coord_fixed(ylim=c(-movement_limit,movement_limit)) + labs(x='Horizontal break (in)',y='Induced vertical break (in)',title='Movement | Hitter View',fill=NULL) +
    theme_minimal(base_size=8) + theme(legend.position='bottom',legend.text=element_text(size=6),
      legend.key.size=unit(8,'pt'),plot.title=element_text(face='bold'),panel.grid.minor=element_blank())
  ref <- read.csv(file.path(APP_ROOT,'reference','d1_heater_movement_reference.csv'))
  types <- canonical_pitch_fuzzy(d$PitchType)
  counts <- table(factor(types,levels=c('Fastball','Sinker')))
  primary <- names(counts)[which.max(counts)]
  hands <- table(na.omit(d$PitcherThrows))
  hb <- mean(d$HB[which(types == primary)],na.rm=TRUE)
  hand <- if(length(hands)) names(hands)[which.max(hands)] else if(is.finite(hb) && hb<0) 'LHP' else if(is.finite(hb)) 'RHP' else NA_character_
  valid <- is.finite(d$RelHeight) & is.finite(d$RelSide)
  angle <- if(any(valid)) atan2(mean(d$RelHeight[valid])-5,mean(abs(d$RelSide[valid])))*180/pi else NA_real_
  bucket <- floor(angle/5+1e-10)*5
  ref <- ref[which(ref$hand == hand & ref$pitch_type == primary & ref$arm_angle_min == bucket & sum(counts)>0),]
  note <- 'D1 reference unavailable for this release / pitch mix'
  if(nrow(ref)) {
    p <- p + geom_point(data=ref,aes(HB,IVB),inherit.aes=FALSE,shape=1,size=6,stroke=1.2)
    note <- sprintf('Open circle: D1 %s %s, %.0f to %.0f deg arm angle',hand,primary,bucket,bucket+5)
  }
  # Infer each pitch's release angle using the same 5 ft shoulder baseline
  # as the D1 arm-angle comparison, retaining below-shoulder release angles.
  arms <- d %>% filter(is.finite(RelHeight),is.finite(RelSide)) %>%
    group_by(PitchType) %>% summarise(vertical=mean(RelHeight)-5,
      lateral=mean(abs(RelSide)),.groups='drop') %>%
    filter(vertical != 0 | lateral != 0) %>%
    mutate(angle=atan2(vertical,lateral),
      xend=ifelse(hand=='LHP',-1,1)*(movement_limit-1)*cos(angle),yend=(movement_limit-1)*sin(angle))
  if(nrow(arms) && !is.na(hand)) {
    p <- p + geom_segment(data=arms,aes(x=0,y=0,xend=xend,yend=yend,colour=PitchType),
      inherit.aes=FALSE,linetype='dashed',linewidth=.6,alpha=.8) +
      scale_colour_manual(values=cols,guide='none')
    angles <- paste(sprintf('%s %.1f deg',arms$PitchType,arms$angle*180/pi),collapse=' | ')
    note <- paste(note,paste(strwrap(paste('Inferred arm angles (5 ft shoulder):',angles),width=65),collapse='\n'),sep='\n')
  } else note <- paste(note,'Inferred arm angle unavailable: missing release / hand data',sep='\n')
  p + labs(caption=note) + theme(plot.caption=element_text(size=6))
}
rick_title <- function(label) grid::textGrob(label,x=.02,hjust=0,gp=gpar(fontsize=10,fontface='bold',col='#501214'))
rick_section <- function(label, content) arrangeGrob(rick_title(label),content,ncol=1,heights=c(.22,1))
rick_border <- function(content, color) {
  grobTree(grobTree(content,vp=viewport(width=.975,height=.975)),
    rectGrob(width=.997,height=.997,gp=gpar(fill=NA,col=color,lwd=3)))
}
rick_shapes <- function(shapes) {
  if (!is.list(shapes) || !length(shapes)) return(list())
  out <- lapply(head(shapes,100),function(s) {
    if (!is.list(s) || !all(c('id','zone','type','color','x','z','w','h') %in% names(s))) return(NULL)
    if (length(s$zone)!=1 || !s$zone %in% c('Pre2k','2k') || length(s$type)!=1 || !s$type %in% c('circle','square','rectangle')) return(NULL)
    vals <- suppressWarnings(as.numeric(unlist(s[c('x','z','w','h')])))
    if(length(vals)!=4 || any(!is.finite(vals))) return(NULL)
    w <- max(.15,min(2.4,vals[3])); h <- max(.15,min(4,vals[4]))
    if(s$type!='rectangle') w <- h <- min(w,h)
    list(id=substr(as.character(s$id)[1],1,100),zone=s$zone,type=s$type,
      color=if(identical(s$color,'red')) 'red' else 'green',
      x=max(-1.2+w/2,min(1.2-w/2,vals[1])),z=max(h/2,min(4-h/2,vals[2])),w=w,h=h)
  })
  Filter(Negate(is.null),out)
}
rick_approach_plot <- function(zone, shapes=list()) {
  p <- rick_zone(TRUE)+labs(title=zone)
  for(s in rick_shapes(shapes)) {
    if(s$zone!=zone) next
    color <- if(s$color=='red') '#cf202f' else '#18833c'
    if(s$type=='circle') {
      theta <- seq(0,2*pi,length.out=100)
      pts <- data.frame(x=s$x+s$w/2*cos(theta),y=s$z+s$h/2*sin(theta))
      p <- p+geom_polygon(data=pts,aes(x,y),inherit.aes=FALSE,fill=scales::alpha(color,.27),colour=color,linewidth=.6)
    } else p <- p+annotate('rect',xmin=s$x-s$w/2,xmax=s$x+s$w/2,ymin=s$z-s$h/2,ymax=s$z+s$h/2,
      fill=scales::alpha(color,.27),colour=color,linewidth=.6)
  }
  p
}
rick_document_path <- function(key) file.path(if (exists('base_team_season_import_root', mode='function')) base_team_season_import_root() else file.path(APP_ROOT,'outputs'),'rick_annotations',paste0(digest::digest(key,algo='sha256'),'.rds'))
rick_read_document <- function(key) {
  path <- rick_document_path(key)
  blank <- list(notes='',shapes=list())
  if(!file.exists(path)) return(blank)
  tryCatch({doc <- readRDS(path);list(notes=as.character(doc$notes %||% ''),shapes=rick_shapes(doc$shapes))},error=function(e) blank)
}
rick_save_document <- function(key, doc) {
  path <- rick_document_path(key);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  temp <- tempfile(tmpdir=dirname(path));on.exit(unlink(temp))
  saveRDS(list(notes=doc$notes,shapes=rick_shapes(doc$shapes)),temp)
  if(!file.rename(temp,path)) stop('Could not save annotations.')
}
rick_sheet <- function(d, pitcher, side, notes='', shapes=list(), pitcher_hand=NULL) {
  d <- rick_prepare(d)
  tbl <- rick_table(d); types <- tbl$Pitch
  pa <- d %>% group_by(PA_ID) %>% slice_tail(n=1) %>% ungroup()
  results <- pa_outcome_summary(pa)
  header_values <- data.frame(
    `K%`=safe_ratio(sum(results$K),nrow(pa)),
    `BB%`=safe_ratio(sum(results$BB),nrow(pa)),
    `GB%`=safe_ratio(sum(d$is_bip & is.finite(d$la) & d$la<5,na.rm=TRUE),sum(d$is_bip & is.finite(d$la),na.rm=TRUE)),check.names=FALSE)
  header_stats <- rick_header_stats(header_values)
  border_color <- if(side=='L') '#cf202f' else '#111111'
  throws <- table(na.omit(pitcher_hand %||% d$PitcherThrows))
  name_color <- if(length(throws) && names(throws)[which.max(throws)]=='LHP') '#cf202f' else '#111111'
  # Full-width 33 pt heading (three times the original 11 pt label).
  name_lines <- strwrap(pitcher,width=23)
  header <- arrangeGrob(textGrob(paste(name_lines,collapse='\n'),x=.01,hjust=0,
      gp=gpar(fontsize=33,fontface='bold',col=name_color,lineheight=.95)),
    textGrob(sprintf('vs %sHH',side),x=.19,hjust=0,gp=gpar(fontsize=33,fontface='bold',col=border_color)),
    ncol=2,widths=c(.68,.32))
  table <- rick_shaded_table(tbl,rick_table(d,formatted=FALSE),
    c(NA,NA,'usage','performance_pre2k_zone_pct','usage','performance_2k_zone_pct','performance_izwhiff_pct','pitchtype_chase_rate'),
    pitch_types=canonical_pitch_fuzzy(types),size=6)
  table$widths <- unit(c(1.1,1.3,1, .65,1,.8,1,.8)/7.65,'npc')
  # Wrap column headings without changing the requested metrics.
  for(i in which(table$layout$name=='colhead-fg')) table$grobs[[i]]$label <- gsub(' ', '\n',table$grobs[[i]]$label)
  table$heights[1] <- unit(9,'mm')
  metrics <- c('Location','IZ Whiff','Chase','Exit velocity')
  heat <- list(nullGrob()); heat <- c(heat,lapply(metrics,function(m) textGrob(m,gp=gpar(fontsize=6,fontface='bold'))))
  for(pt in types) {
    heat <- c(heat,list(textGrob(pt,gp=gpar(fontsize=6))),lapply(metrics,function(m) ggplotGrob(rick_heat(d[d$PitchType==pt,],m))))
  }
  heats <- arrangeGrob(grobs=heat,ncol=5,widths=c(.48,1,1,1,1),heights=c(.18,rep(1,length(types))))
  filters <- list('First Pitch'=d$BallsBefore==0 & d$StrikesBefore==0,
    'Hitter Ahead'=d$BallsBefore>d$StrikesBefore,'Two Strikes'=d$two)
  counts <- lapply(names(filters),function(label) {
    subset <- d[which(filters[[label]]),]
    plots <- lapply(c('Fast','Breaking','Soft'),function(f) {
      x <- subset[which(subset$family==f),]
      p <- ggplot() + rick_zone()$layers + scale_x_reverse() +
        coord_fixed(ratio=1,xlim=c(2,-2),ylim=c(0,5),expand=FALSE) +
        theme_void(base_size=8) +
        theme(plot.margin=margin(2,4,2,4),plot.title=element_text(hjust=.5,face='bold',size=8)) +
        geom_point(data=x,aes(PlateX,PlateZ,fill=PitchType),shape=21,size=1.3,alpha=.65,na.rm=TRUE) +
        scale_fill_manual(values=pitcher_env$match_pitch_colors(unique(d$PitchType)),guide='none') + labs(title=f)
      rates <- data.frame(`Usage%`=safe_ratio(nrow(x),nrow(subset)),
        `IZ%`=safe_ratio(sum(x$in_zone),sum(x$has_zone)),check.names=FALSE)
      zone_metric <- if (label == 'Two Strikes') 'performance_2k_zone_pct' else 'performance_zone_pct'
      rate_table <- rick_metric_table(rates,c('usage',zone_metric),size=6)
      # Eight bold characters per column, plus a small cell inset; center the table.
      cell_width <- grobWidth(textGrob('00000000',gp=gpar(fontsize=6,fontface='bold'))) + unit(2,'mm')
      rate_table$widths <- rep(cell_width,2)
      compact_table <- grobTree(rate_table,vp=viewport(width=sum(rate_table$widths)))
      arrangeGrob(ggplotGrob(p),compact_table,ncol=1,heights=unit.c(unit(1,'null'),unit(.25,'in')))
    })
    heading <- textGrob(label,y=.60,gp=gpar(fontsize=16,fontface='bold'))
    underlined_heading <- grobTree(heading,segmentsGrob(
      x0=unit(.5,'npc')-grobWidth(heading)/2,x1=unit(.5,'npc')+grobWidth(heading)/2,
      y0=unit(.60,'npc')-grobHeight(heading)/2-unit(2,'pt'),
      y1=unit(.60,'npc')-grobHeight(heading)/2-unit(2,'pt'),gp=gpar(lwd=1.2)))
    arrangeGrob(underlined_heading,arrangeGrob(grobs=plots,ncol=3),ncol=1,
      heights=unit.c(unit(.32,'in'),unit(1,'null')))
  })
  count <- arrangeGrob(rick_title('Count Breakdown'),arrangeGrob(grobs=counts,ncol=1),
    ncol=1,heights=unit.c(unit(.24,'in'),unit(1,'null')))
  right <- arrangeGrob(rick_border(table,border_color),rick_border(heats,border_color),rick_border(count,border_color),ncol=1,heights=unit.c(unit(max(.72,.15*nrow(tbl)+.36),'in'),
    unit(max(2.4,length(types)*.84),'in'),unit(1,'null')))
  zones <- arrangeGrob(ggplotGrob(rick_approach_plot('Pre2k',shapes)),ggplotGrob(rick_approach_plot('2k',shapes)),ncol=2)
  note_lines <- unlist(lapply(strsplit(notes,'\n',fixed=TRUE)[[1]],function(s) if(nzchar(s)) strwrap(s,width=46) else ''))
  note_box <- grobTree(textGrob(paste(note_lines,collapse='\n'),x=.04,y=.96,hjust=0,vjust=1,gp=gpar(fontsize=9,lineheight=1.3)))
  left <- arrangeGrob(rick_border(ggplotGrob(rick_movement(d)),border_color),
    rick_border(rick_section('Approach / Go Zones',zones),border_color),
    rick_border(rick_section('Notes / Approach',note_box),border_color),ncol=1,heights=c(3.2,2.4,2.2))
  page <- arrangeGrob(header,header_stats,
    arrangeGrob(left,right,ncol=2,widths=c(1,1.35)),
    textGrob(paste('Locations: catcher POV | IZ = Pre2k zone rate | Hitter Ahead = balls > strikes | EV weight = speed squared',
      'D1 percentiles: green = lower, red = higher (BB reversed) | Header bars show percentile rank',sep='\n'),gp=gpar(fontsize=5.5)),
    ncol=1,heights=unit.c(unit(if(length(name_lines)>1) 1.05 else .65,'in'),unit(.48,'in'),unit(1,'null'),unit(.28,'in')),padding=unit(.1,'in'))
  grobTree(page, vp=viewport(width=.96,height=.97))
}
rick_ui <- function() tabPanel('Rick Advance Sheet',value='rick_advance',
  includeScript(file.path(APP_ROOT,'www','rick-editor.js')),
  div(class='report-shell',div(class='report-toolbar',uiOutput('rick_pitcher_ui'),
    selectInput('rick_side','Batter side',c('vs LHH'='L','vs RHH'='R'),selected='R'),
    downloadButton('rick_pdf','Download PDF (RHH + LHH)')),
    tags$details(tags$summary('Edit notes and approach / go zones'),
      div(id='rick_editor_panel',style='border:2px solid #111;padding:12px;margin:8px 0;',
        div(style='display:flex;gap:18px;flex-wrap:wrap;',
          div(style='flex:1;min-width:280px;',tags$label(`for`='rick_editor_notes','Notes / Approach'),
            tags$textarea(id='rick_editor_notes',class='form-control',rows=12,style='width:100%;')),
          div(style='flex:1.5;min-width:350px;',
            div(style='display:flex;gap:8px;flex-wrap:wrap;align-items:center;',
              tags$label(`for`='rick_target','Add to'),tags$select(id='rick_target',tags$option('Pre2k'),tags$option('2k')),
              lapply(c('Circle','Square','Rectangle'),function(shape) tags$button(type='button',class='btn btn-outline-secondary',
                draggable='true',`data-rick-shape`=tolower(shape),shape)),
              tags$button(id='rick_clear',type='button',class='btn btn-outline-secondary','Clear')),
            helpText('Click a shape button or drag it onto a zone. Drag shapes to move; drag a corner grip to resize. Click a placed shape to switch green/red. Clear removes all shapes.'),
            tags$svg(id='rick_canvas',viewBox='0 0 600 440',role='img',`aria-label`='Editable Pre2k and 2k go zones',
              style='width:100%;max-height:520px;touch-action:none;user-select:none;'))),
        tags$button(id='rick_save',type='button',class='btn btn-primary','Save notes & shapes'),
        textOutput('rick_save_status'),
        helpText('Saved per pitcher and batter side for future sessions. PDF export includes both splits: RHH first, LHH second, with each split’s notes and shapes.'))),
    div(style='overflow-x:auto;',plotOutput('rick_sheet',width='1105px',height='1430px'))))
rick_server <- function(input,output,session,pitcher_data) {
  output$rick_pitcher_ui <- renderUI({
    d <- pitcher_data(); choices <- sort(unique(na.omit(d$PitcherName)))
    selectInput('rick_pitcher','Pitcher',choices)
  })
  documents <- reactiveValues(); status <- reactiveVal('')
  key <- reactive({req(input$rick_pitcher,input$rick_side);paste(input$rick_pitcher,input$rick_side,sep='|')})
  observeEvent(key(),{
    k <- key()
    if(is.null(documents[[k]])) documents[[k]] <- rick_read_document(k)
    doc <- documents[[k]];status('')
    session$sendCustomMessage('rick_document',list(key=k,notes=doc$notes,shapes=unname(doc$shapes),side=input$rick_side))
  },priority=10)
  accept_document <- function(payload) {
    req(payload$key,identical(payload$key,key()))
    doc <- list(notes=substr(as.character(payload$notes %||% '')[1],1,10000),shapes=rick_shapes(payload$shapes))
    documents[[key()]] <- doc
    doc
  }
  observeEvent(input$rick_editor_change,{
    accept_document(input$rick_editor_change);status('Unsaved changes — included in PDF export.')
  })
  observeEvent(input$rick_editor_save,{
    doc <- accept_document(input$rick_editor_save)
    tryCatch({rick_save_document(key(),doc);status('Notes and shapes saved.')},
      error=function(e){status('Save failed. Your edits remain available in this session.');showNotification(conditionMessage(e),type='error')})
  },priority=-1)
  output$rick_save_status <- renderText(status())
  report <- reactive({
    req(input$rick_pitcher,input$rick_side)
    d <- pitcher_data() %>% filter(PitcherName==input$rick_pitcher,BatterSide==input$rick_side)
    shiny::validate(shiny::need(nrow(d)>0,'No pitches for this pitcher and batter side.'))
    doc <- documents[[key()]] %||% list(notes='',shapes=list())
    rick_sheet(d,input$rick_pitcher,input$rick_side,doc$notes,doc$shapes)
  })
  output$rick_sheet <- renderPlot({grid.newpage();grid.draw(report())},res=130)
  export_pages <- reactive({
    req(input$rick_pitcher)
    d <- pitcher_data() %>% filter(PitcherName==input$rick_pitcher)
    shiny::validate(shiny::need(nrow(d)>0,'No pitches for this pitcher.'))
    lapply(c('R','L'),function(side) {
      k <- paste(input$rick_pitcher,side,sep='|')
      # Prefer current-session drafts, including edits on the other split;
      # load saved annotations when that split has not been opened this session.
      doc <- documents[[k]] %||% rick_read_document(k)
      split <- d %>% filter(BatterSide==side)
      rick_sheet(split,input$rick_pitcher,side,doc$notes,doc$shapes,pitcher_hand=d$PitcherThrows)
    })
  })
  output$rick_pdf <- downloadHandler(
    filename=function() paste0('Rick Advance Sheet - ',filename_part(input$rick_pitcher),' - RHH and LHH.pdf'),
    content=function(file){
      pages <- export_pages()
      pdf(file,width=8.5,height=11);on.exit(dev.off())
      for(page in pages) {grid.newpage();grid.draw(page)}
    })
}
