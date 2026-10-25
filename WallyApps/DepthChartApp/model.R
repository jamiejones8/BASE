season_year <- function(date = Sys.Date(), rollover_month = 8L) {
  as.integer(format(date, '%Y')) + as.integer(as.integer(format(date, '%m')) >= rollover_month)
}
parse_positions <- function(value) {
  positions <- unique(trimws(toupper(strsplit(value, '[/,;]')[[1]])))
  positions <- positions[nzchar(positions)]
  positions[positions == 'IF'] <- 'INF'
  positions
}
read_roster <- function(path) {
  d <- read.csv(path, skip = 2, stringsAsFactors = FALSE, check.names = FALSE)
  players <- lapply(seq_len(nrow(d)), function(i) list(id = paste0('roster-', i), first = trimws(d$First[i]), last = trimws(d$Last[i]), positions = parse_positions(d$Position[i]), bats = trimws(strsplit(toupper(d[['B/T']][i]), '/', fixed=TRUE)[[1]][1]), throws = trimws(strsplit(toupper(d[['B/T']][i]), '/', fixed=TRUE)[[1]][2]), entryYear = 2027L, entryClass = match(toupper(d$Class[i]), c('FR','SO','JR','SR','5TH')), status = 'Signed', recruit = FALSE))
  measurements_path <- file.path(dirname(path),'pitcher-measurements.json')
  if (file.exists(measurements_path)) {
    measurements <- jsonlite::fromJSON(measurements_path,simplifyVector=FALSE)
    players <- lapply(players,function(p) {for (key in names(measurements[[p$id]])) p[[key]] <- measurements[[p$id]][[key]];p})
  }
  players
}
project_class <- function(player, year) player$entryClass + year - player$entryYear
valid_money <- function(value) {
  is.numeric(value) && length(value)==1L && is.finite(value) && value>=0 && value<=1e9 && abs(value*100-round(value*100))<0.0001
}
validate_money_schedule <- function(schedule) {
  stopifnot(is.list(schedule))
  if (length(schedule)) stopifnot(length(names(schedule))==length(schedule),
    all(grepl('^[0-9]{4}$',names(schedule))), all(as.integer(names(schedule)) %in% 2027:2200))
  for (value in schedule) stopifnot(valid_money(value))
}
validate_pitcher_details <- function(p) {
  for (key in c('heightIn','releaseHeightFt','releaseSideFt')) if(!is.null(p[[key]])) {
    value <- p[[key]]
    stopifnot(is.numeric(value),length(value)==1,is.finite(value))
    if(key=='heightIn') stopifnot(value>=36,value<=100)
    if(key=='releaseHeightFt') stopifnot(value>0,value<=10)
    if(key=='releaseSideFt') stopifnot(value>=0,value<=10)
  }
}
validate_state <- function(s, roster_ids) {
  stopifnot(is.list(s), is.list(s$profiles), is.list(s$charts))
  ids <- character()
  for (p in s$profiles) {
    stopifnot(is.character(p$id), length(p$id)==1, startsWith(p$id,'recruit-'), !(p$id %in% ids),
      is.character(p$first), nzchar(trimws(p$first)), nchar(p$first)<=60,
      is.character(p$last), nzchar(trimws(p$last)), nchar(p$last)<=60,
      p$entryClass %in% 1:5, p$entryYear >= 2027, p$entryYear <= 2200,
      p$status %in% c('Recruiting','Offered','Committed','Signed'),
      length(p$positions)>0, all(unlist(p$positions) %in% c('C','1B','2B','3B','SS','LF','CF','RF','DH','INF','OF','RHP','LHP','P')))
    validate_pitcher_details(p)
    if (!is.null(p$offerPackage)) stopifnot(valid_money(p$offerPackage))
    if (!is.null(p$bats)) stopifnot(p$bats %in% c('', 'R', 'L', 'S'))
    if (!is.null(p$throws)) stopifnot(p$throws %in% c('', 'R', 'L'))
    ids <- c(ids,p$id)
  }
  if (!is.null(s$playerDetails)) {
    stopifnot(is.list(s$playerDetails),all(names(s$playerDetails) %in% roster_ids))
    for (p in s$playerDetails) {
      stopifnot(is.list(p),all(names(p) %in% c('positions','bats','throws','heightIn','releaseHeightFt','releaseSideFt')))
      validate_pitcher_details(p)
      if (!is.null(p$positions)) stopifnot(length(p$positions)>0,all(unlist(p$positions) %in% c('C','1B','2B','3B','SS','LF','CF','RF','DH','INF','OF','P','RHP','LHP')))
      if (!is.null(p$throws)) stopifnot(p$throws %in% c('','R','L'))
      if (!is.null(p$bats)) stopifnot(p$bats %in% c('','R','L','S'))
    }
  }
  if (!is.null(s$funding)) {
    stopifnot(is.list(s$funding))
    validate_money_schedule(s$funding$budgets)
    stopifnot(is.list(s$funding$awards))
    if (length(s$funding$awards)) stopifnot(all(names(s$funding$awards) %in% c(ids,roster_ids)))
    for (schedule in s$funding$awards) validate_money_schedule(schedule)
  }
  known_ids <- c(ids,roster_ids)
  if (!is.null(s$departures)) {
    stopifnot(is.list(s$departures), all(names(s$departures) %in% known_ids))
    for (y in s$departures) stopifnot(length(y)==1, is.numeric(y), y %in% 2027:2200)
  }
  check_years <- function(store) {
    if (length(store)) stopifnot(is.list(store),length(names(store))==length(store),all(grepl('^[0-9]{4}$',names(store))),all(as.integer(names(store)) %in% 2027:2200))
  }
  check_charts <- function(charts) {
    check_years(charts)
    for (y in names(charts)) {
      ch <- charts[[y]]
      stopifnot(is.list(ch), all(names(ch) %in% c('C','1B','2B','3B','SS','LF','CF','RF','DH','SP','Bullpen','LongRelief','LHP','RHP','Closer')))
      for (v in ch) stopifnot(all(unlist(v) %in% known_ids), !anyDuplicated(unlist(v)))
      stopifnot(length(ch$SP)<=4, length(ch$Closer)<=3,length(ch$LongRelief)<=3)
      arms <- unlist(ch[c('SP','Bullpen','LongRelief','LHP','RHP','Closer')]); stopifnot(!anyDuplicated(arms))
      if (length(s$departures)) for (id in names(s$departures)) if(as.integer(y)>=s$departures[[id]]) stopifnot(!id %in% unlist(ch))
    }
  }
  check_charts(s$charts)
  if (!is.null(s$matchups)) {
    stopifnot(is.list(s$matchups),all(names(s$matchups) %in% c('v LHP','v RHP')))
    for (charts in s$matchups) check_charts(charts)
  }
  if (!is.null(s$lineupOrders)) {
    stopifnot(is.list(s$lineupOrders),all(names(s$lineupOrders) %in% c('General','v LHP','v RHP')))
    for (store in s$lineupOrders) {
      check_years(store)
      for (order in store) stopifnot(length(order)==9,!anyDuplicated(unlist(order)),setequal(unlist(order),c('C','1B','2B','3B','SS','LF','CF','RF','DH')))
    }
  }
  for (edits in list(s$planEdits,s$lineupEdits)) if(!is.null(edits)) {
    stopifnot(is.list(edits),all(names(edits) %in% c('General','v LHP','v RHP')))
    for (store in edits) {check_years(store);for (flag in store) stopifnot(is.logical(flag),length(flag)==1,!is.na(flag))}
  }
  TRUE
}
