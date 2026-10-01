# Shared roster identity and pitch-level import. Raw vendor history stays intact.
player_name_key <- function(x) {
  x <- trimws(as.character(x))
  comma <- !is.na(x) & grepl(",", x, fixed = TRUE)
  x[comma] <- sub("^([^,]+),\\s*(.+)$", "\\2 \\1", x[comma])
  tolower(gsub("[[:space:]]+", " ", x))
}
fall_roster_path <- function() Sys.getenv("FALL_ROSTER_FILE", file.path(dirname(getwd()), "2026 Fall Roster Template.xlsx"))
roster_alias_path <- function() file.path(Sys.getenv("VALD_APP_ROOT", getwd()), "config", "player_aliases.csv")
file_signature <- function(paths) {
  info <- file.info(paths)
  paste(paths, info$size, as.numeric(info$mtime), collapse = "|")
}
read_fall_roster <- function(path = fall_roster_path(), alias_path = roster_alias_path()) {
  tryCatch({
    raw <- readxl::read_excel(path, sheet = "Roster", skip = 1, col_types = "text")
    stopifnot(all(c("First", "Last", "Position") %in% names(raw)))
    raw <- raw[!is.na(raw$First) & !is.na(raw$Last) & nzchar(trimws(raw$First)) & nzchar(trimws(raw$Last)), ]
    r <- data.frame(name = paste(trimws(raw$First), trimws(raw$Last)), position = raw$Position)
    r$key <- player_name_key(r$name)
    if (!nrow(r) || anyDuplicated(r$key)) stop("Roster must contain unique full names.")
    r$role <- ifelse(grepl("RHP|LHP|^P$", r$position), "Pitchers", "Hitters")
    aliases <- data.frame(alias = character(), roster_name = character())
    if (file.exists(alias_path)) aliases <- read.csv(alias_path, stringsAsFactors = FALSE)
    aliases$alias <- player_name_key(aliases$alias)
    aliases$key <- player_name_key(aliases$roster_name)
    if (anyDuplicated(aliases$alias) || any(!aliases$key %in% r$key)) stop("Invalid or ambiguous roster alias mapping.")
    list(players = r, aliases = aliases, error = NULL, source = basename(path))
  }, error = function(e) list(players = data.frame(name=character(),position=character(),key=character(),role=character()),
    aliases = data.frame(alias=character(),key=character()), error = paste("Fall roster:", conditionMessage(e)), source = basename(path)))
}
roster_keys <- function(names, roster) {
  keys <- player_name_key(names)
  i <- match(keys, roster$aliases$alias)
  keys[!is.na(i)] <- roster$aliases$key[i[!is.na(i)]]
  keys
}
roster_filter <- function(d, roster, name_col = "name") {
  if (is.null(d) || !nrow(d)) return(d)
  if (!name_col %in% names(d)) return(d[FALSE, , drop=FALSE])
  keys <- roster_keys(d[[name_col]], roster)
  keep <- !is.na(keys) & keys %in% roster$players$key
  d <- d[keep, , drop=FALSE]; keys <- keys[keep]
  d[[name_col]] <- roster$players$name[match(keys, roster$players$key)]
  if ("key" %in% names(d)) d$key <- keys
  roles <- roster$players$role[match(keys, roster$players$key)]
  for (nm in intersect(c("primaryGroup", "roleGroup", "role"), names(d))) d[[nm]] <- roles
  d
}
trackman_files <- function(root) sort(list.files(root, pattern = "trackman.*\\.csv$", full.names=TRUE, ignore.case=TRUE))
trackman_metrics <- c(fb_velocity="Fastball/sinker velocity (mph)", fb_spin="Fastball/sinker spin rate (rpm)",
                     bb_velocity="Breaking-ball velocity (mph)", bb_spin="Breaking-ball spin rate (rpm)")
trackman_daily <- function(pitches) {
  empty <- tibble::tibble(key=character(),name=character(),date=as.Date(character()),pitch_group=character(),
    pitch_count=integer(),velocity_n=integer(),spin_n=integer(),velocity_mph=double(),spin_rpm=double(),pitcher_ids=character(),source_files=character())
  if (!nrow(pitches)) return(empty)
  safe_mean <- function(x) if(any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  pitches |>
    dplyr::filter(!is.na(pitch_group)) |>
    dplyr::group_by(key, name, date, pitch_group) |>
    dplyr::summarise(pitch_count=dplyr::n(), velocity_n=sum(is.finite(velocity_mph)), spin_n=sum(is.finite(spin_rpm)),
      velocity_mph=safe_mean(velocity_mph), spin_rpm=safe_mean(spin_rpm),
      pitcher_ids=paste(sort(unique(PitcherId)),collapse=";"), source_files=paste(sort(unique(source_file)),collapse=";"), .groups="drop") |>
    dplyr::arrange(key,date,pitch_group)
}
trackman_read <- function(root, roster) {
  out <- list(pitches=data.frame(), daily=trackman_daily(data.frame()), errors=character(), notes=character(), unmatched=character(), duplicate_count=0L, excluded_tags=character(), files=character())
  files <- trackman_files(root)
  if (!length(files)) {out$errors <- "No TrackMan CSV found (filename must contain trackman)."; return(out)}
  # Newer copies win when cumulative exports overlap or tags are corrected.
  files <- files[order(file.info(files)$mtime, files)]
  parts <- lapply(files, function(path) tryCatch({
    d <- read.csv(path, check.names=FALSE, stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM", colClasses="character", na.strings=c("","NA","NaN"))
    required <- c("PitchUID","Pitcher","PitcherId","Date","TaggedPitchType","RelSpeed","SpinRate")
    if(!all(required %in% names(d))) stop(paste("Missing columns:",paste(setdiff(required,names(d)),collapse=", ")))
    d <- d[, required]; d$source_file <- basename(path); d
  },error=function(e) {out$errors <<- c(out$errors,paste(basename(path),conditionMessage(e))); NULL}))
  d <- dplyr::bind_rows(parts)
  out$files <- basename(files)
  if(!nrow(d)) return(out)
  placeholder <- is.na(d$PitchUID) & is.na(d$Pitcher) & is.na(d$Date) & is.na(d$RelSpeed) & is.na(d$SpinRate)
  if(any(placeholder)) out$notes <- c(out$notes,paste(sum(placeholder),"empty placeholder rows skipped."))
  d <- d[!placeholder,,drop=FALSE]
  has_measurement <- is.finite(suppressWarnings(as.numeric(d$RelSpeed))) | is.finite(suppressWarnings(as.numeric(d$SpinRate)))
  bad_uid <- is.na(d$PitchUID) | !nzchar(trimws(d$PitchUID))
  if(any(bad_uid & has_measurement)) out$errors <- c(out$errors,paste(sum(bad_uid & has_measurement),"measured TrackMan rows excluded: missing PitchUID."))
  if(any(bad_uid & !has_measurement)) out$notes <- c(out$notes,paste(sum(bad_uid & !has_measurement),"rows without pitch IDs or velocity/spin measurements skipped."))
  d <- d[!bad_uid,,drop=FALSE]
  out$duplicate_count <- sum(duplicated(d$PitchUID))
  d <- d[!duplicated(d$PitchUID,fromLast=TRUE),,drop=FALSE]
  d$name <- d$Pitcher; d$key <- roster_keys(d$name,roster)
  out$unmatched <- sort(unique(d$Pitcher[!d$key %in% roster$players$key]))
  d <- roster_filter(d,roster)
  d$date <- as.Date(substr(d$Date,1,10),format="%Y-%m-%d")
  us_date <- is.na(d$date) & !is.na(d$Date)
  d$date[us_date] <- as.Date(d$Date[us_date],format="%m/%d/%Y")
  has_measurement <- is.finite(suppressWarnings(as.numeric(d$RelSpeed))) | is.finite(suppressWarnings(as.numeric(d$SpinRate)))
  if(any(is.na(d$date) & has_measurement)) out$errors <- c(out$errors,paste(sum(is.na(d$date) & has_measurement),"measured TrackMan rows excluded: invalid Date."))
  if(any(is.na(d$date) & !has_measurement)) out$notes <- c(out$notes,paste(sum(is.na(d$date) & !has_measurement),"rows without dates or velocity/spin measurements skipped."))
  d <- d[!is.na(d$date),,drop=FALSE]
  tag <- tolower(trimws(d$TaggedPitchType))
  d$pitch_group <- ifelse(tag %in% c("fastball","sinker"), "Fastball/sinker",
                         ifelse(tag %in% c("cutter","slider","curveball","sweeper"), "Breaking ball", NA_character_))
  out$excluded_tags <- sort(unique(d$TaggedPitchType[is.na(d$pitch_group)]))
  d$velocity_mph <- suppressWarnings(as.numeric(d$RelSpeed))
  d$spin_rpm <- suppressWarnings(as.numeric(d$SpinRate))
  for(nm in c("velocity_mph","spin_rpm")) d[[nm]][!is.finite(d[[nm]]) | d[[nm]] <= 0] <- NA_real_
  out$pitches <- d
  out$daily <- trackman_daily(d)
  out
}
trackman_series <- function(daily) {
  if(!nrow(daily)) return(data.frame(key=character(),name=character(),date=as.Date(character()),metric=character(),value=double(),n=integer(),pitch_count=integer()))
  fields <- c("key","name","date","pitch_count","pitcher_ids","source_files")
  velocity <- daily[,fields]; spin <- velocity
  velocity$metric <- ifelse(daily$pitch_group=="Fastball/sinker","fb_velocity","bb_velocity")
  spin$metric <- ifelse(daily$pitch_group=="Fastball/sinker","fb_spin","bb_spin")
  velocity$value <- daily$velocity_mph; velocity$n <- daily$velocity_n
  spin$value <- daily$spin_rpm; spin$n <- daily$spin_n
  dplyr::bind_rows(velocity,spin) |> dplyr::arrange(key,date,metric)
}
