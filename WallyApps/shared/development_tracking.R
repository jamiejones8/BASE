# Persistent season-specific goal plans and shared progress rules.
development_season_rank <- function(code) {
  valid <- grepl("^(PS|S|F)[0-9]{2}$", code)
  year <- suppressWarnings(as.integer(sub("^[A-Z]+", "", code)))
  phase <- ifelse(grepl("^PS", code), 0L, ifelse(grepl("^S", code), 1L, 2L))
  ifelse(valid, year * 3L + phase, NA_integer_)
}

development_next_season <- function(code) {
  year <- suppressWarnings(as.integer(sub("^[A-Z]+", "", code)))
  if (length(code) != 1 || !is.finite(development_season_rank(code))) return(NA_character_)
  if (startsWith(code, "PS")) paste0("S", year) else if (startsWith(code, "S")) paste0("F", year) else paste0("PS", year + 1L)
}

development_previous_season <- function(code) {
  year <- suppressWarnings(as.integer(sub("^[A-Z]+", "", code)))
  if (length(code) != 1 || !is.finite(development_season_rank(code))) return(NA_character_)
  if (startsWith(code, "PS")) paste0("F", year - 1L) else if (startsWith(code, "S")) paste0("PS", year) else paste0("S", year)
}

development_season_label <- function(code) {
  year <- suppressWarnings(as.integer(sub("^[A-Z]+", "", code))) + 2000L
  paste(year, ifelse(startsWith(code, "PS"), "Scrimmages", ifelse(startsWith(code, "S"), "Season", "Fall")))
}

development_baseline_seasons <- function(seasons) {
  codes <- unname(seasons)
  keep <- is.finite(development_season_rank(codes)) & development_season_rank(codes) >= development_season_rank("S26")
  codes <- codes[keep]
  codes <- codes[order(development_season_rank(codes))]
  stats::setNames(codes, development_season_label(codes))
}

development_team_key <- function() {
  code <- tryCatch(TEAM_CONFIG$data_code, error = function(e) NULL)
  if (is.null(code) || !length(code) || !nzchar(code[1])) code <- "Texas State"
  development_name_key(code[1])
}

development_store_path <- function() {
  override <- Sys.getenv("BASE_DEVELOPMENT_GOALS_FILE", "")
  if (nzchar(override)) return(override)
  imports <- tryCatch(TEAM_CONFIG$data$team_season_import_dir, error = function(e) NULL)
  root <- if (!is.null(imports) && length(imports) && nzchar(imports[1])) dirname(imports[1]) else
    if (dir.exists("/base-data")) "/base-data/app_state" else file.path(development_root, "..", "..", "app_state")
  file.path(root, "development-goals.sqlite")
}

# Resolve missing descendants through their closest existing ancestor. This
# keeps the original configured path while detecting symlinks outside a volume.
development_storage_real_path <- function(path) {
  path <- path.expand(path)
  if(file.exists(path) || dir.exists(path)) return(normalizePath(path,winslash="/",mustWork=TRUE))
  parent <- dirname(path)
  if(identical(parent,path)) stop("Could not resolve goal storage path.",call.=FALSE)
  file.path(development_storage_real_path(parent),basename(path))
}

development_storage_is_mount <- function(path, mountinfo = NULL) {
  if(is.null(mountinfo)) {
    if(!file.exists("/proc/self/mountinfo")) return(FALSE)
    mountinfo <- readLines("/proc/self/mountinfo",warn=FALSE)
  }
  points <- vapply(strsplit(mountinfo," ",fixed=TRUE),function(fields) if(length(fields)>=5L) fields[5] else "",character(1))
  for(pair in list(c("\\040"," "),c("\\011","\t"),c("\\134","\\"))) points <- gsub(pair[1],pair[2],points,fixed=TRUE)
  development_storage_real_path(path) %in% vapply(points[nzchar(points)],development_storage_real_path,character(1))
}

development_storage_guard <- function(path = development_store_path(),
    volume = Sys.getenv("RAILWAY_VOLUME_MOUNT_PATH",""),
    railway = any(nzchar(Sys.getenv(c("RAILWAY_SERVICE_ID","RAILWAY_DEPLOYMENT_ID","RAILWAY_ENVIRONMENT_ID"),""))),
    mountinfo = NULL) {
  if(!railway && !nzchar(volume)) return(invisible(development_storage_real_path(path)))
  if(!nzchar(volume) || !dir.exists(volume) || !development_storage_is_mount(volume,mountinfo)) {
    stop("Development Goals require the existing Railway persistent volume to be mounted. Refusing temporary goal storage.",call.=FALSE)
  }
  root <- development_storage_real_path(volume)
  actual <- development_storage_real_path(path)
  if(!startsWith(actual,paste0(sub("/+$","",root),"/"))) {
    stop("BASE_DEVELOPMENT_GOALS_FILE must remain inside the attached persistent volume: ",root,call.=FALSE)
  }
  if(file.access(root,2L)!=0L || (file.exists(path) && file.access(path,2L)!=0L)) {
    stop("Persistent Development Goals storage is not writable.",call.=FALSE)
  }
  parent <- dirname(actual)
  if(!dir.exists(parent) && !dir.create(parent,recursive=TRUE,showWarnings=FALSE)) stop("Persistent goal storage directory could not be created.",call.=FALSE)
  probe <- tempfile(pattern=".goal-storage-check-",tmpdir=parent)
  on.exit(if(file.exists(probe)) unlink(probe),add=TRUE)
  if(!file.create(probe,showWarnings=FALSE)) stop("Persistent Development Goals storage is not writable.",call.=FALSE)
  invisible(actual)
}

# SQLite's online backup API includes committed WAL pages; copying only the
# .sqlite file can lose recent saves. Never modify or seed the source database.
development_store_backup <- function(path = development_store_path()) {
  if(!file.exists(path)) return(NULL)
  dir <- file.path(dirname(path),"development-goals-backups")
  if(!dir.exists(dir) && !dir.create(dir,recursive=TRUE,showWarnings=FALSE)) stop("Could not create the Development Goals backup directory.",call.=FALSE)
  output <- tempfile(pattern=paste0("development-goals-",format(Sys.time(),"%Y%m%dT%H%M%S",tz="UTC"),"-"),tmpdir=dir,fileext=".sqlite")
  staged <- paste0(output,".pending")
  on.exit(if(file.exists(staged)) unlink(staged),add=TRUE)
  from <- development_store_connect(path)
  from_open <- TRUE
  on.exit(if(from_open) DBI::dbDisconnect(from),add=TRUE)
  DBI::dbBegin(from)
  if(!"development_goals" %in% DBI::dbListTables(from)) stop("The existing goals database is missing its saved-goals table; it was not replaced.",call.=FALSE)
  rows <- DBI::dbGetQuery(from,"SELECT COUNT(*) AS n FROM development_goals")$n[1]
  to <- DBI::dbConnect(RSQLite::SQLite(),staged)
  to_open <- TRUE
  on.exit(if(to_open) DBI::dbDisconnect(to),add=TRUE)
  DBI::dbExecute(to,"PRAGMA synchronous=FULL")
  RSQLite::sqliteCopyDatabase(from,to)
  check <- DBI::dbGetQuery(to,"PRAGMA integrity_check")[[1]]
  count <- DBI::dbGetQuery(to,"SELECT COUNT(*) AS n FROM development_goals")$n[1]
  if(!identical(check,"ok") || count!=rows) stop("The goals backup could not be verified; startup stopped without replacing saved goals.",call.=FALSE)
  DBI::dbCommit(from)
  # Close the backup before renaming it. Existing snapshots are never replaced.
  DBI::dbDisconnect(to);to_open <- FALSE
  DBI::dbDisconnect(from);from_open <- FALSE
  if(!file.rename(staged,output)) {
    if(file.exists(staged)) unlink(staged)
    stop("The verified goals backup could not be finalized.",call.=FALSE)
  }
  list(path=output,rows=rows)
}

development_store_preflight <- function(path = development_store_path(),...) {
  development_storage_guard(path,...)
  backup <- development_store_backup(path)
  list(path=path,backup=backup)
}

development_store_connect <- function(path, write = FALSE) {
  if (write && !dir.exists(dirname(path)) && !dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)) {
    stop("The goal storage directory could not be created.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = if (write) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RO)
  ready <- FALSE
  on.exit(if (!ready) DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  if (write) {
    DBI::dbExecute(con, "PRAGMA synchronous = FULL")
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS development_goals (",
      "team_key TEXT NOT NULL, mode TEXT NOT NULL, player_key TEXT NOT NULL,",
      "baseline_season TEXT NOT NULL, revision INTEGER NOT NULL, saved_at TEXT NOT NULL,",
      "payload TEXT NOT NULL, PRIMARY KEY(team_key, mode, player_key, baseline_season))"
    ))
  }
  ready <- TRUE
  con
}

development_store_read <- function(path, mode, team = development_team_key()) {
  if (!file.exists(path)) return(list())
  con <- development_store_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT payload, revision, saved_at FROM development_goals WHERE team_key = ? AND mode = ?",
    params = list(team, mode))
  lapply(seq_len(nrow(rows)), function(i) {
    plan <- jsonlite::fromJSON(rows$payload[i], simplifyVector = FALSE)
    plan$revision <- as.integer(rows$revision[i])
    plan$saved_at <- rows$saved_at[i]
    for (j in seq_along(plan$objectives)) {
      plan$objectives[[j]]$supports <- as.character(unlist(plan$objectives[[j]]$supports))
      plan$objectives[[j]]$drills <- as.character(unlist(plan$objectives[[j]]$drills))
    }
    if(!is.null(plan$defense)) {
      plan$defense$supports <- as.character(unlist(plan$defense$supports))
      plan$defense$drills <- as.character(unlist(plan$defense$drills))
    }
    plan
  })
}

development_store_stamp <- function(path, mode, team = development_team_key()) {
  if (!file.exists(path)) return("empty")
  tryCatch({
    con <- development_store_connect(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    counts <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n, COALESCE(SUM(revision),0) AS revisions FROM development_goals WHERE team_key = ? AND mode = ?",
      params = list(team, mode))
    paste(counts$n, counts$revisions)
  }, error = function(e) paste("error", conditionMessage(e)))
}

development_store_save <- function(path, plan, expected_revision = 0L, team = development_team_key()) {
  if (!plan$mode %in% c("hitting", "pitching") || !nzchar(plan$player_key) ||
      !is.finite(development_season_rank(plan$baseline_season)) ||
      length(plan$objectives) < 1L || length(plan$objectives) > 2L) stop("Invalid goal plan.", call. = FALSE)
  for (i in seq_along(plan$objectives)) {
    o <- plan$objectives[[i]]
    o$supports <- development_support_actions(o$supports)
    if (!is.finite(o$target) || !is.finite(o$baseline_value) ||
        any(!nzchar(trimws(c(o$title, o$metric, o$goal)))) ||
        !length(o$supports) || length(o$supports)>3L || length(o$drills) > 5L) stop("Complete the goals and at least one supporting action (up to three) before saving.", call. = FALSE)
    plan$objectives[[i]] <- o
  }
  if(!is.null(plan$defense)) plan$defense$supports <- development_support_actions(plan$defense$supports)
  defense_errors <- development_defense_errors(plan$defense,plan$mode)
  if(length(defense_errors)) stop(paste(defense_errors,collapse=" "),call.=FALSE)
  con <- development_store_connect(path, write = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  committed <- FALSE
  on.exit(if (!committed) try(DBI::dbRollback(con), silent = TRUE), add = TRUE, after = FALSE)
  key <- list(team, plan$mode, plan$player_key, plan$baseline_season)
  previous <- DBI::dbGetQuery(con,
    "SELECT revision FROM development_goals WHERE team_key = ? AND mode = ? AND player_key = ? AND baseline_season = ?",
    params = key)
  revision <- if (nrow(previous)) as.integer(previous$revision[1]) else 0L
  if (!identical(as.integer(expected_revision), revision)) {
    stop("These goals changed in another session. Use Reload Saved Goals before saving again.", call. = FALSE)
  }
  plan$revision <- revision + 1L
  plan$saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS6 %Z", tz = "UTC")
  plan$schema_version <- if(is.null(plan$defense)) 1L else 2L
  payload <- as.character(jsonlite::toJSON(plan, auto_unbox = TRUE, null = "null", na = "null", digits = 16))
  if (nrow(previous)) {
    DBI::dbExecute(con,
      "UPDATE development_goals SET revision = ?, saved_at = ?, payload = ? WHERE team_key = ? AND mode = ? AND player_key = ? AND baseline_season = ?",
      params = c(list(plan$revision, plan$saved_at, payload), key))
  } else {
    DBI::dbExecute(con,
      "INSERT INTO development_goals(team_key, mode, player_key, baseline_season, revision, saved_at, payload) VALUES(?,?,?,?,?,?,?)",
      params = c(key, list(plan$revision, plan$saved_at, payload)))
  }
  DBI::dbCommit(con)
  committed <- TRUE
  plan
}

development_effective_plans <- function(plans, baseline_season) {
  limit <- development_season_rank(baseline_season)
  eligible <- Filter(function(p) is.finite(development_season_rank(p$baseline_season)) &&
    development_season_rank(p$baseline_season) <= limit, plans)
  if (!length(eligible)) return(list())
  eligible <- eligible[order(vapply(eligible, function(p) development_season_rank(p$baseline_season), numeric(1)), decreasing = TRUE)]
  eligible <- eligible[!duplicated(vapply(eligible, function(p) p$player_key, character(1)))]
  eligible[order(vapply(eligible, function(p) p$player, character(1)))]
}

development_rows_for_season <- function(rows, season, player_column = NULL, player_key = NULL) {
  column <- intersect(c("SeasonTag", "SeasonGroup"), names(rows))[1]
  if (is.na(column)) return(rows[0, , drop = FALSE])
  rows <- rows[!is.na(rows[[column]]) & rows[[column]] == season, , drop = FALSE]
  if ("is_bullpen" %in% names(rows)) rows <- rows[!rows$is_bullpen %in% TRUE, , drop = FALSE]
  if (!is.null(player_key)) {
    keys <- vapply(as.character(rows[[player_column]]), function(x) development_name_key(development_name(x)), character(1))
    rows <- rows[!is.na(keys) & keys == player_key, , drop = FALSE]
  }
  rows
}

development_goal_progress <- function(baseline, current, target, lower_better) {
  result <- list(baseline = baseline, current = current, target = target, hit = FALSE,
                 progress = 0, color = "#171717", state = "No data yet")
  if (!is.finite(current) || !is.finite(target)) return(result)
  direction <- if (isTRUE(lower_better)) -1 else 1
  remaining <- max(0, direction * (target - current))
  result$hit <- remaining <= 1e-12
  if (result$hit) {
    result$progress <- 1
    result$color <- "#087d3e"
    result$state <- "Goal hit"
    return(result)
  }
  if (!is.finite(baseline)) {
    result$state <- "Baseline unavailable"
    return(result)
  }
  start <- max(0, direction * (target - baseline))
  result$progress <- if (start > 1e-12) max(0, min(1, 1 - remaining / start)) else 0
  worse <- remaining > start + 1e-12
  result$color <- if (worse) "#bc2026" else "#171717"
  result$state <- if (worse) "Moving away from goal" else if (remaining < start - 1e-12) "Improving" else "No change yet"
  result
}

development_tracking_models <- function(plans, tracking_season, rows, player_column, summarize) {
  baseline_season <- development_previous_season(tracking_season)
  effective <- development_effective_plans(plans, baseline_season)
  lapply(effective, function(p) {
    before <- development_rows_for_season(rows, baseline_season, player_column, p$player_key)
    current <- development_rows_for_season(rows, tracking_season, player_column, p$player_key)
    before_values <- if (nrow(before)) summarize(before) else list()
    current_values <- if (nrow(current)) summarize(current) else list()
    value <- function(values, metric) {
      x <- values[[metric]]
      if (is.numeric(x) && length(x) == 1L && is.finite(x)) as.numeric(x) else NA_real_
    }
    objectives <- lapply(p$objectives, function(o) {
      baseline <- value(before_values, o$metric)
      if (!is.finite(baseline) && identical(p$baseline_season, baseline_season)) baseline <- o$baseline_value
      progress <- development_goal_progress(baseline, value(current_values, o$metric), o$target, o$lower_better)
      c(o, list(progress = progress))
    })
    list(player = p$player, player_key = p$player_key, plan = p, objectives = objectives,
         baseline_season = baseline_season, tracking_season = tracking_season)
  })
}

development_tracking_totals <- function(models) {
  goals <- unlist(lapply(models, function(m) lapply(m$objectives, function(o) o$progress$hit)), use.names = FALSE)
  list(players = length(models), goals = length(goals), hit = sum(goals),
       fraction = if (length(goals)) sum(goals) / length(goals) else 0)
}

development_progress_bar <- function(value, label, color = "#501214") {
  shiny::div(class = "development-progress", role = "progressbar",
    "aria-label" = label, "aria-valuemin" = 0, "aria-valuemax" = 100,
    "aria-valuenow" = round(100 * value),
    shiny::div(class = "development-progress-fill",
      style = paste0("width:", 100 * value, "%;background:", color)))
}

development_tracking_goal_cell <- function(objective) {
  if (is.null(objective)) return(shiny::tags$td(class = "development-no-goal", "\u2014"))
  p <- objective$progress
  shiny::tags$td(
    shiny::div(class = "development-tracked-goal", style = paste0("color:", p$color),
      shiny::tags$strong(objective$title),
      shiny::div(paste(objective$metric, "\u00b7", objective$goal))),
    shiny::div(class = "development-tracked-values",
      paste("Baseline", development_format(p$baseline, objective$metric), "\u2192",
            "Current", development_format(p$current, objective$metric))),
    development_progress_bar(p$progress, paste(objective$title, p$state), p$color),
    shiny::div(class = "development-tracked-state", style = paste0("color:", p$color),
      paste0(round(100 * p$progress), "% progress \u00b7 ", p$state)))
}
