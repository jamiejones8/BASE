empty_sprint_session_level <- function() {
  data.frame(
    athlete_id = character(),
    vald_profile_id = character(),
    athlete_name = character(),
    role = character(),
    test_date = as.Date(character()),
    test_name = character(),
    test_type = character(),
    sprint_distance = character(),
    best_10yd = numeric(),
    best_30yd = numeric(),
    best_flying_10yd = numeric(),
    mean_10yd = numeric(),
    cv_10yd = numeric(),
    max_velocity = numeric(),
    total_time = numeric(),
    n_reps = integer(),
    valid_reps = integer(),
    source = character(),
    raw_test_id = character(),
    modified_at_utc = character(),
    stringsAsFactors = FALSE
  )
}

smartspeed_pick_col <- function(df, candidates = character(), pattern = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  nms <- names(df)
  low <- tolower(nms)
  cand <- tolower(candidates)
  hit <- match(cand, low, nomatch = 0)
  hit <- hit[hit > 0]
  if (length(hit) > 0) return(nms[hit[[1]]])
  if (!is.null(pattern)) {
    idx <- grep(pattern, low, perl = TRUE)
    if (length(idx) > 0) return(nms[idx[[1]]])
  }
  NULL
}

smartspeed_num <- function(x) suppressWarnings(as.numeric(x))

smartspeed_date <- function(x) {
  if (exists("as_date_safely2", mode = "function")) return(as_date_safely2(x))
  suppressWarnings(as.Date(x))
}

standardize_smartspeed_export <- function(x, roster = NULL, source = "SmartSpeed") {
  if (is.null(x)) return(empty_sprint_session_level())

  # valdr::get_smartspeed_data() returns list(profiles=, tests=) rather than a
  # flat frame; get_smartspeed_tests_only() returns the tests frame directly.
  # Pull profiles out for a name join, then continue with tests as the main table.
  profiles_tbl <- NULL
  if (is.list(x) && !is.data.frame(x) && all(c("tests") %in% names(x))) {
    profiles_tbl <- x[["profiles"]]
    x <- x[["tests"]]
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(empty_sprint_session_level())

  profile_col <- smartspeed_pick_col(x, c("profileId", "profile_id", "valdProfileId", "athleteId", "athlete_id"), "profile.*id|athlete.*id")
  name_col <- smartspeed_pick_col(x, c("athleteName", "athlete_name", "name", "fullName", "profileName"), "athlete.*name|full.*name|profile.*name")
  test_id_col <- smartspeed_pick_col(x, c("testId", "test_id", "id", "raw_test_id"), "test.*id|^id$")
  test_date_col <- smartspeed_pick_col(x, c("testDate", "test_date", "testDateUtc", "recordedDateUtc", "recordedUTC", "date"), "test.*date|recorded|date")
  mod_col <- smartspeed_pick_col(x, c("modifiedDateUtc", "modified_at_utc", "modifiedAt", "updatedAt"), "modified|updated")
  test_name_col <- smartspeed_pick_col(x, c("testName", "test_name", "name", "drillName"), "test.*name|drill")
  test_type_col <- smartspeed_pick_col(x, c("testType", "testTypeName", "test_type", "type", "deviceType"), "test.*type|type")
  distance_col <- smartspeed_pick_col(x, c("sprintDistance", "distance", "distanceYards"), "distance")
  valid_col <- smartspeed_pick_col(x, c("isValid", "valid", "is_valid"), "valid")

  # Explicit per-distance columns only exist in flat/legacy exports (e.g. a toy
  # or hand-built CSV with a "best10yd" column). The real VALD API instead
  # reports one totalTimeSeconds/bestSplitSeconds pair per test row and names
  # the distance in testName (e.g. "10yd Sprint", "30yd Sprint") -- handled
  # below via test-name routing when these explicit columns aren't present.
  ten_col <- smartspeed_pick_col(x, c("best_10yd", "best10yd", "tenYard", "split10yd", "time10yd", "X10yd"), "10.*(yd|yard)|ten.*yard")
  thirty_col <- smartspeed_pick_col(x, c("best_30yd", "best30yd", "thirtyYard", "split30yd", "time30yd", "X30yd"), "30.*(yd|yard)|thirty.*yard")
  flying_col <- smartspeed_pick_col(x, c("best_flying_10yd", "flying10yd", "fly10", "flying_10"), "fly.*10|flying.*10")
  # peakVelocityMetersPerSecond is the real, populated field in the live API;
  # a bare "maxVelocity" column also exists but is always 0 in practice, so it
  # must not be preferred over the peak-velocity field when both are present.
  max_vel_col <- smartspeed_pick_col(x, c("peakVelocityMetersPerSecond", "max_velocity", "peakVelocity", "maxVel"), "peak.*vel|max.*vel")
  total_col <- smartspeed_pick_col(x, c("total_time", "totalTimeSeconds", "totalTime", "time", "duration"), "total.*time|duration|^time$")
  best_split_col <- smartspeed_pick_col(x, c("bestSplitSeconds"), "best.*split")

  test_name_vals <- if (!is.null(test_name_col)) as.character(x[[test_name_col]]) else rep(NA_character_, nrow(x))
  time_val <- if (!is.null(total_col)) {
    smartspeed_num(x[[total_col]])
  } else if (!is.null(best_split_col)) {
    smartspeed_num(x[[best_split_col]])
  } else {
    rep(NA_real_, nrow(x))
  }
  ten_route <- grepl("^\\s*10\\s*yd\\s*sprint\\s*$", test_name_vals, ignore.case = TRUE)
  thirty_route <- grepl("^\\s*30\\s*yd\\s*sprint\\s*$", test_name_vals, ignore.case = TRUE)
  # Only "10yd Sprint"/"30yd Sprint" rows have sane velocity/time data in
  # practice -- other real-API test types (20yd Sprint, 100yd Sprint, Pro
  # Agility, the RSA test) have shown physically impossible values (e.g. a
  # "peak velocity" of 19-210 m/s, or peak exactly equal to mean on every row,
  # a signature of a gate-spacing/protocol mismatch at test time) that would
  # otherwise silently feed into Red/Yellow scoring. Gate max_velocity/
  # total_time to the same trusted test types when routing by name; explicit
  # flat/legacy exports with their own named columns are left untouched.
  trusted_route <- ten_route | thirty_route
  routing_by_name <- is.null(ten_col) && is.null(thirty_col)

  out <- data.frame(
    athlete_id = if (!is.null(profile_col)) as.character(x[[profile_col]]) else NA_character_,
    vald_profile_id = if (!is.null(profile_col)) as.character(x[[profile_col]]) else NA_character_,
    athlete_name = if (!is.null(name_col)) as.character(x[[name_col]]) else NA_character_,
    role = NA_character_,
    test_date = if (!is.null(test_date_col)) smartspeed_date(x[[test_date_col]]) else as.Date(NA),
    test_name = if (!is.null(test_name_col)) as.character(x[[test_name_col]]) else "SmartSpeed",
    test_type = if (!is.null(test_type_col)) as.character(x[[test_type_col]]) else "Timing Gates",
    sprint_distance = if (!is.null(distance_col)) as.character(x[[distance_col]]) else NA_character_,
    best_10yd = if (!is.null(ten_col)) smartspeed_num(x[[ten_col]]) else ifelse(ten_route, time_val, NA_real_),
    best_30yd = if (!is.null(thirty_col)) smartspeed_num(x[[thirty_col]]) else ifelse(thirty_route, time_val, NA_real_),
    best_flying_10yd = if (!is.null(flying_col)) smartspeed_num(x[[flying_col]]) else NA_real_,
    mean_10yd = if (!is.null(ten_col)) smartspeed_num(x[[ten_col]]) else ifelse(ten_route, time_val, NA_real_),
    cv_10yd = NA_real_,
    max_velocity = {
      v <- if (!is.null(max_vel_col)) smartspeed_num(x[[max_vel_col]]) else rep(NA_real_, nrow(x))
      if (routing_by_name) ifelse(trusted_route, v, NA_real_) else v
    },
    total_time = {
      v <- if (!is.null(total_col)) smartspeed_num(x[[total_col]]) else if (!is.null(best_split_col)) smartspeed_num(x[[best_split_col]]) else rep(NA_real_, nrow(x))
      if (routing_by_name) ifelse(trusted_route, v, NA_real_) else v
    },
    n_reps = 1L,
    valid_reps = if (!is.null(valid_col)) as.integer(tolower(as.character(x[[valid_col]])) %in% c("true", "1", "yes", "valid")) else 1L,
    source = source,
    raw_test_id = if (!is.null(test_id_col)) as.character(x[[test_id_col]]) else NA_character_,
    modified_at_utc = if (!is.null(mod_col)) as.character(x[[mod_col]]) else NA_character_,
    stringsAsFactors = FALSE
  )

  # Real VALD SmartSpeed tests carry no athlete name column -- join it from the
  # profiles table returned alongside tests (get_smartspeed_data()) when present.
  if (is.null(name_col) && !is.null(profiles_tbl) && is.data.frame(profiles_tbl) && "profileId" %in% names(profiles_tbl)) {
    given <- if ("givenName" %in% names(profiles_tbl)) as.character(profiles_tbl$givenName) else ""
    family <- if ("familyName" %in% names(profiles_tbl)) as.character(profiles_tbl$familyName) else ""
    pn <- data.frame(
      profileId = as.character(profiles_tbl$profileId),
      athlete_name_from_profile = trimws(paste(given, family)),
      stringsAsFactors = FALSE
    )
    pn <- pn[!duplicated(pn$profileId), , drop = FALSE]
    out <- dplyr::left_join(out, pn, by = c("vald_profile_id" = "profileId"))
    out$athlete_name <- dplyr::coalesce(
      dplyr::na_if(out$athlete_name, ""),
      dplyr::na_if(out$athlete_name_from_profile, "")
    )
    out$athlete_name_from_profile <- NULL
  }

  if (!is.null(roster) && is.data.frame(roster) && nrow(roster) > 0 && "profileId" %in% names(roster)) {
    roster2 <- roster
    if (!("athleteName" %in% names(roster2))) roster2$athleteName <- NA_character_
    if (!("roleGroup" %in% names(roster2))) roster2$roleGroup <- roster2$primaryGroup %||% NA_character_
    roster2 <- roster2[, intersect(c("profileId", "athleteName", "roleGroup", "externalId"), names(roster2)), drop = FALSE]
    # SmartSpeed timing gates are shared across every sport at the facility, so
    # the raw pull includes non-baseball athletes. Scope to the baseball roster
    # the same way the ForceDecks pipeline does, rather than just enriching.
    out <- dplyr::inner_join(out, roster2, by = c("vald_profile_id" = "profileId"), suffix = c("", "_roster"))
    out$athlete_name <- dplyr::coalesce(out$athlete_name, out$athleteName)
    out$role <- dplyr::coalesce(out$role, out$roleGroup)
    out$athlete_id <- dplyr::coalesce(out$athlete_id, out$vald_profile_id)
    out <- out[, names(empty_sprint_session_level()), drop = FALSE]
  }

  out |>
    dplyr::group_by(athlete_id, vald_profile_id, athlete_name, role, test_date, test_name, test_type, sprint_distance, source, raw_test_id, modified_at_utc) |>
    dplyr::summarise(
      best_10yd = if (all(is.na(best_10yd))) NA_real_ else min(best_10yd, na.rm = TRUE),
      best_30yd = if (all(is.na(best_30yd))) NA_real_ else min(best_30yd, na.rm = TRUE),
      best_flying_10yd = if (all(is.na(best_flying_10yd))) NA_real_ else min(best_flying_10yd, na.rm = TRUE),
      mean_10yd = if (all(is.na(mean_10yd))) NA_real_ else mean(mean_10yd, na.rm = TRUE),
      cv_10yd = if (sum(!is.na(mean_10yd)) > 1 && mean(mean_10yd, na.rm = TRUE) != 0) stats::sd(mean_10yd, na.rm = TRUE) / mean(mean_10yd, na.rm = TRUE) * 100 else NA_real_,
      max_velocity = if (all(is.na(max_velocity))) NA_real_ else max(max_velocity, na.rm = TRUE),
      total_time = if (all(is.na(total_time))) NA_real_ else min(total_time, na.rm = TRUE),
      n_reps = dplyr::n(),
      valid_reps = sum(valid_reps %||% 0L, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(test_date), athlete_name)
}

build_sprint_session_level <- function(raw = NULL, roster = NULL,
                                       out_rds = file.path(gold_data_dir(), "sprint_session_level.rds"),
                                       out_csv = file.path(gold_data_dir(), "sprint_session_level.csv")) {
  if (is.null(raw)) {
    raw_path <- file.path(gold_data_dir(), "smartspeed_api_raw.rds")
    raw <- if (file.exists(raw_path)) readRDS(raw_path) else NULL
  }
  if (is.null(roster)) {
    roster_path <- dashboard_data_path("roster_baseball.rds")
    roster <- if (file.exists(roster_path)) readRDS(roster_path) else NULL
  }
  sprint <- standardize_smartspeed_export(raw, roster = roster)
  ensure_dir(dirname(out_rds))
  saveRDS(sprint, out_rds)
  utils::write.csv(sprint, out_csv, row.names = FALSE, na = "")
  if (exists("write_refresh_log", mode = "function")) {
    max_dt <- if (nrow(sprint) && any(!is.na(sprint$test_date))) max(sprint$test_date, na.rm = TRUE) else as.Date(NA)
    write_refresh_log("clean_smartspeed", "ok", list(rows = nrow(sprint), max_test_date = as.character(max_dt)))
  }
  sprint
}

flatten_smartspeed_rep_results <- function(details) {
  if (is.null(details) || length(details) == 0) return(data.frame())
  rows <- list()
  for (test_id in names(details)) {
    item <- details[[test_id]]
    reps <- item$repResults %||% item$reps %||% item$results %||% list()
    if (is.data.frame(reps)) reps <- split(reps, seq_len(nrow(reps)))
    for (i in seq_along(reps)) {
      rep <- reps[[i]]
      splits <- rep$splits %||% rep$splitResults %||% list()
      if (is.data.frame(splits)) splits <- split(splits, seq_len(nrow(splits)))
      if (length(splits) == 0) {
        rows[[length(rows) + 1]] <- data.frame(test_id = test_id, rep_index = i, split_name = NA_character_, split_time = NA_real_)
      } else {
        for (j in seq_along(splits)) {
          sp <- splits[[j]]
          rows[[length(rows) + 1]] <- data.frame(
            test_id = test_id,
            rep_index = i,
            split_name = as.character(sp$name %||% sp$splitName %||% j),
            split_distance = suppressWarnings(as.numeric(sp$distance %||% sp$splitDistance %||% NA_real_)),
            split_time = suppressWarnings(as.numeric(sp$time %||% sp$splitTime %||% NA_real_)),
            total_time = suppressWarnings(as.numeric(rep$totalTime %||% rep$time %||% NA_real_)),
            max_velocity = suppressWarnings(as.numeric(rep$maxVelocity %||% rep$max_velocity %||% NA_real_)),
            is_valid = as.logical(rep$isValid %||% TRUE),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  dplyr::bind_rows(rows)
}

build_sprint_split_table <- function(details, out_rds = file.path(gold_data_dir(), "sprint_split_level.rds")) {
  splits <- flatten_smartspeed_rep_results(details)
  if (!is.null(splits) && is.data.frame(splits) && nrow(splits) > 0) {
    ensure_dir(dirname(out_rds))
    saveRDS(splits, out_rds)
    if (exists("write_refresh_log", mode = "function")) write_refresh_log("clean_smartspeed_splits", "ok", list(rows = nrow(splits)))
  } else if (exists("write_refresh_log", mode = "function")) {
    write_refresh_log("clean_smartspeed_splits", "skipped", list(reason = "No split-level SmartSpeed details available."))
  }
  splits
}
