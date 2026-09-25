# Reconcile the private VALD athlete catalog with BASE's authoritative team
# roster. Matching is deliberately conservative: an exact normalized name is
# accepted only when it is unique on both sides. Everything else needs an
# explicit private crosswalk.

player_health_normalize_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  tolower(gsub("[^a-z0-9]", "", x))
}

player_health_primary_group <- function(pos_type, pos = "") {
  type_value <- as.character(pos_type)
  pos_value <- as.character(pos)
  dplyr::case_when(
    grepl("pitch", type_value, ignore.case = TRUE) ~ "Pitchers",
    grepl("catch", type_value, ignore.case = TRUE) ~ "Catchers",
    grepl("infield", type_value, ignore.case = TRUE) ~ "Infielders",
    grepl("outfield", type_value, ignore.case = TRUE) ~ "Outfielders",
    !nzchar(trimws(type_value)) & grepl("rhp|lhp", pos_value, ignore.case = TRUE) ~ "Pitchers",
    !nzchar(trimws(type_value)) & grepl("(^|[/, ])c($|[/, ])", pos_value, ignore.case = TRUE) ~ "Catchers",
    !nzchar(trimws(type_value)) & grepl("inf", pos_value, ignore.case = TRUE) ~ "Infielders",
    !nzchar(trimws(type_value)) & grepl("of", pos_value, ignore.case = TRUE) ~ "Outfielders",
    TRUE ~ "Other"
  )
}

player_health_role_group <- function(primary_group) {
  dplyr::case_when(
    as.character(primary_group) == "Pitchers" ~ "Pitchers",
    as.character(primary_group) %in% c("Catchers", "Infielders", "Outfielders", "Hitters") ~ "Hitters",
    TRUE ~ "Unknown"
  )
}

player_health_default_roster_path <- function() {
  configured <- Sys.getenv(
    "BASE_PLAYER_HEALTH_ROSTER_FILE",
    unset = Sys.getenv("BASE_ROSTER_FILE", unset = "")
  )
  if (nzchar(trimws(configured))) return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  app_root <- normalizePath(Sys.getenv("VALD_APP_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  normalizePath(
    file.path(dirname(dirname(app_root)), "config", "texas_state_roster_2027.csv"),
    winslash = "/",
    mustWork = FALSE
  )
}

player_health_default_crosswalk_path <- function() {
  configured <- Sys.getenv("BASE_PLAYER_HEALTH_CROSSWALK_FILE", unset = "")
  if (nzchar(trimws(configured))) return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  legacy_dir <- Sys.getenv(
    "CMJ_SPRINT_SHARE_LEGACY_DIR",
    file.path(Sys.getenv("VALD_APP_ROOT", getwd()), "data", "legacy")
  )
  normalizePath(file.path(legacy_dir, "player_health_crosswalk.csv"), winslash = "/", mustWork = FALSE)
}

player_health_read_csv <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
}

player_health_pick_column <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  names_lower <- tolower(names(df))
  hit <- match(tolower(candidates), names_lower, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit)) names(df)[hit[[1]]] else NULL
}

player_health_standardize_vald_roster <- function(roster) {
  required <- c("profileId", "athleteName", "externalId", "groupNames_csv", "primaryGroup", "notes")
  if (is.null(roster) || !is.data.frame(roster)) {
    return(tibble::tibble(
      profileId = character(), athleteName = character(), externalId = character(),
      groupNames_csv = character(), primaryGroup = character(), notes = character()
    ))
  }
  # Older snapshots may already contain a reconciled BASE roster. Strip its
  # synthetic, unmatched identities before treating the object as a VALD
  # source catalog again.
  if ("identityMatched" %in% names(roster)) {
    matched <- !is.na(roster$identityMatched) & as.logical(roster$identityMatched)
    roster <- roster[matched, , drop = FALSE]
  }
  roster <- tibble::as_tibble(roster)
  defaults <- list(
    profileId = "", athleteName = "", externalId = "", groupNames_csv = "",
    primaryGroup = "Other", notes = ""
  )
  for (column in required) {
    if (!(column %in% names(roster))) roster[[column]] <- defaults[[column]]
    roster[[column]] <- as.character(roster[[column]])
    roster[[column]][is.na(roster[[column]])] <- defaults[[column]]
  }
  roster |>
    dplyr::filter(nzchar(.data$profileId)) |>
    dplyr::distinct(.data$profileId, .keep_all = TRUE)
}

player_health_standardize_crosswalk <- function(path) {
  crosswalk <- player_health_read_csv(path)
  if (is.null(crosswalk) || !nrow(crosswalk)) {
    return(tibble::tibble(
      roster_name = character(), profileId = character(), externalId = character(),
      name_key = character()
    ))
  }
  name_col <- player_health_pick_column(crosswalk, c("roster_name", "Name", "athleteName", "player_name"))
  profile_col <- player_health_pick_column(crosswalk, c("profileId", "profile_id", "vald_profile_id", "vald-id"))
  external_col <- player_health_pick_column(crosswalk, c("externalId", "external_id", "vald_external_id"))
  if (is.null(name_col) || (is.null(profile_col) && is.null(external_col))) {
    stop(
      "Player Health crosswalk must contain a roster name and either profileId or externalId.",
      call. = FALSE
    )
  }
  tibble::tibble(
    roster_name = as.character(crosswalk[[name_col]]),
    profileId = if (!is.null(profile_col)) as.character(crosswalk[[profile_col]]) else "",
    externalId = if (!is.null(external_col)) as.character(crosswalk[[external_col]]) else ""
  ) |>
    dplyr::mutate(
      roster_name = trimws(.data$roster_name),
      profileId = dplyr::coalesce(.data$profileId, ""),
      externalId = dplyr::coalesce(.data$externalId, ""),
      name_key = player_health_normalize_name(.data$roster_name)
    ) |>
    dplyr::filter(nzchar(.data$name_key)) |>
    dplyr::distinct(.data$name_key, .keep_all = TRUE)
}

reconcile_player_health_roster <- function(
    vald_roster,
    team_roster_path = player_health_default_roster_path(),
    crosswalk_path = player_health_default_crosswalk_path()) {
  vald <- player_health_standardize_vald_roster(vald_roster) |>
    dplyr::mutate(name_key = player_health_normalize_name(.data$athleteName))
  team <- player_health_read_csv(team_roster_path)

  if (is.null(team) || !nrow(team)) {
    fallback <- vald |>
      dplyr::mutate(
        roleGroup = player_health_role_group(.data$primaryGroup),
        rosterSource = "VALD activity",
        identityMatched = TRUE,
        matchMethod = "vald_only"
      ) |>
      dplyr::select(-dplyr::any_of("name_key"))
    attr(fallback, "identity_summary") <- list(
      roster_total = nrow(fallback), matched = nrow(fallback), unmatched = 0L,
      ambiguous = 0L, unrostered_vald = 0L, source = "VALD activity"
    )
    return(fallback)
  }

  name_col <- player_health_pick_column(team, c("Name", "athleteName", "player_name"))
  pos_type_col <- player_health_pick_column(team, c("pos_type", "position_type", "role"))
  pos_col <- player_health_pick_column(team, c("Pos", "position"))
  number_col <- player_health_pick_column(team, c("Number", "jersey_number", "number"))
  if (is.null(name_col) || is.null(pos_type_col)) {
    stop("BASE Player Health roster must contain Name and pos_type columns.", call. = FALSE)
  }

  base_roster <- tibble::tibble(
    athleteName = trimws(as.character(team[[name_col]])),
    pos_type = as.character(team[[pos_type_col]]),
    position = if (!is.null(pos_col)) as.character(team[[pos_col]]) else "",
    jerseyNumber = if (!is.null(number_col)) as.character(team[[number_col]]) else ""
  ) |>
    dplyr::mutate(name_key = player_health_normalize_name(.data$athleteName)) |>
    dplyr::filter(nzchar(.data$name_key)) |>
    dplyr::distinct(.data$name_key, .keep_all = TRUE)

  crosswalk <- player_health_standardize_crosswalk(crosswalk_path)
  vald_name_counts <- vald |>
    dplyr::count(.data$name_key, name = "vald_name_n")
  base_name_counts <- base_roster |>
    dplyr::count(.data$name_key, name = "base_name_n")
  unique_name_matches <- vald |>
    dplyr::left_join(vald_name_counts, by = "name_key") |>
    dplyr::left_join(base_name_counts, by = "name_key") |>
    dplyr::filter(.data$vald_name_n == 1L, .data$base_name_n == 1L) |>
    dplyr::select("name_key", exact_profileId = "profileId")

  resolved <- base_roster |>
    dplyr::left_join(crosswalk, by = "name_key", suffix = c("", "_crosswalk")) |>
    dplyr::left_join(unique_name_matches, by = "name_key") |>
    dplyr::mutate(
      crosswalk_profile = dplyr::na_if(dplyr::coalesce(.data$profileId, ""), ""),
      crosswalk_external = dplyr::na_if(dplyr::coalesce(.data$externalId, ""), "")
    )

  if (nrow(resolved)) {
    for (i in seq_len(nrow(resolved))) {
      if (is.na(resolved$crosswalk_profile[[i]]) && !is.na(resolved$crosswalk_external[[i]])) {
        candidates <- vald$profileId[vald$externalId == resolved$crosswalk_external[[i]]]
        if (length(unique(candidates)) == 1L) resolved$crosswalk_profile[[i]] <- unique(candidates)[[1]]
      }
    }
  }

  # A typo or duplicate in the private crosswalk must never silently attach
  # one athlete's testing history to another roster row. Keep those rows
  # visible, but unresolved, so the audit report points staff back to the
  # configuration error.
  crosswalk_supplied <- !is.na(resolved$crosswalk_profile) | !is.na(resolved$crosswalk_external)
  invalid_crosswalk <- crosswalk_supplied & (
    is.na(resolved$crosswalk_profile) | !(resolved$crosswalk_profile %in% vald$profileId)
  )
  duplicate_crosswalk <- !is.na(resolved$crosswalk_profile) & (
    duplicated(resolved$crosswalk_profile) | duplicated(resolved$crosswalk_profile, fromLast = TRUE)
  )

  resolved <- resolved |>
    dplyr::mutate(
      resolved_profileId = dplyr::case_when(
        invalid_crosswalk | duplicate_crosswalk ~ NA_character_,
        crosswalk_supplied ~ .data$crosswalk_profile,
        TRUE ~ .data$exact_profileId
      ),
      matchMethod = dplyr::case_when(
        duplicate_crosswalk ~ "ambiguous_crosswalk",
        invalid_crosswalk ~ "invalid_crosswalk",
        !is.na(.data$crosswalk_profile) ~ "crosswalk",
        !is.na(.data$exact_profileId) ~ "exact_name",
        TRUE ~ "unmatched"
      )
    )

  placeholder_ids <- make.unique(paste0("base-roster::", resolved$name_key), sep = "-")
  resolved$profileId <- ifelse(
    is.na(resolved$resolved_profileId) | !nzchar(resolved$resolved_profileId),
    placeholder_ids,
    resolved$resolved_profileId
  )

  vald_lookup <- vald |>
    dplyr::select(
      "profileId",
      valdAthleteName = "athleteName",
      valdExternalId = "externalId",
      valdNotes = "notes"
    )

  out <- resolved |>
    dplyr::left_join(vald_lookup, by = "profileId") |>
    dplyr::mutate(
      externalId = dplyr::coalesce(.data$crosswalk_external, dplyr::na_if(.data$valdExternalId, ""), ""),
      primaryGroup = player_health_primary_group(.data$pos_type, .data$position),
      roleGroup = player_health_role_group(.data$primaryGroup),
      groupNames_csv = .data$pos_type,
      notes = dplyr::case_when(
        .data$matchMethod == "unmatched" ~ "Current BASE roster; VALD identity not mapped",
        .data$matchMethod == "invalid_crosswalk" ~ "Current BASE roster; configured VALD identity was not found",
        .data$matchMethod == "ambiguous_crosswalk" ~ "Current BASE roster; configured VALD identity is assigned more than once",
        TRUE ~ dplyr::coalesce(.data$valdNotes, "")
      ),
      rosterSource = "BASE roster",
      identityMatched = .data$matchMethod %in% c("crosswalk", "exact_name")
    ) |>
    dplyr::select(
      "profileId", "athleteName", "externalId", "groupNames_csv",
      "primaryGroup", "roleGroup", "notes", "position", "pos_type",
      "jerseyNumber", "rosterSource", "identityMatched", "matchMethod"
    ) |>
    dplyr::arrange(.data$primaryGroup, .data$athleteName)

  matched_ids <- out$profileId[out$identityMatched]
  summary <- list(
    roster_total = nrow(out),
    matched = sum(out$identityMatched),
    unmatched = sum(!out$identityMatched),
    ambiguous = sum(grepl("^ambiguous", out$matchMethod)),
    unrostered_vald = sum(!(vald$profileId %in% matched_ids)),
    source = "BASE roster"
  )
  attr(out, "identity_summary") <- summary
  out
}

reconcile_player_health_sprint <- function(sprint, roster) {
  if (is.null(sprint) || !is.data.frame(sprint) || !nrow(sprint)) return(sprint)
  if (is.null(roster) || !is.data.frame(roster) || !nrow(roster)) return(sprint)
  if (!("athlete_id" %in% names(sprint)) || !("profileId" %in% names(roster))) return(sprint)

  lookup <- roster |>
    dplyr::filter(!grepl("^base-roster::", .data$profileId)) |>
    dplyr::distinct(.data$profileId, .keep_all = TRUE) |>
    dplyr::transmute(
      athlete_id = as.character(.data$profileId),
      roster_athlete_name = as.character(.data$athleteName),
      roster_role = as.character(.data$roleGroup)
    )

  if ("rosterSource" %in% names(roster) && any(roster$rosterSource == "BASE roster")) {
    out <- dplyr::inner_join(sprint, lookup, by = "athlete_id")
  } else {
    out <- dplyr::left_join(sprint, lookup, by = "athlete_id")
  }
  if (!("athlete_name" %in% names(out))) out$athlete_name <- NA_character_
  if (!("role" %in% names(out))) out$role <- NA_character_
  out$athlete_name <- dplyr::coalesce(out$roster_athlete_name, out$athlete_name)
  out$role <- dplyr::coalesce(out$roster_role, NA_character_)
  out$role[is.na(out$role) | !nzchar(out$role)] <- "Unknown"
  out$roster_athlete_name <- NULL
  out$roster_role <- NULL
  out
}

player_health_identity_summary <- function(roster, raw_vald_roster = NULL) {
  summary <- attr(roster, "identity_summary", exact = TRUE)
  if (!is.null(summary)) return(summary)
  list(
    roster_total = if (is.data.frame(roster)) nrow(roster) else 0L,
    matched = if (is.data.frame(roster) && "identityMatched" %in% names(roster)) sum(roster$identityMatched) else 0L,
    unmatched = if (is.data.frame(roster) && "identityMatched" %in% names(roster)) sum(!roster$identityMatched) else 0L,
    ambiguous = 0L,
    unrostered_vald = if (is.data.frame(raw_vald_roster)) nrow(raw_vald_roster) else 0L,
    source = if (is.data.frame(roster) && "rosterSource" %in% names(roster)) dplyr::first(roster$rosterSource) else "Unknown"
  )
}

player_health_sprint_metric_choices <- function(sprint, window_days = 365L) {
  labels <- c(
    best_10yd = "Best 10-yard",
    best_30yd = "Best 30-yard",
    best_flying_10yd = "Best flying 10-yard",
    max_velocity = "Max velocity"
  )
  if (is.null(sprint) || !is.data.frame(sprint) || !nrow(sprint)) {
    return(stats::setNames("best_10yd", labels[["best_10yd"]]))
  }
  recent <- sprint
  if ("test_date" %in% names(recent)) {
    dates <- suppressWarnings(as.Date(recent$test_date))
    anchor <- suppressWarnings(max(dates, na.rm = TRUE))
    if (is.finite(as.numeric(anchor))) recent <- recent[!is.na(dates) & dates >= anchor - (as.integer(window_days) - 1L), , drop = FALSE]
  }
  available <- names(labels)[vapply(names(labels), function(metric) {
    metric %in% names(recent) && any(!is.na(suppressWarnings(as.numeric(recent[[metric]]))))
  }, logical(1))]
  if (!length(available)) available <- "best_10yd"
  stats::setNames(available, unname(labels[available]))
}
