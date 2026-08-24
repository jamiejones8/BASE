# Phase 1 geometric fielder-attribution engine.
#
# This deliberately produces candidates, not official scoring decisions.
# Ground balls use perpendicular distance from each infielder's starting point
# to the batted-ball spray ray. Air balls use distance from each defender's
# starting point to the projected landing point. Every result retains the
# runner-up and the separation between candidates so ambiguity stays visible.

BASE_ATTRIBUTION_POSITIONS <- c("1B", "2B", "3B", "SS", "LF", "CF", "RF")
BASE_ATTRIBUTION_INFIELD <- c("1B", "2B", "3B", "SS")

base_attribution_hit_family <- function(auto_type, tagged_type) {
  hit_type <- dplyr::coalesce(as.character(auto_type), as.character(tagged_type))
  value <- tolower(trimws(hit_type))
  dplyr::case_when(
    grepl("bunt", value) ~ "bunt",
    grepl("ground", value) ~ "ground",
    grepl("fly|line|popup|pop up", value) ~ "air",
    TRUE ~ "unknown"
  )
}

base_attribution_candidates <- function(row, positions, mode) {
  empty_candidates <- tibble::tibble(
    Position = character(), Player = character(), PlayerId = character(),
    StartLateral = numeric(), StartDepth = numeric(),
    GeometryDistance = numeric(), RequiredSpeed = numeric()
  )
  theta <- suppressWarnings(as.numeric(row$Bearing[[1]])) * pi / 180
  if (!is.finite(theta)) return(empty_candidates)

  candidates <- lapply(positions, function(position) {
    lateral <- suppressWarnings(as.numeric(row[[paste0(position, "_Lateral")]][[1]]))
    depth <- suppressWarnings(as.numeric(row[[paste0(position, "_Depth")]][[1]]))
    if (!is.finite(lateral) || !is.finite(depth)) return(NULL)

    if (mode == "ground_path") {
      # Unit ray from home plate: (sin(theta), cos(theta)). The perpendicular
      # distance is the movement required to reach that projected path at the
      # fielder's depth; projection guards against points behind home plate.
      projection <- lateral * sin(theta) + depth * cos(theta)
      distance <- abs(lateral * cos(theta) - depth * sin(theta))
      if (!is.finite(projection) || projection < 0) distance <- Inf
      required_speed <- NA_real_
    } else {
      ball_distance <- suppressWarnings(as.numeric(row$Distance[[1]]))
      hang_time <- suppressWarnings(as.numeric(row$HangTime[[1]]))
      if (!is.finite(ball_distance) || ball_distance <= 0) return(NULL)
      landing_lateral <- sin(theta) * ball_distance
      landing_depth <- cos(theta) * ball_distance
      distance <- sqrt((lateral - landing_lateral)^2 + (depth - landing_depth)^2)
      required_speed <- if (is.finite(hang_time) && hang_time > 0) distance / hang_time else NA_real_
    }

    tibble::tibble(
      Position = position,
      Player = as.character(row[[paste0(position, "_Name")]][[1]]),
      PlayerId = as.character(row[[paste0(position, "_Id")]][[1]]),
      StartLateral = lateral,
      StartDepth = depth,
      GeometryDistance = distance,
      RequiredSpeed = required_speed
    )
  })

  bound_candidates <- dplyr::bind_rows(candidates)
  if (!nrow(bound_candidates) || !"GeometryDistance" %in% names(bound_candidates)) {
    return(empty_candidates)
  }
  bound_candidates %>%
    dplyr::filter(is.finite(GeometryDistance)) %>%
    dplyr::arrange(GeometryDistance)
}

base_attribution_confidence <- function(mode, best_distance, margin, required_speed) {
  if (!is.finite(best_distance) || !is.finite(margin)) {
    return(list(score = NA_real_, tier = "Unassigned"))
  }

  scale <- if (mode == "ground_path") 22 else 55
  separation <- margin / (margin + 12)
  reachability <- exp(-best_distance / scale)
  score <- 0.45 + 0.30 * separation + 0.20 * reachability

  # Long travel in limited hang time makes nearest-player attribution less
  # certain even when that player is clearly closest.
  if (mode == "air_landing" && is.finite(required_speed)) {
    if (required_speed > 30) score <- score - 0.18
    else if (required_speed > 24) score <- score - 0.08
  }
  score <- max(0.35, min(0.92, score))
  tier <- dplyr::case_when(
    score >= 0.80 ~ "High",
    score >= 0.65 ~ "Medium",
    TRUE ~ "Low"
  )
  list(score = score, tier = tier)
}

base_geometric_attribution <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(tibble::tibble())
  bip <- rows %>%
    dplyr::filter(IsBIP %in% TRUE) %>%
    dplyr::mutate(HitFamily = base_attribution_hit_family(AutoHitType, TaggedHitType))
  if (!nrow(bip)) return(tibble::tibble())

  results <- lapply(seq_len(nrow(bip)), function(i) {
    row <- bip[i, , drop = FALSE]
    family <- row$HitFamily[[1]]
    result <- as.character(row$PlayResult[[1]])

    reason <- NA_character_
    mode <- NA_character_
    positions <- character()
    if (identical(result, "HomeRun")) reason <- "Home run — no fielding chance assigned"
    else if (family == "bunt") reason <- "Bunt — pitcher/catcher coordinates unavailable"
    else if (family == "unknown") reason <- "Batted-ball type unavailable"
    else if (!is.finite(suppressWarnings(as.numeric(row$Bearing[[1]])))) reason <- "Spray bearing unavailable"
    else if (family == "ground") {
      mode <- "ground_path"
      positions <- BASE_ATTRIBUTION_INFIELD
    } else if (family == "air") {
      mode <- "air_landing"
      positions <- BASE_ATTRIBUTION_POSITIONS
      if (!is.finite(suppressWarnings(as.numeric(row$Distance[[1]])))) reason <- "Landing distance unavailable"
    }

    candidates <- if (is.na(reason)) base_attribution_candidates(row, positions, mode) else tibble::tibble()
    if (is.na(reason) && nrow(candidates) < 2) reason <- "Fewer than two tracked candidates"

    if (!is.na(reason)) {
      return(tibble::tibble(
        DefenseRowId = row$DefenseRowId[[1]], Date = row$Date[[1]],
        GameUID = as.character(row$GameUID[[1]]), PlayID = as.character(row$PlayID[[1]]),
        Batter = as.character(row$Batter[[1]]), BatterTeam = as.character(row$BatterTeam[[1]]),
        HitType = dplyr::coalesce(as.character(row$AutoHitType[[1]]), as.character(row$TaggedHitType[[1]])),
        PlayResult = result, GeometryMode = "Unassigned",
        PrimaryPosition = NA_character_, PrimaryPlayer = NA_character_, PrimaryDistance = NA_real_,
        RequiredSpeed = NA_real_, SecondPosition = NA_character_, SecondPlayer = NA_character_,
        SecondDistance = NA_real_, CandidateMargin = NA_real_, ConfidenceScore = NA_real_,
        ConfidenceTier = "Unassigned", AttributionNote = reason
      ))
    }

    first <- candidates[1, , drop = FALSE]
    second <- candidates[2, , drop = FALSE]
    margin <- second$GeometryDistance[[1]] - first$GeometryDistance[[1]]
    confidence <- base_attribution_confidence(
      mode, first$GeometryDistance[[1]], margin, first$RequiredSpeed[[1]]
    )
    mode_label <- if (mode == "ground_path") "Ground-ball path" else "Air-ball landing"

    tibble::tibble(
      DefenseRowId = row$DefenseRowId[[1]], Date = row$Date[[1]],
      GameUID = as.character(row$GameUID[[1]]), PlayID = as.character(row$PlayID[[1]]),
      Batter = as.character(row$Batter[[1]]), BatterTeam = as.character(row$BatterTeam[[1]]),
      HitType = dplyr::coalesce(as.character(row$AutoHitType[[1]]), as.character(row$TaggedHitType[[1]])),
      PlayResult = result, GeometryMode = mode_label,
      PrimaryPosition = first$Position[[1]], PrimaryPlayer = first$Player[[1]],
      PrimaryDistance = first$GeometryDistance[[1]], RequiredSpeed = first$RequiredSpeed[[1]],
      SecondPosition = second$Position[[1]], SecondPlayer = second$Player[[1]],
      SecondDistance = second$GeometryDistance[[1]], CandidateMargin = margin,
      ConfidenceScore = confidence$score, ConfidenceTier = confidence$tier,
      AttributionNote = "Geometric candidate only — not an official fielding assignment"
    )
  })

  dplyr::bind_rows(results)
}
