pitching_team_d1_average <- function(reference, stat, pitch_type = NULL) {
  if (!nrow(reference)) return(NULL)
  rows <- reference[reference$stat == stat & reference$scope == if (is.null(pitch_type)) "overall" else "pitch_type", , drop = FALSE]
  if (!is.null(pitch_type)) rows <- rows[rows$pitch_type == pitch_type, , drop = FALSE]
  rows <- rows[is.finite(rows$value), , drop = FALSE]
  if (!nrow(rows)) NULL else rows[1, , drop = FALSE]
}

# Team trends may receive rows that were already prepared for another page.
# Reconstruct in PA/pitch order, then restore input order so counts never drift
# when those rows are prepared again. TrackMan Balls is a before-pitch count.
pitching_team_trend_counts <- function(p) {
  p$.team_trend_order <- seq_len(nrow(p))
  p <- p %>% dplyr::arrange(.data$PA_ID, .data$PitchNum) %>%
    dplyr::group_by(.data$PA_ID) %>%
    dplyr::mutate(StrikesPre = dplyr::lag(pmin(2L, cumsum(as.character(.data$PitchCall) %in%
      c("StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "FoulTip"))), default = 0L)) %>%
    dplyr::ungroup()
  balls <- if ("Balls" %in% names(p)) suppressWarnings(as.numeric(p$Balls)) else rep(NA_real_, nrow(p))
  for (col in intersect(c("BallsBefore", "BallsBeforePitch", "Balls_BeforePitch"), names(p))) {
    balls <- dplyr::coalesce(suppressWarnings(as.numeric(p[[col]])), balls)
  }
  p$BallsPre <- balls
  p$TwoStrike <- p$StrikesPre == 2L
  p$.team_pa_last <- !duplicated(p$PA_ID, fromLast = TRUE)
  p <- p[order(p$.team_trend_order), , drop = FALSE]
  p$.team_trend_order <- NULL
  p
}
