# Inclusive launch-angle bands, with continuous EV cutoffs at 95, 100 and 105 mph.
base_barrel_evla <- function(ev, la) {
  ev <- suppressWarnings(as.numeric(as.character(ev)))
  la <- suppressWarnings(as.numeric(as.character(la)))
  ceiling <- ifelse(ev < 100, 25, ifelse(ev < 105, 32, 40))
  is.finite(ev) & is.finite(la) & ev >= 95 & la >= 8 & la <= ceiling
}
