#!/usr/bin/env Rscript

# Run from the repository root. Optional argument: directory for CSV findings.
suppressPackageStartupMessages({ library(dplyr); library(readr) })
source("team_config.R")
source("R/integrations/wally_hitting_workspace.R")
w <- base_wally_hitting_environment(base_prepare_team_hitting_data())
d <- w$txst_df
stopifnot(nrow(d) > 0L)

pa <- d %>%
  mutate(PA_ID = w$make_pa_id(d)) %>%
  group_by(PA_ID) %>% slice_tail(n = 1) %>% ungroup()
pa$event <- w$.pa_event(pa$play_result, pa$KorBB, pa$pitch_call)
pa$actual <- w$.woba_weight(pa$event)
pa$bip <- w$is_bip_txst(pa$pitch_call, pa$play_result)
pa$grid_xw <- w$hitting_xwoba_lookup(pa$ev, pa$la)
pa$source_xw <- w$.get_num(pa, c("xwOBA", "xwoba", "ExpectedWOBA", "ExpectedwOBA"))
pa$contact_xw <- ifelse(is.finite(pa$source_xw), pa$source_xw, pa$grid_xw)
pa$expected <- case_when(
  pa$event == "IBB" ~ NA_real_,
  pa$event == "K" ~ 0,
  pa$event == "BB" ~ w$woba_weights$BB,
  pa$event == "HBP" ~ w$woba_weights$HBP,
  pa$bip ~ pa$contact_xw,
  TRUE ~ NA_real_
)

summarize_audit <- function(g) {
  scored_contact <- g$bip & is.finite(g$contact_xw)
  tibble(
    PA = nrow(g), wOBA_PA = sum(g$event != "IBB"), xwOBA_PA = sum(is.finite(g$expected)),
    BB = sum(g$event == "BB"), HBP = sum(g$event == "HBP"), K = sum(g$event == "K"),
    BIP = sum(g$bip), ScoredBIP = sum(scored_contact),
    SourceScoredBIP = sum(g$bip & is.finite(g$source_xw)),
    wOBA = w$safe_ratio(sum(g$actual), sum(g$event != "IBB")),
    xwOBA = w$.safe_mean(g$expected),
    wOBAcon = w$.safe_mean(g$actual[g$bip]),
    scored_wOBAcon = w$.safe_mean(g$actual[scored_contact]),
    xwOBAcon = w$.safe_mean(g$contact_xw[scored_contact])
  )
}

players <- pa %>% group_by(SeasonGroup, Batter) %>% group_modify(~ summarize_audit(.x)) %>% ungroup()
seasons <- pa %>% group_by(SeasonGroup) %>% group_modify(~ summarize_audit(.x)) %>% ungroup()
contact_bins <- pa %>%
  filter(bip, is.finite(contact_xw)) %>%
  mutate(LaunchAngle = cut(la, c(-Inf, 0, 10, 20, 30, 40, Inf))) %>%
  group_by(SeasonGroup, LaunchAngle) %>%
  summarise(BIP = n(), observed = mean(actual), expected = mean(contact_xw),
            gap = mean(actual - contact_xw), .groups = "drop")
unscored <- pa %>%
  filter(!is.finite(expected), event != "IBB") %>%
  mutate(Reason = case_when(
    !bip ~ "Unresolved terminal row",
    !is.finite(ev) | !is.finite(la) ~ "Missing EV or launch angle",
    TRUE ~ "Grid cell unavailable or outside grid"
  )) %>% count(SeasonGroup, Reason, pitch_call, play_result, name = "PA")

# Ensure this diagnostic reproduces the app instead of silently auditing a
# different PA definition or formula.
for (season in unique(d$SeasonGroup)) {
  production <- w$summarize_overall(d[d$SeasonGroup == season, ])
  audited <- seasons[seasons$SeasonGroup == season, ]
  for (metric in c("PA", "wOBA", "xwOBA", "wOBAcon", "xwOBAcon")) {
    stopifnot(isTRUE(all.equal(as.numeric(production[[metric]]), as.numeric(audited[[metric]]))))
  }
}

print(seasons, width = Inf)
print(players %>% filter(PA >= 20) %>% select(SeasonGroup, Batter, PA, wOBA, xwOBA), n = Inf)
print(contact_bins, n = Inf)
print(unscored, n = Inf)
outputs <- list(players = players, seasons = seasons, contact_bins = contact_bins, unscored = unscored)

# An independent contact sample: opposing hitters facing the team's pitchers.
opponent_path <- base_project_path("WallyApps", "PitchingApp", "data", "2026 Season - cleaned.csv")
if (file.exists(opponent_path)) {
  opponents <- read_csv(opponent_path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    filter(base_team_matches(PitcherTeam), PitchCall == "InPlay") %>%
    distinct(PitchUID, .keep_all = TRUE) %>%
    mutate(
      actual = w$.woba_weight(w$.pa_event(PlayResult, KorBB, PitchCall)),
      expected = w$hitting_xwoba_lookup(ExitSpeed, Angle)
    ) %>% filter(is.finite(expected))
  outputs$opponent_contact <- opponents %>% summarise(
    BIP = n(), Hitters = n_distinct(Batter),
    observed = mean(actual), expected = mean(expected), gap = mean(actual - expected)
  )
  print(outputs$opponent_contact)
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  dir.create(args[1], recursive = TRUE, showWarnings = FALSE)
  for (name in names(outputs)) {
    write_csv(outputs[[name]], file.path(args[1], paste0(name, ".csv")))
  }
}
