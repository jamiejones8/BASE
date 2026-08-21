# Leaderboards Integration Notes

The team analytics app is intended to stay self-contained inside `leaderboards/`.

For normal team updates, change:

- `leaderboards/`
- the season file selected by `BASE_SEASON_DATA_FILE`
- `team_config.R` or deployment environment variables

The parent BASE app should only need these integration touchpoints:

- `app.R`
- `leaderboards_embed.R`
- `Dockerfile`

Expected behavior:

- The BASE hub shows a team analytics card.
- Clicking that card opens the embedded leaderboards app.
- The `BASE` button inside leaderboards returns to the BASE hub.
- Leaderboards load the configured season file as their shared source.
- The app prefers a precomputed cache and falls back to live calculations if the cache is missing or stale.

Preprocessing:

- `leaderboards/scripts/precompute_leaderboards_cache.R` builds `leaderboards/cache/leaderboards_cache.rds` for `BASE_TEAM_DATA_CODE`.
- `leaderboards/cache/` is ignored by git and does not need to be committed.
- The Docker build runs the preprocessing script automatically, so deployment rebuilds the cache from the configured season file.

Quick verification:

```bash
cd <project-root>
Rscript leaderboards/scripts/precompute_leaderboards_cache.R
Rscript check_leaderboards_integration.R
```
