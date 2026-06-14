# Leaderboards Integration Notes

The Whitecaps analytics app is intended to stay self-contained inside `CAPS/leaderboards`.

For normal Whitecaps updates, only touch:

- `CAPS/leaderboards/`
- `CAPS/leaderboards/data/data.csv`

The parent CAPS app should only need these integration touchpoints:

- `CAPS/app.R`
- `CAPS/leaderboards_embed.R`
- `CAPS/Dockerfile`

Expected behavior:

- The CAPS hub shows a `Whitecaps Analytics` card.
- Clicking that card opens the embedded Whitecaps app.
- The `CAPS Hub` button inside Whitecaps returns to the CAPS hub.
- The Whitecaps app loads exactly one bundled CSV from `CAPS/leaderboards/data/`.
- The Whitecaps app prefers a precomputed cache and falls back to live calculations if the cache is missing or stale.

Preprocessing:

- `leaderboards/scripts/precompute_leaderboards_cache.R` builds `leaderboards/cache/leaderboards_cache.rds`.
- `leaderboards/cache/` is ignored by git and does not need to be committed.
- The Docker build runs the preprocessing script automatically, so Hugging Face rebuilds the cache from the current `data.csv` during deploy.

Quick verification:

```bash
cd CAPS
Rscript leaderboards/scripts/precompute_leaderboards_cache.R
Rscript check_leaderboards_integration.R
```
