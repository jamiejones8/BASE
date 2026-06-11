# Leaderboards Integration Notes

The Whitecaps analytics app is intended to stay self-contained inside `CAPS/leaderboards`.

For normal Whitecaps updates, only touch:

- `CAPS/leaderboards/`
- `CAPS/leaderboards/data/2026Season.csv`

The parent CAPS app should only need these integration touchpoints:

- `CAPS/app.R`
- `CAPS/leaderboards_embed.R`
- `CAPS/Dockerfile`

Expected behavior:

- The CAPS hub shows a `Whitecaps Analytics` card.
- Clicking that card opens the embedded Whitecaps app.
- The `CAPS Hub` button inside Whitecaps returns to the CAPS hub.
- The Whitecaps app loads exactly one bundled CSV from `CAPS/leaderboards/data/`.

Quick verification:

```bash
cd CAPS
Rscript check_leaderboards_integration.R
```
