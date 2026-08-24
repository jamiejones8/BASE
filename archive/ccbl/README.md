# CCBL Archive

This directory contains retired Cape Cod Baseball League and related summer-league
materials that are not loaded by the active application.

- `data/` contains legacy exploratory exports and test files.
- `assets/` contains CCBL team marks and Brewster-specific images.

The active Cape Cod player supplement remains at `data/external/CapeCod26.parquet`
because the college scouting pages can optionally combine it with college data.
The embedded leaderboard application and its calculation helpers remain under
`leaderboards/` because they are still loaded by the main app.
