# Project Structure

This repository is organized so new code, data, and assets have a predictable
home:

- `R/`: application source code.
- `R/config/`: shared project and deployment configuration.
- `R/data/`: data-loading and persistence helpers.
- `R/modules/`: larger app modules that support the main Shiny app.
- `R/pages/`: page-specific logic and UI helpers.
- `R/reports/`: report rendering code such as pitcher cards.
- `R/integrations/`: cross-app integrations such as the embedded leaderboards.
- `config/`: team-specific roster and schedule inputs.
- `data/reference/`: stable lookup tables and reference CSVs used by the app.
- `data/external/`: optional supplemental datasets such as Cape Cod data.
- `data/local/`: local-only exploratory or scratch datasets that are not part of
  the deployed runtime layout.
- `archive/ccbl/`: retired CCBL and summer-league material that is not loaded
  by the active application.
- `models/`: local model artifacts used by scouting and report generation.
- `scripts/build/`: one-off builders for derived runtime datasets.
- `scripts/checks/`: repo validation scripts.
- `scripts/tests/`: targeted test scripts.
- `leaderboards/`: the embedded leaderboards app and its own assets/helpers.
- `www/`: Shiny web assets served directly by the main app.

Compatibility wrappers remain at the repo root for the main app and the most
common validation commands, so existing workflows like `Rscript
check_team_config.R` still work.

When adding new files:

- Put reusable R code somewhere under `R/` instead of the repo root.
- Put model artifacts under `models/` and reference tables under
  `data/reference/`.
- Put ad hoc investigation files under `data/local/` unless they are part of
  the actual deployment runtime.
- Keep retired CCBL material under `archive/ccbl/` so it can be moved as one
  self-contained directory.
- Prefer adding new path defaults to `R/config/team_config.R` so file locations
  stay centralized.
