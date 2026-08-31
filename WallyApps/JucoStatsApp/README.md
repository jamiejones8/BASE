# JucoStatsApp

First-pass ingestion layer for junior-college baseball stats.

## Scope

The starter registry includes:

- NJCAA Region 14 baseball programs
- NJCAA Region 5 baseball programs
- The 12 extra programs requested for the first scouting pool

The active scraper is source-driven: each row in `data/program_sources.csv` points to a team stat page, and the browser scraper saves rendered HTML before the R parser normalizes player tables. Each output row keeps `source_url`, `scraped_at`, `stat_type`, `program_name`, `source_group`, `njcaa_region`, and `state`.

## Run

Browser-backed scrape, then parse to the normalized CSV:

```sh
Rscript JucoStatsApp/scripts/build_program_sources.R

/Users/austinwallace/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/bin/node \
  JucoStatsApp/scripts/scrape_njcaa_browser.cjs \
  --mode=sources \
  --season=2025-26 \
  --sources=JucoStatsApp/data/program_sources.csv \
  --output=JucoStatsApp/data/juco_player_stats_latest.csv
```

Useful batch controls:

```sh
/Users/austinwallace/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/bin/node \
  JucoStatsApp/scripts/scrape_njcaa_browser.cjs \
  --headless=false \
  --mode=sources \
  --program-filter="Alvin College" \
  --source-kind-filter="hitting|pitching|fielding" \
  --limit=3
```

If the site requires an interactive browser challenge, run the same command with:

```sh
/Users/austinwallace/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/bin/node \
  JucoStatsApp/scripts/scrape_njcaa_browser.cjs \
  --headless=false \
  --mode=sources \
  --season=2025-26 \
  --sources=JucoStatsApp/data/program_sources.csv \
  --timeout-ms=120000
```

The browser profile is saved at `JucoStatsApp/.browser-profile` so any approved session/cookie can be reused on later runs.
Failed page attempts save HTML and screenshots under `JucoStatsApp/data/debug_njcaa`.

National aggregate scrape attempt:

```sh
Rscript JucoStatsApp/scripts/scrape_njcaa_stats.R \
  --season=2025-26 \
  --division=1 \
  --base-url=https://njcaa.prestosports.com \
  --max-pages=30 \
  --output=JucoStatsApp/data/juco_player_stats_latest.csv
```

Local parser check:

```sh
Rscript JucoStatsApp/scripts/test_scraper_parser.R
```

Run the Shiny leaderboard app:

```sh
Rscript -e 'shiny::runApp("JucoStatsApp", host="127.0.0.1", port=3841, launch.browser=FALSE)'
```

Build the display-stat inventory from a parsed player CSV:

```sh
Rscript JucoStatsApp/scripts/build_stat_inventory.R \
  JucoStatsApp/data/juco_player_stats_inventory_sample.csv \
  --output-dir=JucoStatsApp/data
```

That writes:

- `JucoStatsApp/data/stat_availability.csv` for every normalized stat and its program coverage.
- `JucoStatsApp/data/common_display_stats.csv` for stats that are present for every parsed program in that stat bucket.

Current common display stats from the saved 11-program sample:

- Hitting: `g`, `ab`, `h`, `2b`, `3b`, `hr`, `rbi`, `bb`, `k`, `avg`, `obp`, `slg`, `ops`
- Pitching: `app`, `gs`, `w`, `l`, `sv`, `ip`, `h`, `r`, `er`, `bb`, `k`, `hr`, `hbp`, `wp`, `era`, `whip`
- Fielding: `tc`, `po`, `a`, `e`, `dp`, `sba`, `pb`, `fpct`

## Current Source Caveat

The old national aggregate stats host currently returns a disabled-site page, and direct command-line requests to PrestoSports-hosted pages may return a Cloudflare/browser challenge instead of stats HTML. The parser detects those cases and stops with a clear error.

The active path is source-driven scraping from `JucoStatsApp/data/program_sources.csv`. Add one row per program source URL. The browser scraper writes a `source_manifest.csv` with the saved HTML snapshots so the parser can attach team/program metadata to player tables that do not include a team column.

`JucoStatsApp/data/program_source_bases.csv` is the preferred editing point for Presto-style team pages. Run `scripts/build_program_sources.R` after editing it to regenerate the hitting, pitching, and fielding rows in `program_sources.csv`.

Current extra-program source status:

- Verified Presto team stats: Seminole State, Eastern Oklahoma, Connors State, Johnson County, Wabash Valley, Iowa Western, Cowley, Central Arizona.
- Verified SIDEARM cumulative stats: LSU Eunice, Salt Lake.
- Pending: Walters State uses `https://www.wsccathletics.com/sports/bsb/2025-26/teams/waltersstatecommunitycollege` as the team page, but player stats need an individual-player-page crawler.
- Pending: Yavapai is disabled because the site was down when the source list was created.

Longer-term source options:

- an approved NJCAA/Presto export or API feed,
- browser-saved HTML snapshots for the same stats pages,
- school-specific stat pages that do not block server-side requests.

The registry is intentionally simple CSV so we can add those direct source URLs program by program.
