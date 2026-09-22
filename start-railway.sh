#!/bin/sh
set -eu

# Railway attaches runtime storage before the start command, not during build.
cd "$(dirname "$0")"
Rscript scripts/checks/check_team_config.R

# Limit glibc's per-thread allocation arenas so temporary report-generation
# spikes are more readily returned to the operating system.
export MALLOC_ARENA_MAX="${MALLOC_ARENA_MAX:-2}"

exec R --quiet -e "shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))"
