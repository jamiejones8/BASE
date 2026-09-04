#!/bin/sh
set -eu

# Railway attaches runtime storage before the start command, not during build.
cd "$(dirname "$0")"
Rscript scripts/checks/check_team_config.R

exec R --quiet -e "shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))"
