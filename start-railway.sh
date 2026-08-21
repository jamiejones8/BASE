#!/bin/sh
set -eu

exec R --quiet -e "shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))"
