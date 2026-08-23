FROM ghcr.io/jamiejones8/base-r-dependencies:r4.4.2-v1@sha256:e70330ba4c4bc24c34c8220679923678b28825ba0fce92bdfad9f95324220dcc
WORKDIR /code
COPY . .
RUN Rscript -e "parse(file='team_config.R'); parse(file='data_access.R'); parse(file='pitch_retags.R'); parse(file='app.R'); parse(file='cape_pitcher_page.R'); parse(file='hitter_scouting_page.R'); parse(file='leaderboards/app.R')"
RUN Rscript scripts/test_pitch_retags.R
RUN Rscript leaderboards/scripts/precompute_leaderboards_cache.R
CMD ["sh", "-c", "exec R --quiet -e \"shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))\""]
