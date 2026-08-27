FROM ghcr.io/jamiejones8/base-r-dependencies:r4.4.2-v1@sha256:e70330ba4c4bc24c34c8220679923678b28825ba0fce92bdfad9f95324220dcc
WORKDIR /code
COPY . .
RUN install2.r --error \
    shinyWidgets shinycssloaders cowplot ggplotify ggpubr ggtext hms jpeg lubridate ragg readxl
RUN Rscript -e "parse(file='team_config.R'); parse(file='R/config/team_config.R'); parse(file='R/data/data_access.R'); parse(file='R/data/pitch_retags.R'); parse(file='R/integrations/wally_scouting_workspace.R'); parse(file='WallyApps/ScoutingApp/ScoutingApp.R'); parse(file='app.R'); parse(file='R/app_main.R'); parse(file='R/pages/cape_pitcher_page.R'); parse(file='R/pages/hitter_scouting_page.R'); parse(file='leaderboards/app.R')"
RUN Rscript scripts/tests/test_pitch_retags.R
RUN Rscript scripts/tests/test_wally_pitching_integration.R
RUN Rscript scripts/tests/test_wally_hitting_integration.R
RUN Rscript scripts/tests/test_wally_defense_integration.R
RUN Rscript scripts/tests/test_wally_scouting_integration.R
RUN Rscript leaderboards/scripts/precompute_leaderboards_cache.R
CMD ["sh", "-c", "exec R --quiet -e \"shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))\""]
