FROM rocker/r-ver:4.4.2
WORKDIR /code
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmagick++-dev \
    libpoppler-cpp-dev \
    libpng-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    librsvg2-dev \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r --error \
    shiny htmltools dplyr ggplot2 readr httr xml2 \
    magick workflows parsnip recipes tune \
    patchwork ggridges xgboost \
    reactable tidyr purrr DBI RSQLite \
    bslib shinyjs DT data.table stringr \
    base64enc pdftools \
    plotly gridExtra png sysfonts showtext rsvg \
    shinyBS glue tibble rvest
RUN Rscript -e "install.packages(c('arrow','lightgbm'), repos='https://packagemanager.posit.co/cran/latest')"
COPY . .
RUN Rscript -e "parse(file='team_config.R'); parse(file='data_access.R'); parse(file='pitch_retags.R'); parse(file='app.R'); parse(file='cape_pitcher_page.R'); parse(file='hitter_scouting_page.R'); parse(file='leaderboards/app.R')"
RUN Rscript scripts/test_pitch_retags.R
RUN Rscript leaderboards/scripts/precompute_leaderboards_cache.R
CMD ["sh", "-c", "exec R --quiet -e \"shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))\""]
