FROM rocker/r-ver:4.4.2
WORKDIR /code

RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmagick++-dev \
    cmake \
    libpng-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libtiff-dev \
    libjpeg-dev \
    libcairo2-dev \
    libxt-dev \
    librsvg2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    shiny htmltools dplyr ggplot2 readr httr xml2 \
    magick workflows parsnip recipes tune \
    patchwork ggridges xgboost arrow pdftools \
    reactable tidyr purrr DBI RSQLite \
    bslib shinyjs DT data.table stringr \
    lightgbm plotly gridExtra png sysfonts showtext rsvg

COPY . .
RUN Rscript leaderboards/scripts/precompute_leaderboards_cache.R
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]