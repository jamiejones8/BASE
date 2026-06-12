FROM rocker/r-ver:4.4.2
WORKDIR /code
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmagick++-dev \
    libpoppler-cpp-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r --error \
    shiny htmltools dplyr ggplot2 readr httr xml2 \
    magick workflows parsnip recipes tune \
    patchwork ggridges xgboost \
    reactable tidyr purrr DBI RSQLite \
    bslib shinyjs DT data.table stringr \
    base64enc pdftools
RUN Rscript -e "install.packages('arrow', repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"
COPY . .
RUN Rscript leaderboards/scripts/precompute_leaderboards_cache.R
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]