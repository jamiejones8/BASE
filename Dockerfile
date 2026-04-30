FROM rocker/r-ver:4.4.2
WORKDIR /code

RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    shiny htmltools dplyr ggplot2 readr httr xml2 \
    magick workflows parsnip recipes tune \
    patchwork ggridges xgboost gridExtra

COPY . .
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]