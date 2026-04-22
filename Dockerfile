FROM rocker/r-base:latest
WORKDIR /code

RUN apt-get update && apt-get install -y \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    shiny \
    htmltools \
    dplyr \
    ggplot2 \
    readr \
    httr \
    xml2

COPY . .
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]