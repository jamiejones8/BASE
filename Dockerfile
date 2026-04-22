FROM rocker/r-base:latest
WORKDIR /code

RUN apt-get update && apt-get install -y \
    libuv1-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error shiny
RUN install2.r --error htmltools
RUN install2.r --error dplyr
RUN install2.r --error ggplot2
RUN install2.r --error readr
RUN install2.r --error httr
RUN install2.r --error xml2

COPY . .
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]