FROM ghcr.io/jamiejones8/base-r-dependencies:r4.4.2-v1@sha256:e70330ba4c4bc24c34c8220679923678b28825ba0fce92bdfad9f95324220dcc
WORKDIR /code
COPY . .
RUN install2.r --error \
    shinyWidgets shinycssloaders cowplot ggplotify ggpubr ggtext hms jpeg lubridate ragg readxl
RUN Rscript -e "files <- list.files('.', pattern='[.]R$', recursive=TRUE); invisible(lapply(files, parse))"
RUN Rscript scripts/checks/run_all.R
CMD ["sh", "-c", "exec R --quiet -e \"shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '7860')))\""]
