FROM ghcr.io/jamiejones8/base-r-dependencies:r4.4.2-v1@sha256:e70330ba4c4bc24c34c8220679923678b28825ba0fce92bdfad9f95324220dcc
WORKDIR /code
COPY . .
RUN install2.r --error \
    shinyWidgets shinycssloaders cowplot ggplotify ggpubr ggtext hms jpeg lubridate ragg readxl \
    callr filelock httr2 keyring tidyverse
# The base image's Posit package snapshot predates valdr. Install the version
# validated by the Player Health handoff directly from the official CRAN archive.
RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/valdr/valdr_3.0.0.tar.gz', repos=NULL, type='source')"
RUN Rscript -e "files <- list.files('.', pattern='[.]R$', recursive=TRUE); invisible(lapply(files, parse))"
RUN Rscript scripts/checks/run_all.R --build
CMD ["sh", "start-railway.sh"]
