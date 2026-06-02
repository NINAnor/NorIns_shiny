# Use the pre-baked geospatial image to avoid terra/sf compilation errors
FROM rocker/geospatial:4.4.0

# Set a working directory for build operations
WORKDIR /build

# 1. FIX: Explicitly create the 'shiny' group and user, then configure the directory
RUN groupadd -r shiny && useradd -r -g shiny shiny && \
    mkdir -p /srv/shiny-server/ && \
    chown -R shiny:shiny /srv/shiny-server/

# System updates and database libraries
RUN apt-get update && apt-get install -y \
    htop \
    libpq-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

# 2. Install remaining non-spatial R dependencies
RUN install2.r --error \
    DBI \
    RPostgres \
    remotes \
    qrcode \
    cowplot \
    rintrojs \
    plotly \
    shinythemes \
    shinyjs \
    shinyWidgets \
    pool \
    shinyvalidate \
    shinydashboard \
    shinydashboardPlus \
    shinycssloaders \
    rlist \
    datawizard

# 3. Pin UI engines to older versions to bring the radioButtons close together
RUN R -e "remotes::install_version('shiny', version = '1.8.0', repos = 'https://cloud.r-project.org', upgrade = 'never')" && \
    R -e "remotes::install_version('bslib', version = '0.6.1', repos = 'https://cloud.r-project.org', upgrade = 'never')"

# Install GitHub dependencies
RUN R -e "remotes::install_github('NINAnor/Norimon')" && \
    R -e "remotes::install_github('NINAnor/NinaR')" && \
    rm -rf /tmp/R_libs_* /var/lib/R/library/renv /var/tmp/R_session_*

# Set working directory for the Shiny app
WORKDIR /srv/shiny-server/

# This step will now execute smoothly without permissions errors
COPY --chown=shiny:shiny app/ ./

# Switch execution security context to your new shiny service user
USER shiny
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]