# 1. Use the geospatial R base—pre-baked with terra, raster, sf, and leaflet binaries
FROM rocker/geospatial:4.4.0

# Set a working directory for build operations
WORKDIR /build

# Install system-level database libraries needed for RPostgres/DBI
RUN apt-get update && apt-get install -y \
    htop \
    libpq-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

# Create and set permissions for the Shiny app directory
RUN mkdir -p /srv/shiny-server/ && \
    chown -R shiny:shiny /srv/shiny-server/ 

# 2. Install your non-spatial remaining packages via stable install2.r
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

# 3. FIX: Pin ONLY the UI engines to pre-overhaul versions to pack buttons closer together
RUN R -e "remotes::install_version('shiny', version = '1.8.0', repos = 'https://cloud.r-project.org', upgrade = 'never')" && \
    R -e "remotes::install_version('bslib', version = '0.6.1', repos = 'https://cloud.r-project.org', upgrade = 'never')"

# Install GitHub dependencies
RUN R -e "remotes::install_github('NINAnor/Norimon')" && \
    R -e "remotes::install_github('NINAnor/NinaR')" && \
    rm -rf /tmp/R_libs_* /var/lib/R/library/renv /var/tmp/R_session_*

# Set working directory and copy app
WORKDIR /srv/shiny-server/
COPY --chown=shiny:shiny app/ ./

# Remove default sample configurations safely
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/index.html \
           /srv/shiny-server/[0-9]* \
           /srv/shiny-server/.* || true

# Switch execution context to the shiny service user
USER shiny
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]