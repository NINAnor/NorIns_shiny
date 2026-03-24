# Use a base R image
FROM rocker/shiny:4.4.0

# Set a working directory for build operations
WORKDIR /build

# System updates and dependencies
# Combine related RUN commands to reduce image layers
RUN apt-get update && apt-get install -y \
    htop \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libpq-dev \
    # Clean up apt caches to keep the image small
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

# Create and set permissions for the Shiny app directory
# This should ideally be done before copying app files, and permissions are crucial.
RUN mkdir -p /srv/shiny-server/ && \
    chown -R shiny:shiny /srv/shiny-server/ 

# Install R dependencies (e.g., packages required by your app)
# Using install2.r from littler is generally more robust for Dockerfiles
# It handles dependencies automatically and is designed for non-interactive installs.
RUN install2.r --error \
    shiny \
    DBI \
    RPostgres \
    remotes \
    dplyr \
    ggplot2 \
    ggthemes \
    units \
    qrcode \
    cowplot \
    leaflet \
    rintrojs \
    plotly \
    shinythemes \
    shinyjs \
    shinyWidgets \
    pool \
    forcats \
    shinydashboard \
    leaflet.minicharts \
    shinyvalidate \
    shinydashboardPlus \
    sf \
    shinycssloaders
    

# Install GitHub packages using remotes::install_github
# Do this as separate steps or with specific checks if dependencies are complex
# Using R -e "..." is fine here.
RUN R -e "remotes::install_github('NINAnor/Norimon')" && \
    R -e "remotes::install_github('NINAnor/NinaR')" && \
    # Clean up R installation temporary files
    rm -rf /tmp/R_libs_* /var/lib/R/library/renv /var/tmp/R_session_*

# Set the working directory for the Shiny app
WORKDIR /srv/shiny-server/

# Copy the app directory to the image
# Make sure your 'app' directory is at the same level as your Dockerfile.
# Copying with --chown is good practice to ensure shiny user owns the files.
COPY --chown=shiny:shiny app/ ./

# Remove unnecessary sample apps provided by rocker/shiny
# Using an absolute path and ensuring '|| true' for robustness
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/index.html \
           /srv/shiny-server/[0-9]* \
           /srv/shiny-server/.* || true

# Switch to the non-root user for security
USER shiny

# Expose the port Shiny listens on
EXPOSE 3838

# Command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
