FROM rocker/tidyverse:4.1.2

WORKDIR /workspace

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    curl \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libudunits2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# Set arrow environment variables
ENV LIBARROW_MINIMAL=false
ENV NOT_CRAN=true

# Copy your R scripts into the image
COPY *.R /workspace/
COPY data /workspace/data

# Install R packages efficiently including arrow
RUN R -e 'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]))); install.packages("arrow", repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")' \
    && R -e 'arrow::install_arrow()' \
    && install2.r --error \
    httr \
    httr2 \
    remotes \
    jsonlite \
    rvest \
    lubridate \
    pacman \
    openxlsx \
    xml2 \
    fs \
    countrycode \
    progress \
    cli \
    digest \
    glue \
    vroom \
    prettydoc \
    DT \
    piggyback \
    openssl

# Create necessary directories
RUN mkdir -p /workspace/historic && mkdir -p /workspace/targeting

# Set working directory permissions
RUN chmod -R 777 /workspace