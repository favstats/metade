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

# Install R packages efficiently
RUN install2.r --error \
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
    openssl \
    arrow

# Copy your R scripts into the image
COPY *.R /workspace/
COPY data /workspace/data