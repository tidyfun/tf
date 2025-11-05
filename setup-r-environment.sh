#!/bin/bash
# Auto-setup script for R environment
# Save this as .claude/setup-r.sh and it will run automatically in new sessions

set -e

echo "=== Setting up R environment for tf package ==="

# Check if R is already installed
if command -v R &> /dev/null; then
    echo "✓ R is already installed ($(R --version | head -1))"
else
    echo "Installing R..."
    apt-get update -qq
    apt-get install -y r-base r-base-dev
fi

# Install system dependencies for R packages
echo "Installing system dependencies..."
apt-get install -y --fix-missing \
    libcurl4-openssl-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev 2>&1 | tail -5

# Install R packages via apt (faster and more reliable)
echo "Installing R packages..."
apt-get install -y --fix-missing \
    r-cran-devtools \
    r-cran-testthat \
    r-cran-roxygen2 \
    r-cran-checkmate \
    r-cran-cli \
    r-cran-purrr \
    r-cran-dplyr \
    r-cran-vctrs \
    r-cran-zoo \
    r-cran-mvtnorm \
    r-cran-pracma \
    r-cran-rlang \
    r-cran-tidyverse 2>&1 | tail -10

echo ""
echo "=== R environment setup complete! ==="
echo ""
echo "Installed R packages:"
Rscript -e "cat(paste(rownames(installed.packages()), collapse=', '))" 2>&1 | head -5
echo ""
echo "You can now use:"
echo "  - devtools::load_all()"
echo "  - devtools::test()"
echo "  - devtools::check()"
echo ""
