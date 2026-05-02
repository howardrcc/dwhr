#!/usr/bin/env Rscript
# R-package install plan for dwhr-runtime.
#
# Run during Docker image build. Uses `pak` to install everything in parallel
# from Posit Public Package Manager (PPM), pinning to the bookworm binary
# repo so we get prebuilt binaries instead of source compiles where possible.
#
# Edit this file to add/drop deps, then rebuild the image. The single COPY +
# Rscript step in the Dockerfile keeps this layer cached against unrelated
# Dockerfile changes.

# PPM Ubuntu-Noble binary repo (rocker/r-ver:4.5.3 is Ubuntu 24.04 LTS,
# not Debian Bookworm — recent rocker images switched bases). Massively
# faster than CRAN source for heavy packages (sf, terra, tidyverse, ...).
ppm <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"
options(repos = c(PPM = ppm, CRAN = "https://cloud.r-project.org"))

# Verify pak is loadable (Dockerfile installed it just before this).
stopifnot(requireNamespace("pak", quietly = TRUE))

deps <- c(
    # ---- dwhr DESCRIPTION Imports ----
    "shiny", "shinyjs", "shinyjqui",
    "data.table", "digest", "RODBC", "scales", "DT", "highcharter",
    "rlist", "checkmate", "sparkline", "DBI", "odbc",

    # ---- shared across examples ----
    "magrittr",

    # ---- 15PdfShowcase ----
    "knitr", "kableExtra", "stlplus", "future", "shinytest", "webshot",
    "htmlwidgets", "zoo",

    # ---- 16D3Sankey ----
    "networkD3", "shinycssloaders",

    # ---- 17MunicipalShowcase (CRAN deps) ----
    "ggplot2", "tidyverse", "Cairo", "leaflet", "terra", "sf", "tmaptools",
    "cbsodataR", "readxl", "gapminder", "gganimate", "widgetframe", "dplyr",
    # akima is archived → installed via remotes::install_version in Dockerfile.
    # spDataLarge is on R-universe (geocompr), not CRAN/PPM. Skipped here;
    # 17MunicipalShowcase/leaflet.R won't load that bit until installed at
    # runtime via:
    #   R -e 'remotes::install_github("Nowosad/spDataLarge")'

    # ---- dev / test tooling ----
    "devtools", "roxygen2", "testthat", "rcmdcheck", "remotes", "tinytex"
)

cat(sprintf("[install-r-packages] installing %d packages via pak from PPM\n",
            length(deps)))

pak::pkg_install(
    pkg = deps,
    upgrade = TRUE,
    ask = FALSE
)

# Sanity check: every dep should now be loadable.
missing <- vapply(deps, function(p) !requireNamespace(p, quietly = TRUE),
                  logical(1))
if (any(missing)) {
    stop(sprintf(
        "[install-r-packages] %d packages failed to install: %s",
        sum(missing), paste(deps[missing], collapse = ", ")
    ))
}

cat("[install-r-packages] all packages installed.\n")
