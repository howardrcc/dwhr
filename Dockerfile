# syntax=docker/dockerfile:1.7
#
# dwhr-runtime — R environment for the dwhr Shiny dashboard package + examples.
# Spec: docs/DOCKER.md.
#
# Mount-only design: this image carries only the R/system environment; the
# dwhr source and the chosen example app are bind-mounted at runtime. See
# docker-compose.yml or `make demo`.
#
# Default target arch: linux/amd64. PhantomJS has no Linux arm64 binary,
# so webshot::install_phantomjs() will fail if built for arm64. The
# docker-compose.yml pins `platform: linux/amd64` to enforce this; on
# Apple Silicon, Colima with `--vm-type vz --vz-rosetta` runs the resulting
# amd64 image through Rosetta 2 (much faster than full QEMU emulation).
# If you need native arm64, swap PhantomJS for webshot2+chromium (see
# docs/DOCKER.md §12 future work #6).
#
# Base note: rocker/r-ver:4.5.3 is **Ubuntu 24.04 LTS (Noble)**, not Debian
# Bookworm — recent rocker images switched bases. PPM URL in
# docker/install-r-packages.R is correspondingly `__linux__/noble/latest`.

ARG R_VERSION=4.5.3
FROM rocker/r-ver:${R_VERSION}

LABEL org.opencontainers.image.title="dwhr-runtime"
LABEL org.opencontainers.image.description="R env for the dwhr R/Shiny dashboard package and its example apps"
LABEL org.opencontainers.image.source="https://github.com/howardrcc/dwhr"
LABEL org.opencontainers.image.licenses="MIT"

# ---- system libraries -------------------------------------------------------
# Covers: Cairo, sf/terra geo stack, kableExtra/Sweave PDF chain, network/SSL,
# fonts (ragg/textshaping), and source-build toolchain (gfortran, pkg-config).
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl wget \
        git \
        libcairo2-dev libxt-dev \
        libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
        libssl-dev libxml2-dev libcurl4-openssl-dev \
        libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
        libpng-dev libtiff5-dev libjpeg-dev \
        gfortran pkg-config \
    && rm -rf /var/lib/apt/lists/*

# ---- R package manager ------------------------------------------------------
# `pak` does parallel binary installs against PPM. ~10x faster than
# install.packages on a cold image and produces a deterministic plan.
RUN R -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'

# ---- R packages -------------------------------------------------------------
# Single COPY + Rscript so this layer is cached against changes to the package
# list. Edit docker/install-r-packages.R to add/remove deps.
COPY docker/install-r-packages.R /tmp/install-r-packages.R
RUN Rscript /tmp/install-r-packages.R

# ---- TinyTeX (LaTeX for 15PdfShowcase Sweave .Rnw templates) ----------------
# TinyTeX-2 ships ~300 packages (vs TinyTeX-1's ~100); the 15PdfShowcase
# templates need ~25 LaTeX packages and TinyTeX auto-installs anything else
# missing on first knit2pdf. Path detection works on both x86_64 and aarch64.
RUN R -e 'tinytex::install_tinytex(force = TRUE, bundle = "TinyTeX-2")' \
    && ln -s /root/.TinyTeX/bin/*/* /usr/local/bin/ 2>/dev/null || true
ENV PATH="/root/.TinyTeX/bin/x86_64-linux:${PATH}"

# ---- PhantomJS (webshot HTML→PNG for embedded htmlwidgets in PDFs) ----------
# Bundled binary download; only ~17 MB. Installs to /root/bin/.
RUN R -e 'webshot::install_phantomjs()'

# ---- akima (archived from CRAN, needed by 17MunicipalShowcase/raster.R) -----
RUN R -e 'remotes::install_version("akima", version = "0.6-3.4", repos = "https://cloud.r-project.org")'

# ---- entrypoint helpers -----------------------------------------------------
WORKDIR /workspaces/dwhr
COPY docker/entrypoint.R /entrypoint.R
COPY docker/auth-stub.R  /auth-stub.R

# Default port; override with `-e PORT=4816` if needed.
EXPOSE 4815

# Default behavior: run the chosen example app. dwhr source mounted at
# /workspaces/dwhr; example mounted at /app. See entrypoint.R.
CMD ["Rscript", "/entrypoint.R"]
