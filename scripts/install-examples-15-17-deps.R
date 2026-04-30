#!/usr/bin/env Rscript
# Install R-side dependencies for the three "showcase" example apps:
#
#   inst/examples/15PdfShowcase
#   inst/examples/16D3Sankey
#   inst/examples/17MunicipalShowcase
#
# Run from a shell:  Rscript scripts/install-examples-15-17-deps.R
# Or from inside R:  source("scripts/install-examples-15-17-deps.R")
#
# Idempotent: every step is "install if missing."
#
# Scope notes:
#   - `dwhr` itself is not installed here; use devtools::install(".") from
#     the repo root. The examples do `library(dwhr)`.
#   - Base packages (`graphics`) are skipped.
#   - Two deps are NOT on current CRAN and are handled specially:
#       * akima         — archived from CRAN (ACM license). Pulled from
#                         CRAN archive via remotes::install_version().
#                         The `interp` package is a drop-in modern
#                         alternative if you'd rather not pull from the
#                         archive (17MunicipalShowcase/raster.R only uses
#                         akima::interp).
#       * spDataLarge   — hosted on R-universe (geocompr), not CRAN.
#   - `webshot` (used by 15PdfShowcase) needs PhantomJS at runtime;
#     after install, run `webshot::install_phantomjs()` once.

CRAN <- "https://cloud.r-project.org"
options(repos = c(CRAN = CRAN), Ncpus = max(1L, parallel::detectCores() - 1L))

cat_step <- function(msg) cat(sprintf("\n\033[1;34m[examples-deps]\033[0m %s\n", msg))
cat_ok   <- function(msg) cat(sprintf("\033[1;32m  ✓ %s\033[0m\n", msg))
cat_warn <- function(...) cat(sprintf("\033[1;33m  ! %s\033[0m\n", paste0(...)))
cat_fail <- function(msg) cat(sprintf("\033[1;31m  ✗ %s\033[0m\n", msg))

# 0. R version sanity check ------------------------------------------------

r_ver <- getRversion()
if (r_ver < "4.4.0") {
    cat_warn(sprintf("R %s detected; modernization target is >= 4.4.", r_ver))
} else {
    cat_ok(sprintf("R %s", r_ver))
}

# 1. Bootstrap helpers -----------------------------------------------------

ensure_installed <- function(pkg, ...) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        cat_ok(sprintf("%s (already installed)", pkg))
        return(invisible(TRUE))
    }
    cat(sprintf("  → installing %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE, ...)
    if (!requireNamespace(pkg, quietly = TRUE)) {
        cat_fail(sprintf("failed to install %s", pkg))
        return(invisible(FALSE))
    }
    cat_ok(pkg)
    invisible(TRUE)
}

ensure_from_repo <- function(pkg, repos) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        cat_ok(sprintf("%s (already installed)", pkg))
        return(invisible(TRUE))
    }
    cat(sprintf("  → installing %s from %s...\n", pkg, paste(repos, collapse = ", ")))
    install.packages(pkg, repos = repos, dependencies = TRUE)
    if (!requireNamespace(pkg, quietly = TRUE)) {
        cat_fail(sprintf("failed to install %s", pkg))
        return(invisible(FALSE))
    }
    cat_ok(pkg)
    invisible(TRUE)
}

# `remotes` is needed for the CRAN-archive fallback.
cat_step("Bootstrap (remotes)")
ensure_installed("remotes")

# 2. Shared deps (used by examples 15-17) ----------------------------------
#
# The package's own Imports (shiny, shinyjs, shinyjqui, data.table, magrittr,
# DT, highcharter, scales, digest, RODBC, etc.) are covered by
# scripts/install-r-deps.R — run that first if you haven't.

cat_step("Shared example deps")
shared <- c(
    "magrittr",
    "data.table",
    "shiny",
    "shinyjs",
    "shinyjqui",
    "scales"
)
for (pkg in shared) ensure_installed(pkg)

# 3. 15PdfShowcase --------------------------------------------------------

cat_step("15PdfShowcase")
deps_15 <- c(
    "knitr",            # Sweave / .Rnw rendering
    "kableExtra",       # PDF tables
    "stlplus",          # seasonal-trend decomposition (makeRData.R)
    "future",           # async PDF generation (print.R)
    "RODBC",            # comments persistence (saveComments.R)
    "shinytest",        # script/startBatchMode.R
    "webshot",          # HTML→PDF capture (run install_phantomjs() after)
    "htmlwidgets",
    "highcharter",
    "digest",
    "zoo"
)
for (pkg in deps_15) ensure_installed(pkg)

# 4. 16D3Sankey -----------------------------------------------------------

cat_step("16D3Sankey")
deps_16 <- c(
    "networkD3",        # Sankey diagrams
    "shinycssloaders"
)
for (pkg in deps_16) ensure_installed(pkg)

# 5. 17MunicipalShowcase --------------------------------------------------

cat_step("17MunicipalShowcase (CRAN)")
deps_17_cran <- c(
    "ggplot2",
    "tidyverse",        # meta-package; pulls dplyr, readr, etc.
    "Cairo",            # high-quality PNG/PDF graphics device
    "leaflet",
    "terra",            # raster/vector geospatial (replaces raster)
    "sf",               # simple features
    "tmaptools",
    "cbsodataR",        # Statistics Netherlands open data
    "readxl",
    "gapminder",
    "gganimate",
    "widgetframe",
    "highcharter",
    "dplyr"
)
for (pkg in deps_17_cran) ensure_installed(pkg)

cat_step("17MunicipalShowcase (non-CRAN)")

# akima — archived from CRAN; pull last released version from archive.
if (!requireNamespace("akima", quietly = TRUE)) {
    cat("  → installing akima from CRAN archive...\n")
    ok <- tryCatch({
        remotes::install_version("akima", version = "0.6-3.4", repos = CRAN)
        requireNamespace("akima", quietly = TRUE)
    }, error = function(e) { cat_warn(conditionMessage(e)); FALSE })
    if (!ok) {
        cat_warn("akima install failed; consider using `interp` instead ",
                 "(raster.R only calls akima::interp).")
    } else {
        cat_ok("akima")
    }
} else {
    cat_ok("akima (already installed)")
}

# spDataLarge — hosted on R-universe, not CRAN.
ensure_from_repo(
    "spDataLarge",
    repos = c(
        "https://geocompr.r-universe.dev",
        CRAN
    )
)

# 6. PhantomJS — runtime binary, not an R package -------------------------
# Required by 15PdfShowcase's `.Rnw` templates: when a Highcharter htmlwidget
# is embedded in a PDF, knitr calls `webshot::webshot()` which shells out to
# phantomjs to capture the rendered widget as a PNG. Without it, knit2pdf
# fails inside the chunk with `Error in path.expand(): invalid 'path' argument`.
# webshot ships an install helper that downloads the binary (~17 MB on macOS)
# into ~/Library/Application Support/PhantomJS/.

cat_step("PhantomJS (runtime binary for webshot)")
phantom_path <- tryCatch(webshot:::find_phantom(), error = function(e) "")
if (nzchar(phantom_path) && file.exists(phantom_path)) {
    cat_ok(sprintf("phantomjs (already at %s)", phantom_path))
} else {
    cat("  → installing phantomjs via webshot::install_phantomjs()...\n")
    ok <- tryCatch({ webshot::install_phantomjs(); TRUE },
                   error = function(e) { cat_warn(conditionMessage(e)); FALSE })
    new_path <- tryCatch(webshot:::find_phantom(), error = function(e) "")
    if (ok && nzchar(new_path) && file.exists(new_path)) cat_ok("phantomjs")
    else cat_fail("phantomjs install failed; PDF generation in 15PdfShowcase will not work")
}

# 7. Final summary --------------------------------------------------------

cat_step("Summary")
all_pkgs <- unique(c(
    "remotes", shared, deps_15, deps_16, deps_17_cran, "akima", "spDataLarge"
))
status <- vapply(all_pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) "ok" else "missing"
}, character(1))

ok_n      <- sum(status == "ok")
missing_n <- sum(status == "missing")
cat(sprintf("  %d / %d packages installed\n", ok_n, length(all_pkgs)))
if (missing_n > 0) {
    cat_warn(sprintf(
        "missing: %s",
        paste(names(status)[status == "missing"], collapse = ", ")
    ))
} else {
    cat_ok("all example dependencies present")
}

cat_step("Post-install reminders")
cat("  - Install dwhr itself from the repo root: `devtools::install('.', quick=TRUE)`.\n")
cat("  - 15PdfShowcase also needs a working LaTeX install (e.g. tinytex::install_tinytex()).\n")
