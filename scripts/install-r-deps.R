#!/usr/bin/env Rscript
# Install R-side dev dependencies for dwhr.
#
# Run from a shell:  Rscript scripts/install-r-deps.R
# Or from inside R:  source("scripts/install-r-deps.R")
#
# Idempotent: every step is "install if missing or outdated."
#
# This script covers:
#   - The package's current declared Imports.
#   - Future modernization-target packages (DBI, odbc) so we don't have
#     to re-bootstrap during W3.
#
# Historical note: prior to W2, this script also installed the archived
# `assertive` package and its 15 sub-packages from CRAN Archive in
# dependency order. W2 replaced those calls with `checkmate`, so the
# archive workaround is no longer needed. See docs/BASELINE.md for the
# original finding.

CRAN <- "https://cloud.r-project.org"
options(repos = c(CRAN = CRAN), Ncpus = max(1L, parallel::detectCores() - 1L))

cat_step <- function(msg) cat(sprintf("\n\033[1;34m[r-deps]\033[0m %s\n", msg))
cat_ok   <- function(msg) cat(sprintf("\033[1;32m  ✓ %s\033[0m\n", msg))
cat_warn <- function(...) cat(sprintf("\033[1;33m  ! %s\033[0m\n", paste0(...)))
cat_fail <- function(msg) cat(sprintf("\033[1;31m  ✗ %s\033[0m\n", msg))

# 0. R version sanity check ------------------------------------------------

r_ver <- getRversion()
if (r_ver < "4.4.0") {
    cat_warn(sprintf(
        "R %s detected; modernization target is >= 4.4. Continuing anyway.",
        r_ver
    ))
} else {
    cat_ok(sprintf("R %s", r_ver))
}

# 1. Bootstrap install helpers --------------------------------------------

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

cat_step("Dev tooling")
for (pkg in c("remotes", "devtools", "roxygen2", "testthat", "rcmdcheck")) {
    ensure_installed(pkg)
}

# 2. Current package Imports (modern CRAN versions) ------------------------
#
# Note: this installs the *latest* CRAN version of each, not the pinned
# minimum from DESCRIPTION. The whole point of W4 is to confirm the code
# works against current versions; pinning to old minimums would defeat that.

cat_step("dwhr Imports (latest CRAN)")
current_imports <- c(
    "shiny",
    "shinyjs",
    "shinyjqui",
    "data.table",
    "digest",
    "RODBC",       # to be dropped in W3
    "scales",
    "DT",
    "highcharter",
    "rlist",
    "checkmate",   # replaced `assertive` in W2
    "sparkline"
)
for (pkg in current_imports) {
    ensure_installed(pkg)
}

# 3. Future modernization-target deps --------------------------------------

cat_step("Future modernization-target deps (DBI, odbc)")
target_deps <- c("DBI", "odbc")
for (pkg in target_deps) {
    ensure_installed(pkg)
}

# 4. Final summary ---------------------------------------------------------

cat_step("Summary")
all_pkgs <- c(
    "remotes", "devtools", "roxygen2", "testthat", "rcmdcheck",
    current_imports, target_deps
)
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
    cat_ok("all dependencies present")
}
