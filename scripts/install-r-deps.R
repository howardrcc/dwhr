#!/usr/bin/env Rscript
# Install R-side dev dependencies for dwhr.
#
# Run from a shell:  Rscript scripts/install-r-deps.R
# Or from inside R:  source("scripts/install-r-deps.R")
#
# Idempotent: every step is "install if missing or outdated."
#
# This script covers BOTH:
#   - The current package's declared Imports (so the unmodified package
#     can be loaded for baseline testing).
#   - The modernization-target packages (checkmate, DBI, odbc, testthat)
#     so we don't have to re-bootstrap during W2/W3.
#
# `assertive` is a special case: archived from CRAN as of 2023-07-13.
# We install it from the CRAN Archive via `remotes::install_version()` so
# the *current* code keeps working until W2 replaces every call site.

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
    "RODBC",       # to be dropped in W3, but needed for baseline
    "scales",
    "DT",
    "highcharter",
    "rlist",
    "sparkline"
)
for (pkg in current_imports) {
    ensure_installed(pkg)
}

# 3. Modernization-target deps --------------------------------------------

cat_step("Modernization-target deps (checkmate, DBI, odbc)")
target_deps <- c("checkmate", "DBI", "odbc")
for (pkg in target_deps) {
    ensure_installed(pkg)
}

# 4. Archived `assertive` from CRAN Archive --------------------------------
#
# Required only for the BASELINE phase. Once W2 lands, this can go away.
#
# `remotes::install_version()` reaches the Archive for the *named* package
# but does NOT recurse into the Archive for its archived dependencies, and
# `assertive` has 15 archived sub-packages. We work around this by
# installing each sub-package explicitly, in dependency order, before the
# top-level meta-package.

cat_step("assertive (from CRAN Archive — baseline only)")

assertive_pkgs <- c(
    # Leaves first (no inter-package deps)
    "assertive.base",
    # Direct dependents on .base
    "assertive.properties",
    "assertive.types",
    "assertive.numbers",
    "assertive.strings",
    "assertive.datetimes",
    "assertive.files",
    "assertive.sets",
    "assertive.matrices",
    "assertive.models",
    "assertive.data",
    "assertive.data.uk",
    "assertive.data.us",
    "assertive.reflection",
    "assertive.code",
    # Meta package (depends on all above)
    "assertive"
)

install_archived <- function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        cat_ok(sprintf("%s (already installed)", pkg))
        return(TRUE)
    }
    res <- tryCatch(
        {
            remotes::install_version(
                pkg,
                repos    = CRAN,
                upgrade  = "never",
                quiet    = TRUE,
                dependencies = NA
            )
            requireNamespace(pkg, quietly = TRUE)
        },
        error = function(e) {
            cat_fail(sprintf("%s: %s", pkg, conditionMessage(e)))
            FALSE
        }
    )
    if (isTRUE(res)) cat_ok(pkg)
    isTRUE(res)
}

assertive_results <- vapply(assertive_pkgs, install_archived, logical(1))
if (all(assertive_results)) {
    cat_ok("assertive + all sub-packages installed")
} else {
    failed <- assertive_pkgs[!assertive_results]
    cat_warn(sprintf(
        "could not install: %s. Package will not load until W2 lands.",
        paste(failed, collapse = ", ")
    ))
}

# 5. Final summary ---------------------------------------------------------

cat_step("Summary")
all_pkgs <- c(
    "remotes", "devtools", "roxygen2", "testthat", "rcmdcheck",
    current_imports, target_deps, "assertive"
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
