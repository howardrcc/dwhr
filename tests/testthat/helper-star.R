# Test helpers for constructing a star object inside `shiny::testServer()`.
# `glob.env` is normally created by `dwhrInit()` (called from a Shiny app's
# UI). Tests skip the UI side and call `initGlob()` directly so they can
# exercise the server-side machinery (`new.star`, `addDimView`, `addMeasure`,
# `addPresentation`, `renderDims`) in isolation.

init_glob_for_tests <- function() {
    if (!exists("glob.env", envir = .GlobalEnv, inherits = FALSE)) {
        # Suppress the "ad-user file not found" warning emitted by initGlob
        # when no admin/data/ds_ad_user.txt exists — the test environment
        # never has one, and the warning is noise.
        suppressWarnings(asNamespace("dwhr")$initGlob())
    }
    invisible()
}

# Minimal fixture: a 2-level dimension and a facts table joined on `keyId`.
# `level1Label` doubles as a sortable string for date/range tests.
mini_fixture <- function() {
    dim_data <- data.frame(
        keyId = c(1L, 2L, 3L),
        level1Label = c("A", "B", "C"),
        stringsAsFactors = FALSE
    )
    facts <- data.frame(
        keyId = c(1L, 1L, 2L, 2L, 3L, 3L),
        val = c(10, 20, 30, 40, 50, 60)
    )
    list(
        facts = facts,
        dim_data = dim_data,
        level_names = c("Total", "Item")
    )
}

# Numeric leaf-level fixture (for `rangeSliderInput`, which needs sortable
# numeric `level1Label` values).
numeric_fixture <- function() {
    dim_data <- data.frame(
        keyId = c(1L, 2L, 3L, 4L),
        level1Label = c(10, 20, 30, 40)
    )
    facts <- data.frame(
        keyId = c(1L, 2L, 3L, 4L, 1L, 2L),
        val = c(100, 200, 300, 400, 500, 600)
    )
    list(
        facts = facts,
        dim_data = dim_data,
        level_names = c("Total", "Value")
    )
}

# Date leaf-level fixture (for `dateRangeInput`, which validates that
# `level1Label` values parse as `%Y-%m-%d`).
date_fixture <- function() {
    dim_data <- data.frame(
        keyId = c(1L, 2L, 3L, 4L),
        level1Label = c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01"),
        stringsAsFactors = FALSE
    )
    facts <- data.frame(
        keyId = c(1L, 2L, 3L, 4L, 1L, 2L),
        val = c(100, 200, 300, 400, 500, 600)
    )
    list(
        facts = facts,
        dim_data = dim_data,
        level_names = c("All dates", "Date")
    )
}

# Run a server function inside `shiny::testServer`, with the auth gate
# pre-satisfied so `new.star()` doesn't `stop("Not authenticated")`.
#
# `shiny::MockShinySession` does not inherit from `ShinySession` in current
# shiny versions, but `assert_class(session, 'ShinySession')` in `new.star()`
# and `authenticate()` requires that class. We add it to the class attribute
# of the mock session — `MockShinySession` is API-compatible for the methods
# dwhr actually uses (`$userData`, `$onSessionEnded`, `$ns`, `$input`).
with_authed_testServer <- function(server, expr = quote(session$flushReact())) {
    init_glob_for_tests()
    wrapped <- function(input, output, session) {
        class(session) <- c("ShinySession", class(session))
        session$userData$authenticated <- TRUE
        server(input, output, session)
    }
    shiny::testServer(wrapped, expr)
}
