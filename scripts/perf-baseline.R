# Performance baseline for dwhr at 1M and 10M facts rows.
#
# Output:
#   docs/perf/baseline-summary.txt — phase timings table
#   docs/perf/baseline-1M.html     — profvis flame graph for the 1M run
#
# Run from repo root with: Rscript scripts/perf-baseline.R
#
# Methodology
# -----------
# Builds a synthetic facts table joined to the real `ds_d_periode.txt`
# dimension (3 levels: total / year / month). Wraps a minimal star
# (one numeric measure aggregated by `sum`, one dataTable presentation)
# inside `shiny::testServer()` — same harness as the W5 smoke tests, so
# we measure the same code paths a deployed Shiny session would exercise,
# minus browser-side rendering.
#
# What this measures: server-side R cost of construction, first render,
# and reactive re-filtering on a large facts table.
#
# What this does NOT measure: DT clientside DOM build, Highcharts redraw,
# WebSocket transport, JS bridge in starExtend.js. Those are the *other*
# 50%+ of real-world latency — see docs/PERFORMANCE-BASELINE.md for the
# stack-level discussion.

suppressMessages({
    library(dwhr)
    library(magrittr)
    library(data.table)
    library(profvis)
    library(bench)
})

set.seed(42)

# ---- Setup ----------------------------------------------------------------

per_path <- system.file("examples/01SimpleTable/data/ds_d_periode.txt",
                        package = "dwhr")
stopifnot(nzchar(per_path))

per <- read.csv(
    file = per_path,
    header = FALSE,
    sep = ";",
    col.names = c("maandId", "level1Label", "level2Label"),
    stringsAsFactors = FALSE
)

make_facts <- function(N) {
    data.frame(
        maandId = per$maandId[sample.int(nrow(per), N, replace = TRUE)],
        num1 = runif(N, 100, 200)
    )
}

# Mirror the W5 helper-star.R pattern: initGlob without the UI side
# effects, then patch session class so assert_class(session, "ShinySession")
# passes inside testServer.
init_glob <- function() {
    if (!exists("glob.env", envir = .GlobalEnv, inherits = FALSE)) {
        suppressWarnings(asNamespace("dwhr")$initGlob())
    }
}
init_glob()

build_star <- function(session, facts, per, id = "perf") {
    new.star(
        starId = id, session = session, facts = facts,
        foreignKeyCheck = FALSE
    ) %>%
        addDimView(
            dim = "per", name = "Periode", data = per,
            levelNames = c("Alle perioden", "Jaar", "Maand"),
            useLevels = c(0, 1, 2)
        ) %>%
        addMeasure(
            dim = "per", factColumn = "num1", fun = "sum", as = "som",
            levels = c(0, 1, 2)
        ) %>%
        addPresentation(
            dim = "per", type = "dataTable", as = "tab",
            isDefault = TRUE, checkUiId = FALSE,
            dataTableOpts = list(measures = list(list(viewColumn = "som")))
        )
}

# ---- Run benchmarks -------------------------------------------------------

bench_run <- function(N) {
    facts_template <- make_facts(N)
    collected <- NULL

    # Use one testServer invocation; run bench::mark() inside it so the
    # MockShinySession is shared across iterations. testServer doesn't
    # return the body's value, so write results out via <<-.
    shiny::testServer(
        function(input, output, session) {
            class(session) <- c("ShinySession", class(session))
            session$userData$authenticated <- TRUE

            timings <- list()

            # new.star() refuses duplicate IDs within a session — reset the
            # session-side registry between iterations so each call is fresh.
            timings$construction <- bench::mark(
                {
                    session$userData$starList <- list()
                    s <- build_star(session, copy(facts_template), per)
                },
                iterations = 5, check = FALSE, filter_gc = FALSE
            )

            session$userData$starList <- list()
            s <- build_star(session, copy(facts_template), per)

            timings$first_render <- bench::mark(
                {
                    s <- s %>% renderDims(input, output)
                },
                iterations = 1, check = FALSE, filter_gc = FALSE
            )

            # factsFiltered with no dim selection is a no-op fast path
            # (reactive returns env$facts directly). Measure that, then
            # also measure with a real dim filter applied — that's the
            # actual hot path when a user clicks into a dim level.
            timings$factsFiltered_no_filter <- bench::mark(
                {
                    shiny::isolate({
                        s$reactive$factsChange <- s$reactive$factsChange + 1L
                        s$factsFiltered()
                    })
                },
                iterations = 10, check = FALSE, filter_gc = FALSE
            )

            # Simulate a dim selection: level 1 with 12 IDs (a year's
            # worth of months from the period dim).
            year_ids <- per$maandId[per$level1Label == "2017"]
            s$dims$per$selected <- data.frame(
                level = 1L, parent = "Alle perioden", label = "2017",
                stringsAsFactors = FALSE
            )
            s$dims$per$selectedIds <- year_ids

            timings$factsFiltered_with_filter <- bench::mark(
                {
                    shiny::isolate({
                        s$reactive$factsChange <- s$reactive$factsChange + 1L
                        s$factsFiltered()
                    })
                },
                iterations = 10, check = FALSE, filter_gc = FALSE
            )

            timings$facts_size_mb <- as.numeric(object.size(s$facts)) / 1024^2

            collected <<- timings
        },
        NULL
    )

    collected
}

cat("Benchmarking dwhr at 1M facts rows...\n")
results_1M <- bench_run(1e6)

cat("Benchmarking dwhr at 10M facts rows...\n")
results_10M <- bench_run(1e7)

# ---- Format results -------------------------------------------------------

fmt_row <- function(label, b) {
    if (is.numeric(b) && length(b) == 1) {
        return(sprintf("  %-22s  %.1f MB", label, b))
    }
    # bench::mark returns a tbl with `median` (bench_time) and
    # `n_itr` columns. Format median in human-readable units.
    med <- b$median
    n   <- b$n_itr
    sprintf("  %-22s  median = %s  iters = %d",
            label, format(med), as.integer(n))
}

dir.create("docs/perf", showWarnings = FALSE, recursive = TRUE)

summary_lines <- c(
    sprintf("dwhr performance baseline — generated %s",
            format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    sprintf("R %s.%s, OS %s, %d CPU cores",
            R.version$major, R.version$minor,
            Sys.info()["sysname"], parallel::detectCores()),
    sprintf("data.table threads: %d", data.table::getDTthreads()),
    "",
    "=== 1,000,000 facts rows ===",
    fmt_row("construction",          results_1M$construction),
    fmt_row("first_render",          results_1M$first_render),
    fmt_row("factsFiltered no-op",   results_1M$factsFiltered_no_filter),
    fmt_row("factsFiltered filter",  results_1M$factsFiltered_with_filter),
    fmt_row("facts in memory",       results_1M$facts_size_mb),
    "",
    "=== 10,000,000 facts rows ===",
    fmt_row("construction",          results_10M$construction),
    fmt_row("first_render",          results_10M$first_render),
    fmt_row("factsFiltered no-op",   results_10M$factsFiltered_no_filter),
    fmt_row("factsFiltered filter",  results_10M$factsFiltered_with_filter),
    fmt_row("facts in memory",       results_10M$facts_size_mb)
)

writeLines(summary_lines, "docs/perf/baseline-summary.txt")
cat(paste(summary_lines, collapse = "\n"), "\n", sep = "")

# ---- profvis flame graph (1M run) -----------------------------------------

cat("\nCapturing profvis flame graph for the 1M run...\n")
facts_1M <- make_facts(1e6)

p <- profvis::profvis({
    shiny::testServer(
        function(input, output, session) {
            class(session) <- c("ShinySession", class(session))
            session$userData$authenticated <- TRUE
            s <- build_star(session, copy(facts_1M), per) %>%
                renderDims(input, output)
            shiny::isolate(s$factsFiltered())
        },
        NULL
    )
}, prof_output = "docs/perf/baseline-1M.Rprof")

htmlwidgets::saveWidget(p, "docs/perf/baseline-1M.html", selfcontained = TRUE)
cat("Saved: docs/perf/baseline-1M.html\n")
cat("Saved: docs/perf/baseline-summary.txt\n")
