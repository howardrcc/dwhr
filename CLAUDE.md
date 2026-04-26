# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`dwhr` is an R package for building interactive Shiny dashboards over data-warehouse data using a star-schema model. A consumer wires up a `star` object (facts + dimension views + measures + presentations) in `server.R`, calls `renderDims()`, and the package handles the reactive plumbing between drillable hierarchies and DataTables / Highcharts / form controls.

User-facing documentation (roxygen blocks, error strings) is mostly in **Dutch** — preserve language when editing existing docs unless asked otherwise.

## Common commands

```r
# Dev loop (run from repo root in R)
devtools::load_all()              # source all R/ files into the session
devtools::document()              # regenerate man/*.Rd and NAMESPACE from roxygen
devtools::check()                 # R CMD check
devtools::install(".", quick=TRUE) # install into user library (needed to run examples)

# Run any of the 15 demo apps (requires install — examples do `library(dwhr)`)
shiny::runApp("inst/examples/01SimpleTable")
# or, after installing the package:
dwhr::runExampleDwhr("01SimpleTable")
```

```sh
# Shell equivalents
R CMD build .
R CMD INSTALL .
R CMD check dwhr_*.tar.gz
```

There is **no test suite** (no `tests/` directory, no testthat). The 15 apps under `inst/examples/` are the de-facto regression surface — when changing rendering or reactive code, sanity-check at least one example app per presentation type (`dataTable`, `highCharts`, `radioButton`/`selectInput`, `dateRangeInput`, `rangeSliderInput`).

## Architecture

### The `star` object is an environment, not a list

`new.star()` (R/star.R) returns an `environment` with class `'star'`. The `%>%` chain in user code (`new.star() %>% addDimView() %>% addMeasure() %>% addPresentation() %>% renderDims()`) **mutates the same environment** — each step writes into `env$dims`, `env$dtRenderers`, `env$hcRenderers`, etc. The `magrittr` pipe is used for ergonomics; semantically these are imperative side-effecting calls. Don't refactor as if these were pure functions returning new lists.

`env$ce <- parent.frame()` captures the user's calling environment so the package can resolve user-defined hook functions like `<dim>LevelChangeHook` by name (see R/observe.R).

### Reactive flow

Each dimension owns a bundle of `shiny::reactiveValues` counters (`levelChange`, `selectChange`, `selectedIdsChange`, `dimRefresh`, `visChange`, `presListChange`). Code increments a counter to fire downstream observers rather than passing values. The top-level `env$factsFiltered` reactive (R/star.R) re-filters facts whenever any filtering dim's `selectedIdsChange` ticks.

`renderDims()` (R/render.R) dispatches per dimension to the right renderer based on the dim's `presList` types. Each presentation type has its own file:

| File             | Role                                                                  |
|------------------|-----------------------------------------------------------------------|
| `R/dwhr.R`       | Largest file (~101k); domain logic, measures, formatting, caching     |
| `R/star.R`       | `new.star`, the `domains` allow-list, `domainCheck`, debug helpers    |
| `R/client.R`     | `dwhrInit()` UI shim; registers `inst/www` resource path + JS bindings |
| `R/render.R`     | `renderDims()` per-dim dispatch                                       |
| `R/dataTable.R`  | DT presentation (sparklines, colorbars, conditional formatting)       |
| `R/highCharts.R` | highcharter / Highstock presentation                                  |
| `R/dateRange.R`, `R/rangeSlider.R`, `R/simple.R` | input-control presentations            |
| `R/observe.R`    | `startObserversData()` — wires reactive observers per dim             |

The JS/CSS in `inst/www/` (notably `starExtend.js`) is exposed via `shinyjs::extendShinyjs()` in `dwhrInit()` — UI changes that need browser-side hooks go there.

### `domains` is the validation registry

`R/star.R` defines a top-level `domains` list (allowed values for `aggregateFun`, `format`, `presType`, `dataTableOpts`, etc.). `domainCheck()` validates every user-supplied option against it. When adding a new presentation type, format, or option key, you almost certainly need to extend the relevant `domains` entry or `domainCheck` will reject it.

### Authentication gate

`new.star()` requires `session$userData$authenticated == TRUE`, which is set by `authenticate(session)`. Every example's `server.R` calls `authenticate(session)` first — keep that contract intact when modifying session/init code.

## Dependency notes (modernization-relevant)

The `Imports` in `DESCRIPTION` pin some packages that have aged poorly:

- **`assertive`** is archived from CRAN. Calls like `assert_is_a_string`, `assert_is_data.frame`, `assert_is_a_bool`, `assert_is_all_of` are scattered through R/. Any modernization needs a replacement strategy (e.g., `checkmate`, base R, or vendoring a minimal subset).
- **`RODBC`** still works but `odbc` is the modern replacement; only `getDbHandle` / example 08 touch this.
- `shinyjqui`, `highcharter`, `DT`, `shinyjs` have all had API changes since the pinned minimums — `NEWS.md` 1.6.0 already mentions a prior round of upgrades.

## Conventions

- Roxygen 7.3.1 (`RoxygenNote` in `DESCRIPTION`); always run `devtools::document()` after editing roxygen blocks rather than hand-editing `man/*.Rd` or `NAMESPACE`.
- `data.table` is used internally (`setDT(facts)` in `new.star`) — reference-semantics are intentional.
- 4-space indent, UTF-8 (per `dwhr.Rproj`).
