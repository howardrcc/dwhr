# Construction-and-render smoke tests, one per `presType` registered in
# `domains$presType` (`R/star.R:48`). The goal is breadth, not depth: each
# test builds a minimal star and calls `renderDims()` inside `testServer()`,
# asserting only that no error is raised. JS-side behavior (DT renderer
# output, Highcharts payloads, range slider widgets) is not exercised —
# `testServer` does not run the browser bridge.

test_that("dataTable presType: construct + render through testServer", {
    fx <- mini_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "per", name = "Periode", data = fx$dim_data,
                       levelNames = fx$level_names, useLevels = c(0, 1)) %>%
            addMeasure(dim = "per", factColumn = "val", fun = "sum",
                       as = "som", levels = c(0, 1)) %>%
            addPresentation(dim = "per", type = "dataTable", as = "tabel1",
                            isDefault = TRUE, checkUiId = FALSE,
                            dataTableOpts = list(
                                measures = list(list(viewColumn = "som")))) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})

test_that("highCharts presType: construct + render through testServer", {
    fx <- mini_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "per", name = "Periode", data = fx$dim_data,
                       levelNames = fx$level_names, useLevels = c(0, 1)) %>%
            addMeasure(dim = "per", factColumn = "val", fun = "sum",
                       as = "som", levels = c(0, 1)) %>%
            addPresentation(dim = "per", type = "highCharts", as = "chart1",
                            isDefault = TRUE, checkUiId = FALSE, height = 250,
                            highChartsOpts = list(
                                chart = list(type = "column"),
                                series = list(list(viewColumn = "som", type = "column")))) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})

test_that("radioButton presType: construct + render through testServer", {
    # radioButton/selectInput require dim `type = 'input'`, single selectMode,
    # and a single-level dim (`length(useLevels) == 2`).
    fx <- mini_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "per", name = "Periode", data = fx$dim_data,
                       levelNames = fx$level_names, type = "input",
                       selectMode = "single", useLevels = c(0, 1)) %>%
            addPresentation(dim = "per", type = "radioButton", as = "radio1",
                            isDefault = TRUE, checkUiId = FALSE,
                            simpleOpts = list()) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})

test_that("selectInput presType: construct + render through testServer", {
    fx <- mini_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "per", name = "Periode", data = fx$dim_data,
                       levelNames = fx$level_names, type = "input",
                       selectMode = "single", useLevels = c(0, 1)) %>%
            addPresentation(dim = "per", type = "selectInput", as = "select1",
                            isDefault = TRUE, checkUiId = FALSE,
                            simpleOpts = list()) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})

test_that("dateRangeInput presType: construct + render through testServer", {
    # dateRangeInput requires `dd$level == 1` (initial level) and validates
    # that `level1Label` values parse as `%Y-%m-%d`.
    fx <- date_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "per", name = "Periode", data = fx$dim_data,
                       levelNames = fx$level_names,
                       initLevel = 1, initParent = fx$level_names[1],
                       useLevels = c(1)) %>%
            addPresentation(dim = "per", type = "dateRangeInput", as = "dateRange1",
                            isDefault = TRUE, checkUiId = FALSE,
                            rangeOpts = list()) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})

test_that("rangeSliderInput presType: construct + render through testServer", {
    # rangeSliderInput requires `dd$level == 1` and numeric `level1Label`
    # values (the renderer compares `min()`/`max()` against the slider value).
    fx <- numeric_fixture()
    server <- function(input, output, session) {
        new.star(starId = "s1", session = session, facts = fx$facts,
                 foreignKeyCheck = FALSE) %>%
            addDimView(dim = "val", name = "Value", data = fx$dim_data,
                       levelNames = fx$level_names,
                       initLevel = 1, initParent = fx$level_names[1],
                       useLevels = c(1)) %>%
            addPresentation(dim = "val", type = "rangeSliderInput", as = "Range1",
                            isDefault = TRUE, checkUiId = FALSE,
                            rangeOpts = list()) %>%
            renderDims(input, output)
    }
    expect_no_error(with_authed_testServer(server))
})
