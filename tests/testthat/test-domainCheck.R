test_that("zero-length input is accepted with default minLength = 0", {
    expect_silent(dwhr:::domainCheck(character(), "aggregateFun"))
    expect_silent(dwhr:::domainCheck(NULL, "aggregateFun"))
})

test_that("a single allowed value passes for every registered domain", {
    for (dom in setdiff(names(dwhr:::domains), character())) {
        val <- dwhr:::domains[[dom]][1]
        expect_silent(dwhr:::domainCheck(val, dom))
    }
})

test_that("a vector of allowed values passes", {
    expect_silent(dwhr:::domainCheck(c("sum", "count", "median"), "aggregateFun"))
    expect_silent(dwhr:::domainCheck(c("dataTable", "highCharts"), "presType"))
})

test_that("a disallowed value raises 'Invalid <domain>'", {
    expect_error(dwhr:::domainCheck("not_a_fun", "aggregateFun"), "Invalid aggregateFun")
    expect_error(dwhr:::domainCheck("UPSIDE_DOWN", "ordering"), "Invalid ordering")
})

test_that("any disallowed element in a mixed vector raises", {
    expect_error(dwhr:::domainCheck(c("sum", "bogus"), "aggregateFun"), "Invalid aggregateFun")
})

test_that("an unregistered domain name raises 'Unknown domain'", {
    expect_error(dwhr:::domainCheck("anything", "no_such_domain"), "Unknown domain")
})

test_that("the 'length' pseudo-domain skips the value check", {
    expect_silent(dwhr:::domainCheck("anything-goes", "length"))
    expect_silent(dwhr:::domainCheck(c("a", "b", "c"), "length"))
})

test_that("length outside [minLength, maxLength] raises 'Invalid length'", {
    expect_error(
        dwhr:::domainCheck(c("a", "b", "c"), "length", minLength = 1, maxLength = 2),
        "Invalid length"
    )
    expect_error(
        dwhr:::domainCheck(NULL, "aggregateFun", minLength = 1),
        "Invalid length"
    )
})

test_that("length within [minLength, maxLength] passes", {
    expect_silent(
        dwhr:::domainCheck(c("a", "b"), "length", minLength = 1, maxLength = 2)
    )
})
