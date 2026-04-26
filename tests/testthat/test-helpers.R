test_that("isNull returns the fallback when x is NULL, x otherwise", {
    expect_equal(isNull(NULL, "default"), "default")
    expect_equal(isNull("x", "default"), "x")
    expect_equal(isNull(0, "default"), 0)
    expect_equal(isNull(FALSE, "default"), FALSE)
    expect_null(isNull(NULL, NULL))
})

test_that("isNa returns the fallback when x is NA, x otherwise", {
    expect_equal(isNa(NA, "default"), "default")
    expect_equal(isNa(NA_integer_, 0L), 0L)
    expect_equal(isNa(NA_real_, -1), -1)
    expect_equal(isNa("x", "default"), "x")
    expect_equal(isNa(5, 0), 5)
})

test_that("latexEscape escapes the basic LaTeX metacharacters", {
    expect_equal(latexEscape("$"), "\\$")
    expect_equal(latexEscape("#"), "\\#")
    expect_equal(latexEscape("&"), "\\&")
    expect_equal(latexEscape("%"), "\\%")
    expect_equal(latexEscape("_"), "\\_")
    expect_equal(latexEscape("{"), "\\{")
    expect_equal(latexEscape("}"), "\\}")
})

test_that("latexEscape rewrites caret as \\^{}", {
    expect_equal(latexEscape("^"), "\\^{}")
})

test_that("latexEscape rewrites tilde as \\texttt{\\~{}}", {
    expect_equal(latexEscape("~"), "\\texttt{\\~{}}")
})

test_that("latexEscape rewrites <, >, | to LaTeX commands", {
    expect_equal(latexEscape("a<b"), "a\\textless b")
    expect_equal(latexEscape("a>b"), "a\\textgreater b")
    expect_equal(latexEscape("a|b"), "a\\textbar b")
})

test_that("latexEscape rewrites a backslash to $\\backslash$", {
    expect_equal(latexEscape("\\"), "$\\backslash$")
})

test_that("latexEscape replaces newlines by default and respects replaceNewline = FALSE", {
    expect_equal(latexEscape("a\nb"), "a\\newline b")
    expect_equal(latexEscape("a\nb", replaceNewline = FALSE), "a\nb")
})

test_that("latexEscape trims surrounding whitespace", {
    expect_equal(latexEscape("  hello  "), "hello")
})
