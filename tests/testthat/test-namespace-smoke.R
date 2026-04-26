# Regression tests for the W1 data.table import gap.
#
# In W1, `data.table` was demoted from `Depends:` to `Imports:` in DESCRIPTION
# without a corresponding `@import data.table` in `R/dwhr-package.R`. Bare-name
# calls inside dwhr internals (`setDT`, `data.table`, `copy`, `rbindlist`, `:=`)
# stopped resolving at runtime, but `R CMD check` only flagged the regression
# as "no visible global function" NOTEs. The W1 smoke tests passed because the
# tester's library still held a pre-W1 binary install whose `Depends:` arrangement
# attached `data.table` automatically.
#
# These tests exercise the import surface from inside the dwhr namespace, so a
# missing `@import data.table` would now fail the test suite instead of hiding
# behind a NOTE. See `docs/MODERNIZATION.md` §8 entry dated 2026-04-26 ("W4
# landed (partial)") for the full story.

test_that("data.table symbols used as bare names by dwhr internals resolve via the dwhr namespace", {
    ns <- asNamespace("dwhr")
    for (sym in c("setDT", "data.table", "copy", "rbindlist")) {
        expect_true(
            exists(sym, envir = ns, inherits = TRUE),
            info = sprintf("data.table symbol '%s' is not visible from the dwhr namespace", sym)
        )
    }
})

test_that("dwhrMerge runs end-to-end (exercises bare-name copy(), rbindlist(), and := via parse/eval)", {
    cumDT <- data.table::data.table(id = 1L:2L, val = c("a", "b"))
    incDT <- data.table::data.table(id = c(2L, 3L), val = c("B", "c"))

    out <- dwhrMerge(cumDT, incDT, keyCols = "id")

    expect_s3_class(out, "data.table")
    expect_setequal(out$id, c(1L, 2L, 3L))
    # Row id=2 should have been updated in place to "B" before the rbind.
    expect_equal(out[id == 2L, val], "B")
    # Row id=3 should be appended from incDT.
    expect_equal(out[id == 3L, val], "c")
})
