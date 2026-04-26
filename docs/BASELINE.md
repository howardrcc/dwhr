# Baseline report

Snapshot of `dwhr` 1.6.2 on a clean macOS dev environment, **before any modernization work**. This is the "before" state we'll compare every modernization PR against.

- **Date:** 2026-04-26
- **Platform:** macOS 15.7.3 (Sequoia), Apple Silicon (aarch64-darwin)
- **R:** 4.5.3 (CRAN binary via Homebrew cask)
- **Method:** `devtools::check(".", error_on = "never", document = FALSE)`
- **Raw log:** [`baseline-check.log`](baseline-check.log) (766 lines)

## Headline result

| | count | severity | CRAN blocker? |
|---|---|---|---|
| **Errors**   | 0 | — | n/a |
| **Warnings** | 4 | mixed | yes — all four |
| **Notes**    | 4 | mixed | yes — `License: Whatever`, malformed `Description` |

The package **builds and the namespace loads** on current R + current CRAN deps. That was not guaranteed: the only reason it gets that far is that we manually installed all 15 archived `assertive.*` sub-packages from CRAN Archive (see §[Install findings](#install-findings)). On a fresh setup without that workaround, the package is uninstallable.

## Install findings

1. **R 4.5.3 + every modern Imports installs cleanly** from CRAN binary on Apple Silicon. No source builds were required for `shiny`, `DT`, `highcharter`, `data.table`, `RODBC`, etc.
2. **`assertive` is uninstallable via the standard path.** It and its 15 sub-packages (`assertive.base`, `assertive.types`, `assertive.numbers`, `assertive.strings`, `assertive.datetimes`, `assertive.files`, `assertive.sets`, `assertive.matrices`, `assertive.models`, `assertive.data`, `assertive.data.uk`, `assertive.data.us`, `assertive.reflection`, `assertive.code`, `assertive.properties`) are all archived from CRAN, and `remotes::install_version("assertive")` does not recurse into the Archive for its archived deps. **Workaround:** install each sub-package individually via `remotes::install_version()` in dependency order — done by `scripts/install-r-deps.R`. **This is a hard ceiling on the package's installability; W2 (`assertive` → `checkmate`) is therefore a prerequisite for any ordinary install path, not just nice-to-have.**
3. **`odbc` install required `unixodbc`** at the system level (added to nix-darwin homebrew module).

## `R CMD check` findings

Items below are the *raw* findings; the modernization workstream that owns each is in the right column. Most map to W1 (build hygiene); none surprise the existing plan.

### Warnings (4)

| # | Finding | Files / scope | Workstream |
|---|---|---|---|
| W-a | **Non-portable file names** — directories with spaces. | `inst/examples/04DataTableStyle 1`, `inst/examples/05DataTableStyle 2` | **W1** rename to `04DataTableStyle1`, `05DataTableStyle2` |
| W-b | **Undeclared `::` imports** — `RColorBrewer`, `htmltools`, `htmlwidgets`, `jsonlite` are used via `::` but not in `Imports`. **`library(sparkline)`** is called from package code (forbidden in CRAN packages — must use `::` or `requireNamespace()`). **Declared but unused:** `assertive`, `shinyjqui`, `sparkline`. | various R/ files | **W4** declare or drop |
| W-c | **18 exported functions are undocumented** — every export without a roxygen `@title`/`@description` block. | `addFormatting`, `checkVersion`, `clone.star`, `dwhrMerge`, `getDimViewPrepData`, `getMembers`, `getReportName`, `getSelectedIds`, `invalidateCache`, `isNa`, `isNull`, `latexEscape`, `navigate`, `prepDt`, `renderDims`, `runExampleDwhr`, `setDebug`, `sparkRelativeChange` | **W7** |
| W-d | **Undocumented arguments** — args present in function signatures but missing from Rd `\arguments`. | `addDimView` (`selectParent`, `returnPrepData`, `selectedIds`); `addMeasure` (`formatColumn`); `addPresentation` (`highChartsOpts`, `rangeOpts`, `...`) | **W7** |

### Notes (4)

| # | Finding | Workstream |
|---|---|---|
| N-a | Hidden directory `.claude` shipped in source build. | **W1** add to `.Rbuildignore` |
| N-b | `DESCRIPTION` problems: malformed `Description:` (one-line, not a sentence); `License: Whatever` not standardizable; `data.table` and `assertive` listed in both `Depends` and `Imports`. | **W1** |
| N-c | Non-standard top-level files/dirs: `CLAUDE.md`, `LICENSE.md`, `docs`, `dwhr.Rproj`, `nix`, `scripts`. | **W1** add all to `.Rbuildignore` |
| N-d | **R code style** — `class(x) == "..."` comparisons in 9 places (W3 of CRAN guidance: use `inherits()`); missing `importFrom("grDevices", ...)`, `importFrom("stats", ...)`, `importFrom("utils", ...)` for base R functions used (`coef`, `lm`, `col2rgb`, `colorRampPalette`, `head`, `packageVersion`, `read.csv`); ~80 "no visible global function" complaints — most stem from the package using `Depends: assertive` rather than `importFrom` (W2 will collapse these). | **W2** + **W4** |

## What "0 errors" means and doesn't mean

The check passed structural gates (parse, install, namespace load, S3 consistency, foreign function calls, `Rd` cross-references). It did **not** verify any runtime behavior — no example apps were exercised, no `testthat` suite exists, and `\examples{}` blocks in the existing `man/*.Rd` files are absent. **Smoke tests of the example apps are still outstanding** (task #5) and will become Section 4 of this document once they run.

## Implications for the modernization plan

1. **W1 is correctly first.** Every note is a W1 fix.
2. **W2 is more urgent than the spec implies.** Originally framed as "modernize away from an archived dep"; the baseline shows it's actually "make the package installable at all on a clean system." Bumping its priority does not change the dep order in MODERNIZATION.md (W1 already lands first as a metadata-only PR), but it sharpens the framing for `NEWS.md` / cran-comments: this isn't cosmetic.
3. **No surprises in W4 (rev-dep API audit).** Current CRAN versions of `shiny`, `DT`, `highcharter`, `shinyjqui` install and load fine — runtime API changes (option-key names, signatures) are still unproven and stay on the W4 to-do.
4. **N-d: `inherits()` vs `class() == "..."`** — small, mechanical W2/W4 follow-up. 9 sites total (4 in `dwhr.R`, 2 in `highCharts.R`, 2 in `star.R`, 1 in `client.R`). Easy to bundle with the `assertive` rename pass since the same files are being touched.

## Smoke tests

Each example launched headlessly via `shiny::runApp(launch.browser = FALSE)` for 7s with `timeout --preserve-status`, capturing stdout+stderr.

| Example | `presType` covered | Result | Notes |
|---|---|---|---|
| `01SimpleTable`     | `dataTable`       | ✅ clean startup | `Listening on http://127.0.0.1:49001`, no warnings, no errors |
| `09ColumnChart`     | `highCharts`      | ✅ clean startup | `Listening on http://127.0.0.1:49002`, no warnings, no errors |
| `12DateRangeInput-1` | `dateRangeInput` | ✅ clean startup | `Listening on http://127.0.0.1:49003`, no warnings, no errors |

**What this verifies:** the package namespace loads, `authenticate(session)` + `new.star()` + the `%>%`-chain to `renderDims(input, output)` execute without error during session init across three of the five presentation types. Example data files load. Shiny binds the listening port.

**What this does NOT verify:** actual UI rendering, reactive observer firing on user input, DT/Highcharts JS-side behavior, the `selectInput`/`rangeSliderInput` presTypes (no example smoke-tested in this round), or the DB-backed example `08DataFromDb` (requires a live ODBC connection — gated behind `skip_if_not_installed("odbc")` for the future test suite).

**Implication:** the runtime code is in better shape than the static `R CMD check` suggested. Every CRAN-blocking finding is metadata or documentation, not behavior — which means W1 (build hygiene) is genuinely a low-risk first PR.
