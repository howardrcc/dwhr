# dwhr Modernization Spec

Living document. Update the **Decision log** whenever a non-trivial choice is made or revised; keep the **Workstreams** section current as work lands.

Status: **draft / pre-implementation** — agreed in conversation 2026-04-26, no code changes yet.

---

## 1. Goals

1. Make `dwhr` install and `R CMD check --as-cran` clean on **R ≥ 4.4** with current CRAN versions of all dependencies.
2. Replace the archived `assertive` dependency.
3. Migrate the database layer off `RODBC` onto the modern DBI/`odbc` stack.
4. Add a real test suite (`testthat`) and CI (GitHub Actions) so future upgrades are guard-railed.
5. Submit to CRAN.

## 2. Non-goals (this phase)

- **Full Dutch → English translation** of roxygen blocks and user-facing strings. New/changed code is English; legacy strings are left until a dedicated i18n pass (see §9 TODO).
- Refactoring the star-schema architecture, reactive model, or public concepts.
- Performance work beyond what falls out of dependency upgrades.
- New features or new presentation types.

## 3. Target environment

| | Current | Target |
|---|---|---|
| Minimum R | unspecified | **R ≥ 4.4** (declared in `Depends:`) |
| Distribution | local install / GitHub | **CRAN** |
| OS test matrix | none | macOS, Ubuntu, Windows × R-release + R-devel (CI) |
| License field | `License: Whatever` | `License: MIT + file LICENSE` (CRAN form) |
| Author field | `Author:` / `Maintainer:` legacy | `Authors@R = c(...)` |

## 4. Dependency matrix

| Package | Current pin | Action | Rationale |
|---|---|---|---|
| `shiny` | ≥ 1.0.4 | bump to current CRAN; audit `shiny::*` API | 1.0 is from 2017; many APIs changed |
| `shinyjs` | ≥ 2.0 | retain, bump pin | actively maintained, low risk |
| `shinyjqui` | ≥ 0.3.2 | bump pin, audit | NEWS 1.6.0 already mentions a prior round of fixes |
| `data.table` | ≥ 1.10.4-3 | bump to current; check `setDT` semantics | core internal type |
| `digest` | ≥ 0.6.13 | bump | trivial |
| **`RODBC`** | ≥ 1.3-13 | **drop**, replace with `DBI` + `odbc` | see W3 |
| `scales` | ≥ 0.5.0 | bump | trivial |
| `DT` | ≥ 0.4 | bump, audit option keys | NEWS 1.6.0 mentions DT upgrade work |
| `highcharter` | ≥ 0.5.0 | bump, audit options | NEWS 1.6.0 mentions highcharter upgrade work |
| `rlist` | ≥ 0.4.6.1 | retain or drop if unused | grep before keeping |
| **`assertive`** | ≥ 0.3-5 | **drop**, replace with `checkmate` | archived from CRAN |
| `sparkline` | ≥ 2.0 | retain, bump | low risk |
| **`checkmate`** | — | **add** | replacement for `assertive` |
| **`DBI`** | — | **add** | replacement for `RODBC` |
| **`odbc`** | — | **add** | replacement for `RODBC` |
| **`testthat`** | — | **add** under `Suggests` | test suite |

`Imports`/`Suggests`/`Depends` will be reset per CRAN best practices (no mass-`Depends`; `assertive`/`data.table` move to `Imports`).

## 5. Workstreams

Each workstream has acceptance criteria; tick them off as the work lands.

### W1 — Build hygiene & metadata

- [ ] `License: MIT + file LICENSE`; rename `LICENSE.md` → `LICENSE` in CRAN form (year + copyright holders + standard MIT text).
- [ ] `Authors@R` populated:
  ```r
  Authors@R = c(
    person("Pieter", "Timmerman", role = "aut",
           comment = "original author, in memoriam"),
    person("Howard", "Ching Chung", email = "howardchingchung@protonmail.com",
           role = c("cre", "aut"))
  )
  ```
- [ ] `Depends: R (>= 4.4)`.
- [ ] `Description:` rewritten to a real CRAN-style paragraph (current is a one-line tautology).
- [ ] `.Rbuildignore` covers `.git`, `.Rproj.user`, `*.Rproj`, `docs/`, `.github/`, `CLAUDE.md`, `.DS_Store`.
- [ ] `inst/.DS_Store` and `.DS_Store` purged from the repo.

### W2 — `assertive` → `checkmate`

- [ ] Add `checkmate` to `Imports`.
- [ ] Mechanical rename across R/ (script + manual review):
  - `assert_is_a_string(x)` → `checkmate::assert_string(x)`
  - `assert_is_a_bool(x)` → `checkmate::assert_flag(x)`
  - `assert_is_data.frame(x)` → `checkmate::assert_data_frame(x)`
  - `assert_is_all_of(x, "Cls")` → `checkmate::assert_class(x, "Cls")`
- [ ] Spot-check 10 randomly sampled call sites for behavioral parity (especially error message wording, since some user code may grep on it).
- [ ] Drop `assertive` from `Imports` and `Depends`.

### W3 — `RODBC` → `DBI` + `odbc`

**Plain-English context (re: your earlier question).** `RODBC` is a self-contained connector: `odbcDriverConnect()` returns an opaque `RODBC` handle that you pass to `sqlQuery()` / `sqlSave()`. The modern equivalent splits into two packages: `DBI` defines a generic interface (`dbConnect`, `dbGetQuery`, `dbWriteTable`, `dbExecute`), and `odbc` implements that interface for ODBC drivers. The handle returned by `dbConnect(odbc::odbc(), ...)` is a `DBI` connection — same role as the RODBC handle, but with a standard API shared by `RPostgres`, `RMariaDB`, `bigrquery`, etc. Net upside: standard interface, better error reporting, parameterized queries (safer), active maintenance. Net downside: minor public-API break in `getDbHandle()` (already approved); small example update.

- [ ] Replace `RODBC::odbcDriverConnect(...)` with `DBI::dbConnect(odbc::odbc(), .connection_string = "...")` in `R/client.R` (3 sites).
- [ ] Replace `RODBC::sqlQuery(...)` with `DBI::dbGetQuery(...)` (reads).
- [ ] Add `DBI::dbExecute(...)` / `DBI::dbWriteTable(...)` patterns for the comment-writeback path. Use parameterized queries (`dbBind`/`dbExecute(... params = ...)`) — this is a **security improvement** over RODBC's string-concatenated SQL and should be called out in the migration notes.
- [ ] Update `inst/examples/08DataFromDb` to use `DBI::dbGetQuery`.
- [ ] Document the breaking change in `NEWS.md` with a 4-line "if you used `getDbHandle` directly, here's the new API" snippet.
- [ ] Drop `RODBC` from `Imports`; add `DBI` and `odbc`.

### W4 — Reverse-dependency API audit

For each upgraded dep, grep for usage and confirm the call sites still work:

- [ ] `shiny::*` — `reactiveValues`, `observeEvent`, `renderUI`, `addResourcePath` (used in `client.R`). All stable.
- [ ] `DT::*` — option keys used in `dataTableOpts` (see `domains` in `R/star.R`); audit against current DT options.
- [ ] `highcharter::*` — `domains$highChartsOpts`; audit against current highcharter API.
- [ ] `shinyjs::extendShinyjs` signature — verify `script` + `functions` args still match.
- [ ] `shinyjqui::*` — currently commented out (`#shinyjqui::includeJqueryUI()` in `client.R:14`); confirm it's actually still needed before keeping in `Imports`.
- [ ] `rlist::*` — grep usage; drop if unused.

### W5 — `testthat` scaffolding

- [ ] `tests/testthat.R` + `tests/testthat/` skeleton.
- [ ] Unit tests for the lowest-risk pure-ish helpers first:
  - `domainCheck()` (R/star.R) — exhaustive coverage of allowed/rejected values per domain.
  - `isNa()`, `isNull()`, `latexEscape()` — small surface, safe to lock down.
  - Format helpers in `R/dwhr.R` (TBD: identify pure ones).
- [ ] **Smoke tests** for each presentation type — load the example app, call `new.star() %>% addDimView() %>% ... %>% renderDims()` outside an interactive session using `shiny::testServer()`, assert no errors and that key reactive values update. Target: one smoke test per `presType` (`dataTable`, `highCharts`, `radioButton`/`selectInput`, `dateRangeInput`, `rangeSliderInput`).
- [ ] Coverage target: not a hard %, but every public exported function called at least once.

### W6 — GitHub Actions CI

- [ ] `.github/workflows/R-CMD-check.yaml` using `r-lib/actions/setup-r@v2` + `r-lib/actions/check-r-package@v2`.
- [ ] Matrix: `{macos-latest, ubuntu-latest, windows-latest} × {release, devel}`.
- [ ] Upload check artifacts on failure.
- [ ] Optional follow-up: `lint.yaml` (`lintr::lint_package`) and `test-coverage.yaml` (`covr` → codecov).

### W7 — Roxygen / docs touch-up

- [ ] All **new or modified** roxygen blocks written in English.
- [ ] Legacy Dutch blocks left in place this phase (see §9 TODO).
- [ ] `devtools::document()` regenerates `man/`; verify no `\href`/`\url` rot.
- [ ] Add a top-level `?dwhr` package doc page (`R/dwhr-package.R`) since CRAN expects one.

### W8 — CRAN check & submit

- [ ] `R CMD check --as-cran` clean on local + CI matrix (0 errors / 0 warnings / 0 notes, or only the standard "new submission" note).
- [ ] `urlchecker::url_check()` clean.
- [ ] `devtools::spell_check()` reviewed.
- [ ] `cran-comments.md` written.
- [ ] Submit via `devtools::release()`.
- [ ] Address reviewer feedback (expect 1–2 rounds for first submission).

## 6. Acceptance criteria (definition of done)

- `R CMD check --as-cran` passes on macOS / Ubuntu / Windows × R-release / R-devel in CI.
- No dependency on `assertive` or `RODBC`.
- `testthat` suite present and green; covers all five `presType` values via smoke tests.
- `DESCRIPTION`, `LICENSE`, `NAMESPACE`, `man/` all CRAN-conformant.
- Package accepted on CRAN (or submitted and pending review).

## 7. Risks & open questions

- **Reactive-context tests:** `shiny::testServer()` may not cover everything `dwhrInit()` does (it injects JS via `shinyjs::extendShinyjs`). If smoke tests can't exercise the rendering path headlessly, fall back to: (a) sourcing example apps with `shinytest2`, or (b) accepting that JS-side behavior stays manually verified.
- **`odbc` driver availability in CI.** RODBC migration tests need an ODBC driver. Plan: gate DB tests with `skip_if_not_installed("odbc")` and `skip_on_cran()`; do not require a live DB on CRAN's check farm.
- **`shinyjqui` necessity.** If grep shows no live usage, dropping the dep simplifies CRAN review.
- **`magrittr` pipe vs base `|>`.** Examples use `%>%`. Current pin is implicit via `data.table`/`shiny`. Decision: keep `%>%` to avoid touching every example; do not migrate to `|>` in this phase.

## 8. Decision log

Append-only. New entries on top.

| Date | Decision | Rationale |
|---|---|---|
| 2026-04-30 | **R/star.R:852 `getFirstRow` length-safety fix.** Replaced `is.null(firstRow) \|\| is.na(firstRow) \|\| length(firstRow) == 0` with `length(firstRow) == 0 \|\| is.na(firstRow[1])`. R 4.2+ rejects length-N → logical(1) coercion in `\|\|`/`&&`; `which(tab$member == nm)` returns length-N when `tab$member` has duplicates (legitimate or via demo synth-label collisions in 15PdfShowcase), so the old code crashed on any duplicate-member dim refresh. Reinstall + 15PdfShowcase opmerking-flow smoke test confirmed fix; surface visible in monitor task `b5fwcu8hc`. | Standing rule (per memory `feedback_fix_in_app_not_package`): prefer app-side fixes over R/ changes. Justified here because the bug exists for *any* duplicate-member input, not just our anonymized demo data — every example app + downstream consumer is at risk. Companion follow-up: scan remaining `\|\|`/`&&` chains in `R/dataTable.R` (line 152, `fmt == 'sparkline'` after `!all(is.na(fmt))`) for similar length-coercion risk; defer pending a real reproducer. |
| 2026-04-28 | **Version bump: 1.6.2 → 1.7.0.9000.** `DESCRIPTION` `Version` field updated. `.9000` follows R's standard dev-version convention (CRAN sees `1.7.0` on next release; intermediate dev installs identify as `1.7.0.9000`). | Internal modernization (W3 RODBC→DBI/odbc, Docker dev container, Nix-darwin R env) doesn't break the public API, so a 2.0 major bump would over-claim. Reserved for the day an exported function (e.g. `getDbHandle`) actually changes signature. |
| 2026-04-28 | **CRAN binary R uninstall: deferred.** Nix R (4.5.2) is now PATH-active via `~/.dotfiles/modules/darwin/r.nix` (added today; imported from `hosts/Mac/default.nix`); CRAN R 4.5.3 at `/Library/Frameworks/R.framework/...` is shadowed but kept. dwhr 1.6.2 was reinstalled into Nix R's user library at `~/Library/R/arm64/4.5/library/dwhr` so the example apps still load. | Both R installs share the same `R_LIBS_USER` path on macOS, so old packages compiled against CRAN R 4.5.3's ABI may produce "different internals" warnings when Nix R 4.5.2 tries to load them. Mitigation if/when removing CRAN R: wipe `~/Library/R/arm64/4.5/library/*` first, then reinstall dwhr against Nix R only. Deferred — kept as a safety net while the Nix R env beds in. |
| 2026-04-28 | **R env declarative on macOS: `modules/darwin/r.nix`.** New nix-darwin module pins `R` + every CRAN dep used by dwhr's `Imports` and the three showcase example apps (15PdfShowcase / 16D3Sankey / 17MunicipalShowcase) — 40 packages total — via `pkgs.rWrapper.override`. `texlive.combined.scheme-small`, `pkg-config`, `gcc`, `gnumake` added to system packages for source builds. `spDataLarge` (not in CRAN/nixpkgs) installed once via `remotes::install_github("Nowosad/spDataLarge")` into the user library. | Solves the prior install-script Cairo/akima/spDataLarge build failures on macOS (missing pkg-config, gfortran, X11). All packages verified present in nixpkgs-unstable; reproducible across machines via `flake.lock`. Path forward for Linux/Windows: parallel `scripts/install-system-deps-debian.sh` + Rtools note (deferred). |
| 2026-04-26 | **W5 landed (complete).** Added one `testServer()`-driven smoke test per `presType` (`dataTable`, `highCharts`, `radioButton`, `selectInput`, `dateRangeInput`, `rangeSliderInput`) in `tests/testthat/test-presentation-smoke.R`, plus `helper-star.R` providing `init_glob_for_tests()`, `mini_fixture()`/`numeric_fixture()`/`date_fixture()`, and `with_authed_testServer()` (handles auth-gate + `MockShinySession` class patch). 71 PASS / 0 FAIL / 0 SKIP. `R CMD check`: 0E/2W/1N, **unchanged from the W4 baseline** — tests added no new warnings or notes. Log: [`docs/w5-check.log`](w5-check.log). | Two test-only workarounds called out in the helper file: (1) `MockShinySession` doesn't inherit from `ShinySession` in current shiny, so we add `"ShinySession"` to its class vector — production type checks pass, and `MockShinySession` is API-compatible for the methods dwhr actually touches; (2) every presentation passes `checkUiId = FALSE` because the UI side (`getDimUI()`) isn't called in headless tests. Both are isolated to the helper layer; production code is untouched. JS-side behavior remains untested (per spec §7) — `shinytest2` could be added later if needed. |
| 2026-04-26 | **W5 landed (first slice).** `tests/testthat/` scaffolding + 3 test files; `testthat (>= 3.0.0)` added to `Suggests`, `Config/testthat/edition: 3`. Coverage: `domainCheck()` (every registered domain, length bounds, unknown-domain path); exported helpers `isNa()`/`isNull()`/`latexEscape()`; **W1 data.table-import regression test** in `test-namespace-smoke.R` — asserts `setDT`/`data.table`/`copy`/`rbindlist` resolve from `asNamespace("dwhr")` and exercises bare-name `copy()`/`rbindlist()`/`:=` end-to-end through `dwhrMerge()`. 65 PASS / 0 FAIL. | Locks down the lowest-risk pure helpers and turns the W1 NOTE-as-runtime-crash class into a test failure rather than something that hides behind `R CMD check`'s "no visible global function" notes. Per-`presType` `testServer` smoke tests deferred to a follow-up commit on the same branch (auth-gate + `parent.frame()` capture need a small helper layer). |
| 2026-04-26 | **W4 landed (partial).** `R CMD check` now 0E/2W/1N (down from baseline 0/4/4). Net: package code wires up `@import data.table`, `@import shiny`, plus `@importFrom` for `magrittr`/`highcharter`/`utils`/`stats`/`grDevices`. `RColorBrewer`, `htmltools`, `htmlwidgets`, `jsonlite`, `magrittr` added to `Imports`. `shinyjqui` dropped (verified unused — only a commented-out site). 9 `class(x) == 'string'` patterns rewritten to `inherits()` / `is.function()`. `utils::globalVariables()` declares `glob.env`/`level0Label`/`point` (NSE symbols static analysis can't see). Log: [`docs/w4-check.log`](w4-check.log). | **Important context: W1 silently introduced a runtime regression** by demoting `data.table` from `Depends:` to `Imports:` without adding NAMESPACE imports. Bare-name calls like `data.table()` failed at runtime even though `R CMD check` only emitted "no visible global function" notes. The W1 verification missed this because the smoke tests ran against a pre-W1-installed binary still attaching `data.table` via the old `Depends:`. **Fix going forward:** every PR that touches `DESCRIPTION` or `R/dwhr-package.R` reinstalls before smoke-testing. |
| 2026-04-26 | **W4b deferred:** `htmlwidgets:::getDependency`, `shiny:::resolve`, and `sparkline` "declared but unused" note. | All entangled — sparkline only referenced as a string inside the `htmlwidgets:::` call, so fixing one without the other is messy. No example app uses `format='sparkline'`, so the fix has no smoke-test surface (silent regression risk). Better landed alongside W5 testthat coverage or a new sparkline example. |
| 2026-04-26 | **W2 landed.** All 194 `assertive` call sites across `R/client.R` (13) and `R/dwhr.R` (181) replaced with `checkmate` equivalents; `assertive` removed from `Imports`; `R/dwhr-package.R` added with `@importFrom checkmate ...`. `R CMD check`: still 0E/3W/1N, but the NOTE substance shifted from `assert_*` "no visible global function" (the dominant noise) to a much shorter list of base-R/`shiny` `importFrom` gaps (W4 territory). All 3 smoke-tested example apps clean. Log: [`docs/w2-check.log`](w2-check.log). | Removes the CRAN-Archive install workaround entirely. Package is now installable on a fresh system without sub-package gymnastics — the baseline regression is genuinely fixed. |
| 2026-04-26 | **W2 mapping policy: inline (not shim) for non-1:1 mappings.** Six `assertive` functions had no direct `checkmate` equivalent (`assert_has_no_duplicates`, `assert_are_same_length`, `assert_are_disjoint_sets`, `assert_is_non_empty`, `assert_all_are_positive`, `assert_all_are_date_strings`). Replaced inline using `checkmate::assert_true(...)` rather than building a custom shim layer. | Per spec §5 W2 "no shim layer"; inline keeps the call sites obvious and avoids a private API that future maintainers would have to learn. |
| 2026-04-26 | **W2 namespace style: `@importFrom` for the 11 most-used `checkmate` functions, `::` for rare ones (e.g. `checkmate::test_flag`).** | 194 call sites — bare-name calls keep diff size manageable. `::` for the rare `test_flag` (2 sites) avoids polluting NAMESPACE. |
| 2026-04-26 | **W1 landed.** `R CMD check` now reports 0 errors / 3 warnings / 1 note (down from 0/4/4). Eliminated: portable file names, `.claude` hidden files, `DESCRIPTION` metadata, top-level files. Remaining issues all map cleanly to W2 (`assert_*` no visible global function), W4 (undeclared `::` imports, `library(sparkline)`), W7 (undocumented exports + arguments). Log: [`docs/w1-check.log`](w1-check.log). | Validates W1 was correctly first-and-mechanical; confirms remaining workstream assignments. |
| 2026-04-26 | **LICENSE form: copyright holder is Howard only**, year 2025-2026. | Matches Howard's existing relicense intent in `LICENSE.md`; Pieter is credited via `Authors@R` `comment = "original author, in memoriam"`, which is the canonical CRAN attribution path. |
| 2026-04-26 | **Baseline established** on R 4.5.3: `R CMD check` returns 0 errors / 4 warnings / 4 notes. Full report in [`docs/BASELINE.md`](BASELINE.md). | Confirms W1 + W2 ordering; surfaces W4 sub-tasks (`inherits()` rewrite, undeclared `::` imports, `library(sparkline)` removal). |
| 2026-04-26 | **W2 reframed as a prerequisite, not a modernization step.** All 15 `assertive.*` sub-packages are archived; on a clean system the unmodified package is uninstallable until `assertive` is replaced. | Surfaced by the baseline install: `remotes::install_version` doesn't recurse into Archive for archived deps. Doesn't change PR order — W1 still lands first as metadata-only — but updates `NEWS.md` framing for the v2.0 release. |
| 2026-04-26 | **Pandoc → nixpkgs, not brew.** `pandoc` lives in `environment.systemPackages` (Howard's `~/.dotfiles/modules/darwin/packages.nix`); only `r` (cask) and `unixodbc` (brew) need to be in the brew module. | Pandoc has no PATH constraints that force brew; matches existing pattern of putting CLI runtimes in nixpkgs. |
| 2026-04-26 | **No R version pinning** for local dev. Brew cask installs latest CRAN R (currently 4.5.3 ≥ 4.4 floor). | Reproducibility belongs in CI matrix (W6), not on the dev box; pinning to 4.4 locally would hide forward-compat regressions we want to catch. `rig` is the right tool if version-specific debugging is needed later. |
| 2026-04-26 | Defer Dutch → English translation of legacy strings to a follow-up phase. | Scope control; touch only what we're modifying. |
| 2026-04-26 | Add GitHub Actions CI (R-CMD-check matrix) as part of this phase. | CRAN target makes CI essential; cheap to add now. |
| 2026-04-26 | License: `MIT + file LICENSE`, standard CRAN form. | `LICENSE.md` already references MIT; aligns with CRAN convention. |
| 2026-04-26 | `Authors@R`: Pieter Timmerman as `aut` (in memoriam), Howard Ching Chung as `cre`+`aut`. | Preserve original authorship; new maintainer. |
| 2026-04-26 | API may break (modernize freely); document breaks in `NEWS.md`. | User approval; small user base; CRAN debut is a natural reset point. |
| 2026-04-26 | Minimum R: 4.4. | Covers two latest stable lines; matches modern dep baselines. |
| 2026-04-26 | Target CRAN. | User goal; forces clean metadata, license, docs. |
| 2026-04-26 | RODBC → DBI + odbc (breaking change to `getDbHandle`). | RODBC aging; DBI is standard; parameterized queries are a security win for the comment-writeback path. |
| 2026-04-26 | `assertive` → `checkmate` (rejected: base R, vendored shim). | Active maintenance, best error UX, widely used; new dep is acceptable per user. |

## 9. TODO / deferred

- **i18n: Dutch → English full pass.** Translate remaining roxygen blocks and `stop()` / `warning()` strings in `R/*.R`. Roughly: `R/dwhr.R` (largest), `R/star.R`, `R/observe.R`, `R/dataTable.R`, `R/highCharts.R`. Do as a single PR after the CRAN release lands so it's a clean diff.
- **`magrittr` `%>%` → base `|>`** examples & internals (cosmetic; defer until after CRAN release).
- **`lintr` + `covr` CI jobs** (W6 optional follow-up).
- **`pkgdown` site** for docs hosting.
- **Migration guide** for users of v1.x → v2.x in `vignettes/`.
- **W10 — DuckDB-backed facts (performance ceiling lift).** Replace the in-memory `env$facts` data.table with a DuckDB connection so aggregations push down into DuckDB. Lifts the practical scale ceiling from ~10M rows (current, measured in [`docs/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md): 163ms `factsFiltered()` per click at 10M) to 100M+ rows comfortably. The dwhr DSL doesn't change; only the backend swaps via a small adapter. Estimated effort: 2-4 weeks. Strongest "lightest touch, biggest scale unlock" upgrade post-CRAN.
- **W11 — UI modernization (`bslib` refresh).** Bring dwhr's UI vocabulary out of Bootstrap-4 territory using `bslib`'s themed Bootstrap 5 layer. Visible polish improvement without rewriting the framework. Estimated effort: 1-2 weeks. Part of the "modernize the look without leaving R" path framed in [`docs/ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md) §8.

## 10. Reference docs (analysis, not workstreams)

These are companion analyses — input for future scoping conversations,
not work to be done in this phase.

- [`CHARTING-ALTERNATIVES.md`](CHARTING-ALTERNATIVES.md) — Highcharts licensing + permissive-license replacements (`echarts4r`, `r2d3`, `billboarder`, `plotly`).
- [`PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md) — measured server-side R performance at 1M and 10M facts rows (reproducible via `scripts/perf-baseline.R`). Dutch: [`nl/PERFORMANCE-BASELINE.md`](nl/PERFORMANCE-BASELINE.md).
- [`ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md) — stack-level evaluation of dwhr vs Streamlit, Dash, FastAPI+React/TS, Apache Superset, Evidence/Observable, Metabase. Covers the BI governance loop (write-back + PDF), visual ceiling, and AI-assistability dimensions. Dutch: [`nl/ARCHITECTURE-FUTURES.md`](nl/ARCHITECTURE-FUTURES.md).
- [`BI-STACK-COMPLEMENTS.md`](BI-STACK-COMPLEMENTS.md) — three-tier BI framing (broad consumption / bespoke governance / self-service); detailed Apache Superset vs Power BI comparison; Excel ceiling + Power Pivot; modern self-service tools (Hex, Sigma, Observable Framework, Rill, Mode); pure-OSS self-service stack. Dutch: [`nl/BI-STACK-COMPLEMENTS.md`](nl/BI-STACK-COMPLEMENTS.md).
- [`nl/PPTX-ALTERNATIVE.md`](nl/PPTX-ALTERNATIVE.md) (Dutch only) — analysis of the alternative governance loop proposed by another team within the org: Power BI + PowerPoint snapshots + manager commentary in `.pptx` instead of dwhr's DB-write-back + rmarkdown-PDF. Pros, cons, when each pattern wins, hybrid possibility (officer-package PPTX generation from dwhr), and seven governance questions to clarify with the proposing team before any tool decision.
