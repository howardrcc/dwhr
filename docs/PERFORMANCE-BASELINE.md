# dwhr performance baseline

Empirical performance measurements for dwhr's server-side R code at
1M and 10M fact rows. Companion to
[`docs/ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md), which uses
these numbers as input for the stack-comparison analysis.

**Reproduce:** `Rscript scripts/perf-baseline.R` from the repo root.
Outputs `docs/perf/baseline-summary.txt` and a profvis flame graph at
`docs/perf/baseline-1M.html`.

## Methodology

Synthetic facts table (random `maandId` foreign keys + `runif` `num1`
column) joined to the real `inst/examples/01SimpleTable/data/ds_d_periode.txt`
period dimension (3 levels: total / year / month). One dim, one
`sum`-aggregated measure, one `dataTable` presentation — the minimum
that exercises the full reactive pipeline.

Wrapped in `shiny::testServer()` (the same harness as the W5 smoke
tests) so we measure the same code paths a deployed Shiny session would
exercise, **minus** browser-side rendering.

What this measures: server-side R cost of construction, first
`renderDims()`, and reactive `factsFiltered()` re-filtering.

What this does **not** measure: DT clientside DOM build, Highcharts
redraw, WebSocket transport, the JS bridge in `inst/www/starExtend.js`.
Those layers add another 50%+ of real-world latency in a deployed app.

## Environment

- R 4.5.3 on macOS (Darwin 24.6.0, 8-core arm64)
- `data.table` thread count: 4 (default)
- All deps current as of 2026-04-26 main branch (W4 + W5 merged)

## Results

### 1,000,000 facts rows

| Phase | Median | Iterations |
|---|---|---|
| `new.star() %>% addDimView() %>% addMeasure() %>% addPresentation()` | **28.7 ms** | 5 |
| First `renderDims(input, output)` | **161 ms** | 1 |
| `factsFiltered()` — no dim selection (no-op fast path) | **1.23 ms** | 10 |
| `factsFiltered()` — **with** dim selection (12 IDs filtered) | **17.7 ms** | 10 |
| `facts` in memory | 11.4 MB | — |

### 10,000,000 facts rows

| Phase | Median | Iterations |
|---|---|---|
| `new.star() %>% addDimView() %>% addMeasure() %>% addPresentation()` | **210 ms** | 5 |
| First `renderDims(input, output)` | **766 ms** | 1 |
| `factsFiltered()` — no dim selection | **1.96 ms** | 10 |
| `factsFiltered()` — **with** dim selection (12 IDs filtered) | **163 ms** | 10 |
| `facts` in memory | 114.4 MB | — |

## What the numbers say

**Construction is cheap.** ~30 ms at 1M, ~200 ms at 10M. Scales
~7× for 10× rows — superlinear, almost certainly the foreign-key
uniqueness scan in `addDimView()` (`unique(env$facts[[keyColumn]])`
against the dim's key column). Not a problem unless someone is
constructing stars on every request.

**First render is felt at 10M.** 766 ms is at the upper edge of
"acceptable for a dashboard load." Worth profiling individually if
this becomes the felt bottleneck — `profvis` flame graph at
`docs/perf/baseline-1M.html` shows where the time goes for the 1M case;
re-run with `N <- 1e7` in the script to see the 10M flame graph.

**The hot path is `factsFiltered()` with a real selection.** This
runs *on every dim click and every selection change* in a deployed
dashboard. The scaling is linear: 17.7 ms → 163 ms going from 1M → 10M.
Extrapolating, **at 100M rows each click would cost ~1.6 seconds in
filtering alone**, before the renderer runs. That is the point at
which R/Shiny stops being the right architecture and DB pushdown
(DuckDB, columnar warehouse) becomes necessary.

**Memory is linear and small.** 11.4 MB / 114.4 MB for facts; star
env overhead is negligible. R's interpreter overhead is not the
constraint at any of these scales.

## Practical thresholds

| Facts rows | Server-side latency per interaction | Recommendation |
|---|---|---|
| ≤ 1M | < 20 ms filter, < 200 ms render | dwhr as-is, no tuning needed |
| 1M – 10M | < 200 ms filter, < 1 s render | dwhr as-is; turn on `factCaching = TRUE`, use `serverSideTable = TRUE` for DT presentations |
| 10M – 50M | 200 ms – 1 s filter | Consider replacing `env$facts` (data.frame in memory) with a DuckDB connection. Requires a v3.0-class refactor. |
| 50M+ | filter > 1 s | Move the star backend to a DB pushdown pattern. Either DuckDB (in-process) or a warehouse (Snowflake / BigQuery / ClickHouse). |

## What this does not tell you

- **Clientside cost.** A 200 ms render-side latency typically becomes
  a 600–800 ms perceived latency once DT has built the DOM and
  Highcharts has redrawn. The browser-side gap dwarfs the R-side gap
  at all scales below 10M rows.
- **Multi-user behavior.** Each Shiny session gets its own R process.
  The numbers above are per-session. 100 concurrent users at 10M rows
  ≈ 100 × 114 MB = 11 GB of resident facts data. Memory, not CPU, is
  the multi-user constraint.
- **Construction-overhead amortization.** A real dashboard constructs
  the star *once per session*, not per interaction. The ~200 ms 10M
  construction cost is paid at session start and never again.
- **Caching effectiveness.** dwhr has `factCaching` and DT
  `serverSideTable` opts. We did not benchmark with these enabled —
  worth doing as a follow-up to characterize the realistic floor.

## Profvis flame graph

`docs/perf/baseline-1M.html` is the full profvis output for one
end-to-end pass at 1M rows: construction → render → filtered call.
Open it in a browser; hover the call stack to see per-line cost.

The file is selfcontained (~3 MB); it is not pretty-rendered on
GitHub but renders correctly when downloaded.
