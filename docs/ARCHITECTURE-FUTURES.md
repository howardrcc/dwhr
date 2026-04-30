# Architecture futures — stack-level alternatives to dwhr

Reference document for an open-ended conversation about whether the R/Shiny
stack is the right long-term home for the kind of BI workload `dwhr` enables.
**This is analysis, not a decision.** The current modernization phase is
finishing dwhr's CRAN debut on the R/Shiny stack; this doc is input for any
future "should we rebuild?" conversation.

Companion to [`docs/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md), which
provides the empirical numbers cited below.

---

## 1. The actual workload dwhr enables

`dwhr` is **a programming primitive in the R BI world**, not a finished
dashboard product. The package itself is read-only on data and stops at
"render an interactive star-schema dashboard." Around it, consumers build
the rest of a closed-loop BI workflow:

```
┌───────────────────────────────────────────────────────────────────┐
│  Data warehouse / ODBC                                            │
│       │                          ▲                                │
│       │ read facts/dims          │ writeback                      │
│       ▼                          │ (manager comments              │
│  ┌─────────────────┐             │  on KPIs, decisions,           │
│  │  dwhr dashboard │             │  audit trail)                  │
│  │  (R/Shiny)      │─── click ──▶│                                │
│  │                 │   drill,    │                                │
│  │  star-schema    │   filter,   │                                │
│  │  drill-down,    │   comment   │                                │
│  │  measures,      │             │                                │
│  │  charts/tables  │             │                                │
│  └─────────────────┘             │                                │
│       │                          │                                │
│       └──── data ────────────────┘                                │
│                                                                   │
│       │ snapshot for board                                        │
│       ▼                                                           │
│  ┌─────────────────┐                                              │
│  │ rmarkdown /     │                                              │
│  │ Sweave + LaTeX  │  ──────▶  PDF report                         │
│  │ (latexEscape    │           (board-level                       │
│  │  helper from    │            accountability)                   │
│  │  dwhr)          │                                              │
│  └─────────────────┘                                              │
└───────────────────────────────────────────────────────────────────┘
```

**Three things this loop has to support that pure dashboards do not:**

1. **Write-back to the warehouse.** Managers don't just view KPIs — they
   *comment on them*, and those comments are persisted (audit-trail
   semantics, governance, "what did the board decide last quarter").
2. **Programmable PDF generation.** A real accountability report — not a
   screenshot, not a PNG export — with structured commentary, tables,
   charts, headers, footers, signing pages. R's `rmarkdown` / `Sweave` +
   `latexEscape()` handle this natively; most BI tools don't.
3. **Custom business logic in measures.** dwhr exposes
   `addMeasureDerrived(userFunc = ...)` so an aggregation can be an
   arbitrary R function, plus per-dim hooks (`<dim>LevelChangeHook`) for
   bespoke event handling. The package is *programmable from day one*.

This loop is the deciding lens. Any candidate stack has to be evaluated
on **all three** dimensions, not just "can it draw a chart."

## 2. Where the time goes today

From [`docs/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md), measured
on R 4.5.3:

| Phase | 1M rows | 10M rows |
|---|---|---|
| Star construction | 29 ms | 210 ms |
| First render | 161 ms | 766 ms |
| `factsFiltered()` no-op | 1.2 ms | 2.0 ms |
| **`factsFiltered()` with dim selection** | **17.7 ms** | **163 ms** |

Server-side R is **not** the bottleneck up to 10M rows. The hot path
(`factsFiltered()` with a real selection) is 17.7 ms at 1M and 163 ms
at 10M — perceptible but not painful. The browser-side cost (DT building
the DOM, Highcharts redrawing) typically adds another 50–100% on top of
those numbers in a deployed app.

Practical thresholds:
- **≤ 10M rows**: dwhr as-is is fine.
- **10M – 50M rows**: needs `factCaching = TRUE` and `serverSideTable =
  TRUE`; consider DuckDB backend.
- **50M+ rows**: must push aggregation into a DB (DuckDB in-process or a
  warehouse). At this scale R/Shiny vs Python/anything is irrelevant —
  the SQL engine does the work.

**The "single core per session" framing is real but doesn't bite the way
it sounds.** Each Shiny session gets its own R process. So 100 concurrent
users = 100 R processes scheduled across cores. The constraint isn't
"R is slow per-core"; it's "each user holds the full facts table in
their session." That constraint exists in Streamlit, Dash, and any
in-memory dashboarding framework. DB pushdown sidesteps it in any
language.

## 3. Evaluation dimensions

A serious comparison has to score candidates on **all** of these, not
just the easy ones:

| # | Dimension | Why it matters for dwhr's workload |
|---|---|---|
| 1 | **Read interactivity** (drill-down, filtering, charts) | Table stakes; everyone passes |
| 2 | **Write interactivity** (KPI comments → DB) | Closed-loop governance — a hard requirement |
| 3 | **Report generation** (real PDF, not screenshot) | Board accountability — a hard requirement |
| 4 | **Custom business logic** (programmable measures, hooks) | dwhr's defining feature; not optional |
| 5 | **Scale ceiling** (1M / 10M / 100M+ rows) | Where the architecture breaks |
| 6 | **Visual ceiling** (UX polish, modern UI) | Defines whether you can "build the finest dashboards" or settle for "good enough" |
| 7 | **AI-assistability** (how well Claude/Copilot help) | Modern reality; affects iteration speed and maintainer hiring |
| 8 | **Hiring ecosystem** | Who can you hire to maintain it in 5 years |
| 9 | **Migration cost from dwhr** | What does it actually take |
| 10 | **Multi-user / governance** (auth, roles, audit) | Real-world deployment |

## 4. Stack candidates

Eight candidates, scored against the dimensions above. Disqualifiers
are called out explicitly.

### 4.1 dwhr today — R/Shiny + data.table + DT + Highcharts + RODBC + downstream rmarkdown/LaTeX

- **Read**: ✓ — that's its whole job.
- **Write**: ✓ — consumer adds `RODBC::sqlSave` / `DBI::dbWriteTable`
  after W3 lands.
- **PDF**: ✓✓ — rmarkdown + Sweave + LaTeX is best-in-class for
  programmatic accountability reports. `latexEscape()` is the package's
  contribution to that pipeline.
- **Custom logic**: ✓✓ — `addMeasureDerrived(userFunc = ...)`, dim hooks,
  arbitrary R functions in measures. Programmable from day one.
- **Scale**: 10M rows comfortable; 50M+ painful without DuckDB backend.
- **Visual ceiling**: medium — Shiny's UI vocabulary is dated, DT and
  Highcharts both look 2018. CSS overrides are possible but fight the
  framework. Polish ceiling is below modern React.
- **AI-assistability**: medium — Claude/Copilot know R but the long tail
  of Shiny / `htmlwidgets` / reactive idioms is less well-trained than
  the React/TS ecosystem. Generation quality drops on bespoke patterns
  like dwhr's reactive counters.
- **Hiring**: narrow — R Shiny developers are a smaller pool than
  Python or JS devs and concentrated in academia/biotech.
- **Migration cost**: zero (baseline).

### 4.2 dwhr + DuckDB backend

The dwhr abstractions stay; `env$facts` becomes a DuckDB connection
instead of an in-memory data.table. Aggregations push down into DuckDB.

- **Read** ✓, **Write** ✓ (via DBI), **PDF** ✓✓, **Custom logic** ✓✓
  (just write SQL or use DuckDB's R UDF support).
- **Scale**: 100M+ rows comfortable; billions feasible.
- **Visual ceiling**: same as dwhr (no UI change).
- **Migration cost**: 2-4 weeks. A v3.0 workstream after CRAN.

This is the **lowest-cost serious upgrade**. Keeps Pieter's design,
keeps the R BI loop, removes the scale ceiling. Strongest candidate
if the goal is "make dwhr's usable scale match modern data warehouses."

### 4.3 Streamlit + Polars

Python equivalent of Shiny. Polars (Rust under the hood) is fast.

- **Read**: ✓.
- **Write**: ✓ (any DB driver).
- **PDF**: △ — Quarto-Python or WeasyPrint/Jinja exist but the polish
  gap vs R rmarkdown is real. Plotting tables/charts into a board-grade
  PDF is more wiring.
- **Custom logic**: ✓ — Python functions everywhere.
- **Scale**: similar to dwhr+DuckDB if you wire DuckDB or Polars
  lazyframes; in-memory ceiling is similar.
- **Visual ceiling**: low. Streamlit's UI vocabulary is *more* dated
  than Shiny's; less customizable. Polish ceiling is *worse*.
- **AI-assistability**: high — Streamlit is well-trained.
- **Hiring**: easy.
- **Migration cost**: 4-6 month rewrite. New repo. dwhr's DSL gone.

Streamlit is appealing because Python is widespread, but for *this
workload* (programmable BI primitive with PDF reports) it is a strict
downgrade. Pick if Python is mandated; otherwise skip.

### 4.4 Dash (Plotly Python) + DuckDB + ReportLab

More "framework"-shaped than Streamlit. Closer to Shiny in spirit.

- **Read** ✓, **Write** ✓, **PDF** △ (ReportLab is fine but verbose),
  **Custom logic** ✓, **Scale** ✓ (DuckDB).
- **Visual ceiling**: medium. Plotly chart vocabulary; UI is React under
  the hood but you still write Python and fight Plotly idioms.
- **AI-assistability**: high.
- **Migration cost**: 4-6 month rewrite.

A reasonable Python equivalent of dwhr+DuckDB. Same scale story, weaker
PDF story, similar visual ceiling. Choose if you specifically want a
Python-native BI framework with Shiny-like reactive semantics.

### 4.5 FastAPI + React/TS + DuckDB (+ Puppeteer for PDF)

The "rebuild from scratch with a modern frontend" option. Backend is a
Python or TypeScript API; frontend is a React app with a modern
dataviz stack (D3, ECharts, Recharts, visx, Plotly).

- **Read** ✓, **Write** ✓, **PDF** ✓ (Puppeteer + headless Chromium
  rendering of an HTML report — works well, but you build the report
  template yourself; no rmarkdown equivalent), **Custom logic** ✓
  (anywhere in the stack), **Scale** ✓ (DuckDB or warehouse).
- **Visual ceiling**: ✓✓✓ — the *highest of any candidate*. React +
  TypeScript + a modern design system (Radix, shadcn/ui, Tailwind) lets
  you build dashboards that look like Linear, Vercel, Stripe — the
  current ceiling for BI UX. No framework constraints; you ship what
  you can design.
- **AI-assistability**: ✓✓✓ — the *highest of any candidate*. React +
  TypeScript is the best-trained stack for current LLMs. Claude / Copilot
  generate idiomatic, type-safe components fluently. Component-level
  iteration is fast, refactors are safer (TS catches breakage), and the
  ecosystem (shadcn/ui, Radix primitives, Tailwind) is well-documented
  in training data. **This is real**, not just hype: a single engineer
  with Claude can sustain a React/TS dashboard codebase that would
  otherwise need a small frontend team.
- **Hiring**: ✓✓ (largest pool of any candidate).
- **Migration cost**: 6-12 months for a small team. New repo. dwhr's
  DSL has to be either reimplemented in TS or replaced by a different
  abstraction. The R BI loop (rmarkdown, latexEscape, R-in-the-loop
  consumer code) is gone — replaced by JS/TS equivalents that you build.
- **Cost of leaving R**: this is the deepest cut. The downstream R code
  that uses dwhr (rmarkdown reports, ad-hoc analysis, the consumer
  layer that does write-back and PDF generation) doesn't migrate; it's
  rewritten in Python or TS.

**The honest framing**: this is a different *product*, not a *port*.
You get the best dashboards you can build today, at the cost of leaving
the R BI ecosystem behind. For a CRAN library aimed at R developers,
this isn't a fit. For "we want to build the finest internal BI
platform our org has ever had, and we're willing to staff it" — it's
the right answer.

### 4.6 Apache Superset — out of scope (disqualified on dimensions 2, 3, 4)

- **Read** ✓✓ (mature, polished).
- **Write**: ✗ — Superset is read-only on data. There is no native
  comment-write-back-to-DB workflow that fits dwhr's KPI-commentary loop.
- **PDF**: ✗ — basic screenshot/PNG export only. No board-grade
  programmatic PDF.
- **Custom logic**: ✗ — SQL semantic layer + visual config. No
  programmable measures, no hooks, no per-user R/Python functions.
- **Different audience**: SQL analysts in a UI vs developers shipping
  bespoke dashboards from a library. Different product category.

Superset is a multi-tenant BI server, not an embeddable framework.
**Different, not better.** It would replace dwhr's *use case* with a
different one, not implement dwhr's use case more cheaply. Skip.

### 4.7 Evidence / Observable Framework — out of scope (read-only)

- **Read** ✓ (very polished; DuckDB-WASM in browser, fast).
- **Write**: ✗ — both are static-site / read-only by design.
- **PDF**: △ (HTML → PDF via headless browser; no programmatic
  template).
- **Custom logic**: △ — JS in cells, SQL in cells; less ergonomic for
  dwhr-style measure abstractions.

Excellent for *publishing* dashboards (analyst → stakeholder); useless
for *governance loops* (manager comments → DB → board PDF).

### 4.8 Metabase — out of scope (same as Superset)

Same disqualifiers as Superset on dimensions 2, 3, 4. Different
audience, different product category.

## 5. Comparison

| Stack | Read | Write | PDF | Custom logic | Scale | Visual | AI-assist | Hiring | Migration |
|---|---|---|---|---|---|---|---|---|---|
| dwhr today | ✓ | ✓ | ✓✓ | ✓✓ | 10M | medium | medium | narrow | baseline |
| dwhr + DuckDB | ✓ | ✓ | ✓✓ | ✓✓ | 100M+ | medium | medium | narrow | 2-4 wk |
| Streamlit + Polars | ✓ | ✓ | △ | ✓ | 100M+ | low | high | easy | 4-6 mo |
| Dash + DuckDB | ✓ | ✓ | △ | ✓ | 100M+ | medium | high | easy | 4-6 mo |
| FastAPI + React/TS + DuckDB | ✓ | ✓ | ✓ | ✓ | 100M+ | **✓✓✓** | **✓✓✓** | easy | 6-12 mo |
| Apache Superset | ✓✓ | ✗ | ✗ | ✗ | warehouse | high | low | easy | replace |
| Evidence / Observable | ✓ | ✗ | △ | △ | DuckDB-WASM | high | medium | medium | replace |
| Metabase | ✓ | ✗ | ✗ | ✗ | warehouse | high | low | easy | replace |

## 6. The visual ceiling deep dive

The single biggest gap between dwhr-today and a modern React+TS rebuild
is **how the dashboard *looks and feels***. This matters more than most
performance arguments and is worth treating as a first-class dimension.

What the modern ceiling looks like (concrete examples for reference):
- Linear (linear.app) — keyboard-first, dense, fast, beautiful tables.
- Vercel dashboards — typography-led, dark mode native, immediate
  responsiveness.
- Posit Connect → Quarto dashboards — a credible R-side counter-example
  that shows R *can* approach this ceiling, but not via classic Shiny.
- shadcn/ui + Radix component vocabulary — the modern OSS baseline for
  React component libraries.

What classic Shiny + DT + Highcharts looks like by comparison:
- Bootstrap 4-era components, narrow design vocabulary.
- DataTable visual style is unmistakable and dated.
- Highcharts looks like 2015 (and has the license issue documented in
  [`docs/CHARTING-ALTERNATIVES.md`](CHARTING-ALTERNATIVES.md)).
- CSS overrides are possible but you fight the framework.

**Mitigation paths within R**:
1. **`bslib`** — modernizes Shiny's Bootstrap layer. Real upgrade.
2. **Quarto dashboards** — a meaningfully more modern R-side option.
   Different model than dwhr (Markdown-first), so not a drop-in.
3. **Custom CSS + `htmlwidgets`** — costly, brittle, not what most
   teams sign up for.

If "the finest dashboards you can build today" is a hard requirement,
**no R/Shiny stack reaches the React+TS ceiling.** That's the honest
answer. R can get to "good enough" and "professional" — but not to
"this looks like a 2026 Vercel app."

## 7. The AI-assistability deep dive

Stack choice in 2026 has a new dimension that didn't exist in 2018:
**how well do LLMs help you write and maintain it?**

- **React + TypeScript** is the strongest in the field. The combination
  of: huge training corpus, type system that gives the model concrete
  feedback, well-known component libraries (shadcn/ui, Radix, MUI,
  Chakra), strong build tooling (Vite, Next.js), and modern testing
  patterns means Claude/Copilot can scaffold non-trivial features
  end-to-end with minimal handholding. Refactors are safer because TS
  catches breakage.
- **Python (Streamlit / Dash)** is second. Well-trained. Python's lack
  of types is a friction point — the model can scaffold Python fluently
  but is more error-prone on larger refactors.
- **R / Shiny** is meaningfully behind. Smaller training corpus,
  reactive idioms are bespoke, the long tail (htmlwidgets bindings,
  ODBC patterns, RODBC vs DBI, Sweave) is uneven. Models hallucinate
  more on R Shiny than on React/TS.

What this means in practice:
- A maintainer + Claude can sustain a React/TS dashboard codebase that
  would historically need 2-3 frontend devs.
- The same maintainer + Claude on a Shiny codebase will spend more time
  fighting hallucinations and looking up idioms.
- The gap will widen, not close. Training corpora keep growing on
  React/TS; Shiny's share is fixed.

This is **not** a reason to abandon dwhr — it's a reason to weight the
"if we're rebuilding anyway, what's the destination" question more
heavily toward the AI-strongest stack. If the day comes that
Radboud-style hospitals decide to build "the finest internal BI
platform we've ever had," React+TS is the answer that compounds best
with AI-assisted development over the next 5 years.

## 8. Recommendation framework

**There is no single best stack — there are good answers per priority.**

| Priority | Pick |
|---|---|
| Ship CRAN; keep what works; minimal rewrite | dwhr as-is (current modernization plan) |
| Same as above + remove the scale ceiling | dwhr + DuckDB backend (v3.0 workstream) |
| Modernize the look without leaving R | dwhr + DuckDB + `bslib` UI refresh, or migrate to Quarto dashboards |
| Get out of R but keep the BI workflow shape | Dash + DuckDB (closest Python equivalent) |
| Build the finest dashboard our org has ever had | FastAPI + React/TS + DuckDB (+ Puppeteer PDF) |
| Switch to a turnkey BI tool (no code) | Apache Superset / Metabase — but accept losing the write-back + PDF + custom-logic loop |

**The deciding question** is the same one from the charting analysis,
just at a larger scale:

> **Is `dwhr` fundamentally an R-side BI primitive that consumers
> program around, or is it a deployment of bespoke dashboards that
> happens to be built in R?**

If it's the first (which is what the codebase, the `addMeasureDerrived`
DSL, the `latexEscape` helper, and the explicitly-programmable hooks
all suggest) — then the answer is *dwhr stays, evolve toward
dwhr+DuckDB, modernize the UI in-place, ship CRAN as the OSS tribute
to Pieter that you already started*.

If it's the second (which would be a strategic shift, not a fact about
the current code) — then the rebuild conversation is real, and
React+TS is the destination if the rebuild happens. But that is a
different product, costing 12+ months and an organizational
commitment, not a tech-stack swap.

## 9. What to do *now*

This phase (CRAN modernization):
- Finish W1-W8 as planned. Keep dwhr.
- Ship the Highcharts commercial-license notice (already in the
  separate PR `docs/highcharts-license-notice`).

This phase + 1 (post-CRAN, v3.0):
- Add `bslib` UI refresh as the lightest visible win.
- Sketch and prototype DuckDB-backend integration as W10. The
  dwhr DSL doesn't have to change; the in-memory `env$facts` becomes a
  DuckDB connection behind a small adapter. This gives you the 100M+
  row ceiling without leaving R.

A real rebuild conversation:
- Should be triggered by **product strategy**, not by perf or aesthetics.
- If triggered, the destination is React + TypeScript + DuckDB +
  FastAPI/equivalent + Puppeteer-PDF. Not a port — a new product.
- This document should be the input to that conversation, not the
  output.

---

## Appendix — sources

- Performance numbers: [`docs/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md)
  (run `Rscript scripts/perf-baseline.R` to reproduce).
- Charting library analysis: [`docs/CHARTING-ALTERNATIVES.md`](CHARTING-ALTERNATIVES.md).
- Modernization spec: [`docs/MODERNIZATION.md`](MODERNIZATION.md).
- DuckDB R bindings: <https://duckdb.org/docs/api/r>.
- bslib (Bootstrap for Shiny): <https://rstudio.github.io/bslib/>.
- Quarto dashboards: <https://quarto.org/docs/dashboards/>.
- shadcn/ui (React component vocabulary referenced as "modern ceiling"):
  <https://ui.shadcn.com/>.
- Posit Connect (modern R-side dashboard hosting): <https://posit.co/products/enterprise/connect/>.
