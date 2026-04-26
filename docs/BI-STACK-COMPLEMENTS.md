# Complementary BI tools — Apache Superset, Power BI, self-service options

Reference doc for thinking about how `dwhr` fits inside a larger BI
ecosystem, including tools that **complement** dwhr (different jobs,
same warehouse) and tools that overlap with self-service analytics.
Companion to [`docs/ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md);
this one focuses on tools that sit *alongside* dwhr rather than
replace it.

This is analysis, not a recommendation to adopt any specific tool.

---

## 1. The three-tier BI framing

Most real BI orgs run multiple tools side-by-side because no single
tool wins all three of these jobs:

| Tier | Job | Audience | Interaction depth | Tool shape |
|---|---|---|---|---|
| **1 — Broad consumption** | Exec scorecards, monthly KPIs, "the company's metrics" | Many viewers (10s–1000s) | Read mostly; light filtering | Polished dashboard tool |
| **2 — Bespoke governance** | KPI commentary, write-back to warehouse, board-grade PDF reports, custom business logic | Few important users (managers, controllers) | Heavy interaction + write-back + audit trail | Programmable framework — **dwhr's lane** |
| **3 — Self-service exploration** | Ad-hoc analysis by power users; "I have a hypothesis" | Power users (analysts, finance, ops) | High interaction; throw-away analyses | Notebook / spreadsheet / SQL workbench |

The same warehouse underneath all three. Comments and PDF artifacts
written by Tier 2 (dwhr) become first-class data the warehouse exposes
back to Tier 1 dashboards and Tier 3 self-service queries — the loop
closes through the data layer.

This document is about **Tier 1 and Tier 3 candidates**, since dwhr
already fills Tier 2.

---

## 2. Apache Superset vs Microsoft Power BI

Both target Tier 1. The deciding lens is *where data and compute
live*, not feature checklists.

### Where data and compute live

| | Apache Superset | Microsoft Power BI |
|---|---|---|
| **Data storage** | None — connects to your warehouse | VertiPaq engine (Microsoft cloud) by default; or your warehouse via DirectQuery |
| **Where compute happens** | In the source database | In VertiPaq (Import mode); or DB (DirectQuery); or both (Composite) |
| **Metadata DB** | Postgres/MySQL (Superset's own) | Microsoft cloud |
| **Caching** | Redis/Memcached (optional) | VertiPaq is inherently cached |
| **Implication** | Performance = warehouse performance | Performance = VertiPaq performance, mostly independent of source DB |

**Superset** is a *thin BI layer* — it stores no data of its own. Click
a chart → Superset translates to SQL → DB executes → Superset receives
result rows → renders. Performance is whatever your warehouse delivers.
For teams already on Snowflake / BigQuery / ClickHouse / DuckDB, this
is the natural fit.

**Power BI** is an *opinionated platform* — VertiPaq is a column-store,
in-memory, compressed engine that often makes "small to medium data
feel fast" without involving the source DB at all. The trade is
dataset-size limits (~1 GB on Pro, ~400 GB on Premium capacity) and
deeper Microsoft lock-in.

### Side-by-side comparison

| | Apache Superset | Power BI |
|---|---|---|
| License / pricing | Apache-2.0 OSS; pay for hosting + ops; Preset.io for managed | Pro $14/user/mo; Premium Per User $24/user/mo; Premium capacity ~$5K/mo+ |
| Authoring | Web UI + SQL Lab | Power BI Desktop (Windows-only) → publish to Service |
| Semantic layer | Light — virtual datasets, Jinja in SQL | Rich — relationships, DAX measures, hierarchies, time intelligence |
| Calculation language | SQL (+ Jinja) | DAX (functional, columnar-aware), M (Power Query) |
| PDF / paginated reports | Scheduled email with screenshot/PDF — basic | Paginated Reports (SSRS-derived) — board-grade |
| Native write-back | ✗ | ✗ (possible via Power Apps) |
| Comments / annotations | Limited (chart-level, in metadata DB) | Dashboard-level comments in PBI Service |
| Embedding in your own app | iframe or SDK (free) | Power BI Embedded (Azure SKU, $$$) |
| Auth / governance | Roles, RLS, basic audit | AD/Entra, RLS, sensitivity labels, M365 compliance, lineage |
| Ecosystem fit | Modern data stack (Snowflake, dbt, Airflow) | Microsoft (Excel, Office 365, Azure, Teams, Fabric) |

### Use cases — where each one wins

**Superset wins when:**
- You already have a modern warehouse (Snowflake, BigQuery, ClickHouse, DuckDB).
- SQL-first analyst culture — your team writes SQL anyway.
- OSS / no vendor lock-in is a hard requirement.
- Embedded analytics in your own product (the SDK is decent; per-user pricing doesn't scale to embedded).
- Multi-cloud or "definitely not Microsoft" posture.
- You have data engineering capacity to operate Postgres + Redis + Celery + the Superset server.

**Power BI wins when:**
- Microsoft-shop org (already on M365 / Azure / Entra). Integration cost near zero; governance story best-in-class for compliance-heavy industries.
- Excel-first analyst culture — Power Query is Excel's import wizard evolved; DAX builds on Excel formulas.
- Rich semantic modeling needed (complex measures, time intelligence, role-playing dimensions).
- Drag-drop authoring for non-technical users required.
- Paginated reports (real PDF) are a hard requirement.
- Turnkey deployment with no ops overhead.

### Honest framing

These aren't really competing for the same job:

- **Superset is infrastructure** — how a data team exposes warehouse data to consumers. Tightly coupled to "your warehouse is the source of truth."
- **Power BI is a product** — self-contained dashboarding+modeling+sharing platform that happens to also connect to your warehouse but doesn't depend on one.

Org-shape predicts the choice better than feature comparisons:

- "We have a data engineering team and a warehouse" → Superset (or Looker, Mode, Hex)
- "We have analysts and Excel" → Power BI (or Tableau, Domo)
- "We have R/Python developers building bespoke analytical tools" → dwhr / Streamlit / Dash / custom-built

### Microsoft Fabric — the strategic direction

Microsoft Fabric (launched 2023) bundles Power BI with OneLake (Parquet
on object storage), Synapse, Data Factory, and Copilot. The new
**DirectLake mode** for Power BI queries Parquet in OneLake without
the VertiPaq import step — Microsoft's answer to the modern data
stack.

Fabric fixes some of Power BI's classic bugginess (the `.pbix`
corruption / refresh-failure / VertiPaq-quirk class) by changing the
architecture. But it deepens Microsoft lock-in dramatically and the
pricing (capacity-based, ~$5K/mo+ for meaningful capacity) is
aggressive. If your org commits to the Microsoft stack, Fabric is the
future-proof Power BI path. If not, it's irrelevant.

---

## 3. Self-service analytics — the Tier 3 question

"Self-service" is doing a lot of work in BI conversations. It splits
into three jobs that often need different tools:

1. **Ad-hoc exploration** — "I have a hypothesis, give me data to test
   it." SQL or spreadsheet. Output: usually thrown away after the
   answer.
2. **Recurring analysis** — "I do this every month, can I parameterize
   it?" Notebook or templated dashboard. Output: rerun with new data.
3. **Building a new dashboard for someone else** — semi-developer work.
   Output: a published artifact.

Power users want all three. Excel users mostly want #1, sometimes #2,
almost never #3. Tool choice changes per job.

### The Excel ceiling problem (and Excel's hidden answer)

Excel's hard limit is **1,048,576 rows × 16,384 columns** per sheet.
That's an artifact of the Excel 2007 file format and isn't going to
change.

But: **Power Pivot** (built into Excel since 2010, free) runs the same
VertiPaq columnar engine that Power BI uses. Data loaded into the
Power Pivot **data model doesn't live in a sheet** — it lives in the
model, where the limit is essentially RAM. 100M-row aggregations in
Power Pivot are routine; the sheet only sees the pivot table summary,
which fits within the 1M ceiling because it's already aggregated.

Most Excel-loving users have never been shown Power Pivot. It is the
**lowest-effort answer** to "Excel hits a wall, but I don't want to
learn Power BI" — same engine, same DAX, same M-language Power Query,
but never leave Excel. Worth a one-day workshop before any tool
migration conversation.

That said: even Power Pivot ages out around the time you need real
collaboration, lineage, or cross-team sharing. The `.xlsx` file is
still a single artifact passed by email or SharePoint. For governance,
versioning, or anything beyond a single power user's spreadsheet,
Power Pivot is the bridge, not the destination.

### Modern self-service tools

Six tools worth knowing, ranked roughly by future-proofness for a
non-Microsoft org:

| Tool | Shape | Strength | Pricing | Future-proof? |
|---|---|---|---|---|
| **Hex** | SQL + Python notebooks, reactive, AI-native | Best "analyst's notebook" of the modern era | $24/user/mo | ✓✓✓ — well-funded, AI-first, growing fast |
| **Sigma Computing** | Spreadsheet UX over warehouse | Excel users feel at home; no row limit | Enterprise (opaque) | ✓✓ — built for the "Excel exodus" use case |
| **Observable Framework** | JS notebooks, DuckDB-WASM in browser | Free, OSS, fully portable | Free / cloud paid | ✓✓✓ — open standards, lowest lock-in |
| **Rill Data** | Code-first dashboards on DuckDB | Sub-second aggregations on big data; OSS | Free / Rill Cloud | ✓✓ — newer, smaller community, DuckDB bet pays off |
| **Mode** | SQL + Python notebooks, dashboards | Mature; risk of stagnation post-ThoughtSpot acquisition | $- | ✓ — risk of being deprioritized |
| **Apache Superset SQL Lab** | SQL editor inside Superset | Free, fits the complementary-stack story | Free | ✓✓ — same future-proofness as Superset itself |

Three of these deserve more depth.

#### Hex — the best modern analyst notebook

[hex.tech](https://hex.tech) — notebook-style with reactive cells
(think Jupyter + Observable + Shiny). SQL and Python interleave
seamlessly. Built-in AI ("Hex Magic") writes SQL/Python from prompts.

Where analysts who outgrew Mode but don't want raw Jupyter end up.
Pricing is per-user and not cheap, but the productivity gain is real —
a single Hex user with the AI features replaces what was 2-3 manual
analysts in older tools.

Lock-in: medium. SQL and Python are portable; Hex's notebook format
and reactive runtime aren't.

#### Sigma Computing — Excel for cloud warehouses

[sigmacomputing.com](https://sigmacomputing.com) — spreadsheet
interface over Snowflake/BigQuery/Redshift. Formulas, cell references,
drag-to-fill — the same mental model as Excel. But it runs on the
warehouse, so there's **no row limit** and changes are versioned and
shareable.

Specifically built for the "Excel users won't learn anything else"
situation. Often the answer when an org tries Power BI, fails, and
needs an off-ramp that doesn't require retraining the finance team.

Pricing is enterprise (not transparent), which is the main barrier.

#### Observable Framework — the open-standards bet

[observablehq.com/framework](https://observablehq.com/framework) —
Markdown + JavaScript files, DuckDB-WASM running in the browser,
deploy as static HTML to anywhere.

No vendor, no server, no lock-in. The DuckDB-WASM trick means a
~100MB Parquet file in the browser can serve sub-second aggregations
on millions of rows without any backend.

Heavier learning curve (JavaScript) than Hex or Sigma. The most
portable artifact of any tool here.

### Pure-OSS self-service stack

If "no vendor at all" is the constraint:

- **Apache Superset SQL Lab** for power users who write SQL — query
  the warehouse, save snippets, share results.
- **JupyterHub** (or Posit Workbench for R folks) for notebook power
  users — self-hosted, freely scalable.
- **Quarto** for the "I want my analysis to be a publishable doc, not
  a one-off" pattern — Markdown + R/Python/SQL/Julia → HTML/PDF/Word/
  Reveal.js. Free, OSS, the closest thing to "rmarkdown for everyone."
- **DuckDB CLI** for warehouse-bypass — analysts query Parquet files
  directly from object storage; no server needed for many one-off
  questions.
- **Observable Framework** for shareable artifacts.

This stack costs nothing in licenses, requires real ops capacity,
and handles ~95% of self-service if you have the right culture
(SQL-comfortable analysts).

---

## 4. Updated three-tier framing

Putting it all together:

| Tier | Job | Tools that fit |
|---|---|---|
| **1 — Broad consumption** | Many viewers, simple interactions, exec scorecards | Apache Superset, Looker Studio, Power BI Service / Fabric |
| **2 — Bespoke governance** | KPI commentary write-back, board PDFs, custom logic | **dwhr** (current); eventually dwhr + DuckDB backend |
| **3 — Self-service exploration** | Ad-hoc analysis by power users | **Hex** or **Sigma** (commercial); **Observable + DuckDB** or **Quarto + JupyterHub** (OSS); **Power Pivot** as the Excel bridge |

The same warehouse underneath all three. Comments and PDF artifacts
written by dwhr (Tier 2) become first-class data — Tier 1 dashboards
can chart "comments per KPI per month", Tier 3 self-service queries
can correlate manager commentary with KPI movements.

---

## 5. Decision posture (current)

**The complementary-stack framing should not drive any change to the
current modernization plan.** The CRAN-prep workstreams (W1-W8) finish
dwhr's Tier 2 role; the post-CRAN sketches (W10 DuckDB backend, W11
bslib UI refresh) keep dwhr competitive in that role.

**If/when broader BI tooling questions arise** (typically driven by
"the analyst team needs ad-hoc tools" or "execs want a portfolio view
across all dwhr dashboards"), this document is the input — not a
prescription.

**Specific recommendations to flag for non-technical stakeholders:**

1. If finance/ops are hitting Excel's row ceiling, **try Power Pivot
   first** before any tool-change conversation. Same Excel, no row
   limit, free.
2. If Power BI keeps breaking and pushing people back to Excel, the
   off-ramp that loses the fewest users is **Sigma Computing**, not
   "everyone learn DAX harder."
3. If the data team wants a self-service surface that works alongside
   dwhr, the cheapest credible answer is **Apache Superset's SQL Lab
   plus Quarto + JupyterHub** — entirely OSS, fits the same warehouse
   dwhr already reads from, no per-user licensing.
4. If the org is committed to Microsoft, **Fabric DirectLake is the
   future-proof PBI path**, not classic Import-mode Desktop. Plan the
   capacity license.

---

## Appendix — sources

- Apache Superset: <https://superset.apache.org/>, <https://preset.io/>
- Power BI licensing: <https://www.microsoft.com/en-us/power-platform/products/power-bi/pricing>
- Microsoft Fabric: <https://www.microsoft.com/en-us/microsoft-fabric>
- Hex: <https://hex.tech>
- Sigma Computing: <https://sigmacomputing.com>
- Observable Framework: <https://observablehq.com/framework>
- Rill Data: <https://www.rilldata.com/>
- Power Pivot in Excel: <https://support.microsoft.com/en-us/office/power-pivot-overview-and-learning-f9001958-7901-4caa-ad80-028a6d2432ed>
- DuckDB: <https://duckdb.org>
- Quarto: <https://quarto.org>
- Companion docs: [`ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md), [`PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md), [`MODERNIZATION.md`](MODERNIZATION.md)
