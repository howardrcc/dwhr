# Charting alternatives — Highcharts → permissive-license replacements

Reference document for a future migration of `dwhr`'s `highCharts`
presentation type away from Highcharts. Captured as input for a
not-yet-scoped workstream (sketched in `MODERNIZATION.md` §9 as W9).

This is **analysis, not a decision**. The current modernization phase
keeps `highcharter` in place and ships a commercial-license notice
(README + DESCRIPTION); switching the charting library is deferred to
post-CRAN.

---

## 1. Why this question exists

Highcharts is dual-licensed: **CC BY-NC 3.0** for non-commercial use, **paid
commercial** otherwise. The "non-commercial" carve-out is narrower than most
readers assume:

| Use | Highsoft's stated position |
|---|---|
| Personal projects, school assignments, true non-profits | Free |
| Internal corporate dashboards (not customer-facing) | Commercial — needs paid license |
| Government / public sector | Commercial — separate "Government" SKU |
| Hospitals / academic medical centers | Treated as commercial |
| Grant-funded academic research | Ambiguous — Highsoft asks you to email sales |
| OSS that bundles Highcharts JS | Allowed under CC BY-NC; commercial users of the OSS still owe a license — the OSS license does not launder it |

Sources: <https://www.highcharts.com/license>,
<https://www.highcharts.com/blog/faqs/non-commercial-redistribution/>.

The R wrapper `highcharter` (Joshua Kunst, MIT-licensed code) ships the
Highcharts JS bundle in `inst/htmlwidgets/lib/highcharts/`. CRAN accepts this
because the bundled JS carries its own license file; CRAN policy permits
bundled third-party assets with their own declared licenses. `highcharter`'s
README carries the disclaimer:

> Highcharts offers both a commercial license as well as a free non-commercial
> license. Please review the licensing options and terms before using this
> software, as the `highcharter` license neither provides nor implies a
> license for Highcharts.

`dwhr` itself does not redistribute Highcharts JS — `highcharter` does. CRAN
will not push back on `dwhr` for the dependency. **But anyone who deploys a
`dwhr`-based Shiny dashboard commercially inherits the Highcharts commercial
licence obligation.** That obligation is what motivates this analysis.

---

## 2. The feature surface that has to be replaced

Audit of `R/highCharts.R` (~1100 lines) and the example apps under
`inst/examples/`:

- **Chart types**: line, column/bar, packed bubble, Highstock (`type = 'stock'`).
- **Multi-axis**: `hc_yAxis_multiples()` — multiple y-axes on one chart.
- **Stock-chart features**: `hc_rangeSelector()` (button row + input fields),
  `hc_navigator()` (mini-map at the bottom).
- **Interactivity → Shiny**:
  - Click events on data points that fire back into Shiny and drive
    drill-down (changing the dimension level on click).
  - Series show/hide events (legend toggle, programmatic) that fire back
    into Shiny so the package can update its internal series-visibility state.
- **Custom JS callbacks**: `highcharter::JS()` for tooltip `pointFormatter`,
  click handlers, and series `hide`/`show` events. The package's
  `inst/www/starExtend.js` is the bridge that translates the Highcharts JS
  events into Shiny `input$<id>` updates.
- **Pattern fills**: stripe patterns on bars (one example app uses these via
  `hc_add_dependency(name = "modules/pattern-fill.js")`).
- **Themes**: `hc_add_theme(hc_theme_smpl())`.
- **Programmatic mutation**: the JS bridge in `starExtend.js` updates the
  chart in place (series data, axis bounds, plot bands) — this is
  `highcharts.update()` calls wrapped behind `shinyjs::extendShinyjs`
  function names like `updateSeriesData`, `updateXPlotBands`, `redraw`.
- **Shiny binding**: `highchartOutput()` / `renderHighchart()` from
  `highcharter`.

Any candidate has to provide each of these or have a documented workaround.

---

## 3. Candidates

### 3.1 Plotly (`plotly` R package, plotly.js MIT)

- **License**: plotly.js MIT (<https://github.com/plotly/plotly.js/blob/master/LICENSE>).
- **Shiny binding**: first-class — `plotlyOutput()`/`renderPlotly()`,
  `event_data()` for click/hover/legend events
  (<https://plotly-r.com/linking-views-with-shiny.html>).
- **Multi-axis**: yes (`yaxis2`, `overlaying = "y"`).
- **Click drill-down**: yes via `event_data("plotly_click")`.
- **Legend toggle event**: yes via `event_data("plotly_restyle")` —
  functional but more awkward than Highcharts' `legendItemClick`.
- **Range selector + navigator**: partial. `rangeslider()` exists; date-axis
  `rangeselector` buttons exist. Not as polished as Highstock but usable.
- **Packed bubble**: not native — would need `circlepack` from D3 layout
  or a custom trace.
- **Pattern fills**: yes since plotly.js 2.x (`marker.pattern.shape`).
- **Custom JS**: `htmlwidgets::JS()` works; `onRender()` for arbitrary
  post-render JS hooks.
- **Programmatic update**: `plotlyProxy()` API supports in-place mutation —
  direct analogue of `dwhr`'s current `starExtend.js` bridge.

### 3.2 Apache ECharts (`echarts4r`, Apache-2.0)

- **License**: Apache-2.0 (<https://github.com/apache/echarts>).
- **Shiny binding**: `echarts4r` (John Coene, <https://echarts4r.john-coene.com/>)
  with `e_on()` / `e_capture()` for click + legend events.
- **Multi-axis**: native (`yAxis: [{}, {}]`).
- **Click drill-down**: yes via `e_on("click", ...)`.
- **Legend toggle event**: yes via `legendselectchanged`.
- **Range selector / navigator**: ECharts has `dataZoom` (slider + inside)
  which is the closest equivalent to Highstock's navigator and is arguably
  better designed. Range-selector buttons would need to be built as a
  custom toolbar (~1-2 days).
- **Packed bubble**: not built-in. The `graph` series with force layout
  approximates it; cleaner via the ECharts-GL extension.
- **Pattern fills**: supported via `itemStyle.decal` (ECharts 5+).
- **Custom JS**: `htmlwidgets::JS()` + `e_on(..., JS(...))`.
- **Programmatic update**: `echarts4rProxy()` exists.
- **Activity**: Apache project; releases through 2025.

ECharts is the most feature-complete OSS competitor to Highcharts and the
license fit is the cleanest of the candidates.

### 3.3 billboard.js via `billboarder` (MIT both)

- **License**: billboard.js MIT (<https://github.com/naver/billboard.js/blob/master/LICENSE>);
  `billboarder` MIT (<https://cran.r-project.org/package=billboarder>;
  source <https://github.com/dreamRs/billboarder>).
- **Heritage**: billboard.js is a D3 v4+ based fork of C3 by Naver. Higher-
  level chart API on top of D3 — you write chart specs, not D3 selections.
  This is what "D3 with easy integration" usually points to.
- **Shiny binding**: `billboarderOutput()` / `renderBillboarder()` plus a
  proxy update API (`billboarderProxy()`).
- **Multi-axis**: yes (`bb_y_axis(... position = "right")` with `axes`
  option per series).
- **Chart types**: line, scatter, bar/lollipop, histogram, density, pie,
  donut, gauge — **no packed bubble, no Highstock-style stock chart**.
- **Click event → Shiny**: yes via `bb_data(... onclick = JS(...))`.
- **Legend toggle**: limited — billboard.js has `legend.item.onclick` but
  the round-trip to Shiny is more work than ECharts.
- **Range selector / navigator**: no direct equivalent to Highstock's
  navigator. Has a basic zoom/subchart feature that approximates the
  navigator but lacks the date-button row.
- **Pattern fills**: limited — has SVG patterns but the API is rougher
  than ECharts' `decal`.
- **Themes**: very limited; mostly CSS overrides.
- **Practical position**: lighter and more permissive than ECharts; weaker
  on stock-chart polish, weaker on packed bubble. Best fit if `dwhr`'s
  Highstock usage turns out to be cosmetic rather than load-bearing.

### 3.4 D3 via `r2d3` (MIT) — co-equal recommended candidate

- **License**: D3 BSD-3-Clause (<https://github.com/d3/d3/blob/main/LICENSE>);
  `r2d3` MIT (RStudio, <https://rstudio.github.io/r2d3/>).
- **What it is**: an `htmlwidgets` shim that lets you embed a D3 script
  with Shiny binding plumbing handled for you (`r2d3()`, `dr2d3Output()`,
  `renderD3()`, plus `r2d3::JS()` for callback escape hatches).
- **Architectural fit with `dwhr`**: the package already has an opinionated
  R-side spec layer (`addPresentation(type=..., highChartsOpts=...)`) and a
  bespoke imperative JS bridge (`inst/www/starExtend.js` with
  `updateSeriesData`, `updateXPlotBands`, `updateSeriesOpts`, `redraw`).
  That is already 80% of an r2d3-style architecture. The chart library in
  the middle is doing less heavy lifting than the line count of
  `R/highCharts.R` suggests — a lot of that file is option translation
  from dwhr's spec to Highcharts' spec, which goes away if you target D3
  directly.
- **No abstraction lock-in**: every weird feature dwhr exercises today
  (multi-axis, packed bubble, click → drill, show/hide bridge,
  Highstock-style range selector) maps to "write the D3 code that does
  this". With a chart wrapper, you'll hit features the wrapper doesn't
  expose and end up writing `JS()` workarounds anyway — those workarounds
  *are* D3, but with a chart library's lifecycle in the way.
- **Bus factor / longevity**: D3 (Mike Bostock + huge community) is the
  most durable thing in JS dataviz. A chart-library upstream can be
  abandoned, change ownership, or fork (C3 → billboard.js was already a
  "library died, community forked" event); D3 itself does not have that
  failure mode.
- **Bundle transparency**: D3 modular imports let you ship only what you
  use. No fixed wrapper payload.
- **Cost shape inverts**: with `echarts4r` or `billboarder`,
  `R/highCharts.R` (~1100 lines) becomes ~800 lines of ported R + a small
  JS bridge. With r2d3, R-side might shrink to ~400 lines (dwhr-spec →
  data + config), but JS-side grows to ~1500-2500 lines (one D3 module
  per chart type, plus shared axis/legend/tooltip/responsive primitives).
  **Total LOC goes up**; total *abstraction layers* go down.
- **What you give up**: every chart primitive you currently get for free
  from a chart library — axes, gridlines, legend rendering, tooltip
  positioning, responsive resize, color palettes, accessibility — has to
  be written. Each is 50-200 lines of D3, none individually hard, but
  they add up. Range selector + navigator (Highstock equivalent) is the
  hardest single piece (~1 week of focused D3 brush + linked-axis work).
- **Documentation shape**: `bb_barchart()` has docs. "How do I do a
  multi-axis stacked bar in D3 v7" is a lookup-and-cobble exercise per
  chart type. The community is huge but the answers tend to be 2017 blog
  posts you have to translate forward.
- **Maintainer profile**: a future maintainer needs to know D3 to extend
  dwhr. With a chart wrapper they need to know `bb_*` / `e_*` calls,
  which is shallower. This is a real tradeoff to weigh against the
  flexibility upside.

**When r2d3 wins over a chart-library wrapper**: when `dwhr` is
fundamentally a *star-schema reactive framework* and charts are an
interchangeable rendering target — the dwhr abstraction is the value, the
chart library is just a renderer, and r2d3 is the most flexible, most
permissive, lowest-lock-in renderer. The fact that `starExtend.js`
already exists is strong signal that this framing matches the codebase.

**When a chart-library wrapper wins**: when the value is in *pre-built
dashboards with chart bells and whistles* — you want the chart library
doing the polish work for you, and the dwhr layer is a convenience over
a renderer that already knows how to render.

### 3.5 Out of scope

- **dygraphs** (MIT): time-series only, no bar/bubble. Last meaningful
  release 2018. Would only cover Highstock charts, not Highcharts.
- **Chart.js** wrappers: no mature CRAN-quality R wrapper.
- **`ggiraph`** (interactive ggplot2): wrong abstraction — designed for
  static-spec interactivity, not the imperative-update model `dwhr` uses.
- **Apache Superset** and similar BI tools: standalone servers, not
  embeddable JS libraries. Migrating to one would mean replacing Shiny,
  not Highcharts.

---

## 4. Comparison

| Feature | Highcharts | Plotly | ECharts | billboard.js | D3 via r2d3 |
|---|---|---|---|---|---|
| License | CC BY-NC + commercial | MIT | Apache-2.0 | MIT | BSD-3 (D3) + MIT (r2d3) |
| Line / column / bar | ✓ | ✓ | ✓ | ✓ | hand-built |
| Packed bubble | ✓ | custom | partial (graph/force) | ✗ | hand-built |
| Multi y-axis | ✓ | ✓ | ✓ | ✓ | hand-built |
| Range selector + navigator | ✓ Highstock | partial (rangeslider) | ✓ dataZoom | partial (subchart zoom) | hand-built (~1 week) |
| Click → Shiny | ✓ | `event_data()` | `e_on()` | `bb_data(onclick)` | direct (`Shiny.setInputValue`) |
| Legend toggle → Shiny | ✓ | restyle event (clunky) | `legendselectchanged` | limited | direct |
| Custom JS callbacks | ✓ | `JS()` / `onRender()` | `JS()` via `e_on()` | `JS()` | native (it's all JS) |
| Pattern fills | ✓ | ✓ (2.x) | ✓ (decal) | limited (SVG patterns) | hand-built |
| Themes | `hc_theme_*` | layout templates | `e_theme()` | CSS only | hand-built |
| Proxy / in-place update | needs `starExtend.js` bridge | `plotlyProxy` | `echarts4rProxy` | `billboarderProxy` | direct (own the JS) |
| Last meaningful release | active | active | active (2025) | active | active (D3 v7) |
| Lock-in / abstraction layers | wrapper + JS lib | wrapper + JS lib | wrapper + JS lib | wrapper + JS lib | r2d3 shim only |
| Total R LOC vs today | baseline | similar | similar | similar | smaller |
| Total JS LOC vs today | baseline | similar | similar | similar | larger |
| Cost vs Highcharts | baseline | medium | medium | medium-low | medium-high upfront, low ceiling |

---

## 5. If a migration is scoped

There is no single "best" candidate — the right pick depends on which
priority dominates:

| Priority | Pick |
|---|---|
| Smallest user-visible change vs Highcharts; fastest port | `echarts4r` |
| Lowest long-term lock-in; willing to invest in JS code; want to keep maximum control over the chart layer | **`r2d3`** |
| Want a chart wrapper but lighter than ECharts, and don't need Highstock features | `billboarder` |

### Option A — `echarts4r` (fastest correct port)

Apache-2.0 license; feature surface maps almost 1:1 (`dataZoom` ≈
navigator, `decal` ≈ pattern fill, multi-axis is native). Packed bubble
is the only weak spot, and `dwhr`'s usage of it is cosmetic (one example
app). Best fit if the goal is "remove the commercial license without
restructuring how dwhr renders charts."

| Work item | Estimate |
|---|---|
| Rewrite `R/highCharts.R` (~1100 lines, mechanical `hc_*` → `e_*`) | 2-3 weeks |
| Rewrite `inst/www/starExtend.js` chart-update bridge | 1 week |
| Re-test 15 example apps + add `presType = 'highCharts'` smoke test variant | 1 week |
| **Total** | **4-6 weeks** |

Feature loss: packed bubble fidelity (degrade to `graph` force layout);
Highstock range-selector button row (rebuild as custom toolbar, 1-2 days).

### Option B — `r2d3` + D3 (lowest-lock-in, highest ceiling)

D3 BSD-3 + r2d3 MIT. Eliminates the chart-library middle layer entirely.
Best fit if `dwhr` is fundamentally a star-schema reactive framework with
charts as an interchangeable rendering target — which the existence of
`starExtend.js` strongly suggests.

| Work item | Estimate |
|---|---|
| Shrink `R/highCharts.R` to a dwhr-spec → JSON adapter (~400 lines) | 1-2 weeks |
| Build per-chart-type D3 modules (line, column, bar, packed bubble) | 2-3 weeks |
| Build shared D3 primitives (axes, legend, tooltip, theme, responsive) | 1-2 weeks |
| Build Highstock-equivalent (D3 brush + linked-axis range selector) | 1 week |
| Replace `starExtend.js` with chart-instance update API (own the lifecycle) | 1 week |
| Re-test 15 example apps + add `presType = 'highCharts'` smoke test variant | 1-2 weeks |
| **Total** | **7-11 weeks** |

Feature loss: none in principle (you write whatever you need). What you
trade is upfront engineering time for permanent freedom from any
chart-library upstream.

Maintenance shift: future contributors need to know D3, not `bb_*` /
`e_*` / `hc_*`. Deeper learning curve, but a more transferable skill.

### Option C — `billboarder` (lighter wrapper, simpler port)

billboard.js + `billboarder` both MIT. Smaller surface than ECharts;
easier port from `hc_*` → `bb_*`; weaker on Highstock features (no
direct navigator equivalent).

| Work item | Estimate |
|---|---|
| Rewrite `R/highCharts.R` (~1100 lines, `hc_*` → `bb_*`) | 2 weeks |
| Hand-build navigator + range-selector buttons on top of billboard.js | 1 week |
| Rewrite `inst/www/starExtend.js` chart-update bridge | 1 week |
| Re-test 15 example apps + smoke test variant | 1 week |
| **Total** | **5 weeks** |

Feature loss: packed bubble (degrade or drop); subtle navigator polish vs
Highstock. Cost rises if downstream deployments rely on Highstock features.

### Out of scope for any migration

`dwhr` has no OHLC / candlestick usage in any example app. The polish
gap on financial charts is real for all OSS candidates but does not
affect this codebase.

---

## 6. Decision posture (current)

**Do not migrate as part of the CRAN-prep modernization.** The cheapest
correct action for the CRAN debut is to ship the commercial-license notice
in `README.md` and `DESCRIPTION` (mirroring `highcharter`'s own pattern).
The migration is a v3.0-class change — it competes for engineering budget
with W3 (`RODBC` → `DBI`+`odbc`) and W6 (CI), both of which are also
modernization-critical.

If/when this is scoped as workstream W9, this document is the input. The
spec sketch and acceptance criteria belong in `MODERNIZATION.md` §5;
this file is the analysis backing them.
