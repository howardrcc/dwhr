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

### 3.4 Raw D3 via `r2d3` (MIT)

- **License**: `r2d3` MIT (RStudio, <https://rstudio.github.io/r2d3/>).
- **What it is**: an `htmlwidgets` shim that lets you embed an arbitrary D3
  script with Shiny binding plumbing handled for you.
- **Trade-off**: makes D3 *integration* easy. Does not give you a chart
  library. Replacing `R/highCharts.R` (~1100 lines of high-level chart
  spec → Highcharts options) with raw D3 means writing all the chart
  rendering from scratch — bar/line/bubble/multi-axis/range-selector
  *each* as bespoke D3 code. Estimated cost: an order of magnitude more
  than a wrapper-to-wrapper port.
- **When it fits**: if you want a small number of *bespoke* visualizations
  with full design control. Not a fit for replacing a general-purpose
  charting library backing a framework.

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

| Feature | Highcharts | Plotly | ECharts | billboard.js | r2d3 (raw D3) |
|---|---|---|---|---|---|
| License | CC BY-NC + commercial | MIT | Apache-2.0 | MIT | MIT |
| Line / column / bar | ✓ | ✓ | ✓ | ✓ | rebuild |
| Packed bubble | ✓ | custom | partial (graph/force) | ✗ | rebuild |
| Multi y-axis | ✓ | ✓ | ✓ | ✓ | rebuild |
| Range selector + navigator | ✓ Highstock | partial (rangeslider) | ✓ dataZoom | partial (subchart zoom) | rebuild |
| Click → Shiny | ✓ | `event_data()` | `e_on()` | `bb_data(onclick)` | manual |
| Legend toggle → Shiny | ✓ | restyle event (clunky) | `legendselectchanged` | limited | manual |
| Custom JS callbacks | ✓ | `JS()` / `onRender()` | `JS()` via `e_on()` | `JS()` | native |
| Pattern fills | ✓ | ✓ (2.x) | ✓ (decal) | limited (SVG patterns) | rebuild |
| Themes | `hc_theme_*` | layout templates | `e_theme()` | CSS only | rebuild |
| Proxy / in-place update | needs `starExtend.js` bridge | `plotlyProxy` | `echarts4rProxy` | `billboarderProxy` | manual |
| Last meaningful release | active | active | active (2025) | active | active |
| Cost vs Highcharts | baseline | medium | medium | medium-low | very high |

---

## 5. If a migration is scoped

**Recommended candidate**: `echarts4r`. Apache-2.0 license is the cleanest
fit; feature surface maps almost 1:1 (`dataZoom` ≈ navigator, `decal` ≈
pattern fill, multi-axis is native). Packed bubble is the only weak spot,
and `dwhr`'s usage of it is cosmetic (one example app uses `packedbubble`).

**`billboarder`** is a viable lighter alternative if a future audit shows
that `dwhr`'s actual deployments don't use Highstock features (`navigator`,
`rangeSelector`) — billboard.js doesn't have a direct equivalent and the
gap would be visible to users.

**Realistic budget for ECharts migration**:

| Work item | Estimate |
|---|---|
| Rewrite `R/highCharts.R` (~1100 lines, mechanical port `hc_*` → `e_*`) | 2-3 weeks |
| Rewrite `inst/www/starExtend.js` chart-update bridge | 1 week |
| Re-test 15 example apps + add `presType = 'highCharts'` smoke test variant | 1 week |
| **Total** | **4-6 weeks** of one engineer |

**Feature loss to accept**:
- Packed bubble fidelity (degrade to ECharts' `graph` force layout).
- Highstock's range-selector button row (rebuild as custom toolbar — 1-2 days).

**Out of scope for the migration but worth noting**: `dwhr` has no OHLC /
candlestick usage in any example app. The polish gap on financial charts
is real but does not affect this codebase.

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
