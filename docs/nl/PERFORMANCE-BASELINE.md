# dwhr performance-baseline

Empirische performance-metingen van de server-side R-code van dwhr bij
1M en 10M feiten-rijen. Begeleidend document bij
[`docs/ARCHITECTURE-FUTURES.md`](../ARCHITECTURE-FUTURES.md) (Engelstalig)
of [`docs/nl/ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md) (Nederlandstalig),
dat deze cijfers gebruikt als input voor de stack-vergelijkingsanalyse.

> **Engelstalig origineel:** [`docs/PERFORMANCE-BASELINE.md`](../PERFORMANCE-BASELINE.md)

**Reproduceren:** `Rscript scripts/perf-baseline.R` vanuit de repo-root.
Output gaat naar `docs/perf/baseline-summary.txt` plus een profvis
flame graph in `docs/perf/baseline-1M.html`.

## Methodologie

Synthetische feiten-tabel (willekeurige `maandId` foreign keys plus
een `runif`-kolom `num1`) gekoppeld aan de echte
`inst/examples/01SimpleTable/data/ds_d_periode.txt`-periodendimensie
(3 niveaus: totaal / jaar / maand). Eén dimensie, één met `sum`
geaggregeerde measure, één `dataTable`-presentatie — het minimum dat
de volledige reactive pipeline raakt.

Verpakt in `shiny::testServer()` (dezelfde harness als de
W5-smoketests), zodat we dezelfde codepaden meten als een live
Shiny-sessie zou aanroepen, **minus** de browser-side rendering.

Wat dit meet: server-side R-kosten van de constructie, eerste
`renderDims()` en reactive `factsFiltered()`-herfiltering.

Wat dit **niet** meet: clientside DOM-opbouw door DT,
Highcharts-redraw, WebSocket-transport, of de JS-bridge in
`inst/www/starExtend.js`. Die lagen tellen in een live app nog eens
50%+ aan latency op.

## Omgeving

- R 4.5.3 op macOS (Darwin 24.6.0, 8-core arm64)
- `data.table` thread count: 4 (default)
- Alle dependencies actueel per main-branch op 2026-04-26 (W4 + W5 gemerged)

## Resultaten

### 1.000.000 feiten-rijen

| Fase | Mediaan | Iteraties |
|---|---|---|
| `new.star() %>% addDimView() %>% addMeasure() %>% addPresentation()` | **28,7 ms** | 5 |
| Eerste `renderDims(input, output)` | **161 ms** | 1 |
| `factsFiltered()` — geen dim-selectie (no-op fast path) | **1,23 ms** | 10 |
| `factsFiltered()` — **mét** dim-selectie (12 IDs gefilterd) | **17,7 ms** | 10 |
| `facts` in geheugen | 11,4 MB | — |

### 10.000.000 feiten-rijen

| Fase | Mediaan | Iteraties |
|---|---|---|
| `new.star() %>% addDimView() %>% addMeasure() %>% addPresentation()` | **210 ms** | 5 |
| Eerste `renderDims(input, output)` | **766 ms** | 1 |
| `factsFiltered()` — geen dim-selectie | **1,96 ms** | 10 |
| `factsFiltered()` — **mét** dim-selectie (12 IDs gefilterd) | **163 ms** | 10 |
| `facts` in geheugen | 114,4 MB | — |

## Wat de cijfers vertellen

**Constructie is goedkoop.** ~30 ms bij 1M, ~200 ms bij 10M. Schaalt
~7× voor 10× rijen — superlineair, vrijwel zeker de
foreign-key-uniciteits-scan in `addDimView()` (`unique(env$facts[[keyColumn]])`
versus de key-kolom van de dimensie). Geen probleem zolang er niet
een ster per request gebouwd wordt.

**Eerste render is bij 10M voelbaar.** 766 ms zit aan de bovenkant
van "acceptabel voor een dashboard-load". Het loont om dit individueel
te profielen als dit ooit de gevoelde bottleneck wordt — de
profvis flame graph in `docs/perf/baseline-1M.html` laat zien waar
de tijd naartoe gaat in het 1M-geval; herhaal met `N <- 1e7` in het
script om de 10M-flame-graph te zien.

**Het hot path is `factsFiltered()` met een echte selectie.** Dit
draait *bij elke dim-klik en bij elke selectie-wijziging* in een live
dashboard. De schaling is lineair: 17,7 ms → 163 ms gaande van 1M
naar 10M. Geëxtrapoleerd kost **bij 100M rijen elke klik ~1,6 seconde
alleen al aan filtering**, vóór de renderer ook maar begint. Dat is
het punt waar R/Shiny niet meer de juiste architectuur is en
DB-pushdown (DuckDB, columnar warehouse) noodzakelijk wordt.

**Geheugen is lineair en bescheiden.** 11,4 MB / 114,4 MB voor de
feiten; star-env-overhead is verwaarloosbaar. De interpreter-overhead
van R is niet de beperkende factor op welk van deze schalen dan ook.

## Praktische drempels

| Feiten-rijen | Server-side latency per interactie | Aanbeveling |
|---|---|---|
| ≤ 1M | < 20 ms filter, < 200 ms render | dwhr zoals het is, geen tuning nodig |
| 1M – 10M | < 200 ms filter, < 1 s render | dwhr zoals het is; zet `factCaching = TRUE` aan, gebruik `serverSideTable = TRUE` voor DT-presentaties |
| 10M – 50M | 200 ms – 1 s filter | Overweeg `env$facts` (in-memory data.frame) te vervangen door een DuckDB-connectie. Vereist een v3.0-class refactor. |
| 50M+ | filter > 1 s | Verplaats de star-backend naar een DB-pushdown-pattern. Ofwel DuckDB (in-process) of een warehouse (Snowflake / BigQuery / ClickHouse). |

## Wat dit niet zegt

- **Clientside-kosten.** Een 200 ms render-side latency wordt
  doorgaans een 600–800 ms gevoelde latency zodra DT de DOM heeft
  opgebouwd en Highcharts een redraw heeft gedaan. De browserside-gap
  is op alle schalen onder 10M rijen groter dan de R-side-gap.
- **Multi-user-gedrag.** Elke Shiny-sessie krijgt zijn eigen
  R-process. Bovenstaande cijfers zijn per sessie. 100 gelijktijdige
  gebruikers bij 10M rijen ≈ 100 × 114 MB = 11 GB resident
  feiten-data. Niet CPU maar geheugen is de multi-user-beperking.
- **Amortisatie van constructie-overhead.** Een echt dashboard
  construeert de star *één keer per sessie*, niet per interactie. De
  ~200 ms 10M-constructiekosten worden bij sessie-start eenmalig
  betaald.
- **Effectiviteit van caching.** dwhr heeft `factCaching` en de
  DT-optie `serverSideTable`. Met die ingeschakeld hebben we niet
  gebenchmarkt — dat is een waardevolle follow-up om de realistische
  ondergrens te karakteriseren.

## Profvis flame graph

`docs/perf/baseline-1M.html` is de volledige profvis-output van één
end-to-end pass bij 1M rijen: constructie → render → filter-call.
Open in een browser; hover over de call-stack om kosten per regel te
zien.

Het bestand is selfcontained (~3 MB); GitHub rendert het niet mooi
maar lokaal opent het correct.
