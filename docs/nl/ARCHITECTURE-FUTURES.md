# Architectuur-toekomst — stack-alternatieven voor dwhr

Referentiedocument voor een open gesprek over de vraag of de
R/Shiny-stack op lange termijn het juiste thuis is voor het soort
BI-workload dat `dwhr` mogelijk maakt. **Dit is analyse, geen
beslissing.** De huidige modernization-fase rondt het CRAN-debuut van
dwhr af op de R/Shiny-stack; dit document is input voor een eventueel
toekomstig "moeten we niet herbouwen?"-gesprek.

> **Engelstalig origineel:** [`docs/ARCHITECTURE-FUTURES.md`](../ARCHITECTURE-FUTURES.md)

Begeleidend document bij [`docs/nl/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md),
dat de empirische cijfers levert die hieronder geciteerd worden.

---

## 1. De daadwerkelijke workload die dwhr mogelijk maakt

`dwhr` is **een programmeer-primitief in de R BI-wereld**, geen
afgewerkt dashboard-product. Het package zelf is read-only op data en
stopt bij "render een interactief star-schema-dashboard". Daaromheen
bouwen consumenten de rest van een gesloten BI-werkstroom op:

```
┌───────────────────────────────────────────────────────────────────┐
│  Data warehouse / ODBC                                            │
│       │                          ▲                                │
│       │ leest feiten/dims        │ writeback                      │
│       ▼                          │ (manager-opmerkingen           │
│  ┌─────────────────┐             │  bij KPI's, beslissingen,      │
│  │  dwhr dashboard │             │  audit trail)                  │
│  │  (R/Shiny)      │─── klik ───▶│                                │
│  │                 │   drill,    │                                │
│  │  star-schema    │   filter,   │                                │
│  │  drill-down,    │   commen-   │                                │
│  │  measures,      │   taar      │                                │
│  │  charts/tables  │             │                                │
│  └─────────────────┘             │                                │
│       │                          │                                │
│       └──── data ────────────────┘                                │
│                                                                   │
│       │ snapshot voor de raad                                     │
│       ▼                                                           │
│  ┌─────────────────┐                                              │
│  │ rmarkdown /     │                                              │
│  │ Sweave + LaTeX  │  ──────▶  PDF-rapport                        │
│  │ (latexEscape    │           (verantwoording                    │
│  │  helper uit     │            op bestuursniveau)                │
│  │  dwhr)          │                                              │
│  └─────────────────┘                                              │
└───────────────────────────────────────────────────────────────────┘
```

**Drie dingen die deze loop moet ondersteunen die zuivere dashboards
niet doen:**

1. **Write-back naar het warehouse.** Managers bekijken niet alleen
   KPI's — ze *becommentariëren* ze, en die opmerkingen worden
   gepersisteerd (audit-trail-semantiek, governance, "wat heeft de
   raad vorig kwartaal besloten").
2. **Programmable PDF-generatie.** Een echt verantwoordingsrapport —
   geen screenshot, geen PNG-export — met gestructureerde commentaar,
   tabellen, charts, headers, footers, signing pages. R's `rmarkdown`
   / `Sweave` + `latexEscape()` doen dit native; de meeste BI-tools
   niet.
3. **Custom business-logic in measures.** dwhr exposeert
   `addMeasureDerrived(userFunc = ...)` zodat een aggregatie een
   willekeurige R-functie kan zijn, plus per-dim hooks
   (`<dim>LevelChangeHook`) voor maatwerk-event-handling. Het package
   is *programmeerbaar vanaf dag één*.

Deze loop is de bepalende lens. Elke kandidaat-stack moet beoordeeld
worden op **alle drie** de dimensies, niet alleen "kan het een chart
tekenen."

## 2. Waar de tijd vandaag naartoe gaat

Uit [`docs/nl/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md), gemeten
op R 4.5.3:

| Fase | 1M rijen | 10M rijen |
|---|---|---|
| Star-constructie | 29 ms | 210 ms |
| Eerste render | 161 ms | 766 ms |
| `factsFiltered()` no-op | 1,2 ms | 2,0 ms |
| **`factsFiltered()` met dim-selectie** | **17,7 ms** | **163 ms** |

Server-side R is **niet** de bottleneck tot 10M rijen. Het hot path
(`factsFiltered()` met een echte selectie) is 17,7 ms bij 1M en 163 ms
bij 10M — voelbaar maar niet pijnlijk. De browser-side kosten (DT die
de DOM opbouwt, Highcharts die redraw doet) tellen daar in een live
app meestal nog 50–100% bovenop.

Praktische drempels:
- **≤ 10M rijen**: dwhr zoals het is, prima.
- **10M – 50M rijen**: vereist `factCaching = TRUE` en
  `serverSideTable = TRUE`; overweeg DuckDB-backend.
- **50M+ rijen**: aggregatie moet naar een DB toe (DuckDB in-process
  of een warehouse). Op deze schaal is R/Shiny vs Python/wat-dan-ook
  irrelevant — de SQL-engine doet het werk.

**De "single core per sessie"-framing is reëel maar minder hard dan
het klinkt.** Elke Shiny-sessie krijgt z'n eigen R-process. Dus 100
gelijktijdige gebruikers = 100 R-processes, verdeeld over cores. De
beperking is niet "R is per core langzaam" maar "elke gebruiker houdt
de volledige feiten-tabel in z'n sessie". Die beperking heeft
Streamlit, Dash, en elk in-memory dashboarding framework. DB-pushdown
omzeilt het in welke taal dan ook.

## 3. Evaluatie-dimensies

Een serieuze vergelijking moet kandidaten scoren op **alle**
onderstaande dimensies, niet alleen de makkelijke:

| # | Dimensie | Waarom dit telt voor dwhr's workload |
|---|---|---|
| 1 | **Read-interactiviteit** (drill-down, filter, charts) | Basisvoorwaarde; iedereen voldoet |
| 2 | **Write-interactiviteit** (KPI-comments → DB) | Gesloten governance-loop — harde eis |
| 3 | **Rapportgeneratie** (echte PDF, geen screenshot) | Verantwoording aan de raad — harde eis |
| 4 | **Custom business-logic** (programmeerbare measures, hooks) | Bepalend kenmerk van dwhr; niet optioneel |
| 5 | **Schaal-plafond** (1M / 10M / 100M+ rijen) | Waar de architectuur breekt |
| 6 | **Visueel plafond** (UX-polish, moderne UI) | Bepaalt of je "de mooiste dashboards" kunt bouwen of genoegen neemt met "goed genoeg" |
| 7 | **AI-assisteerbaarheid** (hoe goed Claude/Copilot helpen) | Moderne realiteit; raakt iteratiesnelheid en hiring |
| 8 | **Hiring-ecosysteem** | Wie kun je over 5 jaar hiren om dit te onderhouden |
| 9 | **Migratiekosten vanaf dwhr** | Wat kost het echt |
| 10 | **Multi-user / governance** (auth, rollen, audit) | Echte deployment |

## 4. Stack-kandidaten

Acht kandidaten, gescoord tegen de bovenstaande dimensies.
Diskwalificaties worden expliciet benoemd.

### 4.1 dwhr vandaag — R/Shiny + data.table + DT + Highcharts + RODBC + downstream rmarkdown/LaTeX

- **Read**: ✓ — dat is z'n hele rol.
- **Write**: ✓ — consument voegt `RODBC::sqlSave` /
  `DBI::dbWriteTable` toe zodra W3 geland is.
- **PDF**: ✓✓ — rmarkdown + Sweave + LaTeX is best-in-class voor
  programmable verantwoordingsrapporten. `latexEscape()` is dwhr's
  bijdrage aan die pipeline.
- **Custom logic**: ✓✓ — `addMeasureDerrived(userFunc = ...)`,
  dim-hooks, willekeurige R-functies in measures. Programmeerbaar
  vanaf dag één.
- **Schaal**: 10M rijen comfortabel; 50M+ pijnlijk zonder
  DuckDB-backend.
- **Visueel plafond**: gemiddeld — Shiny's UI-vocabulaire is
  gedateerd, DT en Highcharts zien er beide uit als 2018.
  CSS-overrides kunnen, maar je vecht tegen het framework. Plafond
  ligt onder modern React.
- **AI-assisteerbaarheid**: gemiddeld — Claude/Copilot kennen R, maar
  de long tail van Shiny / `htmlwidgets` / reactive-idioms is minder
  goed getraind dan het React/TS-ecosysteem. Generatiekwaliteit zakt
  bij maatwerk-patronen zoals de reactive counters van dwhr.
- **Hiring**: smal — R Shiny-ontwikkelaars zijn een kleinere pool dan
  Python- of JS-devs en geconcentreerd in academie/biotech.
- **Migratiekosten**: nul (baseline).

### 4.2 dwhr + DuckDB-backend

De dwhr-abstracties blijven; `env$facts` wordt een DuckDB-connectie
in plaats van een in-memory data.table. Aggregaties worden in DuckDB
gepushed.

- **Read** ✓, **Write** ✓ (via DBI), **PDF** ✓✓, **Custom logic** ✓✓
  (gewoon SQL schrijven of DuckDB's R-UDF-support gebruiken).
- **Schaal**: 100M+ rijen comfortabel; miljarden haalbaar.
- **Visueel plafond**: gelijk aan dwhr (geen UI-wijziging).
- **Migratiekosten**: 2-4 weken. Een v3.0-workstream na CRAN.

Dit is de **goedkoopste serieuze upgrade**. Behoudt Pieters ontwerp,
behoudt de R BI-loop, verwijdert het schaal-plafond. Sterkste
kandidaat als het doel is "de bruikbare schaal van dwhr matcht met
moderne data warehouses".

### 4.3 Streamlit + Polars

Python-equivalent van Shiny. Polars (Rust eronder) is snel.

- **Read**: ✓.
- **Write**: ✓ (elke DB-driver).
- **PDF**: △ — Quarto-Python of WeasyPrint/Jinja bestaan, maar de
  polish-gap met R rmarkdown is reëel. Een board-grade PDF met
  tabellen/charts erin krijgen is meer bedrading.
- **Custom logic**: ✓ — Python-functies overal.
- **Schaal**: vergelijkbaar met dwhr+DuckDB als je DuckDB of Polars
  lazyframes inschakelt; in-memory plafond vergelijkbaar.
- **Visueel plafond**: laag. Streamlit's UI-vocabulaire is *meer*
  gedateerd dan dat van Shiny; minder customizable. Plafond is
  *slechter*.
- **AI-assisteerbaarheid**: hoog — Streamlit is goed getraind.
- **Hiring**: makkelijk.
- **Migratiekosten**: 4-6 maanden herschrijven. Nieuwe repo. dwhr's
  DSL weg.

Streamlit is aantrekkelijk omdat Python wijdverbreid is, maar voor
*deze workload* (programmeerbaar BI-primitief met PDF-rapporten) is
het een strikte downgrade. Kies dit als Python verplicht is; anders
overslaan.

### 4.4 Dash (Plotly Python) + DuckDB + ReportLab

Meer "framework"-vormig dan Streamlit. Dichter bij Shiny in geest.

- **Read** ✓, **Write** ✓, **PDF** △ (ReportLab is prima maar
  omslachtig), **Custom logic** ✓, **Schaal** ✓ (DuckDB).
- **Visueel plafond**: gemiddeld. Plotly chart-vocabulaire; UI is
  React onderwater maar je schrijft nog steeds Python en vecht tegen
  Plotly-idioms.
- **AI-assisteerbaarheid**: hoog.
- **Migratiekosten**: 4-6 maanden herschrijven.

Een redelijk Python-equivalent van dwhr+DuckDB. Hetzelfde schaal-
verhaal, zwakkere PDF-story, vergelijkbaar visueel plafond. Kies dit
als je specifiek een Python-native BI-framework met Shiny-achtige
reactive-semantiek wilt.

### 4.5 FastAPI + React/TS + DuckDB (+ Puppeteer voor PDF)

De "alles opnieuw bouwen met een moderne frontend"-optie. Backend is
een Python- of TypeScript-API; frontend is een React-app met een
moderne dataviz-stack (D3, ECharts, Recharts, visx, Plotly).

- **Read** ✓, **Write** ✓, **PDF** ✓ (Puppeteer + headless Chromium
  rendert een HTML-rapport — werkt goed, maar je bouwt zelf het
  rapport-template; geen rmarkdown-equivalent), **Custom logic** ✓
  (overal in de stack), **Schaal** ✓ (DuckDB of warehouse).
- **Visueel plafond**: ✓✓✓ — het *hoogste van alle kandidaten*.
  React + TypeScript + een modern design system (Radix, shadcn/ui,
  Tailwind) maakt het mogelijk om dashboards te bouwen die eruitzien
  als Linear, Vercel, Stripe — het huidige plafond voor BI-UX. Geen
  framework-beperkingen; je shipt wat je kunt ontwerpen.
- **AI-assisteerbaarheid**: ✓✓✓ — het *hoogste van alle kandidaten*.
  React + TypeScript is de best-getrainde stack voor huidige LLM's.
  Claude / Copilot genereren idiomatische, type-safe componenten
  vloeiend. Component-niveau iteratie is snel, refactors zijn veiliger
  (TS vangt breuken), en het ecosysteem (shadcn/ui, Radix-primitives,
  Tailwind) is goed gedocumenteerd in trainingsdata. **Dit is reëel**,
  geen hype: één engineer met Claude kan een React/TS-dashboard-
  codebase volhouden die anders een klein frontend-team nodig had.
- **Hiring**: ✓✓ (grootste pool van alle kandidaten).
- **Migratiekosten**: 6-12 maanden voor een klein team. Nieuwe repo.
  dwhr's DSL moet of in TS opnieuw geïmplementeerd of vervangen door
  een andere abstractie. De R BI-loop (rmarkdown, latexEscape, R-in-
  the-loop consumer-code) verdwijnt — vervangen door JS/TS-equivalent
  dat je zelf bouwt.
- **Kosten van het verlaten van R**: dit is de diepste snee. De
  downstream R-code die dwhr gebruikt (rmarkdown-rapporten, ad-hoc
  analyses, de consumer-laag die write-back en PDF-generatie doet)
  migreert niet; die wordt herschreven in Python of TS.

**De eerlijke framing**: dit is een ander *product*, geen *port*. Je
krijgt de mooiste dashboards die je vandaag kunt bouwen, ten koste
van het verlaten van het R BI-ecosysteem. Voor een CRAN-library
gericht op R-ontwikkelaars past het niet. Voor "we willen het mooiste
interne BI-platform bouwen dat onze organisatie ooit had, en we zijn
bereid het te bemensen" — dan is het het juiste antwoord.

### 4.6 Apache Superset — buiten scope (gediskwalificeerd op dimensies 2, 3, 4)

- **Read** ✓✓ (volwassen, gepolijst).
- **Write**: ✗ — Superset is read-only op data. Er is geen native
  comment-write-back-naar-DB-werkstroom die past bij dwhr's
  KPI-commentaar-loop.
- **PDF**: ✗ — alleen basale screenshot/PNG-export. Geen board-grade
  programmable PDF.
- **Custom logic**: ✗ — SQL-semantische laag + visuele config. Geen
  programmeerbare measures, geen hooks, geen per-user R/Python-functies.
- **Andere doelgroep**: SQL-analisten in een UI versus ontwikkelaars
  die maatwerk-dashboards uit een library shippen. Andere
  productcategorie.

Superset is een multi-tenant BI-server, geen embeddable framework.
**Anders, niet beter.** Het zou dwhr's *use case* vervangen door een
andere, niet dwhr's use case goedkoper implementeren. Overslaan.

### 4.7 Evidence / Observable Framework — buiten scope (read-only)

- **Read** ✓ (zeer gepolijst; DuckDB-WASM in browser, snel).
- **Write**: ✗ — beide zijn static-site / read-only by design.
- **PDF**: △ (HTML → PDF via headless browser; geen programmable
  template).
- **Custom logic**: △ — JS in cellen, SQL in cellen; minder ergonomisch
  voor dwhr-stijl measure-abstracties.

Uitstekend voor *publiceren* van dashboards (analist → stakeholder);
nutteloos voor *governance-loops* (manager comments → DB → board PDF).

### 4.8 Metabase — buiten scope (zelfde als Superset)

Zelfde diskwalificaties als Superset op dimensies 2, 3, 4. Andere
doelgroep, andere productcategorie.

## 5. Vergelijking

| Stack | Read | Write | PDF | Custom logic | Schaal | Visueel | AI-assist | Hiring | Migratie |
|---|---|---|---|---|---|---|---|---|---|
| dwhr vandaag | ✓ | ✓ | ✓✓ | ✓✓ | 10M | gemiddeld | gemiddeld | smal | baseline |
| dwhr + DuckDB | ✓ | ✓ | ✓✓ | ✓✓ | 100M+ | gemiddeld | gemiddeld | smal | 2-4 wk |
| Streamlit + Polars | ✓ | ✓ | △ | ✓ | 100M+ | laag | hoog | makkelijk | 4-6 mnd |
| Dash + DuckDB | ✓ | ✓ | △ | ✓ | 100M+ | gemiddeld | hoog | makkelijk | 4-6 mnd |
| FastAPI + React/TS + DuckDB | ✓ | ✓ | ✓ | ✓ | 100M+ | **✓✓✓** | **✓✓✓** | makkelijk | 6-12 mnd |
| Apache Superset | ✓✓ | ✗ | ✗ | ✗ | warehouse | hoog | laag | makkelijk | vervangen |
| Evidence / Observable | ✓ | ✗ | △ | △ | DuckDB-WASM | hoog | gemiddeld | gemiddeld | vervangen |
| Metabase | ✓ | ✗ | ✗ | ✗ | warehouse | hoog | laag | makkelijk | vervangen |

## 6. Het visuele plafond — diepteanalyse

De grootste kloof tussen dwhr-vandaag en een moderne React+TS-rebuild
is **hoe het dashboard *eruitziet en aanvoelt***. Dat is belangrijker
dan de meeste performance-argumenten en verdient behandeling als
eersterangs dimensie.

Hoe het moderne plafond eruitziet (concrete voorbeelden ter referentie):
- Linear (linear.app) — toetsenbord-eerst, dicht, snel, mooie tabellen.
- Vercel-dashboards — typografie-gedreven, dark mode native,
  onmiddellijke responsiviteit.
- Posit Connect → Quarto-dashboards — een geloofwaardig R-side
  tegenvoorbeeld dat laat zien dat R *kan* benaderen tot dat plafond,
  maar niet via klassiek Shiny.
- shadcn/ui + Radix-componentenvocabulaire — de moderne OSS-baseline
  voor React-componentenbibliotheken.

Hoe klassiek Shiny + DT + Highcharts er ter vergelijking uitziet:
- Bootstrap 4-tijdperk-componenten, smal designvocabulaire.
- DataTable's visuele stijl is onmiskenbaar en gedateerd.
- Highcharts ziet eruit als 2015 (en heeft het licentieprobleem
  gedocumenteerd in [`docs/CHARTING-ALTERNATIVES.md`](../CHARTING-ALTERNATIVES.md)).
- CSS-overrides kunnen, maar je vecht tegen het framework.

**Mitigatie-paden binnen R**:
1. **`bslib`** — moderniseert Shiny's Bootstrap-laag. Echte upgrade.
2. **Quarto-dashboards** — een aanzienlijk modernere R-side optie.
   Ander model dan dwhr (Markdown-eerst), dus geen drop-in.
3. **Custom CSS + `htmlwidgets`** — duur, broos, niet wat de meeste
   teams aandurven.

Als "de mooiste dashboards die je vandaag kunt bouwen" een harde eis
is, **bereikt geen enkele R/Shiny-stack het React+TS-plafond.** Dat is
het eerlijke antwoord. R kan tot "goed genoeg" en "professioneel"
komen — maar niet tot "dit ziet eruit als een 2026 Vercel-app".

## 7. AI-assisteerbaarheid — diepteanalyse

Stack-keuze in 2026 heeft een nieuwe dimensie die in 2018 niet bestond:
**hoe goed helpen LLM's je het te schrijven en te onderhouden?**

- **React + TypeScript** is de sterkste in het veld. De combinatie
  van: enorm trainingscorpus, type-systeem dat het model concrete
  feedback geeft, bekende componenten-bibliotheken (shadcn/ui, Radix,
  MUI, Chakra), sterke buildtooling (Vite, Next.js), en moderne
  test-patronen betekent dat Claude/Copilot niet-triviale features
  end-to-end kunnen scaffolden met minimale handholding. Refactors
  zijn veiliger doordat TS breuken vangt.
- **Python (Streamlit / Dash)** is tweede. Goed getraind. Het
  ontbreken van types in Python is een wrijvingspunt — het model kan
  vloeiend Python scaffolden, maar is foutgevoeliger bij grotere
  refactors.
- **R / Shiny** ligt aanzienlijk achter. Kleiner trainingscorpus,
  reactive-idioms zijn maatwerk, de long tail (htmlwidgets-bindings,
  ODBC-patronen, RODBC vs DBI, Sweave) is wisselend. Modellen
  hallucineren meer op R Shiny dan op React/TS.

Wat dit in de praktijk betekent:
- Een onderhouder + Claude kan een React/TS-dashboard-codebase
  volhouden die historisch 2-3 frontend-devs nodig had.
- Diezelfde onderhouder + Claude op een Shiny-codebase besteedt meer
  tijd aan het bestrijden van hallucinaties en het opzoeken van
  idioms.
- De kloof zal groter worden, niet kleiner. Trainingscorpora groeien
  door op React/TS; Shiny's aandeel staat vast.

Dit is **geen** reden om dwhr op te geven — het is een reden om de
"als we toch herbouwen, wat is de bestemming?"-vraag zwaarder te
wegen richting de AI-sterkste stack. Als de dag komt dat
ziekenhuizen-zoals-Radboud beslissen om "het mooiste interne
BI-platform dat we ooit hadden" te bouwen, is React+TS het antwoord
dat het beste compounded met AI-ondersteund ontwikkelen over de
komende 5 jaar.

## 8. Aanbevelingsraamwerk

**Er is geen enkele beste stack — er zijn goede antwoorden per
prioriteit.**

| Prioriteit | Keuze |
|---|---|
| Ship CRAN; behoud wat werkt; minimaal herschrijven | dwhr zoals het is (huidig modernization-plan) |
| Idem + verwijder schaal-plafond | dwhr + DuckDB-backend (v3.0-workstream) |
| Moderniseer de look zonder R te verlaten | dwhr + DuckDB + `bslib`-UI-refresh, of migreer naar Quarto-dashboards |
| Uit R, BI-werkstroom-vorm behouden | Dash + DuckDB (dichtstbijzijnde Python-equivalent) |
| Bouw het mooiste dashboard ooit voor onze organisatie | FastAPI + React/TS + DuckDB (+ Puppeteer-PDF) |
| Schakel over naar een turnkey BI-tool (no code) | Apache Superset / Metabase — maar accepteer verlies van de write-back + PDF + custom-logic-loop |

**De bepalende vraag** is dezelfde als bij de charting-analyse, maar
op grotere schaal:

> **Is `dwhr` fundamenteel een R-side BI-primitief waar consumenten
> omheen programmeren, of is het een deployment van maatwerk-
> dashboards die toevallig in R is gebouwd?**

Als het de eerste is (wat de codebase, de `addMeasureDerrived`-DSL,
de `latexEscape`-helper en de expliciet-programmeerbare hooks alle
suggereren) — dan is het antwoord *dwhr blijft, evolueer richting
dwhr+DuckDB, moderniseer de UI in-place, ship CRAN als de OSS-
hommage aan Pieter waar je al aan begonnen bent*.

Als het de tweede is (wat een strategische verschuiving zou zijn,
geen feit over de huidige code) — dan is het rebuild-gesprek echt,
en is React+TS de bestemming als de rebuild plaatsvindt. Maar dat is
een ander product, kost 12+ maanden en een organisatorisch
commitment, geen tech-stack-swap.

## 9. Wat te doen *nu*

Deze fase (CRAN-modernization):
- Maak W1-W8 af zoals gepland. Houd dwhr.
- Ship de Highcharts-commerciële-licentie-notice (al in de aparte PR
  `docs/highcharts-license-notice`).

Deze fase + 1 (post-CRAN, v3.0):
- Voeg `bslib`-UI-refresh toe als de lichtste zichtbare winst.
- Schets en prototype DuckDB-backend-integratie als W10. De
  dwhr-DSL hoeft niet te wijzigen; de in-memory `env$facts` wordt
  een DuckDB-connectie achter een kleine adapter. Dit geeft je het
  100M+-rijen-plafond zonder R te verlaten.

Een echt rebuild-gesprek:
- Moet getriggered worden door **productstrategie**, niet door
  performance of esthetiek.
- Als getriggered, is de bestemming React + TypeScript + DuckDB +
  FastAPI/equivalent + Puppeteer-PDF. Geen port — een nieuw product.
- Dit document moet de input zijn voor dat gesprek, niet de output.

---

## Appendix — bronnen

- Performance-cijfers: [`docs/nl/PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md)
  (run `Rscript scripts/perf-baseline.R` om te reproduceren).
- Charting-library-analyse: [`docs/CHARTING-ALTERNATIVES.md`](../CHARTING-ALTERNATIVES.md)
  (Engelstalig).
- Modernization-spec: [`docs/MODERNIZATION.md`](../MODERNIZATION.md).
- DuckDB R-bindings: <https://duckdb.org/docs/api/r>.
- bslib (Bootstrap voor Shiny): <https://rstudio.github.io/bslib/>.
- Quarto-dashboards: <https://quarto.org/docs/dashboards/>.
- shadcn/ui (React-componentenvocabulaire dat naar verwezen wordt
  als "modern plafond"): <https://ui.shadcn.com/>.
- Posit Connect (moderne R-side dashboard-hosting):
  <https://posit.co/products/enterprise/connect/>.
