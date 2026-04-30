# Aanvullende BI-tools — Apache Superset, Power BI, self-service-opties

Referentiedocument om na te denken over hoe `dwhr` past binnen een
groter BI-ecosysteem, inclusief tools die dwhr **aanvullen**
(verschillende taken, hetzelfde warehouse) en tools die overlappen
met self-service-analytics. Begeleidend document bij
[`docs/nl/ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md); dit
document focust op tools die *naast* dwhr staan in plaats van het te
vervangen.

> **Engelstalig origineel:** [`docs/BI-STACK-COMPLEMENTS.md`](../BI-STACK-COMPLEMENTS.md)

Dit is analyse, geen aanbeveling om een specifieke tool te adopteren.

---

## 1. Het drie-lagen BI-raamwerk

De meeste echte BI-organisaties draaien meerdere tools naast elkaar
omdat geen enkele tool al deze drie taken wint:

| Laag | Taak | Doelgroep | Interactiediepte | Vorm van de tool |
|---|---|---|---|---|
| **1 — Brede consumptie** | Exec-scorecards, maandelijkse KPI's, "de cijfers van het bedrijf" | Veel kijkers (10-en–1000-en) | Vooral lezen; lichte filtering | Gepolijste dashboard-tool |
| **2 — Maatwerk-governance** | KPI-commentaar, write-back naar warehouse, board-grade PDF-rapporten, custom business-logic | Weinig belangrijke gebruikers (managers, controllers) | Zware interactie + write-back + audit trail | Programmeerbaar framework — **dwhr's domein** |
| **3 — Self-service-exploratie** | Ad-hoc analyse door power users; "ik heb een hypothese" | Power users (analisten, finance, ops) | Hoge interactie; weggooi-analyses | Notebook / spreadsheet / SQL-workbench |

Hetzelfde warehouse onder alle drie. Opmerkingen en PDF-artefacten
die door laag 2 (dwhr) worden geschreven worden eersterangs data die
het warehouse weer aan laag 1-dashboards en laag 3-self-service-
queries blootstelt — de loop sluit zich via de datalaag.

Dit document gaat over **kandidaten voor laag 1 en laag 3**, omdat
dwhr al laag 2 invult.

---

## 2. Apache Superset versus Microsoft Power BI

Beide richten zich op laag 1. De bepalende lens is *waar data en
compute leven*, niet feature-checklists.

### Waar data en compute leven

| | Apache Superset | Microsoft Power BI |
|---|---|---|
| **Data-opslag** | Geen — verbindt met je warehouse | VertiPaq-engine (Microsoft cloud) by default; of je warehouse via DirectQuery |
| **Waar compute gebeurt** | In de bron-database | In VertiPaq (Import-mode); of DB (DirectQuery); of beide (Composite) |
| **Metadata-DB** | Postgres/MySQL (Superset's eigen) | Microsoft cloud |
| **Caching** | Redis/Memcached (optioneel) | VertiPaq is inherent gecached |
| **Implicatie** | Performance = warehouse-performance | Performance = VertiPaq-performance, grotendeels onafhankelijk van bron-DB |

**Superset** is een *dunne BI-laag* — het slaat geen eigen data op.
Klik op een chart → Superset vertaalt naar SQL → DB voert uit →
Superset ontvangt resultaatrijen → rendert. Performance is wat je
warehouse levert. Voor teams die al op Snowflake / BigQuery /
ClickHouse / DuckDB zitten is dit de natuurlijke fit.

**Power BI** is een *opinionated platform* — VertiPaq is een
column-store, in-memory, compressed engine die "klein tot middelgroot
data snel laat aanvoelen" zonder de bron-DB überhaupt te raken. De
afweging zijn dataset-grootte-limieten (~1 GB op Pro, ~400 GB op
Premium-capaciteit) en diepere Microsoft-lock-in.

### Side-by-side-vergelijking

| | Apache Superset | Power BI |
|---|---|---|
| Licentie / prijs | Apache-2.0 OSS; betaal voor hosting + ops; Preset.io voor managed | Pro $14/user/mnd; Premium Per User $24/user/mnd; Premium-capaciteit ~$5K/mnd+ |
| Authoring | Web-UI + SQL Lab | Power BI Desktop (alleen Windows) → publiceer naar Service |
| Semantische laag | Licht — virtuele datasets, Jinja in SQL | Rijk — relaties, DAX-measures, hiërarchieën, time intelligence |
| Calculatie-taal | SQL (+ Jinja) | DAX (functioneel, columnar-aware), M (Power Query) |
| PDF / paginated reports | Geplande mail met screenshot/PDF — basaal | Paginated Reports (afgeleid van SSRS) — board-grade |
| Native write-back | ✗ | ✗ (mogelijk via Power Apps) |
| Opmerkingen / annotaties | Beperkt (chart-niveau, in metadata-DB) | Dashboard-niveau opmerkingen in PBI Service |
| Embedden in eigen app | iframe of SDK (gratis) | Power BI Embedded (Azure-SKU, $$$) |
| Auth / governance | Rollen, RLS, basale audit | AD/Entra, RLS, sensitivity labels, M365-compliance, lineage |
| Ecosysteem-fit | Moderne data-stack (Snowflake, dbt, Airflow) | Microsoft (Excel, Office 365, Azure, Teams, Fabric) |

### Use cases — waar elk wint

**Superset wint wanneer:**
- Je hebt al een modern warehouse (Snowflake, BigQuery, ClickHouse, DuckDB).
- SQL-eerst analist-cultuur — je team schrijft toch al SQL.
- OSS / geen vendor-lock-in is een harde eis.
- Embedded analytics in je eigen product (de SDK is goed; per-user-pricing schaalt niet naar embedded).
- Multi-cloud of "definitief geen Microsoft"-houding.
- Je hebt data engineering-capaciteit om Postgres + Redis + Celery + de Superset-server te draaien.

**Power BI wint wanneer:**
- Microsoft-shop org (al op M365 / Azure / Entra). Integratiekosten bijna nul; governance-verhaal best-in-class voor compliance-zware industrieën.
- Excel-eerst analist-cultuur — Power Query is Excel's import-wizard geëvolueerd; DAX bouwt voort op Excel-formules.
- Rijke semantische modellering nodig (complexe measures, time intelligence, role-playing dimensions).
- Drag-drop-authoring voor niet-technische gebruikers vereist.
- Paginated reports (echte PDF) zijn een harde eis.
- Turnkey-deployment zonder ops-overhead.

### Eerlijke framing

Deze twee concurreren niet echt om dezelfde taak:

- **Superset is infrastructuur** — hoe een data-team warehouse-data exposeert aan consumenten. Strak gekoppeld aan "je warehouse is de bron van waarheid".
- **Power BI is een product** — zelfvoorzienend dashboarding+modelling+sharing-platform dat ook met je warehouse kan verbinden, maar er niet van afhangt.

Org-vorm voorspelt de keuze beter dan feature-vergelijkingen:

- "We hebben een data engineering-team en een warehouse" → Superset (of Looker, Mode, Hex)
- "We hebben analisten en Excel" → Power BI (of Tableau, Domo)
- "We hebben R/Python-developers die maatwerk-analytische tools bouwen" → dwhr / Streamlit / Dash / zelfgebouwd

### Microsoft Fabric — de strategische richting

Microsoft Fabric (gelanceerd 2023) bundelt Power BI met OneLake
(Parquet op object-storage), Synapse, Data Factory, en Copilot. De
nieuwe **DirectLake-mode** voor Power BI bevraagt Parquet in OneLake
zonder de VertiPaq-import-stap — Microsoft's antwoord op de moderne
data-stack.

Fabric repareert sommige van Power BI's klassieke bugginess (de
`.pbix`-corruptie / refresh-failure / VertiPaq-quirk-klasse) door de
architectuur te wijzigen. Maar het verdiept Microsoft-lock-in
dramatisch en de prijsstelling (capaciteits-gebaseerd, ~$5K/mnd+ voor
zinvolle capaciteit) is agressief. Als je organisatie zich aan de
Microsoft-stack committeert, is Fabric het toekomstvaste Power
BI-pad. Zo niet, dan is het irrelevant.

---

## 3. Self-service-analytics — de laag 3-vraag

"Self-service" doet veel werk in BI-conversaties. Het splitst in drie
taken die vaak verschillende tools nodig hebben:

1. **Ad-hoc-exploratie** — "ik heb een hypothese, geef me data om die
   te testen". SQL of spreadsheet. Output: meestal weggegooid na het
   antwoord.
2. **Terugkerende analyse** — "ik doe dit elke maand, kan ik het
   parametriseren?" Notebook of templated dashboard. Output: opnieuw
   draaien met nieuwe data.
3. **Een nieuw dashboard bouwen voor iemand anders** — semi-
   developer-werk. Output: een gepubliceerd artefact.

Power users willen alle drie. Excel-gebruikers willen meestal #1, soms
#2, bijna nooit #3. Tool-keuze verandert per taak.

### Het Excel-plafond-probleem (en Excels verborgen antwoord)

Excels harde limiet is **1.048.576 rijen × 16.384 kolommen** per
sheet. Dat is een artefact van het Excel 2007-bestandsformaat en gaat
niet veranderen.

Maar: **Power Pivot** (sinds 2010 in Excel ingebouwd, gratis) draait
dezelfde VertiPaq-columnar-engine als Power BI. Data die in het Power
Pivot **datamodel** geladen wordt **leeft niet in een sheet** — het
leeft in het model, waar de limiet in essentie RAM is.
100M-rijen-aggregaties in Power Pivot zijn routine; de sheet ziet
alleen de pivot-table-samenvatting, die binnen het 1M-plafond past
omdat het al geaggregeerd is.

De meeste Excel-liefhebbers hebben Power Pivot nooit getoond gekregen.
Het is het **antwoord met de minste moeite** op "Excel loopt tegen een
muur, maar ik wil geen Power BI leren" — zelfde engine, zelfde DAX,
zelfde M-language Power Query, maar je verlaat Excel niet. Een
één-daagse workshop waard vóór elk tool-migratie-gesprek.

Dat gezegd hebbende: zelfs Power Pivot loopt tegen z'n grenzen aan
zodra je echte samenwerking, lineage, of cross-team-sharing nodig
hebt. Het `.xlsx`-bestand is nog steeds een enkel artefact dat per
e-mail of SharePoint wordt rondgestuurd. Voor governance, versionering
of iets meer dan één power user's spreadsheet, is Power Pivot de
brug, niet de bestemming.

### Moderne self-service-tools

Zes tools die het waard zijn om te kennen, ruwweg gerangschikt op
toekomstvastheid voor een niet-Microsoft-org:

| Tool | Vorm | Sterkte | Prijs | Toekomstvast? |
|---|---|---|---|---|
| **Hex** | SQL + Python-notebooks, reactive, AI-native | Beste "analist-notebook" van de moderne tijd | $24/user/mnd | ✓✓✓ — goed gefinancierd, AI-eerst, snel groeiend |
| **Sigma Computing** | Spreadsheet-UX over warehouse | Excel-gebruikers voelen zich thuis; geen rij-limiet | Enterprise (ondoorzichtig) | ✓✓ — gebouwd voor de "Excel-exodus"-use-case |
| **Observable Framework** | JS-notebooks, DuckDB-WASM in de browser | Gratis, OSS, volledig draagbaar | Gratis / cloud betaald | ✓✓✓ — open standaarden, laagste lock-in |
| **Rill Data** | Code-eerst dashboards op DuckDB | Sub-seconde aggregaties op big data; OSS | Gratis / Rill Cloud | ✓✓ — nieuwer, kleinere community, DuckDB-gok werkt uit |
| **Mode** | SQL + Python-notebooks, dashboards | Volwassen; risico op stagnatie sinds ThoughtSpot-overname | $- | ✓ — risico om gedeprioriteerd te worden |
| **Apache Superset SQL Lab** | SQL-editor binnen Superset | Gratis, past in het complementary-stack-verhaal | Gratis | ✓✓ — zelfde toekomstvastheid als Superset zelf |

Drie hiervan verdienen meer diepgang.

#### Hex — het beste moderne analist-notebook

[hex.tech](https://hex.tech) — notebook-stijl met reactive cells
(denk aan Jupyter + Observable + Shiny). SQL en Python wisselen
naadloos af. Ingebouwde AI ("Hex Magic") schrijft SQL/Python vanuit
prompts.

Waar analisten die Mode ontgroeid zijn maar geen ruwe Jupyter willen
terechtkomen. Prijs is per user en niet goedkoop, maar de
productiviteitswinst is reëel — één Hex-gebruiker met de AI-features
vervangt wat in oudere tools 2-3 handmatige analisten was.

Lock-in: gemiddeld. SQL en Python zijn draagbaar; Hex's notebook-
formaat en reactive-runtime niet.

#### Sigma Computing — Excel voor cloud-warehouses

[sigmacomputing.com](https://sigmacomputing.com) —
spreadsheet-interface over Snowflake/BigQuery/Redshift. Formules,
celverwijzingen, drag-to-fill — hetzelfde mentale model als Excel.
Maar het draait op het warehouse, dus is er **geen rij-limiet** en
worden wijzigingen geversionerd en deelbaar.

Specifiek gebouwd voor de "Excel-gebruikers leren niets anders"-
situatie. Vaak het antwoord wanneer een org Power BI probeert,
faalt, en een uitweg nodig heeft die het finance-team niet hoeft om
te scholen.

Prijs is enterprise (niet transparant), wat de belangrijkste barrière
is.

#### Observable Framework — de open-standaarden-gok

[observablehq.com/framework](https://observablehq.com/framework) —
Markdown + JavaScript-bestanden, DuckDB-WASM die in de browser
draait, deploy als statische HTML naar waar dan ook.

Geen vendor, geen server, geen lock-in. De DuckDB-WASM-truc betekent
dat een ~100MB Parquet-bestand in de browser sub-seconde-aggregaties
op miljoenen rijen kan serveren zonder enige backend.

Steilere leercurve (JavaScript) dan Hex of Sigma. Het meest draagbare
artefact van alle tools hier.

### Pure-OSS self-service-stack

Als "geen vendor, op geen enkele manier" de beperking is:

- **Apache Superset SQL Lab** voor power users die SQL schrijven —
  bevraag het warehouse, sla snippets op, deel resultaten.
- **JupyterHub** (of Posit Workbench voor R-mensen) voor notebook
  power users — self-hosted, vrij schaalbaar.
- **Quarto** voor het "ik wil dat mijn analyse een publiceerbaar
  document is, geen one-off"-patroon — Markdown + R/Python/SQL/Julia
  → HTML/PDF/Word/Reveal.js. Gratis, OSS, het dichtstbijzijnde "rmarkdown
  voor iedereen".
- **DuckDB CLI** voor warehouse-bypass — analisten bevragen
  Parquet-bestanden direct vanuit object-storage; geen server nodig
  voor veel one-off-vragen.
- **Observable Framework** voor deelbare artefacten.

Deze stack kost niets aan licenties, vereist echte ops-capaciteit, en
verzorgt ~95% van self-service als je de juiste cultuur hebt
(SQL-comfortabele analisten).

---

## 4. Bijgewerkte drie-lagen-framing

Alles bij elkaar:

| Laag | Taak | Tools die passen |
|---|---|---|
| **1 — Brede consumptie** | Veel kijkers, simpele interacties, exec-scorecards | Apache Superset, Looker Studio, Power BI Service / Fabric |
| **2 — Maatwerk-governance** | KPI-commentaar write-back, board-PDF's, custom logic | **dwhr** (huidig); uiteindelijk dwhr + DuckDB-backend |
| **3 — Self-service-exploratie** | Ad-hoc analyse door power users | **Hex** of **Sigma** (commercieel); **Observable + DuckDB** of **Quarto + JupyterHub** (OSS); **Power Pivot** als de Excel-brug |

Hetzelfde warehouse onder alle drie. Opmerkingen en PDF-artefacten
geschreven door dwhr (laag 2) worden eersterangs data — laag 1-
dashboards kunnen "opmerkingen per KPI per maand" plotten, laag 3-
self-service-queries kunnen manager-commentaar correleren met KPI-
bewegingen.

---

## 5. Beslis-houding (huidig)

**De aanvullende-stack-framing zou geen verandering aan het huidige
modernization-plan moeten aansturen.** De CRAN-prep-workstreams
(W1-W8) maken dwhr's laag 2-rol af; de post-CRAN-schetsen (W10
DuckDB-backend, W11 bslib UI-refresh) houden dwhr competitief in
die rol.

**Als/wanneer bredere BI-tooling-vragen opkomen** (typisch gestuurd
door "het analist-team heeft ad-hoc-tools nodig" of "execs willen een
portfolio-view over alle dwhr-dashboards"), is dit document de input
— geen voorschrift.

**Specifieke aanbevelingen om met niet-technische stakeholders te
delen:**

1. Als finance/ops tegen Excel's rij-plafond aanlopen, **probeer
   eerst Power Pivot** vóór een tool-wisseling-gesprek. Zelfde Excel,
   geen rij-limiet, gratis.
2. Als Power BI blijft kapotgaan en mensen terugduwt naar Excel, is
   de uitweg met de minste verloren gebruikers **Sigma Computing**,
   niet "iedereen leert harder DAX".
3. Als het data-team een self-service-oppervlak wil dat naast dwhr
   werkt, is het goedkoopste geloofwaardige antwoord **Apache
   Superset's SQL Lab plus Quarto + JupyterHub** — volledig OSS,
   past op hetzelfde warehouse waar dwhr al uit leest, geen per-user-
   licentie.
4. Als de org gecommitteerd is aan Microsoft, is **Fabric DirectLake
   het toekomstvaste PBI-pad**, niet klassiek Import-mode Desktop.
   Plan de capaciteits-licentie.

---

## Appendix — bronnen

- Apache Superset: <https://superset.apache.org/>, <https://preset.io/>
- Power BI-licenties: <https://www.microsoft.com/en-us/power-platform/products/power-bi/pricing>
- Microsoft Fabric: <https://www.microsoft.com/en-us/microsoft-fabric>
- Hex: <https://hex.tech>
- Sigma Computing: <https://sigmacomputing.com>
- Observable Framework: <https://observablehq.com/framework>
- Rill Data: <https://www.rilldata.com/>
- Power Pivot in Excel: <https://support.microsoft.com/nl-nl/office/power-pivot-overzicht-en-leertraject-f9001958-7901-4caa-ad80-028a6d2432ed>
- DuckDB: <https://duckdb.org>
- Quarto: <https://quarto.org>
- Begeleidende docs: [`ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md), [`PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md), [`MODERNIZATION.md`](../MODERNIZATION.md)
