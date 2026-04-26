# Alternatief: Power BI + PowerPoint als governance-loop

Analyse van een alternatief voor dwhr's bespoke-governance-loop dat
door een ander team binnen de organisatie is voorgesteld: gebruik
**Power BI** voor de KPI-dashboards en **PowerPoint (.pptx)** als
artefact voor commentaar, board-cyclus en archivering.

Dit is geen aanbeveling. Het is een eerlijke vergelijking met dwhr's
huidige patroon zodat je een geïnformeerde afweging kunt maken — en
in het bijzonder zodat je het juiste gesprek kunt voeren wanneer het
andere team z'n voorstel verdedigt.

---

## 1. De voorgestelde werkstroom

Zoals begrepen:

```
┌──────────────────────────────────────────────────────────────────┐
│  Data warehouse                                                  │
│       │                                                          │
│       │ leest                                                    │
│       ▼                                                          │
│  ┌─────────────────┐                                             │
│  │  Power BI       │                                             │
│  │  (Service of    │                                             │
│  │   Desktop)      │                                             │
│  └─────────┬───────┘                                             │
│            │                                                     │
│            │ "Export to PowerPoint" / "Power BI for              │
│            │  PowerPoint"-add-in (live-tegels)                   │
│            ▼                                                     │
│  ┌─────────────────┐                                             │
│  │  .pptx-deck     │  ◀── managers voegen commentaar toe         │
│  │  in SharePoint  │      (tekstvakken, opmerkingen, review)     │
│  │  / Teams        │                                             │
│  └─────────┬───────┘                                             │
│            │                                                     │
│            │ "Save as PDF" + verzenden                           │
│            ▼                                                     │
│       Board pack (PDF + bron-.pptx in archief)                   │
└──────────────────────────────────────────────────────────────────┘
```

Twee Microsoft-mogelijkheden die dit echt mogelijk maken:

- **Power BI's "Export to PowerPoint"** (ingebouwd, statische
  beelden) — al jaren beschikbaar, eenvoudig.
- **De Power BI for PowerPoint-add-in** (sinds 2022) — embedt
  *live-refreshing* Power BI-tegels in PPTX-slides. Aanzienlijke
  upgrade ten opzichte van de oude statische export. Vereist Power
  BI Pro of hoger en M365.

Daaraan gekoppeld de standaard PowerPoint-collaboration-features:
- Tekstvakken / opmerkingen / review-tools voor commentaar.
- Co-authoring in M365 voor meerdere bewerkers tegelijk.
- SharePoint-versiehistorie als impliciete audit-trail.
- "Save as PDF" voor het formele bestuursrecord.

---

## 2. Pro's van de PPTX-route

### 2.1 Vertrouwd voor managers
PowerPoint is universeel bekend. Geen training, geen leercurve, geen
"weer een nieuwe tool". Voor een board waarin gemiddelde leeftijd 55+
is en waar Office al 30 jaar onderdeel van het werk is, telt dit
zwaar.

### 2.2 Vrij commentaar
Een tekstvak is vele malen flexibeler dan een gestructureerd
opmerkingenveld in dwhr. Managers kunnen tekenen, pijlen plaatsen,
stickies toevoegen, dingen omcirkelen. De *governance-realiteit* is
dat boards hun commentaar zelden in een database-rij willen typen —
ze willen "hier stoort me iets aan" met een rode markeerstift kunnen
zeggen.

### 2.3 Visuele vrijheid
Managers kunnen layouts aanpassen, branding toevoegen, slides
herordenen, bedrijfslogo's plaatsen. dwhr's PDF's zijn template-
gedreven; PPTX is per-vergadering boetseerbaar.

### 2.4 Geen write-back-complexiteit
Geen DB-schema voor opmerkingen, geen auth-model, geen RBAC op
commentaar-rijen. Opmerkingen leven in het bestand. Veel minder
engineering dan dwhr's loop (waar consumer-code de
`DBI::dbWriteTable`-laag bouwt).

### 2.5 Eenvoudige distributie
.pptx of .pdf in e-mail, SharePoint, Teams. De meeste boards werken
al op deze manier. Geen URL te onthouden, geen login nodig op
vergaderdagen.

### 2.6 Microsoft-native auth
Al geïntegreerd met M365 / Entra. Geen tweede authenticatielaag.
Voor compliance-zware orgs (zorginstellingen, gemeenten,
financiële dienstverleners) telt dit veel.

### 2.7 Snapshot-in-time
De .pptx **is** het artefact. Wat het dashboard op dat exacte moment
liet zien is voor altijd vastgelegd, ook als de onderliggende data
later wijzigt of de dashboard wordt verwijderd. Voor
verantwoordingsdoeleinden is dit een sterk eigenschap.

### 2.8 Past bij hoe boards écht werken
De realiteit van bestuursvergaderingen is: er is een deck, het deck
wordt doorgenomen, er worden besluiten genomen, het deck *is* de
notulen. dwhr's "continu monitoring met opmerkingen in DB" past meer
bij continue performance-management dan bij een kwartaal-board-cyclus.
Voor orgs waar het eindartefact het vergaderdeck is, is PPTX dichter
bij de werkelijkheid.

---

## 3. Con's van de PPTX-route

### 3.1 Opmerkingen niet in het warehouse
Dit is de **kern-afweging**. Opmerkingen leven in bestanden, niet in
queryable data. Je kunt niet:
- "Welke KPI's kregen de meeste opmerkingen vorig kwartaal?"
- "Toon alle commentaar over revenue in 2024."
- Manager-commentaar correleren met KPI-bewegingen over tijd.
- Trend-rapporten bouwen die opmerkingen incorporeren.

De audit-trail is een verzameling .pptx-bestanden in SharePoint, wat
veel zwakker is dan rijen in een opmerkingen-tabel. Voor
toezichthoudende of regelgevende contexten is dit een echt verschil.

### 3.2 Geen gestructureerde workflow
Een opmerking in een tekstvak heeft geen actiehouder, geen
deadline, geen status, geen prioriteit. dwhr kan (mits zo
geprogrammeerd) een opmerking koppelen aan een actie die vervolgens
opgevolgd wordt. PPTX-opmerkingen zijn vrije tekst zonder afdwingbare
structuur.

### 3.3 Versie-wildgroei
Het klassieke `Q3_board_pack_v7_FINAL_ECHT_def.pptx`-probleem.
SharePoint helpt, maar managers slaan nog steeds lokaal op, sturen per
mail door, en vergaderen vanuit een out-of-date kopie. dwhr's
"opmerking is een DB-rij" heeft één bron van waarheid.

### 3.4 Geen drill-down vanuit het artefact
Zodra geëxporteerd, is de snapshot statisch. De live-tegels in de
nieuwe Power BI-add-in helpen, maar je kunt niet vanuit PPT in
historische data drillen. dwhr's interactieve dashboards zijn op dat
punt veel rijker.

### 3.5 Per-board, per-kwartaal handmatig werk
Elke cyclus is mensgedreven export → commentaar → distribueren.
dwhr kan rapport-generatie volledig automatiseren (rmarkdown +
Sweave + cron). Het PPTX-pad maakt het tegenovergestelde — het
*vereist* menselijke tussenkomst in elke cyclus.

### 3.6 Vendor-lock-in op M365
Bestanden werken alleen volledig in PowerPoint. Alternatieven
(LibreOffice Impress, Keynote, Google Slides) verliezen features —
live-tegels, opmerkingen, co-authoring. Als de organisatie ooit
besluit weg te gaan van M365, is het PPTX-archief gedeeltelijk
gestrand.

### 3.7 Geen cross-time aggregatie van inzichten
Een kwartaal-opmerking over "omzet daalde" kan volgend kwartaal niet
programmatisch worden opgehaald; iemand moet het vorige deck zoeken
en lezen. dwhr's database-opmerkingen kunnen worden geaggregeerd over
jaren.

### 3.8 Power BI-afhankelijkheid
Erft alle bugginess van Power BI (`.pbix`-corruptie, refresh-failures,
VertiPaq-quirks) plus de commerciële licentiekosten ($14-$24/user/mnd
of capaciteit). En als het andere team Power BI Premium of Fabric
nodig heeft voor ingebedde live-tegels, schaalt de prijs snel.

### 3.9 Beperkte custom-logic
Measures zijn DAX, geen willekeurige R/Python. Hooks bestaan niet.
Je kunt geen measure bouwen die een externe statistisch model
aanroept of een complexe Markov-keten doorrekent. Voor zorginstellingen
die actuariële berekeningen of patiënt-niveau-statistiek in
KPI-definities willen verwerken is dit een echte beperking.

### 3.10 Auditeerbaarheid voor regulatoren
Veel Nederlandse zorginstellingen, gemeenten, ministeries hebben
expliciete audit-eisen die gestructureerde data vereisen. "Toon mij
alle commentaar door manager X op KPI Y in periode Z" is in dwhr één
SQL-query; in het PPTX-pad is het "open elke quartaal-deck en zoek
handmatig". Dit kan het verschil maken bij een toezichts-audit.

---

## 4. Vergelijking dwhr-loop vs PPTX-loop

| Eigenschap | dwhr-loop | PPTX-loop |
|---|---|---|
| Opmerkingen-opslag | DB-rijen (gestructureerd, queryable) | Bestanden (vrije tekst, niet queryable) |
| Visuele vrijheid voor commentaar | Beperkt (template-gedreven PDF) | Hoog (vrij tekenen, layout aanpassen) |
| Workflow-structuur (actiehouder, deadline) | Programmeerbaar in dwhr-consumer | Geen — vrije tekst |
| Versionering | Eén DB-bron van waarheid | SharePoint-versiehistorie + bestandskopieën |
| Audit-trail voor regulatoren | Sterk (SQL-queryable) | Zwak (bestand-archief) |
| Drill-down vanuit artefact | Ja, levend dashboard | Nee, statische snapshot (live-tegels gedeeltelijk) |
| Mensgedreven werk per cyclus | Laag (geautomatiseerd via rmarkdown) | Hoog (export → commentaar → distribueren) |
| Cross-time analyse van commentaar | Mogelijk (queries over jaren) | Niet mogelijk (handmatig zoeken) |
| Custom business-logic | Volledig programmeerbaar (R-functies) | DAX-beperkt |
| Vendor-lock-in | Geen (R, OSS) | M365 |
| Vertrouwdheid voor managers | Vereist dashboard-training | PowerPoint kennen ze al |
| Engineering-overhead om te implementeren | Hoog (dwhr-consumer-code) | Laag (gebruik kant-en-klare Microsoft-features) |
| Licentiekosten | Geen (na Highcharts-vervanging) | Power BI Pro/Premium per user |
| Past bij kwartaal-board-cyclus | Matig (continu monitoring-model) | Goed (deck-gedreven cyclus) |

---

## 5. Wanneer welk pad wint

### Het PPTX-pad wint wanneer:
- Het board-cyclus is **kwartalig of zeldzamer**, niet continu.
- De organisatie is volledig op M365 — Excel/PowerPoint zijn de
  primaire werktools.
- Commentaar hoeft achteraf **niet** programmatisch te worden bevraagd.
- De auditor accepteert bestand-archieven als governance-record.
- Er is **geen** hard regelgevende eis voor gestructureerd
  audit-bewijs.
- KPI-definities zijn relatief simpel (DAX volstaat).
- Engineering-capaciteit voor maatwerk is schaars.
- "Het deck" is het culturele eindartefact van vergaderingen.

### Het dwhr-pad (huidig) wint wanneer:
- Er is **continu monitoring** nodig naast de kwartaal-cyclus.
- Audits/compliance vereisen gestructureerd opmerkingen-bewijs.
- De organisatie wil opmerkingen kunnen aggregeren over tijd ("welke
  KPI's krijgen consistent commentaar?").
- KPI-definities vereisen maatwerk-business-logic (R-functies,
  externe modellen, complexe statistiek).
- Cross-team consistentie van commentaar-formaat is gewenst.
- De organisatie wil minder Microsoft-lock-in.
- Engineering-capaciteit (R-developers) is beschikbaar.

### Hybride pad (waarschijnlijk de praktische werkelijkheid)

De twee paden hoeven elkaar niet uit te sluiten. Een werkbare
combinatie:

- **dwhr** voor live KPI-dashboards + commentaar-write-back naar het
  warehouse (de werkende datalaag, continue monitoring).
- **PPTX-export** voor de formele board-cyclus (het kwartaal-artefact).
  Genereer de board-deck programmatisch uit dwhr's opmerkingen + charts.
- Beide voeden zich uit hetzelfde warehouse.
- Opmerkingen worden in dwhr vastgelegd, vervolgens in de board-PPTX
  ingesloten of getranscribeerd.

**Belangrijk**: deze hybride is technisch haalbaar omdat R native
PPTX kan genereren via het `officer`-package. Een dwhr-consumer-script
kan een board-deck samenstellen met:
- Snapshots van dashboard-charts (via `webshot2` of `htmlwidgets::saveWidget`)
- Opmerkingen-tabel uit het warehouse
- Layout via `officer::ph_with()` of een Word/PowerPoint-template

Dit geeft je: opmerkingen-als-data (dwhr) + deck-als-artefact (PPTX),
zonder Power BI als afhankelijkheid.

---

## 6. Concrete vragen om aan het andere team te stellen

Vóór een tool-keuze is het waardevoller om de volgende governance-
vragen te helderen dan om feature-checklists te vergelijken:

1. **Waar moeten manager-opmerkingen over 5 jaar leven?** In bestanden
   of in het warehouse?
2. **Wie moet opmerkingen kunnen bevragen?** Alleen de auteur en
   directe lezers, of ook latere analisten/auditeurs?
3. **Is er een formele audit-eis?** Wat verwacht de toezichthouder
   (NZa, IGJ, AP, externe accountant) als bewijs van governance-loop?
4. **Hoe vaak vindt de board-cyclus plaats?** Kwartaal vs maandelijks
   vs continu — bepaalt of het deck-model past.
5. **Wat is het culturele eindartefact?** Een live dashboard waar
   managers in zitten, of een deck dat ze in een vergadering
   doornemen?
6. **Wat is de kosten/baten voor mensgedreven werk per cyclus?** Is
   handmatig export-en-commentaar-werk acceptabel, of moet dit
   geautomatiseerd?
7. **Hoeveel KPI's, hoeveel managers, hoeveel cycli per jaar?** Schaal
   bepaalt of mensgedreven werk vol te houden is.

---

## 7. Eerlijke conclusie

De PPTX-route is een **legitieme alternatieve governance-filosofie**,
geen downgrade. Het optimaliseert voor "PowerPoint is wat het bestuur
ziet" in plaats van "het warehouse is de bron van waarheid". Kies op
basis van hoe het bestuur daadwerkelijk werkt, niet op basis van
tech-features.

**De duidelijkste case voor de PPTX-route**: midden- tot kleinere
org, M365-shop, kwartaal-board-cyclus, opmerkingen hoeven later niet
bevraagd te worden, geen harde regelgevende eis voor gestructureerd
audit-spoor.

**De duidelijkste case tegen**: zorginstelling met
regelgevende/compliance-eisen (Radboud-vorm), complex KPI-portfolio,
opmerkingen moeten terug in volgende-cyclus-analyse, meerjarige
trendrapportage nodig.

Gegeven dat dwhr ontstaan is in een Nederlands academisch ziekenhuis
(Radboud UMC) en dat veel Nederlandse zorginstellingen, gemeenten en
ministeries strikte audit-eisen hebben die gestructureerde data
bevoordelen, vermoed ik dat het PPTX-pad voor deze sector vaker tegen
audit-grenzen zal lopen. Maar dat is een organisatie-specifieke
afweging — niet iets wat ik vanaf de tech-stack kan beslissen.

**Aanbeveling voor het gesprek met het andere team**: vraag eerst de
zeven governance-vragen (sectie 6) door, voordat het over Power BI
versus dwhr versus iets-anders gaat. De *governance-filosofie* moet
de tool-keuze sturen, niet andersom.

---

## Appendix — bronnen

- Power BI for PowerPoint-add-in: <https://learn.microsoft.com/en-us/power-bi/collaborate-share/service-power-bi-powerpoint-add-in-about>
- Power BI Export to PowerPoint: <https://learn.microsoft.com/en-us/power-bi/consumer/end-user-powerpoint>
- `officer` R-package (PPTX-generatie): <https://davidgohel.github.io/officer/>
- `webshot2` voor dashboard-snapshots: <https://rstudio.github.io/webshot2/>
- Begeleidende docs:
  - [`ARCHITECTURE-FUTURES.md`](ARCHITECTURE-FUTURES.md)
  - [`BI-STACK-COMPLEMENTS.md`](BI-STACK-COMPLEMENTS.md)
  - [`PERFORMANCE-BASELINE.md`](PERFORMANCE-BASELINE.md)
