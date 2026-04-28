# 15PdfShowcase — Game-of-Thrones Demo Data

Status: **draft** — not yet implemented. Approve, then we generate everything in §10.

## 1. Goals

- **Strip identifying real-world data** from `inst/examples/15PdfShowcase/` so the example can ship in screenshots, video, and a public GitHub repo without leaking anything Radboudumc-specific.
- **Replace it with Game-of-Thrones theming** — recognizable, clearly fictional, lower risk of "this looks like real hospital data" misreads.
- **Only touch dimension data + UI strings.** Keep all numeric facts intact so the dashboard's analytical surface (trends, rollups, sparklines, conditional formatting) still shows realistic structure.
- **Reproducible pipeline.** Anonymizer script reads `data/*.txt`, applies a deterministic mapping, writes new `data/*.txt`. `makeRData.R` regenerates `tmp/kpiRvb.RData` from those.

## 2. Non-goals

- Translating Dutch UI strings (`Geldstroom`, `Afdeling`, `Periode`, .Rnw PDF headers like `Maandrapportage`, `Samenvatting`). They're concept labels, not branding — kept as-is. Translation deferred per A14.
- Anonymizing the `period` dimension. Real dates (months/years) aren't identifying.
- Anonymizing `RVB` / "Raad van Bestuur" abbreviations *unless* they happen to appear in dim labels (in which case the kostenplaats mapping handles them). Internal R variable names stay Dutch.
- Recovering originals after the rewrite. **Path 1a means destructive: original `data/ds_d_*.txt` files are gone after the script runs.** §11 covers backup/recovery before we pull the trigger.

## 3. Pipeline

```
data/ds_d_kostenplaats.txt   ┐
data/ds_d_kpi.txt            ├──► scripts/anonymize-15-pdfshowcase.R ──► (overwrites same files)
data/ds_bestuurder.txt       │                ↓
                             ┘   (deterministic; seeded RNG; reproducible)
                                              ↓
                                              ↓ then user runs:
                                              ↓
                                  inst/examples/15PdfShowcase/makeRData.R
                                              ↓
                                  inst/examples/15PdfShowcase/tmp/kpiRvb.RData
                                              ↓
                                  shiny::runApp(...) shows GoT-themed app
```

The **facts files** (`kpiRvb.txt`, `ds_f_kpi_opname.txt`, `ds_f_kpi_patient.txt`) are not read or written by the anonymizer — they reference dimensions only by `kpiId` / `kostenplaatsId`, which are preserved as join keys. Numeric values pass through `makeRData.R` unchanged.

The `period` dimension is also not touched.

## 4. Per-dimension mapping

### 4.1 `ds_d_kostenplaats.txt` — 74 rows, 3-level hierarchy

Current shape:

| col            | example                                    | distinct |
|----------------|--------------------------------------------|----------|
| kostenplaatsId | `118182`                                   | 74       |
| level1Label    | `001 UMC - Radboudumc`                     | 1        |
| level2Label    | `Stafdienst Financiën`                     | ~20      |
| level2Code     | `658000`                                   | ~20      |
| level3Label    | `Afdeling Cardio-Thoracale Chirurgie`      | 74       |
| level3Code     | numeric                                    | 74       |

GoT mapping (per A6 hierarchical):

| Level   | Old                       | New                                                                                                          |
|---------|---------------------------|--------------------------------------------------------------------------------------------------------------|
| level1  | `001 UMC - Radboudumc`    | `The Seven Kingdoms`                                                                                         |
| level2  | ~20 distinct *Stafdiensten / Centra / Instituten* | 20 great + notable houses: Stark, Lannister, Targaryen, Baratheon, Tyrell, Greyjoy, Tully, Arryn, Martell, Bolton, Frey, Tarly, Mormont, Reed, Karstark, Hightower, Manderly, Tarth, Dayne, Selmy |
| level3  | 74 distinct dept names    | 74 castles / holdfasts / persons (Winterfell, Casterly Rock, Dragonstone, King's Landing, Highgarden, Pyke, Riverrun, the Eyrie, Sunspear, the Twins, Storm's End, Harrenhal, Pentos, Braavos, Old Valyria, the Wall, Castle Black, Eastwatch, Skagos, Bear Island, …) |

Codes (`level2Code` / `level3Code`) are **preserved**. Only labels change.

The 1:N mapping from level2 → level3 is preserved: i.e. all rows that had the same `level2Label` in the original (say "Stafdienst Financiën") get mapped to the same new `level2Label` (say "House Lannister"), and their level3 entries become Lannister-aligned (Casterly Rock, Lannisport, …). This keeps the drill-down hierarchy coherent.

### 4.2 `ds_d_kpi.txt` — 10,401 rows / 5,487 unique IDs

Per A7 strategy (a) — **hierarchical, manual top + synthetic leaves**. The 30-column file has ~5 group/category columns at the top of the hierarchy and many leaf-label columns.

Categorize columns:

| Column type                              | Examples                                                | Strategy |
|------------------------------------------|---------------------------------------------------------|----------|
| **Group labels** (col 4: `tellerLabel`, etc.) | "Productieplan - ziektebeeld", "Personele ontwikkeling" | Manual map: ~15 distinct → ~15 GoT-event categories |
| **Sub-group labels** (col 6: `noemerLabel`)  | "Oog en adnexen", "Operatieve producten"                | Manual map per group: ~50 distinct → ~50 GoT chapters/scenes |
| **Leaf labels** (cols 11, 22, 23, …)     | "Oper aorta ascendens / aortaboog \| Minder complex \| …" | Synthetic: `[verb] of [house] at [place]` from a vocab list, deterministic per `kpiId` |
| **URL** (col 28)                         | `https://radboudumc.sharepoint.com/sites/xwiki/...`     | Replace with `https://example.com/got-kpi/{kpiId}` |
| **Codes** (cols 1, 3, 5, 7, …)           | `1001`, `20001|kpi`, `30003|kpi`                        | Preserved (join keys) |
| **Numeric / type / format columns**      | `integer`, `LH`, `3`, `4`, `3660`                       | Preserved |

The 15 manually-mapped top-level groups will be a hand-written R named-vector in the anonymizer script. Concrete proposal — final list to be locked in during implementation:

```r
top_level_map <- c(
  "Productie"                         = "Battles",
  "Productieplan - ziektebeeld"       = "Battle Plans by Region",
  "Productieplan - proces"            = "Battle Plans by Tactic",
  "Bedrijfsopbrengsten (x 1000)"      = "Iron Throne Tribute (x 1000)",
  "Bedrijfslasten (x 1000)"           = "Crown Expenditures (x 1000)",
  "Personele ontwikkeling"            = "Bannerman Levies",
  "Financien"                         = "Iron Bank Ledger",
  "Operatieve producten"              = "Siege Engines",
  "Patientverificatie"                = "Hostage Verification",
  "Medicatie"                         = "Maester's Brews",
  "HIP (handdesinfectie)"             = "Septon's Cleansings",
  "Effectieve communicatie (RSVP)"    = "Raven Dispatches",
  "Externe overdracht"                = "Diplomatic Envoys",
  "Pijn"                              = "Wounded by Battle",
  "Vallen"                            = "Defections to the Wall"
  # … plus a "?" → "Mysteries of the Realm" catch-all for anything missed
)
```

Synthetic leaf-label generation (deterministic per `kpiId`):

```r
generate_leaf_label <- function(kpi_id) {
  set.seed(digest::digest(kpi_id, "xxhash32", serialize = FALSE))
  paste(
    sample(c("Siege", "Pact", "Battle", "Pact", "Treaty", "Assault", "Defense"), 1),
    "of",
    sample(c("Winterfell", "Casterly Rock", "Dragonstone", "the Twins", ...), 1),
    "at",
    sample(c("Dawn", "Dusk", "Midnight", "the Hour of the Wolf", ...), 1)
  )
}
```

`xxhash32(kpi_id)` makes the seed stable across machines and R versions.

### 4.3 `ds_bestuurder.txt` — 73 rows, ~4 unique board members

Columns: `rvbLid;afdeling`. Sample shows two unique names (`Berkestijn`, `Lahuis`). Probably 3–4 in total. Per A9, replace with Small Council positions:

| Old name (real)           | New (council title)              |
|---------------------------|----------------------------------|
| Berkestijn                | Hand of the King                 |
| Lahuis                    | Master of Coin                   |
| (3rd unique)              | Master of Whisperers             |
| (4th unique)              | Master of Ships                  |

The `afdeling` column references kostenplaats labels (e.g. `Afdeling Cardio-Thoracale Chirurgie`). Anonymizer reads the kostenplaats mapping built in §4.1 and applies the same level3 substitutions here, so joins between bestuurder and kostenplaats stay consistent.

### 4.4 Geldstroom (in-code, not a text file)

`makeRData.R` defines this inline:
```r
gs <- data.frame(
  gsCode = c('E','O','D'),
  level1Code = c('E','O','D'),
  level1Label = c('Eerste Geldstroom', 'Overige Geldstroom', 'Onbekend'),
  ...
)
```

Per A10:

| gsCode | Old                  | New              |
|--------|----------------------|------------------|
| E      | Eerste Geldstroom    | Crown            |
| O      | Overige Geldstroom   | Bannermen        |
| D      | Onbekend             | Smugglers        |

Edited in `makeRData.R` directly, since the data is embedded in code.

## 5. UI / visual changes

### 5.1 Logo (per A11 — direwolf head)

A new SVG at `inst/examples/15PdfShowcase/www/got-logo.svg` (~30 lines, single grey path on transparent background, ~64px tall, matching the slot currently filled by `bia.png`).

`ui.R` reference updated to point at the new SVG.

`bia.png` (43k, real-world logo) — **deleted**.

### 5.2 App title (per A12)

`ui.R` page title: `Sq 001 Indicatoren Radboudumc` → `Westeros Realm Indicators`.

### 5.3 ui.R Radboudumc strings (per A13)

Grep'd: only `ui.R` contains the literal `Radboudumc`. Anonymizer's UI step does a string-replace pass over `ui.R` for any remaining mentions (header, footer, image alt-text). Plain-text replace, line-precise to avoid touching unrelated literals.

### 5.4 .Rnw PDF templates (per A14)

**Not touched in this phase.** `samenvatting.Rnw` / `meetplan.Rnw` / `maandrapportage.Rnw` are LaTeX templates with embedded R chunks; risk of breaking them with a blind text-replace. The PDFs they generate have Dutch chrome ("Maandrapportage", "Samenvatting", "RVB") that's harmless for showcase use *as long as the PDF demo isn't the central focus*. If we later want clean PDF screenshots, separate phase: walk each .Rnw and replace strings carefully with surrounding context.

## 6. tmp/ wipe (per A3)

Anonymizer deletes / clears (these are real user-generated artifacts and join-key-meaningless after rewrite):

```
inst/examples/15PdfShowcase/tmp/z*Comments
inst/examples/15PdfShowcase/tmp/z*Prev
inst/examples/15PdfShowcase/tmp/z*Prevbak
inst/examples/15PdfShowcase/tmp/score
inst/examples/15PdfShowcase/tmp/cmnts.tgz
inst/examples/15PdfShowcase/tmp/comments.csv
inst/examples/15PdfShowcase/tmp/allComments
inst/examples/15PdfShowcase/tmp/locks
inst/examples/15PdfShowcase/tmp/kpiRvbCache.rds
inst/examples/15PdfShowcase/tmp/kpiRvb.RData       # regenerated by makeRData.R
inst/examples/15PdfShowcase/tmp/kpiRvb_*.rds       # regenerated
```

## 7. Determinism

`set.seed(0xC0FFEE)` once at the top of the anonymizer; every subsequent randomized choice (level3 picks from castle pool, leaf-label vocab samples) is reproducible. Re-running the script after a label-vocab tweak produces a stable diff.

## 8. Vocab lists

Bundled inline in the script as named character vectors:

- `houses` — 20 noble houses
- `castles_holdfasts` — ~80 castle / holdfast / location names (>74 needed for level3 + bestuurder afdeling overlap)
- `events_top` — 15 top-level event-category labels
- `events_sub` — ~50 sub-category labels (organized by parent)
- `verbs`, `targets`, `times` — for synthetic leaf-label generation

Drawn from books-only sources (no show-only canon), to keep us safely on the published-fiction side.

## 9. Verifying the rewrite

Anonymizer ends by printing a small summary:

```
[anonymize-15] kostenplaats: 74 rows, level1=1, level2=20, level3=74
[anonymize-15] kpi:          10401 rows, top groups=15, sub groups=50, leaves=10401
[anonymize-15] bestuurder:   73 rows, ~4 council positions
[anonymize-15] tmp/ wiped (12 files removed)
[anonymize-15] ui.R: 4 Radboudumc references replaced
[anonymize-15] bia.png deleted; got-logo.svg written
[anonymize-15] geldstroom in makeRData.R: 1 block replaced
```

Then the user runs `inst/examples/15PdfShowcase/makeRData.R` and `shiny::runApp(...)` for a sanity check.

## 10. Files this spec produces (when approved)

```
~/workspace/dwhr/
├── docs/
│   └── DEMO-DATA.md                                  # this file
├── scripts/
│   └── anonymize-15-pdfshowcase.R                    # the rewriter (single script, ~250 lines)
└── inst/examples/15PdfShowcase/
    ├── data/                                         # OVERWRITTEN
    │   ├── ds_d_kostenplaats.txt                     # GoT'd
    │   ├── ds_d_kpi.txt                              # GoT'd
    │   └── ds_bestuurder.txt                         # GoT'd
    │   └── (period unchanged; facts unchanged)
    ├── www/
    │   ├── got-logo.svg                              # NEW
    │   └── (bia.png DELETED)
    ├── ui.R                                          # MODIFIED (4-ish string replaces)
    ├── makeRData.R                                   # MODIFIED (geldstroom labels)
    └── tmp/                                          # WIPED
        └── (kpiRvb.RData regenerated by makeRData.R run)
```

11 files modified or written; 12+ files deleted (tmp/ + bia.png).

## 11. Backup before pulling the trigger

Per 1a, originals are gone after script runs. Before approving, confirm one of these:

- **(a) Originals are pullable from prod.** A re-run of whatever ETL produced the `data/*.txt` files in the first place. Document the path in `docs/DEMO-DATA.md` §11 so a future you knows where to get them. **`[recommended]`**
- **(b) Tag the current commit.** `git tag pre-anonymize-15` before the rewrite; `git checkout pre-anonymize-15 -- inst/examples/15PdfShowcase/data` to restore.
- **(c) Keep originals locally outside repo.** `cp -r inst/examples/15PdfShowcase/data ~/dwhr-real-data-backup/` before running.

Pick one (or stack them) and tell me — I'll bake the chosen recovery story into the spec before implementation.

## 12. Open work after this spec lands

- **.Rnw cleanup** for clean PDF demo screenshots (Dutch chrome left in for v1).
- **Periode dim** anonymization if you ever want fictional dates ("AC 300 - Moon of the Wolf" etc.) — currently real dates pass through.
- **fact-data review**: spot-check that no `kpiRvb.txt` row's `kostenplaatsId` orphan-joins to a deleted dim row (should be impossible since we preserve IDs, but worth a smoke test).
- **Score / comments seed data**: if you later want the comments tab populated, generate fake GoT-themed entries in `tmp/z*Comments` from a council-member voice.
- **Move the demo into a public branch / dedicated repo** if you want the original-data history sealed off from public clones (this spec's path 1a leaves the rewrite in `main`'s history; `git filter-repo` is the heavy hammer for actual scrubbing).

---

**Approve to proceed:** confirm §11 backup choice, and say `go` — I'll generate the anonymizer script + SVG + ui.R/makeRData.R edits in one PR.
