# dwhr — production deployment

This doc captures the runtime contract a fork needs to satisfy in order to ship `dwhr` past local demo into a real, multi-user environment. It's the gap between "the example apps run on `localhost:4815`" and "users hit a hosted URL with auth and a real database."

The example apps were built originally for an R + ShinyProxy stack on Ubuntu. That setup is what the env-var, credentials, and badge-suppression logic in `R/client.R` and `R/star.R` is wired for. You can run dwhr without ShinyProxy, but you need to know which knobs to set.

## TL;DR

| You're running | `SHINYPROXY` env var | `omgeving` (in `glob.env`) | dbCred.rds | TEST badge |
|---|---|---|---|---|
| Local demo (default) | unset | `'NONE'` | not needed | shown unless you skip `getReportName` |
| Local "prod-look" demo | unset | `'NONE'` | not needed | hidden by editing the example's ui.R (see §3) |
| ShinyProxy ACC | `SHINYPROXY=ACC` | `'ACC'` | required | shown |
| ShinyProxy PRD | `SHINYPROXY=PRD` | `'PRD'` | required | hidden |

## 1. The runtime contract

`initGlob()` (R/client.R, called from `dwhrInit()` in every example's `ui.R`) reads three things at session start:

1. **`SHINYPROXY` env var** — if set, must be one of `'PRD' / 'ACC' / 'LOCAL' / 'NONE'`. Anything else aborts `initGlob` with `Invalid value Environment variable SHINYPROXY`. When set, dwhr's `securityModel` becomes `'shinyproxy'`.
2. **Top-level `omgeving` in `.GlobalEnv`** — read via `isDefinedGlobal('omgeving', default)`. Set by sourcing `global.R`, which Shiny does before `initGlob()`.
3. **`R_PROJECT_HOME` env var** — path to the deploy root that contains `admin/data/`. Defaults to `getwd()/..`.

The end value of `glob.env$omgeving` is decided in this priority:

```
SHINYPROXY env var (if set)
  └─ else `omgeving` in .GlobalEnv (if set)
        └─ else 'NONE'
```

## 2. What changes when `omgeving != 'NONE'`

Two things fire (`R/client.R:181`+):

1. **Credentials are loaded** from `${R_PROJECT_HOME}/admin/data/dbCred.rds`. Missing file = hard stop.
2. **A SQL call** `exec R.dbo.get_startpunt '<omgeving>'` runs against the configured ODBC connection, populating `glob.env$portalUrl` (the "BI startpunt" link in the header).

If you set `omgeving = 'PRD'` *without* the `dbCred.rds` in place, `initGlob` aborts the session at startup. That's the "credentials file: …/admin/data/dbCred.rds not found" error.

### dbCred.rds structure

```r
saveRDS(
    list(
        PRD = list(dsn = "your-prod-dsn",  user = "user", pwd = "secret"),
        ACC = list(dsn = "your-acc-dsn",   user = "user", pwd = "secret"),
        LOCAL = list(dsn = "your-local-dsn", user = "user", pwd = "secret")
    ),
    file = "admin/data/dbCred.rds"
)
```

- The keys must include the `omgeving` value(s) you plan to deploy under, otherwise `omg %in% names(dbCred)` fails with `<omg> missing in dbCred file`.
- The DB needs a stored procedure `R.dbo.get_startpunt @omgeving` returning a single-column table with the portal URL.

### `admin/` layout

```
$R_PROJECT_HOME/
├── admin/
│   ├── data/
│   │   ├── dbCred.rds       # required when omgeving != 'NONE'
│   │   └── ds_ad_user.txt   # optional AD user lookup; absence is harmless
│   └── …
└── inst/examples/<your-app>/
    ├── server.R
    └── ui.R
```

The default `R_PROJECT_HOME` is `getwd()/..`, so if you launch from `inst/examples/15PdfShowcase/`, dwhr looks for `inst/examples/admin/data/dbCred.rds`. Adjust by setting `R_PROJECT_HOME` explicitly.

## 3. The "TEST" badge

`getReportName(title)` (R/star.R) wraps the report title with a red `TEST` badge whenever `glob.env$omgeving != 'PRD'`. The intent: a deployed ACC/LOCAL instance shouldn't be visually mistakable for production.

**Don't disable the function itself** — the badge is a deliberate safety. To suppress for *demo screenshots only*, replace the call site in your example's `ui.R`:

```diff
- '<h3 class="db-header">', getReportName(title), '</h3>'
+ '<h3 class="db-header">', title, '</h3>'
```

`inst/examples/15PdfShowcase/ui.R:65` was patched this way for the public GoT-themed demo. `inst/examples/16D3Sankey/ui.R:37` still calls `getReportName(title)` — touch only the apps you actually demo publicly.

Flip the badge back on (production) by reverting the diff above OR by deploying with `SHINYPROXY=PRD`.

## 4. Auth gate

`new.star()` requires `session$userData$authenticated == TRUE`. Every example calls `authenticate(session)` (defined in dwhr) which:

- In `securityModel = 'none'` (local dev): bypasses auth, sets `dashUser = 'dev'`, `authenticated = TRUE`.
- In `securityModel = 'shinyproxy'`: reads ShinyProxy headers (`HTTP_X_SP_USERID` etc. — verify against your ShinyProxy version), looks up the user in `admin/data/ds_ad_user.txt`, and sets `dashUser` / `dashUserName` / `dashUserFunc` accordingly.

If you deploy outside ShinyProxy (e.g. behind a different reverse proxy), this gate doesn't auto-recognize your auth headers — you'll need a small adapter in `R/client.R` or to inject `session$userData$authenticated <- TRUE` yourself before `new.star()` runs.

## 5. Common deployment shapes

### (a) ShinyProxy on Ubuntu (the original target)

1. Install R + the dwhr deps system-wide (`scripts/install-r-deps.R` + `scripts/install-examples-15-17-deps.R`).
2. `R CMD INSTALL .` for dwhr itself.
3. Drop `admin/data/dbCred.rds` and `admin/data/ds_ad_user.txt` next to the deploy root.
4. Configure ShinyProxy's `application.yml` to set `SHINYPROXY=PRD` (or `ACC`) and pass the user header through.
5. Run.

### (b) ShinyProxy in Docker (post-DOCKER.md spec)

The dev image at `dwhr-runtime:<version>` (see [`docs/DOCKER.md`](DOCKER.md)) is intended to be the *base* for ShinyProxy app images:

```dockerfile
FROM dwhr-runtime:0.1.0
COPY inst/examples/15PdfShowcase/ /app
COPY admin/ /admin
ENV R_PROJECT_HOME=/
ENV SHINYPROXY=PRD
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/app', port=3838, host='0.0.0.0')"]
```

ShinyProxy spawns one container per session against this image. Auth is handled by ShinyProxy at the proxy layer; the in-container app trusts the headers.

### (c) Reverse-proxy with separate auth (no ShinyProxy)

Doable but you'll need to:

- Skip the SHINYPROXY env var (leave dwhr in `securityModel = 'none'` mode), OR teach `authenticate()` about your proxy's auth header.
- Set `omgeving` from outside ShinyProxy. Easiest: `Sys.setenv(SHINYPROXY="PRD")` in a wrapper script before `runApp` — even though no ShinyProxy is in front, the env var is the lever for the badge + credential branch.
- Provide `dbCred.rds` if you need DB-write-back features (15PdfShowcase comments save).

### (d) Local-only "prod-look" (current 15PdfShowcase demo state)

- `SHINYPROXY` unset, `omgeving` defaults to `'NONE'`, no credentials needed.
- TEST badge suppressed by the ui.R call-site change documented in §3.
- DB write-back paths in 15PdfShowcase will fail at runtime (no `dbCred`); fine for screenshots, not for use by anyone.

## 6. What's NOT documented yet

- **Multi-tenant deployments.** dwhr's `dashUser` per-session model assumes a single tenant DB. Multi-tenant routing isn't in scope.
- **Header-based auth without ShinyProxy.** No first-class adapter; rolling your own means a small patch to `authenticate()`.
- **Secrets management.** `dbCred.rds` is a plain serialized R list — fine for a closed environment, but not encrypted at rest. If you need that, swap to env-var-driven secret injection and read the file from `tempfile()` after decryption.
- **Connection pooling.** dwhr opens one ODBC connection per session at startup. For high-traffic deployments, consider routing through `pool` or a sidecar connection manager.

## 7. Testing your deployment

Before going live:

1. `SHINYPROXY=PRD R -e 'shiny::runApp("inst/examples/15PdfShowcase")'` — should fail with the `dbCred.rds not found` error if your file is missing. That's good — it means the env-var contract is wired.
2. With `dbCred.rds` in place, the same command should start the app, hide the TEST badge (assuming you reverted the §3 patch), and show a working "BI startpunt" link populated from `R.dbo.get_startpunt`.
3. Click the comment / opmerking flow; confirm a row hits your DB.
