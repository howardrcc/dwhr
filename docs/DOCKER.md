# dwhr Docker / Dev Container — Spec

Status: **draft** — not yet implemented. Approve, then we generate the files in §11.

## 1. Goals & non-goals

### Goals

- **One-command demo.** A contributor or stakeholder runs a single command and lands in a working `inst/examples/<chosen-app>` Shiny app at `http://localhost:4815`.
- **Reproducible R env.** R 4.5.3 + the 40-package CRAN env from [`scripts/install-examples-15-17-deps.R`](../scripts/install-examples-15-17-deps.R), pinned in the image. No "works on my machine" friction.
- **Live editing while demo'ing.** Edits to dwhr source (`R/*.R`) and the example app (`inst/examples/<app>/server.R`, `ui.R`, `global.R`) reload without rebuilding the image.
- **Multi-arch.** `linux/arm64` (Apple Silicon) and `linux/amd64` (Linux/Windows VMs). Built locally via `docker buildx`.
- **Test-runnable.** `make check` runs `R CMD check` against dwhr inside the container so contributors can verify before opening a PR.
- **VS Code Dev Containers compatible.** Open the repo in VS Code, "Reopen in Container," done.

### Non-goals (for v1)

- ShinyProxy production deployment. The image is *demo-grade*. ShinyProxy integration (auth headers, `FROM dwhr-runtime` for prod app images) is documented as future work, not built.
- Auth. Every example calls `dwhr::authenticate(session)`; the container bypasses it via `session$userData$authenticated <- TRUE`. **Never deploy this image as-is.**
- Linux/Windows host parity. Designed and tested on macOS hosts. Should work on Linux hosts unmodified (volume mounts behave the same); Windows hosts via Docker Desktop are likely fine but unverified.
- Database-backed example `08DataFromDb` — needs a SQL Server sidecar; out of scope.
- Pushing the image to a registry (Docker Hub / ghcr.io). Local `docker build` only.
- spDataLarge (used by `17MunicipalShowcase/leaflet.R`) — not on CRAN, must be installed at runtime via `remotes::install_github`. Documented, not baked.

## 2. Architecture

```
┌────────────────────────────── HOST (your Mac) ──────────────────────────────┐
│  ~/workspace/dwhr/                                                          │
│  ├── R/                  ←─────────┐                                        │
│  ├── DESCRIPTION         ←─────────┤  bind-mount (live edits)               │
│  ├── inst/examples/15PdfShowcase/  │                                        │
│  │       └── tmp/  data/  *.R      │                                        │
│  └── …                              │                                        │
└─────────────────────────────────────┼────────────────────────────────────────┘
                                      │ -v ~/workspace/dwhr:/workspaces/dwhr
                                      │ -v ~/workspace/dwhr/inst/examples/<app>:/app
                                      ▼
┌─────────────────── CONTAINER (dwhr-runtime:0.1.0) ──────────────────────────┐
│  /                                                                           │
│  ├── /workspaces/dwhr   ← dwhr source (devtools::load_all from here)        │
│  ├── /app               ← chosen example app (shiny::runApp from here)      │
│  ├── R 4.5.3 + 40 CRAN packages (read-only, /usr/local/lib/R/site-library) │
│  ├── tinytex (LaTeX for 15PdfShowcase .Rnw)                                 │
│  ├── phantomjs (webshot HTML→PDF)                                           │
│  ├── system libs (cairo, gdal, geos, proj, udunits2, gfortran, …)           │
│  └── ENTRYPOINT: Rscript /entrypoint.R                                      │
│        → devtools::load_all("/workspaces/dwhr")                              │
│        → shiny::runApp("/app", port = 4815, host = "0.0.0.0")                │
└──────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
                   http://localhost:4815  ←  open manually in browser
```

**Single image. No app code, no dwhr source baked in.** The image is the *environment*; everything you'd want to edit is mounted from the host.

## 3. The image: `dwhr-runtime`

**Base:** `rocker/r-ver:4.5.3` (Debian Bookworm, R 4.5.3 from posit's RStudio Package Manager, multi-arch).

Why rocker over a Nix-derived image: rocker's binary R + binary R-package install via [PPM](https://packagemanager.posit.co/) is the fastest, most-debugged path for v1. Layer cache works well, contributors don't need Nix to rebuild. A Nix-based image (`dockerTools.buildImage` reusing the host's `rEnv`) is documented as future migration in §12.

**Tag:** `dwhr-runtime:0.1.0` (semver-tracked alongside dwhr's `DESCRIPTION` `Version`).

**Image contents:**

| Layer                | Contents                                                                                     |
|----------------------|---------------------------------------------------------------------------------------------|
| Base                 | `rocker/r-ver:4.5.3` — R 4.5.3, locale, base system                                         |
| System libs          | `libcairo2-dev`, `libxt-dev`, `libgdal-dev`, `libgeos-dev`, `libproj-dev`, `libudunits2-dev`, `libssl-dev`, `libxml2-dev`, `libfontconfig1-dev`, `libfreetype6-dev`, `libharfbuzz-dev`, `libfribidi-dev`, `libpng-dev`, `libtiff5-dev`, `libjpeg-dev`, `gfortran`, `pkg-config` |
| R packages           | The 40 from `scripts/install-examples-15-17-deps.R` (incl. `Cairo`, `akima`, but NOT `spDataLarge`) — installed via `pak::pkg_install()` against PPM for binary speed |
| akima                | Installed from CRAN archive via `remotes::install_version` since it's archived          |
| TinyTeX              | `tinytex::install_tinytex(force = TRUE)` during image build                                  |
| PhantomJS            | `webshot::install_phantomjs()` during image build (downloads to `/root/Library/...`)         |
| Dev tools            | `devtools`, `roxygen2`, `testthat`, `rcmdcheck`, `pak`                                       |
| Entrypoint           | `/entrypoint.R` (the load_all + runApp script)                                               |

Notes:
- `pak` is used for parallel binary installs against [Posit Public Package Manager](https://packagemanager.posit.co/cran/__linux__/bookworm/latest) — significantly faster than `install.packages()` and supports binary aarch64-linux too.
- We do NOT install dwhr into the image. The image build doesn't touch the dwhr repo at all.
- **TinyTeX (not a static `texlive-*` apt package).** Yihui Xie's R-managed TeX Live distribution auto-installs missing `.sty` files on first compile — critical because the 15PdfShowcase `.Rnw` templates need `framed`, `soulutf8`, `sparklines`, `tabu`, `threeparttablex`, `wordlike`, `needspace`, etc., and a static scheme can't grow at runtime. First PDF compile inside the image may take 30-60 s extra while TinyTeX fetches the packages it doesn't ship by default; subsequent compiles are fast. (For host-side dev with the same property, see [`docs/INSTALL.md`](INSTALL.md) §2 — same TinyTeX, installed via R rather than baked into a layer.)
- The image is ~1.4 GB compressed, ~2.5 GB extracted (rough estimate, dominated by tidyverse + sf + terra + tinytex).

**Multi-arch build (one command, both platforms):**

```sh
docker buildx create --use --name dwhr-builder
docker buildx build \
  --platform linux/arm64,linux/amd64 \
  --tag dwhr-runtime:0.1.0 \
  --tag dwhr-runtime:latest \
  --load \
  .
```

`--load` works for single-arch only; multi-arch `--push` to a registry is the standard path. For local-only multi-arch, build each arch separately or use `--output type=docker` per-arch.

## 4. Three wrappers (you asked for all three)

### 4.1 `docker-compose.yml` (recommended UX)

Picks the example via env var, mounts everything correctly, exposes port 4815.

```sh
EXAMPLE=15PdfShowcase docker compose up
# or
docker compose up                # uses default EXAMPLE=01SimpleTable
```

Compose file lives at repo root; mounts `~/workspace/dwhr` → `/workspaces/dwhr` and `inst/examples/${EXAMPLE}` → `/app`.

### 4.2 `Makefile` targets (for muscle memory)

```sh
make demo                        # = compose up with EXAMPLE=15PdfShowcase
make demo EXAMPLE=16D3Sankey
make check                       # R CMD check inside the container
make shell                       # interactive bash in the container
make build                       # docker buildx build
```

The Makefile is a thin wrapper around docker compose / docker run; no logic.

### 4.3 Raw `docker run` (documented for the curious / CI)

```sh
docker run --rm -it \
  -v ~/workspace/dwhr:/workspaces/dwhr \
  -v ~/workspace/dwhr/inst/examples/15PdfShowcase:/app \
  -p 4815:4815 \
  dwhr-runtime:0.1.0
```

Documented in `README.md` as "what compose actually runs."

## 5. VS Code Dev Containers

`.devcontainer/devcontainer.json` at repo root makes "Reopen in Container" Just Work. The dev container differs from the demo container in two ways:

1. **No `command` / `entrypoint`** — VS Code keeps the container alive and lets you run apps from the integrated terminal (`Rscript -e 'shiny::runApp("inst/examples/15PdfShowcase", port=4815, host="0.0.0.0")'`).
2. **Forwards port 4815** automatically; VS Code shows a clickable notification.
3. **Installs the R extension** (`reditorsupport.r`) and `radian` for a better R REPL.

Same image, different `command` — no second Dockerfile.

## 6. Hot reload

Two layers, both opt-out:

| Layer            | Mechanism                                  | Trigger                      | Disable                             |
|------------------|--------------------------------------------|------------------------------|-------------------------------------|
| Example app code | `options(shiny.autoreload = TRUE)`         | Change to `/app/*.R`         | `SHINY_AUTORELOAD=false` env var    |
| dwhr package code| Manual `devtools::load_all()` in R console | Change to `/workspaces/dwhr/R/*.R` | n/a — manual by design           |

Why dwhr changes are manual: `shiny.autoreload` watches the app dir, not the package source. We could write a `fs::dir_watch`-based watcher around dwhr's R/, but it's complexity for marginal benefit; in practice during a demo you don't edit dwhr internals.

Documented downsides:
- Editor autosave-on-keystroke can trigger spurious reloads. Mitigate by saving on focus-loss only.
- Each reload re-runs `global.R` (~2s for 15PdfShowcase's 11MB `.RData` load).
- In-memory state lost on reload — fine for demos.

## 7. State / persistence

| Path in container       | Mount source                              | Why                                                                                |
|-------------------------|-------------------------------------------|------------------------------------------------------------------------------------|
| `/workspaces/dwhr`      | bind: `~/workspace/dwhr`                  | Live edits to dwhr R/, DESCRIPTION, etc.                                          |
| `/app`                  | bind: `~/workspace/dwhr/inst/examples/<EXAMPLE>` | The chosen example app, including its `tmp/` and `data/`                  |
| `~/.R-userlib` (in container) | named volume `dwhr-rlib`            | Place to install spDataLarge or any other ad-hoc package without polluting image  |

**`tmp/` and `data/`** of the chosen example are part of `/app` — bind-mounted from the host repo, so changes show up in `git status`. Matches today's host-side behavior. (Confirmed in answers: A10/A11 both = mounts.)

**makeRData.R** does not auto-run (per A12: data is already prepped). Documented as a manual command if `tmp/kpiRvb.RData` is ever missing.

## 8. Auth

Bypassed for demo. The container's entrypoint sets:

```r
options(dwhr.dev.bypass_auth = TRUE)
```

dwhr's `authenticate(session)` (R/star.R) needs a tiny tweak to honor that option — the [W3 modernization spec](MODERNIZATION.md) covers it. **For now, the entrypoint wraps `authenticate` with a stub** that sets `session$userData$authenticated <- TRUE` and returns. No dwhr code changes for v1.

## 9. Test / check support

`make check` runs:

```sh
docker compose run --rm runtime \
  Rscript -e 'devtools::check("/workspaces/dwhr", error_on = "warning")'
```

The image already has `rcmdcheck`, `testthat`, and the LaTeX needed for vignette PDF builds (via tinytex). No `qpdf` for now — the package has no PDF-output vignettes yet. Add when needed.

## 10. Pinning

| Thing             | Pinned to                                    | Bump policy                                      |
|-------------------|----------------------------------------------|--------------------------------------------------|
| Base image        | `rocker/r-ver:4.5.3@sha256:<digest>`         | Manual; bump when dwhr's R version requirement changes |
| R packages        | PPM snapshot (e.g. `2026-04-01`) baked into Dockerfile | Manual; bump quarterly or before a release |
| TinyTeX           | Latest at image build time                   | Implicit — rebuilding the image refreshes        |
| PhantomJS         | webshot 0.5.5's bundled URL                  | Pin once; never bump (webshot+phantomjs are EOL — webshot2+chromote is the future, deferred) |
| Image tag         | `dwhr-runtime:0.1.0` (and `:latest`)         | Track dwhr's `DESCRIPTION` Version              |

## 11. Files this spec produces (when approved)

```
~/workspace/dwhr/
├── Dockerfile                           # multi-arch, multi-stage
├── docker-compose.yml                   # demo wrapper
├── Makefile                             # demo / check / shell / build targets
├── .dockerignore                        # excludes tmp/, .Rproj.user/, etc.
├── .devcontainer/
│   └── devcontainer.json                # VS Code Dev Container config
├── docker/
│   ├── entrypoint.R                     # load_all + runApp
│   ├── install-r-packages.R             # pak-based image-build R deps script
│   └── auth-stub.R                      # bypass authenticate() for dev
└── docs/
    └── DOCKER.md                        # this file (already exists)
```

Adds ~6 new files at the repo root + 3 in `docker/` + 1 in `.devcontainer/`. Touches `README.md` to add a "Quick start (Docker)" section. Touches **no existing R code**.

## 12. Future work (numbered for tracking)

1. **ShinyProxy base image.** Add a second image variant `dwhr-shinyproxy:<version>` that `FROM dwhr-runtime`, `COPY`s a single example into `/app`, exposes 3838, reads `SHINYPROXY_USERNAME` env var, and integrates with dwhr's auth. Build per-app images for prod.
2. **Nix-based image.** `dockerTools.buildImage` from a `flake.nix` reusing the host's `rEnv` from `~/.dotfiles/modules/darwin/r.nix`. Bit-for-bit reproducible across dev/prod. Adds a `flake.nix` at the dwhr repo root (previously declined for the Nix-darwin path; would revisit only if Docker reproducibility becomes critical).
3. **Push to ghcr.io.** Once the image is stable, push to GitHub Container Registry so contributors can `docker pull` instead of building (~5 minutes saved per first run).
4. **CI integration.** GitHub Actions workflow that builds the image on tag pushes and runs `make check` on every PR.
5. **08DataFromDb sidecar.** Add an `mcr.microsoft.com/mssql/server:2022-latest` service to compose with a `data/init.sql` seed; flip example 08 on with `EXAMPLE=08DataFromDb COMPOSE_PROFILES=db docker compose up`.
6. **webshot2 + chromote.** Replace PhantomJS (EOL) with headless Chromium for PDF rendering. Touches 15PdfShowcase's `print.R`.
7. **R version follow-along.** Today the host (Nix) is on R 4.5.2 and the container pins R 4.5.3 — close but not identical. Decide whether to pin them together (track Nix R) or let the container lag CRAN.

## 13. Open issues / known limitations

- **Nix R on host is 4.5.2, container R is 4.5.3.** Minor — patchlevel difference, probably no impact on dwhr — but if a CRAN package is installed against 4.5.3 binaries and the host's Nix 4.5.2 R tries to load it via the bind-mounted `~/.R/...` path, it'd fail. Today nothing crosses that boundary (each R has its own user lib), so OK; flag for awareness.
- **PhantomJS architecture support.** webshot's bundled PhantomJS download for `linux/arm64` may not exist; the image build may fall back to `linux/amd64` PhantomJS under qemu emulation, which works but is slow. Test during implementation; if broken, switch the LaTeX-rendering path to webshot2/chromote earlier than planned.
- **First build is slow** (~10 minutes on M-series with cold cache, ~25 minutes on amd64 emulation). Subsequent builds with layer cache are <1 minute.
- **Demo-grade auth.** Bypassing `authenticate()` is fine for `localhost:4815` demos; if anyone exposes the container's port externally, every "logged-in" user has full dashboard access. Document loudly in README.

---

**Approve to proceed:** if this matches your intent, say `go` and I'll generate every file in §11. If anything's wrong (especially §8 auth bypass approach, §10 pinning policy, or §11 file layout), call it out.
