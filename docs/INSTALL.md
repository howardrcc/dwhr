# Installing dwhr + example apps

This guide covers two macOS install paths:

- **Classic (Homebrew + CRAN binary R)** — minimal setup using brew for system libraries and `install.packages()` for R deps.
- **Nix (declarative)** — reproducible R env wired into a `nix-darwin` config; the version this repo is currently used with day-to-day.

Linux and Windows install scripts are deferred — see [`scripts/install-r-deps.R`](../scripts/install-r-deps.R) and [`scripts/install-examples-15-17-deps.R`](../scripts/install-examples-15-17-deps.R) for the package list to port from.

A Docker proposal is at the bottom of this file.

---

## Path 1 — macOS Classic (Homebrew + CRAN R)

Prereqs:
- macOS with Homebrew installed.
- R ≥ 4.4 from the official CRAN binary (`https://cran.r-project.org/bin/macosx/`) or via `brew install --cask r`.

### 1.1 System libraries (one-time)

The Cairo R package and a few CRAN packages with native code need `pkg-config` plus toolchain bits:

```sh
brew install pkg-config cairo gcc
```

`gcc` from Homebrew brings `gfortran`, which `akima` (and a few geospatial packages) need to compile from source if there's no aarch64 binary on CRAN.

### 1.2 R packages

Two scripts live in `scripts/`:

```sh
# dwhr's own Imports + modernization-target deps (DBI, odbc, checkmate, …)
Rscript scripts/install-r-deps.R

# Deps for inst/examples/15PdfShowcase, 16D3Sankey, 17MunicipalShowcase
Rscript scripts/install-examples-15-17-deps.R
```

The second script is idempotent and gracefully handles three packages that don't install via plain `install.packages()`:

| Package      | Why it's special                                          | What the script does                                      |
|--------------|-----------------------------------------------------------|-----------------------------------------------------------|
| `Cairo`      | Needs `pkg-config` + system Cairo (covered in 1.1).       | `install.packages("Cairo")` — succeeds once libs present. |
| `akima`      | Archived from CRAN (ACM license).                         | Pulls last release via `remotes::install_version()`.      |
| `spDataLarge`| Not on CRAN; hosted on R-universe (geocompr).             | `install.packages(... repos = "https://geocompr.r-universe.dev")`. |

### 1.3 Install dwhr itself + post-install

From the repo root:

```sh
R CMD INSTALL .
# or, from within R:
R -e "devtools::install('.', quick = TRUE)"
```

15PdfShowcase additionally needs:

- **PhantomJS** for `webshot` HTML→PDF capture: `R -e 'webshot::install_phantomjs()'`
- **LaTeX** for the `.Rnw` Sweave templates: `R -e 'tinytex::install_tinytex()'` (or any TeX Live).

### 1.4 Run an example

```sh
cd inst/examples/15PdfShowcase
Rscript -e 'shiny::runApp(".", port = 4815, launch.browser = FALSE)'
# → http://127.0.0.1:4815
```

---

## Path 2 — Nix (nix-darwin module)

This is the reproducible path. The "definitive version" of the dwhr R env is a `nix-darwin` module that pins `R` plus every CRAN dep used by dwhr and the example apps.

Prereqs:
- A `nix-darwin` setup (e.g. via the [Determinate Nix installer](https://determinate.systems/posts/determinate-nix-installer/)).
- A flake-based dotfiles repo similar to [`~/.dotfiles`](https://github.com/howardchingchung/dotfiles) (this guide assumes that layout).

### 2.1 Add the R module

Drop a new module at `~/.dotfiles/modules/darwin/r.nix`:

```nix
{ pkgs, ... }:

let
  rPackages = with pkgs.rPackages; [
    # dwhr DESCRIPTION Imports
    shiny shinyjs shinyjqui data_table digest RODBC scales
    DT highcharter rlist checkmate sparkline DBI odbc

    # Shared across examples
    magrittr

    # 15PdfShowcase
    knitr kableExtra stlplus future shinytest webshot
    htmlwidgets zoo

    # 16D3Sankey
    networkD3 shinycssloaders

    # 17MunicipalShowcase
    ggplot2 tidyverse Cairo leaflet terra sf tmaptools
    cbsodataR readxl gapminder gganimate widgetframe dplyr
    akima                  # archived from CRAN; nixpkgs still ships it

    # Dev tooling
    devtools roxygen2 testthat rcmdcheck remotes
  ];

  rEnv = pkgs.rWrapper.override {
    packages = rPackages;
  };
in
{
  environment.systemPackages = [
    rEnv
    pkgs.gnumake pkgs.pkg-config pkgs.gcc
    pkgs.texlive.combined.scheme-small   # LaTeX for 15PdfShowcase .Rnw
  ];
}
```

Note: the nixpkgs attribute name for `data.table` is `data_table` (Nix replaces `.` with `_`). All other names match CRAN.

### 2.2 Wire it into the host

In `~/.dotfiles/hosts/Mac/default.nix`, add the new module to `imports`:

```nix
imports = [
  ../../modules/darwin/system.nix
  ../../modules/darwin/packages.nix
  ../../modules/darwin/homebrew.nix
  ../../modules/darwin/fonts.nix
  ../../modules/darwin/r.nix         # ← added
  inputs.home-manager.darwinModules.home-manager
];
```

### 2.3 Apply

Track the new file in git so Nix sees it (flakes ignore untracked files), then rebuild:

```sh
git -C ~/.dotfiles add modules/darwin/r.nix hosts/Mac/default.nix
darwin-rebuild switch --flake ~/.dotfiles
```

After the switch, `which R` should point at a `/nix/store/...` path. The CRAN binary R at `/usr/local/bin/R`, if you have one, is shadowed but not removed — uninstall it once you're confident the Nix env is working.

### 2.4 Install dwhr itself + spDataLarge

dwhr is not in nixpkgs and is installed from local source into the user library:

```sh
cd ~/workspace/dwhr
R CMD INSTALL .
```

`spDataLarge` (used only by `17MunicipalShowcase/leaflet.R`) is not in CRAN nor nixpkgs:

```sh
R -e 'remotes::install_github("Nowosad/spDataLarge")'
```

### 2.5 Run an example

Same as Path 1.4:

```sh
cd ~/workspace/dwhr/inst/examples/15PdfShowcase
Rscript -e 'shiny::runApp(".", port = 4815, launch.browser = FALSE)'
```

### 2.6 Updating R or any R package

```sh
cd ~/.dotfiles
nix flake update
darwin-rebuild switch --flake .
```

### Trade-offs vs. the classic path

| Aspect                            | Classic                       | Nix                                |
|-----------------------------------|-------------------------------|------------------------------------|
| Reproducibility across machines   | Implicit                      | Pinned via `flake.lock`            |
| First install time                | ~10 min                       | ~30 min (heavy R packages prebuilt)|
| Cairo / akima compile dance       | Required                      | Handled by nixpkgs builders        |
| Bumping versions                  | `update.packages()` per-host  | One `flake update` for whole env   |
| Native CRAN binaries              | Used directly                 | Rebuilt against Nix-managed libs   |
| spDataLarge                       | One install_packages line     | One install_github line            |

---

## Future: Linux / Windows / Docker

### Linux & Windows

Deferred. The R package list in [`scripts/install-examples-15-17-deps.R`](../scripts/install-examples-15-17-deps.R) is portable; only the system-library bootstrap (Path 1.1) needs a per-OS variant:

- **Debian/Ubuntu**: `apt-get install pkg-config libcairo2-dev libxt-dev libgdal-dev libudunits2-dev gfortran` covers Cairo + the geospatial stack used by 17MunicipalShowcase.
- **Windows**: CRAN binary R ships with `Rtools`; most R packages on Windows install as binaries from CRAN, no system libs needed for Cairo (binary build links against the bundled Cairo). The geospatial stack (sf, terra) likewise has Windows binaries on CRAN.

When we get to it, mirror the classic-path scripts under `scripts/` with `install-system-deps-debian.sh` and `install-system-deps-windows.ps1` (or just an `Rtools`-version note).

### Docker

A Dockerfile would let CI/CD and remote demos run an example app without touching the host. Two reasonable shapes, in increasing reproducibility:

1. **`rocker/r-ver:4.5.3` base.** Install system libs with `apt-get`, R deps with `Rscript scripts/install-examples-15-17-deps.R`, then `COPY . /pkg && R CMD INSTALL /pkg`. Smallest diff from the classic Linux path; image is ~1.5 GB.
2. **`nixpkgs#dockerTools.buildImage` from `flake.nix`.** Reuses the same `rEnv` derivation as the nix-darwin module — guarantees the container's R env is bit-for-bit identical to the dev machine's. Image is ~1 GB after `dockerTools` strip; build is fully sandboxed and reproducible. Cost: a `flake.nix` at the dwhr repo root (the user previously declined this — would revisit only if Docker becomes a goal).

A minimal `docker-compose.yml` could expose port 3838 for any chosen example app, with `tmp/` and `data/` mounted as volumes so makeRData.R artifacts persist across container restarts.

When we want to actually ship this, the open questions are: (a) Linux base or Nix base? (b) which examples to bundle by default — all 17, or just the demo ones? (c) auth — every example calls `authenticate(session)`; that gate currently no-ops in dev but a Docker image headed for a shared deploy needs a real story there.
