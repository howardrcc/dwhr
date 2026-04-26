# Getting started with dwhr (development)

This guide gets a fresh macOS machine to the point where `devtools::load_all()` succeeds and an example app runs in a browser. Aimed at new contributors and at future-me.

> First time on this repo? Also read [`CLAUDE.md`](../CLAUDE.md) for an architecture
> orientation, and [`docs/MODERNIZATION.md`](MODERNIZATION.md) for the modernization plan.

---

## Prerequisites

- macOS 13+ (tested on 15 Sequoia, Apple Silicon)
- Either Homebrew **or** nix-darwin
- ~2 GB free disk for R + dependencies
- Xcode Command Line Tools (`xcode-select --install`) — required to build any R package from source

## Quick path: Homebrew

From the repo root:

```bash
./scripts/install-macos.sh
```

The script:
1. Installs R from the official CRAN cask (or warns if your R is < 4.4).
2. Installs system libraries (`unixodbc`, `pandoc`, `harfbuzz`, `freetype`, …).
3. Calls `Rscript scripts/install-r-deps.R`, which installs every CRAN dep dwhr needs (current Imports + modernization-target packages + the archived `assertive` from CRAN Archive for baseline testing).

Re-running is safe — every step is idempotent.

## Alternative path: nix-darwin

If you manage your Mac with nix-darwin, **don't** run `install-macos.sh` directly — let nix-darwin manage the brew side declaratively, and only run the R bootstrap by hand.

1. Add the nix-darwin module to your config:

   ```nix
   # darwin-configuration.nix
   { ... }: {
     imports = [ /path/to/dwhr/nix/dwhr-darwin.nix ];
   }
   ```

   Or copy the `brews` and `casks` lists from
   [`nix/dwhr-darwin.nix`](../nix/dwhr-darwin.nix) into your existing
   `homebrew = { ... }` block.

2. Rebuild:

   ```bash
   darwin-rebuild switch --flake ~/.config/nix-darwin
   ```

3. Install the R packages (this part stays imperative — they go into the user library, not the nix store):

   ```bash
   Rscript scripts/install-r-deps.R
   ```

> Pandoc lives in nixpkgs, not the brew module. Add `pandoc` to your
> `environment.systemPackages` (e.g. in `modules/darwin/packages.nix`) —
> there's no PATH reason to bring it in via brew.

If `install-macos.sh` is run on a nix-darwin machine, it detects the marker and refuses to run — pointing here instead.

> Why brew at all on a nix-darwin machine? Because the CRAN-binary R cask
> tracks CRAN's release cadence exactly, and this package is targeting
> CRAN submission. Mixing nixpkgs R with brew-managed system libs causes
> dynamic-link drift. See the comments at the top of
> [`nix/dwhr-darwin.nix`](../nix/dwhr-darwin.nix) for the long version.

## Verify the install

```bash
# from repo root
R -e 'devtools::load_all("."); cat("OK\n")'
```

Expected output ends with `OK`. Any errors above that line are the first
findings for the baseline report (see `docs/BASELINE.md` once it exists).

## Install the package (needed to run examples)

`devtools::load_all()` only makes the package available inside *that* R
session. The example apps in `inst/examples/` start with `library(dwhr)`,
so they need the package on the system library:

```bash
R -e 'devtools::install(".", quick = TRUE)'
```

`quick = TRUE` skips doc rebuild and vignette/test runs (~30s vs. a
couple of minutes). Re-run after pulling changes; `devtools::load_all()`
is enough during an active dev session.

## Day-to-day dev commands

```r
# Reload after editing R/*.R
devtools::load_all()

# Regenerate man/*.Rd and NAMESPACE from roxygen
devtools::document()

# Local R CMD check
devtools::check()

# Run the test suite (once W5 lands)
devtools::test()

# Run a single example app
shiny::runApp("inst/examples/01SimpleTable")
```

Shell equivalents:

```bash
R CMD build .
R CMD INSTALL .
R CMD check dwhr_*.tar.gz
```

## Troubleshooting

**`R not found` after `install-macos.sh`.**
The CRAN cask installs R at `/usr/local/bin/R` (Intel) or `/opt/homebrew/bin/R` (Apple Silicon, via a wrapper) — both should be on PATH already. If not, restart your shell or check `$PATH`.

**`assertive` install fails from CRAN Archive.**
Expected on a flaky network — CRAN Archive can rate-limit. Retry, or proceed without it: the package won't load until W2 (`assertive` → `checkmate`) is done, but everything else still works. This is a known baseline finding, not a setup bug.

**`unixodbc` linked against the wrong path.**
Brew installs at `/opt/homebrew/opt/unixodbc` on Apple Silicon, `/usr/local/opt/unixodbc` on Intel. If `install.packages("odbc")` fails to find headers, point R at the right prefix:

```bash
brew --prefix unixodbc
# then add to ~/.R/Makevars:
#   ODBC_INCLUDE = /opt/homebrew/opt/unixodbc/include
#   ODBC_LIBS    = -L/opt/homebrew/opt/unixodbc/lib -lodbc
```

**Pandoc not found by R Markdown.**
Brew puts it on PATH. If R can't see it, `Sys.which("pandoc")` will return `""` — fix by restarting R from a fresh shell that has the brew env loaded.

**`devtools::check()` errors about the `License` field.**
That's W1 of MODERNIZATION.md — expected on the unmodified package. Don't fix in isolation; the whole metadata pass lands together.

## What's next

If this is your first run, the next milestone is establishing the **baseline**:

1. The install script finishes successfully.
2. `devtools::check()` runs (errors and all — we want the raw output).
3. At least one example app launches in a browser.

That output becomes `docs/BASELINE.md` — the "before" snapshot we'll compare every modernization PR against.
