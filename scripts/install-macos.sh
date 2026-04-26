#!/usr/bin/env bash
# Bootstrap a dwhr development environment on macOS.
#
# Idempotent: re-running is safe — every step is "install if missing".
#
# Steps:
#   1. Verify Homebrew is present.
#   2. Install R (via official CRAN cask) if missing.
#   3. Install system libraries that R packages link against
#      (ODBC, image/text libs for ggplot/highcharter, Pandoc for vignettes).
#   4. Hand off to Rscript scripts/install-r-deps.R for R-side packages.
#
# Usage (from repo root):
#   ./scripts/install-macos.sh
#
# Tested on: macOS 15 (Sequoia), Apple Silicon. Should work on Intel + macOS 13+.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MIN_R_MAJOR=4
MIN_R_MINOR=4

log()   { printf '\033[1;34m[dwhr]\033[0m %s\n' "$*"; }
warn()  { printf '\033[1;33m[dwhr]\033[0m %s\n' "$*" >&2; }
fail()  { printf '\033[1;31m[dwhr]\033[0m %s\n' "$*" >&2; exit 1; }

# 1. Homebrew --------------------------------------------------------------

if ! command -v brew >/dev/null 2>&1; then
  fail "Homebrew not found. Install it first: https://brew.sh"
fi
log "Homebrew: $(brew --version | head -1)"

# 1a. nix-darwin guard -----------------------------------------------------
#
# If the machine is managed by nix-darwin, brew is configured declaratively
# via the homebrew module. Running `brew install` imperatively here would
# install packages that the next `darwin-rebuild switch` may remove or
# conflict with. Bail out and point the user at the nix module instead.

is_nix_darwin=false
if [ -e /run/current-system/sw/bin/darwin-rebuild ] \
   || command -v darwin-rebuild >/dev/null 2>&1 \
   || [ -d /run/current-system/Library/LaunchDaemons ]; then
  is_nix_darwin=true
fi

if [ "$is_nix_darwin" = "true" ] && [ -z "${DWHR_FORCE_BREW:-}" ]; then
  warn "nix-darwin detected on this machine."
  warn ""
  warn "Don't install brew packages imperatively here — declare them in your"
  warn "nix-darwin config instead. See:"
  warn "    $REPO_ROOT/nix/dwhr-darwin.nix"
  warn "    $REPO_ROOT/docs/GETTING-STARTED.md  (\"Alternative path: nix-darwin\")"
  warn ""
  warn "Once nix-darwin has applied the brew deps, run only the R bootstrap:"
  warn "    Rscript $REPO_ROOT/scripts/install-r-deps.R"
  warn ""
  warn "If you really want to run this script anyway:"
  warn "    DWHR_FORCE_BREW=1 $0"
  exit 1
fi

# 2. R ---------------------------------------------------------------------

install_r() {
  log "Installing R via Homebrew cask (CRAN binary)..."
  brew install --cask r
}

if ! command -v R >/dev/null 2>&1; then
  install_r
else
  R_VERSION="$(R --version | head -1 | awk '{print $3}')"
  R_MAJOR="$(echo "$R_VERSION" | cut -d. -f1)"
  R_MINOR="$(echo "$R_VERSION" | cut -d. -f2)"
  if [ "$R_MAJOR" -lt "$MIN_R_MAJOR" ] || \
     { [ "$R_MAJOR" -eq "$MIN_R_MAJOR" ] && [ "$R_MINOR" -lt "$MIN_R_MINOR" ]; }; then
    warn "R $R_VERSION is older than the target ($MIN_R_MAJOR.$MIN_R_MINOR)."
    warn "Upgrade with: brew upgrade --cask r   (or reinstall: brew reinstall --cask r)"
  else
    log "R $R_VERSION already installed."
  fi
fi

command -v R >/dev/null || fail "R install appears to have failed; R not on PATH."

# 3. System libraries ------------------------------------------------------
#
# unixodbc       - required by the `odbc` R package (W3 of MODERNIZATION.md)
# pandoc         - required by rmarkdown / vignettes / R CMD check
# harfbuzz       - text shaping (ragg / textshaping, used by ggplot2 backends)
# fribidi        - bidi text (textshaping)
# freetype       - font rendering (ragg)
# libpng / jpeg  - image I/O (ragg, png)
# libgit2        - used by usethis/gert
#
# Most of these are no-ops on a Mac that already has the Xcode CLT, but
# Homebrew versions are picked up first by R's compile chain when present.

BREW_PKGS=(
  unixodbc
  pandoc
  harfbuzz
  fribidi
  freetype
  libpng
  jpeg
  libgit2
)

log "Installing system libraries via Homebrew (idempotent)..."
for pkg in "${BREW_PKGS[@]}"; do
  if brew list --formula "$pkg" >/dev/null 2>&1; then
    printf '  ✓ %s (already installed)\n' "$pkg"
  else
    printf '  → installing %s...\n' "$pkg"
    brew install "$pkg"
  fi
done

# Xcode Command Line Tools are required to build any R package from source.
if ! xcode-select -p >/dev/null 2>&1; then
  warn "Xcode Command Line Tools not found. Triggering installer..."
  xcode-select --install || true
  warn "Re-run this script once the CLT installer finishes."
  exit 1
fi

# 4. R packages ------------------------------------------------------------

log "Installing R package dependencies (this may take several minutes)..."
Rscript "$REPO_ROOT/scripts/install-r-deps.R"

log "Done. Verify with:"
log "  R -e 'devtools::load_all(\"$REPO_ROOT\"); cat(\"OK\\n\")'"
