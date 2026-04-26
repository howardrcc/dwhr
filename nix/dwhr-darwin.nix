# nix-darwin module fragment for dwhr's brew dependencies.
#
# Designed to be merged into an existing nix-darwin config. It only sets
# `homebrew.brews` and `homebrew.casks` (lists, which nix merges by
# concatenation) — it does NOT set `homebrew.enable` or `homebrew.onActivation`,
# so it won't fight with your existing settings.
#
# Two ways to use it:
#
#   A. Import into your nix-darwin configuration (recommended):
#
#        # darwin-configuration.nix or hosts/<host>/default.nix
#        { ... }: {
#          imports = [
#            /path/to/dwhr/nix/dwhr-darwin.nix
#          ];
#        }
#
#      Then rebuild:
#        sudo darwin-rebuild switch --flake ~/.dotfiles  # adjust to your flake
#
#   B. Copy the lists below into your existing `homebrew = { ... }` block.
#
# After the brew side is in place, install R packages (this part is NOT
# declarative — they go into ~/Library/R/<ver>/library):
#
#     Rscript /path/to/dwhr/scripts/install-r-deps.R
#
# Why these specifically?
#   - `r` (cask) — the CRAN-binary R, which tracks CRAN releases exactly.
#     Mandatory for a CRAN-target package; nixpkgs `pkgs.R` lags.
#   - `unixodbc` — headers/libs the `odbc` R package compiles against.
#     R's Makevars expects standard `/opt/homebrew/opt/unixodbc/` paths.
#
# What's deliberately NOT here:
#   - `pandoc` — install via nixpkgs (`pkgs.pandoc` in environment.systemPackages).
#     Pandoc has no special PATH requirements that force brew.
#   - `harfbuzz`, `fribidi`, `freetype`, `libpng`, `jpeg`, `libgit2` —
#     CRAN's macOS binary repo ships ARM64 prebuilt R packages for every
#     dwhr dependency, so these system libs aren't needed at install time.
#     Add them only if a specific R package forces a source build.

{ ... }:

{
  homebrew.brews = [
    "unixodbc"
  ];

  homebrew.casks = [
    "r"
  ];
}
