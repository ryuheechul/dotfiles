# LSP servers are opt-in via MY_NIX_EXTRA_LSP: nvim/mason installs them on
# demand and emacs is configured to reuse those (via PATH), so a minimal build
# skips them. This centralizes the gate so each lang file needs just one import
# plus one wrap, e.g.:
#
#   ifLsp = import ./lsp.nix { inherit pkgs; };
#   ...
#   ++ ifLsp [ gopls ]
#
# (returns `lib.optionals <enabled>`, i.e. a `list -> list` function.)
{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
in
pkgs.lib.optionals (checkEnv "MY_NIX_EXTRA_LSP")
