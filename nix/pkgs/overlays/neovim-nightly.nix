let
  rev = "master";
  url = "https://github.com/nix-community/neovim-nightly-overlay/archive/${rev}.tar.gz";
  overlay = (import (builtins.fetchTarball url));
in
overlay
