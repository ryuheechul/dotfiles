{ pkgs }:

let
  python = import ./python.nix {pkgs=pkgs;};
  javascript = import ./javascript.nix {pkgs=pkgs;};
  lua = import ./lua.nix {pkgs=pkgs;};
  nix = import ./nix.nix {pkgs=pkgs;};
in
    python
    ++
    javascript
    ++
    lua
    ++
    nix
