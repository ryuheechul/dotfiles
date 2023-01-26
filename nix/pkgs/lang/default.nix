{ pkgs }:

let
  go = import ./go.nix { pkgs = pkgs; };
  lua = import ./lua.nix { pkgs = pkgs; };
  nix = import ./nix.nix { pkgs = pkgs; };
  nim = import ./nim.nix { pkgs = pkgs; };
  sql = import ./sql.nix { pkgs = pkgs; };
  rust = import ./rust.nix { pkgs = pkgs; };
  wasm = import ./wasm.nix { pkgs = pkgs; };
  erlang = import ./erlang.nix { pkgs = pkgs; };
  python = import ./python.nix { pkgs = pkgs; };
  support = import ./support.nix { pkgs = pkgs; };
  checkEnv = import ../../utils/checkEnv.nix;
  javascript = import ./javascript.nix { pkgs = pkgs; };
in
with pkgs;
[ ]
++ nix
++ lua
++ python
++ support # this not for a language itself but to support the language related things
++ javascript
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_GO") go
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_NIM") nim
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_SQL") sql
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_RUST") rust
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_WASM") wasm
++ lib.optionals (checkEnv "MY_NIX_EXTRA_LANG_ERLANG") erlang
