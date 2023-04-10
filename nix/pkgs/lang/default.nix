{ pkgs }:

let
  go = import ./go.nix { pkgs = pkgs; };
  lua = import ./lua.nix { pkgs = pkgs; };
  nix = import ./nix.nix { pkgs = pkgs; };
  nim = import ./nim.nix { pkgs = pkgs; };
  sql = import ./sql.nix { pkgs = pkgs; };
  rust = import ./rust.nix { pkgs = pkgs; };
  wasm = import ./wasm.nix { pkgs = pkgs; };
  java = import ./java.nix { pkgs = pkgs; };
  janet = import ./janet.nix { pkgs = pkgs; };
  erlang = import ./erlang.nix { pkgs = pkgs; };
  python = import ./python.nix { pkgs = pkgs; };
  support = import ./support.nix { pkgs = pkgs; };
  graalvm = import ./graalvm.nix { pkgs = pkgs; };
  javascript = import ./javascript.nix { pkgs = pkgs; };
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  defaults = (
    nix
    ++ lua
    ++ python
    ++ support # this is not for a language itself but to support the language related things
    ++ javascript
  );
in
defaults
++ ifEnv "MY_NIX_EXTRA_LANG_GO" go
++ ifEnv "MY_NIX_EXTRA_LANG_NIM" nim
++ ifEnv "MY_NIX_EXTRA_LANG_SQL" sql
++ ifEnv "MY_NIX_EXTRA_LANG_RUST" rust
++ ifEnv "MY_NIX_EXTRA_LANG_WASM" wasm
++ ifEnv "MY_NIX_EXTRA_LANG_JAVA" java
++ ifEnv "MY_NIX_EXTRA_LANG_JANET" janet
++ ifEnv "MY_NIX_EXTRA_LANG_ERLANG" erlang
++ ifEnv "MY_NIX_EXTRA_LANG_GRAALVM" graalvm
