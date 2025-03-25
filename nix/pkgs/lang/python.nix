{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  fix-python = import ../custom/fix-python.nix;
in
with pkgs;
let
  python-with-pkgs = python3.withPackages (
    python-packages:
    with python-packages;
    [
      # essentials
      setuptools
      pip
      pipx

      ## comment yq out to favor yq-go
      # yq # jq for yaml
      pynvim # for neovim

      # for https://spacevim.org/layers/lang/python/
      pylint
      yapf
      autoflake
      isort
      coverage
    ]
    # ++ [
    #   epc
    #   orjson
    #   sexpdata
    #   six
    #   setuptools
    #   paramiko
    #   rapidfuzz
    #   watchdog
    #   packaging
    # ] # for https://github.com/manateelazycat/lsp-bridge/wiki/NixOS
    ++ ifEnv "MY_NIX_EXTRA_LANG_PYTHON_JUPYTER" [
      jupyter # The Jupyter HTML notebook is a web-based notebook environment for interactive computing
    ]
  );
  via-npm = (
    with nodePackages;
    [
      # lsp server for python
      pyright
    ]
  );
  natives = [
    ruff # An extremely fast Python linter and a Language Server Protocol implementation for Ruff
  ];
  break-free = [
    fix-python # A simple CLI tool to use Python venv "normally" on NixOS
  ];
in
[ python-with-pkgs ] ++ via-npm ++ natives ++ break-free
