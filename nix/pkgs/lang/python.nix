{ pkgs }:

with pkgs;
let
  python-with-pkgs = python3.withPackages (python-packages: with python-packages; [
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
  ]);

in
[ python-with-pkgs ]
