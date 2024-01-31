{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
in
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
  ] ++ ifEnv "MY_NIX_EXTRA_LANG_PYTHON_JUPYTER" [
    jupyter # The Jupyter HTML notebook is a web-based notebook environment for interactive computing
  ]);
  via-npm = (
    with nodePackages; [
      # lsp server for python
      pyright
    ]
  );
  natives = [
    ruff # An extremely fast Python linter
    ruff-lsp # A Language Server Protocol implementation for Ruff
  ];
in
[ python-with-pkgs ] ++ via-npm ++ natives
