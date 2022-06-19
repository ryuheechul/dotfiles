{ pkgs }:

with pkgs;
let
  # python
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

  # deno
  bundle-deno = [ deno ];
  # nodejs
  bundle-nodejs-with-pkgs = (let
    nodejs = nodejs-14_x;
  in
  # node essentials
  [
    nodejs
    yarn
  # with nodePackages
  ] ++ (with nodePackages; [
    node2nix
    pnpm
    pyright # lsp server for python
    typescript
    typescript-language-server
]) ++ (import ./node2nix {pkgs=pkgs; nodejs=nodejs;}));

  for-lua = [
    sumneko-lua-language-server # Lua Language Server coded by Lua
  ];
in
  [ python-with-pkgs ] ++
  bundle-nodejs-with-pkgs ++
  for-lua ++
  bundle-deno
