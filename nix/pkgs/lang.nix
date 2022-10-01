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

  # deno - i should move this extra
  bundle-deno = [ deno ];
  # nodejs
  nodejs = nodejs-18_x;
  bundle-nodejs-with-pkgs = (
  # node essentials
  [
    nodejs
    yarn
  # npm packages via nodePackages
  ] ++ (with nodePackages; [
    node2nix
    pnpm
    pyright # lsp server for python
    typescript
    typescript-language-server
  # extra packages via node2nix use nodejs-14_x until issue below is resolved
  # https://github.com/svanderburg/node2nix/issues/236
  ]) ++ (import ./node2nix {pkgs=pkgs; nodejs=nodejs;}));

  for-lua = [
    sumneko-lua-language-server # Lua Language Server coded by Lua
  ];
in
  [ python-with-pkgs ] ++
  bundle-nodejs-with-pkgs ++
  for-lua ++
  bundle-deno
