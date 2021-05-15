{ pkgs }:

with pkgs;
let
  # python
  python-with-pkgs = python3.withPackages (python-packages: with python-packages; [
    pynvim
    setuptools
    pip
    pipx
    yq
  ]);

  # nodejs
  bundle-nodejs-with-pkgs = (let
    nodejs = nodejs-14_x;
    args-for-node = { pkgs=pkgs; nodejs=nodejs; };

    node-global-pkg-neovim = (callPackage ./node/neovim args-for-node).neovim;

    # to avoid Galooshi/vim-import-js plugin to hang while being installed
    node-global-pkg-import-js = (let
      pure-import-js = (callPackage ./node/import-js args-for-node).import-js;
    in
    (pure-import-js.override (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [ nodePackages.node-pre-gyp ];
    })));
  in
  # node essentials
  [
    nodejs
    yarn
  # with nodePackages
  ] ++ (with nodePackages; [
    node2nix
    javascript-typescript-langserver
  # with node2nix
  ]) ++ [
    node-global-pkg-neovim
    node-global-pkg-import-js
  ]);
in
  [ python-with-pkgs ] ++ bundle-nodejs-with-pkgs
