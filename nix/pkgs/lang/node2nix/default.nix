{ pkgs, nodejs }:

let
  args-for-node = { pkgs = pkgs; nodejs = nodejs; };
  packages = (pkgs.callPackage ./packages args-for-node);
  # to avoid Galooshi/vim-import-js plugin to hang while being installed
  import-js = (
    let
      pure-import-js = packages.import-js;
    in
    (pure-import-js.override (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [
        pkgs.nodePackages.node-pre-gyp
      ];
    }))
  );
in
[
  packages.neovim
  import-js
]
