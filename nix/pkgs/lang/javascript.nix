{ pkgs }:

with pkgs;
let
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
    ]) ++ (import ./node2nix { pkgs = pkgs; nodejs = nodejs; })
  );
in
bundle-nodejs-with-pkgs ++ bundle-deno
