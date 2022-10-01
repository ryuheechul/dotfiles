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
      # for https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#eslint
      vscode-langservers-extracted
    ]) ++ (import ./node2nix { pkgs = pkgs; nodejs = nodejs; })
  );
in
bundle-nodejs-with-pkgs ++ bundle-deno
