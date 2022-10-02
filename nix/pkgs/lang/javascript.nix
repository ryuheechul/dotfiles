{ pkgs }:

with pkgs;
let
  bundle-deno = [ deno ];
  nodejs = nodejs-18_x;
  bundle-nodejs-with-pkgs = (
    # node essentials
    [
      # Event-driven I/O framework for the V8 JavaScript engine
      nodejs
      # Fast, reliable, and secure dependency management for javascript
      yarn
      # npm packages via nodePackages
    ] ++ (with nodePackages; [
      # Generate Nix expressions to build NPM packages
      node2nix
      # Fast, disk space efficient package manager
      pnpm
      # Work with npm/yarn packages locally like a boss.
      yalc
      # lsp server for python
      pyright
      # TypeScript is a language for application scale JavaScript development
      typescript
      # Language Server Protocol (LSP) implementation for TypeScript using tsserver
      typescript-language-server
      # for https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#eslint
      vscode-langservers-extracted
    ]) ++ (import ./node2nix { pkgs = pkgs; nodejs = nodejs; })
  );
in
bundle-nodejs-with-pkgs ++ bundle-deno
