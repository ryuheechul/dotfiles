# this file is also for typescript

{ pkgs }:

with pkgs;
let
  bundle-deno = [ deno ];
  nodejs = nodejs_24;
  bundle-nodejs-with-pkgs = (
    # node essentials
    [
      # Event-driven I/O framework for the V8 JavaScript engine
      nodejs
      # Fast, reliable, and secure dependency management for javascript
      yarn
      # npm packages via nodePackages
    ]
    ++ (with nodePackages; [
      # Fast, disk space efficient package manager
      pnpm
      # Work with npm/yarn packages locally like a boss.
      yalc
      # TypeScript is a language for application scale JavaScript development
      typescript
      # Language Server Protocol (LSP) implementation for TypeScript using tsserver
      typescript-language-server
    ])
  );
in
bundle-nodejs-with-pkgs ++ bundle-deno
