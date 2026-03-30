# this file is also for typescript

{ pkgs }:

with pkgs;
let
  nodejs = nodejs_24;
  bundle-nodejs-with-pkgs = (
    [
      nodejs # Event-driven I/O framework for the V8 JavaScript engine
    ]
    # npm packages via nodePackages
    ++ (with nodePackages; [
      pnpm # Fast, disk space efficient package manager
      yalc # Work with npm/yarn packages locally like a boss.
      typescript # TypeScript is a language for application scale JavaScript development
      typescript-language-server # Language Server Protocol (LSP) implementation for TypeScript using tsserver
    ])
  );
  bundle-deno = [
    deno # Secure runtime for JavaScript and TypeScript
  ];
  bundle-bun = [
    bun # Incredibly fast JavaScript runtime, bundler, transpiler and package manager – all in one
  ];
in
bundle-nodejs-with-pkgs ++ bundle-deno ++ bundle-bun
