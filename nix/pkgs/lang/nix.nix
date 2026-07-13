{ pkgs }:

let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
with pkgs;
[
  # nil # Yet another language server for Nix
  # nixpkgs-fmt # Nix code formatter for nixpkgs - deprecated and now there is an official formatter below
  nixfmt # Official formatter for Nix code
  niv # Easy dependency management for Nix projects
  comma # Runs programs without installing them
]
# nixd pulls a large LLVM closure; opt-in like the other LSP servers
++ ifLsp [
  nixd # Feature-rich Nix language server interoperating with C++ nix
]

# regarding niv:
# - https://lucperkins.dev/blog/nix-channel/
# - https://www.youtube.com/watch?v=atmoYyBAhF4
