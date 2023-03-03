{ pkgs }:

with pkgs;
[
  sumneko-lua-language-server # Lua Language Server coded by Lua
  luajitPackages.fennel # A lisp that compiles to Lua
]
