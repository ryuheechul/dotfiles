{ pkgs }:

with pkgs;
[
  # sumneko-lua-language-server # Lua Language Server coded by Lua # now relies on mason via ../../../nvim/lua/plugins/lsp.lua
  luajitPackages.fennel # A lisp that compiles to Lua
]
