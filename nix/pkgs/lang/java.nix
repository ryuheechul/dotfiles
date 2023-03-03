{ pkgs }:

# my preferred references:
# - https://github.com/cachix/devenv/blob/main/src/modules/languages/java.nix
# - https://github.com/cachix/devenv/blob/main/src/modules/languages/clojure.nix
# - https://github.com/cachix/devenv/blob/main/src/modules/languages/scala.nix
# - https://github.com/Olical/conjure/wiki/Quick-start:-Clojure-(babashka)
with pkgs;
[
  jdk # openjdk The open-source Java Development Kit
  clojure # A Lisp dialect for the JVM
  clojure-lsp # Language Server Protocol (LSP) for Clojure
  babashka # A Clojure babushka for the grey areas of Bash
]
