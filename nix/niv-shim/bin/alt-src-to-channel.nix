src:

let
  sources = (import ../nix/sources.nix { });
in
sources.${src}.outPath
