paths:
let
  nix-path = import ./nix-path.nix;
  results = builtins.map (p: nix-path p) paths;
in
builtins.concatStringsSep ":" results
