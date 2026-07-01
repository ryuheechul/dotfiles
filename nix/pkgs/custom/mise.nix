{ pkgs }:

# for https://github.com/jdx/mise - nixpkgs lags upstream releases, pin directly instead

let
  sources = import ./via-niv;
  flake = sources.mise.url;
  result = builtins.getFlake flake;
in
result.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
  # upstream CI already runs mise's test suite; skip re-running it on every local build
  doCheck = false;
})
