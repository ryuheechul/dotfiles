{
  pkgs,
  pkg-name,
  deno-pkg-name,
}:

# declarative alternative to `deno install --global --allow-all ${deno-pkg-name}`:
# - `deno install --global` works better on NixOS than `npm install -g`
#   - since it installs on $HOME/.deno instead of /nix/store (in case of `npm install -g`)
#   - however this requires setting the path for the shells let alone not declarative
# - installing directly from nixpkgs is inflexible
#   - as updating the versions requires the bumping nixpkgs that impacts other packages as well
# - `deno cache --reload ${deno-pkg-name}` may be the way to update? (let me test that later)
pkgs.writeShellScriptBin pkg-name ''
  ${pkgs.deno}/bin/deno run --allow-all ${deno-pkg-name} "$@"
''

# see the usage at ../pkgs/custom/gemini.nix
