{ pkgs }:

let
  viaDeno = import ../../utils/viaDeno.nix;
in

# declarative alternative to `deno install --global --allow-all npm:@google/gemini-cli/gemini`:
# - `deno install --global` works better on NixOS than `npm install -g`
#   - since it installs on $HOME/.deno instead of /nix/store (in case of `npm install -g`)
#   - however this requires setting the path for the shells let alone not declarative
# - installing directly from nixpkgs is inflexible
#   - as updating the versions requires the bumping nixpkgs that impacts other packages as well
# - `deno cache --reload npm:@google/gemini-cli` may be the way to update? (let me test that later)
viaDeno {
  pkgs = pkgs;
  pkg-name = "gemini";
  deno-pkg-name = "npm:@google/gemini-cli";
}
