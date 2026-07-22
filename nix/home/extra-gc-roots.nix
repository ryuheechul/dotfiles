# Extra GC roots for niv-pinned sources.
#
# Without this, `nix-collect-garbage` deletes niv sources (nixpkgs,
# home-manager, custom packages, etc.) and `switch-home` has to re-fetch
# them. This file prevents that by symlinking each source into ~/.nix-pinned/
# via home.file, which ties them to the active profile generation.
#
# How it works:
#   `home.file` creates symlinks in the home directory. Home Manager
#   manages these symlinks, so the targets (store paths) become GC roots
#   tied to the active profile generation. When you bump niv and
#   rebuild, the old symlinks are replaced and the previous store paths
#   become eligible for collection.
#
# OPTIONAL, everything works without it. Safe to exclude for testing,
# debugging, or any reason you want fresh fetches.
#
# Note: ~/.nix-pinned/ is created on first `switch-home` (or `home-manager
# switch`). It does not exist until then.

{ ... }:

let
  # System-wide sources: nixpkgs, home-manager, nixos, nixos-hardware, etc.
  niv-shim-sources = import ../niv-shim/nix/sources.nix;

  # Custom package sources: ghostty, mise, nur, worktrunk, etc.
  via-niv-sources = import ../pkgs/custom/via-niv/nix/sources.nix;

  # sources.nix exports __functor alongside the actual sources; filter it out.
  all-sources = builtins.removeAttrs (niv-shim-sources // via-niv-sources) [ "__functor" ];

  # Prefix each source name with ".nix-pinned/" for the home.file key
  mk-entry = name: source: {
    name = ".nix-pinned/${name}";
    value = {
      source = source;
    };
  };

  entries = builtins.listToAttrs (
    builtins.map (name: mk-entry name all-sources.${name}) (builtins.attrNames all-sources)
  );
in
{
  home.file = entries;
}
