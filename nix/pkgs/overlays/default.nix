let
  unstable = import <nixpkgs> { };
  stdenv = unstable.stdenv;
  lib = unstable.lib;
  checkEnv = import ../../utils/checkEnv.nix;
  shouldNeovimNightly = checkEnv "MY_NIX_OVERLAY_NEOVIM_NIGHTLY";
in
{
  overlays = [ ]
    ++ lib.optionals stdenv.isDarwin [ (import ./darwin.nix) ]
    ++ lib.optionals shouldNeovimNightly [ (import ./neovim-nightly.nix) ];
}
