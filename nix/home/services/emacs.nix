{ pkgs, ... }:

let
  checkEnv = import ../../utils/checkEnv.nix;
in
{
  # gate the daemon on MY_NIX_EXTRA_EMACS too, otherwise the service pulls emacs
  # even when the package is gated out (see ../../pkgs/extra/default.nix)
  services.emacs = {
    enable = pkgs.stdenv.isLinux && checkEnv "MY_NIX_EXTRA_EMACS";
    package = import ../../pkgs/custom/emacs.nix { pkgs = pkgs; };
  };
}
