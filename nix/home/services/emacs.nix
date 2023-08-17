{ pkgs, ... }:

{
  services.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = import ../../pkgs/custom/emacs.nix { pkgs = pkgs; };
  };
}
