{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  use-emacs29 = checkEnv "MY_NIX_OVERRIDE_EMACS_29";
  emacs = if use-emacs29 then pkgs.emacs29 else pkgs.emacs;
  emacsWithNativeComp = emacs.override { withNativeCompilation = true; };
  emacsWithPackages = (pkgs.emacsPackagesFor emacsWithNativeComp).emacsWithPackages;
  customizedEmacs = emacsWithPackages (epkgs: [ epkgs.vterm ]); # include libvterm
  # above is basically the broken down version of below
  # ((emacsPackagesFor (emacs.override { withNativeCompilation = true; })).emacsWithPackages (epkgs: [ epkgs.vterm ]))
in
customizedEmacs # emacs editor including GUI, `emacs -nw` to run as TUI

# more info on https://nixos.wiki/wiki/Emacs
