{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  use-emacs-stable = checkEnv "MY_NIX_OVERRIDE_EMACS_STABLE";
  # https://blog.phundrak.com/emacs-29-what-can-we-expect/
  # emacs29-pgtk: wayland native via pure gtk
  emacs29 = if pkgs.stdenv.isLinux then pkgs.emacs29-pgtk else pkgs.emacs29;
  emacs = if use-emacs-stable then pkgs.emacs else emacs29;
  emacsWithNativeComp = emacs.override { withNativeCompilation = true; };
  emacsWithPackages = (pkgs.emacsPackagesFor emacsWithNativeComp).emacsWithPackages;
  customizedEmacs = emacsWithPackages (epkgs: [ epkgs.vterm ]); # include libvterm
  # above is basically the broken down version of below
  # ((emacsPackagesFor (emacs.override { withNativeCompilation = true; })).emacsWithPackages (epkgs: [ epkgs.vterm ]))
in
customizedEmacs # emacs editor including GUI, `emacs -nw` to run as TUI

# more info on https://nixos.wiki/wiki/Emacs
