{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  # https://blog.phundrak.com/emacs-29-what-can-we-expect/
  # emacs-pgtk: wayland native via pure gtk
  emacs = if pkgs.stdenv.isLinux then pkgs.emacs-pgtk else pkgs.emacs;
  emacsWithNativeComp = emacs.override { withNativeCompilation = true; };
  emacsWithPackages = (pkgs.emacsPackagesFor emacsWithNativeComp).emacsWithPackages;
  customizedEmacs = emacsWithPackages (epkgs: [ epkgs.vterm ]); # include libvterm
  # above is basically the broken down version of below
  # ((emacsPackagesFor (emacs.override { withNativeCompilation = true; })).emacsWithPackages (epkgs: [ epkgs.vterm ]))
in
customizedEmacs # emacs editor including GUI, `emacs -nw` to run as TUI

# more info on https://nixos.wiki/wiki/Emacs
