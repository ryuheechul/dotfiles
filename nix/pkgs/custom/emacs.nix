{ pkgs }:

let
  # emacs = pkgs.emacs29 # use this instead of below if you wish to use emacs29 instead
  emacs = pkgs.emacs;
  emacsWithNativeComp = emacs.override { withNativeCompilation = true; };
  emacsWithPackages = (pkgs.emacsPackagesFor emacsWithNativeComp).emacsWithPackages;
  customizedEmacs = emacsWithPackages (epkgs: [ epkgs.vterm ]); # include libvterm
  # above is basically the broken down version of below
  # ((emacsPackagesFor (emacs.override { withNativeCompilation = true; })).emacsWithPackages (epkgs: [ epkgs.vterm ]))
in
customizedEmacs # emacs editor including GUI, `emacs -nw` to run as TUI

# more info on https://nixos.wiki/wiki/Emacs
