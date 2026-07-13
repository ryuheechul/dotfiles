{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  # https://blog.phundrak.com/emacs-29-what-can-we-expect/
  # emacs-pgtk: wayland native via pure gtk
  emacs = if pkgs.stdenv.isLinux then pkgs.emacs-pgtk else pkgs.emacs;
  emacsWithOptions = emacs.override {
    # these are pretty much the default but still making it explicit for documentation purpose
    withNativeCompilation = true; # this is pretty much default
    withTreeSitter = true; # this is the default for 29+ version
  };
  emacsWithPackages = (pkgs.emacsPackagesFor emacsWithOptions).emacsWithPackages;
  # vterm builds a native libvterm module from source (slow to compile); default
  # to pure emacs and only pull it in when MY_NIX_EXTRA_EMACS_VTERM is set
  extraEmacsPackages =
    epkgs: pkgs.lib.optionals (checkEnv "MY_NIX_EXTRA_EMACS_VTERM") [ epkgs.vterm ];
  customizedEmacs = emacsWithPackages extraEmacsPackages;
  # above is basically the broken down version of below
  # ((emacsPackagesFor (emacs.override { ... })).emacsWithPackages (epkgs: [ epkgs.vterm ]))
in
customizedEmacs # emacs editor including GUI, `emacs -nw` to run as TUI

# more info on https://nixos.wiki/wiki/Emacs
