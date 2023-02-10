let
  rev = "b86511724b9c61f86f9aa36ff9d654b6ea2e182a";
  url = "https://github.com/ryuheechul/alacritty-nightly/archive/${rev}.tar.gz";
  overlay = (import (fetchTarball url));
  pkgs-for-alacritty-nightly = import <nixpkgs> {
    overlays = [ overlay ];
  };
in
pkgs-for-alacritty-nightly.alacritty-nightly
