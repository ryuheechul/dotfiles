{ pkgs }:

# the actual linking is delegated to ../../home/default.nix

let
  # run https://github.com/ryanoasis/nerd-fonts/blob/master/bin/scripts/test-fonts.sh \
  # to view and test fonts
  # (visit the URL to see how-to)
  fonts_for_nerds = [ "FiraMono" "JetBrainsMono" ];
  # If Terminal app is installed on Windows host (e.g. Windows native Alacritty) not on WSL,
  # home-manager running within WSL won't cover installing fonts to Windows host.
  # so running a command below in Windows host would install the fonts above
  # `choco install nerd-fonts-firamono jetbrainsmononf`
in
with pkgs;[
  (nerdfonts.override { fonts = fonts_for_nerds; })
]
