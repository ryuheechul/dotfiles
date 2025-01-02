{ pkgs }:

# the actual linking is delegated to ../../home/default.nix

# run https://github.com/ryanoasis/nerd-fonts/blob/master/bin/scripts/test-fonts.sh \
# to view and test fonts (visit the URL above to see how-to)
# and watch if watch [Effective Nerd Fonts in Multiple Terminals](https://www.youtube.com/watch?v=mQdB_kHyZn8)
# to get a feel about how to configure fonts for different terminals and linux (for fallbacks)
# If Terminal app is installed on Windows host (e.g. Windows native Alacritty) not on WSL,
# home-manager running within WSL won't cover installing fonts to Windows host.
# so running a command below in Windows host would install the fonts above
# `choco install nerd-fonts-firamono jetbrainsmononf`
with pkgs.nerd-fonts;
[
  fira-mono
  jetbrains-mono
]
# run `fc-cache -r` if there is an issue after (re)installing fonts
