#!/usr/bin/env bash

# set -x # uncomment this for debugging

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../../ || exit

. ~/.nix-profile/etc/profile.d/nix.sh

####### Screenshots #######

mkdir -p ~/Screenshots && defaults write com.apple.screencapture location ~/Screenshots

####### Mac App Store #######

# mas let you install Mac App Store app with CLI
brew install mas
mas install 869223134 # KakaoTalk
mas install 1480933944 # Vimari
mas install 1142151959 # Just Focus
mas install 803453959 # Slack
mas install 1475387142 # Tailscale
mas install 1480933944 # Vimari

####### Brew Cask #######

_install_cask () {
  brew list --cask $1 || brew install --cask $1
}

# iterm2
_install_cask iterm2

# for iterm2 and tmux scrolling issue.
# https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022
echo "read https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022 for tmux scrolling issue with iterm"

sleep 3

_install_cask alfred
_install_cask alacritty
_install_cask vanilla
_install_cask 1password
_install_cask amethyst
_install_cask google-chrome
_install_cask notion
_install_cask sublime-text
_install_cask visual-studio-code
./sublime/.link.sh
_install_cask karabiner-elements
./karabiner/.link.sh

echo "do ./karabiner-uk/.link.sh instead if the hardware keyboard layout is British"

sleep 2

####### Keytty #######

# go to Keytty download page
open https://github.com/keytty/shelter/releases

####### font #######

## Maybe JetBrainsMono is enough? so comment this out for now
# brew cask install homebrew/cask-fonts/font-hack-nerd-font

# download nerd font patched jetbrains mono font
brew tap homebrew/cask-fonts
_install_cask font-jetbrains-mono-nerd-font

####### theme #######

echo "registering automatic theme switching"
brew install crescentrose/sunshine/sunshine
bin/mac/theme/register-to-launchd.sh

echo "Do the following:"
# https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
echo "Settings > Accessibility > Display > Reduce Motion checkbox" 
sleep 2

echo "All done in this script!"
