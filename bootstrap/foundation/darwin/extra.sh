#!/usr/bin/env bash

# set -x # uncomment this for debugging

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../../ || exit

. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

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
mas install 1284863847 # Unsplash Wallpapers

####### Brew Cask #######

_install_cask () {
  brew list --cask $1 || brew install --cask $1
}

# iterm2
_install_cask iterm2

# for iterm2 and tmux scrolling issue.
# https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022
echo "read https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022 for tmux scrolling issue with iterm"

echo "Go to Profiles > Command > Command and set '~/.config/dfs-rhc/bin/tmux-attach-or-new.sh default'"

sleep 3

_install_cask alfred
_install_cask alacritty
_install_cask vanilla
_install_cask 1password
_install_cask amethyst
_install_cask google-chrome
_install_cask notion
_install_cask sublime-text
_install_cask secretive
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

echo "You might want to the followings:"
# https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
echo "System Preference > Accessibility > Display > Reduce Motion checkbox"
echo "System Preference > Keyboard > Adjust repeat speed and delay"
echo "Open Karabiner Element and allow permissions"
echo "Open Amethyst to have tiling start working"
echo "System Preference > Display > change the dominant display if you use multi screen setup"
echo "Install Keytty from ~/Downloads/Keytty.x.x.x.dmg"
echo "Open Just Focus to start using Pomodoro technic"

sleep 2

echo "All done in this script!"
