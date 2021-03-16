#!/usr/bin/env bash

set -x

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../ || exit


####### home config #######

# icloud drive symlink
ln -sf ~/Library/Mobile\ Documents/com~apple~CloudDocs/ ~/icd

# to prevent loading zsh stuff twice in case tmux is the default shell command for terminal emulators
echo "export HOST_ALWAYS_USE_TMUX=1" >> ~/.zshrc


####### brew #######

# install homebrew
if ! command -v brew &> /dev/null
then
    echo "brew not found so installing"
    sleep 1
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    brew update --verbose
    exit
else
    echo "brew exist, so skipping installation"
fi


####### Mac App Store #######

# mas let you install Mac App Store app with CLI
brew install mas coreutils gnu-sed
mas install 869223134 # KakaoTalk
mas install 1480933944 # Vimari
mas install 1142151959 # Just Focus
mas install 803453959 # Slack


####### Brew Cask #######

# iterm2
brew install --cask iterm2

# for iterm2 and tmux scrolling issue.
# https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022
echo "read https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022 for tmux scrolling issue with iterm"

sleep 3

brew install --cask alfred
brew install --cask alacritty
brew install --cask vanilla
brew install --cask 1password
brew install --cask amethyst
brew install --cask google-chrome
brew install --cask notion
brew install --cask sublime-text
sublime/.link.sh
brew install --cask karabiner-elements
karabiner/.link.sh

sleep 2


####### system #######

echo "reading key repeat values before set"
defaults read -g InitialKeyRepeat
defaults read -g KeyRepeat

defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)


echo "registering automatic theme switching"
brew install crescentrose/sunshine/sunshine
bin/mac/theme/register-to-launchd.sh

echo "Do the following:"
echo "Settings > Accessibility > Display > Reduce Motion checkbox" # https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
sleep 2


####### font #######

## Maybe JetBrainsMono is enough? so comment this out for now
# brew cask install homebrew/cask-fonts/font-hack-nerd-font

# download nerd font patched jetbrains mono font
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font

# download and install JetBrains Mono
wget -O JetBrainsMono.zip https://download.jetbrains.com/fonts/JetBrainsMono-2.001.zip
unzip JetBrainsMono.zip -d .JetBrainsMono
rm JetBrainsMono.zip
echo "You will need to click stuff to add fonts"
sleep 2

open .JetBrainsMono/ttf/*.ttf
echo ".JetBrainsMono will be cleaned up after a minute"
(sleep 5; rm -rf .JetBrainsMono ) &

sleep 5


####### Keytty #######

# go to Keytty download page
open https://github.com/keytty/shelter/releases


####### Nix #######

# install nix - https://nixos.org/manual/nix/stable/#sect-macos-installation
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume

# source nix profile only for this script to work
. ~/.nix-profile/etc/profile.d/nix.sh

# init channels
./nix/bin/channels.sh

# set up a local ~/.config/nixpkgs/home.nix
./nix/bin/init-home-manager.sh

if [ -x "$(command -v nix)" ]; then
  SKIP_INSTALL_BREW=1
  home-manager switch
fi


####### Brewfile #######

# install via Brewfile
if [ -z "${SKIP_INSTALL_BREW}" ]; then
  brew bundle --verbose --file brew/Brewfile

  # fzf shell integration to enable history and directory search
  yes | $(brew --prefix fzf)/install
fi
