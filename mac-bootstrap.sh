#!/usr/bin/env sh

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# install homebrew
if ! command -v brew &> /dev/null
then
    echo "brew not found so installing"
    sleep 1
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    exit
else
    echo "brew exist, so skipping installation"
fi

# mas let you install Mac App Store app with CLI
brew install mas coreutils gnu-sed
mas install 869223134 # KakaoTalk
mas install 1480933944 # Vimari
mas install 1142151959 # Just Focus
mas install 803453959 # Slack

brew install --cask iterm2

# for iterm2 and tmux scrolling issue.
# https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022

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

echo "reading key repeat values before set"
defaults read -g InitialKeyRepeat
defaults read -g KeyRepeat

defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)

echo "registering automatic theme switching"
brew install crescentrose/sunshine/sunshine
bin/mac/theme/register-to-launchd.sh

echo "Settings > Accessibility > Display > Reduce Motion checkbox" # https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
sleep 2

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
# go to Keytty download page
open https://github.com/keytty/shelter/releases
