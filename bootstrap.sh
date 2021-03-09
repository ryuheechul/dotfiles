#!/usr/bin/env sh

# This script run assumes that system has installed Homebrew via either ./mac-bootstrap.sh or ./linux-bootstrap.sh
# Either machine or human can run this script (there should be no interactions)

set -x

# to be able to be call from anywhere
cd "$(dirname "$0")" || exit

if [ -x "$(command -v nix)" ]; then
  SKIP_INSTALL_BREW=1
  home-manager switch
fi

# in case of using brew in linux
[ -d /home/linuxbrew/.linuxbrew/bin ] && PATH=/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH

# install via Brewfile
if [ -z "${SKIP_INSTALL_BREW}" ]; then
  brew update --verbose
  brew bundle --verbose --file brew/Brewfile
fi

# get repo path
if [ -x "$(command -v greadlink)" ]; then
  this_repo_path="$(greadlink -f "$(pwd)")"
elif [ -x "$(command -v readlink)" ]; then
  this_repo_path="$(readlink -f "$(pwd)")"
else
  echo "readlink binary is required to continue"
  exit 1
fi

# symlink to be read by zshrc
mkdir -p ~/.config
ln -sf "${this_repo_path}"/zshrc.d ~/.config/zshrc.d

# source dotfiles' zshrc
echo "source ~/.config/zshrc.d/zshrc" >> ~/.zshrc

# oh my zsh
git clone https://github.com/ohmyzsh/ohmyzsh ~/.oh-my-zsh

# zinit
zsh -c "source ~/.config/zshrc.d/my_addons/zinit"

# fzf shell integration to enable history and directory search

yes | $(brew --prefix fzf)/install

# starship
ln -sf "${this_repo_path}"/starship.toml ~/.config/starship.toml

# base16

git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell

# lf
ln -sf "${this_repo_path}"/lf ~/.config/lf

# tig
ln -sf "${this_repo_path}"/vim.tigrc ~/.tigrc

# gitmux
ln -sf "${this_repo_path}"/gitmux.conf ~/.gitmux.conf

# tmux
ln -sf "${this_repo_path}"/tmux.conf ~/.tmux.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux start-server && \
  tmux new-session -d && \
  sleep 1 && \
  ~/.tmux/plugins/tpm/bin/install_plugins && \
  sleep 1 && \
  tmux kill-server

# install asdf
ASDF_DIR="${ASDF_DIR:-${HOME}/.asdf}"
ASDF_DATA_DIR="${ASDF_DATA_DIR:-${HOME}/.asdf}"
PATH="${ASDF_DIR}/bin:${ASDF_DATA_DIR}/shims:${PATH}"
ln -sf ${this_repo_path}/asdf/tool-versions ~/.tool-versions

git clone https://github.com/asdf-vm/asdf.git ${ASDF_DIR} --branch v0.8.0

asdf plugin add python
asdf plugin add nodejs
bash -c '${ASDF_DATA_DIR}/plugins/nodejs/bin/import-release-team-keyring'
asdf plugin-add yarn

asdf install

# are these even necessary?
asdf reshim python
asdf reshim nodejs
asdf reshim yarn

# for nvim + spacevim
zsh -c "pip install neovim"

# spacevim
spacevim_ver="v1.4.0"
git clone https://github.com/SpaceVim/SpaceVim ~/.SpaceVim \
  && cd ~/.SpaceVim \
  && git checkout ${spacevim_ver}
ln -sf ~/dotfiles/SpaceVim.d ~/.SpaceVim.d
# shim vimrc
ln -sf ~/.SpaceVim ~/.vim
ln -sf ~/.SpaceVim ~/.config/nvim

# trigger spacevim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  npm install -g import-js # to avoid Galooshi/vim-import-js plugin to hang while being installed
  npm install -g neovim
  nvim --headless -c 'call dein#update()' -c q
  nvim --headless -c 'UpdateRemotePlugins' -c q
fi
