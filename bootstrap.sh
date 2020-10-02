#!/usr/bin/env sh

# this script run assumes that system has installed Homebrew

set -x

# to be able to be call from anywhere
cd "$(dirname "$0")" || exit

# install via Brewfile
if [ -z "${SKIP_INSTALL_BREW}" ]; then
  brew update --verbose
  brew bundle --verbose
fi

# symlink to be read by zshrc
this_repo_path="$(greadlink -f "$(dirname "$0")")"

mkdir -p ~/.config
ln -s "${this_repo_path}"/zshrc.d ~/.config/zshrc.d

# source dotfiles' zshrc
echo "source ~/.config/zshrc.d/zshrc" >> ~/.zshrc

# oh my zsh
git clone https://github.com/ohmyzsh/ohmyzsh ~/.oh-my-zsh

# zinit
zsh -c "source ~/.config/zshrc.d/my_addons/zinit"

# fzf shell integration to enable history and directory search

yes | $(brew --prefix fzf)/install

# starship
ln -s "${this_repo_path}"/starship.toml ~/.config/starship.toml

# lf
ln -s "${this_repo_path}"/lf ~/.config/lf

# tig
ln -s "${this_repo_path}"/vim.tigrc ~/.tigrc

# gitmux
ln -s "${this_repo_path}"/gitmux.conf ~/.gitmux.conf

# tmux
ln -s "${this_repo_path}"/tmux.conf ~/.tmux.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux start-server && \
  tmux new-session -d && \
  sleep 1 && \
  ~/.tmux/plugins/tpm/bin/install_plugins && \
  sleep 1 && \
  tmux kill-server

# install python3 via asdf for nvim + spacevim
if [ -z "${SKIP_INSTALL_ASDF_PYTHON3}" ]; then
  asdf plugin add python
  asdf install python 3.7.7
  asdf global python 3.7.7
  asdf reshim python
  zsh -c "pip install neovim"
fi

# install node via asdf for nvim + spacevim
if [ -z "${SKIP_INSTALL_ASDF_NODEJS}" ]; then
  asdf plugin add nodejs
  bash -c '${ASDF_DATA_DIR:=$HOME/.asdf}/plugins/nodejs/bin/import-release-team-keyring'
  asdf install nodejs 12.18.3
  asdf global nodejs 12.18.3
  asdf reshim nodejs
fi

# spacevim
spacevim_ver="v1.4.0"
git clone https://github.com/SpaceVim/SpaceVim ~/.SpaceVim \
  && cd ~/.SpaceVim \
  && git checkout ${spacevim_ver}
ln -s ~/dotfiles/SpaceVim.d ~/.SpaceVim.d
# shim vimrc
ln -s ~/.SpaceVim ~/.vim
ln -s ~/.SpaceVim ~/.config/nvim

# trigger spacevim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  npm install -g import-js # to avoid Galooshi/vim-import-js plugin to hang while being installed
  npm install -g neovim
  nvim --headless -c 'call dein#update()' -c q
  nvim --headless -c 'UpdateRemotePlugins' -c q
fi
