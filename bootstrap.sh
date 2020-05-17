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
this_repo_path="$(readlink -f "$(dirname "$0")")"
mkdir -p ~/.config
ln -s "${this_repo_path}"/zshrc.d ~/.config/zshrc.d

# source dotfiles' zshrc
echo "source ~/.config/zshrc.d/zshrc" >> ~/.zshrc

# oh my zsh
git clone https://github.com/ohmyzsh/ohmyzsh ~/.oh-my-zsh

# zinit
zsh -c "source ~/.config/zshrc.d/my_addons/zinit"

# lf
ln -s "$(this_repo_path)"/lf ~/.config/lf

# tig
ln -s "$(this_repo_path)"/vim.tigrc ~/.tigrc

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
  nvim --headless -c 'call dein#update()' -c q
fi
