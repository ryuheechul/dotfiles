#!/usr/bin/env sh

# Either machine or human can run this script (there should be no interactions)

# Configuration.sh is platform agnostic and assumes dependant packages are installed via `./foundation/`
# Also make sure to configure right $PATH for this script to work properly

set -e
set -x

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../ || exit

# source to access depedent binaries
. ~/.nix-profile/etc/profile.d/nix.sh

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

# making sure this comes before others
echo 'export PATH="$HOME/.nix-profile/bin:$PATH"' >> ~/.zshrc

# source dotfiles' zshrc
echo "source ~/.config/zshrc.d/zshrc" >> ~/.zshrc

# source my gitconfig
cat << EOF >> ~/.gitconfig
[include]
  path = ${this_repo_path}/gitconfig
EOF

# symlink batconfig
mkdir -p ~/.config/bat
ln -sf "${this_repo_path}"/batconfig ~/.config/bat/config

# zinit
zsh -c "source ~/.config/zshrc.d/my_addons/zinit"

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
  tmux kill-server || true

# install asdf
ASDF_DIR="${ASDF_DIR:-${HOME}/.asdf}"
ASDF_DATA_DIR="${ASDF_DATA_DIR:-${HOME}/.asdf}"
PATH="${ASDF_DIR}/bin:${ASDF_DATA_DIR}/shims:${PATH}"
ln -sf ${this_repo_path}/asdf/tool-versions ~/.tool-versions

git clone https://github.com/asdf-vm/asdf.git ${ASDF_DIR} --branch v0.8.0

## installing packages with asdf is being replaced with Nix - look at ../nix/pkgs.nix

# asdf plugin add python
# asdf plugin add nodejs
# bash -c '${ASDF_DATA_DIR}/plugins/nodejs/bin/import-release-team-keyring'
# asdf plugin-add yarn

# asdf install

# # are these even necessary?
# asdf reshim python
# asdf reshim nodejs
# asdf reshim yarn

# # for nvim + spacevim
# zsh -c "pip install neovim"

# spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -sf "${this_repo_path}"/spacemacs ~/.spacemacs

# spacevim
spacevim_ver="v1.6.0"
git clone https://github.com/SpaceVim/SpaceVim ~/.SpaceVim \
  && cd ~/.SpaceVim \
  && git checkout ${spacevim_ver}
ln -sf "${this_repo_path}"/SpaceVim.d ~/.SpaceVim.d
# shim vimrc
ln -sf ~/.SpaceVim ~/.vim
ln -sf ~/.SpaceVim ~/.config/nvim

# trigger spacevim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  nvim --headless -c 'call dein#update()' -c q
  nvim --headless -c 'UpdateRemotePlugins' -c q
fi
