#!/usr/bin/env sh

# Either machine or human can run this script (there should be no interactions)

# Configuration.sh is platform agnostic and assumes dependant packages are installed via `./foundation/`
# Also make sure to configure right $PATH for this script to work properly

set -e
set -x

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../ || exit


# enable nix for the rest of script

. ./nix/bin/source-nix.sh

if [ -z "$(command -v nix)" ]; then
  echo 'Warning: `nix` is still not found but trying to run the rest of this script anyway'
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

# make sure ~/.config exist
mkdir -p ~/.config

# symlink this repo so it's discoverable no matter where this is located at
# dfs-rhc: shorthand for dotfiles-ryuheechul
ln -sf "${this_repo_path}" ~/.config/dfs-rhc

dfs_rhc="${HOME}/.config/dfs-rhc"

# in case my bins are being used in the configuration step already
# - one discovered usage is that `current-base16` is being used in ../nvim/lua/plugins/theme.lua
export PATH="${dfs_rhc}/bin/discoverable:${PATH}"

# source my gitconfig
cat << EOF >> ~/.gitconfig
[include]
  path = "${dfs_rhc}/gitconfig"
EOF

# symlink gh config
mkdir -p ~/.config/gh
ln -sf "${dfs_rhc}/gh/config.yml" ~/.config/gh/config.yml

# symlink batconfig
ln -sf "${dfs_rhc}/bat" ~/.config/bat
bat cache --build || true

# alacritty
ln -sf "${dfs_rhc}/alacritty.yml" ~/.alacritty.yml

# starship
ln -sf "${dfs_rhc}/starship.toml" ~/.config/starship.toml

# base16
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell \
  || bash -c 'cd ~/.config/base16-shell && git pull && git checkout cd71822de1f9b53eea9beb9d94293985e9ad7122'

# lf
ln -sf "${dfs_rhc}/lf" ~/.config/lf

# tig
ln -sf "${dfs_rhc}/vim.tigrc" ~/.tigrc

# gitmux
ln -sf "${dfs_rhc}/gitmux.conf" ~/.gitmux.conf

# tmux
ln -sf "${dfs_rhc}/tmux.conf" ~/.tmux.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm || bash -c 'cd ~/.tmux/plugins/tpm && git pull'
tmux start-server && \
  tmux new-session -d && \
  sleep 1 && \
  ~/.tmux/plugins/tpm/bin/install_plugins && \
  sleep 1 && \
  tmux kill-server || true

# avoid using `/usr/local/bin` as a global path for yarn
yarn config set prefix ~/.yarn

# install asdf
ASDF_DIR="${ASDF_DIR:-${HOME}/.asdf}"
ASDF_DATA_DIR="${ASDF_DATA_DIR:-${HOME}/.asdf}"
PATH="${ASDF_DIR}/bin:${ASDF_DATA_DIR}/shims:${PATH}"
ln -sf "${dfs_rhc}/asdf/tool-versions" ~/.tool-versions

git clone https://github.com/asdf-vm/asdf.git ${ASDF_DIR} --branch v0.8.0 || true

## installing packages with asdf has been replaced with Nix - look at ../nix/pkgs.nix

## zsh

# source dotfiles' env
echo "source '${dfs_rhc}/sh/zsh/env'" >> ~/.zshenv

# source dotfiles' zshrc
echo "source '${dfs_rhc}/zsh/zshrc'" >> ~/.zshrc

# source zinit now to avoid installing zsh plugins at initial usage
zsh -c "source '${dfs_rhc}/zsh/my_addons/zinit'"

## emacs

# spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || bash -c 'cd ~/.spacemacs.d && git pull'
ln -sf "${dfs_rhc}/emacs.d/spacemacs" ~/.spacemacs

# doom emacs
git clone https://github.com/hlissner/doom-emacs ~/.doom-emacs.d || bash -c 'cd ~/.doom-emacs.d && git pull'
ln -sf "${dfs_rhc}/emacs.d/doom.d" ~/.config/doom
~/.doom-emacs.d/bin/doom -y install || true # let the failure of this command not to block the rest

# chemecs to allow switching between configs like doom emacs and spacemacs
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d || bash -c 'cd ~/.emacs.d && git pull'
ln -sf "${dfs_rhc}/emacs.d/emacs-profiles.el" ~/.emacs-profiles.el

## (neo)vim
# neovim - now this replace SpaceVim
ln -sf "${dfs_rhc}/nvim" ~/.config/nvim

# trigger neovim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  # since I'm not sure about this command to work very well and it's ok to fail for now anyway let it not cause disruption on failure
  nvim --headless "+Lazy! restore" +qa
  nvim --headless -c 'UpdateRemotePlugins' -c q || true
fi

# SpaceVim - this still may be used for vim but not with nvim
spacevim_ver="v1.6.0"
git clone https://github.com/SpaceVim/SpaceVim ~/.SpaceVim || bash -c 'cd ~/.SpaceVim && git checkout master && git pull' \
  && cd ~/.SpaceVim \
  && git checkout ${spacevim_ver}
ln -sf "${dfs_rhc}/SpaceVim.d" ~/.SpaceVim.d
# shim vimrc
ln -sf ~/.SpaceVim ~/.vim

echo "configuration.sh seemed to have run successfully!"
