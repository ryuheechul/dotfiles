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

# symlink to be read by zshrc
mkdir -p ~/.config
ln -sf "${this_repo_path}/zshrc.d" ~/.config/zshrc.d

# source dotfiles' zshrc
echo "source ~/.config/zshrc.d/zshrc" >> ~/.zshrc

# source my gitconfig
cat << EOF >> ~/.gitconfig
[include]
  path = "${this_repo_path}/gitconfig"
EOF

# symlink dotfiles/bin
mkdir -p ~/.local
ln -sf "${this_repo_path}/bin" ~/.local/dotfiles-bin

# additional bin from a separate repo
git clone https://github.com/ryuheechul/bin.git ~/.local/my-bin || bash -c 'cd ~/.local/my-bin && git pull'

# symlink gh config
mkdir -p ~/.config/gh
ln -sf "${this_repo_path}/gh/config.yml" ~/.config/gh/config.yml

# symlink batconfig
ln -sf "${this_repo_path}/bat" ~/.config/bat

# alacritty
ln -sf "${this_repo_path}/alacritty.yml" ~/.alacritty.yml

# zinit
zsh -c "source ~/.config/zshrc.d/my_addons/zinit"

# starship
ln -sf "${this_repo_path}/starship.toml" ~/.config/starship.toml

# base16
git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell || bash -c 'cd ~/.config/base16-shell && git pull'

# lf
ln -sf "${this_repo_path}/lf" ~/.config/lf

# tig
ln -sf "${this_repo_path}/vim.tigrc" ~/.tigrc

# gitmux
ln -sf "${this_repo_path}/gitmux.conf" ~/.gitmux.conf

# tmux
ln -sf "${this_repo_path}/tmux.conf" ~/.tmux.conf
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
ln -sf "${this_repo_path}/asdf/tool-versions" ~/.tool-versions

git clone https://github.com/asdf-vm/asdf.git ${ASDF_DIR} --branch v0.8.0 || true

## installing packages with asdf has been replaced with Nix - look at ../nix/pkgs.nix

## emacs
# spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || bash -c 'cd ~/.spacemacs.d && git pull'
ln -sf "${this_repo_path}/emacs.d/spacemacs" ~/.spacemacs

# doom emacs
git clone https://github.com/hlissner/doom-emacs ~/.doom-emacs.d || bash -c 'cd ~/.doom-emacs.d && git pull'
~/.doom-emacs.d/bin/doom -y install

# chemecs to allow switching between configs like doom emacs and spacemacs
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d || bash -c 'cd ~/.emacs.d && git pull'
ln -sf "${this_repo_path}/emacs.d/emacs-profiles.el" ~/.emacs-profiles.el

## (neo)vim
# neovim - now this replace SpaceVim
ln -sf "${this_repo_path}/nvim" ~/.config/nvim

# trigger neovim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  # since I'm not sure about this command to work very well and it's ok to fail for now anyway let it not cause disruption on failure
  nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync' || true
  nvim --headless -c 'UpdateRemotePlugins' -c q || true
fi

# SpaceVim - this still may be used for vim but not with nvim
spacevim_ver="v1.6.0"
git clone https://github.com/SpaceVim/SpaceVim ~/.SpaceVim || bash -c 'cd ~/.SpaceVim && git checkout master && git pull' \
  && cd ~/.SpaceVim \
  && git checkout ${spacevim_ver}
ln -sf "${this_repo_path}/SpaceVim.d" ~/.SpaceVim.d
# shim vimrc
ln -sf ~/.SpaceVim ~/.vim

echo "configuration.sh seemed to have run successfully!"
