#!/usr/bin/env sh

# Either machine or human can run this script (there should be no interactions)

# Configuration.sh is platform agnostic and assumes dependant packages are installed via `./foundation/`
# Also make sure to configure right $PATH for this script to work properly

set -e
set -x

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../ || exit

# enable nix for the rest of script - wrap with `set +e` to be compatible with older version `[da]sh` like the one the macOS
set +e; source ./nix/bin/source/nix.sh; set -e

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

XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

echo "[INFO] \$XDG_CONFIG_HOME: ${XDG_CONFIG_HOME}"

# make sure ${XDG_CONFIG_HOME} exist
mkdir -p "${XDG_CONFIG_HOME}"

# symlink this repo so it's discoverable no matter where this is located at
# dfs-rhc: shorthand for dotfiles-ryuheechul
ln -sf "${this_repo_path}" "${XDG_CONFIG_HOME}/dfs-rhc"

dfs_rhc="${XDG_CONFIG_HOME}/dfs-rhc"

# in case my bins are being used in the configuration step already
# - one discovered usage is that `current-base16` is being used in ../nvim/lua/plugins/theme.lua
export PATH="${dfs_rhc}/bin/path/default:${PATH}"

# source my gitconfig
cat << EOF >> "${HOME}/.gitconfig"
[include]
  path = "${dfs_rhc}/gitconfig"
EOF

# symlink gh config
mkdir -p "${XDG_CONFIG_HOME}/gh"
ln -sf "${dfs_rhc}/gh/config.yml" "${XDG_CONFIG_HOME}/gh/config.yml"

# symlink batconfig
ln -sf "${dfs_rhc}/bat" "${XDG_CONFIG_HOME}/bat"
bat cache --build || true

# alacritty
ln -sf "${dfs_rhc}/alacritty/unix.toml" "${HOME}/.alacritty.toml"

# starship
ln -sf "${dfs_rhc}/starship.toml" "${XDG_CONFIG_HOME}/starship.toml"

# base16
git clone https://github.com/chriskempson/base16-shell.git "${XDG_CONFIG_HOME}/base16-shell"

# prevent this file missing to satisfy `doom doctor` due to my own configuration
touch "${HOME}/.base16_theme.updated-time"

# lf
ln -sf "${dfs_rhc}/lf" "${XDG_CONFIG_HOME}/lf"

# rsop
rm -rf "${XDG_CONFIG_HOME}/rsop"; ln -sf "${dfs_rhc}/rsop" "${XDG_CONFIG_HOME}/rsop"

# urlscan
ln -sf "${dfs_rhc}/urlscan" "${XDG_CONFIG_HOME}/urlscan"

# espanso
espanso_config="${XDG_CONFIG_HOME}/espanso" \
  && rm -rf "${espanso_config}" \
  && ln -sf "${dfs_rhc}/espanso" "${espanso_config}"

# espanso for darwin
uname | xargs test "Darwin" = \
  && espanso_config_for_darwin="${HOME}/Library/Preferences/espanso" \
  && rm -rf "${espanso_config_for_darwin}" \
  && ln -sf "${espanso_config}" "${espanso_config_for_darwin}"

# viddy
ln -sf "${dfs_rhc}/viddy.toml" "${XDG_CONFIG_HOME}/viddy.toml"

# tig
ln -sf "${dfs_rhc}/vim.tigrc" "${HOME}/.tigrc"

# gitmux
ln -sf "${dfs_rhc}/gitmux.conf" "${HOME}/.gitmux.conf"

# zellij
ln -sf "${dfs_rhc}/zellij" "${XDG_CONFIG_HOME}/zellij"

# tmux
ln -sf "${dfs_rhc}/tmux.conf" "${HOME}/.tmux.conf"
git clone https://github.com/tmux-plugins/tpm "${HOME}/.tmux/plugins/tpm" || bash -c 'cd "${HOME}/.tmux/plugins/tpm" && git pull'
tmux start-server && \
  tmux new-session -d && \
  sleep 1 && \
  "${HOME}/.tmux/plugins/tpm/bin/install_plugins" && \
  sleep 1 && \
  tmux kill-server || true

# avoid using `/usr/local/bin` as a global path for yarn
yarn config set prefix "${HOME}/.yarn"

# install asdf
ASDF_DIR="${ASDF_DIR:-${HOME}/.asdf}"
ASDF_DATA_DIR="${ASDF_DATA_DIR:-${HOME}/.asdf}"
PATH="${ASDF_DIR}/bin:${ASDF_DATA_DIR}/shims:${PATH}"
ln -sf "${dfs_rhc}/asdf/tool-versions" "${HOME}/.tool-versions"

git clone https://github.com/asdf-vm/asdf.git ${ASDF_DIR} --branch v0.8.0 || true

## installing packages with asdf has been replaced with Nix - look at ../nix/pkgs.nix

## zsh

## "linking" these are now done via ../nix/home/programs/shells.nix
# # source dotfiles' env
# echo "source '${dfs_rhc}/zsh/env'" >> "${HOME}/.zshenv"
# # source dotfiles' zlogin
# echo "source '${dfs_rhc}/zsh/zlogin'" >> "${HOME}/.zlogin"
# # source dotfiles' zshrc
# echo "source '${dfs_rhc}/zsh/zshrc'" >> "${HOME}/.zshrc"

# source zinit now to avoid installing zsh plugins at initial usage
zsh -c "source '${dfs_rhc}/zsh/my_addons/zinit'"

## emacs

# chemecs to allow switching between configs like doom emacs and spacemacs
git clone https://github.com/plexus/chemacs2.git "${HOME}/.emacs.d" || bash -c 'cd "${HOME}/.emacs.d" && git pull'
ln -sf "${dfs_rhc}/emacs.d/emacs-profiles.el" "${HOME}/.emacs-profiles.el"

# spacemacs
git clone https://github.com/syl20bnr/spacemacs "${HOME}/.spacemacs.d" || bash -c 'cd "${HOME}/.spacemacs.d" && git pull'
ln -sf "${dfs_rhc}/emacs.d/spacemacs" "${HOME}/.spacemacs"

# doom emacs
git clone https://github.com/hlissner/doom-emacs "${HOME}/.doom-emacs.d" || bash -c 'cd "${HOME}/.doom-emacs.d" && git pull'
ln -sf "${dfs_rhc}/emacs.d/doom.d" "${XDG_CONFIG_HOME}/doom"
yes | "${HOME}/.doom-emacs.d/bin/doom" install || true # let the failure of this command not to block the rest

## (neo)vim
# neovim - now this replace SpaceVim
ln -sf "${dfs_rhc}/nvim" "${XDG_CONFIG_HOME}/nvim"

# trigger neovim plugins install via command line
if [ -z "${SKIP_INSTALL_VIM_PLUGINS}" ]; then
  # since I'm not sure about this command to work very well and it's ok to fail for now anyway let it not cause disruption on failure
  nvim --headless "+Lazy! sync" +qa
  nvim --headless -c 'UpdateRemotePlugins' -c q || true
fi

# SpaceVim - this still may be used for vim but not with nvim
spacevim_ver="v1.6.0"
git clone https://github.com/SpaceVim/SpaceVim "${HOME}/.SpaceVim" || bash -c 'cd "${HOME}/.SpaceVim" && git checkout master && git pull' \
  && cd "${HOME}/.SpaceVim" \
  && git checkout ${spacevim_ver}
ln -sf "${dfs_rhc}/SpaceVim.d" "${HOME}/.SpaceVim.d"
# shim vimrc
ln -sf "${HOME}/.SpaceVim" "${HOME}/.vim"

# wudo in case of WSL
test -n "${WSL_DISTRO_NAME}" && git clone https://github.com/Chronial/wsl-sudo.git "${HOME}/.wsl-sudo"

echo "configuration.sh seemed to have run successfully!"
