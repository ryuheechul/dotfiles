#!/usr/bin/env sh

repo=${X_GITPOD_REPO_DOTFILES}
branch=${X_GITPOD_REPO_DOTFILES_BRANCH:-master}
bootstrap=${X_GITPOD_REPO_DOTFILES_BOOTSTRAP}

if [ -z ${repo} ]; then
  echo "env X_GITPOD_REPO_DOTFILES is not found exiting"
  exit 1;
fi

echo "will start cloning ${repo}"

set -x
git clone --single-branch --branch ${branch} ${repo} ${HOME}/dotfiles

if [ -n ${bootstrap} ]; then
  cd ${HOME}/dotfiles && ./${bootstrap}
fi
