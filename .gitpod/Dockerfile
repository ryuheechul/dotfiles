FROM gitpod/workspace-full

## minimal but efficient caching
# only minimal essential static list
RUN brew install neovim tmux zsh asdf starship

# this should also be static
RUN asdf plugin add python && \
      asdf install python 3.7.7 && \
      asdf global python 3.7.7 && \
      asdf exec pip install neovim

# brew bundle avoid missing cache because of other files
WORKDIR /tmp
COPY --chown=gitpod:gitpod ./Brewfile /tmp/Brewfile
RUN brew bundle
## end of caching

WORKDIR ${HOME}
# make sure the build context is the root of the repo
COPY --chown=gitpod:gitpod . ${HOME}/dotfiles

WORKDIR ${HOME}/dotfiles
RUN ./bootstrap.sh
