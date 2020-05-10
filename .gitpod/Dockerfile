FROM gitpod/workspace-full

WORKDIR ${HOME}
# make sure the build context is the root of the repo
COPY . ${HOME}/dotfiles
RUN ${HOME}/bootstrap.sh
