FROM gitpod/workspace-full

# minimal added step to reduce startup time for workspace
WORKDIR /var/x-gitpod
ADD --chown=gitpod:gitpod https://raw.githubusercontent.com/ryuheechul/dotfiles/gitpod/Brewfile /var/x-gitpod/Brewfile
RUN brew bundle --verbose
