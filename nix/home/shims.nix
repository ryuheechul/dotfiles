{ pkgs, ... }:

# also see these files:
# - ../../nvim/lua/boot/lazy.lua
# - ../../nvim/lua/utils/nixos-shim.lua
{
  # -- may need to re-run `TSInstall all` if the path ever changed
  home.file."./.local/share/nvim/my-local-lazy/nvim-treesitter/" = {
    recursive = true;
    source = pkgs.vimPlugins.nvim-treesitter.withAllGrammars;
  };
}
