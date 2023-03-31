{
  # use nix-direnv to speed up `use nix` in .envrc files
  programs.direnv = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.direnv.enable
    enable = true;
    enableZshIntegration = false; # since I do this manually
    nix-direnv.enable = true;
  };
}
