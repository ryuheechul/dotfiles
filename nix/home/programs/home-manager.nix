{
  # Let Home Manager install and manage itself.
  programs.home-manager = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.home-manager.enable
    enable = true;
  };
}
