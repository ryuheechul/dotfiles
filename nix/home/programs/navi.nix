{
  programs.navi = {
    enable = true;
    # https://nix-community.github.io/home-manager/options.html#opt-programs.navi.settings
    settings = {
      shell = {
        command = "zsh";
      };
    };
  };
}
