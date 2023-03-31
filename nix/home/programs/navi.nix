{
  programs.navi = {
    enable = true;
    enableZshIntegration = false; # since I do this manually
    # https://nix-community.github.io/home-manager/options.html#opt-programs.navi.settings
    settings = {
      shell = {
        command = "zsh";
      };
    };
  };
}
