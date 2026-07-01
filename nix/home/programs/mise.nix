{ pkgs, ... }:

{
  programs.mise = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.mise.enable
    enable = true;
    enableZshIntegration = false; # since I do this manually
    # nixpkgs lags upstream mise releases; pin directly via ../../pkgs/custom/mise.nix instead
    package = pkgs.callPackage ../../pkgs/custom/mise.nix { };
  };
}
