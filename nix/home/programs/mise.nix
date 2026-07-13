{ pkgs, ... }:

let
  # nixpkgs' mise is binary-cached but can lag upstream; the custom one pins
  # the latest release but builds from source (slow, and heavy enough to OOM on
  # small machines). Default to nixpkgs; flip this on only when the lag bites.
  useCustomMise = false;
  customMise = pkgs.callPackage ../../pkgs/custom/mise.nix { };
  mise = if useCustomMise then customMise else pkgs.mise;
in
{
  programs.mise = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.mise.enable
    enable = true;
    enableZshIntegration = false; # since I do this manually
    package = mise;
  };
}
