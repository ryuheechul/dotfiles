{ username, pkgs }:
with pkgs.lib;{
  isNormalUser = mkForce true;
  description = mkForce "user is ${username}";
  extraGroups = mkForce [ "networkmanager" "wheel" ];
  packages = mkForce [ ]; # since I already manage my packages via ../pkgs
  # if want a package that is NixOS specific look at ./system-pkgs.nix
  shell = mkForce pkgs.zsh;
}
