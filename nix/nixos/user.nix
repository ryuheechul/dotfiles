{ username, pkgs }:
{
  isNormalUser = true;
  description = "user ${username}";
  extraGroups = [ "networkmanager" "wheel" ];
  packages = [ ]; # since I already manage my packages via ../pkgs
  # if want a package that is NixOS specific look at ./system-pkgs.nix
  shell = pkgs.zsh;
}
