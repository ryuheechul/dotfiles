{ username, pkgs, config }:
let
  groups = [ ]
    ++ pkgs.lib.optionals config.virtualisation.docker.enable [ "docker" ]
    ++ pkgs.lib.optionals config.virtualisation.libvirtd.enable [ "libvirtd" "qemu-libvirtd" ];
in
with pkgs.lib;{
  isNormalUser = mkForce true;
  description = mkForce "user is ${username}";
  extraGroups = mkForce ([ "networkmanager" "wheel" ] ++ groups);
  packages = mkMerge [ ]; # since I already manage my packages via ../pkgs
  # if want a package that is NixOS specific look at ./system-pkgs.nix
  shell = mkForce pkgs.zsh;
}
