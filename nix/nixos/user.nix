{ username, pkgs, config }:
let
  groups = [ ]
    ++ pkgs.lib.optionals config.virtualisation.docker.enable [ "docker" ]
    ++ pkgs.lib.optionals config.virtualisation.libvirtd.enable [ "libvirtd" "qemu-libvirtd" ]
    ++ pkgs.lib.optionals config.services.davfs2.enable [ "davfs2" ];
in
with pkgs.lib;{
  isNormalUser = mkForce true;
  description = mkForce "user is ${username}";
  # NOTE regarding groups:
  # - changes on groups can be seen right away with `cat /etc/group` but not with `groups`
  # - this will take affect at login time which may require rebooting
  # - https://stackoverflow.com/a/7537275/1570165
  extraGroups = mkForce ([ "networkmanager" "wheel" ] ++ groups);
  packages = mkMerge [ ]; # since I already manage my packages via ../pkgs
  # if want a package that is NixOS specific look at ./system-pkgs.nix
  shell = mkForce pkgs.zsh;
}
