{ ... }:

{
  virtualisation.libvirtd.enable = true;

  users.groups.libvirtd.members = [ "root" ];
  users.groups.qemu-libvirtd.members = [ "qemu-libvirtd" ];
}
