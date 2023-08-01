# assuming it's running as a VM via QEMU
{ ... }:

{
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;
}
