# this is to support the case when it's running as a VM via QEMU
# see also at ./vm-guest-utm.nix
{ ... }:

{
  imports = [
    ./chrony-step.nix
  ];

  # https://docs.getutm.app/guest-support/linux/#qemu-agent
  services.qemuGuest.enable = true;
}
