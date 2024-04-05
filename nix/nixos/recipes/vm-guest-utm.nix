# this is to support the case when it's running as a VM via UTM (powered by QEMU)
{ ... }:

{
  # https://docs.getutm.app/guest-support/linux/#qemu-agent
  services.qemuGuest.enable = true;
  # https://docs.getutm.app/guest-support/linux/#spice-agent
  services.spice-vdagentd.enable = true;
  # https://docs.getutm.app/guest-support/linux/#spice-webdav
  services.spice-webdavd.enable = true;
  # e.g. in a Gnome desktop environment, open Files, click other locations
  # and enter `dav://localhost:9843` to Connect to Server and click Connect
  # (Note that this is not very fast)

  # trusting the incoming traffic from host
  # this still requires individual apps to open on 0.0.0.0 not 127.0.0.1
  networking.firewall.trustedInterfaces = [
    # the interface name below may not match the expected interface
    "enp0s1"
  ];
}
