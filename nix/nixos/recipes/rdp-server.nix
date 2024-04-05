{ pkgs, ... }:

{
  services.xrdp.enable = true;
  services.xrdp.openFirewall = true;
  services.xrdp.defaultWindowManager = "${pkgs.gnome.gnome-session}/bin/gnome-session";

  # use a client like `remmina` to connect
}
