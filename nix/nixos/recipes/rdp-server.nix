{ pkgs, ... }:

# https://nixos.wiki/wiki/Remote_Desktop
{
  services.xrdp = {
    enable = true;
    openFirewall = true;
    defaultWindowManager = "${pkgs.gnome.gnome-session}/bin/gnome-session";
  };

  # use a client like `remmina` to connect
}

# actually https://wiki.gnome.org/Projects/Mutter/RemoteDesktop seems to work better for gnome environment
# "Settings > System > Desktop" Sharing to configure
# `grdctl status` to debug
