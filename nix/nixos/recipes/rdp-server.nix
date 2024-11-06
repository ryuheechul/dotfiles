{ pkgs, ... }:

# NOTE: with Gnome, this should be favored instead, https://release.gnome.org/46/#:~:text=Remote%20Login%20with%20RDP
# read more at ./desktop-gnome.nix

# but also read this about the limitation of current typical RDP setups - https://www.reddit.com/r/linux/comments/wrq4zh/why_remote_desktop_applications_lack_wayland/

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
# ./sunshine.nix can augment the experience of "remote desktop"
