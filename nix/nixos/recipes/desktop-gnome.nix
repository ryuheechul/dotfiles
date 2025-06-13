{ config, pkgs, ... }:

# https://wiki.nixos.org/wiki/Category:Desktop_environment
# https://wiki.nixos.org/wiki/GNOME

let
  isSddmEnabled = config.services.displayManager.sddm.enable;
in
{
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = pkgs.lib.mkForce (
    if isSddmEnabled then false else true
  );
  services.xserver.desktopManager.gnome.enable = true;

  # https://wiki.nixos.org/wiki/KDE_Connect
  programs.kdeconnect = {
    enable = true;
    package = pkgs.lib.mkForce (
      if isSddmEnabled then
        pkgs.kdePackages.kdeconnect-kde
      else
        # https://www.reddit.com/r/gnome/comments/1798vk2/what_happened_to_gsconnect/
        pkgs.valent # Implementation of the KDE Connect protocol, built on GNOME platform libraries
    );
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # this is necessary for sections below at ../../home/dconf.nix
  # - "org/freedesktop/ibus/engine/hangul"
  # - "org/gnome/desktop/input-sources"
  # because `services.xserver.desktopManager.gnome.extraGSettingsOverrides` is for the system
  # but that will be disregarded by individual user's settings
  # hence do this via ../../home/dconf.nix
  i18n.inputMethod = {
    type = "ibus";
    enable = true;
    ibus.engines = with pkgs.ibus-engines; [ hangul ];
  };
  # if were to choose alternative to ibus (and ibus-hangul), I might consider https://github.com/Riey/kime
  # possibly via https://nix-community.github.io/home-manager/options.html#opt-i18n.inputMethod.kime.config

  # some links to read to understand what these are and for:
  # - https://gvolpe.com/blog/gnome3-on-nixos/
  # - https://hoverbear.org/blog/declarative-gnome-configuration-in-nixos/
  # - https://tedyin.com/posts/a-brief-intro-to-linux-input-method-framework/
  # - https://nixos.org/manual/nixos/stable/index.html#module-services-input-methods-ibus
  # - https://wiki.archlinux.org/title/IBus

  # some command to check/verify:
  # - 'printenv GTK_IM_MODULE'
  # - 'printenv QT_IM_MODULE'
  # - 'printenv XMODIFIERS'
  # - 'dconf dump / |less'
  # - 'dconf watch /'
  # - 'gsettings get org.gnome.desktop.input-sources sources'

  ##### Regarding Remote Login (read about it at ./rdp-server.nix) enabled via Gnome 46 #####
  #
  # - enable it via Settings > System > Remote Desktop > Remote Login
  # - username/password can be anything and it is not the same as actual users (it's only needed to establish connection)
  # - reboot seems to stop the service which can be mitigated via `systemctl start gnome-remote-desktop.service` if you can SSH into it
  # - Remmina app might work better than the default Connection app
  #
  # below is necessary if you connect directly (via IP address) instead of via something like Tailscale
  # ```
  # networking = {
  #   firewall = {
  #     enable = true;
  #     allowedTCPPorts = [ 3389 ];
  #   };
  # };
  #
  # and there is also ./sunshine.nix (at least read it for unlocking session)
  # ```
}
