{ pkgs, ... }:

{
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # this is necessary for sections below at ../../home/dconf.nix
  # - "org/freedesktop/ibus/engine/hangul"
  # - "org/gnome/desktop/input-sources"
  # because `services.xserver.desktopManager.gnome.extraGSettingsOverrides` is for the system
  # but that will be disregarded by individual user's settings
  # hence do this via ../../home/dconf.nix
  i18n.inputMethod = {
    enabled = "ibus";
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
}
