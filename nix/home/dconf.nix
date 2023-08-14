# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
# like `dconf dump / | dconf2nix`
# and obviously this is for linux not for darwin
{ lib, ... }:

with lib.hm.gvariant; {
  dconf.settings = {
    # relies on below from ../nixos/recipes/desktop-gnome.nix
    # ```
    # i18n.inputMethod = {
    #   enabled = "ibus";
    #   ibus.engines = with pkgs.ibus-engines; [ hangul ];
    # };
    # ```
    "org/freedesktop/ibus/engine/hangul" = {
      auto-reorder = true;
      hangul-keyboard = "2";
      hanja-keys = "Hangul_Hanja,F9";
      initial-input-mode = "hangul";
      switch-keys = "Hangul,Shift+space";
      word-commit = false;
    };

    # this also relies on ibus configuration above
    "org/gnome/desktop/input-sources" = {
      sources = [ (mkTuple [ "xkb" "us" ]) (mkTuple [ "ibus" "hangul" ]) ];
      per-window = false;
      show-all-sources = true;
    };
    # "org/freedesktop/ibus/engine/hangul"
    # "org/gnome/desktop/input-sources"
    # above two settings basically automate the process below
    # - install gnome.gnome-tweaks via https://search.nixos.org/packages?channel=23.05&show=gnome.gnome-tweaks&from=0&size=50&sort=relevance&type=packages&query=gnome-tweaks
    # - and open tweaks app and turn on `Show Extended Input Sources` and reboot
    # - and go back to gnome settings and add `Korean (Hangul)` and go to preference of that
    # - and check `Start in Hangul mode`

    # display
    "org/gnome/desktop/interface" = {
      cursor-size = 32;
      font-antialiasing = "grayscale";
      show-battery-percentage = true;
      text-scaling-factor = 1.25;
    };

    # touchpad
    "org/gnome/desktop/peripherals/touchpad" = {
      click-method = "areas";
      speed = 0.25;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };
  };
}
