# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
# like `dconf dump / | dconf2nix`
# and obviously this is for Linux only not for Darwin
{ lib, ... }:

with lib.hm.gvariant;
{
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

    # what's the difference between this one and the system level ones at `../nixos/recipes/desktop-gnome.nix`?
    # self guessing for above and confirmed with personal experiments: impact on the user session
    "org/gnome/mutter" = {
      # https://github.com/GNOME/mutter/blob/main/data/org.gnome.mutter.gschema.xml.in
      experimental-features = [
        # scaling being handled in the screen/monitor level instead of individual apps:
        # - https://www.reddit.com/r/gnome/comments/11ekj8o/what_is_the_state_of_fractional_scaling_on_gnome/
        "scale-monitor-framebuffer" # can resolve X11 apps text not following the scaling issue but blurring can happen
        # turn below on for crisp but potentially wrong scale and turn it off if the right scaling is worth dealing with blurriness
        # "xwayland-native-scaling" # enabling this prevent blurring but it will "ignore" the scaling (small texts!!)
        "variable-refresh-rate"
      ];
      # debug with `gsettings get org.gnome.mutter experimental-features`
    };

    # this also relies on ibus configuration above
    "org/gnome/desktop/input-sources" = {
      sources = [
        (mkTuple [
          "xkb"
          "us"
        ])
        (mkTuple [
          "ibus"
          "hangul"
        ])
      ];
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

    # keyboard shortcuts
    # there is also ../nixos/keyd/meta-mac.nix
    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Alt>q" ];
    };

    "org/gnome/shell/keybindings" = {
      screenshot = [ "<Shift><Alt>3" ];
      "show-screenshot-ui" = [ "<Shift><Alt>4" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      screensaver = [ "<Control><Alt>q" ];
      search = [ "<Alt>space" ]; # for a compatibility with macOS spotlight like experience (even though Alt alone does the same already)
    };

    # gnome shell
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        # https://search.nixos.org/packages?show=gnomeExtensions.pop-shell
        "pop-shell@system76.com"
        # https://search.nixos.org/packages?show=gnomeExtensions.night-theme-switcher
        "nightthemeswitcher@romainvigier.fr"
        # https://extensions.gnome.org/extension/6048/peek-top-bar-on-fullscreen/
        "peek-top-bar-on-fullscreen@marcinjahn.com"
        # https://extensions.gnome.org/extension/841/freon/
        "system-monitor@gnome-shell-extensions.gcampax.github.com"
      ];
    };

    "org/gnome/shell/extensions/nightthemeswitcher/commands" = {
      enabled = true;
      sunrise = "zsh -c light";
      sunset = "zsh -c dark";
    };

    "org/gnome/shell/extensions/nightthemeswitcher/time" = {
      sunrise = 6.0;
      sunset = 20.0;
    };

    # pop-shell
    "org/gnome/shell/extensions/pop-shell" = {
      active-hint = false;
      active-hint-border-radius = mkUint32 5;
      fullscreen-launcher = true;
      gap-inner = mkUint32 2;
      gap-outer = mkUint32 2;
      show-title = false;
      tile-by-default = false;
      # custom shortcuts - https://www.reddit.com/r/pop_os/comments/g4t5vt/how_to_change_shell_keyboard_shortcuts/
      # https://github.com/pop-os/shell/blob/master_jammy/schemas/org.gnome.shell.extensions.pop-shell.gschema.xml
      activate-launcher = [ "<Alt>slash" ];
    };
  };
}
