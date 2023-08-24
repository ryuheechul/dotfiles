# - to be used for ../recipes/keyd-host.nix
# - you may debug with `sudo showkey` but there should should be better way too
#   - or with `sudo libinput debug-events --show-keycodes`

let
  meta_mac = import ./meta-mac.nix;
in
{
  space_hjkl = {
    h = "left";
    j = "down";
    k = "up";
    l = "right";
  };

  main = {
    # Basic use of capslock as a dual function key:
    #
    # - when 'capslock' is tapped (pressed + released), it behaves as ESC key
    # - when 'capslock' is held, and used in combination with another key, it
    #   behaves like a 'ctrl' key modifier (just like xcape)
    capslock = "overload(control, esc)";

    # use space bar like a modifier so hjkl can work as arrow keys
    space = "overload(space_hjkl, space)";

    # Mac-Like Configuration Example
    #
    # Uses "Alt" button to the left of spacebar as "Cmd" key
    # based on https://github.com/rvaiya/keyd/blob/master/examples/macos.conf
    # but currently tailed to the physical keyboard layout of Surface pro 8 (on Gnome)
    #
    # Note:
    #   This 'trick' generally requires that the press+release of the Meta
    #   key will do nothing. On my system, I had to disable the "overlay-key"
    #   in mutter to make it inert:
    #     - `gsettings set org.gnome.mutter overlay-key ''`

    # Create a new "Cmd" button, with various Mac OS-like features below
    leftalt = "overload(meta_mac, leftmeta)";
    rightalt = "overload(meta_mac, leftmeta)"; # rightmeta seems not working so fallback to leftmeta

    # Swap meta/alt
    meta = "overload(option_mac, leftalt)";

    # Switch to next input source
    leftcontrol = "macro(M-space)";
  };

  # meta_mac modifier layer; inherits from 'Alt' modifier layer
  # (not inheriting from 'Ctrl' modifier causes to define a lot of keys
  #  but still this works for me while inheriting from `Ctrl` has an issue
  #  with app switching)
  #
  # The main part! Using this layer, we can remap our new "Cmd" key to
  # do almost everything our muscle memory might need...
  "meta_mac:A" = meta_mac;

  # option_mac modifier layer; inherits from 'Alt' modifier layer
  "option_mac:M" = { };
}
