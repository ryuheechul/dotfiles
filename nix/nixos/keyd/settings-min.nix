# - to be used for ../recipes/keyd-guest.nix
# - you may debug with `sudo showkey` but there should should be better way too
#   - or with `sudo libinput debug-events --show-keycodes`

let
  meta_mac = import ./meta-mac.nix;
in
{
  main = {
    meta = "layer(meta_mac)";
  };

  # meta_mac modifier layer; inherits from 'Meta(Super)' modifier layer
  # (not inheriting from 'Ctrl' modifier causes to define a lot of keys
  #  but still this works for me while inheriting from `Ctrl` has an issue
  #  with app switching)
  #
  # The main part! Using this layer, we can remap our new "Cmd" key to
  # do almost everything our muscle memory might need...
  "meta_mac:M" = meta_mac;
}
