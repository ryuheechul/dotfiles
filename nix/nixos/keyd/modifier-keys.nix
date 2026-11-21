# Hardware-specific Command/Option (meta/alt) bindings for ./settings-full.nix
# - "pc": the key next to the spacebar is Alt and the one next to it is Meta,
#   so swap them (tailored to the physical layout of Surface Pro 8 on Gnome)
# - "mac": Command is already meta and Option is already alt, so no swap is needed

{ hardware, meta_mac }:

if hardware == "pc" then {
  # Create a new "Cmd" button, with various Mac OS-like features below
  leftalt = "overload(meta_mac, leftmeta)";
  rightalt = "overload(meta_mac, leftmeta)"; # rightmeta seems not working so fallback to leftmeta

  # Swap meta/alt
  meta = "overload(option_mac, leftalt)"; # alias covering both leftmeta and rightmeta
} else if hardware == "mac" then {
  # Command is already the key next to the spacebar
  leftmeta = "overload(meta_mac, leftmeta)";
  rightmeta = "overload(meta_mac, leftmeta)"; # fallback to leftmeta, same as above

  # Option keeps the macOS-like option_mac layer
  leftalt = "overload(option_mac, leftalt)";
  rightalt = "overload(option_mac, leftalt)";
} else
  abort "unsupported hardware: ${hardware}"
