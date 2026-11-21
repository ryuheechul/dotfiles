{ ... }:

# appropriate when nixos is the host OS on a Mac
# and no key mappings are done already
# unlike ./keyd-hardware-pc.nix, Command and Option are physically flipped
#
# alternative: comment out this module's import in mix-and-match.nix and use
# this instead (uncomment the block below):
# {
#   boot.kernelParams = [ "hid_apple.swap_opt_cmd=1" ];
# }
# (https://github.com/NixOS/nixos-hardware/tree/master/apple)
# the kernel then presents a PC-like layout, so pair it with
# ./keyd-hardware-pc.nix if you still want keyd's other mappings, or use no
# keyd recipe at all. do not combine it with this module (double swap).

{
  services.keyd.enable = true;
  services.keyd.keyboards.default.settings = import ../keyd/settings-full.nix { hardware = "mac"; };
}
