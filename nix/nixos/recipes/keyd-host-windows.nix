{ ... }:

# appropriate when nixos is the host OS on a machine and no key mappings are done already
# like the case of ./keyd-guest.nix

{
  services.keyd.enable = true;
  services.keyd.keyboards.default.settings = import ../keyd/settings-full-windows.nix;
}
