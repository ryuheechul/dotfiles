{ ... }:

# appropriate when nixos is a guest vm and most of key mapping are done in host side
# probably via something like ../../../karabiner

{
  services.keyd.enable = true;
  services.keyd.keyboards.vm.settings = import ../keyd/settings-min.nix;
}
