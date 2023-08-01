{ ... }:

{
  services.keyd.enable = true;
  services.keyd.settings = import ./keyd-settings.nix;
}
