{ pkgs, ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
    ./chrony-step.nix
    ./laptop.nix
    # to mimic launchpad gesture from macOS:
    # - note that it's customized for highly specific environment with assumptions like:
    #   - on Gnome Desktop
    #   - triggering specific shortcut to invoke opening Activities
    ./mlg.nix
  ];

  # works only for intel
  services.thermald.enable = true;

  # improve pen experience
  services.iptsd.config.Touchscreen = {
    DisableOnPalm = true;
    DisableOnStylus = true;
  };

  environment.systemPackages = [
    pkgs.nvtopPackages.intel
  ];
}
