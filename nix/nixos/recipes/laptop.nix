{ ... }:

# may work more efficiently with laptop/tablet devices
# https://nixos.wiki/wiki/Laptop
{
  # services.thermald.enable = true; # this is only for intel and it's being used at ./surface-pro-intel.nix
  services.auto-cpufreq.enable = true;
  # run `auto-cpufreq --stats` to see the effects

  services.power-profiles-daemon.enable = false; # to deal with the conflict with auto-cpufreq
}
