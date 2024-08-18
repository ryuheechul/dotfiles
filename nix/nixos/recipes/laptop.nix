{ ... }:

# may work more efficiently with laptop/tablet devices
# https://nixos.wiki/wiki/Laptop
{
  # services.thermald.enable = true; # this is only for intel and it's being used at ./surface-pro-intel.nix
  services.auto-cpufreq.enable = true;
  # run `auto-cpufreq --stats` to see the effects

  services.power-profiles-daemon.enable = false; # to deal with the conflict with auto-cpufreq

  ##### "keep it alive as long as it's connected to power supply" #####
  #
  # lidSwitchExternalPower alone will not fulfill the purpose;
  # (in case of Gnome) go to Power > Automatic Suspend > (Uncheck Plugged In
  #
  # "ignore" means it ignores sleeping I guess
  # using this as gpd-wm2 seems to be a good device to act as a server
  # read: https://discourse.nixos.org/t/prevent-laptop-from-suspending-when-lid-is-closed-if-on-ac/12630
  services.logind.lidSwitchExternalPower = "lock"; # secure option instead of "ignore"
  # the above requires reboot to take effect or restart logind I guess
  # in reality when the power is unplugged, the logind will process the lid closing as if it's just closed at that moment of the power is unplugged but will this be prevented if the device is docked (for long) via `lidSwitchDocked`? - I need to figure that out
  # can debug with `systemctl status logind.service` and `cat /etc/systemd/logind.conf`
  # read about it at https://www.freedesktop.org/software/systemd/man/latest/logind.conf.html
  # remains unanswered: is there a way to "wake up" again when power is back on
  # NOTE: regardless of "lock" or "ignore", if no graphical login happens after a reboot within 15 minutes or so, it will start suspending, so make sure to login after a reboot to prevent suspending
}
