{ ... }:

{
  # copied from https://discourse.nixos.org/t/stop-pc-from-sleep/5757/2
  # Disable the GNOME3/GDM auto-suspend feature that when it cannot be disabled in GUI!
  # but in my case of disabling in Gnome GUI didn't prevent all cases of suspend/hibernate
  # where enabling below haven't failed at preventing them
  systemd.targets.sleep.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.hybrid-sleep.enable = false;
}
