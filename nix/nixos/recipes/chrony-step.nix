{ ... }:

# Use this to deal with unstable rtc:
# e.g. 1 SP8: https://github.com/linux-surface/linux-surface/issues/1497
# e.g. 2 VM: https://chrony-project.org/faq.html#_is_chronyd_allowed_to_step_the_system_clock
{
  services.chrony.extraConfig = ''
    makestep 1 -1
  '';
  # `makestep 1 -1`: continuously step even when there is drift of 1 sec
}
