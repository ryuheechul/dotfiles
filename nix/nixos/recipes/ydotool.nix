{ ... }:

# check ../user.nix on mapping the group and this may require rebooting
# you may apply the change with no logout or reboot with `newgrp ydotool`
{
  programs.ydotool.enable = true;
}
