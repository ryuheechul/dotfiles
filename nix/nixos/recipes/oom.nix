{ pkgs, config, ... }:

# new to using Linux for desktop OS and wondering why computer freezes when opening another (ram hungry) page in a browser?
# reading things below might help understand why and how to deal with it:
# - https://www.reddit.com/r/NixOS/comments/10o0sdp/computer_hangs_when_all_ram_is_used/
# - https://www.reddit.com/r/Fedora/comments/10s06fd/why_is_systemdoomd_still_a_thing/
# - https://github.com/hakavlad/nohang
# - https://www.reddit.com/r/Ubuntu/comments/vl2ak0/personal_observations_on_systemdoomd_vs_earlyoom/


{
  ### handling OOM

  # based on https://github.com/knopki/devops-at-home/blob/307921320d6147347e830d2c709f142b809d55b4/nixos/profiles/misc/earlyoom.nix

  # favoring earlyoom over systemd.oomd
  services.earlyoom = {
    enable = true;
    enableNotifications = true;
    freeMemThreshold = 10;
    freeSwapThreshold = 10;
    freeMemKillThreshold = 5;
    freeSwapKillThreshold = 5;
  };

  services.systembus-notify.enable = pkgs.lib.mkIf (config.services.earlyoom.enableNotifications) (pkgs.lib.mkDefault true);

  # I can test earlyoom with the one line python code below
  # this will very quickly fill up the memory; run it with your own risk
  # `python -c '[print(j) for j in [i for i in range(1_000_000_000)]]'`

  ### less likely OOM via swaps
  # - https://wiki.archlinux.org/title/Zram
  # - https://www.reddit.com/r/linuxquestions/comments/ju4bft/zram_zswap_or_both/
  # - https://www.reddit.com/r/linux/comments/11dkhz7/zswap_vs_zram_in_2023_whats_the_actual_practical/
  # - https://opensource.com/article/22/11/zram-swap-linux
  zramSwap.enable = true;

  # in addition, you can also enable swapfile to avoid memory pressure situation for longer
  # - https://nixos.wiki/wiki/Swap
  # - https://www.reddit.com/r/NixOS/comments/vbkpnf/how_do_i_add_my_swap_file_to_my/
}
