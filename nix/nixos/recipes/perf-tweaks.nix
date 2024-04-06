{ ... }:

# ./oom.nix prevents freezing due to OOM
# and this file is to tweak performance mostly related memory
# some commands to see changes?:
# - `vmstat`
# - `cat /proc/meminfo`

# NOTE:
# - applying/restoring   may require reboot
#   - restoring can be done by commenting things out
# - reboot may hiccup especially right after restoring
#   - rebooting again may resolve the issue
{
  # taking the recommendation from https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#swappiness
  # debug with `cat /proc/sys/vm/swappiness`
  boot.kernel.sysctl."vm.swappiness" = 1;

  # https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#compaction-proactiveness
  # debug with `cat /proc/sys/vm/compaction_proactiveness`
  boot.kernel.sysctl."vm.compaction_proactiveness" = 0;

  # https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#page-lock-unfairness
  # debug with `cat /proc/sys/vm/page_lock_unfairness`
  boot.kernel.sysctl."vm.page_lock_unfairness" = 1;

  # https://github.com/Zumorica/GradientOS/blob/f5320a1a1fc9bab4b80645d7ac1577ed76adbd80/modules/kernel/memory.nix
  systemd.tmpfiles.settings."10-gradientos-hugepages.conf" = {
    # https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#transparent-hugepages
    # debug with `cat /sys/kernel/mm/transparent_hugepage/enabled`
    "/sys/kernel/mm/transparent_hugepage/enabled".w = {
      argument = "always";
    };
    # https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#hugepage-defragmentation
    # debug with `cat /sys/kernel/mm/transparent_hugepage/khugepaged/defrag`
    "/sys/kernel/mm/transparent_hugepage/khugepaged/defrag".w = {
      argument = "0";
    };
    # https://github.com/CryoByte33/steam-deck-utilities/blob/main/docs/tweak-explanation.md#shared-memory-in-transparent-hugepages
    # debug with `cat /sys/kernel/mm/transparent_hugepage/shmem_enabled`
    "/sys/kernel/mm/transparent_hugepage/shmem_enabled".w = {
      argument = "advise";
    };
  };
}
