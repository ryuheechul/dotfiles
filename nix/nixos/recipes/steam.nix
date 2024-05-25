{ ... }:

{
  # https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
  };
  # when the game seem to struggle make sure the performance mode is set
  # https://github.com/linux-surface/linux-surface/issues/1020#issuecomment-1365528473
  # and see if there is a way to automate toggling depends on which app is being used
}
