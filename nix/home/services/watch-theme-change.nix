{ config, ... }:

# macOS appearance watcher of "One tone, every layer" - ../../../docs/mechanics.md
#
# This used to also run the entr-based theme watcher (the Linux systemd
# service and the launchd watch-theme-change agent) that repainted running
# tmux/herdr subscribers; tinty's global hooks now do those repaints on every
# `tinty apply` (../../../tinted-theming/tinty/config.toml), so all that's
# left here is subscribing to the system appearance on macOS.
# Linux needs no agent either: GNOME's night-theme-switcher runs `light`/
# `dark` directly (../../../nix/home/dconf.nix).
#
{
  launchd.agents = {
    # this will update the ~/.active-theme.updated-time
    # debug with `launchctl print gui/$(id -u)/org.nix-community.home.theme-follow-system-appearance|less`
    # and `tail -f /private/var/log/com.apple.xpc.launchd/launchd.log`
    theme-follow-system-appearance =
      let
        args = [
          "${config.programs.zsh.package}/bin/zsh"
          "-c"
          "theme-follow-system-appearance"
        ];
      in
      {
        enable = true;
        config = {
          ProgramArguments = args;
          KeepAlive = {
            Crashed = true;
            SuccessfulExit = false;
          };
          RunAtLoad = true;
          ProcessType = "Background";
        };
      };
  };
}
