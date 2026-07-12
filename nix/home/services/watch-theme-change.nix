{ config, pkgs, ... }:

# runs the watchers of "One tone, every layer" - ../../../docs/mechanics.md
# may require to run this `systemctl --user start watch-theme-change.service`
# debug with `systemctl --user cat watch-theme-change.service`
let
  args = [
    "${config.programs.zsh.package}/bin/zsh"
    "-c"
    "base16-shell-auto-reload"
  ];
in
{
  # for Linux
  systemd.user.services = {
    watch-theme-change = {
      Unit = {
        Description = "It watches theme changes and trigger repaint on tmux and herdr";
        After = [ "basic.target" ];
      };

      Service = {
        ExecStart = pkgs.lib.escapeShellArgs args;
        Restart = "on-failure";

        # Sandboxing - which is simply mimicking the one from syncthing without really understanding super well yet - works fine for now and I will revisit later
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateUsers = true;
        RestrictNamespaces = true;
        SystemCallArchitectures = "native";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };

  # for Darwin
  # debug with `tail -f /private/var/log/com.apple.xpc.launchd/launchd.log`
  launchd.agents = {
    # this will update the ~/.base16_theme.updated-time
    # debug with `launchctl print gui/$(id -u)/org.nix-community.home.base16-shell-to-follow-system-appearance|less`
    base16-shell-to-follow-system-appearance =
      let
        args = [
          "${config.programs.zsh.package}/bin/zsh"
          "-c"
          "base16-shell-to-follow-system-appearance"
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

    # this will react to the ~/.base16_theme.updated-time
    # debug with `launchctl print gui/$(id -u)/org.nix-community.home.watch-theme-change|less`
    watch-theme-change = {
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
