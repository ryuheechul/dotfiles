{ config, pkgs, ... }:

# this basically doing a similar thing as ../../../bin/path/darwin/react-to-appearance-changes
# may require to run this `systemctl --user start tmux-watch-theme-change.service`
let
  args = [
    "${config.programs.zsh.package}/bin/zsh"
    "-c"
    "base16-shell-auto-reload-on-tmux"
  ];
in
{

  # config = {
  systemd.user.services = {
    # twtc = {
    tmux-watch-theme-change = {
      Unit = {
        Description = "It watches theme changes and trigger repaint on tmux";
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
  # };
}
