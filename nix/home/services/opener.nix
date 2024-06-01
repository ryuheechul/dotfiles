{ pkgs, ... }:

# see `../../../bin/path/ssh/xdg-open` for context

let
  opener = import ../../pkgs/custom/opener.nix { pkgs = pkgs; };
  opener-display0 = pkgs.writeShellScriptBin "opener" ''
    DISPLAY=:0 exec ${opener}/bin/opener
  '';
in
{
  # https://haseebmajid.dev/posts/2023-10-08-how-to-create-systemd-services-in-nix-home-manager/
  systemd.user.services.opener = {
    Unit = {
      Description = "Open URL in your local web browser from the SSH-connected remote environment";
    };
    Install = {
      # probably requires logout/login for the service to be active probably
      # due to "graphical-session" has already been started by the time you install this for the first time
      # or just manually start by `systemctl --user start opener.service` once
      # run `systemctl --user restart opener.service` to debug after rebuilding
      WantedBy = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${opener-display0}/bin/opener";
    };
  };
}
