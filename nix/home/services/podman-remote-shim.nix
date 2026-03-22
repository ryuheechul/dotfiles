{ pkgs, ... }:

# This is for when you want to use podman machine but remotely instead of locally.
# Not good for when volume mounting on local path as the path would mismatch.

let
in
{
  # with the assumption of an ssh entry like below
  #
  # ```sshconfig
  #
  # # to make the service to authenticate via the auth sock
  # Host proxy-host-for-pm-machine pm-machine-on-remote
  #   IdentityAgent "/path/to/your/agent.sock"
  #
  # # this is the machine that hosts podman machine
  # Host proxy-host-for-pm-machine
  #   Hostname your.remote.host.that.hosts.podman.machine
  #
  # # this is the podman machine via proxy-host-for-pm-machine
  # Host pm-machine-on-remote
  #   Hostname 127.0.0.1
  #   # inside the podman machine on the remote host,
  #   # create a key text file at /root/.ssh/authorized_key.d
  #   # and add the ssh public key to use
  #   # in case to use ssh agent
  #   User root     # in case it's rootful
  #
  #   # It will be random, `podman machine inspect --format '{{.SSHConfig.Port}}'` to get the port
  #   port 36541
  #   ProxyJump proxy-host-for-pm-machine
  #   ControlMaster auto
  #   # debug with `ssh -O exit pm-machine-on-remote`
  #   ControlPath ~/.ssh/pmmor-master-%r@%h:%p
  #   # If to avoid making connection again within 24 hours
  #   ControlPersist 1440m # 24h
  # ```

  launchd.agents.podman-tunnel = {
    enable = true;
    config = {
      Label = "dotfiles.tunnel.podman";

      # The traffic comes in via STDIN/STDOUT thanks to inetdCompatibility
      # We pipe that into the remote socket via SSH + Netcat
      ProgramArguments = with pkgs; [
        "${openssh}/bin/ssh"
        "-q" # Quiet mode to prevent MOTD from breaking the binary stream
        "-o"
        "BatchMode=yes" # Prevents interactive prompts from hanging the agent
        "pm-machine-on-remote"
        "socat - UNIX-CONNECT:/run/podman/podman.sock"
      ];

      # run `podman system connection add via-remote unix:///tmp/pmm-via-remote/podman.sock` on this machine and `podman system connection list` to show and `podman system check` and `podman ps` to debug
      Sockets = {
        Listeners = {
          SockPathName = "/tmp/pmm-via-remote/podman.sock";
        };
      };
      RunAtLoad = false; # Activation only on-demand

      inetdCompatibility = {
        Wait = false; # This passes the connection handle to SSH's STDIN
      };

      StandardErrorPath = "/tmp/pmm-via-remote-podman-tunnel.err";
    };
  };


  # Usages:
  # - run `podman system connection add via-remote unix:///tmp/pmm-via-remote/podman.sock` on this machine
  #   - and `podman system connection list` to show
  #   - and `podman ps` to debug
  #     - FYI, `podman system check` somehow break things (even on the podman machine side)
  # - run `podman system connection default via-remote` in case it's not default
  # - create podman machine locally and set that one as default in case to use podman machine locally
  #   - and undo that when you stop using the local one

  # Troubleshoot commands:
  # - `rm /tmp/pmm-via-remote/podman.sock`
  # - `rm ~/.ssh/pmmor-master-root@127.0.0.1:36541`
  # - `launchctl list | grep dotfiles`
  # - `launchctl unload ~/Library/LaunchAgents/dotfiles.tunnel.podman.plist`
  #   - `launchctl load ~/Library/LaunchAgents/dotfiles.tunnel.podman.plist`
  # - `launchctl stop dotfiles.tunnel.podman`
  #   - `launchctl start dotfiles.tunnel.podman`
  # - reboot podman machine
}
