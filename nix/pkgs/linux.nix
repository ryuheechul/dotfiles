{ pkgs }:

let
  grace = import ./custom/grace.nix { pkgs = pkgs; };
in
with pkgs;(
  [ ]
  ++ lib.optionals stdenv.isLinux
    [
      iotop # A tool to find out the processes doing the most IO
      sysstat # A collection of performance monitoring tools for Linux (such as sar, iostat and pidstat)
      traceroute # Tracks the route taken by packets over an IP network
      unixtools.netstat
      nmap # A free and open source utility for network discovery and security auditing
      ed # An implementation of the standard Unix editor
      distrobox # Wrapper around podman or docker to create and start containers
      sshfs # FUSE-based filesystem that allows remote filesystems to be mounted over SSH
      # example:
      # - `sshfs remote-host:directory ~/mnt/target [-o reconnect]` # existing directories and files under will be shadowed
      # - `cd ~/mnt/target` # do stuff
      # - `cd -` # get out
      # - `fusermount -u ~/mnt/target`
      # - more on sshfs, https://www.redhat.com/sysadmin/sshfs
      # - there is also sshocker, see that at ./extra/default.nix
      usbimager # A very minimal GUI app that can write compressed disk images to USB drives
      grace # 🪛 It's strace, with colours.
    ]
)
