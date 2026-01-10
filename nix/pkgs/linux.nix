{ pkgs }:

let
  checkEnv = import ../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  ifX86 = pkgs.lib.optionals pkgs.stdenv.isx86_64;
  grace = import ./custom/grace.nix { pkgs = pkgs; };
  isLinux = with pkgs; lib.optionals stdenv.isLinux;
  for-linux =
    with pkgs;
    (
      [
        apx # Vanilla OS package manager
        iotop # A tool to find out the processes doing the most IO
        sysstat # A collection of performance monitoring tools for Linux (such as sar, iostat and pidstat)
        traceroute # Tracks the route taken by packets over an IP network
        unixtools.netstat
        bridge-utils # Userspace tool to configure linux bridges (deprecated in favour or iproute2)
        nmap # A free and open source utility for network discovery and security auditing
        ed # An implementation of the standard Unix editor
        toolbox # Tool for containerized command line environments on Linux
        distrobox # Wrapper around podman or docker to create and start containers
        # FYI, waypipe actually seems to be slow (at the time of writing) on its own even with a local socket like below without any network overhead (e.g. ssh)
        # I didn't know this and I tried to increase the speed between two machines and only later I tried sshing to localhost and it was still slow with much faster network (~40 Gbits/sec - similar lagginess compare to ~200 Mbits/sec) that's when I tried it without any network overhead like below
        # ```
        # waypipe --socket /tmp/waypipe-test.sock client # in one terminal
        # waypipe --socket /tmp/waypipe-test.sock server [your-gui-app] # in another
        # ````
        waypipe # Network proxy for Wayland clients (applications)
        sshfs # FUSE-based filesystem that allows remote filesystems to be mounted over SSH
        # example:
        # - `sshfs remote-host:directory ~/mnt/target [-o reconnect]` # existing directories and files under will be shadowed
        # - `cd ~/mnt/target` # do stuff
        # - `cd -` # get out
        # - `fusermount -u ~/mnt/target`
        # - more on sshfs, https://www.redhat.com/sysadmin/sshfs
        # - there is also sshocker, see that at ./extra/default.nix
        bluetuith # TUI-based bluetooth connection manager
        sysz # Fzf terminal UI for systemctl
      ]

      ++ ifX86 [
        usbimager # A very minimal GUI app that can write compressed disk images to USB drives
        grace # 🪛 It's strace, with colours.
      ]

      ++ ifEnv "MY_NIX_EXTRA_LINUX_HOTSPOT" [
        # https://nixos.wiki/wiki/Internet_Connection_Sharing
        # Watch out for some hardware (e.g. Intel) not working very well with AP mode - https://www.reddit.com/r/debian/comments/10u76li/hostapd_with_wifi_6e_card/
        linux-wifi-hotspot # Feature-rich wifi hotspot creator for Linux which provides both GUI and command-line interface
        linux-router # Set Linux as router / Wifi hotspot / proxy in one command
      ]
    );
in
[ ] ++ isLinux for-linux
