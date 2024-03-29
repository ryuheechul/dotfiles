{ pkgs }:

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
    ]
)
