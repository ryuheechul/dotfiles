{ pkgs }:

with pkgs;(
  [ ]
  ++ lib.optionals stdenv.isLinux
    [
      iotop # A tool to find out the processes doing the most IO
      sysstat # A collection of performance monitoring tools for Linux (such as sar, iostat and pidstat)
      traceroute # Tracks the route taken by packets over an IP network
    ]
)
