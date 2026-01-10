{ pkgs }:

# exp.nix:
# - for experimenting
# - to be source via ./default.nix

# Inspired by tools from some sources like this, https://github.com/rothgar/awesome-tuis
# but not enough usages to justify to be included by default

let
  checkEnv = import ../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  ifX86 = pkgs.lib.optionals pkgs.stdenv.isx86_64;
  grace = import ./custom/grace.nix { pkgs = pkgs; };
  isLinux = with pkgs; lib.optionals stdenv.isLinux;
  for-linux = with pkgs; ([
    kmon # Linux Kernel Manager and Activity Monitor
    # make sure these are linux only, if not move to ./default.nix or ./extra instead
    netscanner # Network scanner with features like WiFi scanning, packetdump and more
    # there is also https://github.com/pythops/oryx
    s-tui # Stress-Terminal UI monitoring tool
    ttop # Top-like system monitoring tool
    # euporie
    # diskonaut # Terminal disk space navigator
    # INFO: create something like .extra/exp.nix to contain experimental ones (so that it can be easily committed without verified to be actually useful yet)
    # add add some comments like this to help discover more tools quickly - discover more at https://github.com/rothgar/awesome-tuis
    # and also my explain how to run none nix programs (in nixos and other platforms?)
  ]);
  for-any = with pkgs; [
    wtfutil # Personal information dashboard for your terminal
  ];
in
with pkgs;
for-any ++ isLinux for-linux
