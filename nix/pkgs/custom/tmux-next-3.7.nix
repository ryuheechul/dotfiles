{ pkgs }:

# Override the package to use the latest master/nightly
pkgs.tmux.overrideAttrs (old: {
  src = pkgs.fetchFromGitHub {
    owner = "tmux";
    repo = "tmux";
    rev = "bbbfa8f3603177e41fd1369d09de201ee8fff814";
    sha256 = "sha256-mak1x2jglpdiW73CAL0kPMXiz/+OiR1TijaRYIxczx8=";
  };
  # on March 2026
  version = "next-3.7";
})
