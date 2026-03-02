{ pkgs }:

# Override the package to use the latest master/nightly
pkgs.tmux.overrideAttrs (old: {
  src = pkgs.fetchFromGitHub {
    owner = "tmux";
    repo = "tmux";
    # for next-3.7 on March 2026
    rev = "bbbfa8f3603177e41fd1369d09de201ee8fff814";
    sha256 = "sha256-mak1x2jglpdiW73CAL0kPMXiz/+OiR1TijaRYIxczx8=";
  };
})
