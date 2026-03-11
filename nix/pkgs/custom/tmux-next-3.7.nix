{ pkgs }:

# Override the package to use the latest master/nightly
pkgs.tmux.overrideAttrs (old: {
  src = pkgs.fetchFromGitHub {
    owner = "tmux";
    repo = "tmux";
    rev = "04b4952f0e761818c8fb75c1267e85f688c17498";
    sha256 = "sha256-KuP16gv9qaXpFtm4hwZls/7FZDC6fmPezryhPyHGNM4=";
  };
  # on March 2026
  version = "next-3.7";
})
