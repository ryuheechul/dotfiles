{ pkgs }:

# see `../../home/services/opener.nix` for usage

with pkgs;
buildGoModule rec {
  pname = "opener";
  version = "0.1.4";

  src = fetchFromGitHub {
    owner = "superbrothers";
    repo = "opener";
    rev = "v${version}";
    sha256 = "sha256-SU7Yn9PwZ6MtNRI8mZ6sHlW/sCvxR8yLjvXIK3aB1gk=";
  };
  vendorHash = "sha256-Ey1j7CRkKGtgTw962BKS6j1EawcEr8H9B92JIIv45iM=";
}
