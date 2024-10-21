{ pkgs }:

with pkgs;
buildGoModule rec {
  pname = "grace";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "liamg";
    repo = "grace";
    rev = "v${version}";
    sha256 = "sha256-ZNQORv17O8FuT33TM59IotiTG8thw3Gy9UyiC6mX5qI=";
  };
  vendorHash = "sha256-jFwf2npUfQae4naufIMymQa0izlKOTrXskkk+zLS5Lw=";
}
