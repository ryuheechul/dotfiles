{ pkgs }:

with pkgs;
stdenv.mkDerivation rec {
  name = "tf-helper";
  version = "0.2.9";

  src = fetchFromGitHub {
    owner = "hashicorp-community";
    repo = "tf-helper";
    rev = "5b106a5dc2ec1e1a613ef555690955433d538652";
    sha256 = "wXwNlzYHxyFjCeA0yRxvn/WHMNUz9nahRX/5FhYeVCs=";
  };

  phases = "installPhase";

  installPhase = ''
    mkdir -p $out/share
    cp -r ${src} -p $out/share/tf-helper
    ln -s $out/share/tf-helper/tfh/bin $out/bin
  '';
}
