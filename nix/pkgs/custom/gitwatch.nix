{ pkgs }:

with pkgs;
stdenv.mkDerivation rec {
  name = "gitwatch";
  version = "f895a5db275d8c49674961e42fa02e24f19db017";

  src = fetchFromGitHub {
    owner = "gitwatch";
    repo = "gitwatch";
    rev = "f895a5db275d8c49674961e42fa02e24f19db017";
    sha256 = "sha256-a2aNT5fuFccDJ+iMhdMAPKBYCN032uhZpWfLiuPiVvY=";
  };

  phases = "installPhase";

  installPhase = ''
    mkdir -p $out/share
    mkdir -p $out/bin
    cp -r ${src} -p $out/share/gitwatch
    ln -s $out/share/gitwatch/gitwatch.sh $out/bin/gitwatch
  '';

  # also install `fswatch` as this package depends on it on darwin
}
