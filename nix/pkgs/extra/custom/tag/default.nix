{ pkgs }:

with pkgs;
buildGoPackage rec {
  pname = "tag";
  version = "1.4.0";

  goPackagePath = "github.com/aykamko/tag";

  src = fetchFromGitHub {
    owner = "aykamko";
    repo = "tag";
    rev = "v${version}";
    sha256 = "1zy78621wgncbhiyahjbn7jvihcjy8ajbiqv9j5jg7pmnfrmkn9n";
  };

  goDeps = ./deps.nix;
}
