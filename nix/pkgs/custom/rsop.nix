# based on the example code from the link below
# https://github.com/NixOS/nixpkgs/blob/645bc49f34fa8eff95479f0345ff57e55b53437e/pkgs/tools/misc/hexyl/default.nix

# more info on rust + nix can be found at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md
{ pkgs }:

with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "rsop";
  version = "1.4.2";

  src = fetchFromGitHub {
    owner = "desbma";
    repo = pname;
    rev = "${version}";

    sha256 = "sha256-DbAtCExLcH6CxVmfiY0komGDvtLbDJsGCVfKeoSrOfo=";
  };

  cargoHash = "sha256-fwtJrIgquGaz3BjZv8noN7FjBIpWdVnufwMNvBQGLQo=";

  postInstall = ''
    ln -rs "$out"/bin/rs{op,o}
    ln -rs "$out"/bin/rs{op,e}
    ln -rs "$out"/bin/rs{op,p}
    ln -rs "$out"/bin/rs{op,i}
  '';
}
