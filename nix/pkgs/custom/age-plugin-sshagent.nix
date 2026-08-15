# age-plugin-sshagent: age plugin that derives X25519 identities from
# deterministic ssh-agent signatures, so age can decrypt with agent-held SSH
# keys (no private key file on disk).
# https://github.com/eszio/age-plugin-sshagent
#
# more info on go + nix can be found at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/go.section.md

{ pkgs }:

with pkgs;
buildGoModule rec {
  pname = "age-plugin-sshagent";
  version = "unstable-2026-06-12";

  src = fetchFromGitHub {
    owner = "eszio";
    repo = pname;
    rev = "8bc67c4a107f7e00d7d2661b740c903df9f673c6";

    hash = "sha256-ogXZ+3bTGE3n+qfo8WeotjT35m2fcGfCcrooAjqzyyU=";
  };
  vendorHash = "sha256-bYx2qwP9FOZFP3a/NldnyU1FcBjdXusNkjvZdyPI1VI=";
}
