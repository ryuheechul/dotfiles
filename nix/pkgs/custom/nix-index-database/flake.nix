# NOTE: now there is another version that does the same thing without having to write a flake at ../alt-nix-index-database

{
  description = "nix-index for home-manager via nix-index-database";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      nix-index-database,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default = nix-index-database.hmModules.nix-index;
    });
}
