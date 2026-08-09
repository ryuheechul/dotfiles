{
  description = "One's darwin system";

  # the flake tree (this directory) must contain everything its modules import -
  # custom packages live in ./pkgs/custom (see ./pkgs/custom/README.md)
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      darwin,
      nixpkgs,
    }:
    {
      darwinConfigurations = {
        aarch64 = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [ ./configuration.nix ];
        };
        amd64 = darwin.lib.darwinSystem {
          system = "amd64-darwin";
          modules = [ ./configuration.nix ];
        };
      };
    };
}
