{
  description = "John's darwin system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs }: {
    darwinConfigurations.aarch64 = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ ./configuration.nix ];
    };
  };
}
