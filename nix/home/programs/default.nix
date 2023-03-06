let
  imports = [
    ./home-manager.nix
    ./direnv.nix
    ./navi.nix
  ];
in
{
  inherit imports;
}
