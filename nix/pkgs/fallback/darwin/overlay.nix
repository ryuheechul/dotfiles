# to fallback to a darwin channel in case there are issues on certain packages on darwin
self: super:
let
  # thanks to https://evanrelf.com/building-x86-64-packages-with-nix-on-apple-silicon
  # now I can selectively fallback to a x86_64 when a pacakage doesn't support aarch64 yet
  x86_64 = import <nixpkgs> { system = "x86_64-darwin"; };
  amazon-ecs-cli = x86_64.pkgs.amazon-ecs-cli;

  # when packages on unstable (in my case, <nixpkgs>) is literrally to unstable
  stable = import <nixpkgs-stable-darwin> {};
  pkgs = stable.pkgs;
  starship = stable.pkgs.starship;

  # # these two lines are just examples
  # hello = stable.pkgs.hello;
  # bash = stable.pkgs.bash;
in
  {
    inherit starship;
    inherit amazon-ecs-cli;
    # # these two lines are just examples
    # inherit hello;
    # inherit bash;

    # inherit stable.pkgs.hello # this wouldn't work
}
