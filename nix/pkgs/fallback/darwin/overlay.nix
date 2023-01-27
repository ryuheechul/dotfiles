# to fallback to a darwin channel in case there are issues on certain packages on darwin
self: super:
let
  # thanks to https://evanrelf.com/building-x86-64-packages-with-nix-on-apple-silicon
  # now I can selectively fallback to a x86_64 when a pacakage doesn't support aarch64 yet
  x86_64 = import <nixpkgs> { system = "x86_64-darwin"; };
  amazon-ecs-cli = x86_64.pkgs.amazon-ecs-cli;
  gdb = x86_64.pkgs.gdb;

  # when packages on unstable (in my case, <nixpkgs>) is literally to unstable
  stable = import <nixpkgs-stable-darwin> { };
  ## commenting below as the unstable channel one is working fine again
  # starship = stable.pkgs.starship;
  # emacs = stable.pkgs.emacs;
  # emacsPackagesFor = stable.pkgs.emacsPackagesFor;

  ## these two lines are just examples
  # hello = stable.pkgs.hello;
  # bash = stable.pkgs.bash;
in
{
  # commenting as the unstable channel one is working fine again
  # inherit starship;
  inherit amazon-ecs-cli; # from x86
  inherit gdb; # from x86
  # inherit emacs; # from stable
  # inherit emacsPackagesFor; # from stable
  ## these two lines are just examples
  # inherit hello; # because `inherit stable.pkgs.hello` wouldn't work
  # inherit bash;
}
