{
  config,
  pkgs,
  ...
}:

let
  checkEnv = import ../../utils/checkEnv.nix;
  shells = import ./shells.nix {
    config = config;
    pkgs = pkgs;
  };
  terminals = import ./terminals.nix {
    config = config;
    pkgs = pkgs;
  };
in
{
  # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines
  imports = [
    ./home-manager.nix
    shells
    ./mise.nix
    ./direnv.nix
    ./navi.nix
  ]
  # terminals (wezterm/kitty config + desktop entries) is truly extra - a
  # headless case like dotfiles-launchpad has no use for it. Opt in via
  # MY_NIX_EXTRA_TERMINALS, the same env gate ../../pkgs/extra/default.nix uses.
  #
  # unlike that file we can't use `pkgs.lib.optionals` here: the `imports` list
  # spine must resolve before the module args do, and touching `pkgs`/`lib`
  # while computing it triggers infinite recursion. `checkEnv` is a pure
  # `getEnv`, so this `if` keeps the spine independent of module args, while the
  # `terminals` thunk stays lazy (evaluated later, like every other import).
  ++ (if checkEnv "MY_NIX_EXTRA_TERMINALS" then [ terminals ] else [ ]);

  # if anything doesn't want to belong to its own file it can be added below
}
