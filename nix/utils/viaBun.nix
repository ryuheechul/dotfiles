{
  pkgs,
  pkg-name,
  npm-pkg-name,
}:

# declarative alternative to `bunx ${npm-pkg-name}`:
# when ./viaDeno.nix just doesn't work well for some reason
pkgs.writeShellScriptBin pkg-name ''
  ${pkgs.bun}/bin/bunx ${npm-pkg-name} "$@"
''

# see the usage at ../pkgs/custom/gemini.nix
