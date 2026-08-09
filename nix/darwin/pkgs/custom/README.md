# custom packages

Custom packages for the darwin flake (the `nix/darwin` tree).

## Why they live inside the flake tree

(when there is `../../../pkgs/custom/` already)

A path flake (imported with `path:.`, see the `Makefile`) copies its own
directory to the store, and pure evaluation cannot follow symlinks out of that
copy. Anything a module imports must therefore be a real file inside this tree -
including the custom packages.

`nix/pkgs/custom/pam-ssh-agent` is a symlink back here for non-flake consumers
like the nixos side.

## Adding a package

Drop a `default.nix` (or a directory with one) in here and import it from a
module, e.g. `import ./pkgs/custom/<name> { inherit pkgs; }`.
