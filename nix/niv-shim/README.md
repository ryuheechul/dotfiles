# niv-shim

Pins the system-wide sources (nixpkgs, home-manager) to exact revisions with
[niv](https://github.com/nmattia/niv), so the
[home-manager wrapper](../../bin/path/default/home-manager) resolves them from
here instead of relying on `nix-channel`.

[nix/](./nix) holds the niv-generated `sources.json` and `sources.nix`. Manage
them with `niv` (e.g. `niv update nixpkgs`), not by hand; the [bin/](./bin)
helpers turn those sources into the `-I` paths the wrapper passes to nix.

## niv-shim vs via-niv

[../pkgs/custom/via-niv](../pkgs/custom/via-niv) does the same job but scoped
to the custom packages in [../pkgs/custom](../pkgs/custom); this one is the
system-wide set. Keeping them separate is a deliberate balance - not
everything pinned in one place, not scattered across many.

## What to do when a build fails or has no Hydra cache

After `niv update nixpkgs`, the pinned unstable revision may include packages
whose builds failed upstream and were never cached - so they're built locally,
where they likely fail the same way (and, for heavy ones, can exhaust memory).

To check a specific package, run
`hydra-check -- --channel nixpkgs-unstable [pkg-name]`. Open the URL of a
successful build, note its revision, and pin to it with
`niv update nixpkgs -r [revision]`; stepping back to a good revision usually
restores access to the binary cache.

Or run `hydra-check -- --channel nixpkgs-unstable` with no package name to
spot the revision where a batch of builds started failing, and pin to the one
just before it.
