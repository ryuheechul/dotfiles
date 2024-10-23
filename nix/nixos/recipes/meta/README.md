# recipes/meta
Meta here means to highorder functions unlike the recipes in the parent directory which adheres to be a (static) nixos module.

For example, "regular" module would like below:

```nix
# recipes/recipe1.nix
{ pkg, lib, ... }:

{
  # ...
}
```

And you call it like (for example from ../../mix-and-match.nix):

```nix
{ ... }:

{
  imports = [
    ./recipes/recipe1.nix
  ]

  # ...
}
```

And the meta ones would be like

```nix
# recipes/meta/example.nix
{ param1, param2 ? "param2" }:
{ pkg, lib, ... }:

{
  # ...
}
```

And you call it like (for example from ../../mix-and-match.nix):

```nix
{ ... }:

{
  imports = [
    ./recipes/recipe1.nix
    (import ./recipes/meta/example.nix { param1 = "param1"; })
  ]

  # ...
}
```

Real world examples are:
- [zerotier.nix](./zerotier.nix)
- [zeronsd.nix](./zeronsd.nix)
- and more
