# nix/darwin

It's an attempt to manage Darwin in a declarative fashion via [nix-darwin](https://github.com/LnL7/nix-darwin).

Before I attempt to use `nix-darwin` (via [flake.nix](./flake.nix)), I already have been using [home-manager](https://github.com/nix-community/home-manager) which works great cross-platform.

Currently I'm not interested to manage/control `home-manager` although it's possible.

_Look at these code as examples:_
- https://github.com/malob/nixpkgs
- https://github.com/kclejeune/system
- https://github.com/thexyno/nixos-config

Up until this point, I have not played with flakes before and it was a mysterious dragon to me.
Thankfully these were helpful to get started:
- https://nixos.wiki/wiki/Flakes
- https://www.tweag.io/blog/2020-05-25-flakes/
- https://xyno.space/post/nix-darwin-introduction
- https://github.com/LnL7/nix-darwin#flakes-experimental

Hopefully just doing `cd [to this directory]` and `make switch` should kick start `nix-darwin`.

At this point, I'm not intended to do anything crazy with [configuration.nix](./configuration.nix) as I hope to maintain the difference between platforms minimal.
So I will just focus on reproducible the configuration of macOS itself.
