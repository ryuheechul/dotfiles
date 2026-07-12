# Dotfiles

A highly opinionated set of dotfiles that makes my life much easier as a
developer and an individual. It's actually slightly more than just dotfiles -
package installation, services, and cross-tool integration all live here too.

## Goals

- reproducible across multiple devices
- modular, minimal, and clean
- frequently updated as needs grow
- usable by anyone who wants to try it - I keep no names, email addresses, or
  hard-coded home paths of mine in this repo (though bootstrapping can still
  conflict with dotfiles you already manage, so the safe way to try it is a
  throwaway VM, a container, or at least a clean user account)
- adaptable to new ways of doing things - e.g. the main package manager used to
  be [brew](https://brew.sh) and is now [nix](https://github.com/NixOS/nix)
- a place to capture useful workstation changes in a more permanent fashion,
  and to share better settings I've figured out

Cross-tool design themes that aren't specific to any one config live in
[docs](docs): [philosophy.md](docs/philosophy.md) for the *why* and
[mechanics.md](docs/mechanics.md) for the *how*.

## Prepare

Clone this repository to start. I usually clone to `~/dotfiles`, and the rest
of these instructions assume that, but it should work regardless of location.

## Bootstrap

### Be cautious of these steps

These scripts have a high probability of overwriting your existing folders and
files, especially other dotfiles. They're meant to run non-interactively most
of the time, so they often won't prompt before doing what they do - read them
first to avoid surprises.

They also install the Nix package manager, and on macOS create a dedicated
volume (a workaround for a limitation that doesn't exist on Linux), which can
affect all local user accounts.

Safe ways to try this out:

- run it inside a virtual machine, so it's easy to "uninstall"
- run it inside a container (I've done this before)
- things might partially work on GitHub Codespaces, if not all
- at minimum, use a clean user account
- [dotfiles-launchpad](https://github.com/ryuheechul/dotfiles-launchpad)
  bootstraps this as a portable, throwaway environment

Bootstrapping is two steps: first a platform-specific **foundation**
([bootstrap/foundation](bootstrap/foundation)), then the platform-agnostic
[bootstrap/configuration.sh](bootstrap/configuration.sh). The foundation
differs per platform; the examples below cover each.

### macOS

```sh
# prepare essential stuff
~/dotfiles/bootstrap/foundation/darwin/essential.sh

# optional and heavily customized to the author - sign in to the App Store first
~/dotfiles/bootstrap/foundation/darwin/extra.sh

# platform-agnostic essential stuff
# (you might need a new terminal window for this to succeed)
~/dotfiles/bootstrap/configuration.sh
```

### NixOS

```sh
# optionally, run this first to be able to clone this repo and use vim
nix-shell -p git vim

# prepare essential stuff
~/dotfiles/bootstrap/foundation/nixos/switch.sh

# platform-agnostic essential stuff
# (you might need a new terminal window for this to succeed)
~/dotfiles/bootstrap/configuration.sh
```

### Linux

```sh
~/dotfiles/bootstrap/foundation/linux.sh
source /etc/profile.d/nix.sh
source /etc/profile.d/user-shim-for-nix-path.sh
~/dotfiles/bootstrap/configuration.sh
```

On Ubuntu you may be missing `git`/`curl`/`openssh-server` needed to get this
far - [bootstrap/foundation/optional/ubuntu-prerequisite.sh](bootstrap/foundation/optional/ubuntu-prerequisite.sh)
installs them.

### WSL2

[bootstrap/foundation/wsl.sh](bootstrap/foundation/wsl.sh) installs Nix and
runs home-manager itself (it prompts, so run it yourself rather than
unattended), then points you at `bootstrap/configuration.sh` for the rest:

```sh
~/dotfiles/bootstrap/foundation/wsl.sh
~/dotfiles/bootstrap/configuration.sh
```

### GitHub Codespaces

[setup.sh](setup.sh) at the repo root is the Codespaces entry point (per
GitHub's [personal dotfiles](https://docs.github.com/en/codespaces/setting-your-user-preferences/personalizing-github-codespaces-for-your-account)
convention): it installs single-user Nix and runs home-manager.

## Layout

All the source here aims to be self-explanatory - browse the top-level
directories and files. The ones with their own README:

- [agents](agents) - shared instructions symlinked into each AI agent tool's config
- [aliases](aliases) - short, easy-to-type command aliases (no caps, no special chars)
- [bin](bin) - scripts, with `path/` added to `$PATH`
- [karabiner](karabiner) - Karabiner-Elements complex modifications for macOS
- [mise](mise) - [mise](https://mise.jdx.dev/) config, deployed as `~/.config/mise`
- [nvim](nvim) - my own Neovim config (replaced an earlier SpaceVim setup)

And a few more worth knowing:

- [bootstrap](bootstrap) - the entry-point scripts above
- [docs](docs) - cross-tool design: philosophy (why) and mechanics (how)
- [nix](nix) - Nix / home-manager package and service definitions
- [zsh](zsh) - zsh config (functions, integrations, aliases, and more)

## Related

[dotfiles-launchpad](https://github.com/ryuheechul/dotfiles-launchpad) shows how
this repo gets used to bootstrap a portable environment. It clones this repo as
a submodule, which is often not up-to-date - FYI.
