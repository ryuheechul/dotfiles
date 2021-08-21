# Dotfiles

A highly opinionated dotfiles that makes my life much easier as a developer and an individual.
> actually it's slightly more than just dotfiles

It's aimed to:
- be reproducible across multiple devices
- be modular and minimal and clean
- be frequently updated as necessary
- be used for anyone if desired\*
- be adaptable to new ways of doing things as things arise\*\*
- capture recent useful changes in my workstations into more permanent fashion
- share what I learned to configure better settings

> \* I try my best to leave no names or email addresses or assumed specific home path of mine in this repo
> \*\* for example, package installation method is chaing to [nix](https://github.com/NixOS/nix) from [brew](https://brew.sh) at the moment

## Prepare

You may clone this repository to start using.
I usually clone to `${HOME}/dotfiles` or `~/dotfiles` but it should work regardless of location.
Rest of my instruction will assume that you cloned to `~/dotfiles` though.

## Bootstrap

### Platform foundation

`~/dotfiles/bootstrap/foundation/linux.sh` # recommended to look at an example below

or

`~/dotfiles/bootstrap/foundation/mac/essential.sh` # recommended you to look at an example below

### Configuration

`~/dotfiles/bootstrap/configuration.sh` # recommended you to look at an example below


### Examples

#### linux

```
~/dotfiles/bootstrap/foundation/linux.sh
source /etc/profile.d/nix.sh
source /etc/profile.d/user-shim-for-nix-path.sh
~/dotfiles/bootstrap/configuration.sh
```

> See https://github.com/ryuheechul/dotfiles-launchpad/tree/master/Vagrantfile for more details

#### macOS
```
# this should prepare essential stuff
~/dotfiles/bootstrap/foundation/mac/essential.sh

# do below optionally since it's heavily customized to the author
~/dotfiles/bootstrap/foundation/mac/extra.sh

# this should prepare platform agnostic essential stuff
~/dotfiles/bootstrap/configuration.sh
```

## Stuff that come with

All the source code here aim to be self explanatory.
You can take a look at directories and files especially top level ones and `bootstrap/configuration.sh`, `nix/pkgs/`.
Frequent updates are expected as the needs grow.

https://github.com/ryuheechul/dotfiles-launchpad might be useful to see how it's being used
