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

### Be Cautious of These Steps

These scripts that you are about to run have a high probability of overwriting your exisiting folders and files especially other "dotfiles".
Since the scripts are meant to not run interactively most of the time, there is a high chance it doesn't prompt to double check of what it is about to do.
Therefore you must read what they do beforhand to avoid unwanted behavior.

It also installs Nix the package manager, and it does create a volume on macOS to workaround the limitation that doesn't exist on Linux. This might affect across all user accounts locally.

The safe ways to try these out are below:
- run these inside virtual machines as it makes easy to "uninstall".
- things might partially work on Github Codespaces if not all
- I've run these inside containers before
- https://github.com/ryuheechul/dotfiles-launchpad uses Vagrant and Docker container version to bootstrap the portable environment
  - dotfiles-launchpad clones this repo as submodule and many times it's not up-to-date, FYI.

### Platform Foundation

`~/dotfiles/bootstrap/foundation/linux.sh` # recommended to look at an example below

or

`~/dotfiles/bootstrap/foundation/darwin/essential.sh` # recommended you to look at an example below

### Configuration

`~/dotfiles/bootstrap/configuration.sh` # recommended you to look at an example below


### Examples

#### Linux

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
~/dotfiles/bootstrap/foundation/darwin/essential.sh

# do below only optionally since it's heavily customized to the author
# make sure to sign in to appstore first
~/dotfiles/bootstrap/foundation/darwin/extra.sh

# this should prepare platform agnostic essential stuff
# you might need to open a new terminal window to be able to do this successfully
~/dotfiles/bootstrap/configuration.sh
```

## Stuff That Come With

All the source code here aim to be self explanatory.
You can take a look at directories and files especially top level ones and `bootstrap/configuration.sh`, `nix/pkgs/`.
Frequent updates are expected as the needs grow.

https://github.com/ryuheechul/dotfiles-launchpad might be useful to see how it's being used
