# mise

This directory holds this repo's [mise](https://mise.jdx.dev/) config, deployed
as `~/.config/mise` by [`bootstrap/configuration.sh`](../bootstrap/configuration.sh)
via a single symlink to [`home/`](home).

## Why mise, alongside nix and brew

The goal across this whole dotfiles repo is a "system" that works the same
regardless of platform (darwin/macOS, Linux). Nix is the main tool for
that, which is why brew usage here has been shrinking in favor of nix
wherever nix can do the job.

But nix isn't always satisfactory for this. Its channel model updates every
package together, and channels often lag behind upstream releases - awkward
for tools that update frequently, where staying current matters more than
the whole-channel atomicity nix buys you.

mise fills that specific gap: a lightweight, cross-platform package manager
- mostly for development-related tools - that tracks upstream releases
directly instead of through a channel. It's not a replacement for nix or
brew; each still holds its own strong area. And now that mise's `bootstrap`
feature exists, it turns out to double as a genuinely good dotfiles manager
too, as this whole repo's migration to it has shown.

### Priority when adding a new tool

Default order: **nix, then mise, then brew** - each step down trades away a
bit of "behaves the same everywhere" for something the level above can't
give you, so only drop a level when the level above genuinely doesn't fit:

1. **nix** first, always - reproducible, works the same on
   darwin/Linux/WSL, and is what most of `nix/pkgs/` and `nix/home/`
   already use.
2. **mise** when nix's channel lag actually bites: a tool that releases
   often enough that staying current matters more than nix's
   whole-channel-moves-together atomicity (the case this "Why mise" section
   is about).
3. **brew** when it's not a CLI dev tool at all - GUI/macOS apps (casks),
   Mac App Store apps (`mas`), background services (`brew services`) - the
   things neither nix's package model nor mise's tool-version-management
   model are built for (see
   [`bootstrap/foundation/darwin/extra.sh`](../bootstrap/foundation/darwin/extra.sh)).

## Why `home/` and not just `mise/config.toml` + `mise/conf.d/`

mise treats `mise/config.toml` (a `config.toml` file directly inside a
directory named `mise`) as one of its recognized project-local config
locations. Since this whole directory is *already* named `mise` at the repo
root, putting `config.toml` and `conf.d/` directly in it would make mise
accidentally pick this repo's own config up as a *project* config any time
`mise` runs with a cwd inside this repo - a confusing collision with the
*global* config this same content is meant to become once deployed to
`~/.config/mise`.

Nesting everything one level deeper, under [`home/`](home), sidesteps this:
`mise/home/config.toml` doesn't match the `mise/config.toml` pattern mise's
project-local discovery checks for, so it's inert as a project config no
matter what cwd you're in - while still deploying correctly as
`~/.config/mise` once symlinked.

The nesting has a second, more load-bearing benefit: mise only auto-loads
`conf.d/*.toml` when it sits *directly next to* the resolved `config.toml` on
disk - not through a symlink to a config file living elsewhere. Keeping
`config.toml` and `conf.d/` as real siblings inside `home/`, and symlinking
the whole `home/` directory as a single unit, means both are always
genuinely adjacent after symlink resolution - verified empirically: a single
`~/.config/mise -> home/` symlink correctly surfaces `conf.d/*.toml`,
tools, and dotfiles, with no second symlink needed.

See [`home/README.md`](home/README.md) for what's actually in there and how
to work with it once deployed.
