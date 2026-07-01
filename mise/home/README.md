# mise/home

This is the *global* (user-wide, not system-wide - it lives under `~/.config`,
not `/etc/mise`) [mise](https://mise.jdx.dev/) config for this user, deployed
as `~/.config/mise` by
[`../../bootstrap/configuration.sh`](../../bootstrap/configuration.sh),
which symlinks this whole directory as a unit - see [`../README.md`](../README.md)
for why.

## What's here

- [`config.toml`](config.toml) - the entry point, mostly mise-wide settings
  (currently things like `min_version` and `[settings]`) rather than the
  actual machine-setup declarations - those live in `conf.d/*.toml` below

`mise bootstrap`'s machine-setup pieces, declared in [`conf.d/`](conf.d) and
loaded in alphabetical order:

- [`conf.d/10-repos.toml`](conf.d/10-repos.toml) - cloned repos (`[bootstrap.repos]`)
- [`conf.d/20-dotfiles-symlinks.toml`](conf.d/20-dotfiles-symlinks.toml) - symlinked dotfiles (`[dotfiles]`)
- [`conf.d/25-dotfiles-edits.toml`](conf.d/25-dotfiles-edits.toml) - dotfile edits, e.g. the gitconfig include (`[dotfiles]`)
- [`conf.d/30-tools.toml`](conf.d/30-tools.toml) - global CLI tools (`[tools]`)
- [`conf.d/40-tasks.toml`](conf.d/40-tasks.toml) - post-install tasks (`[tasks.bootstrap]`)
- [`conf.d/50-macos-defaults.toml`](conf.d/50-macos-defaults.toml) - macOS defaults (`[bootstrap.macos.*]`)

## Useful commands

Once this is deployed to `~/.config/mise`:

```sh
mise config ls                        # which config files mise is actually loading
mise bootstrap status                 # drift across repos/dotfiles/tools/macos defaults
mise bootstrap --dry-run              # what a full `mise bootstrap` would do, without doing it
mise bootstrap repos status           # same, scoped to just one part (dotfiles/tools/etc. too)
mise bootstrap macos defaults status
mise bootstrap dotfiles apply --dry-run
```

## Merging with a project's own `mise.toml`

mise walks up from the current directory collecting every
`mise.toml`/`.mise.toml`/`mise/config.toml`/`.config/mise.toml` it finds,
then merges all of them with this global config - closer to cwd wins
per-key, but nothing here gets removed wholesale. A project's own `[tools]`
entry for e.g. `node` just adds/overrides that one tool version; it doesn't
touch anything declared globally. `[bootstrap.*]`/`[dotfiles]` entries merge
the same additive way, so a project could declare its own if it ever needed
to, though none do today.
