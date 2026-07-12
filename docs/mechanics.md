# Mechanics

The *how* companion to [philosophy.md](./philosophy.md): which file
implements what, which env var gets scrubbed where, per-subscriber delivery
details. Philosophy is the source of truth for the *why* and each of its
sections links here; this doc holds the implementation inventory that would
otherwise drown the principles. Freely rewritten as the code moves around;
not a changelog - if the implementation changes, edit the section in place.

## Enter anywhere, nest anything

Philosophy: [why no config assumes a fixed hierarchy](./philosophy.md#enter-anywhere-nest-anything).

Barriers lifted so far, and how (each layer decides "mine or the inner
one's?" at runtime instead of by assumption):

- *One prefix/nav keyset can't serve every depth* - `tmux.conf` inspects
  the pane's live process (`is_zellij`, `is_vim`) and forwards `C-b` and
  `C-hjkl` inward when an inner zellij/vim/emacs should own them;
  ghostel does the same via alt-screen detection (pane nav at a prompt,
  raw keys into the inner TUI).
- *"Which mux do the mw* aliases control?" is ambiguous when muxes nest* -
  `zsh/integration/multiplexers` walks real process ancestry instead of
  guessing from env (pane env inherits stale answers), emacs included as
  a mux.
- *A shell inside an editor is normally a dead end* - `$EDITOR` itself
  points back at the host, so anything that shells out to an editor and
  needs to wait (`git commit`, lf, suffix aliases) lands there and
  blocks like an editor should - and opens as a *tab of the host*,
  which closes back to the terminal when done: inside nvim it's
  `nvimclient` (nvr `--remote-tab-wait` + trap-close-for-term - nvim's
  own emacsclient, in `bin/path/nvim/` handed out by the editor itself),
  inside emacs it's plain `emacsclient` against the parent's own socket
  (`EMACS_SOCKET_NAME`, handed down by `prep-env-for-term`) shown in
  its own workspace via `server-window`, closed and returned from by
  `server-done-hook` - the same tab-in/tab-out shape on both editors.
  The bridges
  (`ghostel_cmd`/`vterm_cmd`, nvr) cover the *interactive* openers,
  which shouldn't block the terminal: `find-file`/`emacs`/`vi` opens in
  the editor window above and returns immediately, `q` hides the
  terminal instead of killing the shell, `nvim` inside nvim's terminal
  becomes `nvr --remote-tab` on the host instead of nvim-in-nvim.
- *One command, each layer's own shape* - the base `vi`/`e` in
  `zsh/my_addons/aliases` stay dumb (`$EDITOR` at call time; `e` with
  no args picks with fzf first), and each editor's own `source.zsh`
  reshapes them for its shells rather than the base function trying to
  detect where it runs: inside nvim `e` picks with Telescope (same as
  `<Space>ff`), inside emacs `vi`/`e` route through the non-blocking
  find-file bridge and `e` picks with vertico `find-file` (same as
  `SPC f f`).
- *Editors summoning each other used to degrade* - "open in emacs" from
  nvim runs `emacsclient -t` in a disposable float (not GUI/org-protocol
  indirection); "open in nvim" from emacs is a full-frame modal that
  restores the window layout on exit. One hard limit: a tty client
  frame must never target an *ancestor* emacs (the frame would be
  rendered by the very event loop it's waiting on - a freeze, not a
  degradation), so wherever `EMACS_SOCKET_NAME` reveals an ancestor,
  `emacs`/`<Space>fe` become plain `emacsclient -n` - the ancestor's
  `server-window` opens the file in a fresh workspace (doom tab),
  visible immediately even when its frame is a fullscreen terminal,
  and `q` there closes the workspace and returns.
- *Inherited env lies about the present* - editor markers
  (`INSIDE_EMACS`, `NVIM`) are scrubbed by the spawning editor so only
  the nearest one's integration loads ("nearest editor wins": emacs's
  `prep-env-for-term`, nvim's `boot/misc.lua`); the quick-editor
  fast-path flags get the same scrub before nvim launches, so its nested
  shells load the full zsh config.
- *Tools grab the outermost layer's features when a nearer one fits* -
  `nvim/shell/source.zsh` unsets `TMUX` so fzf uses the current pane,
  not a popup in the tmux two layers up.
- *One-shot vs re-load of shell config across depth* - the
  loaded-markers handshake in `zsh/zshrc` (`*_LOADED` + `UNSET_*`/
  `IGNORE_UNSET_*`) lets each layer decide whether the shell below
  reloads everything or deliberately skips.
- *TERM/terminfo and clipboard degrade through layers* - each layer sets
  its own TERM (ghostel ships terminfo, nvim exports its own for
  `:terminal`), emacs scrubs the outer terminal's leftovers
  (`env-vars-to-exclude`), and both editors pick their clipboard
  provider by inspecting the actual stack instead of one default (see
  [one clipboard, every layer](#one-clipboard-every-layer) below).

How "nearest editor wins" is wired: the spawning tool scrubs the other
editors' markers from the environment it hands to its children (emacs:
`prep-env-for-term` in term-enhance; nvim: `boot/misc.lua`). Consumers
(`zsh/integration/editors`) then stay dumb single-variable checks,
mutually exclusive by construction. `EMACS_SOCKET_NAME` rides the same
one-hop rule: each emacs overwrites it for its own terminals (unique
PID-based server name), so `emacsclient` always reaches the nearest emacs,
never a stale grandparent or an unrelated daemon.

The snapshot case: Doom's env file (`doom sync`) captures the invoking
shell's environment for every future emacs launch, so a sync run from a
shell inside an editor would freeze that session's handed-down vars
(`EDITOR`, `EMACS_SOCKET_NAME`, `NVIM_LISTEN_ADDRESS`) into permanent
config. The deny list in
[env-vars-to-exclude](../emacs.d/doom.d/shell/env-vars-to-exclude) keeps
session-scoped vars out of the snapshot; the rule of thumb lives there:
whatever an editor hands its children never gets baked.

## One tone, every layer

Philosophy: [why light/dark is one system-wide decision](./philosophy.md#one-tone-every-layer).

The mechanism is a tiny pub/sub bus made of two files:
[zsh/fn/theme](../zsh/fn/theme) points `~/.base16_theme` at the new
base16-shell script (the *state*) and bumps
`~/.base16_theme.updated-time` (the *signal*). Publishers: the
system-appearance launchd agent and the manual `dark`/`light` commands
(both in [watch-theme-change.nix](../nix/home/services/watch-theme-change.nix)
/ `zsh/fn/theme`). Subscribers watch the signal file, each with its own
delivery problem:

- **new shells** need no delivery at all - they source `~/.base16_theme`
  at startup, so the state file alone covers everything born after the
  flip. The rest is about what's *already running*.
- **tmux** ([base16-shell-reload-on-tmux](../bin/path/default/base16-shell-reload-on-tmux),
  called by the [base16-shell-auto-reload](../bin/path/default/base16-shell-auto-reload)
  `entr` watcher): a throwaway window sources the theme; the base16
  script sees `$TMUX` and passes the palette escapes through to the host
  terminal, which recolors every pane at once.
- **herdr** ([base16-shell-reload-on-herdr](../bin/path/default/base16-shell-reload-on-herdr),
  same watcher): no passthrough exists, and herdr both keeps
  *per-pane* palettes and renders through each attached client's host
  terminal - so the watcher writes the raw escapes itself, to every
  attached client's host tty (chrome + client-side rendering; herdr's UI
  theme is set to `terminal` so it follows the host palette) and to every
  pane's pty (found via herdr's socket API), which updates the per-pane
  palettes.
- **emacs** ([term-enhance/theme.el](../emacs.d/doom.d/modules/my-custom/term-enhance/theme.el)):
  `file-notify` on the same signal file switches the doom theme, and
  `switch-theme` is also exposed the other way (shell -> emacs) via the
  terminal bridges.
- **nvim** ([plugins/theme.lua](../nvim/lua/plugins/theme.lua)): same
  idea with `fwatch` - the signal file flips the colorscheme tone in
  every running instance.

The two load-bearing choices (indexed colors over truecolor, remote
shells opt out) are stated in philosophy; their implementation is in
[zsh/fn/theme](../zsh/fn/theme) (refuses to run over SSH/tramp) and
[source.zsh](../emacs.d/doom.d/shell/source.zsh)'s theme-drift sync
(skips tramp shells for the same reason).

## One clipboard, every layer

Philosophy: [why one system clipboard across every depth](./philosophy.md#one-clipboard-every-layer).

Three mechanisms compose:

- **Universal verbs**: [pbcopy](../bin/path/default/pbcopy) /
  [pbpaste](../bin/path/default/pbpaste) wrappers give every machine
  the same two commands and dispatch per stack, in preference order:
  native `/usr/bin/pb{copy,paste}` on local macOS, then `osc` (OSC 52
  escapes) for everything remote, and only when neither applies the
  `xsel`/X11-forwarding path as last resort. Consumers stay oblivious:
  nvim's fallback provider and emacs's TTY-frame paste both just call
  `pbcopy`/`pbpaste` and work wherever the wrappers do.
- **Escape-sequence transport**: remotely, OSC 52 is the *preferred*
  method, not one option among peers - the clipboard belongs to the
  terminal being physically looked at, and OSC 52 rides the tty
  connection itself, so it follows through ssh/tmux/nesting with zero
  extra infrastructure. X11 forwarding by contrast needs `ForwardX11`,
  a live X server, and a network round-trip slow enough that the
  pbcopy wrapper has to `nohup` its xsel path - fallback only. Every
  layer is configured to let OSC 52 through: tmux `set-clipboard on`
  plus `get-clipboard both` (so reads pass through too),
  [ghostty](../ghostty/config) `clipboard-read = allow` (deliberate:
  most terminals refuse reads), vterm/ghostel forward inner programs'
  OSC 52 to the system clipboard
  (`vterm-enable-manipulate-selection-data-by-osc52`,
  `ghostel-enable-osc52`), and doom's `:os (tty +osc)` sends copies
  made in TTY emacs out the same way. Zellij is the known hole (no OSC 52
  paste), so the wrappers and providers route around it.
- **Native first, per actual stack**: when an immediate native provider
  exists, it beats the escape-sequence one (no async race) - so each
  editor inspects the stack it really runs in instead of assuming one:
  nvim ([boot/misc.lua](../nvim/lua/boot/misc.lua)) autodetects native
  locally, uses its internal OSC 52 provider only for bare SSH, and
  falls back to the wrappers for SSH+zellij/emacs; emacs
  ([compat/neovim](../emacs.d/doom.d/modules/compat/neovim/config.el))
  keeps GUI frames native and gives TTY frames OSC 52 out /
  `pbpaste` in, per frame (`+neovim/frame-aware-paste`).
