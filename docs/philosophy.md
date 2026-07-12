# Philosophy

Cross-tool intent that isn't specific to any one config. Tool-specific files
(`zsh/`, `nvim/`, `emacs.d/`, ...) should reference a section here for the
*why*, instead of cross-referencing each other's comments directly - this
doc is the source of truth for a theme, individual configs just implement
it. Freely rewritten as understanding evolves; not a changelog, not numbered
ADRs - if a decision changes, edit the section in place.

## Maximum ergonomics, minimum rigidity

The umbrella principle behind the more specific themes below. Two halves,
in tension on purpose:

- **Maximum ergonomics** - minimize physical/cognitive effort: fewer
  modifier keys, fewer awkward reaches, fewer steps between "I want X"
  and "X happens."
- **Minimum rigidity** - don't force uniformity where a tool's own idioms
  fit better, and don't force a single interaction model where it costs
  real functionality. Consistency is a means to ergonomics, not a goal to
  chase for its own sake - the moment it starts costing capability
  (flexibility, discoverability, "can I just type anything"), it's no
  longer ergonomic and the rigid version should be dropped.

Anti-shift (below) is this principle applied to modifier keys
specifically. The evil-ex "one-step" reversal in that section's
trade-offs is the clearest example of the *minimum rigidity* half
actually firing: forcing every `;` press through a searchable picker
was MORE consistent (same UI shape every time) but LESS ergonomic (two
extra keystrokes to escape the picker just to type something new) - so
it was reverted in favor of the tool's own native behavior, even though
that reintroduced a small cross-tool inconsistency.

## Enter anywhere, nest anything

Any tool can be the entry point - a bare terminal, tmux/zellij, emacs,
neovim - and any other tool can be invoked from inside it, nested to
whatever depth is technically possible: a terminal inside nvim inside a
terminal inside emacs is a legitimate stack, not an edge case, and so are
zellij inside tmux, a tmux client inside zellij, or emacs acting as the
multiplexer itself. No config gets to assume a fixed hierarchy ("emacs is
always outermost") - when a nesting order exposes a barrier, the barrier
is what gets fixed, not the workflow ("don't do that") that hit it.

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
  (`env-vars-to-exclude`), and nvim picks its clipboard provider by
  inspecting the actual stack (SSH/zellij/emacs) instead of one default.

The recurring failure mode is *inherited state lying about the present*:
environment variables propagate indefinitely, so a marker that truthfully
described the parent (`INSIDE_EMACS`, `NVIM`, quick-editor fast-path
flags, loaded-markers) reaches grandchildren where it's stale - and
nesting order is undecidable from the variables alone. The mechanism
that keeps entry free is **nearest editor wins**: a marker is only
meaningful one hop from the tool that set it, and the spawning tool -
the only party that knows it's the nearest - scrubs the other editors'
markers from the environment it hands to its children (emacs:
`prep-env-for-term` in term-enhance; nvim: `boot/misc.lua`). Consumers
(`zsh/integration/editors`) then stay dumb single-variable checks,
mutually exclusive by construction, instead of trying to out-guess
nesting order. `EMACS_SOCKET_NAME` rides the same one-hop rule: each
emacs overwrites it for its own terminals (unique PID-based server
name), so `emacsclient` always reaches the nearest emacs, never a
stale grandparent or an unrelated daemon.

Nesting isn't the only way inherited state goes stale - *snapshots*
leak it forward in time. Doom's env file (`doom sync`) captures the
invoking shell's environment for every future emacs launch, so a sync
run from a shell inside an editor would freeze that session's
handed-down vars (`EDITOR`, `EMACS_SOCKET_NAME`, `NVIM_LISTEN_ADDRESS`)
into permanent config. The deny list in
[env-vars-to-exclude](../emacs.d/doom.d/shell/env-vars-to-exclude)
keeps session-scoped vars out of the snapshot; the rule of thumb lives
there: whatever an editor hands its children never gets baked.

## One tone, every layer

Light/dark is a single system-wide decision, not a per-tool setting: when
the tone flips (macOS appearance change, or manual `dark`/`light`),
everything currently on screen follows - every tmux pane, every herdr
client and pane, emacs, and whatever TUI is running inside any of them -
without restarting anything.

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

Two load-bearing choices make "every layer" actually hold:

- **indexed colors over truecolor where a choice exists** - a palette
  remap can only recolor apps that draw with ANSI colors; anything
  painting explicit RGB is immune to the whole mechanism. So when a TUI
  offers an ansi variant of its theme, that variant is the one to pick.
- **remote shells opt out** - a tone flip is a *client-side* event;
  `zsh/fn/theme` refuses to run over SSH/tramp (the remote's palette is
  the local client's business, not the remote's), and
  [source.zsh](../emacs.d/doom.d/shell/source.zsh)'s theme-drift sync
  skips tramp shells for the same reason.

## Anti-shift keybindings

Avoid Shift wherever a Shift-free key can carry the same meaning, across
every tool with a modal/keyboard-driven interface (zsh's vi-mode, Neovim,
Emacs/evil). Concretely: prefer `;` over `:`, and reach for other free,
Shift-free keys (`gh`/`gk`, bare leader-suffix letters, ...) before
Shift-bearing ones when a key is up for grabs.

This isn't "make every tool behave identically" - each tool keeps its own
idioms (zsh has no ex-mode, Vim/Evil's `f`/`t` repeat-finds don't exist in
a shell). It's "the same *shape* of action - invoke a command, search
history, look up help - lands on the same physical key across tools,
Shift-free where possible." (See *maximum ergonomics, minimum rigidity*
above - this is that principle's keybinding instance.)

### Examples

| Key | zsh (vicmd) | nvim (normal) | emacs (evil normal) |
|---|---|---|---|
| `;` | fzf search + execute a past command | enter command-line mode (`:`'s old job) | `evil-ex` (enter command-line) |
| `,` | load last command into buffer, don't run it (`up-line-or-history`) | repeat last command (`@:<CR>`) | repeat last ex command (`evil-ex-repeat`) |
| `:` | prepend `tldr ` to the buffer | search help topics (`Telescope help_tags`) | search documented symbols (`helpful-symbol`) |
| `gh` | - | describe keyword at point, aggregated sources (hover.nvim: LSP/man/dictionary/GitHub/Jira) | describe symbol at point, aggregated sources (`+lookup/documentation`) |
| `gk` | - | describe keyword at point, LSP only (`vim.lsp.buf.hover`) | describe symbol at point, LSP only (`+eldoc-help-at-point`) |
| leader + `;` | n/a | command palette (`Telescope commands`) | M-x (`execute-extended-command`) |
| leader + `:` | n/a | *(free)* | `pp-eval-expression` (displaced from leader `;`) |

`K` (emacs) is intentionally unbound - it duplicated `gh`'s job (Doom's
stock default was `+lookup/documentation`, same command `gh` now carries).

### Trade-offs accepted

- **zsh loses forward repeat-find** (`;` used to repeat the last `f`/`t`
  character search). There's no ex-mode equivalent to swap it into, unlike
  nvim/emacs where `;`/`:` are a clean swap with nothing lost. Reverse
  repeat-find survives on `,`.
- **nvim/emacs's `;`/`:` swap is genuinely lossless** - repeat-find-forward
  just moved from `;` to `:`, noremap on both sides so neither recurses
  into the other's new meaning.
- Evaluated and rejected a "one-step" searchable-history prompt for emacs's
  `;` (`completing-read` + `evil-ex-execute`): it worked, but plain
  `evil-ex` already offers completion-at-point suggestions natively, so
  the custom wrapper was unnecessary complexity for no real gain.
