# Philosophy

Cross-tool intent that isn't specific to any one config: the *why* behind
the themes below. Tool-specific files (`zsh/`, `nvim/`, `emacs.d/`, ...)
should reference a section here for the *why*, instead of cross-referencing
each other's comments directly - this doc is the source of truth for a
theme, individual configs just implement it. The companion
[mechanics.md](./mechanics.md) is the source of truth for the *how* - which
file does what, which env var gets scrubbed where; each section here links
to its mechanics counterpart. Both are freely rewritten as understanding
evolves; not a changelog, not numbered ADRs - if a decision changes, edit
the section in place.

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

The recurring failure mode is *inherited state lying about the present*:
environment variables propagate indefinitely, so a marker that truthfully
described the parent (`INSIDE_EMACS`, `NVIM`, quick-editor fast-path
flags, loaded-markers) reaches grandchildren where it's stale - and
nesting order is undecidable from the variables alone. The principle that
keeps entry free is **nearest editor wins**: a marker is only meaningful
one hop from the tool that set it, and the spawning tool - the only party
that knows it's the nearest - scrubs the other editors' markers from the
environment it hands to its children. Consumers then stay dumb
single-variable checks, mutually exclusive by construction, instead of
trying to out-guess nesting order.

Nesting isn't the only way inherited state goes stale - *snapshots* leak
it forward in time (a captured environment freezes that session's
handed-down vars into permanent config). The rule of thumb: whatever an
editor hands its children never gets baked into a snapshot.

Mechanics: [barriers lifted and how](./mechanics.md#enter-anywhere-nest-anything).

## One tone, every layer

Light/dark is a single system-wide decision, not a per-tool setting: when
the tone flips (macOS appearance change, or manual `dark`/`light`),
everything currently on screen follows - every tmux pane, every herdr
client and pane, emacs, and whatever TUI is running inside any of them -
without restarting anything. The mechanism is a two-file pub/sub bus (a
*state* file and a *signal* file); new shells read the state at startup,
and every already-running subscriber watches the signal.

Two load-bearing choices make "every layer" actually hold:

- **indexed colors over truecolor where a choice exists** - a palette
  remap can only recolor apps that draw with ANSI colors; anything
  painting explicit RGB is immune to the whole mechanism. So when a TUI
  offers an ansi variant of its theme, that variant is the one to pick.
- **remote shells opt out** - a tone flip is a *client-side* event; the
  theme switch refuses to run over SSH/tramp, because the remote's
  palette is the local client's business, not the remote's.

Mechanics: [the pub/sub bus and its subscribers](./mechanics.md#one-tone-every-layer).

## One clipboard, every layer

Copy in any layer at any depth - a TUI on a remote box inside tmux
inside a terminal, nvim inside emacs, plain GUI emacs - and it lands on
the one system clipboard; paste anywhere reads it back. ("Copy" and
"paste" throughout: vim calls the first one yank, emacs calls the
*second* one yank - the word is banned here for everyone's sanity.)
Three principles compose:

- **Universal verbs**: every machine gets the same two commands
  (`pbcopy`/`pbpaste`), so consumers stay oblivious to how the copy
  actually travels - they just call the verbs and work wherever the
  verbs do.
- **OSC 52 is the *preferred* remote transport**, not one option among
  peers: the clipboard belongs to the terminal being physically looked
  at, and OSC 52 rides the tty connection itself, so it follows through
  ssh/tmux/nesting with zero extra infrastructure. X11 forwarding by
  contrast needs a live X server and a network round-trip - fallback
  only.
- **Native first, per actual stack**: when an immediate native provider
  exists it beats the escape-sequence one (no async race), so each tool
  inspects the stack it really runs in instead of assuming one.

Mechanics: [the three mechanisms in detail](./mechanics.md#one-clipboard-every-layer).

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
