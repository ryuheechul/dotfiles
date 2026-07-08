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
