# plugins

This is where Neovim plugins are managed via
[../boot/lazy.lua](../boot/lazy.lua).

## Performance

### Measure and Analyze

Using [profile.nvim](https://github.com/stevearc/profile.nvim), it's possible to
record the profile and view it via https://ui.perfetto.dev/:

- profile.nvim is loaded via [system.lua](./system.lua)
- `pim` is defined at
  [../../../zsh/my_addons/aliases](../../../zsh/my_addons/aliases)

```bash
pim [filename] # this can make nvim slower or being weird (which you can try again to see if improves)

# and `F1` to start recording and `F1` again to finish recording

# Load the `profile.json` to ui.perfetto.dev
```

### Usual Culprits

Most of the time there is no noticeable performance hit but once they do, I have
a usual culprits to look first that I discovered with my experiences here and
there.

These 3 are loaded via [extra.lua](./extra.lua):

- [indent-blankline.nvim](https://github.com/lukas-reineke/indent-blankline.nvim)
- [vim-illuminate](https://github.com/RRethy/vim-illuminate)
- [nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar)

They are reasonably good out of the box. In addition, they all provide some way
to mitigate that by lazily applying the visual changes:

- [`throttle_ms` at nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar/blob/35f99d559041c7c0eff3a41f9093581ceea534e8/README.md?plain=1#L127)
- [`delay` at vim-illuminate](https://github.com/RRethy/vim-illuminate/blob/3bd2ab64b5d63b29e05691e624927e5ebbf0fb86/README.md?plain=1#L25)
- [`debounce` at indent-blankline](https://github.com/lukas-reineke/indent-blankline.nvim/blob/3c8a185da4b8ab7aef487219f5e001b11d4b6aaf/doc/indent_blankline.txt#L188-L192)

Again, they are pretty good usually and you can adjust values above.

But sometimes with some specific environments (whether it's device (e.g. virtual
machine), specific language (filetype), etc.) and hard to trace reasons, it can
still struggle.

Thankfully they all provide ways to not load on certain file types:

- [`excluded_filetypes` at nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar/blob/35f99d559041c7c0eff3a41f9093581ceea534e8/README.md?plain=1#L231)
- [`filetypes_denylist` at vim-illuminate](https://github.com/RRethy/vim-illuminate/blob/3bd2ab64b5d63b29e05691e624927e5ebbf0fb86/README.md?plain=1#L31)
- [`exclude` at indent-blankline](https://github.com/lukas-reineke/indent-blankline.nvim/blob/3c8a185da4b8ab7aef487219f5e001b11d4b6aaf/doc/indent_blankline.txt#L207-L218)
