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

These 4 are loaded via [extra.lua](./extra.lua) and [syntax.lua](./syntax.lua):

- [indent-blankline.nvim](https://github.com/lukas-reineke/indent-blankline.nvim)
- [vim-illuminate](https://github.com/RRethy/vim-illuminate)
- [nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter-context)

They are reasonably good out of the box. In addition, they all provide some way
to mitigate that by lazily applying the visual changes:

- [`throttle_ms` at nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar/blob/35f99d559041c7c0eff3a41f9093581ceea534e8/README.md?plain=1#L127)
- [`delay` at vim-illuminate](https://github.com/RRethy/vim-illuminate/blob/3bd2ab64b5d63b29e05691e624927e5ebbf0fb86/README.md?plain=1#L25)
- [`debounce` at indent-blankline](https://github.com/lukas-reineke/indent-blankline.nvim/blob/3c8a185da4b8ab7aef487219f5e001b11d4b6aaf/doc/indent_blankline.txt#L188-L192)
- [`throttle` is yet to be configurable at nvim-treesitter-context](https://github.com/nvim-treesitter/nvim-treesitter-context/issues/369)
    - [but there is `topline` mode to compromise at least](https://github.com/nvim-treesitter/nvim-treesitter-context?tab=readme-ov-file#configuration)


Again, they are pretty good usually and you can adjust values above.

But sometimes with some specific environments (whether it's device (e.g. virtual
machine), specific language (filetype), etc.) and hard to trace reasons, it can
still struggle.

Thankfully they all provide ways to not load on certain file types:

- [`excluded_filetypes` at nvim-scrollbar](https://github.com/petertriho/nvim-scrollbar/blob/35f99d559041c7c0eff3a41f9093581ceea534e8/README.md?plain=1#L231)
- [`filetypes_denylist` at vim-illuminate](https://github.com/RRethy/vim-illuminate/blob/3bd2ab64b5d63b29e05691e624927e5ebbf0fb86/README.md?plain=1#L31)
- [`exclude` at indent-blankline](https://github.com/lukas-reineke/indent-blankline.nvim/blob/3c8a185da4b8ab7aef487219f5e001b11d4b6aaf/doc/indent_blankline.txt#L207-L218)
- [`on_attach` at nvim-treesitter-context](https://github.com/nvim-treesitter/nvim-treesitter-context?tab=readme-ov-file#configuration)

### More on the "Culprits" Above

Although disabling them helped, even without any of them, I found the same performance issue simply by words being highlighted (e.g. by search). And I vaguely remember the parts of source code of some plugins (that was being slow) something to do with the highlighting!

On top of that I stumbled upon "a fix" from one of these:
- https://docs.getutm.app/settings-qemu/qemu/#balloon-device
- https://docs.getutm.app/settings-qemu/qemu/#use-local-time-for-base-clock

After turning them on, the issue disappeared as if they never existed...
However, I can't confirm this as the problem didn't come back after I turn the options off again
to see whether the absence of them were indeed the culprit.

So now I have some clues like highlight in Neovim and potentially some VM optimizations but cannot be so sure about it at this moment. Nonetheless I'm glad that it's possible to do something about it although I don't know what that is.
Maybe the problem will comeback later and give me a better clue? We will see.

#### Update on "a fix"

The real fix actually is anything but a simple reboot. This fixes it but unfortunately the problems reappears pretty soon-ish (the rebooting fixes the issue every time until it reappears).

I've also tried many other things to see if there is any difference:
- turning off swap
- change the disk interface from IDE to virtio
  - https://passthroughpo.st/disk-passthrough-explained/
  - https://www.reddit.com/r/Proxmox/comments/wvq8ht/perfomance_benchmarking_ide_vs_sata_vs_virtio_vs/
- increased disk size
- bumping all nixpkgs to the lastest
  - also bumping nixos to the lastest
- bumping Neovim plugins
- resorting to Neovim nightly
- using Apple's Virtualization Framework backend instead of QEMU's
- cpu benchmarks are same whether the Neovim slows down or not
  - and other editors stay snappy; this suggests that the issue can't be machine-wide or OS-wide (or at least it impacts noticeably to Neovim only)
  - and the benchmark results in host was not that different as the QEMU backend was relying on hardware Virtualization
  - when CPU is not slowed down and disk benchmarks are also same, what is it?!!

And so far none of them worked (except the simple reboot).

_I will leave more updates here if I ever find the cause and resolution, lol._

#### Is this the end of the journey?

Actually why haven't I noticed this one before?
- https://github.com/nvim-treesitter/nvim-treesitter?tab=readme-ov-file#i-experience-weird-highlighting-issues-similar-to-78
- https://github.com/nvim-treesitter/nvim-treesitter/issues/78

Reading them and trying this and that made me realized that TS highlighting actually wasn't enabled!!! (probably ever since I made [this change](https://github.com/ryuheechul/dotfiles/commit/0a8bae199ea8151e1b90b4075e8925a6717839f5)). Now I discovered what was the issue and implemented a workaround to it at [../utils/nixos-shim.lua](../utils/nixos-shim.lua).

Whoever experiences a similar issue, I would recommend you to start with these:
- `:TSBufEnable highlight`
- `:write | edit | TSBufEnable highlight`

to see if it makes any changes or resolve the slowness, this will probably make you to reverify if your plugin setup was working as expected or not especially if you setup is as quirky as mine.
