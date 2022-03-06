-- originally from https://github.com/mjlbach/defaults.nvim/blob/73d4b205be5711b681ef2df9d171b1c55040803b/init.lua

-- workaround to be able to load other files while using alternative rtp
-- vim.cmd 'set rtp^=~/.config/my-quick-nvim' -- no longer necessary

-- via ./lua
require'plugins-via-packer'
require'misc'
require'lsp'
require'treesitter'
require'completion'
require'keymaps'
require'term'
-- end of via ./lua

-- enables `ds%` and `cs%` via vim-matchup
vim.g.matchup_surround_enabled = 1
vim.g.matchup_matchparen_deferred = 1
vim.g.matchup_matchparen_hi_surround_always = 1

-- let the code be folded by default
-- useful with 'anuvyklack/pretty-fold.nvim'
vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'

--
require'scrollbar'.setup()
--
require'numb'.setup()

-- Gitsigns
require'gitsigns'.setup {
  signs = {
    add = { hl = 'GitGutterAdd', text = '+' },
    change = { hl = 'GitGutterChange', text = '~' },
    delete = { hl = 'GitGutterDelete', text = '_' },
    topdelete = { hl = 'GitGutterDelete', text = '‾' },
    changedelete = { hl = 'GitGutterChange', text = '~' },
  },
}

-- Telescope
require'telescope'.setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- nvimtree

-- `nvim_tree_callback` and `cb` is deprecated
local tree_cb = require'nvim-tree.config'.nvim_tree_callback

require'nvim-tree'.setup {
  view = {
    side = 'right',
    mappings = {
      custom_only = false,
      -- override default mappings
      list = {
        { key = {"o", "<2-LeftMouse>", "l"},         action = "edit" },
        -- { key = {"<2-RightMouse>", "<C-]>", "<CR>"}, cb = tree_cb("cd") },
        { key = {"-", "h"},                          action = "dir_up" },
        { key = {"<Tab>"},                           cb = ':wincmd w<CR>' },
      }
    },
  },
  -- auto_open = true,
  auto_close = false,
  disable_netrw = false,
  hijack_netrw = false,
  open_on_setup = true,
}

-- vim: ts=2 sts=2 sw=2 et
