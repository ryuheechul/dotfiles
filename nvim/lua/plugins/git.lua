-- git related

return {
  { -- Git commands in nvim
    'tpope/vim-fugitive',
    event = 'VeryLazy',
  },
  { -- Fugitive-companion to interact with github
    'tpope/vim-rhubarb',
    event = 'VeryLazy',
  },
  { -- magit for neovim
    'TimUntersberger/neogit',
    dependencies = {
      { -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev
        'sindrets/diffview.nvim',
        config = require('plugins.config.git').diffview,
      },
      'nvim-lua/plenary.nvim',
    },
    event = 'VeryLazy',
    config = require('plugins.config.git').neogit,
  },
  { -- Changes Vim working directory to project root
    'airblade/vim-rooter', -- a replacement candidate, https://github.com/ahmedkhalf/project.nvim
    event = 'VeryLazy',
  },
  { -- Add git related info in the signs columns and popups
    'lewis6991/gitsigns.nvim',
    event = 'VeryLazy',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
    init = function()
      vim.api.nvim_set_hl(0, 'GitSignsAdd', { link = 'GitGutterAdd' })
      vim.api.nvim_set_hl(0, 'GitSignsChange', { link = 'GitGutterChange' })
      vim.api.nvim_set_hl(0, 'GitSignsChangedelete', { link = 'GitGutterChange' })
      vim.api.nvim_set_hl(0, 'GitSignsDelete', { link = 'GitGutterDelete' })
      vim.api.nvim_set_hl(0, 'GitSignsTopdelete', { link = 'GitGutterDelete' })
    end,
  },
  { -- Git Blame plugin for Neovim written in Lua
    'f-person/git-blame.nvim',
    event = 'VeryLazy',
    config = function()
      -- initially don't show with virtual text
      vim.g.gitblame_display_virtual_text = 0
      vim.g.gitblame_ignored_filetypes = { 'gitcommit' }
      -- toggle virtual text
      vim.keymap.set('n', '<space>gbt', function()
        if vim.g.gitblame_display_virtual_text == 0 then
          vim.g.gitblame_display_virtual_text = 1
        else
          vim.g.gitblame_display_virtual_text = 0
        end
      end, { silent = true, noremap = true, desc = 'toggle git blame virtual text' })
    end,
  },
  { -- resolving merge conflicts with ease
    'spacedentist/resolve.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {},
  },
}

-- vim: ts=2 sts=2 sw=2 et
