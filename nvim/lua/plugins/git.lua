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
}

-- vim: ts=2 sts=2 sw=2 et
