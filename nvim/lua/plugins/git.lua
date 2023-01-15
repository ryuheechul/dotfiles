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
      -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev
      'sindrets/diffview.nvim',
      'nvim-lua/plenary.nvim',
    },
    event = 'VeryLazy',
    config = require 'plugins.config.git',
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
        add = { hl = 'GitGutterAdd', text = '+' },
        change = { hl = 'GitGutterChange', text = '~' },
        delete = { hl = 'GitGutterDelete', text = '_' },
        topdelete = { hl = 'GitGutterDelete', text = '‾' },
        changedelete = { hl = 'GitGutterChange', text = '~' },
      },
    },
  },
}

-- vim: ts=2 sts=2 sw=2 et
