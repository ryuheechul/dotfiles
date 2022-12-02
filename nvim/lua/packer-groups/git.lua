-- git related

return {
  'tpope/vim-fugitive', -- Git commands in nvim

  { -- magit for neovim
    'TimUntersberger/neogit',
    requires = {
      -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev
      'sindrets/diffview.nvim',
      'nvim-lua/plenary.nvim',
    },
    config = require 'packer-groups.config.git',
  },
  -- Changes Vim working directory to project root
  'airblade/vim-rooter', -- a replacement candidate, https://github.com/ahmedkhalf/project.nvim
  'tpope/vim-rhubarb', -- Fugitive-companion to interact with github
  -- Add git related info in the signs columns and popups
  {
    'lewis6991/gitsigns.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('gitsigns').setup {
        signs = {
          add = { hl = 'GitGutterAdd', text = '+' },
          change = { hl = 'GitGutterChange', text = '~' },
          delete = { hl = 'GitGutterDelete', text = '_' },
          topdelete = { hl = 'GitGutterDelete', text = '‾' },
          changedelete = { hl = 'GitGutterChange', text = '~' },
        },
      }
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
