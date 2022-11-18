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
    config = function()
      local actions = require 'diffview.actions'

      require('diffview').setup {
        keymaps = {
          view = {
            -- instead of cycle through another buffer, move around window
            ['<tab>'] = '<Cmd>wincmd w<CR>',
            -- instead of closing one buffer, do `DiffviewClose`
            ['q'] = actions.close,
          },
          file_panel = {
            -- just select them when moving
            ['j'] = actions.select_next_entry,
            ['k'] = actions.select_prev_entry,
            ['<down>'] = actions.select_next_entry,
            ['<up>'] = actions.select_prev_entry,
            -- all of them to just go to the diff2 (right panel) so you can edit right at the Diffview tab
            ['gf'] = actions.focus_entry,
            ['<tab>'] = actions.focus_entry,
            ['<cr>'] = actions.focus_entry,
          },
        },
      }

      local neogit = require 'neogit'

      neogit.setup {
        integrations = {
          -- leave it false since it doesn't work very well at the moment
          diffview = false,
        },
      }
    end,
  },
  'airblade/vim-rooter', -- Changes Vim working directory to project root
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
