-- git settings

return function()
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
        ['e'] = actions.focus_entry,
        ['h'] = actions.toggle_flatten_dirs,
        ['l'] = actions.focus_entry,
        -- instead of closing one buffer, do `DiffviewClose`
        ['q'] = ':tabc<cr>',
      },
    },
  }

  local neogit = require 'neogit'

  neogit.setup {
    integrations = {
      -- leave it false since it doesn't work very well at the moment
      diffview = false,
    },
    mappings = {
      status = {
        ['h'] = 'Toggle',
        ['l'] = 'Toggle',
        ['<tab>'] = '',
      },
    },
  }
end
