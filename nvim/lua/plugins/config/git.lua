-- git settings

return function()
  local actions = require 'diffview.actions'

  -- comment these out as I'm not creating a new highlight but still leave here for future usage.
  -- local diffAddHl = vim.api.nvim_get_hl_by_name('DiffAdd', true)
  -- vim.api.nvim_set_hl(0, 'MyDiffAddText', { background = diffAddHl.background })

  require('diffview').setup {
    enhanced_diff_hl = true,
    hooks = {
      -- customize color to differentiate a and b - thanks to these for hints:
      -- https://github.com/sindrets/diffview.nvim/pull/258
      -- https://github.com/sindrets/diffview.nvim/issues/241
      diff_buf_win_enter = function(bufnr, winid, ctx)
        if ctx.layout_name:match '^diff2' then
          if ctx.symbol == 'a' then
            vim.opt_local.winhl = table.concat({
              'DiffText:DiffviewDiffAddAsDelete',
              'DiffAdd:DiffviewDiffAddAsDelete',
            }, ',')
          elseif ctx.symbol == 'b' then
            vim.opt_local.winhl = table.concat({
              'DiffText:DiffAdd', -- 'DiffText:MyDiffAddText',
            }, ',')
          end
        end
      end,
    },
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
