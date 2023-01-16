-- bars aka lualine settings

local item_filename = {
  'filename',
  file_status = true, -- Displays file status (readonly status, modified status)
  -- 0: Just the filename, 1: Relative path, 2: Absolute path
  path = 2,
  shorting_target = 80, -- Shortens path to leave 40 spaces in the window
  -- for other components. (terrible name, any suggestions?)
  symbols = {
    modified = '[+]', -- Text to show when the file is modified.
    readonly = '[-]', -- Text to show when the file is non-modifiable or readonly.
    unnamed = '[No Name]', -- Text to show for unnamed buffers.
  },
}

-- wrap with function to avoid having to require at the time of reading this file for some items
-- to favor performance over concise of syntax/structure

local item_gitblame = {
  function()
    return require('gitblame').get_current_blame_text()
  end,
  cond = function()
    local ft = vim.bo.filetype
    local ignored_fts = vim.g.gitblame_ignored_filetypes

    for _, ignored_ft in ipairs(ignored_fts) do
      if ft == ignored_ft then
        return false
      end
    end

    return require('gitblame').is_blame_text_available()
  end,
}

local item_dap = {
  function()
    local msg = '{}'
    local status = require('dap').status()
    if status and string.len(status) > 0 then
      msg = msg .. ' ' .. status
    end
    return msg
  end,
  cond = function()
    local is_session_active = not not require('dap').session()
    return is_session_active
  end,
}

local item_navic = {
  function()
    return require('nvim-navic').get_location()
  end,
  cond = function()
    return require('nvim-navic').is_available()
  end,
}

local item_keymap_layer = {
  function()
    return require('utils.keymap-layer-info').get_summary()
  end,
  cond = function()
    return require('utils.keymap-layer-info').determine_active()
  end,
}

return function()
  local lualine_setup = {
    options = {
      disabled_filetypes = {
        winbar = {
          'toggleterm',
          'dapui_watches',
          'dapui_stacks',
          'dapui_breakpoints',
          'dapui_scopes',
          'dapui_console',
        },
        statusline = {},
      },
    },
    sections = {
      lualine_c = {
        item_gitblame,
      },
      lualine_x = {
        item_filename,
        'lsp_progress',
        'encoding',
        'fileformat',
        'filetype',
      },
    },
    inactive_sections = {
      lualine_c = {}, -- dedup filename here since lualine_x shows full path
      lualine_x = {
        item_filename,
        'fileformat',
        'filetype',
      },
    },
  }

  -- fallback to lualine when no preference is set for barbecue
  if vim.env.my_nvim_winbar_barbecue == nil then
    local winbar = {
      lualine_a = {
        { 'filename' },
      },
      lualine_b = {
        item_keymap_layer,
        item_dap,
        item_navic,
      },
    }
    lualine_setup.winbar = winbar
    lualine_setup.inactive_winbar = winbar
  end

  require('lualine').setup(lualine_setup)
end

-- vim: ts=2 sts=2 sw=2 et
