-- bars aka lualine settings

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
      },
    },
    sections = {
      lualine_x = {
        -- opening lua file makes lsp work, if I `:PackerCompile` during that time I get an error below
        --[[
            Error executing vim.schedule lua callback: ...ine-lsp-progress/lua/lualine/components/lsp_progress.lua:80: attempt to index a nil value
            stack traceback:
            ...ine-lsp-progress/lua/lualine/components/lsp_progress.lua:80: in function 'progress_callback'
            ...ine-lsp-progress/lua/lualine/components/lsp_progress.lua:133: in function 'handler'
            ...eovim-unwrapped-0.6.1/share/nvim/runtime/lua/vim/lsp.lua:735: in function 'cb'
            vim.lua:285: in function <vim.lua:285>
            ]]
        -- so just wait for that to finish and do compile/sync
        {
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
        },
        'lsp_progress',
        'encoding',
        'fileformat',
        'filetype',
      },
    },
  }

  -- fallback to lualine when no preference is set for barbecue
  if vim.env.my_nvim_winbar_barbecue == nil then
    local navic = require 'nvim-navic'
    local winbar = {
      lualine_a = {
        { 'filename' },
      },
      lualine_b = {
        { require('utils.keymap-layer').get_summary, cond = require('utils.keymap-layer').determine_active },
        { navic.get_location, cond = navic.is_available },
      },
    }
    lualine_setup.winbar = winbar
    lualine_setup.inactive_winbar = winbar
  end

  require('lualine').setup(lualine_setup)
end

-- vim: ts=2 sts=2 sw=2 et
