-- system level plugins or regarding integration

return {
  'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  'christoomey/vim-tmux-navigator', -- navigate with tmux key binding
  {
    'akinsho/toggleterm.nvim', -- a great ergonomic terminal customization
    config = require 'packer-groups.config.term',
  },
  { -- UI to select things (files, grep results, open buffers...)
    'nvim-telescope/telescope.nvim',
    requires = { 'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim' },
    config = function()
      require('telescope').setup {
        defaults = {
          winblend = 20,
          mappings = {
            i = {
              ['<C-u>'] = false,
              ['<C-d>'] = false,
            },
          },
        },
      }
    end,
  },
  { -- A file explorer tree for neovim written in lua
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require('nvim-tree').setup {
        view = {
          side = 'right',
          mappings = {
            custom_only = false,
            -- override default mappings
            list = {
              { key = { 'o', '<2-LeftMouse>', 'l', 'e' }, action = 'edit' },
              { key = { '-', 'h' }, action = 'dir_up' },
              { key = { '<Tab>' }, cb = ':wincmd w<CR>' },
              { key = { 'q' }, cb = ':q<CR>' },
            },
          },
        },
        -- auto_open = true,
        -- not available any more
        -- https://github.com/kyazdani42/nvim-tree.lua/blob/b2ba6dea7105d9afabd3af08abd93947b851a90f/lua/nvim-tree/legacy.lua#L213-L218
        -- auto_close = false,
        disable_netrw = false,
        hijack_netrw = false,
        open_on_setup = false,
      }
    end,
  }, -- enhanced filetree replacing netrw
  { -- https://github.com/fregante/GhostText
    'subnut/nvim-ghost.nvim',
    run = ':call nvim_ghost#installer#install()',
    cond = function()
      return vim.env.my_nvim_ghost ~= nil
    end,
  },
  { -- A snazzy bufferline for Neovim - an upgrade from 'ap/vim-buftabline'
    'akinsho/bufferline.nvim',
    tag = 'v3.*',
    requires = 'nvim-tree/nvim-web-devicons',
    config = function()
      require('bufferline').setup {
        options = {
          diagnostics = 'nvim_lsp',
          always_show_bufferline = false,
        },
      }
    end,
  },
  { -- A blazing fast and easy to configure Neovim statusline written in Lua
    'nvim-lualine/lualine.nvim',
    requires = {
      'kyazdani42/nvim-web-devicons',
      'SmiteshP/nvim-navic', -- Simple winbar/statusline plugin that shows your current code context
    },
    config = function()
      local navic = require 'nvim-navic'
      local winbar = {
        lualine_a = {
          { 'filename' },
        },
        lualine_b = {
          { navic.get_location, cond = navic.is_available },
        },
      }
      require('lualine').setup {
        winbar = winbar,
        inactive_winbar = winbar,
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
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
