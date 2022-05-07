-- system level plugins or regarding integration

return {
  'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  'christoomey/vim-tmux-navigator', -- navigate with tmux key binding
  {
    'akinsho/toggleterm.nvim', -- a great ergonomic terminal customization
    config = function()
      -- augment terminal
      vim.cmd [[
        " cleaner approach than $FORCE_LOAD_MY_ZSH_STUFF
        let $UNSET_MY_BASIC_ZSH_STUFF_LOADED=1
        let $UNSET_ALL_MY_ZSH_STUFF_LOADED=1

        let $NO_VI_KEY_ON_ZSH = 1
      ]]

      require('toggleterm').setup {
        -- size can be a number or function which is passed the current terminal
        size = function(term)
          if term.direction == 'horizontal' then
            return 15
          elseif term.direction == 'vertical' then
            return vim.o.columns * 0.4
          end
        end, -- | 20
        open_mapping = [[<c-\>]],
        hide_numbers = true, -- hide the number column in toggleterm buffers
        shade_filetypes = {},
        shade_terminals = true,
        shading_factor = 3, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
        start_in_insert = true,
        insert_mappings = true, -- whether or not the open mapping applies in insert mode
        persist_size = true,
        direction = 'horizontal', -- | 'vertical' | 'window' | 'float',
        close_on_exit = true, -- close the terminal window when the process exits
        shell = vim.o.shell, -- change the default shell
        -- This field is only relevant if direction is set to 'float'
        float_opts = {
          -- The border key is *almost* the same as 'nvim_win_open'
          -- see :h nvim_win_open for details on borders however
          -- the 'curved' border is a custom border type
          -- not natively supported but implemented in this plugin.
          border = 'single', -- | 'double' | 'shadow' | 'curved' | ... other options supported by win open
          -- width = <value>,
          -- height = <value>,
          winblend = 3,
          highlights = {
            border = 'Normal',
            background = 'Normal',
          },
        },
      }

      local opts = { noremap = true }
      vim.api.nvim_set_keymap('t', '<esc>', [[<C-\><C-n>]], opts)
      vim.api.nvim_set_keymap('t', 'jk', [[<C-\><C-n>]], opts)
      vim.api.nvim_set_keymap('t', '<C-h>', [[<C-\><C-n><C-W>h]], opts)
      vim.api.nvim_set_keymap('t', '<C-j>', [[<C-\><C-n><C-W>j]], opts)
      vim.api.nvim_set_keymap('t', '<C-k>', [[<C-\><C-n><C-W>k]], opts)
      vim.api.nvim_set_keymap('t', '<C-l>', [[<C-\><C-n><C-W>l]], opts)
    end,
  },
  -- UI to select things (files, grep results, open buffers...)
  {
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
  {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      -- `nvim_tree_callback` and `cb` is deprecated
      local tree_cb = require('nvim-tree.config').nvim_tree_callback

      require('nvim-tree').setup {
        view = {
          side = 'right',
          mappings = {
            custom_only = false,
            -- override default mappings
            list = {
              { key = { 'o', '<2-LeftMouse>', 'l', 'e' }, action = 'edit' },
              -- { key = {"<2-RightMouse>", "<C-]>", "<CR>"}, cb = tree_cb("cd") },
              { key = { '-', 'h' }, action = 'dir_up' },
              { key = { '<Tab>' }, cb = ':wincmd w<CR>' },
              { key = { 'q' }, cb = ':q<CR>' },
            },
          },
        },
        -- auto_open = true,
        auto_close = false,
        disable_netrw = false,
        hijack_netrw = false,
        open_on_setup = false,
      }
    end,
  }, -- enhanced filetree replacing netrw
  {
    'subnut/nvim-ghost.nvim',
    run = ':call nvim_ghost#installer#install()',
    cond = function()
      return vim.env.my_nvim_ghost ~= nil
    end,
  }, -- https://github.com/fregante/GhostText
  -- now nvim-cokeline takes over
  'ap/vim-buftabline', -- simple and light tab (actually buffer) visualizer
  -- {
  --   'noib3/nvim-cokeline', -- customzing buftabline
  --   requires = 'kyazdani42/nvim-web-devicons', -- If you want devicons
  --   config = function()
  --     local get_hex = require('cokeline/utils').get_hex
  --     require('cokeline').setup {
  --       default_hl = {
  --         fg = function(buffer)
  --           return buffer.is_focused and get_hex('ColorColumn', 'bg') or get_hex('Normal', 'fg')
  --         end,
  --         bg = function(buffer)
  --           return buffer.is_focused and get_hex('Normal', 'fg') or get_hex('ColorColumn', 'bg')
  --         end,
  --       },
  --
  --       components = {
  --         {
  --           text = function(buffer)
  --             return ' ' .. buffer.devicon.icon
  --           end,
  --           fg = function(buffer)
  --             return buffer.devicon.color
  --           end,
  --         },
  --         {
  --           text = function(buffer)
  --             return buffer.unique_prefix
  --           end,
  --           fg = get_hex('Comment', 'fg'),
  --           style = 'italic',
  --         },
  --         {
  --           text = function(buffer)
  --             return buffer.filename .. ' '
  --           end,
  --         },
  --         {
  --           text = '',
  --           delete_buffer_on_left_click = true,
  --         },
  --         {
  --           text = ' ',
  --         },
  --       },
  --     }
  --   end,
  -- },
  {
    'nvim-lualine/lualine.nvim',
    requires = {
      'kyazdani42/nvim-web-devicons',
      'SmiteshP/nvim-gps',
    },
    config = function()
      local gps = require 'nvim-gps'
      require('lualine').setup {
        sections = {
          lualine_c = {
            { gps.get_location, cond = gps.is_available },
          },
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
