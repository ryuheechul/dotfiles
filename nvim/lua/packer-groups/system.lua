-- system level plugins or regarding integration

return {
  'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  'christoomey/vim-tmux-navigator', -- navigate with tmux key binding
  {
    'akinsho/nvim-toggleterm.lua', -- a great ergonomic terminal customization
    config = function()
      -- augment terminal
      vim.cmd [[
        let $FORCE_LOAD_MY_ZSH_STUFF = 1
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
        shading_factor = '<number>', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
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
              { key = { 'o', '<2-LeftMouse>', 'l' }, action = 'edit' },
              -- { key = {"<2-RightMouse>", "<C-]>", "<CR>"}, cb = tree_cb("cd") },
              { key = { '-', 'h' }, action = 'dir_up' },
              { key = { '<Tab>' }, cb = ':wincmd w<CR>' },
            },
          },
        },
        -- auto_open = true,
        auto_close = false,
        disable_netrw = false,
        hijack_netrw = false,
        open_on_setup = true,
      }
    end,
  }, -- enhanced filetree replacing netrw
  { 'subnut/nvim-ghost.nvim', run = ':call nvim_ghost#installer#install()' }, -- https://github.com/fregante/GhostText
}

-- vim: ts=2 sts=2 sw=2 et
