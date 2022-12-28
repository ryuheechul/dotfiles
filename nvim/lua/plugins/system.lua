-- system level plugins or regarding integration

return {
  -- two plugins below are probably no longer necessary due to `vim.opt.clipboard = 'unnamedplus'`
  -- 'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  -- 'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  { -- A Neovim plugin to improve buffer deletion
    'ojroques/nvim-bufdel',
    event = 'VimEnter',
    config = function()
      -- q to close in a smart way
      vim.keymap.set('n', 'q', require 'utils.my-smart-quit', { noremap = true, desc = 'quit smarter' })
    end,
  },
  { 'christoomey/vim-tmux-navigator', event = 'VimEnter' }, -- navigate with tmux key binding
  { -- a great ergonomic terminal customization
    'akinsho/toggleterm.nvim',
    config = require 'plugins.config.term',
    event = 'VeryLazy',
  },
  --- although floaterm was cool to use and convenient, I met a weird dead-end performance issue on typing only with the combination of followings
  --- bufferline.nvim (only when tab is visible) + pyright lsp (with many files unlisted) + vim-floaterm activated (even after closed the term)
  --- therefore I just use toggleterm instead to mimic the usage of floaterm
  --- indirectly related issue - https://github.com/neovim/neovim/issues/21452
  -- 'voldikss/vim-floaterm', -- 🌟 Terminal manager for (neo)vim
  { -- UI to select things (files, grep results, open buffers...)
    'nvim-telescope/telescope.nvim',
    event = 'VeryLazy',
    dependencies = {
      -- beginning of core dependencies
      'nvim-lua/plenary.nvim',
      'nvim-lua/popup.nvim',
      -- end of core dependencies
      'crispgm/telescope-heading.nvim', -- An extension for telescope.nvim that allows you to switch between headings
      {
        'ANGkeith/telescope-terraform-doc.nvim',
        -- temp workaround to deal with broken head
        commit = '73291b564fed413ced13e890144d359793b3860c',
      },
      'camgraff/telescope-tmux.nvim',
      'benfowler/telescope-luasnip.nvim',
      'xiyaowong/telescope-emoji.nvim',
      'cljoly/telescope-repo.nvim',
      'nvim-telescope/telescope-dap.nvim',
    },
    config = function()
      local telescope = require 'telescope'

      telescope.setup {
        defaults = {
          winblend = 20,
          mappings = {
            i = {
              ['<C-u>'] = false,
              ['<C-d>'] = false,
            },
          },
        },
        pickers = {
          find_files = {
            theme = 'ivy',
          },
          spell_suggest = {
            theme = 'cursor',
          },
        },
      }

      local exts = { 'heading', 'terraform_doc', 'tmux', 'luasnip', 'emoji', 'repo', 'dap' }

      for _, ext in ipairs(exts) do
        telescope.load_extension(ext)
      end
    end,
  },
  { -- A file explorer tree for neovim written in lua
    'nvim-tree/nvim-tree.lua',
    event = 'VeryLazy',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = {
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
    },
  }, -- enhanced filetree replacing netrw
  { -- https://github.com/fregante/GhostText
    'subnut/nvim-ghost.nvim',
    build = ':call nvim_ghost#installer#install()',
    cond = function()
      return vim.env.my_nvim_ghost ~= nil
    end,
  },
  { -- A snazzy bufferline for Neovim - an upgrade from 'ap/vim-buftabline'
    'akinsho/bufferline.nvim',
    event = 'VeryLazy',
    version = 'v3.*',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = {
      options = {
        diagnostics = 'nvim_lsp',
        always_show_bufferline = false,
      },
    },
  },
  { -- A VS Code like winbar for Neovim
    'utilyre/barbecue.nvim',
    dependencies = {
      'neovim/nvim-lspconfig',
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons',
    },
    event = 'VeryLazy',
    cond = function()
      return vim.env.my_nvim_winbar_barbecue ~= nil
    end,
    config = true,
  },
  { -- A blazing fast and easy to configure Neovim statusline written in Lua
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'SmiteshP/nvim-navic', -- Simple winbar/statusline plugin that shows your current code context
    },
    event = 'FocusGained',
    config = require 'plugins.config.bars',
  },
  { -- A lua profiler for neovim that is discovered thanks to https://www.reddit.com/r/neovim/comments/xicxox/comment/ip2hprd
    'stevearc/profile.nvim',
    config = require 'plugins.config.profile',
    cond = function()
      return vim.env.NVIM_PROFILE ~= nil
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
