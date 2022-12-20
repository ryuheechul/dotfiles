-- system level plugins or regarding integration

return {
  'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  'ojroques/nvim-bufdel', -- A Neovim plugin to improve buffer deletion
  'christoomey/vim-tmux-navigator', -- navigate with tmux key binding
  { -- a great ergonomic terminal customization
    'akinsho/toggleterm.nvim',
    config = require 'packer-groups.config.term',
  },
  --- although floaterm was cool to use and convenient, I met a weird dead-end performance issue on typing only with the combination of followings
  --- bufferline.nvim (only when tab is visible) + pyright lsp (with many files unlisted) + vim-floaterm activated (even after closed the term)
  --- therefore I just use toggleterm instead to mimic the usage of floaterm
  --- indirectly related issue - https://github.com/neovim/neovim/issues/21452
  -- 'voldikss/vim-floaterm', -- 🌟 Terminal manager for (neo)vim
  { -- UI to select things (files, grep results, open buffers...)
    'nvim-telescope/telescope.nvim',
    requires = {
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
    requires = 'nvim-tree/nvim-web-devicons',
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
  { -- A VS Code like winbar for Neovim
    'utilyre/barbecue.nvim',
    requires = {
      'neovim/nvim-lspconfig',
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      -- optionally uses it and otherwise it will fallback to lualine via the individual logic below
      if vim.env.my_nvim_winbar_barbecue ~= nil then
        require('barbecue').setup()
      end
    end,
  },
  { -- A blazing fast and easy to configure Neovim statusline written in Lua
    'nvim-lualine/lualine.nvim',
    requires = {
      'nvim-tree/nvim-web-devicons',
      'SmiteshP/nvim-navic', -- Simple winbar/statusline plugin that shows your current code context
    },
    config = require 'packer-groups.config.bars',
  },
  { -- A lua profiler for neovim that is discovered thanks to https://www.reddit.com/r/neovim/comments/xicxox/comment/ip2hprd
    'stevearc/profile.nvim',
    config = require 'packer-groups.config.profile',
  },
}

-- vim: ts=2 sts=2 sw=2 et
