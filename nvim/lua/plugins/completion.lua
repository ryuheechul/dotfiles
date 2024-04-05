-- completion & snippets

return {
  { -- Autocompletion plugin
    'hrsh7th/nvim-cmp',
    event = 'VeryLazy',
    dependencies = {
      'L3MON4D3/LuaSnip', -- Snippets plugin
      'f3fora/cmp-spell', -- spell source for nvim-cmp based on vim's spellsuggest
      'hrsh7th/cmp-path', -- nvim-cmp source for path
      'hrsh7th/cmp-buffer', -- nvim-cmp source for words in the buffer
      'hrsh7th/cmp-cmdline', -- nvim-cmp source for vim's cmdline
      'hrsh7th/cmp-nvim-lsp', -- nvim-cmp source for neovim builtin LSP client
      'hrsh7th/cmp-nvim-lua', -- nvim-cmp source for neovim Lua API
      'onsails/lspkind-nvim', -- vscode-like pictograms for neovim lsp completion items
      'saadparwaiz1/cmp_luasnip', -- luasnip completion source for nvim-cmp
      'dmitmel/cmp-cmdline-history', -- Source for nvim-cmp which reads results from command-line or search histories
      'rafamadriz/friendly-snippets', -- Set of preconfigured snippets for different languages
      'lukas-reineke/cmp-under-comparator', -- nvim-cmp comparator function for completion items that start with one or more underlines
      'hrsh7th/cmp-nvim-lsp-signature-help', -- source for displaying function signatures with the current parameter emphasized
      { -- AI helper to type quicker
        'tzachar/cmp-tabnine',
        build = './install.sh',
        dependencies = 'hrsh7th/nvim-cmp',
        cond = function()
          return vim.env.my_nvim_tabnine ~= nil
        end,
      },
      { -- to turn github copilot into a cmp source
        'zbirenbaum/copilot-cmp',
        dependencies = {
          { -- Chat with GitHub Copilot in Neovim
            'CopilotC-Nvim/CopilotChat.nvim',
            branch = 'canary',
            dependencies = {
              { -- Fully featured & enhanced replacement for copilot.vim complete with API for interacting with Github Copilot
                'zbirenbaum/copilot.lua',
                opts = {},
              },
              { 'nvim-lua/plenary.nvim' }, -- for curl, log wrapper
            },
            opts = {},
          },
        },
        opts = {},
        cond = function()
          return vim.env.my_nvim_copilot ~= nil
        end,
      },
    },
    config = require 'plugins.config.completion',
  },
  { -- A better annotation generator. Supports multiple languages and annotation conventions
    'danymat/neogen',
    -- follow only stable versions
    version = '*',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'L3MON4D3/LuaSnip',
    },
    init = function()
      vim.keymap.set('n', '<Leader>gd', function()
        require('neogen').generate()
      end, { noremap = true, silent = true, desc = 'generate a docstring on cursor for multiple languages' })
    end,
    opts = { snippet_engine = 'luasnip' },
  },
}

-- vim: ts=2 sts=2 sw=2 et
