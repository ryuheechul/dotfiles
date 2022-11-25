-- completion & snippets

return {
  { -- Autocompletion plugin
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp', -- nvim-cmp source for neovim builtin LSP client
      'hrsh7th/cmp-nvim-lsp-signature-help', -- source for displaying function signatures with the current parameter emphasized
      'hrsh7th/cmp-nvim-lua', -- nvim-cmp source for neovim Lua API
      'hrsh7th/cmp-buffer', -- nvim-cmp source for words in the buffer
      'hrsh7th/cmp-path', -- nvim-cmp source for path
      'hrsh7th/cmp-cmdline', -- nvim-cmp source for vim's cmdline
      'dmitmel/cmp-cmdline-history', -- Source for nvim-cmp which reads results from command-line or search histories
      'L3MON4D3/LuaSnip', -- Snippets plugin
      'saadparwaiz1/cmp_luasnip', -- luasnip completion source for nvim-cmp
      'rafamadriz/friendly-snippets', -- Set of preconfigured snippets for different languages
      'lukas-reineke/cmp-under-comparator', -- nvim-cmp comparator function for completion items that start with one or more underlines
      'onsails/lspkind-nvim', -- vscode-like pictograms for neovim lsp completion items
      { -- AI helper to type quicker
        'tzachar/cmp-tabnine',
        run = './install.sh',
        requires = 'hrsh7th/nvim-cmp',
        cond = function()
          return vim.env.my_nvim_tabnine ~= nil
        end,
      },
    },
    config = require 'packer-groups.config.completion',
  },
  { -- :JsDoc (at the function) to generate documentation based on function signature
    'heavenshell/vim-jsdoc',
    run = 'make install',
    config = function()
      vim.g.jsdoc_formatter = 'tsdoc'
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
