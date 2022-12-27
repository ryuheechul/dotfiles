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
    },
    config = require 'plugins.config.completion',
  },
  { -- :JsDoc (at the function) to generate documentation based on function signature
    'heavenshell/vim-jsdoc',
    ft = { 'typescript', 'javascript' },
    build = 'make install',
    config = function()
      vim.g.jsdoc_formatter = 'tsdoc'
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
