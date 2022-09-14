-- completion & snippets

return {
  'hrsh7th/nvim-cmp', -- Autocompletion plugin
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-nvim-lua',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-path',
  -- 'hrsh7th/cmp-cmdline' -- I don't understand what this is yet so skip loading for now.
  'saadparwaiz1/cmp_luasnip',
  'rafamadriz/friendly-snippets',
  {
    'tzachar/cmp-tabnine', -- AI helper to type quicker
    run = './install.sh',
    requires = 'hrsh7th/nvim-cmp',
    cond = function()
      return vim.env.my_nvim_tabnine ~= nil
    end,
  },
  {
    'L3MON4D3/LuaSnip', -- Snippets plugin
    requires = 'onsails/lspkind-nvim',
    config = require 'packer-groups.config.luasnip',
  },
  {
    'heavenshell/vim-jsdoc', -- :JsDoc (at the function) to generate documentation based on function signature
    run = 'make install',
  },
}

-- vim: ts=2 sts=2 sw=2 et
