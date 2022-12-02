-- treesitter & other syntax highlighting stuff
return {
  'earthly/earthly.vim', -- Highlight Earthfile syntax
  { -- Highlight, edit, and navigate code using a fast incremental parsing library
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'RRethy/nvim-treesitter-textsubjects',
      'nvim-treesitter/nvim-treesitter-textobjects', -- Additional textobjects for treesitter
    },
    -- Parsers must be installed manually via :TSInstall
    run = ':TSUpdate',
    config = require 'packer-groups.config.treesitter',
  },
  { -- Highlight arguments' definitions and usages, using Treesitter
    'm-demare/hlargs.nvim',
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('hlargs').setup()
    end,
  },
  { -- A fast Neovim http client written in Lua
    'rest-nvim/rest.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = require 'packer-groups.config.rest',
  },
  -- turn off due to occasional lags
  -- 'haringsrob/nvim_context_vt', -- show context via virtual text
  { -- Show code context
    'nvim-treesitter/nvim-treesitter-context',
    requires = {
      'nvim-treesitter/nvim-treesitter',
    },
  },
}

-- vim: ts=2 sts=2 sw=2 et
