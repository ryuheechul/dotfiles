-- treesitter & other syntax highlighting stuff
return {
  { -- Highlight Earthfile syntax
    'earthly/earthly.vim',
    ft = 'Earthfile',
  },
  {
    'imsnif/kdl.vim',
    ft = 'kdl',
  },
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  require('utils.nixos-shim').nvim_treesitter.extend {
    -- 'nvim-treesitter/nvim-treesitter', -- <- this is provided via `extend`
    dependencies = {
      'RRethy/nvim-treesitter-textsubjects',
      'nvim-treesitter/nvim-treesitter-textobjects', -- Additional textobjects for treesitter
    },
    -- Parsers must be installed manually via :TSInstall
    build = ':TSUpdate',
    config = require 'plugins.config.treesitter',
  },
  { -- Highlight arguments' definitions and usages, using Treesitter
    'm-demare/hlargs.nvim',
    event = 'VeryLazy',
    dependencies = {
      require('utils.nixos-shim').nvim_treesitter.base,
    },
    config = true,
  },
  { -- A fast Neovim http client written in Lua
    'rest-nvim/rest.nvim',
    event = 'VeryLazy',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = require 'plugins.config.rest',
  },
  -- turn off due to occasional lags
  -- 'haringsrob/nvim_context_vt', -- show context via virtual text
  { -- Show code context
    'nvim-treesitter/nvim-treesitter-context',
    event = 'VimEnter',
    dependencies = {
      require('utils.nixos-shim').nvim_treesitter.base,
    },
  },
  { -- debug treesitter context via `:TSPlaygroundToggle`
    'nvim-treesitter/playground',
    event = 'VeryLazy',
    dependencies = {
      require('utils.nixos-shim').nvim_treesitter.base,
    },
  },
}

-- vim: ts=2 sts=2 sw=2 et
