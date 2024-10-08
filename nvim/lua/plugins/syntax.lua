-- treesitter & other syntax highlighting stuff
return {
  -- vim-polyglot actually seems to conflict on how to handle treesitter handling highlights and stuff
  -- and as I'm betting more on treesitter, time to say good bye
  -- - https://github.com/nvim-treesitter/nvim-treesitter/issues/5812
  -- { -- one plugin to accomodate many different filetypes
  --   'sheerun/vim-polyglot',
  --   -- couldn't figure out how to work this with lazy loading option yet
  --   lazy = false,
  --   init = function()
  --     -- turn off sensible as I don't agree with it - https://github.com/sheerun/vim-polyglot/tree/master#default-settings
  --     vim.g.polyglot_disabled = { 'sensible' }
  --   end,
  -- },
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  { -- Nvim Treesitter configurations and abstraction layer
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'RRethy/nvim-treesitter-textsubjects',
      'nvim-treesitter/nvim-treesitter-textobjects', -- Additional textobjects for treesitter
    },
    -- Parsers must be installed manually via :TSInstall
    build = ':TSUpdate',
    config = require 'plugins.config.treesitter',
    -- if `ensure_installed` are not kicked off, chances are the whole chunk is not being loaded for some reason
    -- try clean install (by commenting out this whole chunk for lazy.nvim to detect and delete and uncomment)
  },
  { -- Highlight arguments' definitions and usages, using Treesitter
    'm-demare/hlargs.nvim',
    event = 'VeryLazy',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
    config = true,
  },
  -- remove until it become stable as currently it is not https://github.com/rest-nvim/rest.nvim/issues/306
  -- https://hurl.dev/ is the good alternative to `http` files + `rest.nvim`
  -- { -- A fast Neovim http client written in Lua
  --   'rest-nvim/rest.nvim',
  --   event = 'VeryLazy',
  --   dependencies = { 'nvim-lua/plenary.nvim' },
  --   config = require 'plugins.config.rest',
  -- },
  -- turn off due to occasional lags
  -- 'haringsrob/nvim_context_vt', -- show context via virtual text
  { -- Show code context
    'nvim-treesitter/nvim-treesitter-context',
    event = 'VimEnter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
    opts = {
      -- improves performance a lot (on slower state of the machine)
      -- by removing the needs on calculating on cursor gliding through
      -- different lines when top line is the same
      mode = 'topline',
      min_window_height = 10,
    },
  },
  { -- Good enough syntax highlight for MDX in Neovim using Treesitter
    'davidmh/mdx.nvim',
    config = true,
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
  },
  -- nvim-treesitter/playground has been replaced by Neovim itself (deprecation notice and the guide in the repo)
  -- use these in place of :TSPlaygroundToggle
  -- - `:Inspect` to show the highlight groups under the cursor
  -- - `:InspectTree` to show the parsed syntax tree ("TSPlayground")
  -- - `:EditQuery` to open the Live Query Editor (Nvim 0.10+)
  -- { -- debug treesitter context via `:TSPlaygroundToggle`
  --   'nvim-treesitter/playground',
  --   event = 'VeryLazy',
  --   dependencies = {
  --    'nvim-treesitter/nvim-treesitter',
  --   },
  -- },
}

-- vim: ts=2 sts=2 sw=2 et
