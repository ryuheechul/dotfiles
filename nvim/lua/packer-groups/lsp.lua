-- treesitter & lsp & other syntax highlighting stuff
-- TODO: maybe I need to rename this file to something else
return {
  -- Additional textobjects for treesitter
  'nvim-treesitter/nvim-treesitter-textobjects',
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  {
    'nvim-treesitter/nvim-treesitter',
    -- Parsers must be installed manually via :TSInstall
    run = ':TSUpdate',
    config = require 'packer-groups.config.treesitter',
  },
  -- Highlight Earthfile syntax
  'earthly/earthly.vim',
  {
    'neovim/nvim-lspconfig', -- Collection of configurations for built-in LSP client
    requires = {
      'ray-x/lsp_signature.nvim', -- for floating signature hints
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
    },
    config = require 'packer-groups.config.lsp',
  },
}

-- vim: ts=2 sts=2 sw=2 et
