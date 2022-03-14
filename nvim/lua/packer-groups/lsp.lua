-- treesitter & lsp
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
  {
    'neovim/nvim-lspconfig', -- Collection of configurations for built-in LSP client
    requires = {
      'ray-x/lsp_signature.nvim', -- for floating signature hints
      'folke/lua-dev.nvim',
    },
    config = require 'packer-groups.config.lsp',
  },
}

-- vim: ts=2 sts=2 sw=2 et
