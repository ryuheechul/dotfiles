-- treesitter & lsp
return {
  'neovim/nvim-lspconfig', -- Collection of configurations for built-in LSP client
  -- Additional textobjects for treesitter
  'nvim-treesitter/nvim-treesitter-textobjects',
  -- Highlight, edit, and navigate code using a fast incremental parsing library
{ 'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  },
}

-- vim: ts=2 sts=2 sw=2 et
