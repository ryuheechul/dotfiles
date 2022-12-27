-- lsp stuff
return {
  { -- Collection of configurations for built-in LSP client
    'neovim/nvim-lspconfig',
    dependencies = {
      'ray-x/lsp_signature.nvim', -- for floating signature hints
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
      'arkav/lualine-lsp-progress', -- LSP Progress lualine componenet
      'SmiteshP/nvim-navic', -- Simple winbar/statusline plugin that shows your current code context
      'nanotee/sqls.nvim', -- Neovim plugin for sqls that leverages the built-in LSP client
      -- 💻 Neovim setup for init.lua and plugin development with full signature help, docs and completion for the nvim lua API.
      'folke/neodev.nvim',
    },
    config = require('plugins.config.lsp').lspconfig,
    event = 'VeryLazy',
  },
  { -- use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
    'jose-elias-alvarez/null-ls.nvim',
    dependencies = {
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
    },
    config = require('plugins.config.lsp').null_ls,
  },
}

-- vim: ts=2 sts=2 sw=2 et
