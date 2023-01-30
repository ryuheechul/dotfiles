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
      'lspcontainers/lspcontainers.nvim', -- Neovim plugin for lspcontainers
      { 'folke/neodev.nvim', config = true },
      {
        'williamboman/mason-lspconfig.nvim',
        dependencies = { 'williamboman/mason.nvim', config = true },
        opts = {
          ensure_installed = { 'sumneko_lua', 'jsonls' },
        },
      },
    },
    config = require('plugins.config.lsp').lspconfig,
  },
  { -- use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
    'jose-elias-alvarez/null-ls.nvim',
    dependencies = {
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
    },
    config = require('plugins.config.lsp').null_ls,
  },
  { --  bringing many of the features of sourcegraph.com into Neovim
    'tjdevries/sg.nvim', -- this is technically not directly related to lsp but it uses lsp so it can stay in this file for now.
    -- try `nvim sg://github.com/ryuheechul/hexto256/-/main.go` from shell
    -- or `:edit sg://github.com/ryuheechul/hexto256/-/main.go` inside neovim
    -- I also made a zsh alias so it can be used like this, `sggh ryuheechul/hexto256 main.go`
    build = "np-build-via-nix-shell 'cargo build --workspace'",
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = true,
  },
}

-- vim: ts=2 sts=2 sw=2 et
