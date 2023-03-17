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
          ensure_installed = { 'lua_ls', 'jsonls', 'astro', 'yamlls', 'dockerls' },
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
  { -- to complement the built-in `vim.lsp.buf.hover`
    'lewis6991/hover.nvim', -- Hover plugin framework for Neovim
    event = 'VeryLazy',
    -- enabled = false,
    config = function()
      require('hover').setup {
        init = function()
          -- Require providers
          require 'hover.providers.lsp'
          require 'hover.providers.gh'
          require 'hover.providers.gh_user'
          require 'hover.providers.jira'
          require 'hover.providers.man'
          require 'hover.providers.dictionary'
        end,
        preview_opts = {
          border = nil,
        },
        -- Whether the contents of a currently open hover window should be moved
        -- to a :h preview-window when pressing the hover keymap.
        preview_window = false,
        title = true,
      }
      vim.keymap.set('n', 'gh', require('hover').hover, { desc = 'hover.nvim' })
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
