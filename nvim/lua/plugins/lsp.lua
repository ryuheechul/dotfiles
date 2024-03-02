-- lsp stuff
return {
  { -- Collection of configurations for built-in LSP client
    'neovim/nvim-lspconfig',
    dependencies = {
      'ray-x/lsp_signature.nvim', -- for floating signature hints
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
      -- thanks for the news from https://dotfyle.com/this-week-in-neovim/48#creativenull/efmls-configs-nvim
      'creativenull/efmls-configs-nvim', -- An unofficial collection of linters and formatters configured for efm-langserver for neovim
      'arkav/lualine-lsp-progress', -- LSP Progress lualine componenet
      'SmiteshP/nvim-navic', -- Simple winbar/statusline plugin that shows your current code context
      -- 💻 Neovim setup for init.lua and plugin development with full signature help, docs and completion for the nvim lua API.
      'lspcontainers/lspcontainers.nvim', -- Neovim plugin for lspcontainers
      {
        'folke/neodev.nvim',
        opts = {
          library = { plugins = { 'neotest' }, types = true },
        },
      },
      {
        'williamboman/mason-lspconfig.nvim',
        dependencies = { 'williamboman/mason.nvim', config = true },
        opts = {
          ensure_installed = {
            -- disable `lua_ls` as there is a trouble executing it
            -- falling back to the one from ../../../nix/pkgs/lang/lua.nix
            -- 'lua_ls',
            'jsonls',
            'astro',
            'yamlls',
            'dockerls',
          },
        },
      },
    },
    config = require('plugins.config.lsp').lspconfig,
  },
  { --  bringing many of the features of sourcegraph.com into Neovim
    'sourcegraph/sg.nvim', -- this is technically not directly related to lsp but it uses lsp so it can stay in this file for now.
    -- try `nvim sg://github.com/ryuheechul/hexto256/-/main.go` from shell
    -- or `:edit sg://github.com/ryuheechul/hexto256/-/main.go` inside neovim
    -- I also made a zsh alias so it can be used like this, `sggh ryuheechul/hexto256 main.go`
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = true,
    -- optionally load due to the combination of:
    -- - the performance hit on loading (maybe requiring `nvim-cmp` internally from sg.nvim)
    -- - and `sg.nvim` cannot work properly on start up with `sggh` with lazy loading
    -- I made an alias via ../../../zsh/my_addons/aliases `vis` to toggle this easily
    cond = vim.env.my_nvim_use_sg ~= nil,
    build = "np-build-via-nix-shell 'cargo build --workspace'",
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
