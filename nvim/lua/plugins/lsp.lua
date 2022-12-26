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
    },
    config = require 'plugins.config.lsp',
  },
  { -- use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
    'jose-elias-alvarez/null-ls.nvim',
    dependencies = {
      'lukas-reineke/lsp-format.nvim', -- A wrapper around Neovims native LSP formatting
    },
    config = function()
      local null_ls = require 'null-ls'
      local lspformat = require 'lsp-format'
      -- TODO: there is a duplication at ../editing.lua, should be resolved later
      lspformat.setup {}

      null_ls.setup {
        -- -- not using boilerplate from README to favor https://github.com/lukas-reineke/lsp-format.nvim
        -- -- in fact this might the culprit for the issue below that ask between two on saving
        -- -- https://www.reddit.com/r/neovim/comments/ubgi6h/nullls_issues/
        --
        -- you can reuse a shared lspconfig on_attach callback here
        -- on_attach = function(client)
        --   if client.resolved_capabilities.document_formatting then
        --     vim.cmd [[
        --     augroup LspFormatting
        --         autocmd! * <buffer>
        --         autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
        --     augroup END
        --     ]]
        --   end
        -- end,

        -- using lsp-format instead to enable format on save
        on_attach = function(client)
          -- assuming `lspformat.setup {}` is called from `./config/lsp.lua` already
          require('lsp-format').on_attach(client)
        end,

        sources = {
          null_ls.builtins.formatting.stylua,
        },
      }
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
