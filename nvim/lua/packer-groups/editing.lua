-- editing enhancements

return {
  'christoomey/vim-titlecase', -- `gz[il]` to titlecase [the whole line]
  'tpope/vim-surround', -- surround text with something like quotes
  'AndrewRadev/splitjoin.vim', -- give you `gS` and `gJ`
  'axelf4/vim-strip-trailing-whitespace', -- strip whitespace on save
  'tpope/vim-repeat', -- enhance `.` to repeat on non-native functionality like vim-surround
  'sheerun/vim-polyglot', -- one plugin to accomodate many different filetypes
  { 'kana/vim-textobj-line', requires = { 'kana/vim-textobj-user' } },
  -- for more text objects, visit https://github.com/kana/vim-textobj-user/wiki
  { 'kana/vim-textobj-entire', requires = { 'kana/vim-textobj-user' } },
  -- to fallback in case no treesitter
  { 'sgur/vim-textobj-parameter', requires = { 'kana/vim-textobj-user' } },
  {
    'numToStr/Comment.nvim', -- replacing 'tpope/vim-commentary'
    config = function()
      require('Comment').setup()
    end,
  },
  {
    'andymass/vim-matchup', -- kind of like vim-surround but not
    -- enables `ds%` and `cs%` via vim-matchup
    event = 'VimEnter',
    config = function()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_hi_surround_always = 1
    end,
  },
  {
    'McAuleyPenney/tidy.nvim', -- remove trailing whitespace when save
    event = 'BufWritePre',
  },
  -- use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua
  {
    'jose-elias-alvarez/null-ls.nvim',
    config = function()
      local null_ls = require 'null-ls'

      null_ls.setup {
        -- you can reuse a shared lspconfig on_attach callback here
        on_attach = function(client)
          if client.resolved_capabilities.document_formatting then
            vim.cmd [[
            augroup LspFormatting
                autocmd! * <buffer>
                autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
            augroup END
            ]]
          end
        end,

        sources = {
          null_ls.builtins.formatting.stylua,
          -- haven't tried these two yet so be prepared to tweak as I face using them
          null_ls.builtins.diagnostics.eslint,
          null_ls.builtins.completion.spell,
        },
      }
    end,
  },
  {
    'rmagatti/goto-preview', -- show preview when `gd`
    config = function()
      require('goto-preview').setup {}
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
