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
{ 'numToStr/Comment.nvim', -- replacing 'tpope/vim-commentary'
    config = function()
      require('Comment').setup()
    end
  },
{ 'andymass/vim-matchup', -- kind of like vim-surround but not
    -- enables `ds%` and `cs%` via vim-matchup
    event = 'VimEnter',
    config = function ()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_hi_surround_always = 1
    end
  },
{ 'McAuleyPenney/tidy.nvim',
    event = 'BufWritePre' }, -- remove trailing whitespace when save
}

-- vim: ts=2 sts=2 sw=2 et