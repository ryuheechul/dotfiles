-- editing enhancements

return {
  'christoomey/vim-titlecase', -- `gz[il]` to titlecase [the whole line]
  'tpope/vim-surround', -- surround text with something like quotes
  { -- for splitting/joining blocks of code
    'Wansmer/treesj',
    -- with default keymaps:
    -- <space>m - toggle
    -- <space>j - join
    -- <space>s - split (slow just because of <space>s is a prefix for other keys)
    requires = { 'nvim-treesitter' },
    config = function()
      require('treesj').setup {}
    end,
  },
  'axelf4/vim-strip-trailing-whitespace', -- strip whitespace on save
  'tpope/vim-repeat', -- enhance `.` to repeat on non-native functionality like vim-surround
  'sheerun/vim-polyglot', -- one plugin to accomodate many different filetypes
  { 'kana/vim-textobj-line', requires = { 'kana/vim-textobj-user' } },
  -- for more text objects, visit https://github.com/kana/vim-textobj-user/wiki
  { 'kana/vim-textobj-entire', requires = { 'kana/vim-textobj-user' } },
  -- to fallback in case no treesitter
  { 'sgur/vim-textobj-parameter', requires = { 'kana/vim-textobj-user' } },
  { -- replacing 'tpope/vim-commentary'
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()

      local ft = require 'Comment.ft'
      -- correct comment character
      ft.set('mermaid', '%%%s')
      ft.set('Earthfile', '#%s')
    end,
  },
  { -- kind of like vim-surround but not that enables `ds%` and `cs%` via vim-matchup
    'andymass/vim-matchup',
    event = 'VimEnter',
    config = function()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_hi_surround_always = 1
    end,
  },
  { -- remove trailing whitespace when save
    'McAuleyPenney/tidy.nvim',
    event = 'BufWritePre',
  },
  { -- show preview when `gd`
    'rmagatti/goto-preview',
    config = function()
      require('goto-preview').setup {}
    end,
  },
  -- choosing `nvim-parinfer` over `parinfer-rust` for now
  'gpanders/nvim-parinfer', -- editing helper for lisp family langs
  -- {
  --   'eraserhd/parinfer-rust', -- editing helper for lisp family langs
  --   run = 'nix-shell --run "cargo build --release"',
  -- },
  'arthurxavierx/vim-caser', -- convert between cases - https://github.com/arthurxavierx/vim-caser#usage
  -- | Default Mapping | Case                                    |
  -- | --------------- | --------------------------------------- |
  -- | `gsm` or `gsp`  | `MixedCase` or `PascalCase`             |
  -- | `gsc`           | `camelCase`                             |
  -- | `gs_`           | `snake_case`                            |
  -- | `gsu` or `gsU`  | `UPPER_CASE`                            |
  -- | `gst`           | `Title Case`                            |
  -- | `gss`           | `Sentence case`                         |
  -- | `gs<space>`     | `space case`                            |
  -- | `gs-` or `gsk`  | `dash-case` or `kebab-case`             |
  -- | `gsK`           | `Title-Dash-Case` or `Title-Kebab-Case` |
  -- | `gs.`           | `dot.case`                              |
  { -- Multiple cursors plugin for vim/neovim
    'mg979/vim-visual-multi',
    config = function()
      vim.g.VM_maps = {
        ['Add Cursor Down'] = '<M-Down>',
        ['Add Cursor Up'] = '<M-Up>',
      }
    end,
    -- | keys            | description                             |
    -- |-----------------|-----------------------------------------|
    -- | Ctrl-N          | select words with                       |
    -- | Alt-Down/Alt-Up | create cursors vertically with          |
    -- | n/N             | to get next/previous occurrence         |
    -- | q               | to skip current and get next occurrence |
    -- | Q               | to remove current cursor/selection      |
    -- | i,a,I,A         | start insert mode with                  |
  },
  { -- Space Age seD in neovim. A project wide find and replace plugin with sad & fzf
    'ray-x/sad.nvim',
    requires = 'ray-x/guihua.lua',
    config = function()
      require('sad').setup {}

      vim.keymap.set(
        'n',
        '<space>sr',
        ':Sad<CR>',
        { silent = true, noremap = true, desc = 'search and replace in project' }
      )
    end,
  },
  { -- https://github.com/ziontee113/syntax-tree-surfer/tree/d6d518f48dcc4441b11ee3e6cefd48fa1e09568a
    'ziontee113/syntax-tree-surfer',
    config = require 'packer-groups.config.tree-surfer',
  },
}

-- vim: ts=2 sts=2 sw=2 et
