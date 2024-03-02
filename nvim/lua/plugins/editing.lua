-- editing enhancements

return {
  { 'christoomey/vim-titlecase', event = 'VeryLazy' }, -- `gz[il]` to titlecase [the whole line]
  { 'tpope/vim-surround', event = 'VeryLazy' }, -- surround text with something like quotes
  { -- for splitting/joining blocks of code
    'Wansmer/treesj',
    -- with default keymaps:
    -- <space>m - toggle
    -- <space>j - join
    -- <space>s - split (slow just because of <space>s is a prefix for other keys)
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
    event = 'VeryLazy',
    config = true,
  },
  { 'tpope/vim-repeat', event = 'VeryLazy' }, -- enhance `.` to repeat on non-native functionality like vim-surround
  { 'kana/vim-textobj-line', dependencies = { 'kana/vim-textobj-user' }, event = 'VeryLazy' },
  -- for more text objects, visit https://github.com/kana/vim-textobj-user/wiki
  { 'kana/vim-textobj-entire', dependencies = { 'kana/vim-textobj-user' }, event = 'VeryLazy' },
  -- to fallback in case no treesitter
  { 'sgur/vim-textobj-parameter', dependencies = { 'kana/vim-textobj-user' }, event = 'VeryLazy' },
  { -- replacing 'tpope/vim-commentary'
    'numToStr/Comment.nvim',
    dependencies = {
      { -- Neovim treesitter plugin for setting the commentstring based on the cursor location in a file.
        'JoosepAlviste/nvim-ts-context-commentstring',
        opts = {
          enable = true,
          enable_autocmd = false,
        },
      },
    },
    event = 'VeryLazy',
    config = function()
      require('Comment').setup {
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      }

      local ft = require 'Comment.ft'
      -- correct comment character
      ft.set('mermaid', '%%%s')
      ft.set('Earthfile', '#%s')
    end,
  },
  { -- kind of like vim-surround but not that enables `ds%` and `cs%` via vim-matchup
    'andymass/vim-matchup',
    event = 'VeryLazy',
    config = function()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_hi_surround_always = 1

      -- to reduce lags on CursorMove
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_timeout = 50
      vim.g.matchup_matchparen_insert_timeout = 50
      vim.g.matchup_matchparen_deferred_show_delay = 100
    end,
  },
  { -- remove trailing whitespace when save
    'mcauley-penney/tidy.nvim',
    event = 'BufWritePre',
    config = true,
  },
  { -- show preview when `gd`
    'rmagatti/goto-preview',
    event = 'VeryLazy',
    config = true,
  },
  { -- editing helper for lisp family langs
    'gpanders/nvim-parinfer',
    event = 'VeryLazy',
  }, -- choosing `nvim-parinfer` over `parinfer-rust` for now
  -- {
  --   'eraserhd/parinfer-rust', -- editing helper for lisp family langs
  --   build = 'nix-shell --run "cargo build --release"',
  -- },
  { -- convert between cases - https://github.com/arthurxavierx/vim-caser#usage
    'arthurxavierx/vim-caser',
    -- | Default Mapping | Case                                    |
    -- | --------------- | --------------------------------------- |
    -- | `gmm` or `gmp`  | `MixedCase` or `PascalCase`             |
    -- | `gmc`           | `camelCase`                             |
    -- | `gm_`           | `snake_case`                            |
    -- | `gmu` or `gmU`  | `UPPER_CASE`                            |
    -- | `gmt`           | `Title Case`                            |
    -- | `gms`           | `Sentence case`                         |
    -- | `gm<space>`     | `space case`                            |
    -- | `gm-` or `gmk`  | `dash-case` or `kebab-case`             |
    -- | `gmK`           | `Title-Dash-Case` or `Title-Kebab-Case` |
    -- | `gm.`           | `dot.case`                              |
    event = 'VeryLazy',
    init = function()
      vim.g.caser_prefix = 'gm' -- rationale: "Go Modify" and also to avoid conflict with leap.nvim
    end,
  },
  { -- Multiple cursors plugin for vim/neovim
    'mg979/vim-visual-multi',
    event = 'VeryLazy',
    cond = vim.env.my_nvim_visual_multi ~= nil,
    init = function()
      vim.g.VM_maps = {
        ['Add Cursor Down'] = '<M-Down>',
        ['Add Cursor Up'] = '<M-Up>',
        ['Find Under'] = '<C-m>',
        ['Find Subword Under'] = '<C-m>',
        ['Skip Region'] = 's',
        ['Remove Region'] = 'r',
      }
    end,
    -- | keys            | description                             |
    -- |-----------------|-----------------------------------------|
    -- | Ctrl-M          | select words with                       |
    -- | Alt-Down/Alt-Up | create cursors vertically with          |
    -- | n/N             | to get next/previous occurrence         |
    -- | s               | to skip current and get next occurrence |
    -- | r               | to remove current cursor/selection      | -- exit VM with one more r after reset
    -- | i,a,I,A         | start insert mode with                  |
  },
  { -- Space Age seD in neovim. A project wide find and replace plugin with sad & fzf
    'ray-x/sad.nvim',
    event = 'VeryLazy',
    dependencies = 'ray-x/guihua.lua',
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
    event = 'VeryLazy',
    cond = vim.env.my_nvim_syntax_tree_surfer ~= nil,
    config = require 'plugins.config.tree-surfer', -- follow the file to see key maps
  },
  { -- prevent the cursor from moving when using shift and filter actions
    'gbprod/stay-in-place.nvim',
    event = 'VeryLazy',
    opts = {},
  },
  { -- The Refactoring library based off the Refactoring book by Martin Fowler
    'ThePrimeagen/refactoring.nvim',
    event = 'VeryLazy',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
    },
    opts = {},
  },
}
