-- extra but nice

return {
  'lewis6991/impatient.nvim', -- Improve startup time for Neovim
  'tpope/vim-scriptease', -- A Vim plugin for Vim plugins `:Verbose` will be useful
  { -- highlights headlines for markdown like files
    'lukas-reineke/headlines.nvim',
    config = function()
      require('headlines').setup()
    end,
  },
  -- stop using until https://github.com/sunjon/Shade.nvim/issues/6#issuecomment-1065939353 gets resolved
  -- {
  --   'sunjon/Shade.nvim', -- automatically dims not focused windows
  --   config = function()
  --     require('shade').setup {
  --       overlay_opacity = 60,
  --     }
  --   end,
  -- },
  { -- A more adventurous wildmenu
    'gelguy/wilder.nvim',
    run = 'UpdateRemotePlugins',
    config = function()
      local wilder = require 'wilder'
      wilder.setup { modes = { ':', '/', '?' } }
      wilder.set_option(
        'renderer',
        wilder.popupmenu_renderer {
          highlighter = wilder.basic_highlighter(),
          left = { ' ', wilder.popupmenu_devicons() },
          right = { ' ', wilder.popupmenu_scrollbar() },
        }
      )
    end,
  },
  'voldikss/vim-floaterm', -- 🌟 Terminal manager for (neo)vim
  { -- Better quickfix window in Neovim, polish old quickfix window
    'kevinhwang91/nvim-bqf',
    ft = 'qf',
  },
  -- this doesn't work until treesitter markdown gets install and it's currently unstable and fails to install
  -- {
  --   'jghauser/follow-md-links.nvim', -- <CR> at links to open them
  --   config = function()
  --     require 'follow-md-links'
  --   end,
  -- },
  'RRethy/vim-illuminate', -- Highlight the same words at the cursor
  'haringsrob/nvim_context_vt', -- show context via virtual text
  { -- Automatically expand width of the current window. Maximizes and restore it. And all this with nice animations!
    'anuvyklack/windows.nvim',
    -- this plugin basically replaces the plugins below for my use case
    -- 'junegunn/goyo.vim', -- a helper to focus on one window
    -- 'beauwilliams/focus.nvim', -- automatically adjust the size for focused windows
    requires = {
      'anuvyklack/middleclass',
      -- 'anuvyklack/animation.nvim', disable since I don't need it
    },
    config = function()
      require('windows').setup()
    end,
  },
  'p00f/nvim-ts-rainbow', -- differnciate parenthesis with colors
  { -- `gx` to open url
    'felipec/vim-sanegx',
    event = 'BufRead',
  },
  { -- let you peek lines without moving the cursor to the line
    'nacro90/numb.nvim',
    config = function()
      require('numb').setup()
    end,
  },
  { -- allow you to save at non existing directory
    'jghauser/mkdir.nvim',
    config = function()
      require 'mkdir'
    end,
  },
  -- it's little finicky so commenting out for now
  -- { 'wfxr/minimap.vim', provide minimap on the side
  --     run = 'cargo install --locked code-minimap',
  --     config = function()
  --       vim.g.minimap_width = 6
  --       vim.g.minimap_auto_start = 1
  --       vim.g.minimap_auto_start_win_enter = 1
  --       vim.g.minimap_highlight_range	= 1
  --       vim.g.minimap_highlight_search	= 1
  --       vim.g.minimap_git_colors = 1
  --     end,
  --   },
  { -- easier to work with folded code
    'anuvyklack/pretty-fold.nvim',
    requires = {
      'anuvyklack/nvim-keymap-amend', -- only for preview
      'anuvyklack/fold-preview.nvim',
    },
    config = function()
      require('pretty-fold').setup {}
      require('fold-preview').setup {}

      -- let the code be folded by default
      -- useful with 'anuvyklack/pretty-fold.nvim'
      vim.o.foldmethod = 'expr'
      vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
      vim.o.foldminlines = 5
      vim.o.foldlevel = 2
    end,
  },
  { -- a lua powered greeter like vim-startify / dashboard-nvim
    'goolord/alpha-nvim',
    requires = { 'kyazdani42/nvim-web-devicons' },
    config = function()
      require('alpha').setup(require('alpha.themes.theta').config)
    end,
  },
  { -- prevents the contents being cramped on windows's open/close event
    'luukvbaal/stabilize.nvim',
    config = function()
      require('stabilize').setup {
        -- by setting force to be false,
        -- it does not stabilize window when the content will be hidden behind the new windows
        force = false,
      }
    end,
  },
  'sbulav/nredir.nvim', -- Redirect the output of Vim or external command to scratch buffer, in LUA
  -- telescope extensions
  { -- it could be anything but this was chosen to be the head here
    'crispgm/telescope-heading.nvim',
    requires = {
      -- setup for these at at ./system.lua
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      -- end of dependencies and now other telescope extensions
      {
        'ANGkeith/telescope-terraform-doc.nvim',
        -- temp workaround to deal with broken head
        commit = '73291b564fed413ced13e890144d359793b3860c',
      },
      'camgraff/telescope-tmux.nvim',
      'benfowler/telescope-luasnip.nvim',
      'xiyaowong/telescope-emoji.nvim',
      'cljoly/telescope-repo.nvim',
    },
    config = function()
      require('telescope').load_extension 'heading'
      require('telescope').load_extension 'terraform_doc'
      require('telescope').load_extension 'tmux'
      require('telescope').load_extension 'luasnip'
      require('telescope').load_extension 'emoji'
      require('telescope').load_extension 'repo'
    end,
  },
  { -- Extensible Neovim Scrollbar
    'petertriho/nvim-scrollbar',
    config = function()
      require('scrollbar').setup()
    end,
  },
  { -- pretty cool motion plugin that turns s/f/t to be supurchared.
    -- it takes some to get used to but it's quite powerful and reduce cognitive loads
    -- while it could be super efficient with predicting some potential normal future
    -- they call it "clairvoyant" ability!
    -- my favorite is `f<enter>` to go to the end of the line
    'ggandor/lightspeed.nvim',
    config = function()
      require('lightspeed').setup {
        ignore_case = true,
      }
    end,
  },
  'rhysd/vim-syntax-codeowners', -- syntax support for CODEOWNERS file
}

-- vim: ts=2 sts=2 sw=2 et
