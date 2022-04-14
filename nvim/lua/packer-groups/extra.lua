-- extra but nice

return {
  {
    'lukas-reineke/headlines.nvim', -- highlights headlines for markdown like files
    config = function()
      require('headlines').setup()
    end,
  },
  {
    'beauwilliams/focus.nvim', -- automatically adjust the size for focused windows
    config = function()
      require('focus').setup()
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
  {
    'gelguy/wilder.nvim', -- A more adventurous wildmenu
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
  {
    'kevinhwang91/nvim-bqf', -- Better quickfix window in Neovim, polish old quickfix window
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
  'junegunn/goyo.vim', -- a helper to focus on one window
  'p00f/nvim-ts-rainbow', -- differnciate parenthesis with colors
  {
    'felipec/vim-sanegx', -- `gx` to open url
    event = 'BufRead',
  },
  {
    'nacro90/numb.nvim', -- let you peek lines without moving the cursor to the line
    config = function()
      require('numb').setup()
    end,
  },
  {
    'jghauser/mkdir.nvim', -- allow you to save at non existing directory
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
  {
    'anuvyklack/pretty-fold.nvim', -- easier to work with folded code
    config = function()
      require('pretty-fold').setup {}
      require('pretty-fold.preview').setup()

      -- let the code be folded by default
      -- useful with 'anuvyklack/pretty-fold.nvim'
      vim.o.foldmethod = 'expr'
      vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
      vim.o.foldminlines = 5
      vim.o.foldlevel = 2
    end,
  },
  -- this seems to take up startup time unfortunately, so disable it until I really need it
  -- {
  --   'startup-nvim/startup.nvim', -- A highly configurable neovim startup screen
  --   requires = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' },
  --   config = function()
  --     require('startup').setup()
  --   end,
  -- },
  {
    'luukvbaal/stabilize.nvim', -- prevents the contents being cramped on windows's open/close event
    config = function()
      require('stabilize').setup {
        -- by setting force to be false,
        -- it does not stabilize window when the content will be hidden behind the new windows
        force = false,
      }
    end,
  },
  {
    'SmiteshP/nvim-gps',
    requires = 'nvim-treesitter/nvim-treesitter',
    config = function()
      require('nvim-gps').setup()
    end,
  },
  'arkav/lualine-lsp-progress',
  'sbulav/nredir.nvim', -- Redirect the output of Vim or external command to scratch buffer, in LUA
  -- telescope extensions
  {
    -- it could be anything but this was chosen to be the head here
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
  {
    -- pretty cool motion plugin that turns s/f/t to be supurchared.
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
