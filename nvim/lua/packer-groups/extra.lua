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
  {
    'RRethy/vim-illuminate', -- Highlight the same words at the cursor
    config = function()
      require('illuminate').configure {
        delay = 100,
      }
      -- paused when it brought lags from with lazyredraw
      -- require('illuminate').pause() -- comment out as nolazyredraw seemed to resolved that issue.
    end,
  },
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
  { -- Open the current word with custom openers, GitHub shorthands for example
    'ofirgall/open.nvim',
    requires = {
      'nvim-lua/plenary.nvim',
      'ofirgall/open-jira.nvim',
    },
    config = function()
      require('open').setup {}
      -- to open file instead, do `gf`
      vim.keymap.set('n', 'gx', require('open').open_cword)

      local jira_url = 'https://jira.atlassian.com/browse/'
      if vim.env.my_nvim_jira_url ~= nil then
        jira_url = vim.env.my_nvim_jira_url
      end

      require('open-jira').setup {
        url = jira_url,
      }
    end,
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
      vim.o.foldlevelstart = 2
    end,
  },
  { -- a lua powered greeter like vim-startify / dashboard-nvim
    'goolord/alpha-nvim',
    requires = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      local button = require('alpha.themes.dashboard').button
      local theta_config = require('alpha.themes.theta').config
      local layout = theta_config.layout
      local buttons = layout[#layout]
      buttons.val = {
        -- based on https://github.com/goolord/alpha-nvim/blob/a858e4e7b0805835e61fab2b54704450427d47c3/lua/alpha/themes/theta.lua#LL166C5-L166C5
        { type = 'text', val = 'Quick links', opts = { hl = 'SpecialComment', position = 'center' } },
        { type = 'padding', val = 1 },
        button('nl', '  Luapad', '<cmd>bd<CR><cmd>Luapad<CR>'),
        button('i', '  New file', '<cmd>ene<CR>'),
        button('SPC f f', '  Find file'),
        button('SPC f g', '  Live grep'),
        button('pu', '  Update plugins', '<cmd>PackerUpdate<CR>'),
        button('pi', '  Install plugins', '<cmd>PackerInstall<CR>'),
        button('pc', '  Compile plugins', '<cmd>PackerCompile<CR>:helptags ALL<CR>'),
        button('q', '  Quit'),
      }

      require('alpha').setup(theta_config)
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
  { -- Extensible Neovim Scrollbar
    'petertriho/nvim-scrollbar',
    config = function()
      require('scrollbar').setup {
        -- currently this is the most optimal way for me to deal with this issue, https://github.com/petertriho/nvim-scrollbar/issues/72
        throttle_ms = 2000,
      }
    end,
  },
  { -- NeoVim text object that finds diagnostics
    -- example usages
    -- cig - jump to the next diagnostic (or the one under the cursor) and CHANGE it (delete the text and enter insert mode)
    -- v[g - visually select the previous diagnostic
    -- d]g - delete the next diagnostic text (excluding any diagnostic under the cursor)
    'andrewferrier/textobj-diagnostic.nvim',
    config = function()
      require('textobj-diagnostic').setup()
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
  { -- A markdown preview directly in your neovim
    'ellisonleao/glow.nvim',
    config = function()
      require('glow').setup {
        border = 'none',
      }
    end,
  },
  { -- Indent guides for Neovim
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      -- show some hidden characters - these are vim builtin options
      vim.o.list = true
      vim.o.listchars = [[tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶]]

      require('indent_blankline').setup {
        -- -- disable this related to https://github.com/lukas-reineke/indent-blankline.nvim/issues/440
        -- show_current_context = true,
      }
    end,
  },
  -- to improve the default vim.ui interfaces which something like nvim-gfold.lua can benefits from
  'stevearc/dressing.nvim', -- basically does the same thing as `nvim-telescope/telescope-ui-select.nvim`
  { -- using gfold to switch repo and have statusline component
    'AckslD/nvim-gfold.lua',
    config = function()
      require('gfold').setup()
    end,
  },
  { -- ✍️ All the npm/yarn commands I don't want to type
    'vuki656/package-info.nvim',
    requires = 'MunifTanjim/nui.nvim',
    config = function()
      require('package-info').setup {}

      vim.keymap.set(
        'n',
        '<leader>nt',
        require('package-info').toggle,
        { silent = true, noremap = true, desc = 'toggle npm package versions' }
      )
    end,
  },
  { -- 🧞 Pluggable framework for using AI code assistance in Neovim
    -- at the visually selected sentence(s)
    -- <leader>mcs | code complete
    -- <leader>mds | generate docstring
    -- <leader>mas | alter code
    'jameshiew/nvim-magic',
    config = function()
      require('nvim-magic').setup()
    end,
    requires = {
      'nvim-lua/plenary.nvim',
      'MunifTanjim/nui.nvim',
    },
  },
}

-- vim: ts=2 sts=2 sw=2 et
