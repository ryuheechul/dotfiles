-- extra but nice

return {
  -- not necessary anymore with lazy.nvim https://github.com/folke/lazy.nvim/discussions/150
  -- 'lewis6991/impatient.nvim', -- Improve startup time for Neovim
  { -- A Vim plugin for Vim plugins `:Verbose` will be useful
    'tpope/vim-scriptease',
    event = 'VeryLazy',
  },
  { -- highlights headlines for markdown like files
    'lukas-reineke/headlines.nvim',
    ft = { 'markdown', 'org' },
    config = true,
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
    build = 'UpdateRemotePlugins',
    -- to favor nvim-cmp and noice.nvim
    enabled = false,
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
    opts = {
      func_map = {
        -- comments on the right for default mapping
        open = '<Tab>', -- `<CR>`
        tab = '', -- `t`
        tabb = '', -- `T`
        stoggledown = 't', -- `<Tab>`
        stoggleup = 'T', -- `<S-Tab>`
        stogglevm = '', -- `<Tab>`
      },
    },
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
    event = 'VeryLazy',
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
    event = 'FocusGained',
    -- this plugin basically replaces the plugins below for my use case
    -- 'junegunn/goyo.vim', -- a helper to focus on one window
    -- 'beauwilliams/focus.nvim', -- automatically adjust the size for focused windows
    dependencies = {
      'anuvyklack/middleclass', -- Object-orientation for Lua
      'runiq/neovim-throttle-debounce',
      -- 'anuvyklack/animation.nvim', disable since I don't need it
    },
    init = function()
      -- this is nothing to do with this plugin but placing here for my own consistency
      -- assume that activity on changing windows want to maximize tmux
      local windowsGrp = vim.api.nvim_create_augroup('MyWindowsAG', { clear = true })
      vim.api.nvim_create_autocmd('WinEnter', {
        callback = require('throttle-debounce').throttle_leading(function()
          vim.cmd [[silent exec "!tmux-zoom"]]
        end, 500),
        group = windowsGrp,
      })
    end,
    config = true,
  },
  { -- Open the current word with custom openers, GitHub shorthands for example
    'ofirgall/open.nvim',
    event = 'VeryLazy',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'ofirgall/open-jira.nvim',
    },
    config = function()
      require('open').setup {}
      -- to open file instead, do `gf`
      vim.keymap.set('n', 'gx', require('open').open_cword, { noremap = true, silent = true, desc = 'open cword' })

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
    event = 'VeryLazy',
    config = function()
      require('numb').setup()
    end,
  },
  { -- allow you to save at non existing directory
    'jghauser/mkdir.nvim',
    event = 'BufWritePre',
    config = function()
      require 'mkdir'
    end,
  },
  -- it's little finicky so commenting out for now
  -- { 'wfxr/minimap.vim', provide minimap on the side
  --     build = 'cargo install --locked code-minimap',
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
    event = 'VeryLazy',
    dependencies = {
      {
        'anuvyklack/fold-preview.nvim',
        dependencies = {
          'anuvyklack/keymap-amend.nvim',
        },
        opts = { auto = 100 },
      },
    },
    init = function()
      -- let the code be folded by default
      -- useful with 'anuvyklack/pretty-fold.nvim'
      vim.o.foldmethod = 'expr'
      vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
      vim.o.foldminlines = 5
      vim.o.foldlevelstart = 2
    end,
    config = true,
  },
  { -- a lua powered greeter like vim-startify / dashboard-nvim
    'goolord/alpha-nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    event = 'VimEnter',
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
        button('ll', '  Lazy plugin manager', '<cmd>Lazy<CR>'),
        button('lp', '  Profile plugins', '<cmd>Lazy profile<CR>'),
        button('li', '  Install plugins', '<cmd>Lazy install<CR>'),
        button('lu', '  Update plugins', '<cmd>Lazy update<CR>'),
        button('q', '  Quit'),
      }

      require('alpha').setup(theta_config)
    end,
  },
  { -- prevents the contents being cramped on windows's open/close event
    'luukvbaal/stabilize.nvim',
    event = 'VeryLazy',
    opts = {
      -- by setting force to be false,
      -- it does not stabilize window when the content will be hidden behind the new windows
      force = false,
    },
    -- TODO: remove this section all together when neovim 0.9 becomes stable
    enabled = vim.version().minor < 9,
  },
  {
    'sbulav/nredir.nvim', -- Redirect the output of Vim or external command to scratch buffer, in LUA
    cmd = 'Nredir',
  },
  { -- Extensible Neovim Scrollbar
    'petertriho/nvim-scrollbar',
    -- waiting for this to be resolved - https://github.com/petertriho/nvim-scrollbar/issues/40
    event = 'VeryLazy',
    opts = {
      -- currently this is the most optimal way for me to deal with this issue, https://github.com/petertriho/nvim-scrollbar/issues/72
      throttle_ms = 2000,
    },
  },
  { -- NeoVim text object that finds diagnostics
    -- example usages
    -- cig - jump to the next diagnostic (or the one under the cursor) and CHANGE it (delete the text and enter insert mode)
    -- v[g - visually select the previous diagnostic
    -- d]g - delete the next diagnostic text (excluding any diagnostic under the cursor)
    'andrewferrier/textobj-diagnostic.nvim',
    event = 'VeryLazy',
    config = true,
  },
  {
    -- pretty cool motion plugin that turns s/f/t to be supercharged.
    -- it takes some to get used to but it's quite powerful and reduce cognitive loads
    -- while it could be super efficient with predicting some potential normal future
    -- they call it "clairvoyant" ability!
    -- my favorite used to be `f<enter>` to go to the end of the line with lightspeed.nvim
    -- and this is not the case anymore with leap as `f<enter>` would work as a repeat of the last search with flit.nvim
    'ggandor/leap.nvim',
    event = 'VeryLazy',
    dependencies = {
      {
        'ggandor/flit.nvim', -- f/F/t/T motions on steroids - f[any char]f(repeat until you are there)
        config = true,
      },
      -- { -- disable as it doesn't seem to be stable
      --   'ggandor/leap-spooky.nvim', -- 👻 Actions at a distance - pretty cool if it would work seamlessly
      --   config = true,
      -- },
    },
    config = function()
      require('leap').add_default_mappings()

      -- to give a feedback on search area
      vim.api.nvim_set_hl(0, 'LeapBackdrop', { bold = true })

      -- https://github.com/ggandor/leap.nvim#faq
      vim.keymap.del({ 'x', 'o' }, 'x')
      vim.keymap.del({ 'x', 'o' }, 'X')

      -- to give feedback on first two characters I'm searching for
      require('leap').opts.highlight_unlabeled_phase_one_targets = true
    end,
  },
  {
    -- syntax support for CODEOWNERS file
    'rhysd/vim-syntax-codeowners',
    ft = 'CODEOWNERS',
  },
  { -- A markdown preview directly in your neovim
    'ellisonleao/glow.nvim',
    ft = 'markdown',
    opts = {
      border = 'none',
    },
  },
  { -- Indent guides for Neovim
    'lukas-reineke/indent-blankline.nvim',
    event = 'VeryLazy',
    main = 'ibl', -- https://github.com/lukas-reineke/indent-blankline.nvim/wiki/Migrate-to-version-3
    opts = {
      indent = {
        char = '▏', -- consult ../boot/filetype.lua for tabstop and listchars
      },
      scope = {
        enabled = false,
      },
    },
  },
  -- to improve the default vim.ui interfaces which something like nvim-gfold.lua can benefits from
  {
    'stevearc/dressing.nvim', -- basically does the same thing as `nvim-telescope/telescope-ui-select.nvim`
    event = 'VeryLazy',
  },
  { -- using gfold to switch repo and have statusline component
    'AckslD/nvim-gfold.lua',
    -- because I'm not really using it yet and it makes big difference on start up for some reason
    -- so until I start using it I will defer optimizing this plugin at start up
    enabled = false,
    config = true,
  },
  { -- ✍️ All the npm/yarn commands I don't want to type
    'vuki656/package-info.nvim',
    ft = 'json',
    dependencies = 'MunifTanjim/nui.nvim',
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
    event = 'VeryLazy',
    config = true,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'MunifTanjim/nui.nvim',
    },
  },
  { -- Blazingly fast AI code generation, editing and completion
    'dense-analysis/neural',
    event = 'VeryLazy',
    dependencies = {
      'MunifTanjim/nui.nvim',
      'ElPiloto/significant.nvim',
    },
    opts = {
      mappings = {
        prompt = '<C-Space>',
        swift = '<C-n>',
      },
      open_ai = {
        api_key = vim.env.OPENAI_API_KEY,
      },
      ui = { icon = '' },
    },
  },
  { -- Interact with OpenAI's GPT-3 language model
    -- comes with these commands:
    -- ChatGPT
    -- ChatGPT
    -- ChatGPTRun
    -- ChatGPTActAs
    -- ChatGPTRunCustomCodeAction
    -- ChatGPTEditWithInstructions
    'jackMort/ChatGPT.nvim',
    event = 'VeryLazy',
    config = true,
    dependencies = {
      'MunifTanjim/nui.nvim',
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
    },
    cond = function()
      return vim.env.OPENAI_API_KEY ~= nil
    end,
  },
  { -- Git Blame plugin for Neovim written in Lua
    'f-person/git-blame.nvim',
    event = 'VeryLazy',
    config = function()
      -- initially don't show with virtual text
      vim.g.gitblame_display_virtual_text = 0
      vim.g.gitblame_ignored_filetypes = { 'gitcommit' }
      -- toggle virtual text
      vim.keymap.set('n', '<space>gb', function()
        if vim.g.gitblame_display_virtual_text == 0 then
          vim.g.gitblame_display_virtual_text = 1
        else
          vim.g.gitblame_display_virtual_text = 0
        end
      end, { silent = true, noremap = true, desc = 'toggle git blame virtual text' })
    end,
  },
  { -- 🍁 Fun little plugin that can be used as a screensaver and on your dashboard
    'folke/drop.nvim',
    event = 'FocusGained',
    config = function()
      require('drop').setup { theme = ({ 'xmas', 'stars', 'leaves', 'snow' })[math.random(1, 4)] }
    end,
    cond = function()
      -- TODO: handle the logic elsewhere like zsh and expose single env var to use here
      return vim.env.SSH_CONNECTION == nil and vim.env.WSL_DISTRO_NAME == nil and vim.env.my_nvim_enable_drop ~= nil
    end,
  },
  { -- 💥 completely replaces the UI for messages, cmdline and the popupmenu
    'folke/noice.nvim',
    dependencies = { -- Functions that allow you to call a function not more than once in a given timeframe.
      'runiq/neovim-throttle-debounce',
      'MunifTanjim/nui.nvim',
      -- optional nicer look
      'rcarriga/nvim-notify',
    },
    event = 'VeryLazy',
    init = function()
      vim.keymap.set(
        'n',
        '<space>fm',
        ':Noice telescope<CR>',
        { noremap = true, silent = true, desc = 'List Noice messages' }
      )

      local throttle = require('throttle-debounce').throttle_leading

      local throttled_callback = throttle(function()
        require('notify').dismiss {}
      end, 500)

      local noice_group = vim.api.nvim_create_augroup('MyNoiceAG', { clear = true })
      vim.api.nvim_create_autocmd('CursorMoved', {
        callback = throttled_callback,
        group = noice_group,
      })
    end,
    opts = {
      lsp = {
        -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
        override = {
          ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
          ['vim.lsp.util.stylize_markdown'] = true,
          ['cmp.entry.get_documentation'] = true,
        },
      },
      -- you can enable a preset for easier configuration
      presets = {
        command_palette = true, -- position the cmdline and popupmenu together
        long_message_to_split = true, -- long messages will be sent to a split
        inc_rename = false, -- enables an input dialog for inc-rename.nvim
        lsp_doc_border = false, -- add a border to hover docs and signature help
      },
    },
  },
  -- { -- A minimalist autoclose plugin
  --   'm4xshen/autoclose.nvim',
  --   event = 'VeryLazy',
  --   opts = {
  --     options = {
  --       disable_when_touch = true,
  --     },
  --   },
  -- },
  { -- An autopair plugin designed to have all the features that an autopair plugin needs.
    'altermo/ultimate-autopair.nvim',
    event = { 'InsertEnter', 'CmdlineEnter' },
    opts = {
      fastwarp = {
        enable = true,
        map = '<C-e>',
        cmap = '<C-e>',
      },
    },
  },
  { -- Keep clipboard in sync with Neovim without the peformance hit.
    'EtiamNullam/deferred-clipboard.nvim',
    event = 'VeryLazy',
    opts = {
      lazy = true,
      -- this option makes paste the yank from neovim itself slow so turning it off fow now
      -- fallback = 'unnamedplus',
    },
    -- Currently I need this on WSL
    -- and discovered thanks to https://github.com/neovim/neovim/issues/21739#issuecomment-1379704105
    cond = vim.env.WSL_DISTRO_NAME ~= nil,
  },
  {
    'hiphish/rainbow-delimiters.nvim',
    event = 'BufReadPre',
    init = function()
      vim.g.rainbow_delimiters = {
        highlight = {
          'Black',
          'RainbowDelimiterViolet',
          'RainbowDelimiterOrange',
          'RainbowDelimiterBlue',
          'RainbowDelimiterYellow',
          'RainbowDelimiterCyan',
          'RainbowDelimiterGreen',
          'RainbowDelimiterRed',
        },
      }
    end,
  },
  { -- A feature-rich Go development plugin
    -- it does the job for both ./editing.lua and ./debug.lua
    -- let's just put it here for now
    'ray-x/go.nvim',
    dependencies = { -- optional packages
      'ray-x/guihua.lua',
      'neovim/nvim-lspconfig',
      require('utils.nixos-shim').nvim_treesitter.base,
    },
    event = 'CmdlineEnter',
    ft = { 'go', 'gomod' },
    config = function()
      require('go').setup {}

      vim.keymap.set('n', '<leader>rt', ':GoTestFile<CR>', { silent = true, noremap = true, desc = 'run :GoTestFile' })
    end,
  },
}
