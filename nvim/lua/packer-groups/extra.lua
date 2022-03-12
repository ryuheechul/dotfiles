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
  -- now nvim-cokeline takes over
  -- 'ap/vim-buftabline', -- simple and light tab (actually buffer) visualizer
  {
    'noib3/nvim-cokeline', -- customzing buftabline
    requires = 'kyazdani42/nvim-web-devicons', -- If you want devicons
    config = function()
      local get_hex = require('cokeline/utils').get_hex
      require('cokeline').setup {
        default_hl = {
          fg = function(buffer)
            return buffer.is_focused and get_hex('ColorColumn', 'bg') or get_hex('Normal', 'fg')
          end,
          bg = function(buffer)
            return buffer.is_focused and get_hex('Normal', 'fg') or get_hex('ColorColumn', 'bg')
          end,
        },

        components = {
          {
            text = function(buffer)
              return ' ' .. buffer.devicon.icon
            end,
            fg = function(buffer)
              return buffer.devicon.color
            end,
          },
          {
            text = function(buffer)
              return buffer.unique_prefix
            end,
            fg = get_hex('Comment', 'fg'),
            style = 'italic',
          },
          {
            text = function(buffer)
              return buffer.filename .. ' '
            end,
          },
          {
            text = '',
            delete_buffer_on_left_click = true,
          },
          {
            text = ' ',
          },
        },
      }
    end,
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
  {
    'itchyny/lightline.vim', -- Fancier statusline
    config = function()
      --Set statusbar
      vim.g.lightline = {
        colorscheme = 'onedark',
        active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } },
        component_function = { gitbranch = 'fugitive#head' },
      }
    end,
  },
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
  {
    'startup-nvim/startup.nvim', -- A highly configurable neovim startup screen
    requires = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' },
    config = function()
      require('startup').setup()
    end,
  },
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
}

-- vim: ts=2 sts=2 sw=2 et
