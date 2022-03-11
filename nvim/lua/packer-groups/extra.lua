-- extra but nice

return {
  'joshdick/onedark.vim', -- Theme inspired by Atom
  {
    'overcache/NeoSolarized',
    config = function()
      vim.cmd [[
  colorscheme NeoSolarized
  set background=light
]]
    end,
  },
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
  'ap/vim-buftabline', -- simple and light tab (actually buffer) visualizer
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
}

-- vim: ts=2 sts=2 sw=2 et
