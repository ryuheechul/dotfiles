-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec(
  [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]],
  false
)

local use = require('packer').use
require('packer').startup(function()
  --- plugins from https://github.com/mjlbach/defaults.nvim/blob/73d4b205be5711b681ef2df9d171b1c55040803b/init.lua
  use 'wbthomason/packer.nvim' -- Package manager
  use 'tpope/vim-fugitive' -- Git commands in nvim
  use 'tpope/vim-rhubarb' -- Fugitive-companion to interact with github
  use {
    'numToStr/Comment.nvim', -- replacing 'tpope/vim-commentary'
    config = function()
      require('Comment').setup()
    end
  }
  -- comment since it creates more issue than a help for my usage
  -- use 'ludovicchabant/vim-gutentags' -- Automatic tags management
  -- UI to select things (files, grep results, open buffers...)
  use { 'nvim-telescope/telescope.nvim', requires = { { 'nvim-lua/popup.nvim' }, { 'nvim-lua/plenary.nvim' } } }
  use 'joshdick/onedark.vim' -- Theme inspired by Atom
  use 'itchyny/lightline.vim' -- Fancier statusline
  -- Add indentation guides even on blank lines
  -- comment until this issue gets resolved,
  -- https://github.com/lukas-reineke/indent-blankline.nvim/issues/74
  -- use 'lukas-reineke/indent-blankline.nvim'
  -- Add git related info in the signs columns and popups
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
  -- Additional textobjects for treesitter
  use 'nvim-treesitter/nvim-treesitter-textobjects'
  use 'neovim/nvim-lspconfig' -- Collection of configurations for built-in LSP client

  --- bringing plugins to accomodate my muscle memory with ../SpaceVim.d

  -- for more text objects, visit https://github.com/kana/vim-textobj-user/wiki
  use { 'kana/vim-textobj-line', requires = { 'kana/vim-textobj-user' } }
  use { 'kana/vim-textobj-entire', requires = { 'kana/vim-textobj-user' } }
  -- to fallback in case no treesitter
  use { 'sgur/vim-textobj-parameter', requires = { 'kana/vim-textobj-user' } }
  use 'christoomey/vim-system-copy' -- copy text to clipboard with `cp`
  use 'roxma/vim-tmux-clipboard' -- share clipboard with tmux
  use 'christoomey/vim-tmux-navigator' -- navigate with tmux key binding
  use 'christoomey/vim-titlecase' -- `gz[il]` to titlecase [the whole line]
  use 'tpope/vim-surround' -- surround text with something like quotes
  use 'sheerun/vim-polyglot' -- one plugin to accomodate many different filetypes
  use 'folke/which-key.nvim' -- show key bindings just like SpaceVim
  use 'junegunn/goyo.vim' -- a helper to focus on one window
  use 'akinsho/nvim-toggleterm.lua' -- a great ergonomic terminal customization
  use 'ap/vim-buftabline' -- simple and light tab (actually buffer) visualizer
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'nvim-tree'.setup {} end
  } -- enhanced filetree replacing netrw
  use 'axelf4/vim-strip-trailing-whitespace' -- strip whitespace on save
  use 'tpope/vim-repeat' -- enhance `.` to repeat on non-native functionality like vim-surround
  use 'overcache/NeoSolarized'

  use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  use 'onsails/lspkind-nvim'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-nvim-lua'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  -- use 'hrsh7th/cmp-cmdline' -- I don't understand what this is yet so skip loading for now.
  use 'saadparwaiz1/cmp_luasnip'
  use 'rafamadriz/friendly-snippets'
  use {'tzachar/cmp-tabnine', run='./install.sh', requires = 'hrsh7th/nvim-cmp'} -- AI helper to type quicker
end)

-- vim: ts=2 sts=2 sw=2 et
