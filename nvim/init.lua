-- originally from https://github.com/mjlbach/defaults.nvim/blob/73d4b205be5711b681ef2df9d171b1c55040803b/init.lua

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
  use 'tpope/vim-commentary' -- "gc" to comment visual regions/lines
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
  use 'nvim-treesitter/nvim-treesitter'
  -- Additional textobjects for treesitter
  use 'nvim-treesitter/nvim-treesitter-textobjects'
  use 'neovim/nvim-lspconfig' -- Collection of configurations for built-in LSP client
  use 'hrsh7th/nvim-compe' -- Autocompletion plugin
  use 'L3MON4D3/LuaSnip' -- Snippets plugin

  --- bringing plugins to accomodate my muscle memory with ../SpaceVim.d

  -- for more text objects, visit https://github.com/kana/vim-textobj-user/wiki
  use { 'kana/vim-textobj-line', requires = { 'kana/vim-textobj-user' } }
  use { 'kana/vim-textobj-entire', requires = { 'kana/vim-textobj-user' } }

  use 'christoomey/vim-system-copy' -- copy text to clipboard with `cp`
  use 'roxma/vim-tmux-clipboard' -- share clipboard with tmux
  use 'christoomey/vim-tmux-navigator' -- navigate with tmux key binding
  use 'tpope/vim-surround' -- surround text with something like quotes
  use { 'codota/tabnine-vim', event = 'InsertEnter' } -- AI helper to type quicker
  use 'sheerun/vim-polyglot' -- one plugin to accomodate many different filetypes
  use { 'folke/which-key.nvim' } -- show key bindings just like SpaceVim
  use { 'junegunn/goyo.vim' } -- a helper to focus on one window
  use { 'akinsho/nvim-toggleterm.lua' } -- a great ergonomic terminal customization
  use { 'ap/vim-buftabline' } -- simple and light tab (actually buffer) visualizer
  use { 'kyazdani42/nvim-tree.lua', requires = 'kyazdani42/nvim-web-devicons' } -- enhanced filetree replacing netrw
  use { 'axelf4/vim-strip-trailing-whitespace' } -- strip whitespace on save
  use { 'tpope/vim-repeat' } -- enhance `.` to repeat on non-native functionality like vim-surround
end)

--Incremental live completion
vim.o.inccommand = 'nosplit'

--Set highlight on search
vim.o.hlsearch = true

--Make line numbers default
vim.wo.number = true

--Do not save when switching buffers
vim.o.hidden = true

--Enable mouse mode
vim.o.mouse = 'a'

--Enable break indent
vim.o.breakindent = true

--Save undo history
vim.cmd [[set undofile]]

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

--Set colorscheme (order is important here)
vim.o.termguicolors = true
vim.g.onedark_terminal_italics = 2
vim.cmd [[colorscheme onedark]]

--Set statusbar
vim.g.lightline = {
  colorscheme = 'onedark',
  active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } },
  component_function = { gitbranch = 'fugitive#head' },
}

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
-- vim.g.mapleader = ' '
-- vim.g.maplocalleader = ' '

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

--Map blankline
vim.g.indent_blankline_char = '┊'
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile' }
vim.g.indent_blankline_char_highlight = 'LineNr'
vim.g.indent_blankline_show_trailing_blankline_indent = false

-- Gitsigns
require('gitsigns').setup {
  signs = {
    add = { hl = 'GitGutterAdd', text = '+' },
    change = { hl = 'GitGutterChange', text = '~' },
    delete = { hl = 'GitGutterDelete', text = '_' },
    topdelete = { hl = 'GitGutterDelete', text = '‾' },
    changedelete = { hl = 'GitGutterChange', text = '~' },
  },
}

-- Telescope
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}
--Add leader shortcuts
vim.api.nvim_set_keymap('n', '<space><space>', [[<cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>sf', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>sb', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>sh', [[<cmd>lua require('telescope.builtin').help_tags()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>st', [[<cmd>lua require('telescope.builtin').tags()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>sd', [[<cmd>lua require('telescope.builtin').grep_string()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>sp', [[<cmd>lua require('telescope.builtin').live_grep()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>so', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<CR>]], { noremap = true, silent = true }) vim.api.nvim_set_keymap('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<CR>]], { noremap = true, silent = true })

-- Highlight on yank
vim.api.nvim_exec(
  [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]],
  false
)

-- Y yank until the end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

-- LSP settings
local nvim_lsp = require 'lspconfig'
local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'v', '<leader>ca', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>so', [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], opts)
  vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- Enable the following language servers
local servers = { 'clangd', 'rust_analyzer', 'pyright', 'tsserver' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

-- Example custom server
local sumneko_root_path = vim.fn.getenv("HOME").."/.local/bin/sumneko_lua" -- Change to your sumneko root installation
local sumneko_binary = sumneko_root_path .. '/bin/linux/lua-language-server'

-- Make runtime files discoverable to the server
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

require('lspconfig').sumneko_lua.setup {
  cmd = { sumneko_binary, '-E', sumneko_root_path .. '/main.lua' },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Setup your lua path
        path = runtime_path,
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file('', true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

-- Treesitter configuration
-- Parsers must be installed manually via :TSInstall
require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true, -- false will disable the whole extension
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
  },
}

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Compe setup
require('compe').setup {
  source = {
    path = true,
    nvim_lsp = true,
    luasnip = true,
    buffer = false,
    calc = false,
    nvim_lua = false,
    vsnip = false,
    ultisnips = false,
  },
}

-- Utility functions for compe and luasnip
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
  local col = vim.fn.col '.' - 1
  if col == 0 or vim.fn.getline('.'):sub(col, col):match '%s' then
    return true
  else
    return false
  end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
local luasnip = require 'luasnip'

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t '<C-n>'
  elseif luasnip.expand_or_jumpable() then
    return t '<Plug>luasnip-expand-or-jump'
  elseif check_back_space() then
    return t '<Tab>'
  else
    return vim.fn['compe#complete']()
  end
end

_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t '<C-p>'
  elseif luasnip.jumpable(-1) then
    return t '<Plug>luasnip-jump-prev'
  else
    return t '<S-Tab>'
  end
end

-- Map tab to the above tab complete functiones
vim.api.nvim_set_keymap('i', '<Tab>', 'v:lua.tab_complete()', { expr = true })
vim.api.nvim_set_keymap('s', '<Tab>', 'v:lua.tab_complete()', { expr = true })
vim.api.nvim_set_keymap('i', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })
vim.api.nvim_set_keymap('s', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })

-- Map compe confirm and complete functions
vim.api.nvim_set_keymap('i', '<cr>', 'compe#confirm("<cr>")', { expr = true })
vim.api.nvim_set_keymap('i', '<c-space>', 'compe#complete()', { expr = true })

--- my custom configs to accomodate my muscle memory with ../SpaceVim.d

-- show some hidden characters
vim.o.listchars = [[tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶]]
vim.o.list = true

-- use which-key to accomodate visual assistant on key-bindings ../SpaceVim.d
paste_after_here = '"*p'
paste_before_here = '"*P'
if vim.fn.has('unnamedplus') == 1 then
  paste_after_here = '"+p'
  paste_before_here = '"+P'
end

require('which-key').register({
    p = {paste_after_here, 'paste after here'},
    P = {paste_before_here, 'paste before here'},
}, { prefix = '<leader>' })

require('which-key').register({
    [' '] = {'', 'remove trailing whitespaces'},
}, { prefix = ',' })

require('which-key').register({
    w = {
      m = {':Goyo<CR>', 'maximize/minimize window'}
    },
    ['<Tab>'] = {':bn<CR>', 'rotate buffer'},
    ["'"] = {':ToggleTerm<CR>', 'open shell'},
    j = 'split args', -- only set a text for an already configured keymap
    ['<CR>'] = {'@q', 'macro q'}, -- setting a special key
    f = { -- set a nested structure
        name = '+find',
        b = {'<Cmd>Telescope buffers<CR>', 'buffers'},
        h = {'<Cmd>Telescope help_tags<CR>', 'help tags'},
        c = {
            name = '+commands',
            c = {'<Cmd>Telescope commands<CR>', 'commands'},
            h = {'<Cmd>Telescope command_history<CR>', 'history'},
        },
        q = {'<Cmd>Telescope quickfix<CR>', 'quickfix'},
        g = {
            name = '+git',
            g = {'<Cmd>Telescope git_commits<CR>', 'commits'},
            c = {'<Cmd>Telescope git_bcommits<CR>', 'bcommits'},
            b = {'<Cmd>Telescope git_branches<CR>', 'branches'},
            s = {'<Cmd>Telescope git_status<CR>', 'status'},
        },
        s = {':w<CR>', 'save file'}, -- set a single command and text
        t = {':NvimTreeToggle<CR>', 'toggle file tree'},
    },
    s = {
        name = '+Searching/Symbol',
        c = {'<Cmd>nohlsearch<CR>', 'clear hihglight'},
    },
}, { prefix = '<Space>' })

-- put away `tags` according to https://github.com/ludovicchabant/vim-gutentags/issues/211
-- vim.g.gutentags_ctags_tagfile = '.git/gutentags'

--- my keymaps to to accomodate my muscle memory with ../SpaceVim.d

-- because `use 'zhou13/vim-easyescape'` is too slow on startup
vim.api.nvim_set_keymap('i', 'jk', '<Esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Tab>', '<Cmd>wincmd w<CR>', { noremap = true, silent = true })

-- indent right away without waiting in normal mode
vim.api.nvim_set_keymap('n', '>', '>>', { noremap = true })
vim.api.nvim_set_keymap('n', '<', '<<', { noremap = true })

-- stay in visual mode after indentation in visual mode
vim.api.nvim_set_keymap('v', '>', '>gv', { noremap = true })
vim.api.nvim_set_keymap('v', '<', '<gv', { noremap = true })

-- q to close
vim.api.nvim_set_keymap('n', 'q', '<Cmd>q<CR>', { noremap = true })

-- remember the last position and go to that line
-- https://askubuntu.com/a/202077
vim.cmd [[if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif]]

-- augment terminal

vim.cmd [[
  let $FORCE_LOAD_MY_ZSH_STUFF = 1
  let $NO_VI_KEY_ON_ZSH = 1
]]

require("toggleterm").setup{
  -- size can be a number or function which is passed the current terminal
  size =  function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end, -- | 20
  open_mapping = [[<c-\>]],
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  shade_terminals = true,
  shading_factor = '<number>', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  persist_size = true,
  direction = 'horizontal', -- | 'vertical' | 'window' | 'float',
  close_on_exit = true, -- close the terminal window when the process exits
  shell = vim.o.shell, -- change the default shell
  -- This field is only relevant if direction is set to 'float'
  float_opts = {
    -- The border key is *almost* the same as 'nvim_win_open'
    -- see :h nvim_win_open for details on borders however
    -- the 'curved' border is a custom border type
    -- not natively supported but implemented in this plugin.
    border = 'single', -- | 'double' | 'shadow' | 'curved' | ... other options supported by win open
    -- width = <value>,
    -- height = <value>,
    winblend = 3,
    highlights = {
      border = "Normal",
      background = "Normal",
    }
  }
}

local opts = {noremap = true}
vim.api.nvim_set_keymap('t', '<esc>', [[<C-\><C-n>]], opts)
vim.api.nvim_set_keymap('t', 'jk', [[<C-\><C-n>]], opts)
vim.api.nvim_set_keymap('t', '<C-h>', [[<C-\><C-n><C-W>h]], opts)
vim.api.nvim_set_keymap('t', '<C-j>', [[<C-\><C-n><C-W>j]], opts)
vim.api.nvim_set_keymap('t', '<C-k>', [[<C-\><C-n><C-W>k]], opts)
vim.api.nvim_set_keymap('t', '<C-l>', [[<C-\><C-n><C-W>l]], opts)

-- nvimtree
vim.g.nvim_tree_side = 'right'

-- vim.g.nvim_tree_auto_open = true
vim.g.nvim_tree_auto_close = true
vim.g.nvim_tree_disable_netrw = false
vim.g.nvim_tree_hijack_netrw = false
local tree_cb = require'nvim-tree.config'.nvim_tree_callback
-- override default mappings
vim.g.nvim_tree_bindings = {
  { key = {"o", "<2-LeftMouse>", "l"},         cb = tree_cb("edit") },
  -- { key = {"<2-RightMouse>", "<C-]>", "<CR>"}, cb = tree_cb("cd") },
  { key = {"-", "h"},                          cb = tree_cb("dir_up") },
  { key = {"<Tab>"},                           cb = ':wincmd w<CR>' },
}

-- only show when there is more than two files are open
vim.g.buftabline_show = 1
