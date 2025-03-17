-- command height
vim.o.ch = 0

-- When this option is set, the screen will not be redrawn while executing macros, registers and other commands that have not been typed.
-- Also, updating the window title is postponed.
vim.o.lazyredraw = false
-- lazyredraw seems to cause more lags (especially with nvim-cmp) than not with my setup.
-- possibly it might work better with certain terminals than not so I might consider branching this out per terminal

-- Incremental live completion
vim.o.inccommand = 'nosplit'

-- Set highlight on search
vim.o.hlsearch = true

-- Set spell check
vim.o.spell = true

-- Make line numbers default
vim.wo.number = true

-- Do not save when switching buffers
vim.o.hidden = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.cmd [[set undofile]]

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme (order is important here)
vim.o.termguicolors = true
vim.g.onedark_terminal_italics = 2

-- Replace https://github.com/luukvbaal/stabilize.nvim with Neovim 0.9.0 onward
vim.o.splitkeep = 'screen'

-- fancy pinpointing on the cursor
if vim.env.my_nvim_no_cursor_pinpoint == nil then
  vim.o.cursorline = true
  vim.o.cursorcolumn = true
  -- this makes it feel natural with the options above
  vim.o.virtualedit = 'all'
end

-- Highlight on yank
local yankHighlightGrp = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  pattern = '*',
  command = 'silent! lua vim.highlight.on_yank()',
  group = yankHighlightGrp,
})

-- fix key maps on help buffer so it works like `less`
local helpGrp = vim.api.nvim_create_augroup('MyHelpAUG', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  -- for now, including `checkhealth` and `qf` (for e.g. `gd` and `gr`) too
  pattern = { 'help', 'checkhealth', 'qf', 'lazy' },
  callback = function()
    -- nowait only works with buffer mapping
    vim.keymap.set('n', 'd', '<C-d>', { noremap = true, silent = true, buffer = true, nowait = true })
    vim.keymap.set('n', 'u', '<C-u>', { noremap = true, silent = true, buffer = true, nowait = true })
    vim.keymap.set('n', 'f', '<C-f>', { noremap = true, silent = true, buffer = true, nowait = true })
    vim.keymap.set('n', 'b', '<C-b>', { noremap = true, silent = true, buffer = true, nowait = true })
    vim.keymap.set('n', '<Esc>', require 'utils.my-smart-quit', { noremap = true, silent = true, buffer = true })
  end,
  group = helpGrp,
})

-- always maximize the window for help buffer when help buffer is focused
vim.api.nvim_create_autocmd('BufEnter', {
  pattern = '*',
  callback = function()
    if vim.bo.filetype == 'help' then
      vim.cmd [[ WindowsMaximize ]]
    end
  end,
  group = helpGrp,
})

-- giving option to ignore this since the logic doesn't handle
-- +[linenumber] arg on startup
-- so use it like `my_nvim_forget_line_number=1 nvim +10 filename`
if vim.env.my_nvim_forget_line_number == nil then
  -- remember the last position and go to that line
  -- https://askubuntu.com/a/202077
  -- https://www.reddit.com/r/neovim/comments/1052d98/comment/j38dtlz
  local lastplaceGrp = vim.api.nvim_create_augroup('LastPlace', { clear = true })
  vim.api.nvim_create_autocmd('BufReadPost', {
    group = lastplaceGrp,
    callback = function()
      local valid_line = vim.fn.line [['"]] >= 1 and vim.fn.line [['"]] < vim.fn.line '$'
      local not_commit = vim.bo.filetype ~= 'gitcommit'

      if valid_line and not_commit then
        vim.cmd [[normal! g`"]]
      end
    end,
  })
end

-- only show when there is more than two files are open
vim.g.buftabline_show = 1

-- this enables pseudo-transparency for popup-menu
-- some plugins need to set `winblend` to have same effects.
vim.opt.pumblend = 20

-- sync clipboard with the system's one - https://stackoverflow.com/a/30691754/1570165
vim.opt.clipboard = 'unnamedplus'
-- For the option above to work across SSH connection, `ForwardX11 yes` (and `ForwardX11Trusted yes` if `ForwardX11` wasn't enough) options would be required. Otherwise the OSC 52 would make it trivial to copy and paste, however not all layers support it, notably Zellij currently doesn't support pasting

local shouldOsc52pasteBeAvailable = vim.env.ZELLIJ_SESSION_NAME == nil and vim.env.INSIDE_EMACS == nil

if shouldOsc52pasteBeAvailable then
  -- without this, it will rely on X11 `$DISPLAY` env var via xsel or similar instead of OSC 52 way
  vim.g.clipboard = {
    name = 'OSC 52',
    copy = {
      ['+'] = require('vim.ui.clipboard.osc52').copy '+',
      ['*'] = require('vim.ui.clipboard.osc52').copy '*',
    },
    paste = {
      ['+'] = require('vim.ui.clipboard.osc52').paste '+',
      ['*'] = require('vim.ui.clipboard.osc52').paste '*',
    },
  }
else
  -- Since `osc` command (that is being invoked via `../../../bin/path/default/pb[copy|paste]`) is wasn't able to handle `/dev/tty` when it's not connected to SSH (`$SSH_TTY` is being used in this case)
  if vim.env.SSH_TTY ~= nil then
    vim.g.clipboard = {
      name = 'pbcopy/pbpaste',
      copy = {
        ['+'] = 'pbcopy',
        ['*'] = 'pbcopy',
      },
      paste = {
        ['+'] = 'pbpaste',
        ['*'] = 'pbpaste',
      },
    }
  else
    -- Let nvim to figure out what to do for a local terminal (without native osc 52) by doing nothing (after setting `vim.opt.clipboard = 'unnamedplus'` above for all cases)
  end
end

-- to assist ../shell/source.zsh
vim.env.NVIM_LISTEN_ADDRESS = vim.v.servername

-- a subset extracted from https://github.com/sheerun/vim-polyglot/blob/bc8a81d3592dab86334f27d1d43c080ebf680d42/autoload/polyglot/init.vim#L2704
vim.opt.autoread = true -- Reload unchanged files automatically.
vim.opt.shortmess:append 'A' -- This is needed to avoid swapfile warning when auto-reloading

-- to use host nvim instead of creating another nvim process
-- mainly for `lf.nvim` plugin at ../plugins/system.lua
vim.env.EDITOR = 'editor-in-nvim'
