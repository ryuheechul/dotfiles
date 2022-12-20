-- last status
vim.o.ls = 0

-- command height
vim.o.ch = 0

-- When this option is set, the screen will not be redrawn while executing macros, registers and other commands that have not been typed.
-- Also, updating the window title is postponed.
vim.o.lazyredraw = false
-- lazyredraw seems to cause more lags (especially with nvim-cmp) than not with my setup.
-- possibly it might work better with certain terminals than not so I might consider branching this out per terminal

--Incremental live completion
vim.o.inccommand = 'nosplit'

--Set highlight on search
vim.o.hlsearch = true

--Set spell check
vim.o.spell = true

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

-- Highlight on yank
local yankHighlightGrp = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  pattern = '*',
  command = 'silent! lua vim.highlight.on_yank()',
  group = yankHighlightGrp,
})

-- Maximize on git commit message editing
local gitGrp = vim.api.nvim_create_augroup('MyGitAUG', { clear = true })
vim.api.nvim_create_autocmd('BufReadPost', {
  pattern = 'COMMIT_EDITMSG',
  command = 'WindowsMaximize',
  group = gitGrp,
})

-- fix key maps on help buffer so it works like `less`
local helpGrp = vim.api.nvim_create_augroup('MyHelpAUG', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'help',
  callback = function()
    -- nowait only works with buffer mapping
    vim.keymap.set('n', 'd', '<C-d>', { noremap = true, silent = true, buffer = true, nowait = true })
    vim.keymap.set('n', 'b', '<C-b>', { noremap = true, silent = true, buffer = true, nowait = true })
  end,
  group = helpGrp,
})

-- giving option to ignore this since the logic doesn't handle
-- +[linenumber] arg on startup
-- so use it like `my_nvim_forget_line_number=1 nvim +10 filename`
if vim.env.my_nvim_forget_line_number == nil then
  -- remember the last position and go to that line
  -- https://askubuntu.com/a/202077
  vim.cmd [[if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
  endif]]
end

-- only show when there is more than two files are open
vim.g.buftabline_show = 1

-- this enables pseudo-transparency for popup-menu
-- some plugins need to set `winblend` to have same effects.
vim.opt.pumblend = 20

-- sync clipboard with the system's one - https://stackoverflow.com/a/30691754/1570165
vim.opt.clipboard = 'unnamedplus'

-- to assist ../shell/source.zsh
vim.env.NVIM_LISTEN_ADDRESS = vim.v.servername

-- vim: ts=2 sts=2 sw=2 et
