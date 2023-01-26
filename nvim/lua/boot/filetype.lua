-- where I customize editing related behaviours per file type
-- go to `../../ftdetect/detect.vim` for detecting files

-- show some hidden characters
vim.opt.list = true
vim.opt.listchars:append 'tab:  →' -- use this instead of 'tab:→ ' until https://github.com/lukas-reineke/indent-blankline.nvim/issues/503 gets resolved
vim.opt.listchars:append 'eol:↵'
vim.opt.listchars:append 'trail:·'
vim.opt.listchars:append 'extends:↷'
vim.opt.listchars:append 'precedes:↶'

-- default tabstop - optimized for 2 spaces - this is my go-to option
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2

local ftGrp = vim.api.nvim_create_augroup('MyFileTypeAG', { clear = true })
-- escape for must be tab based files
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    'make',
  },
  callback = function()
    vim.bo.expandtab = false
  end,
  group = ftGrp,
})
