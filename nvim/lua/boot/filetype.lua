-- where I customize editing related behaviours per file type
-- go to `../../ftdetect/all.lua` for detecting files

-- show some hidden characters
vim.opt.list = true
vim.opt.listchars:append 'tab:> '
vim.opt.listchars:append 'eol:↵'
vim.opt.listchars:append 'trail:·'
vim.opt.listchars:append 'extends:↷' -- only visible when nowrap
vim.opt.listchars:append 'precedes:↶' -- same as above

-- fillchars
vim.opt.fillchars:append 'eob: ' -- empty lines at the end of a buffer (the default is '~')

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

-- reinforce in case a drifted result is found for some filetype due to the usage of other plugins
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    'kdl',
  },
  callback = function()
    vim.opt.tabstop = 2
  end,
  group = ftGrp,
})
