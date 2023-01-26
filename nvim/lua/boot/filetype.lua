-- where I customize editing related behaviours per file type
-- go to `../../ftdetect/detect.vim` for detecting files

local ftGrp = vim.api.nvim_create_augroup('MyFileTypeAG', { clear = true })

-- optimized for 2 spaces - this is my go-to option
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    'zsh',
    'bash',
    'lua',
    'vim',
    'nim',
    'json',
    'markdown',
    'Earthfile',
    'javascript',
    'typescript',
  },
  callback = function()
    vim.bo.expandtab = true
    vim.bo.softtabstop = 2
    vim.bo.shiftwidth = 2
  end,
  group = ftGrp,
})

-- give illusion of using 2 spaces for tab based files
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    'make',
  },
  callback = function()
    vim.bo.tabstop = 2
    vim.bo.expandtab = false
  end,
  group = ftGrp,
})
