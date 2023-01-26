-- where I customize editing related behaviours per file type

local ftGrp = vim.api.nvim_create_augroup('MyFileTypeAG', { clear = true })
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

-- vim: ts=2 sts=2 sw=2 et
