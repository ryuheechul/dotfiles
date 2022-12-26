local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    '--single-branch',
    'https://github.com/folke/lazy.nvim.git',
    lazypath,
  }
end
vim.opt.runtimepath:prepend(lazypath)

if vim.env.my_nvim_plugins_default_to_lazy ~= nil then
  require('lazy').setup('plugins', { defaults = { lazy = true } })
else
  require('lazy').setup 'plugins'
end
