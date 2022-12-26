-- bootstrap lazy.nvim the plugin manager
local function bootstrap()
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
end

bootstrap()

-- achieve something similar to https://ramdajs.com/docs/#assocPath
local opts = vim.defaulttable()

-- not much use to see this visually at the moment
opts.change_detection.notify = false

if vim.env.my_nvim_plugins_default_to_lazy ~= nil then
  opts.defaults.lazy = true
end

-- put the table back to
setmetatable(opts, nil)

require('lazy').setup('plugins', opts)
