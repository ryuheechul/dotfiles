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

local max_jobs = 32 -- similar to https://github.com/ryuheechul/dotfiles/blob/4b1d8d8bae07ace02a226efcc533816b0bb5acf6/nvim/lua/plugins-via-packer.lua#L30

-- not much use to see this visually at the moment
opts.change_detection.notify = false
-- limit concurrency - impacts something like `update` and `restore`
opts.concurrency = max_jobs -- not setting limits would likely cause issues - https://github.com/folke/lazy.nvim/issues/462
opts.checker = {
  -- automatically check for plugin updates
  enabled = false,
  concurrency = max_jobs / 4, -- since automatic checks shouldn't need to be too fast that potentially impact more important tasks
  notify = false,
  frequency = 3600 --[[ an hour ]]
    * 24
    * 7, -- every week
}

if vim.env.my_nvim_plugins_default_to_lazy ~= nil then
  opts.defaults.lazy = true
end

-- put the table back to
setmetatable(opts, nil)

require('lazy').setup('plugins', opts)
