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

local max_jobs = 32 -- similar to https://github.com/ryuheechul/dotfiles/blob/4b1d8d8bae07ace02a226efcc533816b0bb5acf6/nvim/lua/plugins-via-packer.lua#L30

-- plain nested tables (previously vim.defaulttable(), for
-- https://ramdajs.com/docs/#assocPath-like convenience - but its nested
-- auto-vivifying tables survive setmetatable(opts, nil), which only strips
-- the top-level metatable. lazy.nvim's own vim.tbl_deep_extend/vim.islist
-- then probes opts.change_detection[1], opts.change_detection[2], ... which
-- never returns nil (each access auto-vivifies another table), and mutating
-- a table while pairs() is iterating it is undefined behavior in Lua -
-- LuaJIT's hash rehashing made this occasionally spin forever, ballooning
-- memory into the tens of GB. Root cause of the intermittent nvim hangs.
local opts = {
  change_detection = {
    -- not much use to see this visually at the moment
    notify = false,
  },
  -- limit concurrency - impacts something like `update` and `restore`
  concurrency = max_jobs, -- not setting limits would likely cause issues - https://github.com/folke/lazy.nvim/issues/462
  checker = {
    -- automatically check for plugin updates
    enabled = false,
    concurrency = max_jobs / 4, -- since automatic checks shouldn't need to be too fast that potentially impact more important tasks
    notify = false,
    frequency = 3600 --[[ an hour ]]
      * 24
      * 7, -- every week
  },
  defaults = {},
}

if vim.env.my_nvim_plugins_default_to_lazy ~= nil then
  opts.defaults.lazy = true
end

require('lazy').setup('plugins', opts)
