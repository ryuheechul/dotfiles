-- Install packer
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local groups = {
  { -- packer itself
    'wbthomason/packer.nvim', -- Package manager
  },
  require 'packer-groups.system',
  require 'packer-groups.theme',
  require 'packer-groups.git',
  require 'packer-groups.editing',
  require 'packer-groups.syntax',
  require 'packer-groups.lsp',
  require 'packer-groups.completion',
  require 'packer-groups.debug',
  require 'packer-groups.extra',
  require 'packer-groups.keymaps',
  { -- leftovers
    -- comment since it creates more issue than a help for my usage
    -- 'ludovicchabant/vim-gutentags' -- Automatic tags management
  },
}

local function _values(t) -- via https://stackoverflow.com/a/39991824/1570165
  local i = 0
  return function()
    i = i + 1
    return t[i]
  end
end

local load_plugins = function(use)
  for group in _values(groups) do
    for plugin in _values(group) do
      use(plugin)
    end
  end
end

return require('packer').startup {
  function(use)
    load_plugins(use)

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
      require('packer').sync()
    end
  end,
  config = {
    -- override this due to https://github.com/wbthomason/packer.nvim/issues/746
    max_jobs = 64,
  },
}

-- vim: ts=2 sts=2 sw=2 et
