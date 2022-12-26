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
local pac = require 'utils.packer-auto-compile'

-- since lua cache loaded function's content it's not easy to load this function itself with autocmd without loading the whole file which is inefficient
-- so for now restarting the nvim the best way to change the behaviour of this function
pac.setup_autocompile()

return require('packer').startup {
  function(use)
    pac.load_plugins(use, pac.require_groups())

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
