-- to help auto compile packer on changes from relevant files

local M = {}

-- this should be a function (not a variable) so it re-requires when needed
function M.require_groups()
  return {
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
  }
end

function M.load_plugins(use, groups)
  vim.tbl_map(function(group)
    vim.tbl_map(function(plugin)
      use(plugin)
    end, group)
  end, groups)
end

local myvimrc_dir = vim.fs.dirname(vim.env.MYVIMRC)
local things_to_be_compiled_via_packer = myvimrc_dir .. '/lua/packer-groups/*'

function M.reload_packer_group_for()
  for module, _ in pairs(package.loaded) do
    -- only need to invalidate my modules related to package setups
    if module:match '^packer%-groups%.' then
      package.loaded[module] = nil
    end
  end

  M.compile(M.require_groups())
end

-- because we can bypass packer.startup with packer.reset and packer.init
function M.compile(groups)
  local packer = require 'packer'
  packer.reset()
  packer.init()
  M.load_plugins(packer.use, groups)
  packer.compile()
end

-- no more manual compiling thanks to:
-- - https://github.com/nvim-lua/kickstart.nvim/blob/master/init.lua
-- - https://github.com/wbthomason/packer.nvim/issues/955
function M.setup_autocompile()
  local packer_group = vim.api.nvim_create_augroup('MyPackerAG', { clear = true })
  vim.api.nvim_create_autocmd('BufWritePost', {
    callback = function()
      M.reload_packer_group_for()
    end,
    group = packer_group,
    pattern = {
      things_to_be_compiled_via_packer,
    },
  })
end

return M

-- vim: ts=2 sts=2 sw=2 et
