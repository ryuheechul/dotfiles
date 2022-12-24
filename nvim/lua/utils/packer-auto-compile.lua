-- to help auto compile packer on changes from relevant files

local M = {}

local run_cmd = require('utils.shell').run_cmd

function M.load_plugins(use, groups)
  vim.tbl_map(function(group)
    vim.tbl_map(function(plugin)
      use(plugin)
    end, group)
  end, groups)
end

local myvimrc_dir = vim.fs.dirname(vim.env.MYVIMRC)
local myluadir = myvimrc_dir .. '/lua'
-- using a marker file is not so elegant
-- this procedure can be improved by embedding a "plugin" that its job is to auto compile
-- maybe I can try that at next iteration
local compile_marker_file = myvimrc_dir .. '/plugin/.recompile_on_start_up'

local things_to_be_compiled_via_packer = myvimrc_dir .. '/lua/packer-groups/*'

function M.reload_deps(namespace)
  local grep = require('utils.shell').grep

  local search_path = things_to_be_compiled_via_packer .. '.lua'

  local result = grep([["']] .. namespace .. [['"]] .. ' -l --max-depth=0', search_path)

  local filenames = vim.split(result, '\n')
  if string.len(result) > 0 then
    vim.tbl_map(function(fname)
      M.reload_packer_group_for(fname)
    end, filenames)
  end
end

function M.reload_packer_group_for(filename)
  if string.len(filename) == 0 then
    return
  end

  local normalized = filename:gsub(myluadir .. '/', ''):gsub('/', '.')
  local namespace = normalized:match '(.*)%.lua'

  if namespace:match '^packer%-groups%.' then
    if package.loaded[namespace] then
      package.loaded[namespace] = nil
    end
  end

  local toplevel_ns = namespace:match '^(packer%-groups%.[^%.]*)'

  -- if it's not top level find the usage at top level and refresh that one instead
  -- which will trigger the original file to be reloaded as well
  if toplevel_ns ~= namespace then
    M.reload_deps(namespace)

    return
  end

  M.compile {
    require(namespace),
  }
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
  -- ort vim.env.MYVIMRC:match '(.*)/.*'
  -- local plugins_via_packer_dot_lua = myvimrc_dir .. '/lua/plugins-via-packer.lua'

  local packer_group = vim.api.nvim_create_augroup('MyPackerAG', { clear = true })
  vim.api.nvim_create_autocmd('BufWritePost', {
    callback = function(args)
      M.reload_packer_group_for(args.match)
      -- since the current fast minimal compile would not be good enough for next start up
      M.mark_to_compile_on_next_startup()
    end,
    group = packer_group,
    pattern = {
      things_to_be_compiled_via_packer,
    },
  })
end

local alpha_needs_to_wake_it_self_up = false

function M.wakup_alpha_if_applicable()
  if alpha_needs_to_wake_it_self_up then
    vim.cmd.Alpha()
    alpha_needs_to_wake_it_self_up = false
  end
end

function M.compile_on_start_up_if_necessary()
  if M.does_need_to_compile_on_start_up() then
    alpha_needs_to_wake_it_self_up = true
    vim.opt.winbar = "       needs recompiling and I'm on it..."
    -- this is optional just to refresh the cache
    vim.cmd.LuaCacheClear()
    require('packer').compile()
    M.mark_compiled_on_startup()
  end
end

function M.mark_to_compile_on_next_startup()
  run_cmd('touch ' .. compile_marker_file)
end

function M.does_need_to_compile_on_start_up()
  local result = run_cmd('ls ' .. compile_marker_file)
  if string.len(result) > 0 then
    return true
  end

  return false
end

function M.mark_compiled_on_startup()
  run_cmd('rm ' .. compile_marker_file)
end

return M

-- vim: ts=2 sts=2 sw=2 et
