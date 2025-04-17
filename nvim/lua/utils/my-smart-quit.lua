-- my attempt to make `q` to work smarter and more resilient with various edge cases
-- see ../plugins/system.lua for depedencies

-- telescope window resilient `close`
local close_telescope_in_mind = function()
  -- first try with the regular close
  local ok = pcall(vim.cmd, 'close')
  -- when it fails, it's assumed that it's telescope window and attempt to close with their "API"
  if not ok then
    vim.cmd [[ execute "normal \<Esc>" ]]
  end
end

local close_trouble = function()
  vim.cmd [[ TroubleClose ]]
  -- vim.cmd [[ Trouble diagnostics close ]] -- for v3+
end

local close_nvim = function()
  -- workaround until the issue is resolved, https://github.com/folke/trouble.nvim/issues/378
  close_trouble()
  vim.cmd [[ q ]]
end

-- extend vim.api.nvim_list_wins
local list_wins = function()
  local wins = vim.api.nvim_list_wins()

  return vim.tbl_filter(function(win)
    local is_valid = vim.api.nvim_win_is_valid(win)
    -- in case there is a "hidden" window like 'folke/drop.nvim'
    local is_focusable = vim.api.nvim_win_get_config(win).focusable

    return is_valid and is_focusable
  end, wins)
end

-- when they are not buflisted meaning when they are not file buffers
local quit_unlisted = function()
  local wins = list_wins()
  local is_there_only_one_window = #wins < 2

  if is_there_only_one_window then
    -- quit when it's the last window
    close_nvim()
  else
    -- just close the window when there are still multiple windows
    -- so a buffer like help can simply close instead of quitting neovim
    close_telescope_in_mind()
  end
end

-- this is not the same with `try_bufdel`
local try_bufdelete = function()
  local ok = pcall(vim.cmd, 'Bdelete')
  -- when it fails fall back to `bd!` instead
  if not ok then
    vim.cmd [[ bd! ]]
  end
end

-- this is not the same with `try_bufdelete`
local try_bufdel = function()
  local is_curr_buf_modifiable = vim.api.nvim_buf_get_option(0, 'modifiable')

  -- this helps to close "support" windows e.g. the ones from `vim.lsp.buf.references`
  if not is_curr_buf_modifiable then
    close_nvim()
  else
    local ok = pcall(vim.cmd, 'BufDel')
    -- when it fails, it's assumed that it's something like Luapad that doesn't work very well with BufDel
    -- so fall back to `bd!` instead
    if not ok then
      try_bufdelete()
    end
  end
end

-- aka file buffers
local quit_listed = function()
  local wins = list_wins()

  local buflisted = vim.fn.getbufinfo { buflisted = 1 }
  local is_one_buffer_with_multi_wins = #wins > 1 and #buflisted < 2

  if not is_one_buffer_with_multi_wins then
    -- now it's time to delegate to BufDel for file buffers
    -- (using https://github.com/ojroques/nvim-bufdel)
    try_bufdel()
  else
    -- but also close windows first if only single buffer left with multiple windows
    -- to prevent closing abruptly when still multiple windows are present
    if vim.bo.filetype == 'qf' then -- to accommodate 'kevinhwang91/nvim-bqf'
      try_bufdelete()
    else
      close_nvim()
    end
  end
end

-- call it via `:lua require('utils.my-smart-quit')()`
return function()
  -- don't try to be too smart on 'nofile' `buftype`
  if vim.bo.buftype == 'nofile' then
    close_nvim()
    return
  end

  local current = vim.fn.bufnr '%'
  local is_listed = vim.fn.buflisted(current) ~= 0

  -- debug this via `set bl` and `set nobl`
  if is_listed then
    quit_listed()
  else
    quit_unlisted()
  end
end
