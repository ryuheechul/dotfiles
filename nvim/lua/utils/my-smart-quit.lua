-- telescope window resilient `close`
local close_telescope_in_mind = function()
  -- first try with the regular close
  local ok = pcall(vim.cmd, 'close')
  -- when it fails, it's assumed that it's telescope window and attempt to close with their "API"
  if not ok then
    vim.cmd [[ execute "normal \<Esc>" ]]
  end
end

-- when they are not buflisted meaning when they are not file buffers
local quit_unlisted = function()
  local wins = vim.api.nvim_list_wins()
  local is_there_only_one_window = #wins < 2

  if is_there_only_one_window then
    -- quit when it's the last window
    vim.cmd [[ q ]]
  else
    -- just close the window when there are still multiple windows
    -- so a buffer like help can simply close instead of quitting neovim
    close_telescope_in_mind()
  end
end

-- aka file buffers
local quit_listed = function()
  local wins = vim.api.nvim_list_wins()

  local buflisted = vim.fn.getbufinfo { buflisted = 1 }
  local is_one_buffer_with_multi_wins = #wins > 1 and #buflisted < 2

  if not is_one_buffer_with_multi_wins then
    -- now it's time to delegate to BufDel for file buffers
    -- (using https://github.com/ojroques/nvim-bufdel)
    vim.cmd [[ BufDel ]]
  else
    -- but also close windows first if only single buffer left with multiple windows
    -- to prevent closing abruptly when still multiple windows are present
    vim.cmd [[ close ]]
  end
end

-- call it via `:lua require('utils.my-smart-quit')()`
return function()
  local current = vim.fn.bufnr '%'
  local is_listed = vim.fn.buflisted(current) ~= 0

  -- debug this via `set bl` and `set nobl`
  if is_listed then
    quit_listed()
  else
    quit_unlisted()
  end
end
